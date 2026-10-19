// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A fit running in another process, reporting its progress back.)

ANY ENGINE, ANY MODEL. The native minimizer reports every improvement through
TFitTask.ShowCurMin, and that one call is where the service records a running
fit's progress. A backend that fits in another process - the Python sidecar, or a
compute server elsewhere - used to be silent until its reply came back, so a fit
on either showed nothing at all. It now relays what the far side has reached
into the same task, through the same call, so everything downstream of it is
the same whichever engine ran.

WHAT IS TESTED HERE. The relay itself, driven through method pointers standing
in for the two HTTP calls: that a reply reaches the task's curves and the
engine's funnel, that a peer with nothing yet is asked again, that a peer with no
progress route stops being asked without failing the fit, and that the fit's own
reply - or its failure - is what the caller gets. The POST runs on a real helper
thread, because that is what lets the calling thread poll; it blocks until the
test has seen the polls it wants, so no assertion depends on timing.

AND THE FIELD THAT ASKS FOR IT, which older peers do not send and must not need.
}
unit testcase_remote_fit_progress;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_task, fit_problem_json, fit_task_marshalling, gauss_points_set, SimpMath,
    remote_fit_progress;

type
    { The far side: a POST that stays out until it has been polled enough, and a
      progress route with a canned answer. }
    TFakeRemote = class(TObject)
    public
        PostReply: string;
        PostFailure: string;
        ProgressReply: string;
        ProgressFailure: string;
        HoldPostUntilPolls: longint;
        Polls: longint;
        LastProgressUrl: string;
        function Post(const AUrl, ABody: string): string;
        function Get(const AUrl: string): string;
    end;

    TRemoteFitProgressTest = class(TTestCase)
    private
        FTask: TFitTask;
        FRemote: TFakeRemote;
        FImprovements: longint;
        procedure Improved;
        function NewTask: TFitTask;
        { The task's own outcome with the first curve's Sigma changed to ASigma. }
        function OutcomeWithSigma(ASigma: double): string;
        function SigmaOfTask: double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  One reply.
        procedure ARelayedReplyReachesTheTasksCurves;
        procedure AndTheEngineIsToldItImproved;
        procedure APeerWithNothingYetIsAskedAgain;
        procedure AnUnreadableReplyStopsTheAsking;

        //  While the fit is out.
        procedure TheFitsOwnReplyIsWhatIsReturned;
        procedure ProgressIsRelayedWhileTheFitIsOut;
        procedure EachPollAsksTheProgressRouteItWasGiven;
        procedure AFitThatFailsRaisesWithItsMessage;
        procedure APeerWithNoProgressRouteIsAskedOnceAndTheFitStillReturns;

        //  Addresses.
        procedure TheProgressRouteHangsOffTheBackendsUrl;
        procedure EveryFitIsGivenItsOwnProgressId;
    end;

    TProgressIdWireTest = class(TTestCase)
    published
        procedure AProblemCarriesItsProgressId;
        procedure AProblemWithoutOneSendsNone;
        procedure AnOlderPeersProblemReadsAsHavingNone;
    end;

implementation

{ ------------------------------- the far side ------------------------------- }

function TFakeRemote.Post(const AUrl, ABody: string): string;
var
    Deadline: TDateTime;
begin
    //  Held until the calling thread has polled enough - or five seconds, which
    //  turns "it never polled" into a failing assertion rather than a hang.
    Deadline := Now + 5 / SecsPerDay;
    while (Polls < HoldPostUntilPolls) and (Now < Deadline) do
        Sleep(5);
    if PostFailure <> '' then
        raise Exception.Create(PostFailure);
    Result := PostReply;
end;

function TFakeRemote.Get(const AUrl: string): string;
begin
    LastProgressUrl := AUrl;
    Inc(Polls);
    if ProgressFailure <> '' then
        raise Exception.Create(ProgressFailure);
    Result := ProgressReply;
end;

{ -------------------------------- the fixture ------------------------------- }

procedure TRemoteFitProgressTest.Improved;
begin
    Inc(FImprovements);
end;

function TRemoteFitProgressTest.NewTask: TFitTask;
var
    P: TFitProblem;
    x: double;
    n: integer;
begin
    P := Default(TFitProblem);
    n := 0;
    x := 0;
    while x <= 20 + 1E-9 do
    begin
        SetLength(P.ProfileX, n + 1);
        SetLength(P.ProfileY, n + 1);
        P.ProfileX[n] := x;
        P.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
        Inc(n);
        x := x + 0.5;
    end;
    P.PositionsX := TDoubleArray.Create(10);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    Result := BuildTaskFromProblem(P);
end;

procedure TRemoteFitProgressTest.SetUp;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    FTask := NewTask;
    //  THE FUNNEL, counted: this is the call the service records progress from.
    FTask.ServerShowCurMin := @Improved;
    FRemote := TFakeRemote.Create;
    FImprovements := 0;
end;

procedure TRemoteFitProgressTest.TearDown;
begin
    FreeAndNil(FRemote);
    FreeAndNil(FTask);
end;

function TRemoteFitProgressTest.OutcomeWithSigma(ASigma: double): string;
var
    O: TFitOutcome;
    j: longint;
begin
    O := ReadOutcomeFromTask(FTask);
    for j := 0 to High(O.Curves[0].Params) do
        if SameText(O.Curves[0].Params[j].Name, 'Sigma') then
            O.Curves[0].Params[j].Value := ASigma;
    Result := FitOutcomeToJson(O);
end;

function TRemoteFitProgressTest.SigmaOfTask: double;
var
    O: TFitOutcome;
    j: longint;
begin
    Result := -1;
    O := ReadOutcomeFromTask(FTask);
    for j := 0 to High(O.Curves[0].Params) do
        if SameText(O.Curves[0].Params[j].Name, 'Sigma') then
            Exit(O.Curves[0].Params[j].Value);
end;

{ --------------------------------- one reply -------------------------------- }

procedure TRemoteFitProgressTest.ARelayedReplyReachesTheTasksCurves;
begin
    AssertTrue('keep asking', RelayProgress(FTask, OutcomeWithSigma(2.75)));
    AssertEquals('the far side''s value is the task''s now', 2.75, SigmaOfTask,
        1e-9);
end;

procedure TRemoteFitProgressTest.AndTheEngineIsToldItImproved;
begin
    RelayProgress(FTask, OutcomeWithSigma(2.75));
    AssertEquals('once, through the funnel every engine uses', 1, FImprovements);
end;

procedure TRemoteFitProgressTest.APeerWithNothingYetIsAskedAgain;
begin
    //  The first poll can arrive before the far side's first improvement.
    AssertTrue('asked again',
        RelayProgress(FTask, '{"ok":true,"found":false,"curves":[]}'));
    AssertEquals('and nothing is claimed', 0, FImprovements);
end;

procedure TRemoteFitProgressTest.AnUnreadableReplyStopsTheAsking;
begin
    //  A proxy page, or a peer that answers something else at this path. Asking
    //  it several times a second for the rest of the fit would change nothing.
    AssertFalse('stop', RelayProgress(FTask, '<html>not here</html>'));
    AssertEquals(0, FImprovements);
end;

{ ---------------------------- while the fit is out -------------------------- }

procedure TRemoteFitProgressTest.TheFitsOwnReplyIsWhatIsReturned;
begin
    FRemote.PostReply := '{"errorCode":0,"rFactor":0.01}';
    FRemote.ProgressReply := '{"ok":true,"found":false,"curves":[]}';
    AssertEquals(FRemote.PostReply, PostRelayingProgress(FTask, @FRemote.Post,
        @FRemote.Get, 'http://h/fit', '{}', 'http://h/fit/progress?id=1', 5));
end;

procedure TRemoteFitProgressTest.ProgressIsRelayedWhileTheFitIsOut;
begin
    FRemote.PostReply := '{"errorCode":0,"rFactor":0.01}';
    FRemote.ProgressReply := OutcomeWithSigma(3.25);
    FRemote.HoldPostUntilPolls := 2;
    PostRelayingProgress(FTask, @FRemote.Post, @FRemote.Get, 'http://h/fit',
        '{}', 'http://h/fit/progress?id=1', 5);
    AssertTrue(Format('relayed before the fit returned (%d)', [FImprovements]),
        FImprovements >= 2);
    AssertEquals('into the task', 3.25, SigmaOfTask, 1e-9);
end;

procedure TRemoteFitProgressTest.EachPollAsksTheProgressRouteItWasGiven;
begin
    FRemote.HoldPostUntilPolls := 1;
    PostRelayingProgress(FTask, @FRemote.Post, @FRemote.Get, 'http://h/fit',
        '{}', 'http://h/fit/progress?id=abc', 5);
    AssertEquals('http://h/fit/progress?id=abc', FRemote.LastProgressUrl);
end;

procedure TRemoteFitProgressTest.AFitThatFailsRaisesWithItsMessage;
var
    Raised: string;
begin
    //  The failure crosses a thread. Lost there, the caller would read an empty
    //  reply as an unreadable one and report the wrong thing.
    FRemote.PostFailure := 'connection refused';
    Raised := '';
    try
        PostRelayingProgress(FTask, @FRemote.Post, @FRemote.Get, 'http://h/fit',
            '{}', 'http://h/fit/progress?id=1', 5);
    except
        on E: Exception do
            Raised := E.Message;
    end;
    AssertTrue('the message came through: ' + Raised,
        Pos('connection refused', Raised) > 0);
end;

procedure TRemoteFitProgressTest.APeerWithNoProgressRouteIsAskedOnceAndTheFitStillReturns;
begin
    //  AN OLDER PEER answers 404 here, which the HTTP client raises. That is a
    //  peer without the feature, not a failed fit: stop asking, and wait for the
    //  reply that was always going to come.
    FRemote.PostReply := '{"errorCode":0,"rFactor":0.01}';
    FRemote.ProgressFailure := 'HTTP 404';
    FRemote.HoldPostUntilPolls := 1;
    AssertEquals('the fit returned', FRemote.PostReply,
        PostRelayingProgress(FTask, @FRemote.Post, @FRemote.Get, 'http://h/fit',
            '{}', 'http://h/fit/progress?id=1', 5));
    AssertEquals('asked once', 1, FRemote.Polls);
end;

{ --------------------------------- addresses -------------------------------- }

procedure TRemoteFitProgressTest.TheProgressRouteHangsOffTheBackendsUrl;
begin
    AssertEquals('http://127.0.0.1:8788/fit/progress?id=abc',
        ProgressUrlFor('http://127.0.0.1:8788', 'abc'));
end;

procedure TRemoteFitProgressTest.EveryFitIsGivenItsOwnProgressId;
var
    A, B: string;
begin
    //  Two fits on one sidecar at once must not read each other's progress, and
    //  the id goes into a URL, so it carries nothing a query would mangle.
    A := NewProgressId;
    B := NewProgressId;
    AssertTrue('not empty', A <> '');
    AssertTrue('distinct', A <> B);
    AssertEquals('no braces', 0, Pos('{', A) + Pos('}', A));
    AssertEquals('nothing a query splits on', 0, Pos('&', A) + Pos('=', A) +
        Pos('?', A) + Pos(' ', A));
end;

{ ---------------------------------- the wire -------------------------------- }

procedure TProgressIdWireTest.AProblemCarriesItsProgressId;
var
    P, Q: TFitProblem;
begin
    P := Default(TFitProblem);
    P.ProgressId := 'abc-123';
    AssertTrue(FitProblemFromJson(FitProblemToJson(P), Q));
    AssertEquals('abc-123', Q.ProgressId);
end;

procedure TProgressIdWireTest.AProblemWithoutOneSendsNone;
begin
    //  ABSENT, so a problem sent to an older peer is byte-for-byte what it was.
    AssertEquals(0, Pos('progressId', FitProblemToJson(Default(TFitProblem))));
end;

procedure TProgressIdWireTest.AnOlderPeersProblemReadsAsHavingNone;
var
    Q: TFitProblem;
begin
    AssertTrue(FitProblemFromJson('{"op":"fit","profileX":[1],"profileY":[1]}',
        Q));
    AssertEquals('', Q.ProgressId);
end;

initialization
    //  A thread and a task, and nothing outside the process: no socket, no
    //  optimiser run to convergence.
    RegisterTest('unit', TRemoteFitProgressTest);
    RegisterTest('unit', TProgressIdWireTest);
end.
