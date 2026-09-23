// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The stateless fit, and the progress it publishes while it runs.)

POST /fit is how a compute server is used as a REMOTE BACKEND: another server
sends it a whole problem and waits for the answer. That wait used to be silent,
so a fit routed to a remote server showed nothing until it returned. Given a
progressId, the fit now publishes what it has reached, and GET /fit/progress
answers with it while the POST is still out - the same contract the Python
sidecar keeps, so the backend relaying it cannot tell the two apart.

MOVED OUT OF fit_server.lpr, where it could only be reached by starting the
process. The route predicates and the store are plain unit tests; running a fit
to convergence is the integration half.
}
unit testcase_stateless_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson, jsonparser,
    fit_problem_json, gauss_points_set, SimpMath, stateless_fit,
    fit_task, fit_task_marshalling, curve_types_singleton,
    int_curve_type_selector;

type
    { Counts what the fit published, and keeps it. }
    TCountingStore = class(TFitProgressStore)
    public
        Publishes: longint;
        procedure Publish(const AId, AOutcomeJson: string); override;
    end;

    TStatelessFitProgressTest = class(TTestCase)
    private
        FStore: TFitProgressStore;
        function Reply(const AUri: string): TJSONObject;
        { A task built from a problem and never fitted. Enough for the hook,
          which reads whatever the task holds at the moment it is asked - which
          during a real fit is the best parameters so far, and here is the
          seeded ones. }
        function AnUnfittedTask: TFitTask;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheProgressRouteIsAGetOfFitProgress;
        procedure AQueryDoesNotHideIt;
        procedure NothingElseIsIt;
        procedure AnUnknownIdHasNothingYet;
        procedure AReplyWithNoIdHasNothing;
        procedure APublishedOutcomeIsWhatTheRouteAnswers;
        procedure ALaterPublishReplacesTheEarlier;
        procedure ADiscardedIdHasNothing;
        procedure OneFitsProgressIsNotAnothers;
        procedure AMalformedProblemIsRefused;

        //  The hook the fit hangs on, without a fit: it is a throttle over a
        //  task, and neither half of that needs the optimiser to run.
        procedure TheHookPublishesTheTasksOutcomeUnderItsId;
        procedure AndNotAgainWithinTheThrottle;
    end;

    TStatelessFitTest = class(TTestCase)
    private
        FStore: TCountingStore;
        function GaussianProblem(const AProgressId: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFitGivenAProgressIdPublishesAsItGoes;
        procedure AndForgetsItWhenItReturns;
        procedure AFitWithoutOnePublishesNothingAndStillFits;
    end;

implementation

procedure TCountingStore.Publish(const AId, AOutcomeJson: string);
begin
    Inc(Publishes);
    inherited Publish(AId, AOutcomeJson);
end;

{ ------------------------------ route and store ----------------------------- }

procedure TStatelessFitProgressTest.SetUp;
begin
    FStore := TFitProgressStore.Create;
end;

procedure TStatelessFitProgressTest.TearDown;
begin
    FreeAndNil(FStore);
end;

function TStatelessFitProgressTest.Reply(const AUri: string): TJSONObject;
var
    D: TJSONData;
begin
    D := GetJSON(StatelessFitProgressReply(FStore, AUri));
    AssertTrue('an object', D is TJSONObject);
    Result := TJSONObject(D);
end;

procedure TStatelessFitProgressTest.TheProgressRouteIsAGetOfFitProgress;
begin
    AssertTrue(IsStatelessFitProgressRoute('GET', '/fit/progress'));
end;

procedure TStatelessFitProgressTest.AQueryDoesNotHideIt;
begin
    AssertTrue(IsStatelessFitProgressRoute('GET', '/fit/progress?id=abc'));
end;

procedure TStatelessFitProgressTest.NothingElseIsIt;
begin
    AssertFalse('the fit itself', IsStatelessFitProgressRoute('POST', '/fit'));
    AssertFalse('a write', IsStatelessFitProgressRoute('POST', '/fit/progress'));
    AssertFalse('a problem''s progress',
        IsStatelessFitProgressRoute('GET', '/problems/1/progress'));
end;

procedure TStatelessFitProgressTest.AnUnknownIdHasNothingYet;
var
    R: TJSONObject;
begin
    //  NOT a 404: the relay's first poll usually arrives before the first
    //  improvement, and a 404 is how an older peer without the route answers -
    //  which makes the relay stop asking.
    R := Reply('/fit/progress?id=nobody');
    try
        AssertTrue('answered', R.Get('ok', False));
        AssertFalse('nothing found', R.Get('found', True));
        AssertEquals('no curves', 0, R.Arrays['curves'].Count);
    finally
        R.Free;
    end;
end;

procedure TStatelessFitProgressTest.AReplyWithNoIdHasNothing;
var
    R: TJSONObject;
begin
    FStore.Publish('', '{"errorCode":0,"rFactor":1,"curves":[{"params":[]}]}');
    R := Reply('/fit/progress');
    try
        AssertFalse(R.Get('found', True));
    finally
        R.Free;
    end;
end;

procedure TStatelessFitProgressTest.APublishedOutcomeIsWhatTheRouteAnswers;
begin
    FStore.Publish('a1', '{"errorCode":0,"rFactor":0.5,"curves":[]}');
    AssertEquals('{"errorCode":0,"rFactor":0.5,"curves":[]}',
        StatelessFitProgressReply(FStore, '/fit/progress?id=a1'));
end;

procedure TStatelessFitProgressTest.ALaterPublishReplacesTheEarlier;
begin
    FStore.Publish('a1', '{"rFactor":0.5}');
    FStore.Publish('a1', '{"rFactor":0.25}');
    AssertEquals('{"rFactor":0.25}',
        StatelessFitProgressReply(FStore, '/fit/progress?id=a1'));
end;

procedure TStatelessFitProgressTest.ADiscardedIdHasNothing;
var
    Json: string;
begin
    FStore.Publish('a1', '{"rFactor":0.5}');
    FStore.Discard('a1');
    AssertFalse(FStore.Latest('a1', Json));
end;

procedure TStatelessFitProgressTest.OneFitsProgressIsNotAnothers;
var
    Json: string;
begin
    FStore.Publish('a1', '{"rFactor":0.5}');
    FStore.Publish('b2', '{"rFactor":0.9}');
    AssertTrue(FStore.Latest('a1', Json));
    AssertEquals('{"rFactor":0.5}', Json);
end;

procedure TStatelessFitProgressTest.AMalformedProblemIsRefused;
begin
    AssertTrue(Pos('"ok" : false', RunStatelessFit('not json', FStore)) +
        Pos('"ok":false', RunStatelessFit('not json', FStore)) > 0);
end;

{ ----------------------------------- a fit ---------------------------------- }

procedure TStatelessFitTest.SetUp;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    FStore := TCountingStore.Create;
end;

procedure TStatelessFitTest.TearDown;
begin
    FreeAndNil(FStore);
end;

function TStatelessFitTest.GaussianProblem(const AProgressId: string): string;
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
    P.PositionsX := TDoubleArray.Create(9.5);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 9.5));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    P.MaxRFactor := 0.0001;
    P.ProgressId := AProgressId;
    Result := FitProblemToJson(P);
end;

procedure TStatelessFitTest.AFitGivenAProgressIdPublishesAsItGoes;
begin
    RunStatelessFit(GaussianProblem('fit-1'), FStore);
    AssertTrue('it published', FStore.Publishes > 0);
end;

procedure TStatelessFitTest.AndForgetsItWhenItReturns;
var
    Json: string;
begin
    //  Kept, it would be a leak per fit for as long as the server runs.
    RunStatelessFit(GaussianProblem('fit-1'), FStore);
    AssertFalse(FStore.Latest('fit-1', Json));
end;

procedure TStatelessFitTest.AFitWithoutOnePublishesNothingAndStillFits;
var
    O: TFitOutcome;
begin
    AssertTrue(FitOutcomeFromJson(RunStatelessFit(GaussianProblem(''), FStore), O));
    AssertEquals('nothing published', 0, FStore.Publishes);
    AssertTrue('a real fit', O.RFactor > 0);
end;


function TStatelessFitProgressTest.AnUnfittedTask: TFitTask;
var
    P: TFitProblem;
    Selector: ICurveTypeSelector;
    x: double;
    n: integer;
begin
    //  Asked for by name: the curve type is a process-wide selection, and a
    //  suite that ran before this one must not decide what is built here.
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);

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
    P.PositionsX := TDoubleArray.Create(9.5);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 9.5));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    P.MaxRFactor := 0.0001;
    Result := BuildTaskFromProblem(P);
end;

procedure TStatelessFitProgressTest.TheHookPublishesTheTasksOutcomeUnderItsId;
var
    Task: TFitTask;
    Hook: TStatelessProgressHook;
    R: TJSONObject;
begin
    //  WHAT A REMOTE PEER SERVES WHILE IT FITS. The hook is what the fit calls
    //  on every improvement; what it publishes has to be the outcome shape,
    //  because the asking side parses it with the same reader the final reply
    //  goes through.
    Task := AnUnfittedTask;
    Hook := TStatelessProgressHook.Create(Task, FStore, 'fit-7');
    try
        Hook.Publish;
        R := Reply(FIT_PROGRESS_ROUTE + '?id=fit-7');
        try
            //  THE OUTCOME SHAPE ITSELF, which is what makes this worth
            //  publishing: the asking side parses it with the same reader the
            //  final reply goes through. The empty answer for an id nobody has
            //  published carries no R-factor, so this tells the two apart.
            AssertTrue('an outcome rather than the empty answer: ' + R.AsJSON,
                R.Find('rFactor') <> nil);
            AssertTrue('with the model in it: ' + R.AsJSON,
                R.Find('curves') <> nil);
        finally
            R.Free;
        end;
    finally
        Hook.Free;
        Task.Free;
    end;
end;

procedure TStatelessFitProgressTest.AndNotAgainWithinTheThrottle;
var
    Task: TFitTask;
    Hook: TStatelessProgressHook;
    Counting: TCountingStore;
begin
    //  A FIT IMPROVES FAR FASTER THAN ANYBODY CAN WATCH. Publishing on every
    //  improvement would spend the remote engine's time serialising a model
    //  nobody asked for; the throttle is what makes reporting affordable.
    Counting := TCountingStore.Create;
    Task := AnUnfittedTask;
    Hook := TStatelessProgressHook.Create(Task, Counting, 'fit-8');
    try
        Hook.Publish;
        Hook.Publish;
        Hook.Publish;
        AssertEquals('published once, the rest inside the interval', 1,
            Counting.Publishes);
    finally
        Hook.Free;
        Task.Free;
        Counting.Free;
    end;
end;

initialization
    RegisterTest('unit', TStatelessFitProgressTest);
    //  Runs the optimiser to convergence.
    RegisterTest('integration', TStatelessFitTest);
end.
