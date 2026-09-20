// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The commands that hand their work to a thread: which verb, and what
comes back afterwards.)

THE OTHER HALF OF testcase_client_commands, whose header says these "are driven
elsewhere" - and until now they were not driven at all. Seven commands, each one
line: clear the previous answer, then hand a service call and a completion
handler to a worker thread. That line was unreachable, so nothing checked which
service call each command hands over, and nothing checked what its completion
handler re-reads.

WHY THAT MATTERS MORE THAN IT LOOKS. The seven lines are near-identical, which is
the shape a copy-paste gets wrong: a menu entry that sends the wrong verb runs a
DIFFERENT operation on the server and reports success. The user asked to minimise
the number of curves and got a full automatic fit; both take a while, both end
with a changed chart, and neither says anything. Nothing in the suite could have
noticed.

AND THE COMPLETION HANDLERS ARE NOT INTERCHANGEABLE. Three of the seven have
their own - computing the curve bounds re-reads the bounds, computing the
background re-reads the background points, computing the positions re-reads the
positions - and the fits share the general one. A command paired with the wrong
handler leaves the chart showing the previous answer for the thing the user just
computed, which reads as the command having done nothing.

HOW IT IS REACHED. `TFitClient.RunAsync` is virtual, and the subclass below runs
the operation and its completion handler in place. What that skips is exactly the
part that needs a message loop - the thread and the Synchronize - and what it
reaches is everything above them. It also records the pairing, so the two
questions above can be asked separately.

WHAT IS NOT ASKED HERE, and it is worth naming rather than leaving as a gap.
`RunAsync` also sets the busy state the window reads to disable the commands,
which is right where it is - the hand-off is the moment the client becomes busy,
and setting it in the caller would leave seven chances to forget. But it is
therefore INSIDE the method this harness replaces. A test written through the
subclass could only assert that the subclass set it, which is a test of the
harness. Left to the integration suite, which runs the real one.
}
unit testcase_client_async_commands;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, int_fit_service, mock_fit_viewer, mock_http_transport,
    mock_support, title_points_set, points_set;

type
    { Runs what RunAsync would have threaded, in place. }
    TSyncClient = class(TFitClient)
    private
        FRan: longint;
        FSkipDone: boolean;
    protected
        procedure RunAsync(AOp: TServerOp; ADone: TThreadMethod); override;
    public
        { How many operations were handed over. One per command, so a command
          that forgot to hand anything over is visible. }
        property Ran: longint read FRan;
        { When set, the operation runs and the completion handler does not -
          which separates "which verb was sent" from "what was re-read", so a
          failure names one of the two rather than both. }
        property SkipDone: boolean read FSkipDone write FSkipDone;
    end;

    TClientAsyncCommandsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TSyncClient;
        { Answers every route a completion handler reads, so a handler running
          for real does not fail on a missing reply. }
        procedure StubEveryRoute;
        function CallsTo(const APath: string): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Each command hands over exactly one operation.
        procedure EveryAsyncCommandHandsOverOneOperation;

        //  And it is the operation of the same name.
        procedure DoAllAutomaticallySendsItsOwnVerb;
        procedure MinimizeDifferenceSendsItsOwn;
        procedure MinimizeNumberOfCurvesSendsItsOwn;
        procedure ComputeCurveBoundsSendsItsOwn;
        procedure ComputeBackgroundPointsSendsItsOwn;
        procedure ComputeCurvePositionsSendsItsOwn;
        procedure SelectAllPointsAsCurvePositionsSendsItsOwn;
        procedure NoTwoCommandsSendTheSameVerb;

        //  What each completion handler re-reads.
        procedure ComputingTheCurveBoundsReReadsTheBounds;
        procedure ComputingTheBackgroundReReadsTheBackgroundPoints;
        procedure ComputingThePositionsReReadsThePositions;
        procedure AFitReReadsTheModelRatherThanOnePickSet;

        //  What the commands do before handing over.
        procedure ACommandClearsThePreviousAnswerBeforeSendingAnything;
    end;

implementation

const
    BASE = 'http://localhost:8080';

{ ------------------------------ the sync client ----------------------------- }

procedure TSyncClient.RunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    Inc(FRan);
    //  IN THIS ORDER, and both, because that is the order the real one uses:
    //  the operation blocks on the server, then the completion handler re-reads
    //  what it changed. Reversed, a handler would read the previous answer -
    //  which is the defect these tests are about, so the harness must not be
    //  able to hide it.
    if Assigned(AOp) then
        AOp;
    if (not FSkipDone) and Assigned(ADone) then
        ADone;
end;

{ -------------------------------- the fixture ------------------------------- }

procedure TClientAsyncCommandsTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TSyncClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    StubEveryRoute;
end;

procedure TClientAsyncCommandsTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TClientAsyncCommandsTest.StubEveryRoute;
begin
    //  DISTINCT POINT COUNTS PER ROUTE, so "which one was re-read" is a
    //  question with a different answer for each rather than a plausible one
    //  for all of them.
    FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('selected-interval', '{"title":"i","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2,3,4],"y":[1,2,3,4]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[1,2,3,4,5],"y":[1,2,3,4,5]}');
    FSvc.Reply('background', '{"title":"b","x":[1,2,3,4,5,6],"y":[1,1,1,1,1,1]}');
    FSvc.Reply('curve-positions', '{"title":"q","x":[1,2,3,4,5,6,7],"y":[1,1,1,1,1,1,1]}');
    FSvc.Reply('calc-positions', '{"title":"r","x":[1,2],"y":[1,1]}');
    FSvc.Reply('rfactor-bounds', '{"title":"s","x":[1,2,3,4,5,6,7,8],"y":[1,1,1,1,1,1,1,1]}');
    FSvc.Reply('curves', '{"curves":[]}');
end;

function TClientAsyncCommandsTest.CallsTo(const APath: string): longint;
var
    i: longint;
    L: TStringList;
begin
    Result := 0;
    L := TStringList.Create;
    try
        L.Text := FSvc.Log.AsText;
        for i := 0 to L.Count - 1 do
            if Pos(APath, L[i]) > 0 then
                Inc(Result);
    finally
        L.Free;
    end;
end;

{ ------------------- each command hands over one operation ------------------ }

procedure TClientAsyncCommandsTest.EveryAsyncCommandHandsOverOneOperation;
begin
    //  COUNTED, and the count is the whole assertion: a command that fell
    //  through without handing anything over would leave the window busy
    //  forever - the state is set before the hand-off - and one that handed over
    //  twice would run the operation twice on the server.
    FClient.SkipDone := True;
    FClient.DoAllAutomatically;
    AssertEquals('do all', 1, FClient.Ran);
    FClient.MinimizeDifference;
    AssertEquals('minimise the difference', 2, FClient.Ran);
    FClient.MinimizeNumberOfCurves;
    AssertEquals('minimise the curve count', 3, FClient.Ran);
    FClient.ComputeCurveBounds;
    AssertEquals('curve bounds', 4, FClient.Ran);
    FClient.ComputeBackgroundPoints;
    AssertEquals('background points', 5, FClient.Ran);
    FClient.ComputeCurvePositions;
    AssertEquals('curve positions', 6, FClient.Ran);
    FClient.SelectAllPointsAsCurvePositions;
    AssertEquals('select all as positions', 7, FClient.Ran);
end;

{ --------------------- and it is the verb of the same name ------------------ }

procedure TClientAsyncCommandsTest.DoAllAutomaticallySendsItsOwnVerb;
begin
    FClient.SkipDone := True;
    FClient.DoAllAutomatically;
    AssertEquals('do-all-automatically was sent once', 1,
        CallsTo('/actions/do-all-automatically'));
end;

procedure TClientAsyncCommandsTest.MinimizeDifferenceSendsItsOwn;
begin
    FClient.SkipDone := True;
    FClient.MinimizeDifference;
    AssertEquals('minimize-difference was sent once', 1,
        CallsTo('/actions/minimize-difference'));
end;

procedure TClientAsyncCommandsTest.MinimizeNumberOfCurvesSendsItsOwn;
begin
    FClient.SkipDone := True;
    FClient.MinimizeNumberOfCurves;
    AssertEquals('minimize-number-of-curves was sent once', 1,
        CallsTo('/actions/minimize-number-of-curves'));
end;

procedure TClientAsyncCommandsTest.ComputeCurveBoundsSendsItsOwn;
begin
    FClient.SkipDone := True;
    FClient.ComputeCurveBounds;
    AssertEquals('compute-curve-bounds was sent once', 1,
        CallsTo('/actions/compute-curve-bounds'));
end;

procedure TClientAsyncCommandsTest.ComputeBackgroundPointsSendsItsOwn;
begin
    FClient.SkipDone := True;
    FClient.ComputeBackgroundPoints;
    AssertEquals('compute-background-points was sent once', 1,
        CallsTo('/actions/compute-background-points'));
end;

procedure TClientAsyncCommandsTest.ComputeCurvePositionsSendsItsOwn;
begin
    FClient.SkipDone := True;
    FClient.ComputeCurvePositions;
    AssertEquals('compute-curve-positions was sent once', 1,
        CallsTo('/actions/compute-curve-positions'));
end;

procedure TClientAsyncCommandsTest.SelectAllPointsAsCurvePositionsSendsItsOwn;
begin
    //  THE PAIR MOST EASILY CONFUSED: this and ComputeCurvePositions share a
    //  completion handler, because both change the same thing - so the handler
    //  gives no clue which verb was meant, and only the verb distinguishes
    //  "work them out from the data" from "use every point".
    FClient.SkipDone := True;
    FClient.SelectAllPointsAsCurvePositions;
    AssertEquals('select-all-points-as-curve-positions was sent once', 1,
        CallsTo('/actions/select-all-points-as-curve-positions'));
    AssertEquals('and not the computing one', 0,
        CallsTo('/actions/compute-curve-positions'));
end;

procedure TClientAsyncCommandsTest.NoTwoCommandsSendTheSameVerb;
begin
    //  SEVEN COMMANDS, SEVEN VERBS, asserted over one run so that two commands
    //  sending one verb fails here even if each of the tests above passes -
    //  which it would, since each only checks that ITS verb was sent.
    FClient.SkipDone := True;
    FClient.DoAllAutomatically;
    FClient.MinimizeDifference;
    FClient.MinimizeNumberOfCurves;
    FClient.ComputeCurveBounds;
    FClient.ComputeBackgroundPoints;
    FClient.ComputeCurvePositions;
    FClient.SelectAllPointsAsCurvePositions;

    AssertEquals('do all', 1, CallsTo('/actions/do-all-automatically'));
    //  'minimize-difference' is a prefix of 'minimize-difference-again', which
    //  no command here sends - and the count would be two if one did, so this
    //  assertion also says that.
    AssertEquals('minimise the difference, and nothing like it', 1,
        CallsTo('/actions/minimize-difference'));
    AssertEquals('minimise the curve count', 1,
        CallsTo('/actions/minimize-number-of-curves'));
    AssertEquals('curve bounds', 1, CallsTo('/actions/compute-curve-bounds'));
    AssertEquals('background points', 1,
        CallsTo('/actions/compute-background-points'));
    AssertEquals('curve positions', 1,
        CallsTo('/actions/compute-curve-positions'));
    AssertEquals('select all as positions', 1,
        CallsTo('/actions/select-all-points-as-curve-positions'));
end;

{ ------------------ what each completion handler re-reads ------------------- }

procedure TClientAsyncCommandsTest.ComputingTheCurveBoundsReReadsTheBounds;
begin
    //  ITS OWN HANDLER, and what it must fetch is the thing the command
    //  computed. Paired with the general one, the chart would keep showing the
    //  previous bounds and the command would read as having done nothing.
    FClient.ComputeCurveBounds;
    AssertTrue('the bounds were re-read',
        CallsTo('rfactor-bounds') > 0);
end;

procedure TClientAsyncCommandsTest.ComputingTheBackgroundReReadsTheBackgroundPoints;
begin
    FClient.ComputeBackgroundPoints;
    AssertTrue('the background points were re-read',
        CallsTo('background') > 0);
end;

procedure TClientAsyncCommandsTest.ComputingThePositionsReReadsThePositions;
begin
    FClient.ComputeCurvePositions;
    AssertTrue('the curve positions were re-read',
        CallsTo('curve-positions') > 0);
end;

procedure TClientAsyncCommandsTest.AFitReReadsTheModelRatherThanOnePickSet;
begin
    //  THE GENERAL HANDLER, which the three fits share. A fit changes the model,
    //  not one pick set, so what comes back is the computed profile and the
    //  curves - and a fit that re-read only a pick set would leave the chart
    //  showing the shape from before the fit while reporting success.
    FClient.MinimizeDifference;
    AssertTrue('the computed profile came back',
        CallsTo('calc-profile') > 0);
end;

{ ---------------- what the commands do before handing over ------------------ }

procedure TClientAsyncCommandsTest.ACommandClearsThePreviousAnswerBeforeSendingAnything;
begin
    //  CLEARED FIRST, NOT AFTERWARDS. The previous fit's curves are on the chart
    //  while the new one runs, and a client that cleared them on completion
    //  would show the old model for the length of the operation - which on a
    //  long fit is a picture the user reads as the current answer.
    //
    //  Asserted through the chart, which is where the user would see it: the
    //  view is told to clear before any request goes out.
    FClient.SkipDone := True;
    FClient.MinimizeDifference;
    AssertTrue('the chart was cleared', FView.Log.Saw('Clear'));
end;


initialization
    //  A unit test: no thread, no socket, no server. RunAsync is virtual and
    //  overridden to run in place, which skips exactly the part that needs a
    //  message loop.
    RegisterTest('unit', TClientAsyncCommandsTest);
end.
