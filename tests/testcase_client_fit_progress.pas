// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client shows while a fit runs, and what it asks the server.)

ENTERED WHERE THE APPLICATION ENTERS. The window's Fit commands call
MinimizeDifference, DoAllAutomatically and MinimizeNumberOfCurves; its timer
calls PollProgress; the completion handler is Done. Those are the calls made
here, against the real THttpFitService marshalling behind a mocked transport and
a viewer that records what it is asked to draw.

THE TWO RULES WORTH A SUITE.

  * THE CHART AREA IS NEVER EMPTY. The moment a fit is started the viewer is
    told so, before any request has gone out, and the data stays drawn under the
    header until there is something better to show.

  * WHILE A FIT RUNS, THE CLIENT ASKS ONLY WHAT IS ANSWERED WITHOUT WAITING. The
    fit holds its problem's lock, so any other read - the statistics, the curves,
    a module's resources - would park the window's own thread until the fit was
    over, which is the freeze this feature exists to end. Every request made
    during a fit is asserted to be a polled route.

HOW THE FIT IS HELD OPEN. RunAsync is overridden to record the operation rather
than run it, and to mark the client busy the way the real one does. The test
decides when the fit "returns" by running what was recorded.
}
unit testcase_client_fit_progress;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_client, int_fit_viewer, fit_progress, fit_progress_json, rfactor_text,
    mock_fit_viewer, mock_http_transport, rest_polling, title_points_set,
    named_points_set, curve_instance_id, int_module_overlay;

type
    { Holds an operation open until the test lets it finish. }
    THeldClient = class(TFitClient)
    private
        FOp: TServerOp;
        FDone: TThreadMethod;
    protected
        procedure RunAsync(AOp: TServerOp; ADone: TThreadMethod); override;
    public
        { The server call returns, and the completion runs - in that order, as
          the real thread does. }
        procedure FinishOperation;
        procedure GiveProfile(APoints: longint);
    end;

    { A service that records what the WINDOW had been shown by the time each
      request went out. The two mocks keep separate logs, so ordering between
      them cannot be read off either one - and the order is the assertion. }
    TWatchingService = class(TMockHttpService)
    private
        FView: TMockFitViewer;
    protected
        function Fetch(const AUrl: string; ATimeoutMs: integer): string; override;
    public
        { How many progress views the viewer had drawn when the first request
          was made. -1 until one is. }
        ViewsAtFirstRequest: longint;
        constructor Create(const ABaseUrl: string; AView: TMockFitViewer);
    end;

    { A viewer whose readouts poll again, as the window's do: they process
      messages, and the timer can fire inside them. }
    TReentrantViewer = class(TMockFitViewer, IFitViewer, IFitProgressView)
    public
        Client: TFitClient;
        procedure ShowTime;
    end;

    TClientFitProgressTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: THeldClient;
        function RequestsTo(const AFragment: string): longint;
        procedure ReplyProgress(const AJson: string);
        function TwoSamples(ANextSeq: longint = 2): string;
        function WithSnapshot: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The chart area is never empty.
        procedure MinimizingTheDifferenceShowsItsProgressAtOnce;
        procedure SoDoesTheAutomaticFit;
        procedure SoDoesMinimizingTheNumberOfCurves;
        procedure NothingIsAskedOfTheServerToShowIt;
        procedure AnimatingAsksTheEngineForTheFirstFrameBeforeTheFit;
        procedure ButTheViewIsUpBeforeEvenThatRequest;
        procedure AndItGoesOutBeforeTheFitItself;
        procedure AFailedAskDoesNotStopTheFit;
        procedure TheDataIsDrawnAgainUnderTheHeader;
        procedure AComputationThatIsNotAFitShowsNoProgress;

        //  Polling.
        procedure APollDrawsTheLossCurve;
        procedure TheNextPollAsksOnlyForWhatIsNew;
        procedure ASnapshotIsAskedForOnlyInAnimationMode;
        procedure APollOutsideAFitAsksNothing;
        procedure APollAfterTheFitReturnedAsksNothing;
        procedure APollInsideAPollIsNotMade;
        procedure AFailedPollIsSurvivedAndTheViewStays;
        procedure APreviousFitsReportIsNotDrawnAsThisOnes;
        procedure WithoutAnimationASnapshotIsNotDrawn;

        //  A computation that is not a fit.
        procedure AComputationThatIsNotAFitStillShowsItsClock;
        procedure EveryComputationStopsItsClockWhenItFinishes;

        //  The status bar while it runs.
        procedure TheReadoutsComeFromTheProgressNotTheStatistics;
        procedure OnceTheFitIsOverTheyComeFromTheServerAgain;

        //  Animation.
        procedure InAnimationASnapshotRedrawsTheComputedProfile;
        procedure AndEveryCurveOfTheModel;
        procedure UnderTheHandlesTheModelGivesThem;
        procedure WithoutTheLossChartCoveringThem;
        procedure AnAnimatedFrameFreezesTheGridsAsAnimationAlwaysDid;
        procedure InAnimationTheDataStaysOnTheChart;
        procedure AnAnimatedFrameAsksNoModuleOverlayToDraw;

        //  The one rule over the whole run.
        procedure EveryRequestDuringAFitIsAPolledRoute;

        //  Finishing.
        procedure DoneTakesTheProgressDownBeforeRedrawing;
        procedure AFitThatFailedStillTakesItDown;

        //  The HTTP service's half.
        procedure TheServiceAsksTheProgressRouteWithItsQuery;
        procedure AndReadsTheSamplesAndTheSnapshot;
        procedure AnUnreadableReplyIsAFaultNotAnEmptyReport;
    end;

implementation

const
    BASE = 'http://127.0.0.1:8787';

var
    { How often a module overlay was drawn. Module-level: the hook is a plain
      procedure pointer, and the registry it goes into is process-global. }
    OverlayDraws: longint;

procedure CountOverlayDraw(AClient: TObject);
begin
    Inc(OverlayDraws);
end;

{ ------------------------------- the harness -------------------------------- }

procedure THeldClient.RunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    //  BUSY, as the real one makes it: the polling decision reads this.
    FAsyncState := AsyncWorks;
    FOp := AOp;
    FDone := ADone;
end;

procedure THeldClient.FinishOperation;
begin
    if Assigned(FOp) then
        FOp;
    if Assigned(FDone) then
        FDone;
    FOp := nil;
    FDone := nil;
end;

procedure THeldClient.GiveProfile(APoints: longint);
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 1 to APoints do
        P.AddNewPoint(i, i);
    SetExpProfile(P);
end;

constructor TWatchingService.Create(const ABaseUrl: string;
    AView: TMockFitViewer);
begin
    inherited Create(ABaseUrl);
    FView := AView;
    ViewsAtFirstRequest := -1;
end;

function TWatchingService.Fetch(const AUrl: string; ATimeoutMs: integer): string;
begin
    if (ViewsAtFirstRequest < 0) and Assigned(FView) then
        ViewsAtFirstRequest := FView.ProgressShown;
    Result := inherited Fetch(AUrl, ATimeoutMs);
end;

procedure TReentrantViewer.ShowTime;
begin
    inherited ShowTime;
    if Assigned(Client) then
        Client.PollProgress;
end;

procedure TClientFitProgressTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := THeldClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    //  Both contracts, as TFitViewer.SetFitClient wires them.
    FClient.FProgressView := FView;
    //  The routes a completion handler reads.
    FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[1,2],"y":[1,2]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
end;

procedure TClientFitProgressTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

function TClientFitProgressTest.RequestsTo(const AFragment: string): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
        if Pos(AFragment, FSvc.Log.Calls[i]) > 0 then
            Inc(Result);
end;

procedure TClientFitProgressTest.ReplyProgress(const AJson: string);
begin
    FSvc.Reply('progress', AJson);
end;

function TClientFitProgressTest.TwoSamples(ANextSeq: longint): string;
begin
    Result := Format('{"ok":true,"busy":true,"elapsed":1.5,"nextSeq":%d,' +
        '"samples":[{"seq":%d,"t":0.5,"value":0.1},' +
        '{"seq":%d,"t":1.0,"value":0.01}]}',
        [ANextSeq, ANextSeq - 2, ANextSeq - 1]);
end;

function TClientFitProgressTest.WithSnapshot: string;
begin
    Result := '{"ok":true,"busy":true,"elapsed":1.5,"nextSeq":1,' +
        '"samples":[{"seq":0,"t":0.5,"value":0.1}],' +
        '"snapshot":{"seq":0,' +
        '"profile":{"title":"","x":[1,2,3],"y":[1,2,3]},' +
        '"delta":{"title":"","x":[1,2,3,4],"y":[0,0,0,0]},' +
        '"curves":[' +
        '{"id":"0B0E4B7C-0000-0000-0000-000000000001",' +
        '"points":{"title":"Gaussian","x":[1,2],"y":[1,2]}},' +
        '{"id":"0B0E4B7C-0000-0000-0000-000000000002",' +
        '"points":{"title":"Gaussian","x":[2,3],"y":[2,3]}}]}}';
end;

{ ------------------------ the chart area is never empty --------------------- }

procedure TClientFitProgressTest.MinimizingTheDifferenceShowsItsProgressAtOnce;
begin
    FClient.MinimizeDifference;
    AssertTrue('the viewer was told at once: ' + FView.Log.AsText,
        FView.ProgressShown > 0);
    AssertTrue('that the fit is starting', FView.LastProgress.Mode = pvStarting);
end;

procedure TClientFitProgressTest.SoDoesTheAutomaticFit;
begin
    FClient.DoAllAutomatically;
    AssertTrue(FView.ProgressShown > 0);
end;

procedure TClientFitProgressTest.SoDoesMinimizingTheNumberOfCurves;
begin
    FClient.MinimizeNumberOfCurves;
    AssertTrue(FView.ProgressShown > 0);
end;

procedure TClientFitProgressTest.NothingIsAskedOfTheServerToShowIt;
begin
    //  IMMEDIATE MEANS BEFORE ANY ROUND TRIP. The fit request is the one call
    //  a start makes; the progress view needs nothing from the server.
    //
    //  WITHOUT ANIMATION, which is the default: an animating client does make
    //  one call here, and the three tests below are about why.
    FClient.MinimizeDifference;
    AssertEquals('no progress request yet', 0, RequestsTo('progress'));
end;

procedure TClientFitProgressTest.AnimatingAsksTheEngineForTheFirstFrameBeforeTheFit;
begin
    //  THE ENGINE BUILDS A FRAME ONLY WHILE SOMEBODY IS ASKING, and it builds it
    //  when it next improves. An ordinary fit - a few curves - has done most of
    //  its improving within milliseconds, so a request that waits for the first
    //  poll arrives after there was anything to catch, and Animation Mode drew
    //  nothing at all for exactly the fits people run.
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    AssertEquals('asked once, for a frame: ' + FSvc.Log.AsText, 1,
        RequestsTo('progress?since=0&snapshot=1'));
end;

procedure TClientFitProgressTest.ButTheViewIsUpBeforeEvenThatRequest;
var
    Svc: TWatchingService;
    View: TMockFitViewer;
    Client: THeldClient;
begin
    //  IMMEDIATE MEANS IMMEDIATE. The first frame has to be asked for before
    //  the fit starts, and asking is a round trip - so if it is made first, an
    //  unreachable or slow server delays the one thing this feature promises
    //  above all others: that pressing Fit changes the screen at once. The
    //  order is view, then ask, then fit.
    View := TMockFitViewer.Create;
    Svc := TWatchingService.Create(BASE, View);
    Client := THeldClient.Create;
    try
        Client.FitService := Svc;
        Client.FFitViewer := View;
        Client.FProgressView := View;
        Client.AnimationMode := True;
        Client.MinimizeDifference;
        AssertTrue('a request was made at all', Svc.ViewsAtFirstRequest >= 0);
        AssertTrue('the view was up before it', Svc.ViewsAtFirstRequest > 0);
    finally
        Client.Free;
        View.Free;
        Svc.Free;
    end;
end;

procedure TClientFitProgressTest.AndItGoesOutBeforeTheFitItself;
var
    Log: string;
begin
    //  ORDER IS THE WHOLE POINT: asked after the fit had started, the engine
    //  would already have improved past the first frame.
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    //  The fit request itself goes out on the worker, which this harness runs
    //  when the test says so.
    FClient.FinishOperation;
    Log := FSvc.Log.Sequence;
    AssertTrue('the ask precedes the fit: ' + Log,
        (Pos('progress', Log) > 0) and
        (Pos('progress', Log) < Pos('minimize-difference', Log)));
end;

procedure TClientFitProgressTest.AFailedAskDoesNotStopTheFit;
begin
    //  A frame is a picture of the fit, never a condition of it.
    //
    //  The first fit is here to get the problem created, so that the failure
    //  planted below lands on the ask for a frame rather than on that.
    FClient.MinimizeDifference;
    FClient.FinishOperation;
    FSvc.Log.Clear;
    FClient.AnimationMode := True;
    FSvc.FailNextWith('connection reset');
    FClient.MinimizeDifference;
    FClient.FinishOperation;
    AssertEquals('the fit was still sent', 1,
        RequestsTo('/actions/minimize-difference'));
    AssertTrue('and the progress view is up', FView.ProgressShown > 0);
end;

procedure TClientFitProgressTest.TheDataIsDrawnAgainUnderTheHeader;
var
    Seq: string;
begin
    //  The chart is cleared of the previous model - that answer is out of date
    //  - but the DATA is not an answer, and it is what the user is fitting.
    FClient.GiveProfile(5);
    FView.Log.Clear;
    FClient.MinimizeDifference;
    Seq := FView.Log.Sequence;
    AssertTrue('cleared: ' + Seq, Pos('Clear', Seq) > 0);
    AssertTrue('then the data drawn again: ' + Seq,
        Pos('PlotExpProfile', Seq) > Pos('Clear', Seq));
    AssertEquals('all of it', 5, FView.PointsPlottedIn('PlotExpProfile'));
end;

procedure TClientFitProgressTest.AComputationThatIsNotAFitShowsNoProgress;
begin
    //  Proposing bounds or positions leaves the chart as it is and finishes
    //  quickly; a loss view would cover the very data it is proposing on.
    FClient.ComputeCurveBounds;
    AssertEquals(0, FView.ProgressShown);
end;

{ --------------------------------- polling ---------------------------------- }

procedure TClientFitProgressTest.APollDrawsTheLossCurve;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    AssertTrue('the loss chart', FView.LastProgress.Mode = pvLossChart);
    AssertTrue('shown', FView.LastProgress.LossChartVisible);
    AssertEquals('both samples', 2, Length(FView.LastProgress.Y));
    AssertEquals('logarithmically', -2.0, FView.LastProgress.Y[1], 1e-12);
end;

procedure TClientFitProgressTest.TheNextPollAsksOnlyForWhatIsNew;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples(7));
    FClient.PollProgress;
    FClient.PollProgress;
    AssertEquals('first from the beginning: ' + FSvc.Log.AsText, 1,
        RequestsTo('progress?since=0'));
    AssertEquals('then from where it left off', 1,
        RequestsTo('progress?since=7'));
end;

procedure TClientFitProgressTest.ASnapshotIsAskedForOnlyInAnimationMode;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    AssertEquals('not animating: ' + FSvc.Log.AsText, 1,
        RequestsTo('snapshot=0'));
    FClient.AnimationMode := True;
    FClient.PollProgress;
    AssertEquals('animating', 1, RequestsTo('snapshot=1'));
end;

procedure TClientFitProgressTest.APollOutsideAFitAsksNothing;
begin
    //  The window's timer may tick once after a fit, or before any.
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    AssertEquals(0, RequestsTo('progress'));
end;

procedure TClientFitProgressTest.APollAfterTheFitReturnedAsksNothing;
begin
    FClient.MinimizeDifference;
    FClient.FinishOperation;
    FSvc.Log.Clear;
    FClient.PollProgress;
    AssertEquals(0, RequestsTo('progress'));
end;

procedure TClientFitProgressTest.APollInsideAPollIsNotMade;
var
    Viewer: TReentrantViewer;
begin
    //  The window's readouts process messages, so the timer can fire while a
    //  poll is still drawing its answer. A second poll there would draw over
    //  the first and ask again for samples already on their way.
    Viewer := TReentrantViewer.Create;
    try
        Viewer.Client := FClient;
        FClient.FFitViewer := Viewer;
        FClient.FProgressView := Viewer;
        FClient.MinimizeDifference;
        ReplyProgress(TwoSamples);
        FSvc.Log.Clear;
        FClient.PollProgress;
        AssertEquals('one request: ' + FSvc.Log.AsText, 1,
            RequestsTo('progress'));
    finally
        FClient.FFitViewer := nil;
        FClient.FProgressView := nil;
        Viewer.Free;
    end;
end;

procedure TClientFitProgressTest.AFailedPollIsSurvivedAndTheViewStays;
begin
    //  A dropped poll is a frame not drawn, not a failed fit. The fit is still
    //  running; the next tick asks again.
    FClient.MinimizeDifference;
    FSvc.FailNextWith('connection reset');
    FClient.PollProgress;
    AssertEquals('not taken down', 0, FView.ProgressHidden);
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    AssertTrue('and the next one draws', FView.LastProgress.Mode = pvLossChart);
end;

procedure TClientFitProgressTest.APreviousFitsReportIsNotDrawnAsThisOnes;
begin
    //  The first poll can arrive before the request that starts this fit, and
    //  the server's log still holds the last fit - finished, so not busy.
    FClient.MinimizeDifference;
    ReplyProgress('{"ok":true,"busy":false,"elapsed":9,"nextSeq":2,' +
        '"samples":[{"seq":0,"t":1,"value":0.3},{"seq":1,"t":2,"value":0.2}]}');
    FClient.PollProgress;
    AssertTrue('still starting', FView.LastProgress.Mode = pvStarting);
    AssertEquals('and no R-factor claimed for this fit', '', FClient.GetRFactorStr);
end;

procedure TClientFitProgressTest.WithoutAnimationASnapshotIsNotDrawn;
begin
    //  Only an animating client asks for one, but a frame arriving anyway must
    //  not replace the loss chart with a model the user did not ask to watch.
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    AssertFalse('no model drawn: ' + FView.Log.AsText, FView.Plotted('PlotCurves'));
    AssertTrue('the loss is', FView.LastProgress.LossChartVisible);
end;

{ ---------------------- a computation that is not a fit --------------------- }

procedure TClientFitProgressTest.AComputationThatIsNotAFitStillShowsItsClock;
begin
    FClient.ComputeCurveBounds;
    ReplyProgress('{"ok":true,"busy":true,"elapsed":5,"nextSeq":0,"samples":[]}');
    FClient.PollProgress;
    AssertEquals('asked: ' + FSvc.Log.AsText, 1, RequestsTo('progress'));
    AssertEquals('no progress view over the data', 0, FView.ProgressShown);
    AssertTrue('the clock is shown', FView.Log.Saw('ShowTime'));
    AssertFalse('an R-factor is not - nothing is being fitted',
        FView.Log.Saw('ShowRFactor'));
    AssertEquals('the server''s clock', '0 day(s) 00:00:05', FClient.GetCalcTimeStr);
end;

procedure TClientFitProgressTest.EveryComputationStopsItsClockWhenItFinishes;

    procedure CheckStops(const AName: string);
    begin
        FClient.FinishOperation;
        FSvc.Log.Clear;
        FClient.PollProgress;
        AssertEquals(AName + ' still asked after it finished', 0,
            RequestsTo('progress'));
        //  THE CLOCK THAT WOULD BE LEFT BEHIND. A computation whose progress
        //  was not ended keeps answering the readout from its last poll, so the
        //  status bar would show the run's frozen clock instead of the server's
        //  own account of it.
        AssertEquals(AName + ': the time comes from the server once it is over',
            'from the server', FClient.GetCalcTimeStr);
    end;

begin
    FSvc.Reply('stats', '{"ok":true,"calcTime":"from the server"}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FSvc.Reply('positions', '{"title":"p","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-positions', '{"title":"q","x":[1,2],"y":[1,2]}');
    FClient.ComputeCurveBounds;
    CheckStops('bounds');
    FClient.ComputeBackgroundPoints;
    CheckStops('background');
    FClient.ComputeCurvePositions;
    CheckStops('positions');
    FClient.SelectAllPointsAsCurvePositions;
    CheckStops('all points as positions');
end;

{ ------------------------ the status bar while it runs ---------------------- }

procedure TClientFitProgressTest.TheReadoutsComeFromTheProgressNotTheStatistics;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    FSvc.Log.Clear;
    AssertEquals('the latest loss', RFactorText(0.01),
        FClient.GetRFactorStr);
    AssertEquals('the server''s clock', '0 day(s) 00:00:01',
        FClient.GetCalcTimeStr);
    AssertEquals('without asking for the statistics, which wait for the fit',
        0, RequestsTo('stats'));
end;

procedure TClientFitProgressTest.OnceTheFitIsOverTheyComeFromTheServerAgain;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    FClient.FinishOperation;
    FSvc.Reply('stats', '{"ok":true,"rFactor":"0.00500000","calcTime":"x"}');
    AssertEquals('the result', '0.00500000', FClient.GetRFactorStr);
end;

{ --------------------------------- animation -------------------------------- }

procedure TClientFitProgressTest.InAnimationASnapshotRedrawsTheComputedProfile;
begin
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    AssertEquals('the model as it stood: ' + FView.Log.AsText, 3,
        FView.PointsPlottedIn('PlotComputedProfile'));
    AssertEquals('and the difference', 4,
        FView.PointsPlottedIn('PlotDeltaProfile'));
end;

procedure TClientFitProgressTest.AndEveryCurveOfTheModel;
begin
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    AssertEquals('both curves', 2, FView.PointsPlottedIn('PlotCurves'));
end;

procedure TClientFitProgressTest.UnderTheHandlesTheModelGivesThem;
begin
    //  So a module's highlight and a deletion address the same curve in a
    //  frame as in the finished model.
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    AssertTrue('the curves were drawn', Assigned(FView.LastCurves));
    AssertEquals('the second curve''s handle',
        '0b0e4b7c-0000-0000-0000-000000000002',
        LowerCase(CurveInstanceIdToWire(
            TNamedPointsSet(FView.LastCurves.Items[1]).FInstanceId)));
end;

procedure TClientFitProgressTest.WithoutTheLossChartCoveringThem;
begin
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    AssertTrue('animated', FView.LastProgress.Mode = pvAnimatedCurves);
    AssertFalse('the model is what is shown',
        FView.LastProgress.LossChartVisible);
end;

procedure TClientFitProgressTest.AnAnimatedFrameFreezesTheGridsAsAnimationAlwaysDid;
begin
    //  A frame is not a result: the tables beside the chart keep the last
    //  finished one until the fit is over.
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FView.Log.Clear;
    FClient.PollProgress;
    AssertTrue('grids off: ' + FView.Log.AsText,
        Pos('SetUpdateGrids(False)', FView.Log.AsText) > 0);
end;

procedure TClientFitProgressTest.InAnimationTheDataStaysOnTheChart;
begin
    FClient.GiveProfile(5);
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    FView.Log.Clear;
    FClient.PollProgress;
    AssertFalse('the data was not taken down for a frame: ' + FView.Log.AsText,
        FView.Log.Saw('HideExpProfile'));
end;

procedure TClientFitProgressTest.AnAnimatedFrameAsksNoModuleOverlayToDraw;
var
    Before: longint;
begin
    //  A MODULE'S OVERLAY MAY READ THE MODEL through its own routes, which wait
    //  for the fit's lock - asked from a frame, it would freeze the window for the
    //  rest of the fit. It draws on the finished model instead.
    RegisterModuleOverlay(@CountOverlayDraw);
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    ReplyProgress(WithSnapshot);
    Before := OverlayDraws;
    FClient.PollProgress;
    AssertEquals('not from a frame', Before, OverlayDraws);
    FClient.FinishOperation;
    AssertTrue('but on the result', OverlayDraws > Before);
end;

{ --------------------------- the rule over the run -------------------------- }

procedure TClientFitProgressTest.EveryRequestDuringAFitIsAPolledRoute;
var
    i, Open, Space: longint;
    Line, Url: string;
begin
    //  A problem exists long before anyone presses Fit; creating one here keeps
    //  that request out of the window being watched.
    FSvc.AsyncOper;
    FClient.AnimationMode := True;
    FClient.MinimizeDifference;
    //  The request that starts the fit is the one exception; it IS the fit.
    FSvc.Log.Clear;
    ReplyProgress(WithSnapshot);
    FClient.PollProgress;
    FClient.PollProgress;
    FClient.GetRFactorStr;
    FClient.GetCalcTimeStr;
    AssertTrue('something was asked', FSvc.Log.Calls.Count > 0);
    for i := 0 to FSvc.Log.Calls.Count - 1 do
    begin
        //  Logged as METHOD(url body).
        Line := FSvc.Log.Calls[i];
        Open := Pos('(', Line);
        Url := Copy(Line, Open + 1, Length(Line) - Open - 1);
        Space := Pos(' ', Url);
        if Space > 0 then
            Url := Copy(Url, 1, Space - 1);
        AssertTrue('asked while the fit holds the lock, and not a polled ' +
            'route: ' + Line, IsPolledRoute(Url));
    end;
end;

{ --------------------------------- finishing -------------------------------- }

procedure TClientFitProgressTest.DoneTakesTheProgressDownBeforeRedrawing;
var
    Seq: string;
begin
    FClient.MinimizeDifference;
    ReplyProgress(TwoSamples);
    FClient.PollProgress;
    FView.Log.Clear;
    FClient.FinishOperation;
    Seq := FView.Log.Sequence;
    AssertEquals('taken down once', 1, FView.ProgressHidden);
    AssertTrue('before the result is drawn: ' + Seq,
        Pos('HideFitProgress', Seq) < Pos('PlotComputedProfile', Seq));
end;

procedure TClientFitProgressTest.AFitThatFailedStillTakesItDown;
begin
    //  The thread calls the completion however the operation ended; so must
    //  this, or a refused fit leaves the loss chart up over nothing.
    FSvc.FailNextSendWith('refused');
    FClient.MinimizeDifference;
    try
        FClient.FinishOperation;
    except
        //  The refusal itself is reported by the thread; here only the view.
        FClient.Done;
    end;
    AssertTrue('taken down', FView.ProgressHidden > 0);
end;

{ ------------------------------ the HTTP half ------------------------------- }

procedure TClientFitProgressTest.TheServiceAsksTheProgressRouteWithItsQuery;
begin
    ReplyProgress(TwoSamples);
    FSvc.GetFitProgress(4, True);
    AssertEquals('the route and its query: ' + FSvc.Log.AsText, 1,
        RequestsTo('/problems/1/progress?since=4&snapshot=1'));
end;

procedure TClientFitProgressTest.AndReadsTheSamplesAndTheSnapshot;
var
    R: TFitProgressReport;
begin
    ReplyProgress(WithSnapshot);
    R := FSvc.GetFitProgress(0, True);
    AssertTrue('busy', R.Busy);
    AssertEquals('the sample', 1, Length(R.Samples));
    AssertTrue('the snapshot', R.HasSnapshot);
    AssertEquals('its curves', 2, Length(R.Snapshot.Curves));
end;

procedure TClientFitProgressTest.AnUnreadableReplyIsAFaultNotAnEmptyReport;
var
    Raised: boolean;
begin
    //  Read as an empty report it would say "not busy", and a client that
    //  believed it would take the progress down while the fit went on.
    ReplyProgress('{"ok":true}');
    Raised := False;
    try
        FSvc.GetFitProgress(0, False);
    except
        Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

initialization
    //  No thread, no socket: RunAsync is held open and the transport mocked.
    RegisterTest('unit', TClientFitProgressTest);
end.
