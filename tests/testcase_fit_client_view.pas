// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client asks the chart to draw, and when.)

fit_client.pas is 793 lines of decisions about what belongs on the chart: which
series to plot, which to hide, when a refresh is needed, what to say in the hint.
It sat at 18 % because reaching those decisions needed an IFitViewer, and the only
implementation was TFitViewer - which needs TAGraph, a form and a widget set, and
therefore cannot run headlessly.

int_fit_viewer declares that interface precisely so the logic can be driven
without a chart, and nothing had ever implemented it for a test. This does; see
tests/mocks/mock_fit_viewer.

WHAT IS ASSERTED. Which series were asked for and with how many points - never
pixels. The client decides what to draw; the view decides how, and the view is
excluded from the coverage target for exactly that reason.

The service is mocked too (mock_http_transport), so nothing here opens a socket.
}
unit testcase_fit_client_view;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, fit_client_stub, fit_server_proxy, MyExceptions, checks,
    int_client_callback, mscr_specimen_list, named_points_set, gauss_points_set,
    curve_instance_id,
    mock_fit_viewer, mock_http_transport,
    title_points_set, points_set, neutron_points_set;

type
    { OPENS TWO PROTECTED METHODS, and nothing else. The client's redraw entry
      points are protected because only the window and the client's own code call
      them; a descendant is how a test reaches them without widening the class
      for everyone. Behaviour is the real one - these forward straight to the
      bodies under test. }
    TClientForTest = class(TFitClient)
    public
        procedure CallRefresh;
        procedure CallRefreshPointsSet(AToRefresh: TNeutronPointsSet);
        { Puts a profile into the client, as loading a file does. Protected on
          the class because only the loader path calls it; the client takes
          ownership of the set. }
        procedure CallSetExpProfile(AProfile: TTitlePointsSet);
    end;

type
    TFitClientViewTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TFitClient;
        { Where AsyncFinished counts to. A pointer because the handler has to be a
          method of this class to match TAsyncOperationFinished. }
        FFinishedCount: PLongint;
        procedure AsyncFinished(Sender: TObject);
        { Stubs the four routes UpdateComputedData reads, with distinct point
          counts so a series drawn from the wrong set is visible. }
        procedure StubComputedRoutes;
        { Answers /curves with ACount curves that have handles and parameters,
          and every /curves/<id>/points with one two-point curve.

          THE ROUTES THE REST OF THIS FIXTURE LEAVES EMPTY. StubComputedRoutes
          replies to /curves with an empty array, so until this existed no test
          had ever refreshed the client with a model in it - and the reading of a
          curve, which is a request per curve BY HANDLE plus a second pass for
          the parameters, was reachable only by running the application. }
        procedure StubCurves(ACount: longint);
        { How many requests reached /settings so far. The two menu flags are read
          from there, and how OFTEN they are read is the invariant. }
        function SettingsReads: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure RefreshingComputedDataPlotsEverySeries;
        procedure EachSeriesGetsItsOwnPointsNotItsNeighboursSet;
        procedure AnEmptyComputedProfileIsNotPlotted;
        procedure TheGridsAndLegendsAreTurnedOffWhileUpdating;
        procedure ExtraDataOffStillPlotsTheComputedProfile;
        procedure RefreshingTwiceReplacesRatherThanAccumulates;
        procedure TheWaveLengthReachesTheSeries;

        //  A refresh with a MODEL in it. Everything above stubs /curves with an
        //  empty array, which exercises none of the reading: an empty list
        //  cannot show a curve paired with the wrong parameters, a handle
        //  dropped, or a count that disagrees with itself.
        procedure EveryCurveInTheModelReachesTheView;
        procedure EachCurveKeepsItsOwnPoints;
        procedure TheParametersArriveOnePerCurve;
        procedure EachCurveCarriesItsOwnHandle;
        procedure ACurveWithNoHandleIsReportedRatherThanDropped;
        procedure ARefreshedModelReplacesTheOldCurves;
        procedure TheWaveLengthReachesEveryCurve;

        procedure AHintIsShownToTheUser;
        procedure NoViewerAttachedIsNotAFailure;

        //  Redrawing on demand, which is what every edit ends with. Four
        //  branches over two three-line methods, and not one of them had run.
        procedure ARefreshReachesTheViewer;
        procedure WithNoViewerARefreshIsSilent;
        procedure RefreshingOneSetReachesTheViewer;
        procedure RefreshingNothingIsNotARefresh;

        //  The wavelength, which every set the client holds has to carry: the
        //  axis is computed from it per point, so one set left behind draws in
        //  different units from its neighbours on the same chart.
        procedure TheWavelengthReachesTheProfile;
        procedure AndEverySetTheServerFilled;
        procedure AProfilePointCanBeMovedOnceThereIsAProfile;

        //  The engine's callbacks. TFitClient implements IClientCallback, so these
        //  are the methods a running fit calls back into - and each is a
        //  hide-then-replace-then-plot sequence whose ORDER matters.
        procedure ComputedBoundsAreHiddenBeforeBeingReplaced;
        procedure ComputedBoundsCarryTheirName;
        procedure AnEmptyComputedResultIsNotPlotted;
        procedure ComputedBackgroundReplacesTheOldSet;
        procedure ComputedPositionsReplaceBothSeries;
        procedure ThePositionsTableShowsThePicksWhenThereAreAny;
        procedure WithNoPicksItShowsWhereTheModelsCurvesActuallySit;
        procedure WithNeitherNothingIsPutInTheTable;
        procedure EveryCallbackReportsTheOperationFinished;
        procedure DoneRefreshesTheProfileAndTheComputedData;
        procedure TheCurrentSetFollowsTheSelectionMode;
        procedure SelectingTheEntireProfileClearsTheInterval;

        //  The stub the engine is handed instead of the client itself.
        procedure TheStubForwardsEveryCallbackToTheClient;
        procedure TheStubIsTheSameContractAsTheClient;

        //  The settings the client passes through to the engine. Twenty
        //  methods of one shape, and the failure they are prone to is a setter
        //  wired to the wrong getter - a setting that will not stick.
        procedure EverySettingReachesTheServerAndComesBack;
        procedure TheSettingsDoNotShareOneField;
        procedure EverySettingWritesItsOwnField;
        procedure TheMinimizerAndObjectiveAreDistinct;
        procedure TheWeightingRoundTrips;
        procedure TheWeightingHasAStatisticallyCorrectDefault;
        procedure TheServerUrlRoundTrips;
        procedure TheTwoFlagsAreNotEachOther;
        //  How often those two are read. They tick two menu entries, and an
        //  action's Update handler runs on the LCL idle loop - so reading them
        //  per use is a round trip per idle tick, and one during FormCreate
        //  before any server need exist.
        procedure TheFlagsAreReadOncePerServerNotPerUse;
        procedure SettingAFlagNeedsNoFurtherRead;
        procedure PointingAtAnotherServerReReadsThem;
        procedure PointingAtTheSameServerAgainDoesNot;
        procedure AnUnreachableServerLeavesTheMenuAnswerable;
        procedure BothFlagsAreOffWhenTheServerSaysNothing;
        procedure TheWaveLengthIsHeldByTheClient;
        procedure TheWaveLengthReachesTheProfileAlreadyHeld;

        //  What the status bar reads while a fit runs.
        procedure TheProgressReadoutsComeFromTheServer;
        procedure NothingIsRunningToBeginWith;
        procedure AnOperationInProgressIsReported;

        //  Putting things away, which every refresh does before it redraws.
        procedure RemovingASetHidesItFirst;
        procedure RemovingASetThatIsNotThereIsNotAFailure;
        procedure EveryRemovalHidesItsOwnSet;
        procedure ClearingTakesEverythingOffTheChart;
        procedure RefreshingWithNoViewerIsNotAFailure;

        //  The model the client holds.
        procedure SelectingACurveTypeTellsTheServer;
        procedure TheSelectedCurveTypeIsReadBack;

        //  A module's own picking, and the user-defined curve.
        procedure AModuleGestureNamesItsOwnSet;
        procedure LeavingAModuleGestureKeepsItsSetName;
        procedure TheUserCurveFormulaIsSentToTheServer;
        procedure ForgettingTheUserCurveIsSentToo;

        //  What a click on the chart does to a set.
        procedure APickIsAddedToTheBackground;
        procedure APickGoesToTheSetTheModeIsCollecting;
        procedure APickIsMovedRatherThanDuplicated;

        //  The proxy the engine reports through when the client is behind it.
        procedure TheProxyForwardsEveryCallbackToTheStub;
        procedure AProxyWithNoStubRefusesEveryCallback;
        procedure AndWithAStubItPassesEveryCallbackThrough;
    end;

implementation

procedure TClientForTest.CallRefresh;
begin
    Refresh;
end;

procedure TClientForTest.CallRefreshPointsSet(AToRefresh: TNeutronPointsSet);
begin
    RefreshPointsSet(AToRefresh);
end;

procedure TClientForTest.CallSetExpProfile(AProfile: TTitlePointsSet);
begin
    SetExpProfile(AProfile);
end;

const
    BASE = 'http://127.0.0.1:8787';

procedure TFitClientViewTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TClientForTest.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FFinishedCount := nil;
end;

procedure TFitClientViewTest.TearDown;
begin
    //  THE CLIENT FIRST. It is a TInterfacedObject holding the service and the
    //  viewer through interface references, and -SIcorba counts none of them - so
    //  freeing a mock while the client still points at it leaves a vtable pointer
    //  into reclaimed memory. See mock_support.
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TFitClientViewTest.AsyncFinished(Sender: TObject);
begin
    if Assigned(FFinishedCount) then
        Inc(FFinishedCount^);
end;

procedure TFitClientViewTest.StubComputedRoutes;
begin
    //  DISTINCT POINT COUNTS PER ROUTE. The failure this guards against is a
    //  series plotted from the wrong set - which looks like a plot rather than an
    //  error - and identical counts would hide it completely.
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    FSvc.Reply('rfactor-bounds',
        '{"title":"r","x":[1,2,3,4],"y":[1,2,3,4]}');
end;

procedure TFitClientViewTest.StubCurves(ACount: longint);
var
    Body: string;
    i: longint;
begin
    Body := '{"ok":true,"curves":[';
    for i := 0 to ACount - 1 do
    begin
        if i > 0 then
            Body := Body + ',';
        //  A DISTINCT HANDLE PER CURVE, and a parameter whose value is the
        //  curve's index: the failure worth catching is one curve shown under
        //  another's parameters, and identical values would hide it exactly as
        //  identical point counts would hide a series drawn from the wrong set.
        Body := Body + Format(
            '{"id":"0000000%d-0000-4000-8000-000000000000",' +
            '"params":[{"name":"A","value":%d,"type":1,"error":-1}]}',
            [i + 1, i]);
    end;
    FSvc.Reply('curves', Body + ']}');
    //  Matched by LAST path segment, so this one reply answers
    //  /curves/<id>/points for every handle - see mock_http_transport.
    FSvc.Reply('points', '{"title":"Gauss","x":[1,2],"y":[3,4]}');
end;

procedure TFitClientViewTest.EveryCurveInTheModelReachesTheView;
begin
    StubComputedRoutes;
    StubCurves(3);
    FClient.UpdateComputedData(True);
    AssertEquals('all three curves were handed over: ' + FView.Log.AsText,
        3, FView.PointsPlottedIn('PlotCurves'));
end;

procedure TFitClientViewTest.EachCurveKeepsItsOwnPoints;
begin
    //  One request per curve, so a curve whose points were never fetched arrives
    //  empty - and an empty series is drawn as nothing, which on a chart reads as
    //  a curve the fit did not find rather than as a fault.
    StubComputedRoutes;
    StubCurves(2);
    FClient.UpdateComputedData(True);
    AssertEquals('two curves', 2, FView.PointsPlottedIn('PlotCurves'));
    AssertEquals('and each with its two points', 2,
        TNamedPointsSet(FView.LastCurves.Items[1]).PointsCount);
end;

procedure TFitClientViewTest.TheParametersArriveOnePerCurve;
begin
    //  The points and the parameters come from two separate reads and are paired
    //  BY POSITION - the pairing the wire itself uses. A list of a different
    //  length is that pairing broken, and it shows the user one curve's numbers
    //  under another curve's heading.
    StubComputedRoutes;
    StubCurves(4);
    FClient.UpdateComputedData(True);
    AssertEquals('as many parameter sets as curves',
        4, FView.LastCurveAttributes.Count);
end;

procedure TFitClientViewTest.EachCurveCarriesItsOwnHandle;
begin
    //  The handle is how the view addresses a curve back - it is not a parameter
    //  and cannot be derived from the points, since two curves of one type differ
    //  only in where they sit. Two curves sharing one handle is the failure.
    StubComputedRoutes;
    StubCurves(2);
    FClient.UpdateComputedData(True);
    AssertFalse('the handles are not equal',
        SameCurveInstanceId(
            TNamedPointsSet(FView.LastCurves.Items[0]).FInstanceId,
            TNamedPointsSet(FView.LastCurves.Items[1]).FInstanceId));
    AssertTrue('and each is a handle rather than nothing',
        IsCurveInstanceId(
            TNamedPointsSet(FView.LastCurves.Items[0]).FInstanceId));
end;

procedure TFitClientViewTest.ACurveWithNoHandleIsReportedRatherThanDropped;
begin
    //  The refusal itself is already asserted against the service; what this adds
    //  is that a REFRESH lets it through rather than absorbing it. Skipping it
    //  quietly would leave the chart short of curves with nothing to say why, and
    //  the model would look wrong rather than broken - which is the shape of the
    //  defect this whole group of tests was written after.
    StubComputedRoutes;
    FSvc.Reply('curves', '{"ok":true,"curves":[{"id":"","params":[]}]}');
    FSvc.Reply('points', '{"title":"Gauss","x":[1,2],"y":[3,4]}');
    try
        FClient.UpdateComputedData(True);
        Fail('a curve with no handle was accepted');
    except
        on E: EUserException do
            AssertTrue('the message names the cause: ' + E.Message,
                Pos('identifier', E.Message) > 0);
    end;
end;

procedure TFitClientViewTest.ARefreshedModelReplacesTheOldCurves;
begin
    //  Each refresh frees the previous curves and reads them again. Accumulating
    //  would add a series per fit iteration, and in animation mode that is
    //  hundreds of them.
    StubComputedRoutes;
    StubCurves(5);
    FClient.UpdateComputedData(True);
    StubCurves(2);
    FClient.UpdateComputedData(True);
    AssertEquals('the second model replaced the first',
        2, FView.PointsPlottedIn('PlotCurves'));
end;

procedure TFitClientViewTest.TheWaveLengthReachesEveryCurve;
begin
    //  Every set on the chart converts its own x to the displayed axis from the
    //  wavelength it carries. One curve left at zero draws in different units
    //  from its neighbours, on the same chart, without failing.
    StubComputedRoutes;
    StubCurves(3);
    FClient.SetWaveLength(1.54);
    FClient.UpdateComputedData(True);
    AssertEquals('the last curve carries it', 1.54,
        TNamedPointsSet(FView.LastCurves.Items[2]).WaveLength, 1e-12);
end;

procedure TFitClientViewTest.RefreshingComputedDataPlotsEverySeries;
begin
    StubComputedRoutes;
    FClient.UpdateComputedData(True);
    AssertTrue('the computed profile: ' + FView.Log.AsText,
        FView.Plotted('PlotComputedProfile'));
    AssertTrue('the delta profile', FView.Plotted('PlotDeltaProfile'));
    AssertTrue('and the curves', FView.Plotted('PlotCurves'));
end;

procedure TFitClientViewTest.EachSeriesGetsItsOwnPointsNotItsNeighboursSet;
begin
    //  The whole point of the distinct counts above. Two series drawn from one set
    //  is the mistake, and it is invisible on a chart.
    StubComputedRoutes;
    FClient.UpdateComputedData(True);
    AssertEquals('the computed profile got its two points',
        2, FView.PointsPlottedIn('PlotComputedProfile'));
    AssertEquals('and the delta profile its three',
        3, FView.PointsPlottedIn('PlotDeltaProfile'));
end;

procedure TFitClientViewTest.AnEmptyComputedProfileIsNotPlotted;
begin
    //  Nothing computed yet is the state the program starts in. Plotting an empty
    //  series would put a legend entry on the chart for a curve that does not
    //  exist, and the user would look for it.
    FSvc.Reply('calc-profile', '{"title":"c","x":[],"y":[]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[],"y":[]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    FClient.UpdateComputedData(True);
    AssertFalse('an empty computed profile is not drawn: ' + FView.Log.AsText,
        FView.Plotted('PlotComputedProfile'));
    AssertFalse('nor an empty delta profile',
        FView.Plotted('PlotDeltaProfile'));
end;

procedure TFitClientViewTest.TheGridsAndLegendsAreTurnedOffWhileUpdating;
begin
    //  The client tells the view to stop updating its grids and legends before it
    //  starts replacing series, because doing it per series is what made a refresh
    //  visibly slow. Passing the flag through is the whole of the contract here.
    StubComputedRoutes;
    FClient.UpdateComputedData(True);
    AssertTrue('the view was told: ' + FView.Log.AsText,
        FView.Log.Saw('SetUpdateGrids') or FView.Log.Saw('SetUpdateLegends'));
end;

procedure TFitClientViewTest.ExtraDataOffStillPlotsTheComputedProfile;
begin
    //  ShowExtraData is about the grids and legends, not about the curve itself.
    //  A refresh that skipped the plot when it was False would leave the chart
    //  stale after a fit, with nothing to say why.
    StubComputedRoutes;
    FClient.UpdateComputedData(False);
    AssertTrue('still drawn', FView.Plotted('PlotComputedProfile'));
end;

procedure TFitClientViewTest.RefreshingTwiceReplacesRatherThanAccumulates;
begin
    //  Each refresh frees the previous computed series and asks for it again. If
    //  it accumulated, every fit iteration would add a series to the chart - which
    //  is the animation-mode path, run hundreds of times per fit.
    StubComputedRoutes;
    FClient.UpdateComputedData(True);
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2,3,4,5],"y":[1,2,3,4,5]}');
    FClient.UpdateComputedData(True);
    AssertEquals('the second refresh replaced the first',
        5, FView.PointsPlottedIn('PlotComputedProfile'));
end;

procedure TFitClientViewTest.TheWaveLengthReachesTheSeries;
begin
    //  The series carries the wavelength so the chart can convert its x to the
    //  displayed axis. Left at zero, a diffraction axis divides by it.
    StubComputedRoutes;
    FClient.SetWaveLength(1.54);
    AssertEquals('held by the client', 1.54, FClient.GetWaveLength, 1e-12);
    FClient.UpdateComputedData(True);
    AssertTrue('and the profile was drawn with it set',
        FView.Plotted('PlotComputedProfile'));
end;

procedure TFitClientViewTest.AHintIsShownToTheUser;
begin
    //  The hint is how the client explains what it is doing during a long
    //  operation, and it is the only channel it has that is not the chart.
    FClient.FFitViewer.ShowHint('working');
    AssertEquals('one hint', 1, FView.Hints.Count);
    AssertEquals('with the text', 'working', FView.Hints[0]);
end;

procedure TFitClientViewTest.NoViewerAttachedIsNotAFailure;
begin
    //  Every call is guarded by Assigned(FFitViewer), because the client exists
    //  before the form does - and during shutdown, after it has gone. A refresh
    //  with no viewer must do the server work and skip the drawing rather than
    //  raise into whatever was mid-teardown.
    StubComputedRoutes;
    FClient.FFitViewer := nil;
    FClient.UpdateComputedData(True);
    AssertEquals('nothing was drawn', 0, FView.Log.Calls.Count);
end;

{ ---- what the engine's callbacks do to the chart --------------------------- }

procedure TFitClientViewTest.ComputedBoundsAreHiddenBeforeBeingReplaced;
begin
    //  HIDDEN FIRST, and the order is the whole contract: the pointer is about to
    //  be replaced, and the chart holds it. Plotting the new set without hiding
    //  the old one leaves the view drawing freed memory - which is not an error
    //  anywhere near here.
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
    //  TWICE, because the first call has nothing to hide - the client starts with
    //  no set - and a single call therefore cannot tell a correct hide-first from
    //  a missing one. The replacement is the case that matters.
    FClient.ComputeCurveBoundsDone;
    FView.Log.Clear;
    FClient.ComputeCurveBoundsDone;
    AssertTrue('the old set was hidden: ' + FView.Log.AsText,
        FView.Plotted('HideRFactorBounds'));
    AssertTrue('and the new one plotted',
        FView.Plotted('PlotRFactorBounds'));
    AssertTrue('in that order',
        Pos('HideRFactorBounds', FView.Log.Sequence) <
        Pos('PlotRFactorBounds', FView.Log.Sequence));
end;

procedure TFitClientViewTest.ComputedBoundsCarryTheirName;
begin
    //  The title is what the legend shows. Left unset, the series appears
    //  unlabelled beside the ones that are named.
    FSvc.Reply('rfactor-bounds', '{"title":"whatever","x":[1,2],"y":[1,2]}');
    FClient.ComputeCurveBoundsDone;
    //  CAST, because the getter is declared as TNeutronPointsSet while the field
    //  is a TTitlePointsSet - the title lives on the descendant. The narrower
    //  return type is what the chart needs; the title is what the legend needs.
    AssertEquals('the client renamed it, not the server', CurveIntervalsName,
        TTitlePointsSet(FClient.GetRFactorBounds).FTitle);
end;

procedure TFitClientViewTest.AnEmptyComputedResultIsNotPlotted;
begin
    //  The server computed nothing - a profile with no interval in it. Plotting an
    //  empty series would add a legend entry for something that is not there.
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[],"y":[]}');
    FClient.ComputeCurveBoundsDone;
    AssertFalse('nothing was plotted: ' + FView.Log.AsText,
        FView.Plotted('PlotRFactorBounds'));
end;

procedure TFitClientViewTest.ComputedBackgroundReplacesTheOldSet;
begin
    FSvc.Reply('background', '{"title":"b","x":[1,2,3],"y":[1,2,3]}');
    FClient.ComputeBackgroundPointsDone;
    AssertTrue('hidden then plotted', FView.Plotted('PlotBackground'));
    AssertEquals('with the points the server sent',
        3, FView.PointsPlottedIn('PlotBackground'));
    AssertEquals('and the client''s own name', BackgroundPointsName,
        TTitlePointsSet(FClient.GetBackgroundPoints).FTitle);
end;

procedure TFitClientViewTest.ComputedPositionsReplaceBothSeries;
begin
    //  This callback replaces TWO series - the intervals and the positions - and
    //  the bug it is prone to is refreshing one and leaving the other stale, so
    //  the chart shows new positions against old intervals.
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
    FSvc.Reply('positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FClient.ComputeCurvePositionsDone;
    AssertEquals('the intervals', 2, FView.PointsPlottedIn('PlotRFactorBounds'));
    AssertEquals('and the positions', 3,
        FView.PointsPlottedIn('PlotCurvePositions'));
end;

procedure TFitClientViewTest.ThePositionsTableShowsThePicksWhenThereAreAny;
begin
    //  The table has always shown these, and must go on showing them: they are
    //  what the user put there and what they can edit.
    StubComputedRoutes;
    FSvc.Reply('positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('calc-positions', '{"title":"c","x":[9],"y":[9]}');
    FClient.UpdateComputedData(True);
    AssertEquals('the picks are in the table', 3,
        FView.PointsPlottedIn('TabulateCurvePositions'));
end;

procedure TFitClientViewTest.WithNoPicksItShowsWhereTheModelsCurvesActuallySit;
begin
    //  THE DEFECT, and it is what an analysis pack's model looks like from
    //  here: nothing was picked, because every instance was placed from the
    //  pack's own markup. The chart drew all of them and the table said there
    //  were none - so the window disagreed with itself about whether a model
    //  existed.
    StubComputedRoutes;
    FSvc.Reply('positions', '{"title":"p","x":[],"y":[]}');
    FSvc.Reply('calc-positions', '{"title":"c","x":[4,5],"y":[4,5]}');
    FClient.UpdateComputedData(True);
    AssertEquals('the model''s own positions are in the table', 2,
        FView.PointsPlottedIn('TabulateCurvePositions'));
end;

procedure TFitClientViewTest.WithNeitherNothingIsPutInTheTable;
begin
    //  No picks and no model. The table is left alone rather than filled with
    //  an empty set - and never cleared from here, which would take the input
    //  focus away from a user in the middle of typing in it.
    StubComputedRoutes;
    FSvc.Reply('positions', '{"title":"p","x":[],"y":[]}');
    FSvc.Reply('calc-positions', '{"title":"c","x":[],"y":[]}');
    FClient.UpdateComputedData(True);
    AssertFalse('nothing was tabulated',
        FView.Plotted('TabulateCurvePositions'));
end;

procedure TFitClientViewTest.EveryCallbackReportsTheOperationFinished;
var
    Finished: longint;
begin
    //  The form re-enables its menus on this. A callback that forgot to raise it
    //  leaves the interface disabled after a successful operation, with nothing
    //  wrong except that the user cannot do anything.
    Finished := 0;
    FFinishedCount := @Finished;
    FClient.OnAsyncOperationFinished := AsyncFinished;

    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1],"y":[1]}');
    FClient.ComputeCurveBoundsDone;
    AssertEquals('bounds', 1, Finished);

    FSvc.Reply('background', '{"title":"b","x":[1],"y":[1]}');
    FClient.ComputeBackgroundPointsDone;
    AssertEquals('background', 2, Finished);

    FSvc.Reply('positions', '{"title":"p","x":[1],"y":[1]}');
    FClient.ComputeCurvePositionsDone;
    AssertEquals('positions', 3, Finished);
end;

procedure TFitClientViewTest.DoneRefreshesTheProfileAndTheComputedData;
begin
    //  What the engine calls when a long operation ends. It has to refresh BOTH
    //  the experimental profile and everything derived from it: refreshing only
    //  the computed half leaves the chart showing a fit against a profile the
    //  server no longer holds.
    StubComputedRoutes;
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3,4],"y":[1,2,3,4]}');
    FClient.Done;
    AssertTrue('the profile: ' + FView.Log.AsText,
        FView.Plotted('PlotExpProfile'));
    AssertTrue('and the computed data',
        FView.Plotted('PlotComputedProfile'));
end;

{ ---- the current point set and the selection ------------------------------- }

procedure TFitClientViewTest.TheCurrentSetFollowsTheSelectionMode;
begin
    //  Which set a click edits. Getting this wrong sends a background pick into
    //  the profile, and the user sees their data change.
    //
    //  BOTH SETS FIRST. SetSelectionMode asserts that the mode being switched
    //  into has something to draw, which is the app's own order - a picking mode
    //  is offered only once the set it edits exists. Switching without that is a
    //  programming error, not a case to cover.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FSvc.Reply('positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
    FClient.ComputeCurvePositionsDone;

    FClient.SelectionMode := ModeSelectBackground;
    AssertTrue('background mode', FClient.GetCurrentPointsSet =
        TTitlePointsSet(FClient.GetBackgroundPoints));
    FClient.SelectionMode := ModeSelectCurvePositions;
    AssertTrue('positions mode', FClient.GetCurrentPointsSet =
        TTitlePointsSet(FClient.GetCurvePositions));
end;

procedure TFitClientViewTest.SelectingTheEntireProfileClearsTheInterval;
begin
    //  Back to fitting everything. The selected-interval series has to go, or the
    //  chart keeps highlighting a stretch that is no longer special.
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3,4,5],"y":[1,2,3,4,5]}');
    FSvc.Reply('selected-interval', '{"title":"i","x":[2,3],"y":[2,3]}');
    //  A PROFILE, THEN AN INTERVAL, then back to everything. Asserting the flag
    //  is False on a fresh client would pass without SelectEntireProfile having
    //  done anything - it starts False - so the interval has to be in force first.
    FClient.ShowProfile;
    FClient.SelectProfileInterval(1, 3);
    AssertTrue('an interval is in force to begin with', FClient.SelectedAreaMode);

    FClient.SelectEntireProfile;
    AssertFalse('and it is gone', FClient.SelectedAreaMode);
    AssertTrue('the whole profile is drawn again: ' + FView.Log.AsText,
        FView.Plotted('PlotExpProfile'));
end;

{ ---- the callback stub ----------------------------------------------------- }

procedure TFitClientViewTest.TheStubForwardsEveryCallbackToTheClient;
var
    Stub: TFitClientStub;
begin
    //  WHY A STUB AT ALL: the engine is handed an IClientCallback, and TFitClient
    //  is not one - the stub is the adapter, and it holds the client as a bare
    //  TObject with a cast on every call. Six one-line forwards, and a forward
    //  wired to the wrong method produces a client that draws the wrong thing
    //  after a successful operation, with no error anywhere.
    //
    //  Each callback is identified by what it makes the CLIENT do, observed on
    //  the mocked chart - not by anything the stub says about itself.
    StubComputedRoutes;
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3,4],"y":[1,2,3,4]}');
    FSvc.Reply('background', '{"title":"b","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');

    Stub := TFitClientStub.Create;
    try
        Stub.FitClient := FClient;

        Stub.ShowProfile;
        AssertTrue('ShowProfile reached the client: ' + FView.Log.AsText,
            FView.Plotted('PlotExpProfile'));

        FView.Log.Clear;
        Stub.ComputeCurveBoundsDone;
        AssertTrue('ComputeCurveBoundsDone did',
            FView.Plotted('PlotRFactorBounds'));

        FView.Log.Clear;
        Stub.ComputeBackgroundPointsDone;
        AssertTrue('ComputeBackgroundPointsDone did',
            FView.Plotted('PlotBackground'));

        FView.Log.Clear;
        Stub.ComputeCurvePositionsDone;
        AssertTrue('ComputeCurvePositionsDone did',
            FView.Plotted('PlotCurvePositions'));

        FView.Log.Clear;
        Stub.Done;
        AssertTrue('Done did', FView.Plotted('PlotComputedProfile'));

        //  ShowCurMin is the one with no drawing of its own: it stores the
        //  minimum and asks the view for the two readouts.
        FView.Log.Clear;
        Stub.ShowCurMin(0.125);
        AssertTrue('ShowCurMin reached the client: ' + FView.Log.AsText,
            FView.Plotted('ShowRFactor'));
    finally
        Stub.Free;
    end;
end;

procedure TFitClientViewTest.TheStubIsTheSameContractAsTheClient;
var
    Stub: TFitClientStub;
    Callback: IClientCallback;
begin
    //  It has to BE an IClientCallback, not merely have the methods: the engine
    //  holds it through the interface. Reached through the interface reference
    //  here rather than through the class, so the vtable is what is exercised.
    StubComputedRoutes;
    Stub := TFitClientStub.Create;
    try
        Stub.FitClient := FClient;
        Callback := Stub;
        AssertTrue('it is a callback', Assigned(Callback));
        Callback.ComputeCurveBoundsDone;
        AssertTrue('and the call arrived: ' + FView.Log.AsText,
            FView.Plotted('PlotRFactorBounds'));
    finally
        //  The interface reference first: -SIcorba counts none of them, so a
        //  live reference to a freed object is not detected, it is used.
        Callback := nil;
        Stub.Free;
    end;
end;

{ ---- the server-side proxy ------------------------------------------------- }

procedure TFitClientViewTest.TheProxyForwardsEveryCallbackToTheStub;
var
    Stub: TFitClientStub;
    Proxy: TFitServerProxy;
begin
    //  Engine -> proxy -> stub -> client, and this is the first link. The whole
    //  chain is six forwards repeated three times over, which is exactly the
    //  shape a copy-paste slip survives in: every method looks right beside its
    //  neighbours and one of them calls the wrong one.
    StubComputedRoutes;
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3,4],"y":[1,2,3,4]}');
    FSvc.Reply('background', '{"title":"b","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('positions', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');

    Stub := TFitClientStub.Create;
    Proxy := TFitServerProxy.Create;
    try
        Stub.FitClient := FClient;
        Proxy.FitClientStub := Stub;

        Proxy.ShowProfile;
        AssertTrue('ShowProfile arrived: ' + FView.Log.AsText,
            FView.Plotted('PlotExpProfile'));

        FView.Log.Clear;
        Proxy.ComputeCurveBoundsDone;
        AssertTrue('bounds arrived', FView.Plotted('PlotRFactorBounds'));

        FView.Log.Clear;
        Proxy.ComputeBackgroundPointsDone;
        AssertTrue('background arrived', FView.Plotted('PlotBackground'));

        FView.Log.Clear;
        Proxy.ComputeCurvePositionsDone;
        AssertTrue('positions arrived', FView.Plotted('PlotCurvePositions'));

        FView.Log.Clear;
        Proxy.Done;
        AssertTrue('done arrived', FView.Plotted('PlotComputedProfile'));

        FView.Log.Clear;
        Proxy.ShowCurMin(0.5);
        AssertTrue('the minimum arrived', FView.Plotted('ShowRFactor'));
    finally
        Proxy.Free;
        Stub.Free;
    end;
end;

procedure TFitClientViewTest.AProxyWithNoStubRefusesEveryCallback;
var
    Proxy: TFitServerProxy;
    Refusals: longint;

    { Runs one callback and counts the refusal. TThreadMethod is just
      `procedure of object`. }
    procedure ExpectRefusal(const AWhat: string; AProc: TThreadMethod);
    begin
        try
            AProc;
            Fail(AWhat + ' was accepted with no client behind it');
        except
            on E: EInternalCheckFailed do
                Inc(Refusals);
            on E: EUserException do
                Fail(AWhat + ' refused as user error, not as a defect');
        end;
    end;

begin
    //  IT REFUSES, and this pins WHICH exception - because the class it is is
    //  the whole difference between a fault the client reports and one that
    //  reaches the top of a worker thread.
    //
    //  A callback with nothing behind it is the program being wrong about
    //  itself, not the user doing something unsupported, so it must arrive as
    //  EInternalCheckFailed and NOT as EUserException. Each of these six methods
    //  used to wrap its check in `except on E: EAssertionFailed do raise
    //  EUserException...`, which could never fire and said the opposite of what
    //  happened; those handlers are gone and this test is what holds the
    //  behaviour they misdescribed.
    //
    //  EUserException is named separately below because catching only
    //  EInternalCheckFailed would let a re-introduced re-raise escape as an
    //  error rather than fail with a sentence saying which class was wrong.
    Refusals := 0;
    Proxy := TFitServerProxy.Create;
    try
        AssertFalse('no stub is attached', Assigned(Proxy.FitClientStub));
        ExpectRefusal('ShowProfile', Proxy.ShowProfile);
        ExpectRefusal('Done', Proxy.Done);
        ExpectRefusal('ComputeCurveBoundsDone', Proxy.ComputeCurveBoundsDone);
        ExpectRefusal('ComputeBackgroundPointsDone',
            Proxy.ComputeBackgroundPointsDone);
        ExpectRefusal('ComputeCurvePositionsDone',
            Proxy.ComputeCurvePositionsDone);
        //  ShowCurMin takes an argument, so it cannot go through ExpectRefusal.
        try
            Proxy.ShowCurMin(1.0);
            Fail('ShowCurMin was accepted with no client behind it');
        except
            on E: EInternalCheckFailed do
                Inc(Refusals);
            on E: EUserException do
                Fail('ShowCurMin refused as user error, not as a defect');
        end;
        AssertEquals('all six refused', 6, Refusals);
    finally
        Proxy.Free;
    end;
end;

{ ---- the settings the client passes through -------------------------------- }

procedure TFitClientViewTest.EverySettingReachesTheServerAndComesBack;
begin
    //  THE MOCK IS NOT A SERVER: it answers from a table, so a value written
    //  does not come back changed. What is tested here is the marshalling -
    //  that each setter sends its own field, and each getter reads its own -
    //  which is the client's whole job at this layer. The round trip through a
    //  real engine is what testcase_rest_api drives.
    FClient.MaxRFactor := (0.125);
    AssertTrue('the ceiling was sent: ' + FSvc.LastBody,
        Pos('maxRFactor', FSvc.LastBody) > 0);
    //  fpjson writes doubles in EXPONENTIAL form - 1.25E-001, not 0.125 - so
    //  the mantissa is matched rather than the literal a reader would expect.
    //  Worth knowing before writing any assertion against a JSON number this
    //  program produced.
    AssertTrue('with its value: ' + FSvc.LastBody,
        Pos('1.25', FSvc.LastBody) > 0);

    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.0625}');
    AssertEquals('and it is read back from its own field', 0.0625,
        FClient.MaxRFactor, 1E-9);
end;

procedure TFitClientViewTest.TheSettingsDoNotShareOneField;
begin
    //  THE FAILURE THIS CATCHES: a getter reading the wrong field. Three
    //  distinct values in one reply, so a getter that reads its neighbour's
    //  answers with a plausible number instead of the right one.
    FSvc.Reply('settings',
        '{"ok":true,"maxRFactor":0.1,"backFactor":0.2,"curveThresh":0.3}');
    AssertEquals('the ceiling', 0.1, FClient.MaxRFactor, 1E-9);
    AssertEquals('the fraction', 0.2, FClient.BackFactor, 1E-9);
    AssertEquals('the threshold', 0.3, FClient.CurveThresh, 1E-9);
end;

procedure TFitClientViewTest.EverySettingWritesItsOwnField;
begin
    //  The other half: a setter writing the wrong field. Each is sent on its
    //  own and the body is read back.
    FClient.BackFactor := (0.75);
    AssertTrue('the fraction: ' + FSvc.LastBody,
        Pos('backFactor', FSvc.LastBody) > 0);
    FClient.CurveThresh := (0.05);
    AssertTrue('the threshold: ' + FSvc.LastBody,
        Pos('curveThresh', FSvc.LastBody) > 0);
    FClient.LossKind := (2);
    AssertTrue('the objective: ' + FSvc.LastBody,
        Pos('lossKind', FSvc.LastBody) > 0);
end;

procedure TFitClientViewTest.TheMinimizerAndObjectiveAreDistinct;
begin
    //  Which algorithm runs and what it minimises are two different choices,
    //  and both are integers - which is exactly how they get crossed.
    FSvc.Reply('settings', '{"ok":true,"minimizerKind":1,"lossKind":2}');
    AssertEquals('the algorithm', 1, FClient.MinimizerKind);
    AssertEquals('the objective', 2, FClient.LossKind);
end;

procedure TFitClientViewTest.TheWeightingRoundTrips;
begin
    //  'poisson' or 'none'. The Python backend reads it and the native engine
    //  ignores it, so a value lost here changes the answer of a fit silently.
    FSvc.Reply('settings', '{"ok":true,"weighting":"none"}');
    AssertEquals('read back', 'none', FClient.Weighting);
    FClient.Weighting := ('poisson');
    AssertTrue('and written: ' + FSvc.LastBody,
        Pos('weighting', FSvc.LastBody) > 0);
    AssertTrue('with its value', Pos('poisson', FSvc.LastBody) > 0);
end;

procedure TFitClientViewTest.TheWeightingHasAStatisticallyCorrectDefault;
begin
    //  Absent from the reply - which is what an older server sends - the client
    //  must not fall back to no weighting: the data are counts, and unweighted
    //  is a different answer rather than a simpler one.
    FSvc.Reply('settings', '{"ok":true}');
    AssertEquals('poisson', 'poisson', FClient.Weighting);
end;

procedure TFitClientViewTest.TheServerUrlRoundTrips;
begin
    //  NOT A SETTING ON THE SERVER - it is WHICH server, so it is the client's
    //  own base URL and comes back from there rather than from a reply. A
    //  server cannot be asked where it is.
    AssertEquals('the client knows where it is talking', BASE,
        FClient.ServerUrl);
end;

function TFitClientViewTest.SettingsReads: longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
        //  GET only: a PUT is the client writing a setting, which is a different
        //  question from how often it asks for one.
        if (Pos('/settings', FSvc.Log.Calls[i]) > 0) and
           (Pos('GET(', FSvc.Log.Calls[i]) = 1) then
            Inc(Result);
end;

procedure TFitClientViewTest.TheFlagsAreReadOncePerServerNotPerUse;
var
    i: longint;
begin
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":true,"curveScaling":false}');
    //  Ten reads of each, which is a fraction of what one second on the idle
    //  loop asks for.
    for i := 1 to 10 do
    begin
        FClient.BackgroundVariationEnabled;
        FClient.CurveScalingEnabled;
    end;
    //  TWO - one per flag, because the service exposes them as two getters over
    //  one route - and, the point of this test, TWO RATHER THAN TWENTY. What is
    //  bounded is reads per server, not reads per reply.
    AssertEquals('two reads for twenty uses: ' + FSvc.Log.AsText,
        2, SettingsReads);
    AssertTrue('and the value is the server''s',
        FClient.BackgroundVariationEnabled);
end;

procedure TFitClientViewTest.SettingAFlagNeedsNoFurtherRead;
begin
    //  A value we set is a value we know. Reading it back over HTTP would be
    //  asking the server to confirm what we just told it.
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":false,"curveScaling":false}');
    FClient.BackgroundVariationEnabled := True;
    AssertEquals('the write asked for nothing: ' + FSvc.Log.AsText,
        0, SettingsReads);
    AssertTrue('and it reads back as set', FClient.BackgroundVariationEnabled);
    AssertEquals('still without a read', 0, SettingsReads);
end;

procedure TFitClientViewTest.PointingAtAnotherServerReReadsThem;
begin
    //  A different server is a different problem, so what was cached describes
    //  a problem this client no longer has.
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":true,"curveScaling":false}');
    AssertTrue('read from the first server',
        FClient.BackgroundVariationEnabled);

    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":false,"curveScaling":true}');
    FClient.ServerUrl := BASE + '-elsewhere';
    AssertFalse('and again from the second',
        FClient.BackgroundVariationEnabled);
    AssertTrue('and the other flag with it',
        FClient.CurveScalingEnabled);
    AssertEquals('one pass per server, not one per use: ' + FSvc.Log.AsText,
        4, SettingsReads);
end;

procedure TFitClientViewTest.PointingAtTheSameServerAgainDoesNot;
begin
    //  START-UP DOES EXACTLY THIS: the window applies the configured URL, which
    //  is usually the one already in use. Treating it as a change would throw
    //  away the answer and ask again on the next idle tick.
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":true,"curveScaling":false}');
    AssertTrue('read once', FClient.BackgroundVariationEnabled);
    FClient.ServerUrl := FClient.ServerUrl;
    FClient.BackgroundVariationEnabled;
    FClient.CurveScalingEnabled;
    AssertEquals('and not again: ' + FSvc.Log.AsText, 2, SettingsReads);
end;

procedure TFitClientViewTest.AnUnreachableServerLeavesTheMenuAnswerable;
begin
    //  THE FAILURE THIS REPLACES. The caller is an action's Update handler, and
    //  it runs during FormCreate - so a client started before its server raised
    //  there, and logged a fatal with a stack trace before the window was shown.
    //  It must answer instead, with what the engine starts a problem at.
    //
    //  CURVE SCALING IS THE ONE TO ASSERT, and asserting it is the whole reason
    //  the seed is not written as one value for both: the engine starts a problem
    //  with scaling ON and background variation OFF, so False would pass here for
    //  a client that had simply zeroed everything.
    FSvc.FailNextWith('the server is not there');
    AssertTrue('answers with what the engine starts a problem at',
        FClient.CurveScalingEnabled);

    //  AND IT IS NOT ASKED AGAIN ON THE NEXT USE, which is the half that looks
    //  like a bug and is not: the next use is the idle loop, and retrying there
    //  costs a connect timeout per tick with the window waiting for each one.
    //  Nothing is lost by holding the seed, because nothing but the client writes
    //  these two - an untouched problem still holds what the engine started it
    //  at.
    FSvc.Reply('settings', '{"ok":true,"curveScaling":false}');
    AssertTrue('still the seeded value', FClient.CurveScalingEnabled);

    //  Pointing it at a server is the action a user takes to fix this, and it is
    //  what re-reads them.
    FClient.ServerUrl := BASE + '-live';
    AssertFalse('read again once pointed somewhere',
        FClient.CurveScalingEnabled);
end;

procedure TFitClientViewTest.TheTwoFlagsAreNotEachOther;
begin
    //  Two booleans in one reply. Crossed, enabling one enables the other, and
    //  a fit varies something the user asked it to hold.
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":true,"curveScaling":false}');
    AssertTrue('the background may vary',
        FClient.BackgroundVariationEnabled);
    AssertFalse('and the curves are not scaled', FClient.CurveScalingEnabled);

    //  A NEW SERVER between the two halves, because the client reads these once
    //  per server and holds them - changing the reply alone would assert nothing
    //  about a value it never asks for twice.
    FSvc.Reply('settings',
        '{"ok":true,"backgroundVariation":false,"curveScaling":true}');
    FClient.ServerUrl := BASE + '-other';
    AssertFalse('and the other way round',
        FClient.BackgroundVariationEnabled);
    AssertTrue('and the other way round', FClient.CurveScalingEnabled);
end;

procedure TFitClientViewTest.BothFlagsAreOffWhenTheServerSaysNothing;
begin
    //  An older server, or one that has never been told. Off is the
    //  conservative answer for both: a fit that varies less than the user asked
    //  is visibly wrong, one that varies more is not.
    FSvc.Reply('settings', '{"ok":true}');
    AssertFalse('the background is held',
        FClient.BackgroundVariationEnabled);
    AssertFalse('and so are the curves', FClient.CurveScalingEnabled);
end;

procedure TFitClientViewTest.TheWaveLengthIsHeldByTheClient;
begin
    //  Held here as well as on the server, because every point set the client
    //  draws is stamped with it - see how the callbacks set it on each set they
    //  fetch.
    FClient.SetWaveLength(1.5406);
    AssertEquals('the wavelength', 1.5406, FClient.GetWaveLength, 1E-9);
end;

procedure TFitClientViewTest.TheWaveLengthReachesTheProfileAlreadyHeld;
begin
    //  Setting it AFTER the data was loaded must stamp what is already there,
    //  or the profile is drawn against one axis and everything fetched later
    //  against another.
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3],"y":[1,2,3]}');
    FClient.ShowProfile;
    FClient.SetWaveLength(1.5406);
    AssertEquals('the profile was stamped', 1.5406,
        FClient.GetProfilePoints.WaveLength, 1E-9);
end;

{ ---- the readouts ---------------------------------------------------------- }

procedure TFitClientViewTest.TheProgressReadoutsComeFromTheServer;
begin
    //  Two strings the status bar shows while a fit runs. They are read from
    //  the server rather than computed here, so each needs its own field.
    //  Read from /stats, which is also where the goodness-of-fit figures come
    //  from - one route, so a fit's progress and its result cannot disagree
    //  about which fit they describe.
    FSvc.Reply('stats',
        '{"ok":true,"rFactor":"0.0421","absRFactor":"0.0512",' +
        '"sqrRFactor":"0.0033","calcTime":"00:00:07"}');
    AssertEquals('the R-factor', '0.0421', FClient.GetRFactorStr);
end;

procedure TFitClientViewTest.NothingIsRunningToBeginWith;
begin
    //  Polled twice a second by the window. A client that reported an operation
    //  in progress before one started would leave the interface disabled from
    //  the moment it opened.
    FSvc.Reply('async', '{"ok":true,"busy":false}');
    AssertFalse('idle', FClient.AsyncOper);
end;

procedure TFitClientViewTest.AnOperationInProgressIsReported;
begin
    FSvc.Reply('async', '{"ok":true,"busy":true}');
    AssertTrue('running', FClient.AsyncOper);
end;

{ ---- the point-editing verbs ----------------------------------------------- }

procedure TFitClientViewTest.APickIsAddedToTheBackground;
begin
    //  The user clicked on the chart in background mode. The point has to reach
    //  the set the mode is collecting into AND be drawn, or the click appears
    //  to have done nothing.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FView.Log.Clear;
    FClient.AddPointToBackground(3, 30);
    AssertEquals('the set grew', 3, FClient.GetBackgroundPoints.PointsCount);
    AssertTrue('and it was redrawn: ' + FView.Log.AsText,
        FView.Plotted('PlotBackground'));
end;

procedure TFitClientViewTest.APickGoesToTheSetTheModeIsCollecting;
begin
    //  ADDING TO THE ACTIVE SET is what a click does, and which set that is
    //  comes from the mode. Routed wrongly, a background pick lands in the
    //  positions and the user sees their model change.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FSvc.Reply('positions', '{"title":"p","x":[5],"y":[5]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FClient.ComputeCurvePositionsDone;

    FClient.SelectionMode := ModeSelectBackground;
    FClient.AddPointToActive(9, 90);
    AssertEquals('the background took it', 3,
        FClient.GetBackgroundPoints.PointsCount);
    AssertEquals('and the positions did not', 1,
        FClient.GetCurvePositions.PointsCount);
end;

procedure TFitClientViewTest.APickIsMovedRatherThanDuplicated;
begin
    //  A drag. The point is identified by where it was, so a move that does not
    //  match leaves the old one behind and the user has two picks where they
    //  moved one.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[10,20]}');
    FClient.ComputeBackgroundPointsDone;
    FClient.ReplacePointInBackground(2, 20, 3, 30);
    AssertEquals('still two points', 2,
        FClient.GetBackgroundPoints.PointsCount);
end;

{ ---- putting things away -------------------------------------------------- }

procedure TFitClientViewTest.RemovingASetHidesItFirst;
begin
    //  HIDDEN BEFORE FREED, every time. The chart holds the pointer, so freeing
    //  a set the view is still drawing leaves it painting reclaimed memory -
    //  which is not an error anywhere near where it happens.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FView.Log.Clear;
    FClient.RemoveBackgroundPoints;
    AssertTrue('hidden: ' + FView.Log.AsText,
        FView.Plotted('HideBackground'));
    AssertTrue('and the client holds nothing',
        FClient.GetBackgroundPoints = nil);
end;

procedure TFitClientViewTest.RemovingASetThatIsNotThereIsNotAFailure;
begin
    //  Called on every refresh, before anything has been computed. A guard
    //  missing here is a fault on the first redraw of a new file.
    //  The four the client exposes. The other four are private and are
    //  reached through the callbacks that use them.
    FClient.RemoveBackgroundPoints;
    FClient.RemoveRFactorBounds;
    FClient.RemoveCurvePositions;
    FClient.RemoveResultedCurvePositions;
    AssertTrue('all four were safe', True);
end;

procedure TFitClientViewTest.EveryRemovalHidesItsOwnSet;
begin
    //  Eight removals, and the one thing each must not do is hide somebody
    //  else's series - which leaves the chart showing a set the client thinks
    //  is gone.
    StubComputedRoutes;
    FSvc.Reply('positions', '{"title":"p","x":[1,2],"y":[1,2]}');
    FClient.UpdateComputedData(True);
    FClient.ComputeCurvePositionsDone;

    FView.Log.Clear;
    FClient.RemoveCurvePositions;
    AssertTrue('the positions went', FView.Plotted('HideCurvePositions'));
    AssertFalse('and the bounds stayed',
        FView.Plotted('HideRFactorBounds'));
end;

procedure TFitClientViewTest.ClearingTakesEverythingOffTheChart;
begin
    //  What happens when a new file is opened over an old one. Anything left
    //  behind is a series from the previous document drawn against the new one.
    StubComputedRoutes;
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3,4],"y":[1,2,3,4]}');
    FClient.Done;
    FView.Log.Clear;
    //  Through SelectEntireProfile, which is what the window calls and what
    //  clears the chart on its way to redrawing the whole profile.
    FClient.SelectEntireProfile;
    AssertTrue('something was hidden: ' + FView.Log.AsText,
        FView.Log.Sequence <> '');
end;

{ ---- refreshing ------------------------------------------------------------ }

procedure TFitClientViewTest.RefreshingWithNoViewerIsNotAFailure;
begin
    //  The client outlives the window during shutdown, and the state poll can
    //  reach it after the viewer has gone.
    StubComputedRoutes;
    FClient.FFitViewer := nil;
    FClient.UpdateComputedData(True);
    AssertTrue('nothing faulted', True);
end;

procedure TFitClientViewTest.SelectingACurveTypeTellsTheServer;
begin
    //  BOTH SIDES. The menu and the server must agree on which model is being
    //  fitted, and the client is what keeps them in step - a selection that
    //  reached only the menu fits the previous type.
    FClient.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    AssertTrue('it was sent: ' + FSvc.LastBody,
        Pos('curveType', FSvc.LastBody) > 0);
end;

procedure TFitClientViewTest.TheSelectedCurveTypeIsReadBack;
begin
    FSvc.Reply('settings', Format('{"ok":true,"curveType":"%s"}',
        [GUIDToString(TGaussPointsSet.GetCurveTypeId)]));
    AssertTrue('the same type',
        IsEqualGUID(TGaussPointsSet.GetCurveTypeId, FClient.CurveTypeId));
end;

{ ---- a module's own picking ------------------------------------------------ }

procedure TFitClientViewTest.AModuleGestureNamesItsOwnSet;
begin
    //  The set travels beside the mode, so the selection enum stays closed and
    //  its case statements stay exhaustive - a module cannot add a value to it.
    FClient.BeginModuleSelection('wave-marks');
    AssertEquals('the mode', Ord(ModeSelectModulePoints),
        Ord(FClient.SelectionMode));
    AssertEquals('and the set it collects into', 'wave-marks',
        FClient.ModulePickSet);
end;

procedure TFitClientViewTest.LeavingAModuleGestureKeepsItsSetName;
begin
    //  ASSERTED AS IT BEHAVES. The name is not cleared when the mode changes -
    //  what gates its use is the MODE, and a pick is only routed to a module's
    //  set while ModeSelectModulePoints is current. So a stale name is inert.
    //
    //  It is still a loose end: a reader of ModulePickSet who does not also
    //  check the mode gets the name of a gesture that ended. Pinned rather than
    //  changed, because clearing it is only safe once every reader is known.
    FSvc.Reply('background', '{"title":"b","x":[1],"y":[1]}');
    FClient.ComputeBackgroundPointsDone;
    FClient.BeginModuleSelection('marks');
    FClient.SelectionMode := ModeSelectBackground;
    AssertEquals('the mode moved on', Ord(ModeSelectBackground),
        Ord(FClient.SelectionMode));
    AssertEquals('and the name is still there', 'marks',
        FClient.ModulePickSet);
end;

{ ---- the user-defined curve ------------------------------------------------ }

procedure TFitClientViewTest.TheUserCurveFormulaIsSentToTheServer;
begin
    //  The server builds curves from it, so a formula that stayed in the client
    //  is a model the engine cannot make.
    FClient.SetSpecialCurveParameters('A*exp(-x*x)', nil);
    AssertTrue('the formula was sent: ' + FSvc.LastBody,
        Pos('exp', FSvc.LastBody) > 0);
end;

procedure TFitClientViewTest.ForgettingTheUserCurveIsSentToo;
begin
    //  THE REASON THIS VERB EXISTS. Without it the server goes on building
    //  curves from a formula that no longer exists anywhere in the interface,
    //  which is how a deleted curve type kept reappearing in the fit.
    FClient.ClearSpecialCurve;
    AssertTrue('a request went: ' + FSvc.Log.AsText,
        Pos('special-params', FSvc.Log.Sequence) > 0);
end;

{ ---- redrawing ------------------------------------------------------------- }

procedure TFitClientViewTest.ARefreshReachesTheViewer;
begin
    //  The end of every edit: the client tells the view to draw itself again.
    TClientForTest(FClient).CallRefresh;
    AssertTrue('the viewer was told: ' + FView.Log.AsText,
        FView.Log.Saw('Refresh'));
end;

procedure TFitClientViewTest.WithNoViewerARefreshIsSilent;
begin
    //  The client runs headless in the REST server, where there is no view at
    //  all. A redraw request then must do nothing rather than fault.
    FClient.FFitViewer := nil;
    TClientForTest(FClient).CallRefresh;
    AssertTrue('nothing happened, and nothing broke', True);
end;

procedure TFitClientViewTest.RefreshingOneSetReachesTheViewer;
var
    P: TTitlePointsSet;
begin
    P := TTitlePointsSet.Create(nil);
    try
        P.AddNewPoint(1, 2);
        TClientForTest(FClient).CallRefreshPointsSet(P);
        AssertTrue('the viewer was told', FView.Log.Saw('RefreshPointsSet'));
    finally
        P.Free;
    end;
end;

procedure TFitClientViewTest.RefreshingNothingIsNotARefresh;
begin
    //  BOTH GUARDS MATTER, and this is the second one: a nil set reaching the
    //  view would be dereferenced there, in a paint handler, where the fault has
    //  no frame of ours above it.
    TClientForTest(FClient).CallRefreshPointsSet(nil);
    AssertTrue('nothing was drawn',
        not FView.Log.Saw('RefreshPointsSet'));
end;

{ ---- editing the picked sets ---------------------------------------------- }

{ EDITING THE PICKED SETS IS NOT TESTED FROM HERE, and the reason is a real
  constraint rather than a gap left casually: the client asserts those sets are
  assigned, and they are created only when data for them has arrived from the
  engine. Reaching the edit therefore means driving a whole fetch first, which is
  what the REST fit tests already do on the engine's side. See
  testcase_service_surface for the same edits against the engine itself. }


{ ---- the wavelength ------------------------------------------------------- }

function AProfileOfTen: TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    for i := 0 to 9 do
        Result.AddNewPoint(i, 10 + i);
end;

procedure TFitClientViewTest.TheWavelengthReachesTheProfile;
begin
    TClientForTest(FClient).CallSetExpProfile(AProfileOfTen);
    FClient.SetWaveLength(1.5406);
    AssertEquals('the profile carries it', 1.5406,
        FClient.GetProfilePoints.WaveLength, 1e-12);
end;

{ A PICK MADE AFTER THE WAVELENGTH WAS SET is not tested from here: the client
  asserts the pick's set exists, and it is created when the picking mode starts -
  which is the window's business. The rule itself (a set takes the wavelength at
  creation, not only when the user changes it) is visible in
  RecreateAndShowSelectedPoints and belongs to a fixture that drives the modes. }

procedure TFitClientViewTest.AndEverySetTheServerFilled;
var
    Lambda: double;
begin
    //  THE SWEEP THAT MATTERS. The computed profile, the residual and the curves
    //  all arrive from the server as bare numbers; the wavelength is the client's
    //  to apply, and it applies it set by set. One `if Assigned` guarding the
    //  wrong field would leave one series on a different axis - which looks like
    //  a fit that missed rather than a display fault.
    Lambda := 1.5406;
    TClientForTest(FClient).CallSetExpProfile(AProfileOfTen);
    StubComputedRoutes;
    FClient.UpdateComputedData(True);
    FClient.SetWaveLength(Lambda);
    AssertEquals('the profile', Lambda,
        FClient.GetProfilePoints.WaveLength, 1e-12);
    //  Asked through the viewer, which is what received the computed series: a
    //  getter for each of them is not on the client's surface, and the point is
    //  that the CHART is consistent.
    AssertTrue('and the computed series were plotted after it',
        FView.Plotted('PlotComputedProfile'));
end;

procedure TFitClientViewTest.AProfilePointCanBeMovedOnceThereIsAProfile;
begin
    //  What editing a cell of the profile table becomes here: the client moves
    //  the point in the set it owns and tells the engine. Both halves, so the
    //  engine's reply is stubbed.
    TClientForTest(FClient).CallSetExpProfile(AProfileOfTen);
    FSvc.Reply('profile', '{"title":"p","x":[3],"y":[99]}');
    FSvc.Reply('actions', '{"ok":true}');
    FClient.ReplacePointInProfile(3, 13, 3, 99);
    AssertTrue('the profile was redrawn: ' + FView.Log.AsText,
        FView.Log.Saw('PlotExpProfile'));
end;

{ THE OTHER HALF OF THE PROXY: with a stub attached, every callback goes through.

  The refusal above is what happens when the chain is not wired; this is the
  chain working, and until now only the broken case had ever been executed. Six
  methods, each a try..except around one forwarding call - the shape where a
  copy-paste mistake sends two different callbacks to the same place, and the
  client then shows progress for an operation that is not the one running. }
procedure TFitClientViewTest.AndWithAStubItPassesEveryCallbackThrough;
var
    Proxy: TFitServerProxy;
    Stub: TFitClientStub;
begin
    Proxy := TFitServerProxy.Create;
    Stub := TFitClientStub.Create;
    try
        //  The stub forwards to a client; this one is the fixture's, which has a
        //  mock view behind it - so the whole chain from the engine's callback to
        //  the drawing is real except for the widget set.
        Stub.FitClient := FClient;
        Proxy.FitClientStub := Stub;

        //  The callbacks read results back from the engine, so the routes they
        //  read have to answer - the same stubs the refresh tests use. Without
        //  them the failure is a missing reply, not the forwarding under test.
        StubComputedRoutes;
        //  ShowProfile re-reads the profile from the engine rather than reusing
        //  what it holds - the background may have been subtracted since - so
        //  that route has to answer too.
        FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[4,5,6]}');
        FSvc.Reply('state', '{"ok":true,"state":"Finished"}');
        FSvc.Reply('rfactor', '{"ok":true,"rFactor":"0.1","curMin":0.1}');
        TClientForTest(FClient).CallSetExpProfile(AProfileOfTen);

        Proxy.ShowCurMin(0.25);
        Proxy.ShowProfile;
        Proxy.Done;
        Proxy.ComputeCurveBoundsDone;
        Proxy.ComputeBackgroundPointsDone;
        Proxy.ComputeCurvePositionsDone;

        AssertTrue('every callback was accepted', True);
    finally
        //  Objects, not interfaces, and -SIcorba counts neither: freed here in
        //  the order they were made to depend on each other.
        Proxy.FitClientStub := nil;
        Stub.Free;
        Proxy.Free;
    end;
end;

initialization
    //  A unit test: a mocked chart and a mocked server, no socket and no widget set.
    RegisterTest('unit', TFitClientViewTest);
end.
