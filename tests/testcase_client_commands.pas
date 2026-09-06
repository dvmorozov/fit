// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The commands that change the data in place, and what the client
re-reads afterwards.)

TWO KINDS OF COMMAND. Most of them hand the work to a thread and come back at
once - those are the fits, and they are driven elsewhere. The three here change
the profile SYNCHRONOUSLY, and each has to leave the screen showing what the
server now holds rather than what it held a moment ago.

THE RULE THEY ALL FOLLOW IS DROP AND RE-READ. The client does not adjust its copy
to match what it thinks the command did; it throws the copy away and asks. That
is not caution for its own sake - smoothing rewrites every ordinate, subtracting
a background rewrites them again, and a client that patched its own copy would
be guessing at arithmetic the server has already done. The two would agree until
they did not, and the disagreement would show as a fit that does not match the
picture.

SUBTRACTING A BACKGROUND HAS A DIRECTION THE OTHERS DO NOT. Automatic finds the
background itself; manual uses the points the user picked, which live on THIS
side and have to be sent up first. Sent in the wrong case, the user's picks are
either ignored - the program silently guesses instead - or a guess overwrites
work they did by hand.

AND WHAT IS ON SCREEN AFTERWARDS DEPENDS ON WHERE THE USER WAS. With an interval
selected, the profile they are looking at is that interval, so it is the interval
that is re-read; otherwise it is the whole profile. Re-reading the wrong one
leaves the chart showing a stretch of data the user did not choose.
}
unit testcase_client_commands;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport,
    title_points_set, points_set;

type
    TTestableCommandClient = class(TFitClient)
    public
        { The state the window reads to decide whether the fit menu is live. }
        procedure PretendAnAreaIsSelected;
        function ProfilePointCount: longint;
        function HasBackgroundPoints: boolean;
    end;

    TClientCommandsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TTestableCommandClient;
        { The routes a command re-reads through. }
        procedure StubTheProfileRoutes;
        procedure GivenPickedBackgroundPoints;
        procedure GivenPicksAndBounds;
        function CallsTo(const APath: string): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Smoothing.
        procedure SmoothingIsSentToTheServer;
        procedure TheProfileIsReReadAfterwards;
        procedure AndRedrawn;

        //  Which stretch of data is re-read.
        procedure WithNoIntervalTheWholeProfileComesBack;
        procedure WithAnIntervalSelectedTheIntervalComesBack;

        //  Subtracting a background.
        procedure AnAutomaticSubtractionSendsNoPicks;
        procedure AManualSubtractionSendsThePicksFirst;
        procedure TheSubtractionItselfIsSentEitherWay;
        procedure ThePicksAreTakenOffTheChartAfterwards;
        procedure AndTheProfileIsReRead;

        //  What the panels read the model from.
        procedure TheDisplayCopiesAreTheClientsOwn;
        procedure AndAreEmptyBeforeAnythingIsRead;

        //  Removing one curve.
        procedure DeletingACurveByHandleReachesTheService;
        //  Moving a pick.
        procedure MovingAPickReachesTheService;
        procedure AndTheModelIsReReadAfterIt;
        procedure MovingAnIntervalBoundReachesItToo;

        procedure AnEmptyHandleIsNotAskedAbout;
        procedure AHandleTheModelHasNotGotAnswersFalse;

        //  Stopping, and building the curve list.
        procedure StoppingIsSentAndNothingIsReadBack;
        procedure BuildingTheCurveListIsSent;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TTestableCommandClient.PretendAnAreaIsSelected;
begin
    FSelectedAreaMode := True;
end;

function TTestableCommandClient.ProfilePointCount: longint;
begin
    Result := 0;
    if Assigned(FExperimentalProfile) then
        Result := FExperimentalProfile.PointsCount;
end;

function TTestableCommandClient.HasBackgroundPoints: boolean;
begin
    Result := Assigned(FBackgroundPoints);
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TClientCommandsTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TTestableCommandClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TClientCommandsTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TClientCommandsTest.StubTheProfileRoutes;
begin
    //  DIFFERENT POINT COUNTS, so re-reading the wrong stretch of data is
    //  visible rather than plausible: three points for the whole profile, two
    //  for the selected interval.
    FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('selected-interval', '{"title":"i","x":[1,2],"y":[1,2]}');
    FSvc.Reply('smooth-profile', '{"message":"ok"}');
    FSvc.Reply('subtract-background', '{"message":"ok"}');
end;

procedure TClientCommandsTest.GivenPickedBackgroundPoints;
begin
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
end;

procedure TClientCommandsTest.GivenPicksAndBounds;
begin
    StubTheProfileRoutes;
    FSvc.Reply('positions', '{"title":"p","x":[10,20],"y":[1,2]}');
    FSvc.Reply('rfactor-bounds', '{"title":"b","x":[5,25],"y":[1,1]}');
    //  A WHOLE curve object, because the move ends in a refresh and the refresh
    //  parses the attributes.
    FSvc.Reply('curves', '{"ok":true,"curves":[{"id":"{C}",' +
        '"params":[{"name":"x0","value":10,"type":2,"error":-1}]}]}');
    //  The path that hands the client both pick sets, which is the state a grid
    //  edit is made from.
    FClient.ComputeCurvePositionsDone;
end;

function TClientCommandsTest.CallsTo(const APath: string): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
        if Pos(APath, FSvc.Log.Calls[i]) > 0 then
            Inc(Result);
end;

{ ---- smoothing ------------------------------------------------------------- }

procedure TClientCommandsTest.SmoothingIsSentToTheServer;
begin
    //  The server holds the data; smoothing a local copy would leave the fit
    //  running against the rough version.
    StubTheProfileRoutes;
    FClient.SmoothProfile;
    AssertTrue('the command went out: ' + FSvc.Log.AsText,
        Pos('smooth-profile', FSvc.Log.AsText) > 0);
end;

procedure TClientCommandsTest.TheProfileIsReReadAfterwards;
begin
    //  DROPPED AND RE-READ, not patched. Smoothing rewrites every ordinate, and
    //  a client that adjusted its own copy would be guessing at arithmetic the
    //  server has already done - the two would agree until they did not.
    StubTheProfileRoutes;
    FClient.SmoothProfile;
    AssertEquals('the server''s profile is what is held', 3,
        FClient.ProfilePointCount);
end;

procedure TClientCommandsTest.AndRedrawn;
begin
    //  A re-read that is not replotted leaves the chart showing the rough data
    //  while the fit uses the smoothed - which reads as a fit that ignores the
    //  data.
    StubTheProfileRoutes;
    FView.Log.Clear;
    FClient.SmoothProfile;
    AssertTrue('the profile was drawn again',
        FView.Plotted('PlotExpProfile'));
end;

{ ---- which stretch of data is re-read -------------------------------------- }

procedure TClientCommandsTest.WithNoIntervalTheWholeProfileComesBack;
begin
    StubTheProfileRoutes;
    FClient.SmoothProfile;
    AssertEquals('all three points', 3, FClient.ProfilePointCount);
end;

procedure TClientCommandsTest.WithAnIntervalSelectedTheIntervalComesBack;
begin
    //  WHAT THE USER IS LOOKING AT. With an interval selected, the profile on
    //  screen IS that interval - so re-reading the whole thing would replace
    //  their chosen stretch with the lot, and the next pick would be made
    //  against data they had deliberately excluded.
    StubTheProfileRoutes;
    FView.Log.Clear;
    FClient.PretendAnAreaIsSelected;
    FClient.SmoothProfile;
    AssertTrue('the interval was drawn, not the whole profile',
        FView.Plotted('PlotSelectedProfileInterval'));
    AssertFalse('and the whole profile was not',
        FView.Plotted('PlotExpProfile'));
end;

{ ---- subtracting a background ---------------------------------------------- }

procedure TClientCommandsTest.AnAutomaticSubtractionSendsNoPicks;
begin
    //  AUTOMATIC MEANS THE SERVER FINDS IT. Sending the picks as well would
    //  have the user's points overwritten by a guess without anything saying
    //  so.
    StubTheProfileRoutes;
    GivenPickedBackgroundPoints;
    FSvc.Log.Clear;
    FClient.SubtractBackground(True);
    AssertEquals('the picks stayed here', 0, CallsTo('/background'));
end;

procedure TClientCommandsTest.AManualSubtractionSendsThePicksFirst;
begin
    //  THE PICKS LIVE ON THIS SIDE. Not sent, the server subtracts whatever it
    //  had - or nothing - and the minute the user spent picking is discarded
    //  silently.
    StubTheProfileRoutes;
    GivenPickedBackgroundPoints;
    FSvc.Log.Clear;
    FClient.SubtractBackground(False);
    AssertTrue('the picks went up: ' + FSvc.Log.AsText,
        CallsTo('/background') > 0);
end;

procedure TClientCommandsTest.TheSubtractionItselfIsSentEitherWay;
begin
    StubTheProfileRoutes;
    GivenPickedBackgroundPoints;
    FSvc.Log.Clear;
    FClient.SubtractBackground(True);
    AssertTrue('automatic: ' + FSvc.Log.AsText,
        Pos('subtract-background', FSvc.Log.AsText) > 0);

    GivenPickedBackgroundPoints;
    FSvc.Log.Clear;
    FClient.SubtractBackground(False);
    AssertTrue('and manual: ' + FSvc.Log.AsText,
        Pos('subtract-background', FSvc.Log.AsText) > 0);
end;

procedure TClientCommandsTest.ThePicksAreTakenOffTheChartAfterwards;
begin
    //  THE MARKS THAT BUILT SOMETHING MUST NOT SIT ON TOP OF IT. The background
    //  has been subtracted; the points that described it now describe a curve
    //  through data that no longer has it, and leaving them drawn invites the
    //  user to subtract again.
    StubTheProfileRoutes;
    GivenPickedBackgroundPoints;
    AssertTrue('there were picks', FClient.HasBackgroundPoints);
    FClient.SubtractBackground(True);
    AssertFalse('and now there are none', FClient.HasBackgroundPoints);
end;

procedure TClientCommandsTest.AndTheProfileIsReRead;
begin
    //  The whole point of the command is that the data changed. A client that
    //  kept its copy would show the background still there.
    StubTheProfileRoutes;
    GivenPickedBackgroundPoints;
    FView.Log.Clear;
    FClient.SubtractBackground(True);
    AssertEquals('the server''s profile', 3, FClient.ProfilePointCount);
    AssertTrue('drawn again', FView.Plotted('PlotExpProfile'));
end;

{ ---- stopping, and building the curve list --------------------------------- }

procedure TClientCommandsTest.AndAreEmptyBeforeAnythingIsRead;
begin
    //  Nothing read yet. The Model panel and the parameter table are refreshed
    //  from a poll, so they meet this state on every start-up and must not
    //  fault on it.
    AssertTrue('no curves yet', not Assigned(FClient.CurvesForDisplay));
    AssertTrue('no attributes yet',
        not Assigned(FClient.CurveAttributesForDisplay));
end;

procedure TClientCommandsTest.TheDisplayCopiesAreTheClientsOwn;
var
    First, Second: TObject;
begin
    StubTheProfileRoutes;
    FSvc.Reply('curves',
        '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}",' +
        '"params":[{"name":"x0","value":1,"type":2,"error":-1}]}]}');
    FClient.UpdateComputedData(True);

    First := FClient.CurvesForDisplay;
    AssertTrue('the curves are there', Assigned(First));
    AssertTrue('and their attributes',
        Assigned(FClient.CurveAttributesForDisplay));

    //  THE SAME OBJECT each time, which is the contract: read-only, not the
    //  caller''s to free, and replaced wholesale on the next refresh. A copy per
    //  call would be a copy per poll, twice a second, of every curve.
    Second := FClient.CurvesForDisplay;
    AssertSame('handed back, not rebuilt', First, Second);
end;

procedure TClientCommandsTest.DeletingACurveByHandleReachesTheService;
begin
    StubTheProfileRoutes;
    //  A WHOLE curve object: deleting one ends with a refresh, and the refresh
    //  parses the attributes, so a reply carrying only a handle is not a model
    //  this client can read back.
    FSvc.Reply('curves', '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}",' +
        '"params":[{"name":"x0","value":1,"type":2,"error":-1}]}]}');
    AssertTrue('the curve was removed',
        FClient.DeleteCurve('{11111111-1111-1111-1111-111111111111}'));
    //  THE SERVICE OWNS THE MODEL. TFitClient.RemoveCurvePositions - the only
    //  removal this client had - frees a local object and tells the server
    //  nothing, which is why the rows came back on the next refresh.
    AssertTrue('and the request went out: ' + FSvc.Log.AsText,
        Pos('/points/positions/', FSvc.Log.AsText) > 0);
    //  AND THE CHART WAS TOLD, by handle.
    //
    //  WHY THIS IS ASSERTED SEPARATELY from the request. The re-plot that
    //  follows takes the framework's own curve series off anyway, and a
    //  contributor with a redraw hook replots its own markers - so the chart
    //  looks right either way, and the one case that does not is a contributor
    //  drawing per-curve series with no hook. Nothing but this call covers it,
    //  and nothing else would notice if it were dropped.
    AssertTrue('the view was told which curve went',
        FView.Plotted('HideSeriesOwnedBy:' +
            '{11111111-1111-1111-1111-111111111111}'));
end;

{ ---- moving a pick -------------------------------------------------------- }

procedure TClientCommandsTest.MovingAPickReachesTheService;
begin
    //  A LOCAL EDIT WAS ALL THIS DID. It moved the point in the client's own
    //  set, redrew it, and told nobody - so the next refresh replaced the set
    //  with the server's unchanged one and the pick jumped back. Worse than a
    //  refusal, because the chart showed the move first.
    //
    //  A PICK IS MODEL INPUT: the curve at that position is seeded from it, so
    //  a move the service never heard about is a curve fitted at the old place.
    GivenPicksAndBounds;
    FSvc.Log.Clear;
    FClient.ReplacePointInCurvePositions(10, 1, 12, 3);
    AssertTrue('the move went out: ' + FSvc.Log.AsText,
        CallsTo('/points/positions') > 0);
end;

procedure TClientCommandsTest.AndTheModelIsReReadAfterIt;
begin
    //  UNLIKE ITS PROFILE AND BACKGROUND SIBLINGS, which send and stop. Moving
    //  a pick moves the curve that pick seeds, so the curves, their attributes
    //  and the fitted positions are all stale - the same reason
    //  AddPointToCurvePositions re-reads.
    GivenPicksAndBounds;
    FSvc.Log.Clear;
    FClient.ReplacePointInCurvePositions(10, 1, 12, 3);
    AssertTrue('the model came back: ' + FSvc.Log.AsText,
        CallsTo('/curves') > 0);
end;

procedure TClientCommandsTest.MovingAnIntervalBoundReachesItToo;
begin
    //  THE SAME OMISSION, in the sibling nobody had looked at. An interval
    //  bound decides which stretch the fit is scored over, so a move kept
    //  locally scores the fit over a window the user is no longer shown.
    GivenPicksAndBounds;
    FSvc.Log.Clear;
    FClient.ReplacePointInRFactorBounds(5, 1, 6, 1);
    AssertTrue('the move went out: ' + FSvc.Log.AsText,
        CallsTo('/points/rfactor-bounds') > 0);
end;

procedure TClientCommandsTest.AnEmptyHandleIsNotAskedAbout;
begin
    StubTheProfileRoutes;
    //  Nothing selected. The ordinary state of a panel with no row chosen, so
    //  it answers False rather than faulting - and asks the service nothing.
    AssertFalse('no handle, no deletion', FClient.DeleteCurve(''));
    //  AND THE CHART IS NOT TOUCHED. An empty handle owns every model-wide
    //  series - the profile, the difference, the background - so a call made
    //  with one and answered literally would clear the chart.
    AssertFalse('nothing was taken off the chart',
        FView.Plotted('HideSeriesOwnedBy:'));
end;

procedure TClientCommandsTest.AHandleTheModelHasNotGotAnswersFalse;
begin
    StubTheProfileRoutes;
    FSvc.Reply('curves', '{"ok":true,"curves":[{"id":"{11111111-1111-1111-1111-111111111111}",' +
        '"params":[{"name":"x0","value":1,"type":2,"error":-1}]}]}');
    //  NOT A FAULT: the same curve may have been deleted twice, or a fit may
    //  have removed it in between. The window says so once and clears its
    //  selection.
    AssertFalse('already gone',
        FClient.DeleteCurve('{99999999-9999-9999-9999-999999999999}'));
    //  Nor here: nothing was deleted, so nothing should leave the chart.
    AssertFalse('and the chart was left alone',
        FView.Plotted('HideSeriesOwnedBy:' +
            '{99999999-9999-9999-9999-999999999999}'));
end;

procedure TClientCommandsTest.StoppingIsSentAndNothingIsReadBack;
begin
    //  NOTHING IS RE-READ HERE, deliberately: the running operation finishes in
    //  its own time and comes back through the usual completion callback.
    //  Reading the profile now would show a half-finished fit as if it were the
    //  answer.
    FSvc.Reply('stop', '{"message":"ok"}');
    FSvc.Log.Clear;
    FClient.StopAsyncOper;
    AssertTrue('the stop went out: ' + FSvc.Log.AsText,
        Pos('/actions/stop', FSvc.Log.AsText) > 0);
    AssertEquals('and nothing was read back', 0, CallsTo('/profile'));
end;

procedure TClientCommandsTest.BuildingTheCurveListIsSent;
begin
    FSvc.Reply('create-curve-list', '{"message":"ok"}');
    FSvc.Log.Clear;
    FClient.CreateCurveList;
    AssertTrue('the command went out: ' + FSvc.Log.AsText,
        Pos('create-curve-list', FSvc.Log.AsText) > 0);
end;

initialization
    //  A unit test: the client over a mock transport and a mock viewer. The
    //  commands here are the SYNCHRONOUS ones - the fits hand their work to a
    //  thread, and driving those is not a unit test.
    RegisterTest('unit', TClientCommandsTest);
end.
