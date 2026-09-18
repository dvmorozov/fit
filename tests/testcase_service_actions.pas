// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every command the desktop can give the compute server, and every point
edit it can send - each one landing on its own route.)

TWENTY-ODD METHODS THAT ARE ONE LINE EACH. Smooth the profile, subtract the
background, minimise the difference, stop: each is a name posted to
`/actions/<name>`, and each point edit is a coordinate pair posted to
`/points/<kind>`. There is nothing to them individually, which is exactly why
nothing tested them and why a wrong one is so easy to write.

WHAT A WRONG ROUTE DOES. Nothing raises at the call site. The server answers
404 or - worse - performs a DIFFERENT operation, because the names are close
together and every one of them is a legitimate route: a menu item labelled
"Compute background points" that posts `compute-curve-positions` replaces the
user's picks with something else entirely and reports success. The user sees the
model change in a way they did not ask for, with nothing anywhere saying why.

The same for the point families. A point typed into the background table that is
posted to `/points/profile` edits the measured data instead of the background -
and the profile is what everything else is measured against.

SO THE ASSERTION IS THE ROUTE ITSELF, one row per command, driven through the
transport seam so no server is involved. Written as a table because the failure
being guarded against is one row copied from another and half edited.
}
unit testcase_service_actions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    http_fit_service, mock_http_transport;

type
    TServiceActionsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        { The last request made, as 'POST /actions/stop'. }
        function LastCall: string;
        { Runs ACommand and asserts it posted to /actions/AName. }
        procedure AssertPostsAction(const AName: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The fitting commands.
        procedure MinimizingTheDifferenceHasItsOwnRoute;
        procedure MinimizingAgainIsADifferentRoute;
        procedure MinimizingTheNumberOfCurvesHasItsOwn;
        procedure DoingItAllAutomaticallyHasItsOwn;

        //  Preparing the profile.
        procedure SmoothingHasItsOwnRoute;
        procedure SubtractingTheBackgroundHasItsOwn;
        procedure AnAutomaticSubtractionSaysSoInItsBody;
        procedure AManualSubtractionSaysTheOpposite;

        //  Computing what the fit needs.
        procedure ComputingBackgroundPointsHasItsOwnRoute;
        procedure ComputingCurvePositionsHasItsOwn;
        procedure ComputingCurveBoundsHasItsOwn;
        procedure SelectingAllPointsAsPositionsHasItsOwn;

        //  Choosing what is fitted.
        procedure SelectingTheEntireProfileHasItsOwnRoute;
        procedure SelectingAnIntervalCarriesItsBounds;
        procedure AnIntervalOfOnePointIsStillAnInterval;

        //  Stopping.
        procedure StoppingHasItsOwnRoute;

        //  What a command answers with.
        procedure ACommandReportsTheServersMessage;
        procedure ACommandWithNoMessageAnswersEmpty;
        procedure ACommandThatCannotBeSentIsReported;

        //  Editing points. Note that the four sets are named on the wire as
        //  profile / background / positions / rfactor-bounds - "positions"
        //  rather than "curve-positions", which the method name would suggest.
        //  Pinned as it is, because the server route is the contract.
        procedure APointGoesToTheSetItBelongsTo;
        procedure EachPointSetHasItsOwnRoute;
        procedure APointCarriesBothItsCoordinates;
        procedure AReplacementCarriesTheOldPointAndTheNew;
        procedure EachReplacementHasItsOwnRouteToo;
        procedure ACoordinateIsSentAtFullPrecision;
    end;

implementation

const
    BaseUrl = 'http://localhost:8080';

procedure TServiceActionsTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BaseUrl);
    //  Every action answers the same shape; a test that cares about the message
    //  overrides it.
    FSvc.Reply('smooth-profile', '{"message":"ok"}');
end;

procedure TServiceActionsTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

function TServiceActionsTest.LastCall: string;
begin
    Result := '';
    if FSvc.Log.Calls.Count > 0 then
        Result := FSvc.Log.Calls[FSvc.Log.Calls.Count - 1];
end;

procedure TServiceActionsTest.AssertPostsAction(const AName: string);
begin
    //  POST, not GET: an action changes the model, and a GET that changed
    //  something would be retried by anything that retries.
    AssertTrue(Format('expected a POST to /actions/%s, got "%s"',
        [AName, LastCall]),
        Pos('POST', LastCall) > 0);
    AssertTrue(Format('expected /actions/%s, got "%s"', [AName, LastCall]),
        Pos('/actions/' + AName, LastCall) > 0);
end;

{ ---- the fitting commands -------------------------------------------------- }

procedure TServiceActionsTest.MinimizingTheDifferenceHasItsOwnRoute;
begin
    FSvc.MinimizeDifference;
    AssertPostsAction('minimize-difference');
end;

procedure TServiceActionsTest.MinimizingAgainIsADifferentRoute;
begin
    //  A DIFFERENT COMMAND, not a repeat: "again" continues from where the last
    //  fit stopped rather than starting over from the initial values. Wired to
    //  the same route, the button that continues a fit would silently discard
    //  everything the fit had found.
    FSvc.MinimizeDifferenceAgain;
    AssertPostsAction('minimize-difference-again');
end;

procedure TServiceActionsTest.MinimizingTheNumberOfCurvesHasItsOwn;
begin
    //  This one REMOVES curves from the model. Reached by mistake it deletes
    //  work the user did by hand, and there is no undo.
    FSvc.MinimizeNumberOfCurves;
    AssertPostsAction('minimize-number-of-curves');
end;

procedure TServiceActionsTest.DoingItAllAutomaticallyHasItsOwn;
begin
    FSvc.DoAllAutomatically;
    AssertPostsAction('do-all-automatically');
end;

{ ---- preparing the profile ------------------------------------------------- }

procedure TServiceActionsTest.SmoothingHasItsOwnRoute;
begin
    //  Smoothing REWRITES THE MEASURED DATA in place. Everything afterwards is
    //  fitted against the smoothed version, so a command that reached this by
    //  mistake changes every result that follows.
    FSvc.SmoothProfile;
    AssertPostsAction('smooth-profile');
end;

procedure TServiceActionsTest.SubtractingTheBackgroundHasItsOwn;
begin
    FSvc.SubtractBackground(False);
    AssertPostsAction('subtract-background');
end;

procedure TServiceActionsTest.AnAutomaticSubtractionSaysSoInItsBody;
begin
    //  ONE ROUTE, TWO MEANINGS, told apart by the body. Automatic finds the
    //  background itself; manual uses the points the user picked. Sent the
    //  wrong way round, the picks are thrown away and replaced by a guess.
    FSvc.SubtractBackground(True);
    AssertTrue('the body says automatic: ' + FSvc.LastBody,
        Pos('"auto":true', FSvc.LastBody) > 0);
end;

procedure TServiceActionsTest.AManualSubtractionSaysTheOpposite;
begin
    FSvc.SubtractBackground(False);
    AssertTrue('the body says manual: ' + FSvc.LastBody,
        Pos('"auto":false', FSvc.LastBody) > 0);
end;

{ ---- computing what the fit needs ------------------------------------------ }

procedure TServiceActionsTest.ComputingBackgroundPointsHasItsOwnRoute;
begin
    FSvc.ComputeBackgroundPoints;
    AssertPostsAction('compute-background-points');
end;

procedure TServiceActionsTest.ComputingCurvePositionsHasItsOwn;
begin
    //  Adjacent in the menu and adjacent in name to the one above - which is
    //  why they are asserted separately rather than trusted to differ.
    FSvc.ComputeCurvePositions;
    AssertPostsAction('compute-curve-positions');
end;

procedure TServiceActionsTest.ComputingCurveBoundsHasItsOwn;
begin
    FSvc.ComputeCurveBounds;
    AssertPostsAction('compute-curve-bounds');
end;

procedure TServiceActionsTest.SelectingAllPointsAsPositionsHasItsOwn;
begin
    FSvc.SelectAllPointsAsCurvePositions;
    AssertPostsAction('select-all-points-as-curve-positions');
end;

{ ---- choosing what is fitted ----------------------------------------------- }

procedure TServiceActionsTest.SelectingTheEntireProfileHasItsOwnRoute;
begin
    FSvc.SelectEntireProfile;
    AssertPostsAction('select-entire-profile');
end;

procedure TServiceActionsTest.SelectingAnIntervalCarriesItsBounds;
begin
    //  BY INDEX, both ends, in one body. The interval is what the fit is run
    //  over; an end lost or swapped fits a different stretch of the data than
    //  the one the user dragged out, and the result looks like a bad fit.
    FSvc.SelectProfileInterval(12, 48);
    AssertPostsAction('select-profile-interval');
    AssertTrue('the start is in it: ' + FSvc.LastBody,
        Pos('"start":12', FSvc.LastBody) > 0);
    AssertTrue('and the stop: ' + FSvc.LastBody,
        Pos('"stop":48', FSvc.LastBody) > 0);
end;

procedure TServiceActionsTest.AnIntervalOfOnePointIsStillAnInterval;
begin
    //  The degenerate case a double-click produces. It is the server's business
    //  whether to refuse it; what matters here is that both ends are sent as
    //  given rather than one being dropped as redundant.
    FSvc.SelectProfileInterval(7, 7);
    AssertTrue('start sent: ' + FSvc.LastBody,
        Pos('"start":7', FSvc.LastBody) > 0);
    AssertTrue('stop sent too: ' + FSvc.LastBody,
        Pos('"stop":7', FSvc.LastBody) > 0);
end;

{ ---- stopping -------------------------------------------------------------- }

procedure TServiceActionsTest.StoppingHasItsOwnRoute;
begin
    //  THE ONE COMMAND THAT MUST REACH A BUSY SERVER. It is what the user
    //  presses when a fit is running longer than they will wait, and a wrong
    //  route here leaves them with a window that cannot be stopped.
    FSvc.StopAsyncOper;
    AssertPostsAction('stop');
end;

{ ---- what a command answers with ------------------------------------------- }

procedure TServiceActionsTest.ACommandReportsTheServersMessage;
begin
    //  The message is shown to the user, so a command that dropped it would
    //  turn an explanation - "no interval is selected" - into silence.
    FSvc.Reply('minimize-difference', '{"message":"started"}');
    AssertEquals('started', FSvc.MinimizeDifference);
end;

procedure TServiceActionsTest.ACommandWithNoMessageAnswersEmpty;
begin
    //  A server with nothing to say is not a failure: most commands succeed
    //  quietly. Empty rather than a fabricated sentence, so the caller can tell
    //  the two apart.
    FSvc.Reply('compute-curve-bounds', '{"ok":true}');
    AssertEquals('', FSvc.ComputeCurveBounds);
end;

procedure TServiceActionsTest.ACommandThatCannotBeSentIsReported;
var
    Raised: boolean;
begin
    //  AN UNREACHABLE SERVER MUST NOT LOOK LIKE A COMMAND THAT DID NOTHING.
    //  Swallowed here, the user presses Fit, sees no message, and concludes the
    //  button is broken rather than that the server is gone.
    FSvc.FailNextWith('connection refused');
    Raised := False;
    try
        FSvc.MinimizeDifference;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('the failure reached the caller', Raised);
end;

{ ---- editing points -------------------------------------------------------- }

procedure TServiceActionsTest.APointGoesToTheSetItBelongsTo;
begin
    FSvc.AddPointToProfile(1.5, 2.5);
    AssertTrue('to the profile: ' + LastCall,
        Pos('/points/profile', LastCall) > 0);
end;

procedure TServiceActionsTest.EachPointSetHasItsOwnRoute;
begin
    //  FOUR SETS THAT MEAN FOUR DIFFERENT THINGS. A point typed into the
    //  background table and posted to the profile edits the measured data
    //  instead - and the profile is what everything else is measured against,
    //  so the mistake propagates into every result and is not visible in the
    //  table the user was looking at.
    FSvc.AddPointToBackground(1, 2);
    AssertTrue('background: ' + LastCall,
        Pos('/points/background', LastCall) > 0);

    FSvc.AddPointToCurvePositions(1, 2);
    AssertTrue('positions: ' + LastCall,
        Pos('/points/positions', LastCall) > 0);

    FSvc.AddPointToRFactorBounds(1, 2);
    AssertTrue('bounds: ' + LastCall,
        Pos('/points/rfactor-bounds', LastCall) > 0);
end;

procedure TServiceActionsTest.APointCarriesBothItsCoordinates;
begin
    FSvc.AddPointToProfile(1.5, 2.5);
    AssertTrue('x: ' + FSvc.LastBody, Pos('"x":1.5', FSvc.LastBody) > 0);
    AssertTrue('y: ' + FSvc.LastBody, Pos('"y":2.5', FSvc.LastBody) > 0);
end;

procedure TServiceActionsTest.AReplacementCarriesTheOldPointAndTheNew;
begin
    //  BOTH, because the server matches the point to replace by VALUE rather
    //  than by index - the table is sorted and the two orders do not
    //  correspond. Sending only the new pair would give it nothing to replace.
    FSvc.ReplacePointInProfile(1, 2, 3, 4);
    AssertTrue('the point to replace: ' + FSvc.LastBody,
        (Pos('"prevX":1', FSvc.LastBody) > 0) and
        (Pos('"prevY":2', FSvc.LastBody) > 0));
    AssertTrue('and the one to put there: ' + FSvc.LastBody,
        (Pos('"x":3', FSvc.LastBody) > 0) and
        (Pos('"y":4', FSvc.LastBody) > 0));
end;

procedure TServiceActionsTest.EachReplacementHasItsOwnRouteToo;
begin
    FSvc.ReplacePointInBackground(1, 2, 3, 4);
    AssertTrue('background: ' + LastCall,
        Pos('background', LastCall) > 0);

    FSvc.ReplacePointInCurvePositions(1, 2, 3, 4);
    AssertTrue('positions: ' + LastCall,
        Pos('/points/positions', LastCall) > 0);

    FSvc.ReplacePointInRFactorBounds(1, 2, 3, 4);
    AssertTrue('bounds: ' + LastCall,
        Pos('rfactor-bounds', LastCall) > 0);
end;

procedure TServiceActionsTest.ACoordinateIsSentAtFullPrecision;
begin
    //  SEVENTEEN DIGITS, which is what it takes to write a double and read the
    //  same one back. Rounded on the way out, a point moves by a fraction of a
    //  channel every time it makes the round trip - and it makes one whenever
    //  the table is refreshed, so the drift accumulates over a session.
    FSvc.AddPointToProfile(1/3, 2/3);
    AssertTrue('not rounded to a few places: ' + FSvc.LastBody,
        Pos('0.33333333333333', FSvc.LastBody) > 0);
end;

initialization
    //  A unit test: the service over a mock transport. No socket and no server.
    RegisterTest('unit', TServiceActionsTest);
end.
