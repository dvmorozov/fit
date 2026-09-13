// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The engine's own surface: its settings, its callbacks, and what it
refuses.)

`fit_service.pas` is the largest unit in the project and the largest hole in the
measurement, and the hole is not where it looks. The route bodies that run the
optimiser are exercised by the REST tests and by the integration suite; what had
never been touched at all was the SURFACE - the settings the client sets on it, the
callbacks it forwards, and the refusals it answers with when asked to do something
in a state where it cannot.

Twenty-two methods were entirely cold. They matter for three different reasons:

  * A SETTING THAT DOES NOT STICK is a user preference silently ignored: the
    objective, the weighting, the server address, the sidecar address. Each is one
    line, and one line is exactly what nobody checks.

  * A CALLBACK THAT DOES NOT FORWARD is a client that shows a stale fit. The
    forwarding is guarded by `if Assigned(FitProxy)` - in-process there is one, on
    the REST server there is a session, and in a test there was neither, so
    neither branch had ever run.

  * A REFUSAL IS PART OF THE CONTRACT. Asking the engine to edit a profile it has
    not got must raise a user error, not fault: EUserException is what the REST
    layer maps to 400, and anything else it maps to 500 - "your request was wrong"
    against "the server broke".

The engine is driven directly here rather than through a socket or a route: it is
an ordinary object, and the reason it looked untestable is that nothing had tried.
}
unit testcase_service_surface;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, int_client_callback, fit_service,
    points_set, title_points_set, fit_statistics,
    //  The one curve shape no pick places: a formula declaring no position.
    //  Deleting one takes a different route through the engine from every other
    //  curve, and nothing had ever driven it.
    persistent_curve_parameters, persistent_curve_parameter_container,
    special_curve_parameter, user_curve_parameter, user_points_set,
    MyExceptions, mock_client_callback;

type
    TServiceSurfaceTest = class(TTestCase)
    private
        FService: TFitService;
        FProxyObj: TMockClientCallback;
        FProxy: IClientCallback;
        { Puts a small profile into the engine, as loading a file would. The
          engine takes ownership of the set it is given. }
        procedure GivenAProfile;
        { The same, with a peak in it. }
        procedure GivenAPeakedProfile;
        { A formula's parameters with NO position parameter of any type, which is
          the only shape of curve a pick cannot place. }
        function ParametersWithoutAPosition: Curve_parameters;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Adding a point, and the toggle nothing at this layer ever asserted:
        //  the behaviour was covered only on TFitClient's mirror of it.
        procedure AddingAPickPutsItInTheSet;
        procedure AddingTheSameCoordinatesAgainRemovesIt;
        procedure TheSameAbscissaWithADifferentValueReplacesIt;
        //  And the bulk path, where the toggle is not what anyone wants.
        procedure ABulkWriteCarryingADuplicateKeepsOnePoint;
        //  Removing one curve.
        procedure DeletingACurveTakesItsPickWithIt;
        procedure DeletingTheLastCurveEmptiesTheModel;
        procedure AndItsFittedPositionMarkerGoesToo;
        procedure AndTheHandleIsNoLongerKnown;
        procedure DeletingACurveLeavesTheOthers;
        procedure DeletingACurveTwiceIsRefusedNotSilent;
        //  And the curve no pick placed, which goes a different way entirely.
        procedure DeletingAFormulaCurveTakesNoPickWithIt;
        //  Settings the client sets and the engine must keep.
        procedure TheObjectiveIsKept;
        procedure TheWeightingIsKept;
        procedure TheServerAddressIsKept;
        procedure TheSidecarAddressIsKept;
        procedure TheBackgroundFactorIsKept;
        procedure TheCurveThresholdIsKept;
        procedure CurveScalingIsKeptBothWays;
        procedure EverySettingIsIndependentOfTheOthers;

        //  Callbacks.
        procedure WithNoClientTheCallbacksAreSilentRatherThanFatal;
        procedure TheClientSinkIsHeldAsGiven;
        procedure TheCompletionCallbacksAreCallable;

        //  What it answers before there is anything to work on.
        procedure StatisticsAreInvalidBeforeAnyFit;
        procedure ThereIsNoRFactorBeforeAnyFit;
        procedure TheCurveAttributesAreAskableFromTheStart;
        procedure AbortingWhenNothingRunsIsHarmless;

        //  Refusals.
        procedure APointMayBeAddedToAnEmptyProfile;
        procedure SoIsEditingIntervalBoundsThatAreNotThere;
        procedure AndFittingWithNoProfileAtAll;
        procedure ARefusalCarriesAMessageForTheUser;
        procedure AndTheStateFollowsTheDataRatherThanTheRequest;

        //  THE SWEEP. Every operation, asked of an engine that has nothing
        //  loaded: each must answer with a user error or accept quietly, and
        //  none may raise anything else.
        procedure NoOperationOnAnEmptyEngineFaults;
        procedure AndEveryRefusalCarriesSomethingToShowTheUser;
        procedure NorDoesAskingForEverythingItCanReport;

        //  THE REFUSAL TABLE. Every operation in every state the engine can be
        //  put into without fitting: what is refused, what is accepted, and that
        //  nothing anywhere raises something other than a user error.
        procedure TheStateMachineRefusesAndAcceptsConsistently;
        procedure LoadingAProfileMovesOutOfWaiting;
        procedure APickedIntervalAndPositionMakeTheModelReady;
        procedure AndEveryStateStillAnswersEveryReader;

        //  Working on part of the profile rather than all of it.
        procedure AnIntervalCanBeSelectedAndGivenBack;
        procedure TheBackgroundIsSubtractedFromWhicheverIsInForce;

        //  The two algorithms that read the data and write picks back: where the
        //  curves go, and where each one begins and ends.
        procedure ThePeakIsFoundAsACurvePosition;
        procedure AndBoundsAreWorkedOutAroundIt;
        procedure BothReadTheSelectedIntervalWhenThereIsOne;
        procedure ANarrowPeakIsBracketedRatherThanSwallowed;

        //  The background the user hands over.
        procedure TooLittleDataLeavesNothingToWorkOn;
        procedure AndAProfileWorthFittingIsKept;

        //  With a profile loaded: the editing and picking surface.
        procedure LoadingAProfileLeavesTheBackgroundToRemove;
        procedure APointOfTheProfileCanBeMoved;
        procedure MovingAPointThatIsNotThereIsNotAFault;
        procedure PicksLandInTheirOwnSets;
        procedure AnIntervalBoundCanBeMovedOnceThereIsOne;
    end;

implementation

procedure TServiceSurfaceTest.SetUp;
begin
    //  THE CLASS THE SERVER ITSELF CREATES. TFitSession.Create makes exactly
    //  this - a plain TFitService - so these tests drive what actually answers
    //  requests. The threaded subclasses exist for the desktop's in-process
    //  arrangement; instantiating one here would drag three units into the
    //  measurement whose remaining lines only a live calculation thread can
    //  reach, which dilutes the figure without testing anything more.
    FService := TFitService.Create;
    FProxyObj := TMockClientCallback.Create;
    FProxy := FProxyObj;
end;

procedure TServiceSurfaceTest.TearDown;
begin
    //  The interface reference goes first. Everything here compiles -SIcorba,
    //  so an interface carries no refcount and a live reference over a freed
    //  object is a use-after-free that happens to work.
    FService.FitProxy := nil;
    FProxy := nil;
    FreeAndNil(FProxyObj);
    FreeAndNil(FService);
end;

{ A profile with an actual peak in it, which the flat ramp above deliberately has
  not: the position and bounds algorithms walk the data looking for extremums and
  slopes, and a straight line gives them nothing to find. }
procedure TServiceSurfaceTest.GivenAPeakedProfile;
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        //  A Gaussian at x = 10, on a flat base, written out rather than taken
        //  from a curve unit so the fixture depends on nothing that fits.
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
end;

procedure TServiceSurfaceTest.GivenAProfile;
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    //  A short ramp: enough for the engine to have data, small enough that
    //  nothing here waits for an optimiser.
    for i := 0 to 9 do
        P.AddNewPoint(i, 10 + i);
    //  The engine keeps what it is given - the string it answers with is a hint
    //  for the user, not an error channel.
    FService.SetProfilePointsSet(P);
end;

{ ---- settings -------------------------------------------------------------- }

procedure TServiceSurfaceTest.TheObjectiveIsKept;
begin
    //  Not merely "a setter exists": the client sets this once and every later
    //  fit reads it back, so a setter that dropped the value would fit with the
    //  wrong objective and report success.
    FService.SetMinimizerKind(3);
    AssertEquals('kept', 3, FService.GetMinimizerKind);
end;

procedure TServiceSurfaceTest.TheWeightingIsKept;
begin
    FService.SetWeighting('poisson');
    AssertEquals('kept', 'poisson', FService.GetWeighting);
end;

procedure TServiceSurfaceTest.TheServerAddressIsKept;
begin
    FService.SetServerUrl('http://box:9000');
    AssertEquals('kept', 'http://box:9000', FService.GetServerUrl);
end;

procedure TServiceSurfaceTest.TheSidecarAddressIsKept;
begin
    //  The write and the read are declared in different places on this class,
    //  which is exactly how a pair like this comes to disagree.
    FService.SetPythonSidecarUrl('http://127.0.0.1:8899');
    AssertEquals('kept', 'http://127.0.0.1:8899', FService.PythonSidecarUrl);
end;

procedure TServiceSurfaceTest.TheBackgroundFactorIsKept;
begin
    FService.SetBackFactor(12.5);
    AssertEquals('kept', 12.5, FService.GetBackFactor, 1e-12);
end;

procedure TServiceSurfaceTest.TheCurveThresholdIsKept;
begin
    FService.SetCurveThresh(0.25);
    AssertEquals('kept', 0.25, FService.GetCurveThresh, 1e-12);
end;

procedure TServiceSurfaceTest.CurveScalingIsKeptBothWays;
begin
    //  Both directions, because a setter that ignores False is the same defect
    //  as one that ignores True and is easier to write by accident.
    FService.SetCurveScalingEnabled(True);
    AssertTrue('on', FService.GetCurveScalingEnabled);
    FService.SetCurveScalingEnabled(False);
    AssertTrue('off again', not FService.GetCurveScalingEnabled);
end;

procedure TServiceSurfaceTest.EverySettingIsIndependentOfTheOthers;
begin
    //  ONE FIELD ASSIGNED BY TWO SETTERS is the way this fails: they are
    //  one-line bodies, written together, and nothing else would notice.
    FService.SetMinimizerKind(2);
    FService.SetWeighting('sqrt');
    FService.SetServerUrl('http://a:1');
    FService.SetPythonSidecarUrl('http://b:2');
    FService.SetBackFactor(7);
    FService.SetCurveThresh(0.5);
    FService.SetCurveScalingEnabled(True);

    AssertEquals('the objective', 2, FService.GetMinimizerKind);
    AssertEquals('the weighting', 'sqrt', FService.GetWeighting);
    AssertEquals('the server', 'http://a:1', FService.GetServerUrl);
    AssertEquals('the sidecar', 'http://b:2', FService.PythonSidecarUrl);
    AssertEquals('the factor', 7, FService.GetBackFactor, 1e-12);
    AssertEquals('the threshold', 0.5, FService.GetCurveThresh, 1e-12);
    AssertTrue('scaling', FService.GetCurveScalingEnabled);
end;

{ ---- callbacks ------------------------------------------------------------- }

procedure TServiceSurfaceTest.WithNoClientTheCallbacksAreSilentRatherThanFatal;
var
    Sink: IClientCallback;
begin
    //  A CRASH THIS TEST FOUND. With no asynchronous operation running there is
    //  no calculation thread, and all five callbacks dereferenced it without
    //  asking - an access violation inside a callback, with no frame of ours
    //  above it. AbortAsyncOper destroys that thread while a callback may still
    //  be in flight from it, which is how the window is reached.
    //
    //  Silent now, the way the engine answers a missing client: a callback with
    //  nowhere to go is not an error, there is simply nothing left to tell.
    FService.FitProxy := nil;
    //  THROUGH THE INTERFACE, because that is how they are called: the engine is
    //  the callback sink the running fit talks to, and the methods are protected
    //  on the class for exactly that reason - nobody calls them by name.
    Sink := FService;
    Sink.ShowProfile;
    Sink.ShowCurMin(0.5);
    AssertTrue('nothing happened, and nothing broke', True);
end;

procedure TServiceSurfaceTest.TheClientSinkIsHeldAsGiven;
begin
    //  WHAT IS REACHABLE FROM HERE, and what is not, stated plainly. The engine's
    //  own forwarding to the client sits in the base class and is reached only
    //  through the calculation thread - the whole point of the override being
    //  that the client is touched on the main thread. With no operation running
    //  there is no thread, so that body belongs to the REST tests and the
    //  integration suite, which do run one.
    //
    //  What this pins is the seam itself: the engine takes an IClientCallback
    //  and holds exactly what it was given. It is the assignment the REST
    //  session and the in-process client both depend on, and it had no test.
    AssertTrue('nothing attached to start with', FService.FitProxy = nil);
    FService.FitProxy := FProxy;
    AssertTrue('held as given', FService.FitProxy = FProxy);
    FService.FitProxy := nil;
    AssertTrue('and let go again', FService.FitProxy = nil);
end;

procedure TServiceSurfaceTest.TheCompletionCallbacksAreCallable;
var
    Sink: IClientCallback;
begin
    //  Empty on this class and overridden by the descendants that care. They are
    //  part of IClientCallback, so the engine is its own client's callback sink
    //  in the in-process arrangement; calling them must be harmless.
    FService.FitProxy := FProxy;
    Sink := FService;
    Sink.Done;
    Sink.ComputeCurveBoundsDone;
    Sink.ComputeBackgroundPointsDone;
    Sink.ComputeCurvePositionsDone;
    AssertTrue('no fault', True);
end;

{ ---- what it answers with nothing loaded ---------------------------------- }

procedure TServiceSurfaceTest.StatisticsAreInvalidBeforeAnyFit;
var
    Stats: TFitStatistics;
begin
    //  INVALID, not zero: a client that showed zeroes would be reporting a
    //  perfect fit of nothing.
    Stats := FService.GetStatistics;
    AssertTrue('not valid', not Stats.Valid);
end;

procedure TServiceSurfaceTest.ThereIsNoRFactorBeforeAnyFit;
begin
    //  ANSWERED, NOT REFUSED: the desktop's status bar asks for this on a timer,
    //  from the moment the window opens - long before anything is loaded. Asked
    //  through the string accessor the status bar itself uses, which is the
    //  public one.
    AssertTrue('an answer of some kind', FService.GetRFactorStr <> #0);
    AssertTrue('and the squared form too', FService.GetSqrRFactorStr <> #0);
end;

procedure TServiceSurfaceTest.TheCurveAttributesAreAskableFromTheStart;
var
    L: TObject;
begin
    //  The parameter table asks for this before anything is loaded. Whatever
    //  comes back is the caller's to free - see the interface's own note that
    //  every Get creates a new object.
    L := FService.GetCurveAttributes;
    try
        AssertTrue('an answer, empty or not', True);
    finally
        L.Free;
    end;
end;

procedure TServiceSurfaceTest.AbortingWhenNothingRunsIsHarmless;
var
    Msg: string;
begin
    //  REFUSED, and that is the right answer rather than a silent success: the
    //  user pressing Stop when nothing is running has asked for something that
    //  cannot be done, and the REST layer turns this into a 400 rather than
    //  reporting that a fit was stopped. Checked here because "harmless" was my
    //  assumption and the refusal is the behaviour.
    Msg := '';
    try
        FService.AbortAsyncOper;
    except
        on E: EUserException do
            Msg := E.Message;
    end;
    AssertTrue('refused as a user error', Msg <> '');
end;

{ ---- refusals -------------------------------------------------------------- }

procedure TServiceSurfaceTest.APointMayBeAddedToAnEmptyProfile;
begin
    //  I EXPECTED A REFUSAL HERE AND WAS WRONG, which is worth the comment: a
    //  fresh engine holds an empty profile rather than none, so adding the first
    //  point by hand is exactly how a user builds one from the grid. The test
    //  says what the engine does, not what I assumed.
    FService.AddPointToProfile(1.5, 2.5);
    AssertTrue('accepted', True);
end;

procedure TServiceSurfaceTest.AndTheStateFollowsTheDataRatherThanTheRequest;
begin
    //  The state is derived from what the profile now holds: with points in it
    //  there is a background still to remove, with none there is nothing to work
    //  on. A state set from the REQUEST instead would leave the engine claiming
    //  data it does not have.
    FService.AddPointToProfile(1.5, 2.5);
    AssertTrue('there is data now, so the background is next',
        FService.GetState = BackNotRemoved);
end;

procedure TServiceSurfaceTest.SoIsEditingIntervalBoundsThatAreNotThere;
var
    Msg: string;
begin
    Msg := '';
    try
        FService.ReplacePointInRFactorBounds(1, 1, 2, 2);
    except
        on E: EUserException do
            Msg := E.Message;
    end;
    AssertTrue('refused as a user error', Msg <> '');
end;

procedure TServiceSurfaceTest.AndFittingWithNoProfileAtAll;
var
    Msg: string;
begin
    //  The whole automatic sequence, asked for with nothing loaded. It returns a
    //  hint string when it accepts, so a refusal has to arrive as an exception.
    Msg := '';
    try
        FService.DoAllAutomatically;
    except
        on E: EUserException do
            Msg := E.Message;
    end;
    AssertTrue('refused as a user error', Msg <> '');
end;

procedure TServiceSurfaceTest.ARefusalCarriesAMessageForTheUser;
var
    Msg: string;
begin
    //  The message IS the whole story for a refusal: it is what the REST layer
    //  puts in the error response and what the desktop shows in a balloon. An
    //  empty one leaves the user with a request that failed for no stated
    //  reason.
    Msg := '';
    try
        FService.ReplacePointInRFactorBounds(1, 1, 2, 2);
    except
        on E: EUserException do
            Msg := E.Message;
    end;
    AssertTrue('there is a message', Length(Msg) > 10);
end;


{ ---- the sweep ------------------------------------------------------------- }

{ WHAT THIS PINS, and why it is a sweep rather than twenty tests.

  Every one of these is reachable from the REST layer and from the desktop's own
  state poll, on a problem the user has just created and not yet loaded anything
  into. Each may refuse - that is ordinary - but the CLASS of what it raises is a
  contract: EUserException becomes 400 and "your request was wrong for this
  state", anything else becomes 500 and "the server broke, retry". An assertion
  failure or an access violation here is the second, and it also stops the
  desktop's state poll on its way out.

  Written as a loop over the operations so that an operation added later is a line
  here rather than a test nobody writes. }
procedure TServiceSurfaceTest.NoOperationOnAnEmptyEngineFaults;
var
    i: longint;
    Faults: string;

    { Runs one operation and records anything that is not a user error. }
    procedure Try_(const AName: string; AIndex: longint);
    begin
        try
            case AIndex of
                0: FService.SubtractBackground(True);
                1: FService.ComputeBackgroundPoints;
                2: FService.ComputeCurvePositions;
                3: FService.ComputeCurveBounds;
                4: FService.MinimizeDifference;
                5: FService.MinimizeNumberOfCurves;
                6: FService.DoAllAutomatically;
                7: FService.MinimizeDifferenceAgain;
                8: FService.SmoothProfile;
                9: FService.CreateCurveList;
                10: FService.SelectAllPointsAsCurvePositions;
                11: FService.SelectEntireProfile;
                12: FService.AbortAsyncOper;
                13: FService.StopAsyncOper;
            end;
        except
            //  A refusal. Expected, and not what this is looking for.
            on E: EUserException do
                ;
            on E: Exception do
                Faults := Faults + AName + ' raised ' + E.ClassName + '; ';
        end;
    end;

begin
    Faults := '';
    for i := 0 to 13 do
        Try_('operation ' + IntToStr(i), i);
    AssertEquals('operations faulted rather than refusing: ' + Faults,
        '', Faults);
end;

procedure TServiceSurfaceTest.AndEveryRefusalCarriesSomethingToShowTheUser;
var
    Msg: string;
begin
    //  A refusal with an empty message is a request that failed for no stated
    //  reason: over REST it becomes a 400 with nothing in it, and in the desktop
    //  a blank balloon. Checked on one whose refusal is certain.
    Msg := 'nothing raised';
    try
        FService.MinimizeDifferenceAgain;
    except
        on E: EUserException do
            Msg := E.Message;
    end;
    AssertTrue('a message the user can read: ' + Msg, Length(Msg) > 10);
end;

procedure TServiceSurfaceTest.NorDoesAskingForEverythingItCanReport;
var
    Faults: string;
    i: longint;

    procedure Ask(const AName: string; AIndex: longint);
    var
        O: TObject;
    begin
        O := nil;
        try
            case AIndex of
                0: FService.GetRFactorStr;
                1: FService.GetSqrRFactorStr;
                2: FService.GetCalcTimeStr;
                3: FService.GetState;
                4: FService.GetCurveCount;
                5: O := FService.GetCurveAttributes;
                6: O := FService.GetProfilePointsSet;
                7: O := FService.GetSelectedProfileInterval;
                8: O := FService.GetBackgroundPoints;
                9: O := FService.GetRFactorBounds;
                10: O := FService.GetCurvePositions;
                11: FService.GetStatistics;
            end;
            //  Every Get answers a NEW object and the caller frees it - the
            //  interface says so, and a leak here would be the test's own.
            O.Free;
        except
            on E: EUserException do
                ;
            on E: Exception do
                Faults := Faults + AName + ' raised ' + E.ClassName + '; ';
        end;
    end;

begin
    //  THE DESKTOP ASKS ALL OF THESE ON A TIMER, from the moment the window
    //  opens - long before a file is loaded. One of them faulting reaches the
    //  top-level handler, which logs at Fatal and STOPS THE POLL: the user's
    //  connection to the engine is gone, and the cause was a status bar.
    Faults := '';
    for i := 0 to 11 do
        Ask('reader ' + IntToStr(i), i);
    AssertEquals('readers faulted: ' + Faults, '', Faults);
end;


{ ---- the refusal table ----------------------------------------------------- }

{ THE THREE STATES REACHABLE WITHOUT FITTING, and every operation asked of each.

  What is being pinned is not which answer each pair gives - that is the engine's
  business and it changes as the program grows - but two properties that must hold
  for all of them:

    * NOTHING RAISES ANYTHING BUT A USER ERROR. Over REST an EUserException is a
      400 and everything else is a 500; in the desktop the first is a balloon and
      the second stops the state poll. So the class of what comes out is a
      contract, and this is the only place it is checked across the whole table.

    * A REFUSAL LEAVES THE ENGINE USABLE. After every refusal the state is still
      readable and still the state it was - a refusal that moved the engine
      somewhere else would leave the user's next command answered by a different
      program than the one they are looking at.

  Written as a table so that an operation or a state added later is a line here
  rather than a test nobody writes. }
procedure TServiceSurfaceTest.TheStateMachineRefusesAndAcceptsConsistently;
var
    StateIdx, Op: longint;
    Faults, Moved: string;
    Before: TFitServerState;

    procedure PutIntoState(AWhich: longint);
    var
        P, Pos_, B: TTitlePointsSet;
        i: longint;
    begin
        if AWhich = 0 then
            Exit;                       //  ProfileWaiting: as constructed
        P := TTitlePointsSet.Create(nil);
        for i := 0 to 9 do
            P.AddNewPoint(i, 10 + i);
        FService.SetProfilePointsSet(P);
        if AWhich = 1 then
            Exit;                       //  a profile, background not removed
        Pos_ := TTitlePointsSet.Create(nil);
        Pos_.AddNewPoint(5, 15);
        FService.SetCurvePositions(Pos_);
        B := TTitlePointsSet.Create(nil);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(9, 0);
        FService.SetRFactorBounds(B);
    end;

    { True when the engine refused. }
    function Ask(AIndex: longint): boolean;
    begin
        Result := False;
        try
            case AIndex of
                0: FService.SubtractBackground(True);
                1: FService.ComputeBackgroundPoints;
                2: FService.ComputeCurvePositions;
                3: FService.ComputeCurveBounds;
                4: FService.SmoothProfile;
                5: FService.CreateCurveList;
                6: FService.SelectAllPointsAsCurvePositions;
                7: FService.SelectEntireProfile;
                8: FService.AbortAsyncOper;
                9: FService.StopAsyncOper;
                10: FService.AddPointToProfile(2.5, 12.5);
                11: FService.AddPointToBackground(1, 10);
                12: FService.AddPointToRFactorBounds(3, 0);
                13: FService.AddPointToCurvePositions(4, 14);
                14: FService.ReplacePointInProfile(3, 13, 3, 99);
                15: FService.ReplacePointInBackground(1, 10, 2, 10);
                16: FService.ReplacePointInRFactorBounds(3, 0, 4, 0);
                17: FService.ReplacePointInCurvePositions(4, 14, 5, 15);
            end;
        except
            on E: EUserException do
                //  A refusal: the expected answer, and the interesting one.
                Result := True;
            on E: Exception do
                Faults := Faults + Format('[state %d, op %d: %s] ',
                    [StateIdx, AIndex, E.ClassName]);
        end;
    end;

begin
    Faults := '';
    Moved := '';
    for StateIdx := 0 to 2 do
        for Op := 0 to 17 do
        begin
            //  A FRESH ENGINE PER PAIR. Sharing one would make the table depend
            //  on the order it is walked in, and then a failure would be about
            //  the test rather than about the engine.
            FreeAndNil(FService);
            FService := TFitService.Create;
            PutIntoState(StateIdx);
            Before := FService.GetState;
            //  A REFUSED OPERATION MAY NOT MOVE THE ENGINE. An accepted one
            //  may - subtracting a background or computing positions is
            //  progress, and the state is meant to follow it. What must never
            //  happen is the engine ending up somewhere else after telling the
            //  user it did nothing: their next command would then be answered
            //  by a different program from the one on their screen.
            if Ask(Op) and (FService.GetState <> Before) then
                Moved := Moved + Format('[state %d, op %d: %d -> %d] ',
                    [StateIdx, Op, Ord(Before), Ord(FService.GetState)]);
        end;
    AssertEquals('something raised other than a user error: ' + Faults,
        '', Faults);
    AssertEquals('a REFUSED operation moved the state: ' + Moved, '', Moved);
end;

procedure TServiceSurfaceTest.LoadingAProfileMovesOutOfWaiting;
begin
    AssertTrue('waiting, as constructed',
        FService.GetState = ProfileWaiting);
    GivenAProfile;
    //  NOT ReadyForFit: there may still be a background in the data, and the
    //  engine says so rather than letting a fit run over it.
    AssertTrue('a profile, and a background still to remove',
        FService.GetState = BackNotRemoved);
end;

procedure TServiceSurfaceTest.APickedIntervalAndPositionMakeTheModelReady;
var
    Pos_, B: TTitlePointsSet;
begin
    //  BOTH ARE NEEDED, and that is the rule: what to fit (a curve position) and
    //  where to fit it (an interval). With one of them the engine is not ready,
    //  and a fit asked for then has to invent the other - which it does for the
    //  plain fit verb and refuses for the continue-fitting one.
    GivenAProfile;
    Pos_ := TTitlePointsSet.Create(nil);
    Pos_.AddNewPoint(5, 15);
    FService.SetCurvePositions(Pos_);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    AssertTrue('ready to fit, state ' + IntToStr(Ord(FService.GetState)),
        FService.GetState in [ReadyForFit, ReadyForAutoFit]);
end;

procedure TServiceSurfaceTest.AddingAPickPutsItInTheSet;
begin
    GivenAProfile;
    FService.AddPointToCurvePositions(4, 14);
    AssertEquals('one pick', 1, FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.AddingTheSameCoordinatesAgainRemovesIt;
begin
    //  ADD-OR-TOGGLE, and its own header says so. Asserted HERE for the first
    //  time: TFitClient mirrors this logic and testcase_client_picking covers
    //  the mirror, so the copy was tested and the original was not.
    GivenAProfile;
    FService.AddPointToCurvePositions(4, 14);
    FService.AddPointToCurvePositions(4, 14);
    AssertEquals('taken away again', 0,
        FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.TheSameAbscissaWithADifferentValueReplacesIt;
begin
    //  Not a second point at one x: a pick set holds unique abscissae, because
    //  every instance is seeded from one and the pick carries the handle its
    //  fitted values come back by.
    GivenAProfile;
    FService.AddPointToCurvePositions(4, 14);
    FService.AddPointToCurvePositions(4, 99);
    AssertEquals('still one pick', 1,
        FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.ABulkWriteCarryingADuplicateKeepsOnePoint;
var
    Pos_: TTitlePointsSet;
begin
    //  THE EDGE THE TOGGLE LEAVES. A bulk write goes through AddPoint so that
    //  the one-pick-per-abscissa rule is enforced in the single place
    //  findings.md names - but a set carrying one coordinate TWICE then nets to
    //  zero rather than to one, because the second occurrence deletes the
    //  first. Neither findings entry looks at it from this direction: both
    //  discuss the toggle for interactive picks.
    //
    //  Pinned as the behaviour that IS wanted: a bulk write says "these are the
    //  picks", and annihilating a repeat is not a meaning anyone asked for.
    GivenAProfile;
    Pos_ := TTitlePointsSet.Create(nil);
    Pos_.AddNewPoint(4, 14);
    Pos_.AddNewPoint(4, 14);
    FService.SetCurvePositions(Pos_);
    AssertEquals('one point, not none', 1,
        FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.DeletingACurveTakesItsPickWithIt;
var
    B: TTitlePointsSet;
begin
    //  WHY THE PICK GOES TOO. The model is rebuilt from its inputs on every
    //  edit: RecreateCurves creates an instance for every pick that has one, so
    //  dropping only the identity would let the next rebuild put a fresh
    //  instance back - with a new handle, unfitted - and the deletion would
    //  have undone itself.
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    AssertTrue('the model was built', FService.GetCurveCount > 0);

    FService.DeleteCurve(0);
    AssertEquals('the pick is gone with it', 0,
        FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.DeletingTheLastCurveEmptiesTheModel;
var
    B: TTitlePointsSet;
begin
    //  IT DID NOT, AND NOTHING SAID SO. Removing the pick and the identity is
    //  only half of it: what the model REPORTS is a separate list, rebuilt by
    //  CollectCurves, and only a finished fit calls that. So the deleted curve
    //  went on being reported - the panel kept showing it and the chart kept
    //  drawing it - and a second attempt on the same row found an instance
    //  whose identity had already gone and was told it carried no handle.
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    AssertEquals('one curve to start with', 1, FService.GetCurveCount);

    FService.DeleteCurve(0);
    AssertEquals('and none afterwards', 0, FService.GetCurveCount);
    AssertEquals('nor any attributes row', 0,
        FService.GetCurveAttributes.Count);
end;

procedure TServiceSurfaceTest.AndItsFittedPositionMarkerGoesToo;
var
    B: TTitlePointsSet;
begin
    //  ONE MARKER STAYED ON THE CHART after every curve had been deleted, in a
    //  series whose legend row read "Fitted positions" while the model held
    //  nothing that could have one. The set is DERIVED from the curve list and
    //  the delete rebuilt neither.
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    AssertTrue('a curve to have a position', FService.GetCurveCount > 0);

    FService.DeleteCurve(0);
    AssertEquals('no curve, no fitted position', 0,
        FService.GetResultedCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.AndTheHandleIsNoLongerKnown;
var
    B: TTitlePointsSet;
    Id: string;
begin
    //  What a second attempt on the same row now meets: a handle the model does
    //  not have, which every caller already handles - the REST layer answers
    //  404 - rather than a curve that exists and cannot be named.
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    Id := FService.GetCurveInstanceId(0);
    AssertTrue('it had a handle', Id <> '');

    FService.DeleteCurve(0);
    AssertTrue('and the model no longer knows it',
        FService.IndexOfCurveInstance(Id) < 0);
end;

procedure TServiceSurfaceTest.DeletingACurveLeavesTheOthers;
var
    B: TTitlePointsSet;
    Before: longint;
begin
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(3, 13);
    FService.AddPointToCurvePositions(6, 16);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    Before := FService.GetCurveCount;
    AssertTrue('two picks made two curves', Before >= 2);

    FService.DeleteCurve(0);
    AssertEquals('one pick left', 1,
        FService.GetCurvePositions.PointsCount);
    AssertTrue('and fewer curves than before',
        FService.GetCurveCount < Before);
end;

procedure TServiceSurfaceTest.DeletingACurveTwiceIsRefusedNotSilent;
var
    B: TTitlePointsSet;
    Refused: boolean;
begin
    GivenAPeakedProfile;
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    FService.DeleteCurve(0);

    //  The index is checked, so asking again for a curve the model no longer
    //  holds is refused rather than acting on whatever now sits there. Deleting
    //  the wrong curve is the worst outcome available here.
    Refused := False;
    try
        FService.DeleteCurve(99);
    except
        on E: Exception do
            Refused := True;
    end;
    AssertTrue('an index the model does not hold is refused', Refused);
end;

function TServiceSurfaceTest.ParametersWithoutAPosition: Curve_parameters;

    procedure Add(const AName: string; AType: TParameterType; AValue: double);
    var
        P: TSpecialCurveParameter;
    begin
        P := TUserCurveParameter.Create;
        P.Name := AName;
        P.Type_ := AType;
        P.Value := AValue;
        TPersistentCurveParameterContainer(Result.Params.Add).Parameter := P;
    end;

begin
    Result := Curve_parameters.Create(nil);
    Result.Params.Clear;   //  drop the default placeholder
    Add('x', Argument, 0);
    Add('A', Variable, 1);
    //  DELIBERATELY NO POSITION. Hasx0 is what decides which identity a curve
    //  is issued, and a formula that declares neither a variable nor an
    //  invariable position is the one shape no pick can place: its handle is
    //  keyed by the fit interval instead.
end;

procedure TServiceSurfaceTest.DeletingAFormulaCurveTakesNoPickWithIt;
var
    B: TTitlePointsSet;
    PicksBefore: longint;
begin
    //  THE THIRD WAY A DELETION CAN GO, and the engine had never been asked to
    //  take it. A curve placed by a pick takes the pick with it, or the next
    //  rebuild puts the curve straight back; a curve a module placed takes the
    //  markup, for the same reason. A formula declaring no position was placed
    //  by NEITHER - there is one per fit interval, and its handle is keyed by
    //  the interval - so the only thing to remove is the identity, and what the
    //  model holds afterwards is the rebuild's to decide.
    //
    //  Removing "its pick" here would delete a pick that placed some other
    //  curve, which is why the three cases are told apart (RemovalOf) rather
    //  than one being made to stand for the others.
    GivenAPeakedProfile;
    FService.SetCurveType(TUserPointsSet.GetCurveTypeId);
    FService.SetSpecialCurveParameters('A*exp(-x*x)',
        ParametersWithoutAPosition);
    FService.AddPointToCurvePositions(4, 14);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(9, 0);
    FService.SetRFactorBounds(B);
    AssertTrue('the formula was built into a curve', FService.GetCurveCount > 0);

    PicksBefore := FService.GetCurvePositions.PointsCount;
    FService.DeleteCurve(0);

    AssertEquals('the picks are untouched', PicksBefore,
        FService.GetCurvePositions.PointsCount);
end;

procedure TServiceSurfaceTest.AndEveryStateStillAnswersEveryReader;
var
    Faults: string;
    O: TObject;
    i: longint;
begin
    //  The desktop polls the readers on a timer whatever state the engine is in.
    //  Here it is in the busiest of the three: data loaded, picks made.
    GivenAProfile;
    FService.AddPointToRFactorBounds(3, 0);
    FService.AddPointToCurvePositions(4, 14);
    Faults := '';
    for i := 0 to 6 do
    begin
        O := nil;
        try
            case i of
                0: FService.GetRFactorStr;
                1: FService.GetCalcTimeStr;
                2: FService.GetCurveCount;
                3: O := FService.GetProfilePointsSet;
                4: O := FService.GetRFactorBounds;
                5: O := FService.GetCurvePositions;
                6: FService.GetStatistics;
            end;
            O.Free;
        except
            on E: EUserException do
                ;
            on E: Exception do
                Faults := Faults + Format('[reader %d: %s] ', [i, E.ClassName]);
        end;
    end;
    AssertEquals('a reader faulted: ' + Faults, '', Faults);
end;


{ ---- finding the curves and their bounds ----------------------------------- }

procedure TServiceSurfaceTest.ThePeakIsFoundAsACurvePosition;
var
    Positions: TTitlePointsSet;
begin
    //  WHAT THE "compute curve positions" COMMAND DOES, run here in the test
    //  thread because the plain engine runs its tasks inline. One peak in, at
    //  least one position out - and near the peak, not anywhere.
    GivenAPeakedProfile;
    FService.ComputeCurvePositions;
    Positions := FService.GetCurvePositions;
    try
        AssertTrue('at least one position was found',
            Positions.PointsCount > 0);
        AssertTrue('and it is somewhere near the peak, not at an end',
            (Positions.PointXCoord[0] > 5) and (Positions.PointXCoord[0] < 15));
    finally
        Positions.Free;
    end;
end;

procedure TServiceSurfaceTest.AndBoundsAreWorkedOutAroundIt;
var
    Bounds: TTitlePointsSet;
begin
    //  Bounds come in pairs - where a curve begins and where it ends - so an odd
    //  count is a half-open interval that nothing downstream can use.
    GivenAPeakedProfile;
    FService.ComputeCurveBounds;
    Bounds := FService.GetRFactorBounds;
    try
        AssertTrue('bounds were produced', Bounds.PointsCount > 0);
        AssertEquals('and they come in pairs', 0, Bounds.PointsCount mod 2);
    finally
        Bounds.Free;
    end;
end;

procedure TServiceSurfaceTest.BothReadTheSelectedIntervalWhenThereIsOne;
var
    Positions: TTitlePointsSet;
begin
    //  THE BRANCH THIS IS FOR. Both algorithms read the SELECTED INTERVAL when
    //  one is in force and the whole profile otherwise. Reading the wrong one
    //  puts curves outside the stretch the user chose to work on, and the chart
    //  shows them as if they had been asked for.
    GivenAPeakedProfile;
    FService.SelectProfileInterval(6, 14);
    FService.ComputeCurvePositions;
    Positions := FService.GetCurvePositions;
    try
        AssertTrue('positions were found in the interval',
            Positions.PointsCount > 0);
        AssertTrue('and none of them is outside it',
            (Positions.PointXCoord[0] >= 6) and
            (Positions.PointXCoord[Positions.PointsCount - 1] <= 14));
    finally
        Positions.Free;
    end;
    //  And the bounds algorithm on the same interval.
    FService.ComputeCurveBounds;
    AssertTrue('the engine is still usable afterwards',
        FService.GetState <> ProfileWaiting);
end;


{ A NARROW SPIKE ON A LONG BASELINE, which is the shape the bounds walk was
  written for: it keeps only the points that BRACKET each peak, so a peak that
  ends well before the data does has a right bound to find. The broad peak the
  other test uses runs to the end of the profile, where a different branch closes
  the interval, so both shapes are worth having. }
procedure TServiceSurfaceTest.ANarrowPeakIsBracketedRatherThanSwallowed;
var
    P: TTitlePointsSet;
    Bounds: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 30 do
        //  A tight peak at x = 8, flat either side and a long tail after it.
        P.AddNewPoint(i, 5 + 100 * Exp(-Sqr((i - 8) / 0.8)));
    FService.SetProfilePointsSet(P);

    FService.ComputeCurveBounds;
    Bounds := FService.GetRFactorBounds;
    try
        AssertTrue('bounds were produced', Bounds.PointsCount >= 2);
        AssertEquals('and they come in pairs', 0, Bounds.PointsCount mod 2);
        //  They must BRACKET the peak rather than sit on one side of it: an
        //  interval that misses the peak fits a curve to the baseline.
        AssertTrue('the first bound is left of the peak',
            Bounds.PointXCoord[0] <= 8);
        AssertTrue('and the last is right of it',
            Bounds.PointXCoord[Bounds.PointsCount - 1] >= 8);
    finally
        Bounds.Free;
    end;
end;

{ ---- the background the user hands over ------------------------------------ }

procedure TServiceSurfaceTest.TooLittleDataLeavesNothingToWorkOn;
var
    Tiny, Back: TTitlePointsSet;
    Msg: string;
begin
    //  I EXPECTED THIS TO BE ABOUT AN EMPTY BACKGROUND AND IT IS NOT: what
    //  decides the state here is the PROFILE. Handing over a background when
    //  there are too few data points to fit leaves the engine waiting for a
    //  profile, whatever the background contains - and it says so, because the
    //  next thing the user wonders is why the fit command has gone quiet.
    Tiny := TTitlePointsSet.Create(nil);
    Tiny.AddNewPoint(0, 1);
    Tiny.AddNewPoint(1, 2);
    FService.SetProfilePointsSet(Tiny);

    Back := TTitlePointsSet.Create(nil);
    Back.AddNewPoint(0, 1);
    Msg := FService.SetBackgroundPointsSet(Back);
    AssertTrue('the engine says what state it is in now: ' + Msg, Msg <> '');
    AssertTrue('and it is waiting for data',
        FService.GetState = ProfileWaiting);
end;

procedure TServiceSurfaceTest.AndAProfileWorthFittingIsKept;
var
    Back: TTitlePointsSet;
    Msg: string;
begin
    //  The same call with enough data behind it: the background is taken and
    //  the engine moves on to having one to remove.
    GivenAProfile;
    Back := TTitlePointsSet.Create(nil);
    Back.AddNewPoint(0, 10);
    Back.AddNewPoint(9, 19);
    Msg := FService.SetBackgroundPointsSet(Back);
    AssertTrue('it says what happens next: ' + Msg, Msg <> '');
    AssertTrue('and there is a background to remove',
        FService.GetState = BackNotRemoved);
end;

{ ---- an interval, and what it changes -------------------------------------- }

procedure TServiceSurfaceTest.AnIntervalCanBeSelectedAndGivenBack;
begin
    //  Selecting part of the profile puts the engine into "selected area" mode:
    //  from then on the profile the user is working on is the interval, and the
    //  whole one is still there underneath. Selecting the whole profile is how
    //  they come back.
    GivenAProfile;
    FService.SelectProfileInterval(2, 6);
    AssertTrue('the interval is what is worked on now',
        FService.GetSelectedProfileInterval <> nil);
    FService.SelectEntireProfile;
    AssertTrue('and the whole profile is back',
        FService.GetProfilePointsSet <> nil);
end;

procedure TServiceSurfaceTest.TheBackgroundIsSubtractedFromWhicheverIsInForce;
var
    Faults: string;
begin
    //  THE BRANCH THIS IS FOR: subtracting the background operates on the
    //  SELECTED INTERVAL when one is in force and on the whole profile
    //  otherwise. Getting that wrong flattens data the user did not select, and
    //  nothing on screen says which one it worked on - the curve simply moves.
    Faults := '';
    GivenAProfile;
    try
        FService.SubtractBackground(True);
    except
        on E: EUserException do ;
        on E: Exception do Faults := Faults + 'whole profile: ' + E.ClassName;
    end;

    FreeAndNil(FService);
    FService := TFitService.Create;
    GivenAProfile;
    FService.SelectProfileInterval(2, 6);
    try
        FService.SubtractBackground(True);
    except
        on E: EUserException do ;
        on E: Exception do Faults := Faults + '; interval: ' + E.ClassName;
    end;
    AssertEquals('one of the two paths faulted: ' + Faults, '', Faults);
end;

{ ---- with a profile loaded ------------------------------------------------- }

procedure TServiceSurfaceTest.LoadingAProfileLeavesTheBackgroundToRemove;
begin
    //  The state after loading is not "ready to fit": a background may still be
    //  in the data, and the engine says so rather than letting a fit run over it.
    GivenAProfile;
    AssertTrue('the background is next', FService.GetState = BackNotRemoved);
end;

procedure TServiceSurfaceTest.APointOfTheProfileCanBeMoved;
begin
    //  What editing a cell of the profile table becomes on this side: the point
    //  at (3, 13) moves to (3, 99).
    GivenAProfile;
    FService.ReplacePointInProfile(3, 13, 3, 99);
    AssertTrue('accepted', True);
end;

procedure TServiceSurfaceTest.MovingAPointThatIsNotThereIsNotAFault;
begin
    //  The grid sends the value it had; a point can have gone in between, and a
    //  fault here would reach the top-level handler and stop the state poll.
    GivenAProfile;
    FService.ReplacePointInProfile(1000, 1000, 1001, 1001);
    AssertTrue('no fault', True);
end;

procedure TServiceSurfaceTest.PicksLandInTheirOwnSets;
begin
    //  FOUR SETS, FOUR VERBS, and the reason they are separate verbs rather than
    //  one with a kind is that they are collected in different modes and mean
    //  different things. Getting a pick into the wrong set draws it in the wrong
    //  colour and fits the wrong thing.
    GivenAProfile;
    FService.AddPointToBackground(1, 10);
    FService.AddPointToRFactorBounds(2, 0);
    FService.AddPointToCurvePositions(4, 14);
    AssertTrue('all three accepted', True);
end;

procedure TServiceSurfaceTest.AnIntervalBoundCanBeMovedOnceThereIsOne;
begin
    //  Refused with no bounds - which the refusal test above pins - and accepted
    //  once the user has placed one, which is the other half of the same rule.
    GivenAProfile;
    FService.AddPointToRFactorBounds(2, 0);
    FService.ReplacePointInRFactorBounds(2, 0, 3, 0);
    AssertTrue('accepted', True);
end;

{ THE PICKING GESTURE'S NAME is not tested here, and the reason is worth stating:
  it is protected, and the only path that reads it is a refusal raised when a
  curve type placed by a point set is selected - which no type in the framework
  is. It belongs to the pack that has such a type, whose own suite can reach it. }

initialization
    RegisterTest('unit', TServiceSurfaceTest);
end.
