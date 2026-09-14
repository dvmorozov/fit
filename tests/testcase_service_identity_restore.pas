// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Pushing picks back WITH the handles their curves are known by - the one
thing that makes a saved fit resume rather than start over.)

WHY THIS EXISTS. A curve instance carries an opaque handle issued to the model
INPUT - the pick - and inherited by the instance rebuilt from it
(curve_instance_id, curve_identity_registry, roadmap 10). Reading one out has
always worked: GetCurveInstanceId reports it and GET /curves emits it. Sending
one back did not exist at all, so a client that saved a model and pushed the same
picks again got FRESH handles for every one of them, and every value it had saved
under the old handles matched nothing. That is not a visible failure - it is a fit
that quietly resumes from its starting guess.

WHAT THESE PIN, and every one of them fails silently in the application:

  * a handle offered with a pick survives the rebuild that writing the picks
    triggers - the whole point, and what was impossible;
  * a pick offered with no handle still gets one, so a client that knows nothing
    about handles behaves exactly as before;
  * a handle whose pick is gone on the next write is dropped, rather than lying
    in wait to be inherited by whatever is placed at that abscissa later;
  * a repeated abscissa leaves ONE point and ONE handle, because SetPointUnique
    collapses the pair - a positional id lookup after the fact would be off by
    one for every pick after it;
  * each pick keeps ITS OWN handle. An id that slid by one names another curve,
    and the fit then resumes the wrong shape while everything reports success.

HOW PAIRING IS ASSERTED WITHOUT REACHING A PRIVATE MEMBER. DeleteCurve removes
the pick the curve was seeded from - documented on IFitService and already tested
- so deleting the curve a handle names and looking at which pick disappeared says
exactly which pick that handle belonged to, through the published surface only.

The engine is driven directly: it is an ordinary object, and a socket would reach
nothing this does not.
}
unit testcase_service_identity_restore;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, points_set, title_points_set,
    gauss_points_set, MyExceptions;

type
    TServiceIdentityRestoreTest = class(TTestCase)
    private
        FService: TFitService;
        { A profile with a peak in it, so the picks name real samples. }
        procedure GivenAProfile;
        { The same, with no fit interval - so nothing is built from the picks. }
        procedure GivenAProfileWithNoIntervals;
        { AHandle in the braceless form the wire and the file use. }
        function Wire(const AHandle: string): string;
        { Picks at AXs, offered with the handles AIds (an empty array = none). }
        procedure WhenPicksArePushed(const AXs: array of double;
            const AIds: array of string);
        { Whether the model still holds the instance AHandle names. }
        function ModelKnows(const AHandle: string): boolean;
        { The abscissae the engine now holds picks at, as text, in order. }
        function PickedAt: string;
        { A one-curve, one-parameter restore payload. }
        function OneCurve(ACurveIndex: longint; const AName: string;
            AValue, AError: double; AFitted: boolean): TCurveValuesList;
        { The calculated profile at the sample nearest AX. }
        function CalculatedAt(AX: double): double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnAdoptedHandleSurvivesTheRebuildThePickWriteTriggers;
        procedure APickOfferedWithNoHandleIsIssuedAFreshOne;
        procedure AnAdoptedHandleWithNoMatchingPickIsDropped;
        procedure ARepeatedAbscissaKeepsOnePointAndOneHandle;
        procedure EveryPickKeepsItsOwnHandleRatherThanTheFirst;
        procedure AHandleListOfTheWrongLengthIsRefused;
        procedure AHandleThatIsNotAHandleIsRefused;
        procedure AnEmptyHandleBesideAPickMeansIssueOne;
        procedure OfferingHandlesChangesNothingElseAboutTheWrite;

        //  WRITING THE VALUES A PREVIOUS FIT FOUND, in one pass. The
        //  per-parameter route can only write one number under a handle the
        //  server already knows, and it rebuilds the whole model each time; a
        //  restore has to write every instance's every parameter, and has to be
        //  able to say that a fit - not a seed - is where they came from.
        procedure ValuesWrittenInOnePassReachTheCalculatedProfile;
        procedure AnInstanceSaidToBeFittedIsReportedAsFitted;
        procedure ValuesAloneDoNotMakeAnInstanceFitted;
        procedure WritingValuesLeavesThePicksUntouched;
        procedure AParameterTheCurveDoesNotHaveIsRefused;
        procedure AnIndexThatNamesNoCurveIsRefused;

        //  READING THE HANDLES BACK BESIDE THE PICKS. The write side exists;
        //  without the read side a client can save the picks and cannot save
        //  which curve each one stands for, which is the same loss by another
        //  route.
        procedure TheHandlesComeBackBesideThePicksTheyBelongTo;
        procedure APickWithNoCurveYetReportsAnEmptyHandle;
        procedure ThereIsOneHandlePerPickOrNoneAtAll;
    end;

implementation

const
    HandleA = '{0A0A0A0A-1111-2222-3333-444444444444}';
    HandleB = '{0B0B0B0B-1111-2222-3333-444444444444}';
    HandleC = '{0C0C0C0C-1111-2222-3333-444444444444}';

procedure TServiceIdentityRestoreTest.SetUp;
begin
    FService := TFitService.Create;
end;

procedure TServiceIdentityRestoreTest.TearDown;
begin
    FreeAndNil(FService);
end;

{ A handle as this program puts it on a wire and in a file: the same identifier
  with no braces. TryStrToCurveInstanceId reads both, so the two are
  interchangeable going back in. }
function TServiceIdentityRestoreTest.Wire(const AHandle: string): string;
begin
    Result := AHandle;
    if (Length(Result) = 38) and (Result[1] = '{') then
        Result := Copy(Result, 2, 36);
end;

procedure TServiceIdentityRestoreTest.GivenAProfileWithNoIntervals;
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
    FService.SetCurveType(TGaussPointsSet.GetCurveTypeId);
end;

procedure TServiceIdentityRestoreTest.GivenAProfile;
var
    P, B: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    //  A Gaussian on a flat base, written out rather than taken from a curve
    //  unit, so this fixture depends on nothing that fits.
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
    //  ASKED FOR BY NAME. The curve-type selection is process-global, so a
    //  fixture that does not state its type gets whatever the previous test
    //  left selected - and these tests name a parameter.
    FService.SetCurveType(TGaussPointsSet.GetCurveTypeId);

    //  AND ONE FIT INTERVAL, because the model is not built without one:
    //  GoToReadyForFit requires bounds (or a module contributing readiness)
    //  before it creates any task, so with none there are picks and no curves
    //  to carry a handle. It is also the order a restore uses - bounds before
    //  picks - for the separate reason that writing bounds clears the curve
    //  attributes.
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    FService.SetRFactorBounds(B);
end;

procedure TServiceIdentityRestoreTest.WhenPicksArePushed(
    const AXs: array of double; const AIds: array of string);
var
    Picks: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
    i: longint;
begin
    Picks := TTitlePointsSet.Create(nil);
    for i := Low(AXs) to High(AXs) do
        Picks.AddNewPoint(AXs[i], 50);
    SetLength(Ids, Length(AIds));
    for i := Low(AIds) to High(AIds) do
        Ids[i] := AIds[i];
    //  The engine takes ownership of the set it is handed.
    FService.SetCurvePositions(Picks, Ids);
end;

function TServiceIdentityRestoreTest.ModelKnows(const AHandle: string): boolean;
begin
    Result := FService.IndexOfCurveInstance(AHandle) >= 0;
end;

function TServiceIdentityRestoreTest.PickedAt: string;
var
    i: longint;
    Picks: TTitlePointsSet;
begin
    Result := '';
    Picks := FService.GetCurvePositions;
    if Picks = nil then
        Exit;
    for i := 0 to Picks.PointsCount - 1 do
    begin
        if Result <> '' then
            Result := Result + ',';
        Result := Result + FloatToStr(Picks.PointXCoord[i]);
    end;
end;

procedure TServiceIdentityRestoreTest.
    AnAdoptedHandleSurvivesTheRebuildThePickWriteTriggers;
begin
    //  THE ONE THAT WAS IMPOSSIBLE. Writing the picks demolishes and rebuilds
    //  every instance, and the handle has to come out the other side - which is
    //  what lets values saved under it be re-attached to the new object.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    AssertTrue('the model carries the handle it was offered',
        ModelKnows(HandleA));
end;

procedure TServiceIdentityRestoreTest.APickOfferedWithNoHandleIsIssuedAFreshOne;
begin
    //  ADDITIVE. Every client until the project file exists offers none, and
    //  must behave exactly as it did.
    GivenAProfile;
    WhenPicksArePushed([10], []);
    AssertTrue('a curve was built', FService.GetCurveCount > 0);
    AssertTrue('and it was issued a handle anyway',
        FService.GetCurveInstanceId(0) <> '');
end;

procedure TServiceIdentityRestoreTest.AnAdoptedHandleWithNoMatchingPickIsDropped;
begin
    //  A handle belongs to a pick. When the next write does not carry that
    //  pick, the handle goes with it - otherwise it waits to be inherited by
    //  whatever is later placed at that abscissa, which is a fit resuming
    //  another curve's shape.
    GivenAProfile;
    WhenPicksArePushed([8, 10], [HandleA, HandleB]);
    AssertTrue('both are known first', ModelKnows(HandleA));
    AssertTrue('', ModelKnows(HandleB));

    WhenPicksArePushed([10], [HandleB]);
    AssertTrue('the surviving pick kept its handle', ModelKnows(HandleB));
    AssertFalse('and the abandoned one names nothing', ModelKnows(HandleA));
end;

procedure TServiceIdentityRestoreTest.ARepeatedAbscissaKeepsOnePointAndOneHandle;
begin
    //  SetPointUnique collapses a repeated abscissa to one point and the later
    //  value wins. The handle follows the same rule, or the count of points and
    //  the count of handles disagree and every later pick's id is off by one.
    GivenAProfile;
    WhenPicksArePushed([10, 10], [HandleA, HandleB]);
    AssertEquals('one pick, not two and not none', 1,
        FService.GetCurvePositions.PointsCount);
    AssertEquals('and one curve', 1, FService.GetCurveCount);
    AssertTrue('the later handle won, as the later value does',
        ModelKnows(HandleB));
    AssertFalse('and the earlier one is not also there', ModelKnows(HandleA));
end;

procedure TServiceIdentityRestoreTest.EveryPickKeepsItsOwnHandleRatherThanTheFirst;
begin
    //  Deleting the curve a handle names takes ITS pick with it, so which pick
    //  disappeared says which pick the handle belonged to. An off-by-one here
    //  is not a crash - it is the wrong curve resuming the wrong shape.
    GivenAProfile;
    WhenPicksArePushed([6, 10, 14], [HandleA, HandleB, HandleC]);
    AssertEquals('three picks', '6,10,14', PickedAt);

    FService.DeleteCurve(FService.IndexOfCurveInstance(HandleB));
    AssertEquals('the middle handle named the middle pick', '6,14', PickedAt);
    AssertTrue('and the others are untouched', ModelKnows(HandleA));
    AssertTrue('', ModelKnows(HandleC));
end;

procedure TServiceIdentityRestoreTest.AHandleListOfTheWrongLengthIsRefused;
var
    Raised: boolean;
begin
    //  REFUSED, not padded and not truncated: nothing here can know whether the
    //  missing handle belonged to the first pick or the last, so there is no
    //  guess to make - and a wrong guess attaches one curve's saved values to
    //  another, which nothing downstream can detect.
    GivenAProfile;
    Raised := False;
    try
        WhenPicksArePushed([8, 10], [HandleA]);
    except
        on E: EUserException do
            Raised := True;
    end;
    AssertTrue('two picks and one handle is refused', Raised);
end;

procedure TServiceIdentityRestoreTest.AHandleThatIsNotAHandleIsRefused;
var
    Raised: boolean;
begin
    //  A refusal explains itself rather than falling through to plausible
    //  behaviour. Text that is not a handle would otherwise become "no handle",
    //  which reads as a brand new curve and loses the values silently.
    GivenAProfile;
    Raised := False;
    try
        WhenPicksArePushed([10], ['not-a-handle']);
    except
        on E: EUserException do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TServiceIdentityRestoreTest.AnEmptyHandleBesideAPickMeansIssueOne;
begin
    //  A caller may know the handle of some picks and not others: a project
    //  carries none for a pick placed after the last fit. An empty entry says
    //  that, and it is not an error.
    GivenAProfile;
    WhenPicksArePushed([8, 10], ['', HandleB]);
    AssertEquals('both picks are there', '8,10', PickedAt);
    AssertTrue('the named one kept what it was given', ModelKnows(HandleB));
    AssertEquals('and the unnamed one still built a curve', 2,
        FService.GetCurveCount);
end;

procedure TServiceIdentityRestoreTest.OfferingHandlesChangesNothingElseAboutTheWrite;
begin
    //  The write is otherwise the same write: same picks, same dedupe, same
    //  state. Handles are an annotation on it, not a different operation.
    GivenAProfile;
    WhenPicksArePushed([6, 10], [HandleA, HandleB]);
    AssertEquals('the picks are what was sent', '6,10', PickedAt);
    AssertEquals('and the model has one curve per pick', 2,
        FService.GetCurveCount);
end;

{ ---- writing back what a fit found ---------------------------------------- }

{ The values for the one curve at ACurveIndex, as a restore would send them. }
function TServiceIdentityRestoreTest.OneCurve(ACurveIndex: longint;
    const AName: string; AValue, AError: double;
    AFitted: boolean): TCurveValuesList;
begin
    SetLength(Result, 1);
    Result[0].CurveIndex := ACurveIndex;
    Result[0].Fitted := AFitted;
    SetLength(Result[0].Params, 1);
    Result[0].Params[0].Name := AName;
    Result[0].Params[0].Value := AValue;
    Result[0].Params[0].Error := AError;
end;

{ The calculated profile's value at the sample nearest AX - the only thing that
  can tell a write which reached the MODEL from one that reached only the
  per-round report the model is regenerated into. }
function TServiceIdentityRestoreTest.CalculatedAt(AX: double): double;
var
    Calc: TTitlePointsSet;
    i: longint;
begin
    Result := 0;
    Calc := FService.GetCalcProfilePointsSet;
    if Calc = nil then
        Exit;
    for i := 0 to Calc.PointsCount - 1 do
        if Abs(Calc.PointXCoord[i] - AX) < 1e-6 then
            Exit(Calc.PointYCoord[i]);
end;

procedure TServiceIdentityRestoreTest.ValuesWrittenInOnePassReachTheCalculatedProfile;
var
    Before, After: double;
begin
    //  ASSERTED ON THE CALCULATED PROFILE, not on the value read back. The
    //  profile is built from the CURVES; the values are written into the
    //  per-round report, and reach the curves only because the rebuild hands
    //  each instance the values stored under its handle. Reading the report
    //  back would pass even if nothing ever reached the model - which is the
    //  distinction the per-parameter route's own test was written to make.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    AssertTrue('the model computes a profile at all',
        FService.GetCalcProfilePointsSet <> nil);
    AssertTrue('and it has points',
        FService.GetCalcProfilePointsSet.PointsCount > 0);
    Before := CalculatedAt(10);

    //  A width nothing would arrive at by itself: the synthetic peak's sigma is
    //  2.5, so a curve carrying 0.4 can only have got it from this write.
    FService.SetCurveValues(OneCurve(0, 'sigma', 0.4, 0.5, True));
    After := CalculatedAt(10);
    AssertTrue('the model was rebuilt around the value written',
        Abs(After - Before) > 1e-6);
end;

procedure TServiceIdentityRestoreTest.AnInstanceSaidToBeFittedIsReportedAsFitted;
begin
    //  THE FLAG CANNOT BE DERIVED, which is the whole reason it crosses. Every
    //  instance has parameter values from the moment it is seeded; only this
    //  says an optimiser produced them. It gates whether a markup move is
    //  refused, whether a rebuild re-seeds, and whether a missing instance is
    //  reported as an orphan.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    AssertFalse('a freshly placed curve is not fitted',
        FService.AnyCurveIsFitted);

    FService.SetCurveValues(OneCurve(0, 'sigma', 0.4, 0.5, True));
    AssertTrue('and a restored one says the optimiser had been there',
        FService.AnyCurveIsFitted);
end;

procedure TServiceIdentityRestoreTest.ValuesAloneDoNotMakeAnInstanceFitted;
begin
    //  The counterpart. A project saved before any fit ran carries seeds, not
    //  results, and restoring them must not claim otherwise - or a move that
    //  should be allowed is refused to protect a fit that never happened.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    FService.SetCurveValues(OneCurve(0, 'sigma', 0.4, 0.5, False));
    AssertFalse('values without a fit behind them', FService.AnyCurveIsFitted);
end;

procedure TServiceIdentityRestoreTest.WritingValuesLeavesThePicksUntouched;
begin
    //  A pick set is model INPUT and a fit may only DELETE from one. Writing
    //  fitted values back is not a fit, and it must not move a pick: the picks
    //  carry the handles, and writing a fitted x over a picked one breaks the
    //  uniqueness the whole identity scheme rests on.
    GivenAProfile;
    WhenPicksArePushed([6, 14], [HandleA, HandleB]);
    FService.SetCurveValues(OneCurve(0, 'sigma', 0.4, 0.5, True));
    AssertEquals('the picks are where the user put them', '6,14', PickedAt);
end;

procedure TServiceIdentityRestoreTest.AParameterTheCurveDoesNotHaveIsRefused;
var
    Raised: boolean;
begin
    //  Refused rather than ignored. A name this curve does not carry means the
    //  project and this build disagree about the model - most likely a curve
    //  type whose parameters have been renamed - and silently dropping it
    //  restores a curve that is missing exactly the value nobody will look for.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    Raised := False;
    try
        FService.SetCurveValues(OneCurve(0, 'no-such-parameter', 1, 0, True));
    except
        on E: EUserException do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TServiceIdentityRestoreTest.AnIndexThatNamesNoCurveIsRefused;
var
    Raised: boolean;
    Nm: string;
    V: double;
    T: longint;
    Before: double;
begin
    //  REFUSED, AND NOT WRITTEN TO CURVE ZERO. That exact fallthrough - an
    //  address that does not resolve quietly naming the first curve - is a
    //  defect this codebase has already had, on the two routes that used to run
    //  their path segment through StrToIntDef(..., 0).
    //
    //  The exception class is deliberately not pinned. Handles are resolved to
    //  indices at the wire's own boundary, so an out-of-range index arriving
    //  here is a fault in the caller rather than in the request, and which of
    //  the two this build calls it is not part of the contract. That it does
    //  not silently write is.
    GivenAProfile;
    WhenPicksArePushed([10], [HandleA]);
    FService.GetCurveParameter(0, 0, Nm, V, T);
    Before := V;

    Raised := False;
    try
        FService.SetCurveValues(OneCurve(7, Nm, Before + 100, 0, True));
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
    FService.GetCurveParameter(0, 0, Nm, V, T);
    AssertEquals('and the first curve was left alone', Before, V, 1e-12);
end;


{ ---- reading the handles back --------------------------------------------- }

procedure TServiceIdentityRestoreTest.TheHandlesComeBackBesideThePicksTheyBelongTo;
var
    Ids: TCurveInstanceIdList;
begin
    //  SYMMETRIC WITH THE WRITE, and in the same order, so a client can read
    //  the picks and their handles and hand exactly that back later. Anything
    //  else and saving a model would mean re-deriving which curve each pick
    //  stands for - which is the correspondence the handle exists to carry.
    GivenAProfile;
    WhenPicksArePushed([6, 14], [HandleA, HandleB]);
    Ids := FService.GetCurvePositionIds;
    AssertEquals('one per pick', 2, Length(Ids));
    //  THE WIRE FORM, with no braces. A handle has two spellings - the registry
    //  brackets, a URL path segment cannot - and this side reports the one the
    //  file and the URL share, so nothing downstream has to know which of the
    //  two it is holding.
    AssertEquals('the first pick''s own handle', Wire(HandleA), Ids[0]);
    AssertEquals('and the second''s', Wire(HandleB), Ids[1]);
end;

procedure TServiceIdentityRestoreTest.APickWithNoCurveYetReportsAnEmptyHandle;
var
    Ids: TCurveInstanceIdList;
begin
    //  A pick placed while nothing can be built from it - no fit interval yet -
    //  has no instance and therefore no handle. Empty rather than absent, so
    //  the list stays one entry per pick and positions never slide.
    GivenAProfileWithNoIntervals;
    FService.AddPointToCurvePositions(4, 14);
    Ids := FService.GetCurvePositionIds;
    AssertEquals('still one entry', 1, Length(Ids));
    AssertEquals('and it names nothing', '', Ids[0]);
end;

procedure TServiceIdentityRestoreTest.ThereIsOneHandlePerPickOrNoneAtAll;
var
    Ids: TCurveInstanceIdList;
begin
    //  The count is the contract: a caller pairs Ids[i] with pick i, and a
    //  short list would pair a handle with the wrong pick rather than fail.
    GivenAProfile;
    AssertEquals('no picks, no handles', 0,
        Length(FService.GetCurvePositionIds));
    WhenPicksArePushed([6, 10, 14], []);
    Ids := FService.GetCurvePositionIds;
    AssertEquals('three picks, three handles', 3, Length(Ids));
end;


initialization
    //  A unit test: the engine is an ordinary object here - no socket, no
    //  process and no optimiser run.
    RegisterTest('unit', TServiceIdentityRestoreTest);
end.
