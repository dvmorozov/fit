// SPDX-License-Identifier: GPL-3.0-or-later
{ Drives the compute server's REST surface directly (no socket): the same
  IFitService verbs the retired XML-RPC transport carried, now over HTTP+JSON.
  Because TFitRestApi.Handle is a pure (method, path, body) -> (code, body)
  function, the whole API can be exercised here. }
unit testcase_rest_api;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
  fit_rest_api, fit_points_json, fit_worker_protocol, SimpMath, gauss_points_set,
  //  The curve type whose formula can be deleted out from under the problem.
  user_points_set;
type
  { The fixture both halves share. NOT registered: it declares no test, and a
    registered class with none would fail testcase_suite_split. }
  TRestApiTestBase = class(TTestCase)
  private
    FApi: TFitRestApi;
    function Call(const M, P, B: string; out Code: longint): TJSONObject;
    function NewProblem: longint;
    { A synthetic Gaussian profile as a wire point set. }
    function GaussianProfileJson: string;
    { The same peak on a coarse grid: few enough samples that seeding a curve on
      every one of them - which is what this program does when nothing is picked
      - stays a quick test rather than a hundred-curve fit. }
    function CoarseGaussianProfileJson: string;
    function FittableProblem: longint;
    { The Sigma of the model's first curve, over the wire. Used where a test has
      to prove a curve kept the WIDTH a fit found for it - the synthetic peak's
      sigma is 1.5 and a curve type's own default is nothing like it, so this
      separates a restored value from a re-seeded one. }
    function SigmaOfFirstCurve(AId: longint): double;
    { The position of the named parameter within the first curve's parameter
      list - which is what the params route addresses it by. }
    function IndexOfParamNamed(AId: longint; const AName: string): longint;
    { A problem with a profile and one fit interval, ready for picks to be
      written to it. }
    function ProblemReadyForPicks: longint;
    { Every handle the model reports, lower-cased, comma-separated. }
    function HandlesOf(AId: longint): string;
    { The named parameter of the model's first curve, over the wire. }
    function ParamOfFirstCurve(AId: longint; const AName: string): double;
    { A problem with a profile, an interval and one pick under AHandle. }
    function ProblemWithOnePickedCurve(const AHandle: string): longint;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { ROUTE DISPATCH, and nothing else: create a problem, push a profile, read
    positions and handles back, get refused where a refusal is right. None of
    these runs the optimiser, so by the project's own rule none of them is an
    integration test - and until this split they all sat in the slow half, which
    is why fit_service showed 0 % coverage despite being exercised on every one
    of them. TFitRestApi.Handle is a pure (method, path, body) -> (code, body)
    call; there is no socket anywhere in this file. }
  TRestApiTest = class(TRestApiTestBase)
  published
    procedure HealthReportsVersion;
    procedure CreateAndDiscardProblem;
    procedure UnknownProblemIs404;
    procedure ProfileRoundTripsThroughTheProblem;
    procedure AutomaticSetupComputesPointsAndKeepsTheProfile;
    procedure SettingsRoundTrip;
    procedure UnknownActionIs404;
    procedure LossKindRoundTripsAndIsValidated;
    procedure AnUnsupportedCurveTypeIsRefusedNotIgnored;
    procedure MovingAPositionBeforeFittingIsAllowed;
    procedure ARefusalIsAClientErrorNotAServerFault;

    //  THE ACTION ROUTES THAT DO NOT RUN THE OPTIMISER. The fitting ones are
    //  in the integration half; these change the problem and return, so they
    //  belong here - and their REFUSALS are the part nothing exercised.
    procedure SelectingTheEntireProfileSucceeds;
    procedure SelectingAnIntervalTakesItsBounds;
    procedure SelectingAnIntervalWithNoBodyIsRefused;
    procedure SelectingAnIntervalWithRubbishForABodyIsRefused;
    procedure ThatRefusalSaysWhatWasMissing;
    procedure SubtractingABackgroundWithNoBodyIsNotRefused;
    procedure BuildingTheCurveListNeedsACompletedCalculation;
    procedure StoppingWhenNothingIsRunningIsRefused;
    procedure AnAutomaticReductionSurvivesTheNextEdit;

    //  THE ROUTES THAT NEED NO FIT. Everything above was written while the split
    //  was being made; these are the rest of the surface, and they were reachable
    //  from the unit half all along - the point sets, the picks, the user-defined
    //  formula, and how each route refuses. Between them they are most of what
    //  the client asks for on an ordinary session that never presses Fit.
    procedure EveryPointSetRouteAnswersOnAFreshProblem;
    procedure AnEmptySetIsNotAnError;
    procedure TheBackgroundRoundTripsThroughTheProblem;
    procedure TheBoundsRoundTripAsPairs;
    procedure PuttingAnUnknownSetIsRefused;
    procedure TheRFactorIsReadableBeforeAnyFit;
    procedure TheSelectedIntervalIsEmptyUntilOneIsChosen;
    procedure TheAsyncStateIsPollableBeforeAnythingRuns;
    procedure TheStatisticsRouteRefusesRatherThanInventing;

    //  THE HANDLES A PICK WRITE MAY CARRY. Reading a curve's handle out has
    //  always worked; sending one back did not exist, which is why a restored
    //  problem re-seeded instead of resuming. These pin the route's half of it,
    //  including the sets that must refuse handles BY NAME rather than ignore
    //  them - being ignored is how a client comes to believe it restored
    //  something it did not.
    procedure PositionsPutWithIdsKeepsThoseIdsOnTheCurvesItBuilds;
    procedure PositionsPutWithoutIdsBehavesExactlyAsBefore;
    procedure IdsAreRefusedByNameOnASetWhoseMembersCarryNone;
    procedure ThatRefusalNamesTheSetItRefused;
    procedure AMalformedHandleInAPositionsPutIsAClientError;

    //  THE WRITE SIDE OF GET /curves. One request for the whole model, so a
    //  restore is one rebuild rather than one per parameter - and the only
    //  place that can say an OPTIMISER produced the values rather than a seed.
    procedure ValuesPutForTheWholeModelReachIt;
    procedure AnUnknownHandleInAValuesPutIs404;
    procedure AMalformedCurvesBodyIsRefused;
    procedure AValuesPutLeavesThePicksWhereTheUserPutThem;

    //  Every module's project-state in one answer, collected server-side.
    procedure TheModuleStatesRouteAnswersOnAProblemWithNoModules;
    procedure ThatRouteIsAGetAndNothingElse;
    procedure APointIsAddedToTheNamedSet;
    procedure APointIsMovedInTheNamedSet;
    //  Removing one member of a set, by the handle that names it.
    procedure RemovingAMemberNeedsASetWhoseMembersHaveHandles;
    procedure RemovingAMemberOfAnUnknownCurveIs404;
    procedure RemovingAMemberTakesTheCurveAndThePick;
    procedure AddingToAnUnknownSetIsRefused;
    procedure AModuleResourceNobodyProvidesIs404;
    procedure TheSpecialCurveFormulaRoundTrips;
    procedure DeletingTheSpecialCurveFormulaForgetsIt;
    procedure AnUnknownRouteOnAKnownProblemIs404;
    procedure AMalformedProblemIdIs404;
    procedure TheRootPathIsNotAProblem;
    procedure CurvePointsForAnUnknownCurveIs404;

    //  THE ACTIONS THAT ARE NOT A FIT. Seven of the fifteen verbs run an
    //  algorithm over the profile and finish - smoothing, the background
    //  search, peak finding, the interval selection - so none of them is an
    //  integration test by the project's own rule, and none of them had ever
    //  been run by anything but a full session.
    procedure AnUnknownActionNamesTheOnesThatExist;
    procedure SmoothingTheProfileKeepsItsShape;
    procedure ComputingBackgroundPointsProposesSome;
    procedure ProposedBackgroundPointsLieOnTheProfile;
    procedure ComputingCurvePositionsProposesSome;
    procedure ComputingCurveBoundsProposesPairs;
    procedure EveryPointCanBeMadeACurvePosition;
    procedure SubtractingTheBackgroundChangesTheProfile;
    procedure SelectingAnIntervalNarrowsTheProfile;
    procedure AnIntervalNeedsItsTwoEnds;
    procedure GoingBackToTheWholeProfileClearsTheInterval;
    procedure RebuildingTheCurveListNeedsAFinishedFit;
    procedure StoppingWhenNothingRunsIsRefused;
    procedure AnActionOnAnUnknownProblemIs404;
    procedure AnActionIsRefusedBeforeThereIsData;
    //  Endpoints that do not exist, at each length the router checks.
    procedure APathWithNoProblemIdIsNotFound;
    procedure APathWithNoResourceIsNotFound;
    procedure AnUnknownResourceIsNotFound;
    procedure AnUnknownMethodOnAKnownPathIsNotFound;
    procedure EverySettingIsWrittenAndReadBackUnderItsOwnName;
    procedure EveryRouteThatTakesABodyRefusesAMalformedOne;
  end;

  { THE HALF THAT FITS. Each of these drives the minimizer to convergence,
    which is the third of the three things that make a test an integration test:
    not cheap, and not a test of one unit. They stay in the slow half
    deliberately - moving them would buy coverage by redefining the rule. }
  TRestApiFitTest = class(TRestApiTestBase)
  published
    procedure MinimizeDifferenceFitsAndReportsRFactor;
    procedure StatsExposesGoodnessOfFit;
    procedure PythonMinimizerNeedsTheSidecar;
    procedure CurvesAndParamsAreExposed;
    procedure AsyncStatusIsPollable;
    procedure ADeletedUserCurveIsRefusedNotFittedOn;
    procedure ARefusedOperationLeavesTheProblemUsable;
    procedure AFitWithNoPositionsStillSeedsThemForAnOrdinaryCurve;
    procedure AFitDoesNotWriteThePickedPositions;
    procedure TheBuiltModelReportsWhereItsCurvesSit;
    procedure TheFitEditRefitCycleSurvivesOverRest;
    procedure MovingAFittedPositionKeepsTheShapeAndReseeds;
    procedure ARefitResumesFromTheFittedParameters;
    procedure EveryCurveHasADistinctHandle;
    procedure AHandleOutlivesAnEditThatRenumbersTheModel;
    procedure AnUnknownCurveHandleIs404;
    procedure AParameterWrittenByHandleReachesTheModel;
    procedure TheWholeModelWrittenAtOnceReachesTheModelToo;
    procedure AMalformedParameterBodyIsRefusedForARealCurve;
    //  The two verbs nothing had ever called.
    procedure TheWholeSequenceRunsInOnePass;
    procedure AndAFitCanBeContinuedFromWhereItStopped;
    procedure TheLastCurveIsNeverPrunedAway;
    procedure PruningAModelOfSeveralCurvesLeavesItFittable;
    procedure ACurveWhosePositionWasRemovedIsDropped;
    procedure AnAutomaticRunOverASELECTEDINTERVALTakesThePicksWithIt;
    procedure APickBetweenTwoFitIntervalsBelongsToNoTaskAndIsLeftAlone;
  end;

implementation

function TRestApiTestBase.SigmaOfFirstCurve(AId: longint): double;
var
  Code, j: longint;
  R: TJSONObject;
  Params: TJSONArray;
  P: TJSONObject;
begin
  Result := -1;
  R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
  try
    AssertEquals('get curves', 200, Code);
    AssertTrue('the model has a curve', R.Arrays['curves'].Count > 0);
    Params := TJSONObject(R.Arrays['curves'].Items[0]).Arrays['params'];
    for j := 0 to Params.Count - 1 do
    begin
      P := TJSONObject(Params.Items[j]);
      if SameText(P.Get('name', ''), 'Sigma') then
      begin
        Result := P.Get('value', 0.0);
        Exit;
      end;
    end;
  finally
    R.Free;
  end;
end;

function TRestApiTestBase.IndexOfParamNamed(AId: longint;
  const AName: string): longint;
var
  Code, j: longint;
  R: TJSONObject;
  Params: TJSONArray;
begin
  Result := -1;
  R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
  try
    AssertEquals('get curves', 200, Code);
    Params := TJSONObject(R.Arrays['curves'].Items[0]).Arrays['params'];
    for j := 0 to Params.Count - 1 do
      if SameText(TJSONObject(Params.Items[j]).Get('name', ''), AName) then
      begin
        Result := j;
        Exit;
      end;
  finally
    R.Free;
  end;
  AssertTrue(Format('the curve has a %s parameter', [AName]), Result >= 0);
end;

procedure TRestApiTestBase.SetUp;
begin
  FApi := TFitRestApi.Create;
end;

procedure TRestApiTestBase.TearDown;
begin
  FreeAndNil(FApi);
end;

function TRestApiTestBase.Call(const M, P, B: string; out Code: longint): TJSONObject;
var
  Body: string;
  D: TJSONData;
begin
  FApi.Handle(M, P, B, Code, Body);
  Result := nil;
  D := nil;
  try
    D := GetJSON(Body);
  except
    D := nil;
  end;
  if D is TJSONObject then
    Result := TJSONObject(D)
  else
    D.Free;
end;

function TRestApiTestBase.NewProblem: longint;
var
  Code: longint;
  R: TJSONObject;
begin
  R := Call('POST', '/problems', '', Code);
  try
    AssertEquals('create problem status', 200, Code);
    AssertTrue('created', R.Get('ok', False));
    Result := R.Get('id', -1);
    AssertTrue('got an id', Result > 0);
  finally
    R.Free;
  end;
end;

function TRestApiTestBase.GaussianProfileJson: string;
var
  P: TPointsData;
  x: double;
  n: integer;
begin
  P := Default(TPointsData);
  P.Title := 'profile';
  n := 0;
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    SetLength(P.X, n + 1);
    SetLength(P.Y, n + 1);
    P.X[n] := x;
    P.Y[n] := GaussPoint(100, 1.5, 10, x);
    Inc(n);
    x := x + 0.2;
  end;
  Result := PointsToJsonString(P);
end;

procedure TRestApiTest.HealthReportsVersion;
var Code: longint; R: TJSONObject;
begin
  R := Call('GET', '/health', '', Code);
  try
    AssertEquals('status', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
    AssertEquals('version', WORKER_PROTOCOL_VERSION, R.Get('version', -1));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.CreateAndDiscardProblem;
var Id, Code: longint; R: TJSONObject;
begin
  Id := NewProblem;
  AssertEquals('one live problem', 1, FApi.Sessions.Count);

  R := Call('DELETE', Format('/problems/%d', [Id]), '', Code);
  try
    AssertEquals('delete status', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;
  AssertEquals('problem discarded', 0, FApi.Sessions.Count);
end;

procedure TRestApiTest.UnknownProblemIs404;
var Code: longint; R: TJSONObject;
begin
  R := Call('GET', '/problems/999/state', '', Code);
  try
    AssertEquals('status', 404, Code);
    AssertFalse('not ok', R.Get('ok', True));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.ProfileRoundTripsThroughTheProblem;
var
  Id, Code: longint;
  R: TJSONObject;
  Got: TPointsData;
  Body: string;
begin
  Id := NewProblem;

  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  try
    AssertEquals('put profile status', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  //  Read it back through the API.
  FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
  AssertEquals('get profile status', 200, Code);
  AssertTrue('profile decoded', PointsFromJsonString(Body, Got));
  AssertEquals('point count survives', 101, Length(Got.X));
  AssertEquals('x[50]', 10.0, Got.X[50], 1e-9);
  AssertEquals('y[50] is the peak', GaussPoint(100, 1.5, 10, 10), Got.Y[50], 1e-9);
end;

{ The long-running actions run through TFitService.RecreateMainCalcThread, whose
  done procedure restores FSavedState. If the service does not enter
  AsyncOperation first, that restore lands on the state preceding the current
  one - ProfileWaiting after SetProfile - whose handler clears the whole
  problem: the action reports success, yet the computed points and even the
  profile are gone. }
procedure TRestApiTest.AutomaticSetupComputesPointsAndKeepsTheProfile;
var
  Id, Code: longint;
  R: TJSONObject;
  Got: TPointsData;
  Body: string;

  procedure Act(const AName: string);
  begin
    R := Call('POST', Format('/problems/%d/actions/%s', [Id, AName]), '', Code);
    try
      AssertEquals(AName + ' status', 200, Code);
      AssertTrue(AName + ' ok', R.Get('ok', False));
    finally
      R.Free;
    end;
  end;

  function PointsOf(const ASet: string): longint;
  begin
    FApi.Handle('GET', Format('/problems/%d/%s', [Id, ASet]), '', Code, Body);
    AssertEquals('get ' + ASet + ' status', 200, Code);
    AssertTrue(ASet + ' decoded', PointsFromJsonString(Body, Got));
    Result := Length(Got.X);
  end;

begin
  Id := NewProblem;
  FApi.Handle('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code, Body);
  AssertEquals('put profile status', 200, Code);

  Act('compute-background-points');
  AssertTrue('background points were found', PointsOf('background') > 0);

  Act('compute-curve-positions');
  AssertTrue('curve positions were found', PointsOf('positions') > 0);

  Act('compute-curve-bounds');
  AssertTrue('R-factor bounds were found', PointsOf('rfactor-bounds') > 0);

  //  None of the above may destroy the problem it was computed from.
  AssertEquals('the profile survives the actions', 101, PointsOf('profile'));
end;

{ ---- the handles a pick write may carry ----------------------------------- }

{ A problem with a profile and one fit interval over all of it, which is what
  makes the model build: GoToReadyForFit creates no task without bounds, so
  there would be picks and no curves to carry a handle. }
function TRestApiTestBase.ProblemReadyForPicks: longint;
var
  Code: longint;
  R: TJSONObject;
  B: TPointsData;
begin
  Result := NewProblem;
  R := Call('PUT', Format('/problems/%d/settings', [Result]),
    Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/profile', [Result]),
    CoarseGaussianProfileJson, Code);
  R.Free;
  B := Default(TPointsData);
  SetLength(B.X, 2);
  SetLength(B.Y, 2);
  B.X[0] := 0;  B.Y[0] := 0;
  B.X[1] := 20; B.Y[1] := 0;
  R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
    PointsToJsonString(B), Code);
  R.Free;
end;

{ Every handle the model reports, comma-separated and lower-cased, in model
  order. Lower-cased because the wire form of a handle carries no braces and
  GUIDToString brackets and upper-cases: what is compared is the identity, not
  one library's spelling of it. }
function TRestApiTestBase.HandlesOf(AId: longint): string;
var
  Code, i: longint;
  R: TJSONObject;
  Curves: TJSONArray;
begin
  Result := '';
  R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
  try
    if R = nil then
      Exit;
    if not (R.Find('curves') is TJSONArray) then
      Exit;
    Curves := TJSONArray(R.Find('curves'));
    for i := 0 to Curves.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + LowerCase(TJSONObject(Curves.Items[i]).Get('id', ''));
    end;
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.PositionsPutWithIdsKeepsThoseIdsOnTheCurvesItBuilds;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE ROUND TRIP THAT MAKES A PROJECT FILE POSSIBLE: the handle a client
  //  sends with a pick is the handle the model reports for the curve built
  //  there, so values saved under it can be handed back afterwards.
  Id := ProblemReadyForPicks;
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    '{"x":[6,14],"y":[20,20],' +
    '"ids":["0a0a0a0a-1111-2222-3333-444444444444",' +
    '"0b0b0b0b-1111-2222-3333-444444444444"]}', Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
  AssertEquals('each curve carries the handle its pick was sent with',
    '0a0a0a0a-1111-2222-3333-444444444444,' +
    '0b0b0b0b-1111-2222-3333-444444444444', HandlesOf(Id));
end;

procedure TRestApiTest.PositionsPutWithoutIdsBehavesExactlyAsBefore;
var
  Id, Code: longint;
  R: TJSONObject;
  Handles: string;
begin
  //  ADDITIVE. Every client that predates the field sends no ids, and must get
  //  what it always got - handles issued by the server, one per pick.
  Id := ProblemReadyForPicks;
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    '{"x":[6,14],"y":[20,20]}', Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
  Handles := HandlesOf(Id);
  AssertTrue('two curves were still built', Pos(',', Handles) > 0);
  AssertTrue('and both were issued a handle', Length(Handles) > 2);
end;

procedure TRestApiTest.IdsAreRefusedByNameOnASetWhoseMembersCarryNone;
var
  Id, Code: longint;
  R: TJSONObject;

  procedure Refuses(const ASet: string);
  begin
    R := Call('PUT', Format('/problems/%d/%s', [Id, ASet]),
      '{"x":[1],"y":[2],"ids":["0a0a0a0a-1111-2222-3333-444444444444"]}', Code);
    try
      AssertEquals(ASet + ' must refuse handles', 400, Code);
    finally
      R.Free;
    end;
  end;

begin
  //  BY NAME, NOT IGNORED. A curve identity is issued to the pick it is seeded
  //  from, so a pick can be named and a profile sample cannot. Accepting the
  //  field and dropping it would let a client believe it had restored an
  //  identity that never existed - the same failure the DELETE member route
  //  refuses by name for the same reason.
  Id := ProblemReadyForPicks;
  Refuses('profile');
  Refuses('background');
  Refuses('rfactor-bounds');
end;

procedure TRestApiTest.ThatRefusalNamesTheSetItRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  A refusal explains itself. "malformed point set" would send the reader
  //  looking at their coordinates.
  Id := ProblemReadyForPicks;
  R := Call('PUT', Format('/problems/%d/background', [Id]),
    '{"x":[1],"y":[2],"ids":["0a0a0a0a-1111-2222-3333-444444444444"]}', Code);
  try
    AssertTrue('the message says which set',
      Pos('background', LowerCase(R.Get('error', ''))) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AMalformedHandleInAPositionsPutIsAClientError;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  400, not 500: the request was inadmissible and will fail again
  //  identically. Text that is not a handle must not read as "no handle",
  //  which would silently build a fresh curve and drop the saved values.
  Id := ProblemReadyForPicks;
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    '{"x":[6],"y":[20],"ids":["not-a-handle"]}', Code);
  try
    AssertEquals('a client error', 400, Code);
  finally
    R.Free;
  end;
end;


{ ---- every module's project state ------------------------------------------ }

procedure TRestApiTest.TheModuleStatesRouteAnswersOnAProblemWithNoModules;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE PUBLISHED FRAMEWORK'S OWN CASE: no module keeps anything, and the
  //  answer is an empty list rather than an error. Saving must not fail in a
  //  build with no analysis pack in it - which is every build of this
  //  repository.
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d/module-states', [Id]), '', Code);
  try
    AssertEquals('answered', 200, Code);
    AssertTrue('with a list', R.Find('states') is TJSONArray);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.ThatRouteIsAGetAndNothingElse;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  Collecting is a read. A POST to it would be a second way to write module
  //  state, beside the module's own resource - which is the bypass this route
  //  was careful not to be.
  Id := NewProblem;
  R := Call('POST', Format('/problems/%d/module-states', [Id]), '{}', Code);
  try
    AssertEquals('not a route', 404, Code);
  finally
    R.Free;
  end;
end;

{ ---- the write side of GET /curves ---------------------------------------- }

{ The named parameter of the model's first curve, over the wire. }
function TRestApiTestBase.ParamOfFirstCurve(AId: longint;
  const AName: string): double;
var
  Code, j: longint;
  R: TJSONObject;
  Curves, Params: TJSONArray;
begin
  Result := 0;
  R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
  try
    if not (R.Find('curves') is TJSONArray) then
      Exit;
    Curves := TJSONArray(R.Find('curves'));
    if Curves.Count = 0 then
      Exit;
    Params := TJSONArray(TJSONObject(Curves.Items[0]).Find('params'));
    for j := 0 to Params.Count - 1 do
      if TJSONObject(Params.Items[j]).Get('name', '') = AName then
        Exit(TJSONObject(Params.Items[j]).Get('value', 0.0));
  finally
    R.Free;
  end;
end;

{ A problem with a profile, an interval and one pick under a known handle. }
function TRestApiTestBase.ProblemWithOnePickedCurve(const AHandle: string): longint;
var
  Code: longint;
  R: TJSONObject;
begin
  Result := ProblemReadyForPicks;
  R := Call('PUT', Format('/problems/%d/positions', [Result]),
    Format('{"x":[10],"y":[20],"ids":["%s"]}', [AHandle]), Code);
  R.Free;
end;

procedure TRestApiTest.ValuesPutForTheWholeModelReachIt;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE OTHER HALF OF THE ROUND TRIP. Picks go out with their handles; the
  //  values a fit found come back under the same handles, in one request.
  Id := ProblemWithOnePickedCurve('0a0a0a0a-1111-2222-3333-444444444444');
  R := Call('PUT', Format('/problems/%d/curves', [Id]),
    '{"curves":[{"id":"0a0a0a0a-1111-2222-3333-444444444444",' +
    '"fitted":true,"params":[{"name":"sigma","value":0.4,"error":0.01}]}]}',
    Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
  AssertEquals('the width the restore sent is the width the model has',
    0.4, ParamOfFirstCurve(Id, 'sigma'), 1e-9);
end;

procedure TRestApiTest.AnUnknownHandleInAValuesPutIs404;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  404, and NOT a write to curve zero. Both curve routes used to run their
  //  path segment through StrToIntDef(..., 0), so a stale address silently read
  //  - and on the params route WROTE - the first curve in the model. A restore
  //  addresses every curve it has, so it is the request most likely to carry a
  //  handle the model has since lost.
  Id := ProblemWithOnePickedCurve('0a0a0a0a-1111-2222-3333-444444444444');
  R := Call('PUT', Format('/problems/%d/curves', [Id]),
    '{"curves":[{"id":"0c0c0c0c-9999-9999-9999-999999999999",' +
    '"fitted":true,"params":[{"name":"sigma","value":0.4}]}]}', Code);
  try
    AssertEquals('unknown handle', 404, Code);
  finally
    R.Free;
  end;
  AssertTrue('and the curve that IS there was not written to',
    Abs(ParamOfFirstCurve(Id, 'sigma') - 0.4) > 1e-9);
end;

procedure TRestApiTest.AMalformedCurvesBodyIsRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := ProblemWithOnePickedCurve('0a0a0a0a-1111-2222-3333-444444444444');
  R := Call('PUT', Format('/problems/%d/curves', [Id]), 'not json', Code);
  try
    AssertEquals('refused', 400, Code);
  finally
    R.Free;
  end;
  R := Call('PUT', Format('/problems/%d/curves', [Id]), '{"curves":42}', Code);
  try
    AssertEquals('and a curves field that is not a list', 400, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AValuesPutLeavesThePicksWhereTheUserPutThem;
var
  Id, Code: longint;
  R: TJSONObject;
  Picks: TPointsData;
begin
  //  A pick set is model INPUT, and writing fitted values back is not a fit.
  //  Writing a fitted x over a picked one breaks the uniqueness the whole
  //  identity scheme rests on, and reports it as a crash several gestures later.
  Id := ProblemWithOnePickedCurve('0a0a0a0a-1111-2222-3333-444444444444');
  R := Call('PUT', Format('/problems/%d/curves', [Id]),
    '{"curves":[{"id":"0a0a0a0a-1111-2222-3333-444444444444",' +
    '"fitted":true,"params":[{"name":"x0","value":11.7}]}]}', Code);
  R.Free;
  R := Call('GET', Format('/problems/%d/positions', [Id]), '', Code);
  try
    AssertTrue('the picks came back', PointsFromJson(R, Picks));
    AssertEquals('one pick', 1, Length(Picks.X));
    AssertEquals('still where it was picked', 10.0, Picks.X[0], 1e-9);
  finally
    R.Free;
  end;
end;


procedure TRestApiFitTest.MinimizeDifferenceFitsAndReportsRFactor;
var
  Id, Code: longint;
  R: TJSONObject;
  RFactorStr: string;
  CurMin: double;
  Pos: TPointsData;
begin
  //  The whole point of the transport: run a real fit through the REST verbs,
  //  driving the problem the way a client does - profile, then curve positions,
  //  then minimize.
  Id := NewProblem;

  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  One curve at the peak. The y is load-bearing: the engine seeds the curve's
  //  amplitude from it (see RecreateCurves).
  Pos := Default(TPointsData);
  Pos.Title := 'positions';
  SetLength(Pos.X, 1);
  SetLength(Pos.Y, 1);
  Pos.X[0] := 10;
  Pos.Y[0] := GaussPoint(100, 1.5, 10, 10);

  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  try
    AssertEquals('minimize status', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/rfactor', [Id]), '', Code);
  try
    AssertEquals('rfactor status', 200, Code);
    RFactorStr := R.Get('rFactor', '');
    AssertTrue('server reports an R-factor (' + RFactorStr + ')', RFactorStr <> '');
    //  Strictly > 0: a 0 would mean the optimizer never actually ran.
    CurMin := R.Get('curMin', -1.0);
    AssertTrue('the fit genuinely converged (curMin=' + FloatToStr(CurMin) + ')',
      (CurMin > 0) and (CurMin < 0.05));
  finally
    R.Free;
  end;
end;

{ Sets up a problem with the Gaussian profile and one curve at the peak. }
function TRestApiTestBase.CoarseGaussianProfileJson: string;
var
  P: TPointsData;
  i: longint;
  x: double;
begin
  P := Default(TPointsData);
  P.Title := 'profile';
  SetLength(P.X, 21);
  SetLength(P.Y, 21);
  for i := 0 to 20 do
  begin
    x := i;
    P.X[i] := x;
    P.Y[i] := GaussPoint(100, 1.5, 10, x);
  end;
  Result := PointsToJsonString(P);
end;

function TRestApiTestBase.FittableProblem: longint;
var
  Code: longint;
  R: TJSONObject;
  Pos: TPointsData;
begin
  Result := NewProblem;
  //  Asked for by name, not inherited from whatever was selected last. The
  //  curve-type selection is process-global, so a problem that does not state
  //  its type gets the previous one - and a suite that fits a Gaussian must not
  //  depend on which suite ran before it.
  R := Call('PUT', Format('/problems/%d/settings', [Result]),
    Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/profile', [Result]), GaussianProfileJson, Code);
  R.Free;
  Pos := Default(TPointsData);
  SetLength(Pos.X, 1);
  SetLength(Pos.Y, 1);
  Pos.X[0] := 10;
  Pos.Y[0] := GaussPoint(100, 1.5, 10, 10);
  R := Call('PUT', Format('/problems/%d/positions', [Result]),
    PointsToJsonString(Pos), Code);
  R.Free;
end;

procedure TRestApiFitTest.StatsExposesGoodnessOfFit;
var
  Id, Code: longint;
  R, Stats: TJSONObject;
  D: TJSONData;
begin
  Id := FittableProblem;
  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  R := Call('GET', Format('/problems/%d/stats', [Id]), '', Code);
  try
    AssertEquals('stats status', 200, Code);
    D := R.Find('statistics');
    AssertTrue('statistics present', D is TJSONObject);
    Stats := TJSONObject(D);
    AssertTrue('statistics are valid after a fit', Stats.Get('valid', False));
    AssertTrue('data points counted', Stats.Get('dataPoints', 0) > 0);
    //  A clean single-Gaussian fit explains almost all the variance over the
    //  (auto-selected) fitting window.
    AssertTrue('R-squared is high (' + FloatToStr(Stats.Get('rSquared', 0.0)) + ')',
      Stats.Get('rSquared', 0.0) > 0.95);
  finally
    R.Free;
  end;
end;

{ Selecting the Python minimizer (MIN_KIND_PYTHON_LM) makes a fit need the
  sidecar fit_server owns. With no sidecar wired (as in this unit test), a fit
  action reports it is unavailable rather than running - and the native path is
  unaffected. This is the framework path: Python is chosen by the same minimizer
  setting as the native algorithm, not a separate API. }
procedure TRestApiFitTest.PythonMinimizerNeedsTheSidecar;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := FittableProblem;
  //  Switch this problem to the Python minimizer through the normal settings.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"minimizerKind":1}', Code);
  R.Free;
  AssertEquals('settings accepted', 200, Code);

  //  The very same minimize verb the native fit uses.
  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  try
    AssertEquals('no sidecar -> unavailable', 503, Code);
    AssertFalse('not ok', R.Get('ok', True));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.SettingsRoundTrip;
var Id, Code: longint; R: TJSONObject;
begin
  Id := NewProblem;

  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"maxRFactor":0.05,"waveLength":1.54,"backgroundVariation":true}', Code);
  try
    AssertEquals('put settings', 200, Code);
    AssertEquals('maxRFactor applied', 0.05, R.Get('maxRFactor', 0.0), 1e-9);
    AssertEquals('waveLength applied', 1.54, R.Get('waveLength', 0.0), 1e-9);
    AssertTrue('backgroundVariation applied', R.Get('backgroundVariation', False));
  finally
    R.Free;
  end;

  //  Absent fields must be left alone.
  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('get settings', 200, Code);
    AssertEquals('still set', 1.54, R.Get('waveLength', 0.0), 1e-9);
    AssertTrue('curve type reported', R.Get('curveType', '') <> '');
  finally
    R.Free;
  end;
end;

{ The objective travels over the wire like any other setting, and an unknown one
  is refused AT THE BOUNDARY. Validating here rather than at the point of use
  matters: an unknown kind reaching the engine raises in the middle of a fit,
  which is a far worse place to discover it. }
procedure TRestApiTest.LossKindRoundTripsAndIsValidated;
var Id, Code: longint; R: TJSONObject;
begin
  Id := NewProblem;

  //  A fresh problem must already report the corrected R-factor (0), so a client
  //  that never sets one is on the right objective.
  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('get settings', 200, Code);
    AssertEquals('default objective is the corrected R-factor',
      0, R.Get('lossKind', -1));
  finally
    R.Free;
  end;

  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"lossKind":3}', Code);
  try
    AssertEquals('put settings', 200, Code);
    AssertEquals('lossKind applied', 3, R.Get('lossKind', -1));
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('lossKind persisted', 3, R.Get('lossKind', -1));
  finally
    R.Free;
  end;

  //  Out of range: refused rather than stored and surprising someone later.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"lossKind":99}', Code);
  try
    AssertTrue(Format('an unknown objective must be refused, got %d', [Code]),
      Code >= 400);
  finally
    R.Free;
  end;

  //  And the refusal must not have corrupted the stored value.
  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('the rejected value was not stored', 3, R.Get('lossKind', -1));
  finally
    R.Free;
  end;
end;
procedure TRestApiFitTest.ARefusedOperationLeavesTheProblemUsable;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := FittableProblem;

  //  Any refusal from inside an operation will do; this one needs no set-up
  //  beyond selecting a type whose formula is missing.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    Format('{"curveType":"%s"}', [GUIDToString(TUserPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  try
    AssertTrue('the fit is refused', Code >= 400);
  finally
    R.Free;
  end;

  //  Not busy any more: the operation that failed is over.
  R := Call('GET', Format('/problems/%d/async', [Id]), '', Code);
  try
    AssertEquals('async status', 200, Code);
    AssertFalse('the problem is not left marked busy', R.Get('busy', True));
  finally
    R.Free;
  end;

  //  What the user actually did next, and what used to fail.
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  try
    AssertEquals('another file can still be opened', 200, Code);
  finally
    R.Free;
  end;

  //  And Stop, on a problem with nothing running, is refused in words rather
  //  than by calling an abstract method.
  R := Call('POST', Format('/problems/%d/actions/stop', [Id]), '', Code);
  try
    AssertTrue('stop with nothing running is refused', Code >= 400);
    AssertTrue('and says so: ' + R.Get('error', ''),
      Pos('Abstract', R.Get('error', '')) = 0);
  finally
    R.Free;
  end;
end;

{ The user deletes the user-defined curve that is being fitted. The client says
  so with DELETE /special-params; from then on the problem must refuse to fit
  rather than go on building curves from the formula it still remembers - which
  is what put "User Defined" curves on a chart whose menu no longer had any user
  curve at all. }
procedure TRestApiFitTest.ADeletedUserCurveIsRefusedNotFittedOn;
var
  Id, Code: longint;
  R: TJSONObject;
  Msg: string;
begin
  Id := FittableProblem;

  //  Created the way the client creates one: the formula first, then the type.
  R := Call('PUT', Format('/problems/%d/special-params', [Id]),
    '{"expression":"A*exp(-sqr((x-x0)/SIGMA))"}', Code);
  try
    AssertEquals('the formula is accepted', 200, Code);
  finally
    R.Free;
  end;

  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    Format('{"curveType":"%s"}', [GUIDToString(TUserPointsSet.GetCurveTypeId)]),
    Code);
  try
    AssertEquals('the user-defined type is selected', 200, Code);
  finally
    R.Free;
  end;

  //  It fits while the curve exists, so the refusal below can only be the
  //  deletion and not some other thing this problem is missing.
  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  try
    AssertEquals('a user curve with a formula fits', 200, Code);
  finally
    R.Free;
  end;

  R := Call('DELETE', Format('/problems/%d/special-params', [Id]), '', Code);
  try
    AssertEquals('the formula can be dropped', 200, Code);
  finally
    R.Free;
  end;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  try
    AssertTrue(Format('fitting a deleted user curve must be refused, got %d',
      [Code]), Code >= 400);
    Msg := R.Get('error', '');
    //  A refusal the user cannot act on is barely better than the wrong result:
    //  the message has to say what to do about it.
    AssertTrue('the refusal says which menu fixes it: ' + Msg,
      Pos('Curve Type', Msg) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AnUnsupportedCurveTypeIsRefusedNotIgnored;
var
  Id, Code: longint;
  R: TJSONObject;
  Before, After: string;
begin
  Id := NewProblem;

  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    Before := R.Get('curveType', '');
  finally
    R.Free;
  end;

  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"curveType":"{00000000-0000-0000-0000-0000DEADBEEF}"}', Code);
  try
    AssertTrue(Format('an unregistered curve type must be refused, got %d',
      [Code]), Code >= 400);
  finally
    R.Free;
  end;

  //  And the refusal must not have quietly changed anything either.
  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    After := R.Get('curveType', '');
    AssertEquals('the selection is unchanged after a refusal', Before, After);
  finally
    R.Free;
  end;
end;

procedure TRestApiFitTest.CurvesAndParamsAreExposed;
var
  Id, Code: longint;
  R: TJSONObject;
  Curves: TJSONArray;
  Params: TJSONArray;
begin
  Id := FittableProblem;
  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;

  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertEquals('get curves', 200, Code);
    Curves := R.Arrays['curves'];
    AssertEquals('one fitted curve', 1, Curves.Count);
    Params := TJSONObject(Curves.Items[0]).Arrays['params'];
    AssertTrue('curve exposes parameters', Params.Count > 0);
    AssertTrue('a parameter has a name',
      TJSONObject(Params.Items[0]).Get('name', '') <> '');
    //  Every parameter carries an uncertainty field. The native engine does not
    //  estimate one, so it is negative here; the Python backend fills it in.
    AssertTrue('a parameter carries an error field',
      TJSONObject(Params.Items[0]).Find('error') <> nil);
    AssertTrue('native leaves the error unestimated (negative)',
      TJSONObject(Params.Items[0]).Get('error', 0.0) < 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiFitTest.AsyncStatusIsPollable;
var Id, Code: longint; R: TJSONObject;
begin
  //  The thin client polls this instead of receiving in-process callbacks.
  Id := FittableProblem;
  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;

  R := Call('GET', Format('/problems/%d/async', [Id]), '', Code);
  try
    AssertEquals('async status', 200, Code);
    AssertFalse('not busy after a synchronous op', R.Get('busy', True));
    AssertTrue('reports completion', R.Get('done', False));
    AssertTrue('reports the achieved R-factor',
      R.Get('curMin', -1.0) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.UnknownActionIs404;
var Id, Code: longint; R: TJSONObject;
begin
  Id := NewProblem;
  R := Call('POST', Format('/problems/%d/actions/nonsense', [Id]), '', Code);
  try
    AssertEquals('status', 404, Code);
    AssertFalse('not ok', R.Get('ok', True));
  finally
    R.Free;
  end;
end;

{ THE AUTOMATIC SEEDING A PEAK TYPE DEPENDS ON. Fitting without having picked a
  single position is the ordinary way to use this program: the service seeds
  them itself, one per candidate sample, and the fit then prunes.

  The guard that stops that seeding for a curve type placed from its own point
  set (where the positions describe nothing and were drawn on the chart as
  one marker per sample) must not reach a Gaussian. Asserted through the REST
  surface because that is where the client sees them. }
procedure TRestApiFitTest.AFitWithNoPositionsStillSeedsThemForAnOrdinaryCurve;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), CoarseGaussianProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  NOTHING is picked: no positions, no R-factor bounds.
  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Got));
  AssertEquals('nothing is picked to begin with', 0, Length(Got.X));

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  try
    AssertEquals('minimize status', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Got));
  AssertTrue('the service seeded the positions itself', Length(Got.X) > 0);
end;

{ THE PICKS ARE INPUT AND A FIT MUST LEAVE THEM ALONE.

  This is the defect that crashed the client: a finished fit wrote the built
  curves' fitted x0 back into the picked positions, which have to be unique and
  have to name real samples of the profile. The next edit then asserted inside
  TPointsSet.Sort, and behind that waited a failed grid lookup in CreateTasks.
  Stated here as the rule it broke, at the surface the client actually uses. }
procedure TRestApiFitTest.AFitDoesNotWriteThePickedPositions;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Before, After: TPointsData;
begin
  Id := FittableProblem;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Before));
  AssertEquals('one position was picked', 1, Length(Before.X));

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  try
    AssertEquals('minimize status', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, After));
  AssertEquals('the fit did not add or remove a pick',
    Length(Before.X), Length(After.X));
  //  EXACTLY equal, not close: a fitted x0 lands near the pick, so a tolerant
  //  comparison would have passed against the very defect this defends.
  AssertTrue('the pick is bit-for-bit what was picked',
    After.X[0] = Before.X[0]);
  AssertTrue('and so is its y', After.Y[0] = Before.Y[0]);
end;

{ The other half of that split: what the model WAS built into is reported, just
  not in the picks. One point per instance, at the instance's own fitted x0. }
procedure TRestApiFitTest.TheBuiltModelReportsWhereItsCurvesSit;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
  Curves: TJSONObject;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  FApi.Handle('GET', Format('/problems/%d/calc-positions', [Id]), '', Code, Body);
  AssertEquals('calc-positions status', 200, Code);
  AssertTrue('calc-positions decoded', PointsFromJsonString(Body, Got));

  Curves := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertEquals('one marker per built instance',
      Curves.Arrays['curves'].Count, Length(Got.X));
  finally
    Curves.Free;
  end;
end;

{ THE FEATURE, end to end and over the wire: fit, change the model, fit again.

  Adding a pick after a fit is the gesture that used to crash. The second fit has
  to be accepted and has to produce a model that includes both curves - which is
  also what proves the rebuild did not simply throw the first one away. }
procedure TRestApiFitTest.TheFitEditRefitCycleSurvivesOverRest;
var
  Id, Code: longint;
  R, Curves: TJSONObject;
  Body: string;
  Got: TPointsData;
  FirstCount: longint;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('the first fit runs', 200, Code);

  Curves := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    FirstCount := Curves.Arrays['curves'].Count;
  finally
    Curves.Free;
  end;
  AssertTrue('the first fit built something', FirstCount > 0);

  //  A second seed, away from the first so it is a genuinely new instance.
  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    Format('{"x":%s,"y":%s}',
      [FloatToStr(6.0), FloatToStr(GaussPoint(100, 1.5, 10, 6.0))]), Code);
  try
    AssertEquals('the pick is accepted after a fit', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Got));
  AssertEquals('both picks are held', 2, Length(Got.X));

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  try
    AssertEquals('the second fit runs', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  Curves := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertTrue('the edited model has more curves than the first fit did',
      Curves.Arrays['curves'].Count > FirstCount);
  finally
    Curves.Free;
  end;
end;

{ MOVING A FITTED PICK KEEPS THE SHAPE AND TAKES THE POSITION FROM THE PICK.

  This used to be REFUSED, and the refusal was honest at the time: the key a
  curve's fitted values were stored under was computed from its seed, so moving
  the pick changed the key and the values could no longer be found - that curve
  alone reverting to its starting guess while its neighbours kept theirs.

  The key is now a handle issued to the pick, and a move carries it across
  (curve_identity_registry.TakeSeedFrom). So the operation is ordinary: the
  curve keeps everything the optimiser found about its SHAPE, and where it sits
  comes from the pick the user just moved. Sigma is what proves the first half -
  the synthetic peak has sigma 1.5 and the curve type's own default is nothing
  like it - and the position proves the second. }
procedure TRestApiFitTest.MovingAFittedPositionKeepsTheShapeAndReseeds;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
  SigmaBefore, SigmaAfter: double;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  SigmaBefore := SigmaOfFirstCurve(Id);
  AssertTrue('the fit found a width', SigmaBefore > 0);

  R := Call('PUT', Format('/problems/%d/points/positions', [Id]),
    Format('{"prevX":%s,"prevY":%s,"x":%s,"y":%s}',
      [FloatToStr(10.0), FloatToStr(GaussPoint(100, 1.5, 10, 10)),
       FloatToStr(6.0), FloatToStr(GaussPoint(100, 1.5, 10, 6.0))]), Code);
  try
    AssertEquals('the move is allowed', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Got));
  AssertEquals('still one pick', 1, Length(Got.X));
  AssertTrue('and it moved', Abs(Got.X[0] - 6.0) < 1e-9);

  //  THE HALF THAT USED TO BE IMPOSSIBLE. A rebuild happens as part of the
  //  move, so this reads the model the move produced - not a stale one.
  SigmaAfter := SigmaOfFirstCurve(Id);
  AssertEquals('the curve kept the width the fit found',
    SigmaBefore, SigmaAfter, 1e-9);
end;

{ The refusal must not take the gesture away when it costs nothing. With no fit
  behind it there is nothing to lose, and moving a pick is ordinary. }
procedure TRestApiTest.MovingAPositionBeforeFittingIsAllowed;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
begin
  Id := FittableProblem;

  R := Call('PUT', Format('/problems/%d/points/positions', [Id]),
    Format('{"prevX":%s,"prevY":%s,"x":%s,"y":%s}',
      [FloatToStr(10.0), FloatToStr(GaussPoint(100, 1.5, 10, 10)),
       FloatToStr(6.0), FloatToStr(GaussPoint(100, 1.5, 10, 6.0))]), Code);
  try
    AssertEquals('the move is allowed', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Got));
  AssertEquals('still one pick', 1, Length(Got.X));
  AssertTrue('and it is where it was moved to', Abs(Got.X[0] - 6.0) < 1e-9);
end;

{ THE INCREMENTAL INVARIANT: an edit does not undo the previous round.

  Every model edit demolishes the task list and rebuilds every instance from the
  picks, so on the face of it the fit would be lost. It is not, and the mechanism
  that saves it is the reason picks may not be quietly rewritten: an instance is
  rebuilt from its pick, is given back the handle issued to that pick, and
  TFitTask.RestoreCurveValues hands it the values stored under that handle.

  So this reads a genuinely fitted parameter, edits the model somewhere else, and
  requires that parameter to still be there. Sigma is the one to look at: the
  synthetic peak has sigma 1.5, and a curve rebuilt from its seed without the
  restore would carry the curve type's own default instead - which is what this
  test would catch. }
procedure TRestApiFitTest.ARefitResumesFromTheFittedParameters;

  { The sigma of the curve seeded near AX0.

    BY POSITION, not by index. Adding a pick renumbers the list - the picks are
    sorted, so a new pick to the left of an existing one becomes curve 0 - and an
    index would then read the brand-new curve and find its seed value, which is
    what this test is trying to distinguish from a lost restore. }
  function SigmaOfCurveNear(AId: longint; AX0: double): double;
  var
    Code, i, j: longint;
    R: TJSONObject;
    Params: TJSONArray;
    P: TJSONObject;
    Sigma, X0: double;
    Found: boolean;
  begin
    Result := -1;
    R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    try
      AssertEquals('get curves', 200, Code);
      AssertTrue('the model has a curve', R.Arrays['curves'].Count > 0);
      for i := 0 to R.Arrays['curves'].Count - 1 do
      begin
        Params := TJSONObject(R.Arrays['curves'].Items[i]).Arrays['params'];
        Sigma := -1;
        X0 := MaxDouble;
        Found := False;
        for j := 0 to Params.Count - 1 do
        begin
          P := TJSONObject(Params.Items[j]);
          if SameText(P.Get('name', ''), 'Sigma') then
            Sigma := P.Get('value', 0.0);
          if SameText(P.Get('name', ''), 'x0') then
          begin
            X0 := P.Get('value', 0.0);
            Found := True;
          end;
        end;
        //  Generous: x0 is fitted, so it has moved off the pick - but not as far
        //  as the next pick, which is what this has to separate it from.
        if Found and (Abs(X0 - AX0) < 2.0) then
        begin
          Result := Sigma;
          Exit;
        end;
      end;
    finally
      R.Free;
    end;
  end;

var
  Id, Code: longint;
  R: TJSONObject;
  Fitted, AfterEdit: double;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('the first fit runs', 200, Code);

  Fitted := SigmaOfCurveNear(Id, 10.0);
  AssertTrue('the curve exposes a Sigma', Fitted > 0);
  //  It really was fitted, rather than left at whatever it started as: the
  //  synthetic peak's sigma is 1.5, and the test is only meaningful if the
  //  optimiser actually found it.
  AssertEquals('sigma was fitted to the synthetic peak', 1.5, Fitted, 0.15);

  //  An edit ELSEWHERE. This is what frees the task list and rebuilds every
  //  instance, including the one just measured.
  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    Format('{"x":%s,"y":%s}',
      [FloatToStr(3.0), FloatToStr(GaussPoint(100, 1.5, 10, 3.0))]), Code);
  R.Free;
  AssertEquals('the pick is accepted', 200, Code);

  AfterEdit := SigmaOfCurveNear(Id, 10.0);
  //  Tight: this is a restore, not a re-fit, so the value must come back
  //  identical rather than merely close.
  AssertEquals('the fitted sigma survived the rebuild', Fitted, AfterEdit, 1e-9);
end;

{ A REFUSAL AND A FAULT ARE DIFFERENT ANSWERS, and the status code has to say
  which. Every engine refusal used to come back as 500 - "the server broke, try
  again" - which is wrong in both halves: nothing broke, and retrying the same
  request cannot help. The desktop client never noticed, because it reads the
  "ok" field and ignores the code; anything else reads the code first.

  Both directions are asserted, because only the pair is meaningful: a build that
  turned every 500 into a 400 would pass the first half and be just as wrong. }
procedure TRestApiTest.ARefusalIsAClientErrorNotAServerFault;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := FittableProblem;

  //  Inadmissible REQUEST: this build has no such curve type. The engine says no
  //  on purpose, so it is a 400.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"curveType":"{00000000-0000-0000-0000-000000000000}"}', Code);
  try
    AssertEquals('an unsupported curve type is a client error', 400, Code);
    AssertFalse('not ok', R.Get('ok', True));
    AssertTrue('and it explains itself', R.Get('error', '') <> '');
  finally
    R.Free;
  end;

  //  Still usable afterwards - a refusal must not leave the problem wedged.
  R := Call('GET', Format('/problems/%d/state', [Id]), '', Code);
  try
    AssertEquals('the problem still answers', 200, Code);
  finally
    R.Free;
  end;

  //  And a route that does not exist stays a 404 rather than being swept into
  //  the refusal branch.
  R := Call('POST', Format('/problems/%d/actions/nonsense', [Id]), '', Code);
  try
    AssertEquals('an unknown action is still 404', 404, Code);
  finally
    R.Free;
  end;
end;

{ EVERY CURVE IS ADDRESSABLE, AND BY SOMETHING THAT SURVIVES AN EDIT.

  The model is ordered by the fit intervals and by the picks inside them, so
  adding a pick to the LEFT of an existing one renumbers everything after it.
  While curves were addressed by ordinal, a client holding index 0 across such an
  edit silently began addressing a different curve - reading one curve's points
  and writing another curve's parameters, with nothing anywhere to say so.

  So this pins the property that removes that class of bug outright: the handle
  a curve had before the edit still names the same curve after it, even though
  its index has changed. }
procedure TRestApiFitTest.EveryCurveHasADistinctHandle;
var
  Id, Code, i, j: longint;
  R: TJSONObject;
  Curves: TJSONArray;
  Handles: TStringList;
  H: string;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  Handles := TStringList.Create;
  try
    R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
    try
      Curves := R.Arrays['curves'];
      AssertTrue('the model has curves', Curves.Count > 0);
      for i := 0 to Curves.Count - 1 do
      begin
        H := TJSONObject(Curves.Items[i]).Get('id', '');
        AssertTrue(Format('curve %d carries a handle', [i]), H <> '');
        //  In a URL path segment, so it must need no encoding.
        AssertEquals(Format('curve %d handle carries no braces', [i]),
          0, Pos('{', H));
        for j := 0 to Handles.Count - 1 do
          AssertTrue('two curves share a handle', Handles[j] <> H);
        Handles.Add(H);
      end;
    finally
      R.Free;
    end;
  finally
    Handles.Free;
  end;
end;

procedure TRestApiFitTest.AHandleOutlivesAnEditThatRenumbersTheModel;
var
  Id, Code, i: longint;
  R: TJSONObject;
  Curves: TJSONArray;
  Before, After: string;
  IndexBefore, IndexAfter: longint;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    Before := TJSONObject(R.Arrays['curves'].Items[0]).Get('id', '');
    IndexBefore := 0;
  finally
    R.Free;
  end;
  AssertTrue('the first curve has a handle', Before <> '');

  //  A pick to the LEFT of the existing one. The picks are sorted, so this
  //  becomes curve 0 and pushes the measured curve along.
  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    Format('{"x":%s,"y":%s}',
      [FloatToStr(6.0), FloatToStr(GaussPoint(100, 1.5, 10, 6.0))]), Code);
  R.Free;
  AssertEquals('the pick is accepted', 200, Code);

  IndexAfter := -1;
  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    Curves := R.Arrays['curves'];
    AssertTrue('the edit added a curve', Curves.Count > 1);
    for i := 0 to Curves.Count - 1 do
      if TJSONObject(Curves.Items[i]).Get('id', '') = Before then
        IndexAfter := i;
  finally
    R.Free;
  end;

  AssertTrue('the curve is still in the model', IndexAfter >= 0);
  //  The point of the whole exercise: the NUMBER moved, the handle did not.
  AssertTrue('and its index really did change', IndexAfter <> IndexBefore);

  //  And the handle still addresses it, rather than merely appearing in a list.
  R := Call('GET', Format('/problems/%d/curves/%s/points', [Id, Before]), '', Code);
  try
    AssertEquals('the handle still addresses its curve', 200, Code);
  finally
    R.Free;
  end;
  After := Before;
  AssertTrue('handle unchanged', After = Before);
end;

{ 404, NOT curve 0. Both routes used to run the path segment through
  StrToIntDef(..., 0), so an unknown or malformed address silently read - and,
  worse, WROTE - the first curve in the model. }
procedure TRestApiFitTest.AnUnknownCurveHandleIs404;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  R := Call('GET', Format(
    '/problems/%d/curves/2f1b7a54-0000-4000-8000-000000000000/points',
    [Id]), '', Code);
  try
    AssertEquals('reading an unknown curve is 404', 404, Code);
  finally
    R.Free;
  end;

  R := Call('PUT', Format(
    '/problems/%d/curves/2f1b7a54-0000-4000-8000-000000000000/params/0',
    [Id]), '{"value":1.0}', Code);
  try
    AssertEquals('writing to an unknown curve is 404', 404, Code);
  finally
    R.Free;
  end;

  //  Not a handle at all - what an ordinal from an older client looks like.
  R := Call('GET', Format('/problems/%d/curves/0/points', [Id]), '', Code);
  try
    AssertEquals('an ordinal is not a handle', 404, Code);
  finally
    R.Free;
  end;
end;

{ THE WRITE PATH, end to end. A parameter edit is written into the model and has
  to reach the curve itself - not merely the report the client reads back - or
  the calculated profile goes on describing the old value. }
procedure TRestApiFitTest.AParameterWrittenByHandleReachesTheModel;
var
  Id, Code: longint;
  R: TJSONObject;
  Handle: string;
  Before, After: double;
  ProfileBefore, ProfileAfter: TPointsData;
  Biggest: double;
  k: longint;

  { The calculated profile, sample by sample.

    NOT its sum: curve scaling multiplies the whole model onto the data, so the
    integral is very nearly invariant by construction and a widened curve would
    look like no change at all. The SHAPE is what moves. }
  function CalcProfile: TPointsData;
  var
    Body: string;
  begin
    FApi.Handle('GET', Format('/problems/%d/calc-profile', [Id]), '', Code, Body);
    AssertTrue('calc profile decoded', PointsFromJsonString(Body, Result));
  end;

begin
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    Handle := TJSONObject(R.Arrays['curves'].Items[0]).Get('id', '');
  finally
    R.Free;
  end;
  AssertTrue('the curve has a handle', Handle <> '');

  Before := SigmaOfFirstCurve(Id);
  AssertTrue('there is a width to change', Before > 0);
  ProfileBefore := CalcProfile;
  AssertTrue('there is a model to change', Length(ProfileBefore.Y) > 0);

  R := Call('PUT', Format('/problems/%d/curves/%s/params/%d',
    [Id, Handle, IndexOfParamNamed(Id, 'Sigma')]),
    Format('{"value":%s}', [FloatToStr(Before * 2)]), Code);
  try
    AssertEquals('the write is accepted', 200, Code);
  finally
    R.Free;
  end;

  After := SigmaOfFirstCurve(Id);
  AssertEquals('the model reports the new width', Before * 2, After, 1e-6);

  //  AND THE MODEL ITSELF CHANGED. Reading the value back only proves the
  //  report was updated - and the report is a separate list from the curves.
  //  The calculated profile is built from the curves themselves, so it is what
  //  proves the write reached the model and not merely what is read back.
  ProfileAfter := CalcProfile;
  AssertEquals('the profile still has its samples',
    Length(ProfileBefore.Y), Length(ProfileAfter.Y));
  Biggest := 0;
  for k := 0 to High(ProfileAfter.Y) do
    if Abs(ProfileAfter.Y[k] - ProfileBefore.Y[k]) > Biggest then
      Biggest := Abs(ProfileAfter.Y[k] - ProfileBefore.Y[k]);
  AssertTrue('the calculated profile was rebuilt from the new value',
    Biggest > 1e-6);
end;

procedure TRestApiFitTest.TheWholeModelWrittenAtOnceReachesTheModelToo;
var
  Id, Code: longint;
  R: TJSONObject;
  Handle, Width: string;
  Before: double;
  ProfileBefore, ProfileAfter: TPointsData;
  Biggest: double;
  j, k: longint;
  Params: TJSONArray;

  function CalcProfile: TPointsData;
  var
    Body: string;
  begin
    FApi.Handle('GET', Format('/problems/%d/calc-profile', [Id]), '', Code, Body);
    AssertTrue('calc profile decoded', PointsFromJsonString(Body, Result));
  end;

begin
  //  THE SAME GUARD AS THE SINGLE-PARAMETER ROUTE ABOVE, for the route a
  //  RESTORE uses. Reading the value back proves only that the report was
  //  updated, and the report is a separate list from the curves; the calculated
  //  profile is built from the curves themselves, so it is the only thing that
  //  can tell a write that reached the model from one that reached the report
  //  and stopped there. Without this, a project could reopen showing the values
  //  it saved over a model that had never been given them.
  Id := FittableProblem;

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '', Code);
  R.Free;
  AssertEquals('minimize status', 200, Code);

  //  THE HANDLE AND THE PARAMETER'S OWN SPELLING, both read from the model.
  //  The write matches parameters BY NAME and exactly, which is right - a
  //  project writes back the names it read - so a test that spells one itself
  //  is testing its own guess.
  Width := '';
  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    Handle := TJSONObject(R.Arrays['curves'].Items[0]).Get('id', '');
    Params := TJSONObject(R.Arrays['curves'].Items[0]).Arrays['params'];
    for j := 0 to Params.Count - 1 do
      if SameText(TJSONObject(Params.Items[j]).Get('name', ''), 'Sigma') then
        Width := TJSONObject(Params.Items[j]).Get('name', '');
  finally
    R.Free;
  end;
  AssertTrue('the curve has a handle', Handle <> '');
  AssertTrue('and a width to change', Width <> '');

  Before := SigmaOfFirstCurve(Id);
  AssertTrue('there is a width to change', Before > 0);
  ProfileBefore := CalcProfile;
  AssertTrue('there is a model to change', Length(ProfileBefore.Y) > 0);

  R := Call('PUT', Format('/problems/%d/curves', [Id]),
    Format('{"curves":[{"id":"%s","fitted":true,"params":' +
    '[{"name":"%s","value":%s,"error":-1}]}]}',
    [Handle, Width, FloatToStr(Before * 2)]), Code);
  try
    AssertEquals('the write is accepted', 200, Code);
  finally
    R.Free;
  end;

  AssertEquals('the model reports the new width', Before * 2,
    SigmaOfFirstCurve(Id), 1e-6);

  ProfileAfter := CalcProfile;
  AssertEquals('the profile still has its samples',
    Length(ProfileBefore.Y), Length(ProfileAfter.Y));
  Biggest := 0;
  for k := 0 to High(ProfileAfter.Y) do
    if Abs(ProfileAfter.Y[k] - ProfileBefore.Y[k]) > Biggest then
      Biggest := Abs(ProfileAfter.Y[k] - ProfileBefore.Y[k]);
  AssertTrue('the calculated profile was rebuilt from the values written',
    Biggest > 1e-6);
end;

{ AN AUTOMATIC RUN'S REDUCTION SURVIVES THE NEXT EDIT.

  MinimizeNumberOfCurves exists to remove curves the model does not need, and it
  removed them from the task's OWN COPY of the picks. The service was never
  told, so its pick list still held every original pick - and the next rebuild,
  which any click causes, seeded them all again. The user watched the program
  work out that eleven curves would do instead of forty, and then saw forty come
  back the moment they touched anything.

  So this runs the reduction, edits the model, and requires the reduction to
  still be there. It is the one thing a fit may do to the picks, and it deletes
  only - never moves, never adds - so the pick set stays what it has to be:
  unique x values, every one a real sample of the profile. }
procedure TRestApiTest.AnAutomaticReductionSurvivesTheNextEdit;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Picked, Reduced, AfterEdit: TPointsData;
begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/profile', [Id]),
    CoarseGaussianProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  A curve on every sample, which is what the automatic mode starts from.
  R := Call('POST',
    Format('/problems/%d/actions/select-all-points-as-curve-positions', [Id]),
    '', Code);
  R.Free;
  AssertEquals('select-all status', 200, Code);

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Picked));
  AssertTrue('every sample is a pick to begin with', Length(Picked.X) > 3);

  R := Call('POST',
    Format('/problems/%d/actions/minimize-number-of-curves', [Id]), '', Code);
  try
    AssertEquals('the automatic run completes', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Reduced));
  //  The reduction reached the MODEL, not just the task that computed it.
  AssertTrue('the run removed curves the model did not need',
    Length(Reduced.X) < Length(Picked.X));

  //  ANY edit rebuilds every instance from the picks. This is the click that
  //  used to bring all of them back.
  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    Format('{"x":%s,"y":%s}',
      [FloatToStr(Reduced.X[0]), FloatToStr(Reduced.Y[0])]), Code);
  R.Free;

  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions decoded', PointsFromJsonString(Body, AfterEdit));
  AssertTrue('and the deleted curves stayed deleted',
    Length(AfterEdit.X) <= Length(Reduced.X));
  AssertTrue('rather than every original pick coming back',
    Length(AfterEdit.X) < Length(Picked.X));
end;

{ ---- the routes that need no fit ------------------------------------------- }

procedure TRestApiTest.EveryPointSetRouteAnswersOnAFreshProblem;
var
  Id, Code, i: longint;
  Body: string;
  Got: TPointsData;
  Routes: array[0..6] of string;
begin
  //  SEVEN ROUTES, none of which had been asked for before a fit. Each returns a
  //  point set the client draws directly, and a route that answers 404 - or
  //  answers with something unreadable - is a blank chart the user cannot
  //  distinguish from "nothing computed yet".
  Id := NewProblem;
  Routes[0] := 'profile';
  Routes[1] := 'background';
  Routes[2] := 'positions';
  Routes[3] := 'rfactor-bounds';
  Routes[4] := 'calc-profile';
  Routes[5] := 'delta-profile';
  Routes[6] := 'calc-positions';
  for i := 0 to High(Routes) do
  begin
    FApi.Handle('GET', Format('/problems/%d/%s', [Id, Routes[i]]), '',
      Code, Body);
    AssertEquals(Routes[i] + ' answers', 200, Code);
    AssertTrue(Routes[i] + ' is a readable point set: ' + Copy(Body, 1, 80),
      PointsFromJsonString(Body, Got));
  end;
end;

procedure TRestApiTest.AnEmptySetIsNotAnError;
var
  Id, Code: longint;
  Body: string;
  Got: TPointsData;
begin
  //  Nothing has been computed, so the calculated profile is empty - and that is
  //  the ordinary state of a problem that has only just been given data. An
  //  error here would make the client report a fault on every Open.
  Id := NewProblem;
  FApi.Handle('GET', Format('/problems/%d/calc-profile', [Id]), '', Code, Body);
  AssertEquals('answered', 200, Code);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
  AssertEquals('and empty', 0, Length(Got.X));
end;

procedure TRestApiTest.TheBackgroundRoundTripsThroughTheProblem;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
  P: TPointsData;
begin
  //  The background is the user's own picks, and it is the one set whose loss
  //  the user cannot recover by recomputing - so it has to survive the wire
  //  exactly.
  //  A PROFILE FIRST. The background is picked off the data, and the engine
  //  refuses to hold picks for a problem that has none - which is right, and is
  //  why this test is not simply a write and a read.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  P := Default(TPointsData);
  P.Title := 'background';
  SetLength(P.X, 3);
  SetLength(P.Y, 3);
  P.X[0] := 1; P.X[1] := 5; P.X[2] := 9;
  P.Y[0] := 2; P.Y[1] := 3; P.Y[2] := 4;

  R := Call('PUT', Format('/problems/%d/background', [Id]),
    PointsToJsonString(P), Code);
  try
    AssertEquals('put background', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
  AssertEquals('get background', 200, Code);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
  AssertEquals('three points', 3, Length(Got.X));
  AssertEquals('the middle abscissa', 5.0, Got.X[1], 1E-9);
  AssertEquals('and its ordinate', 3.0, Got.Y[1], 1E-9);
end;

procedure TRestApiTest.TheBoundsRoundTripAsPairs;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
  P: TPointsData;
begin
  //  Bounds are consecutive (start, end) pairs and everything downstream reads
  //  them that way, so their ORDER is data. A set that came back sorted or
  //  deduplicated would silently redefine the fitting window.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  P := Default(TPointsData);
  P.Title := 'bounds';
  SetLength(P.X, 4);
  SetLength(P.Y, 4);
  P.X[0] := 8; P.X[1] := 12; P.X[2] := 2; P.X[3] := 4;
  P.Y[0] := 0; P.Y[1] := 0; P.Y[2] := 0; P.Y[3] := 0;

  R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
    PointsToJsonString(P), Code);
  try
    AssertEquals('put bounds', 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '', Code, Body);
  AssertEquals('get bounds', 200, Code);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
  AssertEquals('four points', 4, Length(Got.X));
  AssertEquals('and the order is kept', 8.0, Got.X[0], 1E-9);
  AssertEquals('including the second pair', 2.0, Got.X[2], 1E-9);
end;

procedure TRestApiTest.PuttingAnUnknownSetIsRefused;
var
  Id, Code: longint;
  Body: string;
begin
  //  Only four sets are writable; the rest are computed. Accepting a write to a
  //  computed set would let the client overwrite the engine's own answer with no
  //  error and no way to notice.
  Id := NewProblem;
  FApi.Handle('PUT', Format('/problems/%d/calc-profile', [Id]),
    '{"title":"x","x":[1],"y":[1]}', Code, Body);
  AssertTrue(Format('a computed set is not writable (got %d)', [Code]),
    Code >= 400);
end;

procedure TRestApiTest.TheRFactorIsReadableBeforeAnyFit;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  Read on a timer while an operation runs, so it must answer on a problem
  //  that has not fitted yet - which is what it is doing the first time the
  //  client polls, before the engine has produced anything.
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d/rfactor', [Id]), '', Code);
  try
    AssertEquals('answered', 200, Code);
    AssertTrue('with a document', Assigned(R));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.TheSelectedIntervalIsEmptyUntilOneIsChosen;
var
  Id, Code: longint;
  Body: string;
  Got: TPointsData;
begin
  Id := NewProblem;
  FApi.Handle('GET', Format('/problems/%d/selected-interval', [Id]), '',
    Code, Body);
  AssertEquals('answered', 200, Code);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
end;

procedure TRestApiTest.TheAsyncStateIsPollableBeforeAnythingRuns;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  The client polls this to decide whether to re-enable its menus. If it only
  //  answered while an operation was running, the interface would come up
  //  disabled and stay that way.
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d/async', [Id]), '', Code);
  try
    AssertEquals('answered', 200, Code);
    AssertFalse('and nothing is running', R.Get('busy', True));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.TheStatisticsRouteRefusesRatherThanInventing;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  Nothing has been fitted, so there are no statistics. The route must say so
  //  - an invalid record read as zeros would score an unfitted model, and the
  //  candidate ranking is built on these numbers.
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d/stats', [Id]), '', Code);
  try
    AssertEquals('answered', 200, Code);
    AssertTrue('with a document', Assigned(R));
    AssertFalse('and the statistics are not valid',
      TJSONObject(R.Find('statistics')).Get('valid', True));
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.APointIsAddedToTheNamedSet;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
begin
  //  ONE PICK, which is what a click is. The set is named in the path, and a
  //  pick routed to the wrong set changes the user's data in front of them.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  R := Call('POST', Format('/problems/%d/points/background', [Id]),
    '{"x":3.5,"y":7.25}', Code);
  try
    AssertEquals('added: ' + R.AsJSON, 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
  AssertEquals('one point', 1, Length(Got.X));
  AssertEquals('at the abscissa asked for', 3.5, Got.X[0], 1E-9);
  AssertEquals('with the ordinate asked for', 7.25, Got.Y[0], 1E-9);
end;

procedure TRestApiTest.RemovingAMemberNeedsASetWhoseMembersHaveHandles;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  REFUSED BY NAME, not ignored. A curve's identity is issued to the pick it
  //  is seeded from, so a pick can be named and a profile sample cannot - and a
  //  caller asking about the wrong set should learn which sets this answers
  //  for rather than get a 404 that reads as "no such point".
  Id := NewProblem;
  R := Call('DELETE', Format('/problems/%d/points/background/ABC', [Id]), '',
    Code);
  try
    AssertEquals('refused: ' + R.AsJSON, 400, Code);
    AssertTrue('and it says which sets do: ' + R.AsJSON,
      Pos('positions', R.AsJSON) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.RemovingAMemberOfAnUnknownCurveIs404;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  404 rather than a guess. Deleting the wrong curve is the worst outcome
  //  available on this route - see the curve routes, where resolving an
  //  unparseable segment used to hit curve 0.
  Id := NewProblem;
  R := Call('DELETE',
    Format('/problems/%d/points/positions/{99999999-9999-9999-9999-999999999999}',
    [Id]), '', Code);
  try
    AssertEquals('not found: ' + R.AsJSON, 404, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.RemovingAMemberTakesTheCurveAndThePick;
var
  Id, Code: longint;
  R: TJSONObject;
  Body, Handle: string;
  Picks: TPointsData;
  Curves: TJSONObject;
begin
  //  A whole model in process: a profile, two picks and an interval, which is
  //  what the engine needs before it builds anything.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/profile', [Id]),
    CoarseGaussianProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    '{"x":2,"y":1}', Code);
  R.Free;
  R := Call('POST', Format('/problems/%d/points/positions', [Id]),
    '{"x":8,"y":1}', Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
    '{"title":"b","x":[0,10],"y":[0,0]}', Code);
  R.Free;

  //  The handle of the first curve, which is what the route addresses. Read
  //  from /curves, because that is where a client gets one.
  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertEquals('curves readable', 200, Code);
    //  OkResponse flattens its payload into the envelope rather than nesting
    //  it under a data member, so the array is at the top level.
    AssertTrue('the model has a curve', R.Arrays['curves'].Count > 0);
    Curves := TJSONObject(R.Arrays['curves'].Items[0]);
    Handle := Curves.Get('id', '');
  finally
    R.Free;
  end;
  AssertTrue('a handle to delete by', Handle <> '');

  R := Call('DELETE',
    Format('/problems/%d/points/positions/%s', [Id, Handle]), '', Code);
  try
    AssertEquals('removed: ' + R.AsJSON, 200, Code);
  finally
    R.Free;
  end;

  //  AND THE PICK WENT WITH IT, which is what makes the deletion stick: the
  //  model is rebuilt from its inputs, so a pick left behind would put a fresh
  //  instance back on the next rebuild.
  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertTrue('positions readable', PointsFromJsonString(Body, Picks));
  AssertEquals('one pick left', 1, Length(Picks.X));
end;

procedure TRestApiTest.APointIsMovedInTheNamedSet;
var
  Id, Code: longint;
  R: TJSONObject;
  Body: string;
  Got: TPointsData;
begin
  //  A DRAG. The old position identifies the point, so this is where a move
  //  becomes an add if the match is not exact - and the user gets two picks
  //  where they moved one.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  R := Call('POST', Format('/problems/%d/points/background', [Id]),
    '{"x":3.5,"y":7.25}', Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/points/background', [Id]),
    '{"prevX":3.5,"prevY":7.25,"x":4.5,"y":8.25}', Code);
  try
    AssertEquals('moved: ' + R.AsJSON, 200, Code);
  finally
    R.Free;
  end;

  FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
  AssertTrue('readable', PointsFromJsonString(Body, Got));
  AssertEquals('still one point', 1, Length(Got.X));
  AssertEquals('at the new abscissa', 4.5, Got.X[0], 1E-9);
end;

procedure TRestApiTest.AddingToAnUnknownSetIsRefused;
var
  Id, Code: longint;
  Body: string;
begin
  Id := NewProblem;
  FApi.Handle('POST', Format('/problems/%d/points/nonesuch', [Id]),
    '{"x":1,"y":1}', Code, Body);
  AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
end;

procedure TRestApiTest.AModuleResourceNobodyProvidesIs404;
var
  Id, Code: longint;
  Body: string;
begin
  //  A CLIENT ERROR, not a server fault. The router turns any refusal from the
  //  module layer - including "no module owns this resource" - into a 400 with
  //  the message the layer gave, deliberately: the resource name came from the
  //  caller, so the caller is what was wrong. A 500 here would be reported to
  //  the user as the server being broken.
  Id := NewProblem;
  FApi.Handle('GET', Format('/problems/%d/modules/nonesuch/thing', [Id]), '',
    Code, Body);
  AssertEquals('a client error: ' + Body, 400, Code);
  AssertTrue('with a message', Pos('error', Body) > 0);
end;

procedure TRestApiTest.TheSpecialCurveFormulaRoundTrips;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  The user-defined curve type. The formula lives on the server while it is
  //  being fitted, and losing it mid-session means the model cannot be rebuilt.
  //  DATA FIRST. Setting the formula is an operation on a problem, and the
  //  engine refuses every operation until there is something to operate on -
  //  with a message that says exactly that, which is how this test found out.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/special-params', [Id]),
    '{"expression":"A*exp(-x*x)","params":' +
    '[{"name":"A","value":2.5,"type":0}]}', Code);
  try
    AssertTrue(Format('accepted (got %d): %s', [Code, R.AsJSON]), Code < 400);
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/special-params', [Id]), '', Code);
  try
    AssertEquals('readable back', 200, Code);
    //  THE PARAMETERS come back; THE EXPRESSION DOES NOT. SpecialParamsOf builds
    //  its reply from GetSpecialCurveParameters alone, so the formula the server
    //  is fitting is write-only over REST. That is survivable - the client owns
    //  the formula, keeps it in its settings and pushes it - but it means nothing
    //  can ask the server what it is actually fitting, which is exactly the
    //  question asked when a fit produces the wrong shape.
    AssertTrue('the parameter came back: ' + R.AsJSON,
      Pos('"A"', R.AsJSON) > 0);
    AssertTrue('with its value', Pos('2.5', R.AsJSON) > 0);
    AssertTrue('and the expression is not readable back',
      Pos('exp', R.AsJSON) = 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.DeletingTheSpecialCurveFormulaForgetsIt;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE REASON THIS VERB EXISTS: the client deletes the user curve, and without
  //  this the server goes on building curves from a formula that no longer
  //  exists anywhere in the interface - which is how a deleted curve type kept
  //  reappearing in the fit.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson,
    Code);
  R.Free;
  R := Call('PUT', Format('/problems/%d/special-params', [Id]),
    '{"expression":"A*exp(-x*x)","params":' +
    '[{"name":"A","value":2.5,"type":0}]}', Code);
  R.Free;

  R := Call('DELETE', Format('/problems/%d/special-params', [Id]), '', Code);
  try
    AssertTrue(Format('accepted (got %d)', [Code]), Code < 400);
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/special-params', [Id]), '', Code);
  try
    AssertEquals('still answers', 200, Code);
    //  The parameters are what is observable here - see the note in the round
    //  trip above - so their absence is how "forgotten" is visible.
    AssertTrue('the parameters are gone: ' + R.AsJSON,
      Pos('"A"', R.AsJSON) = 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AnUnknownRouteOnAKnownProblemIs404;
var
  Id, Code: longint;
  Body: string;
begin
  //  404 AND NOT 500. A client built against a newer server asks for routes this
  //  one does not have, and it has to be able to tell "this server cannot do
  //  that" from "this server is broken".
  Id := NewProblem;
  FApi.Handle('GET', Format('/problems/%d/nonesuch', [Id]), '', Code, Body);
  AssertEquals('not found', 404, Code);
end;

procedure TRestApiTest.AMalformedProblemIdIs404;
var
  Code: longint;
  Body: string;
begin
  //  Not a number where a number belongs. It must be a refusal rather than a
  //  parse that yields zero and then addresses whatever problem zero is.
  FApi.Handle('GET', '/problems/not-a-number/state', '', Code, Body);
  AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
end;

procedure TRestApiTest.TheRootPathIsNotAProblem;
var
  Code: longint;
  Body: string;
begin
  FApi.Handle('GET', '/', '', Code, Body);
  AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
end;

procedure TRestApiTest.CurvePointsForAnUnknownCurveIs404;
var
  Id, Code: longint;
  Body: string;
begin
  //  Nothing has been built, so curve 0 does not exist. Answering with an empty
  //  set instead would draw a curve that is not in the model.
  Id := NewProblem;
  FApi.Handle('GET', Format('/problems/%d/curves/nosuchhandle/points', [Id]),
    '', Code, Body);
  AssertEquals('not found', 404, Code);
end;

{ ---- the actions that are not a fit ---------------------------------------- }

procedure TRestApiTest.AnUnknownActionNamesTheOnesThatExist;
var
    Id, Code: longint;
    Body: string;
begin
    //  NO ROUTE LISTS THE VERBS - the registry knows them and nothing exposes
    //  it - so the refusal is the only place a caller can find out what this
    //  build offers. A bare "unknown action" would leave a client author
    //  reading the source.
    Id := FittableProblem;
    FApi.Handle('POST', Format('/problems/%d/actions/nonesuch', [Id]), '',
        Code, Body);
    AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
    AssertTrue('and lists what there is: ' + Body,
        Pos('smooth-profile', Body) > 0);
    AssertTrue('including the fits', Pos('minimize-difference', Body) > 0);
end;

procedure TRestApiTest.SmoothingTheProfileKeepsItsShape;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Before, After: TPointsData;
begin
    //  Smoothing is not a fit - it is a pass over the data - so it belongs in
    //  the fast half. What it must not do is change how many points there are or
    //  move them along the axis: the abscissae are the measurement's own grid,
    //  and everything downstream indexes against it.
    Id := FittableProblem;
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the profile before', PointsFromJsonString(Body, Before));

    R := Call('POST', Format('/problems/%d/actions/smooth-profile', [Id]),
        '', Code);
    try
        AssertEquals('smoothed', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the profile after', PointsFromJsonString(Body, After));
    AssertEquals('the same number of points', Length(Before.X), Length(After.X));
    AssertEquals('on the same grid', Before.X[0], After.X[0], 1E-9);
    AssertEquals('to the same end', Before.X[High(Before.X)],
        After.X[High(After.X)], 1E-9);
end;

procedure TRestApiTest.ComputingBackgroundPointsProposesSome;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Got: TPointsData;
begin
    //  THE AUTOMATIC BACKGROUND SEARCH, which assumes a concave background and
    //  says so in a comment rather than in the interface. It had never been run
    //  by anything but a full session.
    Id := FittableProblem;
    R := Call('POST',
        Format('/problems/%d/actions/compute-background-points', [Id]), '',
        Code);
    try
        AssertEquals('computed', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
    AssertEquals('readable', 200, Code);
    AssertTrue('a point set', PointsFromJsonString(Body, Got));
    AssertTrue('and it proposed something', Length(Got.X) > 0);
end;

procedure TRestApiTest.ProposedBackgroundPointsLieOnTheProfile;
var
    Id, Code, i, j: longint;
    Body: string;
    Profile, Back: TPointsData;
    Found: boolean;
begin
    //  A background point the user can then drag is a point ON the data: the
    //  chart draws it against the profile, and one that sits between samples
    //  cannot be picked up again.
    Id := FittableProblem;
    Call('POST', Format('/problems/%d/actions/compute-background-points', [Id]),
        '', Code).Free;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the profile', PointsFromJsonString(Body, Profile));
    FApi.Handle('GET', Format('/problems/%d/background', [Id]), '', Code, Body);
    AssertTrue('the background', PointsFromJsonString(Body, Back));

    for i := 0 to High(Back.X) do
    begin
        Found := False;
        for j := 0 to High(Profile.X) do
            if Abs(Profile.X[j] - Back.X[i]) < 1E-9 then
                Found := True;
        AssertTrue(Format('background point %d sits on a sample', [i]), Found);
    end;
end;

procedure TRestApiTest.ComputingCurvePositionsProposesSome;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Got: TPointsData;
begin
    //  Peak finding. What it proposes is what the fit is seeded from, so
    //  proposing nothing on a profile with an obvious peak is a fit that starts
    //  from nowhere.
    Id := FittableProblem;
    R := Call('POST',
        Format('/problems/%d/actions/compute-curve-positions', [Id]), '', Code);
    try
        AssertEquals('computed', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue('a point set', PointsFromJsonString(Body, Got));
    AssertTrue('and it found the peak', Length(Got.X) > 0);
end;

procedure TRestApiTest.ComputingCurveBoundsProposesPairs;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Got: TPointsData;
begin
    //  Bounds come in pairs by construction - every interval has two ends - and
    //  an odd count is what the whole downstream reads as a half-marked
    //  interval. Whatever the engine proposes must be complete.
    Id := FittableProblem;
    Call('POST', Format('/problems/%d/actions/compute-curve-positions', [Id]),
        '', Code).Free;
    R := Call('POST', Format('/problems/%d/actions/compute-curve-bounds', [Id]),
        '', Code);
    try
        AssertEquals('computed', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/rfactor-bounds', [Id]), '',
        Code, Body);
    AssertTrue('a point set', PointsFromJsonString(Body, Got));
    AssertEquals('an even number of bounds', 0, Length(Got.X) mod 2);
end;

procedure TRestApiTest.EveryPointCanBeMadeACurvePosition;
var
    Id, Code: longint;
    Body: string;
    Profile, Positions: TPointsData;
begin
    //  One curve per sample. This is the command that reaches the palette's
    //  thirty-second colour, and the one that makes a coarse profile into a
    //  model with more curves than anyone would place by hand.
    Id := FittableProblem;
    Call('POST', Format(
        '/problems/%d/actions/select-all-points-as-curve-positions', [Id]),
        '', Code).Free;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the profile', PointsFromJsonString(Body, Profile));
    FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
    AssertTrue('the positions', PointsFromJsonString(Body, Positions));
    AssertEquals('one position per sample', Length(Profile.X),
        Length(Positions.X));
end;

procedure TRestApiTest.SubtractingTheBackgroundChangesTheProfile;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Before, After: TPointsData;
    Changed: boolean;
    i: longint;
begin
    //  The one operation that rewrites the user's data. It has to change
    //  something - a subtraction that quietly did nothing would leave the user
    //  fitting the background - and it has to leave the grid alone.
    Id := FittableProblem;
    Call('POST', Format('/problems/%d/actions/compute-background-points', [Id]),
        '', Code).Free;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('before', PointsFromJsonString(Body, Before));

    R := Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
        '{"auto":true}', Code);
    try
        AssertEquals('subtracted', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('after', PointsFromJsonString(Body, After));
    AssertEquals('the same grid', Length(Before.X), Length(After.X));

    Changed := False;
    for i := 0 to High(After.Y) do
        if Abs(After.Y[i] - Before.Y[i]) > 1E-9 then
            Changed := True;
    AssertTrue('and something came off', Changed);
end;

procedure TRestApiTest.SelectingAnIntervalNarrowsTheProfile;
var
    Id, Code: longint;
    R: TJSONObject;
    Body: string;
    Whole, Part: TPointsData;
begin
    //  Fitting a stretch of the data rather than all of it. The selected
    //  interval is what every subsequent operation reads as "the profile", so
    //  an interval that did not narrow anything is a fit over the whole file
    //  presented as a fit over a region.
    Id := FittableProblem;
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the whole profile', PointsFromJsonString(Body, Whole));

    R := Call('POST',
        Format('/problems/%d/actions/select-profile-interval', [Id]),
        '{"start":2,"stop":5}', Code);
    try
        AssertEquals('selected', 200, Code);
    finally
        R.Free;
    end;

    FApi.Handle('GET', Format('/problems/%d/selected-interval', [Id]), '',
        Code, Body);
    AssertTrue('an interval', PointsFromJsonString(Body, Part));
    AssertEquals('four samples, ends included', 4, Length(Part.X));
    AssertTrue('fewer than the whole', Length(Part.X) < Length(Whole.X));
end;

procedure TRestApiTest.AnIntervalNeedsItsTwoEnds;
var
    Id, Code: longint;
    Body: string;
begin
    //  A body with nothing in it. Reading the missing ends as zero would select
    //  a degenerate interval and report success for it.
    Id := FittableProblem;
    FApi.Handle('POST',
        Format('/problems/%d/actions/select-profile-interval', [Id]), '',
        Code, Body);
    AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
end;

procedure TRestApiTest.GoingBackToTheWholeProfileClearsTheInterval;
var
    Id, Code: longint;
    Body: string;
    Whole, Back: TPointsData;
begin
    Id := FittableProblem;
    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('the whole profile', PointsFromJsonString(Body, Whole));

    Call('POST', Format('/problems/%d/actions/select-profile-interval', [Id]),
        '{"start":2,"stop":5}', Code).Free;
    Call('POST', Format('/problems/%d/actions/select-entire-profile', [Id]),
        '', Code).Free;

    FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
    AssertTrue('readable again', PointsFromJsonString(Body, Back));
    AssertEquals('and it is the whole file again', Length(Whole.X),
        Length(Back.X));
end;

procedure TRestApiTest.RebuildingTheCurveListNeedsAFinishedFit;
var
    Id, Code: longint;
    Body: string;
begin
    //  NOT A NO-OP BEFORE A FIT. The curve list is collected from the fitted
    //  curves' attributes, so there is nothing to rebuild until a fit has
    //  finished - and the engine says so rather than handing back an empty
    //  list, which the client would draw as a model with no curves in it.
    //
    //  Refused with nothing loaded, and refused with a profile but no fit: the
    //  two are the same answer from the caller's side, which is what lets a
    //  client treat "not ready" uniformly.
    Id := NewProblem;
    FApi.Handle('POST', Format('/problems/%d/actions/create-curve-list', [Id]),
        '', Code, Body);
    AssertTrue(Format('refused with nothing loaded (got %d)', [Code]),
        Code >= 400);

    Id := FittableProblem;
    FApi.Handle('POST', Format('/problems/%d/actions/create-curve-list', [Id]),
        '', Code, Body);
    AssertTrue(Format('and refused before a fit (got %d)', [Code]),
        Code >= 400);
    AssertTrue('with a message: ' + Body, Pos('error', Body) > 0);
end;

procedure TRestApiTest.StoppingWhenNothingRunsIsRefused;
var
    Id, Code: longint;
    Body: string;
begin
    //  REFUSED, and asserted as such. The engine gates Stop on an operation
    //  being in progress, which is defensible - and the client is expected not
    //  to offer it otherwise, which is exactly what action_state decides.
    //
    //  Worth knowing because the two have to agree: Stop is offered from a
    //  POLLED state, so a client that read the state a moment before the
    //  operation ended would send a request the server refuses, and the user
    //  would see a refusal for pressing a button that was enabled.
    Id := FittableProblem;
    FApi.Handle('POST', Format('/problems/%d/actions/stop', [Id]), '',
        Code, Body);
    AssertTrue(Format('refused while nothing runs (got %d)', [Code]),
        Code >= 400);
end;

procedure TRestApiTest.AnActionOnAnUnknownProblemIs404;
var
    Code: longint;
    Body: string;
begin
    //  A client that kept an id across a server restart. It must be told the
    //  problem is gone, not that the verb is unknown.
    FApi.Handle('POST', '/problems/999999/actions/smooth-profile', '',
        Code, Body);
    AssertEquals('not found', 404, Code);
end;

procedure TRestApiTest.AnActionIsRefusedBeforeThereIsData;
var
    Id, Code: longint;
    Body: string;
begin
    //  Every one of these operates on a profile. Offered one before a file is
    //  open, the engine refuses with a message that says so - which is what the
    //  client shows the user.
    Id := NewProblem;
    FApi.Handle('POST', Format('/problems/%d/actions/smooth-profile', [Id]),
        '', Code, Body);
    AssertTrue(Format('refused (got %d)', [Code]), Code >= 400);
    AssertTrue('and said why: ' + Body, Pos('error', Body) > 0);
end;

{ ---- the action routes that do not run the optimiser ----------------------- }

procedure TRestApiTest.SelectingTheEntireProfileSucceeds;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST', Format('/problems/%d/actions/select-entire-profile', [Id]),
    '', Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.SelectingAnIntervalTakesItsBounds;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  BY INDEX, both ends in one body. The interval is what a fit runs over, so
  //  an end dropped or swapped fits a different stretch of the data than the
  //  one the user dragged out - and the result reads as a bad fit.
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST',
    Format('/problems/%d/actions/select-profile-interval', [Id]),
    '{"start":2,"stop":8}', Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.SelectingAnIntervalWithNoBodyIsRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  400, NOT A SILENT DEFAULT. Without a body there are no bounds, and
  //  defaulting them to 0 and 0 would select an empty interval and then fit
  //  nothing - reporting success for a request that asked for something else.
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST',
    Format('/problems/%d/actions/select-profile-interval', [Id]), '', Code);
  try
    AssertEquals('refused as a client error', 400, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.SelectingAnIntervalWithRubbishForABodyIsRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  The same refusal for a body that is not JSON at all, which is what a
  //  truncated request looks like.
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST',
    Format('/problems/%d/actions/select-profile-interval', [Id]),
    'not json', Code);
  try
    AssertEquals('refused', 400, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.ThatRefusalSaysWhatWasMissing;
var
  Id, Code: longint;
  Body: string;
begin
  //  A CODE ALONE IS NOT AN EXPLANATION. The client shows this text; "400"
  //  tells the user nothing they can act on, where the names of the two missing
  //  fields tell them exactly what to send.
  Id := NewProblem;
  FApi.Handle('PUT', Format('/problems/%d/profile', [Id]),
    GaussianProfileJson, Code, Body);
  FApi.Handle('POST',
    Format('/problems/%d/actions/select-profile-interval', [Id]),
    '', Code, Body);
  AssertTrue('it names start: ' + Body, Pos('start', Body) > 0);
  AssertTrue('and stop: ' + Body, Pos('stop', Body) > 0);
end;

procedure TRestApiTest.SubtractingABackgroundWithNoBodyIsNotRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE OTHER WAY ROUND FROM THE INTERVAL, deliberately. An absent body here
  //  means "not automatic" - the one flag it carries has a safe default, and
  //  the safe default is to use the points the user picked rather than to guess
  //  at a background and overwrite them.
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST', Format('/problems/%d/actions/subtract-background', [Id]),
    '', Code);
  try
    AssertEquals('accepted', 200, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.BuildingTheCurveListNeedsACompletedCalculation;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  THE CURVE LIST IS BUILT FROM WHAT A FIT FOUND, so asking for one before
  //  anything has been fitted is refused rather than answered with an empty
  //  list - an empty list would read as "this model has no curves", which is a
  //  different statement from "nothing has been computed yet".
  //
  //  Asserted as it behaves, not as one might assume: the state machine is what
  //  decides, and its refusal says which state it is in.
  Id := NewProblem;
  Call('PUT', Format('/problems/%d/profile', [Id]), GaussianProfileJson, Code);
  R := Call('POST', Format('/problems/%d/actions/create-curve-list', [Id]),
    '', Code);
  try
    AssertEquals('refused as a client error', 400, Code);
    AssertTrue('and it says why: ' + R.AsJSON,
      Pos('not accomplished', R.AsJSON) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.StoppingWhenNothingIsRunningIsRefused;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  REFUSED, AND THAT IS A WART - pinned here as it is rather than as it
  //  should be. The stop button is live whenever a fit is running, and a fit
  //  can finish between the press and the request arriving; the client turns a
  //  400 into a message, so the user who pressed Stop a moment too late is told
  //  "The calculation not started" about a calculation they watched run.
  //
  //  Harmless but confusing, and the fix - answering 200 for a stop with
  //  nothing to stop, the way every other idempotent request does - changes
  //  server behaviour, so it is recorded in findings.md rather than made here.
  Id := NewProblem;
  R := Call('POST', Format('/problems/%d/actions/stop', [Id]), '', Code);
  try
    AssertEquals('refused', 400, Code);
    AssertTrue('saying nothing was running: ' + R.AsJSON,
      Pos('not started', R.AsJSON) > 0);
  finally
    R.Free;
  end;
end;

{ ---- the two verbs nothing had ever called -------------------------------- }

{ EVERYTHING IN ONE PASS: the background, the positions and the fit, which is what
  the "Do all automatically" command sends. Its handler, the engine method behind
  it and the whole automatic sequence had never been executed by any test - the
  suite drove the steps one at a time instead, which is not the same code. }
procedure TRestApiFitTest.TheWholeSequenceRunsInOnePass;
var
  Id, Code, i: longint;
  R: TJSONObject;
  CurMin: double;
  Prof: TPointsData;
begin
  Id := NewProblem;
  //  A SHORT PROFILE, DELIBERATELY. This verb works out the curve positions
  //  itself, and on the hundred-point profile the other tests use it seeds
  //  roughly one curve per point - about four hundred free parameters, which the
  //  user experiences as a hang and a test experiences as a timeout. Eleven
  //  points with one peak is the same code path and finishes.
  Prof := Default(TPointsData);
  Prof.Title := 'profile';
  SetLength(Prof.X, 11);
  SetLength(Prof.Y, 11);
  for i := 0 to 10 do
  begin
    Prof.X[i] := i;
    Prof.Y[i] := GaussPoint(100, 2.0, 5, i);
  end;
  R := Call('PUT', Format('/problems/%d/profile', [Id]),
    PointsToJsonString(Prof), Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  No positions given: working out where the curves go is part of what this
  //  verb is for.
  R := Call('POST', Format('/problems/%d/actions/do-all-automatically', [Id]),
    '', Code);
  try
    AssertEquals('accepted', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/rfactor', [Id]), '', Code);
  try
    AssertEquals('rfactor status', 200, Code);
    CurMin := R.Get('curMin', -1.0);
    //  STRICTLY GREATER THAN ZERO, not merely present: a zero would mean the
    //  sequence reported success without the optimiser having run.
    AssertTrue('the fit actually ran (curMin=' + FloatToStr(CurMin) + ')',
      CurMin > 0);
  finally
    R.Free;
  end;

  //  And the profile it worked from is still there afterwards. The automatic
  //  sequence changes state several times, and a state handler that cleared the
  //  problem would report success and leave nothing behind.
  R := Call('GET', Format('/problems/%d/profile', [Id]), '', Code);
  try
    AssertEquals('the profile survives', 200, Code);
  finally
    R.Free;
  end;
end;

{ CONTINUING A FIT, which is the verb "minimize-difference-again" - and its
  contract is narrower than its name suggests, which is what this pins.

  IT REQUIRES THE MODEL TO BE READY AND NOT YET FITTED. The engine demands
  ReadyForFit; a finished fit leaves Finished, so calling it after a fit is
  REFUSED. Continuing from where a fit stopped is what plain "minimize-difference"
  does - it resumes from the parameters in the model - and that is what the
  desktop offers. See findings.md: this verb is reachable over REST, is offered by
  no menu item, and refuses in the state its name describes. }
procedure TRestApiFitTest.AndAFitCanBeContinuedFromWhereItStopped;
var
  Id, Code, i: longint;
  R: TJSONObject;
  Prof, Pos, Bounds: TPointsData;
  Msg: string;
begin
  Id := NewProblem;

  Prof := Default(TPointsData);
  Prof.Title := 'profile';
  SetLength(Prof.X, 11);
  SetLength(Prof.Y, 11);
  for i := 0 to 10 do
  begin
    Prof.X[i] := i;
    Prof.Y[i] := GaussPoint(100, 2.0, 5, i);
  end;
  R := Call('PUT', Format('/problems/%d/profile', [Id]),
    PointsToJsonString(Prof), Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  One curve, and an interval to fit it over. BOTH are what puts the engine
  //  into ReadyForFit - the model knows what to fit and where.
  Pos := Default(TPointsData);
  Pos.Title := 'positions';
  SetLength(Pos.X, 1);
  SetLength(Pos.Y, 1);
  Pos.X[0] := 5;
  Pos.Y[0] := GaussPoint(100, 2.0, 5, 5);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  Bounds := Default(TPointsData);
  Bounds.Title := 'rfactor-bounds';
  SetLength(Bounds.X, 2);
  SetLength(Bounds.Y, 2);
  Bounds.X[0] := 0;
  Bounds.Y[0] := 0;
  Bounds.X[1] := 10;
  Bounds.Y[1] := 0;
  R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
    PointsToJsonString(Bounds), Code);
  R.Free;
  AssertEquals('bounds accepted', 200, Code);

  //  READY, NOT YET FITTED: accepted.
  R := Call('POST',
    Format('/problems/%d/actions/minimize-difference-again', [Id]), '', Code);
  try
    AssertEquals('accepted while the model is ready', 200, Code);
    AssertTrue('ok', R.Get('ok', False));
  finally
    R.Free;
  end;

  //  AND NOW REFUSED, because the fit above finished. This is the engine's rule,
  //  not a defect in the request: 400 is "wrong for this state" and the message
  //  says which state.
  R := Call('POST',
    Format('/problems/%d/actions/minimize-difference-again', [Id]), '', Code);
  try
    AssertEquals('refused once the fit has finished', 400, Code);
    Msg := R.Get('error', '');
    AssertTrue('and it says why (' + Msg + ')', Msg <> '');
  finally
    R.Free;
  end;

  //  The problem is still usable afterwards, which is the difference between a
  //  refusal and a fault.
  R := Call('GET', Format('/problems/%d/rfactor', [Id]), '', Code);
  try
    AssertEquals('still answering', 200, Code);
  finally
    R.Free;
  end;
end;

{ ---- endpoints that do not exist ------------------------------------------ }

{ THE ROUTER CHECKS THE PATH LENGTH TWICE, once before it needs a problem id and
  once before it needs a resource name, and each check answers 404. Both were
  unexercised: every existing test asks for something that exists.

  404 rather than 500 is the contract that matters here - anything driving this
  server has to be able to tell "you asked for something that is not here" from
  "the server broke", and a 500 invites a retry that will fail identically. }
procedure TRestApiTest.APathWithNoProblemIdIsNotFound;
var
  Code: longint;
  R: TJSONObject;
begin
  R := Call('GET', '/nonsense', '', Code);
  try
    AssertEquals('not found', 404, Code);
    AssertTrue('and it says what was asked for',
      Pos('nonsense', R.Get('error', '')) > 0);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.APathWithNoResourceIsNotFound;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  A REAL problem, so this passes the id check and falls at the next one -
  //  which is a different branch from the one above.
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d', [Id]), '', Code);
  try
    AssertEquals('not found', 404, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AnUnknownResourceIsNotFound;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := NewProblem;
  R := Call('GET', Format('/problems/%d/nonesuch', [Id]), '', Code);
  try
    AssertEquals('not found', 404, Code);
  finally
    R.Free;
  end;
end;

procedure TRestApiTest.AnUnknownMethodOnAKnownPathIsNotFound;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  //  The path exists for GET; PATCH is not a verb this server has. The method is
  //  part of the route, so this must not be mistaken for the GET.
  Id := NewProblem;
  R := Call('PATCH', Format('/problems/%d/state', [Id]), '', Code);
  try
    AssertEquals('not found', 404, Code);
  finally
    R.Free;
  end;
end;

{ EVERY SETTING THROUGH THE WIRE, one PUT and one GET.

  The route applies each field only when it is present, so nine near-identical
  two-line branches sit side by side - which is exactly the shape where one field
  ends up assigned from its neighbour's value, or read back from the wrong name.
  Four of them had never been exercised.

  Written and read in ONE request each, so the test also says the reply carries
  every setting rather than only the ones somebody thought to check. }
procedure TRestApiTest.EverySettingIsWrittenAndReadBackUnderItsOwnName;
var
  Id, Code: longint;
  R: TJSONObject;
begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"maxRFactor":0.0025,"backFactor":17,"curveThresh":0.75,' +
    '"waveLength":1.5406,"backgroundVariation":true,"curveScaling":true,' +
    '"minimizerKind":1,"lossKind":2,"weighting":"poisson"}', Code);
  R.Free;
  AssertEquals('accepted', 200, Code);

  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('readable', 200, Code);
    AssertEquals('the R-factor limit', 0.0025, R.Get('maxRFactor', 0.0), 1e-9);
    AssertEquals('the background factor', 17.0,
      R.Get('backFactor', 0.0), 1e-9);
    AssertEquals('the curve threshold', 0.75,
      R.Get('curveThresh', 0.0), 1e-9);
    AssertEquals('the wavelength', 1.5406, R.Get('waveLength', 0.0), 1e-9);
    AssertTrue('background variation', R.Get('backgroundVariation', False));
    AssertTrue('curve scaling', R.Get('curveScaling', False));
    AssertEquals('the minimizer', 1, R.Get('minimizerKind', -1));
    AssertEquals('the objective', 2, R.Get('lossKind', -1));
    AssertEquals('the weighting', 'poisson', R.Get('weighting', ''));
  finally
    R.Free;
  end;

  //  AND A PARTIAL WRITE LEAVES THE REST ALONE, which is the other half of
  //  "applied only when present": a client that sends one field must not reset
  //  the eight it did not mention.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"backFactor":3}', Code);
  R.Free;
  AssertEquals('accepted', 200, Code);
  R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
  try
    AssertEquals('the one sent changed', 3.0, R.Get('backFactor', 0.0), 1e-9);
    AssertEquals('and the others did not', 0.75,
      R.Get('curveThresh', 0.0), 1e-9);
    AssertEquals('nor this one', 'poisson', R.Get('weighting', ''));
  finally
    R.Free;
  end;
end;

{ ---- pruning ---------------------------------------------------------------- }

{ A SHORT PROFILE WITH ONE PEAK, used by both pruning tests below. }
function OnePeakProfileJson: string;
var
  P: TPointsData;
  i: longint;
begin
  P := Default(TPointsData);
  P.Title := 'profile';
  SetLength(P.X, 21);
  SetLength(P.Y, 21);
  for i := 0 to 20 do
  begin
    P.X[i] := i;
    P.Y[i] := GaussPoint(100, 2.5, 10, i);
  end;
  Result := PointsToJsonString(P);
end;

{ "FIT, THEN DROP THE CURVES THAT DO NOT EARN THEIR PLACE" - with one curve in
  the model there is nothing to drop, and the engine must say so by leaving it
  alone rather than by pruning the model down to nothing.

  It is the guard at the top of the pruning routine, and it had never run: every
  existing test either fits without pruning or prunes a model it never checks the
  size of. A model pruned to zero curves is a chart with the data and no fit on
  it, reported as success. }
procedure TRestApiFitTest.TheLastCurveIsNeverPrunedAway;
var
  Id, Code: longint;
  R: TJSONObject;
  Pos_: TPointsData;
  Before, After: longint;

  function CurveCount: longint;
  var
    Reply: TJSONObject;
    Arr: TJSONArray;
  begin
    Result := -1;
    Reply := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
    try
      Arr := TJSONArray(Reply.Find('curves'));
      if Assigned(Arr) then
        Result := Arr.Count;
    finally
      Reply.Free;
    end;
  end;

begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 1);
  SetLength(Pos_.Y, 1);
  Pos_.X[0] := 10;
  Pos_.Y[0] := GaussPoint(100, 2.5, 10, 10);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  R.Free;
  AssertEquals('the fit', 200, Code);
  Before := CurveCount;
  AssertEquals('one curve was built', 1, Before);

  R := Call('POST',
    Format('/problems/%d/actions/minimize-number-of-curves', [Id]), '', Code);
  try
    AssertEquals('pruning is accepted', 200, Code);
  finally
    R.Free;
  end;

  After := CurveCount;
  AssertEquals('and the only curve is still there', 1, After);
end;

{ THE SAME VERB WITH SEVERAL CURVES, which is what it is actually for. What is
  pinned is not how many survive - that is the algorithm's judgement and it
  depends on the data - but that the model is still a model afterwards: at least
  one curve, and the problem still answers. A prune that emptied the model or
  left it unusable would report success either way. }
procedure TRestApiFitTest.PruningAModelOfSeveralCurvesLeavesItFittable;
var
  Id, Code: longint;
  R: TJSONObject;
  Pos_: TPointsData;
  Arr: TJSONArray;
begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  Three curves over one peak: two of them have nothing of their own to
  //  explain, which is the situation the verb exists for.
  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 3);
  SetLength(Pos_.Y, 3);
  Pos_.X[0] := 9;  Pos_.Y[0] := GaussPoint(100, 2.5, 10, 9);
  Pos_.X[1] := 10; Pos_.Y[1] := GaussPoint(100, 2.5, 10, 10);
  Pos_.X[2] := 11; Pos_.Y[2] := GaussPoint(100, 2.5, 10, 11);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('POST',
    Format('/problems/%d/actions/minimize-number-of-curves', [Id]), '', Code);
  try
    AssertEquals('pruning is accepted', 200, Code);
  finally
    R.Free;
  end;

  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertEquals('the model is readable afterwards', 200, Code);
    Arr := TJSONArray(R.Find('curves'));
    AssertTrue('and it still has curves',
      Assigned(Arr) and (Arr.Count >= 1));
  finally
    R.Free;
  end;
end;

{ THE MODEL FOLLOWS THE PICKS. A curve exists because the user put a position
  there; take the position away and refit, and the curve it seeded has to go with
  it.

  The alternative is worse than untidy: the curve stays in the model, keeps being
  fitted, and appears on the chart with nothing under it to explain why - and the
  user has just deleted the only thing that would have explained it. The loop that
  drops it had never been executed, because every test that refits keeps the same
  positions. }
procedure TRestApiFitTest.ACurveWhosePositionWasRemovedIsDropped;
var
  Id, Code: longint;
  R: TJSONObject;
  Pos_: TPointsData;

  function CurveCount: longint;
  var
    Reply: TJSONObject;
    Arr: TJSONArray;
  begin
    Result := -1;
    Reply := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
    try
      Arr := TJSONArray(Reply.Find('curves'));
      if Assigned(Arr) then
        Result := Arr.Count;
    finally
      Reply.Free;
    end;
  end;

begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  Two positions, so there are two curves to tell apart.
  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 2);
  SetLength(Pos_.Y, 2);
  Pos_.X[0] := 8;  Pos_.Y[0] := GaussPoint(100, 2.5, 10, 8);
  Pos_.X[1] := 12; Pos_.Y[1] := GaussPoint(100, 2.5, 10, 12);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  R.Free;
  AssertEquals('the first fit', 200, Code);
  AssertEquals('two curves were built', 2, CurveCount);

  //  Now the user deletes one pick and fits again.
  SetLength(Pos_.X, 1);
  SetLength(Pos_.Y, 1);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('the shorter list is accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  R.Free;
  AssertEquals('the second fit', 200, Code);

  AssertEquals('the curve with no position behind it is gone', 1, CurveCount);
end;

{ RUBBISH IN THE BODY IS THE CALLER'S FAULT, AND MUST BE SAID SO.

  Six routes read a JSON body, and each opens with the same shape: parse, and on
  failure answer 400 with what was wrong. None of those six branches had ever run.

  400 RATHER THAN 500 IS THE WHOLE POINT. A 500 tells the caller the server broke
  and the request is worth retrying unchanged, which for a malformed body is an
  invitation to retry forever. It also buries a real fault among client mistakes
  in the log, where the levels differ: a refusal is a Warning and a fault is
  Fatal.

  AND NOT A CRASH. These bodies arrive from outside the process. A parser that
  faulted on bad input would be reachable by anything that can reach the port. }
procedure TRestApiTest.EveryRouteThatTakesABodyRefusesAMalformedOne;
var
  Id, Code: longint;
  R: TJSONObject;
  Refusals: longint;

  { Sends garbage to one route and counts a well-formed refusal. }
  procedure Rubbish(const AMethod, APath: string);
  var
    Reply: TJSONObject;
    Err: string;
  begin
    //  Not valid JSON at all, which is the case the parser has to survive; a
    //  well-formed document with the wrong fields is a different test.
    Reply := Call(AMethod, APath, '}{ this is not json', Code);
    try
      AssertEquals(AMethod + ' ' + APath + ': the caller is at fault',
        400, Code);
      Err := '';
      if Assigned(Reply) then
        Err := Reply.Get('error', '');
      AssertTrue(AMethod + ' ' + APath + ': and is told what was wrong',
        Err <> '');
      Inc(Refusals);
    finally
      if Assigned(Reply) then
        Reply.Free;
    end;
  end;

begin
  Id := NewProblem;
  Refusals := 0;

  //  A point set, the settings, the user-defined curve's parameters.
  Rubbish('PUT', Format('/problems/%d/profile', [Id]));
  Rubbish('PUT', Format('/problems/%d/settings', [Id]));
  Rubbish('PUT', Format('/problems/%d/special-params', [Id]));
  //  A pick added, and a pick moved.
  Rubbish('POST', Format('/problems/%d/points/background', [Id]));
  Rubbish('PUT', Format('/problems/%d/points/background', [Id]));
  //  NOT the curve-parameter route: it looks the handle up BEFORE reading the
  //  body, so an invented handle is a 404 about the curve rather than a 400
  //  about the body - which is the right order, since a body meant for a curve
  //  that does not exist has nothing to be judged against. Its malformed-body
  //  branch needs a real handle, so it is tested where curves exist: see
  //  AMalformedParameterBodyIsRefusedForARealCurve.

  AssertEquals('every body-taking route reachable here was exercised',
    5, Refusals);

  //  AND THE PROBLEM IS STILL USABLE, which is the difference between a refusal
  //  and damage: six bad requests must leave it exactly as they found it.
  R := Call('GET', Format('/problems/%d/state', [Id]), '', Code);
  try
    AssertEquals('still answering', 200, Code);
  finally
    R.Free;
  end;
end;

{ THE ONE MALFORMED-BODY BRANCH THAT NEEDS A REAL CURVE.

  The parameter route looks its handle up BEFORE reading the body, which is the
  right order - a body meant for a curve that does not exist has nothing to be
  judged against - so the unit test above cannot reach its parse failure and says
  so. Here there is a fitted model, so the handle is real and the body is the only
  thing wrong with the request.

  400 and not 500, for the same reason as the rest: a caller that sent rubbish
  must not be told the server broke. }
procedure TRestApiFitTest.AMalformedParameterBodyIsRefusedForARealCurve;
var
  Id, Code: longint;
  R: TJSONObject;
  Handle, Err: string;
  Pos_: TPointsData;
begin
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 1);
  SetLength(Pos_.Y, 1);
  Pos_.X[0] := 10;
  Pos_.Y[0] := GaussPoint(100, 2.5, 10, 10);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
    '', Code);
  R.Free;
  AssertEquals('the fit', 200, Code);

  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    Handle := TJSONObject(R.Arrays['curves'].Items[0]).Get('id', '');
  finally
    R.Free;
  end;
  AssertTrue('the curve has a handle', Handle <> '');

  R := Call('PUT', Format('/problems/%d/curves/%s/params/0', [Id, Handle]),
    '}{ not json', Code);
  try
    AssertEquals('the body is the caller''s fault', 400, Code);
    Err := '';
    if Assigned(R) then
      Err := R.Get('error', '');
    AssertTrue('and it says so', Err <> '');
  finally
    if Assigned(R) then
      R.Free;
  end;

  //  The model is untouched: a rejected write must not have written anything.
  R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
  try
    AssertEquals('the model still reads', 200, Code);
    AssertEquals('and still has its curve', 1, R.Arrays['curves'].Count);
  finally
    R.Free;
  end;
end;

procedure TRestApiFitTest.AnAutomaticRunOverASELECTEDINTERVALTakesThePicksWithIt;
var
  Id, Code, i: longint;
  R: TJSONObject;
  Pos_, Back: TPointsData;
  Body: string;

  function CurveCount: longint;
  var
    Reply: TJSONObject;
    Arr: TJSONArray;
  begin
    Result := -1;
    Reply := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
    try
      Arr := TJSONArray(Reply.Find('curves'));
      if Assigned(Arr) then
        Result := Arr.Count;
    finally
      Reply.Free;
    end;
  end;

begin
  //  THE BRANCH A WHOLE-PROFILE RUN NEVER TAKES. When a sub-interval is
  //  selected, a task's range is expressed in the SELECTED AREA, and the picks
  //  it is responsible for have to be looked up there too - which is why
  //  AdoptCurveRemovalsFromTasks chooses the set before it looks anything up.
  //
  //  THE INTERVAL IS DELIBERATELY NOT AT THE ORIGIN. With it starting at 0 the
  //  two index spaces coincide and the wrong one is indistinguishable from the
  //  right one; starting at 8 makes the last pick's profile index fall outside
  //  the task's range, so a run using the profile skips it - and leaves the
  //  pick of a curve it has just removed behind, to be rebuilt into that curve
  //  at the next edit.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  R := Call('POST', Format('/problems/%d/actions/select-profile-interval',
    [Id]), '{"start":8,"stop":16}', Code);
  R.Free;
  AssertEquals('the interval is selected', 200, Code);

  //  A GENEROUS CEILING, so the run is free to drop a curve: pruning stops when
  //  removing the next one would push the R-factor past this, and the default
  //  is tight enough that a three-curve model over one peak may keep all three.
  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"maxRFactor":0.5}', Code);
  R.Free;
  AssertEquals('the ceiling is accepted', 200, Code);

  //  MORE CURVES THAN THE PEAK NEEDS, which is what gives the automatic run
  //  something to remove. All inside the interval: a pick outside it is refused
  //  outright ("every curve position must fall on a sample of the profile"),
  //  so this branch is only ever reached with picks that are in it.
  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 3);
  SetLength(Pos_.Y, 3);
  //  Three curves over one peak: two of them have nothing of their own to
  //  explain, which is the situation the verb exists for. Inside the interval,
  //  whose start is what makes the two index spaces differ.
  for i := 0 to 2 do
  begin
    Pos_.X[i] := 9 + i;
    Pos_.Y[i] := GaussPoint(100, 2.5, 10, Pos_.X[i]);
  end;
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  Body := '';
  FApi.Handle('POST', Format('/problems/%d/actions/minimize-number-of-curves',
    [Id]), '', Code, Body);
  AssertEquals('the automatic run: ' + Body, 200, Code);

  //  THE INVARIANT, rather than a count the optimiser is free to choose: one
  //  pick per curve. A pick left behind for a removed curve is invisible until
  //  the next edit rebuilds it.
  Body := '';
  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertEquals('the picks come back', 200, Code);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Back));
  AssertEquals('one pick per curve after the run', CurveCount, Length(Back.X));
  AssertTrue('and the run did remove something', Length(Back.X) < 3);
end;

procedure TRestApiFitTest.APickBetweenTwoFitIntervalsBelongsToNoTaskAndIsLeftAlone;
var
  Id, Code, i: longint;
  R: TJSONObject;
  Pos_, Back: TPointsData;
  Body: string;
  Kept: boolean;
begin
  //  A PICK NO TASK OWNS. Fit intervals are marked in pairs and need not cover
  //  the profile, so a pick can sit in the gap between two of them - it belongs
  //  to no task, and no task can have an opinion about whether it survives.
  //
  //  Without that guard an automatic run reads the gap pick as one of its own,
  //  finds it among no survivors, and deletes a pick the user placed - in a
  //  stretch the run never even looked at.
  Id := NewProblem;
  R := Call('PUT', Format('/problems/%d/profile', [Id]), OnePeakProfileJson, Code);
  R.Free;
  AssertEquals('profile accepted', 200, Code);

  //  TWO INTERVALS WITH A GAP: 0..6 and 12..20, so x = 9 is in neither.
  for i := 0 to 3 do
  begin
    R := Call('POST', Format('/problems/%d/points/rfactor-bounds', [Id]),
      Format('{"x":%d.0,"y":0.0}', [Ord(i = 1) * 6 + Ord(i = 2) * 12 +
        Ord(i = 3) * 20]), Code);
    R.Free;
    AssertEquals('bound accepted', 200, Code);
  end;

  //  One pick in each interval so neither task is empty - an interval with no
  //  curves at all cannot be fitted and the run refuses before it starts - two
  //  more in the second so it has something to remove, and one in the GAP.
  Pos_ := Default(TPointsData);
  Pos_.Title := 'positions';
  SetLength(Pos_.X, 5);
  SetLength(Pos_.Y, 5);
  Pos_.X[0] := 3;  Pos_.Y[0] := GaussPoint(100, 2.5, 10, 3);
  Pos_.X[1] := 9;  Pos_.Y[1] := GaussPoint(100, 2.5, 10, 9);
  Pos_.X[2] := 13; Pos_.Y[2] := GaussPoint(100, 2.5, 10, 13);
  Pos_.X[3] := 14; Pos_.Y[3] := GaussPoint(100, 2.5, 10, 14);
  Pos_.X[4] := 15; Pos_.Y[4] := GaussPoint(100, 2.5, 10, 15);
  R := Call('PUT', Format('/problems/%d/positions', [Id]),
    PointsToJsonString(Pos_), Code);
  R.Free;
  AssertEquals('positions accepted', 200, Code);

  R := Call('PUT', Format('/problems/%d/settings', [Id]),
    '{"maxRFactor":0.5}', Code);
  R.Free;

  Body := '';
  FApi.Handle('POST', Format('/problems/%d/actions/minimize-number-of-curves',
    [Id]), '', Code, Body);
  AssertEquals('the automatic run: ' + Body, 200, Code);

  Body := '';
  FApi.Handle('GET', Format('/problems/%d/positions', [Id]), '', Code, Body);
  AssertEquals('the picks come back', 200, Code);
  AssertTrue('positions decoded', PointsFromJsonString(Body, Back));

  Kept := False;
  for i := 0 to High(Back.X) do
    if Abs(Back.X[i] - 9) < 1e-9 then
      Kept := True;
  AssertTrue('the pick between the intervals was left alone', Kept);
end;

initialization
  //  UNIT: pure route dispatch, no socket and no fit.
  RegisterTest('unit', TRestApiTest);
  //  INTEGRATION: every one of these runs the optimiser to convergence.
  RegisterTest('integration', TRestApiFitTest);
end.
