// SPDX-License-Identifier: GPL-3.0-or-later
{ In-process test of the fit wire contract <-> engine mapping: build a task from
  a TFitProblem, run the native optimization, read the TFitOutcome back. This is
  exactly what the server's /fit endpoint does, minus the HTTP hop. }
unit testcase_fit_marshalling;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, curve_points_set,
  fit_task, fit_problem_json, fit_task_marshalling,
  gauss_points_set, pseudo_voigt_points_set, SimpMath;
type
  { THE MAPPING, without a fit: build a problem from a task and read back what
    the wire would carry. No optimiser, so by the project's own rule these are
    unit tests - and they are what makes fit_task_marshalling measurable, since
    coverage is taken over the unit half alone. }
  TFitMarshallingTest = class(TTestCase)
  private
    { A one-Gaussian problem over a five-sample profile, with the position's
      height supplied. The tests that are about a MISSING height build their own
      profile, so the difference is visible where it matters. }
    function OneGaussian: TFitProblem;
  published
    procedure ProblemFromTaskCarriesExpressionAndSeedCurves;
    procedure ProblemFromTaskCarriesVaryAndSharedFlags;

    //  READING THE OUTCOME BACK, which needs no fit and had no caller but the
    //  integration test - so the unit suite, which is what coverage measures,
    //  never reached it. See the comment at the group.
    procedure TheOutcomeNamesOneCurvePerCurveOnTheTask;
    procedure AndEveryParameterOfEachWithItsName;
    procedure TheValuesAreTheTaskCurrentOnesNotDefaults;
    procedure AnUnestimatedUncertaintyComesBackNegative;
    procedure TheRFactorIsTheTaskCurrentMinimum;
    procedure AnOutcomeReadFromATaskAppliesBackOntoItUnchanged;

    //  SEEDING AN AMPLITUDE THE PROBLEM DID NOT SUPPLY.
    procedure APositionWithNoHeightIsSeededFromTheProfile;
    procedure TheHeightTakenIsTheNearestSampleNotTheFirst;
    procedure APositionPastTheEndTakesTheLastSample;
    procedure AProblemWithNoProfileIsRefusedBeforeAnySeedIsSought;
    procedure SomeHeightsSuppliedAndSomeNotIsHandledPerPosition;

    //  The window.
    procedure APositiveEndIndexIsTakenAsGiven;
    procedure AndANonPositiveOneMeansToTheEndOfTheProfile;
  end;

  { The round trip THROUGH a real fit, which drives the optimiser to convergence
    and therefore stays in the slow half. }
  TFitMarshallingFitTest = class(TTestCase)
  published
    procedure BuildsRunsAndReadsBackAGaussianFit;
  end;

implementation

function HasParam(const C: TFitCurveData; const Name: string; out Value: double): boolean;
var j: integer;
begin
  Result := False;
  for j := 0 to High(C.Params) do
    if C.Params[j].Name = Name then
    begin
      Value := C.Params[j].Value;
      Exit(True);
    end;
end;

function ParamByName(const C: TFitCurveData; const Name: string;
  out Prm: TFitParamData): boolean;
var j: integer;
begin
  Result := False;
  for j := 0 to High(C.Params) do
    if C.Params[j].Name = Name then
    begin
      Prm := C.Params[j];
      Exit(True);
    end;
end;

procedure TFitMarshallingFitTest.BuildsRunsAndReadsBackAGaussianFit;
var
  P: TFitProblem;
  Task: TFitTask;
  Outcome: TFitOutcome;
  x, a: double;
  n: integer;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  //  Synthetic Gaussian (amplitude 100, sigma 1.5, centre 10) sampled 0..20.
  P := Default(TFitProblem);
  n := 0;
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    SetLength(P.ProfileX, n + 1);
    SetLength(P.ProfileY, n + 1);
    P.ProfileX[n] := x;
    P.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
    Inc(n);
    x := x + 0.2;
  end;
  //  The position carries the data height at x=10 (the peak); the engine seeds
  //  the curve's amplitude from it.
  P.PositionsX := TDoubleArray.Create(10);
  P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
  P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
  P.MaxRFactor := 0.01;
  P.MinimizerKind := 0;
  P.BegIndex := 0;
  P.EndIndex := 0;   //  "to the end"

  Task := BuildTaskFromProblem(P);
  try
    Task.MinimizeDifference;
    Outcome := ReadOutcomeFromTask(Task);
  finally
    Task.Free;
  end;

  AssertEquals('errorCode', 0, Outcome.ErrorCode);
  //  A genuine fit yields a small but non-zero R-factor (0 would mean it never ran).
  AssertTrue('R-factor genuinely small (' + FloatToStr(Outcome.RFactor) + ')',
    (Outcome.RFactor > 0) and (Outcome.RFactor < 0.05));
  AssertEquals('one fitted curve', 1, Length(Outcome.Curves));
  AssertTrue('curve reports an amplitude A', HasParam(Outcome.Curves[0], 'A', a));
  AssertTrue('fitted amplitude near 100 (' + FloatToStr(a) + ')',
    Abs(a - 100) < 5);
end;

function TFitMarshallingTest.OneGaussian: TFitProblem;
begin
  Result := Default(TFitProblem);
  Result.ProfileX := TDoubleArray.Create(8, 9, 10, 11, 12);
  Result.ProfileY := TDoubleArray.Create(1, 5, 10, 5, 1);
  Result.PositionsX := TDoubleArray.Create(10);
  Result.PositionsY := TDoubleArray.Create(10);
  Result.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
  Result.MinimizerKind := 0;
end;

procedure TFitMarshallingTest.ProblemFromTaskCarriesExpressionAndSeedCurves;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  Ref: TGaussPointsSet;
  Expected: string;
  a: double;
begin
  Ref := TGaussPointsSet.Create(nil, 10.0);
  Expected := Ref.GetCurveExpression;
  Ref.Free;
  //  A minimal Gaussian problem, just enough to seed one curve.
  P := Default(TFitProblem);
  P.ProfileX := TDoubleArray.Create(8, 9, 10, 11, 12);
  P.ProfileY := TDoubleArray.Create(1, 5, 10, 5, 1);
  P.PositionsX := TDoubleArray.Create(10);
  P.PositionsY := TDoubleArray.Create(10);
  P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
  P.MinimizerKind := 0;

  Task := BuildTaskFromProblem(P);
  try
    //  The client side of the wire: describe the live task as a problem for the
    //  Python backend. It must carry the formula and the placed curve's seeds.
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;

  AssertEquals('expression is the Gaussian formula', Expected, Q.Expression);
  AssertEquals('one seed curve', 1, Length(Q.Curves));
  AssertTrue('seed curve carries amplitude A', HasParam(Q.Curves[0], 'A', a));
  AssertTrue('seed A is the placed height', Abs(a - 10) < 1e-6);
end;

{ ------------------------- reading the outcome back ------------------------- }

{ NO FIT IS RUN BELOW, and that is the whole reason these exist.
  ReadOutcomeFromTask is what the compute server answers /fit with, and its only
  caller in the suite was the integration test in this file - which drives the
  optimiser to convergence and so does not run under coverage. The function that
  turns a finished fit into the numbers a client receives was therefore reported
  as a quarter of this unit going unmeasured, when nothing about it needs a fit:
  it reads the curves a task already holds, and a task built and never optimised
  holds its seeds, which is a perfectly good thing to read. }

procedure TFitMarshallingTest.TheOutcomeNamesOneCurvePerCurveOnTheTask;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
begin
  P := OneGaussian;
  P.PositionsX := TDoubleArray.Create(9, 11);
  P.PositionsY := TDoubleArray.Create(5, 5);
  Task := BuildTaskFromProblem(P);
  try
    O := ReadOutcomeFromTask(Task);
    //  ONE PER CURVE, in the task's order. The client matches its own curves to
    //  these BY INDEX - only the parameters within a curve are matched by name -
    //  so a dropped or reordered curve puts one curve's fit onto another.
    AssertEquals('curves', Task.GetCurves.Count, Length(O.Curves));
    AssertEquals('two positions gave two curves', 2, Length(O.Curves));
    AssertEquals('and no error', 0, O.ErrorCode);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.AndEveryParameterOfEachWithItsName;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
  Prm: TFitParamData;
begin
  P := OneGaussian;
  Task := BuildTaskFromProblem(P);
  try
    O := ReadOutcomeFromTask(Task);
    //  ASKED AS A COUNT AGAINST THE TASK, not as the three names a Gaussian
    //  happens to have: a curve type with a fourth parameter must carry it too,
    //  and the client silently drops whatever is not sent.
    AssertEquals('every parameter of the curve',
      TCurvePointsSet(Task.GetCurves.Items[0]).Parameters.Count,
      Length(O.Curves[0].Params));
    AssertTrue('x0 is named', ParamByName(O.Curves[0], 'x0', Prm));
    AssertTrue('A is named', ParamByName(O.Curves[0], 'A', Prm));
    AssertTrue('sigma is named', ParamByName(O.Curves[0], 'sigma', Prm));
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.TheValuesAreTheTaskCurrentOnesNotDefaults;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
  Prm: TFitParamData;
begin
  P := OneGaussian;
  Task := BuildTaskFromProblem(P);
  try
    //  Moved first, so a reader answering the curve TYPE's defaults rather than
    //  the instance's values would be caught. After a real fit those two differ
    //  by the whole of the fit, and the client would be shown the seeds.
    TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma'] := 2.75;
    O := ReadOutcomeFromTask(Task);
    AssertTrue('sigma came back', ParamByName(O.Curves[0], 'sigma', Prm));
    AssertEquals('with the value the curve holds', 2.75, Prm.Value, 1e-9);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.AnUnestimatedUncertaintyComesBackNegative;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
  Prm: TFitParamData;
begin
  P := OneGaussian;
  Task := BuildTaskFromProblem(P);
  try
    O := ReadOutcomeFromTask(Task);
    //  CARRIED SEPARATELY FROM THE VALUE, and it must not arrive as zero: a
    //  task that has not been fitted has estimated nothing, and a zero
    //  uncertainty reads in the parameters table as a value known exactly.
    AssertTrue('x0 came back', ParamByName(O.Curves[0], 'x0', Prm));
    AssertTrue('its uncertainty is not claimed as zero', Prm.Error < 0);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.TheRFactorIsTheTaskCurrentMinimum;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
begin
  P := OneGaussian;
  Task := BuildTaskFromProblem(P);
  try
    O := ReadOutcomeFromTask(Task);
    //  Read off the task rather than restated here, so the two cannot drift. It
    //  is the number the client shows as the fit's quality, and a stale or
    //  defaulted one reports a fit that did not happen as a good one.
    AssertEquals('the R-factor is the task value',
      Task.GetCurMin, O.RFactor, 1e-12);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.AnOutcomeReadFromATaskAppliesBackOntoItUnchanged;
var
  P: TFitProblem;
  Task: TFitTask;
  O: TFitOutcome;
  Before, After: double;
begin
  //  THE ROUND TRIP, WITH NO FIT IN THE MIDDLE. Read and write are the two
  //  halves of one wire contract, and this identity is what says they agree on
  //  the same names: a reader emitting a parameter the writer looks up under
  //  another name would leave every value at its seed, and both halves would
  //  still pass their own tests.
  P := OneGaussian;
  Task := BuildTaskFromProblem(P);
  try
    TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma'] := 2.75;
    Before := TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma'];
    O := ReadOutcomeFromTask(Task);
    ApplyOutcomeToTask(Task, O);
    After := TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma'];
    AssertEquals('a value survives being read out and written back',
      Before, After, 1e-12);
  finally
    Task.Free;
  end;
end;

{ ------------------ seeding an amplitude the problem omitted ---------------- }

{ THE ENGINE SEEDS EACH CURVE'S AMPLITUDE FROM THE HEIGHT AT ITS POSITION, so a
  problem naming a position and no height would start the fit from a
  zero-amplitude curve - degenerate, and not something an optimiser recovers
  from. The fallback reads the height off the profile instead. It had no test at
  all: every caller in the suite supplied both arrays at the same length. }

procedure TFitMarshallingTest.APositionWithNoHeightIsSeededFromTheProfile;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  a: double;
begin
  P := OneGaussian;
  //  A position and NO heights whatsoever.
  P.PositionsY := nil;
  Task := BuildTaskFromProblem(P);
  try
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;
  AssertTrue('the curve carries an amplitude', HasParam(Q.Curves[0], 'A', a));
  AssertEquals('seeded from the profile height at x=10', 10.0, a, 1e-6);
end;

procedure TFitMarshallingTest.TheHeightTakenIsTheNearestSampleNotTheFirst;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  a: double;
begin
  //  NEAREST, and the heights are deliberately unrelated to the position: a
  //  fallback answering the first sample, or the last one it walked past, would
  //  seed a peak from a height measured somewhere else entirely, and the fit
  //  would look merely poor rather than wrong.
  P := OneGaussian;
  P.ProfileX := TDoubleArray.Create(0, 10, 20, 30);
  P.ProfileY := TDoubleArray.Create(7, 42, 3, 99);
  P.PositionsX := TDoubleArray.Create(11);
  P.PositionsY := nil;
  Task := BuildTaskFromProblem(P);
  try
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;
  AssertTrue('the curve carries an amplitude', HasParam(Q.Curves[0], 'A', a));
  AssertEquals('the sample at 10, nearest to 11', 42.0, a, 1e-6);
end;

procedure TFitMarshallingTest.APositionPastTheEndTakesTheLastSample;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  a: double;
begin
  //  Clamped rather than refused or zeroed. A position outside the profile is
  //  not this function's business to reject - the window does that - and a zero
  //  here would be the degenerate seed the fallback exists to avoid.
  P := OneGaussian;
  P.ProfileX := TDoubleArray.Create(0, 10, 20);
  P.ProfileY := TDoubleArray.Create(7, 42, 3);
  P.PositionsX := TDoubleArray.Create(1000);
  P.PositionsY := nil;
  Task := BuildTaskFromProblem(P);
  try
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;
  AssertTrue('the curve carries an amplitude', HasParam(Q.Curves[0], 'A', a));
  AssertEquals('the last sample', 3.0, a, 1e-6);
end;

procedure TFitMarshallingTest.AProblemWithNoProfileIsRefusedBeforeAnySeedIsSought;
var
  P: TFitProblem;
  Raised: boolean;
  i: integer;
begin
  //  CHARACTERISED, because it settles a question the code asks twice. The
  //  fallback that reads a height off the profile opens by answering zero for an
  //  empty one - a sensible guard against indexing an empty array. It cannot
  //  fire. Setting the profile on the task comes first, and the task checks that
  //  it holds at least two points, so a problem with no profile never reaches
  //  the seed lookup at all.
  //
  //  So the guard is dead in the way that is hardest to see: it reads as the
  //  handling of a case that is in fact refused upstream, with a different
  //  message, at a different layer. Left in place - it is one comparison, and
  //  the function is exposed to nothing else that could keep the invariant - but
  //  recorded here rather than covered by a test that would have to lie about
  //  which check answered.
  //
  //  Both lengths that fail the task's check, so "at least two" is pinned rather
  //  than "not empty".
  for i := 0 to 1 do
  begin
    P := Default(TFitProblem);
    if i = 1 then
    begin
      P.ProfileX := TDoubleArray.Create(10);
      P.ProfileY := TDoubleArray.Create(5);
    end;
    P.PositionsX := TDoubleArray.Create(10);
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    Raised := False;
    try
      BuildTaskFromProblem(P).Free;
    except
      on Exception do
        Raised := True;
    end;
    AssertTrue(Format('a profile of %d points is refused', [i]), Raised);
  end;
end;

procedure TFitMarshallingTest.SomeHeightsSuppliedAndSomeNotIsHandledPerPosition;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  a0, a1: double;
begin
  //  PER POSITION, not per problem. The test is on the array lengths, so a
  //  short PositionsY means "the rest are missing" rather than "none were
  //  given" - and a fallback applied to the whole problem would throw away the
  //  height the caller did supply for the first curve.
  P := OneGaussian;
  P.ProfileX := TDoubleArray.Create(0, 10, 20);
  P.ProfileY := TDoubleArray.Create(7, 42, 3);
  P.PositionsX := TDoubleArray.Create(0, 20);
  P.PositionsY := TDoubleArray.Create(555);
  Task := BuildTaskFromProblem(P);
  try
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;
  AssertEquals('two curves', 2, Length(Q.Curves));
  AssertTrue('the first has an amplitude', HasParam(Q.Curves[0], 'A', a0));
  AssertTrue('the second has one', HasParam(Q.Curves[1], 'A', a1));
  AssertEquals('the supplied height was kept', 555.0, a0, 1e-6);
  AssertEquals('the missing one came off the profile', 3.0, a1, 1e-6);
end;

{ ------------------------------- the window --------------------------------- }

procedure TFitMarshallingTest.APositiveEndIndexIsTakenAsGiven;
var
  P: TFitProblem;
  Task: TFitTask;
begin
  P := OneGaussian;
  P.BegIndex := 1;
  P.EndIndex := 3;
  Task := BuildTaskFromProblem(P);
  try
    AssertEquals('the window the problem asked for', 1, Task.BegIndex);
    AssertEquals('and its end', 3, Task.EndIndex);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.AndANonPositiveOneMeansToTheEndOfTheProfile;
var
  P: TFitProblem;
  Task: TFitTask;
begin
  //  ZERO IS NOT A WINDOW OF ONE SAMPLE. A record's default is zero, so a
  //  caller that never set the field would otherwise fit the first point alone
  //  and report a perfect R-factor over it.
  P := OneGaussian;
  P.EndIndex := 0;
  Task := BuildTaskFromProblem(P);
  try
    AssertEquals('to the last computed point',
      Task.GetCalcProfile.PointsCount - 1, Task.EndIndex);
  finally
    Task.Free;
  end;
end;

procedure TFitMarshallingTest.ProblemFromTaskCarriesVaryAndSharedFlags;
var
  P, Q: TFitProblem;
  Task: TFitTask;
  Prm: TFitParamData;
  x: double;
  n: integer;
begin
  //  Pseudo-Voigt: its sigma is a Shared parameter and the abscissa is Argument.
  //  The marshalled seeds must mark sigma shared, A varied, and omit the abscissa.
  P := Default(TFitProblem);
  n := 0;
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    SetLength(P.ProfileX, n + 1);
    SetLength(P.ProfileY, n + 1);
    P.ProfileX[n] := x;
    P.ProfileY[n] := PseudoVoigtPoint(100, 1.5, 0.5, 10, x);
    Inc(n);
    x := x + 0.5;
  end;
  P.PositionsX := TDoubleArray.Create(10);
  P.PositionsY := TDoubleArray.Create(PseudoVoigtPoint(100, 1.5, 0.5, 10, 10));
  P.CurveTypeId := GUIDToString(TPseudoVoigtPointsSet.GetCurveTypeId);
  P.MinimizerKind := 0;

  Task := BuildTaskFromProblem(P);
  try
    Q := BuildProblemFromTask(Task);
  finally
    Task.Free;
  end;

  AssertEquals('one seed curve', 1, Length(Q.Curves));
  AssertFalse('abscissa (?) is not a seed parameter',
    ParamByName(Q.Curves[0], '?', Prm));
  AssertTrue('sigma present', ParamByName(Q.Curves[0], 'sigma', Prm));
  AssertTrue('sigma is shared', Prm.Shared);
  AssertTrue('sigma is varied', Prm.Vary);
  AssertTrue('A present', ParamByName(Q.Curves[0], 'A', Prm));
  AssertFalse('A is not shared', Prm.Shared);
  AssertTrue('A is varied', Prm.Vary);
  //  The amplitude's physical bound (SetValue takes Abs) travels with it, so the
  //  Python fit stays in the region the native engine would accept.
  AssertEquals('A min is 0', 0.0, Prm.Min, 1e-12);
  //  Weighting defaults to the statistically-correct choice for counting data.
  AssertEquals('weighting default', 'poisson', Q.Weighting);
end;

initialization
  //  UNIT: the wire mapping, no optimiser.
  RegisterTest('unit', TFitMarshallingTest);
  //  INTEGRATION: runs the fit to convergence.
  RegisterTest('integration', TFitMarshallingFitTest);
end.
