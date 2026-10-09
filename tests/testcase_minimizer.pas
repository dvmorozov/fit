// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_minimizer;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, downhill_simplex_minimizer, DownhillSimplexServer, CombEnumerator, SimpMath,
  DownhillSimplexAlgorithm,
  log, allocation_counter;
type
  TParabola = class(TComponent)
  public
    P: array[0..1] of double;
    Step: array[0..1] of double;
    Idx: longint;
    function GetFunc: double;
    procedure ComputeFunc;
    function GetVariationStep: double;
    procedure SetVariationStep(NewStep: double);
    procedure SetFirstParam;
    procedure SetNextParam;
    function GetParam: double;
    procedure SetParam(NewVal: double);
    function EndOfCycle: boolean;
    procedure ShowCurMin;
  end;
  { A host that answers like the application does: on every reported minimum it
    recomputes the goal function from the parameters it is holding AT THAT
    MOMENT, and keeps that beside the number the minimizer reported. }
  TReportingParabola = class(TParabola)
  public
    Reported: array of double;
    Recomputed: array of double;
    Minimizer: TObject;
    //  The minimum sits where a step in the FIRST parameter helps most, so the
    //  best vertex of the starting simplex is not the last one built.
    function GetFunc: double;
    procedure Record_;
  end;

  { A host with many parameters that counts how far the minimizer walks its
    parameter list. Stops the run itself after a fixed number of evaluations,
    so the count is per evaluation of a real Minimize and not of a contrived
    call sequence. }
  TCountingHost = class(TComponent)
  public
    P: array of double;
    Idx: longint;
    Steps: int64;
    Evaluations: longint;
    StopAfter: longint;
    Minimizer: TObject;
    function GetFunc: double;
    procedure ComputeFunc;
    function GetVariationStep: double;
    procedure SetVariationStep(NewStep: double);
    procedure SetFirstParam;
    procedure SetNextParam;
    function GetParam: double;
    procedure SetParam(NewVal: double);
    function EndOfCycle: boolean;
    procedure ShowCurMin;
  end;

  { A flat goal over any number of parameters that counts how often the
    simplex reads a parameter's starting step: once per parameter each time a
    simplex is built - at the start, and again at every restart. }
  TStepReadingHost = class(TComponent)
  public
    P: array of double;
    Idx: longint;
    StepReads: longint;
    function GetFunc: double;
    procedure ComputeFunc;
    function GetVariationStep: double;
    procedure SetVariationStep(NewStep: double);
    procedure SetFirstParam;
    procedure SetNextParam;
    function GetParam: double;
    procedure SetParam(NewVal: double);
    function EndOfCycle: boolean;
    procedure ShowCurMin;
  end;

  TMinimizerTest = class(TTestCase)
  private
    { Simplexes built per parameter in a fit of ACount parameters. }
    function BuildsPerParameter(ACount: longint): double;
  published
    procedure WritingEveryParameterWalksTheListOnceNotOncePerParameter;
    procedure ANewMinimumBuildsNoLogLineWhileTraceIsOff;
    procedure FindsParabolaMinimum;
    procedure EveryReportedMinimumIsTheStateTheHostIsIn;
    procedure AParameterIsAddressedByItsIndex;
    procedure TheMinimizerIsOneDiscreteValueThatCannotBeMoved;
    procedure SphericalAndCartesianAgreeBothWays;
    procedure ARestartIsAllowedWhateverTheParameterCount;
    procedure TheDirectionsRunOutOnlyForAFewParameters;
    procedure TheFirstSimplexStepsEveryParameterUp;
    procedure ARestartTurnsParametersBeyondTheFirstTwo;
    procedure TheSameRestartDrawsTheSameDirections;
    procedure AFitOfThirtyTwoParametersRestartsAsOneOfThirtyDoes;
  end;
implementation
function TParabola.GetFunc: double; begin Result := Sqr(P[0]-3) + Sqr(P[1]-5); end;
procedure TParabola.ComputeFunc; begin end;
function TParabola.GetVariationStep: double; begin Result := Step[Idx]; end;
procedure TParabola.SetVariationStep(NewStep: double); begin Step[Idx] := NewStep; end;
procedure TParabola.SetFirstParam; begin Idx := 0; end;
procedure TParabola.SetNextParam; begin Inc(Idx); end;
function TParabola.GetParam: double; begin Result := P[Idx]; end;
procedure TParabola.SetParam(NewVal: double); begin P[Idx] := NewVal; end;
function TParabola.EndOfCycle: boolean; begin Result := Idx >= 2; end;
procedure TParabola.ShowCurMin; begin end;
function TCountingHost.GetFunc: double;
var i: longint;
begin
  Result := 0;
  for i := 0 to High(P) do
    Result := Result + Sqr(P[i] - i);
end;
procedure TCountingHost.ComputeFunc;
begin
  Inc(Evaluations);
  if Evaluations = StopAfter then
    TDownhillSimplexMinimizer(Minimizer).Terminated := True;
end;
function TCountingHost.GetVariationStep: double; begin Result := 1.0; end;
procedure TCountingHost.SetVariationStep(NewStep: double); begin end;
procedure TCountingHost.SetFirstParam; begin Idx := 0; end;
procedure TCountingHost.SetNextParam; begin Inc(Idx); Inc(Steps); end;
function TCountingHost.GetParam: double; begin Result := P[Idx]; end;
procedure TCountingHost.SetParam(NewVal: double); begin P[Idx] := NewVal; end;
function TCountingHost.EndOfCycle: boolean; begin Result := Idx >= Length(P); end;
procedure TCountingHost.ShowCurMin; begin end;

function TReportingParabola.GetFunc: double;
begin
  Result := Sqr(P[0] - 3) + Sqr(P[1]);
end;

procedure TReportingParabola.Record_;
var n: longint;
begin
  n := Length(Reported);
  SetLength(Reported, n + 1);
  SetLength(Recomputed, n + 1);
  Reported[n] := TDownhillSimplexMinimizer(Minimizer).FCurrentMinimum;
  Recomputed[n] := GetFunc;
end;

{ WHAT THE HOST IS HOLDING WHEN IT IS TOLD OF A NEW MINIMUM, and it is the whole
  contract behind the live loss chart: the application does not keep the number
  it is handed - TFitTask.ShowCurMin RECOMPUTES the R-factor from the model as it
  stands, because a special algorithm may have moved the parameters. So a report
  made while the host holds some other trial point records a value that belongs
  to no accepted state, and the chart then shows a fit that jumped to a value it
  never reached (a dip of five decades, in the case that found this).

  The start of the search is where that happened: the simplex is built by
  evaluating every vertex in turn, so the host was left holding the LAST vertex
  while the best of them was reported. }
procedure TMinimizerTest.EveryReportedMinimumIsTheStateTheHostIsIn;
var M: TDownhillSimplexMinimizer; F: TReportingParabola;
    ErrorCode, i: longint;
begin
  F := TReportingParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    F.Minimizer := M;
    F.P[0]:=0; F.P[1]:=0; F.Step[0]:=1.0; F.Step[1]:=1.0; F.Idx:=0;
    M.OnGetFunc:=@F.GetFunc; M.OnComputeFunc:=@F.ComputeFunc;
    M.OnGetVariationStep:=@F.GetVariationStep; M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam; M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnGetParam:=@F.GetParam; M.OnSetParam:=@F.SetParam;
    M.OnEndOfCycle:=@F.EndOfCycle; M.OnShowCurMin:=@F.Record_;
    ErrorCode:=0; M.Minimize(ErrorCode);

    AssertTrue('the search reported at least one minimum', Length(F.Reported) > 0);
    for i := 0 to High(F.Reported) do
      AssertEquals(Format('report %d is the state the host is in', [i]),
        F.Reported[i], F.Recomputed[i], 1e-12);
  finally M.Free; F.Free; end;
end;

{ THE PARAMETERS ARE WRITTEN IN ONE PASS. Before each evaluation the simplex
  writes every parameter by index (DownhillSimplexServer.FillParameters), and
  this minimizer answered each indexed access by walking its host's parameter
  list from the start - twice, once to count the parameters and once to reach
  the index. Writing P parameters cost about 4.5 P^2 steps of the host's
  iterator, every evaluation: with the 174 parameters of a real wave-pattern
  model that was some 136,000 steps before a single curve was computed.

  Counted over a real Minimize, per evaluation, against a budget linear in P
  with a generous constant. The quadratic walk exceeds it many times over. }
procedure TMinimizerTest.WritingEveryParameterWalksTheListOnceNotOncePerParameter;
const
  N = 120;
var M: TDownhillSimplexMinimizer; H: TCountingHost; ErrorCode, i: longint;
begin
  H := TCountingHost.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    SetLength(H.P, N);
    for i := 0 to N - 1 do
      H.P[i] := 0;
    H.StopAfter := 400;
    H.Minimizer := M;
    M.OnGetFunc:=@H.GetFunc; M.OnComputeFunc:=@H.ComputeFunc;
    M.OnGetVariationStep:=@H.GetVariationStep; M.OnSetVariationStep:=@H.SetVariationStep;
    M.OnSetNextParam:=@H.SetNextParam; M.OnSetFirstParam:=@H.SetFirstParam;
    M.OnGetParam:=@H.GetParam; M.OnSetParam:=@H.SetParam;
    M.OnEndOfCycle:=@H.EndOfCycle; M.OnShowCurMin:=@H.ShowCurMin;
    ErrorCode := 0;
    M.Minimize(ErrorCode);
    AssertTrue('the run evaluated the goal function', H.Evaluations > 0);
    AssertTrue(Format('at most a few walks of the list per evaluation ' +
      '(%d steps over %d evaluations of %d parameters)',
      [H.Steps, H.Evaluations, N]),
      H.Steps <= int64(10) * N * H.Evaluations);
    //  And the fast path still writes where it is told: the first parameters
    //  moved towards their minima, the host holding what the simplex chose.
    AssertTrue('the search moved the parameters', H.GetFunc < N * (N - 1) * (2 * N - 1) / 6);
  finally M.Free; H.Free; end;
end;

{ A NEW MINIMUM IS REPORTED WITHOUT BUILDING A LINE NOBODY WRITES. Every
  improvement logged 'Current minimium = ' + FloatToStr(...) at Trace - and
  built that string first, whatever the tier, because Pascal evaluates the
  argument before WriteLog can look (fit-performance.md, stage 3). Hundreds of
  improvements per fit, each a float formatted and dropped. }
procedure TMinimizerTest.ANewMinimumBuildsNoLogLineWhileTraceIsOff;
var M: TDownhillSimplexMinimizer; F: TParabola; Saved: TMsgType;
    i, Allocated: longint;
begin
  F := TParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  Saved := GetLogLevel;
  try
    M.OnShowCurMin := @F.ShowCurMin;
    SetLogLevel(Debug);
    StartCountingAllocations;
    try
      for i := 1 to 20 do
        M.UpdateResults(nil);
    finally
      Allocated := StopCountingAllocations;
    end;
    AssertEquals('twenty reported minima built no log line', 0, Allocated);
  finally
    SetLogLevel(Saved);
    M.Free; F.Free;
  end;
end;

procedure TMinimizerTest.FindsParabolaMinimum;
var M: TDownhillSimplexMinimizer; F: TParabola; ErrorCode: longint;
begin
  F := TParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    F.P[0]:=0; F.P[1]:=0; F.Step[0]:=1.0; F.Step[1]:=1.0; F.Idx:=0;
    M.OnGetFunc:=@F.GetFunc; M.OnComputeFunc:=@F.ComputeFunc;
    M.OnGetVariationStep:=@F.GetVariationStep; M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam; M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnGetParam:=@F.GetParam; M.OnSetParam:=@F.SetParam;
    M.OnEndOfCycle:=@F.EndOfCycle; M.OnShowCurMin:=@F.ShowCurMin;
    ErrorCode:=0; M.Minimize(ErrorCode);
    AssertEquals('p0 -> 3', 3.0, F.P[0], 1e-3);
    AssertEquals('p1 -> 5', 5.0, F.P[1], 1e-3);
  finally M.Free; F.Free; end;
end;
{ ADDRESSING ONE PARAMETER BY INDEX, which is how the caller reads and writes a
  single variation step without knowing how the minimizer walks its parameters.

  The pair had never been called. It matters because the walk is stateful - the
  minimizer selects a parameter by stepping from the first until the index is
  reached - so a getter that left the cursor somewhere else would make the NEXT
  read answer about a different parameter. Reading index 1 and then index 0 is
  what catches that, and reading a step back after writing it is what catches a
  setter addressing its neighbour. }
procedure TMinimizerTest.AParameterIsAddressedByItsIndex;
var M: TDownhillSimplexMinimizer; F: TParabola;
    Params: IDownhillRealParameters;
begin
  F := TParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    F.Step[0]:=1.0; F.Step[1]:=2.0; F.Idx:=0;
    M.OnGetVariationStep:=@F.GetVariationStep;
    M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam;
    M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnEndOfCycle:=@F.EndOfCycle;

    //  THROUGH THE INTERFACE, because that is the whole surface: the optimiser
    //  reaches its host's parameters this way and no other, so the indexed
    //  property is the thing under test rather than a method on a class.
    Params := M;

    //  Read out of order, so a cursor left behind by the first read would show
    //  up in the second.
    AssertEquals('the second step', 2.0, Params.VariationStep[1], 1e-12);
    AssertEquals('the first step', 1.0, Params.VariationStep[0], 1e-12);

    //  Written by index, and read back by index.
    Params.VariationStep[1] := 7.5;
    AssertEquals('the second was written', 7.5, F.Step[1], 1e-12);
    AssertEquals('and the first was left alone', 1.0, F.Step[0], 1e-12);
    AssertEquals('read back', 7.5, Params.VariationStep[1], 1e-12);
  finally M.Free; F.Free; end;
end;

{ THE MINIMIZER AS A DISCRETE VALUE. The optimiser's enumerator drives every
  quantity it knows through IDiscretValue - "how many values have you, which is
  selected, select this one" - and this minimizer has exactly one, itself. So the
  answers are fixed: one value, index zero, and any attempt to select another is
  refused rather than quietly ignored.

  It reads like boilerplate and is not: an enumerator that believed there were two
  would walk a position that does not exist, and one that accepted a non-zero
  index would leave this object claiming to be something it is not. }
procedure TMinimizerTest.TheMinimizerIsOneDiscreteValueThatCannotBeMoved;
var M: TDownhillSimplexMinimizer; V: IDownhillRealParameters;
    Refused: boolean;
begin
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    V := M;
    AssertEquals('one value', 1, V.NumberOfValues);
    AssertEquals('and it is the one selected', 0, V.ValueIndex);
    //  Selecting the only value it has is fine.
    V.ValueIndex := 0;
    AssertEquals('still the same', 0, V.ValueIndex);
    Refused := False;
    try
      V.ValueIndex := 1;
    except
      on E: Exception do
        Refused := True;
    end;
    AssertTrue('selecting a value it does not have is refused', Refused);
  finally M.Free; end;
end;

{ THE COORDINATE CONVERSION the optimiser's geometry rests on, both ways and over
  the cases the formula treats separately: z = 0, where the polar angle cannot be
  derived from a ratio and is taken as a right angle; and x = 0, where the azimuth
  is a right angle whose SIGN comes from y. Those two branches are the whole of
  the function's difficulty, and neither had been executed. }
procedure TMinimizerTest.SphericalAndCartesianAgreeBothWays;
var Theta, Phi, R, X, Y, Z: double;
begin
  //  An ordinary point: out and back.
  ConvertDekartToSpherical(1, 2, 3, Theta, Phi, R);
  AssertEquals('the radius', Sqrt(1.0 + 4.0 + 9.0), R, 1e-12);
  ConvertSphericalToDekart(Theta, Phi, R, X, Y, Z);
  AssertEquals('x survives the round trip', 1.0, X, 1e-9);
  AssertEquals('y survives it', 2.0, Y, 1e-9);
  AssertEquals('z survives it', 3.0, Z, 1e-9);

  //  z = 0: in the equatorial plane, which the formula answers directly rather
  //  than by a ratio it cannot form.
  ConvertDekartToSpherical(1, 0, 0, Theta, Phi, R);
  AssertEquals('a right angle from the pole', pi / 2, Theta, 1e-12);
  AssertEquals('and no azimuth', 0.0, Phi, 1e-12);

  //  x = 0: the azimuth is a right angle, and its SIGN is y's. Getting that
  //  wrong reflects the point through the origin.
  ConvertDekartToSpherical(0, 2, 0, Theta, Phi, R);
  AssertEquals('positive y looks one way', pi / 2, Phi, 1e-12);
  ConvertDekartToSpherical(0, -2, 0, Theta, Phi, R);
  AssertEquals('negative y the other', -pi / 2, Phi, 1e-12);
end;

{ THE RESTART GUARD WRAPPED. It was FRestartCount < (1 shl P) - 1 on a 32-bit
  integer, where x86 shifts by P mod 32: at 32, 64, 96... parameters the bound
  came out 0 and the simplex never restarted, at 33, 65... it restarted once -
  so whether a model got the restarts the fit is configured for (MaxRestarts,
  3 in this application) depended on how many parameters it had. Two wave
  patterns in one interval have about that many. }
procedure TMinimizerTest.ARestartIsAllowedWhateverTheParameterCount;
var
  P, Count: longint;
begin
  for P := 2 to 100 do
    for Count := 0 to 2 do
      AssertTrue(Format('%d parameters, restart %d', [P, Count]),
        DirectionPatternsLeft(Count, P));
end;

{ ...while the bound it was written for still holds where it can: one
  parameter has one other direction, two have three. }
{ THE FIRST SIMPLEX STEPS EVERY PARAMETER UP, as it always has: a fit that never
  restarts is the fit it was. }
procedure TMinimizerTest.TheFirstSimplexStepsEveryParameterUp;
var
  D: TStepDirections;
  i: longint;
begin
  D := SimplexStepDirections(0, 40);
  for i := 0 to High(D) do
    AssertEquals(Format('parameter %d', [i]), 1, D[i]);
end;

{ A RESTART TURNS ANY PARAMETER, AT RANDOM. The directions were bit i of the
  restart counter, so with the three restarts this application allows only the
  first two parameters were ever turned, whatever the model: a restart explored
  two directions of a forty-parameter space. Agreed in the fit-performance plan
  as a defect to fix in the simplex (findings.md, "How each simplex is
  started"). }
procedure TMinimizerTest.ARestartTurnsParametersBeyondTheFirstTwo;
var
  D: TStepDirections;
  r, i, Turned: longint;
begin
  Turned := 0;
  for r := 1 to 3 do
  begin
    D := SimplexStepDirections(r, 40);
    for i := 2 to High(D) do
      if D[i] = -1 then
        Inc(Turned);
  end;
  AssertTrue(Format('%d steps beyond the first two parameters turned', [Turned]),
    Turned > 20);
end;

{ AND THE SAME RUN DRAWS THE SAME: fit intervals run side by side, so the
  process-wide Random would make no fit reproducible. }
procedure TMinimizerTest.TheSameRestartDrawsTheSameDirections;
var
  A, B, C: TStepDirections;
  i, Differ: longint;
begin
  A := SimplexStepDirections(2, 40);
  B := SimplexStepDirections(2, 40);
  C := SimplexStepDirections(3, 40);
  Differ := 0;
  for i := 0 to 39 do
  begin
    AssertEquals('the same restart, the same draw', A[i], B[i]);
    if A[i] <> C[i] then
      Inc(Differ);
  end;
  AssertTrue('another restart, another draw', Differ > 0);
end;

procedure TMinimizerTest.TheDirectionsRunOutOnlyForAFewParameters;
begin
  AssertTrue(DirectionPatternsLeft(0, 1));
  AssertFalse(DirectionPatternsLeft(1, 1));
  AssertTrue(DirectionPatternsLeft(2, 2));
  AssertFalse(DirectionPatternsLeft(3, 2));
  AssertTrue('more than a 32-bit integer can count',
    DirectionPatternsLeft(MaxInt - 1, 40));
end;

function TStepReadingHost.GetFunc: double;
begin
  //  FLAT: every vertex of every simplex scores the same, so the spread is
  //  zero at once and the run goes straight to the restart check - which is
  //  then the only thing deciding how many simplexes are built. On a smooth
  //  goal the stagnation rule usually ends the run before that check is met.
  Result := 1;
end;
procedure TStepReadingHost.ComputeFunc; begin end;
function TStepReadingHost.GetVariationStep: double;
begin
  Inc(StepReads);
  Result := 1.0;
end;
procedure TStepReadingHost.SetVariationStep(NewStep: double); begin end;
procedure TStepReadingHost.SetFirstParam; begin Idx := 0; end;
procedure TStepReadingHost.SetNextParam; begin Inc(Idx); end;
function TStepReadingHost.GetParam: double; begin Result := P[Idx]; end;
procedure TStepReadingHost.SetParam(NewVal: double); begin P[Idx] := NewVal; end;
function TStepReadingHost.EndOfCycle: boolean; begin Result := Idx >= Length(P); end;
procedure TStepReadingHost.ShowCurMin; begin end;

function TMinimizerTest.BuildsPerParameter(ACount: longint): double;
var M: TDownhillSimplexMinimizer; F: TStepReadingHost; ErrorCode: longint;
begin
  F := TStepReadingHost.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    SetLength(F.P, ACount);
    M.OnGetFunc:=@F.GetFunc; M.OnComputeFunc:=@F.ComputeFunc;
    M.OnGetVariationStep:=@F.GetVariationStep; M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam; M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnGetParam:=@F.GetParam; M.OnSetParam:=@F.SetParam;
    M.OnEndOfCycle:=@F.EndOfCycle; M.OnShowCurMin:=@F.ShowCurMin;
    ErrorCode:=0; M.Minimize(ErrorCode);
    Result := F.StepReads / ACount;
  finally M.Free; F.Free; end;
end;

{ THE SAME FIT, THE SAME RESTARTS, whatever the parameter count: through the
  adapter the application uses, a 32-parameter bowl is restarted as often as
  a 30-parameter one. Before the guard was fixed it was built once - its
  restart bound wrapped to 0 - while the 30-parameter one was restarted the
  three times this application allows (MaxRestarts). }
procedure TMinimizerTest.AFitOfThirtyTwoParametersRestartsAsOneOfThirtyDoes;
var Thirty, ThirtyTwo: double;
begin
  Thirty := BuildsPerParameter(30);
  ThirtyTwo := BuildsPerParameter(32);
  AssertTrue(Format('30 parameters restarted (%.2f builds)', [Thirty]),
    Thirty > 1.5);
  AssertEquals('32 parameters, as many builds', Thirty, ThirtyTwo, 0.5);
end;

initialization RegisterTest('unit', TMinimizerTest);
end.
