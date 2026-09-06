// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The downhill simplex itself, driven through its own server interface.)

WHAT THIS REACHES THAT THE FIT TESTS DO NOT. Every fit in this program goes
through TDownhillSimplexAlgorithm, and the suite drove it through one test that
minimises a parabola. That exercises the happy path and nothing else: not the
restart machinery, not the stopping rules, not the counters the engine reports,
and not the simulated-annealing variant at all - which is a second algorithm
class, complete with its own decision type, that had never been instantiated.

THE STOPPING RULES ARE WHERE THE INTERESTING FAILURES ARE, and the unit's own
comments record two of them at length: a tolerance test that reads a large
constant term in the goal function as convergence and stops a fit after two
cycles, and a stagnation window that cut a diffraction fit off after twelve. Both
are settings, both are checked here against functions chosen to provoke them.

IT DRIVES THE ALGORITHM, NOT A FIT. The goal function is arithmetic over an array
of doubles - no profile, no curve, no service - so this is a unit test by the
project's rule, in the same way testcase_minimizer already is.
}
unit testcase_simplex;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    Algorithm, DownhillSimplexAlgorithm, Decisions;

type
    { A goal function over N parameters, as the algorithm's server.

      IT COUNTS ITS OWN CALLS, because how hard the algorithm worked is half of
      what is being asserted - an algorithm that finds the answer by evaluating
      the function ten thousand times has not solved the problem the engine has. }
    TQuadraticServer = class(TComponent, IDownhillSimplexServer)
    private
        FCentre: array of double;
        FStart: array of double;
        FStep: double;
        FConstant: double;
        FEvaluations: longint;
        FStopAfter: longint;
        FBest: double;
        FHasBest: boolean;
    public
        constructor Create(AOwner: TComponent; const ACentre,
            AStart: array of double); reintroduce;

        //  IDownhillSimplexServer
        function GetVariationStep(Sender: TComponent; index: LongInt): Double;
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision);
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision);
        procedure UpdateResults(Sender: TComponent; Decision: TFloatDecision);
        function EndOfCalculation(Sender: TComponent): Boolean;

        { Added to every evaluation. The unit's own comment says a large one
          defeats the relative-tolerance test; this is how that is provoked. }
        property Constant: double read FConstant write FConstant;
        { The initial step handed back for every parameter. }
        property Step: double read FStep write FStep;
        { Stop unconditionally after this many evaluations; 0 for no limit. }
        property StopAfter: longint read FStopAfter write FStopAfter;
        property Evaluations: longint read FEvaluations;
        { The best value seen, and whether anything was reported at all. }
        property Best: double read FBest;
        property HasBest: boolean read FHasBest;
    end;

    TSimplexTest = class(TTestCase)
    private
        FServer: TQuadraticServer;
        FAlgorithm: TDownhillSimplexAlgorithm;
        { A minimiser over the given centre, started from AStart. }
        procedure Given(const ACentre, AStart: array of double);
        procedure GivenAnnealing(const ACentre, AStart: array of double);
        procedure Run;
        { How far the best decision ended from the true centre. }
        function DistanceFromCentre: double;
    protected
        procedure TearDown; override;
    published
        //  That it minimises at all.
        procedure ItFindsTheMinimumOfAParabola;
        procedure ItFindsAMinimumOffTheAxes;
        procedure ItWorksInMoreThanTwoDimensions;
        procedure ItReportsTheBestDecisionItFound;

        //  What it reports about its own work.
        procedure ItCountsItsCycles;
        procedure ItCountsItsEvaluations;
        procedure EveryCycleCostsSeveralEvaluations;

        //  When it stops.
        procedure ACycleLimitIsRespected;
        procedure ARestartLimitIsRespected;
        procedure RestartsCanBeDisabledEntirely;
        procedure TheServerCanStopItAtAnyTime;

        //  The stopping rule the unit's own comment warns about.
        procedure ALargeConstantTermDefeatsTheToleranceTest;
        procedure TheStagnationWindowStopsAnUnproductiveSearch;
        procedure ALargeConstantStopsTheSearchBeforeItStarts;
        procedure AZeroStagnationLimitDisablesThatTest;

        //  The initial simplex.
        procedure TheStartingPointIsWhereTheServerSaysItIs;
        procedure TheVariationStepIsAskedOfTheServer;

        //  The simulated-annealing variant.
        procedure TheAnnealingVariantAlsoMinimises;
        procedure TheAnnealingVariantAtZeroTemperatureIsTheOrdinaryOne;
        procedure AnAnnealingDecisionCopiesItsFluctuation;
    end;

implementation

constructor TQuadraticServer.Create(AOwner: TComponent;
    const ACentre, AStart: array of double);
var
    i: longint;
begin
    inherited Create(AOwner);
    SetLength(FCentre, Length(ACentre));
    SetLength(FStart, Length(AStart));
    for i := 0 to High(ACentre) do
        FCentre[i] := ACentre[i];
    for i := 0 to High(AStart) do
        FStart[i] := AStart[i];
    FStep := 1.0;
    FBest := MaxDouble;
end;

function TQuadraticServer.GetVariationStep(Sender: TComponent;
    index: LongInt): Double;
begin
    Result := FStep;
end;

procedure TQuadraticServer.FillStartDecision(Sender: TComponent;
    StartDecision: TFloatDecision);
var
    i: longint;
begin
    StartDecision.ParametersNumber := Length(FStart);
    for i := 0 to High(FStart) do
        StartDecision.Parameters[i] := FStart[i];
end;

procedure TQuadraticServer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
var
    i: longint;
    Sum: double;
begin
    Inc(FEvaluations);
    Sum := FConstant;
    for i := 0 to High(FCentre) do
        Sum := Sum + Sqr(Decision.Parameters[i] - FCentre[i]);
    Decision.Evaluation := Sum;
end;

procedure TQuadraticServer.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    FHasBest := True;
    if Decision.Evaluation < FBest then
        FBest := Decision.Evaluation;
end;

function TQuadraticServer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := (FStopAfter > 0) and (FEvaluations >= FStopAfter);
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TSimplexTest.TearDown;
begin
    FreeAndNil(FAlgorithm);
    FreeAndNil(FServer);
end;

procedure TSimplexTest.Given(const ACentre, AStart: array of double);
begin
    FServer := TQuadraticServer.Create(nil, ACentre, AStart);
    //  A tolerance tight enough to be worth asserting against, restarts
    //  allowed, and no exit-derivative test.
    FAlgorithm := TDownhillSimplexAlgorithm.Create(nil, 1E-8, False, 0);
    FAlgorithm.DownhillSimplexServer := FServer;
    //  A ceiling on both, so a test that provokes a stopping rule cannot run
    //  away if the rule fails to fire.
    FAlgorithm.MaxCycles := 20000;
    FAlgorithm.MaxRestarts := 20;
end;

procedure TSimplexTest.GivenAnnealing(const ACentre, AStart: array of double);
begin
    FServer := TQuadraticServer.Create(nil, ACentre, AStart);
    FAlgorithm := TDownhillSimplexSAAlgorithm.Create(nil, 1E-8, False, 0);
    FAlgorithm.DownhillSimplexServer := FServer;
    FAlgorithm.MaxCycles := 20000;
    FAlgorithm.MaxRestarts := 20;
end;

procedure TSimplexTest.Run;
begin
    FAlgorithm.AlgorithmRealization;
end;

function TSimplexTest.DistanceFromCentre: double;
begin
    //  The server's best REPORTED evaluation, less the constant, is the squared
    //  distance - the goal function is exactly that by construction.
    Result := Sqrt(Max(0, FServer.Best - FServer.Constant));
end;

{ ---- that it minimises at all ---------------------------------------------- }

procedure TSimplexTest.ItFindsTheMinimumOfAParabola;
begin
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    AssertTrue('it got close: ' + FloatToStr(DistanceFromCentre),
        DistanceFromCentre < 1E-3);
end;

procedure TSimplexTest.ItFindsAMinimumOffTheAxes;
begin
    //  A minimum at negative and fractional coordinates, so a search that only
    //  steps in one direction, or that assumes a positive answer, fails.
    Given([-2.5, 7.25], [10.0, -10.0]);
    Run;
    AssertTrue('it got close: ' + FloatToStr(DistanceFromCentre),
        DistanceFromCentre < 1E-3);
end;

procedure TSimplexTest.ItWorksInMoreThanTwoDimensions;
begin
    //  A real model has one parameter per curve per shape parameter, so five is
    //  a small fit rather than a large one. The simplex is N+1 vertices, and
    //  the bookkeeping that keeps them straight is what this exercises.
    Given([1.0, 2.0, 3.0, 4.0, 5.0], [0.0, 0.0, 0.0, 0.0, 0.0]);
    Run;
    AssertTrue('it got close: ' + FloatToStr(DistanceFromCentre),
        DistanceFromCentre < 1E-2);
end;

procedure TSimplexTest.ItReportsTheBestDecisionItFound;
begin
    //  THROUGH UpdateResults, which is how the engine learns the answer - an
    //  algorithm that converged and never reported would leave the fit with its
    //  starting parameters and no error.
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    AssertTrue('something was reported', FServer.HasBest);
end;

{ ---- what it reports about its own work ------------------------------------ }

procedure TSimplexTest.ItCountsItsCycles;
begin
    //  Shown to the user and used by the stopping rules. A counter that never
    //  advanced would make MaxCycles unreachable.
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    AssertTrue('it ran cycles', FAlgorithm.CycleCount > 0);
end;

procedure TSimplexTest.ItCountsItsEvaluations;
begin
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    AssertTrue('it evaluated', FAlgorithm.EvaluationCount > 0);
    AssertEquals('and the server agrees', FServer.Evaluations,
        FAlgorithm.EvaluationCount);
end;

procedure TSimplexTest.EveryCycleCostsSeveralEvaluations;
begin
    //  A simplex cycle reflects, and may then extend or contract - so the two
    //  counters cannot be equal. Equal counters would mean the cycle is not
    //  doing what a simplex cycle does.
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    AssertTrue('more evaluations than cycles',
        FAlgorithm.EvaluationCount > FAlgorithm.CycleCount);
end;

{ ---- when it stops --------------------------------------------------------- }

procedure TSimplexTest.ACycleLimitIsRespected;
begin
    //  THE BACKSTOP. Every other stopping rule is a judgement about progress;
    //  this one is the promise that a fit ends. A fit that does not end is the
    //  application hung with a progress bar.
    Given([3.0, 5.0], [1000.0, -1000.0]);
    FAlgorithm.MaxCycles := 5;
    Run;
    AssertTrue(Format('stopped at the limit (%d cycles)',
        [FAlgorithm.CycleCount]), FAlgorithm.CycleCount <= 5);
end;

procedure TSimplexTest.ARestartLimitIsRespected;
begin
    //  Restarting is what gets a simplex out of a local minimum, and it is
    //  unbounded without this - each restart converges and asks for another.
    Given([3.0, 5.0], [0.0, 0.0]);
    FAlgorithm.MaxRestarts := 2;
    Run;
    AssertTrue(Format('at most two restarts (%d)', [FAlgorithm.RestartCount]),
        FAlgorithm.RestartCount <= 2);
end;

procedure TSimplexTest.RestartsCanBeDisabledEntirely;
begin
    //  What the engine does for a refit: the previous answer is the starting
    //  point and starting again from elsewhere would throw it away.
    FServer := TQuadraticServer.Create(nil, [3.0, 5.0], [0.0, 0.0]);
    FAlgorithm := TDownhillSimplexAlgorithm.Create(nil, 1E-8, True, 0);
    FAlgorithm.DownhillSimplexServer := FServer;
    FAlgorithm.MaxCycles := 20000;
    Run;
    AssertEquals('no restarts at all', 0, FAlgorithm.RestartCount);
    AssertTrue('and it still converged', DistanceFromCentre < 1E-2);
end;

procedure TSimplexTest.TheServerCanStopItAtAnyTime;
begin
    //  How the user's Stop reaches a running fit: the server answers
    //  EndOfCalculation and the algorithm puts down what it has. A rule that
    //  only checked between cycles would leave Stop unresponsive for as long as
    //  one cycle takes, which on a large model is a long time.
    Given([3.0, 5.0], [1000.0, -1000.0]);
    FServer.StopAfter := 20;
    Run;
    AssertTrue(Format('it stopped early (%d evaluations)',
        [FServer.Evaluations]), FServer.Evaluations < 200);
end;

{ ---- the stopping rule the unit's own comment warns about ------------------ }

procedure TSimplexTest.ALargeConstantTermDefeatsTheToleranceTest;
var
    WithConstant, Plain: double;
begin
    //  THE DEFECT THE UNIT DOCUMENTS AT LENGTH. The tolerance test compares the
    //  spread of the goal function's values against its MAGNITUDE, so a large
    //  constant term makes the useful variation a tiny fraction of the value
    //  and convergence is declared while the fit has barely started.
    //
    //  Asserted as it behaves: with a large constant the search stops further
    //  from the answer than without one, everything else being equal.
    Given([3.0, 5.0], [0.0, 0.0]);
    Run;
    Plain := DistanceFromCentre;
    FreeAndNil(FAlgorithm);
    FreeAndNil(FServer);

    Given([3.0, 5.0], [0.0, 0.0]);
    FServer.Constant := 1E9;
    Run;
    WithConstant := DistanceFromCentre;

    AssertTrue(Format('a constant term costs accuracy (%g against %g)',
        [WithConstant, Plain]), WithConstant > Plain);
end;

procedure TSimplexTest.TheStagnationWindowStopsAnUnproductiveSearch;
begin
    //  The rule added to answer the above: stop when the best decision has
    //  gained less than a fraction of what the fit has already gained, over a
    //  window of passes. Driven at the settings the engine itself uses
    //  (downhill_simplex_minimizer: 1e-6 over a window of 12), because those
    //  are the ones a real fit meets.
    Given([3.0, 5.0], [0.0, 0.0]);
    FAlgorithm.MinRelImprovement := 1E-6;
    FAlgorithm.StagnationLimit := 12;
    Run;
    AssertTrue('it ran', FAlgorithm.CycleCount > 0);
    AssertTrue(Format('and did not run to the ceiling (%d cycles)',
        [FAlgorithm.CycleCount]), FAlgorithm.CycleCount < 20000);
    AssertTrue('and it still converged: ' + FloatToStr(DistanceFromCentre),
        DistanceFromCentre < 1E-2);
end;

procedure TSimplexTest.ALargeConstantStopsTheSearchBeforeItStarts;
begin
    //  THE DEFECT, at its worst. With a constant term nine orders above the
    //  variation, the tolerance test declares convergence before a single cycle
    //  runs - not "after two cycles with eleven of twelve parameters at their
    //  initial values", as the unit's comment records, but after none at all.
    //
    //  The stagnation window does not rescue this shape: it measures whether
    //  the best decision is IMPROVING, and a search that never started has
    //  nothing to compare. Pinned as it behaves so that the limit of that rule
    //  is written down beside it. See findings.md.
    Given([3.0, 5.0], [0.0, 0.0]);
    FServer.Constant := 1E9;
    FAlgorithm.MinRelImprovement := 1E-6;
    FAlgorithm.StagnationLimit := 12;
    Run;
    AssertEquals('it never ran a cycle', 0, FAlgorithm.CycleCount);
    AssertTrue('and the answer is the starting point',
        DistanceFromCentre > 1.0);
end;

procedure TSimplexTest.AZeroStagnationLimitDisablesThatTest;
begin
    //  So that callers written before the rule existed are unaffected - which
    //  is what makes it safe to have added at all.
    Given([3.0, 5.0], [0.0, 0.0]);
    FAlgorithm.StagnationLimit := 0;
    Run;
    AssertTrue('it still converged', DistanceFromCentre < 1E-2);
end;

{ ---- the initial simplex --------------------------------------------------- }

procedure TSimplexTest.TheStartingPointIsWhereTheServerSaysItIs;
begin
    //  The engine seeds every parameter from the data before the fit runs, and
    //  a simplex built from somewhere else throws that away - which is the
    //  difference between a fit that converges and one that wanders.
    Given([3.0, 5.0], [3.0, 5.0]);
    FServer.StopAfter := 1;
    Run;
    AssertTrue('the first evaluation was at the start',
        Abs(FServer.Best - FServer.Constant) < 1E-12);
end;

procedure TSimplexTest.TheVariationStepIsAskedOfTheServer;
var
    Small, Large: longint;
begin
    //  The step is the size of the initial simplex, and the server sets it per
    //  parameter because a width and a position are not measured in the same
    //  units. A step the algorithm chose for itself would be right for one of
    //  them at best.
    Given([3.0, 5.0], [0.0, 0.0]);
    FServer.Step := 0.01;
    Run;
    Small := FAlgorithm.EvaluationCount;
    FreeAndNil(FAlgorithm);
    FreeAndNil(FServer);

    Given([3.0, 5.0], [0.0, 0.0]);
    FServer.Step := 100.0;
    Run;
    Large := FAlgorithm.EvaluationCount;

    AssertTrue(Format('the step changes the search (%d against %d)',
        [Small, Large]), Small <> Large);
end;

{ ---- the simulated-annealing variant --------------------------------------- }

procedure TSimplexTest.TheAnnealingVariantAlsoMinimises;
begin
    //  A SECOND ALGORITHM CLASS, with its own decision type, its own way of
    //  ranking the vertices and its own trial step - and it had never been
    //  instantiated by anything. It is offered to the engine as an alternative
    //  minimiser, so a build that selected it would have been running code
    //  nothing had ever executed.
    GivenAnnealing([3.0, 5.0], [0.0, 0.0]);
    TDownhillSimplexSAAlgorithm(FAlgorithm).Temperature := 0.1;
    Run;
    AssertTrue('it reported something', FServer.HasBest);
    AssertTrue('and it evaluated', FAlgorithm.EvaluationCount > 0);
end;

procedure TSimplexTest.TheAnnealingVariantAtZeroTemperatureIsTheOrdinaryOne;
begin
    //  With no temperature there is no fluctuation, so it must behave as the
    //  plain simplex does - which is what makes the temperature a dial rather
    //  than a different algorithm.
    GivenAnnealing([3.0, 5.0], [0.0, 0.0]);
    TDownhillSimplexSAAlgorithm(FAlgorithm).Temperature := 0;
    Run;
    AssertTrue('it converged: ' + FloatToStr(DistanceFromCentre),
        DistanceFromCentre < 1E-2);
end;

procedure TSimplexTest.AnAnnealingDecisionCopiesItsFluctuation;
var
    A, B: TDownhillSimplexSADecision;
begin
    //  The fluctuated evaluation is what the annealing variant ranks by, so a
    //  copy that lost it would rank the copy by zero and always prefer it.
    A := TDownhillSimplexSADecision.Create(nil);
    B := nil;
    try
        A.ParametersNumber := 2;
        A.Parameters[0] := 1.5;
        A.Parameters[1] := 2.5;
        A.Evaluation := 7.0;
        A.FluctEvaluation := 6.25;
        B := TDownhillSimplexSADecision(A.GetCopy);
        AssertTrue('a copy was made', Assigned(B));
        AssertEquals('the parameters', 1.5, B.Parameters[0], 1E-12);
        AssertEquals('the evaluation', 7.0, B.Evaluation, 1E-12);
        AssertEquals('and the fluctuated one', 6.25, B.FluctEvaluation, 1E-12);
    finally
        B.Free;
        A.Free;
    end;
end;

initialization
    //  A unit test: the goal function is arithmetic over an array of doubles.
    //  No profile, no curve, no service - the same standing as
    //  testcase_minimizer, which drives the same algorithm one level up.
    RegisterTest('unit', TSimplexTest);
end.
