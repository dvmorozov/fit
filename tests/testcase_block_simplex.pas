// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The block-wise Downhill Simplex, on hosts with no fit in sight.)

A NEW KIND BESIDE THE SIMPLEX, never instead of it (fit-performance.md, stage
9a). It runs the unchanged TDownhillSimplexMinimizer over one block of the
host's parameters at a time - one curve's, or the parameters every curve shares
- widest first, and sweeps the blocks again until a whole sweep stops improving
the goal. What it promises is tested here on plain functions: it reaches the
minimum, a block's run writes only that block, the widest goes first, sweeps
repeat while they pay, and Stop ends the run.
}
unit testcase_block_simplex;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, block_simplex_minimizer;

type
    { A host of AParams parameters in blocks, with the goal a test chooses. }
    TBlockHost = class(TComponent)
    public
        P, Step, Target: array of double;
        Block: array of longint;
        Width: array of double;
        Idx: longint;
        Coupled: boolean;
        { A goal no move of one block can improve from the origin. }
        Locked: boolean;
        { A valley: |sum of every parameter - 4|. Every point of a plane is
          optimal, so where a fit settles on it is decided by the way its
          simplexes stepped. }
        Valley: boolean;
        Evaluations, StopAfter, Reports: longint;
        Minimizer: TBlockDownhillSimplexMinimizer;
        { The block of every parameter written, in order. }
        Written: array of longint;
        procedure Setup(const ABlocks: array of longint;
            const AWidths: array of double; const ATargets: array of double);
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
        function ParamBlock: longint;
        function BlockWidth(ABlock: longint): double;
    end;

    TBlockSimplexTest = class(TTestCase)
    private
        FHost: TBlockHost;
        FMin: TBlockDownhillSimplexMinimizer;
        procedure Wire;
        procedure Run;
        function Transitions: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ItReachesTheMinimumOfASeparableGoal;
        procedure ItReachesTheMinimumWhenTheBlocksAreCoupled;
        procedure ABlocksRunWritesOnlyThatBlock;
        procedure TheWidestBlockIsFittedFirst;
        procedure SweepsRepeatOnlyWhileTheyImprove;
        procedure EveryImprovementIsReported;
        procedure StopEndsTheRun;
        procedure AJointPassFinishesWhatNoBlockCan;
        procedure OneBlockIsOneSimplexNotASweep;
    end;

    { The restarted simplex (stage 9c): every parameter one block, swept. }
    TRestartedSimplexTest = class(TTestCase)
    private
        FHost: TBlockHost;
        FMin: TRestartedDownhillSimplexMinimizer;
        procedure Run;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ItReachesTheMinimum;
        procedure EveryRunMovesEveryParameter;
        procedure ItStartsAgainWhileARunStillImproves;
        procedure ItStopsWhenARunNoLongerDoes;
        procedure StopEndsTheRun;
        //  Which way each parameter's first step goes, run by run.
        procedure TheFirstRunStepsEveryParameterForward;
        procedure ALaterRunTurnsParametersBeyondTheFirstTwo;
        procedure EachLaterRunTurnsOtherParameters;
        procedure TheSameSeedTurnsTheSameParameters;
        procedure TheDirectionsReachTheSimplex;
    end;

implementation

{ ------------------------------------------------------------- TBlockHost }

procedure TBlockHost.Setup(const ABlocks: array of longint;
    const AWidths: array of double; const ATargets: array of double);
var
    i: longint;
begin
    SetLength(P, Length(ABlocks));
    SetLength(Step, Length(ABlocks));
    SetLength(Target, Length(ABlocks));
    SetLength(Block, Length(ABlocks));
    for i := 0 to High(ABlocks) do
    begin
        P[i] := 0;
        Step[i] := 1;
        Target[i] := ATargets[i];
        Block[i] := ABlocks[i];
    end;
    SetLength(Width, Length(AWidths));
    for i := 0 to High(AWidths) do
        Width[i] := AWidths[i];
end;

function TBlockHost.GetFunc: double;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(P) do
        Result := Result + Sqr(P[i] - Target[i]);
    //  COUPLED: the first parameter of each block also has to match the
    //  first of the next, so fitting one block moves the optimum of another
    //  and one sweep cannot be enough.
    //  LOCKED: max(|x-1|, |y-1|) + |x-y|. From (0, 0), moving x alone or y
    //  alone never goes below 1; moving both reaches 0 at (1, 1).
    if Locked then
        Exit(Max(Abs(P[0] - 1), Abs(P[1] - 1)) + Abs(P[0] - P[1]));
    if Valley then
    begin
        Result := -4;
        for i := 0 to High(P) do
            Result := Result + P[i];
        Exit(Abs(Result));
    end;
    if Coupled then
        Result := Result + 4 * Sqr(P[0] - P[High(P)] - (Target[0] - Target[High(P)]))
            + 2 * Sqr(P[0] + P[High(P)] - (Target[0] + Target[High(P)]));
end;

procedure TBlockHost.ComputeFunc;
begin
    Inc(Evaluations);
    if (StopAfter > 0) and (Evaluations = StopAfter) then
        Minimizer.Terminated := True;
end;

function TBlockHost.GetVariationStep: double;
begin
    Result := Step[Idx];
end;

procedure TBlockHost.SetVariationStep(NewStep: double);
begin
    Step[Idx] := NewStep;
end;

procedure TBlockHost.SetFirstParam;
begin
    Idx := 0;
end;

procedure TBlockHost.SetNextParam;
begin
    Inc(Idx);
end;

function TBlockHost.GetParam: double;
begin
    Result := P[Idx];
end;

procedure TBlockHost.SetParam(NewVal: double);
begin
    P[Idx] := NewVal;
    //  The closing pass over everything writes every block by design.
    if Minimizer.InJointPass then
        Exit;
    SetLength(Written, Length(Written) + 1);
    Written[High(Written)] := Block[Idx];
end;

function TBlockHost.EndOfCycle: boolean;
begin
    Result := Idx > High(P);
end;

procedure TBlockHost.ShowCurMin;
begin
    Inc(Reports);
end;

function TBlockHost.ParamBlock: longint;
begin
    Result := Block[Idx];
end;

function TBlockHost.BlockWidth(ABlock: longint): double;
begin
    if ABlock < 0 then
        Result := Infinity
    else
        Result := Width[ABlock];
end;

{ ------------------------------------------------------ TBlockSimplexTest }

procedure TBlockSimplexTest.SetUp;
begin
    FHost := TBlockHost.Create(nil);
    FMin := TBlockDownhillSimplexMinimizer.Create(nil);
    FHost.Minimizer := FMin;
end;

procedure TBlockSimplexTest.TearDown;
begin
    FreeAndNil(FMin);
    FreeAndNil(FHost);
end;

procedure TBlockSimplexTest.Wire;
begin
    FMin.OnGetFunc := @FHost.GetFunc;
    FMin.OnComputeFunc := @FHost.ComputeFunc;
    FMin.OnGetVariationStep := @FHost.GetVariationStep;
    FMin.OnSetVariationStep := @FHost.SetVariationStep;
    FMin.OnSetFirstParam := @FHost.SetFirstParam;
    FMin.OnSetNextParam := @FHost.SetNextParam;
    FMin.OnGetParam := @FHost.GetParam;
    FMin.OnSetParam := @FHost.SetParam;
    FMin.OnEndOfCycle := @FHost.EndOfCycle;
    FMin.OnShowCurMin := @FHost.ShowCurMin;
    FMin.OnGetParamBlock := @FHost.ParamBlock;
    FMin.OnGetBlockWidth := @FHost.BlockWidth;
end;

procedure TBlockSimplexTest.Run;
var
    ErrorCode: longint;
begin
    Wire;
    FMin.Minimize(ErrorCode);
    AssertEquals('the run started', 0, ErrorCode);
end;

function TBlockSimplexTest.Transitions: longint;
var
    i: longint;
begin
    Result := 0;
    for i := 1 to High(FHost.Written) do
        if FHost.Written[i] <> FHost.Written[i - 1] then
            Inc(Result);
end;

procedure TBlockSimplexTest.ItReachesTheMinimumOfASeparableGoal;
var
    i: longint;
begin
    FHost.Setup([0, 0, 1, 1, 2, 2], [10, 20, 30], [3, -2, 7, 1, -4, 5]);
    Run;
    for i := 0 to High(FHost.P) do
        AssertEquals(Format('parameter %d', [i]), FHost.Target[i],
            FHost.P[i], 1e-3);
end;

procedure TBlockSimplexTest.ItReachesTheMinimumWhenTheBlocksAreCoupled;
var
    i: longint;
begin
    FHost.Setup([0, 0, 1, 1], [10, 20], [3, -2, 7, 1]);
    FHost.Coupled := True;
    Run;
    for i := 0 to High(FHost.P) do
        AssertEquals(Format('parameter %d', [i]), FHost.Target[i],
            FHost.P[i], 1e-3);
end;

procedure TBlockSimplexTest.ABlocksRunWritesOnlyThatBlock;
begin
    //  Interleaved in the host's order, so a run over "the next few
    //  parameters" would mix them.
    FHost.Setup([0, 1, 0, 1, 2, 2], [10, 20, 30], [3, -2, 7, 1, -4, 5]);
    Run;
    AssertTrue('it swept', FMin.Sweeps > 0);
    //  Each block's run writes only that block, so the writes change block
    //  at most once per block per sweep.
    AssertTrue(Format('%d changes of block in %d sweeps of 3 blocks',
        [Transitions, FMin.Sweeps]), Transitions < FMin.Sweeps * 3);
end;

procedure TBlockSimplexTest.TheWidestBlockIsFittedFirst;
begin
    FHost.Setup([0, 0, 1, 1, 2, 2], [10, 30, 20], [3, -2, 7, 1, -4, 5]);
    Run;
    AssertEquals('the widest block first', 1, FHost.Written[0]);
end;

procedure TBlockSimplexTest.SweepsRepeatOnlyWhileTheyImprove;
var
    Separable: longint;
begin
    //  Separable: the first sweep finds every block's optimum, the second
    //  finds nothing to gain, and that is the end.
    FHost.Setup([0, 0, 1, 1], [10, 20], [3, -2, 7, 1]);
    Run;
    Separable := FMin.Sweeps;
    AssertTrue(Format('a separable goal took %d sweeps', [Separable]),
        Separable <= 2);
    //  Coupled: fitting one block moves the other's optimum, so it takes more.
    FreeAndNil(FMin);
    FreeAndNil(FHost);
    SetUp;
    FHost.Setup([0, 0, 1, 1], [10, 20], [3, -2, 7, 1]);
    FHost.Coupled := True;
    Run;
    AssertTrue(Format('a coupled goal took %d sweeps, a separable one %d',
        [FMin.Sweeps, Separable]), FMin.Sweeps > Separable);
end;

procedure TBlockSimplexTest.EveryImprovementIsReported;
begin
    FHost.Setup([0, 0, 1, 1], [10, 20], [3, -2, 7, 1]);
    Run;
    AssertTrue('improvements were reported', FHost.Reports > 0);
end;

procedure TBlockSimplexTest.StopEndsTheRun;
const
    STOP_AT = 50;
begin
    FHost.Setup([0, 0, 1, 1, 2, 2], [10, 20, 30], [3, -2, 7, 1, -4, 5]);
    FHost.Coupled := True;
    FHost.StopAfter := STOP_AT;
    Run;
    //  The block's simplex ends the cycle it is in, and no further block or
    //  sweep starts.
    AssertTrue(Format('%d evaluations after a stop at %d',
        [FHost.Evaluations, STOP_AT]), FHost.Evaluations < STOP_AT + 20);
end;

{ COORDINATE MOVES CAN STALL where a joint move does not - on a goal coupled
  through something no block owns, as curve scaling couples every curve's
  amplitude through one factor. So the run ends with one simplex over every
  parameter, from where the blocks left it. }
procedure TBlockSimplexTest.AJointPassFinishesWhatNoBlockCan;
begin
    FHost.Setup([0, 1], [10, 20], [0, 0]);
    FHost.Locked := True;
    Run;
    AssertEquals('the goal', 0, FHost.GetFunc, 1e-3);
end;

{ ONE BLOCK IS THE SIMPLEX OVER EVERYTHING: the host's curves all shape
  everything, so this kind must give exactly what the simplex gives - one run,
  never a sweep that restarts it from its own result. }
procedure TBlockSimplexTest.OneBlockIsOneSimplexNotASweep;
begin
    FHost.Setup([0, 0, 0], [10], [3, -2, 7]);
    FHost.Coupled := True;
    Run;
    AssertEquals('one run', 1, FMin.Sweeps);
end;

{ ------------------------------------------------- TRestartedSimplexTest }

procedure TRestartedSimplexTest.SetUp;
begin
    FHost := TBlockHost.Create(nil);
    FMin := TRestartedDownhillSimplexMinimizer.Create(nil);
    FHost.Minimizer := FMin;
end;

procedure TRestartedSimplexTest.TearDown;
begin
    FreeAndNil(FMin);
    FreeAndNil(FHost);
end;

procedure TRestartedSimplexTest.Run;
var
    ErrorCode: longint;
begin
    FMin.OnGetFunc := @FHost.GetFunc;
    FMin.OnComputeFunc := @FHost.ComputeFunc;
    FMin.OnGetVariationStep := @FHost.GetVariationStep;
    FMin.OnSetVariationStep := @FHost.SetVariationStep;
    FMin.OnSetFirstParam := @FHost.SetFirstParam;
    FMin.OnSetNextParam := @FHost.SetNextParam;
    FMin.OnGetParam := @FHost.GetParam;
    FMin.OnSetParam := @FHost.SetParam;
    FMin.OnEndOfCycle := @FHost.EndOfCycle;
    FMin.OnShowCurMin := @FHost.ShowCurMin;
    //  Blocks the host offers are not asked for: it is one simplex.
    FMin.OnGetParamBlock := @FHost.ParamBlock;
    FMin.OnGetBlockWidth := @FHost.BlockWidth;
    ErrorCode := -1;
    FMin.Minimize(ErrorCode);
    AssertEquals('the run started', 0, ErrorCode);
end;

procedure TRestartedSimplexTest.ItReachesTheMinimum;
var
    i: longint;
begin
    FHost.Setup([0, 0, 1, 1], [10, 20], [3, -2, 7, 1]);
    FHost.Coupled := True;
    Run;
    for i := 0 to High(FHost.P) do
        AssertEquals(Format('parameter %d', [i]), FHost.Target[i],
            FHost.P[i], 1e-3);
end;

procedure TRestartedSimplexTest.EveryRunMovesEveryParameter;
begin
    //  The host offers three blocks; a restarted simplex ignores them, so
    //  its writes change block far more often than three times a run.
    FHost.Setup([0, 1, 2, 0, 1, 2], [10, 20, 30], [3, -2, 7, 1, -4, 5]);
    Run;
    AssertTrue(Format('%d runs', [FMin.Sweeps]), FMin.Sweeps >= 1);
    AssertTrue('the blocks were not fitted apart',
        Length(FHost.Written) > 0);
    AssertTrue('one simplex over everything',
        FHost.Written[0] <> FHost.Written[1]);
end;

procedure TRestartedSimplexTest.ItStartsAgainWhileARunStillImproves;
begin
    //  The locked goal is not smooth: one simplex stops on a ridge, and a
    //  fresh one from there still finds more.
    FHost.Setup([0, 1], [10, 20], [0, 0]);
    FHost.Locked := True;
    Run;
    AssertTrue(Format('%d runs', [FMin.Sweeps]), FMin.Sweeps > 1);
end;

procedure TRestartedSimplexTest.ItStopsWhenARunNoLongerDoes;
begin
    //  A quadratic bowl: the first run finds its bottom, the second gains
    //  nothing, and that is the end.
    FHost.Setup([0, 0], [10], [3, -2]);
    Run;
    AssertTrue(Format('%d runs', [FMin.Sweeps]), FMin.Sweeps <= 2);
end;

procedure TRestartedSimplexTest.StopEndsTheRun;
const
    STOP_AT = 50;
begin
    FHost.Setup([0, 1], [10, 20], [0, 0]);
    FHost.Locked := True;
    FHost.StopAfter := STOP_AT;
    Run;
    AssertTrue(Format('%d evaluations after a stop at %d',
        [FHost.Evaluations, STOP_AT]), FHost.Evaluations < STOP_AT + 20);
end;

{ ----------------------------------- the directions of a restarted run }

function Turned(const D: TStepDirections): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(D) do
        if D[i] < 0 then
            Inc(Result);
end;

{ THE FIRST RUN IS THE SIMPLEX, as it is: every step forward, so the restarted
  kind can never end above the simplex it starts as. }
procedure TRestartedSimplexTest.TheFirstRunStepsEveryParameterForward;
begin
    AssertEquals(0, Turned(RestartDirections(1, 0, 40)));
    AssertEquals('as many as asked for', 40, Length(RestartDirections(1, 0, 40)));
end;

{ WHAT THE COUNTER-BIT SCHEME COULD NOT DO: with three restarts it only ever
  turned the first two parameters. A random direction per parameter reaches
  every one. }
procedure TRestartedSimplexTest.ALaterRunTurnsParametersBeyondTheFirstTwo;
var
    D: TStepDirections;
    i, Beyond: longint;
begin
    D := RestartDirections(1, 1, 40);
    Beyond := 0;
    for i := 2 to High(D) do
        if D[i] < 0 then
            Inc(Beyond);
    AssertTrue(Format('%d of 38 turned', [Beyond]), Beyond > 5);
    AssertTrue('and not all of them', Turned(D) < 35);
end;

procedure TRestartedSimplexTest.EachLaterRunTurnsOtherParameters;
var
    A, B: TStepDirections;
    i, Differ: longint;
begin
    A := RestartDirections(1, 1, 40);
    B := RestartDirections(1, 2, 40);
    Differ := 0;
    for i := 0 to 39 do
        if A[i] <> B[i] then
            Inc(Differ);
    AssertTrue(Format('%d of 40 differ', [Differ]), Differ > 5);
end;

{ REPRODUCIBLE: the same seed, the same run, the same directions - a fit is
  the same fit the next time, with no global random state behind it. }
procedure TRestartedSimplexTest.TheSameSeedTurnsTheSameParameters;
var
    A, B: TStepDirections;
    i: longint;
begin
    A := RestartDirections(7, 3, 40);
    B := RestartDirections(7, 3, 40);
    for i := 0 to 39 do
        AssertEquals(Format('parameter %d', [i]), A[i], B[i]);
end;

{ THE DIRECTIONS REACH THE SIMPLEX: the later runs build their simplexes
  along them, so two seeds walk different paths - a different number of
  evaluations - and one seed always the same. A view that dropped the
  directions would walk one path whatever the seed. On the valley the first run
  reaches the floor and the later ones cannot improve on it, so it is the path
  that tells, not where the fit ends. }
procedure TRestartedSimplexTest.TheDirectionsReachTheSimplex;

    function PathWithSeed(ASeed: longword): longint;
    begin
        FreeAndNil(FMin);
        FreeAndNil(FHost);
        SetUp;
        FHost.Setup([0, 1, 2, 3], [10, 20, 30, 40], [0, 0, 0, 0]);
        FHost.Valley := True;
        FMin.DirectionSeed := ASeed;
        Run;
        AssertTrue('a later run was made', FMin.Sweeps > 1);
        Result := FHost.Evaluations;
    end;

var
    A, B, C: longint;
begin
    A := PathWithSeed(1);
    B := PathWithSeed(1);
    C := PathWithSeed(2);
    AssertEquals('one seed, one path', A, B);
    AssertTrue(Format('two seeds, two paths (%d, %d evaluations)', [A, C]),
        A <> C);
end;

initialization
    RegisterTest('unit', TBlockSimplexTest);
    RegisterTest('unit', TRestartedSimplexTest);
end.
