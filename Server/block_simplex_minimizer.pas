// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Downhill Simplex over one block of parameters at a time.)

A KIND BESIDE THE SIMPLEX, NEVER INSTEAD OF IT (docs/internal/fit-performance.md,
stage 9a). The Downhill Simplex over every parameter of a task gives the best
results this application has, and is not touched: this runs THAT minimizer,
unchanged, over one block of the host's parameters at a time - one curve's, or
the parameters the curves share - and sweeps the blocks again until a whole sweep
stops improving the goal.

WHY BLOCKS. A simplex over P parameters has P + 1 vertices, and each of its
steps moves all of them; the cost of a fit grows much faster than P. A model of
many curves of which each shapes only its own stretch is nearly separable, and a
simplex over one curve's dozen parameters converges in a fraction of the steps.
What it gives up is the joint move: two curves that trade off against each
other are reconciled by the sweeps, not inside one simplex - which is why this is
a separate kind to be compared on real tasks, not a replacement.

HOW, and why each choice:

  * THE HOST SAYS WHICH BLOCK a parameter is in (OnGetParamBlock), and how
    wide each block is (OnGetBlockWidth); the minimizer knows nothing of curves.
    A host that cannot tell answers one block for everything, and this is then
    the Downhill Simplex with one extra measurement.
  * WIDEST FIRST, by the extent of what a block shapes: a curve that spans
    others is placed before what sits on it, without the framework knowing what
    a parent is. Equal widths keep the host's order, so a run is reproducible.
  * THROUGH A VIEW of the host's own callbacks. The inner simplex walks "its"
    parameters with the same first/next/end-of-cycle protocol, and the view
    steps the host's iterator to the block's members - forward only, so a sweep
    over a block costs one walk of the host's list.
  * SWEEPING ENDS when a sweep gains less than SWEEP_REL_GAIN of the goal it
    started from, or less than SWEEP_ABS_GAIN of the goal the run started from
    - the second for a goal driven to near zero, where every relative gain is
    large. MAX_SWEEPS is a backstop, not a working limit.
  * THEN ONE SIMPLEX OVER EVERYTHING, from where the blocks left it. Moves of
    one block at a time can stall where a joint move does not, on a goal
    coupled through something no block owns - curve scaling couples every
    curve's amplitude through one factor, and on three separate Gaussians the
    sweeps alone stopped at twice the simplex's R-factor. The sweeps are what
    bring the model close cheaply; the joint pass is what the simplex over
    everything would have done, from a much better start.
  * STOP reaches the block running, under a lock (Stop arrives on another
    thread), and no further block or sweep starts.
}
unit block_simplex_minimizer;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, int_minimizer, downhill_simplex_minimizer;

type
    { The block the host's current parameter belongs to. }
    TGetParamBlock = function: longint of object;
    { How wide a block is - the extent of what its parameters shape. Infinity
      for one that shapes everything. }
    TGetBlockWidth = function(ABlock: longint): double of object;

    { The direction of each parameter's first step in a simplex: 1 forward,
      -1 backward. }
    TStepDirections = array of shortint;

    TBlockDownhillSimplexMinimizer = class(TMinimizer)
    private
        FOnGetParamBlock: TGetParamBlock;
        FOnGetBlockWidth: TGetBlockWidth;
        FSweeps: longint;
        { The blocks in the order they are fitted, each as the host's indices
          of its parameters, ascending. }
        FBlocks: array of array of longint;
        { The block being fitted, and where the view and the host stand. }
        FView: array of longint;
        FViewPos: longint;
        FHostPos: longint;
        { The simplex running a block, published for Stop. }
        FInner: TDownhillSimplexMinimizer;
        FInnerLock: TRTLCriticalSection;
        FStopBelow: double;
        FInJointPass: boolean;
        { The direction of each parameter's first step in the run in progress,
          by the host's index; empty means every step forward. }
        FDirections: TStepDirections;
        FParamCount: longint;

        procedure CollectBlocks;
        function Goal: double;
        { Runs the simplex over the host's parameters AIndices. }
        procedure RunView(const AIndices: array of longint);
        procedure MoveHostTo(AIndex: longint);

        { The view the inner simplex is given. }
        procedure ViewSetFirstParam;
        procedure ViewSetNextParam;
        function ViewEndOfCycle: boolean;
        function ViewGetParam: double;
        procedure ViewSetParam(AValue: double);
        function ViewGetVariationStep: double;
        procedure ViewSetVariationStep(AValue: double);
        function ViewGetFunc: double;
        procedure ViewComputeFunc;
        procedure ViewShowCurMin;

    protected
        procedure SetTerminated(ATerminated: boolean); override;
        { The block of the host's current parameter. }
        function BlockOfCurrent: longint; virtual;
        { Whether one block is swept like many - started again from its own
          best until a run stops paying - rather than run once. }
        function SweepsOneBlock: boolean; virtual;
        { Called before run ARun (0 the first) of the sweep: sets the
          directions the simplexes of that run start out in. Every step
          forward, here - the simplex as it is. }
        procedure BeginRun(ARun: longint); virtual;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Minimize(var ErrorCode: longint); override;
        { As TDownhillSimplexMinimizer's: every block's simplex converges
          loosely, and the run ends once the goal is below AStopBelow. }
        procedure UseDecompositionConvergence(AStopBelow: double);
        { How many sweeps the last run made. }
        property Sweeps: longint read FSweeps;
        { True while the closing simplex over every parameter runs. }
        property InJointPass: boolean read FInJointPass;
        property OnGetParamBlock: TGetParamBlock
            read FOnGetParamBlock write FOnGetParamBlock;
        property OnGetBlockWidth: TGetBlockWidth
            read FOnGetBlockWidth write FOnGetBlockWidth;
    end;

    { THE SIMPLEX OVER EVERYTHING, STARTED AGAIN from its own best until a run
      gains less than SWEEP_REL_GAIN (docs/internal/fit-performance.md, stage
      9c). One block, swept.

      WHY. The simplex stops when its vertices agree, and a fresh simplex built
      around that point with the starting steps often still finds more: on the
      module's wave-pattern fits about half the loss again, in a few times the
      time.

      IN NEW DIRECTIONS EACH TIME (RestartDirections). A run here after the
      first turns each parameter at random, from a generator of its own seeded
      by DirectionSeed and the run, so every parameter is looked at from both
      sides and a fit is the same fit every time. The first run steps every
      parameter forward: it is the simplex as it is, and this kind never ends
      above it. What sets this kind apart is WHEN it restarts - after every run
      that still gained, however wide the simplex - where the ordinary simplex
      restarts only once its vertices agree to 1e-9, which its stagnation
      window usually pre-empts. Since 2026-10-06 the ordinary simplex's own
      restarts draw random directions too (fitminimizers'
      SimplexStepDirections); before, they took bit i of a restart counter and
      only ever turned the first two parameters.

      Measured (the module's golden fits, 2026-10-04): about the loss the
      counter directions reached, in roughly 60 % of the time. }
    TRestartedDownhillSimplexMinimizer = class(TBlockDownhillSimplexMinimizer)
    private
        FDirectionSeed: longword;
    public
        constructor Create(AOwner: TComponent); override;
        { What the directions of every run after the first are drawn from. A
          fixed default, so a fit is the same fit every time. }
        property DirectionSeed: longword read FDirectionSeed write FDirectionSeed;
    protected
        procedure BeginRun(ARun: longint); override;
        function BlockOfCurrent: longint; override;
        function SweepsOneBlock: boolean; override;
    end;

{ Which way each of ACount parameters takes its first step in run ARun of a
  restarted simplex. }
function RestartDirections(ASeed: longword; ARun, ACount: longint): TStepDirections;

const
    { A sweep that gains less than this share of the goal it started from has
      found what sweeping cheaply can: the joint pass finishes from there.
      Measured on the module's golden fit, whose blocks are a pattern and the
      one nested on it: sweeps gain well under a percent each for dozens of
      sweeps (coupled blocks zig-zag), and swept to a millionth the fit took
      28 s where it takes 4 s this way, to the same R-factor. }
    SWEEP_REL_GAIN = 1e-2;
    { ...or less than this share of the goal the run started from. }
    SWEEP_ABS_GAIN = 1e-9;
    MAX_SWEEPS = 1000;

implementation

constructor TBlockDownhillSimplexMinimizer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    InitCriticalSection(FInnerLock);
    FHostPos := -1;
end;

destructor TBlockDownhillSimplexMinimizer.Destroy;
begin
    DoneCriticalSection(FInnerLock);
    inherited Destroy;
end;

procedure TBlockDownhillSimplexMinimizer.UseDecompositionConvergence(
    AStopBelow: double);
begin
    FStopBelow := AStopBelow;
end;

procedure TBlockDownhillSimplexMinimizer.SetTerminated(ATerminated: boolean);
begin
    inherited SetTerminated(ATerminated);
    if not ATerminated then
        Exit;
    EnterCriticalSection(FInnerLock);
    try
        if Assigned(FInner) then
            FInner.Terminated := True;
    finally
        LeaveCriticalSection(FInnerLock);
    end;
end;

function RestartDirections(ASeed: longword; ARun, ACount: longint): TStepDirections;
var
    i: longint;
    State: longword;
begin
    SetLength(Result, ACount);
    //  THE FIRST RUN IS THE SIMPLEX AS IT IS: every step forward.
    if ARun = 0 then
    begin
        for i := 0 to ACount - 1 do
            Result[i] := 1;
        Exit;
    end;
    //  A generator of its own - xorshift32 - not the RTL's Random: that is one
    //  state for the whole process, which fit intervals fitted side by side
    //  would share, so no run could be reproduced. Seeded from the seed and the
    //  run, so every run draws anew and the same run draws the same.
    {$PUSH}{$Q-}{$R-}
    State := (ASeed xor $9E3779B9) * 2654435761 + longword(ARun) * 40503;
    if State = 0 then
        State := $6D2B79F5;
    for i := 0 to ACount - 1 do
    begin
        State := State xor (State shl 13);
        State := State xor (State shr 17);
        State := State xor (State shl 5);
        if (State shr 31) <> 0 then
            Result[i] := -1
        else
            Result[i] := 1;
    end;
    {$POP}
end;

procedure TBlockDownhillSimplexMinimizer.BeginRun(ARun: longint);
begin
    SetLength(FDirections, 0);
end;

constructor TRestartedDownhillSimplexMinimizer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FDirectionSeed := 20261004;
end;

procedure TRestartedDownhillSimplexMinimizer.BeginRun(ARun: longint);
begin
    FDirections := RestartDirections(FDirectionSeed, ARun, FParamCount);
end;

function TBlockDownhillSimplexMinimizer.BlockOfCurrent: longint;
begin
    if Assigned(FOnGetParamBlock) then
        Result := FOnGetParamBlock()
    else
        Result := 0;
end;

function TBlockDownhillSimplexMinimizer.SweepsOneBlock: boolean;
begin
    Result := False;
end;

function TRestartedDownhillSimplexMinimizer.BlockOfCurrent: longint;
begin
    Result := 0;
end;

function TRestartedDownhillSimplexMinimizer.SweepsOneBlock: boolean;
begin
    Result := True;
end;

procedure TBlockDownhillSimplexMinimizer.CollectBlocks;
var
    Ids: array of longint;
    Widths: array of double;
    Id, i, j, k, Count: longint;
    TId: longint;
    TWidth: double;
    TMembers: array of longint;
begin
    SetLength(FBlocks, 0);
    SetLength(Ids, 0);
    Count := 0;
    OnSetFirstParam;
    while not OnEndOfCycle() do
    begin
        Id := BlockOfCurrent;
        //  The block, in order of first appearance.
        j := 0;
        while (j <= High(Ids)) and (Ids[j] <> Id) do
            Inc(j);
        if j > High(Ids) then
        begin
            SetLength(Ids, j + 1);
            Ids[j] := Id;
            SetLength(FBlocks, j + 1);
        end;
        SetLength(FBlocks[j], Length(FBlocks[j]) + 1);
        FBlocks[j][High(FBlocks[j])] := Count;
        Inc(Count);
        OnSetNextParam;
    end;
    //  The walk left the host past the end.
    FHostPos := -1;

    SetLength(Widths, Length(Ids));
    for j := 0 to High(Ids) do
        if Assigned(FOnGetBlockWidth) then
            Widths[j] := FOnGetBlockWidth(Ids[j])
        else
            Widths[j] := 0;
    //  WIDEST FIRST, stable: an insertion sort that moves a block only past
    //  narrower ones.
    for i := 1 to High(Ids) do
    begin
        TId := Ids[i];
        TWidth := Widths[i];
        TMembers := FBlocks[i];
        k := i - 1;
        while (k >= 0) and (Widths[k] < TWidth) do
        begin
            Ids[k + 1] := Ids[k];
            Widths[k + 1] := Widths[k];
            FBlocks[k + 1] := FBlocks[k];
            Dec(k);
        end;
        Ids[k + 1] := TId;
        Widths[k + 1] := TWidth;
        FBlocks[k + 1] := TMembers;
    end;
end;

function TBlockDownhillSimplexMinimizer.Goal: double;
begin
    OnComputeFunc;
    Result := OnGetFunc();
end;

procedure TBlockDownhillSimplexMinimizer.Minimize(var ErrorCode: longint);
var
    Start, Before, After: double;
    b, i, Count: longint;
    All: array of longint;
begin
    FSweeps := 0;
    FInJointPass := False;
    //  Every step forward until a run says otherwise - including the one-block
    //  path below, which runs no sweep and must not inherit a last run's.
    SetLength(FDirections, 0);
    ErrorCode := IsReady;
    if ErrorCode <> MIN_NO_ERRORS then
        Exit;
    //  NOT reset: a stop published before the run began is a stop.
    if Terminated then
        Exit;

    CollectBlocks;
    if Length(FBlocks) = 0 then
        Exit;
    //  ONE BLOCK IS THE SIMPLEX OVER EVERYTHING, once - not swept: running it
    //  again from its own result is a restart, which is a different algorithm
    //  (stage 9c), and a model whose curves all shape everything must get from
    //  this kind exactly what the simplex gives it.
    if (Length(FBlocks) = 1) and not SweepsOneBlock then
    begin
        RunView(FBlocks[0]);
        FSweeps := 1;
        Exit;
    end;

    Start := Goal;
    Before := Start;
    FParamCount := 0;
    for b := 0 to High(FBlocks) do
        Inc(FParamCount, Length(FBlocks[b]));
    repeat
        BeginRun(FSweeps);
        //  A block after a stop returns at once (RunView).
        for b := 0 to High(FBlocks) do
            RunView(FBlocks[b]);
        Inc(FSweeps);
        if Terminated then
            Break;
        After := Goal;
        if (FStopBelow > 0) and (After < FStopBelow) then
            Break;
        if (Before - After <= SWEEP_REL_GAIN * Abs(Before)) or
           (Before - After <= SWEEP_ABS_GAIN * Abs(Start)) then
            Break;
        Before := After;
    until FSweeps >= MAX_SWEEPS;

    //  One block is already the simplex over everything.
    if Terminated or (Length(FBlocks) < 2) then
        Exit;
    Count := 0;
    for b := 0 to High(FBlocks) do
        Inc(Count, Length(FBlocks[b]));
    SetLength(All, Count);
    for i := 0 to Count - 1 do
        All[i] := i;
    FInJointPass := True;
    try
        RunView(All);
    finally
        FInJointPass := False;
    end;
end;

procedure TBlockDownhillSimplexMinimizer.RunView(
    const AIndices: array of longint);
var
    Inner: TDownhillSimplexMinimizer;
    Code, i: longint;
begin
    SetLength(FView, Length(AIndices));
    for i := 0 to High(AIndices) do
        FView[i] := AIndices[i];
    FViewPos := 0;
    FHostPos := -1;

    Inner := TDownhillSimplexMinimizer.Create(nil);
    try
        Inner.OnGetFunc := @ViewGetFunc;
        Inner.OnComputeFunc := @ViewComputeFunc;
        Inner.OnGetVariationStep := @ViewGetVariationStep;
        Inner.OnSetVariationStep := @ViewSetVariationStep;
        Inner.OnSetFirstParam := @ViewSetFirstParam;
        Inner.OnSetNextParam := @ViewSetNextParam;
        Inner.OnGetParam := @ViewGetParam;
        Inner.OnSetParam := @ViewSetParam;
        Inner.OnEndOfCycle := @ViewEndOfCycle;
        Inner.OnShowCurMin := @ViewShowCurMin;
        if FStopBelow > 0 then
            Inner.UseDecompositionConvergence(FStopBelow);

        //  Published fully built, and told of a stop already in - as
        //  TFitTask.CreateNativeMinimizer publishes its own.
        EnterCriticalSection(FInnerLock);
        try
            FInner := Inner;
        finally
            LeaveCriticalSection(FInnerLock);
        end;
        if Terminated then
            Exit;
        Inner.Minimize(Code);
    finally
        EnterCriticalSection(FInnerLock);
        try
            FInner := nil;
        finally
            LeaveCriticalSection(FInnerLock);
        end;
        Inner.Free;
        FHostPos := -1;
    end;
end;

procedure TBlockDownhillSimplexMinimizer.MoveHostTo(AIndex: longint);
begin
    //  FORWARD ONLY, except to go back to the start: the host can be walked
    //  no other way.
    if (FHostPos < 0) or (AIndex < FHostPos) then
    begin
        OnSetFirstParam;
        FHostPos := 0;
    end;
    while FHostPos < AIndex do
    begin
        OnSetNextParam;
        Inc(FHostPos);
    end;
end;

procedure TBlockDownhillSimplexMinimizer.ViewSetFirstParam;
begin
    FViewPos := 0;
    if Length(FView) > 0 then
        MoveHostTo(FView[0]);
end;

procedure TBlockDownhillSimplexMinimizer.ViewSetNextParam;
begin
    Inc(FViewPos);
    if FViewPos <= High(FView) then
        MoveHostTo(FView[FViewPos]);
end;

function TBlockDownhillSimplexMinimizer.ViewEndOfCycle: boolean;
begin
    Result := FViewPos > High(FView);
end;

function TBlockDownhillSimplexMinimizer.ViewGetParam: double;
begin
    Result := OnGetParam();
end;

procedure TBlockDownhillSimplexMinimizer.ViewSetParam(AValue: double);
begin
    OnSetParam(AValue);
end;

function TBlockDownhillSimplexMinimizer.ViewGetVariationStep: double;
begin
    Result := OnGetVariationStep();
    //  BACKWARD BY A NEGATIVE STEP: the simplex builds vertex i as the start
    //  plus parameter i's step, so the sign handed over here is the direction.
    if FView[FViewPos] <= High(FDirections) then
        Result := Result * FDirections[FView[FViewPos]];
end;

procedure TBlockDownhillSimplexMinimizer.ViewSetVariationStep(AValue: double);
begin
    //  The host keeps its step as it gave it: the direction is this run's.
    if FView[FViewPos] <= High(FDirections) then
        AValue := AValue * FDirections[FView[FViewPos]];
    OnSetVariationStep(AValue);
end;

function TBlockDownhillSimplexMinimizer.ViewGetFunc: double;
begin
    Result := OnGetFunc();
end;

procedure TBlockDownhillSimplexMinimizer.ViewComputeFunc;
begin
    OnComputeFunc;
end;

procedure TBlockDownhillSimplexMinimizer.ViewShowCurMin;
begin
    //  The block's best is the goal's best: every block minimises the one
    //  goal the host computes.
    if Assigned(FInner) then
        FCurrentMinimum := FInner.FCurrentMinimum;
    if Assigned(OnShowCurMin) then
        OnShowCurMin;
end;

end.
