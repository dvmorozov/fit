// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Native in-process compute backend.)

The default, zero-dependency backend (decision D4): it runs the app's own
Downhill Simplex engine in-process by driving the task's optimization. This is
the adapter that Stage 2's later slices turn into a bundled worker process and
sit a Python sidecar beside - all behind the same IFitBackend contract.
}
unit native_fit_backend;

{$MODE Delphi}

interface

uses
    int_fit_backend, fit_task, int_minimizer;

type
    { IFitBackend backed by the native in-process engine. }
    TNativeFitBackend = class(TInterfacedObject, IFitBackend)
    private
        FFactory: TNativeMinimizerFactory;
        FName: string;
    public
        { The Downhill Simplex. }
        constructor Create; overload;
        { Another native algorithm: AFactory makes its minimizer. }
        constructor Create(AFactory: TNativeMinimizerFactory;
            const AName: string); overload;
        function Name: string;
        function Fit(ATask: TFitTask): TFitResult;
    end;

{ The block-wise Downhill Simplex (block_simplex_minimizer), wired to the task's
  blocks: one per curve, and one for what the curves share. }
function NewBlockDownhillSimplex(ATask: TFitTask; ADecomposing: boolean;
    AStopBelow: double): TMinimizer;

{ The Downhill Simplex over every parameter, started again from its own best
  while that pays (block_simplex_minimizer). }
function NewRestartedDownhillSimplex(ATask: TFitTask; ADecomposing: boolean;
    AStopBelow: double): TMinimizer;

implementation

uses
    block_simplex_minimizer;

function NewBlockDownhillSimplex(ATask: TFitTask; ADecomposing: boolean;
    AStopBelow: double): TMinimizer;
var
    Created: TBlockDownhillSimplexMinimizer;
begin
    Created := TBlockDownhillSimplexMinimizer.Create(nil);
    Created.OnGetParamBlock := ATask.ParamBlock;
    Created.OnGetBlockWidth := ATask.BlockWidth;
    if ADecomposing then
        Created.UseDecompositionConvergence(AStopBelow);
    Result := Created;
end;

function NewRestartedDownhillSimplex(ATask: TFitTask; ADecomposing: boolean;
    AStopBelow: double): TMinimizer;
var
    Created: TRestartedDownhillSimplexMinimizer;
begin
    Created := TRestartedDownhillSimplexMinimizer.Create(nil);
    if ADecomposing then
        Created.UseDecompositionConvergence(AStopBelow);
    Result := Created;
end;

constructor TNativeFitBackend.Create;
begin
    inherited Create;
    FName := 'Native (Downhill Simplex)';
end;

constructor TNativeFitBackend.Create(AFactory: TNativeMinimizerFactory;
    const AName: string);
begin
    inherited Create;
    FFactory := AFactory;
    FName := AName;
end;

function TNativeFitBackend.Name: string;
begin
    Result := FName;
end;

function TNativeFitBackend.Fit(ATask: TFitTask): TFitResult;
begin
    ATask.RunNativeOptimization(FFactory);
    Result.ErrorCode := 0;
    Result.RFactor   := ATask.GetCurMin;
end;

end.
