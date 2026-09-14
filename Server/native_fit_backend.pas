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
    int_fit_backend, fit_task;

type
    { IFitBackend backed by the native in-process engine. }
    TNativeFitBackend = class(TInterfacedObject, IFitBackend)
    public
        function Name: string;
        function Fit(ATask: TFitTask): TFitResult;
    end;

implementation

function TNativeFitBackend.Name: string;
begin
    Result := 'Native (Downhill Simplex)';
end;

function TNativeFitBackend.Fit(ATask: TFitTask): TFitResult;
begin
    ATask.RunNativeOptimization;
    Result.ErrorCode := 0;
    Result.RFactor   := ATask.GetCurMin;
end;

end.
