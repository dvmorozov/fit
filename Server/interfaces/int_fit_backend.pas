// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Transport-pluggable compute-backend contract for fitting.)

The backend is the coarse adapter seam through which a fit is executed
(decisions D3 / D7 / D12): one call performs one whole fit, so the same
contract works for the in-process native engine, a separate native worker
process, or a Python sidecar reached over a transport - the caller does not
know or care which.

Today the only implementation is the native in-process engine (TNativeFitBackend
in native_fit_backend). When the engine moves out of process (Stage 2 slices 2-3)
the request/result below grows into an array/JSON-marshalled form; the seam and
its callers stay the same.
}
unit int_fit_backend;

{$MODE Delphi}

interface

uses
    fit_task;

type
    { Outcome of one fit. Kept deliberately small for now; Stage 2 slice 4 extends
      it with parameter uncertainties, weighted reduced chi-squared, R^2 and
      AIC/BIC once the optimizer backends can return them. }
    TFitResult = record
        { Optimizer error code (MIN_* constants); 0 on success. }
        ErrorCode: longint;
        { R-factor of the fitted model. }
        RFactor:   double;
    end;

    { A compute backend: performs one fit end to end. Implementations differ only
      in where/how the numerics run (in-process | worker process | sidecar). }
    IFitBackend = interface
        ['{6B3C9E2A-1D4F-4A7B-9C0E-2F5A8D6B1C34}']
        { Human-readable backend name (for the UI / logs). }
        function Name: string;
        { Runs the optimization for ATask to completion and returns the outcome. }
        function Fit(ATask: TFitTask): TFitResult;
    end;

implementation

end.
