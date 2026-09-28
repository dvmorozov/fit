// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which fitting engines this build offers - declarations, no engine.)

The half of the engine registration the desktop client needs: names and what
each engine needs, for the Minimizer menu and its greying. It names no backend,
so it links no fitting engine (non-negotiable 1); minimizer_registration, on the
server, declares through here and then binds each kind to its backend.

The default engine declares FIRST, because registration order is menu order and
the fallback.
}
unit minimizer_declarations;

{$mode objfpc}{$H+}

interface

{ Declares every engine this build ships. Idempotent - called by the client, the
  compute server and every fit. The client and the server must offer the same
  set, or a fit would be accepted and then run by something else, which is why
  both call this one procedure. }
procedure DeclareAllMinimizers;

implementation

uses
    int_fit_service, minimizer_registry;

procedure DeclareAllMinimizers;
var
    Info: TMinimizerInfo;
begin
    if IsKnownMinimizer(MIN_KIND_DHS) then
        Exit;

    Info := Default(TMinimizerInfo);
    Info.Kind := MIN_KIND_DHS;
    Info.Name := 'Downhill Simplex (native)';
    Info.Description :=
        'The original algorithm. Needs no Python and fits any curve type, ' +
        'including those with no formula.';
    //  Evaluates the curve objects themselves, so a curve with no closed form is
    //  fine.
    Info.NeedsFormula := False;
    Info.NeedsPythonSidecar := False;
    //  Always fits unweighted.
    Info.SupportsWeighting := False;
    //  Curve scaling is this engine's own trick.
    Info.SupportsCurveScaling := True;
    RegisterMinimizer(Info);

    Info := Default(TMinimizerInfo);
    Info.Kind := MIN_KIND_PYTHON_LM;
    Info.Name := 'Levenberg-Marquardt (Python/lmfit)';
    Info.Description :=
        'Trust-region least squares with uncertainties. Needs the Python ' +
        'sidecar, and a curve type that has a formula.';
    Info.NeedsFormula := True;
    Info.NeedsPythonSidecar := True;
    Info.SupportsWeighting := True;
    //  Fits the amplitude itself, so scaling afterwards would rescale an
    //  already-fitted value.
    Info.SupportsCurveScaling := False;
    RegisterMinimizer(Info);
end;

end.
