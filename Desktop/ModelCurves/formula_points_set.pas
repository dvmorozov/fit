// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Base class for built-in curves defined by a single analytic formula.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit formula_points_set;

{$mode delphi}

interface

//  A built-in lineshape that owns exactly one formula and is computed by the
//  same cross-platform expression engine (native_math_expr) as user-defined
//  curves - the single source of truth is the formula string, evaluated
//  identically here (native engine) and, after ExpressionToNumpy, by the Python
//  sidecar. That guarantees the two engines agree by construction, and the
//  transpiler is pinned to numpy by the expr-fidelity differential test. New
//  lineshapes that need no special function (Pearson VII, Moffat, ...) subclass
//  this and only supply GetNativeExpression + parameters + name/GUID/extremum.

uses
    SysUtils, native_math_expr,
    argument_axis, curve_points_set, named_points_set, points_set,
    special_curve_parameter;

type
    { Container for a curve given by a fixed analytic formula. }
    TFormulaPointsSet = class(TNamedPointsSet)
    protected
        { The curve's formula in the native engine's syntax (fpexprpars: '^' for
          power, 'ln' natural log, 'sqr', ...), in x and the curve's parameter
          names. This is the ONLY thing a concrete lineshape must define besides
          its parameters and identity. }
        function GetNativeExpression: string; virtual; abstract;
        { Evaluates the formula at one abscissa value with the current parameters. }
        function CalcValue(ArgValue: double): double;
        procedure DoCalc; override;

    public
        { The same formula translated to numpy syntax for the Python backend. }
        function GetCurveExpression: string; override;
        { Every lineshape built on this base is a diffraction peak, so its
          argument is the scattering angle. A future non-diffraction formula
          curve overrides this back to the inherited native axis. }
        class function CreatePreferredAxis(AWaveLength: double): TArgumentAxis;
            override;
    end;

implementation

uses
    checks;

function TFormulaPointsSet.CalcValue(ArgValue: double): double;
var
    P:   TSpecialCurveParameter;
    Prs: string;
    i:   longint;
begin
    CheckAssigned(Parameters, 'the parameter list the formula is evaluated against');

    { Build the '<name>=<value>' #0-separated list the native engine expects,
      from every parameter, plus the abscissa 'x'. }
    Prs := '';
    for i := 0 to Parameters.Count - 1 do
    begin
        P   := Parameters[i];
        Prs := Prs + P.Name + '=' + FloatToStr(P.Value) + Chr(0);
    end;
    Prs := Prs + 'x=' + FloatToStr(ArgValue) + Chr(0);

    Result := 0;
    { A non-1 return means the current parameters give no finite value (e.g. a
      zero denominator while the optimizer probes); leave Result at 0 so the fit
      continues away from that region instead of aborting. }
    ParseAndCalcExpression(PChar(GetNativeExpression), PChar(Prs), @Result);
end;

procedure TFormulaPointsSet.DoCalc;
var
    j: longint;
begin
    for j := 0 to PointsCount - 1 do
        PointYCoord[j] := CalcValue(PointXCoord[j]);
end;

function TFormulaPointsSet.GetCurveExpression: string;
begin
    Result := ExpressionToNumpy(GetNativeExpression);
end;

class function TFormulaPointsSet.CreatePreferredAxis(
    AWaveLength: double): TArgumentAxis;
begin
    //  Coordinates of a diffraction pattern are stored in 2*Theta.
    Result := TDiffractionAngleAxis.Create(dmTwoTheta, AWaveLength);
end;

end.
