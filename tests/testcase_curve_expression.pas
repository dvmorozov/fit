// SPDX-License-Identifier: GPL-3.0-or-later
{ Guards the analytic expression each curve hands the model-agnostic Python
  backend (GetCurveExpression). The backend fits whatever formula it is sent, so
  the expression must be non-empty and must name every parameter the curve
  exposes - a renamed or forgotten parameter would silently break the fit. The
  formula's numeric fidelity to SimpMath is checked end-to-end (native vs Python)
  and by the sidecar's own tests. }
unit testcase_curve_expression;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  named_points_set, curve_points_set, special_curve_parameter,
  gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
  asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
  pearson7_points_set, moffat_points_set, doniach_sunjic_points_set, emg_points_set, voigt_points_set,
  skewed_gaussian_points_set, step_points_set;
type
  TCurveExpressionTest = class(TTestCase)
  private
    procedure CheckCurve(C: TNamedPointsSet);
  published
    procedure GaussianExpressionNamesItsParameters;
    procedure LorentzianExpressionNamesItsParameters;
    procedure PseudoVoigtExpressionNamesItsParameters;
    procedure AsymPseudoVoigtExpressionNamesItsParameters;
    procedure TwoBranchesPseudoVoigtExpressionNamesItsParameters;
    procedure Pearson7ExpressionNamesItsParameters;
    procedure MoffatExpressionNamesItsParameters;
    procedure DoniachSunjicExpressionNamesItsParameters;
    procedure EmgExpressionNamesItsParameters;
    procedure VoigtExpressionNamesItsParameters;
    procedure SkewedGaussianExpressionNamesItsParameters;
    procedure StepExpressionNamesItsParameters;
  end;

implementation

{ Asserts the curve has a non-empty expression that mentions every parameter it
  exposes. Frees the curve. }
procedure TCurveExpressionTest.CheckCurve(C: TNamedPointsSet);
var
  Expr, PName: string;
  j: integer;
begin
  try
    Expr := C.GetCurveExpression;
    AssertTrue('expression not empty', Length(Expr) > 0);
    for j := 0 to C.Parameters.Count - 1 do
    begin
      //  Skip the abscissa placeholder (named '?'); it is not a model parameter.
      if C.Parameters[j].Type_ = Argument then
        Continue;
      PName := C.Parameters[j].Name;
      AssertTrue('expression names parameter ' + PName, Pos(PName, Expr) > 0);
    end;
  finally
    C.Free;
  end;
end;

procedure TCurveExpressionTest.GaussianExpressionNamesItsParameters;
begin
  CheckCurve(TGaussPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.LorentzianExpressionNamesItsParameters;
begin
  CheckCurve(TLorentzPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.PseudoVoigtExpressionNamesItsParameters;
begin
  CheckCurve(TPseudoVoigtPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.AsymPseudoVoigtExpressionNamesItsParameters;
begin
  CheckCurve(TAsymPseudoVoigtPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.TwoBranchesPseudoVoigtExpressionNamesItsParameters;
begin
  CheckCurve(T2BranchesPseudoVoigtPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.Pearson7ExpressionNamesItsParameters;
begin
  CheckCurve(TPearson7PointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.MoffatExpressionNamesItsParameters;
begin
  CheckCurve(TMoffatPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.DoniachSunjicExpressionNamesItsParameters;
begin
  CheckCurve(TDoniachSunjicPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.EmgExpressionNamesItsParameters;
begin
  CheckCurve(TEmgPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.VoigtExpressionNamesItsParameters;
begin
  CheckCurve(TVoigtPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.SkewedGaussianExpressionNamesItsParameters;
begin
  CheckCurve(TSkewedGaussianPointsSet.Create(nil, 10.0));
end;

procedure TCurveExpressionTest.StepExpressionNamesItsParameters;
begin
  CheckCurve(TStepPointsSet.Create(nil, 10.0));
end;

initialization
  RegisterTest('unit', TCurveExpressionTest);
end.
