// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for user curve given as expression.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit user_points_set;

{$mode delphi}

interface

//  User-defined curves are available on every platform: the expression is
//  evaluated by the cross-platform native_math_expr engine (formerly the
//  Windows-only 'MathExpr' library).

uses
    SysUtils, native_math_expr,
    configurable_points_set, curve_points_set, curve_types_singleton,
    named_points_set, points_set, special_curve_parameter;

type
    { Container for points of user curve given as expression. }
    TUserPointsSet = class(TNamedPointsSet)
    protected
        { Expression given in general text form. }
        FExpression: string;
        { Performs recalculation of all points of function. }
        procedure DoCalc; override;
        { Performs calculation of function value for given value of argument. }
        function CalcValue(ArgValue: double): double;

    public
        procedure CopyParameters(Dest: TObject); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;

        class function GetConfigurablePointsSet: TConfigurablePointsSetClass;
            override;
        { The user's formula translated to the Python backend's numpy syntax so a
          user curve fits under the Python minimizer too. }
        function GetCurveExpression: string; override;

        property Expression: string read FExpression write FExpression;
    end;



implementation


uses configurable_user_points_set, int_curve_factory, checks;

class function TUserPointsSet.GetCurveTypeName: string;
begin
    Result := 'User Defined';
end;

class function TUserPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{d8cafce5-8b03-4cce-9e93-ea28acb8e7ca}');
end;

class function TUserPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := MaximumsAndMinimums;
end;

function TUserPointsSet.CalcValue(ArgValue: double): double;
var
    P:   TSpecialCurveParameter;
    Prs: string;
    i:   longint;
begin
    CheckAssigned(Parameters, 'the parameter list the user expression is evaluated against');
    CheckAssigned(FVariableParameters, 'the list of parameters the fit may vary');
    CheckAssigned(FArgP, 'the parameter that carries the abscissa into the user expression');

    { Sets up value of argument. }
    P   := FArgP;
    P.Value := ArgValue;
    { Creates string of VariableParameters. }
    Prs := '';
    for i := 0 to Parameters.Count - 1 do
    begin
        P   := Parameters[i];
        Prs := Prs + P.Name + '=' + FloatToStr(P.Value) + Chr(0);
    end;
    Result := 0;
    { Sets parameter values and calculates the expression. A non-1 return means
      the current parameter values give no finite value (e.g. a zero denominator
      while the optimizer probes); leave Result at 0 so the fit continues and
      moves away from that region instead of aborting. The formula itself was
      already validated when the curve type was selected. }
    ParseAndCalcExpression(PChar(Expression), PChar(Prs), @Result);
end;

procedure TUserPointsSet.DoCalc;
var
    j: longint;
begin
        for j := 0 to PointsCount - 1 do
            PointYCoord[j] := CalcValue(PointXCoord[j]);
    //  The shape of the curve is not known here - it is whatever expression
    //  the user typed - so the interval optimisation the built-in types use is
    //  not available and everything is recomputed.
end;

procedure TUserPointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TUserPointsSet(Dest).Expression := Expression;
end;

class function TUserPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TConfigurableUserPointsSet;
end;

function TUserPointsSet.GetCurveExpression: string;
begin
    Result := ExpressionToNumpy(FExpression);
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TUserPointsSet);

end.
