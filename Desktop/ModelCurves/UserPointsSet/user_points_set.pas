// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for user curve given as expression.)

Copyright (C) Dmitry Morozov
}
unit user_points_set;

{$mode delphi}

interface

//  User-defined curves are available on every platform: the expression is
//  evaluated by the cross-platform native_math_expr engine (formerly the
//  Windows-only 'MathExpr' library).

uses
    explanation, SysUtils, native_math_expr,
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
        { CalcValue, in the shape peak_full_width measures. }
        function ValueAt(const AX: double): double;

    private
        { What FullWidthPerUnit last measured, for which parameter, in which
          window: measured once per window, not before every calculation - a
          sampled and bisected formula is many evaluations of it. }
        FMeasuredName: string;
        FMeasuredFirstX, FMeasuredLastX, FMeasuredPerUnit: double;

    public
        { A Width role's full width at half maximum per unit, MEASURED from
          the user's formula (peak_full_width): the program cannot know whether
          the formula reads it as a standard deviation, a half width or a full
          width, so it looks. Measured at the values the curve has when its
          window is set, with an amplitude of 0 taken as 1, and kept for that
          window - exact for a width that scales the curve, as a width does. A
          formula with no peak to measure counts its Width as a full width. }
        function FullWidthPerUnit(const AParameterName: string): double; override;

    public
        procedure CopyParameters(Dest: TObject); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function Explanation: TExplanation; override;
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


uses configurable_user_points_set, int_curve_factory, checks, peak_full_width;

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

function TUserPointsSet.ValueAt(const AX: double): double;
begin
    Result := CalcValue(AX);
end;

function TUserPointsSet.FullWidthPerUnit(const AParameterName: string): double;
var
    P, Amplitude_: TSpecialCurveParameter;
    i: longint;
    W, SavedA, Centre, Full: double;
begin
    P := Parameters.FindByName(AParameterName);
    if not (Assigned(P) and (P.Type_ = Width) and FParamWindowSet) then
        Exit(inherited FullWidthPerUnit(AParameterName));
    if (FMeasuredName = AParameterName) and
       (FMeasuredFirstX = FParamWindow.FirstX) and
       (FMeasuredLastX = FParamWindow.LastX) then
        Exit(FMeasuredPerUnit);

    Result := inherited FullWidthPerUnit(AParameterName);
    W := P.Value;
    if W > 0 then
    begin
        //  A NEW CURVE'S AMPLITUDE IS 0, which is flat: measured at 1, and put
        //  back - the measurement must not change the curve.
        Amplitude_ := nil;
        for i := 0 to Parameters.Count - 1 do
            if Parameters[i].Type_ = Amplitude then
                Amplitude_ := Parameters[i];
        SavedA := 0;
        if Assigned(Amplitude_) then
        begin
            SavedA := Amplitude_.Value;
            if SavedA = 0 then
                Amplitude_.Value := 1;
        end;
        if Hasx0 then
            Centre := x0
        else
            Centre := (FParamWindow.FirstX + FParamWindow.LastX) / 2;
        Full := MeasuredFullWidth(ValueAt, Centre, 50 * W);
        if Assigned(Amplitude_) then
            Amplitude_.Value := SavedA;
        if Full > 0 then
            Result := Full / W;
    end;
    FMeasuredName := AParameterName;
    FMeasuredFirstX := FParamWindow.FirstX;
    FMeasuredLastX := FParamWindow.LastX;
    FMeasuredPerUnit := Result;
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

class function TUserPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A curve whose formula you type yourself, with parameters named ' +
        'in that formula.',
        esModelChoice);
    AddParagraph(Result,
        'The expression is parsed and evaluated as written; every name in ' +
        'it other than x becomes a parameter the fit can vary.');
    AddParagraph(Result,
        'Use it for a model the built-in types do not provide, without ' +
        'writing a module.');
    AddLimitation(Result,
        'Nothing checks that the formula means something physical, or ' +
        'that its parameters are identifiable from the data.');
    AddLimitation(Result,
        'A parameter that does not appear in the expression cannot vary, ' +
        'so the fit silently holds it at its starting value.');
    AddLimitation(Result,
        'The expression is interpreted rather than compiled, so it is ' +
        'slower than a built-in type.');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TUserPointsSet);

end.
