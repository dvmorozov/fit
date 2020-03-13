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

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses SysUtils, curve_points_set, named_points_set, curve_types_singleton,
    configurable_points_set
{$IFDEF _WINDOWS}
    , points_set, special_curve_parameter
    , Windows
{$ENDIF}
    ;

{$IFDEF _WINDOWS}
function ParseAndCalcExpression(Expr: LPCSTR; ParamList: LPCSTR;
    Result: PDouble): LongInt; cdecl;
    external 'MathExpr' name 'ParseAndCalcExpression';
function GetSymbols: LPCSTR; cdecl;
    external 'MathExpr' name 'GetSymbols';
procedure FreeSymbols(Symbols: LPCSTR); cdecl;
    external 'MathExpr' name 'FreeSymbols';
{$ENDIF}

type
    { Container for points of user curve given as expression. }
    TUserPointsSet = class(TNamedPointsSet)
    protected
        { Expression given in general text form. }
        FExpression: string;
{$IFDEF _WINDOWS}
        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        { Performs calculation of function value for given value of argument. }
        function CalcValue(ArgValue: Double): Double;
{$ENDIF}

    public
        procedure CopyParameters(const Dest: TObject); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;

        class function GetConfigurablePointsSet:
            TConfigurablePointsSetClass; override;

        property Expression: string read FExpression write FExpression;
    end;
    
implementation

uses configurable_user_points_set, int_curve_factory;

{=========================== TUserPointsSet ================================}

class function TUserPointsSet.GetCurveTypeName: string;
begin
    Result := 'User defined';
end;

class function TUserPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{d8cafce5-8b03-4cce-9e93-ea28acb8e7ca}');
end;

class function TUserPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := MaximumsAndMinimums;
end;

{$IFDEF _WINDOWS}
function TUserPointsSet.CalcValue(ArgValue: Double): Double;
var P: TSpecialCurveParameter;
    Prs: string;
    i: LongInt;
begin
    Assert(Assigned(Parameters));
    Assert(Assigned(FVariableParameters));
    Assert(Assigned(ArgP));
    { Sets up value of argument. }
    P := ArgP;
    P.Value := ArgValue;
    { Creates string of VariableParameters. }
    Prs := '';
    for i := 0 to Parameters.Count - 1 do
    begin
        P := Parameters[i];
        Prs := Prs + P.Name + '=' + FloatToStr(P.Value) + Chr(0);
    end;
    Result := 0;
    { Sets parameter values and calculates expression. }
    if ParseAndCalcExpression(PChar(Expression), PChar(Prs), @Result) <> 1 then
        raise Exception.Create('Inadmissible or invalid expression');
end;

procedure TUserPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    PointYCoord[j] := CalcValue(PointXCoord[j]);
        end;
    end
    else
    begin
        //  poskol'ku vid krivoy ne izvesten, to optimizatsiya
        //  nevozmozhna - delaem polnyy pereschet
        for j := 0 to PointsCount - 1 do
            PointYCoord[j] := CalcValue(PointXCoord[j]);
    end;
end;
{$ENDIF}

procedure TUserPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TUserPointsSet(Dest).Expression := Expression;
end;

class function TUserPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TConfigurableUserPointsSet;
end;

var CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TUserPointsSet);
end.

