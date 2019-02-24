{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for user curve given as expression.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit SpecialPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, PointsSet, CurvePointsSet;

type
    PDouble = ^Double;
    (*
    function ParseAndCalcExpression(Expr: LPCSTR; ParamList: LPCSTR;
        Result: PDouble): LongInt; cdecl;
        external 'MathExpr' name 'ParseAndCalcExpression';
    function GetSymbols: LPCSTR; cdecl;
        external 'MathExpr' name 'GetSymbols';
    procedure FreeSymbols(Symbols: LPCSTR); cdecl;
        external 'MathExpr' name 'FreeSymbols';
    bez ispol'zovaniya...
    *)

    { Container for points of user curve given as expression. }
    TSpecialPointsSet = class(TCurvePointsSet)
    protected
        { Expression given in general text form. }
        FExpression: string;
        FName: string;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        { Performs calculation of function value for given value of argument. }
        function CalcValue(ArgValue: Double): Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        function GetName: string; override;
        procedure SetName(AName: string);
        
        property Expression: string read FExpression write FExpression;
    end;
    
implementation

{=========================== TSpecialPointsSet ================================}

function TSpecialPointsSet.CalcValue(ArgValue: Double): Double;
var P: TSpecialCurveParameter;
    Prs: string;
    i: LongInt;
begin
    Assert(Assigned(Params));
    Assert(Assigned(Params.Params));
    Assert(Assigned(Links));
    Assert(Assigned(ArgP));
    //  zapolnyaetsya znachenie argumenta;
    P := ArgP;
    P.Value := ArgValue;
    //  sozdaetsya stroka parametrov;
    //  zdes' ispol'zuyutsya vse parametry
    Prs := '';
    for i := 0 to Params.Params.Count - 1 do
    begin
        P := TSpecialCurveParameter(Params.Params.Items[i]);
        Prs := Prs + P.Name + '=' + FloatToStr(P.Value) + Chr(0);
    end;
    //  !!! ustanovka znacheniy parametrov i vychislenie vyrazheniya
    //  d. delat'sya atomarnym vyzovom v mnogopotochnoy srede =>
    //  nel'zya predvaritel'no gotovit' znacheniya parametrov !!!
    (*
    if ParseAndCalcExpression(PChar(Expression), PChar(Prs), @Result) <> 1 then
        //??? mozhet zamenit' na Assert nad rezul'tatom
        raise Exception.Create('Inadmissible or invalid expression');

        bez ispol'zovaniya...*)
end;

function TSpecialPointsSet.GetName: string;
begin
    Result := FName;
end;

procedure TSpecialPointsSet.SetName(AName: string);
begin
    FName := AName;
end;

procedure TSpecialPointsSet.DoCalc(const Intervals: TPointsSet);
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

procedure TSpecialPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TSpecialPointsSet(Dest).Expression := Expression;
end;

end.

