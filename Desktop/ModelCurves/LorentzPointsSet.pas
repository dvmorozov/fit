{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Lorentz form.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit LorentzPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, PointsSet, NamedPointsSet, GaussPointsSet, SimpMath;

type
    { Curve class having Lorentz form. }
    TLorentzPointsSet = class(TGaussPointsSet)
    protected
        procedure DoCalc(const Intervals: TPointsSet); override;

    public
        { Replaces method defined in TNamedPointsSet. }
        class function GetTypeName: string;
        { Replaces method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId;
    end;

implementation

{========================== TLorentzPointsSet =================================}

class function TLorentzPointsSet.GetTypeName: string;
begin
    Result := 'Lorentzian';
end;

class function TLorentzPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{7ca6fdaf-95b7-4d84-bcba-130c828407cc}');
end;

procedure TLorentzPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
    //x0Index, LastRightIndex: LongInt;
    //Zero: Boolean;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            (*  takoy variant ne daet uskoreniya, a kazhetsya rabotaet
                dazhe chut' medlennee - vse s'edaet poisk indeksov ?!
            for j := IndexOfValueX(Intervals.GetPointXCoord(i shl 1)) to
                IndexOfValueX(Intervals.GetPointXCoord((i shl 1) + 1)) do
                    Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
            *)
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
        end;
    end
    else
    begin
        //  snachala nuzhno obnulit' tochki, chtoby vse tochki, znachenie
        //  funktsii v kotoryh < ZeroCurveAmplitude byli bez musora
        for j := 0 to PointsCount - 1 do PointYCoord[j] := 0;

        //  schitaem optimal'no, ispol'zuya porog nulya i simmetriyu
        (*  optimal'nyi schet rabotaet tol'ko kogda x0 ne var'iruetsya,
            t.e. krivaya simmetrichna otnositel'no izmeneniya indeksa
        x0Index := IndexOfValueX(x0);
        Points[x0Index][2] := LorentzPoint(A, Sigma, x0, x0);

        Zero := False; LastRightIndex := x0Index;

        for j := x0Index - 1 downto 0 do
        begin
            Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
            if (x0Index shl 1) - j <= PointsCount - 1 then
            begin
                Points[(x0Index shl 1) - j][2] := Points[j][2];
                LastRightIndex := (x0Index shl 1) - j;
            end;
            //  vsegda polozhitelen
            if Points[j][2] < ZeroCurveAmplitude then
            begin
                Zero := True;
                Break;
            end;
        end;

        if not Zero then
            //  0 esche ne dostignut
            for j := LastRightIndex + 1 to PointsCount - 1 do
            begin
                Points[j][2] := LorentzPoint(A, Sigma, x0, Points[j][1]);
                //  vsegda polozhitelen
                if Points[j][2] < ZeroCurveAmplitude then Break;
            end;
        *)
        //  polnyy pereschet bez optimizatsii
        Lorentz(Points, A, Sigma, x0);
    end;
end;

end.
