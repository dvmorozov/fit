{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Gauss form.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit gauss_points_set;

interface

uses Classes, SysUtils, int_points_set, points_set, curve_points_set, named_points_set,
  curve_types_singleton, SimpMath;

type
    { Curve having Gauss form. }
    TGaussPointsSet = class(TNamedPointsSet)
    protected
        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;

    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeId: TCurveTypeId; override;
        class function GetCurveTypeId_: TCurveTypeId; override;
    end;

    ValuePair = class(TObject)
    public
      X: double;
      Y: double;
    end;

implementation

{=========================== TGaussPointsSet ==================================}

constructor TGaussPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'A'; P.Value := 0; P.Type_ := Variable;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'x0'; P.Value := 0;
    P.Type_ := VariablePosition;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigma'; P.Value := 0.25;
    P.Type_ := Variable;       //  ne var'iruetsya otdel'no,
                               //  prinimaet odno znachenie dlya vseh
                               //  krivyh podzadachi (intervala)
    InitLinks;
end;

function TGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian';
end;

function TGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := GetCurveTypeId_;
end;

class function TGaussPointsSet.GetCurveTypeId_: TCurveTypeId;
begin
    Result := StringToGUID('{ff4e399c-c33c-482e-84d7-952700bcd4ae}');
end;

procedure TGaussPointsSet.DoCalc(const Intervals: TPointsSet);
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
                    Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
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
        Assert(x0Index <> -1);
        Points[x0Index][2] := GaussPoint(A, Sigma, x0, x0);
        
        Zero := False; LastRightIndex := x0Index;

        for j := x0Index - 1 downto 0 do
        begin
            Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
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
                Points[j][2] := GaussPoint(A, Sigma, x0, Points[j][1]);
                //  vsegda polozhitelen
                if Points[j][2] < ZeroCurveAmplitude then Break;
            end;
        *)
        //  polnyy pereschet bez optimizatsii
        Gauss(Points, A, Sigma, x0);
    end;
end;

function ComparePairs(Item1, Item2: Pointer): Integer;
begin
    if ValuePair(Item1).X < ValuePair(Item2).X then Result := -1
    else
      if ValuePair(Item1).X > ValuePair(Item2).X then Result := 1
      else Result := 0;
end;

var CTS: TCurveTypesSingleton;

initialization
    CTS := TCurveTypesSingleton.Create;
    CTS.RegisterCurveType(TGaussPointsSet);
end.


