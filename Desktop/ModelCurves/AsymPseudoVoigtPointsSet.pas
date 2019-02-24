{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having asymmetrical Pseudo-Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit AsymPseudoVoigtPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, PointsSet, PseudoVoigtPointsSet, CurvePointsSet,
  SimpMath;

type
    { Curve having asymmetrical Pseudo-Voigt form. }
    TAsymPseudoVoigtPointsSet = class(TPseudoVoigtPointsSet)
    protected
        { Difference of half-widths of left and right sides of the curve. }
        DeltaSigmaP: TSpecialCurveParameter;
        DeltaSigmaIndex: LongInt;

        procedure SetDeltaSigma(Value: Double);
        function GetDeltaSigma: Double;

        { Sets up pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        { Sets up indexes of parameters with predefined semantics. }
        procedure SetSpecParamVarIndex(P: TSpecialCurveParameter; Index: LongInt); override;

        { Returns parameter with given index. }
        function GetParam(Index: LongInt): Double; override;
        { Sets up variable paremeter with given index. }
        procedure SetParam(Index: LongInt; Value: Double); override;
        
        { Returns parameter with given name. }
        function GetParamByName(Name: string): Double; override;
        { Sets up parameter with given name. }
        procedure SetParamByName(Name: string; Value: Double); override;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        
    public
        constructor Create(AOwner: TComponent); override;
        function GetName: string; override;

        function HasDeltaSigma: Boolean;

        property DeltaSigma: Double read GetDeltaSigma write SetDeltaSigma;
    end;

implementation

{====================== TAsymPseudoVoigtPointsSet =============================}

procedure TAsymPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    Points[j][2] := AsymPseudoVoigtPoint(
                        A, Sigma, Eta, x0, Points[j][1], DeltaSigma);
        end;
    end
    else
    begin
        AsymPseudoVoigt(Points, A, Sigma, Eta, x0, DeltaSigma);
    end;
end;

constructor TAsymPseudoVoigtPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    DeltaSigmaIndex := -1;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'deltasigma'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna
    InitLinks;
end;

procedure TAsymPseudoVoigtPointsSet.SetDeltaSigma(Value: Double);
begin
    Assert(Assigned(DeltaSigmaP));
    Modified := True;
    //  !!! dopuskayutsya otritsatel'nye znacheniya !!!
    DeltaSigmaP.Value := Value;
end;

function TAsymPseudoVoigtPointsSet.GetDeltaSigma: Double;
begin
    Assert(Assigned(DeltaSigmaP));
    Result := DeltaSigmaP.Value;
end;

function TAsymPseudoVoigtPointsSet.HasDeltaSigma: Boolean;
begin
    if Assigned(DeltaSigmaP) then Result := True else Result := False;
end;

function TAsymPseudoVoigtPointsSet.GetName: string;
begin
    Result := 'Asym. Pseudo-Voigt';
end;

procedure TAsymPseudoVoigtPointsSet.SetParamByName(Name: string; Value: Double);
var P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Modified := True;

    if UpperCase(Name) = 'DELTASIGMA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) +
            ' SetParamByName(DeltaSigma): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        DeltaSigma := Value;
    end
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
begin
    if UpperCase(Name) = 'DELTASIGMA' then Result := DeltaSigma
    else Result := inherited;
end;

procedure TAsymPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = DeltaSigmaIndex then DeltaSigma := Value
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(Index < GetParamCount);

    if Index = DeltaSigmaIndex then Result := DeltaSigma
    else Result := inherited;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TAsymPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'DELTASIGMA' then DeltaSigmaP := P
    else inherited;
end;
//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TAsymPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'DELTASIGMA' then DeltaSigmaIndex := Index
    else inherited;
end;

end.
