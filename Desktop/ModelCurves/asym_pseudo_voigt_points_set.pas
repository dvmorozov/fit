{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having asymmetrical Pseudo-Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit asym_pseudo_voigt_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, int_points_set, points_set, pseudo_voigt_points_set,
    curve_points_set, curve_types_singleton, special_curve_parameter,
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

        { Sets up pointers to VariableParameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        { Sets up indexes of VariableParameters with predefined semantics. }
        procedure SetSpecParamVarIndex(P: TSpecialCurveParameter; Index: LongInt); override;

        { Returns parameter with given index. }
        function GetVariableParameterValue(Index: LongInt): Double; override;
        { Sets up variable paremeter with given index. }
        procedure SetVariableParameterValue(Index: LongInt; Value: Double); override;
        
        { Returns parameter with given name. }
        function GetParameterByName(Name: string): Double; override;
        { Sets up parameter with given name. }
        procedure SetParameterByName(Name: string; Value: Double); override;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        
    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeId: TCurveTypeId; override;
        class function GetCurveTypeId_: TCurveTypeId; override;

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
var Parameter: TSpecialCurveParameter;
begin
    inherited;
    DeltaSigmaIndex := -1;

    Parameter := TSpecialCurveParameter.Create;
    Parameter.Name := 'deltasigma'; Parameter.Value := 0;
    Parameter.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                        //  otdel'no dlya kazhdogo ekzemplyara
                                        //  patterna
    AddParameter(Parameter);

    InitListOfVariableParameters;
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

function TAsymPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Asym. Pseudo-Voigt';
end;

class function TAsymPseudoVoigtPointsSet.GetCurveTypeId_: TCurveTypeId;
begin
    Result := StringToGUID('{74a6ec30-a019-475d-99a3-b62c4ab03a6c}');
end;

function TAsymPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := GetCurveTypeId_;
end;

procedure TAsymPseudoVoigtPointsSet.SetParameterByName(Name: string; Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
    Modified := True;

    if UpperCase(Name) = 'DELTASIGMA' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := IntToStr(LongInt(Self)) +
            ' SetVariableParameterValueByName(DeltaSigma): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        DeltaSigma := Value;
    end
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetParameterByName(Name: string): Double;
begin
    if UpperCase(Name) = 'DELTASIGMA' then Result := DeltaSigma
    else Result := inherited;
end;

procedure TAsymPseudoVoigtPointsSet.SetVariableParameterValue(Index: LongInt; Value: Double);
begin
    Assert((Index < GetVariableParameterCount) and (Index >= 0));
    Modified := True;

    if Index = DeltaSigmaIndex then DeltaSigma := Value
    else inherited;
end;

function TAsymPseudoVoigtPointsSet.GetVariableParameterValue(Index: LongInt): Double;
begin
    Assert(Index < GetVariableParameterCount);

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

var CTS: TCurveTypesSingleton;

initialization
    CTS := TCurveTypesSingleton.Create;
    CTS.RegisterCurveType(TAsymPseudoVoigtPointsSet);
end.
