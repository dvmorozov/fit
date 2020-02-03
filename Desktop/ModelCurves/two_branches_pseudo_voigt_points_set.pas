{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class-container of Pseudo-Voigt curve having different form parameters for the right and left branches.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit two_branches_pseudo_voigt_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, int_points_set, points_set, named_points_set,
    curve_points_set, curve_types_singleton, special_curve_parameter,
    amplitude_curve_parameter, sigma_curve_parameter, position_curve_parameter,
    SimpMath;

type
    { Pseudo-Voigt curve having different form parameters for
      the right and left branches. }
    T2BranchesPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        SigmaRightP: TSpecialCurveParameter;
        SigmaRightIndex: LongInt;
        
        EtaRightP: TSpecialCurveParameter;
        EtaRightIndex: LongInt;
        
        EtaP: TSpecialCurveParameter;
        EtaIndex: LongInt;

        procedure SetSigmaRight(Value: Double);
        function GetSigmaRight: Double;
        procedure SetEtaRight(Value: Double);
        function GetEtaRight: Double;
        procedure SetEta(Value: Double);
        function GetEta: Double;

        { Sets up pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        { Sets up indexes of parameters with predefined semantics. }
        procedure SetSpecParamVarIndex(P: TSpecialCurveParameter; Index: LongInt); override;

        { Returns variable parameter with given index. }
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
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeId: TCurveTypeId; override;
        class function GetCurveTypeId_: TCurveTypeId; override;

        function HasSigmaRight: Boolean;
        function HasEtaRight: Boolean;
        function HasEta: Boolean;

        property SigmaRight: Double read GetSigmaRight write SetSigmaRight;
        property EtaRight: Double read GetEtaRight write SetEtaRight;
        property Eta: Double read GetEta write SetEta;
    end;

implementation

{=================== T2BranchesPseudoVoigtPointsSet ===========================}

procedure T2BranchesPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    Points[j][2] := TwoBranchesPseudoVoigtPoint(
                        A, Sigma, Eta, SigmaRight, EtaRight, x0, Points[j][1]);
        end;
    end
    else
    begin
        TwoBranchesPseudoVoigt(Points, A, Sigma, Eta, SigmaRight, EtaRight, x0);
    end;
end;

constructor T2BranchesPseudoVoigtPointsSet.Create(AOwner: TComponent);
var Parameter: TSpecialCurveParameter;
begin
    inherited;
    SigmaRightIndex := -1;
    EtaRightIndex := -1;
    EtaIndex := -1;

    Parameter := TAmplitudeCurveParameter.Create;
    AddParameter(Parameter);

    Parameter := TPositionCurveParameter.Create(Self);
    AddParameter(Parameter);

    Parameter := TSigmaCurveParameter.Create;
    AddParameter(Parameter);

    Parameter := TSpecialCurveParameter.Create;
    Parameter.Name := 'eta'; Parameter.Value := 0;
    Parameter.Type_ := Variable;    //  razreschaetsya var'irovanie parametra
                                    //  otdel'no dlya kazhdogo ekzemplyara
                                    //  patterna
    AddParameter(Parameter);

    Parameter := TSpecialCurveParameter.Create;
    Parameter.Name := 'sigmaright'; Parameter.Value := 0.25;
    Parameter.Type_ := Variable;
    //Parameter.Type_ := Shared;    //  ne var'iruetsya otdel'no,
                                    //  prinimaet odno znachenie dlya vseh
                                    //  krivyh podzadachi
    AddParameter(Parameter);

    Parameter := TSpecialCurveParameter.Create;
    Parameter.Name := 'etaright'; Parameter.Value := 0;
    Parameter.Type_ := Variable;    //  razreschaetsya var'irovanie parametra
                                    //  otdel'no dlya kazhdogo ekzemplyara
                                    //  patterna
    AddParameter(Parameter);

    InitListOfVariableParameters;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetEta(Value: Double);
begin
    Assert(Assigned(EtaP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaP.Value := Abs(Value);
    if EtaP.Value > 1 then EtaP.Value := 1;
end;

function T2BranchesPseudoVoigtPointsSet.GetEta: Double;
begin
    Assert(Assigned(EtaP));
    Result := EtaP.Value;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetEtaRight(Value: Double);
begin
    Assert(Assigned(EtaRightP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaRightP.Value := Abs(Value);
    if EtaRightP.Value > 1 then EtaRightP.Value := 1;
end;

function T2BranchesPseudoVoigtPointsSet.GetEtaRight: Double;
begin
    Assert(Assigned(EtaRightP));
    Result := EtaRightP.Value;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetSigmaRight(Value: Double);
begin
    Assert(Assigned(SigmaRightP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    SigmaRightP.Value := Abs(Value);
    if SigmaRightP.Value = 0 then SigmaRightP.Value := TINY;
end;

function T2BranchesPseudoVoigtPointsSet.GetSigmaRight: Double;
begin
    Assert(Assigned(SigmaRightP));
    Result := SigmaRightP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.HasEta: Boolean;
begin
    if Assigned(EtaP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.HasEtaRight: Boolean;
begin
    if Assigned(EtaRightP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.HasSigmaRight: Boolean;
begin
    if Assigned(SigmaRightP) then Result := True else Result := False;
end;

function T2BranchesPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := '2 br. Pseudo-Voigt';
end;

function T2BranchesPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := GetCurveTypeId_;
end;

class function T2BranchesPseudoVoigtPointsSet.GetCurveTypeId_: TCurveTypeId;
begin
    Result := StringToGUID('{6de06c1b-e51a-48c6-b036-c81a841ec468}');
end;

procedure T2BranchesPseudoVoigtPointsSet.SetParamByName(
    Name: string; Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Modified := True;

    if UpperCase(Name) = 'ETARIGHT' then
    begin
{$IFDEF WRITE_PARAMS_LOG}
        LogStr := ' SetParamByName(EtaRight): Value = ' + FloatToStr(Value);
        WriteLog(LogStr, Notification_);
{$ENDIF}
        EtaRight := Value;
    end
    else
    begin
        if UpperCase(Name) = 'SIGMARIGHT' then
        begin
{$IFDEF WRITE_PARAMS_LOG}
            LogStr := ' SetParamByName(SigmaRight): Value = ' + FloatToStr(Value);
            WriteLog(LogStr, Notification_);
{$ENDIF}
            SigmaRight := Value;
        end
        else
        begin
            if UpperCase(Name) = 'ETA' then
            begin
{$IFDEF WRITE_PARAMS_LOG}
                LogStr := ' SetParamByName(Eta): Value = ' + FloatToStr(Value);
                WriteLog(LogStr, Notification_);
{$ENDIF}
                Eta := Value;
            end
            else inherited;
        end;
    end;
end;

function T2BranchesPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
begin
    if UpperCase(Name) = 'ETARIGHT' then
        Result := EtaRight
    else
    begin
        if UpperCase(Name) = 'SIGMARIGHT' then
            Result := SigmaRight
        else
        begin
            if UpperCase(Name) = 'ETA' then
                Result := Eta
            else Result := inherited;
        end;
    end;
end;

procedure T2BranchesPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = EtaRightIndex then
        EtaRight := Value
    else
    begin
        if Index = SigmaRightIndex then
            SigmaRight := Value
        else
        begin
            if Index = EtaIndex then
                Eta := Value
            else inherited;
        end;
    end;
end;

function T2BranchesPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(Index < GetParamCount);

    if Index = EtaRightIndex then
        Result := EtaRight
    else
    begin
        if Index = SigmaRightIndex then
            Result := SigmaRight
        else
        begin
            if Index = EtaIndex then
                Result := Eta
            else Result := inherited;
        end;
    end;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure T2BranchesPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then
        EtaP := P
    else
    begin
        if UpperCase(P.Name) = 'ETARIGHT' then
            EtaRightP := P
        else
        begin
            if UpperCase(P.Name) = 'SIGMARIGHT' then
                SigmaRightP := P
            else inherited;
        end;
    end;
end;

//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy semantikoy
procedure T2BranchesPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then
        EtaIndex := Index
    else
    begin
        if UpperCase(P.Name) = 'ETARIGHT' then
            EtaRightIndex := Index
        else
        begin
            if UpperCase(P.Name) = 'SIGMARIGHT' then
                SigmaRightIndex := Index
            else inherited;
        end;
    end;
end;

var CTS: TCurveTypesSingleton;

initialization
    CTS := TCurveTypesSingleton.Create;
    CTS.RegisterCurveType(T2BranchesPseudoVoigtPointsSet);
end.


