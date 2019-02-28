{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Pseudo-Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit PseudoVoigtPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, CurvePointsSet, PointsSet, NamedPointsSet, SimpMath;

type
    { Function having Pseudo-Voigt form. }
    TPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        { Relative weights of gaussian and lorentzian. }
        EtaP: TSpecialCurveParameter;
        EtaIndex: LongInt;
        
        procedure SetEta(Value: Double);
        function GetEta: Double;
        
        { Initializes pointers to parameters with predefined semantics. }
        procedure SetSpecParamPtr(P: TSpecialCurveParameter); override;
        { Initializes indexes of parameters with predefined semantics. }
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
        { Replaces method defined in TNamedPointsSet. }
        class function GetTypeName: string;
        { Replaces method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId;

        function HasEta: Boolean;
        
        property Eta: Double read GetEta write SetEta;
    end;

implementation

uses Main;

{======================== TPseudoVoigtPointsSet ===============================}

procedure TPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    Points[j][2] := PseudoVoigtPoint(A, Sigma, Eta, x0, Points[j][1]);
        end;
    end
    else
    begin
        PseudoVoigt(Points, A, Sigma, Eta, x0);
    end;
end;

constructor TPseudoVoigtPointsSet.Create(AOwner: TComponent);
var P: TSpecialCurveParameter;
begin
    inherited;
    EtaIndex := -1;
    
    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'A'; P.Value := 0; P.Type_ := Variable;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'x0'; P.Value := 0;
    P.Type_ := VariablePosition;
    //P.Type_ := InvariablePosition;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'sigma'; P.Value := 0.25;
    //P.Type_ := Variable       //  ne var'iruetsya otdel'no,
                                //  prinimaet odno znachenie dlya vseh
                                //  krivyh podzadachi
    P.Type_ := Shared;

    P := TSpecialCurveParameter(FParams.Params.Add);
    P.Name := 'eta'; P.Value := 0;
    P.Type_ := Variable;        //  razreschaetsya var'irovanie parametra
                                //  otdel'no dlya kazhdogo ekzemplyara
                                //  patterna
    InitLinks;
end;

procedure TPseudoVoigtPointsSet.SetEta(Value: Double);
begin
    Assert(Assigned(EtaP));
    Modified := True;
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    EtaP.Value := Abs(Value);
    if EtaP.Value > 1 then EtaP.Value := 1;
end;

function TPseudoVoigtPointsSet.GetEta: Double;
begin
    Assert(Assigned(EtaP));
    Result := EtaP.Value;
end;

function TPseudoVoigtPointsSet.HasEta: Boolean;
begin
    if Assigned(EtaP) then Result := True else Result := False;
end;

class function TPseudoVoigtPointsSet.GetTypeName: string;
begin
    Result := 'Pseudo-Voigt';
end;

class function TPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{9f27dc7c-970f-4dac-88cd-f5fb3400d38d}');
end;

function TPseudoVoigtPointsSet.GetParamByName(Name: string): Double;
var P: TSpecialCurveParameter;
begin
    if UpperCase(Name) = 'ETA' then Result := Eta
    else Result := inherited;
end;

procedure TPseudoVoigtPointsSet.SetParamByName(Name: string; Value: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
    Modified := True;
    
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

procedure TPseudoVoigtPointsSet.SetParam(Index: LongInt; Value: Double);
var P: TSpecialCurveParameter;
begin
    Assert((Index < GetParamCount) and (Index >= 0));
    Modified := True;

    if Index = EtaIndex then
        Eta := Value
    else inherited;
end;

function TPseudoVoigtPointsSet.GetParam(Index: LongInt): Double;
begin
    Assert(index < GetParamCount);

    if Index = EtaIndex then Result := Eta
    else Result := inherited;
end;

//  ustanavlivaet ukazateli na parametry s predopredelennoy semantikoy
procedure TPseudoVoigtPointsSet.SetSpecParamPtr(
    P: TSpecialCurveParameter);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then EtaP := P
    else inherited;
end;

//  ustanavlivaet indeksy var'iruemyh parametrov s predopredelennoy
//  semantikoy
procedure TPseudoVoigtPointsSet.SetSpecParamVarIndex(
    P: TSpecialCurveParameter; Index: LongInt);
begin
    Assert(Assigned(P));
    if UpperCase(P.Name) = 'ETA' then EtaIndex := Index
    else inherited;
end;

end.

