{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit Decisions;

{$MODE Delphi}

interface

uses SysUtils, Classes, ComponentList, MyExceptions, SimpMath;

type
  TAbstractDecision = class(TComponent)
  protected
    FEvaluation: Double;
    (*!!! znachenie Evaluation mozhet byt' i otritsatel'nym !!!*)
    function GetParametersNumber: LongInt; virtual; abstract;
    procedure SetParametersNumber(
    AParametersNumber: LongInt); virtual; abstract;
  public
    procedure FillCopy(const Copy: TAbstractDecision); virtual; abstract;
    (*pravil'no zapolnyaet vse polya sozdannoy kopii resheniya*)
    function GetCopy: TAbstractDecision; virtual; abstract;
    (*!!! obyazatel'no perekryvat' GetCopy i FillCopy u vseh potomkov !!!*)
    function Coincide(
    const Decision: TAbstractDecision): Boolean; virtual; abstract;
    (*vozvraschaet True, esli parametry resheniy sovpadayut*)

    property Evaluation: Double read FEvaluation write FEvaluation;
    property ParametersNumber: LongInt
    read GetParametersNumber write SetParametersNumber;
  end;

  TOneDimDecision = class(TAbstractDecision)
  public
    procedure InvertParameter(const ParamNum: LongInt); virtual; abstract;
    (*izmenyaet znachenie parametra na protivopolozhnoe*)
    procedure ExchangeParameters(
    const ParamNum1, ParamNum2: LongInt); virtual; abstract;
    (*menyaet parametry mestami v predelah resheniya*)
    procedure ExchangeWithOuter(const Decision: TAbstractDecision;
    ParamNum: LongInt); virtual; abstract;
    (*obmen parametrami s drugim resheniem*)
    procedure CopyParameter(
    const ParamNum, NewParamNum: LongInt); virtual; abstract;
    (*kopiruet znachenie parametra v ukazannuyu pozitsiyu
    !!! staroe znachenie parametra v pozitsii pri etom zatiraetsya !!!*)
  end;

  TTwoDimDecision = class(TAbstractDecision)
  (*abstraktnyy komponent dlya postroeniya resheniy s dvumernym
  massivom parametrov; kazhdyy stroka massiva - odin gen*)
  protected
    function GetGenesNumber: LongInt; virtual; abstract;
    procedure SetGenesNumber(AGenesNumber: LongInt); virtual; abstract;
  public
    procedure InvertParameter(const GeneNum,
    ParamNum: LongInt); virtual; abstract;
    procedure InvertBlock(const StartGeneNum, EndGeneNum,
    StartParamNum, EndParamNum: LongInt); virtual;
    (*izmenyaet znachenie parametra na protivopolozhnoe*)

    procedure ExchangeParameters(const GeneNum1, ParamNum1,
    GeneNum2, ParamNum2: LongInt); virtual; abstract;
    (*menyaet parametry mestami v predelah resheniya*)
    procedure ExchangeWithOuter(const Decision: TAbstractDecision;
    MyGeneNum, OuterGeneNum, ParamNum: LongInt); virtual; abstract;
    procedure ExchangeBlocksWithOuter(
    const Decision: TAbstractDecision; StartGeneNum, EndGeneNum,
    StartParamNum, EndParamNum: LongInt); virtual;
    (*obmen parametrami s drugim resheniem*)

    procedure CopyParameter(const SrcGeneNum, SrcParamNum,
    DestGeneNum, DestParamNum: LongInt); virtual; abstract;
    procedure CopyBlock(const StartGeneNum, EndGeneNum, StartParamNum,
    EndParamNum, GeneOffset, ParamOffset: LongInt); virtual; abstract;
    (*kopiruet znachenie parametra v ukazannuyu pozitsi
    !!! staroe znachenie parametra v pozitsii pri etom zatiraetsya !!!*)

    property GenesNumber: LongInt read GetGenesNumber write SetGenesNumber;
  end;

  (*bazovye komponenty - resheniya dlya raboty s parametrami razlichnyh tipov*)

  EFloatDecision = class(Exception);

  TFloatDecision = class(TOneDimDecision)
  protected
    FParameters: array of Double;
    function GetParameter(index: LongInt): Double;
    procedure SetParameter(index: LongInt; AParameter: Double);
    function GetParametersNumber: LongInt; override;
    procedure SetParametersNumber(AParametersNumber: LongInt); override;
  public
    destructor Destroy; override;
    procedure FillCopy(const Copy: TAbstractDecision); override;
    function GetCopy: TAbstractDecision; override;
    function Coincide(const Decision: TAbstractDecision): Boolean; override;
    procedure InvertParameter(const ParamNum: LongInt); override;
    procedure ExchangeParameters(
    const ParamNum1, ParamNum2: LongInt); override;
    procedure ExchangeWithOuter(const Decision: TAbstractDecision;
    ParamNum: LongInt); override;
    procedure CopyParameter(
    const ParamNum, NewParamNum: LongInt); override;

    property Parameters[index: LongInt]: Double
    read GetParameter write SetParameter; default;
  end;

  EByteDecision = class(Exception);

  TByteDecision = class(TOneDimDecision)
  protected
    FParameters: array of Byte;
    function GetParameter(index: LongInt): Byte;
    procedure SetParameter(index: LongInt; AParameter: Byte);
    function GetParametersNumber: LongInt; override;
    procedure SetParametersNumber(AParametersNumber: LongInt); override;
  public
    destructor Destroy; override;
    procedure FillCopy(const Copy: TAbstractDecision); override;    
    function GetCopy: TAbstractDecision; override;
    function Coincide(const Decision: TAbstractDecision): Boolean; override;
    procedure InvertParameter(const ParamNum: LongInt); override;
    (*invertiruet bayt posredstvom operatsii not*)
    procedure ExchangeParameters(
    const ParamNum1, ParamNum2: LongInt); override;
    procedure ExchangeWithOuter(const Decision: TAbstractDecision;
    ParamNum: LongInt); override;
    procedure CopyParameter(
    const ParamNum, NewParamNum: LongInt); override;

    property Parameters[index: LongInt]: Byte
    read GetParameter write SetParameter; default;
  end;

  ETwoDimFloatDecision = class(Exception);

  TTwoDimFloatDecision = class(TTwoDimDecision)
  (*ponyatie "gen" v dannom komponente
  oznachaet stolbets dvumernoy matritsy*)
  protected
    FParameters: array of array of Double;
    FParametersNumber: LongInt;
    FSelectedGene: LongInt;
    function GetParameter(index: LongInt): Double;
    procedure SetParameter(index: LongInt; AParameter: Double);
    function GetParametersNumber: LongInt; override;
    procedure SetParametersNumber(AParametersNumber: LongInt); override;
    function GetGenesNumber: LongInt; override;
    procedure SetGenesNumber(AGenesNumber: LongInt); override;
    function GetSelectedGene: LongInt;
    procedure SetSelectedGene(ASelectedGene: LongInt);
  public
    destructor Destroy; override;
    procedure FillCopy(const Copy: TAbstractDecision); override;    
    function GetCopy: TAbstractDecision; override;
    function Coincide(const Decision: TAbstractDecision): Boolean; override;

    procedure InvertParameter(const GeneNum,
    ParamNum: LongInt); override;
    procedure ExchangeParameters(const GeneNum1, ParamNum1,
    GeneNum2, ParamNum2: LongInt); override;
    procedure CopyParameter(const SrcGeneNum, SrcParamNum,
    DestGeneNum, DestParamNum: LongInt); override;
    procedure ExchangeWithOuter(const Decision: TAbstractDecision;
    MyGeneNum, OuterGeneNum, ParamNum: LongInt); override;
    procedure CopyBlock(const StartGeneNum, EndGeneNum, StartParamNum,
    EndParamNum, GeneOffset, ParamOffset: LongInt); override;
    (*!!! EndGeneNum d.b. >= StartGeneNum, EndParamNum d.b. >= StartParamNum !!!*)

    property Parameters[index: LongInt]: Double
    read GetParameter write SetParameter; default;
    property SelectedGene: LongInt read GetSelectedGene write SetSelectedGene;
  end;

  EDecisionsList = class(Exception);

  TDecisionsList = class(TComponentList)
  public
    function GetMaxDecision(
    const StartIndex: LongInt; UpLimit: Double): TAbstractDecision;
    (*vozvraschaet reshenie, imeyuschee maksimal'noe znachenie Evaluation
    sredi teh, u kot. znachenie Evaluation <= UpLimit, nachinaya so StartIndex
    !!! funktsiya budet rabotat' pravil'no, tol'ko esli elementy spiska
    otsortirovany po ubyvaniyu Evaluation !!!
    ??? ne vnesti li sortirovku pryamo v funktsiyu ???*)
    function GetMinDecision(
    const StartIndex: LongInt; LowLimit: Double): TAbstractDecision;
    (*vozvraschaet reshenie, imeyuschee minimal'noe znachenie Evaluation
    sredi teh, u kot. znachenie Evaluation >= LowLimit, nachinaya so StartIndex
    !!! funktsiya budet rabotat' pravil'no, tol'ko esli elementy spiska
    otsortirovany po vozrastaniyu Evaluation !!!*)
    function GetAbsoluteMin: TAbstractDecision;
    function GetAbsoluteMax: TAbstractDecision;
    function HasThisDecision(const Decision: TAbstractDecision): Boolean;
  end;

function EvalDownSortFunc(Item1, Item2: Pointer): Integer;
(*sortirovka po ubyvaniyu Evaluation*)
function EvalUpSortFunc(Item1, Item2: Pointer): Integer;
(*sortirovka po vozrastaniyu Evaluation*)

const
  EvalDownSort: TListSortCompare = @EvalDownSortFunc;
  EvalUpSort: TListSortCompare = @EvalUpSortFunc;

implementation

destructor TFloatDecision.Destroy;
begin
  Finalize(FParameters);
  inherited Destroy;
end;

function TFloatDecision.GetParameter(index: LongInt): Double;
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise EFloatDecision.Create('Parameter index out of range...');
  Result := FParameters[index];
end;

procedure TFloatDecision.SetParameter(index: LongInt;
AParameter: Double);
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise EFloatDecision.Create('Parameter index out of range...');
  FParameters[index] := AParameter;
end;

function TFloatDecision.GetParametersNumber: LongInt;
begin
  if Assigned(FParameters) then Result := Length(FParameters)
  else Result := 0;
end;

procedure TFloatDecision.SetParametersNumber(
AParametersNumber: LongInt);
var i: LongInt;
    SavedLength: LongInt;
begin
  if AParametersNumber > ParametersNumber then
  begin
    SavedLength := ParametersNumber;
    SetLength(FParameters, AParametersNumber);
    for i := SavedLength to ParametersNumber - 1 do Parameters[i] := 0;
  end
  else SetLength(FParameters, AParametersNumber);
end;

function TFloatDecision.GetCopy: TAbstractDecision;
begin
  Result := TFloatDecision.Create(nil);
  FillCopy(Result);
end;

procedure TFloatDecision.FillCopy(const Copy: TAbstractDecision);
var i: LongInt;
begin
  TFloatDecision(Copy).ParametersNumber := ParametersNumber;
  for i := 0 to ParametersNumber - 1 do
    TFloatDecision(Copy).Parameters[i] := Parameters[i];
  Copy.Evaluation := Evaluation;
end;

function TFloatDecision.Coincide(const Decision: TAbstractDecision): Boolean;
var FloatDecision: TFloatDecision absolute Decision;
    i: LongInt;
begin
  if not (Decision is TFloatDecision) then
    raise EFloatDecision.Create('Invalid decision type...');
  Result := True;
  for i := 0 to ParametersNumber - 1 do
  begin
    if Abs(FloatDecision[i] - Self[i]) >= TINY then
    begin Result := False; Exit end;
  end;
end;

procedure TFloatDecision.InvertParameter(const ParamNum: LongInt);
begin
  Self[ParamNum] := (-1) * Self[ParamNum];
end;

procedure TFloatDecision.ExchangeParameters(
const ParamNum1, ParamNum2: LongInt);
var TempDouble: Double;
begin
  TempDouble := Self[ParamNum2];
  Self[ParamNum2] := Self[ParamNum1];
  Self[ParamNum1] := TempDouble;
end;

procedure TFloatDecision.ExchangeWithOuter(
const Decision: TAbstractDecision; ParamNum: LongInt);
var TempDouble: Double;
begin
  if not (Decision is TFloatDecision) then
    raise EFloatDecision.Create('Invalid decision type ' + Decision.ClassName);

  TempDouble := Self[ParamNum];
  Self[ParamNum] := TFloatDecision(Decision)[ParamNum];
  TFloatDecision(Decision)[ParamNum] := TempDouble;
end;

procedure TFloatDecision.CopyParameter(
const ParamNum, NewParamNum: LongInt);
begin
  Self[NewParamNum] := Self[ParamNum];
end;

destructor TByteDecision.Destroy;
begin
  Finalize(FParameters);
  inherited Destroy;
end;

function TByteDecision.GetParameter(index: LongInt): Byte;
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise EByteDecision.Create('Parameter index out of range...');
  Result := FParameters[index];
end;

procedure TByteDecision.SetParameter(index: LongInt;
AParameter: Byte);
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise EByteDecision.Create('Parameter index out of range...');
  FParameters[index] := AParameter;
end;

function TByteDecision.GetParametersNumber: LongInt;
begin
  if Assigned(FParameters) then Result := Length(FParameters)
  else Result := 0;
end;

procedure TByteDecision.SetParametersNumber(
AParametersNumber: LongInt);
var i: LongInt;
    SavedLength: LongInt;
begin
  if AParametersNumber > ParametersNumber then
  begin
    SavedLength := ParametersNumber;
    SetLength(FParameters, AParametersNumber);
    for i := SavedLength to ParametersNumber - 1 do Parameters[i] := 0;
  end
  else SetLength(FParameters, AParametersNumber);
end;

function TByteDecision.GetCopy: TAbstractDecision;
begin
  Result := TByteDecision.Create(nil);
  FillCopy(Result);
end;

procedure TByteDecision.FillCopy(const Copy: TAbstractDecision);
var i: LongInt;
begin
  TByteDecision(Copy).ParametersNumber := ParametersNumber;
  for i := 0 to ParametersNumber - 1 do
    TByteDecision(Copy).Parameters[i] := Parameters[i];
  Copy.Evaluation := Evaluation;
end;

function TByteDecision.Coincide(const Decision: TAbstractDecision): Boolean;
var ByteDecision: TByteDecision absolute Decision;
var i: LongInt;
begin
  if not (Decision is TByteDecision) then
    raise EByteDecision.Create('Invalid decision type...');
  Result := True;
  for i := 0 to ParametersNumber - 1 do
  begin
    if ByteDecision[i] <> Self[i] then
    begin Result := False; Exit end;
  end;
end;

procedure TByteDecision.InvertParameter(const ParamNum: LongInt);
begin
  Self[ParamNum] := not Self[ParamNum];
end;

procedure TByteDecision.ExchangeParameters(
const ParamNum1, ParamNum2: LongInt);
var TempByte: Byte;
begin
  TempByte := Self[ParamNum2];
  Self[ParamNum2] := Self[ParamNum1];
  Self[ParamNum1] := TempByte;
end;

procedure TByteDecision.ExchangeWithOuter(
const Decision: TAbstractDecision; ParamNum: LongInt);
var TempByte: Byte;
begin
  if not (Decision is TByteDecision) then
    raise EByteDecision.Create('Invalid decision type ' + Decision.ClassName);

  TempByte := Self[ParamNum];
  Self[ParamNum] := TByteDecision(Decision)[ParamNum];
  TFloatDecision(Decision)[ParamNum] := TempByte;
end;

procedure TByteDecision.CopyParameter(
const ParamNum, NewParamNum: LongInt);
begin
  Self[NewParamNum] := Self[ParamNum];
end;

function TTwoDimFloatDecision.GetParameter(index: LongInt): Double;
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise ETwoDimFloatDecision.Create('Parameter index out of range...');
  Result := FParameters[SelectedGene, index];
end;

procedure TTwoDimFloatDecision.SetParameter(index: LongInt; AParameter: Double);
begin
  if (index > ParametersNumber - 1) or (index < 0) then
    raise ETwoDimFloatDecision.Create('Parameter index out of range...');
  FParameters[SelectedGene, index] := AParameter;
end;

function TTwoDimFloatDecision.GetParametersNumber: LongInt;
begin
  Result := FParametersNumber;
end;

procedure TTwoDimFloatDecision.SetParametersNumber(AParametersNumber: LongInt);
var i: LongInt;
begin
  FParametersNumber := AParametersNumber;
  for i := 0 to Length(FParameters) - 1 do
    SetLength(FParameters[i], AParametersNumber);
end;

function TTwoDimFloatDecision.GetGenesNumber: LongInt;
begin
  Result := Length(FParameters);
end;

procedure TTwoDimFloatDecision.SetGenesNumber(AGenesNumber: LongInt);
var SavedLength: LongInt;
    i: LongInt;
begin
  SavedLength := Length(FParameters);
  SetLength(FParameters, AGenesNumber);
  if Length(FParameters) > SavedLength then
    for i := SavedLength to Length(FParameters) - 1 do
      SetLength(FParameters[i], ParametersNumber);
end;

procedure TTwoDimFloatDecision.SetSelectedGene(ASelectedGene: LongInt);
begin
  if (ASelectedGene > GenesNumber - 1) or (ASelectedGene < 0) then
    raise ETwoDimFloatDecision.Create('Selected gene out of range...');
  FSelectedGene := ASelectedGene;
end;

function TTwoDimFloatDecision.GetSelectedGene: LongInt;
begin
  if (FSelectedGene > GenesNumber - 1) then
    raise ETwoDimFloatDecision.Create('Selected gene out of range...');
  Result := FSelectedGene;
end;

procedure TTwoDimFloatDecision.FillCopy(const Copy: TAbstractDecision);
var i, j: LongInt;
begin
  TTwoDimFloatDecision(Copy).GenesNumber := GenesNumber;
  TTwoDimFloatDecision(Copy).ParametersNumber := ParametersNumber;
  for j := 0 to GenesNumber - 1 do
  begin
    TTwoDimFloatDecision(Copy).SelectedGene := j;
    SelectedGene := j;
    for i := 0 to ParametersNumber - 1 do
      TTwoDimFloatDecision(Copy).Parameters[i] := Parameters[i];
  end;
  Copy.Evaluation := Evaluation;
end;

destructor TTwoDimFloatDecision.Destroy;
var i: LongInt;
begin
  for i := 0 to Length(FParameters) - 1 do Finalize(FParameters[i]);
  Finalize(FParameters);
  inherited Destroy;
end;

function TTwoDimFloatDecision.GetCopy: TAbstractDecision;
begin
  Result := TTwoDimFloatDecision.Create(nil);
  FillCopy(Result);
end;

function TTwoDimFloatDecision.Coincide(
const Decision: TAbstractDecision): Boolean;
var i, j: LongInt;
    TwoDimFloatDecision: TTwoDimFloatDecision absolute Decision;
begin
  if not (Decision is TTwoDimFloatDecision) then
    raise ETwoDimFloatDecision.Create('Invalid decision type...');
  Result := True;
  for i := 0 to GenesNumber - 1 do
  begin
    SelectedGene := i;
    for j := 0 to ParametersNumber - 1 do
    begin
      if Abs(TwoDimFloatDecision[j] - Self[j]) >= TINY then
      begin Result := False; Exit end;
    end;
  end;
end;

procedure TTwoDimFloatDecision.InvertParameter(
const GeneNum, ParamNum: LongInt);
begin
  SelectedGene := GeneNum;
  Self[ParamNum] := (-1) * Self[ParamNum];
end;

procedure TTwoDimFloatDecision.ExchangeParameters(
const GeneNum1, ParamNum1, GeneNum2, ParamNum2: LongInt);
var TempDouble1, TempDouble2: Double;
begin
  SelectedGene := GeneNum1;
  TempDouble1 := Self[ParamNum1];
  SelectedGene := GeneNum2;
  TempDouble2 := Self[ParamNum2];
  SelectedGene := GeneNum1;
  Self[ParamNum1] := TempDouble2;
  SelectedGene := GeneNum2;
  Self[ParamNum2] := TempDouble1;
end;

procedure TTwoDimFloatDecision.ExchangeWithOuter(
const Decision: TAbstractDecision; MyGeneNum, OuterGeneNum, ParamNum: LongInt);
var TempDouble: Double;
begin
  if not (Decision is TTwoDimFloatDecision) then
    raise ETwoDimFloatDecision.Create(
    'Invalid decision type ' + Decision.ClassName);

  Self.SelectedGene := MyGeneNum;
  TTwoDimFloatDecision(Decision).SelectedGene := OuterGeneNum;

  TempDouble := Self[ParamNum];
  Self[ParamNum] := TTwoDimFloatDecision(Decision)[ParamNum];
  TTwoDimFloatDecision(Decision)[ParamNum] := TempDouble;
end;

procedure TTwoDimFloatDecision.CopyParameter(
const SrcGeneNum, SrcParamNum, DestGeneNum, DestParamNum: LongInt);
var TempDouble: Double;
begin
  SelectedGene := SrcGeneNum;
  TempDouble := Self[SrcParamNum];
  SelectedGene := DestParamNum;
  Self[DestParamNum] := TempDouble;
end;

procedure TTwoDimFloatDecision.CopyBlock(const StartGeneNum, EndGeneNum,
StartParamNum, EndParamNum, GeneOffset, ParamOffset: LongInt);
var SavedBlock: array of array of Double;
    i, j: LongInt;
    Index1, Index2: LongInt;
begin
  (*poskol'ku oblast' naznacheniya mozhet perekryvat'sya s
  ishodnoy oblast'yu, to nuzhno predvaritel'no sohranit'
  kopiruemuyu informatsiyu v promezhutochnom massive*)
  SetLength(SavedBlock, EndGeneNum - StartGeneNum + 1);
  for i := 0 to Length(SavedBlock) - 1 do
    SetLength(SavedBlock[i], EndParamNum - StartParamNum + 1);

  for i := 0 to Length(SavedBlock) - 1 do
    for j := 0 to Length(SavedBlock[i]) - 1 do
      SavedBlock[i, j] := FParameters[i + StartGeneNum, j + StartParamNum];

  for i := 0 to Length(SavedBlock) - 1 do
    for j := 0 to Length(SavedBlock[i]) - 1 do
    begin
      Index1 := StartGeneNum + GeneOffset + i;
      Index2 := StartParamNum + ParamOffset + j;

      if Index1 > GenesNumber - 1 then Index1 := Index1 -
      GenesNumber * (Index1 div GenesNumber);

      if Index1 < 0 then Index1 := (GenesNumber - 1) +
      (Index1 + GenesNumber * (Abs(Index1) div GenesNumber));

      if Index2 > ParametersNumber - 1 then Index2 := Index2 -
      ParametersNumber * (Index2 div ParametersNumber);

      if Index2 < 0 then Index2 := (ParametersNumber - 1) +
      (Index2 + ParametersNumber * (Abs(Index2) div ParametersNumber));

      FParameters[Index1, Index2] := SavedBlock[i, j];
    end;

  for i := 0 to Length(SavedBlock) - 1 do Finalize(SavedBlock[i]);
  Finalize(SavedBlock);
end;

procedure TTwoDimDecision.ExchangeBlocksWithOuter(
const Decision: TAbstractDecision; StartGeneNum,
EndGeneNum, StartParamNum, EndParamNum: LongInt);
var i, j: LongInt;
begin
  for i := StartGeneNum to EndGeneNum do
    for j := StartParamNum to EndParamNum do
      ExchangeWithOuter(Decision, i, i, j);
end;

procedure TTwoDimDecision.InvertBlock(const StartGeneNum,
EndGeneNum, StartParamNum, EndParamNum: LongInt);
var i, j: LongInt;
begin
  for i := StartGeneNum to EndGeneNum do
    for j := StartParamNum to EndParamNum do
      InvertParameter(i, j);
end;

function TDecisionsList.GetMaxDecision(
const StartIndex: LongInt; UpLimit: Double): TAbstractDecision;
(*vozvraschaet reshenie, imeyuschee maksimal'noe znachenie Evaluation
sredi teh, u kot. znachenie Evaluation <= UpLimit, nachinaya so StartIndex*)
var i: LongInt;
    Decision: TAbstractDecision;
    Max: Double;
    Flag: Boolean;
begin
  Result := nil;
  if Count = 0 then
    raise EDecisionsList.Create('Decisions list should not be empty...');

  Flag := True;
  for i := StartIndex to Count - 1 do
  begin
    Decision := TAbstractDecision(Items[i]);
    if Flag then
      if Decision.Evaluation <= UpLimit then
      begin
        Max := Decision.Evaluation;
        Result := Decision;
        Flag := False;
        if Decision.Evaluation = UpLimit then Exit;
      end
    else
      if (Decision.Evaluation >= Max) and (Decision.Evaluation <= UpLimit) then
      begin
        Max := Decision.Evaluation;
        Result := Decision;
        if Decision.Evaluation = UpLimit then Exit;        
      end;
  end;
end;

function TDecisionsList.GetMinDecision(
const StartIndex: LongInt; LowLimit: Double): TAbstractDecision;
(*vozvraschaet reshenie, imeyuschee minimal'noe znachenie Evaluation
sredi teh, u kot. znachenie Evaluation >= LowLimit, nachinaya so StartIndex*)
var i: LongInt;
    Decision: TAbstractDecision;
    Min: Double;
    Flag: Boolean;
begin
  Result := nil;
  if Count = 0 then
    raise EDecisionsList.Create('Decisions list should not be empty...');

  Flag := True;
  for i := StartIndex to Count - 1 do
  begin
    Decision := TAbstractDecision(Items[i]);
    if Flag then
      if Decision.Evaluation >= LowLimit then
      begin
        Min := Decision.Evaluation;
        Result := Decision;
        Flag := False;
        if Decision.Evaluation = LowLimit then Exit;
      end
    else
      if (Decision.Evaluation <= Min) and (Decision.Evaluation >= LowLimit) then
      begin
        Min := Decision.Evaluation;
        Result := Decision;
        if Decision.Evaluation = LowLimit then Exit;
      end;
  end;
end;

function TDecisionsList.GetAbsoluteMin: TAbstractDecision;
var i: LongInt;
    Decision: TAbstractDecision;
begin
  Result := nil;
  if Count = 0 then
    raise EDecisionsList.Create('Decisions list should not be empty...');

  Decision := TAbstractDecision(Items[0]);
  Result := Decision;
  for i := 1 to Count - 1 do
  begin
    Decision := TAbstractDecision(Items[i]);
    if Decision.Evaluation < Result.Evaluation then
      Result := Decision;
  end;
end;

function TDecisionsList.GetAbsoluteMax: TAbstractDecision;
var i: LongInt;
    Decision: TAbstractDecision;
begin
  Result := nil;
  if Count = 0 then
    raise EDecisionsList.Create('Decisions list should not be empty...');

  Decision := TAbstractDecision(Items[0]);
  Result := Decision;
  for i := 1 to Count - 1 do
  begin
    Decision := TAbstractDecision(Items[i]);
    if Decision.Evaluation > Result.Evaluation then
      Result := Decision;
  end;
end;

function TDecisionsList.HasThisDecision(
const Decision: TAbstractDecision): Boolean;
var i: LongInt;
    TempDecision: TAbstractDecision;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    TempDecision := TAbstractDecision(Items[i]);
    if TempDecision.Coincide(Decision) then
    begin Result := True; Exit end;
  end;
end;

function EvalUpSortFunc(Item1, Item2: Pointer): Integer;
(*sortirovka po vozrastaniyu Evaluation*)
var Decision1: TAbstractDecision absolute Item1;
    Decision2: TAbstractDecision absolute Item2;
begin
  if Decision1.Evaluation > Decision2.Evaluation then Result := 1;
  if Decision1.Evaluation < Decision2.Evaluation then Result := -1;
  if Decision1.Evaluation = Decision2.Evaluation then Result := 0;
end;

function EvalDownSortFunc(Item1, Item2: Pointer): Integer;
(*sortirovka po ubyvaniyu Evaluation*)
var Decision1: TAbstractDecision absolute Item1;
    Decision2: TAbstractDecision absolute Item2;
begin
  if Decision1.Evaluation > Decision2.Evaluation then Result := -1;
  if Decision1.Evaluation < Decision2.Evaluation then Result := 1;
  if Decision1.Evaluation = Decision2.Evaluation then Result := 0;
end;

initialization
  RegisterClass(TFloatDecision);
  RegisterClass(TTwoDimFloatDecision);
  RegisterClass(TByteDecision);
  RegisterClass(TDecisionsList);
end.


