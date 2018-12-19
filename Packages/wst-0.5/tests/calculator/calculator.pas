unit calculator;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$DEFINE HAS_QWORD}
  {$DEFINE USE_INLINE}
{$ELSE}
  {$UNDEF HAS_QWORD}
  {$UNDEF USE_INLINE}
{$ENDIF}

interface
uses SysUtils,
     base_service_intf;

Type


  TCalc_Op = ( coAdd, coSub, coMul, coDiv );

  { TCalcHeader }

  TCalcHeader = class(THeaderBlock)
  private
    FLogin: string;
    FPassword: string;
    FWantedPrecision: Integer;
  published
    property Login : string read FLogin write FLogin;
    property Password : string read FPassword write FPassword;
    property WantedPrecision : Integer read FWantedPrecision write FWantedPrecision;
  end;

  { TCalcResultHeader }

  TCalcResultHeader = class(TCalcHeader)
  private
    FSessionID: string;
    FTimeStamp: string;
  published
    property TimeStamp : string read FTimeStamp write FTimeStamp;
    property SessionID : string read FSessionID write FSessionID;
  end;

  TBinaryArgsResult = class(TBaseComplexRemotable)
  private
    FArg_A: Integer;
    FArg_B: Integer;
    FArg_OP: string;
    FArg_OpEnum: TCalc_Op;
    FArg_R: Integer;
    FComment: string;
  private
    function GetHasComment: boolean;
  Published
    Property Arg_A : Integer Read FArg_A Write FArg_A;
    Property Arg_B : Integer Read FArg_B Write FArg_B;
    Property Arg_R : Integer Read FArg_R Write FArg_R;
    Property Arg_OP : string Read FArg_OP Write FArg_OP;
    Property Arg_OpEnum : TCalc_Op Read FArg_OpEnum Write FArg_OpEnum;
    property Comment : string read FComment write FComment stored GetHasComment;
  End;

  TBinaryArgsResultArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TBinaryArgsResult;
  Public
    class function GetItemClass():TBaseRemotableClass;override;
    Property Item[AIndex:Integer] : TBinaryArgsResult Read GetItem;Default;
  End;
  
  ICalculator = Interface
    function AddInt(
      Const A:Integer;
      Const B:Integer
    ):TBinaryArgsResult;
    function DivInt(
      Const A:Integer;
      Const B:Integer
    ):Integer;
    function DoAllOperations(
      Const A:Integer;
      Const B:Integer
    ):TBinaryArgsResultArray;
    function DoOperation(
      Const A:Integer;
      Const B:Integer;
      const AOperation : TCalc_Op
    ):TBinaryArgsResult;
  End;

implementation


{ TBinaryArgsResultArray }

function TBinaryArgsResultArray.GetItem(AIndex: Integer): TBinaryArgsResult;
begin
  Result := Inherited GetItem(AIndex) as TBinaryArgsResult;
end;

class function TBinaryArgsResultArray.GetItemClass(): TBaseRemotableClass;
begin
  Result := TBinaryArgsResult;
end;

{ TBinaryArgsResult }

function TBinaryArgsResult.GetHasComment: boolean;
begin
  Result := ( Length(Trim(FComment)) > 0 ) ;
end;

Initialization
  GetTypeRegistry().Register('urn:calculator',TypeInfo(TCalc_Op),'TCalc_Op');
  GetTypeRegistry().Register('urn:calculator',TypeInfo(TBinaryArgsResult),'TBinaryArgsResult');
  GetTypeRegistry().Register('urn:calculator',TypeInfo(TBinaryArgsResultArray),'TBinaryArgsResultArray');
  GetTypeRegistry().Register('urn:calculator',TypeInfo(TCalcHeader),'CalcHeader').AddPascalSynonym('TCalcHeader');
  GetTypeRegistry().Register('urn:calculator',TypeInfo(TCalcResultHeader),'CalcResultHeader').AddPascalSynonym('TCalcResultHeader');

end.
