{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit testformatter_unit;

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry,
{$ELSE}
  TestFrameWork,
{$ENDIF}
  TypInfo,
  base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  TTestEnum = ( teOne, teTwo, teThree, teFour );

  { TClass_A }

  TClass_A = class(TBaseComplexRemotable)
  private
    FVal_32S: LongInt;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
  Published
    property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
  End;

  { TClass_B }

  TClass_B = class(TBaseComplexRemotable)
  private
    FNonStored: Integer;
    FObjProp: TClass_A;
    FVal_32S: LongInt;
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
    procedure SetObjProp(const AValue: TClass_A);
  Public
    constructor Create();override;
    destructor Destroy();override;
  Published
    property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
    property ObjProp : TClass_A Read FObjProp Write SetObjProp stored True;
    property NonStored : Integer Read FNonStored Write FNonStored stored false;
  End;
  
  { TClass_Int }

  TClass_Int = class(TBaseComplexRemotable)
  private
    FVal_16S: SmallInt;
    FVal_16U: Word;
    FVal_32S: LongInt;
    FVal_32U: LongWord;
    FVal_64S: Int64;
    FVal_64U: QWord;
    FVal_8S: ShortInt;
    FVal_8U: Byte;
  Published
    property Val_8U : Byte Read FVal_8U Write FVal_8U;
      property Val_8S : ShortInt Read FVal_8S Write FVal_8S;
    property Val_16U : Word Read FVal_16U Write FVal_16U;
      property Val_16S : SmallInt Read FVal_16S Write FVal_16S;
    property Val_32U : LongWord Read FVal_32U Write FVal_32U;
      property Val_32S : LongInt Read FVal_32S Write FVal_32S;
    property Val_64U : QWord Read FVal_64U Write FVal_64U;
      property Val_64S : Int64 Read FVal_64S Write FVal_64S;
  End;

  T_ComplexInt64SContent = class(TComplexInt64SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt64UContent = class(TComplexInt64UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  { T_ComplexInt32SContent }

  T_ComplexInt32SContent = class(TComplexInt32SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt32UContent = class(TComplexInt32UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexInt16SContent = class(TComplexInt16SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexInt16UContent = class(TComplexInt16UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt8SContent = class(TComplexInt8SContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexInt8UContent = class(TComplexInt8UContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  T_ComplexFloatExtendedContent = class(TComplexFloatExtendedContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexFloatDoubleContent = class(TComplexFloatDoubleContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;

  T_ComplexStringContent = class(TComplexStringContentRemotable)
  private
    FBoolSimpleAtt_Exemple: Boolean;
    FIntSimpleAtt_Exemple: Integer;
    FStrSimpleAtt_Exemple: string;
  published
    property StrSimpleAtt_Exemple : string read FStrSimpleAtt_Exemple write FStrSimpleAtt_Exemple;
    property IntSimpleAtt_Exemple : Integer read FIntSimpleAtt_Exemple write FIntSimpleAtt_Exemple;
    property BoolSimpleAtt_Exemple : Boolean read FBoolSimpleAtt_Exemple write FBoolSimpleAtt_Exemple;
  end;
  
  { TClass_CplxSimpleContent }

  TClass_CplxSimpleContent = class(TBaseComplexRemotable)
  private
    FElt_Exemple: string;
    FVal_CplxDouble: T_ComplexFloatDoubleContent;
    FVal_CplxInt16S: T_ComplexInt16SContent;
    FVal_CplxInt16U: T_ComplexInt16UContent;
    FVal_CplxInt32S: T_ComplexInt32SContent;
    FVal_CplxInt32U: T_ComplexInt32UContent;
    FVal_CplxExtended : T_ComplexFloatExtendedContent;
    FVal_CplxInt64S: T_ComplexInt64SContent;
    FVal_CplxInt64U: T_ComplexInt64UContent;
    FVal_CplxInt8S: T_ComplexInt8SContent;
    FVal_CplxInt8U: T_ComplexInt8UContent;
    FVal_CplxString: T_ComplexStringContent;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Val_CplxInt64S : T_ComplexInt64SContent read FVal_CplxInt64S write FVal_CplxInt64S;
    property Val_CplxInt64U : T_ComplexInt64UContent read FVal_CplxInt64U write FVal_CplxInt64U;
    
    property Val_CplxInt32S : T_ComplexInt32SContent read FVal_CplxInt32S write FVal_CplxInt32S;
    property Val_CplxInt32U : T_ComplexInt32UContent read FVal_CplxInt32U write FVal_CplxInt32U;

    property Val_CplxInt16U : T_ComplexInt16UContent read FVal_CplxInt16U write FVal_CplxInt16U;
    property Val_CplxInt16S : T_ComplexInt16SContent read FVal_CplxInt16S write FVal_CplxInt16S;
    
    property Val_CplxInt8U : T_ComplexInt8UContent read FVal_CplxInt8U write FVal_CplxInt8U;
    property Val_CplxInt8S : T_ComplexInt8SContent read FVal_CplxInt8S write FVal_CplxInt8S;

    property Val_CplxExtended : T_ComplexFloatExtendedContent read FVal_CplxExtended write FVal_CplxExtended;
    property Val_CplxDouble : T_ComplexFloatDoubleContent read FVal_CplxDouble write FVal_CplxDouble;
    property Val_CplxString : T_ComplexStringContent read FVal_CplxString write FVal_CplxString;

    property Elt_Exemple : string read FElt_Exemple write FElt_Exemple;
  end;
  
  { TClass_Enum }

  TClass_Enum = class(TBaseComplexRemotable)
  private
    FVal_Bool: Boolean;
    FVal_Enum: TTestEnum;
    FVal_String: string;
  Published
    property Val_Enum : TTestEnum Read FVal_Enum Write FVal_Enum;
    property Val_Bool : Boolean Read FVal_Bool Write FVal_Bool;
    property Val_String : string Read FVal_String Write FVal_String;
  End;

  { TClass_Float }

  TClass_Float = class(TBaseComplexRemotable)
  private
    FVal_Currency: Currency;
    FVal_Double: Double;
    FVal_Extended: Extended;
    FVal_Single: Single;
  Published
    property Val_Single : Single Read FVal_Single Write FVal_Single;
    property Val_Double : Double Read FVal_Double Write FVal_Double;
    property Val_Extended : Extended Read FVal_Extended Write FVal_Extended;
    property Val_Currency : Currency Read FVal_Currency Write FVal_Currency;
  End;

  TEmbeddedArrayOfStringRemotable = class(TArrayOfStringRemotable);

  { TTestFormatterSimpleType }

  TTestFormatterSimpleType= class(TTestCase)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;virtual;abstract;
    function Support_ComplextType_with_SimpleContent():Boolean;virtual;
    function Support_nil():Boolean;virtual;
  published
    procedure Test_Int_8;
    procedure Test_Int_8_ScopeData;
    procedure Test_Int_16;
    procedure Test_Int_32;
    procedure Test_Int_64;

    procedure Test_Single_4;
    procedure Test_Double_8;
    procedure Test_Currency_8;
    procedure Test_Extended_10;

    procedure Test_String;
    procedure Test_Bool;
    procedure Test_Enum;
  end;
  
  { TTestFormatter }

  TTestFormatter= class(TTestFormatterSimpleType)
  published
    procedure Test_Int_WithClass;
    
    procedure Test_Float_WithClass;

    procedure Test_Enum_Bool_String_WithClass;
    
    procedure Test_CplxInt64SimpleContent_WithClass;
    procedure Test_CplxInt32SimpleContent_WithClass;
    procedure Test_CplxInt16SimpleContent_WithClass;
    procedure Test_CplxInt8SimpleContent_WithClass;
    
    procedure Test_CplxFloatExtendedSimpleContent_WithClass;
    procedure Test_CplxStringSimpleContent_WithClass;
    
    procedure Test_Object();
    procedure Test_Object_Nil();
    procedure Test_StringArray();
    procedure Test_StringArray_Embedded();
    procedure Test_StringArrayZeroLength();
    procedure Test_BooleanArray();
    
    procedure Test_Int8UArray();
    procedure Test_Int8SArray();
    
    procedure Test_Int16SArray();
    procedure Test_Int16UArray();
    
    procedure Test_Int32UArray();
    procedure Test_Int32SArray();
    
    procedure Test_Int64SArray();
    procedure Test_Int64UArray();
    
    procedure Test_FloatSingleArray();
    procedure Test_FloatDoubleArray();
    procedure Test_FloatExtendedArray();
    procedure Test_FloatCurrencyArray();
    
    procedure  Test_ComplexInt32S();
  end;

  { TTestBinaryFormatter }

  TTestBinaryFormatter= class(TTestFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestBinaryFormatterAttributes }

  TTestBinaryFormatterAttributes= class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestSOAPFormatter }

  TTestSOAPFormatter= class(TTestFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestSOAPFormatterAttributes }

  TTestSOAPFormatterAttributes = class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;

  { TTestXmlRpcFormatterAttributes }

  TTestXmlRpcFormatterAttributes = class(TTestFormatterSimpleType)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
  end;
  
  TTestXmlRpcFormatter= class(TTestFormatter)
  protected
    function CreateFormatter(ARootType : PTypeInfo):IFormatterBase;override;
    function Support_ComplextType_with_SimpleContent():Boolean;override;
    function Support_nil():Boolean;override;
  end;
  
  { TTestArray }

  TTestArray= class(TTestCase)
  published
    procedure Test_StringArray();
    procedure Test_BooleanArray();
    
    procedure Test_Int8UArray();
    procedure Test_Int8SArray();
    
    procedure Test_Int16SArray();
    procedure Test_Int16UArray();
    
    procedure Test_Int32UArray();
    procedure Test_Int32SArray();
    
    procedure Test_Int64SArray();
    procedure Test_Int64UArray();
    
    procedure Test_FloatSingleArray();
    procedure Test_FloatDoubleArray();
    procedure Test_FloatExtendedArray();
    procedure Test_FloatCurrencyArray();
  end;

  { TTest_TBaseComplexRemotable }

  TTest_TBaseComplexRemotable= class(TTestCase)
  published
    procedure Test_Assign();
  end;

  { TTest_TDateRemotable }

  TTest_TDateRemotable = class(TTestCase)
  published
    procedure FormatDate();
    procedure ParseDate();
  end;

  { TTest_TDurationRemotable }

  TTest_TDurationRemotable = class(TTestCase)
  published
    procedure FormatDate();
    procedure ParseDate();
  end;

  { TTest_TTimeRemotable }

  TTest_TTimeRemotable = class(TTestCase)
  published
    procedure FormatDate();
    procedure ParseDate();
  end;
  
implementation
uses base_binary_formatter, base_soap_formatter, base_xmlrpc_formatter;

function TTestFormatterSimpleType.Support_ComplextType_with_SimpleContent( ): Boolean;
begin
  Result := True;
end;

function TTestFormatterSimpleType.Support_nil(): Boolean;
begin
  Result := True;
end;

procedure TTestFormatterSimpleType.Test_Int_8;
const VAL_1 = 12; VAL_2 = -10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Byte; intVal_S : ShortInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(Byte),intVal_U);
      f.Put('intVal_S',TypeInfo(ShortInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.xml');
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(Byte),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(ShortInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_8_ScopeData;
const VAL_1 = 12; VAL_2 = -10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Byte; intVal_S : ShortInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Int));
    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.PutScopeInnerValue(TypeInfo(Byte),intVal_U);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      f.GetScopeInnerValue(TypeInfo(Byte),intVal_U);
    f.EndScopeRead();
    CheckEquals(VAL_1,intVal_U);
    ///
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));
    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.PutScopeInnerValue(TypeInfo(ShortInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      f.GetScopeInnerValue(TypeInfo(ShortInt),intVal_S);
    f.EndScopeRead();
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_16;
const VAL_1 = 1210; VAL_2 : SmallInt = -1012;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : Word; intVal_S : SmallInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(Word),intVal_U);
      f.Put('intVal_S',TypeInfo(SmallInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(Word),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(SmallInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_32;
const VAL_1 = 121076; VAL_2 : LongInt = -101276;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : LongWord; intVal_S : LongInt;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(LongWord),intVal_U);
      f.Put('intVal_S',TypeInfo(LongInt),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(LongWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(LongInt),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Int_64;
const VAL_1 = 121076; VAL_2 : Int64 = -101276;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_U : QWord; intVal_S : Int64;
begin
  s := Nil;
  Try
    intVal_U := VAL_1;
    intVal_S := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_U',TypeInfo(QWord),intVal_U);
      f.Put('intVal_S',TypeInfo(Int64),intVal_S);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_U := 0;
    intVal_S := 0;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_U';
      f.Get(TypeInfo(QWord),x,intVal_U);
      x := 'intVal_S';
      f.Get(TypeInfo(Int64),x,intVal_S);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_U);
    CheckEquals(VAL_2,intVal_S);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Single_4;
const VAL_1 : single = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Single;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Single),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Single_4.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Single),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);//,0.00001);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Double_8;
const VAL_1 : Double = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Double;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Double),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Double_8.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Double),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Currency_8;
const VAL_1 : Currency = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Currency;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Currency),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Currency_8.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Currency),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Extended_10;
const VAL_1 : Extended = 12.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  tmpVal : Extended;
begin
  s := Nil;
  Try
    tmpVal := VAL_1;
    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('tmpVal',TypeInfo(Extended),tmpVal);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);s.SaveToFile(ClassName + '.Test_Extended_10.xml');
    tmpVal := 0;

    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'tmpVal';
      f.Get(TypeInfo(Extended),x,tmpVal);
    f.EndScopeRead();

    CheckEquals(VAL_1,tmpVal);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_String;
const VAL_1 = 'AzErTy'; VAL_2 = 'QwErTy';
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : string; intVal_3 : string;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_1',TypeInfo(string),intVal_1);
      f.Put('intVal_3',TypeInfo(string),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := '';
    intVal_3 := 'yyyyyyyy';

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(string),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(string),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_1);
    CheckEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Bool;
const VAL_1 = True; VAL_2 = False;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : Boolean; intVal_3 : Boolean;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('intVal_1',TypeInfo(Boolean),intVal_1);
      f.Put('intVal_3',TypeInfo(Boolean),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := False;
    intVal_3 := True;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(Boolean),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(Boolean),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(VAL_1,intVal_1);
    CheckEquals(VAL_2,intVal_3);
  Finally
    s.Free();
  End;
end;

procedure TTestFormatterSimpleType.Test_Enum;
const VAL_1 = teTwo; VAL_2 = teFour;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  intVal_1 : TTestEnum; intVal_3 : TTestEnum;
begin
  s := Nil;
  Try
    intVal_1 := VAL_1;
    intVal_3 := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Enum));
      f.Put('intVal_1',TypeInfo(TTestEnum),intVal_1);
      f.Put('intVal_3',TypeInfo(TTestEnum),intVal_3);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    intVal_1 := teOne;
    intVal_3 := teOne;

    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'intVal_1';
      f.Get(TypeInfo(TTestEnum),x,intVal_1);
      x := 'intVal_3';
      f.Get(TypeInfo(TTestEnum),x,intVal_3);
    f.EndScopeRead();

    CheckEquals(Ord(VAL_1),Ord(intVal_1));
    CheckEquals(Ord(VAL_2),Ord(intVal_3));
  Finally
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Int_WithClass;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Int;
  x : string;
begin
  s := Nil;
  a := TClass_Int.Create();
  Try
    a.Val_8U := 8;
      a.Val_8S := -8;
    a.Val_16U := 16;
      a.Val_16S := -16;
    a.Val_32U := 32;
      a.Val_32S := -32;
    a.Val_64U := 64;
      a.Val_64S := -64;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_Int),a);
    f.EndScope();
    
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '_test_int_withclass.xml');
    FreeAndNil(a);
    
    a := TClass_Int.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_Int),x,a);
    f.EndScopeRead();
    
    CheckEquals(8,a.Val_8U);
      CheckEquals(-8,a.Val_8S);
    CheckEquals(16,a.Val_16U);
      CheckEquals(-16,a.Val_16S);
    CheckEquals(32,a.Val_32U);
      CheckEquals(-32,a.Val_32S);
    CheckEquals(64,a.Val_64U);
      CheckEquals(-64,a.Val_64S);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Float_WithClass;
const VAL_CUR : Currency = 8.8;
      VAL_DBL : Double = 8.8;
      VAL_SGL : Single = 4.4;
      VAL_EXT : Extended = 10.10;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Float;
  x : string;
begin
  s := Nil;
  a := TClass_Float.Create();
  Try
    a.Val_Currency := VAL_CUR;
    a.Val_Double := VAL_DBL;
    a.Val_Extended := VAL_EXT;
    a.Val_Single := VAL_SGL;

    f := CreateFormatter(TypeInfo(TClass_Float));

    f.BeginObject('Root',TypeInfo(TClass_Float));
      f.Put('o1',TypeInfo(TClass_Float),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_Float.Create();
    f := CreateFormatter(TypeInfo(TClass_Float));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Float));
      x := 'o1';
      f.Get(TypeInfo(TClass_Float),x,a);
    f.EndScopeRead();

    CheckEquals(VAL_SGL,a.Val_Single);
    CheckEquals(VAL_DBL,a.Val_Double);
    CheckEquals(VAL_CUR,a.Val_Currency);
    CheckEquals(VAL_EXT,a.Val_Extended);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Enum_Bool_String_WithClass;
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_Enum;
  x : string;
begin
  s := Nil;
  a := TClass_Enum.Create();
  Try
    a.Val_Bool := True;
    a.Val_Enum := teThree;
    a.Val_String := 'atou';
    f := CreateFormatter(TypeInfo(TClass_Enum));

    f.BeginObject('Root',TypeInfo(TClass_Enum));
      f.Put('o1',TypeInfo(TClass_Enum),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_Enum.Create();
    f := CreateFormatter(TypeInfo(TClass_Enum));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Enum));
      x := 'o1';
      f.Get(TypeInfo(TClass_Enum),x,a);
    f.EndScopeRead();

    CheckEquals(True,a.Val_Bool);
    CheckEquals(Ord(teThree),Ord(a.Val_Enum));
    CheckEquals('atou',a.Val_String);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_CplxInt64SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 121;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt64SContentRemotable;
  nu : TComplexInt64UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;
    
  s := nil;
  ns := TComplexInt64SContentRemotable.Create();
  nu := TComplexInt64UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt64S.Value := VAL_S;
    a.Val_CplxInt64S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt64S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt64S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt64U.Value := VAL_U;
    a.Val_CplxInt64U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt64U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt64U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt64SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt64UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt64SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt64UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt64S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt64S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt64S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt64S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt64U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt64U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt64U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt64U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt32SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 1210;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt32SContentRemotable;
  nu : TComplexInt32UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt32SContentRemotable.Create();
  nu := TComplexInt32UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt32S.Value := VAL_S;
    a.Val_CplxInt32S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt32S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt32S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt32U.Value := VAL_U;
    a.Val_CplxInt32U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt32U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt32U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt32SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt32UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt32UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt32S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt32S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt32S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt32S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);
    
    CheckEquals(VAL_U,a.Val_CplxInt32U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt32U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt32U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt32U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt16SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 1210;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt16SContentRemotable;
  nu : TComplexInt16UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt16SContentRemotable.Create();
  nu := TComplexInt16UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt16S.Value := VAL_S;
    a.Val_CplxInt16S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt16S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt16S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt16U.Value := VAL_U;
    a.Val_CplxInt16U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt16U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt16U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt16SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt16UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt16SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt16UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt16S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt16S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt16S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt16S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt16U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt16U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt16U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt16U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxInt8SimpleContent_WithClass;
const VAL_S = -12; VAL_U = 10; VAL_X = 121;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexInt8SContentRemotable;
  nu : TComplexInt8UContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexInt8SContentRemotable.Create();
  nu := TComplexInt8UContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxInt8S.Value := VAL_S;
    a.Val_CplxInt8S.StrSimpleAtt_Exemple := VAL_STR_S;
    a.Val_CplxInt8S.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt8S.BoolSimpleAtt_Exemple := True;
    a.Elt_Exemple := VAL_STR_X;
    ns.Value := VAL_S;

    a.Val_CplxInt8U.Value := VAL_U;
    a.Val_CplxInt8U.StrSimpleAtt_Exemple := VAL_STR_U;
    a.Val_CplxInt8U.IntSimpleAtt_Exemple := VAL_X;
    a.Val_CplxInt8U.BoolSimpleAtt_Exemple := False;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexInt8SContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexInt8UContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexInt8SContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexInt8UContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxInt8S.Value);
    CheckEquals(VAL_X,a.Val_CplxInt8S.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_S,a.Val_CplxInt8S.StrSimpleAtt_Exemple);
    CheckEquals(True,a.Val_CplxInt8S.BoolSimpleAtt_Exemple);
    CheckEquals(VAL_STR_X,a.Elt_Exemple);

    CheckEquals(VAL_U,a.Val_CplxInt8U.Value);
    CheckEquals(VAL_X,a.Val_CplxInt8U.IntSimpleAtt_Exemple);
    CheckEquals(VAL_STR_U,a.Val_CplxInt8U.StrSimpleAtt_Exemple);
    CheckEquals(False,a.Val_CplxInt8U.BoolSimpleAtt_Exemple);
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxFloatExtendedSimpleContent_WithClass;
const VAL_S : Extended = -12.10; VAL_U : Double = 10.76; VAL_X = 1210.76;
      VAL_STR_S = 'Test Attribute S'; VAL_STR_U = 'Test Attribute U'; VAL_STR_X = 'test it';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexFloatExtendedContentRemotable;
  nu : TComplexFloatDoubleContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexFloatExtendedContentRemotable.Create();
  nu := TComplexFloatDoubleContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxExtended := T_ComplexFloatExtendedContent.Create();
    a.Val_CplxExtended.Value := VAL_S;
    a.Val_CplxDouble := T_ComplexFloatDoubleContent.Create();
    a.Val_CplxDouble.Value := VAL_U;
    
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;

    ns.Value := VAL_S;
    nu.Value := VAL_U;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexFloatExtendedContentRemotable),ns);
      f.Put('nu',TypeInfo(TComplexFloatDoubleContentRemotable),nu);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;
    a.Val_CplxExtended := T_ComplexFloatExtendedContent.Create();
    a.Val_CplxDouble := T_ComplexFloatDoubleContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexFloatExtendedContentRemotable),x,ns);
      x := 'nu';
      f.Get(TypeInfo(TComplexFloatDoubleContentRemotable),x,nu);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxExtended.Value,'VAL_S <> a.Val_CplxExtended.Value');
    CheckEquals(VAL_U,a.Val_CplxDouble.Value,'VAL_S <> a.Val_CplxDouble.Value');
    CheckEquals(VAL_S,ns.Value,'VAL_S <> ns.Value');
    CheckEquals(VAL_U,nu.Value,'VAL_U <> nu.Value');
    CheckNull(a.Val_CplxInt32S,'a.Val_CplxInt32S <> nil');
    CheckNull(a.Val_CplxInt32U,'a.Val_CplxInt32U <> nil');
  finally
    FreeAndNil(nu);
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_CplxStringSimpleContent_WithClass;
const VAL_S = 'web services toolkit';
      VAL_STR_S = 'Test Attribute S';
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_CplxSimpleContent;
  ns : TComplexStringContentRemotable;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  ns := TComplexStringContentRemotable.Create();
  a := TClass_CplxSimpleContent.Create();
  try
    a.Val_CplxString := T_ComplexStringContent.Create();
    a.Val_CplxString.Value := VAL_S;
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;

    ns.Value := VAL_STR_S;

    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('o1',TypeInfo(TClass_CplxSimpleContent),a);
      f.Put('ns',TypeInfo(TComplexStringContentRemotable),ns);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.txt');
    FreeAndNil(a);

    a := TClass_CplxSimpleContent.Create();
    a.Val_CplxInt32S.Free();
    a.Val_CplxInt32S := nil;
    a.Val_CplxInt32U.Free();
    a.Val_CplxInt32U := nil;
    a.Val_CplxString := T_ComplexStringContent.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'o1';
      f.Get(TypeInfo(TClass_CplxSimpleContent),x,a);
      x := 'ns';
      f.Get(TypeInfo(TComplexStringContentRemotable),x,ns);
    f.EndScopeRead();

    CheckEquals(VAL_S,a.Val_CplxString.Value,'VAL_S <> a.Val_CplxString.Value');
    CheckEquals(VAL_STR_S,ns.Value,'VAL_STR_S <> ns.Value');
  finally
    FreeAndNil(ns);
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Object();
Var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_B;
  x : string;
begin
  s := Nil;
  a := TClass_B.Create();
  Try
    a.Val_Bool := False;
    a.Val_Enum := teThree;
    a.Val_String := '123';
    a.ObjProp.Val_String := '456';
    a.ObjProp.Val_Enum := teFour;
    a.ObjProp.Val_Bool := True;
    a.ObjProp.Val_32S := 121076;
    a.NonStored := 121076;
    
    f := CreateFormatter(TypeInfo(TClass_B));

    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('o1',TypeInfo(TClass_B),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := TClass_B.Create();
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'o1';
      f.Get(TypeInfo(TClass_B),x,a);
    f.EndScopeRead();

    CheckEquals(False,a.Val_Bool);
    CheckEquals(Ord(teThree),Ord(a.Val_Enum));
    CheckEquals('123',a.Val_String);
    
    CheckEquals(True,a.ObjProp.Val_Bool);
    CheckEquals(Ord(teFour),Ord(a.ObjProp.Val_Enum));
    CheckEquals('456',a.ObjProp.Val_String);
    CheckEquals(121076,a.ObjProp.Val_32S);
    
    CheckEquals(0,a.NonStored);
  Finally
    a.Free();
    s.Free();
  End;
end;

procedure TTestFormatter.Test_Object_Nil();
var
  f : IFormatterBase;
  s : TMemoryStream;
  a : TClass_B;
  x : string;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  a := nil;
  try
    f := CreateFormatter(TypeInfo(TClass_B));

    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('o1',TypeInfo(TClass_B),a);
    f.EndScope();

    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    a := nil;
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'o1';
      f.Get(TypeInfo(TClass_B),x,a);
    f.EndScopeRead();

    CheckNull(a);
  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length, 'Length 1');

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.Test_StringArray.xml');
    FreeAndNil(a);
    a := TArrayOfStringRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length, 'Length 2');

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArray_Embedded();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  b : TEmbeddedArrayOfStringRemotable;
  i, intVal : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  b := nil;
  a := TArrayOfStringRemotable.Create();
  try
    b := TEmbeddedArrayOfStringRemotable.Create();
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length, 'Length 1');

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    b.SetLength(AR_LEN);
    CheckEquals(AR_LEN,b.Length);

    for i := 0 to Pred(AR_LEN) do begin
      a[i] := VAL_AR[i];
      b[i] := VAL_AR[Pred(AR_LEN)-i];
    end;

    intVal := 1210;
    
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
      f.Put('x',TypeInfo(Integer),intVal);
      f.Put('b',TypeInfo(TEmbeddedArrayOfStringRemotable),b);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s); s.SaveToFile(ClassName + '.XML');
    FreeAndNil(a);
    FreeAndNil(b);
    intVal := 0;
    a := TArrayOfStringRemotable.Create();
    b := TEmbeddedArrayOfStringRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    b.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
      x := 'x';
      f.Get(TypeInfo(Integer),x,intVal);
      x := 'b';
      f.Get(TypeInfo(TEmbeddedArrayOfStringRemotable),x,b);
    f.EndScopeRead();
    CheckEquals(1210,intVal, 'IntVal');
    CheckEquals(AR_LEN,a.Length, 'Length 2 a');
    CheckEquals(AR_LEN,b.Length,'Length 2 b');

    for i := 0 to Pred(AR_LEN) do begin
      CheckEquals(VAL_AR[i],a[i]);
      CheckEquals(VAL_AR[Pred(AR_LEN)-i],b[i]);
    end;

  finally
    b.Free();
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_StringArrayZeroLength();
var
  a : TArrayOfStringRemotable;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(0,a.Length);

    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfStringRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);

    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfStringRemotable),x,a);
    f.EndScopeRead();
    CheckNotNull(a);
    CheckEquals(0,a.Length);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_BooleanArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Boolean = (True,True,False,True,False);
var
  a : TArrayOfBooleanRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfBooleanRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfBooleanRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfBooleanRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfBooleanRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int8UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Byte = (12,34,100,200,180);
var
  a : TArrayOfInt8URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt8URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt8URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt8URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,120,110);
var
  a : TArrayOfInt8SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt8SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt8SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt8SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int16SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of SmallInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt16SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt16SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt16SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt16SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int16UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Word = (12,34,100,200,180);
var
  a : TArrayOfInt16URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt16URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt16URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt16URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt16URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int32UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongWord = (12,34,100,200,180);
var
  a : TArrayOfInt32URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt32URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt32URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt32URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(True,VAL_AR[i] = a[i],Format('VAL_AR[%d] = a[%d]',[i,i]));

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int32SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt32SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt32SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt32SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt32SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt32SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int64SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Int64 = (-12,-34,100,200,180);
var
  a : TArrayOfInt64SRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt64SRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt64SRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt64SRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64SRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      CheckEquals(VAL_AR[i],a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_Int64UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of QWord = (12,34,100,200,180);
var
  a : TArrayOfInt64URemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfInt64URemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfInt64URemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfInt64URemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfInt64URemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatSingleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Single = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatSingleRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatSingleRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatSingleRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatSingleRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatSingleRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatDoubleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Double = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatDoubleRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatDoubleRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatDoubleRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatDoubleRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatDoubleRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatExtendedArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Extended = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatExtendedRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatExtendedRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatExtendedRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatExtendedRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatExtendedRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_FloatCurrencyArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Currency = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatCurrencyRemotable;
  i : Integer;
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
begin
  a := TArrayOfFloatCurrencyRemotable.Create();
  try
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    a.SetLength(AR_LEN);
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      a[i] := VAL_AR[i];
    f := CreateFormatter(TypeInfo(TClass_B));
    f.BeginObject('Root',TypeInfo(TClass_B));
      f.Put('a',TypeInfo(TArrayOfFloatCurrencyRemotable),a);
    f.EndScope();
    s := TMemoryStream.Create();
    f.SaveToStream(s);
    FreeAndNil(a);
    a := TArrayOfFloatCurrencyRemotable.Create();
    a.SetLength(0);
    a.SetLength(0);
    a.SetLength(0);
    f := CreateFormatter(TypeInfo(TClass_B));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_B));
      x := 'a';
      f.Get(TypeInfo(TArrayOfFloatCurrencyRemotable),x,a);
    f.EndScopeRead();
    CheckEquals(AR_LEN,a.Length);

    for i := 0 to Pred(AR_LEN) do
      Check(VAL_AR[i]=a[i]);

  finally
    a.Free();
    s.Free();
  end;
end;

procedure TTestFormatter.Test_ComplexInt32S();
const VAL_1 = 121076; VAL_2 : LongInt = -101276;
var
  f : IFormatterBase;
  s : TMemoryStream;
  x : string;
  a, b : TComplexInt32SContentRemotable;
begin
  if not Support_ComplextType_with_SimpleContent() then
    Exit;

  s := nil;
  a := nil;
  b := nil;
  try
    a := TComplexInt32SContentRemotable.Create();
    b := TComplexInt32SContentRemotable.Create();
    a.Value := VAL_1;
    b.Value := VAL_2;
    f := CreateFormatter(TypeInfo(TClass_Int));

    f.BeginObject('Root',TypeInfo(TClass_Int));
      f.Put('a',TypeInfo(TComplexInt32SContentRemotable),a);
      f.Put('b',TypeInfo(TComplexInt32SContentRemotable),b);
    f.EndScope();
    FreeAndNil(a);FreeAndNil(b);
    s := TMemoryStream.Create();
    f.SaveToStream(s);

    a := TComplexInt32SContentRemotable.Create();
    b := TComplexInt32SContentRemotable.Create();
    f := CreateFormatter(TypeInfo(TClass_Int));
    s.Position := 0;
    f.LoadFromStream(s);
    x := 'Root';
    f.BeginObjectRead(x,TypeInfo(TClass_Int));
      x := 'a';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,a);
      x := 'b';
      f.Get(TypeInfo(TComplexInt32SContentRemotable),x,b);
    f.EndScopeRead();

    CheckEquals(VAL_1,a.Value);
    CheckEquals(VAL_2,b.Value);
  finally
    s.Free();
    FreeAndNil(a);
    FreeAndNil(b);
  end;
end;


{ TTestBinaryFormatter }

function TTestBinaryFormatter.CreateFormatter(ARootType : PTypeInfo):IFormatterBase;
begin
  Result := TBaseBinaryFormatter.Create() as IFormatterBase;
  //Result.BeginObject('root',Nil);
end;

{ TTestSOAPFormatter }

function TTestSOAPFormatter.CreateFormatter(ARootType : PTypeInfo):IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType)
end;

{ TClass_B }

procedure TClass_B.SetObjProp(const AValue: TClass_A);
begin
  FObjProp.Assign(AValue);
end;

constructor TClass_B.Create();
begin
  inherited Create();
  FObjProp := TClass_A.Create();
end;

destructor TClass_B.Destroy();
begin
  FreeAndNil(FObjProp);
  inherited Destroy();
end;

{ TTestArray }

procedure TTestArray.Test_StringArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of string = ('AzErTy','QwErTy','123456','','1');
var
  a : TArrayOfStringRemotable;
  i, j : Integer;
begin
  a := TArrayOfStringRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(ansistring))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(ansistring))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');
    
    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_BooleanArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Boolean = (True,True,False,True,False);
var
  a : TArrayOfBooleanRemotable;
  i, j : Integer;
begin
  a := TArrayOfBooleanRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Boolean))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Boolean))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int8UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Byte = (12,34,100,200,180);
var
  a : TArrayOfInt8URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt8URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Byte))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Byte))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int8SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of ShortInt = (-12,-34,100,120,110);
var
  a : TArrayOfInt8SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt8SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(ShortInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(ShortInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int16SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of SmallInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt16SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt16SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(SmallInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(SmallInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int16UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Word = (12,4,100,200,180);
var
  a : TArrayOfInt16URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt16URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Word))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Word))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int32UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongWord = (12,4,100,200,180);
var
  a : TArrayOfInt32URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt32URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(LongWord))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(LongWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int32SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of LongInt = (-12,-34,100,200,180);
var
  a : TArrayOfInt32SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt32SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(LongInt))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(LongInt))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int64SArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Int64 = (-12,-34,100,200,180);
var
  a : TArrayOfInt64SRemotable;
  i, j : Integer;
begin
  a := TArrayOfInt64SRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Int64))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Int64))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        CheckEquals(VAL_AR[j],a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_Int64UArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of QWord = (12,4,100,200,180);
var
  a : TArrayOfInt64URemotable;
  i, j : Integer;
begin
  a := TArrayOfInt64URemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(QWord))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(QWord))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatSingleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Single = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatSingleRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatSingleRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Single))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Single))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatDoubleArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Double = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatDoubleRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatDoubleRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Double))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Double))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatExtendedArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Extended = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatExtendedRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatExtendedRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Extended))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Extended))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

procedure TTestArray.Test_FloatCurrencyArray();
const AR_LEN = 5; VAL_AR : array[0..(AR_LEN-1)] of Currency = (-12.10,-4.45,100,200.58,180.3);
var
  a : TArrayOfFloatCurrencyRemotable;
  i, j : Integer;
begin
  a := TArrayOfFloatCurrencyRemotable.Create();
  try
    CheckEquals(PTypeInfo(TypeInfo(Currency))^.Name,a.GetItemTypeInfo()^.Name,'TypeInfo');
    CheckEquals(Ord(PTypeInfo(TypeInfo(Currency))^.Kind),Ord(a.GetItemTypeInfo()^.Kind),'TypeInfo');

    CheckEquals(0,a.Length);
    a.SetLength(0);
    CheckEquals(0,a.Length);

    for i := 1 to AR_LEN do begin
      a.SetLength(i);
      CheckEquals(i,a.Length,'Length');
      for j := 0 to Pred(i) do
        a[j] := VAL_AR[j];
      for j := 0 to Pred(i) do
        Check(VAL_AR[j]=a[j],'Item');
    end;

    a.SetLength(0);
    CheckEquals(0,a.Length);
  finally
    a.Free();
  end;
end;

{ TTest_TBaseComplexRemotable }

procedure TTest_TBaseComplexRemotable.Test_Assign();
Var
  a,b : TClass_B;
begin
  b := nil;
  a := TClass_B.Create();
  Try
    a.Val_Bool := False;
    a.Val_Enum := teThree;
    a.Val_String := '123';
    a.ObjProp.Val_String := '456';
    a.ObjProp.Val_Enum := teFour;
    a.ObjProp.Val_Bool := True;
    a.ObjProp.Val_32S := 121076;
    a.NonStored := 121076;

    b := TClass_B.Create();
    
    b.Assign(a);

    CheckEquals(a.Val_Bool,b.Val_Bool);
    CheckEquals(Ord(a.Val_Enum),Ord(b.Val_Enum));
    CheckEquals(a.Val_String,b.Val_String);

    CheckEquals(a.ObjProp.Val_Bool,b.ObjProp.Val_Bool);
    CheckEquals(Ord(a.ObjProp.Val_Enum),Ord(b.ObjProp.Val_Enum));
    CheckEquals(a.ObjProp.Val_String,a.ObjProp.Val_String);
  Finally
    a.Free();
    b.Free();
  End;
end;

{ TTestSOAPFormatterAttributes }

function TTestSOAPFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

{ TTestBinaryFormatterAttributes }

function TTestBinaryFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TSOAPBaseFormatter.Create() as IFormatterBase;
  Result.BeginObject('Env',ARootType);
  Result.SetSerializationStyle(ssAttibuteSerialization);
end;

{ TClass_CplxSimpleContent }

constructor TClass_CplxSimpleContent.Create();
begin
  FVal_CplxInt64S := T_ComplexInt64SContent.Create();
    FVal_CplxInt64U := T_ComplexInt64UContent.Create();
  FVal_CplxInt32S := T_ComplexInt32SContent.Create();
    FVal_CplxInt32U := T_ComplexInt32UContent.Create();
  FVal_CplxInt16S := T_ComplexInt16SContent.Create();
    FVal_CplxInt16U := T_ComplexInt16UContent.Create();
  FVal_CplxInt8S := T_ComplexInt8SContent.Create();
    FVal_CplxInt8U := T_ComplexInt8UContent.Create();
end;

destructor TClass_CplxSimpleContent.Destroy();
begin
  FreeAndNil(FVal_CplxInt64S);
    FreeAndNil(FVal_CplxInt64U);
  FreeAndNil(FVal_CplxInt32U);
    FreeAndNil(FVal_CplxInt32S);
  FreeAndNil(FVal_CplxInt16U);
    FreeAndNil(FVal_CplxInt16S);
  FreeAndNil(FVal_CplxInt8U);
    FreeAndNil(FVal_CplxInt8S);
  inherited Destroy();
end;

{ TTest_TDateRemotable }

procedure TTest_TDateRemotable.FormatDate();
const sDATE = '1976-10-12T23:34:56';
var
  d : TDateTime;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  d := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  Check(AnsiPos(TDateRemotable.FormatDate(d),sDATE) = 1);
end;

procedure TTest_TDateRemotable.ParseDate();
const sDATE = '1976-10-12T23:34:56';
var
  s : string;
  objd : TDateRemotable;
  d : TDateTime;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56';
  d := TDateRemotable.ParseDate(s);
  DecodeDate(d,y,m,dy);
    CheckEquals(y,1976,'Year');
    CheckEquals(m,10,'Month');
    CheckEquals(dy,12,'Day');

  DecodeTime(d,hh,mn,ss,ssss);
    CheckEquals(hh,23,'Hour');
    CheckEquals(mn,34,'Minute');
    CheckEquals(ss,56,'Second');

  objd := TDateRemotable.Create();
  try
    objd.AsDate := d;
    CheckEquals(objd.Year,1976,'Year');
    CheckEquals(objd.Month,10,'Month');
    CheckEquals(objd.Day,12,'Day');
    CheckEquals(objd.Hour,23,'Hour');
    CheckEquals(objd.Minute,34,'Minute');
    CheckEquals(objd.Second,56,'Second');
  finally
    FreeAndNil(objd);
  end;
end;

{ TTest_TDurationRemotable }

procedure TTest_TDurationRemotable.FormatDate();
begin
  Fail('Write me!');
end;

procedure TTest_TDurationRemotable.ParseDate();
begin
  Fail('Write me!');
end;

{ TTest_TTimeRemotable }

procedure TTest_TTimeRemotable.FormatDate();
begin
  Fail('Write me!');
end;

procedure TTest_TTimeRemotable.ParseDate();
begin
  Fail('Write me!');
end;

{ TTestXmlRpcFormatterAttributes }

function TTestXmlRpcFormatterAttributes.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TXmlRpcBaseFormatter.Create() as IFormatterBase;
  //Result.BeginObject('Env',ARootType)
end;

{ TTestXmlRpcFormatter }

function TTestXmlRpcFormatter.CreateFormatter(ARootType: PTypeInfo): IFormatterBase;
begin
  Result := TXmlRpcBaseFormatter.Create() as IFormatterBase;
end;

function TTestXmlRpcFormatter.Support_ComplextType_with_SimpleContent(): Boolean;
begin
  Result := False;
end;

function TTestXmlRpcFormatter.Support_nil(): Boolean;
begin
  Result := False;
end;

initialization
  RegisterStdTypes();
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TTestEnum),'TTestEnum').RegisterExternalPropertyName('teOne', '1');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Int),'TClass_Int').RegisterExternalPropertyName('Val_8U','U8');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Enum),'TClass_Enum');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_A),'TClass_A');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_B),'TClass_B');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_Float),'TClass_Float');

  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt32SContent),'T_ComplexInt32SContent');
    GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt32UContent),'T_ComplexInt32UContent');

  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt16SContent),'T_ComplexInt16SContent');
    GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexInt16UContent),'T_ComplexInt16UContent');
    
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexFloatExtendedContent),'T_ComplexFloatExtendedContent');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(T_ComplexFloatDoubleContent),'T_ComplexFloatDoubleContent');
  
  TClass_CplxSimpleContent.RegisterAttributeProperty('Elt_Exemple');
  GetTypeRegistry().Register(sXSD_NS,TypeInfo(TClass_CplxSimpleContent),'TClass_CplxSimpleContent').RegisterExternalPropertyName('Elt_Exemple', 'published');

  with GetTypeRegistry().Register(sWST_BASE_NS,TypeInfo(TEmbeddedArrayOfStringRemotable),'TEmbeddedArrayOfStringRemotable') do begin
    RegisterExternalPropertyName(sARRAY_ITEM,'abc');
    RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  end;

{$IFDEF FPC}
  RegisterTest(TTestArray);
  RegisterTest(TTestSOAPFormatter);
  RegisterTest(TTestBinaryFormatter);
  RegisterTest(TTest_TBaseComplexRemotable);
  RegisterTest(TTestSOAPFormatterAttributes);
  RegisterTest(TTestBinaryFormatterAttributes);
  RegisterTest(TTest_TDateRemotable);
  RegisterTest(TTest_TDurationRemotable);
  RegisterTest(TTest_TTimeRemotable);

  RegisterTest(TTestXmlRpcFormatterAttributes);
  RegisterTest(TTestXmlRpcFormatter);
{$ELSE}
  RegisterTest(TTestArray.Suite);
  RegisterTest(TTestSOAPFormatter.Suite);
  RegisterTest(TTestBinaryFormatter.Suite);
  RegisterTest(TTest_TBaseComplexRemotable.Suite);
  RegisterTest(TTestSOAPFormatterAttributes.Suite);
  RegisterTest(TTestBinaryFormatterAttributes.Suite);
  RegisterTest(TTest_TDateRemotable.Suite);
  RegisterTest(TTest_TDurationRemotable.Suite);
  RegisterTest(TTest_TTimeRemotable.Suite);

  RegisterTest(TTestXmlRpcFormatterAttributes.Suite);
  RegisterTest(TTestXmlRpcFormatter.Suite);
{$ENDIF}
end.
