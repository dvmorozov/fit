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
unit base_service_intf;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs, syncobjs, semaphore
{$IFNDEF FPC}
  ,Windows
{$ENDIF}
  ;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  stBase   = 0;
  stObject = stBase + 1;
  stArray  = stBase + 2;

  sARRAY_ITEM = 'item';
  sARRAY_STYLE = 'style';

  // array style string
  sScoped  = 'scoped';
  sEmbedded = 'embedded';

type

  { standart data types defines }
  anyURI = type string;
  token = type string;
  nonNegativeInteger = type LongWord;
  positiveInteger = type nonNegativeInteger;
  float = Single;
  
  TScopeType = Integer;
  TArrayStyle = ( asScoped, asEmbeded, asNone );
  THeaderDirection = ( hdOut, hdIn );
  THeaderDirections = set of THeaderDirection;
const
  AllHeaderDirection = [Low(THeaderDirection)..High(THeaderDirection)];

type

  EServiceException = class(Exception)
  End;

  EBaseRemoteException = class(EServiceException)
  private
    FFaultCode: string;
    FFaultString: string;
  Published
    property FaultCode : string Read FFaultCode Write FFaultCode;
    property FaultString : string Read FFaultString Write FFaultString;
  End;

  ETypeRegistryException = class(EServiceException)
  End;

  IItemFactory = Interface;
  IFormatterBase = Interface;
  IFormatterRegistry = Interface;

  TBaseRemotable = class;
  THeaderBlock = class;

  //Utility interface used to configure its parent.
  IPropertyManager = Interface
    ['{A3A6B8F4-E50D-4956-B416-C642C72E4672}']
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  End;

  IItemFactory = interface
    ['{38258BC0-CBE6-437B-B104-9A62475E53AC}']
    function CreateInstance():IInterface;
  end;

  IItemFactoryEx = interface(IItemFactory)
    ['{66B77926-7E45-4780-8FFB-FB78625EDC1D}']
    procedure ReleaseInstance(const AInstance : IInterface);
    procedure DiscardInstance(const AInstance : IInterface);
    function GetPropertyManager(
      const APropertyGroup : string;
      const ACreateIfNotExists : Boolean
    ):IPropertyManager;
  end;

  IFormatterRegistry = Interface
    ['{E4D69D2A-F0A5-43E1-8C56-B47E7AB5D1AF}']
    function Find(const AFormatterName : string):IFormatterBase;
    procedure Register(
      const AFormatterName,
            AContentType   : string;
            AFactory       : IItemFactory
    );
  End;

  ICallContext = Interface
    ['{855EB8E2-0700-45B1-B852-2101023200E0}']
    procedure AddObjectToFree(const AObject : TObject);
    procedure Clear();
    function AddHeader(
      const AHeader        : THeaderBlock;
      const AKeepOwnership : Boolean
    ):Integer;
    function GetHeaderCount(const ADirections : THeaderDirections):Integer;
    function GetHeader(const AIndex : Integer) : THeaderBlock;
    procedure ClearHeaders(const ADirection : THeaderDirection);
  End;

  TSerializationStyle = ( ssNodeSerialization, ssAttibuteSerialization );
  
  IFormatterBase = Interface
    ['{2AB3BF54-B7D6-4C46-8245-133C8775E9C1}']
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    function GetCurrentScope():string;
    procedure Clear();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : PTypeInfo
    );
    procedure BeginArray(
      const AName         : string;
      const ATypeInfo     : PTypeInfo;
      const AItemTypeInfo : PTypeInfo;
      const ABounds       : Array Of Integer;
      const AStyle        : TArrayStyle
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    procedure AddScopeAttribute(Const AName,AValue : string);
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AStyle     : TArrayStyle;
      const AItemName  : string
    ):Integer;
    procedure EndScopeRead();
    property CurrentScope : String Read GetCurrentScope;

    procedure BeginHeader();
    procedure EndHeader();

    procedure Put(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData
    );
    procedure PutScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      const AData
    );
    procedure Get(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData
    );
    procedure GetScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      var   AData
    );
    function ReadBuffer(const AName : string) : string;
    
    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  End;

  { TSimpleCallContext }

  TSimpleCallContext = class(TInterfacedObject,ICallContext)
  private
    FHeaderList : TObjectList;
    FFreeObjectList : TObjectList;
  protected
    procedure AddObjectToFree(const AObject : TObject);
    procedure Clear();
    function AddHeader(
      const AHeader        : THeaderBlock;
      const AKeepOwnership : Boolean
    ):Integer;
    function GetHeaderCount(const ADirections : THeaderDirections):Integer;
    function GetHeader(const AIndex : Integer) : THeaderBlock;
    procedure ClearHeaders(const ADirection : THeaderDirection);
    procedure FreeHeader(AHeader : THeaderBlock);
  Public
    constructor Create();
    destructor Destroy();override;
  End;

  { TBaseRemotable }
  TBaseRemotableClass = class of TBaseRemotable;
  TBaseRemotable = class(TPersistent)
  Public
    constructor Create();virtual;
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo
    );virtual;abstract;
    class procedure Load(
      Var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : String;
      const ATypeInfo : PTypeInfo
    );virtual;abstract;
  End;

  TAbstractSimpleRemotableClass = class of TAbstractSimpleRemotable;
  TAbstractSimpleRemotable = class(TBaseRemotable)
  end;

  { TStringBufferRemotable }

  TStringBufferRemotable = class(TAbstractSimpleRemotable)
  private
    FData : string;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    procedure Assign(Source: TPersistent); override;
    property Data : string read FData write FData;
  end;
  
  { TBaseDateRemotable }

  TBaseDateRemotable = class(TAbstractSimpleRemotable)
  private
    FDate : TDateTime;
    FYear   : Integer;
    FMonth  : Integer;
    FDay    : Integer;
  protected
    procedure SetDate(const AValue: TDateTime);virtual;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class function FormatDate(const ADate : TDateTime):string;virtual;abstract;
    class function ParseDate(const ABuffer : string):TDateTime;virtual;abstract;

    procedure Assign(Source: TPersistent); override;

    property AsDate : TDateTime read FDate write SetDate;
    property Year : Integer read FYear;
    property Month : Integer read FMonth;
    property Day : Integer read FDay;
  end;

  { TDateRemotable }

  TDateRemotable = class(TBaseDateRemotable)
  private
    FHour: Integer;
    FMinute: Integer;
    FSecond: Integer;
  protected
    procedure SetDate(const AValue: TDateTime);override;
  public
    class function FormatDate(const ADate : TDateTime):string;override;
    class function ParseDate(const ABuffer : string):TDateTime;override;
    property Hour : Integer read FHour;
    property Minute : Integer read FMinute;
    property Second : Integer read FSecond;
  end;
  
  TDurationRemotable = class(TBaseDateRemotable)
  protected
    //class function FormatDate(const ADate : TDateTime):string;override;
    //class function ParseDate(const ABuffer : string):TDateTime;override;
  end;

  TTimeRemotable = class(TBaseDateRemotable)
  protected
    //class function FormatDate(const ADate : TDateTime):string;override;
    //class function ParseDate(const ABuffer : string):TDateTime;override;
  end;
  
  TAbstractComplexRemotableClass = class of TAbstractComplexRemotable;

  { TAbstractComplexRemotable }

  TAbstractComplexRemotable = class(TBaseRemotable)
  public
    class procedure RegisterAttributeProperty(const AProperty : shortstring);virtual;
    class procedure RegisterAttributeProperties(const APropertList : array of shortstring);virtual;
    class function IsAttributeProperty(const AProperty : shortstring):Boolean;

    procedure Assign(Source: TPersistent); override;
  end;

  TBaseComplexRemotableClass = class of TBaseComplexRemotable;

  { TBaseComplexRemotable }

  TBaseComplexRemotable = class(TAbstractComplexRemotable)
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;

  { TBaseComplexSimpleContentRemotable }

  TBaseComplexSimpleContentRemotable = class(TAbstractComplexRemotable)
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);virtual;abstract;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);virtual;abstract;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;

  { TComplexInt8UContentRemotable }

  TComplexInt8UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Byte;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Byte read FValue write FValue;
  end;

  { TComplexInt8SContentRemotable }
  
  TComplexInt8SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: ShortInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : ShortInt read FValue write FValue;
  end;

  { TComplexInt16SContentRemotable }

  TComplexInt16SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: SmallInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : SmallInt read FValue write FValue;
  end;

  TComplexInt16UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Word;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Word read FValue write FValue;
  end;
  
  { TComplexInt32SContentRemotable }

  TComplexInt32SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: LongInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : LongInt read FValue write FValue;
  end;

  { TComplexInt32UContentRemotable }

  TComplexInt32UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: LongWord;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : LongWord read FValue write FValue;
  end;
  
  { TComplexInt64SContentRemotable }

  TComplexInt64SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Int64;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Int64 read FValue write FValue;
  end;

  { TComplexInt64UContentRemotable }

  TComplexInt64UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: QWord;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : QWord read FValue write FValue;
  end;

  { TComplexFloatExtendedContentRemotable }

  TComplexFloatExtendedContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Extended;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Extended read FValue write FValue;
  end;

  { TComplexFloatDoubleContentRemotable }

  TComplexFloatDoubleContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Double;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Double read FValue write FValue;
  end;

  { TComplexFloatSingleContentRemotable }

  TComplexFloatSingleContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Single;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Single read FValue write FValue;
  end;

  { TComplexStringContentRemotable }

  TComplexStringContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: string;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : string read FValue write FValue;
  end;

  { TComplexBooleanContentRemotable }

  TComplexBooleanContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Boolean;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Boolean read FValue write FValue;
  end;
  
  THeaderBlockClass = class of THeaderBlock;

  { THeaderBlock }

  THeaderBlock = class(TBaseComplexRemotable)
  private
    FDirection: THeaderDirection;
    FmustUnderstand: Integer;
    FUnderstood: Boolean;
    function HasmustUnderstand: boolean;
    procedure SetmustUnderstand(const AValue: Integer);
  public
    property Direction : THeaderDirection read FDirection write FDirection;
    property Understood : Boolean read FUnderstood write FUnderstood;
  published
    property mustUnderstand : Integer read FmustUnderstand write SetmustUnderstand stored HasmustUnderstand;
  end;

  TBaseArrayRemotableClass = class of TBaseArrayRemotable;

  { TBaseArrayRemotable }

  TBaseArrayRemotable = class(TAbstractComplexRemotable)
  protected
    class function GetItemName():string;virtual;
    class function GetStyle():TArrayStyle;virtual;
    procedure CheckIndex(const AIndex : Integer);
    function GetLength():Integer;virtual;abstract;
  public
    class function GetItemTypeInfo():PTypeInfo;virtual;abstract;
    destructor Destroy();override;

    procedure SetLength(const ANewSize : Integer);virtual;abstract;
    property Length : Integer Read GetLength;
  end;

  { TBaseObjectArrayRemotable
      An implementation for array handling. The array items are "owned" by
      this class instance, so one has not to free them.
  }
  TBaseObjectArrayRemotable = class(TBaseArrayRemotable)
  Private
    FArray : Array Of TBaseRemotable;
  Protected
    function GetItem(AIndex: Integer): TBaseRemotable;
    function GetLength():Integer;override;
  Public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      Var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : String;
      const ATypeInfo : PTypeInfo
    );override;

    class function GetItemClass():TBaseRemotableClass;virtual;abstract;
    class function GetItemTypeInfo():PTypeInfo;override;

    constructor Create();override;
    procedure Assign(Source: TPersistent); override;

    procedure SetLength(Const ANewSize : Integer);override;
    Property Item[AIndex:Integer] : TBaseRemotable Read GetItem;Default;
  End;

  TBaseObjectArrayRemotableClass = class of TBaseObjectArrayRemotable;

  { TBaseSimpleTypeArrayRemotable }

  TBaseSimpleTypeArrayRemotable = class(TBaseArrayRemotable)
  protected
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );virtual;abstract;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );virtual;abstract;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;

  { TArrayOfStringRemotable }
  //  --------- AnsiString !!!! ----------
  TArrayOfStringRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of ansistring;
    function GetItem(AIndex: Integer): ansistring;
    procedure SetItem(AIndex: Integer; const AValue: ansistring);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : ansistring read GetItem write SetItem; default;
  end;

  { TArrayOfBooleanRemotable }

  TArrayOfBooleanRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Boolean;
    function GetItem(AIndex: Integer): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: Boolean);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Boolean read GetItem write SetItem; default;
  end;

  { TArrayOfInt8URemotable }

  TArrayOfInt8URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Byte;
    function GetItem(AIndex: Integer): Byte;
    procedure SetItem(AIndex: Integer; const AValue: Byte);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Byte read GetItem write SetItem; default;
  end;

  { TArrayOfInt8SRemotable }

  TArrayOfInt8SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of ShortInt;
    function GetItem(AIndex: Integer): ShortInt;
    procedure SetItem(AIndex: Integer; const AValue: ShortInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : ShortInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt16SRemotable }

  TArrayOfInt16SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of SmallInt;
    function GetItem(AIndex: Integer): SmallInt;
    procedure SetItem(AIndex: Integer; const AValue: SmallInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : SmallInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt16URemotable }

  TArrayOfInt16URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Word;
    function GetItem(AIndex: Integer): Word;
    procedure SetItem(AIndex: Integer; const AValue: Word);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Word read GetItem write SetItem; default;
  end;

  { TArrayOfInt32URemotable }

  TArrayOfInt32URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of LongWord;
    function GetItem(AIndex: Integer): LongWord;
    procedure SetItem(AIndex: Integer; const AValue: LongWord);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : LongWord read GetItem write SetItem; default;
  end;

  { TArrayOfInt32SRemotable }

  TArrayOfInt32SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of LongInt;
    function GetItem(AIndex: Integer): LongInt;
    procedure SetItem(AIndex: Integer; const AValue: LongInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : LongInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt64SRemotable }

  TArrayOfInt64SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Int64;
    function GetItem(AIndex: Integer): Int64;
    procedure SetItem(AIndex: Integer; const AValue: Int64);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Int64 read GetItem write SetItem; default;
  end;

  { TArrayOfInt64URemotable }

  TArrayOfInt64URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of QWord;
    function GetItem(AIndex: Integer): QWord;
    procedure SetItem(AIndex: Integer; const AValue: QWord);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : QWord read GetItem write SetItem; default;
  end;

  { TArrayOfFloatSingleRemotable }

  TArrayOfFloatSingleRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Single;
    function GetItem(AIndex: Integer): Single;
    procedure SetItem(AIndex: Integer; const AValue: Single);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Single read GetItem write SetItem; default;
  end;

  { TArrayOfFloatDoubleRemotable }

  TArrayOfFloatDoubleRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Double;
    function GetItem(AIndex: Integer): Double;
    procedure SetItem(AIndex: Integer; const AValue: Double);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Double read GetItem write SetItem; default;
  end;

  { TArrayOfFloatExtendedRemotable }

  TArrayOfFloatExtendedRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Extended;
    function GetItem(AIndex: Integer): Extended;
    procedure SetItem(AIndex: Integer; const AValue: Extended);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Extended read GetItem write SetItem; default;
  end;

  { TArrayOfFloatCurrencyRemotable }

  TArrayOfFloatCurrencyRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Currency;
    function GetItem(AIndex: Integer): Currency;
    procedure SetItem(AIndex: Integer; const AValue: Currency);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : Currency read GetItem write SetItem; default;
  end;

  { TBaseFactoryRegistryItem }
  // Implementation helpers
  TBaseFactoryRegistryItem = class
  private
    FFactory: IItemFactory;
    FName: string;
  public
    constructor Create(
      const AName    : string;
      const AFactory : IItemFactory
    );
    destructor Destroy();override;
    property Name    : string Read FName;
    property Factory : IItemFactory Read FFactory;
  End;

  { TBaseFactoryRegistry }
  TBaseFactoryRegistry = class(TInterfacedObject,IInterface)
  private
    FList : TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TBaseFactoryRegistryItem;
  protected
    function FindFactory(const AName: string): IItemFactory;
    procedure Register(
      const AName    : string;
            AFactory : IItemFactory
    );
  protected
    property Count : Integer read GetCount;
    property Item[Index:Integer] : TBaseFactoryRegistryItem read GetItem;
  public
    constructor Create();
    destructor Destroy();override;
  End;

  { TSimpleFactoryItem }

  TSimpleFactoryItem = class(TInterfacedObject)
  public
    constructor Create();virtual;
  End;

  TSimpleFactoryItemClass = class of TSimpleFactoryItem;

  { TSimpleItemFactory }

  TSimpleItemFactory = class(TInterfacedPersistent,IInterface,IItemFactory)
  private
    FItemClass : TSimpleFactoryItemClass;
  protected
    function CreateInstance():IInterface;virtual;
    function GetItemClass() : TSimpleFactoryItemClass;
  public
    constructor Create(AItemClass : TSimpleFactoryItemClass);
  End;

  { TIntfPoolItem }

  TIntfPoolItem = class
  private
    FIntf: IInterface;
    FUsed: Boolean;
  public
    constructor Create(AIntf : IInterface; const AUsed : Boolean);
    destructor Destroy();override;
    property Intf : IInterface read FIntf;
    property Used : Boolean read FUsed write FUsed;
  end;
  
  TIntfPool = class
  private
    FList : TObjectList;
    FCS : TCriticalSection;
    FLock : TSemaphoreObject;
    FFactory : IItemFactory;
    FMin : PtrInt;
    FMax : PtrInt;
  private
    function CreateNew(const AUsed : Boolean) : TIntfPoolItem;
    function TryGet(const AIndex : PtrInt) : Boolean;
  public
    constructor Create(
      const AMin, AMax : PtrInt;
            AFactory   : IItemFactory
    );
    destructor Destroy();override;
    function Get(const ATimeOut : Cardinal) : IInterface;
    procedure Release(const AItem : IInterface);
    procedure Discard(const AItem : IInterface);
    function GetInstancesCount() : PtrInt;
    property Min : PtrInt read FMin;
    property Max : PtrInt read FMax;
  end;
  
  { TSimpleItemFactoryEx }

  TSimpleItemFactoryEx = class(TSimpleItemFactory,IInterface,IItemFactory,IItemFactoryEx)
  private
    FPooled: Boolean;
    FPoolMax: PtrInt;
    FPoolMin: PtrInt;
    FPropertyNames : TStringList;
    FProperties : IInterfaceList;
    FPool : TIntfPool;
    FTimeOut: PtrUInt;
  private
    procedure PreparePool();
    procedure SetPooled(const AValue: Boolean);
    procedure SetPoolMax(const AValue: PtrInt);
    procedure SetPoolMin(const AValue: PtrInt);
  protected
    function CreateInstance():IInterface;override;
    procedure ReleaseInstance(const AInstance : IInterface);virtual;
    procedure DiscardInstance(const AInstance : IInterface);virtual;
    function GetPropertyManager(
      const APropertyGroup : string;
      const ACreateIfNotExists : Boolean
    ):IPropertyManager;
  public
    constructor Create(
      AItemClass         : TSimpleFactoryItemClass;
      const APropsString : string
    );overload;
    constructor Create(AItemClass : TSimpleFactoryItemClass);overload;
    destructor Destroy();override;
  published
    property PoolMax : PtrInt read FPoolMax write SetPoolMax;
    property PoolMin : PtrInt read FPoolMin write SetPoolMin;
    property Pooled : Boolean read FPooled write SetPooled;
    property TimeOut : PtrUInt read FTimeOut write FTimeOut;
  end;

  TTypeRegistryItemOption = ( trioNonVisibleToMetadataService );
  TTypeRegistryItemOptions = set of TTypeRegistryItemOption;

  { TTypeRegistryItem }

  TTypeRegistryItem = class
  private
    FDataType: PTypeInfo;
    FNameSpace: string;
    FDeclaredName : string;
    FOptions: TTypeRegistryItemOptions;
    FSynonymTable : TStrings;
    FExternalNames : TStrings;
    FInternalNames : TStrings;
  public
    constructor Create(
            ANameSpace    : string;
            ADataType     : PTypeInfo;
      Const ADeclaredName : string = ''
    );
    destructor Destroy();override;
    function AddPascalSynonym(const ASynonym : string):TTypeRegistryItem;//inline;
    function IsSynonym(const APascalTypeName : string):Boolean;//inline;
    
    procedure RegisterExternalPropertyName(const APropName, AExtPropName : string);
    function GetExternalPropertyName(const APropName : string) : string;
    function GetInternalPropertyName(const AExtPropName : string) : string;

    property DataType : PTypeInfo read FDataType;
    property NameSpace : string read FNameSpace;
    property DeclaredName : string read FDeclaredName;
    property Options : TTypeRegistryItemOptions read FOptions write FOptions;
  end;

  { TTypeRegistry }

  TTypeRegistry = class
  Private
    FList : TObjectList;
    function GetCount: Integer;
    function GetItemByIndex(Index: Integer): TTypeRegistryItem;
    function GetItemByTypeInfo(Index: PTypeInfo): TTypeRegistryItem;
  Public
    constructor Create();
    destructor Destroy();override;
    function IndexOf(Const ATypeInfo : PTypeInfo):Integer;
    function Add(AItem:TTypeRegistryItem):Integer;
    function Register(
      Const ANameSpace    : String;
      Const ADataType     : PTypeInfo;
      Const ADeclaredName : String = ''
    ):TTypeRegistryItem;
    function Find(ATypeInfo : PTypeInfo; Const AExact : Boolean):TTypeRegistryItem;overload;
    function Find(const APascalTypeName : string):TTypeRegistryItem;overload;
    function FindByDeclaredName(const ATypeName,ANameSpace : string):TTypeRegistryItem;
    Property Count : Integer Read GetCount;
    Property Item[Index:Integer] : TTypeRegistryItem Read GetItemByIndex;default;
    Property ItemByTypeInfo[Index:PTypeInfo] : TTypeRegistryItem Read GetItemByTypeInfo;
  End;

  TPropStoreType = ( pstNever, pstOptional, pstAlways );
  
  EPropertyException = class(Exception)
  end;

  { TStoredPropertyManager }

  TStoredPropertyManager = class(TInterfacedObject,IPropertyManager)
  private
    FData : TStringList;
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  protected
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  public
    constructor Create();
    destructor Destroy();override;
  end;

const
  sXSD_NS = 'http://www.w3.org/2001/XMLSchema';
  sXSD = 'xsd';
  sSOAP_ENV = 'http://schemas.xmlsoap.org/soap/envelope/';
  sSOAP_ENV_ABR = 'SOAP-ENV';
  sWST_BASE_NS = 'urn:wst_base';

  PROP_LIST_DELIMITER = ';';

  function GetTypeRegistry():TTypeRegistry;
  procedure RegisterStdTypes();

  function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;

{$IFDEF FPC}
  {$IFDEF FPC_211}
var
  wst_FormatSettings : TFormatSettings;
  {$ENDIF}
{$ELSE}
var
  wst_FormatSettings : TFormatSettings;
{$ENDIF}

implementation
uses imp_utils;

Var
  TypeRegistryInstance : TTypeRegistry = Nil;

function GetTypeRegistry():TTypeRegistry;
begin
  If Not Assigned(TypeRegistryInstance) Then
    TypeRegistryInstance := TTypeRegistry.Create();
  Result := TypeRegistryInstance;
end;

procedure RegisterStdTypes();
Var
  r : TTypeRegistry;
  ri : TTypeRegistryItem;
begin
  r := GetTypeRegistry();
  r.Register(sXSD_NS,TypeInfo(Integer),'int').AddPascalSynonym('Integer');
    r.Register(sXSD_NS,TypeInfo(LongWord),'unsignedInt');
    r.Register(sXSD_NS,TypeInfo(positiveInteger),'positiveInteger');
    r.Register(sXSD_NS,TypeInfo(nonNegativeInteger),'nonNegativeInteger');


  r.Register(sXSD_NS,TypeInfo(string),'string').AddPascalSynonym('string');
  r.Register(sXSD_NS,TypeInfo(AnsiString),'ansistring').AddPascalSynonym('ansistring');
  r.Register(sXSD_NS,TypeInfo(anyURI),'anyURI').AddPascalSynonym('anyURI');

  r.Register(sXSD_NS,TypeInfo(boolean),'boolean').AddPascalSynonym('boolean');

  r.Register(sXSD_NS,TypeInfo(Byte),'unsignedByte').AddPascalSynonym('Byte');
    r.Register(sXSD_NS,TypeInfo(ShortInt),'byte').AddPascalSynonym('ShortInt');
  r.Register(sXSD_NS,TypeInfo(Word),'unsignedShort').AddPascalSynonym('Word');
    r.Register(sXSD_NS,TypeInfo(SmallInt),'short').AddPascalSynonym('SmallInt');
  r.Register(sXSD_NS,TypeInfo(Int64),'long').AddPascalSynonym('Int64');
    r.Register(sXSD_NS,TypeInfo(QWord),'unsignedLong').AddPascalSynonym('QWord');

  r.Register(sXSD_NS,TypeInfo(Single),'float').AddPascalSynonym('Single');
  r.Register(sXSD_NS,TypeInfo(Currency),'float').AddPascalSynonym('Currency');
  r.Register(sXSD_NS,TypeInfo(Comp),'float').AddPascalSynonym('Comp');
  r.Register(sXSD_NS,TypeInfo(Double),'double').AddPascalSynonym('Double');
  r.Register(sXSD_NS,TypeInfo(Extended),'decimal').AddPascalSynonym('Extended');

  r.Register(sXSD_NS,TypeInfo(TDateRemotable),'dateTime').AddPascalSynonym('TDateRemotable');
  r.Register(sXSD_NS,TypeInfo(TDurationRemotable),'duration').AddPascalSynonym('TDurationRemotable');
  r.Register(sXSD_NS,TypeInfo(TTimeRemotable),'time').AddPascalSynonym('TTimeRemotable');

  ri := r.Register(sWST_BASE_NS,TypeInfo(TBaseArrayRemotable),'TBaseArrayRemotable');
  ri.Options := ri.Options + [trioNonVisibleToMetadataService];

  THeaderBlock.RegisterAttributeProperty('mustUnderstand');
  ri := r.Register(sSOAP_ENV,TypeInfo(THeaderBlock),'THeaderBlock');
  ri.Options := ri.Options + [trioNonVisibleToMetadataService];


  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfStringRemotable),'TArrayOfStringRemotable').AddPascalSynonym('TArrayOfStringRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfBooleanRemotable),'TArrayOfBooleanRemotable').AddPascalSynonym('TArrayOfBooleanRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt8URemotable),'TArrayOfInt8URemotable').AddPascalSynonym('TArrayOfInt8URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt8SRemotable),'TArrayOfInt8SRemotable').AddPascalSynonym('TArrayOfInt8SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt16URemotable),'TArrayOfInt16URemotable').AddPascalSynonym('TArrayOfInt16URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt16SRemotable),'TArrayOfInt16SRemotable').AddPascalSynonym('TArrayOfInt16SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt32URemotable),'TArrayOfInt32URemotable').AddPascalSynonym('TArrayOfInt32URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt32SRemotable),'TArrayOfInt32SRemotable').AddPascalSynonym('TArrayOfInt32SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt64URemotable),'TArrayOfInt64URemotable').AddPascalSynonym('TArrayOfInt64URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt64SRemotable),'TArrayOfInt64SRemotable').AddPascalSynonym('TArrayOfInt64SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatSingleRemotable),'TArrayOfFloatSingleRemotable').AddPascalSynonym('TArrayOfFloatSingleRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatDoubleRemotable),'TArrayOfFloatDoubleRemotable').AddPascalSynonym('TArrayOfFloatDoubleRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatExtendedRemotable),'TArrayOfFloatExtendedRemotable').AddPascalSynonym('TArrayOfFloatExtendedRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatCurrencyRemotable),'TArrayOfFloatCurrencyRemotable').AddPascalSynonym('TArrayOfFloatCurrencyRemotable');
  
  r.Register(sXSD_NS,TypeInfo(TComplexInt64SContentRemotable),'long').AddPascalSynonym('TComplexInt64SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt64UContentRemotable),'unsignedLong').AddPascalSynonym('TComplexInt64UContentRemotable');
  
  r.Register(sXSD_NS,TypeInfo(TComplexInt32SContentRemotable),'int').AddPascalSynonym('TComplexInt32SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt32UContentRemotable),'unsignedInt').AddPascalSynonym('TComplexInt32UContentRemotable');
    
  r.Register(sXSD_NS,TypeInfo(TComplexInt16SContentRemotable),'short').AddPascalSynonym('TComplexInt16SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt16UContentRemotable),'unsignedShort').AddPascalSynonym('TComplexInt16UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexInt8SContentRemotable),'byte').AddPascalSynonym('TComplexInt8SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt8UContentRemotable),'unsignedByte').AddPascalSynonym('TComplexInt8UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexFloatExtendedContentRemotable),'decimal').AddPascalSynonym('TComplexFloatExtendedContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexFloatDoubleContentRemotable),'double').AddPascalSynonym('TComplexFloatDoubleContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexFloatSingleContentRemotable),'Single').AddPascalSynonym('TComplexFloatSingleContentRemotable');
  
  r.Register(sXSD_NS,TypeInfo(TComplexStringContentRemotable),'string').AddPascalSynonym('TComplexStringContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexBooleanContentRemotable),'boolean').AddPascalSynonym('TComplexBooleanContentRemotable');
end;

{$IFDEF FPC}
function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;
begin
  case (PropInfo^.PropProcs shr 4) and 3 of
    ptfield:
      Result := pstOptional;
    ptconst:
      begin
        if LongBool(PropInfo^.StoredProc) then
          Result := pstAlways
        else
          Result := pstNever;
      end;
    ptstatic,
    ptvirtual:
      Result := pstOptional;
  end;
end;
{$ELSE}
function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;
{var
  b : PByte;
begin
  if ( ( PropInfo^.StoredProc and $0FFFFFF00 ) = 0 ) then begin
    if LongBool(PropInfo^.StoredProc) then // constante
      Result := pstAlways
    else
      Result := pstNever;
  end else begin
    b := PByte(PropInfo^.StoredProc);
    Inc(b,3);
    if ( b^ < $FE ) then begin //StaticMethod
      Result := pstOptional;
    end else ( b^ > $FE ) begin Field
    end else begin // virtual method
    end;
  end;
end;}
begin
  if ( ( Cardinal(PropInfo^.StoredProc) and $0FFFFFF00 ) = 0 ) then begin
    if LongBool(PropInfo^.StoredProc) then begin
      Result := pstAlways
    end else begin
      Result := pstNever;
    end;
  end else begin
    Result := pstOptional;
  end;
end;
{$ENDIF}

{ TBaseRemotable }

constructor TBaseRemotable.Create();
begin
end;

{ TBaseComplexRemotable }
Type
  TEnumBuffer = Record
    Case TOrdType Of
      otSByte : (ShortIntData : ShortInt);
      otUByte : (ByteData : Byte);
      otSWord : (SmallIntData : SmallInt);
      otUWord : (WordData : Word);
      otSLong : (SLongIntData : LongInt);
      otULong : (ULongIntData : LongWord);
  End;
  TFloatBuffer = Record
    Case TFloatType Of
      ftSingle : (SingleData : Single);
      ftDouble : (DoubleData : Double);
      ftExtended : (ExtendedData : Extended);
      ftCurr : (CurrencyData : Currency);
      ftComp : (CompData : Comp);
  End;

  { TSerializeOptions }

  TSerializeOptions = class
  private
    FAttributeFieldList : TStringList;
  private
    FElementClass: TAbstractComplexRemotableClass;
    procedure AddAttributeField(const AAttributeField : string);
    function GetAttributeCount: Integer;
    function GetAttributeField(AIndex : Integer): string;
  public
    constructor Create(const AElementClass : TAbstractComplexRemotableClass);
    destructor Destroy();override;
    function IsAttributeField(const AField : string):Boolean;
    property ElementClass : TAbstractComplexRemotableClass read FElementClass;
    property AttributeFieldCount : Integer read GetAttributeCount;
    property AttributeField[AIndex : Integer] : string read GetAttributeField;
  end;

  { TSerializeOptionsRegistry }

  TSerializeOptionsRegistry = class
  private
    FList : TObjectList;
  private
    function GetCount: Integer;
    function GetItem(AIndex : Integer): TSerializeOptions;
    function IndexOf(const AElementClass : TAbstractComplexRemotableClass):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    function RegisterClass(const AElementClass : TAbstractComplexRemotableClass):TSerializeOptions;
    function Find(const AElementClass : TAbstractComplexRemotableClass):TSerializeOptions;
    property Count : Integer read GetCount;
    property Item[AIndex : Integer] : TSerializeOptions read GetItem;
  end;

var
  SerializeOptionsRegistryInstance : TSerializeOptionsRegistry = nil;
  
function GetSerializeOptionsRegistry():TSerializeOptionsRegistry;
begin
  if not Assigned(SerializeOptionsRegistryInstance) then
    SerializeOptionsRegistryInstance := TSerializeOptionsRegistry.Create();
  Result := SerializeOptionsRegistryInstance;
end;
  
{ TSerializeOptionsRegistry }

function TSerializeOptionsRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSerializeOptionsRegistry.GetItem(AIndex : Integer): TSerializeOptions;
begin
  Result := FList[AIndex] as TSerializeOptions;
end;

function TSerializeOptionsRegistry.IndexOf(
  const AElementClass: TAbstractComplexRemotableClass
): Integer;
begin
  for Result := 0 to Pred(Count) do begin
    if ( Item[Result].ElementClass = AElementClass ) then
      Exit;
  end;
  Result := -1;
end;

constructor TSerializeOptionsRegistry.Create();
begin
  FList := TObjectList.Create(True);
end;

destructor TSerializeOptionsRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

function TSerializeOptionsRegistry.RegisterClass(
  const AElementClass: TAbstractComplexRemotableClass
): TSerializeOptions;
var
  i, j, k, c : Integer;
  ri : TSerializeOptions;
begin
  i := IndexOf(AElementClass);
  if ( i < 0 ) then begin
    c := FList.Count;
    i := FList.Add(TSerializeOptions.Create(AElementClass));
    Result := FList[i] as TSerializeOptions;
    for j := 0 to Pred(c) do begin
      ri := FList[j] as TSerializeOptions;
      for k := 0 to Pred(ri.AttributeFieldCount) do begin
        Result.FAttributeFieldList.Add(ri.FAttributeFieldList[k]);
      end;
    end;
  end;
  Result := FList[i] as TSerializeOptions;
end;

function TSerializeOptionsRegistry.Find(const AElementClass: TAbstractComplexRemotableClass): TSerializeOptions;
var
  i : Integer;
begin
  i := IndexOf(AElementClass);
  if ( i >= 0 ) then
    Result := FList[i] as TSerializeOptions
  else
    Result := nil;
end;
  
{ TSerializeOptions }

procedure TSerializeOptions.AddAttributeField(const AAttributeField: string);
begin
  if ( FAttributeFieldList.IndexOf(AAttributeField) < 0 ) then
    FAttributeFieldList.Add(AAttributeField);
end;

function TSerializeOptions.GetAttributeCount: Integer;
begin
  Result := FAttributeFieldList.Count;
end;

function TSerializeOptions.GetAttributeField(AIndex : Integer): string;
begin
  Result := FAttributeFieldList[AIndex];
end;

constructor TSerializeOptions.Create(const AElementClass: TAbstractComplexRemotableClass);
begin
  FElementClass := AElementClass;
  FAttributeFieldList := TStringList.Create();
  FAttributeFieldList.Duplicates := dupIgnore;
  FAttributeFieldList.Sorted := True;
end;

destructor TSerializeOptions.Destroy();
begin
  FreeAndNil(FAttributeFieldList);
  inherited Destroy();
end;

function TSerializeOptions.IsAttributeField(const AField: string): Boolean;
begin
  Result := ( FAttributeFieldList.IndexOf(AField) >= 0 );
end;

class procedure TBaseComplexRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : String;
  const ATypeInfo  : PTypeInfo
);
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  int64Data : Int64;
  strData : String;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumBuffer;
  floatDt : TFloatBuffer;
  p : PPropInfo;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
  prpName : string;
begin
  oldSS := AStore.GetSerializationStyle();
  AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    propCount := GetTypeData(ATypeInfo)^.PropCount;
    if ( propCount > 0 ) then begin
      propListLen := GetPropList(ATypeInfo,propList);
      try
        ss := AStore.GetSerializationStyle();
        typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
        for i := 0 to Pred(propCount) do begin
          p := propList^[i];
          pt := p^.PropType{$IFNDEF FPC}^{$ENDIF};
          if IsStoredProp(AObject,p) then begin
            if IsAttributeProperty(p^.Name) then begin
              if ( ss <> ssAttibuteSerialization ) then
                ss := ssAttibuteSerialization;
            end else begin
              if ( ss <> ssNodeSerialization ) then
                ss := ssNodeSerialization;
            end;
            if ( ss <> AStore.GetSerializationStyle() ) then
              AStore.SetSerializationStyle(ss);
            prpName := typRegItem.GetExternalPropertyName(p^.Name);
            case pt^.Kind of
              tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
                begin
                  int64Data := GetInt64Prop(AObject,p^.Name);
                  AStore.Put(prpName,pt,int64Data);
                end;
              tkLString{$IFDEF FPC},tkAString{$ENDIF} :
                begin
                  strData := GetStrProp(AObject,p^.Name);
                  AStore.Put(prpName,pt,strData);
                end;
              tkClass :
                begin
                  objData := GetObjectProp(AObject,p^.Name);
                  AStore.Put(prpName,pt,objData);
                end;
              {$IFDEF FPC}
              tkBool :
                begin
                  boolData := Boolean(GetOrdProp(AObject,p^.Name));
                  AStore.Put(prpName,pt,boolData);
                end;
              {$ENDIF}
              tkEnumeration,tkInteger :
                begin
                {$IFNDEF FPC}
                  if ( pt^.Kind = tkEnumeration ) and
                     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                  then begin
                    boolData := Boolean(GetOrdProp(AObject,p^.Name));
                    AStore.Put(prpName,pt,boolData);
                  end else begin
                {$ENDIF}
                    FillChar(enumData,SizeOf(enumData),#0);
                    case GetTypeData(pt)^.OrdType of
                      otSByte :
                        begin
                          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ShortIntData);
                        end;
                      otUByte :
                        begin
                          enumData.ByteData := Byte(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ByteData);
                        end;
                      otSWord :
                        begin
                          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.SmallIntData);
                        end;
                      otUWord :
                        begin
                          enumData.WordData := Word(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.WordData);
                        end;
                      otSLong :
                        begin
                          enumData.SLongIntData := LongInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.SLongIntData);
                        end;
                      otULong :
                        begin
                          enumData.ULongIntData := LongWord(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ULongIntData);
                        end;
                    end;
                {$IFNDEF FPC}
                  end;
                {$ENDIF}
                end;
              tkFloat :
                begin
                  FillChar(floatDt,SizeOf(floatDt),#0);
                  case GetTypeData(pt)^.FloatType of
                    ftSingle :
                      begin
                        floatDt.SingleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.SingleData);
                      end;
                    ftDouble :
                      begin
                        floatDt.DoubleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.DoubleData);
                      end;
                    ftExtended :
                      begin
                        floatDt.ExtendedData := Extended(GetFloatProp(AObject,p^.Name));
                        AStore.Put(prpName,pt,floatDt.ExtendedData);
                      end;
                    ftCurr :
                      begin
                        floatDt.CurrencyData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.CurrencyData);
                      end;
{$IFDEF CPU86}
                    ftComp :
                      begin
                        floatDt.CompData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.CompData);
                      end;
{$ENDIF}
                  end;
                end;
            end;
          end;
        end;
      finally
        Freemem(propList,propListLen*SizeOf(Pointer));
      end;
    end;
  finally
    AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

Type
  TFloatExtendedType = Extended;
class procedure TBaseComplexRemotable.Load(
  Var   AObject    : TObject;
        AStore     : IFormatterBase;
  var   AName      : String;
  const ATypeInfo  : PTypeInfo
);
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  propName : String;
  int64Data : Int64;
  strData : String;
  objData : TObject;
    objDataCreateHere : Boolean;
  boolData : Boolean;
  p : PPropInfo;
  enumData : TEnumBuffer;
  floatDt : TFloatExtendedType;
  floatBuffer : TFloatBuffer;
  persistType : TPropStoreType;
  objTypeData : PTypeData;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      If Not Assigned(AObject) Then
        AObject := Create();
      objTypeData := GetTypeData(ATypeInfo);
      propCount := objTypeData^.PropCount;
      If ( propCount > 0 ) Then Begin
        propListLen := GetPropList(ATypeInfo,propList);
        Try
          typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
          For i := 0 To Pred(propCount) Do Begin
            p := propList^[i];
            persistType := IsStoredPropClass(objTypeData^.ClassType,p);
            If ( persistType in [pstOptional,pstAlways] ) Then Begin
              pt := p^.PropType{$IFNDEF FPC}^{$ENDIF};
              propName := typRegItem.GetExternalPropertyName(p^.Name);
              if IsAttributeProperty(p^.Name) then begin
                ss := ssAttibuteSerialization;
              end else begin
                ss := ssNodeSerialization;
              end;
              if ( ss <> AStore.GetSerializationStyle() ) then
                AStore.SetSerializationStyle(ss);
              try
                Case pt^.Kind Of
                  tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
                    begin
                      AStore.Get(pt,propName,int64Data);
                      SetInt64Prop(AObject,p^.Name,int64Data);
                    end;
                  tkLString{$IFDEF FPC}, tkAString{$ENDIF} :
                    Begin
                      AStore.Get(pt,propName,strData);
                      SetStrProp(AObject,p^.Name,strData);
                    End;
                  {$IFDEF FPC}
                  tkBool :
                    Begin
                      AStore.Get(pt,propName,boolData);
                      SetOrdProp(AObject,p^.Name,Ord(boolData));
                    End;
                  {$ENDIF}
                  tkClass :
                    Begin
                      objData := GetObjectProp(AObject,p^.Name);
                      objDataCreateHere := not Assigned(objData);
                      try
                        AStore.Get(pt,propName,objData);
                        if objDataCreateHere then
                          SetObjectProp(AObject,p^.Name,objData);
                      finally
                        if objDataCreateHere and ( objData <> GetObjectProp(AObject,p^.Name) ) then
                          FreeAndNil(objData);
                      end;
                    End;
                  tkEnumeration,tkInteger :
                    Begin
                    {$IFNDEF FPC}
                      if ( pt^.Kind = tkEnumeration ) and
                         ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                      then begin
                        AStore.Get(pt,propName,boolData);
                        SetPropValue(AObject,p^.Name,boolData);
                      end else begin
                    {$ENDIF}
                        FillChar(enumData,SizeOf(enumData),#0);
                        Case GetTypeData(pt)^.OrdType Of
                          otSByte :
                            Begin
                              AStore.Get(pt,propName,enumData.ShortIntData);
                              int64Data := enumData.ShortIntData;
                            End;
                          otUByte :
                            Begin
                              AStore.Get(pt,propName,enumData.ByteData);
                              int64Data := enumData.ByteData;
                            End;
                          otSWord :
                            Begin
                              AStore.Get(pt,propName,enumData.SmallIntData);
                              int64Data := enumData.SmallIntData;
                            End;
                          otUWord :
                            Begin
                              AStore.Get(pt,propName,enumData.WordData);
                              int64Data := enumData.WordData;
                            End;
                          otSLong:
                            Begin
                              AStore.Get(pt,propName,enumData.SLongIntData);
                              int64Data := enumData.SLongIntData;
                            End;
                          otULong :
                            Begin
                              AStore.Get(pt,propName,enumData.ULongIntData);
                              int64Data := enumData.ULongIntData;
                            End;
                        End;
                        SetOrdProp(AObject,p^.Name,int64Data);
                    {$IFNDEF FPC}
                      end;
                    {$ENDIF}
                    End;
                  tkFloat :
                    Begin
                      FillChar(floatDt,SizeOf(floatBuffer),#0);
                      Case GetTypeData(pt)^.FloatType Of
                        ftSingle :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.SingleData);
                            floatDt := floatBuffer.SingleData;
                          End;
                        ftDouble :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.DoubleData);
                            floatDt := floatBuffer.DoubleData;
                          End;
                        ftExtended :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.ExtendedData);
                            floatDt := floatBuffer.ExtendedData;
                          End;
                        ftCurr :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CurrencyData);
                            floatDt := floatBuffer.CurrencyData;
                          End;
                        ftComp :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CompData);
                            floatDt := floatBuffer.CompData;
                          End;
                      End;
                      SetFloatProp(AObject,p^.Name,floatDt);
                    End;
                End;
              except
                on E : EServiceException do begin
                  if ( persistType = pstAlways ) then
                    raise;
                end;
              end;
            End;
          End;
        Finally
          Freemem(propList,propListLen*SizeOf(Pointer));
        End;
      End;
    finally
      AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

{ TBaseObjectArrayRemotable }

function TBaseObjectArrayRemotable.GetItem(AIndex: Integer): TBaseRemotable;
begin
  CheckIndex(AIndex);
  Result := FArray[AIndex];
end;

function TBaseObjectArrayRemotable.GetLength(): Integer;
begin
  Result := System.Length(FArray);
end;

class procedure TBaseObjectArrayRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : String;
  const ATypeInfo  : PTypeInfo
);
Var
  itmTypInfo : PTypeInfo;
  i,j : Integer;
  nativObj : TBaseObjectArrayRemotable;
  itm : TObject;
  itmName : string;
  styl : TArrayStyle;
begin
  if Assigned(AObject) then begin
    Assert(AObject.InheritsFrom(TBaseObjectArrayRemotable));
    nativObj := AObject as TBaseObjectArrayRemotable;
    j := nativObj.Length;
  end else begin
    j := 0;
  end;
  itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
  styl := GetStyle();
  AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),itmTypInfo,[0,Pred(j)],styl);
  try
    if ( styl = asScoped ) then begin
      itmName := GetItemName();
    end else begin
      itmName := AName;
    end;
    for i := 0 to Pred(j) do begin
      itm := nativObj.Item[i];
      AStore.Put(itmName,itmTypInfo,itm);
    end;
  finally
    AStore.EndScope();
  end;
end;

class procedure TBaseObjectArrayRemotable.Load(
  var   AObject   : TObject;
        AStore    : IFormatterBase;
  var   AName     : String;
  const ATypeInfo : PTypeInfo
);
Var
  i, len : Integer;
  nativObj : TBaseObjectArrayRemotable;
  s : string;
  itmTypInfo : PTypeInfo;
  itm : TBaseRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  styl := GetStyle();
  if ( styl = asScoped ) then begin
    itmName := GetItemName();
  end else begin
    itmName := AName;
  end;
  len := AStore.BeginArrayRead(AName,ATypeInfo,styl,itmName);
  if ( len >= 0 ) then begin
    Try
      If Not Assigned(AObject) Then
        AObject := Create();
      itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
      nativObj := AObject as TBaseObjectArrayRemotable;
      If ( len > 0 ) Then Begin
        s := '';
        nativObj.SetLength(len);
        For i := 0 To Pred(len) Do Begin
          itm := nativObj[i];
          AStore.Get(itmTypInfo,s,itm);
        End;
      End;
    Finally
      AStore.EndScopeRead();
    End;
  end;
end;

class function TBaseObjectArrayRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result:= GetItemClass().ClassInfo;
end;

constructor TBaseObjectArrayRemotable.Create();
begin
  FArray := Nil;
end;

procedure TBaseObjectArrayRemotable.Assign(Source: TPersistent);
var
  src : TBaseObjectArrayRemotable;
  i, c : PtrInt;
begin
  if Assigned(Source) then begin
    if Source.InheritsFrom(TBaseObjectArrayRemotable) then begin
      src := TBaseObjectArrayRemotable(Source);
      c := src.Length;
      SetLength(c);
      for i := 0 to Pred(c) do begin
        Item[i].Assign(src.Item[i]);
      end;
    end else begin
      inherited Assign(Source);
    end;
  end else begin
    SetLength(0);
  end;
end;

procedure TBaseObjectArrayRemotable.SetLength(const ANewSize: Integer);
var
  i,oldLen : Integer;
  itmClss : TBaseRemotableClass;
begin
  oldLen := GetLength;
  if ( oldLen = ANewSize ) then
    Exit;

  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt('Invalid array length : %d',[ANewSize]);

  if ( oldLen > ANewSize ) then begin
    for i := ANewSize to Pred(oldLen) do
      FreeAndNil(FArray[i]);
    System.SetLength(FArray,ANewSize);
  end else begin
    System.SetLength(FArray,ANewSize);
    itmClss := GetItemClass();
    for i := oldLen to Pred(ANewSize) do
      FArray[i] := itmClss.Create();
  end;
end;

{ TBaseFactoryRegistryItem }

constructor TBaseFactoryRegistryItem.Create(
  const AName    : string;
  const AFactory : IItemFactory
);
begin
  Assert(Assigned(AFactory));
  FName := AName;
  FFactory := AFactory;
end;

destructor TBaseFactoryRegistryItem.Destroy();
begin
  FName := '';
  FFactory := nil;
  inherited Destroy();
end;

function TBaseFactoryRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBaseFactoryRegistry.GetItem(Index: Integer): TBaseFactoryRegistryItem;
begin
  Result := FList[Index] as TBaseFactoryRegistryItem;
end;

{ TBaseFactoryRegistry }
function TBaseFactoryRegistry.FindFactory(const AName: string): IItemFactory;
Var
  i , c : Integer;
  s : string;
begin
  s := LowerCase(Trim(AName));
  c := Pred(FList.Count);
  For i := 0 To c Do Begin
    If AnsiSameText(TBaseFactoryRegistryItem(FList[i]).Name,s) Then Begin
      Result := TBaseFactoryRegistryItem(FList[i]).Factory;
      Exit;
    End;
  End;
  Result := Nil;
end;

procedure TBaseFactoryRegistry.Register(
  const AName    : string;
        AFactory : IItemFactory
);
begin
  Assert(Assigned(AFactory));
  If Not Assigned(FindFactory(AName)) Then
    FList.Add(TBaseFactoryRegistryItem.Create(AName,AFactory));
end;

constructor TBaseFactoryRegistry.Create();
begin
  inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TBaseFactoryRegistry.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

{ TSimpleItemFactory }

function TSimpleItemFactory.CreateInstance(): IInterface;
begin
  Result := FItemClass.Create() as IInterface;
end;

function TSimpleItemFactory.GetItemClass(): TSimpleFactoryItemClass;
begin
  Result := FItemClass;
end;

constructor TSimpleItemFactory.Create(AItemClass: TSimpleFactoryItemClass);
begin
  Assert(Assigned(AItemClass));
  FItemClass := AItemClass;
end;

{ TSimpleItemFactoryEx }

procedure TSimpleItemFactoryEx.PreparePool();
begin
  if ( FPool = nil ) then begin
    FPool := TIntfPool.Create(PoolMin,PoolMax,TSimpleItemFactory.Create(FItemClass));
  end;
end;

procedure TSimpleItemFactoryEx.SetPooled(const AValue: Boolean);
begin
  if ( FPooled = AValue ) then
    Exit;
  FreeAndNil(FPool);
  if AValue then begin
    if ( PoolMin < 0 ) or ( PoolMin > PoolMax ) or ( PoolMax < 1 ) then
      raise EServiceException.Create('Invalid pool parametters.');
    PreparePool();
  end;
  FPooled := AValue;
end;

procedure TSimpleItemFactoryEx.SetPoolMax(const AValue: PtrInt);
begin
  if ( FPoolMax = AValue ) then
    Exit;
  if Pooled then
    raise EServiceException.Create('Operation not allowed on an active pool.');
  FPoolMax := AValue;
end;

procedure TSimpleItemFactoryEx.SetPoolMin(const AValue: PtrInt);
begin
  if ( FPoolMin = AValue ) then
    Exit;
  if Pooled then
    raise EServiceException.Create('Operation not allowed on an active pool.');
  FPoolMin := AValue;
end;

function TSimpleItemFactoryEx.CreateInstance(): IInterface;
begin
  if Pooled then begin
    Result := FPool.Get(TimeOut);
  end else begin
    Result := inherited CreateInstance();
  end;
end;

procedure TSimpleItemFactoryEx.ReleaseInstance(const AInstance : IInterface);
begin
  if Pooled then begin
    FPool.Release(AInstance);
  end;
end;

procedure TSimpleItemFactoryEx.DiscardInstance(const AInstance : IInterface);
begin
  if Pooled then
    FPool.Discard(AInstance);
end;

function TSimpleItemFactoryEx.GetPropertyManager(
  const APropertyGroup : string;
  const ACreateIfNotExists : Boolean
):IPropertyManager;
var
  i : Integer;
  s : string;
begin
  Result := nil;
  s := Trim(APropertyGroup);
  i := FPropertyNames.IndexOf(s);
  if ( i < 0 ) then begin
    if not ACreateIfNotExists then
      Exit;
    i := FPropertyNames.Add(s);
    if ( s = '' ) then
      FProperties.Add(TPublishedPropertyManager.Create(Self) as IInterface)
    else
      FProperties.Add(TStoredPropertyManager.Create() as IInterface);
  end;
  Result := FProperties.Get(i) as IPropertyManager;
end;

constructor TSimpleItemFactoryEx.Create(
  AItemClass         : TSimpleFactoryItemClass;
  const APropsString : string
);
begin
  inherited Create(AItemClass);
  FPropertyNames := TStringList.Create();
  FProperties := TInterfaceList.Create();
  if ( Length(APropsString) > 0 ) then begin
    GetPropertyManager('',True).SetProperties(APropsString);
  end;
end;

constructor TSimpleItemFactoryEx.Create(AItemClass: TSimpleFactoryItemClass);
begin
  Create(AItemClass,'');
end;

destructor TSimpleItemFactoryEx.Destroy();
begin
  FreeAndNil(FPropertyNames);
  FProperties := nil;
  FreeAndNil(FPool);
  inherited Destroy();
end;

{ TSimpleFactoryItem }

constructor TSimpleFactoryItem.Create();
begin
end;


{ TSimpleCallContext }

procedure TSimpleCallContext.Clear();
begin
  FHeaderList.Clear();
  FFreeObjectList.Clear();;
end;

procedure TSimpleCallContext.AddObjectToFree(const AObject: TObject);
begin
  if ( FFreeObjectList.IndexOf(AObject) < 0 ) then
    FFreeObjectList.Add(AObject);
end;

function TSimpleCallContext.AddHeader(
  const AHeader: THeaderBlock;
  const AKeepOwnership: Boolean
): Integer;
begin
  Result := FHeaderList.IndexOf(AHeader);
  if ( Result = -1 ) then
    Result := FHeaderList.Add(AHeader);
  if AKeepOwnership then
    AddObjectToFree(AHeader);
end;

function TSimpleCallContext.GetHeaderCount(const ADirections : THeaderDirections):Integer;
var
  i : Integer;
begin
  if ( ADirections = [Low(THeaderDirection)..High(THeaderDirection)] ) then
    Result := FHeaderList.Count
  else begin
    Result := 0;
    for i := 0 to Pred(FHeaderList.Count) do begin
      if ( THeaderBlock(FHeaderList[i]).Direction in ADirections ) then
        Inc(Result);
    end;
  end;
end;

function TSimpleCallContext.GetHeader(const AIndex: Integer): THeaderBlock;
begin
  Result := FHeaderList[AIndex] as THeaderBlock;
end;

procedure TSimpleCallContext.ClearHeaders(const ADirection: THeaderDirection);
var
  i, c : Integer;
  h : THeaderBlock;
  fl : TObjectList;
begin
  c := FHeaderList.Count;
  if ( c > 0 ) then begin
    fl := TObjectList.Create(False);
    try
      for i := 0 to Pred(c) do begin
        h := FHeaderList[i] as THeaderBlock;
        if ( h.Direction = ADirection ) then
          fl.Add(h);
      end;
      for i := 0 to Pred(fl.Count) do
        FreeHeader(fl[i] as THeaderBlock);
    finally
      fl.Free();
    end;
  end;
end;

procedure TSimpleCallContext.FreeHeader(AHeader: THeaderBlock);
begin
  if Assigned(AHeader) then begin
    if ( FHeaderList.IndexOf(AHeader) >= 0 ) then
      FHeaderList.Remove(AHeader);
    if ( FFreeObjectList.IndexOf(AHeader) >= 0 ) then
      FHeaderList.Remove(AHeader)
    else
      AHeader.Free();
  end;
end;

constructor TSimpleCallContext.Create();
begin
  FHeaderList := TObjectList.Create(False);
  FFreeObjectList := TObjectList.Create(True);
end;

destructor TSimpleCallContext.Destroy();
begin
  FreeAndNil(FHeaderList);
  FreeAndNil(FFreeObjectList);
  inherited Destroy();
end;

{ TTypeRegistryItem }

constructor TTypeRegistryItem.Create(
        ANameSpace    : String;
        ADataType     : PTypeInfo;
  Const ADeclaredName : String
);
begin
  FNameSpace := ANameSpace;
  FDataType  := ADataType;
  FDeclaredName := Trim(ADeclaredName);
  If ( Length(FDeclaredName) = 0 ) Then
    FDeclaredName := FDataType^.Name;
end;

destructor TTypeRegistryItem.Destroy();
begin
  FreeAndNil(FInternalNames);
  FreeAndNil(FExternalNames);
  FreeAndNil(FSynonymTable);
  inherited Destroy();
end;

function TTypeRegistryItem.AddPascalSynonym(const ASynonym: string):TTypeRegistryItem; //inline;
begin
  Result := Self;
  if AnsiSameText(ASynonym,DataType^.Name) then
    Exit;
  if not Assigned(FSynonymTable) then begin
    FSynonymTable := TStringList.Create();
    FSynonymTable.Add(FDataType^.Name);
  end;
  if ( FSynonymTable.IndexOf(ASynonym) = -1 ) then
    FSynonymTable.Add(AnsiLowerCase(ASynonym));
end;

function TTypeRegistryItem.IsSynonym(const APascalTypeName: string): Boolean;//inline;
begin
  Result := AnsiSameText(APascalTypeName,DataType^.Name);
  if ( not Result ) and Assigned(FSynonymTable) then
    Result := ( FSynonymTable.IndexOf(APascalTypeName) >= 0 ) ;
end;

procedure TTypeRegistryItem.RegisterExternalPropertyName(const APropName,AExtPropName: string);
begin
  if not Assigned(FExternalNames) then begin
    FExternalNames := TStringList.Create();
    FInternalNames := TStringList.Create();
  end;
  FExternalNames.Values[APropName] := AExtPropName;
  FInternalNames.Values[AExtPropName] := APropName;
end;

function TTypeRegistryItem.GetExternalPropertyName(const APropName: string): string;
begin
  if Assigned(FExternalNames) and ( FExternalNames.IndexOfName(APropName) <> -1 ) then begin
    Result := FExternalNames.Values[APropName];
  end else begin
    Result := APropName;
  end;
end;

function TTypeRegistryItem.GetInternalPropertyName(const AExtPropName: string): string;
begin
  if Assigned(FInternalNames) and ( FInternalNames.IndexOfName(AExtPropName) <> -1 ) then
    Result := FInternalNames.Values[AExtPropName]
  else
    Result := AExtPropName;
end;

{ TTypeRegistry }

function TTypeRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTypeRegistry.GetItemByIndex(Index: Integer): TTypeRegistryItem;
begin
  Result := FList[Index] as TTypeRegistryItem;
end;

function TTypeRegistry.GetItemByTypeInfo(Index: PTypeInfo): TTypeRegistryItem;
Var
  i : Integer;
begin
  Assert(Assigned(Index));
  i := IndexOf(Index);
  If ( i > -1 ) Then
    Result := FList[i] as TTypeRegistryItem
  Else
    Raise ETypeRegistryException.CreateFmt('Type not registered : %s',[Index^.Name])
end;

constructor TTypeRegistry.Create();
begin
  Inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TTypeRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

function TTypeRegistry.IndexOf(Const ATypeInfo: PTypeInfo): Integer;
begin
  For Result := 0 To Pred(Count) Do Begin
    If ( ATypeInfo^.Kind = Item[Result].DataType^.Kind ) And
       AnsiSameText(ATypeInfo^.Name,Item[Result].DataType^.Name)
    Then
      Exit;
  End;
  Result := -1;
end;

function TTypeRegistry.Add(AItem: TTypeRegistryItem): Integer;
begin
  Result := IndexOf(AItem.DataType);
  If ( Result = -1 ) Then
    Result := FList.Add(AItem)
  Else
    Raise ETypeRegistryException.CreateFmt('Type already registered : "%s"',[AItem.DataType^.Name]);
end;

function TTypeRegistry.Register(
  Const ANameSpace    : String;
  Const ADataType     : PTypeInfo;
  Const ADeclaredName : String = ''
): TTypeRegistryItem;
var
  i : Integer;
begin
  i := IndexOf(ADataType);
  if ( i = -1 ) then
    i := Add(TTypeRegistryItem.Create(ANameSpace,ADataType,ADeclaredName));
  Result := Item[i];
end;

function TTypeRegistry.Find(ATypeInfo : PTypeInfo; Const AExact : Boolean):TTypeRegistryItem;
Var
  i : Integer;
  searchClass : TClass;
begin
  Result := Nil;
  i := IndexOf(ATypeInfo);
  If ( i > -1 ) Then
    Result := Item[i]
  Else If ( Not AExact ) And
          Assigned(ATypeInfo) And
          ( ATypeInfo^.Kind = tkClass )
  Then Begin
    searchClass := GetTypeData(ATypeInfo)^.ClassType;
    For i := 0 To Pred(Count) Do Begin
      Result := Item[i];
      If ( Result.DataType^.Kind = tkClass ) And
         searchClass.InheritsFrom(GetTypeData(Result.DataType)^.ClassType)
      Then
        Exit;
    End;
    Result := Nil;
  End;
end;

function TTypeRegistry.Find(const APascalTypeName: string): TTypeRegistryItem;
var
  i,c : Integer;
begin
  c := Count;
  for i := 0 to Pred(c) do begin
    Result := Item[i];
    if Result.IsSynonym(APascalTypeName) then
      Exit;
  end;
  Result := nil;
end;

function TTypeRegistry.FindByDeclaredName(
  const ATypeName,
        ANameSpace : string
): TTypeRegistryItem;
var
  i, c : Integer;
begin
  c := Count;
  for i := 0 to Pred(c) do begin
    Result := Item[i];
    if AnsiSameText(ANameSpace,Result.NameSpace) and
       AnsiSameText(ATypeName,Result.DeclaredName)
    then
      Exit;
  end;
  Result := nil;
end;


{ TBaseSimpleTypeArrayRemotable }


class procedure TBaseSimpleTypeArrayRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
var
  i,j : Integer;
  nativObj : TBaseSimpleTypeArrayRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  if Assigned(AObject) then begin
    Assert(AObject.InheritsFrom(TBaseSimpleTypeArrayRemotable));
    nativObj := AObject as TBaseSimpleTypeArrayRemotable;
    j := nativObj.Length;
  end else begin
    j := 0;
  end;
  styl := GetStyle();
  AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),GetItemTypeInfo(),[0,Pred(j)],styl);
  try
    if ( styl = asScoped ) then begin
      itmName := GetItemName();
    end else begin
      itmName := AName;
    end;
    for i := 0 to Pred(j) do begin
      nativObj.SaveItem(AStore,itmName,i);
    end;
  finally
    AStore.EndScope();
  end;
end;

class procedure TBaseSimpleTypeArrayRemotable.Load(
  var   AObject     : TObject;
        AStore      : IFormatterBase;
  var   AName       : String;
  const ATypeInfo   : PTypeInfo
);
Var
  i, len : Integer;
  nativObj : TBaseSimpleTypeArrayRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  styl := GetStyle();
  if ( styl = asScoped ) then begin
    itmName := GetItemName();
  end else begin
    itmName := AName;
  end;
  len := AStore.BeginArrayRead(AName,ATypeInfo, GetStyle(),itmName);
  if ( len >= 0 ) then begin
    try
      if not Assigned(AObject) then
        AObject := Create();
      nativObj := nil;
      nativObj := AObject as TBaseSimpleTypeArrayRemotable;
      if ( len >= 0 ) then begin
        nativObj.SetLength(len);
        for i := 0 to Pred(len) do begin
          nativObj.LoadItem(AStore,i);
        end;
      end;
    finally
      AStore.EndScopeRead();
    end;
  end;
end;

{ TArrayOfStringRemotable }

function TArrayOfStringRemotable.GetItem(AIndex: Integer): ansistring;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfStringRemotable.SetItem(AIndex: Integer;const AValue: ansistring);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfStringRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData)
end;

procedure TArrayOfStringRemotable.SaveItem(
        AStore : IFormatterBase;
  const AName  : String;
  const AIndex : Integer
);
begin
  AStore.Put(AName,TypeInfo(ansistring),FData[AIndex]);
end;

procedure TArrayOfStringRemotable.LoadItem(
        AStore : IFormatterBase;
  const AIndex : Integer
);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(ansistring),sName,FData[AIndex]);
end;

class function TArrayOfStringRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(ansistring);
end;

procedure TArrayOfStringRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TBaseArrayRemotable }

class function TBaseArrayRemotable.GetItemName(): string;
var
  tri : TTypeRegistryItem;
begin
  tri := GetTypeRegistry().Find(PTypeInfo(Self.ClassInfo),False);
  if Assigned(tri) then
    Result := Trim(tri.GetExternalPropertyName(sARRAY_ITEM));
  if ( System.Length(Result) = 0 ) then
    Result := sARRAY_ITEM;
end;

class function TBaseArrayRemotable.GetStyle(): TArrayStyle;
var
  tri : TTypeRegistryItem;
begin
  tri := GetTypeRegistry().Find(PTypeInfo(Self.ClassInfo),False);
  if Assigned(tri) and AnsiSameText(sEmbedded,Trim(tri.GetExternalPropertyName(sARRAY_STYLE))) then begin
    Result := asEmbeded;
  end else begin
    Result := asScoped;
  end;
end;

procedure TBaseArrayRemotable.CheckIndex(const AIndex : Integer);
begin
  if ( AIndex < 0 ) or ( AIndex >= Length ) then
    raise EServiceException.CreateFmt('Index out of bound : %d',[AIndex]);
end;

destructor TBaseArrayRemotable.Destroy();
begin
  SetLength(0);
  inherited Destroy();
end;

{ TArrayOfBooleanRemotable }

function TArrayOfBooleanRemotable.GetItem(AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfBooleanRemotable.SetItem(AIndex: Integer;const AValue: Boolean);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfBooleanRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfBooleanRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Boolean),FData[AIndex]);
end;

procedure TArrayOfBooleanRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Boolean),sName,FData[AIndex]);
end;

class function TArrayOfBooleanRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Boolean);
end;

procedure TArrayOfBooleanRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt8URemotable }

function TArrayOfInt8URemotable.GetItem(AIndex: Integer): Byte;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt8URemotable.SetItem(AIndex: Integer; const AValue: Byte);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt8URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt8URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Byte),FData[AIndex]);
end;

procedure TArrayOfInt8URemotable.LoadItem(AStore: IFormatterBase; const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Byte),sName,FData[AIndex]);
end;

class function TArrayOfInt8URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Byte);
end;

procedure TArrayOfInt8URemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt8SRemotable }

function TArrayOfInt8SRemotable.GetItem(AIndex: Integer): ShortInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt8SRemotable.SetItem(AIndex: Integer; const AValue: ShortInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt8SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt8SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(ShortInt),FData[AIndex]);
end;

procedure TArrayOfInt8SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(ShortInt),sName,FData[AIndex]);
end;

class function TArrayOfInt8SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(ShortInt);
end;

procedure TArrayOfInt8SRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt16SRemotable }

function TArrayOfInt16SRemotable.GetItem(AIndex: Integer): SmallInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt16SRemotable.SetItem(AIndex: Integer;const AValue: SmallInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt16SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt16SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(SmallInt),FData[AIndex]);
end;

procedure TArrayOfInt16SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(SmallInt),sName,FData[AIndex]);
end;

class function TArrayOfInt16SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(SmallInt);
end;

procedure TArrayOfInt16SRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt16URemotable }

function TArrayOfInt16URemotable.GetItem(AIndex: Integer): Word;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt16URemotable.SetItem(AIndex: Integer; const AValue: Word);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt16URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt16URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Word),FData[AIndex]);
end;

procedure TArrayOfInt16URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Word),sName,FData[AIndex]);
end;

class function TArrayOfInt16URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Word);
end;

procedure TArrayOfInt16URemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt32URemotable }

function TArrayOfInt32URemotable.GetItem(AIndex: Integer): LongWord;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt32URemotable.SetItem(AIndex: Integer;const AValue: LongWord);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt32URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt32URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(LongWord),FData[AIndex]);
end;

procedure TArrayOfInt32URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(LongWord),sName,FData[AIndex]);
end;

class function TArrayOfInt32URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongWord);
end;

procedure TArrayOfInt32URemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt32SRemotable }

function TArrayOfInt32SRemotable.GetItem(AIndex: Integer): LongInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt32SRemotable.SetItem(AIndex: Integer; const AValue: LongInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt32SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt32SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(LongInt),FData[AIndex]);
end;

procedure TArrayOfInt32SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(LongInt),sName,FData[AIndex]);
end;

class function TArrayOfInt32SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongInt);
end;

procedure TArrayOfInt32SRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt64SRemotable }

function TArrayOfInt64SRemotable.GetItem(AIndex: Integer): Int64;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt64SRemotable.SetItem(AIndex: Integer; const AValue: Int64);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt64SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt64SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Int64),FData[AIndex]);
end;

procedure TArrayOfInt64SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Int64),sName,FData[AIndex]);
end;

class function TArrayOfInt64SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Int64);
end;

procedure TArrayOfInt64SRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfInt64URemotable }

function TArrayOfInt64URemotable.GetItem(AIndex: Integer): QWord;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt64URemotable.SetItem(AIndex: Integer; const AValue: QWord);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt64URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt64URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(QWord),FData[AIndex]);
end;

procedure TArrayOfInt64URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(QWord),sName,FData[AIndex]);
end;

class function TArrayOfInt64URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(QWord);
end;

procedure TArrayOfInt64URemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfFloatSingleRemotable }

function TArrayOfFloatSingleRemotable.GetItem(AIndex: Integer): Single;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatSingleRemotable.SetItem(AIndex: Integer;const AValue: Single);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatSingleRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatSingleRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Single),FData[AIndex]);
end;

procedure TArrayOfFloatSingleRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Single),sName,FData[AIndex]);
end;

class function TArrayOfFloatSingleRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Single);
end;

procedure TArrayOfFloatSingleRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfFloatDoubleRemotable }

function TArrayOfFloatDoubleRemotable.GetItem(AIndex: Integer): Double;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatDoubleRemotable.SetItem(AIndex: Integer;const AValue: Double);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatDoubleRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatDoubleRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Double),FData[AIndex]);
end;

procedure TArrayOfFloatDoubleRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Double),sName,FData[AIndex]);
end;

class function TArrayOfFloatDoubleRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Double);
end;

procedure TArrayOfFloatDoubleRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfFloatExtendedRemotable }

function TArrayOfFloatExtendedRemotable.GetItem(AIndex: Integer): Extended;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatExtendedRemotable.SetItem(AIndex: Integer;const AValue: Extended);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatExtendedRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatExtendedRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Extended),FData[AIndex]);
end;

procedure TArrayOfFloatExtendedRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Extended),sName,FData[AIndex]);
end;

class function TArrayOfFloatExtendedRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Extended);
end;

procedure TArrayOfFloatExtendedRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TArrayOfFloatCurrencyRemotable }

function TArrayOfFloatCurrencyRemotable.GetItem(AIndex: Integer): Currency;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatCurrencyRemotable.SetItem(AIndex: Integer;const AValue: Currency);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatCurrencyRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatCurrencyRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Currency),FData[AIndex]);
end;

procedure TArrayOfFloatCurrencyRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Currency),sName,FData[AIndex]);
end;

class function TArrayOfFloatCurrencyRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Currency);
end;

procedure TArrayOfFloatCurrencyRemotable.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;


{ THeaderBlock }

function THeaderBlock.HasmustUnderstand: boolean;
begin
  Result := ( FmustUnderstand <> 0 );
end;

procedure THeaderBlock.SetmustUnderstand(const AValue: Integer);
begin
  if ( AValue <> 0 ) then
    FmustUnderstand := 1
  else
    FmustUnderstand := 0;
end;

{ TStoredPropertyManager }

procedure TStoredPropertyManager.Error(Const AMsg: string);
begin
  raise EPropertyException.Create(AMsg);
end;

procedure TStoredPropertyManager.Error(
  Const AMsg: string;
  Const AArgs: array of const
);
begin
  raise EPropertyException.CreateFmt(AMsg,AArgs);
end;

procedure TStoredPropertyManager.SetProperty(Const AName, AValue: string);
begin
  FData.Values[AName] := AValue;
end;

procedure TStoredPropertyManager.SetProperties(Const APropsStr: string);
var
  lst : TStringList;
  i : Integer;
begin
  if ( Length(Trim(APropsStr)) = 0 ) then
    Exit;
  lst := TStringList.Create();
  try
    lst.QuoteChar := #0;
    lst.Delimiter := PROP_LIST_DELIMITER;
    lst.DelimitedText := APropsStr;
    for i := 0 to Pred(lst.Count) do
      SetProperty(lst.Names[i],lst.ValueFromIndex[i]);
  finally
    lst.Free();
  end;
end;

function TStoredPropertyManager.GetProperty(Const AName: String): string;
begin
  Result := FData.Values[AName];
end;

function TStoredPropertyManager.GetPropertyNames(ADest: TStrings): Integer;
var
  i : Integer;
begin
  ADest.Clear();
  Result := FData.Count;
  for i := 0 to Pred(Result) do
    ADest.Add(FData.Names[i]);
end;

procedure TStoredPropertyManager.Clear();
begin
  FData.Clear();
end;

procedure TStoredPropertyManager.Copy(
        ASource      : IPropertyManager;
  Const AClearBefore : Boolean
);
var
  lst : TStringList;
  i : Integer;
  s : string;
begin
  if AClearBefore then
    Clear();
  if Assigned(ASource) then begin
    lst := TStringList.Create();
    try
      ASource.GetPropertyNames(lst);
      for i := 0 to Pred(lst.Count) do begin
        s := lst[i];
        SetProperty(s,ASource.GetProperty(s));
      end;
    finally
      lst.Free();
    end;
  end;
end;

constructor TStoredPropertyManager.Create();
begin
  FData := TStringList.Create();
end;

destructor TStoredPropertyManager.Destroy();
begin
  FreeAndNil(FData);
  inherited Destroy();
end;


{ TAbstractComplexRemotable }

class procedure TAbstractComplexRemotable.RegisterAttributeProperty(const AProperty: shortstring);
var
  ri : TSerializeOptions;
begin
  ri := GetSerializeOptionsRegistry().Find(Self);
  if not Assigned(ri) then
    ri := GetSerializeOptionsRegistry().RegisterClass(Self);
  ri.AddAttributeField(AProperty);
end;

class procedure TAbstractComplexRemotable.RegisterAttributeProperties(const APropertList: array of shortstring);
var
  i : Integer;
begin
  for i := Low(APropertList) to High(APropertList) do
    RegisterAttributeProperty(APropertList[i]);
end;

class function TAbstractComplexRemotable.IsAttributeProperty(const AProperty: shortstring): Boolean;
var
  ri : TSerializeOptions;
  pc : TClass;
begin
  Result := False;
  if ( Self = TBaseComplexRemotable ) then
    Exit;
  pc := Self;
  while Assigned(pc) and pc.InheritsFrom(TBaseComplexRemotable) do begin
    ri := GetSerializeOptionsRegistry().Find(TBaseComplexRemotableClass(pc));
    if Assigned(ri) then begin
      Result := ri.IsAttributeField(AProperty);
      Exit;
    end;
    pc := pc.ClassParent;
  end;
end;

procedure TAbstractComplexRemotable.Assign(Source: TPersistent);
var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  p, sp : PPropInfo;
  selfTypeInfo : PTypeInfo;
  srcObj, dstObj : TObject;
begin
  if not Assigned(Source) then
    Exit;
  selfTypeInfo := Self.ClassInfo;
  propCount := GetTypeData(selfTypeInfo)^.PropCount;
  if ( propCount > 0 ) then begin
    propListLen := GetPropList(selfTypeInfo,propList);
    try
      for i := 0 to Pred(propCount) do begin
        p := propList^[i];
        sp := GetPropInfo(Source,p^.Name);
        if Assigned(sp) and Assigned(sp^.GetProc) and
           Assigned(p^.SetProc)
        then begin
          case p^.PropType^.Kind of
            tkInt64{$IFDEF FPC},tkQWord, tkBool{$ENDIF}, tkEnumeration,tkInteger :
              SetOrdProp(Self,p,GetOrdProp(Source,p^.Name));
            tkLString{$IFDEF FPC}, tkAString{$ENDIF} :
              SetStrProp(Self,p,GetStrProp(Source,p^.Name));
            tkClass :
              begin
                srcObj := GetObjectProp(Source,p^.Name);
                dstObj := GetObjectProp(Self,p^.Name);
                if ( not Assigned(dstObj) ) and
                   ( Assigned(srcObj) and srcObj.InheritsFrom(TAbstractComplexRemotable) )
                then begin
                  dstObj := TAbstractComplexRemotableClass(srcObj.ClassType).Create();
                  SetObjectProp(Self,p,dstObj);
                end;
                if Assigned(dstObj) then begin
                  if ( srcObj = nil ) then begin
                    FreeAndNil(dstObj);
                    SetObjectProp(Self,p,dstObj);
                  end else begin
                    if dstObj.InheritsFrom(TPersistent) and srcObj.InheritsFrom(TPersistent) then
                      TPersistent(dstObj).Assign(TPersistent(srcObj));
                  end;
                end;
              end;
            tkFloat :
              SetFloatProp(Self,p,GetFloatProp(Source,p^.Name));
          end;
        end;
      end;
    finally
      Freemem(propList,propListLen*SizeOf(Pointer));
    end;
  end;
end;

{ TBaseComplexSimpleContentRemotable }

class procedure TBaseComplexSimpleContentRemotable.Save(
        AObject: TBaseRemotable;
        AStore: IFormatterBase;
  const AName: string;
  const ATypeInfo: PTypeInfo
);
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  int64Data : Int64;
  strData : String;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumBuffer;
  floatDt : TFloatBuffer;
  p : PPropInfo;
  oldSS : TSerializationStyle;
  tr : TTypeRegistry;
  propName : string;
begin
  oldSS := AStore.GetSerializationStyle();
  AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    SaveValue(AObject,AStore);
    propCount := GetTypeData(ATypeInfo)^.PropCount;
    if ( propCount > 0 ) then begin
      propListLen := GetPropList(ATypeInfo,propList);
      try
        tr := GetTypeRegistry();
        AStore.SetSerializationStyle(ssAttibuteSerialization);
        for i := 0 to Pred(propCount) do begin
          p := propList^[i];
          pt := p^.PropType{$IFNDEF FPC}^{$ENDIF};
          propName := tr.ItemByTypeInfo[pt].GetExternalPropertyName(p^.Name);
          if IsStoredProp(AObject,p) then begin
            case pt^.Kind of
              tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
                begin
                  int64Data := GetOrdProp(AObject,p^.Name);
                  AStore.Put(propName,pt,int64Data);
                end;
              tkLString{$IFDEF FPC},tkAString{$ENDIF} :
                begin
                  strData := GetStrProp(AObject,p^.Name);
                  AStore.Put(propName,pt,strData);
                end;
              tkClass :
                begin
                  objData := GetObjectProp(AObject,p^.Name);
                  AStore.Put(propName,pt,objData);
                end;
              {$IFDEF FPC}
              tkBool :
                begin
                  boolData := Boolean(GetOrdProp(AObject,p^.Name));
                  AStore.Put(propName,pt,boolData);
                end;
              {$ENDIF}
              tkEnumeration,tkInteger :
                begin
                {$IFNDEF FPC}
                  if ( pt^.Kind = tkEnumeration ) and
                     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                  then begin
                    boolData := Boolean(GetOrdProp(AObject,p^.Name));
                    AStore.Put(propName,pt,boolData);
                  end else begin
                {$ENDIF}
                    FillChar(enumData,SizeOf(enumData),#0);
                    case GetTypeData(pt)^.OrdType of
                      otSByte :
                        begin
                          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ShortIntData);
                        end;
                      otUByte :
                        begin
                          enumData.ByteData := Byte(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ByteData);
                        end;
                      otSWord :
                        begin
                          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.SmallIntData);
                        end;
                      otUWord :
                        begin
                          enumData.WordData := Word(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.WordData);
                        end;
                      otSLong :
                        begin
                          enumData.SLongIntData := LongInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.SLongIntData);
                        end;
                      otULong :
                        begin
                          enumData.ULongIntData := LongWord(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ULongIntData);
                        end;
                    end;
                {$IFNDEF FPC}
                  end;
                {$ENDIF}
                end;
              tkFloat :
                begin
                  FillChar(floatDt,SizeOf(floatDt),#0);
                  case GetTypeData(pt)^.FloatType of
                    ftSingle :
                      begin
                        floatDt.SingleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.SingleData);
                      end;
                    ftDouble :
                      begin
                        floatDt.DoubleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.DoubleData);
                      end;
                    ftExtended :
                      begin
                        floatDt.ExtendedData := Extended(GetFloatProp(AObject,p^.Name));
                        AStore.Put(propName,pt,floatDt.ExtendedData);
                      end;
                    ftCurr :
                      begin
                        floatDt.CurrencyData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.CurrencyData);
                      end;
{$IFDEF CPU86}
                    ftComp :
                      begin
                        floatDt.CompData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.CompData);
                      end;
{$ENDIF}
                  end;
                end;
            end;
          end;
        end;
      finally
        Freemem(propList,propListLen*SizeOf(Pointer));
      end;
    end;
  finally
    AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

class procedure TBaseComplexSimpleContentRemotable.Load(
  var AObject: TObject;
      AStore: IFormatterBase;
  var AName: string;
  const ATypeInfo: PTypeInfo
);
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  propName : String;
  int64Data : Int64;
  strData : String;
  objData : TObject;
    objDataCreateHere : Boolean;
  {$IFDEF FPC}boolData : Boolean;{$ENDIF}
  p : PPropInfo;
  enumData : TEnumBuffer;
  floatDt : TFloatExtendedType;
  floatBuffer : TFloatBuffer;
  persistType : TPropStoreType;
  objTypeData : PTypeData;
  oldSS : TSerializationStyle;
  tr : TTypeRegistry;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      If Not Assigned(AObject) Then
        AObject := Create();
      LoadValue(AObject,AStore);
      objTypeData := GetTypeData(ATypeInfo);
      propCount := objTypeData^.PropCount;
      If ( propCount > 0 ) Then Begin
        propListLen := GetPropList(ATypeInfo,propList);
        Try
          tr := GetTypeRegistry();
          AStore.SetSerializationStyle(ssAttibuteSerialization);
          For i := 0 To Pred(propCount) Do Begin
            p := propList^[i];
            persistType := IsStoredPropClass(objTypeData^.ClassType,p);
            If ( persistType in [pstOptional,pstAlways] ) Then Begin
              pt := p^.PropType{$IFNDEF FPC}^{$ENDIF};
              propName := tr.ItemByTypeInfo[pt].GetExternalPropertyName(p^.Name);
              try
                Case pt^.Kind Of
                  tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
                    Begin
                      AStore.Get(pt,propName,int64Data);
                      SetOrdProp(AObject,p^.Name,int64Data);
                    End;
                  tkLString{$IFDEF FPC},tkAString{$ENDIF} :
                    Begin
                      AStore.Get(pt,propName,strData);
                      SetStrProp(AObject,p^.Name,strData);
                    End;
                  {$IFDEF FPC}
                  tkBool :
                    Begin
                      AStore.Get(pt,propName,boolData);
                      SetOrdProp(AObject,p^.Name,Ord(boolData));
                    End;
                  {$ENDIF}
                  tkClass :
                    Begin
                      objData := GetObjectProp(AObject,p^.Name);
                      objDataCreateHere := not Assigned(objData);
                      try
                        AStore.Get(pt,propName,objData);
                        if objDataCreateHere then
                          SetObjectProp(AObject,p^.Name,objData);
                      finally
                        if objDataCreateHere then
                          FreeAndNil(objData);
                      end;
                    End;
                  tkEnumeration,tkInteger :
                    Begin
                      FillChar(enumData,SizeOf(enumData),#0);
                      Case GetTypeData(pt)^.OrdType Of
                        otSByte :
                          Begin
                            AStore.Get(pt,propName,enumData.ShortIntData);
                            int64Data := enumData.ShortIntData;
                          End;
                        otUByte :
                          Begin
                            AStore.Get(pt,propName,enumData.ByteData);
                            int64Data := enumData.ByteData;
                          End;
                        otSWord :
                          Begin
                            AStore.Get(pt,propName,enumData.SmallIntData);
                            int64Data := enumData.SmallIntData;
                          End;
                        otUWord :
                          Begin
                            AStore.Get(pt,propName,enumData.WordData);
                            int64Data := enumData.WordData;
                          End;
                        otSLong:
                          Begin
                            AStore.Get(pt,propName,enumData.SLongIntData);
                            int64Data := enumData.SLongIntData;
                          End;
                        otULong :
                          Begin
                            AStore.Get(pt,propName,enumData.ULongIntData);
                            int64Data := enumData.ULongIntData;
                          End;
                      End;
                      SetOrdProp(AObject,p^.Name,int64Data);
                    End;
                  tkFloat :
                    Begin
                      FillChar(floatDt,SizeOf(floatBuffer),#0);
                      Case GetTypeData(pt)^.FloatType Of
                        ftSingle :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.SingleData);
                            floatDt := floatBuffer.SingleData;
                          End;
                        ftDouble :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.DoubleData);
                            floatDt := floatBuffer.DoubleData;
                          End;
                        ftExtended :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.ExtendedData);
                            floatDt := floatBuffer.ExtendedData;
                          End;
                        ftCurr :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CurrencyData);
                            floatDt := floatBuffer.CurrencyData;
                          End;
                        ftComp :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CompData);
                            floatDt := floatBuffer.CompData;
                          End;
                      End;
                      SetFloatProp(AObject,p^.Name,floatDt);
                    End;
                End;
              except
                on E : EServiceException do begin
                  if ( persistType = pstAlways ) then
                    raise;
                end;
              end;
            End;
          End;
        Finally
          Freemem(propList,propListLen*SizeOf(Pointer));
        End;
      End;
    finally
      AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

{ TComplexInt32SContentRemotable }

class procedure TComplexInt32SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(LongInt),(AObject as TComplexInt32SContentRemotable).Value);
end;

class procedure TComplexInt32SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : LongInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(LongInt),i);
  (AObject as TComplexInt32SContentRemotable).Value := i;
end;

{ TComplexInt32UContentRemotable }

class procedure TComplexInt32UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(LongWord),(AObject as TComplexInt32UContentRemotable).Value);
end;

class procedure TComplexInt32UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : LongWord;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(LongWord),i);
  (AObject as TComplexInt32UContentRemotable).Value := i;
end;

{ TComplexInt16SContentRemotable }

class procedure TComplexInt16SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(SmallInt),(AObject as TComplexInt16SContentRemotable).Value);
end;

class procedure TComplexInt16SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : SmallInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(SmallInt),i);
  (AObject as TComplexInt16SContentRemotable).Value := i;
end;

{ TComplexInt16UContentRemotable }

class procedure TComplexInt16UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Word),(AObject as TComplexInt16UContentRemotable).Value);
end;

class procedure TComplexInt16UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Word;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Word),i);
  (AObject as TComplexInt16UContentRemotable).Value := i;
end;

{ TComplexFloatExtendedContentRemotable }

class procedure TComplexFloatExtendedContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Extended),(AObject as TComplexFloatExtendedContentRemotable).Value);
end;

class procedure TComplexFloatExtendedContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Extended;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Extended),i);
  (AObject as TComplexFloatExtendedContentRemotable).Value := i;
end;

{ TComplexFloatDoubleContentRemotable }

class procedure TComplexFloatDoubleContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Double),(AObject as TComplexFloatDoubleContentRemotable).Value);
end;

class procedure TComplexFloatDoubleContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Double;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Double),i);
  (AObject as TComplexFloatDoubleContentRemotable).Value := i;
end;

{ TComplexStringContentRemotable }

class procedure TComplexStringContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(string),(AObject as TComplexStringContentRemotable).Value);
end;

class procedure TComplexStringContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : string;
begin
  i := '';
  AStore.GetScopeInnerValue(TypeInfo(string),i);
  (AObject as TComplexStringContentRemotable).Value := i;
end;

{ TDateRemotable }

procedure TDateRemotable.SetDate(const AValue: TDateTime);
var
  hh, mn, ss, ssss : Word;
begin
  inherited SetDate(AValue);
  DecodeTime(AsDate,hh,mn,ss,ssss);
  FHour := hh;
  FMinute := mn;
  FSecond := ss;
end;

class function TDateRemotable.FormatDate(const ADate: TDateTime): string;
var
  s, buffer : string;
  d, m, y : Word;
  hh, mn, ss, ssss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?

  DecodeDate(ADate,y,m,d);
    s := IntToStr(y);
    buffer := IntToStr(m);
    if ( m < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s-%s',[s,buffer]);

    buffer := IntToStr(d);
    if ( d < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s-%s',[s,buffer]);

  DecodeTime(ADate,hh,mn,ss,ssss);
    buffer := IntToStr(hh);
    if ( hh < 10 ) then
      buffer := '0' + buffer;
    s := Format('%sT%s',[s,buffer]);

    buffer := IntToStr(mn);
    if ( mn < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s:%s',[s,buffer]);

    buffer := IntToStr(ss);
    if ( ss < 10 ) then
      buffer := '0' + buffer;
    s := Format('%s:%s',[s,buffer]);
    
  Result := s;
end;

class function TDateRemotable.ParseDate(const ABuffer: string): TDateTime;
var
  buffer : string;
  bufferPos, bufferLen : PtrUInt;

  function ReadInt() : PtrUInt;
  var
    neg : Boolean;
    s : shortstring;
  begin
    neg := False;
    Result := 0;

    while ( bufferPos <= bufferLen ) and ( buffer[bufferPos] < #33 ) do begin
      Inc(bufferPos);
    end;
    
    if ( bufferPos <= bufferLen ) then begin
      if ( ABuffer[bufferPos] = '-' ) then begin
        neg := True;
        Inc(bufferPos);
      end;
    end;
    s := '';
    while ( bufferPos <= bufferLen ) and ( buffer[bufferPos] in ['0'..'9'] ) do begin
      s := s + buffer[bufferPos];
      Inc(bufferPos);
    end;
    if ( Length(s) = 0 ) then
      raise EServiceException.Create('Invalid INTEGER BUFFER');
    Result := StrToInt(s);
    if neg then begin
      Result := -Result;
    end;
  end;
  
var
  d, m, y : Word;
  hh, mn, ss : Word;
begin
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?

  buffer := Trim(ABuffer);
  bufferPos := 1;
  bufferLen := Length(buffer);
  if ( bufferLen > 0 ) then begin
    y := ReadInt();
    Inc(bufferPos);

    m := ReadInt();
    Inc(bufferPos);

    d := ReadInt();
    Inc(bufferPos);

    hh := ReadInt();
    Inc(bufferPos);

    mn := ReadInt();
    Inc(bufferPos);

    ss := ReadInt();

    Result := EncodeDate(y,m,d) + EncodeTime(hh,mn,ss,0);
  end else begin
    Result := 0;
  end;
end;

{ TBaseDateRemotable }

procedure TBaseDateRemotable.SetDate(const AValue: TDateTime);
var
  y, m, d : Word;
begin
  DecodeDate(AValue,y,m,d);
  FDate := AValue;
  FYear := y;
  FMonth := m;
  FDay := d;
end;

class procedure TBaseDateRemotable.Save(
        AObject   : TBaseRemotable;
        AStore    : IFormatterBase;
  const AName     : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := FormatDate(TDateRemotable(AObject).AsDate);
  AStore.BeginObject(AName,ATypeInfo);
  try
    AStore.PutScopeInnerValue(TypeInfo(string),buffer);
  finally
    AStore.EndScope();
  end;
end;

class procedure TBaseDateRemotable.Load(
  var AObject     : TObject;
      AStore      : IFormatterBase;
  var AName       : string;
  const ATypeInfo : PTypeInfo
);
var
  strBuffer : string;
begin
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      strBuffer := '';
      AStore.GetScopeInnerValue(TypeInfo(string),strBuffer);
      (AObject as TDateRemotable).AsDate := ParseDate(strBuffer);
    finally
      AStore.EndScopeRead();
    end;
  end;
end;

procedure TBaseDateRemotable.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TDateRemotable) then begin
    FDate := TDateRemotable(Source).AsDate;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TComplexInt8SContentRemotable }

class procedure TComplexInt8SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(ShortInt),(AObject as TComplexInt8SContentRemotable).Value);
end;

class procedure TComplexInt8SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : ShortInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(ShortInt),i);
  (AObject as TComplexInt8SContentRemotable).Value := i;
end;

{ TComplexInt8UContentRemotable }

class procedure TComplexInt8UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Byte),(AObject as TComplexInt8UContentRemotable).Value);
end;

class procedure TComplexInt8UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Byte;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Byte),i);
  (AObject as TComplexInt8UContentRemotable).Value := i;
end;

{ TComplexFloatSingleContentRemotable }

class procedure TComplexFloatSingleContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Single),(AObject as TComplexFloatSingleContentRemotable).Value);
end;

class procedure TComplexFloatSingleContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Single;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Single),i);
  (AObject as TComplexFloatSingleContentRemotable).Value := i;
end;

{ TComplexInt64SContentRemotable }

class procedure TComplexInt64SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Int64),(AObject as TComplexInt64SContentRemotable).Value);
end;

class procedure TComplexInt64SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Int64;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Int64),i);
  (AObject as TComplexInt64SContentRemotable).Value := i;
end;

{ TComplexInt64UContentRemotable }

class procedure TComplexInt64UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(QWord),(AObject as TComplexInt64UContentRemotable).Value);
end;

class procedure TComplexInt64UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : QWord;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(QWord),i);
  (AObject as TComplexInt64UContentRemotable).Value := i;
end;

{ TComplexBooleanContentRemotable }

class procedure TComplexBooleanContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Boolean),(AObject as TComplexBooleanContentRemotable).Value);
end;

class procedure TComplexBooleanContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Boolean;
begin
  i := False;
  AStore.GetScopeInnerValue(TypeInfo(Boolean),i);
  (AObject as TComplexBooleanContentRemotable).Value := i;
end;

{ TIntfPoolItem }

constructor TIntfPoolItem.Create(AIntf: IInterface; const AUsed: Boolean);
begin
  FIntf := AIntf as IInterface;
  FUsed := AUsed;
end;

destructor TIntfPoolItem.Destroy();
begin
  FIntf := nil;
  inherited Destroy();
end;

{ TIntfPool }

function TIntfPool.CreateNew(const AUsed : Boolean): TIntfPoolItem;
begin
  FCS.Acquire();
  try
    Result := TIntfPoolItem.Create(FFactory.CreateInstance(),AUsed);
    FList.Add(Result);
  finally
    FCS.Release();
  end;
end;

function TIntfPool.TryGet(const AIndex: PtrInt): Boolean;
var
  itm : TIntfPoolItem;
begin
  FCS.Acquire();
  try
    itm := TIntfPoolItem(FList[AIndex]);
    Result := not itm.Used;
    if Result then begin
      itm.Used := True;
    end;
  finally
    FCS.Release();
  end;
end;

constructor TIntfPool.Create(
  const AMin, AMax : PtrInt;
        AFactory   : IItemFactory
);
var
  i : PtrInt;
begin
  if not ( ( AMin >= 0 ) and ( AMax >= AMin ) and ( AFactory <> nil ) ) then
    raise Exception.CreateFmt('Invalid pool arguments Min = %d; Max = %d .',[AMin,AMax]);
  FMax := AMax;
  FMin := AMin;
  FFactory := AFactory;
  FLock := TSemaphoreObject.Create(FMax);
  FList := TObjectList.Create(True);
  FCS := TCriticalSection.Create();
  for i := 0 to Pred(AMin) do begin
    CreateNew(False);
  end;
end;

destructor TIntfPool.Destroy();
begin
  FFactory := nil;
  FreeAndNil(FCS);
  FreeAndNil(FLock);
  FreeAndNil(FList);
  inherited Destroy();
end;

function TIntfPool.Get(const ATimeOut : Cardinal): IInterface;
var
  i : PtrInt;
begin
  Result := nil;
  if ( FLock.WaitFor(ATimeOut) = wrSignaled ) then begin
    for i := 0 to Pred(FList.Count) do begin
      if TryGet(i) then begin
        Result := TIntfPoolItem(FList[i]).Intf;
        Break;
      end;
    end;
    if ( Result = nil ) then begin
      Result := CreateNew(True).Intf;
    end;
  end else begin
    raise EServiceException.Create('Unable to create the object : Timeout expired.');
  end;
end;

procedure TIntfPool.Release(const AItem: IInterface);
var
  i : PtrInt;
  a : IInterface;
begin
  a := AItem as IInterface;
  for i := 0 to Pred(FList.Count) do begin
    if ( TIntfPoolItem(FList[i]).Intf = a ) then begin
      TIntfPoolItem(FList[i]).Used := False;
      FLock.Release();
      Break;
    end;
  end;
end;

procedure TIntfPool.Discard(const AItem : IInterface);
var
  i : PtrInt;
  a : IInterface;
  itm : TIntfPoolItem;
begin
  a := AItem as IInterface;
  for i := 0 to Pred(FList.Count) do begin
    if ( TIntfPoolItem(FList[i]).Intf = a ) then begin
      itm := TIntfPoolItem(FList[i]);
      itm.FIntf := FFactory.CreateInstance() as IInterface;
      itm.Used := False;
      FLock.Release();
      Break;
    end;
  end;
end;

function TIntfPool.GetInstancesCount() : PtrInt;
begin
  FCS.Acquire();
  try
    Result := FList.Count;
  finally
    FCS.Release();
  end;
end;

{ TStringBufferRemotable }

class procedure TStringBufferRemotable.Save (
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  if ( AObject <> nil ) then
    buffer := TStringBufferRemotable(AObject).Data
  else
    buffer := '';
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TStringBufferRemotable.Load (
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
  locObj : TStringBufferRemotable;
begin
  buffer := AStore.ReadBuffer(AName);
  if ( AObject = nil ) then
    AObject := Create();
  locObj := AObject as TStringBufferRemotable;;
  locObj.Data := buffer;
end;

procedure TStringBufferRemotable.Assign (Source : TPersistent );
begin
  if ( Source = nil ) then begin
    FData := '';
  end else begin
    if Source.InheritsFrom(TStringBufferRemotable) then
      Self.Data := TStringBufferRemotable(Source).Data
    else
      inherited Assign(Source);
  end;
end;


initialization
{$IFDEF FPC}
  {$IFDEF FPC_211}
  wst_FormatSettings := DefaultFormatSettings;
  wst_FormatSettings.DecimalSeparator := '.';
  {$ENDIF}
{$ELSE}
  GetLocaleFormatSettings(GetThreadLocale(),wst_FormatSettings);
  wst_FormatSettings.DecimalSeparator := '.';
{$ENDIF}

  TypeRegistryInstance := TTypeRegistry.Create();
  SerializeOptionsRegistryInstance := TSerializeOptionsRegistry.Create();

finalization
  FreeAndNil(SerializeOptionsRegistryInstance);
  FreeAndNil(TypeRegistryInstance);
end.
