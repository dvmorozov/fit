{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


unit parserdefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

Type

  ESymbolException = class(Exception)
  End;

  TSymbolTable = class;
  TTypeDefinition = class;
  TForwardTypeDefinition = class;

  { TAbstractSymbolDefinition }

  TAbstractSymbolDefinition = class
  private
    FName: String;
    FExternalAlias : string;
  protected
    procedure SetName(const AName : string);virtual;
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );virtual;
  Public
    constructor Create(Const AName : String);
    procedure RegisterExternalAlias(const AExternalName : String);
    function SameName(const AName : string) : Boolean;virtual;
    Property Name : String Read FName;
    Property ExternalName : String Read FExternalAlias;
  End;

  TAbstractSymbolDefinitionClass = class of TAbstractSymbolDefinition;

  TPascalTokenDefinition = class(TAbstractSymbolDefinition)
  end;
  
  TSymbolTableChange = ( stcAdding, stcDeleting );
  ISymbolTableChangeListner = interface
    ['{0147E0EE-FF1A-4CFA-BD71-3F8E90494EC9}']
    procedure NotifyChange(
            ASender : TSymbolTable;
            AItem   : TAbstractSymbolDefinition;
      const AEvent  : TSymbolTableChange
    );
  end;

  { TAbstractConstantDefinition }

  TAbstractConstantDefinition = class(TAbstractSymbolDefinition) end;

  TSimpleConstantType = ( sctString, sctInteger );
  TSimpleConstantBuffer = record
    case DataType : TSimpleConstantType of
      sctInteger    : ( IntValue : Integer; );
      sctString     : ( StrValue : string[255]; );
  end;

  { TSimpleConstantDefinition }

  TSimpleConstantDefinition = class(TAbstractConstantDefinition)
  private
    FValue: TSimpleConstantBuffer;
  public
    constructor Create(const AName : string; const AValue : string);overload;
    constructor Create(const AName : string; const AValue : Integer);overload;
    property Value : TSimpleConstantBuffer read FValue;
  end;
  
  { TTypeDefinition }

  TTypeDefinition = class(TAbstractSymbolDefinition)
  public
    function NeedFinalization():Boolean;virtual;
  end;
  
  TAnyTypeDefinition = class(TTypeDefinition)
  end;

  { TTypeAliasDefinition }

  TTypeAliasDefinition = class(TTypeDefinition)
  private
    FBaseType: TTypeDefinition;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  public
    constructor Create(const AName : string; ABaseType : TTypeDefinition);
    property BaseType : TTypeDefinition read FBaseType;
  end;
  
  { TSimpleTypeDefinition }

  TSimpleTypeDefinition = class(TTypeDefinition)
  public
    function NeedFinalization():Boolean;override;
  end;
  
  { TForwardTypeDefinition }

  TForwardTypeDefinition = class(TTypeDefinition)
  end;

  TArrayStyle = ( asScoped, asEmbeded );
  
  { TArrayDefinition }

  TArrayDefinition = class(TTypeDefinition)
  private
    FItemExternalName: string;
    FItemName: string;
    FItemType: TTypeDefinition;
    FStyle: TArrayStyle;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  public
    constructor Create(
      const AName              : string;
            AItemType          : TTypeDefinition;
      const AItemName,
            AItemExternalName  : string;
      const AStyle             : TArrayStyle
      );
    function NeedFinalization():Boolean;override;
    property ItemName : string read FItemName;
    property ItemType : TTypeDefinition read FItemType;
    property ItemExternalName : string read FItemExternalName;
    property Style : TArrayStyle read FStyle;
  end;
  
  TEnumTypeDefinition = class;
  
  { TEnumItemDefinition }

  TEnumItemDefinition = class(TAbstractSymbolDefinition)
  private
    FEnumType: TEnumTypeDefinition;
    FOrder: Integer;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  Public
    constructor Create(
      Const AName     : String;
            AEnumType : TEnumTypeDefinition;
      Const AOrder    : Integer
    );
    Property Order : Integer Read FOrder;
    property EnumType : TEnumTypeDefinition read FEnumType;
  End;

  { TEnumTypeDefinition }

  TEnumTypeDefinition = class(TTypeDefinition)
  Private
    FItemList : TObjectList;
  private
    function GetItem(Index: Integer): TEnumItemDefinition;
    function GetItemCount: Integer;
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    function NeedFinalization():Boolean;override;
    Procedure AddItem(AItem:TEnumItemDefinition);
    function FindItem(Const AName:String):TEnumItemDefinition;
    Property ItemCount : Integer Read GetItemCount;
    Property Item[Index:Integer]:TEnumItemDefinition Read GetItem;
  End;

  TStorageOption = ( soAlways, soOptional, soNever );

  { TPropertyDefinition }

  TPropertyDefinition = class(TAbstractSymbolDefinition)
  private
    FDataType: TTypeDefinition;
    FIsAttribute: Boolean;
    FStorageOption: TStorageOption;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  public
    constructor Create(
      Const AName     : String;
            ADataType : TTypeDefinition
    );
    property DataType : TTypeDefinition Read FDataType;
    property IsAttribute : Boolean read FIsAttribute write FIsAttribute;
    property StorageOption : TStorageOption read FStorageOption write FStorageOption;
  End;
  
  { TClassTypeDefinition }

  TClassTypeDefinition = class(TTypeDefinition)
  private
    FParent: TTypeDefinition;
    FPropertyList : TObjectList;
  private
    function GetProperty(const Index : Integer): TPropertyDefinition;
    function GetPropertyCount: Integer;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    function NeedFinalization():Boolean;override;
    function IsDescendantOf(ABaseType : TTypeDefinition) : Boolean;
    procedure SetParent(const AValue: TTypeDefinition);
    function AddProperty(
      Const AName     : String;
            ADataType : TTypeDefinition
    ) : TPropertyDefinition;
    function IndexOfProperty(const AName : string):Integer;
    property Parent : TTypeDefinition read FParent;
    property PropertyCount : Integer read GetPropertyCount;
    property Properties[const Index : Integer] : TPropertyDefinition read GetProperty;
  end;

  TClassTypeDefinitionClass = class of TClassTypeDefinition;

  TNativeClassTypeDefinition = class(TClassTypeDefinition)
  end;
  
  { TNativeSimpleTypeDefinition }

  TNativeSimpleTypeDefinition = class(TSimpleTypeDefinition)
  private
    FBoxedType: TNativeClassTypeDefinition;
  public
    procedure SetBoxedType(ABoxedType : TNativeClassTypeDefinition);
    property BoxedType : TNativeClassTypeDefinition read FBoxedType;
  end;
  
  TParameterModifier = ( pmNone, pmConst, pmVar, pmOut );

  { TParameterDefinition }

  TParameterDefinition = class(TAbstractSymbolDefinition)
  private
    FDataType: TTypeDefinition;
    FModifier: TParameterModifier;
  protected
    procedure SetModifier(const AModifier : TParameterModifier);
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  Public
    constructor Create(
      Const AName     : String;
      Const AModifier : TParameterModifier;
            ADataType     : TTypeDefinition
    );
    property Modifier : TParameterModifier Read FModifier;
    property DataType : TTypeDefinition Read FDataType;
  End;

  TMethodType = ( mtProcedure, mtFunction );
Const
      ParameterModifierMAP : Array[TParameterModifier] Of String =
        ( '', 'Const', 'Var', 'Out' );
Type

  { TMethodDefinition }

  TMethodDefinition = class(TAbstractSymbolDefinition)
  private
    FMethodType: TMethodType;
    FParameterList : TObjectList;
    FProperties: TStrings;
  private
    function GetParameter(Index: Integer): TParameterDefinition;
    function GetParameterCount: Integer;
  protected
    procedure SetMethodType( AMethodType : TMethodType );
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  Public
    constructor Create(Const AName : String; Const AMethodType : TMethodType);
    destructor Destroy();override;
    function AddParameter(
      Const AName     : String;
      Const AModifier : TParameterModifier;
            ADataType     : TTypeDefinition
    ):TParameterDefinition;
    function GetParameterIndex(Const AName : String):Integer;
    function FindParameter(Const AName : String):TParameterDefinition;
    property MethodType : TMethodType Read FMethodType;
    property ParameterCount : Integer Read GetParameterCount;
    property Parameter[Index:Integer] : TParameterDefinition Read GetParameter;
    property Properties : TStrings read FProperties;
  End;

  TBindingStyle = ( bsDocument, bsRPC, bsUnknown );
  
  { TInterfaceDefinition }

  TInterfaceDefinition = class(TAbstractSymbolDefinition)
  Private
    FInterfaceGUID: string;
    FMethodList : TObjectList;
  private
    FAddress: string;
    FBindingStyle: TBindingStyle;
    function GetMethod(Index: Integer): TMethodDefinition;
    function GetMethodCount: Integer;
  protected
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    function GetMethodIndex(Const AName : String):Integer;
    function FindMethod(Const AName : String):TMethodDefinition;
    function AddMethod(
      Const AName : String;
      Const AMethodType : TMethodType
    ):TMethodDefinition;
    function AddMethod(AMthd : TMethodDefinition):TMethodDefinition;
    Property MethodCount : Integer Read GetMethodCount;
    Property Method[Index:Integer] : TMethodDefinition Read GetMethod;
    property InterfaceGUID : string read FInterfaceGUID write FInterfaceGUID;
    property Address : string read FAddress write FAddress;
    property BindingStyle : TBindingStyle read FBindingStyle write FBindingStyle;
  End;
  
  { TSymbolTable }

  TSymbolTable = class(TAbstractSymbolDefinition)
  Private
    FList : TObjectList;
    FLinkedTables : TObjectList;
    FListners : IInterfaceList;
  private
    procedure CheckIndex(Const AIndex : Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): TAbstractSymbolDefinition;
    function GetLinkedTableCount: Integer;
    function GetLinkedTables(Index : Integer): TSymbolTable;
    procedure SetName(const AValue: String);
    procedure ReorderClass(ASym : TClassTypeDefinition);
  protected
    procedure NotifyChange(
            ASender : TSymbolTable;
            AItem   : TAbstractSymbolDefinition;
      const AEvent  : TSymbolTableChange
    );
    procedure FixForwardTypeDefinitions(
      AFrw  : TForwardTypeDefinition;
      Atype : TTypeDefinition
    );override;
  Public
    constructor Create(Const AName : String);
    destructor Destroy();override;
    procedure Clear();
    function Add(ASym : TAbstractSymbolDefinition):Integer;
    procedure Delete(ASym : TAbstractSymbolDefinition);
    function IndexOf(Const AName : String):Integer;overload;
    function IndexOf(
      const AName     : string;
      const AMinClass : TAbstractSymbolDefinitionClass
    ):Integer;overload;
    function IndexOf(ASym : TAbstractSymbolDefinition):Integer;overload;
    function Find(Const AName : String):TAbstractSymbolDefinition;overload;
    function Find(
      const AName     : string;
      const AMinClass : TAbstractSymbolDefinitionClass
    ):TAbstractSymbolDefinition;overload;
    function ByName(Const AName : String):TAbstractSymbolDefinition;
    procedure RegisterListner(AListner : ISymbolTableChangeListner);
    procedure UnregisterListner(AListner : ISymbolTableChangeListner);
    Property Name : String Read FName Write SetName;
    Property Count : Integer Read GetCount;
    Property Item[Index:Integer] : TAbstractSymbolDefinition Read GetItem;default;
    property LinkedTables[Index : Integer] : TSymbolTable read GetLinkedTables;
    property LinkedTableCount : Integer read GetLinkedTableCount;
  End;


  //function CreateSystemSymbolTable() : TSymbolTable;
  procedure AddSystemSymbol(ADest : TSymbolTable);
  procedure AddSoapencSymbol(ADest : TSymbolTable);
  function CreateWstInterfaceSymbolTable() : TSymbolTable;
  function IsReservedKeyWord(const AValue : string):Boolean ;
  
implementation
uses StrUtils, parserutils;

const LANGAGE_TOKEN : array[0..107] of string = (
  'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM',
  'BEGIN', 'BOOLEAN', 'BYTE',
  'CASE', 'CDECL', 'CHAR', 'CLASS', 'COMP', 'CONST', 'CONSTRUCTOR', 'CONTAINS', 'CURRENCY',
  'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOUBLE', 'DOWNTO', 'DYNAMIC',
  'END', 'EXPORT', 'EXPORTS', 'EXTERNAL',
  'FAR', 'FILE', 'FINALLY', 'FOR', 'FORWARD', 'FUNCTION', 'GOTO',
  'ELSE', 'EXCEPT', 'EXTENDED',
  'IF', 'IMPLEMENTATION', 'IMPLEMENTS', 'IN', 'INHERITED', 'INT64', 'INITIALIZATION',
    'INTEGER', 'INTERFACE', 'IS',
  'LABEL', 'LIBRARY', 'LOCAL', 'LONGINT', 'LONGWORD',
  'MOD', 'NEAR', 'NIL', 'NODEFAULT', 'NOT',
  'OBJECT', 'OF', 'OLEVARIANT', 'OR', 'OUT', 'OVERLOAD', 'OVERRIDE',
  'PACKAGE', 'PACKED', 'PASCAL', 'PCHAR', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PUBLISHED',
  'RAISE', 'READ', 'REAL', 'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT', 'REQUIRES', 'RESULT',
  'SAFECALL', 'SET', 'SHL', 'SHORTINT', 'SHR', 'SINGLE', 'SMALLINT', 'STDCALL', 'STORED',
  'THEN', 'TO', 'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES',
  'VAR', 'VARARGS', 'VARIANT', 'VIRTUAL', 'WHILE', 'WIDECHAR', 'WITH', 'WORD', 'WRITE', 'XOR'
);
const WST_RESERVED_TOKEN : array[0..1] of string = ( 'Item', 'Item' );
function IsReservedKeyWord(const AValue : string):Boolean ;
begin
  Result := AnsiMatchText(AValue,LANGAGE_TOKEN) or
            AnsiMatchText(AValue,WST_RESERVED_TOKEN);
end;

{ TAbstractSymbolDefinition }

constructor TAbstractSymbolDefinition.Create(const AName: String);
begin
  Assert(Not IsStrEmpty(AName));
  FName := AName;
  FExternalAlias := FName;
end;

procedure TAbstractSymbolDefinition.RegisterExternalAlias(const AExternalName : String);
begin
  FExternalAlias := AExternalName;
end;

function TAbstractSymbolDefinition.SameName(const AName: string): Boolean;
begin
  Result := AnsiSameText(AName,Self.Name) or AnsiSameText(AName,Self.ExternalName);
end;

procedure TAbstractSymbolDefinition.SetName(const AName: string);
begin
  FName := AName;
end;

procedure TAbstractSymbolDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
begin

end;

{ TParameterDefinition }

procedure TParameterDefinition.SetModifier(const AModifier: TParameterModifier);
begin
  FModifier := AModifier;
end;

procedure TParameterDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
begin
  if ( FDataType = AFrw ) then
    FDataType := Atype;
end;

constructor TParameterDefinition.Create(
  const AName: String;
  const AModifier: TParameterModifier;
  ADataType: TTypeDefinition
);
begin
  Inherited Create(AName);
  Assert(Assigned(ADataType));
  FModifier := AModifier;
  FDataType := ADataType;
end;

{ TMethodDefinition }

function TMethodDefinition.GetParameter(Index: Integer): TParameterDefinition;
begin
  Result := FParameterList[Index] As TParameterDefinition;
end;

function TMethodDefinition.GetParameterCount: Integer;
begin
  Result := FParameterList.Count;
end;

procedure TMethodDefinition.SetMethodType(AMethodType: TMethodType);
begin
  FMethodType := AMethodType;
end;

procedure TMethodDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
var
  i : Integer;
begin
  for i := 0 to Pred(ParameterCount) do
    Parameter[i].FixForwardTypeDefinitions(AFrw, Atype);
end;

constructor TMethodDefinition.Create(
  const AName: String;
  const AMethodType: TMethodType
);
begin
  Inherited Create(AName);
  FMethodType := AMethodType;
  FParameterList := TObjectList.create(True);
  FProperties := TStringList.Create();
end;

destructor TMethodDefinition.Destroy();
begin
  FreeAndNil(FProperties);
  FreeAndNil(FParameterList);
  inherited Destroy();
end;

function TMethodDefinition.AddParameter(
  Const AName     : String;
  Const AModifier : TParameterModifier;
        ADataType     : TTypeDefinition
): TParameterDefinition;
begin
  If ( GetParameterIndex(AName) = -1 ) Then Begin
    Result := TParameterDefinition.Create(AName,AModifier,ADataType);
    FParameterList.Add(Result);
  End Else Begin
    Raise ESymbolException.CreateFmt('Duplicated parameter : %s.%s',[Name,AName]);
  End;
end;

function TMethodDefinition.GetParameterIndex(const AName: String): Integer;
begin
  For Result := 0 To Pred(ParameterCount) Do
    If AnsiSameText(AName,Parameter[Result].Name) Then
      Exit;
  Result := -1;
end;

function TMethodDefinition.FindParameter(
  const AName: String
): TParameterDefinition;
Var
  i : Integer;
begin
  i := GetParameterIndex(AName);
  If ( i > -1 ) Then
    Result := Parameter[i]
  Else
    Result := Nil;
end;

{ TInterfaceDefinition }

function TInterfaceDefinition.GetMethod(Index: Integer): TMethodDefinition;
begin
  Result := FMethodList[Index] As TMethodDefinition;
end;

function TInterfaceDefinition.GetMethodCount: Integer;
begin
  Result := FMethodList.Count;
end;

procedure TInterfaceDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
var
  i : Integer;
begin
  for i := 0 to Pred(MethodCount) do
    Method[i].FixForwardTypeDefinitions(AFrw, Atype);
end;

constructor TInterfaceDefinition.Create(const AName: String);
begin
  Inherited Create(AName);
  FMethodList := TObjectList.create(True);
end;

destructor TInterfaceDefinition.Destroy();
begin
  FreeAndNil(FMethodList);
  inherited Destroy();
end;

function TInterfaceDefinition.GetMethodIndex(const AName: String): Integer;
begin
  For Result := 0 To Pred(MethodCount) Do
    If AnsiSameText(AName,Method[Result].Name) Then
      Exit;
  Result := -1;
end;

function TInterfaceDefinition.FindMethod(const AName: String): TMethodDefinition;
Var
  i : Integer;
begin
  i := GetMethodIndex(AName);
  If ( i > -1 ) Then
    Result := Method[i]
  Else
    Result := Nil;
end;

function TInterfaceDefinition.AddMethod(
  Const AName : String;
  Const AMethodType : TMethodType
):TMethodDefinition;
begin
  if ( GetMethodIndex(Name) = -1 ) then begin
    Result := AddMethod(TMethodDefinition.Create(AName,AMethodType));
  end else begin
    raise ESymbolException.CreateFmt('Duplicated methode : %s.%s',[Name,AName]);
  end;
end;

function TInterfaceDefinition.AddMethod(AMthd: TMethodDefinition): TMethodDefinition;
begin
  if ( GetMethodIndex(AMthd.Name) = -1 ) then begin
    Result := AMthd;
    FMethodList.Add(Result);
  end else begin
    raise ESymbolException.CreateFmt('Duplicated methode : %s.%s',[Name,AMthd.Name]);
  end;
end;

{ TSymbolTable }

procedure TSymbolTable.CheckIndex(const AIndex: Integer);
begin
  If ( AIndex < 0 ) Or ( AIndex >= Count ) Then
    Raise ESymbolException.CreateFmt('Invalid Table Index : %d',[AIndex]);
end;

function TSymbolTable.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSymbolTable.GetItem(Index: Integer): TAbstractSymbolDefinition;
begin
  CheckIndex(Index);
  Result := FList[Index] As TAbstractSymbolDefinition;
end;

function TSymbolTable.GetLinkedTableCount: Integer;
begin
  Result := FLinkedTables.Count;
end;

function TSymbolTable.GetLinkedTables(Index : Integer): TSymbolTable;
begin
  Result := FLinkedTables[Index] as TSymbolTable;
end;

procedure TSymbolTable.SetName(const AValue: String);
begin
  if ( FName = AValue ) then
    Exit;
  FName := AValue;
end;

procedure TSymbolTable.ReorderClass(ASym: TClassTypeDefinition);
var
  i ,j : Integer;
  locSymb : TClassTypeDefinition;
begin
  locSymb := ASym;
  while True do begin
    if not Assigned(locSymb.Parent) then
      Exit;
    i := FList.IndexOf(locSymb);
    if ( i < 0 ) then
      Exit;
    j := FList.IndexOf(locSymb.Parent);
    if ( j < 0 ) then
      Exit;
    //if ( i > j ) then
      //Exit;
    if ( i < j ) then
      FList.Exchange(i,j);
    if not locSymb.Parent.InheritsFrom(TClassTypeDefinition) then
      Exit;
    locSymb := locSymb.Parent as TClassTypeDefinition;
  end;
end;

procedure TSymbolTable.NotifyChange(
        ASender : TSymbolTable;
        AItem   : TAbstractSymbolDefinition;
  const AEvent  : TSymbolTableChange
);
var
  i : Integer;
begin
  for i := 0 to Pred(FListners.Count) do
    (FListners[i] as ISymbolTableChangeListner).NotifyChange(ASender,AItem,AEvent);
end;

procedure TSymbolTable.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
var
  i : Integer;
begin
  for i := 0 to Pred(Count) do
    Item[i].FixForwardTypeDefinitions(AFrw, Atype);
end;

constructor TSymbolTable.Create(Const AName : String);
begin
  Inherited Create(AName);
  FList := TObjectList.Create(True);
  FLinkedTables := TObjectList.Create(False);
  FListners := TInterfaceList.Create();
end;

destructor TSymbolTable.Destroy();
begin
  if Assigned(FList) then
    Clear();
  FreeAndNil(FList);
  FreeAndNil(FLinkedTables);
  FListners := nil;
  inherited Destroy();
end;

procedure TSymbolTable.Clear();
var
  i : Integer;
begin
  FLinkedTables.Clear();
  for i := 0 to Pred(FList.Count) do
    Delete(FList[0] as TAbstractSymbolDefinition);
end;

function TSymbolTable.Add(ASym: TAbstractSymbolDefinition): Integer;
var
  i : Integer;
  locNeedFix : Boolean;
  frwdTyp : TForwardTypeDefinition;
begin
  Result := IndexOf(ASym);
  If ( Result = -1 ) Then Begin
    locNeedFix := False;
    i := IndexOf(ASym.Name);
    if ( i <> -1 ) then begin
      if Item[i].InheritsFrom(TForwardTypeDefinition) and
        ( not ASym.InheritsFrom(TForwardTypeDefinition) )
      then
        locNeedFix := True
      else
        raise ESymbolException.CreateFmt('Duplicated symbol name %s : ( %s/%s ), ( %s/%s )',[ASym.Name,Item[i].ClassName,Item[i].ExternalName,ASym.ClassName,ASym.ExternalName]);
    end;
    NotifyChange(Self,ASym,stcAdding);
    Result := FList.Add(ASym);
    if ASym.InheritsFrom(TSymbolTable) then
      FLinkedTables.Add(ASym);
    if locNeedFix then begin
      frwdTyp := Item[i] as TForwardTypeDefinition;
      FixForwardTypeDefinitions( frwdTyp, (ASym as TTypeDefinition ) );
      FList.Exchange(i,Result);
      Delete(frwdTyp);
    end;
    Result := IndexOf(ASym);
  End;
end;

procedure TSymbolTable.Delete(ASym: TAbstractSymbolDefinition);
var
  i : Integer;
begin
  if Assigned(ASym) then begin
    i := FList.IndexOf(ASym);
    if ( i >= 0 ) then begin
      NotifyChange(Self,ASym,stcDeleting);
      FList.Delete(i);
    end;
  end;
end;

function TSymbolTable.IndexOf(const AName: String): Integer;
begin
  for Result := 0 to Pred(Count) do
    if Item[Result].SameName(AName) then
      Exit;
  Result := -1;
end;

function TSymbolTable.IndexOf(
  const AName     : string;
  const AMinClass : TAbstractSymbolDefinitionClass
): Integer;
var
  syb : TAbstractSymbolDefinition;
begin
  for Result := 0 to Pred(Count) do begin
    syb := Item[Result];
    if syb.SameName(AName) and syb.InheritsFrom(AMinClass) then
      Exit;
  end;
  Result := -1;
end;

function TSymbolTable.IndexOf(ASym: TAbstractSymbolDefinition): Integer;
begin
  Result := FList.IndexOf(ASym);
end;

function TSymbolTable.Find(const AName: String): TAbstractSymbolDefinition;
Var
  i : Integer;
begin
  i := IndexOf(AName);
  if ( i > -1 ) then begin
    Result := Item[i]
  end else begin
    for i := 0 to Pred(LinkedTableCount) do begin
      Result := LinkedTables[i].Find(AName);
      if Assigned(Result) then
        Exit;
    end;
    Result := Nil;
  end;
end;

function TSymbolTable.Find(
  const AName     : string;
  const AMinClass : TAbstractSymbolDefinitionClass
): TAbstractSymbolDefinition;
var
  i : Integer;
begin
  i := IndexOf(AName,AMinClass);
  if ( i > -1 ) then begin
    Result := Item[i]
  end else begin
    for i := 0 to Pred(LinkedTableCount) do begin
      Result := LinkedTables[i].Find(AName,AMinClass);
      if Assigned(Result) then
        Exit;
    end;
    Result := Nil;
  end;
end;

function TSymbolTable.ByName(const AName: String): TAbstractSymbolDefinition;
begin
  Result := Find(AName);
  If Not Assigned(Result) Then
    Raise ESymbolException.CreateFmt('No such Symbol : %s',[AName]);
end;

procedure TSymbolTable.RegisterListner(AListner: ISymbolTableChangeListner);
begin
  if Assigned(AListner) and ( FListners.IndexOf(AListner) < 0 ) then
    FListners.Add(AListner);
end;

procedure TSymbolTable.UnregisterListner(AListner: ISymbolTableChangeListner);
begin
  if Assigned(AListner) and ( FListners.IndexOf(AListner) >= 0 ) then
    FListners.Remove(AListner);
end;

{ TEnumItemDefinition }

procedure TEnumItemDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
begin
  if ( TObject(AFrw) = TObject(FEnumType) ) then
    FEnumType := Atype as TEnumTypeDefinition;
end;

constructor TEnumItemDefinition.Create(
  const AName     : string;
        AEnumType : TEnumTypeDefinition;
  const AOrder    : Integer
);
begin
  Assert(Assigned(AEnumType));
  inherited Create(AName);
  FOrder := AOrder;
  FEnumType := AEnumType;
end;

{ TEnumTypeDefinition }

function TEnumTypeDefinition.GetItem(Index: Integer): TEnumItemDefinition;
begin
  Result := FItemList[Index] As TEnumItemDefinition;
end;

function TEnumTypeDefinition.GetItemCount: Integer;
begin
  Result := FItemList.Count;
end;

constructor TEnumTypeDefinition.Create(const AName: String);
begin
  Inherited Create(AName);
  FItemList := TObjectList.Create(False);
end;

destructor TEnumTypeDefinition.Destroy();
begin
  FItemList.Free();
  inherited Destroy();
end;

function TEnumTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := False;
end;

procedure TEnumTypeDefinition.AddItem(AItem:TEnumItemDefinition);
Begin
  If ( FItemList.IndexOf(AItem) = -1 ) Then
    FItemList.Add(AItem);
end;

function TEnumTypeDefinition.FindItem(const AName: String): TEnumItemDefinition;
Var
  i,c : Integer;
begin
  c := Pred(ItemCount);
  For i := 0 To c Do Begin
    If AnsiSameText(AName,Item[i].Name) Then Begin
      Result := Item[i];
      Exit;
    End;
  End;
  Result := Nil;
end;

{ TTypeDefinition }
const SIMPLE_TYPES : Array[0..14] Of array[0..2] of string = (
        ('string', 'TComplexStringContentRemotable', 'string'),
        ('integer', 'TComplexInt32SContentRemotable', 'int'),
        ('LongWord', 'TComplexInt32UContentRemotable', 'unsignedInt' ),
        ('SmallInt', 'TComplexInt16SContentRemotable', 'short'),
        ('ShortInt', 'TComplexInt8SContentRemotable', 'byte'),
        ('char', '', ''),
        ('boolean', 'TComplexBooleanContentRemotable', 'boolean'),
        ('Byte', 'TComplexInt8UContentRemotable', 'unsignedByte'),
        ('Word', 'TComplexInt16UContentRemotable', 'unsignedShort'),
        ('Longint', 'TComplexInt32SContentRemotable', 'int'),
        ('Int64', 'TComplexInt64SContentRemotable', 'long'),
        ('Qword', 'TComplexInt64UContentRemotable', 'unsignedLong'),
        ('Single', 'TComplexFloatSingleContentRemotable', 'single'),
        ('Double', 'TComplexFloatDoubleContentRemotable', 'double'),
        ('Extended', 'TComplexFloatExtendedContentRemotable', 'decimal')
      );

function TTypeDefinition.NeedFinalization(): Boolean;
var
  i : Integer;
begin
  for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
    if AnsiSameText(SIMPLE_TYPES[i][0],Name) then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{ TClassTypeDefinition }

procedure TClassTypeDefinition.SetParent(const AValue: TTypeDefinition);
begin
  if ( AValue = Self ) then begin
    raise ESymbolException.Create('A class can not be its parent.');
  end;
  if ( FParent = AValue ) then begin
    Exit;
  end;
  FParent := AValue;
end;

function TClassTypeDefinition.AddProperty(
  const AName     : String;
        ADataType : TTypeDefinition
): TPropertyDefinition;
var
  i : Integer;
begin
  if not Assigned(ADataType) then
    raise ESymbolException.CreateFmt('Property data type not provided : "%s".',[AName]);
  i := IndexOfProperty(AName);
  if ( i = -1 ) then
    i := FPropertyList.Add(TPropertyDefinition.Create(AName,ADataType));
  Result := FPropertyList[i] as TPropertyDefinition;
end;

function TClassTypeDefinition.IndexOfProperty(const AName: string): Integer;
begin
  for Result := 0 to Pred(PropertyCount) do begin
    if AnsiSameText(AName,Properties[Result].Name) then
      Exit;
  end;
  Result := -1;
end;

function TClassTypeDefinition.GetProperty(const Index : Integer): TPropertyDefinition;
begin
  Result := FPropertyList[Index] as TPropertyDefinition;
end;

function TClassTypeDefinition.GetPropertyCount: Integer;
begin
  Result := FPropertyList.Count;
end;

procedure TClassTypeDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
var
  i : Integer;
begin
  if ( FParent = AFrw ) then
    FParent := Atype;
  for i := 0 to Pred(PropertyCount) do begin
    Properties[i].FixForwardTypeDefinitions(AFrw,Atype);
  end;
end;

constructor TClassTypeDefinition.Create(const AName: String);
begin
  inherited Create(AName);
  FPropertyList := TObjectList.Create(True);
end;

destructor TClassTypeDefinition.Destroy();
begin
  FreeAndNil(FPropertyList);
  inherited Destroy();
end;

function TClassTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := True;
end;

function TClassTypeDefinition.IsDescendantOf(ABaseType: TTypeDefinition): Boolean;
var
  tmpDef : TTypeDefinition;
begin
  tmpDef := Self;
  while Assigned(tmpDef) do begin
    if ( tmpDef = ABaseType ) then begin
      Result := True;
      Exit;
    end;
    if tmpDef is TClassTypeDefinition then begin
      tmpDef := (tmpDef as TClassTypeDefinition).Parent;
    end else begin
      tmpDef := nil;
    end;
  end;
  Result := False;
end;


{ TPropertyDefinition }

procedure TPropertyDefinition.FixForwardTypeDefinitions(
  AFrw  : TForwardTypeDefinition;
  Atype : TTypeDefinition
);
begin
  if ( FDataType = AFrw ) then
    FDataType := Atype;
end;

constructor TPropertyDefinition.Create(
  const AName     : String;
        ADataType : TTypeDefinition
);
begin
  inherited Create(AName);
  FDataType := ADataType;
end;

{ TSimpleTypeDefinition }

function TSimpleTypeDefinition.NeedFinalization(): Boolean;
begin
  Result := False;
end;

procedure AddSystemSymbol(ADest: TSymbolTable);
var
  i : Integer;
  splTyp : TNativeSimpleTypeDefinition;
  syb : TNativeClassTypeDefinition;
  s : string;
begin
  for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
    splTyp := TNativeSimpleTypeDefinition.Create(SIMPLE_TYPES[i][0]);
    ADest.Add(splTyp);
    s := SIMPLE_TYPES[i][1];
    if not IsStrEmpty(s) then begin
      syb := ADest.Find(SIMPLE_TYPES[i][1]) as TNativeClassTypeDefinition;
      if not Assigned(syb) then begin
        syb := TNativeClassTypeDefinition.Create(SIMPLE_TYPES[i][1]);
      end;
      ADest.Add(syb);
      //syb.RegisterExternalAlias(SIMPLE_TYPES[i][2]);
      splTyp.SetBoxedType(syb);
    end;
  end;
  for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
    splTyp := ADest.ByName(SIMPLE_TYPES[i][0]) as TNativeSimpleTypeDefinition;
    if not IsStrEmpty(SIMPLE_TYPES[i][2]) then begin
      splTyp.RegisterExternalAlias(SIMPLE_TYPES[i][2]);
    end;
  end;
end;

procedure AddSoapencSymbol(ADest: TSymbolTable);
var
  locSymTable : TSymbolTable;
begin
  locSymTable := TSymbolTable.Create('soapenc');
  ADest.Add(locSymTable);
  locSymTable.RegisterExternalAlias('http://schemas.xmlsoap.org/soap/encoding/');
  locSymTable.Add(TAnyTypeDefinition.Create('any'));
end;

function CreateWstInterfaceSymbolTable() : TSymbolTable;
  function AddClassDef(
          ATable      : TSymbolTable;
    const AClassName,
          AParentName : string;
    const AClassType  : TClassTypeDefinition = nil
  ):TClassTypeDefinition;
  begin
    if Assigned(AClassType) then begin
      Result := AClassType.Create(AClassName);
    end else begin
      Result := TClassTypeDefinition.Create(AClassName);
    end;
    if not IsStrEmpty(AParentName) then
      Result.SetParent(ATable.ByName(AParentName) as TClassTypeDefinition);
    ATable.Add(Result);
  end;

var
  loc_TBaseComplexSimpleContentRemotable : TClassTypeDefinition;
  locTyp : TTypeDefinition;
begin
  Result := TSymbolTable.Create('base_service_intf');
  try
    AddSystemSymbol(Result);
    AddClassDef(Result,'TBaseRemotable','');
      AddClassDef(Result,'TAbstractSimpleRemotable','TBaseRemotable');
        AddClassDef(Result,'TDateRemotable','TAbstractSimpleRemotable').RegisterExternalAlias('dateTime');
        AddClassDef(Result,'TDurationRemotable','TAbstractSimpleRemotable').RegisterExternalAlias('duration');
        AddClassDef(Result,'TTimeRemotable','TAbstractSimpleRemotable').RegisterExternalAlias('time');

      AddClassDef(Result,'TAbstractComplexRemotable','TBaseRemotable');
        loc_TBaseComplexSimpleContentRemotable := AddClassDef(Result,'TBaseComplexSimpleContentRemotable','TAbstractComplexRemotable');
          (Result.ByName('TComplexInt16SContentRemotable') as TClassTypeDefinition).SetParent(loc_TBaseComplexSimpleContentRemotable);
          (Result.ByName('TComplexFloatDoubleContentRemotable') as TClassTypeDefinition).SetParent(loc_TBaseComplexSimpleContentRemotable);

        AddClassDef(Result,'TBaseComplexRemotable','TAbstractComplexRemotable');
          AddClassDef(Result,'THeaderBlock','TBaseComplexRemotable');
        AddClassDef(Result,'TBaseArrayRemotable','TAbstractComplexRemotable');
          AddClassDef(Result,'TBaseObjectArrayRemotable','TBaseArrayRemotable');
          AddClassDef(Result,'TBaseSimpleTypeArrayRemotable','TBaseArrayRemotable');
            AddClassDef(Result,'TArrayOfStringRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfBooleanRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt8URemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt8SRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt16SRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt16URemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt32URemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt32SRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt64SRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfInt64URemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfFloatSingleRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfFloatDoubleRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfFloatExtendedRemotable','TBaseSimpleTypeArrayRemotable');
            AddClassDef(Result,'TArrayOfFloatCurrencyRemotable','TBaseSimpleTypeArrayRemotable');

    locTyp := TTypeAliasDefinition.Create('token',Result.ByName('string') as TTypeDefinition);
    Result.Add(locTyp);
    locTyp := TTypeAliasDefinition.Create('anyURI',Result.ByName('string') as TTypeDefinition);
    Result.Add(locTyp);
    locTyp := TTypeAliasDefinition.Create('float',Result.ByName('Single') as TTypeDefinition);
    Result.Add(locTyp);
    locTyp := TTypeAliasDefinition.Create('nonNegativeInteger',Result.ByName('LongWord') as TTypeDefinition);
    Result.Add(locTyp);
    locTyp := TTypeAliasDefinition.Create('positiveInteger',Result.ByName('nonNegativeInteger') as TTypeDefinition);
    Result.Add(locTyp);
    
    locTyp := TTypeAliasDefinition.Create('base64Binary',Result.ByName('string') as TTypeDefinition);
    Result.Add(locTyp);

  except //base64Binary
    FreeAndNil(Result);
    raise;
  end;
end;

{ TTypeAliasDefinition }

procedure TTypeAliasDefinition.FixForwardTypeDefinitions(
  AFrw: TForwardTypeDefinition;
  Atype: TTypeDefinition
);
begin
  if ( FBaseType = AFrw ) then
    FBaseType := Atype;
end;

constructor TTypeAliasDefinition.Create(
  const AName     : string;
        ABaseType : TTypeDefinition
);
begin
  Assert(Assigned(ABaseType));
  inherited Create(AName);
  FBaseType := ABaseType;
end;

{ TSimpleConstantDefinition }

constructor TSimpleConstantDefinition.Create(const AName: string;const AValue: string);
begin
  inherited Create(AName);
  FValue.DataType := sctString;
  FValue.StrValue := AValue;
end;

constructor TSimpleConstantDefinition.Create(const AName: string;const AValue: Integer);
begin
  inherited Create(AName);
  FValue.DataType := sctInteger;
  FValue.IntValue := AValue;
end;

{ TArrayDefinition }

procedure TArrayDefinition.FixForwardTypeDefinitions(
  AFrw: TForwardTypeDefinition;
  Atype: TTypeDefinition
);
begin
  if ( FItemType = AFrw ) then
    FItemType := Atype;
end;

constructor TArrayDefinition.Create(
  const AName              : string;
        AItemType          : TTypeDefinition;
  const AItemName,
        AItemExternalName  : string;
  const AStyle             : TArrayStyle
);
begin
  Assert(Assigned(AItemType));
  inherited Create(AName);
  FStyle := AStyle;
  FItemType := AItemType;
  FItemName := AItemName;
  FItemExternalName := AItemExternalName;
  if IsStrEmpty(FItemExternalName) then
    FItemExternalName := FItemName;
end;

function TArrayDefinition.NeedFinalization(): Boolean;
begin
  Result := True;
end;

{ TNativeSimpleTypeDefinition }

procedure TNativeSimpleTypeDefinition.SetBoxedType(ABoxedType: TNativeClassTypeDefinition);
begin
  FBoxedType := ABoxedType;
end;

end.
