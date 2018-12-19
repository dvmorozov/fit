{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit pascal_parser_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  pparser, pastree;

const
  sEXTERNAL_NAME = '_E_N_';
  sATTRIBUTE = '_ATTRIBUTE_';
  sARRAY_ITEM_NAME = 'ARRAY_ITEM_NAME';
  sARRAY_ITEM_EXT_NAME = 'ARRAY_ITEM_EXT_NAME';
  sARRAY_STYLE = 'ARRAY_STYLE';
    sARRAY_STYLE_SCOPED = 'ARRAY_STYLE_SCOPED';
    sARRAY_STYLE_EMBEDDED = 'ARRAY_STYLE_EMBEDDED';
  
  sXSD_NS = 'http://www.w3.org/2001/XMLSchema';
  
type

  TBindingStyle = ( bsDocument, bsRPC, bsUnknown );
  
const
  BindingStyleNames : array[TBindingStyle] of string = ( 'Document', 'RPC', 'Unknown' );
  
type
  TArrayStyle = ( asScoped, asEmbeded );

  ESymbolException = class(Exception)
  end;
  { TwstBinding }

  TwstBinding = class(TPasElement)
  private
    FAddress: string;
    FBindingStyle: TBindingStyle;
    FIntf: TPasClassType;
  public
    constructor Create(
      const AName : string;
            AIntf : TPasClassType;
            AParent: TPasElement
    );
    destructor Destroy();override;
    property Intf : TPasClassType read FIntf;
    property Address : string read FAddress write FAddress;
    property BindingStyle : TBindingStyle read FBindingStyle write FBindingStyle;
  end;
  
  { TPropertyHolder }

  TPropertyHolder = class
  private
    FObjects : TObjectList;
    FProps : TObjectList;
  private
  public
    constructor Create();
    destructor Destroy();override;
    procedure SetValue(AOwner : TObject; const AName, AValue : string);
    function GetValue(AOwner : TObject; const AName : string) : string;
    function FindList(AOwner : TObject) : TStrings;
    function GetList(AOwner : TObject) : TStrings;
  end;
  
  { TwstPasTreeContainer }

  TwstPasTreeContainer = class(TPasTreeContainer)
  private
    FCurrentModule: TPasModule;
    FBindingList : TObjectList;
    FProperties : TPropertyHolder;
  private
    function GetBinding(AIndex : Integer): TwstBinding;
    function GetBindingCount: Integer;
  public
    constructor Create();
    destructor Destroy();override;
    function CreateElement(
            AClass              : TPTreeElement;
      const AName               : String;
            AParent             : TPasElement;
            AVisibility         : TPasMemberVisibility;
      const ASourceFilename     : String;
            ASourceLinenumber   : Integer
    ): TPasElement;override;
    function CreateArray(
      const AName              : string;
            AItemType          : TPasType;
      const AItemName,
            AItemExternalName  : string;
      const AStyle             : TArrayStyle
    ) : TPasArrayType;
    function GetArrayItemName(AArray : TPasArrayType) : string;
    function GetArrayItemExternalName(AArray : TPasArrayType) : string;
    function GetArrayStyle(AArray : TPasArrayType) : TArrayStyle;
    procedure SetArrayStyle(AArray : TPasArrayType; const AStyle : TArrayStyle);
    procedure SetArrayItemExternalName(AArray : TPasArrayType; const AExternalName : string);
    function FindElement(const AName: String): TPasElement; override;
    function FindElementInModule(const AName: String; AModule: TPasModule): TPasElement;
    function FindModule(const AName: String): TPasModule;override;
    function IsEnumItemNameUsed(const AName : string) : Boolean;
    procedure SetCurrentModule(AModule : TPasModule);
    property CurrentModule : TPasModule read FCurrentModule;
    
    function AddBinding(const AName : string; AIntf : TPasClassType):TwstBinding;
    procedure DeleteBinding(ABinding : TwstBinding);
    function FindBinding(const AName : string):TwstBinding;overload;
    function FindBinding(const AIntf : TPasClassType; const AOrder : Integer = 0):TwstBinding;overload;
    property BindingCount : Integer read GetBindingCount;
    property Binding[AIndex : Integer] : TwstBinding read GetBinding;
    property Properties : TPropertyHolder read FProperties;
    
    procedure RegisterExternalAlias(AObject : TPasElement; const AExternalName : String);
    function SameName(AObject : TPasElement; const AName : string) : Boolean;
    function GetExternalName(AObject : TPasElement) : string;
    function IsAttributeProperty(AObject : TPasProperty) : Boolean;
    procedure SetPropertyAsAttribute(AObject : TPasProperty; const AValue : Boolean);

    function IsInitNeed(AType: TPasType): Boolean;
    function IsOfType(AType: TPasType; AClass: TClass): Boolean;
  end;
  
  TPasClassTypeClass = class of TPasClassType;
  TPasNativeClassType = class(TPasClassType) end;
  TPasNativeSimpleContentClassType = class(TPasNativeClassType) end;

  { TPasNativeSimpleType }

  TPasNativeSimpleType = class(TPasType)
  private
    FBoxedType: TPasNativeSimpleContentClassType;
  public
    destructor Destroy();override;
    procedure SetBoxedType(ABoxedType : TPasNativeSimpleContentClassType);
    property BoxedType : TPasNativeSimpleContentClassType read FBoxedType;
  end;
  
  function GetParameterIndex(
          AProcType  : TPasProcedureType;
    const AParamName : string;
    const AStartPos  : Integer = 0
  ) : Integer;
  function FindParameter(
          AProcType  : TPasProcedureType;
    const AParamName : string;
    const AStartPos  : Integer = 0
  ) : TPasArgument;
  function FindMember(AClass : TPasClassType; const AName : string) : TPasElement ;
  function GetElementCount(AList : TList; AElementClass : TPTreeElement):Integer ;
  
  function GetUltimeType(AType : TPasType) : TPasType;
  function MakeInternalSymbolNameFrom(const AName : string) : string ;


  function CreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
  
implementation
uses parserutils;

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

procedure AddSystemSymbol(
  ADest : TPasModule;
  AContainer : TwstPasTreeContainer
);
var
  i : Integer;
  splTyp : TPasNativeSimpleType;
  syb : TPasNativeSimpleContentClassType;
  s : string;
begin
  for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
    splTyp := TPasNativeSimpleType(AContainer.CreateElement(TPasNativeSimpleType,SIMPLE_TYPES[i][0],ADest.InterfaceSection,visPublic,'',0));
    ADest.InterfaceSection.Declarations.Add(splTyp);
    ADest.InterfaceSection.Types.Add(splTyp);
    s := SIMPLE_TYPES[i][1];
    if not IsStrEmpty(s) then begin
      syb := AContainer.FindElementInModule(SIMPLE_TYPES[i][1],ADest) as TPasNativeSimpleContentClassType;
      if not Assigned(syb) then begin
        syb := TPasNativeSimpleContentClassType(AContainer.CreateElement(TPasNativeSimpleContentClassType,s,ADest.InterfaceSection,visDefault,'',0));
        ADest.InterfaceSection.Declarations.Add(syb);
        ADest.InterfaceSection.Types.Add(splTyp);
      end;
      splTyp.SetBoxedType(syb);
    end;
  end;
  for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
    splTyp := AContainer.FindElementInModule(SIMPLE_TYPES[i][0],ADest) as TPasNativeSimpleType;
    if not IsStrEmpty(SIMPLE_TYPES[i][2]) then begin
      AContainer.RegisterExternalAlias(splTyp,SIMPLE_TYPES[i][2]);
      if ( splTyp.BoxedType <> nil ) then begin
        AContainer.RegisterExternalAlias(splTyp.BoxedType,SIMPLE_TYPES[i][2]);
      end;
    end;
  end;
end;

function CreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
  function AddClassDef(
          ATable      : TPasModule;
    const AClassName,
          AParentName : string;
    const AClassType  : TPasClassTypeClass = nil
  ):TPasClassType;
  var
    locClassType : TPasClassTypeClass;
  begin
    if Assigned(AClassType) then begin
      locClassType := AClassType;
    end else begin
      locClassType := TPasClassType;
    end;
    Result := TPasClassType(AContainer.CreateElement(locClassType,AClassName,ATable.InterfaceSection,visDefault,'',0));
    if not IsStrEmpty(AParentName) then begin
      Result.AncestorType := AContainer.FindElementInModule(AParentName,ATable) as TPasType;
      if Assigned(Result.AncestorType) then
        Result.AncestorType.AddRef();
    end;
    ATable.InterfaceSection.Classes.Add(Result);
    ATable.InterfaceSection.Declarations.Add(Result);
    ATable.InterfaceSection.Types.Add(Result);
  end;

  function AddAlias(const AName, ABaseType : string; ATable : TPasModule) : TPasTypeAliasType;
  begin
    Result := TPasTypeAliasType(AContainer.CreateElement(TPasAliasType,AName,ATable,visPublic,'',0));
    Result.DestType := AContainer.FindElementInModule(ABaseType,ATable) as TPasType;
    if Assigned(Result.DestType) then
      Result.DestType.AddRef();
    ATable.InterfaceSection.Declarations.Add(Result);
    ATable.InterfaceSection.Classes.Add(Result);
    ATable.InterfaceSection.Types.Add(Result);
  end;
  
var
  loc_TBaseComplexSimpleContentRemotable : TPasClassType;
begin
  Result := TPasModule(AContainer.CreateElement(TPasModule,'base_service_intf',AContainer.Package,visPublic,'',0));
  try
    AContainer.RegisterExternalAlias(Result,sXSD_NS);
    Result.InterfaceSection := TPasSection(AContainer.CreateElement(TPasSection,'',Result,visDefault,'',0));
    AddSystemSymbol(Result,AContainer);
    AddClassDef(Result,'TBaseRemotable','',TPasNativeClassType);
      AddClassDef(Result,'TAbstractSimpleRemotable','TBaseRemotable',TPasNativeClassType);
        AContainer.RegisterExternalAlias(AddClassDef(Result,'TDateRemotable','TAbstractSimpleRemotable'),'dateTime');
        AContainer.RegisterExternalAlias(AddClassDef(Result,'TDurationRemotable','TAbstractSimpleRemotable'),'duration');
        AContainer.RegisterExternalAlias(AddClassDef(Result,'TTimeRemotable','TAbstractSimpleRemotable'),'time');

      AddClassDef(Result,'TAbstractComplexRemotable','TBaseRemotable',TPasNativeClassType);
        loc_TBaseComplexSimpleContentRemotable := AddClassDef(Result,'TBaseComplexSimpleContentRemotable','TAbstractComplexRemotable',TPasNativeClassType);
          (AContainer.FindElementInModule('TComplexInt16SContentRemotable',Result) as TPasClassType).AncestorType := loc_TBaseComplexSimpleContentRemotable;
          (AContainer.FindElementInModule('TComplexFloatDoubleContentRemotable',Result) as TPasClassType).AncestorType := loc_TBaseComplexSimpleContentRemotable;

        AddClassDef(Result,'TBaseComplexRemotable','TAbstractComplexRemotable',TPasNativeClassType);
          AddClassDef(Result,'THeaderBlock','TBaseComplexRemotable',TPasNativeClassType);
        AddClassDef(Result,'TBaseArrayRemotable','TAbstractComplexRemotable',TPasNativeClassType);
          AddClassDef(Result,'TBaseObjectArrayRemotable','TBaseArrayRemotable',TPasNativeClassType);
          AddClassDef(Result,'TBaseSimpleTypeArrayRemotable','TBaseArrayRemotable',TPasNativeClassType);
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

    AddAlias('token','string',Result);
    AddAlias('anyURI','string',Result);
    AddAlias('float','Single',Result);
    AddAlias('nonNegativeInteger','LongWord',Result);
    AddAlias('positiveInteger','nonNegativeInteger',Result);
    AddAlias('base64Binary','string',Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function GetUltimeType(AType : TPasType) : TPasType;
begin
  Result := AType;
  if ( Result <> nil ) then begin
    while Result.InheritsFrom(TPasAliasType) and
          ( TPasAliasType(Result).DestType <> nil )
    do begin
      Result := TPasAliasType(Result).DestType;
    end;
  end;
end;

function GetElementCount(AList : TList; AElementClass : TPTreeElement):Integer ;
var
  i : Integer;
begin
  Result := 0;
  if Assigned(AList) then begin
    for i := 0 to Pred(AList.Count) do begin
      if TObject(AList[i]).InheritsFrom(AElementClass) then begin
        Inc(Result);
      end;
    end;
  end;
end;

function GetParameterIndex(
        AProcType  : TPasProcedureType;
  const AParamName : string;
  const AStartPos  : Integer
) : Integer;
var
  pl : TList;
  i : Integer;
begin
  pl := AProcType.Args;
  if ( AStartPos >= 0 ) then
    i := AStartPos
  else
    i := 0;
  for i := i to Pred(pl.Count) do begin
    if AnsiSameText(AParamName,TPasArgument(pl[i]).Name) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function FindParameter(
        AProcType  : TPasProcedureType;
  const AParamName : string;
  const AStartPos  : Integer
) : TPasArgument;
var
  i : Integer;
begin
  i := GetParameterIndex(AProcType,AParamName,i);
  if ( i >= 0 ) then begin
    Result := TPasArgument(AProcType.Args[i]);
  end else begin
    Result := nil;
  end;
end;

function FindMember(AClass : TPasClassType; const AName : string) : TPasElement ;
var
  memberList : TList;
  i : Integer;
begin
  Result := nil;
  if ( AClass <> nil ) then begin
    memberList := AClass.Members;
    for i := 0 to Pred(memberList.Count) do begin
      if AnsiSameText(AName,TPasElement(memberList[i]).Name) then begin
        Result := TPasElement(memberList[i]);
      end;
    end;
  end;
end;

function MakeInternalSymbolNameFrom(const AName : string) : string ;
begin
  Result := ExtractIdentifier(AName);
  if IsStrEmpty(AName) then begin
    raise ESymbolException.CreateFmt('Unable to make an internal symbol Name from "%s".',[AName]);
  end;
  if IsReservedKeyWord(Result) then begin
    Result := '_' + Result;
  end;
end;


{ TwstPasTreeContainer }

function TwstPasTreeContainer.GetBinding(AIndex : Integer): TwstBinding;
begin
  Result := TwstBinding(FBindingList[AIndex]);
end;

function TwstPasTreeContainer.GetBindingCount: Integer;
begin
  Result := FBindingList.Count;
end;

constructor TwstPasTreeContainer.Create();
begin
  FPackage := TPasPackage.Create('sample',nil);
  FBindingList := TObjectList.Create(True);
  FProperties := TPropertyHolder.Create();
end;

destructor TwstPasTreeContainer.Destroy();
begin
  FreeAndNil(FProperties);
  FreeAndNil(FBindingList);
  FreeAndNil(FPackage);
  inherited Destroy();
end;

function TwstPasTreeContainer.CreateElement(
            AClass              : TPTreeElement;
      const AName               : String;
            AParent             : TPasElement;
            AVisibility         : TPasMemberVisibility;
      const ASourceFilename     : String;
            ASourceLinenumber   : Integer
) : TPasElement;
begin
  Result := AClass.Create(AName,AParent);
  RegisterExternalAlias(Result,AName);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  if Result.InheritsFrom(TPasModule) then begin
    FCurrentModule := Result as TPasModule;
    Package.Modules.Add(Result);
  end;
end;

function TwstPasTreeContainer.CreateArray(
  const AName              : string;
        AItemType          : TPasType;
  const AItemName,
        AItemExternalName  : string;
  const AStyle             : TArrayStyle
) : TPasArrayType;
var
  s : string;
begin
  Result := TPasArrayType(CreateElement(TPasArrayType,AName,CurrentModule.InterfaceSection,visDefault,'',0));
  Result.ElType := AItemType;
  AItemType.AddRef();
  Properties.SetValue(Result,sARRAY_ITEM_NAME,AItemName);
  Properties.SetValue(Result,sARRAY_ITEM_EXT_NAME,AItemExternalName);
  if ( AStyle = asEmbeded ) then
    s := sARRAY_STYLE_EMBEDDED
  else
    s := sARRAY_STYLE_SCOPED;
  Properties.SetValue(Result,sARRAY_STYLE,s);
end;

function TwstPasTreeContainer.GetArrayItemName(AArray: TPasArrayType): string;
begin
  Result := Properties.GetValue(AArray,sARRAY_ITEM_NAME);
end;

function TwstPasTreeContainer.GetArrayItemExternalName(AArray: TPasArrayType): string;
begin
  Result := Properties.GetValue(AArray,sARRAY_ITEM_EXT_NAME);
end;

function TwstPasTreeContainer.GetArrayStyle(AArray: TPasArrayType): TArrayStyle;
begin
  if AnsiSameText(sARRAY_STYLE_EMBEDDED,Properties.GetValue(AArray,sARRAY_STYLE)) then
    Result := asEmbeded
  else
    Result := asScoped;
end;

procedure TwstPasTreeContainer.SetArrayStyle(
        AArray : TPasArrayType;
  const AStyle : TArrayStyle
);
begin
  if ( AStyle = asEmbeded ) then
    Properties.SetValue(AArray,sARRAY_STYLE,sARRAY_STYLE_EMBEDDED)
  else
    Properties.SetValue(AArray,sARRAY_STYLE,sARRAY_STYLE_SCOPED);
end;

procedure TwstPasTreeContainer.SetArrayItemExternalName(
        AArray : TPasArrayType;
  const AExternalName : string
);
begin
  Properties.SetValue(AArray,sARRAY_ITEM_EXT_NAME,AExternalName);
end;

function TwstPasTreeContainer.FindElementInModule(const AName: String; AModule : TPasModule): TPasElement;
var
  decs : TList;
  i, c : Integer;
begin
  Result := nil;
  if Assigned(AModule) and Assigned(AModule.InterfaceSection.Declarations) then begin
    decs := AModule.InterfaceSection.Declarations;
    c := decs.Count;
    for i := 0 to Pred(c) do begin
      if SameName(TPasElement(decs[i]),AName) then begin
        Result := TPasElement(decs[i]);
        Exit;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.FindElement(const AName: String): TPasElement;
var
  i : Integer;
  mls : TList;
  mdl : TPasModule;
begin
  Result := FindElementInModule(AName,CurrentModule);
  if ( Result = nil ) then begin
    mls := Package.Modules;
    for i := 0 to Pred(mls.Count) do begin
      mdl := TPasModule(mls[i]);
      if ( CurrentModule <> mdl ) then begin
        Result := FindElementInModule(AName,mdl);
        if ( Result <> nil ) then begin
          Break;
        end;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.FindModule(const AName: String): TPasModule;
var
  i , c : Integer;
  mdl : TList;
begin
  Result := nil;
  mdl := Package.Modules;
  c := mdl.Count;
  for i := 0 to Pred(c) do begin
    if AnsiSameText(AName,TPasModule(mdl[i]).Name) then begin
      Result := TPasModule(mdl[i]);
    end;
  end;
end;

function TwstPasTreeContainer.IsEnumItemNameUsed(const AName: string): Boolean;
var
  i, c, j : Integer;
  elt : TPasElement;
  enumList : TList;
  typeList : TList;
begin
  Result := False;
  typeList := CurrentModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to Pred(c) do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasEnumType) then begin
      enumList := TPasEnumType(elt).Values;
      for j := 0 to Pred(enumList.Count) do begin
        if AnsiSameText(AName,TPasEnumValue(enumList[j]).Name) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.IsOfType(AType : TPasType; AClass : TClass) : Boolean;
var
  ut : TPasType;
begin
  Result := False;
  if Assigned(AType) then begin
    ut := AType;
    if ut.InheritsFrom(TPasUnresolvedTypeRef) then begin
      ut := FindElement(GetExternalName(ut)) as TPasType;
      if ( ut = nil ) then
        ut := AType;
    end;
    ut := GetUltimeType(ut);
    if ut.InheritsFrom(AClass) then begin
      Result := True;
    end;
  end;
end;

function TwstPasTreeContainer.IsInitNeed(AType : TPasType) : Boolean;
begin
  Result := IsOfType(AType,TPasClassType) or
            IsOfType(AType,TPasPointerType) or
            IsOfType(AType,TPasArrayType);
end;

procedure TwstPasTreeContainer.SetCurrentModule(AModule: TPasModule);
begin
  FCurrentModule := AModule;
end;

function TwstPasTreeContainer.AddBinding(const AName : string; AIntf : TPasClassType):TwstBinding;
begin
  Result := FindBinding(AName);
  if Assigned(Result) then begin
    raise Exception.CreateFmt('Duplicated binding : "%s"',[AName]);
  end;
  Result := TwstBinding.Create(AName, AIntf, AIntf.Parent);
  FBindingList.Add(Result);
end;

procedure TwstPasTreeContainer.DeleteBinding(ABinding: TwstBinding);
begin
  FBindingList.Extract(ABinding);
  ABinding.Release();
end;

function TwstPasTreeContainer.FindBinding(const AName: string): TwstBinding;
var
  i : Integer;
begin
  for i := 0 to Pred(BindingCount) do begin
    if AnsiSameText(AName,Binding[i].Name) then begin
      Result := Binding[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TwstPasTreeContainer.FindBinding(const AIntf: TPasClassType; const AOrder : Integer): TwstBinding;
var
  i, c, ordr : Integer;
begin
  ordr := AOrder;
  c := BindingCount;
  for i := 0 to Pred(c) do begin
    Result := Binding[i];
    if ( Result.Intf = AIntf ) then begin
      if ( ordr <= 0 ) then
        Exit
      else
        Dec(ordr);
    end;
  end;
  Result := nil;
end;

procedure TwstPasTreeContainer.RegisterExternalAlias(
        AObject : TPasElement;
  const AExternalName : String
);
begin
  Properties.SetValue(AObject,sEXTERNAL_NAME,AExternalName);
end;

function TwstPasTreeContainer.SameName(
        AObject : TPasElement;
  const AName   : string
): Boolean;
begin
  Result := AnsiSameText(AName,AObject.Name) or AnsiSameText(AName,GetExternalName(AObject)) ;
end;

function TwstPasTreeContainer.GetExternalName(AObject: TPasElement): string;
begin
  Result := Properties.GetValue(AObject,sEXTERNAL_NAME);
  if IsStrEmpty(Result) then begin
    Result := AObject.Name;
  end;
end;

function TwstPasTreeContainer.IsAttributeProperty(AObject: TPasProperty): Boolean;
begin
  Result := AnsiSameText(Properties.GetValue(AObject,sATTRIBUTE),'True');
end;

procedure TwstPasTreeContainer.SetPropertyAsAttribute(AObject: TPasProperty; const AValue: Boolean);
var
  s : string;
begin
  if AValue then
    s := 'True'
  else
    s := 'False';
  Properties.SetValue(AObject,sATTRIBUTE,s);
end;

{ TwstBinding }

constructor TwstBinding.Create(
  const AName : string;
        AIntf : TPasClassType;
        AParent: TPasElement
);
begin
  Assert((not IsStrEmpty(AName)) and Assigned(AIntf) and ( AIntf.ObjKind = okInterface ));
  inherited Create(AName,AParent);
  FIntf := AIntf;
  FIntf.AddRef();
end;

destructor TwstBinding.Destroy();
begin
  if Assigned(FIntf) then begin
    FIntf.Release();
    FIntf := nil;
  end;
  inherited Destroy();
end;

{ TPropertyHolder }

function TPropertyHolder.FindList(AOwner: TObject): TStrings;
var
  i : Integer;
begin
  i := FObjects.IndexOf(AOwner);
  if ( i >= 0 ) then begin
    Result := FProps[i] as TStrings;
  end else begin
    Result := nil ;
  end;
end;

function TPropertyHolder.GetList(AOwner: TObject): TStrings;
begin
  Result := FindList(AOwner);
  if ( Result = nil ) then begin
    FObjects.Add(AOwner);
    Result := TStringList.Create();
    FProps.Add(Result);
  end;
end;

constructor TPropertyHolder.Create();
begin
  FObjects := TObjectList.Create(False);
  FProps := TObjectList.Create(True);
end;

destructor TPropertyHolder.Destroy();
begin
  FreeAndNil(FProps);
  FreeAndNil(FObjects);
  inherited Destroy();
end;

procedure TPropertyHolder.SetValue(AOwner: TObject; const AName, AValue: string);
begin
  GetList(AOwner).Values[AName] := AValue;
end;

function TPropertyHolder.GetValue(AOwner: TObject; const AName: string): string;
var
  ls : TStrings;
begin
  ls := FindList(AOwner);
  if ( ls = nil ) then begin
    Result := '';
  end else begin
    Result := ls.Values[AName];
  end;
end;

{ TPasNativeSimpleTypeDefinition }

destructor TPasNativeSimpleType.Destroy();
begin
  if Assigned(FBoxedType) then begin
    FBoxedType.Release();
    FBoxedType := nil
  end;
  inherited Destroy();
end;

procedure TPasNativeSimpleType.SetBoxedType(
  ABoxedType : TPasNativeSimpleContentClassType
);
begin
  if ( FBoxedType <> ABoxedType ) then begin
    if ( FBoxedType <> nil ) then begin
      FBoxedType.Release();
    end;
    FBoxedType := ABoxedType;
    if ( FBoxedType <> nil ) then
      FBoxedType.AddRef();
  end;
end;

end.

