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
unit base_soap_formatter;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  sPROTOCOL_NAME = 'SOAP';

  sXML_NS = 'xmlns';
  sXSI_NS = 'http://www.w3.org/1999/XMLSchema-instance';
  sTYPE = 'type';
  sNIL = 'nil';

  sSOAP_ENC = 'http://schemas.xmlsoap.org/soap/encoding/';
  sSOAP_ENC_ABR = 'SOAP-ENC';

  sARRAY_TYPE = 'arrayType';

  sCONTENT_TYPE = 'contenttype';
  sFORMAT = 'format';
  sSOAP_CONTENT_TYPE = 'text/xml';

  sHEADER   = 'Header';
  sENVELOPE = 'Envelope';

type

  TwstXMLDocument = {$IFNDEF FPC}wst_delphi_xml.TXMLDocument{$ELSE}TXMLDocument{$ENDIF};

  TEnumIntType = Int64;

  { ESOAPException }

  ESOAPException = class(EBaseRemoteException)
  End;
  
  { TStackItem }

  TStackItem = class
  private
    FEmbeddedScopeCount: Integer;
    FNameSpace: string;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
  protected
    function GetItemsCount() : Integer;virtual;
  Public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    procedure SetNameSpace(const ANameSpace : string);
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property NameSpace : string Read FNameSpace;
    property ItemsCount : Integer read GetItemsCount;

    property EmbeddedScopeCount : Integer read FEmbeddedScopeCount;
    function BeginEmbeddedScope() : Integer;
    function EndEmbeddedScope() : Integer;
  End;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    function FindNode(var ANodeName : string):TDOMNode;override;
  End;

  { TAbstractArrayStackItem }

  TAbstractArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
    FItemName : string;
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList(const ANodeName : string):TDOMNodeList;virtual;abstract;
  public
    constructor Create(
            AScopeObject : TDOMNode;
      const AScopeType   : TScopeType;
      const AItemName    : string
    );
    destructor Destroy();override;
    function FindNode(var ANodeName : string):TDOMNode;override;
  end;

  { TScopedArrayStackItem }

  TScopedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  end;

  { TEmbeddedArrayStackItem }

  TEmbeddedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  end;

  TSOAPEncodingStyle = ( Literal, Encoded );
  TSOAPDocumentStyle = ( RPC, Document );
  
{$M+}

  { TSOAPBaseFormatter }

  TSOAPBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FContentType: string;
    FEncodingStyle: TSOAPEncodingStyle;
    FStyle: TSOAPDocumentStyle;
    FHeaderEnterCount : Integer;
    
    FNameSpaceCounter : Integer;
    FDoc : TwstXMLDocument;
    FStack : TObjectStack;

    FKeepedStyle : TSOAPDocumentStyle;
    FKeepedEncoding : TSOAPEncodingStyle;
    FSerializationStyle : TSerializationStyle;

    procedure InternalClear(const ACreateDoc : Boolean);
    
    function NextNameSpaceCounter():Integer;//inline;
    function HasScope():Boolean;//inline;

    procedure CheckScope();//inline;
    function InternalPutData(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : string
    ):TDOMNode;
    function PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    ):TDOMNode;
    function PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    ):TDOMNode;
    function PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    ):TDOMNode;
    function PutStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    ):TDOMNode;
    function PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Extended
    ):TDOMNode;
    procedure PutObj(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TObject
    );

    function GetNodeValue(var AName : String):DOMString;
    procedure GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumIntType
    );
    procedure GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    );
    {$IFDEF FPC}
    procedure GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Integer
    );
    {$ENDIF}
    procedure GetInt64(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Int64
    );
    procedure GetFloat(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Extended
    );
    procedure GetStr(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : String
    );
    procedure GetObj(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TObject
    );
  protected
    function GetXmlDoc():TwstXMLDocument;
    function PushStack(AScopeObject : TDOMNode):TStackItem;overload;
    function PushStack(
            AScopeObject : TDOMNode;
      const AStyle       : TArrayStyle;
      const AItemName    : string
    ):TStackItem;overload;
    function FindAttributeByValueInNode(
      Const AAttValue : String;
      Const ANode     : TDOMNode;
      Out   AResAtt   : string
    ):boolean;
    function FindAttributeByNameInNode(
      Const AAttName     : String;
      Const ANode        : TDOMNode;
      Out   AResAttValue : string
    ):boolean;
    function FindAttributeByValueInScope(Const AAttValue : String):String;
    function FindAttributeByNameInScope(Const AAttName : String):String;
    function GetNameSpaceShortName(
      const ANameSpace        : string;
      const ACreateIfNotFound : Boolean
    ):shortstring;
  protected
    function GetCurrentScope():String;
    function GetCurrentScopeObject():TDOMElement;
    function StackTop():TStackItem;
    function PopStack():TStackItem;
    procedure ClearStack();
    procedure BeginScope(
      Const AScopeName,ANameSpace : string;
      Const ANameSpaceShortName   : string ;
      Const AScopeType            : TScopeType;
      const AStyle                : TArrayStyle
    );
    function InternalBeginScopeRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AScopeType : TScopeType;
      const AStyle     : TArrayStyle;
      const AItemName  : string
    ):Integer;

    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetStyleAndEncoding(
      const AStyle : TSOAPDocumentStyle;
      const AEncoding : TSOAPEncodingStyle
    );
    procedure RestoreStyleAndEncoding();
    procedure Prepare();
    function ReadHeaders(ACallContext : ICallContext):Integer;
    function WriteHeaders(ACallContext : ICallContext):Integer;
  public
    constructor Create();override;
    destructor Destroy();override;
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

    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  Published
    property EncodingStyle : TSOAPEncodingStyle Read FEncodingStyle Write FEncodingStyle;
    property ContentType : string Read FContentType Write FContentType;
    property Style : TSOAPDocumentStyle Read FStyle Write FStyle;
  End;
{$M-}


implementation
Uses {$IFNDEF FPC}XMLDoc,XMLIntf,{$ELSE}XMLWrite, XMLRead,wst_fpc_xml,{$ENDIF}
     StrUtils, imp_utils;

{ TStackItem }

function TStackItem.GetItemsCount: Integer;
begin
  Result := GetNodeItemsCount(ScopeObject);
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

procedure TStackItem.SetNameSpace(const ANameSpace: string);
begin
  FNameSpace := ANameSpace;
end;

function TStackItem.BeginEmbeddedScope(): Integer;
begin
  Inc(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;

function TStackItem.EndEmbeddedScope(): Integer;
begin
  if ( FEmbeddedScopeCount < 1 ) then begin
    raise Exception.Create('Invalid opération on scope, their are no embedded scope.');
  end;
  Dec(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
{$IFNDEF FPC}
  Result := wst_delphi_xml.FindNode(ScopeObject,ANodeName);
{$ELSE}
  Result := ScopeObject.FindNode(ANodeName);
{$ENDIF}
end;

{ TAbstractArrayStackItem }

procedure TAbstractArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList(FItemName);
  end;
end;

function TAbstractArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

constructor TAbstractArrayStackItem.Create(
        AScopeObject : TDOMNode;
  const AScopeType   : TScopeType;
  const AItemName    : string
);
begin
  inherited Create(AScopeObject,AScopeType);
  FItemName := AItemName;
end;

destructor TAbstractArrayStackItem.Destroy();
begin
  if Assigned(FItemList) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TAbstractArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise ESOAPException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  Inc(FIndex);
  ANodeName := Result.NodeName;
end;

{ TSOAPBaseFormatter }

procedure TSOAPBaseFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

function TSOAPBaseFormatter.PushStack(AScopeObject : TDOMNode) : TStackItem;
begin
  Result := FStack.Push(TObjectStackItem.Create(AScopeObject,stObject)) as TStackItem;
end;

function TSOAPBaseFormatter.PushStack(
        AScopeObject : TDOMNode;
  const AStyle       : TArrayStyle;
  const AItemName    : string
): TStackItem;
begin
  case AStyle of
    asScoped  : Result := FStack.Push(TScopedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    asEmbeded : Result := FStack.Push(TEmbeddedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    else
      Assert(False);
  end;
end;

function TSOAPBaseFormatter.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stObject,asNone,'');
end;

function TSOAPBaseFormatter.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stArray,AStyle,AItemName);
end;

procedure TSOAPBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TSOAPBaseFormatter.BeginHeader();
begin
  if ( FHeaderEnterCount <= 0 ) then begin
    Inc(FHeaderEnterCount);
    Prepare();
    BeginScope(sHEADER,sSOAP_ENV,sSOAP_ENV_ABR,stObject,asNone);
    SetStyleAndEncoding(Document,Literal);
  end;
end;

procedure TSOAPBaseFormatter.EndHeader();
begin
  if ( FHeaderEnterCount > 0 ) then begin
    Dec(FHeaderEnterCount);
    RestoreStyleAndEncoding();
    EndScope();
  end;
end;

procedure TSOAPBaseFormatter.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  ReleaseDomNode(FDoc);
  if ACreateDoc then
    FDoc := CreateDoc();
end;

function TSOAPBaseFormatter.NextNameSpaceCounter(): Integer;
begin
  Inc(FNameSpaceCounter);
  Result := FNameSpaceCounter;
end;

function TSOAPBaseFormatter.HasScope(): Boolean;
begin
  Result := FStack.AtLeast(1);
end;

function TSOAPBaseFormatter.FindAttributeByValueInNode(
  Const AAttValue : String;
  Const ANode     : TDOMNode;
  Out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  if Assigned(ANode) and
     Assigned(ANode.Attributes) and
     ( ANode.Attributes.Length > 0 )
  then begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  end;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByNameInNode(
  const AAttName: String;
  const ANode: TDOMNode;
  Out AResAttValue: string
): boolean;
var
  i,c : Integer;
begin
  AResAttValue := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttName,ANode.Attributes.Item[i].NodeName) Then Begin
        AResAttValue := ANode.Attributes.Item[i].NodeValue;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByValueInScope(const AAttValue: String): String;
Var
  tmpNode : TDOMNode;
begin
  If HasScope() Then Begin
    tmpNode := GetCurrentScopeObject();
    While Assigned(tmpNode) Do Begin
      If FindAttributeByValueInNode(AAttValue,tmpNode,Result) Then
        Exit;
      tmpNode := tmpNode.ParentNode;
    End;
  End;
  Result := '';
end;

function TSOAPBaseFormatter.FindAttributeByNameInScope(const AAttName: String): String;
var
  tmpNode : TDOMNode;
begin
  if HasScope() then begin
    tmpNode := GetCurrentScopeObject();
    while Assigned(tmpNode) do begin
      if FindAttributeByNameInNode(AAttName,tmpNode,Result) then
        Exit;
      tmpNode := tmpNode.ParentNode;
    end;
  end;
  Result := '';
end;

function TSOAPBaseFormatter.GetNameSpaceShortName(
  const ANameSpace        : string;
  const ACreateIfNotFound : Boolean
): shortstring;
begin
  Result := FindAttributeByValueInScope(ANameSpace);
  if IsStrEmpty(Result) then begin
    if ACreateIfNotFound then begin
      Result := 'ns' + IntToStr(NextNameSpaceCounter());
      AddScopeAttribute('xmlns:'+Result, ANameSpace);
    end;
  end else begin
    Result := Copy(Result,Length('xmlns:')+1,MaxInt);
  end;
end;

procedure TSOAPBaseFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error('There is no scope.');
end;

function ExtractNameSpaceShortName(const ANameSpaceDeclaration : string):string;
var
  i : integer;
begin
  i := AnsiPos(sXML_NS,ANameSpaceDeclaration);
  if ( i > 0 ) then begin
    Result := Copy(ANameSpaceDeclaration, (i + Length(sXML_NS) + 1 ), MaxInt );
  end else begin
    Result := '';
  end;
end;

function TSOAPBaseFormatter.InternalPutData(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : string
): TDOMNode;
Var
  namespaceLongName, namespaceShortName, strName, strNodeName, s : string;
  regItem : TTypeRegistryItem;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := Copy(FindAttributeByValueInScope(StackTop().NameSpace),AnsiPos(':',namespaceShortName) + 1,MaxInt);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    Result := FDoc.CreateElement(strNodeName);
    Result.AppendChild(FDoc.CreateTextNode(AData));
    GetCurrentScopeObject().AppendChild(Result);
    If ( EncodingStyle = Encoded ) Then Begin
      regItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
      strName := regItem.DeclaredName;
      namespaceLongName := regItem.NameSpace;
      If Not IsStrEmpty(namespaceLongName) Then Begin
        namespaceShortName := FindAttributeByValueInScope(namespaceLongName);
        If IsStrEmpty(namespaceShortName) Then Begin
          namespaceShortName := Format('ns%d',[NextNameSpaceCounter()]);
          AddScopeAttribute(sXML_NS + ':'+namespaceShortName,namespaceLongName);
        End Else Begin
          namespaceShortName := ExtractNameSpaceShortName(namespaceShortName);//Copy(namespaceShortName,AnsiPos(':',namespaceShortName) + 1,MaxInt);
        End;
        strName := Format('%s:%s',[namespaceShortName,strName])
      End;
      namespaceShortName := GetNameSpaceShortName(sXSI_NS,True);
      if not IsStrEmpty(namespaceShortName) then
        namespaceShortName := namespaceShortName + ':';
      (Result As TDOMElement).SetAttribute(namespaceShortName + sTYPE,strName);
    End;
  end else begin
    Result := GetCurrentScopeObject();
    (Result as TDOMElement).SetAttribute(strNodeName,AData);
  end;
end;

function TSOAPBaseFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumIntType
): TDOMNode;
begin
  Result := InternalPutData(
              AName,
              ATypeInfo,
              GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,AData))
            );
end;

function TSOAPBaseFormatter.PutBool(
  const AName     : String;
  const ATypeInfo : PTypeInfo;
  const AData     : Boolean
): TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,LowerCase(BoolToStr(AData)));
end;

function TSOAPBaseFormatter.PutInt64(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Int64
): TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,IntToStr(AData));
end;

function TSOAPBaseFormatter.PutStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: String
):TDOMNode;
begin
  Result := InternalPutData(AName,ATypeInfo,AData);
end;

procedure TSOAPBaseFormatter.PutObj(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

function TSOAPBaseFormatter.PutFloat(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
):TDOMNode;
Var
  s, frmt : string;
  prcsn : Integer;
{$IFDEF FPC} {$IFNDEF FPC_211} i : Integer; {$ENDIF}{$ENDIF}
begin
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle,
    ftCurr,
    ftComp      : prcsn := 7;
    ftDouble,
    ftExtended  : prcsn := 15;
  End;
  frmt := '#.' + StringOfChar('#',prcsn) + 'E-0';
{$IFDEF FPC}
  {$IFDEF FPC_211}
  s := FormatFloat(frmt,AData,wst_FormatSettings);
  {$ELSE}
  s := FormatFloat(frmt,AData);
  i := Pos(',',s);
  If ( i > 0 ) Then
    s[i] := '.';
  {$ENDIF}
{$ELSE}
  s := FormatFloat(frmt,AData,wst_FormatSettings);
{$ENDIF}
  Result := InternalPutData(AName,ATypeInfo,s);
end;

function TSOAPBaseFormatter.GetNodeValue(var AName: String): DOMString;
Var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s : string;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := Copy(FindAttributeByValueInScope(StackTop().NameSpace),AnsiPos(':',namespaceShortName) + 1,MaxInt);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName) As TDOMElement;
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;

  if Assigned(locElt) then begin
    if locElt.HasChildNodes then
      Result := locElt.FirstChild.NodeValue
    else
      Result := locElt.NodeValue;
  end else begin
    Error('Param or Attribute not found : "%s"',[AName]);
  end;
end;

procedure TSOAPBaseFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TEnumIntType
);
Var
  locBuffer : String;
begin
  locBuffer := Trim(GetNodeValue(AName));
  If IsStrEmpty(locBuffer) Then
    AData := 0
  Else
    AData := GetEnumValue(ATypeInfo,locBuffer)
End;

procedure TSOAPBaseFormatter.GetBool(
  const ATypeInfo  : PTypeInfo;
  var   AName      : String;
  var   AData      : Boolean
);
Var
  locBuffer : String;
begin
  locBuffer := LowerCase(Trim(GetNodeValue(AName)));
  If IsStrEmpty(locBuffer) Then
    AData := False
  Else
    AData := StrToBool(locBuffer);
end;

{$IFDEF FPC}
procedure TSOAPBaseFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: Integer
);
begin
  AData := StrToIntDef(Trim(GetNodeValue(AName)),0);
end;
{$ENDIF}

procedure TSOAPBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Int64
);
begin
  AData := StrToInt64Def(Trim(GetNodeValue(AName)),0);
end;

procedure TSOAPBaseFormatter.GetFloat(
  const ATypeInfo  : PTypeInfo;
  var AName        : String;
  var AData        : Extended
);
begin
{$IFDEF FPC}
  {$IFDEF FPC_211}
    AData := StrToFloatDef(Trim(GetNodeValue(AName)),0,wst_FormatSettings);
  {$ELSE}
  AData := StrToFloatDef(Trim(GetNodeValue(AName)),0);
  {$ENDIF}
{$ELSE}
    AData := StrToFloatDef(Trim(GetNodeValue(AName)),0,wst_FormatSettings);
{$ENDIF}
  //AData := StrToFloatDef(Trim(GetNodeValue(AName)),0,wst_FormatSettings);
end;

procedure TSOAPBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : String
);
begin
  AData := GetNodeValue(AName);
end;

procedure TSOAPBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
end;

function TSOAPBaseFormatter.GetXmlDoc(): TwstXMLDocument;
begin
  Result := FDoc;
end;

function TSOAPBaseFormatter.GetCurrentScope(): String;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TSOAPBaseFormatter.GetCurrentScopeObject(): TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TSOAPBaseFormatter.StackTop(): TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TSOAPBaseFormatter.PopStack(): TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

constructor TSOAPBaseFormatter.Create();
begin
  Inherited Create();
  FContentType := sSOAP_CONTENT_TYPE;
  FStack := TObjectStack.Create();
  FDoc := CreateDoc();
end;

destructor TSOAPBaseFormatter.Destroy();
begin
  ReleaseDomNode(FDoc);
  ClearStack();
  FStack.Free();
  inherited Destroy();
end;

procedure TSOAPBaseFormatter.Clear();
begin
  InternalClear(True);
end;

procedure TSOAPBaseFormatter.BeginObject(
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH, xsiNmspcSH : string;
  mustAddAtt : Boolean;
  strNodeName : string;
begin
  typData := GetTypeRegistry().Find(ATypeInfo,False);
  If Not Assigned(typData) Then
    Error('Object type not registered : %s',[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
  mustAddAtt := False;
  nmspc := typData.NameSpace;
  If IsStrEmpty(nmspc) Then
    nmspcSH := 'tns'
  Else Begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    If IsStrEmpty(nmspcSH) Then Begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      If HasScope() Then
        AddScopeAttribute('xmlns:'+nmspcSH, nmspc)
      Else Begin
        mustAddAtt := True;
      End;
    End Else Begin
      nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
    End;
  End;

  if ( Style = Document ) then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  BeginScope(strNodeName,'','',stObject,asNone);
  If mustAddAtt Then
    AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
  if ( EncodingStyle = Encoded ) then begin
    xsiNmspcSH := GetNameSpaceShortName(sXSI_NS,True);
    if not IsStrEmpty(xsiNmspcSH) then
      xsiNmspcSH := xsiNmspcSH + ':';
    AddScopeAttribute(xsiNmspcSH + sTYPE,Format('%s:%s',[nmspcSH,typData.DeclaredName]));
  end;
  StackTop().SetNameSpace(nmspc);
end;

procedure TSOAPBaseFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  i,j, k : Integer;
  strNodeName : string;
  xsiNmspcSH : string;
begin
  if ( Length(ABounds) < 2 ) then begin
    Error('Invalid array bounds.');
  end;
  i := ABounds[0];
  j := ABounds[1];
  k := j - i + 1;
  if ( k < 0 ) then begin
    Error('Invalid array bounds.');
  end;
  typData := GetTypeRegistry().Find(ATypeInfo,False);
  if not Assigned(typData) then begin
    Error('Array type not registered.');
  end;
  nmspc := typData.NameSpace;
  if IsStrEmpty(nmspc) then begin
    nmspcSH := 'tns'
  end else begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    if IsStrEmpty(nmspcSH) then begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
    end else begin
      nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
    end;
  end;

  if ( Style = Document ) then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  //if ( AStyle = asScoped ) then begin
    BeginScope(strNodeName,'','',stArray,AStyle);
  //end;

  if ( EncodingStyle = Encoded ) then begin
    //AddScopeAttribute(sXSI_TYPE,nmspc);
    //SOAP-ENC:arrayType="xsd:int[2]"
    {AddScopeAttribute(
      Format('%s:%s',[sSOAP_ENC_ABR,sARRAY_TYPE]) ,
      Format('%s:%s[%d]',[nmspcSH,typData.DeclaredName,k])
    );}
    xsiNmspcSH := GetNameSpaceShortName(sXSI_NS,True);
    if not IsStrEmpty(xsiNmspcSH) then
      xsiNmspcSH := xsiNmspcSH + ':';
    AddScopeAttribute(xsiNmspcSH + sTYPE,Format('%s:%s',[nmspcSH,typData.DeclaredName]));
  end;
  StackTop().SetNameSpace(nmspc);
end;

procedure TSOAPBaseFormatter.NilCurrentScope();
var
  nmspcSH : shortstring;
begin
  CheckScope();
  nmspcSH := FindAttributeByValueInScope(sXSI_NS);
  if IsStrEmpty(nmspcSH) then begin
    nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
    AddScopeAttribute('xmlns:'+nmspcSH, sXSI_NS);
  end else begin
    nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
  end;
  GetCurrentScopeObject().SetAttribute(nmspcSH + ':' + sNIL,'true');
end;

function TSOAPBaseFormatter.IsCurrentScopeNil(): Boolean;
Var
  s,nsShortName,nilName : shortstring;
begin
  CheckScope();
  nsShortName := FindAttributeByValueInScope(sXSI_NS);
  Result := False;
  if IsStrEmpty(nsShortName) then begin
    nilName := 'nil';
  end else begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    if not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
    nilName := nsShortName + 'nil';
  end;
  s := Trim(GetCurrentScopeObject().GetAttribute(nilName));
  if ( Length(s) > 0 ) and ( AnsiSameText(s,'true') or AnsiSameText(s,'"true"') ) then begin
    Result := True;
  end;
end;

procedure TSOAPBaseFormatter.BeginScope(
  Const AScopeName,ANameSpace : string;
  Const ANameSpaceShortName   : string;
  Const AScopeType            : TScopeType;
  const AStyle                : TArrayStyle
);
Var
  nsStr, scpStr : String;
  e : TDOMElement;
  hasNmspc, addAtt : Boolean;
begin
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    scpStr := AScopeName;
    hasNmspc := Not IsStrEmpty(ANameSpace);
    If hasNmspc Then Begin
      nsStr := FindAttributeByValueInScope(ANameSpace);
      addAtt := IsStrEmpty(nsStr);
      If addAtt Then Begin
        If IsStrEmpty(ANameSpaceShortName) Then
          nsStr := 'ns' + IntToStr(NextNameSpaceCounter())
        Else
          nsStr := Trim(ANameSpaceShortName);
      End Else Begin
        nsStr := Copy(nsStr,Succ(AnsiPos(':',nsStr)),MaxInt);
      End;
      scpStr := nsStr + ':' + scpStr;
    End;

    e := FDoc.CreateElement(scpStr);
    If HasScope() Then
      GetCurrentScopeObject().AppendChild(e)
    Else
      FDoc.AppendChild(e);
    if ( AScopeType = stObject ) then begin
      PushStack(e);
    end else begin
      PushStack(e,AStyle,'');
    end;
    if hasNmspc and addAtt then begin
      e.SetAttribute('xmlns:'+nsStr,ANameSpace);
      StackTop().SetNameSpace(ANameSpace);
    end;
  end else if ( ( AScopeType = stArray ) and ( AStyle = asEmbeded ) ) then begin
    StackTop().BeginEmbeddedScope();
  end;
end;

function TSOAPBaseFormatter.InternalBeginScopeRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AScopeType : TScopeType;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  strNodeName : string;
begin
  if ( Style = Document ) then begin
    typData := GetTypeRegistry().Find(ATypeInfo,False);
    if not Assigned(typData) then begin
      Error('Object type not registered : %s',[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
    end;
    nmspc := typData.NameSpace;
    if IsStrEmpty(nmspc) then begin
      nmspcSH := ''
    end else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if not IsStrEmpty(nmspcSH) then begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    end;
    if IsStrEmpty(nmspcSH) then begin
      strNodeName := AScopeName
    end else begin
      if ( Pos(':',AScopeName) < 1 ) then begin
        strNodeName := nmspcSH + ':' + AScopeName
      end else begin
        strNodeName := AScopeName;
      end;
    end;
  end else begin
    nmspcSH := '';
    strNodeName := AScopeName;
  end;

  stk := StackTop();
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    locNode := stk.FindNode(strNodeName);
  end else begin
    locNode := stk.ScopeObject;
  end;
  
  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    if ( AScopeType = stObject ) then begin
      PushStack(locNode);
    end else begin
      PushStack(locNode,AStyle,AItemName);
    end;
    if ( Style = Document ) then begin
      StackTop().SetNameSpace(nmspc);
    end;
    Result := StackTop().GetItemsCount();
  end;
end;

procedure TSOAPBaseFormatter.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TSOAPBaseFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

procedure TSOAPBaseFormatter.SetStyleAndEncoding(
  const AStyle: TSOAPDocumentStyle;
  const AEncoding: TSOAPEncodingStyle
);
begin
  FKeepedStyle := Style;
  FKeepedEncoding := EncodingStyle;
  Style := AStyle;
  EncodingStyle := AEncoding;
end;

procedure TSOAPBaseFormatter.RestoreStyleAndEncoding();
begin
  EncodingStyle := FKeepedEncoding;
  Style := FKeepedStyle;
end;

procedure TSOAPBaseFormatter.Prepare();
var
  locDoc : TwstXMLDocument;
begin
  locDoc := GetXmlDoc();
  if Assigned(locDoc.DocumentElement) and
     AnsiSameText(locDoc.DocumentElement.NodeName,( sSOAP_ENV_ABR + ':' + sENVELOPE ))
  then begin
    ClearStack();
    PushStack(locDoc.DocumentElement);
  end else begin
    BeginScope(sENVELOPE,sSOAP_ENV,sSOAP_ENV_ABR,stObject,asNone);
      AddScopeAttribute('xmlns:xsi',sXSI_NS);
      AddScopeAttribute('xmlns:'+sXSD, sXSD_NS);
      AddScopeAttribute('xmlns:'+sSOAP_ENC_ABR, sSOAP_ENC);
  end;
end;

function TSOAPBaseFormatter.ReadHeaders(ACallContext: ICallContext): Integer;

  function ExtractTypeInfo(ANode : TDOMElement) : TTypeRegistryItem;
  var
    j : Integer;
    ndName, nsSN, nsLN, s : string;
  begin
    ndName := ANode.NodeName;
    j := Pos(':',ndName);
    if ( j > 0 ) then
      nsSN := Copy(ndName,1,Pred(j))
    else
      nsSN := '';
    if IsStrEmpty(nsSN) then
      s := sXML_NS
    else
      s := sXML_NS + ':' + nsSN;
    nsLN := FindAttributeByNameInScope(s);
    Result := GetTypeRegistry().FindByDeclaredName(Copy(ndName,Succ(j),MaxInt),nsLN);
  end;

var
  i : Integer;
  nd : TDOMElement;
  typItm : TTypeRegistryItem;
  tmpObj : THeaderBlock;
  locName : string;
  chdLst : TDOMNodeList;
begin
  SetStyleAndEncoding(Document,Literal);
  try
    Result := StackTop().ItemsCount;
    if ( Result > 0 ) then begin
      chdLst := StackTop().ScopeObject.ChildNodes;
      try
        for i := 0 to Pred(Result) do begin
          nd := chdLst.Item[i] as TDOMElement;
          typItm := ExtractTypeInfo(nd);
          if Assigned(typItm) then begin
            if ( typItm.DataType^.Kind = tkClass ) then begin
              tmpObj := nil;
              locName := nd.NodeName;
              Get(typItm.DataType,locName,tmpObj);
              if Assigned(tmpObj) then begin
                tmpObj.Direction := hdIn;
                ACallContext.AddHeader(tmpObj,True);
              end;
            end;
          end;
        end;
      finally
        ReleaseDomNode(chdLst);
      end;
    end;
  finally
    RestoreStyleAndEncoding();
  end;
end;

function TSOAPBaseFormatter.WriteHeaders(ACallContext : ICallContext): Integer;
var
  ptyp : PTypeInfo;
  h : THeaderBlock;
  i, c : Integer;
begin
  Result := ACallContext.GetHeaderCount([hdOut]);
  if ( Result > 0 ) then begin
    BeginHeader();
    try
      c := ACallContext.GetHeaderCount(AllHeaderDirection);
      for i := 0 to Pred(c) do begin
        h := ACallContext.GetHeader(i);
        if ( h.Direction = hdOut ) then begin
          ptyp := PTypeInfo(h.ClassInfo);
          Put(GetTypeRegistry().ItemByTypeInfo[ptyp].DeclaredName,ptyp,h);
        end;
      end;
    finally
      EndHeader();
    end;
  end;
end;

procedure TSOAPBaseFormatter.EndScope();
begin
  CheckScope();
  if ( StackTop().EmbeddedScopeCount = 0 ) then begin
    FStack.Pop().Free();
  end else begin
    StackTop().EndEmbeddedScope();
  end;
end;

procedure TSOAPBaseFormatter.AddScopeAttribute(const AName, AValue: string);
begin
  CheckScope();
  GetCurrentScopeObject().SetAttribute(AName,AValue);
end;

procedure TSOAPBaseFormatter.Put(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
begin
  Case ATypeInfo^.Kind Of
    tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
      Begin
        int64Data := Int64(AData);
        PutInt64(AName,ATypeInfo,int64Data);
      End;
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := String(AData);
        PutStr(AName,ATypeInfo,strData);
      End;
    tkClass :
      Begin
        objData := TObject(AData);
        PutObj(AName,ATypeInfo,objData);
      End;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := Boolean(AData);
        PutBool(AName,ATypeInfo,boolData);
      End;
    {$ENDIF}  
    tkInteger, tkEnumeration :
      begin
      {$IFNDEF FPC}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := Boolean(AData);
          PutBool(AName,ATypeInfo,boolData);
        end else begin
      {$ENDIF}
          enumData := 0;
          Case GetTypeData(ATypeInfo)^.OrdType Of
            otSByte : enumData := ShortInt(AData);
            otUByte : enumData := Byte(AData);
            otSWord : enumData := SmallInt(AData);
            otUWord : enumData := Word(AData);
            otSLong,
            otULong : enumData := LongInt(AData);
          End;
          If ( ATypeInfo^.Kind = tkInteger ) Then
            PutInt64(AName,ATypeInfo,enumData)
          Else
            PutEnum(AName,ATypeInfo,enumData);
      {$IFNDEF FPC}
        end;
      {$ENDIF}
      end;
    tkFloat :
      Begin
        floatDt := 0;
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : floatDt := Single(AData);
          ftDouble : floatDt := Double(AData);
          ftExtended : floatDt := Extended(AData);
          ftCurr : floatDt := Currency(AData);
          ftComp : floatDt := Comp(AData);
        End;
        PutFloat(AName,ATypeInfo,floatDt);
      End;
  End;
end;

procedure TSOAPBaseFormatter.PutScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  const AData
);
Var
  int64SData : Int64;
  {$IFDEF FPC}
    int64UData : QWord;
    boolData : Boolean;
  {$ENDIF}
  strData : string;
  enumData : TEnumIntType;
  floatDt : Extended;
  dataBuffer : string;
  frmt : string;
  prcsn,i : Integer;
begin
  CheckScope();
  Case ATypeInfo^.Kind Of
    tkInt64 :
      begin
        int64SData := Int64(AData);
        dataBuffer := IntToStr(int64SData);
      end;
    {$IFDEF FPC}
    tkQWord :
      begin
        int64UData := QWord(AData);
        dataBuffer := IntToStr(int64UData);
      end;
    {$ENDIF}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      begin
        strData := string(AData);
        dataBuffer := strData;
      end;
    tkClass :
      begin
        raise ESOAPException.Create('Inner Scope value must be a "simple type" value.');
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        boolData := Boolean(AData);
        dataBuffer := BoolToStr(boolData);
      end;
    {$ENDIF}
    tkInteger :
      begin
        case GetTypeData(ATypeInfo)^.OrdType of
          otSByte : enumData := ShortInt(AData);
          otUByte : enumData := Byte(AData);
          otSWord : enumData := SmallInt(AData);
          otUWord : enumData := Word(AData);
          otSLong,
          otULong : enumData := LongInt(AData);
        end;
        dataBuffer := IntToStr(enumData);
      end;
    tkEnumeration :
      begin
        enumData := 0;
        case GetTypeData(ATypeInfo)^.OrdType of
          otSByte : enumData := ShortInt(AData);
          otUByte : enumData := Byte(AData);
          otSWord : enumData := SmallInt(AData);
          otUWord : enumData := Word(AData);
          otSLong,
          otULong : enumData := LongInt(AData);
        end;
        dataBuffer := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,enumData))
      end;
    tkFloat :
      begin
        floatDt := 0;
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle :
            begin
              floatDt := Single(AData);
              prcsn := 7;
            end;
          ftDouble :
            begin
              floatDt := Double(AData);
              prcsn := 15;
            end;
          ftExtended :
            begin
              floatDt := Extended(AData);
              prcsn := 15;
            end;
          ftCurr :
            begin
              floatDt := Currency(AData);
              prcsn := 7;
            end;
          ftComp :
            begin
              floatDt := Comp(AData);
              prcsn := 7;
            end;
        end;
        frmt := '#.' + StringOfChar('#',prcsn) + 'E-0';
        dataBuffer := FormatFloat(frmt,floatDt);
        i := Pos(',',dataBuffer);
        if ( i > 0 ) then
          dataBuffer[i] := '.';
      end;
  end;
  StackTop().ScopeObject.AppendChild(FDoc.CreateTextNode(dataBuffer));
end;

procedure TSOAPBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
begin
  Case ATypeInfo^.Kind Of
    tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
      Begin
        int64Data := 0;
        GetInt64(ATypeInfo,AName,int64Data);
        Int64(AData) := int64Data;
      End;
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := '';
        GetStr(ATypeInfo,AName,strData);
        String(AData) := strData;
      End;
    tkClass :
      Begin
        objData := TObject(AData);
        GetObj(ATypeInfo,AName,objData);
        TObject(AData) := objData;
      End;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := False;
        GetBool(ATypeInfo,AName,boolData);
        Boolean(AData) := boolData;
      End;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFNDEF FPC}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := False;
          GetBool(ATypeInfo,AName,boolData);
          Boolean(AData) := boolData;
        end else begin
      {$ENDIF}
          enumData := 0;
          If ( ATypeInfo^.Kind = tkInteger ) Then
            GetInt64(ATypeInfo,AName,enumData)
          Else
            GetEnum(ATypeInfo,AName,enumData);
          Case GetTypeData(ATypeInfo)^.OrdType Of
            otSByte : ShortInt(AData) := enumData;
            otUByte : Byte(AData) := enumData;
            otSWord : SmallInt(AData) := enumData;
            otUWord : Word(AData) := enumData;
            otSLong,
            otULong : LongInt(AData) := enumData;
          End;
      {$IFNDEF FPC}
        end;
      {$ENDIF}
      end;
    tkFloat :
      Begin
        floatDt := 0;
        GetFloat(ATypeInfo,AName,floatDt);
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : Single(AData)    := floatDt;
          ftDouble : Double(AData)    := floatDt;
          ftExtended : Extended(AData)    := floatDt;
          ftCurr : Currency(AData)    := floatDt;
{$IFDEF CPU86}
          ftComp : Comp(AData)    := floatDt;
{$ENDIF}
        End;
      End;
  End;
end;

procedure TSOAPBaseFormatter.GetScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  var   AData
);
Var
  enumData : TEnumIntType;
  floatDt : Extended;
  dataBuffer : string;
  nd : TDOMNode;
begin
  CheckScope();
  nd := StackTop().ScopeObject;
  if nd.HasChildNodes() then
    dataBuffer := nd.FirstChild.NodeValue
  else
    dataBuffer := StackTop().ScopeObject.NodeValue;
  Case ATypeInfo^.Kind Of
    tkInt64      : Int64(AData) := StrToInt64Def(Trim(dataBuffer),0);
    {$IFDEF FPC}
    tkQWord      : QWord(AData) := StrToInt64Def(Trim(dataBuffer),0);
    {$ENDIF}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} : string(AData) := dataBuffer;
    tkClass :
      begin
        raise ESOAPException.Create('Inner Scope value must be a "simple type" value.');
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        dataBuffer := LowerCase(Trim(dataBuffer));
        if IsStrEmpty(dataBuffer) then
          Boolean(AData) := False
        else
          Boolean(AData) := StrToBool(dataBuffer);
      end;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
        if ( ATypeInfo^.Kind = tkInteger ) then
          enumData := StrToIntDef(Trim(dataBuffer),0)
        else
          enumData := GetEnumValue(ATypeInfo,GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetInternalPropertyName(dataBuffer));
        case GetTypeData(ATypeInfo)^.OrdType of
          otSByte : ShortInt(AData) := enumData;
          otUByte : Byte(AData)     := enumData;
          otSWord : SmallInt(AData) := enumData;
          otUWord : Word(AData)     := enumData;
          otSLong,
          otULong : LongInt(AData)  := enumData;
        end;
      end;
    tkFloat :
      begin
{$IFDEF FPC}
  {$IFDEF FPC_211}
        floatDt := StrToFloatDef(Trim(dataBuffer),0,wst_FormatSettings);
  {$ELSE}
        floatDt := StrToFloatDef(Trim(dataBuffer),0);
  {$ENDIF}
{$ELSE}
        floatDt := StrToFloatDef(Trim(dataBuffer),0,wst_FormatSettings);
{$ENDIF}
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle    : Single(AData)        := floatDt;
          ftDouble    : Double(AData)        := floatDt;
          ftExtended  : Extended(AData)      := floatDt;
          ftCurr      : Currency(AData)      := floatDt;
{$IFDEF CPU86}
          ftComp      : Comp(AData)          := floatDt;
{$ENDIF}
        end;
      end;
  end;
end;

function TSOAPBaseFormatter.ReadBuffer (const AName : string ) : string;
Var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s : string;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := Copy(FindAttributeByValueInScope(StackTop().NameSpace),AnsiPos(':',namespaceShortName) + 1,MaxInt);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName);
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;

  if Assigned(locElt) then begin
    Result := NodeToBuffer(locElt);
  end else begin
    Error('Param or Attribute not found : "%s"',[AName]);
  end;
end;

procedure TSOAPBaseFormatter.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TSOAPBaseFormatter.LoadFromStream(AStream: TStream);
Var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := GetXmlDoc().DocumentElement;
  If Assigned(nd) Then
    PushStack(nd);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string);
begin
  Raise ESOAPException.Create(AMsg);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise ESOAPException.CreateFmt(AMsg,AArgs);
end;

{ TScopedArrayStackItem }

function TScopedArrayStackItem.CreateList(const ANodeName : string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := ScopeObject.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

{ TEmbeddedArrayStackItem }

function TEmbeddedArrayStackItem.CreateList(const ANodeName: string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
{$IFNDEF FPC}
    Result := FilterList(ScopeObject.childNodes,ANodeName);
{$ELSE}
    Result := {$IFNDEF FPC_211}TDOMNodeList{$ELSE}TDOMElementList{$ENDIF}.Create(ScopeObject,ANodeName);
{$ENDIF}
  end else begin
    Result := nil;
  end;
end;

end.
