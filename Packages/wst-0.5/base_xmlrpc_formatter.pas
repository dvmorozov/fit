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
unit base_xmlrpc_formatter;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  {$IFNDEF FPC}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  sPROTOCOL_NAME = 'XMLRPC';


  sCONTENT_TYPE = 'contenttype';
  sFORMAT = 'format';
  sXMLRPC_CONTENT_TYPE = 'text/xml';

  sDATA  = 'data';
  sFAULT = 'fault';
  sFAULT_CODE = 'faultCode';
  sFAULT_STRING = 'faultString';
  sMEMBER = 'member';
  sMETHOD_CALL  = 'methodCall';
  sMETHOD_NAME  = 'methodName';
  sMETHOD_RESPONSE  = 'methodResponse';
  sNAME = 'name';
  sPARAM = 'param';
  sPARAMS = 'params';
  sVALUE = 'value';

type

  TwstXMLDocument = {$IFNDEF FPC}wst_delphi_xml.TXMLDocument{$ELSE}TXMLDocument{$ENDIF};
  
  TEnumIntType = Int64;

  TXmlRpcDataType = (
    xdtString, xdtInt, xdtBoolean, xdtdouble, xdtDateTime, xdtBase64,
    xdtStruct, xdtArray
  );
  
const
  XmlRpcDataTypeNames : array[TXmlRpcDataType] of string = (
    'string', 'int', 'boolean', 'double', 'dateTime.iso8601', 'base64',
    'struct', 'array'
  );
  
type
  { ESOAPException }

  EXmlRpcException = class(EBaseRemoteException)
  end;

  TFoundState = ( fsNone, fsFoundNonNil, fsFoundNil );
  
  { TStackItem }

  TStackItem = class
  private
    FFoundState : TFoundState;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
  protected
    function GetItemsCount() : Integer;virtual;
    procedure SetFoundState(const AFoundState : TFoundState);
  public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;virtual;abstract;
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property ItemsCount : Integer read GetItemsCount;
    property FoundState : TFoundState read FFoundState;
  end;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  public
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;
  end;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
    FDataScope : TDOMNode;
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList():TDOMNodeList;
  public
    destructor Destroy();override;
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;
  end;

  { TParamsArrayStackItem }

  TParamsArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList():TDOMNodeList;
  public
    destructor Destroy();override;
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;
  end;
  
{$M+}

  { TXmlRpcBaseFormatter }

  TXmlRpcBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FContentType: string;
    FDoc : TXMLDocument;
    FStack : TObjectStack;
    FSerializationStyle: TSerializationStyle;
  private
    procedure InternalClear(const ACreateDoc : Boolean);

    function HasScope():Boolean;//inline;

    procedure CheckScope();//inline;
    function InternalPutData(
      const AName      : string;
      const AType      : TXmlRpcDataType;
      const AData      : string
    ):TDOMNode;
    function PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    ):TDOMNode;
    {$IFDEF FPC}
    function PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    ):TDOMNode;
    {$ENDIF}
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
    {$IFDEF FPC}
    procedure GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    );
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
    function GetXmlDoc():TXMLDocument;
    function PushStack(AScopeObject : TDOMNode):TStackItem;overload;
    function PushStack(
            AScopeObject : TDOMNode;
      const AStyle       : TArrayStyle;
      const AItemName    : string
    ):TStackItem;overload;
    function PushStackParams(AScopeObject : TDOMNode) : TStackItem;
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
  published
    property ContentType : string Read FContentType Write FContentType;
  end;
{$M-}

implementation
Uses {$IFNDEF FPC}XMLDoc,XMLIntf,{$ELSE}XMLWrite, XMLRead,wst_fpc_xml,{$ENDIF}
     imp_utils;

{ TStackItem }

function TStackItem.GetItemsCount(): Integer;
begin
  Result := GetNodeItemsCount(ScopeObject);
end;

procedure TStackItem.SetFoundState (const AFoundState : TFoundState );
begin
  FFoundState := AFoundState;
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
var
  memberNode, tmpNode : TDOMNode;
  i : Integer;
  chilNodes : TDOMNodeList;
  nodeFound : Boolean;
begin
  Result := nil;
  if ScopeObject.HasChildNodes() then begin
    nodeFound := False;
    memberNode := ScopeObject.FirstChild;
    while ( not nodeFound ) and ( memberNode <> nil ) do begin
      if memberNode.HasChildNodes() then begin
        chilNodes := memberNode.ChildNodes;
        for i := 0 to Pred(GetNodeListCount(chilNodes)) do begin
          tmpNode := chilNodes.Item[i];
          if AnsiSameText(sNAME,tmpNode.NodeName) and
             ( tmpNode.FirstChild <> nil ) and
             AnsiSameText(ANodeName,tmpNode.FirstChild.NodeValue)
          then begin
            nodeFound := True;
            Break;
          end;
        end;
        if nodeFound then begin
        {$IFNDEF FPC}
          tmpNode := wst_delphi_xml.FindNode(memberNode,sVALUE);
        {$ELSE}
          tmpNode := memberNode.FindNode(sVALUE);
        {$ENDIF}
          if ( tmpNode <> nil ) and ( tmpNode.FirstChild <> nil ) then begin
            Result := tmpNode.FirstChild;
            Break;
          end;
        end;
      end;
      memberNode := memberNode.NextSibling;
    end;
  end;
  if ( Result <> nil ) then begin
    if Result.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
  end else begin
    SetFoundState(fsNone);
  end;
end;

function TObjectStackItem.CreateBuffer(
  const AName: String;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  memberNode, nd : TDOMNode;
begin
  memberNode := ScopeObject.OwnerDocument.CreateElement(sMEMBER);
  ScopeObject.AppendChild(memberNode);

  nd := ScopeObject.OwnerDocument.CreateElement(sNAME);
  memberNode.AppendChild(nd);
  nd.AppendChild(ScopeObject.OwnerDocument.CreateTextNode(AName));

  nd := ScopeObject.OwnerDocument.CreateElement(sVALUE);
  memberNode.AppendChild(nd);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  nd.AppendChild(Result);
end;

{ TArrayStackItem }

procedure TArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList();
  end;
end;

function TArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

function TArrayStackItem.CreateList(): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() and ScopeObject.FirstChild.HasChildNodes() then begin
    Result := ScopeObject.FirstChild.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

destructor TArrayStackItem.Destroy();
begin
  if Assigned(FItemList) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise EXmlRpcException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  if Result.HasChildNodes() then begin
    if Result.FirstChild.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
    Result := Result.FirstChild;//.FirstChild;
    Inc(FIndex);
    ANodeName := Result.NodeName;
  end else begin
    raise EXmlRpcException.CreateFmt('Invalid array item : Index = %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  end;
end;

function TArrayStackItem.CreateBuffer(
  const AName: string;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  nd : TDOMNode;
begin
  if ( FDataScope = nil ) then begin
    FDataScope := ScopeObject.OwnerDocument.CreateElement(sDATA);
    ScopeObject.AppendChild(FDataScope);
  end;

  nd := FDataScope.OwnerDocument.CreateElement(sVALUE);
  FDataScope.AppendChild(nd);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  nd.AppendChild(Result);
end;

{ TXmlRpcBaseFormatter }

procedure TXmlRpcBaseFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

function TXmlRpcBaseFormatter.PushStack(AScopeObject : TDOMNode) : TStackItem;
begin
  Result := FStack.Push(TObjectStackItem.Create(AScopeObject,stObject)) as TStackItem;
end;

function TXmlRpcBaseFormatter.PushStack(
        AScopeObject : TDOMNode;
  const AStyle       : TArrayStyle;
  const AItemName    : string
): TStackItem;
begin
  Result := FStack.Push(TArrayStackItem.Create(AScopeObject,stArray)) as TStackItem;
end;

function TXmlRpcBaseFormatter.PushStackParams(AScopeObject: TDOMNode): TStackItem;
begin
  Result := FStack.Push(TParamsArrayStackItem.Create(AScopeObject,stArray)) as TStackItem;
end;

function TXmlRpcBaseFormatter.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stObject,asNone,'');
end;

function TXmlRpcBaseFormatter.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stArray,AStyle,AItemName);
end;

procedure TXmlRpcBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TXmlRpcBaseFormatter.BeginHeader();
begin
end;

procedure TXmlRpcBaseFormatter.EndHeader();
begin
end;

procedure TXmlRpcBaseFormatter.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  ReleaseDomNode(FDoc);
  if ACreateDoc then
    FDoc := CreateDoc();
end;

function TXmlRpcBaseFormatter.HasScope(): Boolean;
begin
  Result := FStack.AtLeast(1);
end;

function TXmlRpcBaseFormatter.FindAttributeByValueInNode(
  Const AAttValue : String;
  Const ANode     : TDOMNode;
  Out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TXmlRpcBaseFormatter.FindAttributeByNameInNode(
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

function TXmlRpcBaseFormatter.FindAttributeByValueInScope(const AAttValue: String): String;
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

function TXmlRpcBaseFormatter.FindAttributeByNameInScope(const AAttName: String): String;
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

procedure TXmlRpcBaseFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error('There is no scope.');
end;

function TXmlRpcBaseFormatter.InternalPutData(
  const AName      : string;
  const AType      : TXmlRpcDataType;
  const AData      : string
): TDOMNode;
begin
  Result := StackTop().CreateBuffer(AName,AType).AppendChild(FDoc.CreateTextNode(AData));
end;

function TXmlRpcBaseFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumIntType
): TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,AData))
            );
end;

{$IFDEF FPC}
function TXmlRpcBaseFormatter.PutBool(
  const AName     : String;
  const ATypeInfo : PTypeInfo;
  const AData     : Boolean
) : TDOMNode;
var
  v : Char;
begin
  if AData then
    v := '1'
  else
    v := '0';
  Result := InternalPutData(AName,xdtBoolean,v);
end;
{$ENDIF}

function TXmlRpcBaseFormatter.PutInt64(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Int64
): TDOMNode;
begin
  Result := InternalPutData(AName,xdtInt,IntToStr(AData));
end;

function TXmlRpcBaseFormatter.PutStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: String
):TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              StringReplace(StringReplace(AData,'<','&lt;',[rfReplaceAll]),'&','&amp;',[rfReplaceAll])
            );
end;

procedure TXmlRpcBaseFormatter.PutObj(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

function TXmlRpcBaseFormatter.PutFloat(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
):TDOMNode;
Var
  s, frmt : string;
  prcsn,i : Integer;
begin
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle,
    ftCurr,
    ftComp      : prcsn := 7;
    ftDouble,
    ftExtended  : prcsn := 15;
  End;
  frmt := '#.' + StringOfChar('#',prcsn) + 'E-0';
  s := FormatFloat(frmt,AData);
  i := Pos(',',s);
  If ( i > 0 ) Then
    s[i] := '.';
  Result := InternalPutData(AName,xdtdouble,s);
end;

function TXmlRpcBaseFormatter.GetNodeValue(var AName: string): DOMString;
var
  locElt : TDOMNode;
  stkTop : TStackItem;
begin
  stkTop := StackTop();
  locElt := stkTop.FindNode(AName) as TDOMElement;

  if Assigned(locElt) then begin
    if locElt.HasChildNodes then begin
      Result := locElt.FirstChild.NodeValue
    end else begin
      if ( stkTop.FoundState = fsFoundNil ) then
        Result := ''
      else
        Result := locElt.NodeValue;
    end;
  end else begin
    Error('Param or Attribute not found : "%s"',[AName]);
  end;
end;

procedure TXmlRpcBaseFormatter.GetEnum(
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

{$IFDEF FPC}
procedure TXmlRpcBaseFormatter.GetBool(
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

procedure TXmlRpcBaseFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: Integer
);
begin
  AData := StrToIntDef(Trim(GetNodeValue(AName)),0);
end;
{$ENDIF}

procedure TXmlRpcBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Int64
);
begin
  AData := StrToInt64Def(Trim(GetNodeValue(AName)),0);
end;

procedure TXmlRpcBaseFormatter.GetFloat(
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

procedure TXmlRpcBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : String
);
begin
  AData := GetNodeValue(AName);
end;

procedure TXmlRpcBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
end;

function TXmlRpcBaseFormatter.GetXmlDoc(): TwstXMLDocument;
begin
  Result := FDoc;
end;

function TXmlRpcBaseFormatter.GetCurrentScope(): String;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TXmlRpcBaseFormatter.GetCurrentScopeObject(): TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TXmlRpcBaseFormatter.StackTop(): TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TXmlRpcBaseFormatter.PopStack(): TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

constructor TXmlRpcBaseFormatter.Create();
begin
  Inherited Create();
  FContentType := sXMLRPC_CONTENT_TYPE;
  FStack := TObjectStack.Create();
  FDoc := CreateDoc();
end;

destructor TXmlRpcBaseFormatter.Destroy();
begin
  ReleaseDomNode(FDoc);
  ClearStack();
  FStack.Free();
  inherited Destroy();
end;

procedure TXmlRpcBaseFormatter.Clear();
begin
  InternalClear(True);
end;

procedure TXmlRpcBaseFormatter.BeginObject(
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
begin
  BeginScope(AName,'','',stObject,asNone);
end;

procedure TXmlRpcBaseFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
var
  i,j, k : Integer;
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

  BeginScope(AName,'','',stArray,AStyle);
end;

procedure TXmlRpcBaseFormatter.NilCurrentScope();
begin
end;

function TXmlRpcBaseFormatter.IsCurrentScopeNil(): Boolean;
begin
  Result := False;
end;

procedure TXmlRpcBaseFormatter.BeginScope(
  Const AScopeName,ANameSpace : string;
  Const ANameSpaceShortName   : string;
  Const AScopeType            : TScopeType;
  const AStyle                : TArrayStyle
);
Var
  e : TDOMNode;
  dtType : TXmlRpcDataType;
begin
  if ( AScopeType = stArray ) then
    dtType := xdtArray
  else
    dtType := xdtStruct;
  if HasScope() then begin
    e := StackTop().CreateBuffer(AScopeName,dtType);
  end else begin
    e := FDoc.CreateElement(XmlRpcDataTypeNames[dtType]);
    FDoc.AppendChild(e);
  end;
  if ( AScopeType = stObject ) then begin
    PushStack(e);
  end else begin
    PushStack(e,AStyle,'');
  end;
end;

function TXmlRpcBaseFormatter.InternalBeginScopeRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AScopeType : TScopeType;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.FindNode(AScopeName);
  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    if ( AScopeType = stObject ) then begin
      PushStack(locNode);
    end else begin
      PushStack(locNode,AStyle,AItemName);
    end;
    Result := StackTop().GetItemsCount();
  end;
end;

procedure TXmlRpcBaseFormatter.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TXmlRpcBaseFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

procedure TXmlRpcBaseFormatter.EndScope();
begin
  CheckScope();
  FStack.Pop().Free();
end;

procedure TXmlRpcBaseFormatter.AddScopeAttribute(const AName, AValue: string);
begin
//  CheckScope();
  //GetCurrentScopeObject().SetAttribute(AName,AValue);
end;

procedure TXmlRpcBaseFormatter.Put(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  {$IFDEF FPC}boolData : Boolean;{$ENDIF}
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
      Begin
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
      End;
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

procedure TXmlRpcBaseFormatter.PutScopeInnerValue(
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
        raise EXmlRpcException.Create('Inner Scope value must be a "simple type" value.');
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

procedure TXmlRpcBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData
);
Var
  int64Data : Int64;
  strData : string;
  objData : TObject;
  {$IFDEF FPC}boolData : Boolean;{$ENDIF}
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
      Begin
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
      End;
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

procedure TXmlRpcBaseFormatter.GetScopeInnerValue(
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
        raise EXmlRpcException.Create('Inner Scope value must be a "simple type" value.');
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
        //floatDt := StrToFloatDef(Trim(dataBuffer),0,wst_FormatSettings);
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

function TXmlRpcBaseFormatter.ReadBuffer (const AName : string ) : string;
var
  locElt : TDOMNode;
  stkTop : TStackItem;
  locName : string;
begin
  stkTop := StackTop();
  locName := AName;
  locElt := stkTop.FindNode(locName);

  if Assigned(locElt) then begin
    Result := NodeToBuffer(locElt);
  end else begin
    Error('Param or Attribute not found : "%s"',[AName]);
  end;
end;

procedure TXmlRpcBaseFormatter.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TXmlRpcBaseFormatter.LoadFromStream(AStream: TStream);
Var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := GetXmlDoc().DocumentElement;
  If Assigned(nd) Then
    PushStack(nd);
end;

procedure TXmlRpcBaseFormatter.Error(const AMsg: string);
begin
  Raise EXmlRpcException.Create(AMsg);
end;

procedure TXmlRpcBaseFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EXmlRpcException.CreateFmt(AMsg,AArgs);
end;


{ TParamsArrayStackItem }

procedure TParamsArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList();
  end;
end;

function TParamsArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

function TParamsArrayStackItem.CreateList(): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := ScopeObject.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

destructor TParamsArrayStackItem.Destroy();
begin
  if Assigned(FItemList) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TParamsArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise EXmlRpcException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  if Result.HasChildNodes() then begin
    if Result.FirstChild.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
    Result := Result.FirstChild.FirstChild;
    Inc(FIndex);
    ANodeName := Result.NodeName;
  end else begin
    raise EXmlRpcException.CreateFmt('Invalid array item : Index = %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  end;
end;

function TParamsArrayStackItem.CreateBuffer(
  const AName: string;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  prmNode, valueNode : TDOMNode;
begin
  prmNode := ScopeObject.OwnerDocument.CreateElement(sPARAM);
  ScopeObject.AppendChild(prmNode);
  valueNode := ScopeObject.OwnerDocument.CreateElement(sVALUE);
  prmNode.AppendChild(valueNode);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  valueNode.AppendChild(Result);
end;

end.
