unit wst_delphi_xml;

interface
uses
  SysUtils, Classes, xmldom, XMLIntf;

const
  LineEnding = sLineBreak;

type

  TDOMNode = IDOMNode;
  TDOMNodeList = IDOMNodeList;
  TDOMNamedNodeMap  = IDOMNamedNodeMap;
  TXMLDocument = IDOMDocument;
  TDOMElement = IDOMElement;

  function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;
  function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;
  procedure ReleaseDomNode(ADomNode : IInterface);overload;
  procedure ReleaseDomNode(var ADomNode : TXMLDocument);overload;

  function CreateDoc() : TXMLDocument ;
  procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);
  procedure ReadXMLFile(ADoc : TXMLDocument; AStream : TStream);
  function NodeToBuffer(ANode : TDOMNode):string ;

  function FilterList(const ALIst : IDOMNodeList; const ANodeName : widestring):IDOMNodeList ;
  
implementation
uses XmlDoc;

function FindNode(ANode : TDOMNode; const ANodeName : string):TDOMNode;
var
  i, c : Integer;
  lst : TDOMNodeList;
begin
  Result := nil;
  if ANode.hasChildNodes then begin
    lst := ANode.childNodes;
    c := lst.length;
    for i  := 0 to Pred(c) do begin
      if ( ANodeName = lst.item[i].nodeName ) then begin
        Result := lst[i];
        Break;
      end;
    end;
  end;
end;

procedure WriteXMLFile(ADoc : TXMLDocument; AStream : TStream);
begin
  (ADoc as IDOMPersist).saveToStream(AStream);
end;

procedure ReadXMLFile(ADoc : TXMLDocument; AStream : TStream);
begin
  (ADoc as IDOMPersist).loadFromStream(AStream);
end;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
begin
  if ANode.HasChildNodes then begin
    Result := ANode.childNodes.length;
  end else begin
    Result := 0;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;
begin
  Result := ANodeList.length;
end;

function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;
begin
  Result := ANodeList.length;
end;

procedure ReleaseDomNode(ADomNode : IInterface);
begin
end;

procedure ReleaseDomNode(var ADomNode : TXMLDocument);
begin

end;

function CreateDoc() : TXMLDocument ;
var
  locDoc : IXMLDocument;
begin
  locDoc := XmlDoc.TXMLDocument.Create(nil);
  locDoc.Active := True;
  Result := locDoc.DOMDocument;
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locNodeEx : IDOMNodeEx;
begin
  if Supports(ANode,IDOMNodeEx,locNodeEx) then begin
    Result := locNodeEx.xml;
  end else begin
    raise Exception.Create('This Xml library do not provide "IDOMNodeEx" support.');
  end;
end;

type
  TDOMNodeSelectListImp = class(TInterfacedObject,IDOMNodeList)
  private
    FItemName : widestring;
    FInnerList : IDOMNodeList;
    FCount : Integer;
  private
    function internal_get_item(index: Integer): IDOMNode;
  protected
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(
      const AInnerList : IDOMNodeList;
      const AItemName  : widestring
    );
  end;

function FilterList(const ALIst : IDOMNodeList; const ANodeName : widestring):IDOMNodeList ;
begin
  Result := TDOMNodeSelectListImp.Create(ALIst,ANodeName);
end;

{ TDOMNodeSelectListImp }

constructor TDOMNodeSelectListImp.Create(
  const AInnerList: IDOMNodeList;
  const AItemName: widestring
);
begin
  Assert(AInnerList <> nil);
  FInnerList := AInnerList;
  FItemName := AItemName;
  FCount := -1;
end;

function TDOMNodeSelectListImp.get_item(index: Integer): IDOMNode;
begin
  Result := internal_get_item(index);
  if ( Result = nil ) then
    raise Exception.CreateFmt('Invalid item at %d.',[index]);
end;

function TDOMNodeSelectListImp.get_length() : Integer;
begin
  if ( FCount >= 0 ) then begin
    Result := FCount;
  end else begin
    FCount := 0;
    while Assigned(internal_get_item(FCount)) do begin
      Inc(FCount);
    end;
    Result := FCount;
  end;
end;

function TDOMNodeSelectListImp.internal_get_item(index: Integer): IDOMNode;
var
  i : Integer;
  crt : IDOMNode;
begin
  Result := nil;
  if ( FInnerList.length > 0 ) then begin
    i := -1;
    crt := FInnerList.item[0];
    while ( crt <> nil ) do begin
      if ( FItemName = crt.nodeName ) then begin
        Inc(i);
        if ( i = index ) then begin
          Result := crt;
          Break;
        end;
      end;
      crt := crt.nextSibling;
    end;
  end;
end;

end.
