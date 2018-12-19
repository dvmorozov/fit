{$INCLUDE wst_global.inc}
unit wst_fpc_xml;

interface

uses
  Classes, SysUtils, DOM;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNode);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function CreateDoc() : TXMLDocument ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}

  function NodeToBuffer(ANode : TDOMNode):string ;
  
implementation
uses XMLWrite;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
var
  chdLst : TDOMNodeList;
begin
  if ANode.HasChildNodes then begin
    chdLst := ANode.ChildNodes;
    try
      Result := chdLst.Count;
    finally
      chdLst.Release();
    end;
  end else begin
    Result := 0;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ANodeList.Count;
end;

function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ANodeList.Length;
end;

procedure ReleaseDomNode(ADomNode : TDOMNode);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  ADomNode.Free();
end;

procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  ADomNode.Release();
end;

function CreateDoc() : TXMLDocument ;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := TXMLDocument.Create();
  Result.Encoding := 'UTF-8';
end;

function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ANode.FindNode(ANodeName);
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locStream : TStringStream;
begin
  locStream := TStringStream.Create('');
  try
    WriteXML(ANode,locStream);
    Result := locStream.DataString;
  finally
    locStream.Free();
  end;
end;

end.
