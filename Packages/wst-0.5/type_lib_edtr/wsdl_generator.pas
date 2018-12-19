{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit wsdl_generator;

{$INCLUDE wst.inc}

interface

uses
  Classes, SysUtils, TypInfo,
  DOM,
  pastree, pascal_parser_intf;
  
type

  EWsdlGeneratorException = class(Exception) end;
  TBaseTypeHandler = class;
  TBaseTypeHandlerClass = class of TBaseTypeHandler;
  
  IWsdlTypeHandler = interface
    ['{541EA377-4F70-49B1-AFB4-FC62B24F567B}']
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );
  end;

  IWsdlTypeHandlerRegistry = Interface
    ['{C5666646-3426-4696-93EE-AFA8EE7CAE53}']
    function Find(
          ASymbol  : TPasElement;
      out AHandler : IWsdlTypeHandler
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  End;

  TBaseTypeHandler = class(TInterfacedObject,IWsdlTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );virtual;abstract;
    class function CanHandle(ASymbol : TObject) : Boolean;virtual;abstract;
  end;

  { TTypeDefinition_TypeHandler }

  TTypeDefinition_TypeHandler = class(TBaseTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  { TTypeAliasDefinition_TypeHandler }

  TTypeAliasDefinition_TypeHandler = class(TBaseTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;
  
  { TEnumTypeHandler }

  TEnumTypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  { TClassTypeDefinition_TypeHandler }

  TClassTypeDefinition_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  { TBaseArrayRemotable_TypeHandler }

  TBaseArrayRemotable_TypeHandler = class(TTypeDefinition_TypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
    class function CanHandle(ASymbol : TObject) : Boolean;override;
  end;

  procedure GenerateWSDL(ASymbolTable : TwstPasTreeContainer; ADoc : TDOMDocument);

  function GetWsdlTypeHandlerRegistry():IWsdlTypeHandlerRegistry;
  
implementation
uses Contnrs;

const
  sWSDL_NS       = 'http://schemas.xmlsoap.org/wsdl/';
  sSOAP_NS       = 'http://schemas.xmlsoap.org/wsdl/soap/';
  sSOAP          = 'soap';
  //sSOAP_ENC_NS   = 'http://schemas.xmlsoap.org/soap/encoding/';
  sXMLNS         = 'xmlns';
  sXSD_NS        = 'http://www.w3.org/2001/XMLSchema';
  sXSD           = 'xsd';
  sTNS           = 'tns';

  sSOAP_ACTION      = 'soapAction';
  //sSOAP_ENCODED     = 'encoded';
  //sSOAP_ENCODING_STYLE = 'encodingStyle';
  sSOAP_RPC         = 'rpc';
  sSOAP_TRANSPORT   = 'http://schemas.xmlsoap.org/soap/http';
  sSOAP_USE         = 'use';
  
  sADDRESS            = 'address';
  sATTRIBUTE          = 'attribute';
  sBASE               = 'base';
  sBINDING            = 'binding';
  sBODY               = 'body';
  sCOMPLEX_TYPE       = 'complexType';
  sCUSTOM_ATTRIBUTE   = 'customAttributes';
  sDOCUMENT           = 'document';
  sELEMENT            = 'element';
  sENUMERATION        = 'enumeration';
  sEXTENSION          = 'extension';
  sGUID               = 'GUID';
  sHEADER_Block       = 'headerBlock';
  sITEM               = 'item';
  sLOCATION           = 'location';
  sMIN_OCCURS         = 'minOccurs';
  sMAX_OCCURS         = 'maxOccurs';
  sNAME               = 'name';
  sNAME_SPACE         = 'namespace';
  sPORT_TYPE          = 'portType';
  sRESTRICTION        = 'restriction';
  sSEQUENCE           = 'sequence';
  sSERVICE           = 'service';
  sSIMPLE_TYPE        = 'simpleType';
  sSTYLE              = 'style';
  sTRANSPORT          = 'transport';
  sTYPE               = 'type';
  sUNBOUNDED          = 'unbounded';
  //sUSE                = 'use';
  sVALUE              = 'value';

  sWSDL_DEFINITIONS        = 'definitions';
  sWSDL_INPUT              = 'input';
  sWSDL_MESSAGE            = 'message';
  sWSDL_NAME               = 'name';
  sWSDL_OPERATION          = 'operation';
  sWSDL_OUTPUT             = 'output';
  sWSDL_PART               = 'part';
  sWSDL_PORT               = 'port';
  sWSDL_PORT_TYPE          = sPORT_TYPE;
  sWSDL_SCHEMA             = 'schema';
  sWSDL_TARGET_NS          = 'targetNamespace';
  sWSDL_TYPE               = sTYPE;
  sWSDL_TYPES              = 'types';
  
var
  WsdlTypeHandlerRegistryInst : IWsdlTypeHandlerRegistry;


function GetTypeNameSpace(
  AContainer : TwstPasTreeContainer;
  AType      : TPasElement
) : string;
var
  locElt : TPasElement;
begin
  Result := '';
  locElt := AType;
  if ( locElt <> nil ) then begin
    if locElt.InheritsFrom(TPasUnresolvedTypeRef) then
      locElt := AContainer.FindElement(AContainer.GetExternalName(locElt));
    if ( locElt <> nil ) and
       ( not locElt.InheritsFrom(TPasUnresolvedTypeRef) ) and
       //locElt.InheritsFrom(TPasType) and
       ( locElt.Parent <> nil ) and
       ( locElt.Parent.Parent <> nil )
    then begin
      Result := AContainer.GetExternalName(locElt.Parent.Parent);
    end;
  end;
  Result := Trim(Result);
  if ( Length(Result) = 0 ) then
    Result := AContainer.GetExternalName(AContainer.CurrentModule);
end;


type

  { TWsdlTypeHandlerRegistry }

  TWsdlTypeHandlerRegistry = class(TInterfacedObject,IInterface,IWsdlTypeHandlerRegistry)
  private
    FList : TClassList;
  private
    function FindIndexOfHandler(ASymbol : TPasElement) : Integer;
  protected
    function Find(
          ASymbol  : TPasElement;
      out AHandler : IWsdlTypeHandler
    ) : Boolean;
    procedure Register(AFactory : TBaseTypeHandlerClass);
  public
    constructor Create();
    destructor Destroy();override;
  end;

{ TWsdlTypeHandlerRegistry }

function TWsdlTypeHandlerRegistry.FindIndexOfHandler(ASymbol: TPasElement): Integer;
Var
  i, c : Integer;
begin
  Result := -1;
  c := FList.Count;
  for i := 0 to Pred(c) do begin
    if TBaseTypeHandlerClass(FList[i]).CanHandle(ASymbol) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TWsdlTypeHandlerRegistry.Find(
          ASymbol  : TPasElement;
      out AHandler : IWsdlTypeHandler
) : Boolean;
var
  fct : TBaseTypeHandlerClass;
  i : Integer;
begin
  i := FindIndexOfHandler(ASymbol);
  Result := ( i >= 0 );
  if Result then begin
    fct := TBaseTypeHandlerClass(FList[i]);
    AHandler := fct.Create() as IWsdlTypeHandler;
  end;
end;

procedure TWsdlTypeHandlerRegistry.Register(AFactory: TBaseTypeHandlerClass);
begin
  if ( FList.IndexOf(AFactory) = -1 ) then begin
    FList.Add(AFactory);
  end;
end;

constructor TWsdlTypeHandlerRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TWsdlTypeHandlerRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;//inline;
begin
  Result := ADoc.CreateElement(ANodeName);
  AParent.AppendChild(Result);
end;

function FindAttributeByValueInNode(
  const AAttValue        : string;
  const ANode            : TDOMNode;
  out   AResAtt          : string;
  const AStartIndex      : Integer = 0;
  const AStartingWith    : string = ''
):boolean;
var
  i,c : Integer;
  b : Boolean;
begin
  AResAtt := '';
  if Assigned(ANode) and Assigned(ANode.Attributes) then begin
    b := ( Length(AStartingWith) = 0);
    c := Pred(ANode.Attributes.Length);
    if ( AStartIndex >= 0 ) then
      i := AStartIndex;
    for i := 0 to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) and
         ( b or ( Pos(AStartingWith,ANode.Attributes.Item[i].NodeName) = 1 ))
      then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        AWsdlDocument : TDOMDocument
):string;//inline;
begin
  if FindAttributeByValueInNode(ANameSpace,AWsdlDocument.DocumentElement,Result,0,sXMLNS) then begin
    Result := Copy(Result,Length(sXMLNS+':')+1,MaxInt);
  end else begin
    Result := Format('ns%d',[AWsdlDocument.DocumentElement.Attributes.{$IFNDEF FPC_211}Count{$ELSE}Length{$ENDIF}]) ;
    AWsdlDocument.DocumentElement.SetAttribute(Format('%s:%s',[sXMLNS,Result]),ANameSpace);
  end;
end;

type TServiceElementType = ( setPortType, setBinding, setPort, setService,setAddress );
function GetServicePartName(AContainer : TwstPasTreeContainer; AService : TPasClassType; const AServicePart : TServiceElementType):string;
const PART_NAME_MAP : array[TServiceElementType] of shortstring = ('', 'Binding', 'Port', '','');
begin
  Result := AContainer.GetExternalName(AService) + PART_NAME_MAP[AServicePart];
end;

procedure GenerateWSDL(ASymbolTable : TwstPasTreeContainer; ADoc : TDOMDocument);

  procedure GenerateServiceMessages(
          AContract   : TPasClassType;
          ARootNode  : TDOMElement
  );
  
    procedure GenerateOperationMessage(AOperation : TPasProcedure);
    
      procedure GenerateParam(APrm : TPasArgument; AMsgNode : TDOMElement);
      var
        tmpNode : TDOMElement;
        ns_shortName, s : string;
        typItm : TPasType;
      begin
        tmpNode := CreateElement(sWSDL_PART,AMsgNode,ADoc);
        tmpNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(APrm));
        typItm := APrm.ArgType;
        if Assigned(typItm.Parent) and Assigned(typItm.Parent.Parent) then
          s := ASymbolTable.GetExternalName(typItm.Parent.Parent)
        else
          s := ASymbolTable.GetExternalName(ASymbolTable.CurrentModule);
        ns_shortName := GetNameSpaceShortName(s,ADoc);
        s := Format('%s:%s',[ns_shortName,ASymbolTable.GetExternalName(typItm)]);
        tmpNode.SetAttribute(sWSDL_TYPE,s);
      end;

      procedure GenerateResultParam(APrm : TPasResultElement; AMsgNode : TDOMElement);
      var
        tmpNode : TDOMElement;
        ns_shortName, s : string;
        typItm : TPasType;
      begin
        tmpNode := CreateElement(sWSDL_PART,AMsgNode,ADoc);
        tmpNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(APrm));
        typItm := APrm.ResultType;
        if Assigned(typItm.Parent) and Assigned(typItm.Parent.Parent) then
          s := ASymbolTable.GetExternalName(typItm.Parent.Parent)
        else
          s := ASymbolTable.GetExternalName(ASymbolTable.CurrentModule);
        ns_shortName := GetNameSpaceShortName(s,ADoc);
        s := Format('%s:%s',[ns_shortName,ASymbolTable.GetExternalName(typItm)]);
        tmpNode.SetAttribute(sWSDL_TYPE,s);
      end;

    var
      qryNode, rspNode : TDOMElement;
      ii, cc : Integer;
      pp : TPasArgument;
    begin
      qryNode := CreateElement(sWSDL_MESSAGE,ARootNode,ADoc);
      qryNode.SetAttribute(sWSDL_NAME,Format('%s',[ASymbolTable.GetExternalName(AOperation)]));
      rspNode := CreateElement(sWSDL_MESSAGE,ARootNode,ADoc);
      rspNode.SetAttribute(sWSDL_NAME,Format('%sResponse',[ASymbolTable.GetExternalName(AOperation)]));
      cc := AOperation.ProcType.Args.Count;
      for ii := 0 to Pred(cc) do begin
        pp := TPasArgument(AOperation.ProcType.Args[ii]);
        if ( pp.Access in [argDefault, argConst] ) then
          GenerateParam(pp,qryNode)
        else if ( pp.Access in [argVar, argOut] ) then
          GenerateParam(pp,rspNode);
      end;
      if AOperation.InheritsFrom(TPasFunction) then begin
        GenerateResultParam(TPasFunctionType(AOperation.ProcType).ResultEl,rspNode);
      end;
    end;
    
  Var
    j, k : Integer;
    po : TPasProcedure;
  begin
    k := AContract.Members.Count;
    if ( k > 0 ) then begin
      for j := 0 to pred(k) do begin
        if TPasElement(AContract.Members[j]).InheritsFrom(TPasProcedure) then begin
          po := TPasProcedure(AContract.Members[j]);
          GenerateOperationMessage(po);
        end;
      end;
    end;
  end;

  procedure GenerateServicePortType(AContract : TPasClassType; ARootNode : TDOMElement);

    procedure GenerateOperation(AOperation : TPasProcedure; APrtTypeNode : TDOMElement);
    var
      opNode, inNode, outNode : TDOMElement;
    begin
      opNode := CreateElement(sWSDL_OPERATION,APrtTypeNode,ADoc);
      opNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(AOperation));
      inNode := CreateElement(sWSDL_INPUT,opNode,ADoc);
      inNode.SetAttribute(sWSDL_MESSAGE,Format('%s:%s',[sTNS,ASymbolTable.GetExternalName(AOperation)]));
      outNode := CreateElement(sWSDL_OUTPUT,opNode,ADoc);
      outNode.SetAttribute(sWSDL_MESSAGE,Format('%s:%sResponse',[sTNS,ASymbolTable.GetExternalName(AOperation)]));
    end;

  var
    prtTypeNode, docNode : TDOMElement;
    j, k : Integer;
    po : TPasProcedure;
  begin
    prtTypeNode := CreateElement(sWSDL_PORT_TYPE,ARootNode,ADoc);
    if ( Length(AContract.InterfaceGUID) > 0 ) then begin
      docNode := CreateElement(sDOCUMENT,prtTypeNode,ADoc);
      CreateElement(sGUID,docNode,ADoc).SetAttribute(sVALUE,AContract.InterfaceGUID);
    end else begin
      docNode := nil;
    end;
    prtTypeNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(AContract));
    k := AContract.Members.Count;
    if ( k > 0 ) then begin
      for j := 0 to pred(k) do begin
        if TPasElement(AContract.Members[j]).InheritsFrom(TPasProcedure) then begin
          po := TPasProcedure(AContract.Members[j]);
          GenerateOperation(po,prtTypeNode);
        end;
      end;
    end;
  end;

  procedure GenerateServiceBinding(ABinding : TwstBinding; ARootNode : TDOMElement);

    procedure GenerateOperation(AOperation : TPasProcedure; ABndngNode : TDOMElement);
    var
      opNode, inNode, outNode, bdyNode : TDOMElement;
      strBuff, strSoapActBuffer : string;
      encdStyl{,encdStylURI} : string;
    begin
      strBuff := Format('%s:%s',[sSOAP,sWSDL_OPERATION]);
      opNode := CreateElement(sWSDL_OPERATION,ABndngNode,ADoc);
      opNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(AOperation));
        strSoapActBuffer := Trim(ASymbolTable.Properties.GetValue(AOperation,sTRANSPORT + '_' + sSOAP_ACTION));
        {if ( Length(strSoapActBuffer) = 0 ) then begin
          strSoapActBuffer := Format('%s/%s/%s',[ASymbolTable.GetExternalName(ASymbolTable.CurrentModule),ASymbolTable.GetExternalName(ABinding.Intf),ASymbolTable.GetExternalName(AOperation)]);
        end;}
        CreateElement(strBuff,opNode,ADoc).SetAttribute(sSOAP_ACTION,strSoapActBuffer);
        inNode := CreateElement(sWSDL_INPUT,opNode,ADoc);
          strBuff := Format('%s:%s',[sSOAP,sBODY]);
          bdyNode := CreateElement(strBuff,inNode,ADoc);
          encdStyl := 'literal';
          {encdStylURI := '';
          propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyle);
          if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
            encdStyl := Trim(propData^.Data);
          end;}
          bdyNode.SetAttribute(sSOAP_USE,encdStyl);
          bdyNode.SetAttribute(sNAME_SPACE,Format('%s',[ASymbolTable.GetExternalName(ASymbolTable.CurrentModule)]));
          {propData := Find(AOperation^.Properties,sFORMAT_Input_EncodingStyleURI);
          if Assigned(propData) and ( Length(Trim(propData^.Data)) > 0 ) then begin
            encdStylURI := Trim(propData^.Data);
          end;
          if ( Length(encdStylURI) > 0 ) then
            bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI); }

        outNode := CreateElement(sWSDL_OUTPUT,opNode,ADoc);
          strBuff := Format('%s:%s',[sSOAP,sBODY]);
          bdyNode := CreateElement(strBuff,outNode,ADoc);
          bdyNode.SetAttribute(sSOAP_USE,encdStyl);
          bdyNode.SetAttribute(sNAME_SPACE,Format('%s',[ASymbolTable.GetExternalName(ASymbolTable.CurrentModule)]));
          {if ( Length(encdStylURI) > 0 ) then
            bdyNode.SetAttribute(sSOAP_ENCODING_STYLE,encdStylURI);}
    end;

  var
    bndgNode, soapbndgNode : TDOMElement;
    j, k : Integer;
    po : TPasProcedure;
    strBuf : string;
  begin
    bndgNode := CreateElement(sBINDING,ARootNode,ADoc);
    bndgNode.SetAttribute(sWSDL_NAME,ABinding.Name);
    bndgNode.SetAttribute(sWSDL_TYPE,Format('%s:%s',[sTNS,ASymbolTable.GetExternalName(ABinding.Intf)]));

    strBuf := Format('%s:%s',[sSOAP,sBINDING]);
    soapbndgNode := CreateElement(strBuf,bndgNode,ADoc);
    soapbndgNode.SetAttribute(sSTYLE,sSOAP_RPC);
    soapbndgNode.SetAttribute(sTRANSPORT,sSOAP_TRANSPORT);

    k := ABinding.Intf.Members.Count;
    if ( k > 0 ) then begin
      for j := 0 to pred(k) do begin
        if TPasElement(ABinding.Intf.Members[j]).InheritsFrom(TPasProcedure) then begin
          po := TPasProcedure(ABinding.Intf.Members[j]);
          GenerateOperation(po,bndgNode);
        end;
      end;
    end;
  end;

  procedure GenerateServicePublication(ABinding : TwstBinding; ARootNode : TDOMElement);
  var
    srvcNode, portNode, soapAdrNode : TDOMElement;
    strBuf : string;
  begin
    srvcNode := CreateElement(sSERVICE,ARootNode,ADoc);
    srvcNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(ABinding.Intf));

    strBuf := Format('%s',[sWSDL_PORT]);
    portNode := CreateElement(strBuf,srvcNode,ADoc);
    portNode.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(ABinding.Intf) + 'Port');
    portNode.SetAttribute(sBINDING,Format('%s:%s',[sTNS,ABinding.Name]));

    strBuf := Format('%s:%s',[sSOAP,sADDRESS]);
    soapAdrNode := CreateElement(strBuf,portNode,ADoc);
    soapAdrNode.SetAttribute(sLOCATION,ABinding.Address);
  end;
  
  procedure GenerateServiceTypes();
  var
    j, k : Integer;
    tri : TPasElement;
    g : IWsdlTypeHandler;
    gr : IWsdlTypeHandlerRegistry;
    typeList : TList;
  begin
    gr := GetWsdlTypeHandlerRegistry();
    typeList := ASymbolTable.CurrentModule.InterfaceSection.Declarations;
    k := typeList.Count;
    for j := 0 to Pred(k) do begin
      tri := TPasElement(typeList[j]);
      if tri.InheritsFrom(TPasType) and
         ( not tri.InheritsFrom(TPasNativeClassType) ) and
         ( not tri.InheritsFrom(TPasNativeSimpleType) )
      then begin
        if gr.Find(tri,g) then
          g.Generate(ASymbolTable, tri,ADoc);
      end;
    end;
  end;

  function CreateRootNode():TDOMElement;
  begin
    Result := CreateElement(sWSDL_DEFINITIONS,ADoc,ADoc);
    Result.SetAttribute(sWSDL_NAME,ASymbolTable.GetExternalName(ASymbolTable.CurrentModule));
    
    Result.SetAttribute(sWSDL_TARGET_NS,ASymbolTable.GetExternalName(ASymbolTable.CurrentModule));
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sSOAP]),sSOAP_NS);
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sXSD]),sXSD_NS);
    Result.SetAttribute(Format('%s:%s',[sXMLNS,sTNS]),ASymbolTable.GetExternalName(ASymbolTable.CurrentModule));
    Result.SetAttribute(sXMLNS,sWSDL_NS);
  end;
  
  function CreateTypesRootNode(ARootNode :  TDOMNode):TDOMElement;
  begin
    Result := CreateElement(sWSDL_TYPES,ARootNode,ADoc);
  end;
  
var
  defNode, typesNode, schNode : TDOMElement;
  j, c : Integer;
  sym : TPasElement;
  ps : TPasClassType;
  decList : TList;
  bndg : TwstBinding;
begin
  if not ( Assigned(ASymbolTable) and Assigned(ADoc)) then
    Exit;

  defNode := CreateRootNode();
  typesNode := CreateTypesRootNode(defNode);
  schNode := CreateElement(sXSD + ':' + sWSDL_SCHEMA,typesNode,ADoc);
  schNode.SetAttribute(sXMLNS,sXSD_NS);
  schNode.SetAttribute(sWSDL_TARGET_NS,ASymbolTable.GetExternalName(ASymbolTable.CurrentModule));

  GenerateServiceTypes();

  decList := ASymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := decList.Count;
  for j := 0 to Pred(c) do begin
    sym := TPasElement(decList[j]);
    if sym.InheritsFrom(TPasClassType) and ( TPasClassType(sym).ObjKind = okInterface ) then begin
      ps := TPasClassType(sym);
      GenerateServiceMessages(ps,defNode);
      GenerateServicePortType(ps,defNode);
    end;
  end;

  for j := 0 to Pred(ASymbolTable.BindingCount) do begin
    bndg := ASymbolTable.Binding[j];
    GenerateServiceBinding(bndg,defNode);
      GenerateServicePublication(bndg,defNode);
  end;
end;

function GetWsdlTypeHandlerRegistry():IWsdlTypeHandlerRegistry;
begin
  Result := WsdlTypeHandlerRegistryInst;
end;

type

  { TFakeTypeHandler }

  TFakeTypeHandler = class(TBaseTypeHandler)
  protected
    procedure Generate(
            AContainer    : TwstPasTreeContainer;
      const ASymbol       : TPasElement;
            AWsdlDocument : TDOMDocument
    );override;
  end;

{ TClassTypeDefinition_TypeHandler }
type TTypeCategory = ( tcComplexContent, tcSimpleContent );
procedure TClassTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        AWsdlDocument : TDOMDocument
);
var
  cplxNode, docNode : TDOMElement;
  
  procedure CreateDocNode();
  begin
    if ( docNode = nil ) then begin
      docNode := CreateElement(sDOCUMENT,cplxNode,AWsdlDocument);
    end;
  end;
  
var
  typItm : TPasClassType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defTypesNode, defSchemaNode, sqcNode, propNode, derivationNode : TDOMElement;
  i : Integer;
  p : TPasProperty;
  typeCategory : TTypeCategory;
  hasSequence : Boolean;
  trueParent : TPasType;
begin
  inherited;
  docNode := nil;
  typItm := ASymbol as TPasClassType;
  if Assigned(typItm) then begin
    GetNameSpaceShortName(AContainer.GetExternalName(AContainer.CurrentModule) ,AWsdlDocument);
    defTypesNode := AWsdlDocument.DocumentElement.FindNode(sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;
    
    s := Format('%s:%s',[sXSD,sCOMPLEX_TYPE]);
    cplxNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    cplxNode.SetAttribute(sNAME, AContainer.GetExternalName(typItm)) ;

    typeCategory := tcComplexContent;
    derivationNode := nil;
    hasSequence := True;
    if Assigned(typItm.AncestorType) then begin
      trueParent := typItm.AncestorType;
      
      if trueParent.InheritsFrom(TPasNativeClassType) and AnsiSameText('THeaderBlock',trueParent.Name) then begin
        CreateDocNode();
        CreateElement(sCUSTOM_ATTRIBUTE,docNode,AWsdlDocument).SetAttribute(sHEADER_Block,'true');
      end;

      if trueParent.InheritsFrom(TPasAliasType) then begin
        trueParent := GetUltimeType(trueParent);
      end;
      if trueParent.InheritsFrom(TPasNativeSimpleContentClassType) or
         trueParent.InheritsFrom(TPasNativeSimpleType)
      then begin
        typeCategory := tcSimpleContent;
        derivationNode := CreateElement(Format('%s:%s',[sXSD,sEXTENSION]),cplxNode,AWsdlDocument);
        s := Trim(GetNameSpaceShortName(GetTypeNameSpace(AContainer,trueParent),AWsdlDocument));
        if ( Length(s) > 0 ) then begin
          s := s + ':';
        end;
        s := s + AContainer.GetExternalName(trueParent);
        derivationNode.SetAttribute(sBASE,s);
        hasSequence := False;
      end;
    end;
    for i := 0 to Pred(typItm.Members.Count) do begin
      if TPasElement(typItm.Members[i]).InheritsFrom(TPasProperty) then begin
        p := TPasProperty(typItm.Members[i]);
        if not AContainer.IsAttributeProperty(p) then begin
          if ( typeCategory = tcSimpleContent ) then begin
            raise EWsdlGeneratorException.CreateFmt('Invalid type definition, a simple type cannot have "not attribute" properties : "%s"',[AContainer.GetExternalName(ASymbol)]);
            hasSequence := True;
          end;
        end;
      end;
    end;
    if hasSequence then begin
      s := Format('%s:%s',[sXSD,sSEQUENCE]);
      if Assigned(derivationNode) then begin
        sqcNode := CreateElement(s,derivationNode,AWsdlDocument);
      end else begin
        sqcNode := CreateElement(s,cplxNode,AWsdlDocument);
      end;
    end else begin
      sqcNode := nil;
    end;


      for i := 0 to Pred(typItm.Members.Count) do begin
        if TPasElement(typItm.Members[i]).InheritsFrom(TPasProperty) then begin
          p := TPasProperty(typItm.Members[i]);
          if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) or AnsiSameText('True',p.StoredAccessorName) then begin
            if AContainer.IsAttributeProperty(p) then begin
              s := Format('%s:%s',[sXSD,sATTRIBUTE]);
              if Assigned(derivationNode) then
                propNode := CreateElement(s,derivationNode,AWsdlDocument)
              else
                propNode := CreateElement(s,cplxNode,AWsdlDocument);
            end else begin
              s := Format('%s:%s',[sXSD,sELEMENT]);
              propNode := CreateElement(s,sqcNode,AWsdlDocument);
            end;
            propNode.SetAttribute(sNAME,AContainer.GetExternalName(p));
            propTypItm := p.VarType;
            if Assigned(propTypItm) then begin
              prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm),AWsdlDocument);
              propNode.SetAttribute(sTYPE,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
              if AContainer.IsAttributeProperty(p) then begin
                if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then
                  propNode.SetAttribute(sATTRIBUTE,'optional')
                else
                  propNode.SetAttribute(sATTRIBUTE,'required');
              end else begin
                if AnsiSameText('Has',Copy(p.StoredAccessorName,1,3)) then
                  propNode.SetAttribute(sMIN_OCCURS,'0')
                else
                  propNode.SetAttribute(sMIN_OCCURS,'1');
                propNode.SetAttribute(sMAX_OCCURS,'1');
              end;
            end;
          end;
        end;
      end;
  end;
end;

class function TClassTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and
            ( ASymbol.InheritsFrom(TPasClassType) and ( TPasClassType(ASymbol).ObjKind = okClass ));
end;
  
{ TEnumTypeHandler }

procedure TEnumTypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol : TPasElement;
        AWsdlDocument : TDOMDocument
);
var
  typItm : TPasEnumType;
  ns_shortName, s : string;
  defTypesNode, defSchemaNode, resNode, restrictNode : TDOMElement;
  i, c : Integer;
  unitExternalName : string;
begin
  typItm := ASymbol as TPasEnumType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    if FindAttributeByValueInNode(unitExternalName,AWsdlDocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(sXMLNS+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[AWsdlDocument.DocumentElement.Attributes.{$IFNDEF FPC_211}Count{$ELSE}Length{$ENDIF}]) ;
      AWsdlDocument.DocumentElement.SetAttribute(Format('%s:%s',[sXMLNS,ns_shortName]),unitExternalName);
    end;
    defTypesNode := AWsdlDocument.DocumentElement.FindNode(sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    //s := Format('%s:%s',[sXSD,sELEMENT]);
    //eltNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    //eltNode.SetAttribute(sNAME, typItm.DeclaredName) ;
    s := Format('%s:%s',[sXSD,sSIMPLE_TYPE]);
    resNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    resNode.SetAttribute(sNAME, AContainer.GetExternalName(typItm)) ;
      s := Format('%s:%s',[sXSD,sRESTRICTION]);
      restrictNode := CreateElement(s,resNode,AWsdlDocument);
      restrictNode.SetAttribute(sBASE,Format('%s:%s',[sXSD,'string'])) ;
      c := typItm.Values.Count;
      for i := 0 to pred(c) do begin
        s := Format('%s:%s',[sXSD,sENUMERATION]);
        CreateElement(s,restrictNode,AWsdlDocument).SetAttribute(
          sVALUE,
          AContainer.GetExternalName(TPasEnumValue(typItm.Values[i]))
        );
      end;
  end;
end;

class function TEnumTypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasEnumType);
end;
  

{ TFakeTypeHandler }

procedure TFakeTypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        AWsdlDocument : TDOMDocument
);
begin
end;

{ TBaseArrayRemotable_TypeHandler }

procedure TBaseArrayRemotable_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol       : TPasElement;
        AWsdlDocument : TDOMDocument
);

  function GetNameSpaceShortName(const ANameSpace : string):string;//inline;
  begin
    if FindAttributeByValueInNode(ANameSpace,AWsdlDocument.DocumentElement,Result,0,sXMLNS) then begin
      Result := Copy(Result,Length(sXMLNS+':')+1,MaxInt);
    end else begin
      Result := Format('ns%d',[AWsdlDocument.DocumentElement.Attributes.{$IFNDEF FPC_211}Count{$ELSE}Length{$ENDIF}]) ;
      AWsdlDocument.DocumentElement.SetAttribute(Format('%s:%s',[sXMLNS,Result]),ANameSpace);
    end;
  end;

var
  typItm : TPasArrayType;
  propTypItm : TPasType;
  s, prop_ns_shortName : string;
  defTypesNode, defSchemaNode, cplxNode, sqcNode, propNode : TDOMElement;
  unitExternalName : string;
begin
  inherited;
  typItm := ASymbol as TPasArrayType;
  if not Assigned(typItm) then
    Exit;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,typItm);
    GetNameSpaceShortName(unitExternalName);
    defTypesNode := AWsdlDocument.DocumentElement.FindNode(sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    s := Format('%s:%s',[sXSD,sCOMPLEX_TYPE]);
    cplxNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    cplxNode.SetAttribute(sNAME, AContainer.GetExternalName(typItm)) ;

      s := Format('%s:%s',[sXSD,sSEQUENCE]);
      sqcNode := CreateElement(s,cplxNode,AWsdlDocument);
      propTypItm := typItm.ElType;
      s := Format('%s:%s',[sXSD,sELEMENT]);
      propNode := CreateElement(s,sqcNode,AWsdlDocument);
      propNode.SetAttribute(sNAME,sITEM);
      if Assigned(propTypItm) then begin
        prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(AContainer,propTypItm));//  AContainer.GetExternalName(propTypItm.Parent.Parent));
        propNode.SetAttribute(sTYPE,Format('%s:%s',[prop_ns_shortName,AContainer.GetExternalName(propTypItm)]));
        propNode.SetAttribute(sMIN_OCCURS,'0');
        propNode.SetAttribute(sMAX_OCCURS,sUNBOUNDED);
      end;
  end;
end;

class function TBaseArrayRemotable_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := inherited CanHandle(ASymbol) and ASymbol.InheritsFrom(TPasArrayType);
end;

{ TTypeDefinition_TypeHandler }

procedure TTypeDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        AWsdlDocument: TDOMDocument
);
begin
  Assert(ASymbol.InheritsFrom(TPasType));
end;

class function TTypeDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasType);
end;

procedure RegisterFondamentalTypes();
var
  r : IWsdlTypeHandlerRegistry;
begin
  r := GetWsdlTypeHandlerRegistry();
  r.Register(TEnumTypeHandler);
  r.Register(TClassTypeDefinition_TypeHandler);
  r.Register(TBaseArrayRemotable_TypeHandler);
  r.Register(TTypeAliasDefinition_TypeHandler);
end;

{ TTypeAliasDefinition_TypeHandler }

procedure TTypeAliasDefinition_TypeHandler.Generate(
        AContainer    : TwstPasTreeContainer;
  const ASymbol: TPasElement;
        AWsdlDocument: TDOMDocument
);
var
  typItm : TPasAliasType;
  ns_shortName, s : string;
  defTypesNode, defSchemaNode, resNode : TDOMElement;
  unitExternalName, baseUnitExternalName : string;
begin
  typItm := ASymbol as TPasAliasType;
  if Assigned(typItm) then begin
    unitExternalName := GetTypeNameSpace(AContainer,ASymbol);
    if FindAttributeByValueInNode(unitExternalName,AWsdlDocument.DocumentElement,ns_shortName) then begin
      ns_shortName := Copy(ns_shortName,Length(sXMLNS+':')+1,MaxInt);
    end else begin
      ns_shortName := Format('ns%d',[AWsdlDocument.DocumentElement.Attributes.{$IFNDEF FPC_211}Count{$ELSE}Length{$ENDIF}]) ;
      AWsdlDocument.DocumentElement.SetAttribute(Format('%s:%s',[sXMLNS,ns_shortName]),unitExternalName);
    end;
    defTypesNode := AWsdlDocument.DocumentElement.FindNode(sWSDL_TYPES) as TDOMElement;
    Assert(Assigned(defTypesNode));
    defSchemaNode := defTypesNode.FirstChild as TDOMElement;

    s := Format('%s:%s',[sXSD,sELEMENT]);
    resNode := CreateElement(s,defSchemaNode,AWsdlDocument);
    resNode.SetAttribute(sNAME, AContainer.GetExternalName(typItm)) ;

    baseUnitExternalName := GetTypeNameSpace(AContainer,typItm.DestType);
    s := GetNameSpaceShortName(baseUnitExternalName,AWsdlDocument);
    s := Format('%s:%s',[s,AContainer.GetExternalName(typItm.DestType)]);
    resNode.SetAttribute(sTYPE,s) ;
  end;
end;

class function TTypeAliasDefinition_TypeHandler.CanHandle(ASymbol: TObject): Boolean;
begin
  Result := Assigned(ASymbol) and ASymbol.InheritsFrom(TPasAliasType);
end;

initialization
  WsdlTypeHandlerRegistryInst := TWsdlTypeHandlerRegistry.Create() as IWsdlTypeHandlerRegistry;
  RegisterFondamentalTypes();

finalization
  WsdlTypeHandlerRegistryInst := nil;
  
end.
