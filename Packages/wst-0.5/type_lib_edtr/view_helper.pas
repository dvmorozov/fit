{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit view_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  pastree, pascal_parser_intf;

type

  ISymbolPainter = interface
    ['{C13B3547-F338-43D7-8A44-2F81CC34A188}']
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;
  end;
  
  function FindPainter(AObj : TPasElement) : ISymbolPainter ;

  function SetCursorHourGlass():IInterface;

implementation
uses Contnrs, Controls, Forms;

const
  IMG_TABLE   = 0;
  IMG_TYPES   = 1;
  IMG_CONST   = 2;
  IMG_TYPE_DEF    = 4;
  IMG_INTF_DEF    = 5;
  //IMG_PROP_DEF    = 6;
  IMG_ENUM        = 6;
  IMG_CONST_ITEM  = 7;
  IMG_ENUM_ITEM   = 8;
  IMG_PROC_ITEM   = 9;
  IMG_BINDING_ITEM   = 10;

type

  { TCursorHolder }

  TCursorHolder = class(TInterfacedObject,IInterface)
  private
    FCursor : TCursor;
    FOldCursor : TCursor;
  public
    constructor Create(const ACursor : TCursor);
    destructor Destroy();override;
  end;

function SetCursorHourGlass():IInterface;
begin
  Result := TCursorHolder.Create(crHourGlass);
  Application.ProcessMessages();
end;

function AddChildNode(AParent: TTreeNode; const AText : string):TTreeNode ;
begin
  Result := AParent.TreeNodes.AddChild(AParent,AText);
  Result.ImageIndex := -1;
  Result.StateIndex := -1;
  Result.SelectedIndex := -1;
end;

{ TCursorHolder }

constructor TCursorHolder.Create(const ACursor: TCursor);
begin
  FCursor := ACursor;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := FCursor;
end;

destructor TCursorHolder.Destroy();
begin
  Screen.Cursor := FOldCursor;
  inherited Destroy();
end;

type

  { TSymbolPainter }

  TSymbolPainter = class(TInterfacedObject,ISymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;virtual;abstract;
  public
    constructor Create();virtual;
    //class function CanHandle(AObjClass : TClass):Boolean;overload;virtual;abstract;
    class function CanHandle(AObj : TObject):Boolean;overload;virtual;abstract;
  end;
  
  TSymbolPainterClass = class of TSymbolPainter;
  
  { TPainterRegistry }

  TPainterRegistry = class
  private
    FList : TClassList;
  private
    function FindHanlderIndex(AObj : TObject):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterHandler(APainterClass : TSymbolPainterClass);
    function FindHandler(AObj : TObject; out AHandler : ISymbolPainter) : Boolean;
  end;

var
  FPainterRegistryInst : TPainterRegistry;
  
type

  { TAbstractSymbolPainter }

  TAbstractSymbolPainter = class(TSymbolPainter,ISymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    //class function CanHandle(AObjClass : TClass):Boolean;overload;override;
    class function CanHandle(AObj : TObject):Boolean;overload;override;
  end;
  
  { TPackagePainter }

  TPackagePainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TModulePainter }

  TModulePainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TAbstractConstantDefinitionPainter }

  TAbstractConstantDefinitionPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TTypeSymbolPainter }

  TTypeSymbolPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  end;

  { TAnyTypeDefinitionPainter }

  TAnyTypeDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TTypeAliasDefinitionPainter }

  TTypeAliasDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TEnumTypeDefinitionPainter }

  TEnumTypeDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TClassTypeDefinitionPainter }

  TClassTypeDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TArrayTypeDefinitionPainter }

  TArrayTypeDefinitionPainter = class(TTypeSymbolPainter)
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TMethodDefinitionPainter }

  TMethodDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TInterfaceDefinitionPainter }

  TInterfaceDefinitionPainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

  { TPasNativeSimpleTypePainter }

  TPasNativeSimpleTypePainter = class(TTypeSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;
  
  { TBindingPainter }

  TBindingPainter = class(TAbstractSymbolPainter)
  protected
    function Paint(
      AContainer : TwstPasTreeContainer;
      AObj    : TPasElement;
      AParent : TTreeNode
    ):TTreeNode;override;
  public
    class function CanHandle(AObj : TObject):Boolean;override;
  end;

{ TArrayTypeDefinitionPainter }

class function TArrayTypeDefinitionPainter.CanHandle(AObj : TObject) : Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasArrayType);
end;

{ TBindingPainter }

function TBindingPainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TwstBinding;
begin
  locObj := TwstBinding(AObj);
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_BINDING_ITEM;
  Result.StateIndex := IMG_BINDING_ITEM;
  Result.SelectedIndex := IMG_BINDING_ITEM;
    AddChildNode(Result,BindingStyleNames[locObj.BindingStyle]);
    AddChildNode(Result,locObj.Address);
end;

class function TBindingPainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TwstBinding);
end;

{ TPasNativeSimpleTypePainter }

function TPasNativeSimpleTypePainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasNativeSimpleType;
  boxeNode : TTreeNode;
begin
  locObj := TPasNativeSimpleType(AObj);
  Result := inherited Paint(AContainer, locObj, AParent);
  {if ( locObj.BoxedType <> nil ) then begin
    boxeNode := AddChildNode(Result,locObj.BoxedType.Name);
    boxeNode.Data := locObj.BoxedType;
    boxeNode.ImageIndex := -1;
    boxeNode.StateIndex := -1;
    boxeNode.SelectedIndex := -1;
  end;}
end;

class function TPasNativeSimpleTypePainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasNativeSimpleType);
end;
  
{ TModulePainter }

function TModulePainter.Paint(
  AContainer: TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  i , c: Integer;
  locObj : TPasModule;
  objPtr : ISymbolPainter;
  {constNode,} typNode, intfNode : TTreeNode;
  objItm : TPasElement;
  decList : TList;
begin
  locObj := AObj as TPasModule;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_TABLE;
  Result.StateIndex := IMG_TABLE;
  Result.SelectedIndex := IMG_TABLE;
  {constNode := AddChildNode(Result,'Const');
  constNode.ImageIndex := IMG_CONST;
  constNode.StateIndex := IMG_CONST;
  constNode.SelectedIndex := IMG_CONST;}
  typNode := AddChildNode(Result,'Type');
  typNode.ImageIndex := IMG_TYPES;
  typNode.StateIndex := IMG_TYPES;
  typNode.SelectedIndex := IMG_TYPES;
  intfNode := AddChildNode(Result,'Interface');
  decList := locObj.InterfaceSection.Declarations;
  c := decList.Count;
  for i := 0 to Pred(c) do begin
    objItm := TPasElement(decList[i]);
    objPtr := FindPainter(objItm) ;
    if Assigned(objPtr) then begin
      if objItm.InheritsFrom(TPasClassType) and ( TPasClassType(objItm).ObjKind = okInterface ) then
        objPtr.Paint(AContainer,objItm,intfNode)
      else
        objPtr.Paint(AContainer,objItm,typNode);
    end;
  end;
end;

class function TModulePainter.CanHandle(AObj: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasModule);
end;

{ TMethodDefinitionPainter }

function TMethodDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  j : Integer;
  ss : string;
  pmr : TPasArgument;
  locMthd : TPasProcedure;
  memberList : TList;
begin
  locMthd := AObj as TPasProcedure;
  Result := AddChildNode(AParent,AContainer.GetExternalName(locMthd));
  Result.Data := locMthd;
  Result.ImageIndex := IMG_PROC_ITEM;
  Result.StateIndex := IMG_PROC_ITEM;
  Result.SelectedIndex := IMG_PROC_ITEM;
  memberList := locMthd.ProcType.Args;
  for j := 0 to Pred(memberList.Count) do begin
    pmr := TPasArgument(memberList[j]);
    ss := AccessNames[pmr.Access];
    if ( Length(ss) > 0 ) then begin
      ss := ss + ' ' + AContainer.GetExternalName(pmr);
    end;
    AddChildNode(Result,ss);
  end;
  if locMthd.InheritsFrom(TPasFunction) then begin
    AddChildNode(
      Result,
      '>> ' + AContainer.GetExternalName(TPasFunctionType(locMthd.ProcType).ResultEl)
    );
  end;
end;

class function TMethodDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasProcedure);
end;

{ TInterfaceDefinitionPainter }

function TInterfaceDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj : TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasClassType;
  locMthd : TPasProcedure;
  i : Integer;
  memberList : TList;
  bindingsNode : TTreeNode;
  b : TwstBinding;
begin
  locObj := AObj as TPasClassType;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_INTF_DEF;
  Result.StateIndex := IMG_INTF_DEF;
  Result.SelectedIndex := IMG_INTF_DEF;
  memberList := locObj.Members;
  for i := 0 to Pred(memberList.Count) do begin
    if TPasElement(memberList[i]).InheritsFrom(TPasProcedure) then begin
      locMthd := TPasProcedure(memberList[i]);
      FindPainter(locMthd).Paint(AContainer,locMthd,Result);
    end;
  end;
  i := 0;
  bindingsNode := AddChildNode(Result,'Bindings >');
    while True do begin
      b := AContainer.FindBinding(locObj,i);
      if ( b = nil ) then
        Break;
      Inc(i);
      FindPainter(b).Paint(AContainer,b,bindingsNode);
    end;
end;

class function TInterfaceDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and
            AObj.InheritsFrom(TPasClassType) and
            ( TPasClassType(AObj).ObjKind = okInterface );
end;
  
{ TAbstractConstantDefinitionPainter }

function TAbstractConstantDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
begin
  Result := inherited Paint(AContainer,AObj, AParent);
  Result.ImageIndex := IMG_CONST_ITEM;
  Result.StateIndex := IMG_CONST_ITEM;
  Result.SelectedIndex := IMG_CONST_ITEM;
end;

class function TAbstractConstantDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasConst);
end;
  
{ TTypeSymbolPainter }

function TTypeSymbolPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
begin
  Result := inherited Paint(AContainer, AObj, AParent);
  Result.ImageIndex := IMG_TYPE_DEF;
  Result.StateIndex := IMG_TYPE_DEF;
  Result.SelectedIndex := IMG_TYPE_DEF;
end;

{ TClassTypeDefinitionPainter }

function TClassTypeDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj: TPasElement;
  AParent: TTreeNode
): TTreeNode;
var
  locObj : TPasClassType;
  locProp : TPasProperty;
  i : Integer;
  s : string;
begin
  locObj := AObj as TPasClassType;
  Result := inherited Paint(AContainer,locObj, AParent);
  if Assigned(locObj.AncestorType) then begin
    Result.Text := Format('%s (%s)',[AContainer.GetExternalName(locObj),AContainer.GetExternalName(locObj.AncestorType)]);
  end;
  for i := 0 to Pred(locObj.Members.Count) do begin
    if TPasElement(locObj.Members[i]).InheritsFrom(TPasProperty) then begin
      locProp := TPasProperty(locObj.Members[i]);
      s := Format('%s : %s',[AContainer.GetExternalName(locProp),AContainer.GetExternalName(locProp.VarType)]);
      if AContainer.IsAttributeProperty(locProp) then begin
        s := s + ' ( Attribute )';
      end;
      AddChildNode(Result,s);
    end;
  end;
end;

class function TClassTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and
            ( AObj.InheritsFrom(TPasClassType) and ( TPasClassType(AObj).ObjKind = okClass ) ) and
            ( not AObj.InheritsFrom(TPasNativeClassType) );
end;
  
{ TEnumTypeDefinitionPainter }

function TEnumTypeDefinitionPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
): TTreeNode;
var
  locObj : TPasEnumType;
  locItem : TPasEnumValue;
  i : Integer;
  locNode : TTreeNode;
begin
  locObj := AObj as TPasEnumType;
  Result := inherited Paint(AContainer, locObj, AParent);
  Result.ImageIndex := IMG_ENUM;
  Result.StateIndex := IMG_ENUM;
  Result.SelectedIndex := IMG_ENUM;
  for i := 0 to Pred(locObj.Values.Count) do begin
    locItem := TPasEnumValue(locObj.Values[i]);
    locNode := AddChildNode(Result,AContainer.GetExternalName(locItem));
    locNode.ImageIndex := IMG_ENUM_ITEM;
    locNode.StateIndex := IMG_ENUM_ITEM;
    locNode.SelectedIndex := IMG_ENUM_ITEM;
  end;
end;

class function TEnumTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasEnumType);
end;
  
  
{ TTypeAliasDefinitionPainter }

class function TTypeAliasDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasAliasType);
end;
  
{ TAnyTypeDefinitionPainter }

class function TAnyTypeDefinitionPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasUnresolvedTypeRef);
end;
  
{ TPackagePainter }

function TPackagePainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
):TTreeNode;
var
  objPtr : ISymbolPainter;
begin
  Result := AParent;
  objPtr := FindPainter(AContainer.CurrentModule) ;
  if Assigned(objPtr) then begin
    objPtr.Paint(AContainer,AContainer.CurrentModule,Result);
  end;
end;

class function TPackagePainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObj) ) and AObj.InheritsFrom(TPasPackage);
end;

function FindPainter(AObj: TPasElement): ISymbolPainter;
begin
  Result := nil;
  if Assigned(AObj) then begin
    FPainterRegistryInst.FindHandler(AObj,Result);
  end;
end;

{ TPainterRegistry }

function TPainterRegistry.FindHanlderIndex(AObj: TObject): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(FList.Count) do begin
    if TSymbolPainterClass(FList[i]).CanHandle(AObj) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TPainterRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TPainterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TPainterRegistry.RegisterHandler(APainterClass: TSymbolPainterClass);
begin
  if ( FList.IndexOf(APainterClass) = -1 ) then begin
    FList.Add(APainterClass);
  end;
end;

function TPainterRegistry.FindHandler(AObj: TObject; out AHandler: ISymbolPainter): Boolean;
var
  i : Integer;
begin
  AHandler := nil;
  i := FindHanlderIndex(AObj);
  Result := ( i >= 0 );
  if Result then begin
    AHandler := TSymbolPainterClass(FList[i]).Create();
  end;
end;

function TAbstractSymbolPainter.Paint(
  AContainer : TwstPasTreeContainer;
  AObj    : TPasElement;
  AParent : TTreeNode
):TTreeNode;
begin
  Assert(Assigned(AParent));
  if Assigned(AObj) then begin
    Result := AddChildNode(AParent,AContainer.GetExternalName(AObj));
    Result.Data := AObj;
    Result.ImageIndex := -1;
    Result.StateIndex := -1;
    Result.SelectedIndex := -1;
  end;
end;

class function TAbstractSymbolPainter.CanHandle(AObj : TObject): Boolean;
begin
  Result := Assigned(AObj) and AObj.InheritsFrom(TPasElement);
end;

{ TSymbolPainter }

constructor TSymbolPainter.Create();
begin

end;



initialization
  FPainterRegistryInst := TPainterRegistry.Create();
  FPainterRegistryInst.RegisterHandler(TPackagePainter);
  FPainterRegistryInst.RegisterHandler(TModulePainter);
  FPainterRegistryInst.RegisterHandler(TAnyTypeDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TTypeAliasDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TEnumTypeDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TClassTypeDefinitionPainter);
  //FPainterRegistryInst.RegisterHandler(TAbstractConstantDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TInterfaceDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TMethodDefinitionPainter);
  FPainterRegistryInst.RegisterHandler(TPasNativeSimpleTypePainter);
  FPainterRegistryInst.RegisterHandler(TBindingPainter);
  FPainterRegistryInst.RegisterHandler(TArrayTypeDefinitionPainter);

finalization
  FreeAndNil(FPainterRegistryInst);
  
end.
