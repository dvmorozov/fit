{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit edit_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  pastree, pascal_parser_intf;

type

  TEditType = ( etCreate, etUpdate, etDelete );
  
  { TObjectUpdater }

  TObjectUpdater = class
  public
    class function CanHandle(AObject : TObject):Boolean;virtual;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;virtual;abstract;
    class procedure DeleteObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    );virtual;
  end;
  TObjectUpdaterClass = class of TObjectUpdater;
  
  function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
  function CreateCompoundObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  function CreateArray(ASymbolTable : TwstPasTreeContainer) : TPasArrayType;
  function CreateInterface(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
  function CreateMethod(
    AOwner       : TPasClassType;
    ASymbolTable : TwstPasTreeContainer
  ) : TPasProcedure;
  function CreateArgument(
    AOwner       : TPasProcedureType;
    ASymbolTable : TwstPasTreeContainer
  ) : TPasArgument;
  function CreateAliasType(ASymbolTable : TwstPasTreeContainer) : TPasAliasType;



  function HasEditor(AObject : TPasElement):Boolean;
  function UpdateObject(
    AObject : TPasElement;
    ASymbolTable : TwstPasTreeContainer
  ):Boolean;
  procedure DeleteObject(
    AObject : TPasElement;
    ASymbolTable : TwstPasTreeContainer
  );
  
  procedure FillList(ALs : TStrings;AContainer : TwstPasTreeContainer);
  procedure FillTypeList(
    ALs : TStrings;
    ASymbol : TwstPasTreeContainer
  );


implementation
uses Contnrs, Forms, ufEnumedit, ufclassedit, uinterfaceedit, uprocedit,
     uargedit, umoduleedit, ubindingedit, ufarrayedit, uftypealiasedit;

type

  { TUpdaterRegistry }

  TUpdaterRegistry = class
  private
    FList : TClassList;
  private
    function FindHanlderIndex(AObj : TObject):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterHandler(AHandlerClass : TObjectUpdaterClass);
    function FindHandler(AObj : TObject; out AHandler : TObjectUpdaterClass) : Boolean;
  end;

var UpdaterRegistryInst : TUpdaterRegistry;

function CreateInterface(ASymbolTable: TwstPasTreeContainer): TPasClassType;
var
  f : TfInterfaceEdit;
begin
  Result := nil;
  f := TfInterfaceEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function HasEditor(AObject: TPasElement): Boolean;
var
  h : TObjectUpdaterClass;
begin
  Result := UpdaterRegistryInst.FindHandler(AObject,h);
end;

function UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
):Boolean;
var
  h : TObjectUpdaterClass;
begin
  if not UpdaterRegistryInst.FindHandler(AObject,h) then begin
    raise Exception.Create('No handler found.');
  end;
  Result := h.UpdateObject(AObject,ASymbolTable);
end;

procedure DeleteObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  h : TObjectUpdaterClass;
begin
  if not UpdaterRegistryInst.FindHandler(AObject,h) then begin
    raise Exception.Create('No handler found.');
  end;
  h.DeleteObject(AObject,ASymbolTable);
end;

type
  { TEnumUpdater }

  TEnumUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TClassUpdater }

  TClassUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TTypeAliasUpdater }

  TTypeAliasUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;
  
  { TArrayUpdater }

  TArrayUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;
  
  { TInterfaceUpdater }

  TInterfaceUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TMethodUpdater }

  TMethodUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TArgumentUpdater }

  TArgumentUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TModuleUpdater }

  TModuleUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

  { TBindingUpdater }

  TBindingUpdater = class(TObjectUpdater)
  public
    class function CanHandle(AObject : TObject):Boolean;override;
    class function UpdateObject(
      AObject : TPasElement;
      ASymbolTable : TwstPasTreeContainer
    ):Boolean;override;
  end;

{ TTypeAliasUpdater }

class function TTypeAliasUpdater.CanHandle(AObject : TObject) : Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and AObject.InheritsFrom(TPasAliasType);
end;

class function TTypeAliasUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfTypeAliasEdit;
  e : TPasAliasType;
begin
  e := AObject as TPasAliasType;
  f := TfTypeAliasEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TArrayUpdater }

class function TArrayUpdater.CanHandle(AObject : TObject) : Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and AObject.InheritsFrom(TPasArrayType);
end;

class function TArrayUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfArrayEdit;
  e : TPasArrayType;
begin
  e := AObject as TPasArrayType;
  f := TfArrayEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TBindingUpdater }

class function TBindingUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and  AObject.InheritsFrom(TwstBinding);
end;

class function TBindingUpdater.UpdateObject(
  AObject: TPasElement;
  ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfBindingEdit;
  e : TwstBinding;
begin
  e := AObject as TwstBinding;
  f := TfBindingEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TModuleUpdater }

class function TModuleUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and  AObject.InheritsFrom(TPasModule);
end;

class function TModuleUpdater.UpdateObject(
  AObject: TPasElement;
  ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfModuleEdit;
  e : TPasModule;
begin
  e := AObject as TPasModule;
  f := TfModuleEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TArgumentUpdater }

class function TArgumentUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and  AObject.InheritsFrom(TPasArgument);
end;

class function TArgumentUpdater.UpdateObject(
  AObject      : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfArgEdit;
  e : TPasArgument;
begin
  e := AObject as TPasArgument;
  f := TfArgEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TMethodUpdater }

class function TMethodUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and  AObject.InheritsFrom(TPasProcedure);
end;

class function TMethodUpdater.UpdateObject(
  AObject: TPasElement;
  ASymbolTable: TwstPasTreeContainer
): Boolean;
var
  f : TfProcEdit;
  e : TPasProcedure;
begin
  e := AObject as TPasProcedure;
  f := TfProcEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TInterfaceUpdater }

class function TInterfaceUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okInterface ) );
end;

class function TInterfaceUpdater.UpdateObject(AObject: TPasElement; ASymbolTable: TwstPasTreeContainer): Boolean;
var
  f : TfInterfaceEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfInterfaceEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TClassUpdater }

class function TClassUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and
            ( AObject.InheritsFrom(TPasClassType) and ( TPasClassType(AObject).ObjKind = okClass ) );
end;

class function TClassUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfClassEdit;
  e : TPasClassType;
begin
  e := AObject as TPasClassType;
  f := TfClassEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;
  
{ TUpdaterRegistry }

function TUpdaterRegistry.FindHanlderIndex(AObj : TObject): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(FList.Count) do begin
    if TObjectUpdaterClass(FList[i]).CanHandle(AObj) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TUpdaterRegistry.Create();
begin
  FList := TClassList.Create();
end;

destructor TUpdaterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TUpdaterRegistry.RegisterHandler(AHandlerClass : TObjectUpdaterClass);
begin
  if ( FList.IndexOf(AHandlerClass) < 0 ) then begin
    FList.Add(AHandlerClass);
  end;
end;

function TUpdaterRegistry.FindHandler(
      AObj      : TObject;
  out AHandler  : TObjectUpdaterClass
): Boolean;
var
  i : Integer;
begin
  AHandler := nil;
  i := FindHanlderIndex(AObj);
  Result := ( i >= 0 );
  if Result then begin
    AHandler := TObjectUpdaterClass(FList[i]);
  end;
end;

{ TEnumUpdater }

class function TEnumUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := ( inherited CanHandle(AObject) ) and AObject.InheritsFrom(TPasEnumType);
end;

class function TEnumUpdater.UpdateObject(
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  f : TfEnumEdit;
  e : TPasEnumType;
begin
  e := AObject as TPasEnumType;
  f := TfEnumEdit.Create(Application);
  try
    Result := f.UpdateObject(e,etUpdate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateEnum(AContainer : TwstPasTreeContainer) : TPasEnumType;
var
  f : TfEnumEdit;
begin
  Result := nil;
  f := TfEnumEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,AContainer);
  finally
    f.Release();
  end;
end;

function CreateCompoundObject(ASymbolTable : TwstPasTreeContainer) : TPasClassType;
var
  f : TfClassEdit;
begin
  Result := nil;
  f := TfClassEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateArray(ASymbolTable : TwstPasTreeContainer) : TPasArrayType;
var
  f : TfArrayEdit;
begin
  Result := nil;
  f := TfArrayEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateMethod(
  AOwner       : TPasClassType;
  ASymbolTable : TwstPasTreeContainer
) : TPasProcedure;
var
  f : TfProcEdit;
begin
  Result := TPasProcedure(ASymbolTable.CreateElement(TPasProcedure,'new_proc',AOwner,visPublic,'',0));
  Result.ProcType := TPasProcedureType(ASymbolTable.CreateElement(TPasProcedureType,'',Result,visDefault,'',0));
  AOwner.Members.Add(Result);
  f := TfProcEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateArgument(
  AOwner       : TPasProcedureType;
  ASymbolTable : TwstPasTreeContainer
) : TPasArgument;
var
  f : TfArgEdit;
begin
  Result := TPasArgument(ASymbolTable.CreateElement(TPasArgument,'AValue',AOwner,visPublic,'',0));
  Result.ArgType := ASymbolTable.FindElement('string') as TPasType;
  f := TfArgEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

function CreateAliasType(ASymbolTable : TwstPasTreeContainer) : TPasAliasType;
var
  f : TfTypeAliasEdit;
begin
  Result := nil;
  f := TfTypeAliasEdit.Create(Application);
  try
    f.UpdateObject(Result,etCreate,ASymbolTable);
  finally
    f.Release();
  end;
end;

{ TObjectUpdater }

class function TObjectUpdater.CanHandle(AObject: TObject): Boolean;
begin
  Result := Assigned(AObject);
end;

class procedure TObjectUpdater.DeleteObject (
  AObject : TPasElement;
  ASymbolTable : TwstPasTreeContainer
);
var
  sct : TPasSection;
begin
  if ( AObject <> nil ) then begin
    sct := ASymbolTable.CurrentModule.InterfaceSection;
    sct.Declarations.Extract(AObject);
    sct.Types.Extract(AObject);
    sct.Classes.Extract(AObject);
    AObject.Release();
  end;
end;

procedure InternalFillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  i : Integer;
  sym : TPasElement;
  decList : TList;
begin
  decList := AContainer.CurrentModule.InterfaceSection.Declarations;
  for i := 0 to Pred(decList.Count) do begin
    sym := TPasElement(decList[i]);
    if sym.InheritsFrom(TPasType) and
       ( sym.InheritsFrom(TPasClassType) or
         sym.InheritsFrom(TPasNativeSimpleType) or
         ( sym.InheritsFrom(TPasAliasType) and
           Assigned(TPasAliasType(sym).DestType) and
           ( TPasAliasType(sym).DestType.InheritsFrom(TPasClassType) or
             TPasAliasType(sym).DestType.InheritsFrom(TPasNativeSimpleType)
           )
         )
       ) and
       ( not sym.InheritsFrom(TPasNativeSimpleContentClassType) )
    then begin
      if ( ALs.IndexOfObject(sym) = -1 ) then begin
        ALs.AddObject(AContainer.GetExternalName(sym),sym);
      end;
    end;
  end;
end;

procedure FillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillList(locLST,AContainer);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

procedure InternalFillTypeList(ALs : TStrings; AContainer : TwstPasTreeContainer);
var
  i, j : Integer;
  sym : TPasElement;
  moduleList, decList : TList;
  mdl : TPasModule;
begin
  moduleList := AContainer.Package.Modules;
  for i := 0 to Pred(moduleList.Count) do begin
    mdl := TPasModule(moduleList[i]);
    decList := mdl.InterfaceSection.Declarations;
    for j := 0 to Pred(decList.Count) do begin
      sym := TPasElement(decList[j]);
      if sym.InheritsFrom(TPasType) and ( not sym.InheritsFrom(TPasNativeSimpleContentClassType) ) then begin
        if ( ALs.IndexOfObject(sym) = -1 ) then begin
          ALs.AddObject(AContainer.GetExternalName(sym),sym);
        end;
      end;
    end;
  end;
end;

procedure FillTypeList(
  ALs : TStrings;
  ASymbol : TwstPasTreeContainer
);
var
  locLST : TStringList;
begin
  locLST := TStringList.Create();
  try
    locLST.Assign(ALs);
    locLST.Duplicates := dupAccept;
    InternalFillTypeList(locLST,ASymbol);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

initialization
  UpdaterRegistryInst := TUpdaterRegistry.Create();
  UpdaterRegistryInst.RegisterHandler(TEnumUpdater);
  UpdaterRegistryInst.RegisterHandler(TClassUpdater);
  UpdaterRegistryInst.RegisterHandler(TInterfaceUpdater);
  UpdaterRegistryInst.RegisterHandler(TMethodUpdater);
  UpdaterRegistryInst.RegisterHandler(TArgumentUpdater);
  UpdaterRegistryInst.RegisterHandler(TModuleUpdater);
  UpdaterRegistryInst.RegisterHandler(TBindingUpdater);
  UpdaterRegistryInst.RegisterHandler(TArrayUpdater);
  UpdaterRegistryInst.RegisterHandler(TTypeAliasUpdater);
  
finalization
  FreeAndNil(UpdaterRegistryInst);
  
end.
