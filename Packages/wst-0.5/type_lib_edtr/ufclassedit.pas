{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufclassedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, ComCtrls, Buttons, StdCtrls,
  pastree, pascal_parser_intf,
  edit_helper, Menus;

type

  { TfClassEdit }

  TfClassEdit = class(TForm)
    actPropDelete: TAction;
    actPropEdit: TAction;
    actPropAdd: TAction;
    ActionList1: TActionList;
    actOK: TAction;
    actOK1: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    edtParent: TComboBox;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtProp: TListView;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    PC: TPageControl;
    PopupMenu1: TPopupMenu;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure actPropAddExecute(Sender: TObject);
    procedure actPropDeleteExecute(Sender: TObject);
    procedure actPropEditExecute(Sender: TObject);
    procedure actPropEditUpdate(Sender: TObject);
    procedure edtPropDblClick(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasClassType;
    FSymbolTable : TwstPasTreeContainer;
    FOldAncestor : TPasType;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure PrepareParentCombo();
    procedure LoadProperty(APropDef : TPasProperty);
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasClassType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end;

var
  fClassEdit: TfClassEdit;

implementation
uses parserutils, ufpropedit, common_gui_utils;


{ TfClassEdit }

procedure TfClassEdit.actPropAddExecute(Sender: TObject);
var
  prp : TPasProperty;
begin
  prp := CreateProperty(FObject,FSymbolTable);
  if Assigned(prp) then begin
    LoadProperty(prp);
  end;
end;

procedure TfClassEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfClassEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfClassEdit.actPropDeleteExecute(Sender: TObject);
var
  prop : TPasProperty;
begin
  prop := TPasProperty(edtProp.ItemFocused.Data);
  FObject.Members.Extract(prop);
  prop.Release();
  edtProp.ItemFocused.Free();
end;

procedure TfClassEdit.actPropEditExecute(Sender: TObject);
var
  prp : TPasProperty;
  itm : TListItem;
begin
  itm := edtProp.ItemFocused;
  if Assigned(itm) then begin
    prp := TPasProperty(itm.Data);
    if UpdateProperty(prp,FSymbolTable) then begin
      itm.Free();
      LoadProperty(prp);
    end;
  end;
end;

procedure TfClassEdit.actPropEditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(edtProp.ItemFocused);
end;

procedure TfClassEdit.edtPropDblClick(Sender: TObject);
begin
  if actPropEdit.Enabled then begin
    actPropEdit.Execute();
  end else if actPropAdd.Enabled then begin
    actPropAdd.Execute();
  end;
end;

procedure InternalFillList(
  ALs : TStrings;
  AContainer : TwstPasTreeContainer
);
var
  i, j : Integer;
  sym : TPasElement;
  modulList, decList : TList;
  mdl : TPasModule;
begin
  modulList := AContainer.Package.Modules;
  for i := 0 to Pred(modulList.Count) do begin
    mdl := TPasModule(modulList[i]);
    decList := mdl.InterfaceSection.Declarations;
    for j := 0 to Pred(decList.Count) do begin
      sym := TPasElement(decList[j]);
      if sym.InheritsFrom(TPasType) and
         ( sym.InheritsFrom(TPasClassType) or
           sym.InheritsFrom(TPasNativeSimpleContentClassType) or
           ( sym.InheritsFrom(TPasAliasType) and
             Assigned(TPasAliasType(sym).DestType) and
             ( TPasAliasType(sym).DestType.InheritsFrom(TPasClassType) or
               TPasAliasType(sym).DestType.InheritsFrom(TPasNativeSimpleType)
             )
           )
         )  and
        ( not sym.InheritsFrom(TPasNativeSimpleType) )
      then begin
        if ( ALs.IndexOfObject(sym) = -1 ) then begin
          ALs.AddObject(AContainer.GetExternalName(sym),sym);
        end;
      end;
    end;
  end;
end;

procedure FillList(
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
    InternalFillList(locLST,ASymbol);
    locLST.Sort();
    ALs.Assign(locLST);
  finally
    FreeAndNil(locLST);
  end;
end;

procedure TfClassEdit.PrepareParentCombo();
begin
  edtParent.Items.BeginUpdate();
  try
    FillList(edtParent.Items,FSymbolTable);
  finally
    edtParent.Items.EndUpdate();
  end;
end;

procedure TfClassEdit.LoadProperty(APropDef: TPasProperty);
var
  itm : TListItem;
  s, extName : string;
begin
  extName := FSymbolTable.GetExternalName(APropDef);
  itm := FindItem(extName,edtProp.Items);
  if ( itm = nil ) then begin
    itm := edtProp.Items.Add();
  end;
  itm.Caption := extName;
  itm.SubItems.Add(FSymbolTable.GetExternalName(APropDef.VarType));
  if FSymbolTable.IsAttributeProperty(APropDef) then begin
    s := 'Y';
  end else begin
    s := 'N';
  end;
  itm.SubItems.Add(s);
  itm.Data := APropDef;
end;

procedure TfClassEdit.LoadFromObject();
var
  i : Integer;
  prp : TPasProperty;
  extName : string;
begin
  edtName.Text := '';
  edtProp.Clear();
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    for i := 0 to Pred(FObject.Members.Count) do begin
      if TPasElement(FObject.Members[i]).InheritsFrom(TPasProperty) then begin
        prp := TPasProperty(FObject.Members[i]);
        LoadProperty(prp);
      end;
    end;
    if Assigned(FObject.AncestorType) then begin
      edtParent.ItemIndex := edtParent.Items.IndexOfObject(FObject.AncestorType);
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfClassEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasClassType;
  trueParent : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  if ( edtParent.ItemIndex >= 0 ) then begin
    trueParent := edtParent.Items.Objects[edtParent.ItemIndex] as TPasType;
    if trueParent.InheritsFrom(TPasAliasType) then begin
      trueParent := GetUltimeType(trueParent);
    end;
    if trueParent.InheritsFrom(TPasNativeSimpleType) and
       Assigned(TPasNativeSimpleType(trueParent).BoxedType)
    then begin
      trueParent := TPasNativeSimpleType(trueParent).BoxedType;
    end;
  end else begin
    trueParent := nil;
  end;
  if ( trueParent <> FOldAncestor ) then begin
    if ( FOldAncestor <> nil ) then
      FOldAncestor.Release();
    locObj.AncestorType := trueParent;
    locObj.AncestorType.AddRef();
  end;
end;

function TfClassEdit.UpdateObject(
  var   AObject     : TPasClassType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
): Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    FObject := TPasClassType(FSymbolTable.CreateElement(TPasClassType,'new_class',FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FObject.ObjKind := okClass;
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(FObject);
  end;
  FOldAncestor := FObject.AncestorType;
  try
    PrepareParentCombo();
    LoadFromObject();
    Result := ( ShowModal() = mrOK );
    if Result then begin
      try
        SaveToObject();
        if ( AUpdateType = etCreate ) then begin
          AObject := FObject;
        end;
      except
        Result := False;
        raise;
      end;
    end;
  finally
    if ( not Result ) and ( UpdateType = etCreate ) and ( AObject = nil ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Classes.Extract(FObject);
      FObject.Release();
      FObject := nil;
    end;
  end;
end;

initialization
  {$I ufclassedit.lrs}

end.

