{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufpropedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, Buttons, ComCtrls, StdCtrls,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfPropEdit }

  TfPropEdit = class(TForm)
    ActionList1: TActionList;
    actOK: TAction;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    edtOptional : TCheckBox;
    edtAttribute: TCheckBox;
    edtType: TComboBox;
    edtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FClassObject: TPasClassType;
    FUpdateType : TEditType;
    FObject : TPasProperty;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
    property ClassObject : TPasClassType read FClassObject;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject      : TPasProperty;
      const AUpdateType  : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ) : Boolean;
  end; 

var
  fPropEdit: TfPropEdit;

  function CreateProperty(AClass : TPasClassType; ASymboltable : TwstPasTreeContainer):TPasProperty ;
  function UpdateProperty(AProp : TPasProperty; ASymboltable : TwstPasTreeContainer):Boolean;
  
implementation
uses parserutils;

function CreateProperty(AClass : TPasClassType; ASymboltable : TwstPasTreeContainer):TPasProperty ;
var
  f : TfPropEdit;
begin
  Result := nil;
  f := TfPropEdit.Create(Application);
  try
    f.FClassObject := AClass;
    f.UpdateObject(Result,etCreate,ASymboltable);
  finally
    f.Release();
  end;
end;

function UpdateProperty(AProp : TPasProperty; ASymboltable : TwstPasTreeContainer):Boolean;
var
  f : TfPropEdit;
begin
  f := TfPropEdit.Create(Application);
  try
    Result := f.UpdateObject(AProp,etUpdate,ASymboltable);
  finally
    f.Release();
  end;
end;

{ TfPropEdit }

procedure TfPropEdit.actOKUpdate(Sender: TObject);
var
  internalName : string;
begin
  internalName := ExtractIdentifier(edtName.Text);
  TAction(Sender).Enabled :=
    ( not IsStrEmpty(internalName) ) and
    ( edtType.ItemIndex >= 0 ) and
    ( FindMember(ClassObject,internalName) = nil );
end;

procedure TfPropEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfPropEdit.LoadFromObject();
begin
  edtName.Text := '';
  edtType.Clear();
  edtType.Items.BeginUpdate();
  try
    edtType.Items.Clear();
    FillTypeList(edtType.Items,FSymbolTable);
  finally
    edtType.Items.EndUpdate();
  end;
  if Assigned(FObject) then begin
    Self.Caption := FSymbolTable.GetExternalName(FObject);
    edtName.Text := FSymbolTable.GetExternalName(FObject);
    edtType.ItemIndex := edtType.Items.IndexOfObject(FObject.VarType);
    edtAttribute.Checked := FSymbolTable.IsAttributeProperty(FObject);
    edtOptional.Checked := AnsiSameText('Has',Copy(FObject.StoredAccessorName,1,3)) ;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfPropEdit.SaveToObject();
var
  locObj : TPasProperty;
  typExtName, typIntName : string;
  propType : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  propType := edtType.Items.Objects[edtType.ItemIndex] as TPasType;
  if ( UpdateType = etCreate ) then begin
    locObj := TPasProperty(FSymbolTable.CreateElement(TPasProperty,typIntName,ClassObject,visPublished,'',0));
    FreeAndNil(FObject);
    FObject := locObj;
    locObj.VarType := propType;
    locObj.VarType.AddRef();
    ClassObject.Members.Add(FObject);
  end else begin
    locObj := FObject;
    if ( propType <> locObj.VarType ) then begin
      if ( locObj.VarType <> nil ) then
        locObj.VarType.Release();
      locObj.VarType := propType;
      locObj.VarType.AddRef();
    end;
    locObj.Name := typIntName;
  end;
  if edtOptional.Checked then
    locObj.StoredAccessorName := 'Has' + locObj.Name
  else
    locObj.StoredAccessorName := 'True';
  locObj.ReadAccessorName := 'F' + locObj.Name;
  locObj.WriteAccessorName := 'F' + locObj.Name;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  //if ( edtAttribute.Checked <> FSymbolTable.IsAttributeProperty(locObj) ) then
    FSymbolTable.SetPropertyAsAttribute(locObj,edtAttribute.Checked);
end;

function TfPropEdit.UpdateObject(
  var   AObject       : TPasProperty;
  const AUpdateType   : TEditType;
        ASymbolTable  : TwstPasTreeContainer
): Boolean;
begin
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  Result := ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
    if ( AUpdateType = etCreate ) then begin
      AObject := FObject;
    end;
  end;
end;

initialization
  {$I ufpropedit.lrs}

end.

