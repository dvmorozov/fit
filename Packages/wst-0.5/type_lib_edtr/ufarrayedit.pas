unit ufarrayedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Buttons,
  pastree, pascal_parser_intf, edit_helper;

type

  { TfArrayEdit }

  TfArrayEdit = class(TForm)
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    edtEmbedded : TCheckBox;
    edtElementName : TEdit;
    edtElementType : TComboBox;
    edtName : TEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    TabSheet1 : TTabSheet;
    procedure actOKExecute(Sender : TObject);
    procedure actOKUpdate(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasArrayType;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject      : TPasArrayType;
      const AUpdateType  : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ) : Boolean;
  end;

var
  fArrayEdit : TfArrayEdit;

implementation
uses parserutils;

{ TfArrayEdit }

procedure TfArrayEdit.actOKUpdate(Sender : TObject);
var
  internalName : string;
begin
  internalName := ExtractIdentifier(edtName.Text);
  TAction(Sender).Enabled :=
    ( not IsStrEmpty(internalName) ) and
    ( not IsStrEmpty(ExtractIdentifier(edtElementName.Text)) ) and
    ( edtElementType.ItemIndex >= 0 ) ;
end;

procedure TfArrayEdit.actOKExecute(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TfArrayEdit.LoadFromObject();
begin
  edtElementType.Clear();
  edtElementType.Items.BeginUpdate();
  try
    edtElementType.Items.Clear();
    FillTypeList(edtElementType.Items,FSymbolTable);
  finally
    edtElementType.Items.EndUpdate();
  end;
  if Assigned(FObject) then begin
    Self.Caption := FSymbolTable.GetExternalName(FObject);
    edtName.Text := FSymbolTable.GetExternalName(FObject);
    edtElementName.Text := FSymbolTable.GetArrayItemExternalName(FObject);
    edtElementType.ItemIndex := edtElementType.Items.IndexOf(FSymbolTable.GetExternalName(FObject.ElType));
    edtEmbedded.Checked := ( FSymbolTable.GetArrayStyle(FObject) = asEmbeded );
  end else begin
    Self.Caption := 'NewArray';
  end;
end;

procedure TfArrayEdit.SaveToObject();
var
  locObj : TPasArrayType;
  typExtName, typIntName : string;
  eltExtName, eltIntName : string;
  eltType : TPasType;
  arrStyle : TArrayStyle;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  eltExtName := ExtractIdentifier(edtElementName.Text);
  eltIntName := MakeInternalSymbolNameFrom(eltExtName);
  if edtEmbedded.Checked then
    arrStyle := asEmbeded
  else
    arrStyle := asScoped;
  eltType := edtElementType.Items.Objects[edtElementType.ItemIndex] as TPasType;
  if ( UpdateType = etCreate ) then begin
    locObj := FSymbolTable.CreateArray(typIntName,eltType,eltExtName,eltIntName,arrStyle);
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(locObj);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(locObj);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(locObj);
    FreeAndNil(FObject);
    FObject := locObj;
  end else begin
    locObj := FObject;
    if ( eltType <> locObj.ElType ) then begin
      if ( locObj.ElType <> nil ) then
        locObj.ElType.Release();
      locObj.ElType := eltType;
      locObj.ElType.AddRef();
    end;
    locObj.Name := typIntName;
    FSymbolTable.SetArrayStyle(locObj,arrStyle);
    FSymbolTable.SetArrayItemExternalName(locObj,eltExtName);
  end;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
end;

function TfArrayEdit.UpdateObject(
  var   AObject : TPasArrayType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
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
  {$I ufarrayedit.lrs}

end.

