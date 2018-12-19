unit uftypealiasedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons,
  pastree, pascal_parser_intf, edit_helper, ActnList;

type

  { TfTypeAliasEdit }

  TfTypeAliasEdit = class(TForm)
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    edtBaseType : TComboBox;
    edtName : TEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    TabSheet1 : TTabSheet;
    procedure actOKExecute(Sender : TObject);
    procedure actOKUpdate(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasAliasType;
    FSymbolTable : TwstPasTreeContainer;
    FOldBaseType : TPasType;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure PrepareParentCombo();
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasAliasType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;

  end; 

var
  fTypeAliasEdit : TfTypeAliasEdit;

implementation
uses parserutils;

{ TfTypeAliasEdit }

procedure TfTypeAliasEdit.actOKUpdate(Sender : TObject);
begin
  TAction(actOK).Enabled := ( not IsStrEmpty(edtName.Text) ) and ( edtBaseType.ItemIndex >= 0 );
end;

procedure TfTypeAliasEdit.actOKExecute(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TfTypeAliasEdit.PrepareParentCombo();
begin
  edtBaseType.Items.BeginUpdate();
  try
    edtBaseType.Items.Clear();
    FillTypeList(edtBaseType.Items,FSymbolTable);
  finally
    edtBaseType.Items.EndUpdate();
  end;
end;

procedure TfTypeAliasEdit.LoadFromObject();
var
  extName : string;
begin
  edtName.Text := '';
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    if Assigned(FObject.DestType) then begin
      edtBaseType.ItemIndex := edtBaseType.Items.IndexOfObject(FObject.DestType);
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfTypeAliasEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasAliasType;
  baseType : TPasType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);

  if ( FObject = nil ) then begin
    FObject := TPasAliasType(FSymbolTable.CreateElement(TPasAliasType,typIntName,FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Classes.Add(FObject);
  end;

  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
  baseType := edtBaseType.Items.Objects[edtBaseType.ItemIndex] as TPasType;
  if ( baseType <> FOldBaseType ) then begin
    if ( FOldBaseType <> nil ) then
      FOldBaseType.Release();
    locObj.DestType := baseType;
    locObj.DestType.AddRef();
  end;
end;

function TfTypeAliasEdit.UpdateObject(
  var   AObject : TPasAliasType;
  const AUpdateType : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( FObject <> nil ) then
    FOldBaseType := FObject.DestType;
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
  {$I uftypealiasedit.lrs}

end.

