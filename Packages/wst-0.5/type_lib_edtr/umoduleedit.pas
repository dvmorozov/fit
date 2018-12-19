{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit umoduleedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ActnList, Buttons,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfModuleEdit }

  TfModuleEdit = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtName: TEdit;
    edtNamespace: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasModule;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasModule;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fModuleEdit: TfModuleEdit;

implementation
uses parserutils;

{ TfModuleEdit }

procedure TfModuleEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not ( IsStrEmpty(edtName.Text) or IsStrEmpty(edtNamespace.Text));
end;

procedure TfModuleEdit.LoadFromObject();
begin
  edtName.Text := FSymbolTable.GetExternalName(FObject);
  edtNamespace.Text := FSymbolTable.GetExternalName(FObject);
end;

procedure TfModuleEdit.SaveToObject();
begin
  FObject.Name := ExtractIdentifier(edtName.Text);
  FSymbolTable.RegisterExternalAlias(FObject,Trim(edtNamespace.Text));
end;

function TfModuleEdit.UpdateObject(
  var AObject: TPasModule;
  const AUpdateType: TEditType;
        ASymbolTable: TwstPasTreeContainer
): Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  LoadFromObject();
  Result := ( ShowModal() = mrOK );
  if Result then begin
    SaveToObject();
  end;
end;

procedure TfModuleEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

initialization
  {$I umoduleedit.lrs}

end.

