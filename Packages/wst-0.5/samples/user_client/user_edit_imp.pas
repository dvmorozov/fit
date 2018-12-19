unit user_edit_imp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, RTTICtrls, user_service_intf;

type

  { TfUserEdit }

  TfUserEdit = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    edtCategory: TTIComboBox;
    edtName: TTIEdit;
    edteMail: TTIEdit;
    edtPreferences: TTIEdit;
  private
    FInfos: TUser;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
    property Infos : TUser read FInfos;
    function UpdateObject( AUser : TUser ) : Boolean;
  end; 

var
  fUserEdit: TfUserEdit;

implementation

{ TfUserEdit }

constructor TfUserEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfos := TUser.Create();
  edtName.Link.TIObject := FInfos;
  edteMail.Link.TIObject := FInfos;
  edtPreferences.Link.TIObject := FInfos;
  edtCategory.Link.TIObject := FInfos;
end;

destructor TfUserEdit.Destroy();
begin
  FreeAndNil(FInfos);
  inherited Destroy();
end;

function TfUserEdit.UpdateObject(AUser: TUser): Boolean;
begin
  Infos.Assign(AUser);
  Result := ( ShowModal() = mrOK );
  if Result then begin
    AUser.Assign(Infos);
  end;
end;

initialization
  {$I user_edit_imp.lrs}

end.

