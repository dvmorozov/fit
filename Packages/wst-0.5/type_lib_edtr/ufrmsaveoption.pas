unit ufrmsaveoption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, Buttons;

type

  { TfrmSaveOptions }

  TfrmSaveOptions = class (TForm )
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    btnSelectDir : TButton;
    edtInterface : TCheckBox;
    edtProxy : TCheckBox;
    edtImplementation : TCheckBox;
    edtBinder : TCheckBox;
    edtOutputDir : TEdit;
    GroupBox1 : TGroupBox;
    Label1 : TLabel;
    Panel1 : TPanel;
    Panel2 : TPanel;
    SD : TSelectDirectoryDialog;
    procedure actOKExecute (Sender : TObject );
    procedure actOKUpdate (Sender : TObject );
    procedure btnSelectDirClick (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSaveOptions : TfrmSaveOptions;

implementation

{ TfrmSaveOptions }

procedure TfrmSaveOptions.actOKUpdate (Sender : TObject );
begin
  TAction(Sender).Enabled :=
    ( Trim(edtOutputDir.Text) <> '' ) and
    ( edtInterface.Checked or edtProxy.Checked or
      edtImplementation.Checked or edtBinder.Checked
    );
end;

procedure TfrmSaveOptions.btnSelectDirClick (Sender : TObject );
begin
  SD.FileName := edtOutputDir.Text;
  if SD.Execute() then begin
    edtOutputDir.Text := SD.FileName;
  end;
end;


procedure TfrmSaveOptions.actOKExecute (Sender : TObject );
begin
  ForceDirectories(edtOutputDir.Text);
  ModalResult := mrOK;
end;

initialization
  {$I ufrmsaveoption.lrs}

end.

