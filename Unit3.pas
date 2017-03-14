unit Unit3;

{$MODE Delphi}

interface

uses LCLIntf, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, LResources;

type

  { TInputWavelengthDlg }

  TInputWavelengthDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    WavelengthValueEdit: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private

  public
    Value: Double;
  end;

var
  InputWavelengthDlg: TInputWavelengthDlg;

implementation

uses Unit4;

procedure TInputWavelengthDlg.FormCloseQuery(Sender: TObject;
    var CanClose: Boolean
    );
const Msg: string = 'Improper wavelength input. Wavelength should not be zero.';
begin
    CanClose := True;
    if ModalResult = mrOk then
    begin
        try
            Value := StringToValue(WavelengthValueEdit.Text);
            if Value = 0 then raise Exception.Create(' ');
        except
{$ifdef windows}
            ShowBalloon(WavelengthValueEdit.Handle, WideString(Msg),
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$else}
            MessageDlg(Msg, mtError, [mbOk], 0);
{$endif}
            ActiveControl := WavelengthValueEdit;
            CanClose := False;
        end;
    end;{if ModalResult = mrOk then...}
end;

procedure TInputWavelengthDlg.FormActivate(Sender: TObject);
begin
    WavelengthValueEdit.Text := FloatToStr(Value);
    ActiveControl := WavelengthValueEdit;
end;

initialization
    {$i Unit3.lrs}
end.


