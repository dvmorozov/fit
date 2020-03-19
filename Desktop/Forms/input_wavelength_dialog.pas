{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TInputWavelengthDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit input_wavelength_dialog;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses SysUtils, Forms, Controls, StdCtrls,
    ExtCtrls, LResources
{$IFNDEF _WINDOWS}
    , Dialogs
{$ENDIF}
    ;

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

uses input_max_rfactor_dialog;

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
{$IFDEF _WINDOWS}
            ShowBalloon(WavelengthValueEdit.Handle, WideString(Msg),
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(Msg, mtError, [mbOk], 0);
{$ENDIF}
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
    {$i input_wavelength_dialog.lrs}
end.

