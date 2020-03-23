{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TInputBackFactorDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit input_back_factor_dialog;

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
{$ENDIF}    ;

type
    TInputBackFactorDlg = class(TForm)
        OKBtn:     TButton;
        CancelBtn: TButton;
        Bevel1:    TBevel;
        ValueEdit: TEdit;
        Label1:    TLabel;
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormActivate(Sender: TObject);
    private

    public
        FValue: double;
    end;

var
    InputBackFactorDlg: TInputBackFactorDlg;

implementation

uses input_max_rfactor_dialog;

procedure TInputBackFactorDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
    Msg: string = 'Improper factor input. Factor should be more than 1.';
begin
    CanClose := True;
    //  posle uspeshnogo zakrytiya okna d. b.
    //  garantirovano, chto znachenie korrektno
    if ModalResult = mrOk then
        try
            FValue := StringToValue(ValueEdit.Text);
            if FValue <= 1 then
                raise Exception.Create(' ');
        except
{$IFDEF _WINDOWS}
            ShowBalloon(ValueEdit.Handle, WideString(Msg),
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(Msg, mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := ValueEdit;
            CanClose      := False;
        end;{if ModalResult = mrOk then...}
end;

procedure TInputBackFactorDlg.FormActivate(Sender: TObject);
begin
    ValueEdit.Text := FloatToStr(FValue);
    ActiveControl  := ValueEdit;
end;

initialization
    {$i input_back_factor_dialog.lrs}
end.
