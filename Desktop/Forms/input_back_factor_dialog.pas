// SPDX-License-Identifier: GPL-3.0-or-later
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

uses
    Controls, ExtCtrls, Forms, LResources, StdCtrls, SysUtils
{$IFNDEF _WINDOWS}
    , Dialogs
{$ENDIF}
    ;

type
    TInputBackFactorDlg = class(TForm)
        ButtonOK:     TButton;
        ButtonCancel: TButton;
        BevelButtons:    TBevel;
        EditBackFactor: TEdit;
        LabelBackFactor:    TLabel;
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormActivate(Sender: TObject);
    private

    public
        FValue: double;
    end;

var
    InputBackFactorDlg: TInputBackFactorDlg;

implementation

uses
    set_maximum_rfactor_dialog, typed_number;

procedure TInputBackFactorDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
    Msg: string = 'Improper factor input. Factor should be more than 1.';
begin
    CanClose := True;
    //  posle uspeshnogo zakrytiya okna d. b.
    //  garantirovano, chto znachenie korrektno
    if ModalResult = mrOk then
        //  Read through typed_number, which never touches the process-wide
        //  separator - see findings.md.
        if not TryTypedNumber(EditBackFactor.Text, FValue) or (FValue <= 1) then
        begin
{$IFDEF _WINDOWS}
            ShowBalloon(EditBackFactor.Handle, WideString(Msg),
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(Msg, mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := EditBackFactor;
            CanClose      := False;
        end;{if ModalResult = mrOk then...}
end;

procedure TInputBackFactorDlg.FormActivate(Sender: TObject);
begin
    EditBackFactor.Text := FloatToStr(FValue);
    ActiveControl  := EditBackFactor;
end;

initialization
    {$i input_back_factor_dialog.lrs}
end.
