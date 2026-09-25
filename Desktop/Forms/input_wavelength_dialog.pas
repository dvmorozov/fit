// SPDX-License-Identifier: GPL-3.0-or-later
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

uses
    Controls, ExtCtrls, Forms, LResources, StdCtrls, SysUtils
{$IFNDEF _WINDOWS}
    , Dialogs
{$ENDIF}
    ;

type

    { TInputWavelengthDlg }

    TInputWavelengthDlg = class(TForm)
        ButtonOK:     TButton;
        ButtonCancel: TButton;
        BevelButtons:    TBevel;
        EditWavelength: TEdit;
        LabelWavelength:    TLabel;
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormActivate(Sender: TObject);
    private

    public
        FValue: double;
    end;

var
    InputWavelengthDlg: TInputWavelengthDlg;

implementation

uses
    set_maximum_rfactor_dialog, typed_number;

procedure TInputWavelengthDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
    //  Says what IS wanted, not only what is refused: the box now also
    //  refuses a negative, and 'should not be zero' would not explain that.
    Msg: string = 'Improper wavelength input. Please enter a positive ' +
        'number (for example, 1.5406).';
begin
    CanClose := True;
    if ModalResult = mrOk then
        //  READ AND JUDGED IN ONE PLACE. A wavelength divides - the diffraction
        //  abscissa is sin(theta)/lambda - so zero is as wrong as a letter, and
        //  so is a negative one. typed_number says both in a single question
        //  and never touches the process-wide separator; the local parser this
        //  replaced swapped it and, on a typo, raised before swapping it back.
        if not TypedNumberIsPositive(EditWavelength.Text) then
        begin
{$IFDEF _WINDOWS}
            ShowBalloon(EditWavelength.Handle, WideString(Msg),
                ''          //vmesto Error - tak luchshe smotritsya
                );
{$ELSE}
            MessageDlg(Msg, mtError, [mbOK], 0);
{$ENDIF}
            ActiveControl := EditWavelength;
            CanClose      := False;
        end
        else
            //  Only now, and it cannot fail: the question above already read it.
            FValue := TypedNumberOr(EditWavelength.Text, FValue);{if ModalResult = mrOk then...}
end;

procedure TInputWavelengthDlg.FormActivate(Sender: TObject);
begin
    EditWavelength.Text := FloatToStr(FValue);
    ActiveControl := EditWavelength;
end;

initialization
    {$i input_wavelength_dialog.lrs}
end.
