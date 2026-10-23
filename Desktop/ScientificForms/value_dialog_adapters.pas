// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The wavelength and background-fraction windows, as the window asks
for them (value_dialogs).)

Each adapter is what the window did with its form, and nothing more: show it,
read its FValue, read its edit's text. The form is created when first asked
for - it was created at start-up while the window named it - and stays for
the session, as it did.
}
unit value_dialog_adapters;

{$mode objfpc}{$H+}

interface

uses
    value_dialogs;

type
    TWavelengthDialogAdapter = class(TObject, IValueDialog)
    public
        function Execute: boolean;
        function GetValue: double;
        procedure SetValue(AValue: double);
        function Text: string;
    end;

    TBackFactorDialogAdapter = class(TObject, IValueDialog)
    public
        function Execute: boolean;
        function GetValue: double;
        procedure SetValue(AValue: double);
        function Text: string;
    end;

implementation

uses
    Controls, Forms, input_wavelength_dialog, input_back_factor_dialog;

procedure EnsureWavelengthDlg;
begin
    if not Assigned(InputWavelengthDlg) then
        Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
end;

procedure EnsureBackFactorDlg;
begin
    if not Assigned(InputBackFactorDlg) then
        Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
end;

function TWavelengthDialogAdapter.Execute: boolean;
begin
    EnsureWavelengthDlg;
    Result := InputWavelengthDlg.ShowModal = mrOk;
end;

function TWavelengthDialogAdapter.GetValue: double;
begin
    EnsureWavelengthDlg;
    Result := InputWavelengthDlg.FValue;
end;

procedure TWavelengthDialogAdapter.SetValue(AValue: double);
begin
    EnsureWavelengthDlg;
    InputWavelengthDlg.FValue := AValue;
end;

function TWavelengthDialogAdapter.Text: string;
begin
    EnsureWavelengthDlg;
    Result := InputWavelengthDlg.EditWavelength.Text;
end;

function TBackFactorDialogAdapter.Execute: boolean;
begin
    EnsureBackFactorDlg;
    Result := InputBackFactorDlg.ShowModal = mrOk;
end;

function TBackFactorDialogAdapter.GetValue: double;
begin
    EnsureBackFactorDlg;
    Result := InputBackFactorDlg.FValue;
end;

procedure TBackFactorDialogAdapter.SetValue(AValue: double);
begin
    EnsureBackFactorDlg;
    InputBackFactorDlg.FValue := AValue;
end;

function TBackFactorDialogAdapter.Text: string;
begin
    EnsureBackFactorDlg;
    Result := InputBackFactorDlg.EditBackFactor.Text;
end;

end.
