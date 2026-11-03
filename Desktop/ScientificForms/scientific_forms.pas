// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The Free and Pro client's windows for the scientific module,
registered in one call.)

THESE ARE THE CLIENT APPLICATION'S, NOT THE MODULE'S (decided with the user,
2026-10-17): a module names no window, so its UI survives a replacement of
the thin client, and the windows a module needs are part of the client that
offers them. They are linked only into a client that links the module: its
module.json names this unit as its "forms", and the product's client_modules
- which only the desktop client calls - calls RegisterScientificForms. The
core and the main window name none of them, so a product without the module
carries none of their code.
}
unit scientific_forms;

{$mode objfpc}{$H+}

interface

{ Registers every window this folder holds where the module looks for it. }
procedure RegisterScientificForms;

implementation

uses
    user_curve_dialog_registry, create_user_points_set_dlg_adapter,
    user_curve_roles_dlg_adapter, expression_parser_adapter,
    value_dialogs, value_dialog_adapters, spectrum_view, residual_spectrum_view,
    //  The name the wavelength is asked by: the number the angles read.
    diffraction_axis_modes;

var
    Wavelength: TWavelengthDialogAdapter;
    BackFactor: TBackFactorDialogAdapter;
    Spectrum: TResidualSpectrumView;

procedure RegisterScientificForms;
begin
    //  The two windows that define a formula, and the parser that reports a
    //  bad expression in a message box.
    RegisterUserCurveDialogs(TCreateUserPointsSetDlgAdapter.Create,
        TUserCurveRolesDlgAdapter.Create, TExpressionParserAdapter.Create);
    //  The wavelength of the diffraction angles, and the background fraction
    //  the peak search stops at: the window asks them by name.
    if not Assigned(Wavelength) then
        Wavelength := TWavelengthDialogAdapter.Create;
    if not Assigned(BackFactor) then
        BackFactor := TBackFactorDialogAdapter.Create;
    RegisterValueDialog(WavelengthParameter, Wavelength);
    RegisterValueDialog(BackgroundFractionDialog, BackFactor);
    //  The Residual Spectrum tab's chart, which the window builds into the
    //  tab the module offers.
    if not Assigned(Spectrum) then
        Spectrum := TResidualSpectrumView.Create;
    RegisterSpectrumView(Spectrum);
end;

finalization
    Wavelength.Free;
    BackFactor.Free;
    Spectrum.Free;
end.
