// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's entry under Data > Argument Transformation >
Set Rule Parameters: the wavelength the diffraction angles read.)

THE MODULE'S MENU, NOT THE WINDOW'S. The Wavelength entry was written into the
window's form file, with a handler that named the module's dialog and the id of
the sin(theta)/lambda mode it switches to. It is declared here, as data
(int_ui_host), registered to join Set Rule Parameters
(TUiMenuPlace.mpRuleParameters) with no menu of the module's around it - so a
product without this module has no such entry, and the window names neither the
number nor the mode. The words and what the command does are as they were: ask
for the wavelength, and on OK show the argument as Sin Theta / Lambda.

THE ASKING IS THE WINDOW'S (IUiHost.AskAxisValue): the dialog, the refusal of a
number that is not positive, and making it the problem's are the same steps the
axis menu takes when Sin Theta / Lambda is chosen with no wavelength known, so
the two ways of giving one cannot differ.
}
unit scientific_rules_ui;

{$mode objfpc}{$H+}

interface

uses
    int_ui_host;

const
    { The entry's id, unique within this module. }
    SetWavelengthId = 'scientific.set-wavelength';
    { The module's name in the registry of window modules. Not the curve
      types' owner, and not a menu of its own: its one entry joins Set Rule
      Parameters. }
    ScientificRulesUiName = 'scientific-rules';

function ScientificRulesUi: IUiModule;

{ Registers the entry where the window puts module rule parameters.
  Idempotent. }
procedure RegisterScientificRulesUi;

implementation

uses
    coordinate_axis, diffraction_axis_modes, scientific_guide;

type
    TScientificRulesUi = class(TObject, IUiModule)
    public
        function Name: string;
        function MenuItems: TUiMenuDeclArray;
        procedure Command(const AId, AData: string; AHost: IUiHost);
        function PanelId: string;
        function PanelEmptyText: string;
        function PanelDetachedSuffix: string;
        procedure PanelSelectionChanged(const ARowId, ARowText: string;
            AHost: IUiHost);
        function RowMenuItems(const ARowId: string): TUiMenuDeclArray;
        function ReportCaption: string;
    end;

var
    Instance: TScientificRulesUi = nil;

function TScientificRulesUi.Name: string;
begin
    Result := ScientificRulesUiName;
end;

function TScientificRulesUi.MenuItems: TUiMenuDeclArray;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0] := Default(TUiMenuDecl);
    Result[0].Id := SetWavelengthId;
    //  The caption the form file gave it.
    Result[0].Caption := 'Wavelength...';
    Result[0].Hint := 'The wavelength of the radiation, which the Sin Theta / ' +
        'Lambda axis divides by.';
    Result[0].Kind := mkCommand;
    Result[0].Topic := WavelengthTopic;
end;

procedure TScientificRulesUi.Command(const AId, AData: string; AHost: IUiHost);
begin
    if AId <> SetWavelengthId then
        Exit;
    //  The wavelength is what the sin(theta)/lambda axis divides by, so giving
    //  one switches to it, as it always has.
    if AHost.AskAxisValue(WavelengthParameter) then
        AHost.ShowAxisMode(adArgument, SinThetaOverLambdaAxisModeId);
end;

function TScientificRulesUi.PanelId: string;
begin
    Result := '';
end;

function TScientificRulesUi.PanelEmptyText: string;
begin
    Result := '';
end;

function TScientificRulesUi.PanelDetachedSuffix: string;
begin
    Result := '';
end;

{$hints off}
procedure TScientificRulesUi.PanelSelectionChanged(const ARowId,
    ARowText: string; AHost: IUiHost);
begin
end;

function TScientificRulesUi.RowMenuItems(const ARowId: string): TUiMenuDeclArray;
begin
    Result := nil;
end;
{$hints on}

function TScientificRulesUi.ReportCaption: string;
begin
    Result := '';
end;

function ScientificRulesUi: IUiModule;
begin
    if not Assigned(Instance) then
        Instance := TScientificRulesUi.Create;
    Result := Instance;
end;

procedure RegisterScientificRulesUi;
begin
    RegisterUiModule(ScientificRulesUi, mpRuleParameters);
end;

finalization
    Instance.Free;
end.
