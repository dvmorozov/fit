// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the scientific module's front door contributes.)

Entered where the application enters: the window calls RegisterAppModules,
which in free Fit calls RegisterScientificModule. What it contributes beside
its curve types is the diffraction profile it decomposes: the angles of a
diffraction pattern as axis modes, and '.DAT' as a diffraction profile - the
framework itself claims neither, so a product without the module offers no
angle, no wavelength and no such format.

AND IT EXPLAINS THEM, in a chapter of its own (non-negotiable 10): the
background, the diffraction angles, the wavelength and the Residual Spectrum
were the core guide's, and a product without the module read about menus it
does not have. The topics are named here as text, not through the unit that
writes them, so the test enters where the window does - the front door - and
fails for a front door that forgets to register them.
}
unit testcase_scientific_module;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, axis_mode_registry,
    diffraction_axis_modes, data_loader, data_loader_registry, dat_file_loader,
    chart_tabs, explanation, explanation_registry, client_explanations,
    axis_choice, coordinate_axis, int_ui_host, mock_ui_host,
    problem_value_store, fit_project_document, scientific_rules_ui,
    scientific_module;

type
    TScientificModuleTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        procedure ItRegistersTheDiffractionAngles;
        procedure ItClaimsDatAsADiffractionProfile;
        procedure ItOffersTheResidualSpectrum;
        procedure ItExplainsTheBackgroundTheAnglesAndTheSpectrum;
        procedure ItsChapterLinksOnlyWhereTheCoreGuideAndItselfReach;
        procedure ItSaysWhatAnOldFilesDisplayModesMeant;
        procedure ItDescribesAndKeepsTheWavelengthTheAnglesRead;
        procedure ItOffersTheWavelengthUnderSetRuleParameters;
        procedure GivingAWavelengthShowsSinThetaOverLambda;
        procedure CancellingTheWavelengthLeavesTheAxisAlone;
    end;

implementation

procedure TScientificModuleTest.SetUp;
begin
    RegisterScientificModule;
end;

procedure TScientificModuleTest.ItRegistersTheDiffractionAngles;
begin
    AssertTrue(FindAxisMode(TwoThetaAxisModeId) = TTwoThetaAxisMode);
    AssertTrue(FindAxisMode(ThetaAxisModeId) = TThetaAxisMode);
    AssertTrue(FindAxisMode(SinThetaOverLambdaAxisModeId) = TSinThetaOverLambdaAxisMode);
    AssertTrue('and reads a number',
        FindAxisMode(SinThetaOverLambdaAxisModeId).Info.Parameter = apValue);
    AssertEquals('the wavelength', WavelengthParameter,
        FindAxisMode(SinThetaOverLambdaAxisModeId).Info.ParameterName);
end;

procedure TScientificModuleTest.ItClaimsDatAsADiffractionProfile;
var
    Info: TDataLoaderInfo;
begin
    AssertTrue(FindDataLoaderInfo('profile.dat', Info));
    AssertEquals('Diffraction profile', Info.FormatName);
    AssertTrue('read by the two-column reader', FindDataLoaderClass('profile.dat') = TDATFileLoader);
end;

{ The map of where the difference to a model of peaks is not noise: a tab of
  the window this module offers, and a product without it does not show. }
procedure TScientificModuleTest.ItOffersTheResidualSpectrum;
begin
    AssertTrue(ctSpectrum in OfferedChartTabs);
end;

const
    ScientificTopics: array[0..12] of string = (
        'peaks-and-diffraction/background',
        'peaks-and-diffraction/background-curve',
        'peaks-and-diffraction/background-subtraction',
        'peaks-and-diffraction/background-fraction',
        'peaks-and-diffraction/background-variation',
        'peaks-and-diffraction/background-points-table',
        'peaks-and-diffraction/diffraction-angles',
        'peaks-and-diffraction/wavelength',
        'peaks-and-diffraction/residual-spectrum',
        'peaks-and-diffraction/user-defined-curves',
        'peaks-and-diffraction/user-curve-formula',
        'peaks-and-diffraction/user-curve-roles',
        'peaks-and-diffraction/user-curve-library');

procedure TScientificModuleTest.ItExplainsTheBackgroundTheAnglesAndTheSpectrum;
var
    E: TExplanation;
    Missing: string;
    i: longint;
begin
    for i := 0 to High(ScientificTopics) do
    begin
        AssertTrue(ScientificTopics[i] + ' is explained',
            FindExplanation(ScientificTopics[i], E));
        AssertTrue(ScientificTopics[i] + ': ' + Missing,
            ExplanationIsComplete(E, Missing));
    end;
end;

{ Over the core chapters and this module's alone - what a build of the bare
  core plus this module carries - so a link to a topic only another module
  holds fails here, rather than in a product that leaves that module out. }
procedure TScientificModuleTest.ItsChapterLinksOnlyWhereTheCoreGuideAndItselfReach;
var
    Providers, Core: TExplanationProviders;
    Findings: TStringArray;
    i: longint;
begin
    Core := ClientExplanationProviders;
    Providers := Core;
    for i := 0 to High(RegisteredExplanationProviders) do
        if RegisteredExplanationProviders[i].Namespace = 'peaks-and-diffraction' then
        begin
            SetLength(Providers, Length(Providers) + 1);
            Providers[High(Providers)] := RegisteredExplanationProviders[i];
        end;
    AssertEquals('the chapter is registered', Length(Core) + 1, Length(Providers));
    Findings := ExplanationFindings(Providers);
    for i := 0 to High(Findings) do
        Fail(Findings[i]);
end;

{ What a file written before the registry meant by display modes 0, 1 and 2:
  the angles, which are this module's - so the framework does not answer for
  them, and the module says. }
procedure TScientificModuleTest.ItSaysWhatAnOldFilesDisplayModesMeant;
begin
    AssertEquals(TwoThetaAxisModeId, LegacyArgumentModeId(0));
    AssertEquals(ThetaAxisModeId, LegacyArgumentModeId(1));
    AssertEquals(SinThetaOverLambdaAxisModeId, LegacyArgumentModeId(2));
end;

{ The wavelength is this module's number: described to the framework under the
  name the angles read it by, kept where the client reads it, and the number an
  old file's settings hand over on open. }
procedure TScientificModuleTest.ItDescribesAndKeepsTheWavelengthTheAnglesRead;
var
    Info: TAxisParameterInfo;
    Value_: double;
    Refusal: string;
begin
    AssertTrue('described', FindAxisParameter(WavelengthParameter, Info));
    AssertEquals('Wavelength', Info.Caption);
    AssertTrue('kept for each problem',
        Assigned(ProblemValueStore(WavelengthParameter)));
    AssertEquals('the name an old file''s wavelength is handed to',
        FormerWaveLengthValue, WavelengthParameter);
    //  Refused in the words the window always used.
    AssertFalse(AxisValueFromText(Info, '1,5406', Value_, Refusal));
    AssertEquals('The wavelength must be a number, written with a full stop ' +
        '- for example 1.5406.', Refusal);
end;

function RulesUiIndex: longint;
var
    Mods: TUiModuleArray;
    i: longint;
begin
    Result := -1;
    Mods := RegisteredUiModules;
    for i := 0 to High(Mods) do
        if Mods[i].Name = ScientificRulesUiName then
            Exit(i);
end;

{ Data > Argument Transformation > Set Rule Parameters > Wavelength..., the
  entry the window's form file held: now the module's, joining that submenu. }
procedure TScientificModuleTest.ItOffersTheWavelengthUnderSetRuleParameters;
var
    Decls: TUiMenuDeclArray;
    i: longint;
begin
    i := RulesUiIndex;
    AssertTrue('the front door registers it', i >= 0);
    AssertTrue('into Set Rule Parameters',
        UiModuleMenuPlace(ScientificRulesUiName) = mpRuleParameters);
    Decls := RegisteredUiModules[i].MenuItems;
    AssertEquals(1, Length(Decls));
    AssertEquals('the words it always had', 'Wavelength...', Decls[0].Caption);
    AssertTrue('a command', Decls[0].Kind = mkCommand);
    AssertTrue('explained', RegisteredTopicResolves(Decls[0].Topic));
end;

procedure TScientificModuleTest.GivingAWavelengthShowsSinThetaOverLambda;
var
    Host: TMockUiHost;
begin
    Host := TMockUiHost.Create;
    try
        RegisteredUiModules[RulesUiIndex].Command(SetWavelengthId, '', Host);
        AssertEquals('asked for the wavelength', WavelengthParameter,
            Host.AxisValueAsked);
        AssertEquals('and switched to the axis that divides by it',
            SinThetaOverLambdaAxisModeId, Host.AxisModeShown);
        AssertTrue('the argument', Host.AxisDimensionShown = adArgument);
    finally
        Host.Free;
    end;
end;

procedure TScientificModuleTest.CancellingTheWavelengthLeavesTheAxisAlone;
var
    Host: TMockUiHost;
begin
    Host := TMockUiHost.Create;
    try
        Host.AxisValueAccepted := False;
        RegisteredUiModules[RulesUiIndex].Command(SetWavelengthId, '', Host);
        AssertEquals('asked', WavelengthParameter, Host.AxisValueAsked);
        AssertEquals('nothing shown', '', Host.AxisModeShown);
    finally
        Host.Free;
    end;
end;

initialization
    RegisterTest('unit', TScientificModuleTest);
end.
