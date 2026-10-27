// SPDX-License-Identifier: GPL-3.0-or-later
{ Every command the window's menus offer is named in the guide by the path a
  user follows to reach it - with this module's chapter in the guide.

  WHY HERE AND NOT IN THE FRAMEWORK'S SUITE. The walk is over the window's
  designed menus (Desktop/Forms/form_main.lfm), and those still carry entries
  only this module explains - Model > Background, the user's own curve, the
  Residual Spectrum - whose words the core chapters are held to leave out
  (testcase_client_explanations.TheCoreChaptersLeaveTheShapesAndTheAnglesToTheirModule).
  So the walk holds only for a guide that includes this module's chapter, as
  free Fit's and every product linking the module do; the framework's suite,
  which links no module, cannot run it. A command added to a menu and left out
  of the guide still fails here by name, in every suite that links the module.

  When the window stops carrying the module's entries in its own form, the walk
  belongs back in the framework's testcase_client_explanations. }
unit testcase_guide_menu_paths;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_registry,
    client_explanations, menu_paths, scientific_module;

type
    TGuideMenuPathsTest = class(TTestCase)
    private
        function GuideText: string;
    published
        procedure EveryDesignedMenuCommandIsNamedByItsPath;
        procedure EveryMenuCommandBuiltInCodeIsNamedByItsPath;
    end;

implementation

const
    //  Entries the window adds in code rather than in the form file, by the path
    //  the user sees. Kept here because the form file cannot say they exist.
    //  Delete curve is not among them: it is a row command, offered only by the
    //  Model panel's context menu.
    //  The axis entries are not here: they are generated from the registered
    //  modes, and testcase_client_explanations.EveryAxisEntryIsNamedByItsPath
    //  walks those instead.
    //  Data > Price Data is not here either: it is the menu of the module that
    //  reads price files now, and that module's guide names it.
    //  The background shapes under Model > Background > Curve are not listed
    //  one by one either: they are generated from the registry, like the curve
    //  types, and each explains itself as a curve type. None is the one entry
    //  that names no type.
    CodeBuiltMenuPaths: array[0..4] of string = (
        'Help > Explain Everything',
        'Fit > Loss Function',
        'Model > Clear Model',
        'Model > Curve Type > User > New User Curve',
        'Model > Background > Curve > None');

{ Every word the given chapters say, one explanation after another. }
function TextOfChapters(const AProviders: TExplanationProviders): string;
var
    Topics: TStringArray;
    E: TExplanation;
    i, j: longint;
begin
    Result := '';
    Topics := StaticTopicsOf(AProviders);
    for i := 0 to High(Topics) do
        if FindExplanationIn(AProviders, Topics[i], E) then
        begin
            Result := Result + #10 + E.Title + #10 + E.Summary;
            for j := 0 to High(E.Body) do
                Result := Result + #10 + E.Body[j];
            for j := 0 to High(E.Limitations) do
                Result := Result + #10 + E.Limitations[j];
        end;
end;

{ Every word the guide says: the client's chapters and this module's, as the
  window registers them. }
function TGuideMenuPathsTest.GuideText: string;
begin
    RegisterClientExplanations;
    RegisterScientificModule;
    Result := TextOfChapters(RegisteredExplanationProviders);
end;

procedure TGuideMenuPathsTest.EveryDesignedMenuCommandIsNamedByItsPath;
var
    Paths: TStringArray;
    Text, Missing: string;
    i: longint;
begin
    Text := GuideText;
    Paths := DesignedMenuPaths;
    Missing := '';
    for i := 0 to High(Paths) do
        if Pos(Paths[i], Text) = 0 then
            Missing := Missing + LineEnding + '  ' + Paths[i];
    AssertEquals('menu commands the guide never names by their path:' +
        Missing, '', Missing);
end;

procedure TGuideMenuPathsTest.EveryMenuCommandBuiltInCodeIsNamedByItsPath;
var
    Text, Missing: string;
    i: longint;
begin
    Text := GuideText;
    Missing := '';
    for i := 0 to High(CodeBuiltMenuPaths) do
        if Pos(CodeBuiltMenuPaths[i], Text) = 0 then
            Missing := Missing + LineEnding + '  ' + CodeBuiltMenuPaths[i];
    AssertEquals('menu commands the guide never names by their path:' +
        Missing, '', Missing);
end;

initialization
    RegisterTest('unit', TGuideMenuPathsTest);
end.
