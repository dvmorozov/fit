// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(That the framework's own sources name no field's data.)

THE RULE. A module covers one class of fitting tasks and brings everything that
class needs; the framework hosts. Price data - the OHLC reader, the Bar, Date and
Price axis modes, and the choice of which column is read - is the price-series
module's: the public build showed "Price" under Value Transformation and a Price
Data menu to users with no price series and no module to analyse one.

WHY A SOURCE SCAN. What is wrong is that a name is PRESENT in the framework's
tree: no registry query can see it, because in a build that also links the module
the registries hold the same things either way. So the absence itself is what is
asserted, over the directories the framework ships - Modules/ is excluded, since
a module in this tree is a module.

Diffraction is the other field still here, and is not listed: its shapes, angles
and formats are the scientific module's already, but the wavelength, the legacy
view-mode ids and some of the window's words are still in the core, and its
names join this list once those have left.
}
unit testcase_framework_boundary;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, StrUtils, fpcunit, testregistry, source_scan;

type
    TFrameworkBoundaryTest = class(TTestCase)
    published
        procedure TheFrameworkNamesNoPriceData;
        { The user's formula type is the scientific module's (2026-10-17): the
          engine builds and configures it through the registry and the
          IsDefinedByFormula capability, and the core's list of the types it
          ships names none. The window's user-curve menus and dialogs still
          name it - hidden in a build without the module - until their own
          seam moves them; they are not scanned. }
        procedure TheEngineAndTheCurveListNameNoFormulaClass;
        { And so are its dialogs, the formulas a user saved and the menu that
          lists them (2026-10-17): the window adds a module's entries to its
          group of Model > Curve Type through a seam
          (curve_type_menu_extension) and names none of them, so a product
          without the module carries none of their code. The same for the
          module's other windows - the wavelength and the background
          fraction - which are the Free client's (Desktop/ScientificForms). }
        procedure TheFrameworkNamesNoUserFormulaUnit;
    end;

implementation

const
    { What the price-data feature is made of, by the names that would have to
      appear for the framework to be using it. }
    PriceDataNames: array[0..5] of string = (
        'ohlc_csv_loader', 'price_axis_modes', 'TOHLCFileLoader',
        'RegisterPriceAxisModes', 'PriceDataTopic', 'BuildPriceDataMenu');

    FrameworkDirs: array[0..3] of string = ('Desktop', 'Common', 'Server', 'Worker');

function NamesWord(const AText, AWord: string): boolean;
var
    Text, Word_: string;
    k, After: integer;
begin
    //  As a whole identifier: 'price_axis_modes_test' is not the unit.
    Text := LowerCase(AText);
    Word_ := LowerCase(AWord);
    k := PosEx(Word_, Text, 1);
    while k > 0 do
    begin
        After := k + Length(Word_);
        if ((k = 1) or not (Text[k - 1] in ['a'..'z', '0'..'9', '_'])) and
            ((After > Length(Text)) or
            not (Text[After] in ['a'..'z', '0'..'9', '_'])) then
            Exit(True);
        k := PosEx(Word_, Text, k + 1);
    end;
    Result := False;
end;

procedure TFrameworkBoundaryTest.TheFrameworkNamesNoPriceData;
var
    Root: string;
    Files, Offenders, Source: TStringList;
    i, d, n: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found - a scan of nothing is not a pass',
        Root <> '');
    Files := TStringList.Create;
    Offenders := TStringList.Create;
    Source := TStringList.Create;
    try
        for d := 0 to High(FrameworkDirs) do
            CollectPascalSources(Root + FrameworkDirs[d], Files);
        AssertTrue('the framework''s sources were found', Files.Count > 0);
        for i := 0 to Files.Count - 1 do
        begin
            Source.LoadFromFile(Files[i]);
            Source.Text := StripCommentsAndStrings(Source.Text);
            for n := 0 to High(PriceDataNames) do
                if NamesWord(Source.Text, PriceDataNames[n]) then
                    Offenders.Add(ExtractRelativePath(Root, Files[i]) + ' names ' +
                        PriceDataNames[n]);
        end;
        AssertEquals('price data is its module''s, not the framework''s: ' +
            Offenders.Text, 0, Offenders.Count);
    finally
        Source.Free;
        Offenders.Free;
        Files.Free;
    end;
end;

procedure TFrameworkBoundaryTest.TheEngineAndTheCurveListNameNoFormulaClass;
const
    Names: array[0..1] of string = ('user_points_set', 'TUserPointsSet');
var
    Root: string;
    Files, Offenders, Source: TStringList;
    i, n: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found', Root <> '');
    Files := TStringList.Create;
    Offenders := TStringList.Create;
    Source := TStringList.Create;
    try
        CollectPascalSources(Root + 'Server', Files);
        Files.Add(Root + 'Desktop' + PathDelim + 'ModelCurves' + PathDelim +
            'curve_type_registration.pas');
        for i := 0 to Files.Count - 1 do
        begin
            Source.LoadFromFile(Files[i]);
            Source.Text := StripCommentsAndStrings(Source.Text);
            for n := 0 to High(Names) do
                if NamesWord(Source.Text, Names[n]) then
                    Offenders.Add(ExtractRelativePath(Root, Files[i]) + ' names ' +
                        Names[n]);
        end;
        AssertEquals('the formula type is its module''s: ' + Offenders.Text,
            0, Offenders.Count);
    finally
        Source.Free;
        Offenders.Free;
        Files.Free;
    end;
end;

procedure TFrameworkBoundaryTest.TheFrameworkNamesNoUserFormulaUnit;
const
    Names: array[0..21] of string = (
        'user_points_set', 'configurable_user_points_set',
        'create_user_points_set_dlg', 'user_points_set_prop_dialog',
        'user_curve_library', 'curve_type_storage_adapter', 'user_curve_flow',
        'user_curve_wizard', 'TCreateUserPointsSetDlg', 'TUserPointsSetPropDlg',
        //  And the wavelength and background-fraction windows, which the
        //  window asks for by name (value_dialogs) since they moved too.
        'input_wavelength_dialog', 'input_back_factor_dialog',
        'TInputWavelengthDlg', 'TInputBackFactorDlg',
        //  And the Residual Spectrum's chart, built through spectrum_view.
        'residual_spectrum_view', 'TResidualSpectrumView', 'scientific_forms',
        'value_dialog_adapters',
        //  And the spectrum's grid, the map's logic.
        'difference_scalogram', 'TScalogramGrid',
        //  And the background arithmetic, which the engine asks for by name
        //  (background_algorithms).
        'background_search',
        //  And the peak search, likewise (peak_search_registry).
        'peak_search');
var
    Root: string;
    Files, Offenders, Source: TStringList;
    i, d, n: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found', Root <> '');
    Files := TStringList.Create;
    Offenders := TStringList.Create;
    Source := TStringList.Create;
    try
        for d := 0 to High(FrameworkDirs) do
            CollectPascalSources(Root + FrameworkDirs[d], Files);
        AssertTrue('the framework''s sources were found', Files.Count > 0);
        for i := 0 to Files.Count - 1 do
        begin
            //  THE CLIENT APPLICATION'S WINDOWS FOR THE MODULE are not the
            //  framework: they live in the Free client's tree, are linked only
            //  with the module (its module.json "forms", client_modules), and
            //  name its units by design (decided with the user, 2026-10-17).
            if Pos(PathDelim + 'ScientificForms' + PathDelim, Files[i]) > 0 then
                Continue;
            Source.LoadFromFile(Files[i]);
            Source.Text := StripCommentsAndStrings(Source.Text);
            for n := 0 to High(Names) do
                if NamesWord(Source.Text, Names[n]) then
                    Offenders.Add(ExtractRelativePath(Root, Files[i]) + ' names ' +
                        Names[n]);
        end;
        AssertEquals('the user''s formulas are the scientific module''s: ' +
            Offenders.Text, 0, Offenders.Count);
    finally
        Source.Free;
        Offenders.Free;
        Files.Free;
    end;
end;

initialization
    //  A unit test, as testcase_no_pascal_labels is: it reads sources already
    //  on disk beside it and writes nothing.
    RegisterTest('unit', TFrameworkBoundaryTest);
end.
