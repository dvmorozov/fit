// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the user guide the application carries: the chapters Help > Explain
  Everything lists and the published site repeats.

  WHY THESE EXIST. The guide is prose, and prose drifts from the program it
  describes without any build noticing. Two things are checked here that a
  reader would otherwise find out the hard way: that the chapters reach the
  index the user opens, in the order they are meant to be read, and that every
  command the window's menus offer is named in the guide by the path a user
  follows to reach it. A command added to a menu and left out of the guide fails
  here by name.

  THE GUIDE IS EVERY CHAPTER THE BUILD REGISTERS: the core's, then each
  module's. The core chapters are held here to naming none of what only a
  module offers - the background, the angles and the Residual Spectrum are the
  scientific module's to explain since 2026-10-16 - so a product without that
  module reads about nothing it does not have. The walk over every menu command
  is that module's test (testcase_guide_menu_paths): the window's form still
  carries the module's entries, so it holds only with the module's chapter in
  the guide, and this suite links no module. }
unit testcase_client_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_registry,
    explanation_index, client_explanations, menu_paths,
    coordinate_axis, axis_mode_registry, axis_mode_registration,
    ui_commands, int_ui_host;

type
    TClientExplanationsTest = class(TTestCase)
    published
        procedure TheChaptersReachTheIndexInReadingOrder;
        procedure TheChaptersAreWellFormed;
        procedure RegisteringTwiceRegistersOnce;
        procedure TheMenuFileYieldsTheMenuPaths;
        procedure EveryAxisEntryIsNamedByItsPath;
        procedure TheBackgroundFractionHintSaysWhatItGoverns;
        { The message a held value shows points here, so it must exist, and
          say both rules this software chose: a width no wider than its fit
          interval, a background never below zero. }
        procedure TheLimitsOnParameterValuesAreExplained;
        { What History and the status bar say about samples no curve covers
          points the reader here, and the R-factor entry leads to it. }
        procedure TheSamplesNoCurveCoversAreExplained;
        procedure TheHistoryTabSaysItsRowsFollowTheRFactorScale;
        { The automatic run replaces the fit intervals, and says so: the
          guide says that it does and what it then tells the user. }
        procedure TheAutomaticRunSaysWhenItReplacedTheIntervals;
        { The core chapters say nothing of what only a module offers: no
          background menu, no wavelength, no diffraction angle, no Residual
          Spectrum - those are the scientific module's to explain. }
        procedure TheCoreChaptersLeaveTheShapesAndTheAnglesToTheirModule;
        { The Model panel's context menu is drawn from the command table's row
          commands, and the Explain pane follows the entry hovered there: an
          entry with no topic, or one naming nothing, leaves the pane with
          nothing to say about it. }
        procedure EveryRowCommandOfTheModelPanelExplainsItself;
    end;

implementation

const
    //  The guide's own chapters. Fit progress follows them in the application,
    //  but another test in this process may have registered it first, so its
    //  place is asserted over the providers rather than the process registry.
    ExpectedChapters: array[0..5] of string = (
        'Getting started', 'Projects and files', 'Data', 'Model', 'Fitting',
        'The window');

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

procedure TClientExplanationsTest.TheChaptersReachTheIndexInReadingOrder;
var
    Rows: TExplanationIndexRows;
    i, Last, At: longint;
begin
    //  THROUGH THE REGISTRY the window's index is built from, after the one call
    //  the application makes - not over the providers handed in directly.
    RegisterClientExplanations;
    Rows := ExplanationIndexOf(RegisteredExplanationProviders);
    Last := -1;
    for i := 0 to High(ExpectedChapters) do
    begin
        At := 0;
        while (At <= High(Rows)) and not (Rows[At].IsHeader and
            (Rows[At].Caption = ExpectedChapters[i])) do
            Inc(At);
        AssertTrue(ExpectedChapters[i] + ' is a heading of the index',
            At <= High(Rows));
        AssertTrue(ExpectedChapters[i] + ' comes after the chapter before it',
            At > Last);
        AssertFalse(ExpectedChapters[i] + ' lists at least one topic',
            (At = High(Rows)) or Rows[At + 1].IsHeader);
        Last := At;
    end;
    Rows := ExplanationIndexOf(ClientExplanationProviders);
    At := High(Rows);
    while (At > 0) and not Rows[At].IsHeader do
        Dec(At);
    AssertEquals('Fit progress closes the guide', 'Fit progress',
        Rows[At].Caption);
end;

procedure TClientExplanationsTest.TheChaptersAreWellFormed;
var
    Findings: TStringArray;
    i: longint;
begin
    Findings := ExplanationFindings(ClientExplanationProviders);
    for i := 0 to High(Findings) do
        Fail(Findings[i]);
end;

procedure TClientExplanationsTest.RegisteringTwiceRegistersOnce;
var
    Before: longint;
begin
    RegisterClientExplanations;
    Before := Length(RegisteredExplanationProviders);
    RegisterClientExplanations;
    AssertEquals(Before, Length(RegisteredExplanationProviders));
end;

procedure TClientExplanationsTest.TheMenuFileYieldsTheMenuPaths;
var
    Paths: TStringArray;
    Joined: string;
    i: longint;
begin
    //  The parse itself, against entries known to be in the form file - an
    //  empty list would pass the coverage walk over it (testcase_guide_menu_paths,
    //  with the module whose chapter names the rest) by checking nothing.
    Paths := DesignedMenuPaths;
    Joined := '';
    for i := 0 to High(Paths) do
        Joined := Joined + '|' + Paths[i];
    AssertTrue(Joined, Pos('|File > Import Profile|', Joined + '|') > 0);
    AssertTrue(Joined, Pos('|Fit > Automatically|', Joined + '|') > 0);
    AssertTrue(Joined, Pos('|Model > Background > Points > Compute Automatically|',
        Joined + '|') > 0);
    AssertTrue('a separator is not a command', Pos('> -|', Joined + '|') = 0);
    AssertTrue('a hidden entry is left out: ' + Joined,
        Pos('Create Rule', Joined) = 0);
end;

procedure TClientExplanationsTest.EveryAxisEntryIsNamedByItsPath;
const
    Menu: array[TAxisDimension] of string = (
        'Data > Argument Transformation > Use Rule > ',
        'Data > Value Transformation > Use Rule > ',
        'View > R-factor Scale > ');
var
    Text, Missing, Path: string;
    Providers: TExplanationProviders;
    Topics: TStringArray;
    Modes: TAxisModeClasses;
    E: TExplanation;
    D: TAxisDimension;
    i, j: longint;
begin
    //  A WALK OVER WHAT IS REGISTERED, so the next mode - here or in a module -
    //  arrives with the sentence that names it or fails by name. Over every
    //  registered chapter, not the framework's alone: a module names its own
    //  modes in its own chapter.
    RegisterAllAxisModes;
    RegisterClientExplanations;
    Text := '';
    Providers := RegisteredExplanationProviders;
    Topics := StaticTopicsOf(Providers);
    for i := 0 to High(Topics) do
        if FindExplanationIn(Providers, Topics[i], E) then
        begin
            Text := Text + #10 + E.Summary;
            for j := 0 to High(E.Body) do
                Text := Text + #10 + E.Body[j];
        end;
    Missing := '';
    for D := Low(TAxisDimension) to High(TAxisDimension) do
    begin
        //  The entry every coordinate's menu starts with.
        if Pos(Menu[D] + 'Automatic', Text) = 0 then
            Missing := Missing + LineEnding + '  ' + Menu[D] + 'Automatic';
        Modes := AxisModesFor(D);
        for i := 0 to High(Modes) do
        begin
            //  Named without the ellipsis a dialog's entry carries.
            Path := Menu[D] + StringReplace(Modes[i].CaptionFor(D), '...', '',
                []);
            if Pos(Path, Text) = 0 then
                Missing := Missing + LineEnding + '  ' + Path;
        end;
    end;
    AssertEquals('axis entries the guide never names by their path:' + Missing,
        '', Missing);
end;

procedure TClientExplanationsTest.TheBackgroundFractionHintSaysWhatItGoverns;
var
    Hint: string;
begin
    //  THE REPORT: the hint said the fraction was "for automatic background
    //  points generation". It is not read there at all: it is where the
    //  automatic PEAK SEARCH stops - Compute Automatically for positions and
    //  intervals, and Fit > Automatically. The guide says so; the status line
    //  must not say the opposite. In the window's words, which are no
    //  field's: the search for curve positions.
    Hint := DesignedActionHint('ActionSetBackgroundFraction');
    AssertTrue('the hint is read', Hint <> '');
    AssertTrue('it names the search for curve positions: ' + Hint,
        Pos('search for curve positions', Hint) > 0);
    AssertTrue('and not the background points: ' + Hint,
        Pos('background points', Hint) = 0);
end;

procedure TClientExplanationsTest.TheLimitsOnParameterValuesAreExplained;
var
    Providers: TExplanationProviders;
    E: TExplanation;
    Text, Missing: string;
    j: longint;
begin
    Providers := ClientExplanationProviders;
    AssertTrue('the topic exists',
        FindExplanationIn(Providers, 'model/parameter-limits', E));
    AssertEquals('Limits on parameter values', E.Title);
    AssertTrue('this software''s choice, and says so',
        E.Standing = esModelChoice);
    AssertTrue(Missing, ExplanationIsComplete(E, Missing));
    Text := E.Summary;
    for j := 0 to High(E.Body) do
        Text := Text + #10 + E.Body[j];
    AssertTrue('the width rule', Pos('wider than the fit interval', Text) > 0);
    AssertTrue('the background rule', Pos('below zero', Text) > 0);
    AssertTrue('what a typed value meets', Pos('Curve Attributes', Text) > 0);
    AssertTrue('and it says what it does not cover', Length(E.Limitations) > 0);
end;

function TextOf(const E: TExplanation): string;
var
    j: longint;
begin
    Result := E.Summary;
    for j := 0 to High(E.Body) do
        Result := Result + #10 + E.Body[j];
end;

function RelatesTo(const E: TExplanation; const ATopic: string): boolean;
var
    j: longint;
begin
    Result := False;
    for j := 0 to High(E.Related) do
        if E.Related[j] = ATopic then
            Exit(True);
end;

procedure TClientExplanationsTest.TheHistoryTabSaysItsRowsFollowTheRFactorScale;
const
    Topic = 'the-window/history-tab';
var
    E: TExplanation;
    Text: string;
begin
    AssertTrue('the topic exists',
        FindExplanationIn(ClientExplanationProviders, Topic, E));
    Text := TextOf(E);
    AssertTrue('the menu that chooses the form, by its path: ' + Text,
        Pos('View > R-factor Scale', Text) > 0);
    AssertTrue('and an example of a row on the default scale: ' + Text,
        Pos('log10 R-factor', Text) > 0);
end;

procedure TClientExplanationsTest.TheSamplesNoCurveCoversAreExplained;
const
    Topic = 'fitting/samples-no-curve-covers';
var
    Providers: TExplanationProviders;
    E: TExplanation;
    Text, Missing: string;
begin
    Providers := ClientExplanationProviders;
    AssertTrue('the topic exists', FindExplanationIn(Providers, Topic, E));
    AssertTrue('this software''s choice, and says so',
        E.Standing = esModelChoice);
    AssertTrue(Missing, ExplanationIsComplete(E, Missing));
    Text := TextOf(E);
    AssertTrue('what the model is there', Pos('0 there', Text) > 0);
    AssertTrue('that they still count', Pos('still count', Text) > 0);
    AssertTrue('where it is said', Pos('History', Text) > 0);
    AssertTrue('and on the status bar', Pos('status bar', Text) > 0);
    AssertTrue('and in a dialog when a fit ends', Pos('dialog', Text) > 0);
    AssertTrue('and asked before a fit starts', Pos('Before a fit', Text) > 0);
    AssertTrue('once for the same points', Pos('once', Text) > 0);
    AssertTrue('narrow the interval', Pos('narrow', Text) > 0);
    AssertTrue('or extend the model', Pos('extend', Text) > 0);
    AssertTrue('and it says what it does not cover', Length(E.Limitations) > 0);
    AssertTrue('the R-factor leads to it',
        FindExplanationIn(Providers, 'fitting/r-factor', E) and
        RelatesTo(E, Topic));
    AssertTrue('and so does the end of a fit',
        FindExplanationIn(Providers, 'fitting/when-a-fit-ends', E) and
        RelatesTo(E, Topic));
end;

procedure TClientExplanationsTest.TheCoreChaptersLeaveTheShapesAndTheAnglesToTheirModule;
const
    ModulesWords: array[0..5] of string = (
        'Model > Background', 'Set Rule Parameters', 'Sin Theta / Lambda',
        'Residual Spectrum', 'Background Points table', 'New User Curve');
var
    Text, Found: string;
    i: longint;
begin
    Text := TextOfChapters(ClientExplanationProviders);
    Found := '';
    for i := 0 to High(ModulesWords) do
        if Pos(ModulesWords[i], Text) > 0 then
            Found := Found + LineEnding + '  ' + ModulesWords[i];
    AssertEquals('the core chapters name what only a module offers:' + Found,
        '', Found);
end;

procedure TClientExplanationsTest.TheAutomaticRunSaysWhenItReplacedTheIntervals;
var
    E: TExplanation;
    Text: string;
begin
    AssertTrue('the topic exists', FindExplanationIn(ClientExplanationProviders,
        'fitting/automatically', E));
    Text := TextOf(E);
    AssertTrue('it says so when it ends: ' + Text,
        Pos('says so when it ends', Text) > 0);
    AssertTrue('and which samples are no longer fitted',
        Pos('no longer fitted', Text) > 0);
end;

procedure TClientExplanationsTest.EveryRowCommandOfTheModelPanelExplainsItself;
var
    Decls: TCommandDecls;
    E: TExplanation;
    Rows, Missing: string;
    i: longint;
begin
    RegisterClientExplanations;
    Decls := FrameworkCommands;
    Rows := '';
    Missing := '';
    for i := 0 to High(Decls) do
    begin
        if Decls[i].Scope <> scRow then
            Continue;
        Rows := Rows + ' ' + Decls[i].Id;
        if not FindExplanation(Decls[i].Topic, E) then
            Missing := Missing + LineEnding + '  ' + Decls[i].Id + ' (topic ''' +
                Decls[i].Topic + ''')';
    end;
    AssertTrue('the framework has row commands', Rows <> '');
    AssertEquals('row commands the Explain pane cannot explain:' + Missing,
        '', Missing);
end;

initialization
    RegisterTest('unit', TClientExplanationsTest);
end.
