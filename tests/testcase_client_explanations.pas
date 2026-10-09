// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the user guide the application carries: the chapters Help > Explain
  Everything lists and the published site repeats.

  WHY THESE EXIST. The guide is prose, and prose drifts from the program it
  describes without any build noticing. Two things are checked here that a
  reader would otherwise find out the hard way: that the chapters reach the
  index the user opens, in the order they are meant to be read, and that every
  command the window's menus offer is named in the guide by the path a user
  follows to reach it. A command added to a menu and left out of the guide fails
  here by name. }
unit testcase_client_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_registry,
    explanation_index, client_explanations, menu_paths,
    coordinate_axis, axis_mode_registry, axis_mode_registration;

type
    TClientExplanationsTest = class(TTestCase)
    private
        function GuideText: string;
    published
        procedure TheChaptersReachTheIndexInReadingOrder;
        procedure TheChaptersAreWellFormed;
        procedure RegisteringTwiceRegistersOnce;
        procedure TheMenuFileYieldsTheMenuPaths;
        procedure EveryDesignedMenuCommandIsNamedByItsPath;
        procedure EveryMenuCommandBuiltInCodeIsNamedByItsPath;
        procedure EveryAxisEntryIsNamedByItsPath;
        procedure TheBackgroundFractionHintSaysWhatItGoverns;
        { The message a held value shows points here, so it must exist, and
          say both rules this software chose: a width no wider than its fit
          interval, a background never below zero. }
        procedure TheLimitsOnParameterValuesAreExplained;
        { What History and the status bar say about samples no curve covers
          points the reader here, and the R-factor entry leads to it. }
        procedure TheSamplesNoCurveCoversAreExplained;
        { The automatic run replaces the fit intervals, and says so: the
          guide says that it does and what it then tells the user. }
        procedure TheAutomaticRunSaysWhenItReplacedTheIntervals;
    end;

implementation

const
    //  Entries the window adds in code rather than in the form file, by the path
    //  the user sees. Kept here because the form file cannot say they exist.
    //  Delete curve is not among them: it is a row command, offered only by the
    //  Model panel's context menu.
    //  The axis entries are not here: they are generated from the registered
    //  modes, and EveryAxisEntryIsNamedByItsPath walks those instead.
    //  Data > Price Data is not here either: it is the menu of the module that
    //  reads price files now, and that module's guide names it.
    //  The background shapes under Model > Background > Curve are not listed
    //  one by one either: they are generated from the registry, like the curve
    //  types, and each explains itself as a curve type. None is the one entry
    //  that names no type.
    CodeBuiltMenuPaths: array[0..7] of string = (
        'Help > Explain Everything',
        'Fit > Loss Function',
        'Model > Clear Model',
        'Model > Curve Type > User > New User Curve',
        'Model > Background > Curve > None',
        'Model > History > Make Current',
        'Model > History > Rename',
        'Model > History > Delete');

    //  The guide's own chapters. Fit progress follows them in the application,
    //  but another test in this process may have registered it first, so its
    //  place is asserted over the providers rather than the process registry.
    ExpectedChapters: array[0..5] of string = (
        'Getting started', 'Projects and files', 'Data', 'Model', 'Fitting',
        'The window');

{ Every word the client's chapters say, one explanation after another. }
function TClientExplanationsTest.GuideText: string;
var
    Providers: TExplanationProviders;
    Topics: TStringArray;
    E: TExplanation;
    i, j: longint;
begin
    Result := '';
    Providers := ClientExplanationProviders;
    Topics := StaticTopicsOf(Providers);
    for i := 0 to High(Topics) do
        if FindExplanationIn(Providers, Topics[i], E) then
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
    //  empty list would pass the coverage test below by checking nothing.
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

procedure TClientExplanationsTest.EveryDesignedMenuCommandIsNamedByItsPath;
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

procedure TClientExplanationsTest.EveryMenuCommandBuiltInCodeIsNamedByItsPath;
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
    //  must not say the opposite.
    Hint := DesignedActionHint('ActionSetBackgroundFraction');
    AssertTrue('the hint is read', Hint <> '');
    AssertTrue('it names the peak search: ' + Hint, Pos('peak', Hint) > 0);
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

initialization
    RegisterTest('unit', TClientExplanationsTest);
end.
