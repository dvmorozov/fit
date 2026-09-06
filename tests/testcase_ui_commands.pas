// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The one table every command surface is drawn from.)

WHY THESE TESTS EXIST. The window used to map commands onto widgets by hand, and
none of it could be checked: the mapping lived in a method that needs a window,
so a command bound to the wrong action, or to none, or ticked on an entry that
must not carry a tick, was found by clicking. The table is data now, and every
one of those is a question a test can ask.

The two that would cost the most are asked first: a target name that no action
answers to (a dead button, silently), and a row that follows a command whose
state nothing sets (a button that is never enabled, silently).
}
unit testcase_ui_commands;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    ui_commands, action_state, fit_client, int_ui_host, module_menu,
    tool_pane_layout;

type
    TUiCommandsTest = class(TTestCase)
    private
        FTable: TCommandTable;
        FCounts: TModelCounts;
        { Declarations as a module would make them. }
        function Decl(const AId, ACaption: string;
            AKind: TUiMenuKind = mkCommand): TUiMenuDecl;
        procedure Refresh(const AInputs: TUiInputs);
        { The table's row for AId, asserted to exist. }
        function RowOf(const AId: string): longint;
        function CaptionOf(const AId: string): string;
        function EnabledOf(const AId: string): boolean;
        { Whether any framework row follows ACommand. }
        function AnyRowFollows(ACommand: TUiCommand): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  SELF-ENFORCING: the enumeration and the table are two lists
        //  that have to agree, and nothing said so.
        procedure EveryCommandTheWindowDecidesIsDrawnSomewhere;
        //  What the framework declares.
        procedure EveryRowHasAnIdAndTheyAreUnique;
        procedure EveryDrivingRowNamesATarget;
        procedure ARowThatRendersNowhereStillDrivesSomething;
        procedure ThePaneRowsAreGroupedInTheOrderOfTheWork;
        procedure TheFitRowComesLast;
        procedure OnlyTheDeclaredCheckableTargetsCarryATick;
        procedure DeletingACurveIsRowScopedAndNotOnThePane;
        procedure EveryPaneCaptionFitsTheDesignedPane;

        //  Headings and their counts.
        procedure AHeadingCarriesItsCount;
        procedure AZeroCountIsShownRatherThanHidden;
        procedure AGroupThatCountsNothingKeepsItsBareCaption;
        procedure AModulesGroupCountsNothing;

        //  Availability, from action_state.
        procedure WithNothingOpenTheModelRowsAreAllDisabled;
        procedure OpeningAProfileEnablesThem;
        procedure DuringAFitOnlyStopIsOffered;
        procedure DeleteCurveNeedsARowThatNamesACurve;
        procedure DeleteCurveIsRefusedDuringAFit;

        //  Picking: the pressed state and the caption come from one mode.
        procedure APickingRowIsPressedOnlyInItsOwnMode;
        procedure APickingRowsMenuCaptionSaysStopInItsOwnMode;
        procedure ANonPickingRowNeverAsksForACaptionChange;
        procedure EveryPickingEntryIsRepresentedExactlyOnce;

        //  Where a click goes.
        procedure AFrameworkRowTargetsItsAction;
        procedure ASubmenuParentTargetsNothing;
        procedure AModuleRowTargetsTheModule;

        //  Modules.
        procedure AModuleDeclaringOnlyMenuEntriesAddsNoPaneRows;
        procedure AModuleRowOptedIntoThePaneAppearsThere;
        procedure AModuleRowUsesItsShortCaptionWhenItHasOne;
        procedure AModuleRowFallsBackToItsFullCaption;
        procedure SeparatorsAndSubmenusAreNotCommands;
        procedure ARowNamingAnUndeclaredGroupSitsUnderTheModule;
        procedure ARowNamingADeclaredGroupSitsUnderIt;
        procedure AGroupWhoseSubmenuIsUnnamedFallsBackToTheModule;
        procedure AModuleThatAsksForNoButtonsAddsNoHeading;

        //  Latches: a module's toggles and radios, pressed in the pane.
        procedure AModuleToggleIsALatch;
        procedure AModuleRadioIsALatch;
        procedure AModuleCommandIsNot;
        procedure ARadioStartsPressedWhenItIsTheCheckedOne;
        procedure RadiosOfOneGroupLatchTogether;
        procedure RadiosOfAnotherGroupLatchApart;
        procedure ATogglesLatchIsItsOwn;
        procedure AFrameworkPickingRowLatchesOnItsOwn;
        procedure ChoosingARadioReleasesItsSiblings;
        procedure ChoosingARadioLeavesTheOtherGroupAlone;
        procedure ChoosingIsForRadiosOnly;
        procedure ChoosingOutsideTheTableIsNotAnError;
        procedure TickingOneRadioThroughTheHostReleasesTheOthers;
        procedure TheLatchGroupOfARowThatIsNotThereIsItsOwnIndex;
        procedure TwoModulesWithTheSameIdDoNotCollide;
        //  The hint, taken from the widget the row drives.
        procedure ARowTakesTheHintOfTheActionItDrives;
        procedure AModulesOwnHintIsNotOverwritten;
        procedure AnActionWithNoHintLeavesTheRowWithout;
        procedure AdoptingOntoARowThatIsNotThereIsNotAnError;

        //  Asking for something that is not there.
        procedure ARowOutsideTheTableAnswersEmptily;
        procedure AnEmptyIdMatchesNoRow;
        procedure NorDoesAnEmptyModuleId;
        procedure AModuleRowStartsAvailable;
        procedure AModuleCanDisableItsOwnRow;
        procedure AModuleCanTickItsOwnRow;
        procedure ThatSurvivesARefreshThatDoesNotMentionIt;
        procedure AModuleRowIgnoresTheFrameworksState;
    end;

implementation

const
    //  The rows the Tools pane draws, by id, so a test naming one is naming
    //  something the table promises to have.
    PositionsPick = 'PositionsPick';
    IntervalsPick = 'IntervalsPick';
    BackgroundPick = 'BackgroundPick';
    FitStart = 'FitStart';
    FitStop = 'FitStop';

procedure TUiCommandsTest.SetUp;
begin
    FTable := TCommandTable.Create;
    FTable.AddFrameworkCommands;
    FCounts := EmptyModelCounts;
end;

procedure TUiCommandsTest.TearDown;
begin
    FTable.Free;
    FTable := nil;
end;

function TUiCommandsTest.Decl(const AId, ACaption: string;
    AKind: TUiMenuKind): TUiMenuDecl;
begin
    Result := Default(TUiMenuDecl);
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Kind := AKind;
end;

procedure TUiCommandsTest.Refresh(const AInputs: TUiInputs);
begin
    FTable.Refresh(CommandStates(AInputs), AInputs.Selection, FCounts);
end;

function TUiCommandsTest.RowOf(const AId: string): longint;
begin
    Result := FTable.IndexOfId(AId);
    AssertTrue('the table has a row for ' + AId, Result >= 0);
end;

function TUiCommandsTest.CaptionOf(const AId: string): string;
begin
    Result := FTable.Item(RowOf(AId)).PaneCaption;
end;

function TUiCommandsTest.EnabledOf(const AId: string): boolean;
begin
    Result := FTable.IsEnabled(RowOf(AId));
end;

{ ---- what the framework declares ---- }

procedure TUiCommandsTest.EveryRowHasAnIdAndTheyAreUnique;
var
    i: longint;
    Seen: TStringList;
begin
    Seen := TStringList.Create;
    try
        for i := 0 to FTable.Count - 1 do
        begin
            AssertTrue('row ' + IntToStr(i) + ' has an id',
                FTable.Item(i).Id <> '');
            AssertTrue('the id ' + FTable.Item(i).Id + ' is used once',
                Seen.IndexOf(FTable.Item(i).Id) < 0);
            Seen.Add(FTable.Item(i).Id);
        end;
    finally
        Seen.Free;
    end;
end;

procedure TUiCommandsTest.EveryDrivingRowNamesATarget;
var
    i: longint;
begin
    //  THE EXPENSIVE MISTAKE. A framework row with no target is a command the
    //  window will never enable or disable, and nothing about it looks wrong.
    for i := 0 to FTable.Count - 1 do
        if FTable.Item(i).ModuleIndex < 0 then
            AssertTrue(FTable.Item(i).Id + ' names a component',
                FTable.Item(i).TargetName <> '');
end;

procedure TUiCommandsTest.ARowThatRendersNowhereStillDrivesSomething;
var
    i: longint;
    D: TCommandDecl;
begin
    //  A row is worth having if it renders OR drives. One that does neither is
    //  a line nobody reads.
    for i := 0 to FTable.Count - 1 do
    begin
        D := FTable.Item(i);
        AssertTrue(D.Id + ' either renders or drives',
            (D.PaneCaption <> '') or (D.TargetName <> ''));
    end;
end;

procedure TUiCommandsTest.ThePaneRowsAreGroupedInTheOrderOfTheWork;
var
    G: TGroupList;
begin
    G := FTable.PaneGroups;
    AssertEquals('four groups', 4, Length(G));
    AssertEquals('place the curves first', GroupPositions, G[0]);
    AssertEquals('then say what to fit', GroupIntervals, G[1]);
    AssertEquals('then take the background off', GroupBackground, G[2]);
    AssertEquals('then fit', GroupFit, G[3]);
end;

procedure TUiCommandsTest.TheFitRowComesLast;
var
    i, FitAt, Last: longint;
begin
    //  THE ONLY EMPHASIS IT GETS. Fit used to carry a flag that drew it double
    //  width, and every button is one width now - so its place in the table is
    //  what puts it at the bottom of the pane, where the work ends.
    FitAt := RowOf(FitStart);
    Last := FitAt;
    for i := 0 to FTable.Count - 1 do
        if (FTable.Item(i).Surface in [csPane, csBoth]) and
            (FTable.Item(i).Scope = scGlobal) then
            Last := i;
    AssertTrue('Fit is at the end of what the pane draws', FitAt >= Last - 1);
end;

procedure TUiCommandsTest.OnlyTheDeclaredCheckableTargetsCarryATick;
var
    i: longint;
    D: TCommandDecl;
begin
    //  WithChecked on a target that was never declared checkable is how a
    //  widget set comes to destroy a menu entry under an open menu - see
    //  TFormMain.DeclareCheckableMenuEntries and ui_menus. The three picking
    //  rows are deliberately NOT ticked: their mode is said by the caption.
    for i := 0 to FTable.Count - 1 do
    begin
        D := FTable.Item(i);
        if D.HasPicking then
            AssertFalse(D.Id + ' says its mode by its caption, not a tick',
                D.WithChecked);
    end;
end;

procedure TUiCommandsTest.DeletingACurveIsRowScopedAndNotOnThePane;
var
    D: TCommandDecl;
begin
    D := FTable.Item(RowOf(CmdDeleteCurve));
    AssertTrue('it needs a row to act on', D.Scope = scRow);
    //  A global button for it would be enabled from a selection the user
    //  cannot see from the Tools pane.
    AssertTrue('so it is not on the pane', D.Surface = csMenu);
end;

procedure TUiCommandsTest.EveryPaneCaptionFitsTheDesignedPane;
var
    i: longint;
    D: TCommandDecl;
begin
    //  A PANE CAPTION IS SPENT, not free. Every button in the pane is one
    //  width, and that width is set by the longest caption in it - so one long
    //  caption widens all of them, which widens the pane, which moves the
    //  splitter and shrinks the chart. The budget is the framework's own, and
    //  it is stated in one place because a module has to spend it too.
    for i := 0 to FTable.Count - 1 do
    begin
        D := FTable.Item(i);
        if D.Surface in [csPane, csBoth] then
            AssertTrue(D.Id + ' fits the designed pane: "' + D.PaneCaption + '"',
                PaneCaptionFits(D.PaneCaption));
    end;
end;

{ ---- headings ---- }

procedure TUiCommandsTest.AHeadingCarriesItsCount;
begin
    FCounts.Positions := 3;
    AssertEquals('Positions (3)', GroupHeading(GroupPositions, FCounts));
end;

procedure TUiCommandsTest.AZeroCountIsShownRatherThanHidden;
begin
    //  "Positions (0)" says the model is empty. A bare heading says nothing.
    AssertEquals('Positions (0)', GroupHeading(GroupPositions, FCounts));
end;

procedure TUiCommandsTest.AGroupThatCountsNothingKeepsItsBareCaption;
begin
    AssertEquals(GroupFit, GroupHeading(GroupFit, FCounts));
end;

procedure TUiCommandsTest.AModulesGroupCountsNothing;
begin
    AssertEquals('waves', GroupHeading('waves', FCounts));
end;

{ ---- availability ---- }

procedure TUiCommandsTest.WithNothingOpenTheModelRowsAreAllDisabled;
begin
    Refresh(EmptyUiInputs);
    AssertFalse('pick positions', EnabledOf(PositionsPick));
    AssertFalse('pick intervals', EnabledOf(IntervalsPick));
    AssertFalse('pick background', EnabledOf(BackgroundPick));
    AssertFalse('fit', EnabledOf(FitStart));
end;

procedure TUiCommandsTest.OpeningAProfileEnablesThem;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    Refresh(I);
    AssertTrue('pick positions', EnabledOf(PositionsPick));
    AssertTrue('pick intervals', EnabledOf(IntervalsPick));
    AssertTrue('pick background', EnabledOf(BackgroundPick));
end;

procedure TUiCommandsTest.DuringAFitOnlyStopIsOffered;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    I.Async := AsyncWorks;
    Refresh(I);
    AssertFalse('no second fit', EnabledOf(FitStart));
    AssertTrue('but it can be stopped', EnabledOf(FitStop));
    AssertFalse('and the model cannot be edited', EnabledOf(PositionsPick));
end;

procedure TUiCommandsTest.DeleteCurveNeedsARowThatNamesACurve;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    Refresh(I);
    AssertFalse('nothing selected', EnabledOf(CmdDeleteCurve));

    I.ModelRowNamesACurve := True;
    Refresh(I);
    AssertTrue('a curve is selected', EnabledOf(CmdDeleteCurve));
end;

procedure TUiCommandsTest.DeleteCurveIsRefusedDuringAFit;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    I.ModelRowNamesACurve := True;
    I.Async := AsyncWorks;
    Refresh(I);
    //  Editing the model under a running optimiser is what the whole
    //  during-a-fit override exists to prevent.
    AssertFalse('not while a fit runs', EnabledOf(CmdDeleteCurve));
end;

{ ---- picking ---- }

procedure TUiCommandsTest.APickingRowIsPressedOnlyInItsOwnMode;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;

    I.Selection := ModeSelectCurvePositions;
    Refresh(I);
    AssertTrue('positions is pressed', FTable.IsDown(RowOf(PositionsPick)));
    AssertFalse('background is not', FTable.IsDown(RowOf(BackgroundPick)));

    I.Selection := ModeSelectBackground;
    Refresh(I);
    AssertFalse('positions is released', FTable.IsDown(RowOf(PositionsPick)));
    AssertTrue('background is pressed', FTable.IsDown(RowOf(BackgroundPick)));

    I.Selection := ModeSelectNothing;
    Refresh(I);
    AssertFalse('neither, with no mode', FTable.IsDown(RowOf(PositionsPick)));
    AssertFalse('neither, with no mode', FTable.IsDown(RowOf(BackgroundPick)));
end;

procedure TUiCommandsTest.APickingRowsMenuCaptionSaysStopInItsOwnMode;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;

    I.Selection := ModeSelectCurvePositions;
    Refresh(I);
    //  THE SAME CLICK the pressed button describes. Both come from
    //  action_state, which is what stops a caption saying "stop" on an entry
    //  that starts something.
    AssertEquals(POSITIONS_STOP_CAPTION,
        FTable.MenuCaption(RowOf(PositionsPick)));
    AssertEquals(PICKING_START_CAPTION,
        FTable.MenuCaption(RowOf(BackgroundPick)));

    I.Selection := ModeSelectNothing;
    Refresh(I);
    AssertEquals(POSITIONS_START_CAPTION,
        FTable.MenuCaption(RowOf(PositionsPick)));
end;

procedure TUiCommandsTest.ANonPickingRowNeverAsksForACaptionChange;
var
    I: TUiInputs;
begin
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    I.Selection := ModeSelectCurvePositions;
    Refresh(I);
    //  Empty means "leave the caption alone", which is what the .lfm already
    //  says for every entry that does not toggle a mode.
    AssertEquals('', FTable.MenuCaption(RowOf(FitStart)));
    AssertEquals('', FTable.MenuCaption(RowOf('PositionsAuto')));
end;

procedure TUiCommandsTest.EveryPickingEntryIsRepresentedExactlyOnce;
var
    i: longint;
    Counts: array[TPickingEntry] of longint;
    E: TPickingEntry;
begin
    for E := Low(TPickingEntry) to High(TPickingEntry) do
        Counts[E] := 0;
    for i := 0 to FTable.Count - 1 do
        if FTable.Item(i).HasPicking then
            Inc(Counts[FTable.Item(i).Picking]);
    //  Two rows claiming one mode would both light up and both read "stop".
    for E := Low(TPickingEntry) to High(TPickingEntry) do
        AssertEquals('one row per picking mode', 1, Counts[E]);
end;

{ ---- where a click goes ---- }

procedure TUiCommandsTest.AFrameworkRowTargetsItsAction;
var
    T: TCommandTarget;
begin
    T := FTable.TargetOf(RowOf(FitStart));
    AssertTrue('an action', T.Kind = ctAction);
    AssertEquals('ActionMinimizeDifference', T.ActionName);
end;

procedure TUiCommandsTest.ASubmenuParentTargetsNothing;
var
    T: TCommandTarget;
begin
    //  Opening a submenu is not a command, so clicking its row means nothing.
    T := FTable.TargetOf(RowOf('CurvePositionsMenu'));
    AssertTrue('nothing to run', T.Kind = ctNothing);
end;

procedure TUiCommandsTest.AModuleRowTargetsTheModule;
var
    D: TUiMenuDeclArray;
    T: TCommandTarget;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark bounds');
    FTable.AddModuleCommands(2, 'waves', D);

    T := FTable.TargetOf(RowOf('waves.mark'));
    AssertTrue('routed back', T.Kind = ctModuleCommand);
    AssertEquals('to that module', 2, T.ModuleIndex);
    //  ITS OWN ID, not the table's prefixed one: the module named the entry and
    //  must get its own name back.
    AssertEquals('mark', T.CommandId);
end;

{ ---- modules ---- }

procedure TUiCommandsTest.AModuleDeclaringOnlyMenuEntriesAddsNoPaneRows;
var
    D: TUiMenuDeclArray;
    Before, After: longint;
begin
    Before := Length(FTable.IndicesFor(csPane, scGlobal));
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark bounds');
    FTable.AddModuleCommands(0, 'waves', D);
    After := Length(FTable.IndicesFor(csPane, scGlobal));
    //  A module written before the pane existed leaves Surface at csMenu, and
    //  its arrival must change nothing for that module.
    AssertEquals('no pane rows added', Before, After);
end;

procedure TUiCommandsTest.AModuleRowOptedIntoThePaneAppearsThere;
var
    D: TUiMenuDeclArray;
    Rows: TIndexList;
    i: longint;
    Found: boolean;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark bounds');
    D[0].Surface := csBoth;
    FTable.AddModuleCommands(0, 'waves', D);

    Rows := FTable.IndicesFor(csPane, scGlobal);
    Found := False;
    for i := 0 to High(Rows) do
        if FTable.Item(Rows[i]).Id = 'waves.mark' then
            Found := True;
    AssertTrue('the module row is on the pane', Found);
end;

procedure TUiCommandsTest.AModuleRowUsesItsShortCaptionWhenItHasOne;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark wave bounds on the chart');
    D[0].ShortCaption := 'Mark';
    FTable.AddModuleCommands(0, 'waves', D);
    AssertEquals('Mark', CaptionOf('waves.mark'));
end;

procedure TUiCommandsTest.AModuleRowFallsBackToItsFullCaption;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    FTable.AddModuleCommands(0, 'waves', D);
    AssertEquals('Mark', CaptionOf('waves.mark'));
end;

procedure TUiCommandsTest.SeparatorsAndSubmenusAreNotCommands;
var
    D: TUiMenuDeclArray;
    Before: longint;
begin
    Before := FTable.Count;
    SetLength(D, 2);
    D[0] := Decl('sep', '', mkSeparator);
    D[1] := Decl('sub', 'More', mkSubmenu);
    FTable.AddModuleCommands(0, 'waves', D);
    //  A separator draws a line and a submenu opens one. Neither answers a
    //  click, so neither belongs in a table of commands.
    AssertEquals('nothing added', Before, FTable.Count);
end;

procedure TUiCommandsTest.ARowNamingAnUndeclaredGroupSitsUnderTheModule;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    D[0].Surface := csBoth;
    D[0].PaneGroup := 'nosuch';
    FTable.AddModuleCommands(0, 'waves', D);
    //  STILL SHOWN, under the module's own heading. A missing entry is
    //  invisible, and invisible is how a whole pack was once unreachable.
    //
    //  CAPITALISED, by the same rule that captions the module's root entry in
    //  the menu bar: the heading and the menu name one thing, and "waves" over
    //  the buttons beside "Waves" in the menu reads as two.
    AssertEquals('Waves', FTable.Item(RowOf('waves.mark')).Group);
end;

procedure TUiCommandsTest.ARowNamingADeclaredGroupSitsUnderIt;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('markup', 'Markup', mkSubmenu);
    D[1] := Decl('mark', 'Mark');
    D[1].Surface := csBoth;
    D[1].PaneGroup := 'markup';
    FTable.AddModuleCommands(0, 'waves', D);
    //  THE HEADING READS AS THE MENU READS. PaneGroup names the submenu by its
    //  id, which is the module's own word for it - a heading saying
    //  "waves.degrees" over the buttons is a leaked identifier, and the
    //  submenu already carries the words a user was meant to see.
    AssertEquals('Markup', FTable.Item(RowOf('waves.mark')).Group);
end;

procedure TUiCommandsTest.AGroupWhoseSubmenuIsUnnamedFallsBackToTheModule;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('markup', '', mkSubmenu);
    D[1] := Decl('mark', 'Mark');
    D[1].Surface := csBoth;
    D[1].PaneGroup := 'markup';
    FTable.AddModuleCommands(0, 'waves', D);
    //  A blank heading is a group the user cannot name; the module's own name
    //  is the fallback the undeclared case already uses.
    AssertEquals('Waves', FTable.Item(RowOf('waves.mark')).Group);
end;

procedure TUiCommandsTest.AModuleThatAsksForNoButtonsAddsNoHeading;
var
    D: TUiMenuDeclArray;
    G: TGroupList;
    i: longint;
begin
    SetLength(D, 1);
    D[0] := Decl('act', 'Act');
    FTable.AddModuleCommands(0, 'waves', D);
    //  A HEADING OVER NOTHING. The groups come from the rows, so a module that
    //  declared only menu entries must contribute no heading either - one with
    //  no buttons under it is a section the user cannot use and cannot dismiss.
    G := FTable.PaneGroups;
    for i := 0 to High(G) do
        AssertTrue('no heading for a module with no pane rows',
            G[i] <> ModuleRootCaption('waves'));
end;

{ ---- latches ---- }

procedure TUiCommandsTest.AModuleToggleIsALatch;
var
    D: TUiMenuDeclArray;
begin
    //  A TOGGLE IS ON OR OFF, and a button that looks the same either way says
    //  neither. The framework's picking rows latch for the same reason; a
    //  module's toggle is the same claim, made by a module.
    SetLength(D, 1);
    D[0] := Decl('mode', 'Mode', mkToggle);
    D[0].Surface := csBoth;
    FTable.AddModuleCommands(0, 'waves', D);
    AssertTrue('a toggle latches', FTable.Item(RowOf('waves.mode')).Latching);
end;

procedure TUiCommandsTest.AModuleRadioIsALatch;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('deep', 'All', mkRadio);
    D[0].Surface := csBoth;
    FTable.AddModuleCommands(0, 'waves', D);
    AssertTrue('a radio latches', FTable.Item(RowOf('waves.deep')).Latching);
end;

procedure TUiCommandsTest.AModuleCommandIsNot;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('act', 'Act');
    D[0].Surface := csBoth;
    FTable.AddModuleCommands(0, 'waves', D);
    //  Writing Down on a plain button leaves one looking held after a click.
    AssertFalse('a command does not', FTable.Item(RowOf('waves.act')).Latching);
end;

procedure TUiCommandsTest.ARadioStartsPressedWhenItIsTheCheckedOne;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].Surface := csBoth;
    D[0].RadioGroup := 7;
    D[0].Checked := True;
    D[1] := Decl('top', 'Top', mkRadio);
    D[1].Surface := csBoth;
    D[1].RadioGroup := 7;
    FTable.AddModuleCommands(0, 'waves', D);
    Refresh(EmptyUiInputs);
    //  THE DECLARATION SAYS WHICH ONE IS ON. The menu shows that tick from the
    //  first draw, and a pane that started with none would say the setting is
    //  unset when it has a value.
    AssertTrue('the declared one is pressed', FTable.IsDown(RowOf('waves.all')));
    AssertFalse('and only it', FTable.IsDown(RowOf('waves.top')));
end;

procedure TUiCommandsTest.RadiosOfOneGroupLatchTogether;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].RadioGroup := 7;
    D[1] := Decl('top', 'Top', mkRadio);
    D[1].RadioGroup := 7;
    FTable.AddModuleCommands(0, 'waves', D);
    //  ONE PRESSED AT A TIME is the whole meaning of a radio group, and the
    //  pane expresses it by giving those buttons one latch group.
    AssertEquals('one latch group', FTable.LatchGroup(RowOf('waves.all')),
        FTable.LatchGroup(RowOf('waves.top')));
end;

procedure TUiCommandsTest.RadiosOfAnotherGroupLatchApart;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].RadioGroup := 7;
    D[1] := Decl('wedge', 'Contracting', mkRadio);
    D[1].RadioGroup := 8;
    FTable.AddModuleCommands(0, 'waves', D);
    AssertTrue('two settings, two groups',
        FTable.LatchGroup(RowOf('waves.all')) <>
        FTable.LatchGroup(RowOf('waves.wedge')));
end;

procedure TUiCommandsTest.ATogglesLatchIsItsOwn;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('mode', 'Mode', mkToggle);
    D[1] := Decl('other', 'Other', mkToggle);
    FTable.AddModuleCommands(0, 'waves', D);
    //  Two toggles are two independent facts. Sharing a latch group would make
    //  turning one on turn the other off.
    AssertTrue('independent',
        FTable.LatchGroup(RowOf('waves.mode')) <>
        FTable.LatchGroup(RowOf('waves.other')));
end;

procedure TUiCommandsTest.AFrameworkPickingRowLatchesOnItsOwn;
begin
    //  The framework's latches predate this and must keep pressing
    //  independently - two picking buttons in one group would release each
    //  other and lie about which mode is running.
    AssertTrue('its own group',
        FTable.LatchGroup(RowOf(PositionsPick)) <>
        FTable.LatchGroup(RowOf(BackgroundPick)));
end;

procedure TUiCommandsTest.ChoosingARadioReleasesItsSiblings;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].RadioGroup := 7;
    D[0].Checked := True;
    D[1] := Decl('top', 'Top', mkRadio);
    D[1].RadioGroup := 7;
    FTable.AddModuleCommands(0, 'waves', D);

    //  THE FRAMEWORK OWNS THE CHOICE, because a module need not say which of
    //  its radios is on: the menu ticks the clicked entry itself, and a pane
    //  that waited to be told would snap back to the old one on the next poll.
    FTable.ChooseModuleRow(RowOf('waves.top'));
    Refresh(EmptyUiInputs);
    AssertTrue('the chosen one', FTable.IsDown(RowOf('waves.top')));
    AssertFalse('and the one it replaced', FTable.IsDown(RowOf('waves.all')));
end;

procedure TUiCommandsTest.ChoosingARadioLeavesTheOtherGroupAlone;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].RadioGroup := 7;
    D[0].Checked := True;
    D[1] := Decl('wedge', 'Contracting', mkRadio);
    D[1].RadioGroup := 8;
    FTable.AddModuleCommands(0, 'waves', D);

    FTable.ChooseModuleRow(RowOf('waves.wedge'));
    Refresh(EmptyUiInputs);
    AssertTrue('the other setting keeps its value',
        FTable.IsDown(RowOf('waves.all')));
end;

procedure TUiCommandsTest.ChoosingIsForRadiosOnly;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mode', 'Mode', mkToggle);
    FTable.AddModuleCommands(0, 'waves', D);
    //  A TOGGLE'S TICK IS THE MODULE'S TO SAY. A module's marking mode outlives
    //  the click that started it and ends in ways the click never hears about,
    //  so the click must not decide it.
    FTable.ChooseModuleRow(RowOf('waves.mode'));
    Refresh(EmptyUiInputs);
    AssertFalse('untouched by the click', FTable.IsDown(RowOf('waves.mode')));
end;

procedure TUiCommandsTest.ChoosingOutsideTheTableIsNotAnError;
begin
    //  Reached from a click carrying a widget's Tag, which can go stale between
    //  a rebuild and the click that was already on its way.
    FTable.ChooseModuleRow(-1);
    FTable.ChooseModuleRow(FTable.Count);
end;

procedure TUiCommandsTest.TickingOneRadioThroughTheHostReleasesTheOthers;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 2);
    D[0] := Decl('all', 'All', mkRadio);
    D[0].RadioGroup := 7;
    D[0].Checked := True;
    D[1] := Decl('top', 'Top', mkRadio);
    D[1].RadioGroup := 7;
    FTable.AddModuleCommands(0, 'waves', D);

    //  TICKING ONE OF A SET IS CHOOSING IT. A module that ticks a radio without
    //  unticking its siblings - which is what a module's own radio menu does, and
    //  what a menu's own radio group makes correct - means the choice moved,
    //  not that two settings hold at once.
    FTable.SetModuleChecked('top', True);
    Refresh(EmptyUiInputs);
    AssertTrue('the one it ticked', FTable.IsDown(RowOf('waves.top')));
    AssertFalse('and only it', FTable.IsDown(RowOf('waves.all')));
end;

procedure TUiCommandsTest.TheLatchGroupOfARowThatIsNotThereIsItsOwnIndex;
begin
    //  Asked while a button is being built, from an index the table answered a
    //  moment earlier - so a stale one must answer rather than fault, and
    //  answering with the index itself is a group of one, which is what a row
    //  that shares nothing needs.
    AssertEquals('past the end', FTable.Count, FTable.LatchGroup(FTable.Count));
    AssertEquals('and before the start', -1, FTable.LatchGroup(-1));
end;

procedure TUiCommandsTest.TwoModulesWithTheSameIdDoNotCollide;
var
    D: TUiMenuDeclArray;
    A, B: longint;
begin
    SetLength(D, 1);
    D[0] := Decl('act', 'Act');
    FTable.AddModuleCommands(0, 'alpha', D);
    FTable.AddModuleCommands(1, 'beta', D);

    A := RowOf('alpha.act');
    B := RowOf('beta.act');
    AssertTrue('two distinct rows', A <> B);
    //  Addressed by module AND id, because the id alone is the module's own and
    //  two modules may pick the same word.
    AssertEquals('the first module''s row', A,
        FTable.IndexOfModuleRow(0, 'act'));
    AssertEquals('the second module''s row', B,
        FTable.IndexOfModuleRow(1, 'act'));
end;

procedure TUiCommandsTest.ARowTakesTheHintOfTheActionItDrives;
var
    i: longint;
begin
    //  ONE TEXT FOR ONE COMMAND. The actions carry the hints, beside the
    //  captions the menus show; a tool button that declared its own would agree
    //  with the menu on the day it was written and not after the first edit.
    i := RowOf(FitStart);
    AssertEquals('nothing declared here', '', FTable.Item(i).Hint);
    FTable.AdoptHint(i, 'Fits the model to the data');
    AssertEquals('what the action says', 'Fits the model to the data',
        FTable.Item(i).Hint);
end;

procedure TUiCommandsTest.AModulesOwnHintIsNotOverwritten;
var
    D: TUiMenuDeclArray;
    i: longint;
begin
    //  A MODULE'S ROW DRIVES NO WIDGET OF THE WINDOW'S, so its declaration is
    //  the only text there is - and adoption must not replace it with the hint
    //  of whatever happened to resolve.
    SetLength(D, 1);
    D[0] := Decl('act', 'Act');
    D[0].Hint := 'What the module says';
    FTable.AddModuleCommands(0, 'alpha', D);
    i := RowOf('alpha.act');
    FTable.AdoptHint(i, 'What some action says');
    AssertEquals('the module keeps its own', 'What the module says',
        FTable.Item(i).Hint);
end;

procedure TUiCommandsTest.AnActionWithNoHintLeavesTheRowWithout;
var
    i: longint;
begin
    //  ADOPTING NOTHING IS AN ANSWER. Not every action carries a hint, and a
    //  surface showing none is better than one inventing a caption as a hint -
    //  which is what a fallback to the caption would amount to.
    i := RowOf(FitStop);
    FTable.AdoptHint(i, '');
    AssertEquals('still none', '', FTable.Item(i).Hint);
end;

procedure TUiCommandsTest.AdoptingOntoARowThatIsNotThereIsNotAnError;
begin
    //  The window walks the table by index while resolving targets, and an
    //  index that has gone stale must answer rather than fault.
    FTable.AdoptHint(FTable.Count, 'anything');
    FTable.AdoptHint(-1, 'anything');
end;

procedure TUiCommandsTest.ARowOutsideTheTableAnswersEmptily;
begin
    //  A DEFAULT RECORD RATHER THAN A FAULT. The table is read from the state
    //  poll and from a menu click's Tag, twice a second and after every rebuild
    //  - so an index that has gone stale must answer, and a row with no id is
    //  something every caller here already handles.
    AssertEquals('past the end', '', FTable.Item(FTable.Count).Id);
    AssertEquals('well past it', '', FTable.Item(9999).Id);
    AssertEquals('and before the start', '', FTable.Item(-1).Id);
end;

procedure TUiCommandsTest.AnEmptyIdMatchesNoRow;
begin
    //  NOT THE FIRST ROW, which is what a bare loop would answer. A framework
    //  row that renders nowhere still has an id, and a caller asking for '' is
    //  asking for nothing - the Model panel does exactly that with no row
    //  selected.
    AssertEquals('nothing', -1, FTable.IndexOfId(''));
end;

procedure TUiCommandsTest.NorDoesAnEmptyModuleId;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('act', 'Act');
    FTable.AddModuleCommands(0, 'alpha', D);
    //  The same rule on the module-scoped lookup, which a module reaches
    //  through SetMenuEnabled with whatever id it holds.
    AssertEquals('nothing', -1, FTable.IndexOfModuleRow(0, ''));
end;

procedure TUiCommandsTest.AModuleRowStartsAvailable;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    FTable.AddModuleCommands(0, 'waves', D);
    Refresh(EmptyUiInputs);
    //  A module that never calls SetMenuEnabled is the ordinary case. A row
    //  that started disabled would be unreachable until it spoke.
    AssertTrue('available until the module says otherwise',
        EnabledOf('waves.mark'));
end;

procedure TUiCommandsTest.AModuleCanDisableItsOwnRow;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    FTable.AddModuleCommands(0, 'waves', D);
    FTable.SetModuleEnabled('mark', False);
    Refresh(EmptyUiInputs);
    AssertFalse('the module disabled it', EnabledOf('waves.mark'));
end;

procedure TUiCommandsTest.AModuleCanTickItsOwnRow;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mode', 'Mode', mkToggle);
    FTable.AddModuleCommands(0, 'waves', D);
    FTable.SetModuleChecked('mode', True);
    Refresh(EmptyUiInputs);
    AssertTrue('the module ticked it', FTable.IsDown(RowOf('waves.mode')));
end;

procedure TUiCommandsTest.ThatSurvivesARefreshThatDoesNotMentionIt;
var
    D: TUiMenuDeclArray;
    I: TUiInputs;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    FTable.AddModuleCommands(0, 'waves', D);
    FTable.SetModuleEnabled('mark', False);

    //  THE POLL MUST NOT UNDO THE MODULE. The state is held in the table
    //  rather than written straight to a widget precisely so that a module can
    //  speak while a menu is open and be obeyed on the next poll.
    I := EmptyUiInputs;
    I.Open := OpenSuccess;
    Refresh(I);
    Refresh(I);
    AssertFalse('still disabled two polls later', EnabledOf('waves.mark'));
end;

procedure TUiCommandsTest.AModuleRowIgnoresTheFrameworksState;
var
    D: TUiMenuDeclArray;
begin
    SetLength(D, 1);
    D[0] := Decl('mark', 'Mark');
    FTable.AddModuleCommands(0, 'waves', D);
    //  With nothing open every framework row is off; the module's is not,
    //  because only the module knows when its own command applies.
    Refresh(EmptyUiInputs);
    AssertFalse('the framework row is off', EnabledOf(PositionsPick));
    AssertTrue('the module row is not', EnabledOf('waves.mark'));
end;

function TUiCommandsTest.AnyRowFollows(ACommand: TUiCommand): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to FTable.Count - 1 do
        //  A module's rows follow no framework command, so only the
        //  framework's own are asked.
        if (FTable.Item(i).ModuleIndex < 0) and
            (FTable.Item(i).Follows = ACommand) then
            Exit(True);
end;

procedure TUiCommandsTest.EveryCommandTheWindowDecidesIsDrawnSomewhere;
var
    Cmd: TUiCommand;
begin
    //  THE MIRROR OF action_state's REACHABILITY CHECK, and the other half of
    //  the same failure. action_state decides whether a command is available;
    //  this table says which widget hears the answer. A command with a rule and
    //  no row is decided correctly and applied to nothing - the menu entry sits
    //  there in whatever state the .lfm left it, which is exactly as invisible
    //  as the unreachable export was.
    //
    //  Two lists that must agree and nothing checking it is how they come to
    //  disagree; this is what makes adding the next command cost a failing test
    //  rather than a silent gap.
    for Cmd := Low(TUiCommand) to High(TUiCommand) do
        AssertTrue('command ' + IntToStr(Ord(Cmd)) +
            ' is decided and nothing is drawn from it', AnyRowFollows(Cmd));
end;

initialization
    //  A unit test: records in, records out. No window, no widget, and no
    //  module - which is why the module half had never been exercised at all.
    RegisterTest('unit', TUiCommandsTest);
end.
