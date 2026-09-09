// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every command the window offers, declared once and rendered three
times.)

WHAT THIS REPLACES. The window mapped commands onto widgets by hand: some forty
lines in TFormMain.CheckState, one or two per widget, each naming an action or a
menu item and copying `Enabled` - and sometimes `Checked` - out of the state
action_state had just decided. That worked while there was one surface. A second
surface means a second such table, a third a third, and two tables that must
agree about which commands exist are two places to forget one.

So the table is data. action_state still decides WHETHER a command is available;
this unit says WHICH WIDGET says so, WHAT it is captioned in a panel too narrow
for the menu's wording, and WHERE it is shown. The window loops.

NOTHING HERE KNOWS WHAT A WIDGET IS. A target is a component NAME, resolved by
the window that owns the component. That is what lets the whole table be checked
by a test with no window: a typo in an action name is a failing test rather than
a button that quietly does nothing.

THE MENU KEEPS ITS OWN WORDING. A menu entry has room for "Start Manual
Selection"; a 189-pixel panel has room for "Pick". Both are stated, because
deriving either from the other gives a cramped menu or an unreadable panel.
}
unit ui_commands;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, action_state, fit_client, int_ui_host, module_menu;

type
    { What kind of component a row drives. The window maps these onto whatever
      it holds; this unit names no widget type. }
    TUiTargetKind = (
        //  A TAction, which carries Enabled and Checked to every widget bound
        //  to it.
        tkAction,
        //  A menu item addressed directly - a submenu parent, which no action
        //  drives because opening one is not a command.
        tkMenuItem
        );

    { One command, described once.

      A row may drive a widget, or render in a panel, or both. A row that drives
      nothing is a module's contribution; a row that renders nowhere is a
      command the menus offer and the panels do not. }
    TCommandDecl = record
        { Stable, and unique across the table. What a generated widget takes its
          name from. }
        Id: string;
        { Whose availability this row follows. }
        Follows: TUiCommand;

        //  ---- what it drives in the window
        { The component to apply the state to; empty when this row drives
          none. }
        TargetName: string;
        TargetKind: TUiTargetKind;
        { True when Checked is applied as well as Enabled. Stated per row rather
          than assumed, because an entry that is not declared checkable must not
          be ticked - see TFormMain.DeclareCheckableMenuEntries. }
        WithChecked: boolean;

        //  ---- what it renders as
        Surface: TCommandSurface;
        Scope: TCommandScope;
        { The section it sits under in a panel. }
        Group: string;
        { Short enough for a narrow panel; empty when this row renders
          nowhere. }
        PaneCaption: string;
        Hint: string;

        //  ---- picking
        { Set for the rows that enter a picking mode. The pressed state of a
          panel button and the Start/Stop caption of a menu entry are then both
          decided from the one mode, and cannot disagree. }
        HasPicking: boolean;
        Picking: TPickingEntry;

        //  ---- latching
        { True when the button for this row stays pressed to say something is
          on: a module's toggle, or one of its radio settings. The framework's
          picking rows say it through HasPicking, which is the same claim made
          from a mode the framework owns.

          A PLAIN COMMAND MUST NOT LATCH. Writing Down on one leaves a button
          looking held after the click that ran it. }
        Latching: boolean;
        { True for one of a mutually exclusive set. Radio is what makes
          ChooseModuleRow release the siblings, and RadioGroup is the module's
          own number for the set - two modules may both use 1. }
        Radio: boolean;
        RadioGroup: longint;

        //  ---- provenance
        { -1 for the framework's own rows; otherwise which module declared it,
          so a click can be routed back. }
        ModuleIndex: longint;
        { The module's own id for the row, which is what comes back on a
          click. }
        CommandId: string;
    end;

    TCommandDecls = array of TCommandDecl;

    TIndexList = array of longint;
    TGroupList = array of string;

const
    { The framework's own groups, in the order a panel shows them. Named so a
      test can assert the order without repeating the strings. }
    GroupPositions  = 'Positions';
    GroupIntervals  = 'Fit intervals';
    GroupBackground = 'Background';
    GroupFit        = 'Fit';

    { The one command this plan adds that the .lfm does not already carry. }
    CmdDeleteCurve = 'DeleteCurve';

type
    { How many of each thing the model holds - what a panel's headings count.
      Gathered by the caller, so nothing here reaches for it. }
    TModelCounts = record
        Positions: longint;
        Intervals: longint;
        BackgroundPoints: longint;
    end;

    { What clicking a row means, so no window decides it. }
    TCommandTargetKind = (ctNothing, ctAction, ctModuleCommand);

    TCommandTarget = record
        Kind: TCommandTargetKind;
        ActionName: string;
        ModuleIndex: longint;
        CommandId: string;
    end;

{ Every command the framework itself offers: the ones that only drive widgets,
  and the ones that also appear in the Tools pane.

  THE ORDER IS THE PANEL'S ORDER, which is the order of the work - place the
  positions, say what to fit, take the background off, fit. }
function FrameworkCommands: TCommandDecls;

function EmptyModelCounts: TModelCounts;

{ A heading with its count, or the bare caption for a group that counts
  nothing. }
function GroupHeading(const AGroup: string;
    const ACounts: TModelCounts): string;

type
    { The framework's commands plus whatever the modules added, and the state of
      each. ONE INSTANCE, rendered by the menus, by the Tools pane and by the
      Model panel's context menu. }
    TCommandTable = class
    private
        FItems: TCommandDecls;
        FEnabled: array of boolean;
        FDown: array of boolean;
        FMenuCaption: array of string;
        { What a module last said about its own rows. Held here rather than in a
          widget: a module may speak while a menu is open, and the window then
          applies it on the next poll rather than destroying the entry the user
          is standing in. }
        FModuleEnabled: array of boolean;
        FModuleChecked: array of boolean;
        procedure Append(const ADecl: TCommandDecl;
            AStartsChecked: boolean = False);
    public
        { The framework's own rows. Called once. }
        procedure AddFrameworkCommands;

        { One module's declarations, appended as its own group.

          A row naming a group no declaration provides is still SHOWN, under the
          module's own heading - the rule and the reason of module_menu, where a
          missing entry is invisible and invisible is how a whole pack was once
          unreachable. }
        procedure AddModuleCommands(AModuleIndex: longint; const AName: string;
            const ADecls: TUiMenuDeclArray);

        function Count: longint;
        function Item(AIndex: longint): TCommandDecl;
        { -1 when no row carries that id. }
        function IndexOfId(const AId: string): longint;
        function IndexOfModuleRow(AModuleIndex: longint;
            const AId: string): longint;

        { Takes the hint from the widget this row drives, so a tool button says
          what the menu entry for the same command says.

          WHY THE HINT IS NOT DECLARED HERE. The framework's rows name an
          action, and the actions carry hints already - in the designed form,
          beside the captions the menus show. Declaring them a second time in
          this table would be two texts for one command, which is the drift this
          whole table exists to prevent: they would agree on the day they were
          written and not after the first edit.

          A MODULE'S OWN HINT WINS, because a module's row drives no widget of
          the window's and its declaration is the only text there is. Adopting
          nothing is also an answer - an action with no hint leaves the row
          without one, and the surfaces show none rather than inventing one. }
        procedure AdoptHint(AIndex: longint; const AFromTarget: string);

        { Everything that changes, from the polled state. }
        procedure Refresh(const AStates: TCommandStates; AMode: TSelMode;
            const ACounts: TModelCounts);
        function IsEnabled(AIndex: longint): boolean;
        function IsDown(AIndex: longint): boolean;
        { Which rows press exclusively together, as one number per set. The
          radios of one group answer the same; everything else that latches
          answers only for itself, because two toggles are two independent
          facts and two picking buttons name two different modes. }
        function LatchGroup(AIndex: longint): longint;
        { For a picking row, what its MENU entry reads now. Empty for every
          other row, meaning "leave the caption alone". }
        function MenuCaption(AIndex: longint): string;

        procedure SetModuleEnabled(const AId: string; AEnabled: boolean);
        procedure SetModuleChecked(const AId: string; AChecked: boolean);

        { One of a module's radio settings was chosen - clicked in the pane, or
          in the menu, which are the same choice made twice.

          THE FRAMEWORK OWNS IT, and that is the whole point: a menu ticks the
          radio entry the user clicked without asking anyone, so a module need
          never say which of its settings is on - and the module this was built for
          does not. A pane
          that waited to be told would show the click, then be written back to
          the old choice by the next state poll. Does nothing for any other kind
          of row: a toggle's tick is the module's to state, because its mode
          outlives the click and ends in ways the click never hears about. }
        procedure ChooseModuleRow(AIndex: longint);

        function TargetOf(AIndex: longint): TCommandTarget;

        { The rows a surface draws, in table order. }
        function IndicesFor(ASurface: TCommandSurface;
            AScope: TCommandScope): TIndexList;
        { The groups the pane shows, in the order the rows first name them. }
        function PaneGroups: TGroupList;
    end;

implementation

function EmptyModelCounts: TModelCounts;
begin
    Result.Positions := 0;
    Result.Intervals := 0;
    Result.BackgroundPoints := 0;
end;

function GroupHeading(const AGroup: string;
    const ACounts: TModelCounts): string;

    function WithCount(ACount: longint): string;
    begin
        Result := AGroup + ' (' + IntToStr(ACount) + ')';
    end;

begin
    //  A COUNT IS THE ONE THING THE MENUS CANNOT SHOW, which is why the panel
    //  states it. Zero is shown rather than hidden: "Positions (0)" says the
    //  model is empty, and a bare heading says nothing at all.
    if AGroup = GroupPositions then
        Result := WithCount(ACounts.Positions)
    else if AGroup = GroupIntervals then
        Result := WithCount(ACounts.Intervals)
    else if AGroup = GroupBackground then
        Result := WithCount(ACounts.BackgroundPoints)
    else
        //  The Fit group, and every group a module named: nothing here counts
        //  what those hold.
        Result := AGroup;
end;

{ One row of the framework's table. Every argument is spelled at the call site
  below, so the table reads as a table. }
function Cmd(const AId: string; AFollows: TUiCommand;
    const ATarget: string; AKind: TUiTargetKind; AWithChecked: boolean;
    ASurface: TCommandSurface; const AGroup, APaneCaption: string): TCommandDecl;
begin
    Result := Default(TCommandDecl);
    Result.Id := AId;
    Result.Follows := AFollows;
    Result.TargetName := ATarget;
    Result.TargetKind := AKind;
    Result.WithChecked := AWithChecked;
    Result.Surface := ASurface;
    Result.Scope := scGlobal;
    Result.Group := AGroup;
    Result.PaneCaption := APaneCaption;
    Result.ModuleIndex := -1;
end;

function FrameworkCommands: TCommandDecls;
var
    N: longint;

    procedure Add(const ADecl: TCommandDecl);
    begin
        SetLength(Result, N + 1);
        Result[N] := ADecl;
        Inc(N);
    end;

    procedure AddPicking(const ADecl: TCommandDecl; AEntry: TPickingEntry);
    var
        D: TCommandDecl;
    begin
        D := ADecl;
        D.HasPicking := True;
        D.Picking := AEntry;
        Add(D);
    end;

begin
    Result := nil;
    N := 0;

    //  ---- The model-building loop, in the order it is worked. These are the
    //  rows the Tools pane draws; they drive the same actions the menus do.
    AddPicking(Cmd('PositionsPick', ucCurvePositions,
        'ActionSelectCurvePositionsManually', tkAction, False,
        csBoth, GroupPositions, 'Pick'), peCurvePositions);
    Add(Cmd('PositionsAuto', ucCurvePositions,
        'ActionComputCurvePositions', tkAction, False,
        csBoth, GroupPositions, 'Auto'));
    Add(Cmd('PositionsClear', ucCurvePositions,
        'ActionRemoveCurvePositions', tkAction, False,
        csBoth, GroupPositions, 'Clear'));

    AddPicking(Cmd('IntervalsPick', ucRFactorIntervals,
        'ActionSelectRFactorBoundsManually', tkAction, False,
        csBoth, GroupIntervals, 'Pick'), peIntervalBounds);
    Add(Cmd('IntervalsAuto', ucRFactorIntervals,
        'ActionComputeRFactorBounds', tkAction, False,
        csBoth, GroupIntervals, 'Auto'));
    Add(Cmd('IntervalsClear', ucRFactorIntervals,
        'ActionRemoveRFactorBounds', tkAction, False,
        csBoth, GroupIntervals, 'Clear'));

    AddPicking(Cmd('BackgroundPick', ucSubtractBackground,
        'ActionSelectBackgroundManually', tkAction, False,
        csBoth, GroupBackground, 'Pick'), peBackground);
    Add(Cmd('BackgroundAuto', ucSubtractBackground,
        'ActionComputeBackgroundPoints', tkAction, False,
        csBoth, GroupBackground, 'Auto'));
    Add(Cmd('BackgroundClear', ucSubtractBackground,
        'ActionRemoveBackgroundPoints', tkAction, False,
        csBoth, GroupBackground, 'Clear'));
    Add(Cmd('BackgroundSubtract', ucSubtractBackground,
        'ActionSubtractBackgroundAutomatically', tkAction, False,
        csBoth, GroupBackground, 'Subtract'));

    //  FIT IS LAST, and that is all the emphasis it gets. It used to carry a
    //  flag that drew it double width; a row of buttons in two sizes reads as
    //  two kinds of control, and the group heading already says what this one
    //  is. Its place at the end of the table is the order of the work.
    Add(Cmd('FitStart', ucMinimizeDifference,
        'ActionMinimizeDifference', tkAction, False,
        csBoth, GroupFit, 'Fit'));
    Add(Cmd('FitStop', ucStopFit,
        'ActionStopFit', tkAction, False,
        csBoth, GroupFit, 'Stop'));

    //  Deleting one curve is offered on a right-click over the Model panel and
    //  nowhere else: it needs a row to act on, so a global button for it would
    //  be enabled or disabled by a selection the user cannot see from there.
    Add(Cmd(CmdDeleteCurve, ucDeleteCurve,
        'ActionDeleteCurve', tkAction, False,
        csMenu, GroupPositions, 'Delete curve'));
    Result[High(Result)].Scope := scRow;

    //  ---- Everything else the window derives. These drive widgets and render
    //  in no panel, and they are here rather than in a second hand-written
    //  table for exactly that reason.

    //  File
    Add(Cmd('ReloadData', ucReloadData, 'ActionReloadData', tkAction, False,
        csMenu, '', ''));
    //  THE DOCUMENT AND THE EXPORTS. One row per command and one command per
    //  table, so a menu entry says what it will do without the reader having to
    //  know which tab is in front.
    Add(Cmd('NewProject', ucNewProject, 'ActionNewProject',
        tkAction, False, csMenu, '', ''));
    Add(Cmd('OpenProject', ucOpenProject, 'ActionOpenProject',
        tkAction, False, csMenu, '', ''));
    Add(Cmd('SaveProject', ucSaveProject, 'ActionSaveProject',
        tkAction, False, csMenu, '', ''));
    Add(Cmd('SaveProjectAs', ucSaveProjectAs, 'ActionSaveProjectAs',
        tkAction, False, csMenu, '', ''));
    Add(Cmd('ExportCurveParameters', ucExportCurveParameters,
        'ActionExportCurveParameters', tkAction, False, csMenu, '', ''));
    Add(Cmd('ExportSummaryTable', ucExportSummaryTable,
        'ActionExportSummaryTable', tkAction, False, csMenu, '', ''));

    //  Operation
    Add(Cmd('DoAllAutomatically', ucDoAllAutomatically,
        'ActionDoAllAutomatically', tkAction, False, csMenu, '', ''));
    Add(Cmd('SmoothProfile', ucSmoothProfile, 'ActionSmoothProfile',
        tkAction, False, csMenu, '', ''));
    Add(Cmd('MinimizeNumberOfCurves', ucMinimizeNumberOfCurves,
        'ActionMinimizeNumberOfCurves', tkAction, False, csMenu, '', ''));

    //  The background submenu and the entries under it move together: an entry
    //  left enabled beneath a disabled parent is unreachable but looks
    //  available, which is how "nothing happens when I click it" starts. The
    //  four actions above already follow ucSubtractBackground; the parent is
    //  what is left.
    Add(Cmd('BackgroundMenu', ucSubtractBackground, 'MenuSubtractBackground',
        tkMenuItem, False, csMenu, '', ''));
    Add(Cmd('SubtractBackgroundBySelectedPoints',
        ucSubtractBackgroundBySelectedPoints,
        'ActionSubtractBackgroundBySelectedPoints', tkAction, False,
        csMenu, '', ''));

    //  Dataset. The three that carry a tick are declared checkable in the
    //  window, which is why WithChecked is True only for them.
    Add(Cmd('SelectIntervalBounds', ucSelectIntervalBounds,
        'ActionSelectIntervalBounds', tkAction, True, csMenu, '', ''));
    Add(Cmd('SelectDataInterval', ucSelectDataInterval,
        'ActionSelectDataInterval', tkAction, False, csMenu, '', ''));
    Add(Cmd('SelectEntireProfile', ucSelectEntireProfile,
        'ActionSelectEntireProfile', tkAction, False, csMenu, '', ''));
    Add(Cmd('SelectCharacteristicPoints', ucSelectCharacteristicPoints,
        'ActionSelectCharacteristicPoints', tkAction, True, csMenu, '', ''));
    Add(Cmd('SelectCurveBounds', ucSelectCurveBounds,
        'ActionSelectCurveBounds', tkAction, True, csMenu, '', ''));

    //  The three submenu parents. Not checkable: a submenu parent is not a
    //  togglable thing, and its mode is said by the caption of the entry
    //  inside it.
    Add(Cmd('CurvePositionsMenu', ucCurvePositions, 'MenuCurvePositions',
        tkMenuItem, True, csMenu, '', ''));
    Add(Cmd('BackgroundPointsMenu', ucBackground, 'MenuBackground',
        tkMenuItem, True, csMenu, '', ''));
    Add(Cmd('RFactorIntervalsMenu', ucRFactorIntervals,
        'MenuRFactorIntervals', tkMenuItem, True, csMenu, '', ''));

    //  The results grid
    Add(Cmd('Copy', ucCopy, 'ActionCopy', tkAction, False, csMenu, '', ''));
    Add(Cmd('Delete', ucDelete, 'ActionDelete', tkAction, False,
        csMenu, '', ''));
    Add(Cmd('SelectAll', ucSelectAll, 'ActionSelectAll', tkAction, False,
        csMenu, '', ''));

    //  The chart
    Add(Cmd('ZoomIn', ucZoomIn, 'ActionZoomIn', tkAction, False,
        csMenu, '', ''));
    Add(Cmd('ZoomOut', ucZoomOut, 'ActionZoomOut', tkAction, False,
        csMenu, '', ''));
    Add(Cmd('ViewMarkers', ucViewMarkers, 'ActionViewMarkers', tkAction, False,
        csMenu, '', ''));
    Add(Cmd('UseRule', ucUseRule, 'MenuUseRule', tkMenuItem, False,
        csMenu, '', ''));
end;

{ ------------------------------------------------------------------ }

procedure TCommandTable.Append(const ADecl: TCommandDecl;
    AStartsChecked: boolean);
var
    N: longint;
begin
    N := Length(FItems);
    SetLength(FItems, N + 1);
    FItems[N] := ADecl;
    SetLength(FEnabled, N + 1);
    SetLength(FDown, N + 1);
    SetLength(FMenuCaption, N + 1);
    SetLength(FModuleEnabled, N + 1);
    SetLength(FModuleChecked, N + 1);
    FEnabled[N] := False;
    FDown[N] := False;
    FMenuCaption[N] := '';
    //  A MODULE'S ROW STARTS AVAILABLE. The module speaks when it has something
    //  to say; a row that started disabled would be unreachable until it did,
    //  and a module that never calls SetMenuEnabled is the ordinary case.
    FModuleEnabled[N] := True;
    //  THE DECLARATION SAYS WHICH RADIO IS ON. The menu shows that tick from
    //  the first draw; a pane starting with none would say a setting with a
    //  value has none.
    FModuleChecked[N] := AStartsChecked;
end;

procedure TCommandTable.AddFrameworkCommands;
var
    D: TCommandDecls;
    i: longint;
begin
    D := FrameworkCommands;
    for i := 0 to High(D) do
        Append(D[i]);
end;

procedure TCommandTable.AddModuleCommands(AModuleIndex: longint;
    const AName: string; const ADecls: TUiMenuDeclArray);
var
    i: longint;
    D: TCommandDecl;

    { What a PaneGroup names: the caption of the submenu with that id, and
      empty when no declaration provides one.

      THE CAPTION, NOT THE ID. PaneGroup names the submenu the way the module
      addresses it - 'waves.degrees' - and that is an identifier the user was
      never meant to read. The submenu already carries the words the menu shows
      over the same entries. }
    function GroupCaptionFor(const AId: string): string;
    var
        k: longint;
    begin
        Result := '';
        if AId = '' then
            Exit;
        for k := 0 to High(ADecls) do
            if (ADecls[k].Kind = mkSubmenu) and (ADecls[k].Id = AId) then
                Exit(ADecls[k].Caption);
    end;

begin
    for i := 0 to High(ADecls) do
    begin
        //  A separator draws a line and a submenu opens one; neither is a
        //  command, and neither belongs in a panel of buttons.
        if ADecls[i].Kind in [mkSeparator, mkSubmenu] then
            Continue;

        D := Default(TCommandDecl);
        //  PREFIXED BY THE MODULE, so two modules declaring 'act' do not
        //  collide in one table. The module's own id is kept beside it, because
        //  that is what has to come back on a click.
        D.Id := AName + '.' + ADecls[i].Id;
        D.CommandId := ADecls[i].Id;
        D.ModuleIndex := AModuleIndex;
        //  A module's availability is its own business, so Follows is left at
        //  its default and never read for this row - see Refresh.
        //  A MODULE'S ROW DRIVES THE ENTRY IT DECLARED. It names no
        //  component of the window's - the entry was made from this same
        //  declaration and is addressed by the module's own id - but it is a
        //  menu item all the same, so the window's one apply loop writes its
        //  Enabled and its tick, and no second loop has to know what a module
        //  is. See TFormMain.BuildCommandTable, which resolves it.
        D.TargetName := '';
        D.TargetKind := tkMenuItem;
        D.Surface := ADecls[i].Surface;
        D.Scope := ADecls[i].Scope;

        //  A TOGGLE AND A RADIO ARE BOTH ON OR OFF, and a button that looks the
        //  same either way says neither. Which of the two it is decides who may
        //  change it - see ChooseModuleRow.
        D.Latching := ADecls[i].Kind in [mkToggle, mkRadio];
        D.Radio := ADecls[i].Kind = mkRadio;
        D.RadioGroup := ADecls[i].RadioGroup;
        //  ONLY A LATCH CARRIES A TICK, and a declaration is what made the
        //  entry checkable in the first place - so this cannot ask for a tick
        //  on an entry that has no check box, which is the defect
        //  DeclareCheckableMenuEntries exists to prevent.
        D.WithChecked := D.Latching;

        if ADecls[i].ShortCaption <> '' then
            D.PaneCaption := ADecls[i].ShortCaption
        else
            D.PaneCaption := ADecls[i].Caption;
        D.Hint := ADecls[i].Hint;

        //  STILL SHOWN when the group is not declared, under the module's own
        //  heading rather than dropped: a missing entry is invisible, and
        //  invisible is how a whole pack was once unreachable.
        //
        //  THE MODULE'S OWN HEADING IS THE ONE THE MENU BAR USES, from
        //  module_menu, so the heading over the buttons and the entry in the
        //  menu are one word rather than two spellings of it.
        D.Group := GroupCaptionFor(ADecls[i].PaneGroup);
        if D.Group = '' then
            D.Group := ModuleRootCaption(AName);

        Append(D, ADecls[i].Checked);
    end;
end;

function TCommandTable.Count: longint;
begin
    Result := Length(FItems);
end;

function TCommandTable.Item(AIndex: longint): TCommandDecl;
begin
    Result := Default(TCommandDecl);
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    Result := FItems[AIndex];
end;

function TCommandTable.IndexOfId(const AId: string): longint;
var
    i: longint;
begin
    Result := -1;
    //  AN EMPTY ID MATCHES NOTHING, deliberately: a framework row that renders
    //  nowhere still has an id, but a caller asking for '' is asking for
    //  nothing and must not get the first row.
    if AId = '' then
        Exit;
    for i := 0 to High(FItems) do
        if FItems[i].Id = AId then
            Exit(i);
end;

function TCommandTable.IndexOfModuleRow(AModuleIndex: longint;
    const AId: string): longint;
var
    i: longint;
begin
    Result := -1;
    if AId = '' then
        Exit;
    for i := 0 to High(FItems) do
        if (FItems[i].ModuleIndex = AModuleIndex) and
            (FItems[i].CommandId = AId) then
            Exit(i);
end;

procedure TCommandTable.AdoptHint(AIndex: longint; const AFromTarget: string);
begin
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    //  Only into an empty one: a module declared its own, and this must not
    //  overwrite it with the hint of whatever widget happens to be resolved.
    if FItems[AIndex].Hint <> '' then
        Exit;
    FItems[AIndex].Hint := AFromTarget;
end;

procedure TCommandTable.Refresh(const AStates: TCommandStates;
    AMode: TSelMode; const ACounts: TModelCounts);
var
    i: longint;
begin
    for i := 0 to High(FItems) do
    begin
        if FItems[i].ModuleIndex >= 0 then
        begin
            //  A module's row follows what the module last said, not the
            //  framework's state: only the module knows when its own command
            //  applies.
            FEnabled[i] := FModuleEnabled[i];
            FDown[i] := FModuleChecked[i];
            FMenuCaption[i] := '';
            Continue;
        end;

        FEnabled[i] := AStates[FItems[i].Follows].Enabled;

        if FItems[i].HasPicking then
        begin
            //  PRESSED WHILE ITS OWN MODE RUNS. Decided from the mode rather
            //  than from the click that started it, because a mode ends in ways
            //  the row never hears about - another mode starting, a profile
            //  being loaded.
            FDown[i] := (PickingEntryMode(FItems[i].Picking) <>
                ModeSelectNothing) and
                (AMode = PickingEntryMode(FItems[i].Picking));
            FMenuCaption[i] := PickingEntryCaption(FItems[i].Picking, AMode);
        end
        else
        begin
            FDown[i] := AStates[FItems[i].Follows].Checked;
            FMenuCaption[i] := '';
        end;
    end;
end;

function TCommandTable.IsEnabled(AIndex: longint): boolean;
begin
    Result := False;
    if (AIndex < 0) or (AIndex > High(FEnabled)) then
        Exit;
    Result := FEnabled[AIndex];
end;

function TCommandTable.IsDown(AIndex: longint): boolean;
begin
    Result := False;
    if (AIndex < 0) or (AIndex > High(FDown)) then
        Exit;
    Result := FDown[AIndex];
end;

function TCommandTable.LatchGroup(AIndex: longint): longint;
var
    i: longint;
begin
    Result := AIndex;
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    if not FItems[AIndex].Radio then
        Exit;
    //  THE FIRST OF THE SET ANSWERS FOR IT, which is a number every sibling
    //  arrives at without one being handed out. Scoped by module as well as by
    //  group number, because the numbers are the modules' own and two modules
    //  may both call a set 1.
    for i := 0 to High(FItems) do
        if FItems[i].Radio and
            (FItems[i].ModuleIndex = FItems[AIndex].ModuleIndex) and
            (FItems[i].RadioGroup = FItems[AIndex].RadioGroup) then
            Exit(i);
end;

function TCommandTable.MenuCaption(AIndex: longint): string;
begin
    Result := '';
    if (AIndex < 0) or (AIndex > High(FMenuCaption)) then
        Exit;
    Result := FMenuCaption[AIndex];
end;

procedure TCommandTable.SetModuleEnabled(const AId: string;
    AEnabled: boolean);
var
    i: longint;
begin
    for i := 0 to High(FItems) do
        if (FItems[i].ModuleIndex >= 0) and (FItems[i].CommandId = AId) and
            (AId <> '') then
            FModuleEnabled[i] := AEnabled;
end;

procedure TCommandTable.SetModuleChecked(const AId: string;
    AChecked: boolean);
var
    i: longint;
begin
    for i := 0 to High(FItems) do
        if (FItems[i].ModuleIndex >= 0) and (FItems[i].CommandId = AId) and
            (AId <> '') then
        begin
            //  TICKING ONE OF A SET IS CHOOSING IT. A module that ticks a radio
            //  without untickng its siblings means the choice moved, not that
            //  two settings hold at once - the menu's radio group does exactly
            //  this to the entries, and the pane must not be the surface that
            //  shows two pressed.
            if FItems[i].Radio and AChecked then
                ChooseModuleRow(i)
            else
                FModuleChecked[i] := AChecked;
        end;
end;

procedure TCommandTable.ChooseModuleRow(AIndex: longint);
var
    i: longint;
begin
    //  Reached from a click carrying a widget's Tag, which can go stale between
    //  a rebuild and a click already on its way.
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    if not FItems[AIndex].Radio then
        Exit;

    for i := 0 to High(FItems) do
        if LatchGroup(i) = LatchGroup(AIndex) then
            FModuleChecked[i] := i = AIndex;
end;

function TCommandTable.TargetOf(AIndex: longint): TCommandTarget;
begin
    Result := Default(TCommandTarget);
    Result.Kind := ctNothing;
    Result.ModuleIndex := -1;
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;

    if FItems[AIndex].ModuleIndex >= 0 then
    begin
        Result.Kind := ctModuleCommand;
        Result.ModuleIndex := FItems[AIndex].ModuleIndex;
        Result.CommandId := FItems[AIndex].CommandId;
        Exit;
    end;

    if FItems[AIndex].TargetKind = tkAction then
    begin
        if FItems[AIndex].TargetName = '' then
            Exit;
        Result.Kind := ctAction;
        Result.ActionName := FItems[AIndex].TargetName;
    end;
    //  A submenu parent answers no click: opening it is not a command.
end;

function TCommandTable.IndicesFor(ASurface: TCommandSurface;
    AScope: TCommandScope): TIndexList;
var
    i, N: longint;

    function ShownOn(ADeclared: TCommandSurface): boolean;
    begin
        //  csBoth satisfies either question, which is what "both" means. Asking
        //  for csBoth asks for the rows that are on both, and there is no
        //  caller for that today - it answers exactly those rows rather than
        //  something surprising.
        if ADeclared = csBoth then
            Result := ASurface in [csMenu, csPane, csBoth]
        else
            Result := ADeclared = ASurface;
    end;

begin
    Result := nil;
    N := 0;
    for i := 0 to High(FItems) do
    begin
        if FItems[i].Scope <> AScope then
            Continue;
        if not ShownOn(FItems[i].Surface) then
            Continue;
        SetLength(Result, N + 1);
        Result[N] := i;
        Inc(N);
    end;
end;

function TCommandTable.PaneGroups: TGroupList;
var
    i, N: longint;
    Groups: TGroupList;

    function Seen(const AGroup: string): boolean;
    var
        k: longint;
    begin
        Result := False;
        for k := 0 to N - 1 do
            if Groups[k] = AGroup then
                Exit(True);
    end;

begin
    Groups := nil;
    N := 0;
    for i := 0 to High(FItems) do
    begin
        if FItems[i].Scope <> scGlobal then
            Continue;
        if not (FItems[i].Surface in [csPane, csBoth]) then
            Continue;
        if FItems[i].Group = '' then
            Continue;
        if Seen(FItems[i].Group) then
            Continue;
        SetLength(Groups, N + 1);
        Groups[N] := FItems[i].Group;
        Inc(N);
    end;
    Result := Groups;
end;

end.
