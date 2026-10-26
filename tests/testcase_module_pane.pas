// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A module's declarations, all the way to positioned buttons in the
Tools pane.)

WHY THIS EXISTS. Every piece of this path had tests and the path itself had
none, so it was possible for all of them to pass over a pane that showed no
module at all - which is what shipped. TUiMenuDecl.Surface named the pane from
the day the pane arrived; no module ever set it, nothing pushed a module's state
into the table the pane reads, and a module's toggle could not have been drawn
pressed if it had. The window is where those meet, and the window needs a widget
set the suite has not got.

WHAT IS WALKED HERE is what TFormMain does between RegisterAppModules and the
last button it places: register the modules, build the one table from
AddFrameworkCommands plus every module's declarations, ask it what the pane
draws, and lay those out with the same TToolPaneLayout the window uses. The
widgets are the only thing left out, and ui_selfcheck is what watches those,
inside the running application.

WHY A UNIT TEST. It crosses no process, touches no file and fits nothing - it is
records and arithmetic, which is the split tests/README states. What it does
share is the process-global UI module registry, which has no unregister: it
registers one module under a name of its own, and asserts against counts taken
before it, never absolute ones.
}
unit testcase_module_pane;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_ui_host, ui_commands, module_menu, action_state, fit_client,
    tool_pane_layout, mock_ui_module;

type
    { The rectangles a pane full of buttons occupies. }
    TButtonBoxes = array of TPaneRect;

    TModulePaneTest = class(TTestCase)
    private
        FModule: TMockUiModule;
        FAsModule: IUiModule;
        FTable: TCommandTable;
        { The declarations the module makes: a marking toggle, two commands, a
          two-entry depth setting under a submenu, and one entry it keeps to
          the menu. The shape a real module has, in miniature. }
        function Declarations: TUiMenuDeclArray;
        { The table, built the way TFormMain.BuildCommandTable builds it. }
        procedure GivenTheWindowBuiltItsTable;
        function RowOf(const AId: string): longint;
        function GroupIndexOf(const AGroup: string): longint;
        { Every button the pane would place, in order, with its rectangle. }
        function LayOutThePane: TButtonBoxes;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheModuleIsRegisteredOnceUnderItsOwnName;
        procedure ItsPaneRowsAreTheOnesItOptedIn;
        procedure ItsMenuOnlyRowStaysOutOfThePane;
        procedure ItsHeadingsReadAsItsMenuDoes;
        procedure TheFrameworksOwnGroupsComeFirst;
        procedure EveryPaneRowCarriesAHint;
        procedure ItsButtonsAreLaidOutWithoutOverlapping;
        procedure ItsMarkingToggleIsPressedWhenTheModuleSaysSo;
        procedure ItsDepthSettingKeepsOneChoicePressed;
        procedure AndTheModuleCanDisableWhatItOffers;
    end;

implementation

const
    MODULE_NAME  = 'panetest';
    ID_MARK      = 'mark';
    ID_DETECT    = 'detect';
    ID_DEPTHS    = 'depths';
    ID_DEPTH_ALL = 'depth.all';
    ID_DEPTH_TOP = 'depth.top';
    ID_QUIET     = 'quiet';
    DEPTH_GROUP  = 11;

    //  The pane the window lays out, as ui_dpi scales it. Any consistent set
    //  does; these are near the real ones so the arithmetic is not degenerate.
    PANE_WIDTH  = 180;
    GAP         = 6;
    BUTTON_H    = 25;
    HEADING_H   = 17;

procedure TModulePaneTest.SetUp;
begin
    FModule := TMockUiModule.Create(MODULE_NAME);
    FModule.SetMenuItems(Declarations);
    FAsModule := FModule;
    FTable := TCommandTable.Create;
end;

procedure TModulePaneTest.TearDown;
begin
    FTable.Free;
    FTable := nil;
    //  mock_support's rule: the interface goes before the object it points at.
    //  The registry keeps its own reference, which is why the module outlives
    //  this - it has no unregister, and every assertion here allows for it.
    FAsModule := nil;
end;

function TModulePaneTest.Declarations: TUiMenuDeclArray;

    procedure Add(const AId, AParent, ACaption, AHint, AShort: string;
        AKind: TUiMenuKind; ASurface: TCommandSurface = csBoth;
        AGroup: longint = 0; AChecked: boolean = False);
    begin
        SetLength(Result, Length(Result) + 1);
        with Result[High(Result)] do
        begin
            Id := AId;
            Parent := AParent;
            Caption := ACaption;
            Hint := AHint;
            ShortCaption := AShort;
            Kind := AKind;
            Surface := ASurface;
            RadioGroup := AGroup;
            Checked := AChecked;
            if AParent <> '' then
                PaneGroup := AParent;
        end;
    end;

begin
    Result := nil;
    Add(ID_MARK, '', 'Mark Wave Bounds', 'Pick the ends of a pattern',
        'Mark', mkToggle);
    Add(ID_DETECT, '', 'Detect Waves...', 'Proposes wave counts', 'Detect',
        mkCommand);
    Add(ID_DEPTHS, '', 'Show Degrees', '', '', mkSubmenu);
    Add(ID_DEPTH_ALL, ID_DEPTHS, 'All', 'Draws every degree', 'All',
        mkRadio, csBoth, DEPTH_GROUP, True);
    Add(ID_DEPTH_TOP, ID_DEPTHS, 'Top level only', 'Draws the top degree',
        'Top', mkRadio, csBoth, DEPTH_GROUP);
    //  KEPT TO THE MENU on purpose: a module says where each entry goes, one
    //  entry at a time, and the pane must honour the ones it was not offered.
    Add(ID_QUIET, '', 'Rarely wanted', 'Not worth a button', '', mkCommand,
        csMenu);
end;

procedure TModulePaneTest.GivenTheWindowBuiltItsTable;
var
    Mods: TUiModuleArray;
    m: longint;
begin
    RegisterUiModule(FAsModule);
    FTable.AddFrameworkCommands;
    //  THE WINDOW'S OWN LOOP, from TFormMain.BuildCommandTable: every
    //  registered module, in registration order, appended to the one table the
    //  menus and the pane are both drawn from.
    Mods := RegisteredUiModules;
    for m := 0 to High(Mods) do
        FTable.AddModuleCommands(m, Mods[m].Name, Mods[m].MenuItems);
end;

function TModulePaneTest.RowOf(const AId: string): longint;
begin
    Result := FTable.IndexOfId(MODULE_NAME + '.' + AId);
    AssertTrue('the table has a row for ' + AId, Result >= 0);
end;

function TModulePaneTest.GroupIndexOf(const AGroup: string): longint;
var
    G: TGroupList;
    i: longint;
begin
    Result := -1;
    G := FTable.PaneGroups;
    for i := 0 to High(G) do
        if G[i] = AGroup then
            Exit(i);
end;

function TModulePaneTest.LayOutThePane: TButtonBoxes;
var
    Metrics: TPaneMetrics;
    Layout: TToolPaneLayout;
    Groups: TGroupList;
    Rows: TIndexList;
    g, r, N: longint;
begin
    Result := nil;
    N := 0;
    Metrics.PaneWidth := PANE_WIDTH;
    Metrics.Gap := GAP;
    Metrics.ButtonHeight := BUTTON_H;
    Metrics.HeadingHeight := HEADING_H;
    Layout := TToolPaneLayout.Create(Metrics);
    try
        Groups := FTable.PaneGroups;
        Rows := FTable.IndicesFor(csPane, scGlobal);
        for g := 0 to High(Groups) do
        begin
            Layout.StartGroup;
            for r := 0 to High(Rows) do
                if FTable.Item(Rows[r]).Group = Groups[g] then
                begin
                    SetLength(Result, N + 1);
                    Result[N] := Layout.NextButton;
                    Inc(N);
                end;
            Layout.EndGroup;
        end;
    finally
        Layout.Free;
    end;
end;

{ ---- what reached the table ---- }

procedure TModulePaneTest.TheModuleIsRegisteredOnceUnderItsOwnName;
var
    Counted: longint;
begin
    GivenTheWindowBuiltItsTable;
    Counted := UiModuleCount;
    //  Registered by every host that builds a window, so twice is ordinary and
    //  the registry ignores the second. What must not happen is a build where
    //  the module is absent - the state this whole path was in.
    AssertTrue('it is there', Counted > 0);
    RegisterUiModule(FAsModule);
    AssertEquals('and it is there once', Counted, UiModuleCount);
end;

procedure TModulePaneTest.ItsPaneRowsAreTheOnesItOptedIn;
var
    Rows: TIndexList;
    i, Mine: longint;
begin
    GivenTheWindowBuiltItsTable;
    Rows := FTable.IndicesFor(csPane, scGlobal);
    Mine := 0;
    for i := 0 to High(Rows) do
        if FTable.Item(Rows[i]).ModuleIndex >= 0 then
            Inc(Mine);
    //  Four: the toggle, the command and the two depths. The submenu is not a
    //  command and the menu-only entry did not ask.
    AssertEquals('the four it offered', 4, Mine);
end;

procedure TModulePaneTest.ItsMenuOnlyRowStaysOutOfThePane;
var
    Rows: TIndexList;
    i: longint;
begin
    GivenTheWindowBuiltItsTable;
    Rows := FTable.IndicesFor(csPane, scGlobal);
    for i := 0 to High(Rows) do
        AssertTrue('the quiet entry is not on the pane',
            FTable.Item(Rows[i]).Id <> MODULE_NAME + '.' + ID_QUIET);
    //  It is still a command, and the menu still offers it.
    AssertTrue('but the table has it', FTable.IndexOfId(
        MODULE_NAME + '.' + ID_QUIET) >= 0);
end;

procedure TModulePaneTest.ItsHeadingsReadAsItsMenuDoes;
begin
    GivenTheWindowBuiltItsTable;
    //  The entries that named no submenu sit under the module's own name; the
    //  two depths sit under the submenu's CAPTION, which is the wording the
    //  menu shows. An id for a heading would print "panetest.depths".
    AssertTrue('the module has a heading of its own',
        GroupIndexOf(ModuleRootCaption(MODULE_NAME)) >= 0);
    AssertTrue('and the submenu is one too', GroupIndexOf('Show Degrees') >= 0);
end;

procedure TModulePaneTest.TheFrameworksOwnGroupsComeFirst;
begin
    GivenTheWindowBuiltItsTable;
    //  THE ORDER IS THE ORDER OF THE WORK, and a module's tools are what you
    //  reach for after the model exists - the framework's four groups are
    //  appended first and a module cannot get between them.
    AssertEquals('positions first', 0, GroupIndexOf(GroupPositions));
    AssertTrue('and the module after the fit',
        GroupIndexOf(ModuleRootCaption(MODULE_NAME)) > GroupIndexOf(GroupFit));
end;

procedure TModulePaneTest.EveryPaneRowCarriesAHint;
var
    Rows: TIndexList;
    i: longint;
begin
    GivenTheWindowBuiltItsTable;
    Rows := FTable.IndicesFor(csPane, scGlobal);
    for i := 0 to High(Rows) do
        //  A module's row drives no action to take a hint from, so its
        //  declaration is the only text there is. ui_selfcheck reports a
        //  hintless button in the running window; this says the same thing
        //  about the rows the module contributed, before there is a window.
        if FTable.Item(Rows[i]).ModuleIndex >= 0 then
            AssertTrue(FTable.Item(Rows[i]).Id + ' explains itself',
                FTable.Item(Rows[i]).Hint <> '');
end;

{ ---- where they land ---- }

procedure TModulePaneTest.ItsButtonsAreLaidOutWithoutOverlapping;
var
    R: TButtonBoxes;
    i, k: longint;
begin
    GivenTheWindowBuiltItsTable;
    R := LayOutThePane;
    AssertTrue('the pane has buttons', Length(R) > 0);
    for i := 0 to High(R) do
    begin
        AssertTrue('inside the pane', R[i].Left + R[i].Width <= PANE_WIDTH);
        for k := i + 1 to High(R) do
            //  A button drawn over another looks exactly like the pane not
            //  offering that command, which is the failure the whole layout
            //  unit exists to prevent - and adding a module's groups is how a
            //  pane grows past the height anyone laid out by hand.
            AssertFalse('buttons do not overlap',
                (R[i].Left < R[k].Left + R[k].Width) and
                (R[k].Left < R[i].Left + R[i].Width) and
                (R[i].Top < R[k].Top + R[k].Height) and
                (R[k].Top < R[i].Top + R[i].Height));
    end;
end;

{ ---- and what they show ---- }

procedure TModulePaneTest.ItsMarkingToggleIsPressedWhenTheModuleSaysSo;
begin
    GivenTheWindowBuiltItsTable;
    FTable.Refresh(CommandStates(EmptyUiInputs), ModeSelectNothing,
        EmptyModelCounts);
    AssertFalse('not marking yet', FTable.IsDown(RowOf(ID_MARK)));

    //  WHAT THE HOST DOES WITH SetMenuChecked. The mode outlives the click that
    //  started it and ends in ways the click never hears about, so the tick is
    //  the module's to state - and the pane must hear it, or the button says
    //  the mode is off while the menu says it is on.
    FTable.SetModuleChecked(ID_MARK, True);
    FTable.Refresh(CommandStates(EmptyUiInputs), ModeSelectNothing,
        EmptyModelCounts);
    AssertTrue('marking now', FTable.IsDown(RowOf(ID_MARK)));
end;

procedure TModulePaneTest.ItsDepthSettingKeepsOneChoicePressed;
begin
    GivenTheWindowBuiltItsTable;
    FTable.Refresh(CommandStates(EmptyUiInputs), ModeSelectNothing,
        EmptyModelCounts);
    AssertTrue('the declared choice', FTable.IsDown(RowOf(ID_DEPTH_ALL)));

    FTable.ChooseModuleRow(RowOf(ID_DEPTH_TOP));
    FTable.Refresh(CommandStates(EmptyUiInputs), ModeSelectNothing,
        EmptyModelCounts);
    AssertTrue('moves to the one clicked', FTable.IsDown(RowOf(ID_DEPTH_TOP)));
    AssertFalse('and only one is pressed',
        FTable.IsDown(RowOf(ID_DEPTH_ALL)));
end;

procedure TModulePaneTest.AndTheModuleCanDisableWhatItOffers;
begin
    GivenTheWindowBuiltItsTable;
    FTable.SetModuleEnabled(ID_DETECT, False);
    FTable.Refresh(CommandStates(EmptyUiInputs), ModeSelectNothing,
        EmptyModelCounts);
    AssertFalse('the module refused it', FTable.IsEnabled(RowOf(ID_DETECT)));
    AssertTrue('and said nothing about the rest',
        FTable.IsEnabled(RowOf(ID_MARK)));
end;

initialization
    RegisterTest('unit', TModulePaneTest);
end.
