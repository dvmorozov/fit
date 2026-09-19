// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Model panel shows when the framework is the one filling it.)

THE PANEL IS SHARED, and this is the framework's half of it. A model placed from
its own markup - a pattern, a shape a module derives - is a hierarchy only the
module that placed it can describe, and it fills the panel itself. A model built
from picks is a flat list of curves, and that is what this unit builds.

WHICH OF THE TWO FILLS IT IS NOT ARBITRATED. It follows the selected curve type's
PlacedByPointSet, and the reason is written down in named_points_set: the
decision "is a property of the type, known before anything is built", and
deriving it from whether a contributor happens to have rows - "did the module
handle this?" - is what made the ordinary case (the type is selected, nothing is
marked yet) fall through to the position path and generate one curve per data
point. That presented as a hang once already.

A ROW IS A CURVE, NOT A PICK. Both were considered. A pick is the user's input
and exists first, but nothing client-side can say which curve a given pick
produced: the pairing lives in the engine's identity registry, and a curve's
position PARAMETER is where the fit put it, not the pick it started from. A
curve, on the other hand, carries the handle the whole model is addressed by. So
the rows are curves, keyed by that handle - and the model is built from the picks
as soon as there are any, so the panel fills the moment the user has placed
something rather than only after a fit.
}
unit model_outline;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, module_view_types;

type
    { One curve of the model, as much of it as the panel needs. Gathered by the
      caller from the client, so nothing here reaches for anything. }
    TModelCurveRow = record
        { What the curve calls itself - the same title the chart's series and
          the summary table's column carry. }
        Title: string;
        { The handle the model addresses this instance by. Empty when the
          instance carries none, which is a curve nothing can be asked about
          individually. }
        InstanceId: string;
        { Where the fit put it. Shown because it is what tells two curves of the
          same type apart, and it is the number the user is looking for. }
        Position: double;
        HasPosition: boolean;
        { The explanation for this curve's TYPE, or '' when the type is unknown.
          Per curve, not the selected type: a model may hold curves of several
          types, and a row explaining the wrong one is worse than none. }
        Topic: string;
    end;

    TModelCurveRows = array of TModelCurveRow;

{ True when the framework fills the panel for the selected curve type - that is,
  when the type is placed by one pick per curve.

  APlacedByPointSet is TNamedPointsSet.PlacedByPointSet for the selected type:
  empty for a type placed by picks, and the name of its own point set for a type
  placed by marking an extent. }
function FrameworkFillsStructure(const APlacedByPointSet: string): boolean;

{ Whether rows pushed under APanelId are shown now.

  THE PANEL SHOWS ONE DESCRIPTION OF THE MODEL, and which follows the selected
  type. A module pushes its rows on every redraw; shown unconditionally, a push
  from a pack with nothing placed replaced a Gaussian model's rows and the
  selection on them. So rows under a registered module's panel - AModulePanelIds,
  each module's PanelId, which is its PlacedByPointSet - are shown only while
  ASelectedPlacedByPointSet names that set. The framework's own rows are gated
  where they are built (FrameworkFillsStructure), and rows under an id no module
  claims have no type to wait for. }
function ModulePanelRowsShown(const APanelId, ASelectedPlacedByPointSet: string;
    const AModulePanelIds: array of string): boolean;

type
    { Rows a module pushed while another type was selected, the latest per panel
      id. Parallel arrays. }
    THeldPanelRows = record
        Ids: array of string;
        Rows: array of TOutline;
    end;

{ Keeps ARows as what APanelId last pushed, replacing any earlier push. }
procedure HoldPanelRows(var AHeld: THeldPanelRows; const APanelId: string;
    const ARows: TOutline);

{ The held rows to show now that ASelectedPlacedByPointSet is selected: True
  with the panel id and its rows when some are held for that type and the panel
  does not already show them (AShownPanelId), so a refresh does not rebuild the
  tree from the rows it already holds. }
function HeldRowsToShow(const AHeld: THeldPanelRows;
    const ASelectedPlacedByPointSet, AShownPanelId: string;
    out APanelId: string; out ARows: TOutline): boolean;

{ Takes rows a contributor pushed under APanelId: keeps them, unless they are
  the framework's own, and answers whether they are shown now
  (ModulePanelRowsShown).

  KEPT EVEN WHEN SHOWN: switching away from the module's type and back must
  bring back what it pushed last, not what it pushed the last time its rows were
  held. The framework's rows are rebuilt from the model whenever they are
  needed, so a kept copy of them could only be an old one. }
function PushPanelRows(var AHeld: THeldPanelRows;
    const APanelId, ASelectedPlacedByPointSet: string;
    const AModulePanelIds: array of string; const ARows: TOutline): boolean;

const
    { A report's "show" named a row the panel keeps for a type not selected. }
    RowKeptHint = 'That part of the model is in the Model panel while one of ' +
        'its own curve types is selected. Select one to see it.';
    { ...or a row no contributor holds any more. }
    RowGoneHint = 'That part of the model is no longer in the Model panel.';

{ What to tell the user when a link names ARowId and the panel is not showing
  it: RowKeptHint when the row is among the kept rows, RowGoneHint otherwise. }
function RowLinkMissHint(const AHeld: THeldPanelRows; const ARowId: string): string;

{ What one row reads. The title, and the position where there is one.

  THE POSITION IS WHAT TELLS TWO CURVES APART. Six Gaussians all read "Gaussian"
  otherwise, and the user is looking for the one at 23.5. }
function ModelRowCaption(const ARow: TModelCurveRow): string;

{ The framework's outline: one flat row per curve, in the order the model holds
  them.

  FLAT, with every row at indent 0, and that is not a placeholder for a
  hierarchy - a model built from picks has no nesting to show. The same TOutline
  a module produces, so one renderer serves both. }
function ModelOutlineOf(const ARows: TModelCurveRows): TOutline;

{ The curve the row carrying ARowId names, or '' when it names none.

  WHOEVER FILLED THE PANEL. The framework's rows are identified by the handle
  itself; a pack's rows are identified by its own markup and carry the handle
  beside it - and both are answered here, from the rows, rather than by asking
  whose rows these were. That question is what the window asked, and its answer
  for a contributor's row was always "no curve": Delete curve was permanently
  greyed over every pattern an analysis pack had placed, and no framework
  command on one curve could ever apply to one.

  An empty ARowId names nothing, deliberately: the sentence shown in an empty
  panel is a row with no id, and matching it would offer the commands over a
  message. }
function CurveHandleForRowId(const ARows: TOutline;
    const ARowId: string): string;

{ The index of the first row that stands for the curve AHandle, or -1.

  THE REVERSE OF CurveHandleForRowId, and asked of the rows for the same reason:
  a framework row is identified by the handle itself and a pack's row carries it
  beside its own id, so only CurveId answers for both. Used when a curve is
  chosen somewhere other than the panel - the Curve Attributes table - and the
  panel has to show the same curve selected. An empty handle names no row. }
function RowIndexForCurveHandle(const ARows: TOutline;
    const AHandle: string): longint;

type
    { What choosing a curve in the Curve Attributes table does to the
      selection. }
    TTableCurveChoice = record
        { False when the row names no curve, or the curve already selected:
          nothing moves. }
        Changes: boolean;
        { The curve chosen. }
        CurveId: string;
        { The id of the panel's row standing for it, or '' when no row does -
          a pack's panel shows only its own rows. Then the panel selects
          nothing and the curve is held anyway, so the chart still shows the
          choice and the panel points at nothing rather than something else. }
        RowId: string;
    end;

{ The selection a Curve Attributes row asks for, given the curve that row
  names (AHandle), the panel's rows and the curve already selected.

  THE PANEL'S ROW, NOT THE HANDLE BESIDE IT, wherever a row stands for the
  curve: the panel's Delete curve acts on the held handle, so a panel showing
  one curve while another is held would aim that command at a curve the user
  is not looking at. }
function CurveChoiceFromTable(const ARows: TOutline;
    const AHandle, ASelectedCurveId: string): TTableCurveChoice;

{ The curve-type name a curve's title begins with.

  THE ONLY PER-CURVE STATEMENT OF TYPE THE CLIENT RECEIVES. A curve's type is not
  on the wire - 'curveType' is the model's SELECTED type, and a model may hold
  curves of several types - but the engine titles every curve it builds
  '<type name> [<n>]'. So the name is read back from the title, and a title with
  no bracket is taken whole. Recorded as a gap: a per-curve type field would make
  this unnecessary. }
function CurveTypeNameOfTitle(const ATitle: string): string;

{ The explanation topic of the row carrying ARowId, or '' when it has none -
  answered from the rows, for the reason CurveHandleForRowId gives. }
function TopicForRowId(const ARows: TOutline; const ARowId: string): string;

{ What the panel says when it is empty, which depends on why it is empty:
  nothing is open, nothing is placed, or the selected type is placed some other
  way and its own contributor has said nothing yet.

  NEVER BLANK. An empty box is indistinguishable from a broken one (D26), and
  this panel is empty in three quite different situations that the user can do
  three different things about. }
function EmptyStructureText(AProfileIsOpen: boolean;
    AFrameworkFills: boolean): string;

const
    { What the FRAMEWORK pushes its rows under.

      A panel id, like a module's, because the rows go in through the same entry
      point - so one renderer serves both and the framework's own path is
      exercised in every build rather than only in a build with a module. It
      names no module, which is what keeps PanelTextFor from lending a module's
      wording to the framework's rows. }
    FrameworkStructureId = 'fit.model';

    { The panel's own name. The framework's, not a contributor's: the panel
      shows the model, whoever describes it. }
    ModelPanelCaption = 'Model';
    ModelPanelHint = 'The curves this model is made of';

    { What the Explain pane says when nothing in focus can be explained. Says
      what to point at, because an empty pane is indistinguishable from a
      broken one (D26). }
    ExplainPaneEmptyText = 'Select a curve type, a row of the Model panel or ' +
        'a menu entry to see it explained.';

implementation

function FrameworkFillsStructure(const APlacedByPointSet: string): boolean;
begin
    //  A type that names a point set of its own is placed by marking an extent
    //  in it, so the module owning that set is the one that can describe what
    //  the markup produced. Everything else is one curve per pick.
    Result := Trim(APlacedByPointSet) = '';
end;

function ModulePanelRowsShown(const APanelId, ASelectedPlacedByPointSet: string;
    const AModulePanelIds: array of string): boolean;
var
    i: longint;
begin
    Result := True;
    if APanelId = FrameworkStructureId then
        Exit;
    for i := 0 to High(AModulePanelIds) do
        if SameText(AModulePanelIds[i], APanelId) then
            Exit(SameText(Trim(ASelectedPlacedByPointSet), APanelId));
end;

procedure HoldPanelRows(var AHeld: THeldPanelRows; const APanelId: string;
    const ARows: TOutline);
var
    i: longint;
begin
    for i := 0 to High(AHeld.Ids) do
        if AHeld.Ids[i] = APanelId then
        begin
            AHeld.Rows[i] := ARows;
            Exit;
        end;
    SetLength(AHeld.Ids, Length(AHeld.Ids) + 1);
    SetLength(AHeld.Rows, Length(AHeld.Rows) + 1);
    AHeld.Ids[High(AHeld.Ids)] := APanelId;
    AHeld.Rows[High(AHeld.Rows)] := ARows;
end;

function HeldRowsToShow(const AHeld: THeldPanelRows;
    const ASelectedPlacedByPointSet, AShownPanelId: string;
    out APanelId: string; out ARows: TOutline): boolean;
var
    i: longint;
begin
    APanelId := '';
    ARows := nil;
    for i := 0 to High(AHeld.Ids) do
        if SameText(AHeld.Ids[i], Trim(ASelectedPlacedByPointSet)) then
        begin
            if AHeld.Ids[i] = AShownPanelId then
                Exit(False);
            APanelId := AHeld.Ids[i];
            ARows := AHeld.Rows[i];
            Exit(True);
        end;
    Result := False;
end;

function PushPanelRows(var AHeld: THeldPanelRows;
    const APanelId, ASelectedPlacedByPointSet: string;
    const AModulePanelIds: array of string; const ARows: TOutline): boolean;
begin
    if APanelId <> FrameworkStructureId then
        HoldPanelRows(AHeld, APanelId, ARows);
    Result := ModulePanelRowsShown(APanelId, ASelectedPlacedByPointSet,
        AModulePanelIds);
end;

function RowLinkMissHint(const AHeld: THeldPanelRows; const ARowId: string): string;
var
    i, j: longint;
begin
    Result := RowGoneHint;
    for i := 0 to High(AHeld.Rows) do
        for j := 0 to High(AHeld.Rows[i]) do
            if AHeld.Rows[i][j].Id = ARowId then
                Exit(RowKeptHint);
end;

function ModelRowCaption(const ARow: TModelCurveRow): string;
begin
    Result := ARow.Title;
    if Result = '' then
        //  A curve with no title of its own. Better a word than a blank row.
        Result := 'Curve';
    if ARow.HasPosition then
        //  Trailing zeroes stripped: a position is a place on the axis the user
        //  is reading, not a measurement being reported to full precision.
        Result := Result + '  at ' + FloatToStr(ARow.Position);
end;

function ModelOutlineOf(const ARows: TModelCurveRows): TOutline;
var
    i: longint;
begin
    SetLength(Result, Length(ARows));
    for i := 0 to High(ARows) do
    begin
        Result[i].Indent := 0;
        Result[i].Caption := ModelRowCaption(ARows[i]);
        //  THE HANDLE IS THE ROW'S IDENTITY, because that is what every
        //  operation on one curve takes. A row whose curve carries none gets an
        //  empty id, and the commands that need one stay disabled over it.
        Result[i].Id := ARows[i].InstanceId;
        //  THE SAME HANDLE TWICE for the framework's own rows, because for
        //  these two it IS the same thing. A pack's rows are identified by its
        //  markup and name the curve separately, which is why the row carries
        //  both and why nothing reads the identity as a handle.
        Result[i].CurveId := ARows[i].InstanceId;
        //  Nothing here is detached: a flat list has no parent to lose.
        Result[i].IsDetached := False;
        Result[i].Topic := ARows[i].Topic;
    end;
end;

function CurveHandleForRowId(const ARows: TOutline;
    const ARowId: string): string;
var
    i: longint;
begin
    Result := '';
    if ARowId = '' then
        Exit;
    for i := 0 to High(ARows) do
        if ARows[i].Id = ARowId then
            Exit(ARows[i].CurveId);
end;

function RowIndexForCurveHandle(const ARows: TOutline;
    const AHandle: string): longint;
var
    i: longint;
begin
    Result := -1;
    //  A heading carries CurveId '', and '' must not select it.
    if AHandle = '' then
        Exit;
    for i := 0 to High(ARows) do
        if ARows[i].CurveId = AHandle then
            Exit(i);
end;

function CurveChoiceFromTable(const ARows: TOutline;
    const AHandle, ASelectedCurveId: string): TTableCurveChoice;
var
    Index: longint;
begin
    Result := Default(TTableCurveChoice);
    if (AHandle = '') or (AHandle = ASelectedCurveId) then
        Exit;
    Result.Changes := True;
    Result.CurveId := AHandle;
    Index := RowIndexForCurveHandle(ARows, AHandle);
    if Index >= 0 then
        Result.RowId := ARows[Index].Id;
end;

function CurveTypeNameOfTitle(const ATitle: string): string;
var
    Bracket: longint;
begin
    Result := ATitle;
    Bracket := Pos(' [', Result);
    if Bracket > 0 then
        Result := Copy(Result, 1, Bracket - 1);
    Result := Trim(Result);
end;

function TopicForRowId(const ARows: TOutline; const ARowId: string): string;
var
    i: longint;
begin
    Result := '';
    if ARowId = '' then
        Exit;
    for i := 0 to High(ARows) do
        if ARows[i].Id = ARowId then
            Exit(ARows[i].Topic);
end;

function EmptyStructureText(AProfileIsOpen: boolean;
    AFrameworkFills: boolean): string;
begin
    if not AProfileIsOpen then
        Result := 'Open a data file to start building a model.'
    else if AFrameworkFills then
        //  Names the two ways to place a curve, because "nothing here" is not
        //  something the user can act on and this is.
        Result := 'No curves yet - place some with Positions on the Tools tab.'
    else
        //  The selected type is placed from its own markup, and whatever owns
        //  that markup has not described anything yet. The framework must not
        //  say how to make it: it does not know what the markup is.
        Result := 'No curves yet - this curve type is placed from its own ' +
            'markup.';
end;

end.
