// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Model panel shows when the framework is the one filling it.)

THE PANEL IS SHARED, and this is the framework's half of it. A model placed from
its own markup - a pattern, a shape a module derives - is a hierarchy only the
module that placed it can describe, and it fills the panel itself. A model built
from picks is a flat list of curves, and that is what this unit builds.

WHOSE ROWS ARE SHOWN FOLLOWS WHAT THE MODEL HOLDS (ComposePanel), not the
curve type selected in the Tools list. A module's tree is shown whenever it
describes something, whatever type is selected, and the framework's rows fill in
the curves no module row stands for. It once followed the selected type, and
choosing an ordinary type to place next turned a nested model into a flat list.

The selected type's PlacedByPointSet still decides what it always decided
elsewhere - how the NEXT curve is placed, which named_points_set says "is a
property of the type, known before anything is built" - and here only whose
"nothing yet" an empty panel shows (FrameworkFillsStructure). The framework's
rows are read from the curves the client already holds, so building them under
a markup type builds nothing.

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

type
    { The latest rows each module pushed, per panel id. Parallel arrays. }
    THeldPanelRows = record
        Ids: array of string;
        Rows: array of TOutline;
    end;

{ Keeps ARows as what APanelId last pushed, replacing any earlier push. }
procedure HoldPanelRows(var AHeld: THeldPanelRows; const APanelId: string;
    const ARows: TOutline);

type
    { Everything the panel has been told, from which what it shows is composed:
      the framework's rows and each module's latest push. }
    TPanelSources = record
        FrameworkRows: TOutline;
        Held: THeldPanelRows;
    end;

    { What the panel shows: whose rows they are, and the rows. }
    TPanelContents = record
        PanelId: string;
        Rows: TOutline;
        { How many of Rows, from the first, are PanelId's own. The rest are
          the framework's rows for curves those do not stand for. }
        OwnRowCount: longint;
    end;

{ Keeps ARows where they belong: the framework's as its own, a module's as that
  module's latest push, replacing any earlier one.

  True when the panel is to be composed again from what is kept (ComposePanel).
  False for rows under an id no module claims (AModulePanelIds): nothing
  describes the model with those - the window's self-check pushes some - so
  they are shown as they came and not kept, or they would sit over the real
  model for good. }
function KeepPanelRows(var ASources: TPanelSources; const APanelId: string;
    const AModulePanelIds: array of string; const ARows: TOutline): boolean;

{ What the panel shows, composed from ASources.

  THE PANEL DESCRIBES THE MODEL, NOT THE TOOLS LIST. It used to follow the
  selected type - a module's rows were shown only while one of its own types
  was selected - so choosing an ordinary type to place next swapped a module's
  nested description for the framework's flat list of the same curves, and a
  model that was a hierarchy stopped looking like one. The selected type says
  what the user will place next; it says nothing about what is placed.

  So a module whose latest push names a curve the model holds describes part
  of it (DescribesTheModel), and its rows are shown whatever is selected. The selected type only breaks a tie
  between two modules that both describe something - the panel has one owner
  at a time, because its row menu asks that one - and the first to have pushed
  wins otherwise.

  NOTHING THE MODEL HOLDS IS DROPPED: the framework's rows for curves no row of
  that module stands for (by CurveId) follow its tree, at the top level. A
  model may hold curves of several kinds, and a panel that hid some of them
  would deny what the chart shows (D26).

  WHAT THE OLD RULE PROTECTED STILL HOLDS: a module with nothing placed pushes
  an empty outline on every redraw, and an empty push names no curve, so it
  never displaces the framework's rows. Only when there is nothing anywhere
  does the selection decide whose "nothing yet" is shown - the selected
  module's, if it has pushed, because only it knows what its markup is. }
function ComposePanel(const ASources: TPanelSources;
    const ASelectedPlacedByPointSet: string): TPanelContents;

{ What to tell the user when a link names ARowId and the panel is not showing
  it: RowKeptHint when the row is among kept rows that still describe the model
  (DescribesTheModel) - another module's rows outrank them - and RowGoneHint
  otherwise, including kept rows about a model no longer open, which no
  selection would bring back. }
function RowLinkMissHint(const ASources: TPanelSources;
    const ARowId: string): string;

{ Whether ARows describe the model whose own rows are AFrameworkRows: True when
  any of them stands for a curve the model holds (by CurveId), and when there
  are rows and none of them carries a handle at all - which cannot be judged,
  so is shown rather than hidden (a count whose wave ids are not GUIDs issues
  no handles).

  CHECKED AGAINST THE MODEL rather than cleared when a model is replaced:
  nothing would have to remember to clear a module's rows on close or open,
  and a module whose redraw exits early cannot leave its old tree over a model
  it has no part in. ONE CURVE IS ENOUGH, because a pattern just placed can be
  named a moment before the framework's rows include it. }
function DescribesTheModel(const ARows, AFrameworkRows: TOutline): boolean;

{ The panel id whose row menu ARowId asks for: the panel's own id over one of
  its own rows, FrameworkStructureId over a row listed after them. }
function RowMenuPanelId(const AContents: TPanelContents;
    const ARowId: string): string;

{ True when the selected curve ASelectedCurveId is not the one the selected row
  ARowId stands for, with that curve's handle in AHandle. Nothing to do with no
  row selected: a curve can be held with no row showing it. }
function CurveToReselect(const ARows: TOutline;
    const ARowId, ASelectedCurveId: string; out AHandle: string): boolean;

{ Whether two outlines draw the same panel - every field a row shows or is
  addressed by.

  WHY THE WINDOW ASKS. The panel is refreshed on every command refresh, and a
  rebuild clears the tree, opens every subtree the user closed and puts the
  selection back, which calls into the module that owns the rows. So a panel
  whose rows have not changed is not drawn again. }
function SameOutline(const A, B: TOutline): boolean;

const
    { A report's "show" named a row the panel keeps, but another module's
      rows are on show: only then does selecting a type bring it back. }
    RowKeptHint = 'That part of the model is described by a different curve ' +
        'pack from the one the Model panel is showing. Select one of its ' +
        'curve types to see it.';
    { ...or a row no contributor holds any more. }
    RowGoneHint = 'That part of the model is no longer in the Model panel.';


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

function KeepPanelRows(var ASources: TPanelSources; const APanelId: string;
    const AModulePanelIds: array of string; const ARows: TOutline): boolean;
var
    i: longint;
begin
    if APanelId = FrameworkStructureId then
    begin
        ASources.FrameworkRows := ARows;
        Exit(True);
    end;
    for i := 0 to High(AModulePanelIds) do
        if SameText(AModulePanelIds[i], APanelId) then
        begin
            HoldPanelRows(ASources.Held, APanelId, ARows);
            Exit(True);
        end;
    Result := False;
end;

function ComposePanel(const ASources: TPanelSources;
    const ASelectedPlacedByPointSet: string): TPanelContents;
var
    Owner, Selected, i, N: longint;
    Held: THeldPanelRows;
begin
    Result := Default(TPanelContents);
    Held := ASources.Held;
    Selected := -1;
    for i := 0 to High(Held.Ids) do
        if SameText(Held.Ids[i], Trim(ASelectedPlacedByPointSet)) then
            Selected := i;
    Owner := -1;
    if (Selected >= 0) and
        DescribesTheModel(Held.Rows[Selected], ASources.FrameworkRows) then
        Owner := Selected
    else
        for i := 0 to High(Held.Ids) do
            if DescribesTheModel(Held.Rows[i], ASources.FrameworkRows) then
            begin
                Owner := i;
                Break;
            end;

    if Owner < 0 then
    begin
        Result.PanelId := FrameworkStructureId;
        Result.Rows := ASources.FrameworkRows;
        Result.OwnRowCount := Length(Result.Rows);
        if (Length(ASources.FrameworkRows) = 0) and (Selected >= 0) then
            Result.PanelId := Held.Ids[Selected];
        Exit;
    end;

    Result.PanelId := Held.Ids[Owner];
    //  A COPY: appending to the held array itself would grow the module's kept
    //  push by the framework's rows every time the panel is composed.
    Result.Rows := Copy(Held.Rows[Owner]);
    N := Length(Result.Rows);
    Result.OwnRowCount := N;
    for i := 0 to High(ASources.FrameworkRows) do
        //  Matched by the curve the row STANDS FOR, not by row id: a module may
        //  identify its rows by its own markup and carry the handle beside it.
        if RowIndexForCurveHandle(Held.Rows[Owner],
            ASources.FrameworkRows[i].CurveId) < 0 then
        begin
            SetLength(Result.Rows, N + 1);
            Result.Rows[N] := ASources.FrameworkRows[i];
            Result.Rows[N].Indent := 0;
            Inc(N);
        end;
end;

function RowLinkMissHint(const ASources: TPanelSources;
    const ARowId: string): string;
var
    i, j: longint;
begin
    Result := RowGoneHint;
    for i := 0 to High(ASources.Held.Rows) do
        if DescribesTheModel(ASources.Held.Rows[i], ASources.FrameworkRows) then
            for j := 0 to High(ASources.Held.Rows[i]) do
                if ASources.Held.Rows[i][j].Id = ARowId then
                    Exit(RowKeptHint);
end;

function DescribesTheModel(const ARows, AFrameworkRows: TOutline): boolean;
var
    i: longint;
    AnyHandle: boolean;
begin
    AnyHandle := False;
    for i := 0 to High(ARows) do
    begin
        //  RowIndexForCurveHandle finds nothing for an empty handle, so a row
        //  standing for no curve says nothing either way.
        if RowIndexForCurveHandle(AFrameworkRows, ARows[i].CurveId) >= 0 then
            Exit(True);
        if ARows[i].CurveId <> '' then
            AnyHandle := True;
    end;
    //  NO HANDLE ANYWHERE cannot be judged, and is shown - as every push was
    //  before rows were checked against the model. Only a push naming curves,
    //  none of them the model's, is known to be about another model.
    Result := (Length(ARows) > 0) and not AnyHandle;
end;

function RowMenuPanelId(const AContents: TPanelContents;
    const ARowId: string): string;
var
    i: longint;
begin
    Result := AContents.PanelId;
    for i := AContents.OwnRowCount to High(AContents.Rows) do
        if AContents.Rows[i].Id = ARowId then
            Exit(FrameworkStructureId);
end;

function CurveToReselect(const ARows: TOutline;
    const ARowId, ASelectedCurveId: string; out AHandle: string): boolean;
begin
    AHandle := '';
    Result := False;
    if ARowId = '' then
        Exit;
    AHandle := CurveHandleForRowId(ARows, ARowId);
    Result := AHandle <> ASelectedCurveId;
end;

function SameOutline(const A, B: TOutline): boolean;
var
    i: longint;
begin
    Result := False;
    if Length(A) <> Length(B) then
        Exit;
    for i := 0 to High(A) do
        if (A[i].Indent <> B[i].Indent) or (A[i].Caption <> B[i].Caption) or
            (A[i].Id <> B[i].Id) or (A[i].CurveId <> B[i].CurveId) or
            (A[i].IsDetached <> B[i].IsDetached) or
            (A[i].Topic <> B[i].Topic) then
            Exit;
    Result := True;
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
