// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The tree a module's flattened outline describes.)

WHAT A MODULE SENDS is a depth-first list of rows, each carrying its own indent.
What the window shows is a tree. Turning one into the other is an algorithm with
a real invariant behind it - a row at indent N hangs from the last row seen at
indent N-1 - and it lived inside the method that fills a TTreeView, so it could
only be exercised by opening a window with a module installed, and the framework
ships no module.

WHY IT MATTERS THAT IT IS RIGHT. The outline is what a module shows the user
about their own data, and the node a row hangs from is a claim about what
belongs to what. A row re-parented by an off-by-one indent is not a rendering
glitch: it says a thing is part of something it is not, and it looks entirely
normal. Nothing else in the program would notice.

INDENT IS NOT TRUSTED BLINDLY. A row claiming to be deeper than one level below
the row before it has nowhere to hang - the level between them does not exist -
and the old code would have indexed an array slot that was never filled. Here
such a row is attached to the deepest level that does exist and reported as
detached, which is the same treatment a row whose parent was not found already
gets: shown differently rather than silently promoted.
}
unit outline_layout;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, module_view_types;

type
    { One node of the tree the rows describe, with its parent resolved to a
      position in the same array.

      An ARRAY WITH PARENT INDICES rather than nested records: it is what a tree
      control consumes one node at a time, it keeps the order explicit, and it
      can be compared in a test without walking anything. }
    TOutlineNode = record
        { What to show, with the detached suffix already applied. }
        Caption: string;
        { The module's own identity for the thing this row stands for. }
        Id: string;
        { Position of this node's parent in the same array, or -1 for a root. }
        ParentIndex: longint;
        { The level the node ended up at, which is not always the level the row
          asked for - see IsDetached. }
        Indent: longint;
        { True when this row could not hang where it said it should: either the
          module marked it detached, or its indent skipped a level that does not
          exist. }
        IsDetached: boolean;
    end;

    TOutlineNodes = array of TOutlineNode;

{ What a row is shown as. The suffix is the module's wording - it knows what a
  damaged row means in its own terms - and is appended rather than replacing the
  caption, because the row still has to be recognisable. }
function OutlineCaption(const ARow: TOutlineRow;
    const ADetachedSuffix: string): string;

{ The tree the rows describe. }
function BuildOutlineNodes(const ARows: TOutline;
    const ADetachedSuffix: string): TOutlineNodes;

{ Where the node carrying AId sits, or -1 when no node does.

  Used to put a selection back after a rebuild. BY IDENTITY, never by position:
  a rebuild reorders rows, and restoring a selection by row number points it at
  whatever now occupies that row. }
function IndexOfOutlineId(const ANodes: TOutlineNodes;
    const AId: string): longint;

{ How deep the tree goes; 0 when it is empty. }
function OutlineDepth(const ANodes: TOutlineNodes): longint;

implementation

function OutlineCaption(const ARow: TOutlineRow;
    const ADetachedSuffix: string): string;
begin
    Result := ARow.Caption;
    if ARow.IsDetached then
        Result := Result + ADetachedSuffix;
end;

function BuildOutlineNodes(const ARows: TOutline;
    const ADetachedSuffix: string): TOutlineNodes;
var
    i, Level, Depth: longint;
    //  Level -> the index of the node most recently opened at that level. The
    //  rows are depth-first with parents before children, so the node a row
    //  hangs from is always the last one seen one level shallower.
    OpenAt: array of longint;
    Detached: boolean;
    Row: TOutlineRow;
begin
    SetLength(Result, Length(ARows));
    SetLength(OpenAt, Length(ARows) + 1);
    for i := 0 to High(OpenAt) do
        OpenAt[i] := -1;

    //  How deep the tree is SO FAR. A row may sit one level below the deepest
    //  open node and no further.
    Depth := 0;

    for i := 0 to High(ARows) do
    begin
        Row := ARows[i];
        Level := Row.Indent;
        if Level < 0 then
            Level := 0;

        Detached := Row.IsDetached;
        if Level > Depth then
        begin
            //  A LEVEL THAT DOES NOT EXIST. The row asked to hang from a parent
            //  no earlier row opened, so it is put at the deepest level that is
            //  real and marked - the same treatment a row whose parent could not
            //  be found already gets.
            Level := Depth;
            Detached := True;
        end;

        Result[i].Id := Row.Id;
        Result[i].Indent := Level;
        Result[i].IsDetached := Detached;
        Row.IsDetached := Detached;
        Result[i].Caption := OutlineCaption(Row, ADetachedSuffix);

        if Level = 0 then
            Result[i].ParentIndex := -1
        else
            Result[i].ParentIndex := OpenAt[Level - 1];

        OpenAt[Level] := i;
        //  Everything below this level is closed: the next row at one of those
        //  levels opens a new node rather than hanging from a stale one.
        Depth := Level + 1;
    end;
end;

function IndexOfOutlineId(const ANodes: TOutlineNodes;
    const AId: string): longint;
var
    i: longint;
begin
    Result := -1;
    if AId = '' then
        Exit;
    for i := 0 to High(ANodes) do
        if ANodes[i].Id = AId then
            Exit(i);
end;

function OutlineDepth(const ANodes: TOutlineNodes): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(ANodes) do
        if ANodes[i].Indent + 1 > Result then
            Result := ANodes[i].Indent + 1;
end;

end.
