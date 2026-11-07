// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which rows of the Model panel stay collapsed when it is drawn again.)

WHY THIS EXISTS. The panel is drawn again whenever what it shows changes - a fit
moves a curve, a pattern is placed, a curve is deleted - and drawing it again
clears the tree and opens every subtree. A user who closed a pattern to read the
rest of a long count saw it open again after every change to the model. A
refresh that changed nothing was already kept from drawing at all; a real change
has to draw, so what the user closed has to be carried across it.

KEYED BY THE ROW'S ID, never by position. The id is the identity the selection
already survives a rebuild by, and for the framework's rows and a module's alike
it is the handle of the curve the row stands for (module_view_types), which a fit
keeps and a move keeps. A position is whatever row now occupies that place.

WHAT THE USER CLOSED IS READ OFF THE TREE before it is cleared, not kept by
listening to the tree's collapse events: the tree as drawn is the one record of
what the user did, and a second record kept beside it could disagree with it.

A ROW STAYS REMEMBERED while it is drawn, children or not: a pattern whose nested
rows are gone for one change and back in the next is still the pattern the user
closed. A row the panel no longer draws is forgotten, so the list does not grow
with every curve ever deleted.
}
unit outline_collapse;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, outline_layout;

type
    { One node of the tree as it is drawn now: the row it shows, whether rows
      hang from it, and whether it is open. }
    TDrawnNode = record
        Id: string;
        HasChildren: boolean;
        Expanded: boolean;
    end;
    TDrawnNodes = array of TDrawnNode;

    TRowIds = array of string;
    TNodeFlags = array of boolean;

{ The rows the user has closed, once the tree as drawn is read: AKept, the rows
  remembered closed, with each drawn row that has rows under it added when it is
  closed and dropped when it is open. A row with nothing under it, and a node
  with no row behind it (the empty panel's sentence), say nothing either way. }
function CollapsedRowsOf(const AKept: TRowIds;
    const ADrawn: TDrawnNodes): TRowIds;

{ What is remembered once the panel is drawn from ANodes: the rows of
  ACollapsed that it still draws. }
function CollapsedRowsStillShown(const ACollapsed: TRowIds;
    const ANodes: TOutlineNodes): TRowIds;

{ For each of ANodes, whether it is drawn closed: its row is one of ACollapsed
  and a row hangs from it. A row with nothing under it is not closed, because
  closing it hides nothing and a later child would arrive hidden. }
function NodesDrawnCollapsed(const ANodes: TOutlineNodes;
    const ACollapsed: TRowIds): TNodeFlags;

implementation

function IndexOfRow(const AIds: TRowIds; const AId: string): longint;
begin
    for Result := 0 to High(AIds) do
        if AIds[Result] = AId then
            Exit;
    Result := -1;
end;

procedure AddRow(var AIds: TRowIds; const AId: string);
begin
    SetLength(AIds, Length(AIds) + 1);
    AIds[High(AIds)] := AId;
end;

function CollapsedRowsOf(const AKept: TRowIds;
    const ADrawn: TDrawnNodes): TRowIds;
var
    i, At: longint;
begin
    Result := Copy(AKept);
    for i := 0 to High(ADrawn) do
    begin
        //  A LEAF SAYS NOTHING: it reports itself closed, and remembering it
        //  would hide the first row that ever hangs from it.
        if (ADrawn[i].Id = '') or not ADrawn[i].HasChildren then
            Continue;
        At := IndexOfRow(Result, ADrawn[i].Id);
        if ADrawn[i].Expanded and (At >= 0) then
            Delete(Result, At, 1)
        else if (not ADrawn[i].Expanded) and (At < 0) then
            AddRow(Result, ADrawn[i].Id);
    end;
end;

function CollapsedRowsStillShown(const ACollapsed: TRowIds;
    const ANodes: TOutlineNodes): TRowIds;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(ACollapsed) do
        if IndexOfOutlineId(ANodes, ACollapsed[i]) >= 0 then
            AddRow(Result, ACollapsed[i]);
end;

function NodesDrawnCollapsed(const ANodes: TOutlineNodes;
    const ACollapsed: TRowIds): TNodeFlags;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(ANodes));
    //  A node is closed when a row hangs from it: the rows name their parents,
    //  so it is each child that marks its parent.
    for i := 0 to High(ANodes) do
        if (ANodes[i].ParentIndex >= 0) and
            (IndexOfRow(ACollapsed, ANodes[ANodes[i].ParentIndex].Id) >= 0) then
            Result[ANodes[i].ParentIndex] := True;
end;

end.
