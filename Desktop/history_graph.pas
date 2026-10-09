// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where each model sits in the History tab's lineage graph, and which
lines run through its row - the layout `git log --graph` draws, for a tree.)

THE SHAPE. Rows are drawn newest first, so a model is always above the one it
was fitted from. Each row has a node in one LANE (a column). A line runs down
from a node to its parent's row; when two models were fitted from the same one,
their lines meet at its node - the point the work branched. Lanes are reused as
soon as a branch has met its parent, so the graph is as narrow as the history's
widest moment rather than as wide as its number of branches.

A TREE, NOT A GRAPH WITH MERGES. Every model has at most one parent - the model
its run started from - so no row ever has two lines leaving it downwards. That is
what keeps this simple enough to be exhaustively right; combining two models into
one is not something a fit does.

WHY IT IS HERE. Drawn wrongly, a lineage graph misleads silently: a line into the
wrong node says a model came from somewhere it did not. The canvas cannot be
asked what it drew, so the decision is made here, as numbers, and the window only
strokes them.
}
unit history_graph;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    TLaneList = array of longint;

    THistoryGraphRow = record
        { The node's column. }
        Lane: longint;
        { Lanes whose line enters this row from the top and leaves at the
          bottom, untouched: a branch passing by. }
        Through: TLaneList;
        { Lanes whose line enters from the top and ends at this row's node:
          the models drawn above that were fitted from this one. }
        Into: TLaneList;
        { Whether a line leaves the node downwards, towards its parent. }
        Down: boolean;
        { How many lanes this row uses, so the text can start after them. }
        Width: longint;
    end;
    THistoryGraphRows = array of THistoryGraphRow;

{ The layout of rows whose ids and parents are AIds and AParents, in DISPLAY
  order - newest first. A parent that is '' or not below its child starts no
  line. }
function HistoryGraphOf(const AIds, AParents: array of string): THistoryGraphRows;

{ The widest row, in lanes: where the text of every row starts, so the rows
  line up. At least 1. }
function HistoryGraphWidth(const ARows: THistoryGraphRows): longint;

implementation

procedure Append(var AList: TLaneList; AValue: longint);
begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AValue;
end;

function HistoryGraphOf(const AIds, AParents: array of string): THistoryGraphRows;
var
    { What each lane is waiting for: the id of the row its line runs down to,
      or '' when the lane is free. }
    Waiting: array of string;
    r, k, j, Used: longint;
    ParentBelow: boolean;
begin
    Result := nil;
    SetLength(Result, Length(AIds));
    Waiting := nil;
    for r := 0 to High(AIds) do
    begin
        Result[r] := Default(THistoryGraphRow);
        Result[r].Lane := -1;
        //  THE LINES ARRIVING: every lane waiting for this row ends here, and
        //  the node takes the leftmost of them - so a chain stays straight.
        for k := 0 to High(Waiting) do
            if Waiting[k] = AIds[r] then
            begin
                Append(Result[r].Into, k);
                if Result[r].Lane < 0 then
                    Result[r].Lane := k;
            end;
        //  NONE ARRIVING: a model nothing below the rows above was fitted
        //  from - the newest of a branch. It takes the first free lane.
        if Result[r].Lane < 0 then
        begin
            k := 0;
            while (k <= High(Waiting)) and (Waiting[k] <> '') do
                Inc(k);
            if k > High(Waiting) then
                SetLength(Waiting, k + 1);
            Result[r].Lane := k;
        end;
        for k := 0 to High(Waiting) do
            if (Waiting[k] <> '') and (Waiting[k] <> AIds[r]) then
                Append(Result[r].Through, k);
        for k := 0 to High(Result[r].Into) do
            Waiting[Result[r].Into[k]] := '';

        //  A LINE DOWN only to a parent that IS below: a parent named and not
        //  there would draw a line that ends nowhere.
        ParentBelow := False;
        if AParents[r] <> '' then
            for j := r + 1 to High(AIds) do
                if AIds[j] = AParents[r] then
                begin
                    ParentBelow := True;
                    Break;
                end;
        Result[r].Down := ParentBelow;
        Used := Length(Waiting);
        if ParentBelow then
            Waiting[Result[r].Lane] := AParents[r];

        Result[r].Width := Used;
        if Result[r].Width < Result[r].Lane + 1 then
            Result[r].Width := Result[r].Lane + 1;
        //  FREED LANES AT THE RIGHT ARE DROPPED, so the graph narrows again
        //  once a branch has met its parent.
        while (Length(Waiting) > 0) and (Waiting[High(Waiting)] = '') do
            SetLength(Waiting, Length(Waiting) - 1);
    end;
end;

function HistoryGraphWidth(const ARows: THistoryGraphRows): longint;
var
    i: longint;
begin
    Result := 1;
    for i := 0 to High(ARows) do
        if ARows[i].Width > Result then
            Result := ARows[i].Width;
end;

end.
