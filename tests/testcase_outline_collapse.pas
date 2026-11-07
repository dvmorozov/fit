// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which rows of the Model panel stay collapsed when it is drawn again.)

WHAT IS AT STAKE. A user closes a pattern in the Model panel to read the rest of
a long count; the next change to the model draws the panel again, and drawing it
again opened every subtree. These tests walk what the window does around a
redraw - read the tree, build the new nodes, close what was closed - over plain
records, which is where the decision lives. That the window really calls them is
checked inside the running application (ui_selfcheck's Model panel reading).
}
unit testcase_outline_collapse;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, outline_collapse, outline_layout,
    module_view_types;

type
    TOutlineCollapseTest = class(TTestCase)
    private
        function Drawn(const AId: string; AHasChildren,
            AExpanded: boolean): TDrawnNode;
        { A parent row P with one child C, and a separate root R, as the panel
          builds them; ACaption stands in for what a change to the model alters. }
        function ParentChildAndRoot(const ACaption: string): TOutlineNodes;
    published
        procedure ARowTheUserClosedStaysClosedAfterTheModelChanges;
        procedure ARowTheUserOpenedAgainIsForgotten;
        procedure ARowThatWasNeverClosedIsDrawnOpen;
        procedure ARowWithNothingUnderItSaysNothing;
        procedure TheEmptyPanelsSentenceSaysNothing;
        procedure ARowNoLongerDrawnIsForgotten;
        procedure AClosedRowWhoseChildrenAreGoneForAChangeIsStillRemembered;
        procedure AClosedRowWithNothingUnderItIsNotDrawnClosed;
        procedure TheCollapseFollowsTheRowNotItsPosition;
    end;

implementation

function TOutlineCollapseTest.Drawn(const AId: string; AHasChildren,
    AExpanded: boolean): TDrawnNode;
begin
    Result.Id := AId;
    Result.HasChildren := AHasChildren;
    Result.Expanded := AExpanded;
end;

function TOutlineCollapseTest.ParentChildAndRoot(
    const ACaption: string): TOutlineNodes;
var
    Rows: TOutline;
begin
    Rows := nil;
    SetLength(Rows, 3);
    Rows[0].Id := 'P';
    Rows[0].Caption := ACaption;
    Rows[0].Indent := 0;
    Rows[1].Id := 'C';
    Rows[1].Caption := 'child';
    Rows[1].Indent := 1;
    Rows[2].Id := 'R';
    Rows[2].Caption := 'root';
    Rows[2].Indent := 0;
    Result := BuildOutlineNodes(Rows, '');
end;

{ THE DEFECT. The parent closed as a user closes it, then a change to the model
  draws the panel again from rows whose captions moved: the parent is drawn
  closed, its sibling open. }
procedure TOutlineCollapseTest.ARowTheUserClosedStaysClosedAfterTheModelChanges;
var
    Collapsed: TRowIds;
    Nodes: TOutlineNodes;
    Flags: TNodeFlags;
begin
    Collapsed := CollapsedRowsOf(nil, [Drawn('P', True, False),
        Drawn('C', False, False), Drawn('R', False, True)]);
    Nodes := ParentChildAndRoot('parent, fitted');
    Collapsed := CollapsedRowsStillShown(Collapsed, Nodes);
    Flags := NodesDrawnCollapsed(Nodes, Collapsed);
    AssertEquals(3, Length(Flags));
    AssertTrue('the parent the user closed is drawn closed', Flags[0]);
    AssertFalse('its child is not a closed row', Flags[1]);
    AssertFalse('the other root stays open', Flags[2]);
end;

procedure TOutlineCollapseTest.ARowTheUserOpenedAgainIsForgotten;
var
    Collapsed: TRowIds;
begin
    Collapsed := CollapsedRowsOf(['P'], [Drawn('P', True, True)]);
    AssertEquals(0, Length(Collapsed));
end;

procedure TOutlineCollapseTest.ARowThatWasNeverClosedIsDrawnOpen;
var
    Flags: TNodeFlags;
begin
    Flags := NodesDrawnCollapsed(ParentChildAndRoot('parent'),
        CollapsedRowsOf(nil, [Drawn('P', True, True)]));
    AssertFalse(Flags[0]);
end;

{ A leaf reports itself not expanded; read as closed, every leaf would be
  remembered and its first child would arrive hidden. }
procedure TOutlineCollapseTest.ARowWithNothingUnderItSaysNothing;
var
    Collapsed: TRowIds;
begin
    Collapsed := CollapsedRowsOf(nil, [Drawn('R', False, False)]);
    AssertEquals('a leaf is not a closed row', 0, Length(Collapsed));
    Collapsed := CollapsedRowsOf(['R'], [Drawn('R', False, True)]);
    AssertEquals('nor an opened one', 1, Length(Collapsed));
end;

procedure TOutlineCollapseTest.TheEmptyPanelsSentenceSaysNothing;
begin
    AssertEquals(0, Length(CollapsedRowsOf(nil, [Drawn('', True, False)])));
end;

procedure TOutlineCollapseTest.ARowNoLongerDrawnIsForgotten;
var
    Collapsed: TRowIds;
begin
    //  'Q' was closed, and its curve deleted since.
    Collapsed := CollapsedRowsStillShown(['P', 'Q'], ParentChildAndRoot('p'));
    AssertEquals(1, Length(Collapsed));
    AssertEquals('P', Collapsed[0]);
end;

procedure TOutlineCollapseTest.AClosedRowWhoseChildrenAreGoneForAChangeIsStillRemembered;
var
    Rows: TOutline;
    Collapsed: TRowIds;
begin
    Rows := nil;
    SetLength(Rows, 1);
    Rows[0].Id := 'P';
    Rows[0].Indent := 0;
    Collapsed := CollapsedRowsStillShown(['P'], BuildOutlineNodes(Rows, ''));
    AssertEquals(1, Length(Collapsed));
    //  And when its child is back, it is drawn closed.
    AssertTrue(NodesDrawnCollapsed(ParentChildAndRoot('p'), Collapsed)[0]);
end;

procedure TOutlineCollapseTest.AClosedRowWithNothingUnderItIsNotDrawnClosed;
begin
    AssertFalse(NodesDrawnCollapsed(ParentChildAndRoot('p'), ['R'])[2]);
end;

procedure TOutlineCollapseTest.TheCollapseFollowsTheRowNotItsPosition;
var
    Rows: TOutline;
    Flags: TNodeFlags;
begin
    //  A new root placed before the closed parent moves it down one place.
    Rows := nil;
    SetLength(Rows, 3);
    Rows[0].Id := 'N';
    Rows[0].Indent := 0;
    Rows[1].Id := 'P';
    Rows[1].Indent := 0;
    Rows[2].Id := 'C';
    Rows[2].Indent := 1;
    Flags := NodesDrawnCollapsed(BuildOutlineNodes(Rows, ''), ['P']);
    AssertFalse('the new row is open', Flags[0]);
    AssertTrue('the closed row is closed where it now is', Flags[1]);
end;

initialization
    RegisterTest('unit', TOutlineCollapseTest);
end.
