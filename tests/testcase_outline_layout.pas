// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The tree a module's flattened outline describes.)

WHAT IS AT STAKE. The outline is what a module shows the user about their own
data, and the node a row hangs from is a claim about what belongs to what. A row
re-parented by an off-by-one indent is not a rendering glitch - it says a thing
is part of something it is not, and it looks entirely normal on screen. Nothing
else in the program would notice, and nothing did: the flattening lived inside
the method that fills a TTreeView, so exercising it needed a window with a module
installed, and the framework ships no module at all.

The rows here are written by hand, which is the point: a module's real outline is
whatever that module decided, and what is being tested is the rule that turns any
such list into a tree.
}
unit testcase_outline_layout;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, outline_layout, module_view_types;

type
    TOutlineLayoutTest = class(TTestCase)
    private
        FRows: TOutline;
        FNodes: TOutlineNodes;
        procedure AddRow(AIndent: longint; const ACaption, AId: string;
            ADetached: boolean = False);
        procedure Build(const ASuffix: string = ' (detached)');
    protected
        procedure SetUp; override;
    published
        //  The shape.
        procedure AnEmptyOutlineIsAnEmptyTree;
        procedure AFlatListIsAllRoots;
        procedure AChildHangsFromTheRowAbove;
        procedure ASecondChildHangsFromTheSameParent;
        procedure AGrandchildHangsFromItsOwnParent;
        procedure ReturningToATopLevelStartsANewBranch;
        procedure AShallowerRowDoesNotHangFromADeeperOne;
        procedure EveryParentComesBeforeItsChild;
        procedure TheOrderOfTheRowsIsKept;

        //  Damage.
        procedure ARowMarkedDetachedSaysSo;
        procedure ARowThatSkipsALevelIsAttachedWhereItCanBe;
        procedure ARowThatSkipsALevelIsReportedAsDetached;
        procedure ANegativeIndentIsARoot;
        procedure AnUndamagedRowIsNotDecorated;
        procedure AnEmptySuffixDecoratesNothing;

        //  Identity.
        procedure ARowIsFoundByItsId;
        procedure AnUnknownIdIsNotFound;
        procedure AnEmptyIdMatchesNothing;
        procedure TheIdIsCarriedThroughUnchanged;

        //  Depth.
        procedure TheDepthOfAFlatListIsOne;
        procedure TheDepthCountsTheDeepestBranch;
        procedure AnEmptyOutlineHasNoDepth;
    end;

implementation

procedure TOutlineLayoutTest.SetUp;
begin
    SetLength(FRows, 0);
    SetLength(FNodes, 0);
end;

procedure TOutlineLayoutTest.AddRow(AIndent: longint;
    const ACaption, AId: string; ADetached: boolean = False);
var
    Row: TOutlineRow;
begin
    Row := Default(TOutlineRow);
    Row.Indent := AIndent;
    Row.Caption := ACaption;
    Row.Id := AId;
    Row.IsDetached := ADetached;
    SetLength(FRows, Length(FRows) + 1);
    FRows[High(FRows)] := Row;
end;

procedure TOutlineLayoutTest.Build(const ASuffix: string = ' (detached)');
begin
    FNodes := BuildOutlineNodes(FRows, ASuffix);
end;

{ ---- the shape ------------------------------------------------------------- }

procedure TOutlineLayoutTest.AnEmptyOutlineIsAnEmptyTree;
begin
    //  A module with nothing marked yet. The window shows its own "nothing yet"
    //  wording for this, so producing a node here would put a phantom row in it.
    Build;
    AssertEquals('no nodes', 0, Length(FNodes));
end;

procedure TOutlineLayoutTest.AFlatListIsAllRoots;
begin
    AddRow(0, 'one', 'a');
    AddRow(0, 'two', 'b');
    Build;
    AssertEquals('two nodes', 2, Length(FNodes));
    AssertEquals('the first is a root', -1, FNodes[0].ParentIndex);
    AssertEquals('and so is the second', -1, FNodes[1].ParentIndex);
end;

procedure TOutlineLayoutTest.AChildHangsFromTheRowAbove;
begin
    //  THE WHOLE RULE, in its simplest case: depth-first with parents before
    //  children, so a row at indent 1 belongs to the last row at indent 0.
    AddRow(0, 'parent', 'p');
    AddRow(1, 'child', 'c');
    Build;
    AssertEquals('the child hangs from the parent', 0, FNodes[1].ParentIndex);
end;

procedure TOutlineLayoutTest.ASecondChildHangsFromTheSameParent;
begin
    AddRow(0, 'parent', 'p');
    AddRow(1, 'first', 'c1');
    AddRow(1, 'second', 'c2');
    Build;
    AssertEquals('the first', 0, FNodes[1].ParentIndex);
    AssertEquals('and the second, not the first child', 0,
        FNodes[2].ParentIndex);
end;

procedure TOutlineLayoutTest.AGrandchildHangsFromItsOwnParent;
begin
    AddRow(0, 'root', 'r');
    AddRow(1, 'child', 'c');
    AddRow(2, 'grandchild', 'g');
    Build;
    AssertEquals('the grandchild hangs from the child', 1,
        FNodes[2].ParentIndex);
end;

procedure TOutlineLayoutTest.ReturningToATopLevelStartsANewBranch;
begin
    //  THE CASE THAT NEEDS THE LEVELS CLOSED. After descending to indent 2, a
    //  row back at indent 1 must hang from the ROOT, not from the node that was
    //  open at level 0 before the descent - which is the same node, but only by
    //  luck. The row after it at indent 2 must then hang from the NEW level-1
    //  node and not from the old one.
    AddRow(0, 'root', 'r');
    AddRow(1, 'branch one', 'b1');
    AddRow(2, 'leaf one', 'l1');
    AddRow(1, 'branch two', 'b2');
    AddRow(2, 'leaf two', 'l2');
    Build;
    AssertEquals('branch two hangs from the root', 0, FNodes[3].ParentIndex);
    AssertEquals('leaf two hangs from branch two, not branch one', 3,
        FNodes[4].ParentIndex);
end;

procedure TOutlineLayoutTest.AShallowerRowDoesNotHangFromADeeperOne;
begin
    //  Coming back up two levels at once.
    AddRow(0, 'root', 'r');
    AddRow(1, 'branch', 'b');
    AddRow(2, 'leaf', 'l');
    AddRow(0, 'another root', 'r2');
    Build;
    AssertEquals('back to a root', -1, FNodes[3].ParentIndex);
end;

procedure TOutlineLayoutTest.EveryParentComesBeforeItsChild;
var
    i: longint;
begin
    //  A TREE CONTROL IS FILLED IN ORDER, so a node whose parent has not been
    //  created yet cannot be attached at all. This holds for any input the rule
    //  accepts, so it is asserted over a deliberately awkward one.
    AddRow(0, 'a', 'a');
    AddRow(1, 'b', 'b');
    AddRow(2, 'c', 'c');
    AddRow(1, 'd', 'd');
    AddRow(0, 'e', 'e');
    AddRow(1, 'f', 'f');
    Build;
    for i := 0 to High(FNodes) do
        AssertTrue(Format('node %d parent %d comes first',
            [i, FNodes[i].ParentIndex]), FNodes[i].ParentIndex < i);
end;

procedure TOutlineLayoutTest.TheOrderOfTheRowsIsKept;
begin
    //  The module chose the order and it means something - it is the order the
    //  user sees. Nothing here may sort or regroup.
    AddRow(0, 'first', 'a');
    AddRow(1, 'second', 'b');
    AddRow(0, 'third', 'c');
    Build;
    AssertEquals('first', 'a', FNodes[0].Id);
    AssertEquals('second', 'b', FNodes[1].Id);
    AssertEquals('third', 'c', FNodes[2].Id);
end;

{ ---- damage ---------------------------------------------------------------- }

procedure TOutlineLayoutTest.ARowMarkedDetachedSaysSo;
begin
    //  An orphan sits at the top level, which is exactly where a genuine root
    //  sits - so without the suffix the damage is invisible.
    AddRow(0, 'orphan', 'o', True);
    Build(' (detached)');
    AssertEquals('decorated', 'orphan (detached)', FNodes[0].Caption);
    AssertTrue('and flagged', FNodes[0].IsDetached);
end;

procedure TOutlineLayoutTest.ARowThatSkipsALevelIsAttachedWhereItCanBe;
begin
    //  A row at indent 2 with no indent-1 row before it has nowhere to hang.
    //  The old code indexed an array slot that had never been filled; here it is
    //  attached to the deepest level that exists.
    AddRow(0, 'root', 'r');
    AddRow(2, 'too deep', 'd');
    Build;
    AssertEquals('attached to the root', 0, FNodes[1].ParentIndex);
    AssertEquals('at the level that exists', 1, FNodes[1].Indent);
end;

procedure TOutlineLayoutTest.ARowThatSkipsALevelIsReportedAsDetached;
begin
    //  SHOWN, not silently fixed. A row that could not hang where it said it
    //  should means the module's data is damaged, and that is worth seeing -
    //  the same treatment a row whose parent could not be found already gets.
    AddRow(0, 'root', 'r');
    AddRow(3, 'lost', 'd');
    Build(' (detached)');
    AssertTrue('flagged', FNodes[1].IsDetached);
    AssertEquals('and decorated', 'lost (detached)', FNodes[1].Caption);
end;

procedure TOutlineLayoutTest.ANegativeIndentIsARoot;
begin
    //  Nonsense from a module must not index backwards through the level array.
    AddRow(-3, 'odd', 'o');
    Build;
    AssertEquals('a root', -1, FNodes[0].ParentIndex);
    AssertEquals('at level zero', 0, FNodes[0].Indent);
end;

procedure TOutlineLayoutTest.AnUndamagedRowIsNotDecorated;
begin
    AddRow(0, 'fine', 'f');
    Build(' (detached)');
    AssertEquals('untouched', 'fine', FNodes[0].Caption);
    AssertFalse('and not flagged', FNodes[0].IsDetached);
end;

procedure TOutlineLayoutTest.AnEmptySuffixDecoratesNothing;
begin
    //  A module that supplies no wording gets no decoration rather than a
    //  caption with a trailing space.
    AddRow(0, 'orphan', 'o', True);
    Build('');
    AssertEquals('unchanged', 'orphan', FNodes[0].Caption);
    AssertTrue('but still flagged', FNodes[0].IsDetached);
end;

{ ---- identity -------------------------------------------------------------- }

procedure TOutlineLayoutTest.ARowIsFoundByItsId;
begin
    //  BY IDENTITY, NEVER BY POSITION. A rebuild reorders rows, and restoring a
    //  selection by row number points it at whatever now occupies that row -
    //  which the user then acts on believing it is what they had selected.
    AddRow(0, 'one', 'a');
    AddRow(0, 'two', 'b');
    AddRow(0, 'three', 'c');
    Build;
    AssertEquals('found', 1, IndexOfOutlineId(FNodes, 'b'));
end;

procedure TOutlineLayoutTest.AnUnknownIdIsNotFound;
begin
    //  A row the module has deleted. The selection is correctly dropped rather
    //  than left pointing somewhere.
    AddRow(0, 'one', 'a');
    Build;
    AssertEquals('not found', -1, IndexOfOutlineId(FNodes, 'gone'));
end;

procedure TOutlineLayoutTest.AnEmptyIdMatchesNothing;
begin
    //  "Nothing was selected" is the empty id, and it must not match a row that
    //  happens to carry no id of its own.
    AddRow(0, 'nameless', '');
    Build;
    AssertEquals('no match', -1, IndexOfOutlineId(FNodes, ''));
end;

procedure TOutlineLayoutTest.TheIdIsCarriedThroughUnchanged;
begin
    //  The id addresses the thing the row stands for, inside the module. Any
    //  decoration applied to it would break the selection round trip.
    AddRow(0, 'shown differently', '{0B0E-ID}', True);
    Build(' (detached)');
    AssertEquals('untouched', '{0B0E-ID}', FNodes[0].Id);
    AssertEquals('and still findable', 0,
        IndexOfOutlineId(FNodes, '{0B0E-ID}'));
end;

{ ---- depth ----------------------------------------------------------------- }

procedure TOutlineLayoutTest.TheDepthOfAFlatListIsOne;
begin
    AddRow(0, 'one', 'a');
    AddRow(0, 'two', 'b');
    Build;
    AssertEquals('one level', 1, OutlineDepth(FNodes));
end;

procedure TOutlineLayoutTest.TheDepthCountsTheDeepestBranch;
begin
    AddRow(0, 'root', 'r');
    AddRow(1, 'branch', 'b');
    AddRow(2, 'leaf', 'l');
    AddRow(0, 'another', 'r2');
    Build;
    AssertEquals('three levels', 3, OutlineDepth(FNodes));
end;

procedure TOutlineLayoutTest.AnEmptyOutlineHasNoDepth;
begin
    Build;
    AssertEquals('none', 0, OutlineDepth(FNodes));
end;

initialization
    //  A unit test: records in, records out. No tree, no window, and no module -
    //  which is why none of this had ever been exercised.
    RegisterTest('unit', TOutlineLayoutTest);
end.
