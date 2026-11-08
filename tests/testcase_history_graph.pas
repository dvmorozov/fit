// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The History tab's lineage graph as numbers: which lane each model sits
in, and which lines run through, into and out of its row.)

A unit test. Each fixture is a small history written newest first, as the tab
shows it; each assertion is a fact a person reading the graph would rely on.
}
unit testcase_history_graph;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, history_graph;

type
    THistoryGraphTest = class(TTestCase)
    private
        function Lanes(const AList: TLaneList): string;
    published
        procedure AnEmptyHistoryDrawsNothing;
        procedure OneModelIsANodeWithNoLines;
        procedure AChainIsOneStraightLine;
        procedure TwoModelsFittedFromOneMeetAtItsNode;
        procedure ABranchPassesByTheRowsBetween;
        procedure ALaneIsReusedOnceItsBranchHasMet;
        procedure TwoUnrelatedHistoriesDrawSideBySide;
        procedure AParentNotBelowStartsNoLine;
        procedure ManyBranchesFromOneModel;
        procedure TheWidthIsTheWidestRow;
        procedure ALongSessionOfFitsIsStillOneLane;
        procedure EveryRowHasANodeInALaneItDrawsIn;
    end;

implementation

function THistoryGraphTest.Lanes(const AList: TLaneList): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AList) do
    begin
        if Result <> '' then
            Result := Result + ',';
        Result := Result + IntToStr(AList[i]);
    end;
end;

procedure THistoryGraphTest.AnEmptyHistoryDrawsNothing;
begin
    AssertEquals(0, Length(HistoryGraphOf([], [])));
    AssertEquals('still one lane wide, so text has somewhere to start', 1,
        HistoryGraphWidth(nil));
end;

procedure THistoryGraphTest.OneModelIsANodeWithNoLines;
var
    R: THistoryGraphRows;
begin
    R := HistoryGraphOf(['a'], ['']);
    AssertEquals('lane 0', 0, R[0].Lane);
    AssertFalse('nothing below', R[0].Down);
    AssertEquals('nothing above', '', Lanes(R[0].Into));
    AssertEquals('nothing passing', '', Lanes(R[0].Through));
end;

procedure THistoryGraphTest.AChainIsOneStraightLine;
var
    R: THistoryGraphRows;
begin
    //  c <- b <- a, newest first.
    R := HistoryGraphOf(['c', 'b', 'a'], ['b', 'a', '']);
    AssertEquals('c', 0, R[0].Lane);
    AssertEquals('b', 0, R[1].Lane);
    AssertEquals('a', 0, R[2].Lane);
    AssertTrue('c goes down to b', R[0].Down);
    AssertEquals('and arrives', '0', Lanes(R[1].Into));
    AssertTrue('b goes down to a', R[1].Down);
    AssertFalse('a is the start', R[2].Down);
end;

procedure THistoryGraphTest.TwoModelsFittedFromOneMeetAtItsNode;
var
    R: THistoryGraphRows;
begin
    //  c and b were both fitted from a.
    R := HistoryGraphOf(['c', 'b', 'a'], ['a', 'a', '']);
    AssertEquals('c', 0, R[0].Lane);
    AssertEquals('b on a lane of its own', 1, R[1].Lane);
    AssertEquals('c''s line passes b', '0', Lanes(R[1].Through));
    AssertEquals('a', 0, R[2].Lane);
    AssertEquals('both lines end at a', '0,1', Lanes(R[2].Into));
end;

procedure THistoryGraphTest.ABranchPassesByTheRowsBetween;
var
    R: THistoryGraphRows;
begin
    //  d from a, c from b, b from a: d's line runs past c and b to a.
    R := HistoryGraphOf(['d', 'c', 'b', 'a'], ['a', 'b', 'a', '']);
    AssertEquals('c beside it', 1, R[1].Lane);
    AssertEquals('d''s line passes c', '0', Lanes(R[1].Through));
    AssertEquals('b continues c''s lane', 1, R[2].Lane);
    AssertEquals('and d''s line passes b', '0', Lanes(R[2].Through));
    AssertEquals('both end at a', '0,1', Lanes(R[3].Into));
end;

procedure THistoryGraphTest.ALaneIsReusedOnceItsBranchHasMet;
var
    R: THistoryGraphRows;
begin
    //  e from c, d from b, c from a, b from a, a. After b and c meet at a
    //  their lanes are free - but here e and d come first, so: e lane 0,
    //  d lane 1, c lane 0 (continuing e), b lane 1 (continuing d), a lane 0.
    //  Then a SECOND history entirely below: f, unrelated, takes lane 0 again.
    R := HistoryGraphOf(['e', 'd', 'c', 'b', 'a', 'f'],
        ['c', 'b', 'a', 'a', '', '']);
    AssertEquals('a', 0, R[4].Lane);
    AssertEquals('f reuses the freed lane', 0, R[5].Lane);
    AssertEquals('and nothing passes it', '', Lanes(R[5].Through));
end;

procedure THistoryGraphTest.TwoUnrelatedHistoriesDrawSideBySide;
var
    R: THistoryGraphRows;
begin
    //  b from a; y from x; interleaved in time.
    R := HistoryGraphOf(['y', 'b', 'x', 'a'], ['x', 'a', '', '']);
    AssertEquals('y', 0, R[0].Lane);
    AssertEquals('b beside', 1, R[1].Lane);
    AssertEquals('x ends y''s line', '0', Lanes(R[2].Into));
    AssertEquals('while b''s passes', '1', Lanes(R[2].Through));
    AssertEquals('a ends b''s', '1', Lanes(R[3].Into));
end;

procedure THistoryGraphTest.AParentNotBelowStartsNoLine;
var
    R: THistoryGraphRows;
begin
    //  A file edited by hand, naming a parent that is not there.
    R := HistoryGraphOf(['b', 'a'], ['gone', '']);
    AssertFalse('no line to nowhere', R[0].Down);
    AssertEquals('and a starts its own', '', Lanes(R[1].Into));
end;

procedure THistoryGraphTest.ManyBranchesFromOneModel;
var
    R: THistoryGraphRows;
begin
    R := HistoryGraphOf(['d', 'c', 'b', 'a'], ['a', 'a', 'a', '']);
    AssertEquals('three lanes meet at a', '0,1,2', Lanes(R[3].Into));
    AssertEquals('a sits in the first', 0, R[3].Lane);
end;

procedure THistoryGraphTest.ALongSessionOfFitsIsStillOneLane;
var
    Ids, Parents: array of string;
    R: THistoryGraphRows;
    i: longint;
begin
    //  FIFTY FITS, EACH FROM THE ONE BEFORE: the everyday history. A lane
    //  leaked per row would push the text off the panel long before fifty.
    SetLength(Ids, 50);
    SetLength(Parents, 50);
    for i := 0 to 49 do
    begin
        Ids[i] := 'm' + IntToStr(49 - i);
        if i < 49 then
            Parents[i] := 'm' + IntToStr(48 - i)
        else
            Parents[i] := '';
    end;
    R := HistoryGraphOf(Ids, Parents);
    AssertEquals('one lane wide', 1, HistoryGraphWidth(R));
    for i := 0 to 48 do
        AssertTrue(Format('row %d goes on down', [i]), R[i].Down);
    AssertFalse('the first fit is the end of the line', R[49].Down);
end;

procedure THistoryGraphTest.TheWidthIsTheWidestRow;
begin
    AssertEquals(3, HistoryGraphWidth(
        HistoryGraphOf(['d', 'c', 'b', 'a'], ['a', 'a', 'a', ''])));
    AssertEquals(1, HistoryGraphWidth(
        HistoryGraphOf(['c', 'b', 'a'], ['b', 'a', ''])));
end;

procedure THistoryGraphTest.EveryRowHasANodeInALaneItDrawsIn;
var
    R: THistoryGraphRows;
    i: longint;
begin
    //  SELF-CHECKING over a messy history: a node outside its row's width
    //  would be drawn over the text.
    R := HistoryGraphOf(['g', 'f', 'e', 'd', 'c', 'b', 'a'],
        ['c', 'a', 'b', 'a', 'a', 'a', '']);
    for i := 0 to High(R) do
        AssertTrue(Format('row %d: lane %d within width %d',
            [i, R[i].Lane, R[i].Width]), R[i].Lane < R[i].Width);
end;

initialization
    //  A unit test: numbers out of strings.
    RegisterTest('unit', THistoryGraphTest);
end.
