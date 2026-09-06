// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which disagreements between the pane and the menus are reported.)

WHY THESE TESTS EXIST. The check they cover runs inside the application, over
real widgets, and fails a build - so its rules have to be right, and the check
itself is the one thing that cannot verify them. A rule that never fires is
indistinguishable from a window that never disagrees, and that is exactly the
state this project keeps finding: a green everything over a path nobody walks.

So each rule is fired here on purpose, and the one that must NOT fire - a
module's row, which has no menu side to disagree with - is fired at too.
}
unit testcase_ui_selfcheck;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, ui_selfcheck;

type
    TUiSelfCheckTest = class(TTestCase)
    private
        FRows: TSurfaceRows;
        { One row that agrees with itself in every way, as the baseline every
          case below breaks in exactly one place. }
        procedure GivenAGoodRow(const AId: string = 'Cmd');
        function Findings: TUiFindings;
        function FindingsMention(const AText: string): boolean;
    protected
        procedure SetUp; override;
    published
        //  Nothing wrong.
        procedure SurfacesThatAgreeReportNothing;
        procedure NorDoSeveralOfThem;

        //  The hint.
        procedure APaneButtonWithNoHintIsReported;
        procedure SoIsAWhitespaceHint;
        procedure ADifferentHintInEachSurfaceIsReported;
        procedure AndTheFindingQuotesBoth;

        //  Availability.
        procedure ACommandLiveInOneSurfaceAndRefusedInTheOtherIsReported;
        procedure WhicheverWayRound;

        //  Widths.
        procedure AButtonWiderThanTheRestIsReported;
        procedure AndTheFindingNamesTheOneItDiffersFrom;
        procedure EqualWidthsReportNothing;

        //  The picking latch.
        procedure ALatchLeftDownOverAFinishedModeIsReported;
        procedure AndOneLeftUpDuringARunningModeToo;
        procedure ARowThatDoesNotPickIsNotJudgedOnIt;

        //  The legend against the chart it describes.
        procedure ALegendThatMatchesItsChartReportsNothing;
        procedure ARowCarryingNoSeriesIsReported;
        procedure ARowNamingASeriesOffTheChartIsReported;
        procedure ARowReadingOneNameForAnotherSeriesIsReported;
        procedure AnEmptyLegendIsNotAFinding;
        procedure TheLegendSummaryStatesBothCounts;

        //  A module's row.
        procedure AModuleRowHasNoMenuSideToDisagreeWith;
        procedure ButItsMissingHintIsStillReported;

        //  A module's latch, against the tick on its own menu entry.
        procedure ALatchAgreeingWithItsMenuTickReportsNothing;
        procedure ALatchPressedOverAnUntickedEntryIsReported;
        procedure AndOneReleasedUnderATickedOneToo;
        procedure ARowWithNoMenuTickIsNotJudgedOnIt;

        //  The verdict.
        procedure TheSummaryStatesBothCounts;
        procedure AndIsWrittenEvenWhenNothingIsWrong;
    end;

implementation

procedure TUiSelfCheckTest.SetUp;
begin
    FRows := nil;
end;

procedure TUiSelfCheckTest.GivenAGoodRow(const AId: string);
var
    n: longint;
begin
    n := Length(FRows);
    SetLength(FRows, n + 1);
    FRows[n].Id := AId;
    FRows[n].PaneCaption := 'Pick';
    FRows[n].PaneHint := 'Starts picking curve positions';
    FRows[n].PaneEnabled := True;
    FRows[n].PaneWidth := 83;
    FRows[n].HasMenuSide := True;
    FRows[n].MenuHint := 'Starts picking curve positions';
    FRows[n].MenuEnabled := True;
end;

function TUiSelfCheckTest.Findings: TUiFindings;
begin
    Result := SurfaceFindings(FRows);
end;

function TUiSelfCheckTest.FindingsMention(const AText: string): boolean;
var
    F: TUiFindings;
    i: longint;
begin
    Result := False;
    F := Findings;
    for i := 0 to High(F) do
        if Pos(AText, F[i]) > 0 then
            Exit(True);
end;

{ ---- nothing wrong ---- }

procedure TUiSelfCheckTest.SurfacesThatAgreeReportNothing;
begin
    GivenAGoodRow;
    AssertEquals('a passing build', 0, Length(Findings));
end;

procedure TUiSelfCheckTest.NorDoSeveralOfThem;
begin
    GivenAGoodRow('A');
    GivenAGoodRow('B');
    GivenAGoodRow('C');
    AssertEquals('still nothing', 0, Length(Findings));
end;

{ ---- the hint ---- }

procedure TUiSelfCheckTest.APaneButtonWithNoHintIsReported;
begin
    //  THE DEFECT THIS RULE WAS WRITTEN FOR: the framework's rows took no hint
    //  from the actions they drive, so every pane button was silent while every
    //  menu entry behind it explained itself.
    GivenAGoodRow;
    FRows[0].PaneHint := '';
    FRows[0].MenuHint := '';
    AssertTrue('reported', FindingsMention('has no hint'));
end;

procedure TUiSelfCheckTest.SoIsAWhitespaceHint;
begin
    //  A hint of spaces shows an empty tooltip, which is worse than none: the
    //  user learns the button explains nothing rather than that it has more to
    //  say elsewhere.
    GivenAGoodRow;
    FRows[0].PaneHint := '   ';
    FRows[0].MenuHint := '   ';
    AssertTrue('reported', FindingsMention('has no hint'));
end;

procedure TUiSelfCheckTest.ADifferentHintInEachSurfaceIsReported;
begin
    GivenAGoodRow;
    FRows[0].MenuHint := 'Something else entirely';
    AssertEquals('one finding', 1, Length(Findings));
end;

procedure TUiSelfCheckTest.AndTheFindingQuotesBoth;
begin
    //  A person reads this after a build fails, so it has to say which two
    //  texts drifted - naming the command alone would send them looking.
    GivenAGoodRow;
    FRows[0].MenuHint := 'Something else entirely';
    AssertTrue('the pane''s text',
        FindingsMention('Starts picking curve positions'));
    AssertTrue('and the menu''s', FindingsMention('Something else entirely'));
end;

{ ---- availability ---- }

procedure TUiSelfCheckTest.ACommandLiveInOneSurfaceAndRefusedInTheOtherIsReported;
begin
    //  THE ONE THAT WOULD COST THE MOST. The program contradicts itself in
    //  front of the user, and whichever surface they reach for first decides
    //  whether the feature exists at all.
    GivenAGoodRow;
    FRows[0].MenuEnabled := False;
    AssertTrue('reported', FindingsMention('enabled in the pane'));
    AssertTrue('and which way round', FindingsMention('disabled in the menu'));
end;

procedure TUiSelfCheckTest.WhicheverWayRound;
begin
    GivenAGoodRow;
    FRows[0].PaneEnabled := False;
    AssertTrue('reported', FindingsMention('disabled in the pane'));
    AssertTrue('and which way round', FindingsMention('enabled in the menu'));
end;

{ ---- widths ---- }

procedure TUiSelfCheckTest.AButtonWiderThanTheRestIsReported;
begin
    //  One button used to be drawn across both columns deliberately. It is not
    //  any more, and this is what keeps it that way.
    GivenAGoodRow('A');
    GivenAGoodRow('B');
    FRows[1].PaneWidth := 170;
    AssertTrue('reported', FindingsMention('px wide'));
end;

procedure TUiSelfCheckTest.AndTheFindingNamesTheOneItDiffersFrom;
begin
    GivenAGoodRow('A');
    GivenAGoodRow('B');
    FRows[1].PaneWidth := 170;
    //  Both ids, because "A is 83 and B is 170" is a sentence someone can act
    //  on without opening the form.
    AssertTrue('the narrow one', FindingsMention('A: the button is 83'));
    AssertTrue('and the wide one', FindingsMention('B is 170'));
end;

procedure TUiSelfCheckTest.EqualWidthsReportNothing;
begin
    GivenAGoodRow('A');
    GivenAGoodRow('B');
    GivenAGoodRow('C');
    AssertEquals('no width finding', 0, Length(Findings));
end;

{ ---- the picking latch ---- }

procedure TUiSelfCheckTest.ALatchLeftDownOverAFinishedModeIsReported;
begin
    //  THE BUTTON IS THE ONLY THING THAT SAYS A MODE IS RUNNING - the menu
    //  entry says start or stop instead - so a latch left down tells the user
    //  to keep clicking a chart that is no longer taking their clicks.
    GivenAGoodRow;
    FRows[0].HasPicking := True;
    FRows[0].PaneDown := True;
    FRows[0].ModeSaysDown := False;
    AssertTrue('reported', FindingsMention('the button is pressed'));
    AssertTrue('and the mode', FindingsMention('picking mode is not running'));
end;

procedure TUiSelfCheckTest.AndOneLeftUpDuringARunningModeToo;
begin
    //  The other way round is worse: the user's clicks ARE being taken and
    //  nothing on screen says so.
    GivenAGoodRow;
    FRows[0].HasPicking := True;
    FRows[0].PaneDown := False;
    FRows[0].ModeSaysDown := True;
    AssertTrue('reported', FindingsMention('not pressed'));
end;

procedure TUiSelfCheckTest.ARowThatDoesNotPickIsNotJudgedOnIt;
begin
    //  THE RULE THAT MUST NOT FIRE. Most rows are ordinary buttons that are
    //  never down, and judging them on a latch they do not have would report
    //  every one of them in every build.
    GivenAGoodRow;
    FRows[0].HasPicking := False;
    FRows[0].PaneDown := False;
    FRows[0].ModeSaysDown := True;
    AssertEquals('nothing to report', 0, Length(Findings));
end;

{ ---- the legend against its chart ---- }

procedure TUiSelfCheckTest.ALegendThatMatchesItsChartReportsNothing;
var
    L: TLegendRows;
begin
    SetLength(L, 2);
    L[0].Text_ := 'Profile';
    L[0].HasSeries := True; L[0].SeriesOnChart := True;
    L[0].SeriesTitle := 'Profile';
    L[1].Text_ := 'Gaussian';
    L[1].HasSeries := True; L[1].SeriesOnChart := True;
    L[1].SeriesTitle := 'Gaussian';
    AssertEquals('a passing build', 0, Length(LegendFindings(L)));
end;

procedure TUiSelfCheckTest.ARowCarryingNoSeriesIsReported;
var
    L: TLegendRows;
begin
    //  The state the old code could not detect at all: it left rows behind when
    //  a series went, and drew them against whatever sat at that position.
    SetLength(L, 1);
    L[0].Text_ := 'Gaussian';
    L[0].HasSeries := False;
    AssertEquals('one finding', 1, Length(LegendFindings(L)));
    AssertTrue('and it says which row',
        Pos('carries no series', LegendFindings(L)[0]) > 0);
end;

procedure TUiSelfCheckTest.ARowNamingASeriesOffTheChartIsReported;
var
    L: TLegendRows;
begin
    //  A series taken off the chart and freed, with its row still holding the
    //  pointer - which is what ticking that row would then follow.
    SetLength(L, 1);
    L[0].Text_ := 'Gaussian';
    L[0].HasSeries := True;
    L[0].SeriesOnChart := False;
    AssertTrue('reported',
        Pos('not on the chart', LegendFindings(L)[0]) > 0);
end;

procedure TUiSelfCheckTest.ARowReadingOneNameForAnotherSeriesIsReported;
var
    L: TLegendRows;
begin
    //  THE FAILURE THIS PAIRING EXISTS TO PREVENT, and it is invisible until
    //  the user ticks the row and the wrong curve disappears.
    SetLength(L, 1);
    L[0].Text_ := 'Gaussian';
    L[0].HasSeries := True;
    L[0].SeriesOnChart := True;
    L[0].SeriesTitle := 'Lorentzian';
    AssertTrue('both names in the finding',
        (Pos('Gaussian', LegendFindings(L)[0]) > 0) and
        (Pos('Lorentzian', LegendFindings(L)[0]) > 0));
end;

procedure TUiSelfCheckTest.AnEmptyLegendIsNotAFinding;
begin
    //  Nothing plotted yet, which is every build that opens no file.
    AssertEquals('nothing', 0, Length(LegendFindings(nil)));
end;

procedure TUiSelfCheckTest.TheLegendSummaryStatesBothCounts;
begin
    AssertTrue('how many rows', Pos('7', LegendSummary(7, 2)) > 0);
    AssertTrue('and how many wrong', Pos('2', LegendSummary(7, 2)) > 0);
end;

{ ---- a module's row ---- }

procedure TUiSelfCheckTest.AModuleRowHasNoMenuSideToDisagreeWith;
begin
    //  THE RULE THAT MUST NOT FIRE. A row with no menu side has nothing to
    //  compare against, and comparing it with an empty one would report it in
    //  every build. A module's row usually HAS one - the entry it declared
    //  under Model - and is compared with it; this is the case where it does
    //  not, a row the menus never drew.
    GivenAGoodRow;
    FRows[0].HasMenuSide := False;
    FRows[0].MenuHint := '';
    FRows[0].MenuEnabled := False;
    AssertEquals('nothing to report', 0, Length(Findings));
end;

procedure TUiSelfCheckTest.ButItsMissingHintIsStillReported;
begin
    //  A module declares its own hint, so a module row without one is the
    //  module's omission and worth saying - it is the same silent button.
    GivenAGoodRow;
    FRows[0].HasMenuSide := False;
    FRows[0].PaneHint := '';
    AssertTrue('reported', FindingsMention('has no hint'));
end;

{ ---- a module's latch against its menu entry ---- }

procedure TUiSelfCheckTest.ALatchAgreeingWithItsMenuTickReportsNothing;
begin
    GivenAGoodRow;
    FRows[0].HasMenuTick := True;
    FRows[0].PaneDown := True;
    FRows[0].MenuChecked := True;
    AssertEquals('they agree', 0, Length(Findings));
end;

procedure TUiSelfCheckTest.ALatchPressedOverAnUntickedEntryIsReported;
begin
    //  THE DRIFT THIS RULE EXISTS FOR. A module's toggle is drawn twice - a
    //  ticked entry in the menu and a pressed button in the pane - and the two
    //  are written by different code paths. One saying the mode is on while the
    //  other says it is off is a program contradicting itself, and whichever
    //  the user looks at first decides what they believe.
    GivenAGoodRow;
    FRows[0].HasMenuTick := True;
    FRows[0].PaneDown := True;
    FRows[0].MenuChecked := False;
    AssertTrue('reported', FindingsMention('ticked'));
end;

procedure TUiSelfCheckTest.AndOneReleasedUnderATickedOneToo;
begin
    GivenAGoodRow;
    FRows[0].HasMenuTick := True;
    FRows[0].PaneDown := False;
    FRows[0].MenuChecked := True;
    AssertTrue('reported', FindingsMention('ticked'));
end;

procedure TUiSelfCheckTest.ARowWithNoMenuTickIsNotJudgedOnIt;
begin
    //  A plain command carries no tick anywhere. Comparing its button's
    //  pressed state - always False - with a tick nothing writes would report
    //  every ordinary row in the pane.
    GivenAGoodRow;
    FRows[0].HasMenuTick := False;
    FRows[0].PaneDown := True;
    FRows[0].MenuChecked := False;
    AssertEquals('not its business', 0, Length(Findings));
end;

{ ---- the verdict ---- }

procedure TUiSelfCheckTest.TheSummaryStatesBothCounts;
begin
    AssertTrue('how many rows', Pos('12', SurfaceSummary(12, 3)) > 0);
    AssertTrue('and how many findings', Pos('3', SurfaceSummary(12, 3)) > 0);
end;

procedure TUiSelfCheckTest.AndIsWrittenEvenWhenNothingIsWrong;
begin
    //  A check that logs only when it finds something is indistinguishable from
    //  a check that did not run, and the task that reads the log cannot tell
    //  the difference either.
    AssertTrue('a verdict either way', SurfaceSummary(12, 0) <> '');
    AssertTrue('and it says none', Pos('0 disagreement', SurfaceSummary(12, 0)) > 0);
end;

initialization
    //  A unit test: records in, sentences out. No widget and no window - which
    //  is the point, because the check these rules belong to can only run
    //  inside the application and cannot verify itself.
    RegisterTest('unit', TUiSelfCheckTest);
end.
