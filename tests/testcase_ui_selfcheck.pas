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
    Classes, SysUtils, fpcunit, testregistry, ui_selfcheck, series_style;

type
    TUiSelfCheckTest = class(TTestCase)
    private
        FRows: TSurfaceRows;
        { One row that agrees with itself in every way, as the baseline every
          case below breaks in exactly one place. }
        procedure GivenAGoodRow(const AId: string = 'Cmd');
        function Findings: TUiFindings;
        function FindingsMention(const AText: string): boolean;
        function AFitThatWasWatched: TLiveProgressReading;
    protected
        procedure SetUp; override;
    published
        //  Every toolbar, as the window measured it. The spacing is applied
        //  from one unit to every bar there is, so a bar the application
        //  builds later and never spaces is the failure worth naming.
        procedure ASpacedToolBarReportsNothing;
        procedure AButtonTooSmallToAimAtIsReported;
        procedure AndTheFindingNamesTheBarAndBothSizes;
        procedure ButtonsHardAgainstTheBarsEdgeAreReported;
        procedure ABarStillDrawingItsOwnEdgeIsReported;
        procedure AnIndentOnTopOfTheMarginIsReported;
        procedure BarsThatDisagreeWithEachOtherAreReported;
        procedure AndTheFindingNamesTheOddOneOut;
        procedure ABarWithNoButtonsIsReported;
        procedure AnUnparentedBarIsReported;
        procedure TheToolBarVerdictIsAlwaysWritten;

        //  A pane's own zoom buttons, as the window found them.
        procedure APaneWhoseButtonsWorkReportsNothing;
        procedure AStripMissingAButtonIsReported;
        procedure AZoomInThatChangedNothingIsReported;
        procedure AZoomOutThatDidNotComeBackIsReported;
        procedure AButtonThatMovedTheOtherPaneIsReported;
        procedure AButtonStillOfferedAtTheEndOfItsRangeIsReported;
        procedure TheZoomVerdictIsAlwaysWritten;
        procedure ABuildWithNoReportTabSaysSoRatherThanFailing;

        //  Nothing wrong.
        procedure SurfacesThatAgreeReportNothing;
        procedure NorDoSeveralOfThem;

        //  The hint.
        procedure APaneButtonWithNoHintIsReported;
        procedure SoIsAWhitespaceHint;
        procedure ADifferentHintInEachSurfaceIsReported;
        procedure AndTheFindingQuotesBoth;
        procedure AHintThatOnlyRepeatsTheCaptionIsReported;
        procedure CaseAndSpacingDoNotHideARepeatedCaption;
        procedure AHintThatSaysMoreThanTheCaptionIsNotReported;

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
        procedure AnEmptyBarIsReportedOnceAndNotComparedForLooks;
        procedure TheSummarySaysWhenNoCommandReachesTheSources;

        //  The pointer's readout beside the chart.
        procedure AReadoutInOneColumnThatFitsReportsNothing;
        procedure ValuesThatDoNotFormAColumnAreReported;
        procedure AValueItsLabelCutsOffIsReported;
        procedure ACaptionRunningIntoItsValueIsReported;
        procedure TheReadoutSummaryStatesBothCounts;

        //  What the window saw while a fit really ran.
        procedure AFitThatDrewFramesAndMovedItsCurvesReportsNothing;
        procedure AFitThatDrewNoFrameAtAllIsReported;
        procedure AnimationThatMovedNoCurveIsReported;
        procedure ButWithoutAnimationNoCurveIsExpectedToMove;
        procedure ALossChartWithNoPointIsReported;
        procedure AFitTooShortToWatchIsNotJudged;
        procedure AFitThatNeverRanIsSaidSoRatherThanPassed;
        procedure TheLiveProgressSummaryStatesWhatWasDrawn;
        procedure EveryLiveProgressVerdictIsRecognisableAsOne;
        procedure AModulesRowsDrawnWhileAFitRanAreReported;
        procedure AModulesRowsNeverDrawnAfterTheFitAreReported;
        procedure ABuildWithNoModulePanelDoesNotJudgeThePanelDuringAFit;

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

        //  Why the row-command check could not run at all.
        procedure NoDataFileIsSaidRatherThanSkipped;
        procedure AndSoIsAProfileTooShortToPlacePicksIn;
        procedure TheMissingFileIsNamedAloneRatherThanWithItsConsequence;
        procedure AProfileLongEnoughBlocksNothing;
        procedure AModelWithNoRowNamingACurveSaysWhatThatCosts;

        //  What the Model panel's context menu offers over a row naming a curve.
        procedure AMenuLiveOverBothKindsOfRowReportsNothing;
        procedure AContextMenuWithNoEntriesAtAllIsItsOwnFinding;
        procedure AndIsNotAlsoReportedAsEveryEntryDisabled;
        procedure EveryEntryDisabledOverTheFrameworksOwnRowIsReported;
        procedure AContributorRowReadAsNamingNoCurveIsReported;
        procedure EveryEntryDisabledOverAContributorRowIsReportedToo;
        procedure AModelReportingNoCurveHandleIsNotCountedAsAPass;
        procedure AndNothingBeyondItIsJudged;
        procedure AContextEntryWhoseTopicResolvesNowhereIsReported;
        procedure EveryUnresolvedTopicIsNamedNotJustTheFirst;
        procedure EntriesAModuleAddedMeanTheMenuIsNotEmpty;

        //  A module's nested rows, with a type placed by picks selected
        procedure ANestedTreeDrawnAsATreeReportsNothing;
        procedure ATreeDrawnFlatIsReported;
        procedure ACurveListedTwiceIsReported;
        procedure AReadingUnderAModulesOwnTypeIsNotCountedAsAPass;
        procedure ABuildWithNoModulePanelBowsOutWithoutAFinding;
        procedure BothModelTreeVerdictsNameTheSameCheck;
        procedure ACollapsedPatternOpenedByARefreshIsReported;
        procedure RowsOfAModelNotOpenShownAreReported;
        procedure ASelectedCurveNotRestoredByARefreshIsReported;

        //  The selected curve, as the chart draws it
        procedure ASelectedCurveDrawnHighlightedReportsNothing;
        procedure AWideSeriesUnderneathIsNotHighlighted;
        procedure ASeriesHighlightedForAnotherCurveIsReported;
        procedure OnTopAloneOnAnotherCurveCountsAgainstTheChart;
        procedure AModelWideSeriesDrawnNormallyCountsForNothing;
        procedure ASelectionTheWindowDidNotHoldIsNotCountedAsAPass;
        procedure ASelectedCurveWithNoSeriesIsNotCountedAsAPass;
        procedure APanelShowingAnotherCurveIsReported;
        procedure WithNothingSelectedAnyHighlightIsReported;
        procedure WithNothingSelectedAndNothingHighlightedReportsNothing;
        procedure EachFindingSaysWhenItWasMeasured;
        procedure TheHighlightVerdictStatesWhatWasMeasured;
        procedure APanelShowingNoRowIsNotADisagreement;
        procedure ASelectionThatDidNotFollowTheGestureIsReported;
        procedure ASelectionThatFollowedTheGestureReportsNothing;
        procedure ATableWithNothingToChooseIsNotCountedAsAPass;

        //  The two verdicts, which are separate lines on purpose.
        procedure EachKindOfRowGetsItsOwnCount;
        procedure AndBothAreWrittenWhenNothingIsOffered;
        //  What the running application has to say about its data sources:
        //  registration is a property of start-up and of linking, which no
        //  headless suite can answer for the binary a user runs.
        procedure ASoundSetOfSourcesReportsNothing;
        procedure NoSourcesAtAllIsReportedAsStartUpNotRegistering;
        procedure SourcesThatAllNeedTheNetworkAreReported;
        procedure AnIncompleteSourceIsReportedWithTheWalksWords;
        procedure NoCommandReachingTheWizardIsReported;
        procedure ACommandOnTheMenuAndDisabledIsReportedDifferently;
        procedure TheSummarySaysWhatWasCountedEitherWay;
    end;

implementation

{ ---- a pane's own zoom buttons --------------------------------------------- }



{ A reading in which everything the window could get wrong went right. }
function WorkingZoom: TZoomReading;
begin
    Result := Default(TZoomReading);
    Result.Pane := 'analysis report';
    Result.Buttons := 2;
    Result.Before := 100;
    Result.AfterIn := 120;
    Result.AfterOut := 100;
    Result.OtherBefore := 100;
    Result.OtherAfter := 100;
    Result.OfferedInAtTop := False;
    Result.OfferedOutAtBottom := False;
end;

{ One toolbar as a correctly spaced window would report it: buttons big enough
  to aim at, and white space between the outermost of them and the bar's edge.
  The baseline every case below breaks in exactly one place. }
function SpacedBar: TToolBarReading;
begin
    Result := Default(TToolBarReading);
    Result.Name := 'ToolBarMain';
    Result.Attached := True;
    Result.ButtonCount := 4;
    Result.SmallestButtonWidth := 28;
    Result.SmallestButtonHeight := 28;
    Result.Margin := 4;
    Result.DrawsEdgeBorder := False;
    Result.LeadingIndent := 0;
end;

function OneBar(const AReading: TToolBarReading): TToolBarReadings;
begin
    SetLength(Result, 1);
    Result[0] := AReading;
end;

procedure TUiSelfCheckTest.AnEmptyBarIsReportedOnceAndNotComparedForLooks;
var
    Bars: TToolBarReadings;
    Said: TUiFindings;
    i: longint;
begin
    //  An empty bar has no buttons to compare: it is reported as empty, and
    //  not ALSO as drawing 0x0 buttons where the first bar draws 28x28 -
    //  three findings for one empty bar bury the one that matters.
    SetLength(Bars, 2);
    Bars[0] := SpacedBar;
    Bars[1] := SpacedBar;
    Bars[1].Name := 'ToolBarEmpty';
    Bars[1].ButtonCount := 0;
    Bars[1].SmallestButtonWidth := 0;
    Bars[1].SmallestButtonHeight := 0;
    Said := ToolBarFindings(Bars, 24, 2);
    AssertEquals('one finding', 1, Length(Said));
    AssertTrue(Said[0], Pos('holds no buttons', Said[0]) > 0);
    for i := 0 to High(Said) do
        AssertEquals(Said[i], 0, Pos('do not look alike', Said[i]));
end;

procedure TUiSelfCheckTest.ASpacedToolBarReportsNothing;
begin
    AssertEquals('nothing to say', 0,
        Length(ToolBarFindings(OneBar(SpacedBar), 24, 2)));
end;

procedure TUiSelfCheckTest.AButtonTooSmallToAimAtIsReported;
var
    R: TToolBarReading;
begin
    //  THE DEFECT THIS EXISTS FOR: a bar that never had the spacing applied
    //  draws its buttons at the widget set's own metrics - barely larger than
    //  the icon - and looks like an ordinary toolbar while it does it.
    R := SpacedBar;
    R.SmallestButtonWidth := 22;
    AssertEquals('reported', 1, Length(ToolBarFindings(OneBar(R), 24, 2)));
    //  Either dimension, not just the width.
    R := SpacedBar;
    R.SmallestButtonHeight := 22;
    AssertEquals('reported', 1, Length(ToolBarFindings(OneBar(R), 24, 2)));
end;

procedure TUiSelfCheckTest.AndTheFindingNamesTheBarAndBothSizes;
var
    R: TToolBarReading;
    Said: TUiFindings;
begin
    R := SpacedBar;
    R.SmallestButtonWidth := 20;
    Said := ToolBarFindings(OneBar(R), 24, 2);
    AssertEquals(1, Length(Said));
    AssertTrue('it names the bar', Pos('ToolBarMain', Said[0]) > 0);
    AssertTrue('the size it found', Pos('20', Said[0]) > 0);
    AssertTrue('and the size it wanted', Pos('24', Said[0]) > 0);
end;

procedure TUiSelfCheckTest.ButtonsHardAgainstTheBarsEdgeAreReported;
var
    R: TToolBarReading;
begin
    //  A bar whose buttons touch its edge reads as continuous with whatever it
    //  is docked against, however big the buttons themselves are.
    R := SpacedBar;
    R.Margin := 0;
    AssertEquals('reported', 1, Length(ToolBarFindings(OneBar(R), 24, 2)));
end;

procedure TUiSelfCheckTest.BarsThatDisagreeWithEachOtherAreReported;
var
    R: TToolBarReadings;
begin
    //  WHAT THE USER SEES IS THE COMPARISON. Every bar passing its own rules
    //  says nothing about whether they look alike, and the one that does not
    //  is the one noticed - the main bar's buttons sat two pixels further in
    //  than every other bar's, because the panel holding it has a bevel the
    //  other panels were given none of. No rule about a single bar can see
    //  that; it is only visible between bars.
    SetLength(R, 3);
    R[0] := SpacedBar;
    R[1] := SpacedBar;
    R[1].Name := 'ToolBarSummary';
    R[2] := SpacedBar;
    R[2].Name := 'ToolBarMain';
    R[2].Margin := 7;
    AssertEquals('reported', 1, Length(ToolBarFindings(R, 24, 2)));
end;

procedure TUiSelfCheckTest.AndTheFindingNamesTheOddOneOut;
var
    R: TToolBarReadings;
    Said: TUiFindings;
begin
    SetLength(R, 2);
    R[0] := SpacedBar;
    R[1] := SpacedBar;
    R[1].Name := 'ToolBarMain';
    R[1].SmallestButtonWidth := 26;
    Said := ToolBarFindings(R, 24, 2);
    AssertEquals(1, Length(Said));
    AssertTrue('it names the bar that differs', Pos('ToolBarMain', Said[0]) > 0);
end;

procedure TUiSelfCheckTest.AnIndentOnTopOfTheMarginIsReported;
var
    R: TToolBarReading;
begin
    //  A TToolBar lays its buttons out from a rectangle that is indented on the
    //  LEADING EDGE ONLY - the left, or the top of a right-to-left one - and on
    //  no other side. So an indent asked for in addition to the margin around
    //  the bar makes that one edge twice the other three, which is what a
    //  vertical strip shows plainly: the column sits further from the panel's
    //  side than from its top. The margin is the spacing; the indent is a
    //  second helping of it on one side.
    R := SpacedBar;
    R.LeadingIndent := 5;
    AssertEquals('reported', 1, Length(ToolBarFindings(OneBar(R), 24, 2)));
end;

procedure TUiSelfCheckTest.ABarStillDrawingItsOwnEdgeIsReported;
var
    R: TToolBarReading;
begin
    //  A TToolBar draws a border on all four sides by default. While its
    //  buttons sat hard against that border it read as the frame around them;
    //  with white space between the two it is a rule floating above the first
    //  button, pointing at nothing. The margin is the boundary now, so the
    //  border is one boundary too many - and it only becomes visible once a
    //  bar has been spaced, which is exactly when nobody is looking for it.
    R := SpacedBar;
    R.DrawsEdgeBorder := True;
    AssertEquals('reported', 1, Length(ToolBarFindings(OneBar(R), 24, 2)));
end;

procedure TUiSelfCheckTest.ABarWithNoButtonsIsReported;
var
    R: TToolBarReading;
begin
    //  An empty bar is a strip of grey the user cannot use, and it is what a
    //  toolbar whose buttons were never parented looks like.
    R := SpacedBar;
    R.ButtonCount := 0;
    AssertTrue('reported', Length(ToolBarFindings(OneBar(R), 24, 2)) > 0);
end;

procedure TUiSelfCheckTest.AnUnparentedBarIsReported;
var
    R: TToolBarReading;
begin
    //  A bar built and never attached is invisible, and nothing else reports
    //  it - PopupViewMode was in exactly that state for years.
    R := SpacedBar;
    R.Attached := False;
    AssertTrue('reported', Length(ToolBarFindings(OneBar(R), 24, 2)) > 0);
end;

procedure TUiSelfCheckTest.TheToolBarVerdictIsAlwaysWritten;
begin
    //  A check that speaks only when it finds something cannot be told from
    //  one that never ran.
    AssertTrue('the count of bars', Pos('6', ToolBarSpacingSummary(6, 0)) > 0);
    AssertTrue('and of problems', Pos('2', ToolBarSpacingSummary(6, 2)) > 0);
end;

function OneZoom(const AReading: TZoomReading): TZoomReadings;
begin
    SetLength(Result, 1);
    Result[0] := AReading;
end;

procedure TUiSelfCheckTest.ABuildWithNoReportTabSaysSoRatherThanFailing;
begin
    //  Which modules a build links changes underneath this check, and a build
    //  with none has no analysis tab to zoom. That is not two missing buttons;
    //  it is a half that did not run, and it has to say so.
    AssertTrue('it names what was not checked',
        Pos('zoom', NoReportTabToZoom) > 0);
    AssertTrue('and why', Pos('no module', NoReportTabToZoom) > 0);
end;

procedure TUiSelfCheckTest.APaneWhoseButtonsWorkReportsNothing;
begin
    AssertEquals('nothing to say', 0,
        Length(ZoomFindings(OneZoom(WorkingZoom))));
end;

procedure TUiSelfCheckTest.AStripMissingAButtonIsReported;
var
    R: TZoomReading;
begin
    //  A strip built with one button is a pane that can be enlarged and never
    //  brought back - and it looks like a toolbar, so nothing says otherwise.
    R := WorkingZoom;
    R.Buttons := 1;
    AssertEquals('reported', 1, Length(ZoomFindings(OneZoom(R))));
end;

procedure TUiSelfCheckTest.AZoomInThatChangedNothingIsReported;
var
    R: TZoomReading;
    Said: TUiFindings;
begin
    //  THE DEFECT THIS EXISTS FOR: the button is there, the action runs, and
    //  the pane is drawn exactly as it was. No headless suite can see it.
    R := WorkingZoom;
    R.AfterIn := R.Before;
    Said := ZoomFindings(OneZoom(R));
    AssertEquals('reported', 1, Length(Said));
    AssertTrue('and it names the pane', Pos('analysis report', Said[0]) > 0);
end;

procedure TUiSelfCheckTest.AZoomOutThatDidNotComeBackIsReported;
var
    R: TZoomReading;
begin
    R := WorkingZoom;
    R.AfterOut := 140;
    AssertEquals('reported', 1, Length(ZoomFindings(OneZoom(R))));
end;

procedure TUiSelfCheckTest.AButtonThatMovedTheOtherPaneIsReported;
var
    R: TZoomReading;
begin
    //  Each strip sits on the pane it moves. One that resizes the other pane
    //  changes something the user is not looking at.
    R := WorkingZoom;
    R.OtherAfter := 120;
    AssertEquals('reported', 1, Length(ZoomFindings(OneZoom(R))));
end;

procedure TUiSelfCheckTest.AButtonStillOfferedAtTheEndOfItsRangeIsReported;
var
    R: TZoomReading;
begin
    R := WorkingZoom;
    R.OfferedInAtTop := True;
    R.OfferedOutAtBottom := True;
    AssertEquals('both ends reported', 2, Length(ZoomFindings(OneZoom(R))));
end;

procedure TUiSelfCheckTest.TheZoomVerdictIsAlwaysWritten;
begin
    //  A check that logs only when it finds something reads exactly like a
    //  check that did not run.
    AssertTrue('the count is in it', Pos('2', ZoomSummary(2, 0)) > 0);
    AssertTrue('and so is the verdict', Pos('0 problem', ZoomSummary(2, 0)) > 0);
end;

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

function ReadoutRow(const ACaption: string; ACaptionRight, AValueLeft,
    AValueWidth: longint; const AValue: string;
    AValueTextWidth: longint): TReadoutRow;
begin
    Result.Caption := ACaption;
    Result.CaptionRight := ACaptionRight;
    Result.ValueLeft := AValueLeft;
    Result.ValueWidth := AValueWidth;
    Result.Value := AValue;
    Result.ValueTextWidth := AValueTextWidth;
end;

function TwoRows(const A, B: TReadoutRow): TReadoutRows;
begin
    SetLength(Result, 2);
    Result[0] := A;
    Result[1] := B;
end;

procedure TUiSelfCheckTest.AReadoutInOneColumnThatFitsReportsNothing;
begin
    AssertEquals(0, Length(ReadoutFindings(TwoRows(
        ReadoutRow('Position:', 60, 66, 120, '158.00', 40),
        ReadoutRow('Price:', 60, 66, 120, '1004.21', 48)))));
end;

procedure TUiSelfCheckTest.ValuesThatDoNotFormAColumnAreReported;
var
    F: TUiFindings;
begin
    //  Each value where its own caption ended: "Position:" is wider.
    F := ReadoutFindings(TwoRows(
        ReadoutRow('Position:', 60, 66, 120, '158.00', 40),
        ReadoutRow('Price:', 40, 46, 140, '1004.21', 48)));
    AssertEquals(1, Length(F));
    AssertTrue(F[0], Pos('column', F[0]) > 0);
end;

procedure TUiSelfCheckTest.AValueItsLabelCutsOffIsReported;
var
    F: TUiFindings;
begin
    //  THE REPORT: "Price: (" with the rest of the value cut off.
    F := ReadoutFindings(TwoRows(
        ReadoutRow('Position:', 60, 66, 120, '158.00', 40),
        ReadoutRow('Price:', 60, 66, 8, '1004.21', 48)));
    AssertEquals(1, Length(F));
    AssertTrue('names the caption', Pos('Price:', F[0]) > 0);
    AssertTrue('and what it needs', Pos('48', F[0]) > 0);
end;

procedure TUiSelfCheckTest.ACaptionRunningIntoItsValueIsReported;
var
    F: TUiFindings;
begin
    F := ReadoutFindings(TwoRows(
        ReadoutRow('Sin(Theta)/Lambda:', 90, 66, 120, '0.17', 30),
        ReadoutRow('Intensity:', 90, 66, 120, '15.56', 36)));
    AssertTrue(Length(F) >= 1);
    AssertTrue(F[0], Pos('runs into', F[0]) > 0);
end;

procedure TUiSelfCheckTest.TheReadoutSummaryStatesBothCounts;
begin
    AssertTrue(Pos('2 readout line(s)', ReadoutSummary(2, 1)) > 0);
    AssertTrue(Pos('1 problem(s)', ReadoutSummary(2, 1)) > 0);
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

{ ---- why the row-command check could not run ------------------------------- }

procedure TUiSelfCheckTest.NoDataFileIsSaidRatherThanSkipped;
var
    Said: string;
begin
    //  THE CHECK BOWS OUT, AND SAYS SO. Without a file there is nothing to place
    //  picks on - but a check that quietly does nothing reads exactly like one
    //  that passed, and the task that greps the log cannot tell them apart. The
    //  sentence also has to say what to do about it, which is why the option is
    //  named in it.
    Said := RowCommandBlocked(False, 0);
    AssertTrue('it is blocked', Said <> '');
    AssertTrue('and says what would unblock it: ' + Said,
        Pos('/INFILE', Said) > 0);
end;

procedure TUiSelfCheckTest.AndSoIsAProfileTooShortToPlacePicksIn;
var
    Said: string;
begin
    //  An open file whose profile has too few samples for two picks and an
    //  interval to land on distinct ones.
    Said := RowCommandBlocked(True, MinProfilePointsForPicks - 1);
    AssertTrue('it is blocked', Said <> '');
    AssertTrue('and names the profile as the reason: ' + Said,
        Pos('too short', Said) > 0);
end;

procedure TUiSelfCheckTest.TheMissingFileIsNamedAloneRatherThanWithItsConsequence;
var
    Said: string;
begin
    //  WITH NO FILE the profile is empty as a CONSEQUENCE, not as a second
    //  fault. Reporting its length here would name a symptom as a cause and send
    //  whoever reads the log looking at the data.
    Said := RowCommandBlocked(False, 0);
    AssertTrue('the file is the reason given: ' + Said,
        Pos('no data file', Said) > 0);
    AssertTrue('and its length is not also blamed: ' + Said,
        Pos('too short', Said) = 0);
end;

procedure TUiSelfCheckTest.AProfileLongEnoughBlocksNothing;
begin
    AssertEquals('nothing in the way', '',
        RowCommandBlocked(True, MinProfilePointsForPicks));
end;

procedure TUiSelfCheckTest.AModelWithNoRowNamingACurveSaysWhatThatCosts;
begin
    //  The panel filled and nothing in it is what the menu is about, so every
    //  entry on it is unreachable - which is the thing worth saying, not that a
    //  row was missing.
    AssertTrue('it says nothing on the menu can apply: ' + NoRowNamesACurve,
        Pos('can ever apply', NoRowNamesACurve) > 0);
end;

{ ---- what the context menu offers over a row naming a curve ---------------- }

function GoodReach: TRowCommandReach;
begin
    //  A menu that works over both kinds of row, as the baseline each case below
    //  breaks in exactly one place.
    Result.EntryCount := 4;
    Result.OfferedOnOwnRow := True;
    Result.ContributorCurveId := 'curve-7';
    Result.SelectedCurveId := 'curve-7';
    Result.OfferedOnContributorRow := True;
    Result.ModuleEntryCount := 0;
    Result.UnresolvedTopics := nil;
end;

function Mentions(const AFindings: TUiFindings; const AText: string): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(AFindings) do
        if Pos(AText, AFindings[i]) > 0 then
            Exit(True);
end;

procedure TUiSelfCheckTest.AMenuLiveOverBothKindsOfRowReportsNothing;
begin
    AssertEquals('nothing to report', 0,
        Length(RowCommandFindings(GoodReach)));
end;

procedure TUiSelfCheckTest.AContextMenuWithNoEntriesAtAllIsItsOwnFinding;
var
    R: TRowCommandReach;
begin
    //  A MENU WITH NOTHING ON IT cannot be told from one whose entries are all
    //  disabled by counting enabled entries - both count zero - and the two have
    //  different causes: one lost its entries, the other lost its inputs.
    R := GoodReach;
    R.EntryCount := 0;
    R.OfferedOnOwnRow := False;
    AssertTrue('the empty menu is named',
        Mentions(RowCommandFindings(R), 'no entries at all'));
end;

procedure TUiSelfCheckTest.AndIsNotAlsoReportedAsEveryEntryDisabled;
var
    R: TRowCommandReach;
begin
    R := GoodReach;
    R.EntryCount := 0;
    R.OfferedOnOwnRow := False;
    R.OfferedOnContributorRow := False;
    //  ONE FAULT, ONE FINDING. Two sentences for one cause makes a log that
    //  reads as two problems and sends the reader looking for the second.
    AssertFalse('not also reported as disabled entries',
        Mentions(RowCommandFindings(R), 'every context entry is still'));
    AssertFalse('nor over the contributor row',
        Mentions(RowCommandFindings(R), 'every context entry is disabled'));
end;

procedure TUiSelfCheckTest.EveryEntryDisabledOverTheFrameworksOwnRowIsReported;
var
    R: TRowCommandReach;
begin
    //  THE ORIGINAL DEFECT. The one entry there was shipped permanently greyed:
    //  the input saying a row names a curve was assigned after the decision that
    //  reads it, and both halves were right on their own.
    R := GoodReach;
    R.OfferedOnOwnRow := False;
    AssertTrue('the dead menu is reported',
        Mentions(RowCommandFindings(R), 'the menu can never be used'));
end;

procedure TUiSelfCheckTest.AContributorRowReadAsNamingNoCurveIsReported;
var
    R: TRowCommandReach;
begin
    //  THE DEFECT THE USER REPORTED. A pack identifies its rows by its own
    //  markup and carries the curve handle beside that id; the window answered
    //  "which curve is selected?" from the id alone, so every pattern in a wave
    //  count read as naming none and Delete curve was dead over all of them.
    R := GoodReach;
    R.SelectedCurveId := '';
    AssertTrue('the mismatch is reported',
        Mentions(RowCommandFindings(R), 'read it as naming none'));
end;

procedure TUiSelfCheckTest.EveryEntryDisabledOverAContributorRowIsReportedToo;
var
    R: TRowCommandReach;
begin
    //  SEPARATELY FROM THE FRAMEWORK'S ROW, because the framework's own rows
    //  went on passing this check for as long as the contributor rows failed it.
    R := GoodReach;
    R.OfferedOnContributorRow := False;
    AssertTrue('the contributor row is judged on its own',
        Mentions(RowCommandFindings(R), 'over a contributor row'));
end;

procedure TUiSelfCheckTest.AModelReportingNoCurveHandleIsNotCountedAsAPass;
var
    R: TRowCommandReach;
begin
    //  NOT SILENCE. The contributor half was never reached, and the difference
    //  between "checked and sound" and "never looked" is the whole value of the
    //  check.
    R := GoodReach;
    R.ContributorCurveId := '';
    AssertTrue('it says the half was not checked',
        Mentions(RowCommandFindings(R), 'could not be checked'));
end;

procedure TUiSelfCheckTest.AndNothingBeyondItIsJudged;
var
    R: TRowCommandReach;
begin
    //  The readings after it are of a row that was never put on screen, so
    //  judging them would report a mismatch nothing caused.
    R := GoodReach;
    R.ContributorCurveId := '';
    R.SelectedCurveId := '';
    R.OfferedOnContributorRow := False;
    AssertFalse('no mismatch is invented',
        Mentions(RowCommandFindings(R), 'read it as naming none'));
    AssertFalse('and no dead menu either',
        Mentions(RowCommandFindings(R), 'over a contributor row'));
end;

{ ---- whether the chart shows the selected curve ---------------------------- }

function SelectedReading: THighlightReading;
begin
    Result := Default(THighlightReading);
    Result.ExpectSelection := True;
    Result.CurveId := 'curve-7';
    Result.PanelCurveId := 'curve-7';
end;

procedure TUiSelfCheckTest.ASelectedCurveDrawnHighlightedReportsNothing;
var
    R: THighlightReading;
begin
    R := SelectedReading;
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    CountSeriesHighlight(R, 'curve-3', NormalLineWidth, False);
    CountSeriesHighlight(R, '', NormalLineWidth, False);
    AssertEquals('one series drawn for it', 1, R.OwnedCount);
    AssertEquals('and it is highlighted', 1, R.OwnedHighlighted);
    AssertEquals('nothing else is', 0, R.OtherHighlighted);
    AssertEquals('so nothing to report', 0,
        Length(HighlightFindings(R, 'after a row was selected')));
end;

procedure TUiSelfCheckTest.AWideSeriesUnderneathIsNotHighlighted;
var
    R: THighlightReading;
begin
    //  HALF A HIGHLIGHT. Wide but drawn in its place, a later curve covers it -
    //  the state a chart that lost its third pass would be in.
    R := SelectedReading;
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, False);
    AssertEquals('counted as drawn for it', 1, R.OwnedCount);
    AssertEquals('but not as highlighted', 0, R.OwnedHighlighted);
    AssertTrue('and reported',
        Mentions(HighlightFindings(R, 'after a row was selected'),
            'not highlighted on the chart'));
end;

procedure TUiSelfCheckTest.ASeriesHighlightedForAnotherCurveIsReported;
var
    R: THighlightReading;
begin
    R := SelectedReading;
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    CountSeriesHighlight(R, 'curve-3', HighlightedLineWidth, True);
    AssertEquals('the stray one is counted', 1, R.OtherHighlighted);
    AssertTrue('and reported',
        Mentions(HighlightFindings(R, 'after a row was selected'),
            'highlighted although'));
end;

procedure TUiSelfCheckTest.OnTopAloneOnAnotherCurveCountsAgainstTheChart;
var
    R: THighlightReading;
begin
    //  Drawn last though not wide: it covers the curve the user chose.
    R := SelectedReading;
    CountSeriesHighlight(R, 'curve-3', NormalLineWidth, True);
    AssertEquals(1, R.OtherHighlighted);
end;

procedure TUiSelfCheckTest.AModelWideSeriesDrawnNormallyCountsForNothing;
var
    R: THighlightReading;
begin
    R := SelectedReading;
    CountSeriesHighlight(R, '', NormalLineWidth, False);
    AssertEquals('not owned by the selection', 0, R.OwnedCount);
    AssertEquals('and not a stray highlight', 0, R.OtherHighlighted);
end;

procedure TUiSelfCheckTest.ASelectionTheWindowDidNotHoldIsNotCountedAsAPass;
var
    R: THighlightReading;
begin
    //  NOTHING SELECTED WHERE SOMETHING SHOULD BE: every series then counts as
    //  somebody else's, so nothing is highlighted and nothing is wrong - and
    //  nothing was checked.
    R := SelectedReading;
    R.CurveId := '';
    R.PanelCurveId := '';
    AssertTrue('it says so',
        Mentions(HighlightFindings(R, 'after a row was selected'),
            'could not be checked'));
end;

procedure TUiSelfCheckTest.ASelectedCurveWithNoSeriesIsNotCountedAsAPass;
var
    R: THighlightReading;
begin
    //  Zero of zero highlighted must say so rather than pass.
    R := SelectedReading;
    CountSeriesHighlight(R, '', NormalLineWidth, False);
    AssertTrue('it says the highlight was not checked',
        Mentions(HighlightFindings(R, 'after a row was selected'),
            'could not be checked'));
end;

procedure TUiSelfCheckTest.APanelShowingAnotherCurveIsReported;
var
    R: THighlightReading;
begin
    //  THE CHART AND THE PANEL DISAGREE. The panel's Delete curve acts on the
    //  held selection, so a row showing one curve while another is held is a
    //  command aimed at a curve the user is not looking at.
    R := SelectedReading;
    R.PanelCurveId := 'curve-3';
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    AssertTrue('the disagreement is reported',
        Mentions(HighlightFindings(R, 'after a table row was selected'),
            'the Model panel shows another'));
end;

procedure TUiSelfCheckTest.WithNothingSelectedAnyHighlightIsReported;
var
    R: THighlightReading;
begin
    R := Default(THighlightReading);
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    AssertTrue('a highlight left behind is reported',
        Mentions(HighlightFindings(R, 'with no curve selected'),
            'still highlighted'));
end;

procedure TUiSelfCheckTest.WithNothingSelectedAndNothingHighlightedReportsNothing;
var
    R: THighlightReading;
begin
    R := Default(THighlightReading);
    CountSeriesHighlight(R, 'curve-7', NormalLineWidth, False);
    AssertEquals(0, Length(HighlightFindings(R, 'with no curve selected')));
end;

procedure TUiSelfCheckTest.EachFindingSaysWhenItWasMeasured;
var
    R: THighlightReading;
    F: TUiFindings;
    i: longint;
begin
    R := SelectedReading;
    CountSeriesHighlight(R, 'curve-7', NormalLineWidth, False);
    CountSeriesHighlight(R, 'curve-3', HighlightedLineWidth, True);
    F := HighlightFindings(R, 'after the chart was replotted');
    AssertTrue('there are findings', Length(F) > 0);
    for i := 0 to High(F) do
        AssertEquals('finding ' + IntToStr(i) + ' begins with the moment: ' + F[i],
            1, Pos('after the chart was replotted', F[i]));
end;

procedure TUiSelfCheckTest.TheHighlightVerdictStatesWhatWasMeasured;
var
    R: THighlightReading;
    S: string;
begin
    R := SelectedReading;
    R.OwnedCount := 3;
    R.OwnedHighlighted := 2;
    R.OtherHighlighted := 1;
    S := HighlightSummary(R, 'after the chart was replotted');
    AssertTrue('the moment: ' + S, Pos('after the chart was replotted', S) > 0);
    AssertTrue('the owned count: ' + S, Pos('2 of 3', S) > 0);
    AssertTrue('the stray count: ' + S, Pos('1 other', S) > 0);
    //  The phrase the checking task recognises a verdict by.
    AssertTrue('the verdict phrase: ' + S,
        Pos('series highlighted on the chart', S) > 0);
    R := Default(THighlightReading);
    S := HighlightSummary(R, 'with no curve selected');
    AssertTrue('with nothing selected too: ' + S,
        Pos('series highlighted on the chart', S) > 0);
end;

procedure TUiSelfCheckTest.APanelShowingNoRowIsNotADisagreement;
var
    R: THighlightReading;
begin
    //  A curve chosen in the table that no panel row stands for - a pack's
    //  panel shows only its own rows - leaves the panel with nothing selected.
    //  That is the panel declining to point, not pointing elsewhere.
    R := SelectedReading;
    R.PanelCurveId := '';
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    AssertFalse('not reported',
        Mentions(HighlightFindings(R, 'after a table row was selected'),
            'the Model panel shows another'));
end;

procedure TUiSelfCheckTest.ASelectionThatDidNotFollowTheGestureIsReported;
var
    R: THighlightReading;
begin
    //  THE COUNTS ALONE CANNOT SEE THIS: the old curve is still consistently
    //  highlighted, so everything agrees - with the wrong curve.
    R := SelectedReading;
    R.ExpectedCurveId := 'curve-3';
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    AssertTrue('reported',
        Mentions(HighlightFindings(R, 'after a table row was selected'),
            'rather than'));
end;

procedure TUiSelfCheckTest.ASelectionThatFollowedTheGestureReportsNothing;
var
    R: THighlightReading;
begin
    R := SelectedReading;
    R.ExpectedCurveId := 'curve-7';
    CountSeriesHighlight(R, 'curve-7', HighlightedLineWidth, True);
    AssertEquals(0,
        Length(HighlightFindings(R, 'after a table row was selected')));
end;

procedure TUiSelfCheckTest.ATableWithNothingToChooseIsNotCountedAsAPass;
begin
    AssertTrue('it says what was not checked: ' + NoTableRowToSelect,
        Pos('could not be checked', NoTableRowToSelect) > 0);
    AssertTrue('and where: ' + NoTableRowToSelect,
        Pos('Curve Attributes', NoTableRowToSelect) > 0);
end;

{ ---- the two verdicts ------------------------------------------------------ }

procedure TUiSelfCheckTest.EachKindOfRowGetsItsOwnCount;
begin
    //  TWO LINES, NOT ONE. A single count would let a menu live over the
    //  framework's rows hide one that is dead over every pack row - which is
    //  exactly the state that was shipped.
    AssertTrue('the framework row''s verdict names its rows: ' +
        RowCommandSummary(True, 4),
        Pos('over a row naming a curve', RowCommandSummary(True, 4)) > 0);
    AssertTrue('the contributor row''s names its own: ' +
        ContributorRowSummary(True, 4),
        Pos('contributor row', ContributorRowSummary(True, 4)) > 0);
    AssertTrue('and it counts what was offered',
        Pos('4 of 4', RowCommandSummary(True, 4)) > 0);
end;

procedure TUiSelfCheckTest.AndBothAreWrittenWhenNothingIsOffered;
begin
    //  A verdict either way, by the same rule the surface and legend checks
    //  follow: one that speaks only on a finding cannot be told from one that
    //  never ran.
    AssertTrue('a verdict for the framework row',
        Pos('0 of 4', RowCommandSummary(False, 4)) > 0);
    AssertTrue('and one for the contributor row',
        Pos('0 of 4', ContributorRowSummary(False, 4)) > 0);
end;


procedure TUiSelfCheckTest.AHintThatOnlyRepeatsTheCaptionIsReported;
begin
    //  'About' over 'About', 'Glossary' over 'Glossary': a hint that repeats
    //  the caption passes a check for an empty hint and explains nothing.
    GivenAGoodRow;
    FRows[0].PaneHint := 'Pick';
    FRows[0].MenuHint := 'Pick';
    AssertTrue(FindingsMention('repeats its caption'));
end;

procedure TUiSelfCheckTest.CaseAndSpacingDoNotHideARepeatedCaption;
begin
    GivenAGoodRow;
    FRows[0].PaneHint := '  pick. ';
    FRows[0].MenuHint := FRows[0].PaneHint;
    AssertTrue(FindingsMention('repeats its caption'));
end;

procedure TUiSelfCheckTest.AHintThatSaysMoreThanTheCaptionIsNotReported;
begin
    GivenAGoodRow;
    AssertFalse(FindingsMention('repeats its caption'));
end;

procedure TUiSelfCheckTest.AContextEntryWhoseTopicResolvesNowhereIsReported;
var
    R: TRowCommandReach;
begin
    R := GoodReach;
    SetLength(R.UnresolvedTopics, 1);
    R.UnresolvedTopics[0] := 'pack/rule/P9';
    AssertTrue(Mentions(RowCommandFindings(R), 'pack/rule/P9'));
end;

procedure TUiSelfCheckTest.EveryUnresolvedTopicIsNamedNotJustTheFirst;
var
    R: TRowCommandReach;
begin
    R := GoodReach;
    SetLength(R.UnresolvedTopics, 2);
    R.UnresolvedTopics[0] := 'a/one';
    R.UnresolvedTopics[1] := 'a/two';
    AssertTrue(Mentions(RowCommandFindings(R), 'a/one'));
    AssertTrue(Mentions(RowCommandFindings(R), 'a/two'));
end;

procedure TUiSelfCheckTest.EntriesAModuleAddedMeanTheMenuIsNotEmpty;
var
    R: TRowCommandReach;
begin
    R := GoodReach;
    R.EntryCount := 0;
    R.ModuleEntryCount := 3;
    AssertFalse(Mentions(RowCommandFindings(R), 'no entries at all'));
end;


{ ---- what the window saw while a fit really ran ---------------------------- }

function TUiSelfCheckTest.AFitThatWasWatched: TLiveProgressReading;
begin
    //  THE BASELINE EVERY CASE BELOW BREAKS IN ONE PLACE: a fit long enough to
    //  watch, animating, drawing frames, with its curves moving.
    Result := Default(TLiveProgressReading);
    Result.Ran := True;
    Result.Animating := True;
    Result.Seconds := 4.0;
    Result.Frames := 12;
    Result.LossPoints := 9;
    Result.Curves := 5;
end;

procedure TUiSelfCheckTest.AFitThatDrewFramesAndMovedItsCurvesReportsNothing;
begin
    AssertEquals('nothing wrong', 0,
        Length(LiveProgressFindings(AFitThatWasWatched)));
end;

procedure TUiSelfCheckTest.AFitThatDrewNoFrameAtAllIsReported;
var
    R: TLiveProgressReading;
    F: TUiFindings;
begin
    //  THE DEFECT THIS CHECK EXISTS FOR, reported by a user twice: the window
    //  sits unchanged for the whole fit and then shows the answer.
    R := AFitThatWasWatched;
    R.Frames := 0;
    R.LossPoints := 0;
    R.Curves := 0;
    F := LiveProgressFindings(R);
    AssertTrue('reported', Length(F) > 0);
    AssertTrue('and says the chart area never changed: ' + F[0],
        Pos('no progress frame', F[0]) > 0);
end;

procedure TUiSelfCheckTest.AnimationThatMovedNoCurveIsReported;
var
    R: TLiveProgressReading;
begin
    //  Frames drawn, but never a frame OF THE MODEL: this is what Animation
    //  Mode looked like when it was broken - a live header over a still chart.
    R := AFitThatWasWatched;
    R.Curves := 0;
    AssertTrue('reported', Length(LiveProgressFindings(R)) > 0);
end;

procedure TUiSelfCheckTest.ButWithoutAnimationNoCurveIsExpectedToMove;
var
    R: TLiveProgressReading;
begin
    //  The default mode draws the loss chart and deliberately leaves the model
    //  alone, so a still chart is right rather than wrong.
    R := AFitThatWasWatched;
    R.Animating := False;
    R.Curves := 0;
    AssertEquals('nothing wrong', 0, Length(LiveProgressFindings(R)));
end;

procedure TUiSelfCheckTest.ALossChartWithNoPointIsReported;
var
    R: TLiveProgressReading;
begin
    //  The other half of the same promise: without animation the user watches
    //  the loss fall, and an empty chart is the empty screen this feature ends.
    R := AFitThatWasWatched;
    R.Animating := False;
    R.Curves := 0;
    R.LossPoints := 0;
    AssertTrue('reported', Length(LiveProgressFindings(R)) > 0);
end;

procedure TUiSelfCheckTest.AFitTooShortToWatchIsNotJudged;
var
    R: TLiveProgressReading;
begin
    //  A fit that converges before the first tick had nothing to show, and
    //  calling that a defect would fail builds over a fast machine.
    R := AFitThatWasWatched;
    R.Seconds := 0.05;
    R.Frames := 0;
    R.LossPoints := 0;
    R.Curves := 0;
    AssertEquals('not judged', 0, Length(LiveProgressFindings(R)));
    AssertTrue('but said out loud: ' + LiveProgressSummary(R),
        Pos('too short', LiveProgressSummary(R)) > 0);
end;

procedure TUiSelfCheckTest.AFitThatNeverRanIsSaidSoRatherThanPassed;
var
    R: TLiveProgressReading;
begin
    //  A check that quietly measures nothing reads exactly like one that passed.
    R := Default(TLiveProgressReading);
    AssertTrue('reported', Length(LiveProgressFindings(R)) > 0);
end;

procedure TUiSelfCheckTest.TheLiveProgressSummaryStatesWhatWasDrawn;
var
    S: string;
begin
    S := LiveProgressSummary(AFitThatWasWatched);
    AssertTrue('the frames: ' + S, Pos('12', S) > 0);
    AssertTrue('the frames of the model: ' + S, Pos('5', S) > 0);
end;


procedure TUiSelfCheckTest.EveryLiveProgressVerdictIsRecognisableAsOne;
var
    R: TLiveProgressReading;
const
    Prefix = 'ui: live progress:';
begin
    //  THE BUILD TASK READS FOR THIS PREFIX and fails when it finds none, so
    //  that a check which quietly did not run cannot pass for one that did
    //  (the build's required-verdicts table). All three
    //  outcomes must therefore carry it - including the two that report having
    //  judged nothing, which are the ones silence would be mistaken for.
    R := AFitThatWasWatched;
    AssertEquals('watched', Prefix,
        Copy(LiveProgressSummary(R), 1, Length(Prefix)));

    R.Seconds := 0.05;
    AssertEquals('too short to judge', Prefix,
        Copy(LiveProgressSummary(R), 1, Length(Prefix)));

    R := Default(TLiveProgressReading);
    AssertEquals('no fit at all', Prefix,
        Copy(LiveProgressSummary(R), 1, Length(Prefix)));
end;

{ ---- a module's nested rows, with a type placed by picks selected ---- }

{ THE DEFECT THIS CHECK WAS ADDED FOR. With an ordinary curve type selected in
  the Tools list, the Model panel replaced a module's nested description of the
  model with the framework's flat list of the same curves - correct in every
  unit test of the rule, and only visible in the window. }
function GoodTreeReading: TModelTreeReading;
begin
    Result := Default(TModelTreeReading);
    Result.Ran := True;
    Result.PicksTypeSelected := True;
    Result.CurveCount := 2;
    Result.NodesDrawn := 2;
    Result.ChildUnderParent := True;
    Result.CollapseSurvivedRefresh := True;
    Result.StaleRowsShown := False;
    Result.CurveFollowsRowAfterRefresh := True;
end;

procedure TUiSelfCheckTest.ANestedTreeDrawnAsATreeReportsNothing;
begin
    AssertEquals(0, Length(ModelTreeFindings(GoodTreeReading)));
end;

procedure TUiSelfCheckTest.ATreeDrawnFlatIsReported;
var
    R: TModelTreeReading;
begin
    R := GoodTreeReading;
    R.ChildUnderParent := False;
    AssertTrue(Mentions(ModelTreeFindings(R), 'not drawn as a tree'));
end;

procedure TUiSelfCheckTest.ACurveListedTwiceIsReported;
var
    R: TModelTreeReading;
begin
    //  The tree kept, and the framework's flat rows for the same curves drawn
    //  after it: the hierarchy is shown, and every curve is shown twice.
    R := GoodTreeReading;
    R.NodesDrawn := 4;
    AssertTrue(Mentions(ModelTreeFindings(R), 'listed twice or not at all'));
end;

procedure TUiSelfCheckTest.AReadingUnderAModulesOwnTypeIsNotCountedAsAPass;
var
    R: TModelTreeReading;
begin
    //  Under the module's own type the old rule showed the tree too, so such a
    //  reading proves nothing about the case that failed.
    R := GoodTreeReading;
    R.PicksTypeSelected := False;
    AssertTrue(Mentions(ModelTreeFindings(R), 'was not checked'));
end;

procedure TUiSelfCheckTest.ABuildWithNoModulePanelBowsOutWithoutAFinding;
var
    R: TModelTreeReading;
begin
    //  A build with no module describing the model has no tree to keep. That
    //  is a verdict - written, so it cannot be mistaken for silence - and not a
    //  failure of the build.
    R := Default(TModelTreeReading);
    AssertEquals(0, Length(ModelTreeFindings(R)));
    AssertTrue(Pos('not checked', ModelTreeSummary(R)) > 0);
end;

{ A REFRESH THAT CHANGED NOTHING MUST NOT DRAW THE PANEL AGAIN. The panel is
  refreshed on every command refresh, and each rebuild opened every subtree -
  a user who closed a pattern to read the rest saw it open again at once. }
procedure TUiSelfCheckTest.ACollapsedPatternOpenedByARefreshIsReported;
var
    R: TModelTreeReading;
begin
    R := GoodTreeReading;
    R.CollapseSurvivedRefresh := False;
    AssertTrue(Mentions(ModelTreeFindings(R), 'collapsed opened again'));
end;

{ Rows kept from a model that is no longer open - a module whose redraw exited
  early never replaced them - must not be shown over the model that is. }
procedure TUiSelfCheckTest.RowsOfAModelNotOpenShownAreReported;
var
    R: TModelTreeReading;
begin
    R := GoodTreeReading;
    R.StaleRowsShown := True;
    AssertTrue(Mentions(ModelTreeFindings(R), 'a model that is not open'));
end;

{ THE SELECTED CURVE FOLLOWS THE SELECTED ROW. A refused Delete curve clears
  the selected curve and leaves the row, and the panel, as they were; each
  refresh used to redraw the panel, which re-selected the row and so its
  curve. A refresh that draws nothing must put the curve back itself, or the
  row stays highlighted over a curve the window no longer holds. }
procedure TUiSelfCheckTest.ASelectedCurveNotRestoredByARefreshIsReported;
var
    R: TModelTreeReading;
begin
    R := GoodTreeReading;
    R.CurveFollowsRowAfterRefresh := False;
    AssertTrue(Mentions(ModelTreeFindings(R), 'highlighted row'));
end;

procedure TUiSelfCheckTest.BothModelTreeVerdictsNameTheSameCheck;
const
    { What check-ui requires of the log, in the orchestrator's log reader
      (Read-CheckUiLog). }
    Phrase = 'a module''s nested rows';
begin
    AssertTrue('when it ran', Pos(Phrase, ModelTreeSummary(GoodTreeReading)) > 0);
    AssertTrue('when it bowed out',
        Pos(Phrase, ModelTreeSummary(Default(TModelTreeReading))) > 0);
end;

{ ---- a module's rows while a fit runs ---- }

{ NOT DRAWN WHILE A FIT RUNS. Drawing the panel puts the selection back, which
  hands the row to the module that owns it, and a module answering by asking
  the engine waits for the fit's lock - with the whole window waiting behind
  it. The framework's own refresh already waited; a module's push did not. }
procedure TUiSelfCheckTest.AModulesRowsDrawnWhileAFitRanAreReported;
var
    R: TLiveProgressReading;
begin
    R := AFitThatWasWatched;
    R.PanelPushChecked := True;
    R.PanelDrawnDuringFit := True;
    R.PanelDrawnAfterFit := True;
    AssertTrue(Mentions(LiveProgressFindings(R), 'while the fit ran'));
end;

procedure TUiSelfCheckTest.AModulesRowsNeverDrawnAfterTheFitAreReported;
var
    R: TLiveProgressReading;
begin
    //  Kept rather than drawn - and then they have to be drawn once it ends.
    R := AFitThatWasWatched;
    R.PanelPushChecked := True;
    R.PanelDrawnDuringFit := False;
    R.PanelDrawnAfterFit := False;
    AssertTrue(Mentions(LiveProgressFindings(R), 'never drawn after'));
end;

procedure TUiSelfCheckTest.ABuildWithNoModulePanelDoesNotJudgeThePanelDuringAFit;
var
    R: TLiveProgressReading;
begin
    R := AFitThatWasWatched;
    R.PanelPushChecked := False;
    R.PanelDrawnDuringFit := True;
    AssertEquals(0, Length(LiveProgressFindings(R)));
end;

{ ------------------------------ data sources -------------------------------- }

{ A build whose sources are all present, explained and reachable. }
function ASoundBuild: TDataSourceReading;
begin
    Result := Default(TDataSourceReading);
    Result.SourceCount := 3;
    Result.OfflineCount := 1;
    Result.Findings := '';
    Result.CommandFound := True;
    Result.CommandEnabled := True;
end;

procedure TUiSelfCheckTest.ASoundSetOfSourcesReportsNothing;
begin
    AssertEquals(0, Length(DataSourceFindings(ASoundBuild)));
end;

procedure TUiSelfCheckTest.NoSourcesAtAllIsReportedAsStartUpNotRegistering;
var
    R: TDataSourceReading;
    Reported: TUiFindings;
begin
    R := ASoundBuild;
    R.SourceCount := 0;
    R.OfflineCount := 0;
    Reported := DataSourceFindings(R);
    AssertTrue('an empty wizard must be reported', Length(Reported) > 0);
    //  It names the CAUSE, because an empty list in front of a user reads as a
    //  build without the feature rather than as a call start-up did not make.
    AssertTrue(Reported[0], Pos('register', Reported[0]) > 0);
end;

procedure TUiSelfCheckTest.SourcesThatAllNeedTheNetworkAreReported;
var
    R: TDataSourceReading;
begin
    R := ASoundBuild;
    R.OfflineCount := 0;
    //  Nothing to fetch on a machine with no connection, and nothing for the
    //  end-to-end path to run against offline.
    AssertTrue('reported', Length(DataSourceFindings(R)) > 0);
end;

procedure TUiSelfCheckTest.AnIncompleteSourceIsReportedWithTheWalksWords;
var
    R: TDataSourceReading;
    Reported: TUiFindings;
begin
    R := ASoundBuild;
    R.Findings := 'TRruffSource declares no explanation topic';
    Reported := DataSourceFindings(R);
    AssertTrue('reported', Length(Reported) > 0);
    //  The walk's own sentence, which names the class: rewording it here would
    //  lose the one thing that makes it actionable.
    AssertTrue(Reported[0], Pos('TRruffSource', Reported[0]) > 0);
end;

procedure TUiSelfCheckTest.NoCommandReachingTheWizardIsReported;
var
    R: TDataSourceReading;
begin
    R := ASoundBuild;
    R.CommandFound := False;
    R.CommandEnabled := False;
    //  Sources nothing can reach are the shape of defect this project has had
    //  more than once: a menu built and never attached, an engine with no way
    //  in.
    AssertTrue('reported', Length(DataSourceFindings(R)) > 0);
end;

procedure TUiSelfCheckTest.ACommandOnTheMenuAndDisabledIsReportedDifferently;
var
    R: TDataSourceReading;
    Reported: TUiFindings;
begin
    R := ASoundBuild;
    R.CommandEnabled := False;
    Reported := DataSourceFindings(R);
    AssertTrue('reported', Length(Reported) > 0);
    AssertTrue(Reported[0], Pos('disabled', Reported[0]) > 0);
end;

procedure TUiSelfCheckTest.TheSummarySaysWhatWasCountedEitherWay;
var
    Summary: string;
begin
    Summary := DataSourceSummary(ASoundBuild);
    //  Written whatever the outcome: a check that speaks only when it finds
    //  something cannot be told from one that never ran - and carrying the
    //  prefix the driving task selects on, or it is written and never read.
    AssertTrue(Summary, Copy(Summary, 1, 4) = 'ui: ');
    AssertTrue(Summary, Pos('3', Summary) > 0);
    AssertTrue(Summary, Pos('offline', Summary) > 0);
end;

procedure TUiSelfCheckTest.TheSummarySaysWhenNoCommandReachesTheSources;
var
    R: TDataSourceReading;
begin
    R := ASoundBuild;
    R.CommandFound := False;
    AssertTrue(Pos('no menu command reaches them', DataSourceSummary(R)) > 0);
    AssertEquals('and a sound build does not say it', 0,
        Pos('no menu command', DataSourceSummary(ASoundBuild)));
end;

initialization
    //  A unit test: records in, sentences out. No widget and no window - which
    //  is the point, because the check these rules belong to can only run
    //  inside the application and cannot verify itself.
    RegisterTest('unit', TUiSelfCheckTest);
end.
