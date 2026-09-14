// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the datasheet says about a fit.)

THE ONLY PLACE THE FIT IS READABLE AS NUMBERS, and until this was extracted it
was written one grid cell at a time inside a unit that uses Forms and reaches
into the main form by name - so none of it could be checked by anything but a
person reading a screen.

Nothing here is a formatting detail. A table with one column too few silently
drops a curve. A row index off by one shifts a whole column against the profile
beside it. Both look exactly like data, which is the worst way for a defect to
present in a program whose entire output is numbers.

The model is built by hand from point sets rather than by running a fit: what is
being tested is the tabulation, and a fit would make the expected values
something to be derived rather than stated.
}
unit testcase_summary_table;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    summary_table, points_set, title_points_set, curve_points_set,
    self_copied_component, gauss_points_set, fit_client;

type
    TSummaryTableTest = class(TTestCase)
    private
        FTable: TSummaryTable;
        FProfile, FComputed, FDelta, FBounds: TTitlePointsSet;
        FCurves: TSelfCopiedCompList;
        { A ten-sample profile at x = 0..9, y = 10*x. }
        procedure GiveAProfile;
        { One interval covering x in [AFrom, ATo]. }
        procedure AddInterval(AFrom, ATo: double);
        { A curve named ATitle covering profile indices AFrom..ATo. }
        procedure AddCurve(const ATitle: string; AFrom, ATo: longint);
        function Build: TSummaryOutcome;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  How a number reads.
        procedure AValueIsWrittenWithAFixedNumberOfDecimals;
        procedure EveryValueInAColumnHasTheSameWidthOfDecimals;
        procedure AVerySmallValueIsNotWrittenInExponentialForm;

        //  Which curve belongs where.
        procedure ACurveStartingInsideTheIntervalBelongsToIt;
        procedure ACurveStartingAtEitherEndBelongsToIt;
        procedure ACurveStartingOutsideTheIntervalDoesNot;
        procedure AnEmptyCurveBelongsNowhere;
        procedure ANilCurveIsNotAMember;

        //  How wide and how tall.
        procedure TheWidestIntervalDecidesTheColumnCount;
        procedure EveryIntervalContributesItsSamplesAndAHeading;
        procedure WithNoCurvesThereAreOnlyTheFixedColumns;

        //  When it refuses.
        procedure AnIncompleteModelBuildsNothing;
        procedure NoIntervalsIsNotAnIncompleteModel;
        procedure AHalfPlacedIntervalBuildsNothingRatherThanFaulting;
        procedure AndSoDoesAnIntervalPlacedBeforeAnythingIsComputed;
        procedure OrAComputedProfileOfADifferentLength;
        procedure OrNoMeasuredDataAtAll;
        procedure ARefusalLeavesTheTableEmpty;

        //  What lands in the cells.
        procedure TheFixedColumnsAreNamed;
        procedure EachIntervalHasItsOwnNumberedSubheading;
        procedure TheProfileTheModelAndTheDifferenceAreSideBySide;
        procedure ACurveIsNamedAtTheHeadOfItsColumn;
        procedure ACurvesValuesLineUpWithTheProfileRows;
        procedure ACurveThatCoversPartOfAnIntervalLeavesTheRestBlank;
        procedure TwoIntervalsAreStackedInOneTable;
        procedure ACellOutsideTheTableIsBlankRatherThanAFault;
    end;

implementation

procedure TSummaryTableTest.SetUp;
begin
    FTable := TSummaryTable.Create;
    FProfile := TTitlePointsSet.Create(nil);
    FComputed := TTitlePointsSet.Create(nil);
    FDelta := TTitlePointsSet.Create(nil);
    FBounds := TTitlePointsSet.Create(nil);
    FCurves := TSelfCopiedCompList.Create(True);
end;

procedure TSummaryTableTest.TearDown;
begin
    FreeAndNil(FCurves);
    FreeAndNil(FBounds);
    FreeAndNil(FDelta);
    FreeAndNil(FComputed);
    FreeAndNil(FProfile);
    FreeAndNil(FTable);
end;

procedure TSummaryTableTest.GiveAProfile;
var
    i: longint;
begin
    //  DISTINCT VALUES in all three profiles, so a column filled from the wrong
    //  set is visible rather than plausible.
    for i := 0 to 9 do
    begin
        FProfile.AddNewPoint(i, 10 * i);
        FComputed.AddNewPoint(i, 10 * i + 1);
        FDelta.AddNewPoint(i, -1);
    end;
end;

procedure TSummaryTableTest.AddInterval(AFrom, ATo: double);
begin
    FBounds.AddNewPoint(AFrom, 0);
    FBounds.AddNewPoint(ATo, 0);
end;

procedure TSummaryTableTest.AddCurve(const ATitle: string;
    AFrom, ATo: longint);
var
    C: TGaussPointsSet;
    i: longint;
begin
    //  A real curve class, because the table reads it as one - and its x values
    //  are copied from the profile, which is what makes the containment test
    //  exact rather than approximate.
    C := TGaussPointsSet.Create(nil, FProfile.PointXCoord[AFrom]);
    C.FTitle := ATitle;
    for i := AFrom to ATo do
        C.AddNewPoint(FProfile.PointXCoord[i], 100 + i);
    FCurves.Add(C);
end;

function TSummaryTableTest.Build: TSummaryOutcome;
begin
    Result := FTable.Build(FProfile, FCurves, FComputed, FDelta, FBounds);
end;

{ ---- how a number reads ---------------------------------------------------- }

procedure TSummaryTableTest.AValueIsWrittenWithAFixedNumberOfDecimals;
begin
    AssertEquals('four decimals', '1.5000', Trim(CurveValueText(1.5)));
end;

procedure TSummaryTableTest.EveryValueInAColumnHasTheSameWidthOfDecimals;
begin
    //  THE REASON FOR ffFixed. A column whose numbers have differing numbers of
    //  decimals is hard to read, and ffGeneral gives exactly that.
    AssertEquals('a whole number is as wide as a fractional one',
        Length(Trim(CurveValueText(1))), Length(Trim(CurveValueText(1.5))));
    AssertEquals('and so is a larger pair',
        Length(Trim(CurveValueText(1000))),
        Length(Trim(CurveValueText(1000.25))));
    AssertEquals('four decimals either way', '1000.0000',
        Trim(CurveValueText(1000)));
end;

procedure TSummaryTableTest.AVerySmallValueIsNotWrittenInExponentialForm;
begin
    //  ffGeneral shows anything below 0.00001 in exponential form, which in a
    //  column of fixed-point numbers reads as a different quantity entirely.
    //  ffFixed rounds it to zero instead, which is accepted.
    AssertTrue('no exponent: ' + CurveValueText(0.0000001),
        Pos('E', UpperCase(CurveValueText(0.0000001))) = 0);
end;

{ ---- which curve belongs where --------------------------------------------- }

procedure TSummaryTableTest.ACurveStartingInsideTheIntervalBelongsToIt;
begin
    GiveAProfile;
    AddCurve('c', 3, 5);
    AssertTrue('inside',
        CurveIsInInterval(TCurvePointsSet(FCurves.Items[0]), 2, 8));
end;

procedure TSummaryTableTest.ACurveStartingAtEitherEndBelongsToIt;
begin
    //  INCLUSIVE AT BOTH ENDS. A curve seeded on the interval's own first
    //  sample is the ordinary case, and excluding it would drop the first curve
    //  of every interval.
    GiveAProfile;
    AddCurve('start', 2, 4);
    AddCurve('finish', 8, 9);
    AssertTrue('at the start',
        CurveIsInInterval(TCurvePointsSet(FCurves.Items[0]), 2, 8));
    AssertTrue('at the finish',
        CurveIsInInterval(TCurvePointsSet(FCurves.Items[1]), 2, 8));
end;

procedure TSummaryTableTest.ACurveStartingOutsideTheIntervalDoesNot;
begin
    GiveAProfile;
    AddCurve('before', 0, 1);
    AddCurve('after', 9, 9);
    AssertFalse('before it',
        CurveIsInInterval(TCurvePointsSet(FCurves.Items[0]), 2, 8));
    AssertFalse('after it',
        CurveIsInInterval(TCurvePointsSet(FCurves.Items[1]), 2, 7));
end;

procedure TSummaryTableTest.AnEmptyCurveBelongsNowhere;
var
    C: TGaussPointsSet;
begin
    //  It has no first abscissa to compare, and reading one would be a read
    //  past the end of the set.
    C := TGaussPointsSet.Create(nil, 0);
    try
        AssertFalse('nowhere', CurveIsInInterval(C, 0, 10));
    finally
        C.Free;
    end;
end;

procedure TSummaryTableTest.ANilCurveIsNotAMember;
begin
    AssertFalse('nil', CurveIsInInterval(nil, 0, 10));
end;

{ ---- how wide and how tall ------------------------------------------------- }

procedure TSummaryTableTest.TheWidestIntervalDecidesTheColumnCount;
begin
    //  THE MAXIMUM ACROSS INTERVALS, not the total and not the first. Curve
    //  columns are reused interval by interval, so the table needs as many as
    //  the busiest interval holds - one fewer and a curve vanishes from the
    //  table with no error anywhere.
    GiveAProfile;
    AddInterval(0, 3);
    AddInterval(5, 9);
    AddCurve('a', 0, 1);
    AddCurve('b', 5, 6);
    AddCurve('c', 6, 7);
    AddCurve('d', 7, 8);
    AssertEquals('the busiest interval has three', 3,
        MaxCurvesInAnyInterval(FCurves, FBounds));
    AssertTrue('built', Build = soBuilt);
    AssertEquals('four fixed columns and three curve columns',
        FixedColumnCount + 3, FTable.ColCount);
end;

procedure TSummaryTableTest.EveryIntervalContributesItsSamplesAndAHeading;
begin
    //  Four samples in the first interval, five in the second, one heading row
    //  for the table and one subheading each.
    GiveAProfile;
    AddInterval(0, 3);
    AddInterval(5, 9);
    AssertEquals('nine samples in all', 9,
        PointsInBounds(FProfile, FBounds));
    AssertTrue('built', Build = soBuilt);
    AssertEquals('a heading, nine samples and two subheadings',
        1 + 9 + 2, FTable.RowCount);
    AssertEquals('two intervals', 2, FTable.IntervalCount);
end;

procedure TSummaryTableTest.WithNoCurvesThereAreOnlyTheFixedColumns;
begin
    //  The profile, the model and the difference are worth tabulating on their
    //  own; a fit that produced no curve is still a fit that ran.
    GiveAProfile;
    AddInterval(0, 3);
    AssertTrue('built', Build = soBuilt);
    AssertEquals('four columns', FixedColumnCount, FTable.ColCount);
end;

{ ---- when it refuses ------------------------------------------------------- }

procedure TSummaryTableTest.AnIncompleteModelBuildsNothing;
begin
    //  Asked before a fit has produced its half. Ordinary during a refresh, so
    //  it must be a quiet refusal and not a fault.
    GiveAProfile;
    AddInterval(0, 3);
    AssertTrue('no computed profile',
        FTable.Build(FProfile, FCurves, nil, FDelta, FBounds) =
        soModelIncomplete);
    AssertTrue('no profile',
        FTable.Build(nil, FCurves, FComputed, FDelta, FBounds) =
        soModelIncomplete);
    AssertTrue('no bounds',
        FTable.Build(FProfile, FCurves, FComputed, FDelta, nil) =
        soModelIncomplete);
end;

procedure TSummaryTableTest.NoIntervalsIsNotAnIncompleteModel;
begin
    //  DISTINGUISHED DELIBERATELY. A model with no interval yet is the ordinary
    //  state of a freshly opened file, and the caller must EMPTY the table for
    //  it - where an incomplete model means leave the grid alone. Reporting one
    //  as the other leaves the previous fit's numbers on screen beside new data.
    GiveAProfile;
    AssertTrue('no intervals', Build = soNoIntervals);
end;

procedure TSummaryTableTest.AHalfPlacedIntervalBuildsNothingRatherThanFaulting;
begin
    //  A CRASH THE USER COULD REACH IN TWO CLICKS, and this is the assertion
    //  that was missing. Picking fit intervals adds ONE bound per click, and
    //  every click re-reads the model and refills this table - so the first
    //  click of every interval asks it to draw an odd number of bounds. It
    //  raised, the exception reached the top level and the client aborted.
    //
    //  The rule that a fit needs paired bounds is not weakened; it simply does
    //  not belong to a table being drawn mid-gesture.
    AddInterval(2, 6);
    //  One more click: the second bound of the next interval has not landed.
    FBounds.AddNewPoint(9, 0);
    AssertTrue('a gesture in progress is not a model',
        Build = soModelIncomplete);
    AssertEquals('and nothing was drawn', 0, FTable.RowCount);
end;

procedure TSummaryTableTest.AndSoDoesAnIntervalPlacedBeforeAnythingIsComputed;
begin
    //  THE SECOND CRASH ON THE SAME PATH. The computed profile and the
    //  difference exist from the moment a file is open and hold nothing until
    //  something has been computed - so an interval placed before the first fit
    //  had this reading index 0 of an empty set, and the client aborted.
    //  Assigned is not the same as filled.
    AddInterval(2, 6);
    FComputed.Clear;
    FDelta.Clear;
    AssertTrue('nothing computed is an incomplete model',
        Build = soModelIncomplete);
    AssertEquals('and nothing was drawn', 0, FTable.RowCount);
end;

procedure TSummaryTableTest.OrAComputedProfileOfADifferentLength;
begin
    //  Read sample by sample AGAINST the measured profile, so a shorter one
    //  overruns rather than merely showing less - the same fault one index
    //  later.
    AddInterval(2, 6);
    FComputed.DeletePoint(FComputed.PointsCount - 1);
    AssertTrue('a mismatch is an incomplete model',
        Build = soModelIncomplete);
end;

procedure TSummaryTableTest.OrNoMeasuredDataAtAll;
begin
    //  Everything assigned and the measured profile empty - the state between
    //  a file being closed and the next one opening. Every column here is read
    //  against that profile, so there is nothing to draw and nothing to index.
    AddInterval(2, 6);
    FProfile.Clear;
    AssertTrue('no data is an incomplete model', Build = soModelIncomplete);
end;

procedure TSummaryTableTest.ARefusalLeavesTheTableEmpty;
begin
    //  A second Build that refuses must not leave the first one's contents
    //  readable, or a caller that ignores the outcome shows stale numbers.
    GiveAProfile;
    AddInterval(0, 3);
    AssertTrue('built', Build = soBuilt);
    AssertTrue('there was a table', FTable.RowCount > 0);
    FBounds.Clear;
    AssertTrue('now refused', Build = soNoIntervals);
    AssertEquals('no rows', 0, FTable.RowCount);
    AssertEquals('no columns', 0, FTable.ColCount);
end;

{ ---- what lands in the cells ----------------------------------------------- }

procedure TSummaryTableTest.TheFixedColumnsAreNamed;
begin
    //  The same words that name the series on the chart, single-sourced with
    //  them: the column a user reads and the curve they are looking at are the
    //  same thing.
    GiveAProfile;
    AddInterval(0, 3);
    Build;
    AssertEquals('position', PositionName, FTable.CellAt(0, 0));
    AssertEquals('amplitude', AmplitudeName, FTable.CellAt(1, 0));
    AssertEquals('total', TotalAmplitudeName, FTable.CellAt(2, 0));
    AssertEquals('difference', DifferenceName, FTable.CellAt(3, 0));
end;

procedure TSummaryTableTest.EachIntervalHasItsOwnNumberedSubheading;
begin
    //  NUMBERED FROM ONE, because it is what the user is told to look at. The
    //  subheading is the only thing separating one interval's rows from the
    //  next's in a single scrolling table.
    GiveAProfile;
    AddInterval(0, 2);
    AddInterval(5, 7);
    Build;
    AssertEquals('the first subheading', IntervalHeading, FTable.CellAt(1, 1));
    AssertEquals('says which', '1', FTable.CellAt(3, 1));
    //  1 heading + 1 subheading + 3 samples = row 5 is the second subheading.
    AssertEquals('the second subheading', IntervalHeading, FTable.CellAt(1, 5));
    AssertEquals('says which', '2', FTable.CellAt(3, 5));
end;

procedure TSummaryTableTest.TheProfileTheModelAndTheDifferenceAreSideBySide;
begin
    //  THE POINT OF THE TABLE. Reading a row across answers "what did the model
    //  say here, and by how much was it wrong" - so a column filled from the
    //  wrong set is the one defect that makes the whole table lie.
    GiveAProfile;
    AddInterval(0, 3);
    Build;
    //  Row 2: heading, subheading, then the sample at x = 0.
    AssertEquals('the abscissa', CurveValueText(0), FTable.CellAt(0, 2));
    AssertEquals('the measurement', CurveValueText(0), FTable.CellAt(1, 2));
    AssertEquals('the model', CurveValueText(1), FTable.CellAt(2, 2));
    AssertEquals('the difference', CurveValueText(-1), FTable.CellAt(3, 2));
    //  And the next sample, so a column that is right only at its first row
    //  cannot pass.
    AssertEquals('the next abscissa', CurveValueText(1), FTable.CellAt(0, 3));
    AssertEquals('its measurement', CurveValueText(10), FTable.CellAt(1, 3));
    AssertEquals('its model value', CurveValueText(11), FTable.CellAt(2, 3));
end;

procedure TSummaryTableTest.ACurveIsNamedAtTheHeadOfItsColumn;
begin
    GiveAProfile;
    AddInterval(0, 3);
    AddCurve('Gaussian 1', 0, 2);
    Build;
    AssertEquals('named', 'Gaussian 1', FTable.CellAt(FixedColumnCount, 0));
end;

procedure TSummaryTableTest.ACurvesValuesLineUpWithTheProfileRows;
begin
    //  UNDER THE ROW ITS OWN ABSCISSA NAMES. A curve written from its own first
    //  row instead would sit one or more rows above the profile it belongs to,
    //  and every value in the column would be attributed to the wrong x.
    GiveAProfile;
    AddInterval(0, 5);
    AddCurve('c', 2, 4);
    Build;
    //  The curve's first sample is at profile index 2, which is table row 4
    //  (heading, subheading, then indices 0 and 1).
    AssertEquals('its first value', CurveValueText(102),
        FTable.CellAt(FixedColumnCount, 4));
    AssertEquals('and it is on the row of its own abscissa',
        CurveValueText(2), FTable.CellAt(0, 4));
end;

procedure TSummaryTableTest.ACurveThatCoversPartOfAnIntervalLeavesTheRestBlank;
begin
    //  THE HONEST PICTURE of where a curve exists. Filling the rest with zeros
    //  would say the curve contributes nothing there, which is a different
    //  claim from saying it is not there at all.
    GiveAProfile;
    AddInterval(0, 5);
    AddCurve('c', 2, 3);
    Build;
    AssertEquals('nothing above it', '',
        FTable.CellAt(FixedColumnCount, 2));
    AssertTrue('something where it is',
        FTable.CellAt(FixedColumnCount, 4) <> '');
    AssertEquals('and nothing below it', '',
        FTable.CellAt(FixedColumnCount, 7));
end;

procedure TSummaryTableTest.TwoIntervalsAreStackedInOneTable;
begin
    //  The second interval's rows start after the first interval's, and its own
    //  curve starts again at the first curve column. Getting the running row
    //  index wrong here overwrites the first interval with the second.
    GiveAProfile;
    AddInterval(0, 2);
    AddInterval(6, 8);
    AddCurve('first', 0, 1);
    AddCurve('second', 6, 7);
    Build;
    //  THE HEADER NAMES THE LAST INTERVAL'S CURVE, and that is what this
    //  table has always done: curve columns are reused interval by interval,
    //  and there is one header row for all of them, so each interval overwrites
    //  the previous one's names. Pinned as it behaves rather than as it should
    //  be - the fix is a heading per interval, which is a change to what the
    //  user sees, not to what this class computes. See findings.md.
    AssertEquals('the header names the last interval to use the column',
        'second', FTable.CellAt(FixedColumnCount, 0));
    //  Rows: 0 heading, 1 subheading, 2..4 samples, 5 subheading, 6..8 samples.
    AssertEquals('the first interval kept its own abscissa',
        CurveValueText(0), FTable.CellAt(0, 2));
    AssertEquals('and the second interval starts where it should',
        CurveValueText(6), FTable.CellAt(0, 6));
    AssertTrue('the second curve wrote into the second interval',
        FTable.CellAt(FixedColumnCount, 6) <> '');
end;

procedure TSummaryTableTest.ACellOutsideTheTableIsBlankRatherThanAFault;
begin
    //  The grid this fills outlives any one table and may be larger than the
    //  current one while it is being resized.
    GiveAProfile;
    AddInterval(0, 2);
    Build;
    AssertEquals('past the last column', '', FTable.CellAt(999, 0));
    AssertEquals('past the last row', '', FTable.CellAt(0, 999));
    AssertEquals('before the first', '', FTable.CellAt(-1, -1));
end;

initialization
    //  A unit test: point sets in memory and a table of strings. No grid, no
    //  form, no fit.
    RegisterTest('unit', TSummaryTableTest);
end.
