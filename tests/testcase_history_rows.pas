// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the History tab says: each row's two lines, the details pane, and
the words of its refusals and its question.)

A unit test: strings out of records. The window that draws them is not involved.
}
unit testcase_history_rows;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry,
    fit_points_json, fit_project_document, model_history, history_rows,
    sample_coverage, coordinate_axis, axis_mode_registry, axis_choice,
    axis_mode_registration, chart_axes, rfactor_text;

type
    THistoryRowsTest = class(TTestCase)
    private
        FHistory: TModelHistory;
        function AnEntry(const AId: string; ARFactor: double;
            ACurves: longint): THistoryEntry;
        procedure Record_(const AId: string; ARFactor: double;
            ACurves: longint);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AHeadlineSaysTheRFactorAndTheCurves;
        procedure TheFirstFittedRowSkipsAModelNobodyFitted;
        procedure TheRowsAreRewrittenOnTheScaleInForce;
        procedure AListNotShowingThisHistoryIsLeftToTheRedraw;
        procedure ARowListWithNoFitHasNoFittedRow;
        procedure OneCurveIsNotCurves;
        procedure AModelNobodyFittedSaysSoRatherThanShowingMinusOne;
        procedure ANameComesFirst;
        procedure OnALogarithmicScaleTheRowGivesTheLogarithm;
        procedure OnALinearScaleTheRowGivesTheRFactor;
        procedure OnACustomScaleTheRowGivesTheUsersQuantity;
        procedure AFormulaWithNoValueHereFallsBackToTheRFactor;
        procedure AModelNobodyFittedReadsNotFittedOnEveryScale;
        procedure TheDetailsKeepTheRFactorItselfWhateverTheScale;
        procedure TheDeleteQuestionNamesTheModelAsItsRowDoes;

        procedure ABylineSaysWhenAndWhatMadeIt;
        procedure AStoppedRunSaysItWasStopped;
        procedure TheTimeIsLocal;
        procedure AnotherDayShowsItsDate;
        procedure ATimeThisProgramDidNotWriteIsShownAsWritten;

        procedure TheCurrentAndTheBestAreMarked;

        procedure TheDetailsSayEverythingTheRowCannot;
        procedure TheDetailsNameTheModelItStartedFrom;
        procedure TheDetailsNameTheCurveTypeWhenTheWindowCanName;
        procedure TheStatisticsAreShownOnlyWhenThereAreAny;
        procedure TheSamplesNoCurveCoveredAreExplainedBesideTheFigures;
        procedure WhetherTheDifferenceWasNoiseIsKeptWithTheFigures;
        procedure TheBestIsSaidToBeTheBestAndAmongWhat;
        procedure AMissingProfileIsExplained;

        procedure MakeCurrentSaysWhyItIsOrIsNotOffered;
        procedure DeletingNamesTheModelAndWhatHappensToItsChildren;
        procedure DeletingSaysTheLiveModelIsNotTouched;
    end;

implementation

{ Every entry AHistory holds, as Load takes them - so a test can load them back
  without a profile, which is what a damaged file looks like. Here rather than on
  TModelHistory: nothing in the program needs it (find-dead-code). }
function EntriesOf(AHistory: TModelHistory): THistoryEntries;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, AHistory.Count);
    for i := 0 to AHistory.Count - 1 do
        Result[i] := AHistory[i];
end;

procedure THistoryRowsTest.SetUp;
begin
    //  As the window's start-up does: the scales are modes in a registry.
    RegisterAllAxisModes;
    FHistory := TModelHistory.Create;
end;

procedure THistoryRowsTest.TearDown;
begin
    FreeAndNil(FHistory);
end;

function THistoryRowsTest.AnEntry(const AId: string; ARFactor: double;
    ACurves: longint): THistoryEntry;
begin
    Result := Default(THistoryEntry);
    Result.Id := AId;
    Result.CreatedUtc := '2026-10-02T12:33:00Z';
    Result.RunKind := hrkMinimizeDifference;
    Result.ContentHash := AId;
    Result.ProfileHash := 'p';
    Result.Model := EmptyProjectDocument;
    Result.Model.RFactor := ARFactor;
    SetLength(Result.Model.Curves, ACurves);
end;

procedure THistoryRowsTest.Record_(const AId: string; ARFactor: double;
    ACurves: longint);
var
    P: TPointsData;
begin
    P := Default(TPointsData);
    AssertTrue(FHistory.Add(AnEntry(AId, ARFactor, ACurves), P));
end;

procedure THistoryRowsTest.TheFirstFittedRowSkipsAModelNobodyFitted;
begin
    Record_('a', 0.5, 2);
    Record_('b', 0.25, 3);
    Record_('c', -1, 4);
    //  Newest first, as the list shows them: 'c' was never fitted.
    AssertEquals(1, FirstFittedRow(FHistory, [2, 1, 0]));
end;

procedure THistoryRowsTest.ARowListWithNoFitHasNoFittedRow;
begin
    Record_('a', -1, 2);
    AssertEquals('none fitted', -1, FirstFittedRow(FHistory, [0]));
    AssertEquals('no rows', -1, FirstFittedRow(FHistory, []));
end;

procedure THistoryRowsTest.AHeadlineSaysTheRFactorAndTheCurves;
begin
    AssertEquals('R-factor 0.0421300 · 7 curves',
        HistoryRowHeadline(AnEntry('a', 0.04213, 7), nil));
end;

procedure THistoryRowsTest.OneCurveIsNotCurves;
begin
    AssertEquals('R-factor 0.100000 · 1 curve',
        HistoryRowHeadline(AnEntry('a', 0.1, 1), nil));
end;

procedure THistoryRowsTest.AModelNobodyFittedSaysSoRatherThanShowingMinusOne;
begin
    AssertEquals('Not fitted · 3 curves',
        HistoryRowHeadline(AnEntry('a', -1, 3), nil));
end;

procedure THistoryRowsTest.ANameComesFirst;
var
    E: THistoryEntry;
begin
    E := AnEntry('a', 0.04213, 7);
    E.Name := 'free widths';
    AssertEquals('free widths · R-factor 0.0421300 · 7 curves', HistoryRowHeadline(E, nil));
end;

{ THE AXIS THE WINDOW HANDS OVER: TChartAxes, chosen as View > R-factor Scale
  chooses (TFormMain.ChooseAxisMode), and its Axis(adLoss) - not an axis built
  here, which would leave the window's own wiring untried. }
function LossAxisChosen(AAxes: TChartAxes; const AModeId: string): TCoordinateAxis;
begin
    AAxes.Choose(adLoss, AModeId);
    Result := AAxes.Axis(adLoss);
end;

procedure THistoryRowsTest.OnALogarithmicScaleTheRowGivesTheLogarithm;
var
    Axes: TChartAxes;
begin
    //  WHAT THE USER ASKED FOR: models a power of ten apart read 5.46825E-7
    //  and 1.74638E-5, and the eye compares 5.4 with 1.7. Their logarithms
    //  read -6.26 and -4.76. Automatic is logarithmic for the R-factor.
    Axes := TChartAxes.Create;
    try
        AssertEquals('log10 R-factor ' + RFactorText(-2) + ' · 7 curves',
            HistoryRowHeadline(AnEntry('a', 0.01, 7),
            LossAxisChosen(Axes, AutomaticAxisModeId)));
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.TheRowsAreRewrittenOnTheScaleInForce;
var
    Axes: TChartAxes;
    T: THistoryRowTexts;
begin
    Record_('a', 0.01, 2);
    Record_('b', 0.001, 3);
    Axes := TChartAxes.Create;
    try
        T := HistoryRowTexts(FHistory, [1, 0], 2,
            LossAxisChosen(Axes, AutomaticAxisModeId));
        AssertEquals('a row each', 2, Length(T));
        AssertEquals('newest first, on that scale',
            'log10 R-factor ' + RFactorText(-3) + ' · 3 curves', T[0]);
        AssertEquals('log10 R-factor ' + RFactorText(-2) + ' · 2 curves', T[1]);
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.AListNotShowingThisHistoryIsLeftToTheRedraw;
begin
    Record_('a', 0.01, 2);
    Record_('b', 0.001, 3);
    AssertEquals('a row short', 0,
        Length(HistoryRowTexts(FHistory, [1, 0], 1, nil)));
    AssertEquals('an entry the order has not caught up with', 0,
        Length(HistoryRowTexts(FHistory, [0], 1, nil)));
end;

procedure THistoryRowsTest.OnALinearScaleTheRowGivesTheRFactor;
var
    Axes: TChartAxes;
begin
    Axes := TChartAxes.Create;
    try
        AssertEquals('R-factor ' + RFactorText(0.01) + ' · 7 curves',
            HistoryRowHeadline(AnEntry('a', 0.01, 7),
            LossAxisChosen(Axes, ValueAxisModeId)));
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.OnACustomScaleTheRowGivesTheUsersQuantity;
var
    Axes: TChartAxes;
    Definition: TAxisDefinition;
begin
    Definition.Name := 'log R';
    Definition.UnitName := '';
    Definition.Forward := 'log(x)';
    Definition.Inverse := '10^x';
    Axes := TChartAxes.Create;
    try
        Axes.SetDefinition(adLoss, Definition);
        AssertEquals('log R ' + RFactorText(-2) + ' · 7 curves',
            HistoryRowHeadline(AnEntry('a', 0.01, 7),
            LossAxisChosen(Axes, CustomAxisModeId)));
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.AFormulaWithNoValueHereFallsBackToTheRFactor;
var
    Axes: TChartAxes;
    Definition: TAxisDefinition;
begin
    //  A formula of the user's may have no value for this R-factor; the row
    //  still tells the model apart, by the R-factor as it is.
    Definition.Name := 'Odd';
    Definition.UnitName := '';
    Definition.Forward := 'x + y';
    Definition.Inverse := 'x - y';
    Axes := TChartAxes.Create;
    try
        Axes.SetDefinition(adLoss, Definition);
        AssertEquals('R-factor ' + RFactorText(0.01) + ' · 7 curves',
            HistoryRowHeadline(AnEntry('a', 0.01, 7),
            LossAxisChosen(Axes, CustomAxisModeId)));
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.AModelNobodyFittedReadsNotFittedOnEveryScale;
var
    Axes: TChartAxes;
begin
    //  -1 has no logarithm, and on any scale it would read as a number.
    Axes := TChartAxes.Create;
    try
        AssertEquals('Not fitted · 3 curves', HistoryRowHeadline(
            AnEntry('a', -1, 3), LossAxisChosen(Axes, LogarithmicAxisModeId)));
        AssertEquals('Not fitted · 3 curves', HistoryRowHeadline(
            AnEntry('a', -1, 3), LossAxisChosen(Axes, ValueAxisModeId)));
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.TheDetailsKeepTheRFactorItselfWhateverTheScale;
var
    Axes: TChartAxes;
    D: string;
begin
    //  THE ROW IS FOR TELLING MODELS APART, the details for the record: the
    //  R-factor itself is what a paper quotes, so it stays there on every
    //  scale - and the model it started from is named as its row names it.
    Record_('a', 0.1, 2);
    Record_('b', 0.01, 2);
    Axes := TChartAxes.Create;
    try
        D := HistoryDetails(FHistory, 1, 0,
            LossAxisChosen(Axes, LogarithmicAxisModeId));
        AssertTrue('the R-factor itself: ' + D,
            Pos('R-factor ' + RFactorText(0.01) + '.', D) > 0);
        AssertTrue('the parent as its row reads: ' + D,
            Pos('Fitted from the model log10 R-factor ' + RFactorText(-1),
            D) > 0);
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.TheDeleteQuestionNamesTheModelAsItsRowDoes;
var
    Axes: TChartAxes;
    Q: string;
begin
    //  The question names the model the way the user found it in the list.
    Record_('a', 0.01, 2);
    Axes := TChartAxes.Create;
    try
        Q := HistoryDeleteQuestion(FHistory, 0, 0,
            LossAxisChosen(Axes, LogarithmicAxisModeId));
        AssertTrue(Q, Pos('(log10 R-factor ' + RFactorText(-2) + ' · 2 curves)',
            Q) > 0);
    finally
        Axes.Free;
    end;
end;

procedure THistoryRowsTest.ABylineSaysWhenAndWhatMadeIt;
begin
    AssertEquals('12:33 · Minimize Difference',
        HistoryRowByline(AnEntry('a', 0.1, 1), 0,
            EncodeDateTime(2026, 10, 2, 18, 0, 0, 0)));
end;

procedure THistoryRowsTest.AStoppedRunSaysItWasStopped;
var
    E: THistoryEntry;
begin
    E := AnEntry('a', 0.1, 1);
    E.RunKind := hrkAutomatically;
    E.Stopped := True;
    AssertEquals('12:33 · Automatically, stopped',
        HistoryRowByline(E, 0, EncodeDateTime(2026, 10, 2, 18, 0, 0, 0)));
end;

procedure THistoryRowsTest.TheTimeIsLocal;
begin
    //  Stored as 12:33 UTC; three hours east reads 15:33.
    AssertEquals('15:33 · Minimize Difference',
        HistoryRowByline(AnEntry('a', 0.1, 1), 180,
            EncodeDateTime(2026, 10, 2, 18, 0, 0, 0)));
end;

procedure THistoryRowsTest.AnotherDayShowsItsDate;
begin
    AssertEquals('2026-10-02 12:33 · Minimize Difference',
        HistoryRowByline(AnEntry('a', 0.1, 1), 0,
            EncodeDateTime(2026, 10, 5, 9, 0, 0, 0)));
end;

procedure THistoryRowsTest.ATimeThisProgramDidNotWriteIsShownAsWritten;
var
    E: THistoryEntry;
    T: TDateTime;
begin
    E := AnEntry('a', 0.1, 1);
    E.CreatedUtc := 'yesterday';
    AssertFalse(TryLocalTimeOf('yesterday', 0, T));
    AssertEquals('yesterday · Minimize Difference',
        HistoryRowByline(E, 0, EncodeDateTime(2026, 10, 5, 9, 0, 0, 0)));
end;

procedure THistoryRowsTest.TheCurrentAndTheBestAreMarked;
begin
    AssertEquals('neither', '', HistoryRowMarks(False, False));
    AssertTrue('the current one', Pos('●', HistoryRowMarks(True, False)) > 0);
    AssertTrue('the best one', Pos('★', HistoryRowMarks(False, True)) > 0);
    AssertTrue('both', (Pos('●', HistoryRowMarks(True, True)) > 0) and
        (Pos('★', HistoryRowMarks(True, True)) > 0));
end;

procedure THistoryRowsTest.TheDetailsSayEverythingTheRowCannot;
var
    D: string;
begin
    Record_('a', 0.04213, 7);
    D := HistoryDetails(FHistory, 0, 0, nil);
    AssertTrue('the R-factor: ' + D, Pos('0.04213', D) > 0);
    AssertTrue('the curves: ' + D, Pos('7', D) > 0);
    AssertTrue('the run: ' + D, Pos('Minimize Difference', D) > 0);
    AssertTrue('the date: ' + D, Pos('2026-10-02', D) > 0);
    AssertTrue('that it is current: ' + D, Pos('current', D) > 0);
end;

function NameOfType(const ATypeId: string): string;
begin
    if ATypeId = '{11111111-2222-3333-4444-555555555555}' then
        Result := 'Gaussian'
    else
        Result := '';
end;

procedure THistoryRowsTest.TheDetailsNameTheCurveTypeWhenTheWindowCanName;
var
    E: THistoryEntry;
    P: TPointsData;
begin
    //  "7 curves" says how many, not of what: two models of the same count
    //  and different shapes are the comparison that matters most.
    E := AnEntry('a', 0.1, 7);
    E.Model.Settings.CurveTypeId := '{11111111-2222-3333-4444-555555555555}';
    P := Default(TPointsData);
    FHistory.Add(E, P);
    AssertTrue('named: ' + HistoryDetails(FHistory, 0, 0, nil, @NameOfType),
        Pos('Gaussian', HistoryDetails(FHistory, 0, 0, nil, @NameOfType)) > 0);
    AssertEquals('and nothing invented when there is no namer', 0,
        Pos('Gaussian', HistoryDetails(FHistory, 0, 0, nil)));
end;

procedure THistoryRowsTest.TheDetailsNameTheModelItStartedFrom;
var
    D: string;
begin
    Record_('a', 0.1, 2);
    Record_('b', 0.05, 2);
    D := HistoryDetails(FHistory, 1, 0, nil);
    AssertTrue('started from a''s model: ' + D, Pos('R-factor 0.1000', D) > 0);
    D := HistoryDetails(FHistory, 0, 0, nil);
    AssertTrue('a started from nothing recorded: ' + D,
        Pos('first', LowerCase(D)) > 0);
end;

procedure THistoryRowsTest.TheStatisticsAreShownOnlyWhenThereAreAny;
var
    E: THistoryEntry;
    P: TPointsData;
begin
    E := AnEntry('a', 0.1, 2);
    E.Model.Statistics.Valid := True;
    E.Model.Statistics.RSquared := 0.9876;
    P := Default(TPointsData);
    FHistory.Add(E, P);
    AssertTrue('R²', Pos('0.9876', HistoryDetails(FHistory, 0, 0, nil)) > 0);
    Record_('b', -1, 2);
    AssertEquals('none for a model nobody fitted', 0,
        Pos('R²', HistoryDetails(FHistory, 1, 0, nil)));
end;

procedure THistoryRowsTest.TheSamplesNoCurveCoveredAreExplainedBesideTheFigures;
var
    E: THistoryEntry;
    P: TPointsData;
    D: string;
begin
    //  WHERE THE R-FACTOR IS READ, so that is where its cause is said: a model
    //  whose first curve starts one sample after the data scored 70 times
    //  worse over the whole profile, and the entry said nothing about why.
    E := AnEntry('a', 0.1, 2);
    E.Model.Statistics.Valid := True;
    E.Model.Statistics.UncoveredRanges :=
        UncoveredRanges([0, 1, 2], [False, True, True]);
    E.Model.Statistics.UncoveredResidualShare := 0.985;
    P := Default(TPointsData);
    FHistory.Add(E, P);
    D := HistoryDetails(FHistory, 0, 0, nil);
    AssertTrue('named: ' + D, Pos('covered by no curve', D) > 0);
    AssertTrue('after the figures it explains: ' + D,
        Pos('covered by no curve', D) > Pos('R²', D));
    Record_('b', 0.1, 2);
    AssertEquals('and only where there are any', 0,
        Pos('covered by no curve', HistoryDetails(FHistory, 1, 0, nil)));
end;

procedure THistoryRowsTest.WhetherTheDifferenceWasNoiseIsKeptWithTheFigures;
var
    E: THistoryEntry;
    P: TPointsData;
    D: string;
begin
    E := AnEntry('a', 0.1, 2);
    E.Model.Statistics.Valid := True;
    E.Model.Statistics.Residual.Tested := True;
    E.Model.Statistics.Residual.P := 0.31;
    P := Default(TPointsData);
    FHistory.Add(E, P);
    D := HistoryDetails(FHistory, 0, 0, nil);
    AssertTrue('said: ' + D, Pos('Difference: random (p = 0.31).', D) > 0);
    AssertTrue('after the figures: ' + D,
        Pos('Difference:', D) > Pos('R²', D));
    Record_('b', 0.1, 2);
    AssertEquals('and only for a model judged', 0,
        Pos('Difference:', HistoryDetails(FHistory, 1, 0, nil)));
end;

procedure THistoryRowsTest.TheBestIsSaidToBeTheBestAndAmongWhat;
var
    D: string;
begin
    //  "Best" without saying among what reads as a verdict on the model; it is
    //  a comparison with the others fitted to the same data the same way.
    Record_('a', 0.1, 2);
    Record_('b', 0.05, 2);
    D := HistoryDetails(FHistory, 1, 0, nil);
    AssertTrue('the best: ' + D, Pos('lowest', LowerCase(D)) > 0);
    AssertTrue('among what: ' + D, Pos('same data', LowerCase(D)) > 0);
end;

procedure THistoryRowsTest.AMissingProfileIsExplained;
var
    Entries: THistoryEntries;
begin
    Record_('a', 0.1, 2);
    Entries := EntriesOf(FHistory);
    FHistory.Load(Entries, nil, '', '');
    AssertTrue(Pos('profile', HistoryDetails(FHistory, 0, 0, nil)) > 0);
end;

procedure THistoryRowsTest.MakeCurrentSaysWhyItIsOrIsNotOffered;
var
    Entries: THistoryEntries;
begin
    Record_('a', 0.1, 2);
    Record_('b', 0.05, 2);
    AssertTrue('offered', Pos('Make', HistoryMakeCurrentHint(FHistory, 0)) > 0);
    AssertTrue('already current',
        Pos('already', HistoryMakeCurrentHint(FHistory, 1)) > 0);
    AssertTrue('nothing selected',
        Pos('Select', HistoryMakeCurrentHint(FHistory, -1)) > 0);
    Entries := EntriesOf(FHistory);
    FHistory.Load(Entries, nil, '', '');
    AssertTrue('no profile',
        Pos('profile', HistoryMakeCurrentHint(FHistory, 0)) > 0);
end;

procedure THistoryRowsTest.DeletingNamesTheModelAndWhatHappensToItsChildren;
var
    Q: string;
begin
    Record_('a', 0.1, 2);
    Record_('b', 0.05, 2);
    Q := HistoryDeleteQuestion(FHistory, 0, 0, nil);
    AssertTrue('names it: ' + Q, Pos('R-factor 0.1000', Q) > 0);
    AssertTrue('says what becomes of the model fitted from it: ' + Q,
        Pos('fitted from it', Q) > 0);
    Q := HistoryDeleteQuestion(FHistory, 1, 0, nil);
    AssertEquals('nothing was fitted from b: ' + Q, 0,
        Pos('fitted from it', Q));
end;

procedure THistoryRowsTest.DeletingSaysTheLiveModelIsNotTouched;
begin
    Record_('a', 0.1, 2);
    AssertTrue(Pos('not changed', HistoryDeleteQuestion(FHistory, 0, 0, nil)) > 0);
end;

initialization
    //  A unit test: strings out of records.
    RegisterTest('unit', THistoryRowsTest);
end.
