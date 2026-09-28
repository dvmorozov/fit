// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The numbers along the bottom of the window.)

DURING A LONG FIT THE STATUS BAR IS THE ONLY THING THAT CHANGES. It carries the
elapsed time, the goodness of fit, a hint and the engine's advice - and what it
says was three `Format` calls in three different handlers of a form, with the
arithmetic that divides the bar between them in a fourth.

The failure that matters most is the quiet one: statistics shown for a model
nobody fitted. Zeros in those two panels read as a fit that went very badly
indeed, which is a worse answer than no answer.
}
unit testcase_status_readout;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, status_readout, fit_statistics;

type
    TStatusReadoutTest = class(TTestCase)
    private
        function Stats(AValid: boolean; AChi, ARSquared: double): TFitStatistics;
    published
        //  The goodness of fit.
        procedure AValidFitIsSummarised;
        procedure NoFitShowsNothingRatherThanZero;
        procedure BothFiguresAppear;
        procedure AVerySmallChiSquaredIsStillLegible;
        procedure AVeryLargeChiSquaredIsStillLegible;

        //  The elapsed time.
        procedure TheElapsedTimeIsLabelled;
        procedure AnEmptyTimeStillReadsAsALabel;

        //  The pointer readout.
        procedure ACoordinateHasTwoDecimals;
        procedure CoordinatesAreTheSameWidth;
        procedure ANegativeCoordinateIsShown;

        //  The samples the measured panels are sized to.
        procedure TheStatsSampleIsAsWideAsARealSummary;
        procedure TheElapsedSampleIsAsWideAsARealTime;

        //  How the rest of the bar is divided.
        procedure TheProsePanelsShareWhatIsLeft;
        procedure TheOddPixelGoesToTheAdvice;
        procedure WithNoAdvicePanelTheHintTakesItAll;
        procedure ABarTooNarrowGivesNothingRatherThanANegativeWidth;
        procedure NothingIsEverNegative;
        procedure TheTwoWidthsNeverExceedWhatIsLeft;
    end;

implementation

function TStatusReadoutTest.Stats(AValid: boolean;
    AChi, ARSquared: double): TFitStatistics;
begin
    Result := Default(TFitStatistics);
    Result.Valid := AValid;
    Result.ReducedChiSquare := AChi;
    Result.RSquared := ARSquared;
end;

{ ---- the goodness of fit --------------------------------------------------- }

procedure TStatusReadoutTest.AValidFitIsSummarised;
begin
    AssertTrue('something to read',
        Trim(StatisticsSummary(Stats(True, 1.25, 0.9987))) <> '');
end;

procedure TStatusReadoutTest.NoFitShowsNothingRatherThanZero;
begin
    //  THE ONE THAT MATTERS. Zeros in these panels read as a fit that went very
    //  badly, which is a worse answer than no answer - and a model that has not
    //  been fitted is the ordinary state of a freshly opened file.
    AssertEquals('nothing', '', StatisticsSummary(Stats(False, 0, 0)));
end;

procedure TStatusReadoutTest.BothFiguresAppear;
begin
    //  Two numbers that answer different questions: how far off the model is,
    //  and how much of the variation it explains. One without the other is half
    //  an answer.
    AssertTrue('the chi-squared',
        Pos('Chi2', StatisticsSummary(Stats(True, 1.25, 0.9987))) > 0);
    AssertTrue('and the R-squared',
        Pos('R2', StatisticsSummary(Stats(True, 1.25, 0.9987))) > 0);
end;

procedure TStatusReadoutTest.AVerySmallChiSquaredIsStillLegible;
var
    S: string;
begin
    //  ITS MAGNITUDE VARIES BY ORDERS between models, which is why the format is
    //  %.4g and not a fixed one - a fixed format shows a good fit as 0.0000.
    S := StatisticsSummary(Stats(True, 0.0000001234, 0.999));
    AssertTrue('not rounded to nothing: ' + S, Pos('0.0000 ', S) = 0);
end;

procedure TStatusReadoutTest.AVeryLargeChiSquaredIsStillLegible;
var
    S: string;
begin
    //  And a hopeless fit must not fill the panel with digits and push the
    //  R-squared out of it.
    S := StatisticsSummary(Stats(True, 1.2E12, 0.01));
    AssertTrue('the R-squared survives: ' + S, Pos('R2', S) > 0);
    AssertTrue('and it is not written out in full: ' + S, Length(S) < 60);
end;

{ ---- the elapsed time ------------------------------------------------------ }

procedure TStatusReadoutTest.TheElapsedTimeIsLabelled;
begin
    //  A bare duration in a bar of four panels is a number with no question.
    AssertEquals('labelled', 'Elapsed time: 00:01:23',
        ElapsedTimeText('00:01:23'));
end;

procedure TStatusReadoutTest.AnEmptyTimeStillReadsAsALabel;
begin
    //  Before anything has run. The label with nothing after it is honest; a
    //  blank panel is indistinguishable from a broken one.
    AssertEquals('the label alone', 'Elapsed time: ', ElapsedTimeText(''));
end;

{ ---- the pointer readout --------------------------------------------------- }

procedure TStatusReadoutTest.ACoordinateHasTwoDecimals;
begin
    AssertEquals('two', '  1.50', CoordinateReadout(1.5));
end;

procedure TStatusReadoutTest.CoordinatesAreTheSameWidth;
begin
    //  FIXED WIDTH, so the two readouts do not jitter sideways as the pointer
    //  moves - which is what makes a number beside a moving crosshair readable
    //  at all.
    AssertEquals('a small one and a large one',
        Length(CoordinateReadout(1.5)), Length(CoordinateReadout(12.75)));
end;

procedure TStatusReadoutTest.ANegativeCoordinateIsShown;
begin
    //  A chart can be scrolled past zero, and a readout that dropped the sign
    //  would put the pointer somewhere it is not.
    AssertTrue('the sign is there',
        Pos('-', CoordinateReadout(-3.25)) > 0);
end;

{ ---- the samples ----------------------------------------------------------- }

procedure TStatusReadoutTest.TheStatsSampleIsAsWideAsARealSummary;
begin
    //  THE SAMPLE AND THE FORMAT ARE ONE FACT. The panel is sized by measuring
    //  the sample, so a format string widened without its sample gives a panel
    //  too narrow for its own text - and the text is then clipped, silently.
    AssertTrue('the sample covers an ordinary summary: ' + StatsSample,
        Length(StatsSample) >=
        Length(StatisticsSummary(Stats(True, 1.2345, 0.99987))));
end;

procedure TStatusReadoutTest.TheElapsedSampleIsAsWideAsARealTime;
begin
    AssertTrue('the sample covers an ordinary time',
        Length(ElapsedSample) >= Length(ElapsedTimeText('00:01:23.45')));
end;

{ ---- how the rest of the bar is divided ------------------------------------ }

procedure TStatusReadoutTest.TheProsePanelsShareWhatIsLeft;
var
    Hint, Advice: longint;
begin
    ProsePanelWidths(500, 100, 200, True, Hint, Advice);
    AssertEquals('half of the remaining 200', 100, Hint);
    AssertEquals('and the other half', 100, Advice);
end;

procedure TStatusReadoutTest.TheOddPixelGoesToTheAdvice;
var
    Hint, Advice: longint;
begin
    //  It holds the longer text of the two. Losing a character of advice is
    //  worse than losing one of a hint that repeats what the pointer is over.
    ProsePanelWidths(501, 100, 200, True, Hint, Advice);
    AssertEquals('the hint gets the floor', 100, Hint);
    AssertEquals('the advice the rest', 101, Advice);
end;

procedure TStatusReadoutTest.WithNoAdvicePanelTheHintTakesItAll;
var
    Hint, Advice: longint;
begin
    //  A build with no engine advice has three panels, not four, and halving
    //  the remainder would leave a quarter of the bar drawn as nothing.
    ProsePanelWidths(500, 100, 200, False, Hint, Advice);
    AssertEquals('all of it', 200, Hint);
    AssertEquals('and none for a panel that is not there', 0, Advice);
end;

procedure TStatusReadoutTest.ABarTooNarrowGivesNothingRatherThanANegativeWidth;
var
    Hint, Advice: longint;
begin
    //  A window dragged narrower than its own measured panels. A negative width
    //  is clamped silently by one widget set and drawn as an empty bar by
    //  another, so neither reports it.
    ProsePanelWidths(50, 100, 200, True, Hint, Advice);
    AssertEquals('nothing', 0, Hint);
    AssertEquals('and nothing', 0, Advice);
end;

procedure TStatusReadoutTest.NothingIsEverNegative;
var
    Hint, Advice, W: longint;
begin
    //  A sweep across every width a window can be dragged to.
    for W := 0 to 400 do
    begin
        ProsePanelWidths(W, 100, 200, True, Hint, Advice);
        AssertTrue(Format('at %d the hint is not negative', [W]), Hint >= 0);
        AssertTrue(Format('at %d the advice is not negative', [W]),
            Advice >= 0);
    end;
end;

procedure TStatusReadoutTest.TheTwoWidthsNeverExceedWhatIsLeft;
var
    Hint, Advice, W: longint;
begin
    //  Together they must fit: handing out more than the bar has pushes a panel
    //  off the end, and the one that goes is the last, which is the advice.
    for W := 300 to 900 do
    begin
        ProsePanelWidths(W, 100, 200, True, Hint, Advice);
        AssertEquals(Format('at %d they fill exactly what is left', [W]),
            W - 300, Hint + Advice);
    end;
end;

initialization
    //  A unit test: numbers and strings. No status bar.
    RegisterTest('unit', TStatusReadoutTest);
end.
