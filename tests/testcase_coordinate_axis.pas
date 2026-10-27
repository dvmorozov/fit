// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_coordinate_axis;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Types, Math, DateUtils, fpcunit, testregistry, coordinate_axis,
  //  A number a module's mode reads, as the test stand-ins describe one.
  test_axis_modes, axis_mode_registry;
type
  TCoordinateAxisTest = class(TTestCase)
  published
    procedure Identity;

    //  WHAT EVERY AXIS SAYS ABOUT ITSELF. The chart's title and the readout
    //  beside it are read off the axis in force, so both follow the mode rather
    //  than a caption written into the form.
    procedure ATitleIsTheNameFollowedByTheUnitInBrackets;
    procedure AnAxisWithNoUnitIsTitledByItsNameAlone;
    procedure TheReadoutIsCaptionedByTheName;
    procedure APlainAxisReadsOutTwoDecimals;

    //  A LOGARITHMIC AXIS draws the logarithm and reads in the quantity: the
    //  marks and the readout are the prices or counts themselves, as on any
    //  log chart, and only the spacing is logarithmic.
    procedure ALogAxisDrawsTheDecimalLogarithm;
    procedure ALogAxisReadsBackThePowerOfTen;
    procedure ALogAxisKeepsTheQuantitysNameAndSaysItIsLog;
    procedure WhatALogAxisDrawsIsCalledTheLogarithm;
    procedure WhatAnAxisOfTheQuantityDrawsIsTheQuantity;
    procedure ZeroHasNoLogarithmAndIsLeftUndrawn;
    procedure ANegativeValueIsLeftUndrawnToo;
    procedure ALogAxisMarksAndReadsInTheQuantity;
    procedure ALogAxisLeavesTheSpacingToTheChart;
    //  WHERE A LOG AXIS PUTS ITS MARKS. Even steps in the logarithm put the
    //  R-factor's grid at quarter decades - 1E-6, 1.778E-6, 3.162E-6 - and a
    //  decade took four lines to cross (reported 2026-10-25). A log axis is
    //  marked at whole decades and, where they are few, their twos and fives.
    procedure ALogAxisIsMarkedAtDecadesAndTheirTwosAndFives;
    procedure AWideLogAxisIsMarkedAtEveryFewDecades;
    procedure ALogAxisAcrossOneDecadeIsMarkedAtEveryDigit;
    procedure ALogAxisTooNarrowForRoundMarksLeavesThemToTheChart;
    procedure AnAxisOfEvenStepsListsNoMarksOfItsOwn;
    procedure ATinyOrHugeMarkIsWrittenWithAnExponent;

    //  A DATE AXIS holds a calendar date as the day count it is stored as, and
    //  writes every mark and reading as a date.
    procedure ADateIsShownAsItIsStored;
    procedure ADateMarkIsWrittenAsADate;
    procedure ADateReadoutIsWrittenAsADate;
    procedure DateMarksFallOnWholeDays;
    procedure ALongSeriesIsMarkedInWeeksOrMore;

    //  BARS LABELLED BY THEIR DATES: the argument stays the bar number, so the
    //  bars stay evenly spaced - what a count is read in - and each mark and
    //  reading names the day that bar is.
    procedure ABarIsShownAtItsNumberAndNamedByItsDate;
    procedure AMarkBetweenBarsNamesTheNearestBar;
    procedure AMarkBeyondTheSeriesNamesNothing;
    procedure BarMarksFallOnWholeBars;
    procedure ExpressionAxisLinear;
    procedure ExpressionAxisLogarithmic;
    procedure AFormulaThatCannotBeEvaluatedIsRefusedInWords;

    //  WHAT A TYPED NUMBER A MODE READS HAS TO BE. A mode may divide by it
    //  (the wavelength of a module's sin(theta)/lambda axis does), and both
    //  refusals used to live in an LCL menu handler with a MessageDlg each,
    //  reachable only by typing into the box.
    procedure APlainNumberIsAccepted;
    procedure ANonNumberIsRefusedWithTheSeparatorNamed;
    procedure ZeroIsRefusedBecauseItAlreadyMeansNotSet;
    procedure ANegativeNumberIsRefused;
    procedure ARefusedValueComesBackAsZeroRatherThanHalfParsed;
    procedure AnAcceptedValueCarriesNoRefusal;
  //  What a tick of an axis reads.
  procedure AnAxisThatWritesItsOwnMarksWritesThem;
  procedure AnyOtherTickTakesTheChartsFormat;
  end;
implementation

procedure TCoordinateAxisTest.Identity;
var ax: TNamedAxis;
begin
  ax := TNamedAxis.Create('Position', '');
  try
    AssertEquals('to-display identity', 17.3, ax.ToDisplay(17.3), 1e-12);
    AssertEquals('from-display identity', 17.3, ax.FromDisplay(17.3), 1e-12);
    AssertEquals('name', 'Position', ax.DisplayName);
    AssertEquals('unit', '', ax.UnitName);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ATitleIsTheNameFollowedByTheUnitInBrackets;
var ax: TNamedAxis;
begin
  ax := TNamedAxis.Create('Intensity', 'counts');
  try
    AssertEquals('Intensity [counts]', ax.Title);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AnAxisWithNoUnitIsTitledByItsNameAlone;
var ax: TNamedAxis;
begin
  ax := TNamedAxis.Create('Price', '');
  try
    AssertEquals('no empty brackets', 'Price', ax.Title);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.TheReadoutIsCaptionedByTheName;
var ax: TNamedAxis;
begin
  //  The caption beside the pointer's coordinate - "Intensity:" was written
  //  into the form, and read as nonsense over a price.
  ax := TNamedAxis.Create('Price', 'USD');
  try
    AssertEquals('Price:', ax.ReadoutCaption);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.APlainAxisReadsOutTwoDecimals;
var ax: TNamedAxis;
begin
  ax := TNamedAxis.Create('Position', '');
  try
    AssertEquals('what the readout always showed', '  3.14',
      ax.ReadoutText(3.14159));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisDrawsTheDecimalLogarithm;
var ax: TLogarithmicAxis;
begin
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertEquals('log10(1000)', 3.0, ax.ToDisplay(1000), 1e-12);
    AssertEquals('log10(1)', 0.0, ax.ToDisplay(1), 1e-12);
    AssertEquals('log10(0.01)', -2.0, ax.ToDisplay(0.01), 1e-12);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisReadsBackThePowerOfTen;
var ax: TLogarithmicAxis;
begin
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertEquals('10^2', 100.0, ax.FromDisplay(2), 1e-9);
    AssertEquals('round trip', 1234.5, ax.FromDisplay(ax.ToDisplay(1234.5)), 1e-9);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisKeepsTheQuantitysNameAndSaysItIsLog;
var ax: TLogarithmicAxis;
begin
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertEquals('the quantity is still a price', 'Price', ax.DisplayName);
    AssertEquals('in its own unit', 'USD', ax.UnitName);
    AssertEquals('and the title says the scale', 'Price [USD], log scale',
      ax.Title);
    AssertEquals('the readout is a price', 'Price:', ax.ReadoutCaption);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.WhatALogAxisDrawsIsCalledTheLogarithm;
var ax: TLogarithmicAxis;
begin
  //  Read in the quantity, but what it DRAWS is the logarithm: what a number
  //  printed from ToDisplay is called.
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertEquals('log10 Price', ax.DrawnQuantityName);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.WhatAnAxisOfTheQuantityDrawsIsTheQuantity;
var ax: TNamedAxis;
begin
  ax := TNamedAxis.Create('Price', 'USD');
  try
    AssertEquals('Price', ax.DrawnQuantityName);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ZeroHasNoLogarithmAndIsLeftUndrawn;
var ax: TLogarithmicAxis;
begin
  //  NaN, which the chart leaves as a gap. Anything else - a floor, a clamp,
  //  an exception - either draws a point that is not there or stops the plot.
  ax := TLogarithmicAxis.Create('Intensity', '');
  try
    AssertTrue('no logarithm of zero', IsNan(ax.ToDisplay(0)));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ANegativeValueIsLeftUndrawnToo;
var ax: TLogarithmicAxis;
begin
  ax := TLogarithmicAxis.Create('Difference', '');
  try
    AssertTrue('nor of a negative number', IsNan(ax.ToDisplay(-5)));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisMarksAndReadsInTheQuantity;
var ax: TLogarithmicAxis;
begin
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertTrue('the marks are the axis''s to write', ax.FormatsMarks);
    AssertEquals('a mark at log 3 reads 1000', '1000', ax.MarkText(3, 0.5));
    AssertEquals('one at log 2.5 reads its price', '316.2', ax.MarkText(2.5, 0.5));
    AssertEquals('the pointer at log 2 reads 100', '100.00', ax.ReadoutText(2));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisLeavesTheSpacingToTheChart;
var ax: TLogarithmicAxis; Start, Step: double;
begin
  //  A log axis never asks for even steps of its own: where it places its
  //  marks it lists them (MarkValues), and where it does not the chart's
  //  even steps in the logarithm are the logarithmic spacing.
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertFalse(ax.ChooseMarks(1, 4, Start, Step));
  finally ax.Free; end;
end;

{ The marks listed, as AAxis writes them on the chart. }
function ListedMarks(AAxis: TCoordinateAxis; const AValues: TDoubleDynArray): string;
var i: integer;
begin
  Result := '';
  for i := 0 to High(AValues) do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + AAxis.MarkText(AValues[i], 0);
  end;
end;

procedure TCoordinateAxisTest.ALogAxisIsMarkedAtDecadesAndTheirTwosAndFives;
var ax: TLogarithmicAxis; Marks: TDoubleDynArray;
begin
  //  The range of the report: an R-factor from 4.2E-5 down to 7E-7.
  ax := TLogarithmicAxis.Create('R-factor', '');
  try
    AssertTrue('the axis lists its own marks',
      ax.MarkValues(Log10(7e-7), Log10(4.2e-5), 10, Marks));
    AssertEquals('1E-6 2E-6 5E-6 1E-5 2E-5', ListedMarks(ax, Marks));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AWideLogAxisIsMarkedAtEveryFewDecades;
var ax: TLogarithmicAxis; Marks: TDoubleDynArray;
begin
  //  Thirteen decades are more than ten marks: every second one, counted from
  //  1 rather than from the bottom of the window, so the grid does not jump
  //  as the window moves.
  ax := TLogarithmicAxis.Create('R-factor', '');
  try
    AssertTrue(ax.MarkValues(-12.5, 0.5, 10, Marks));
    AssertEquals('1E-12 1E-10 1E-8 1E-6 1E-4 0.01 1', ListedMarks(ax, Marks));
    AssertTrue(ax.MarkValues(-3.5, 2.5, 10, Marks));
    AssertEquals('six decades fit as they are',
      '0.001 0.01 0.1 1 10 100', ListedMarks(ax, Marks));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisAcrossOneDecadeIsMarkedAtEveryDigit;
var ax: TLogarithmicAxis; Marks: TDoubleDynArray;
begin
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertTrue(ax.MarkValues(Log10(95), Log10(1050), 10, Marks));
    AssertEquals('100 200 300 400 500 600 700 800 900 1000', ListedMarks(ax, Marks));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALogAxisTooNarrowForRoundMarksLeavesThemToTheChart;
var ax: TLogarithmicAxis; Marks: TDoubleDynArray;
begin
  //  A price from 120 to 250 has two round marks at most; the chart's own
  //  even steps, written to four figures, say more.
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertFalse(ax.MarkValues(Log10(120), Log10(250), 10, Marks));
    AssertEquals('and lists none', 0, Length(Marks));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AnAxisOfEvenStepsListsNoMarksOfItsOwn;
var ax: TNamedAxis; Marks: TDoubleDynArray;
begin
  ax := TNamedAxis.Create('Position', '');
  try
    AssertFalse(ax.MarkValues(0, 100, 10, Marks));
    AssertEquals(0, Length(Marks));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ATinyOrHugeMarkIsWrittenWithAnExponent;
var ax: TLogarithmicAxis;
begin
  //  0.00001 beside 5E-6 read as two different notations on one axis.
  ax := TLogarithmicAxis.Create('R-factor', '');
  try
    AssertEquals('1E-5', ax.MarkText(-5, 0));
    AssertEquals('2E-6', ax.MarkText(Log10(2e-6), 0));
    AssertEquals('3.162E-5', ax.MarkText(Log10(3.162e-5), 0));
    AssertEquals('1E6', ax.MarkText(6, 0));
    AssertEquals('within reach of a plain number it is one', '0.001',
      ax.MarkText(-3, 0));
    AssertEquals('9999 is the last with four figures', '1E4', ax.MarkText(4, 0));
    AssertEquals('1000', ax.MarkText(3, 0));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ADateIsShownAsItIsStored;
var ax: TDateAxis; Day: double;
begin
  ax := TDateAxis.Create;
  try
    Day := EncodeDate(2024, 3, 15);
    AssertEquals('stored as the day count', Day, ax.ToDisplay(Day), 0);
    AssertEquals('and read back as it', Day, ax.FromDisplay(Day), 0);
    AssertEquals('Date', ax.DisplayName);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ADateMarkIsWrittenAsADate;
var ax: TDateAxis;
begin
  ax := TDateAxis.Create;
  try
    AssertTrue(ax.FormatsMarks);
    AssertEquals('2024-03-15', ax.MarkText(EncodeDate(2024, 3, 15), 7));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ADateReadoutIsWrittenAsADate;
var ax: TDateAxis;
begin
  ax := TDateAxis.Create;
  try
    //  The pointer between two closes still names the day it is over.
    AssertEquals('2024-03-15',
      ax.ReadoutText(EncodeDate(2024, 3, 15) + 0.4));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.DateMarksFallOnWholeDays;
var ax: TDateAxis; Start, Step: double;
begin
  ax := TDateAxis.Create;
  try
    AssertTrue('the date axis chooses its own marks',
      ax.ChooseMarks(45000.3, 45010.7, Start, Step));
    AssertEquals('a whole number of days apart', Step, Round(Step), 0);
    AssertEquals('starting on a whole day', Start, Round(Start), 0);
    AssertTrue('at or after the start', Start >= 45000.3);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ALongSeriesIsMarkedInWeeksOrMore;
var ax: TDateAxis; Start, Step: double;
begin
  ax := TDateAxis.Create;
  try
    //  Ten years of daily closes would otherwise be marked every day.
    AssertTrue(ax.ChooseMarks(40000, 43650, Start, Step));
    AssertTrue('no more than about eight marks', (43650 - 40000) / Step <= 8);
    AssertTrue('in steps of at least a week', Step >= 7);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ExpressionAxisLinear;
var ax: TExpressionAxis;
begin
  //  display = 2*x + 1 ; inverse = (x - 1)/2
  ax := TExpressionAxis.Create('Scaled', 'u', '2*x+1', '(x-1)/2');
  try
    AssertEquals('name', 'Scaled', ax.DisplayName);
    AssertEquals('unit', 'u', ax.UnitName);
    AssertEquals('to-display 2*3+1', 7.0, ax.ToDisplay(3.0), 1e-9);
    AssertEquals('inverse of 7', 3.0, ax.FromDisplay(7.0), 1e-9);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.ExpressionAxisLogarithmic;
var ax: TExpressionAxis; d: double;
begin
  //  display = ln(x) ; inverse = exp(x)
  ax := TExpressionAxis.Create('Log', '', 'ln(x)', 'exp(x)');
  try
    d := ax.ToDisplay(10.0);
    AssertEquals('ln(10)', 2.302585, d, 1e-6);
    AssertEquals('round-trip via exp', 10.0, ax.FromDisplay(d), 1e-9);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AFormulaThatCannotBeEvaluatedIsRefusedInWords;
var ax: TExpressionAxis; Raised: boolean;
begin
  ax := TExpressionAxis.Create('Broken', '', 'x+', 'x');
  Raised := False;
  try
    try
      ax.ToDisplay(1);
    except
      on E: Exception do
      begin
        Raised := True;
        AssertTrue('names the formula', Pos('"x+"', E.Message) > 0);
        AssertTrue('and says how to write one', Pos('Use x', E.Message) > 0);
      end;
    end;
    AssertTrue(Raised);
  finally ax.Free; end;
end;

function ThreeBars: TAxisDates;
begin
  SetLength(Result, 3);
  Result[0] := EncodeDate(2020, 9, 24);
  Result[1] := EncodeDate(2020, 9, 25);
  Result[2] := EncodeDate(2020, 9, 28);    //  over a weekend: still one bar on
end;

procedure TCoordinateAxisTest.ABarIsShownAtItsNumberAndNamedByItsDate;
var ax: TBarDateAxis;
begin
  ax := TBarDateAxis.Create(ThreeBars);
  try
    AssertEquals('Date', ax.DisplayName);
    AssertEquals('drawn at its number', 2.0, ax.ToDisplay(2), 0);
    AssertEquals('read back as it', 2.0, ax.FromDisplay(2), 0);
    AssertTrue(ax.FormatsMarks);
    AssertEquals('named by its date', '2020-09-28', ax.MarkText(2, 1));
    AssertEquals('and read out the same way', '2020-09-25', ax.ReadoutText(1));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AMarkBetweenBarsNamesTheNearestBar;
var ax: TBarDateAxis;
begin
  ax := TBarDateAxis.Create(ThreeBars);
  try
    AssertEquals('2020-09-25', ax.ReadoutText(1.4));
    AssertEquals('2020-09-28', ax.ReadoutText(1.6));
    //  And a position that is no number names no day at all.
    AssertEquals('', ax.ReadoutText(NaN));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.AMarkBeyondTheSeriesNamesNothing;
var ax: TBarDateAxis;
begin
  //  No bar is there, so there is no day to name - not the day of the last.
  ax := TBarDateAxis.Create(ThreeBars);
  try
    AssertEquals('', ax.MarkText(5, 1));
    AssertEquals('', ax.MarkText(-1, 1));
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.BarMarksFallOnWholeBars;
var ax: TBarDateAxis; Start, Step: double;
begin
  ax := TBarDateAxis.Create(ThreeBars);
  try
    AssertTrue(ax.ChooseMarks(0.3, 1500, Start, Step));
    AssertEquals('a whole number of bars apart', Step, Round(Step), 0);
    AssertEquals('starting on a bar', Start, Round(Start), 0);
    AssertTrue('no more than about eight marks', 1500 / Step <= 8);
  finally ax.Free; end;
end;

{ ---- what a typed number a mode reads has to be ----------------------------- }

procedure TCoordinateAxisTest.APlainNumberIsAccepted;
var V: double; R: string;
begin
    AssertTrue('accepted', AxisValueFromText(TestScaleParameterInfo, '1.5406', V, R));
    AssertEquals('the value', 1.5406, V, 1e-12);
end;

procedure TCoordinateAxisTest.ANonNumberIsRefusedWithTheSeparatorNamed;
var V: double; R: string;
begin
    //  A comma is what a great many keyboards and locales produce, so the
    //  refusal is a mystery unless it says which separator is wanted.
    AssertFalse('refused', AxisValueFromText(TestScaleParameterInfo, '1,5406', V, R));
    AssertTrue('the separator is named', Pos('full stop', R) > 0);
    AssertTrue('the module''s example is given', Pos('2.5', R) > 0);
end;

procedure TCoordinateAxisTest.ZeroIsRefusedBecauseItAlreadyMeansNotSet;
var V: double; R: string;
begin
    //  THE ONE THAT WOULD FAIL SILENTLY. Zero is what the client already reads
    //  as "none known", so accepting it would do nothing at all - and the
    //  axis it is for may divide by it.
    AssertFalse('refused', AxisValueFromText(TestScaleParameterInfo, '0', V, R));
    AssertTrue('and says why', Pos('greater than zero', R) > 0);
end;

procedure TCoordinateAxisTest.ANegativeNumberIsRefused;
var V: double; R: string;
begin
    AssertFalse('refused', AxisValueFromText(TestScaleParameterInfo, '-1.5', V, R));
end;

procedure TCoordinateAxisTest.ARefusedValueComesBackAsZeroRatherThanHalfParsed;
var V: double; R: string;
begin
    //  The caller shows the message and returns; if it ever did read the value,
    //  it must not be a partially converted one.
    V := 99;
    AssertFalse('refused', AxisValueFromText(TestScaleParameterInfo, 'rubbish', V, R));
    AssertEquals('cleared', 0, V, 1e-12);
end;

procedure TCoordinateAxisTest.AnAcceptedValueCarriesNoRefusal;
var V: double; R: string;
begin
    //  So that a caller which shows R unconditionally shows nothing.
    R := 'left over';
    AssertTrue('accepted', AxisValueFromText(TestScaleParameterInfo, '2', V, R));
    AssertEquals('no message', '', R);
end;

procedure TCoordinateAxisTest.AnAxisThatWritesItsOwnMarksWritesThem;
var
  ax: TCoordinateAxis;
begin
  ax := TLogarithmicAxis.Create('Intensity', '');
  try
    AssertTrue(ax.FormatsMarks);
    AssertEquals(ax.MarkText(100, 1), AxisMarkText(ax, 100, 1));
  finally
    ax.Free;
  end;
end;

procedure TCoordinateAxisTest.AnyOtherTickTakesTheChartsFormat;
var
  ax: TCoordinateAxis;
begin
  ax := TNamedAxis.Create('Position', '');
  try
    AssertFalse(ax.FormatsMarks);
    AssertEquals(Trim(Format('%6.4g', [1234.5])), AxisMarkText(ax, 1234.5, 1));
  finally
    ax.Free;
  end;
  AssertEquals('and with no axis at all', Trim(Format('%6.4g', [0.5])),
    AxisMarkText(nil, 0.5, 1));
end;

initialization
  RegisterTest('unit', TCoordinateAxisTest);
end.
