// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_coordinate_axis;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, DateUtils, fpcunit, testregistry, coordinate_axis;
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
    procedure ZeroHasNoLogarithmAndIsLeftUndrawn;
    procedure ANegativeValueIsLeftUndrawnToo;
    procedure ALogAxisMarksAndReadsInTheQuantity;
    procedure ALogAxisLeavesTheSpacingToTheChart;

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
    procedure DiffractionTwoTheta;
    procedure DiffractionTheta;
    procedure DiffractionSinThetaOverLambda;
    procedure ExpressionAxisLinear;
    procedure ExpressionAxisLogarithmic;
    procedure AFormulaThatCannotBeEvaluatedIsRefusedInWords;

    //  WHAT A TYPED WAVELENGTH HAS TO BE. The sin(theta)/lambda axis divides by
    //  it, and both refusals used to live in an LCL menu handler with a
    //  MessageDlg each, reachable only by typing into the box.
    procedure APlainWavelengthIsAccepted;
    procedure ANonNumberIsRefusedWithTheSeparatorNamed;
    procedure ZeroIsRefusedBecauseItAlreadyMeansNotSet;
    procedure ANegativeWavelengthIsRefused;
    procedure ARefusedValueComesBackAsZeroRatherThanHalfParsed;
    procedure AnAcceptedValueCarriesNoRefusal;
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
  //  Even steps in the logarithm ARE the logarithmic spacing; only the text
  //  on each mark is the axis's own.
  ax := TLogarithmicAxis.Create('Price', 'USD');
  try
    AssertFalse(ax.ChooseMarks(1, 4, Start, Step));
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

procedure TCoordinateAxisTest.DiffractionTwoTheta;
var ax: TDiffractionAngleAxis;
begin
  ax := TDiffractionAngleAxis.Create(dmTwoTheta, 1.5);
  try
    AssertEquals('2theta to-display', 30.0, ax.ToDisplay(30.0), 1e-12);
    AssertEquals('2theta from-display', 30.0, ax.FromDisplay(30.0), 1e-12);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.DiffractionTheta;
var ax: TDiffractionAngleAxis;
begin
  ax := TDiffractionAngleAxis.Create(dmTheta, 1.5);
  try
    AssertEquals('theta = 2theta/2', 15.0, ax.ToDisplay(30.0), 1e-12);
    AssertEquals('inverse theta', 30.0, ax.FromDisplay(15.0), 1e-12);
  finally ax.Free; end;
end;

procedure TCoordinateAxisTest.DiffractionSinThetaOverLambda;
var ax: TDiffractionAngleAxis; d: double;
begin
  ax := TDiffractionAngleAxis.Create(dmSinThetaOverLambda, 1.5);
  try
    // 2theta=30 -> sin(15deg)/1.5 = 0.2588190/1.5 = 0.1725460
    d := ax.ToDisplay(30.0);
    AssertEquals('sin(theta)/lambda', 0.1725460, d, 1e-6);
    // round-trips back to the raw 2theta
    AssertEquals('round-trip to 2theta', 30.0, ax.FromDisplay(d), 1e-9);
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

{ ---- what a typed wavelength has to be ------------------------------------- }

procedure TCoordinateAxisTest.APlainWavelengthIsAccepted;
var V: double; R: string;
begin
    AssertTrue('accepted', WavelengthFromText('1.5406', V, R));
    AssertEquals('the value', 1.5406, V, 1e-12);
end;

procedure TCoordinateAxisTest.ANonNumberIsRefusedWithTheSeparatorNamed;
var V: double; R: string;
begin
    //  A comma is what a great many keyboards and locales produce, so the
    //  refusal is a mystery unless it says which separator is wanted.
    AssertFalse('refused', WavelengthFromText('1,5406', V, R));
    AssertTrue('the separator is named', Pos('full stop', R) > 0);
    AssertTrue('an example is given', Pos('1.5406', R) > 0);
end;

procedure TCoordinateAxisTest.ZeroIsRefusedBecauseItAlreadyMeansNotSet;
var V: double; R: string;
begin
    //  THE ONE THAT WOULD FAIL SILENTLY. Zero is what the client already reads
    //  as "no wavelength", so accepting it would do nothing at all - and the
    //  axis it is for divides by it.
    AssertFalse('refused', WavelengthFromText('0', V, R));
    AssertTrue('and says why', Pos('greater than zero', R) > 0);
end;

procedure TCoordinateAxisTest.ANegativeWavelengthIsRefused;
var V: double; R: string;
begin
    AssertFalse('refused', WavelengthFromText('-1.5', V, R));
end;

procedure TCoordinateAxisTest.ARefusedValueComesBackAsZeroRatherThanHalfParsed;
var V: double; R: string;
begin
    //  The caller shows the message and returns; if it ever did read the value,
    //  it must not be a partially converted one.
    V := 99;
    AssertFalse('refused', WavelengthFromText('rubbish', V, R));
    AssertEquals('cleared', 0, V, 1e-12);
end;

procedure TCoordinateAxisTest.AnAcceptedValueCarriesNoRefusal;
var V: double; R: string;
begin
    //  So that a caller which shows R unconditionally shows nothing.
    R := 'left over';
    AssertTrue('accepted', WavelengthFromText('2', V, R));
    AssertEquals('no message', '', R);
end;

initialization
  RegisterTest('unit', TCoordinateAxisTest);
end.
