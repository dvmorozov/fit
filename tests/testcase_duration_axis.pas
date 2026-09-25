// SPDX-License-Identifier: GPL-3.0-or-later
{ The elapsed-time axis of the loss chart reads in hours, minutes and seconds.

  It used to read in plain seconds, so a fit left running overnight put 3.5E4 at
  the end of its axis - a number nobody reads as nine and a half hours. }
unit testcase_duration_axis;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, coordinate_axis;
type
  TDurationAxisTest = class(TTestCase)
  private
    FAxis: TDurationAxis;
    function StepFor(AMax: double): double;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AnOvernightFitIsMarkedInHours;
    procedure AFewMinutesAreMarkedInMinutes;
    procedure AShortFitIsMarkedInSeconds;
    procedure MarksStartOnAMultipleOfTheStep;
    procedure AMarkNamesItsUnits;
    procedure ASubSecondStepKeepsItsFraction;
    procedure DaysAreNamed;
    procedure AnAxisThatDoesNotChooseLeavesTheChartToIt;
    procedure ADurationIsShownAsItIsStoredUnderItsName;
  end;
implementation

procedure TDurationAxisTest.SetUp;
begin
  FAxis := TDurationAxis.Create;
end;

procedure TDurationAxisTest.TearDown;
begin
  FreeAndNil(FAxis);
end;

function TDurationAxisTest.StepFor(AMax: double): double;
var
  Start: double;
begin
  AssertTrue('the duration axis chooses its own marks',
    FAxis.ChooseMarks(0, AMax, Start, Result));
end;

procedure TDurationAxisTest.AnOvernightFitIsMarkedInHours;
begin
  //  The run in the report: 0 to about 35 000 s.
  AssertEquals('two-hourly marks over nine and a half hours', 7200,
    StepFor(35000), 0);
end;

procedure TDurationAxisTest.AFewMinutesAreMarkedInMinutes;
begin
  AssertEquals(60, StepFor(400), 0);
  AssertEquals(300, StepFor(2400), 0);
end;

procedure TDurationAxisTest.AShortFitIsMarkedInSeconds;
begin
  AssertEquals(5, StepFor(30), 0);
  AssertEquals(1, StepFor(7), 0);
end;

procedure TDurationAxisTest.MarksStartOnAMultipleOfTheStep;
var
  Start, Step: double;
begin
  AssertTrue(FAxis.ChooseMarks(130, 500, Start, Step));
  AssertEquals('step', 60, Step, 0);
  AssertEquals('first mark at or after the start', 180, Start, 0);
end;

procedure TDurationAxisTest.AMarkNamesItsUnits;
begin
  AssertEquals('0', FAxis.MarkText(0, 3600));
  AssertEquals('9 h', FAxis.MarkText(32400, 3600));
  AssertEquals('1 h 30 min', FAxis.MarkText(5400, 1800));
  AssertEquals('1 min 30 s', FAxis.MarkText(90, 30));
  AssertEquals('45 s', FAxis.MarkText(45, 5));
end;

procedure TDurationAxisTest.ASubSecondStepKeepsItsFraction;
begin
  AssertEquals('0.5 s', FAxis.MarkText(0.5, 0.1));
  AssertEquals('1.2 s', FAxis.MarkText(1.2, 0.2));
end;

procedure TDurationAxisTest.DaysAreNamed;
begin
  AssertEquals('1 d 6 h', FAxis.MarkText(86400 + 6 * 3600, 21600));
end;

procedure TDurationAxisTest.AnAxisThatDoesNotChooseLeavesTheChartToIt;
var
  Other: TNamedAxis;
  Start, Step: double;
  Text: string;
begin
  //  Every other axis keeps the chart's own marks and number format.
  Other := TNamedAxis.Create('Position', '');
  try
    AssertFalse(Other.ChooseMarks(0, 10, Start, Step));
    AssertFalse(Other.FormatsMarks);
    Text := Other.MarkText(2.5, 0.5);
    AssertEquals('', Text);
  finally
    Other.Free;
  end;
  AssertTrue(FAxis.FormatsMarks);
end;

procedure TDurationAxisTest.ADurationIsShownAsItIsStoredUnderItsName;
begin
  AssertEquals('Elapsed time', FAxis.DisplayName);
  AssertEquals('the unit is in every mark instead', '', FAxis.UnitName);
  AssertEquals(90.5, FAxis.ToDisplay(90.5), 0);
  AssertEquals(90.5, FAxis.FromDisplay(90.5), 0);
end;

initialization
  RegisterTest('unit', TDurationAxisTest);
end.
