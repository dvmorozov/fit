// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the OHLC price-CSV loader.

  The emphasis is on the decisions a loader makes silently, because those are the
  ones a user cannot see in the result: which column became y, which delimiter
  was assumed, how x was derived, and which rows were dropped. A loader that
  guesses wrong here produces data that looks plausible and is wrong. }
unit testcase_ohlc_loader;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, DateUtils, fpcunit, testregistry,
  data_loader, ohlc_csv_loader, points_set, title_points_set;
type
  TOhlcLoaderTest = class(TTestCase)
  private
    FLines: TStringList;
    { The CSV as lines, never as a file. See the unit header for why. }
    function Csv(const ALines: array of string): TStrings;
    function Load(ALines: TStrings; AXMode: TOhlcXMode;
      AColumn: TOhlcValueColumn; out ASkipped: integer): TTitlePointsSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadsClosePricesAgainstBarIndexByDefault;
    procedure BarIndexRemovesCalendarGaps;
    procedure DateSerialModeKeepsCalendarGaps;
    procedure AdjustedCloseIsPreferredOverRawClose;
    procedure SemicolonDelimiterAndCommaDecimalsAreHandled;
    procedure OtherPriceColumnsCanBeSelected;
    procedure ShortAndBlankRowsAreSkippedNotFatal;
    procedure HeaderlessFileAssumesOhlcvOrder;
    procedure ATwoColumnSeriesTakesTheSecondColumn;
    procedure DayFirstLayoutIsProvedByADayAboveTwelve;
    procedure MonthFirstLayoutIsProvedByAMonthAboveTwelve;
    procedure ContradictoryDateColumnIsRefused;
    procedure AmbiguousDatesAreRefusedRatherThanGuessed;
    procedure TheAmbiguousDateErrorExplainsBothRemedies;
    procedure AFileWithNoUsableRowsIsAnError;

    //  The refusals a malformed file gets, each naming what to do about it.
    procedure AnEmptyFileIsRefused;
    procedure AHeaderlessFileTooNarrowToGuessIsRefused;
    procedure AskingForDatesWhenThereAreNoneIsRefused;
    procedure AskingForAColumnTheFileHasNotIsRefused;
    procedure ATabSeparatedFileIsRead;
  end;

  { Reads the sample CSV from Data/, so it crosses to the filesystem. }
  TOhlcSampleFileTest = class(TTestCase)
  private
    function Load(const APath: string; AXMode: TOhlcXMode;
      AColumn: TOhlcValueColumn; out ASkipped: integer): TTitlePointsSet;
  published
    procedure TheBundledSampleLoads;
  end;

implementation

procedure TOhlcLoaderTest.SetUp;
begin
  FLines := TStringList.Create;
end;

procedure TOhlcLoaderTest.TearDown;
begin
  FreeAndNil(FLines);
end;

function TOhlcLoaderTest.Csv(const ALines: array of string): TStrings;
var
  i: integer;
begin
  FLines.Clear;
  for i := Low(ALines) to High(ALines) do
    FLines.Add(ALines[i]);
  Result := FLines;
end;

function TOhlcLoaderTest.Load(ALines: TStrings; AXMode: TOhlcXMode;
  AColumn: TOhlcValueColumn; out ASkipped: integer): TTitlePointsSet;
var
  Loader: TOHLCFileLoader;
begin
  Loader := TOHLCFileLoader.Create(nil);
  try
    Loader.XMode := AXMode;
    Loader.ValueColumn := AColumn;
    //  LoadFromLines, not LoadDataSet: the same parser, reached without a file.
    //  That is what makes every assertion below a unit test.
    Loader.LoadFromLines(ALines);
    ASkipped := Loader.SkippedRows;
    Result := Loader.GetPointsSetCopy;
  finally
    Loader.Free;
  end;
end;

procedure TOhlcLoaderTest.LoadsClosePricesAgainstBarIndexByDefault;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Open,High,Low,Close,Volume',
    '2024-01-02,100,101,99,100.5,1000',
    '2024-01-03,100.5,103,100,102.5,1100',
    '2024-01-04,102.5,104,102,103.5,1200']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('three bars', 3, P.PointsCount);
    AssertEquals('first x is bar 0', 0.0, P.PointXCoord[0], 1e-12);
    AssertEquals('third x is bar 2', 2.0, P.PointXCoord[2], 1e-12);
    AssertEquals('y is the close', 100.5, P.PointYCoord[0], 1e-9);
    AssertEquals('last close', 103.5, P.PointYCoord[2], 1e-9);
    AssertEquals('nothing skipped', 0, Skipped);
  finally
    P.Free;
  end;
end;

{ The reason bar index is the default: a weekend must not become a gap, or wave
  durations would measure calendar time instead of trading activity. }
procedure TOhlcLoaderTest.BarIndexRemovesCalendarGaps;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Close',
    '2024-01-05,100',      //  Friday
    '2024-01-08,101',      //  Monday - three calendar days later
    '2024-01-09,102']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('steps are uniform across the weekend',
      1.0, P.PointXCoord[1] - P.PointXCoord[0], 1e-12);
    AssertEquals('and uniform within the week',
      1.0, P.PointXCoord[2] - P.PointXCoord[1], 1e-12);
  finally
    P.Free;
  end;
end;

procedure TOhlcLoaderTest.DateSerialModeKeepsCalendarGaps;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Close',
    '2024-01-05,100',
    '2024-01-08,101']);
  P := Load(Lines, xmDateSerial, vcClose, Skipped);
  try
    //  Correct for a chronological axis, and exactly what makes it unsuitable
    //  for measuring wave duration.
    AssertEquals('the weekend gap is preserved',
      3.0, P.PointXCoord[1] - P.PointXCoord[0], 1e-9);
    AssertEquals('x is a real date serial',
      EncodeDate(2024, 1, 5), P.PointXCoord[0], 1e-9);
  finally
    P.Free;
  end;
end;

{ An unadjusted series has artificial jumps at splits and dividends, and a jump
  reads as a wave boundary - so the adjusted column is the honest choice when a
  file offers both. }
procedure TOhlcLoaderTest.AdjustedCloseIsPreferredOverRawClose;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Close,Adj Close',
    '2024-01-02,200,100',
    '2024-01-03,210,105']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('the adjusted close was taken', 100.0, P.PointYCoord[0], 1e-9);
    AssertEquals('and again on the second bar', 105.0, P.PointYCoord[1], 1e-9);
  finally
    P.Free;
  end;
end;

{ The European convention: semicolon delimiter BECAUSE the comma is the decimal
  separator. Guessing comma as the delimiter here would split every price. }
procedure TOhlcLoaderTest.SemicolonDelimiterAndCommaDecimalsAreHandled;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date;Open;High;Low;Close',
    '2024-01-02;100,25;101,50;99,75;100,80',
    '2024-01-03;100,80;103,10;100,60;102,90']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('two bars', 2, P.PointsCount);
    AssertEquals('comma decimals parsed', 100.80, P.PointYCoord[0], 1e-9);
    AssertEquals('and the second', 102.90, P.PointYCoord[1], 1e-9);
  finally
    P.Free;
  end;
end;

procedure TOhlcLoaderTest.OtherPriceColumnsCanBeSelected;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Open,High,Low,Close',
    '2024-01-02,10,20,5,15']);
  P := Load(Lines, xmBarIndex, vcHigh, Skipped);
  try
    AssertEquals('high', 20.0, P.PointYCoord[0], 1e-9);
  finally P.Free; end;

  P := Load(Lines, xmBarIndex, vcLow, Skipped);
  try
    AssertEquals('low', 5.0, P.PointYCoord[0], 1e-9);
  finally P.Free; end;

  P := Load(Lines, xmBarIndex, vcOpen, Skipped);
  try
    AssertEquals('open', 10.0, P.PointYCoord[0], 1e-9);
  finally P.Free; end;
end;

{ Real feed exports end with blank lines, footers, or a partial last bar.
  Aborting the whole load over one bad row would throw away thousands of good
  ones - but the count of skips is reported, so a mostly-junk file is not
  mistaken for a small clean one. }
procedure TOhlcLoaderTest.ShortAndBlankRowsAreSkippedNotFatal;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Open,High,Low,Close',
    '2024-01-02,100,101,99,100.5',
    '',
    '2024-01-03,100.5',                 //  truncated row
    '2024-01-04,102.5,104,102,103.5',
    'Totals,,,,',                       //  footer
    '2024-01-05,103.5,105,103,104.5']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('the good bars loaded', 3, P.PointsCount);
    AssertEquals('the bad rows were counted', 2, Skipped);
    //  Bar indices must be contiguous over the SURVIVING rows, or the x axis
    //  would carry holes where junk used to be.
    AssertEquals('bar 0', 0.0, P.PointXCoord[0], 1e-12);
    AssertEquals('bar 1', 1.0, P.PointXCoord[1], 1e-12);
    AssertEquals('bar 2', 2.0, P.PointXCoord[2], 1e-12);
    AssertEquals('third close', 104.5, P.PointYCoord[2], 1e-9);
  finally
    P.Free;
  end;
end;

procedure TOhlcLoaderTest.HeaderlessFileAssumesOhlcvOrder;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    '2024-01-02,100,101,99,100.5,1000',
    '2024-01-03,100.5,103,100,102.5,1100']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('two bars', 2, P.PointsCount);
    AssertEquals('column 4 taken as close', 100.5, P.PointYCoord[0], 1e-9);
  finally
    P.Free;
  end;
end;

{ How central banks and statistical releases publish: a date and one value, with
  no price column to name. Refusing these would exclude a whole class of real
  data - FRED emits exactly this shape, missing days marked with a dot. }
procedure TOhlcLoaderTest.ATwoColumnSeriesTakesTheSecondColumn;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'observation_date,DEXUSEU',
    '1999-01-04,1.1812',
    '1999-01-05,1.1760',
    '1999-01-06,.',            //  FRED marks a missing day with a dot
    '1999-01-07,1.1672']);
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('the three real observations loaded', 3, P.PointsCount);
    AssertEquals('the missing day was skipped', 1, Skipped);
    AssertEquals('first value', 1.1812, P.PointYCoord[0], 1e-9);
    AssertEquals('last value', 1.1672, P.PointYCoord[2], 1e-9);
    //  Bar indices stay contiguous over the surviving rows.
    AssertEquals('bar 2', 2.0, P.PointXCoord[2], 1e-12);
  finally
    P.Free;
  end;
end;

{ 01/02/2024 is January 2nd or February 1st depending on where you are. Guessing
  would silently REORDER the series, and a wrong x axis is worse than a refusal -
  so the ambiguous form is not accepted in date-serial mode. }
procedure TOhlcLoaderTest.AmbiguousDatesAreRefusedRatherThanGuessed;
var Lines: TStrings; Raised: boolean; Skipped: integer; P: TTitlePointsSet;
begin
  Lines := Csv([
    'Date,Close',
    '01/02/2024,100',
    '02/02/2024,101']);
  Raised := False;
  try
    P := Load(Lines, xmDateSerial, vcClose, Skipped);
    P.Free;
  except
    on EInvalidFileFormat do Raised := True;
  end;
  AssertTrue('an ambiguous date is not silently interpreted', Raised);

  //  In bar-index mode the dates are not needed at all, so the same file loads.
  P := Load(Lines, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('bar index mode ignores the date column', 2, P.PointsCount);
  finally
    P.Free;
  end;
end;

{ Arithmetic settles most real files, so the user is never asked: a first
  component above 12 can only be a day. 15/03 proves the whole column is
  day-first, which then fixes how 01/02 is read. }
procedure TOhlcLoaderTest.DayFirstLayoutIsProvedByADayAboveTwelve;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Close',
    '01/02/2024,100',
    '15/03/2024,101']);       //  15 can only be a day
  P := Load(Lines, xmDateSerial, vcClose, Skipped);
  try
    AssertEquals('two bars', 2, P.PointsCount);
    AssertEquals('01/02 read as 2 February',
      EncodeDate(2024, 2, 1), P.PointXCoord[0], 1e-9);
    AssertEquals('15/03 read as 15 March',
      EncodeDate(2024, 3, 15), P.PointXCoord[1], 1e-9);
  finally
    P.Free;
  end;
end;

procedure TOhlcLoaderTest.MonthFirstLayoutIsProvedByAMonthAboveTwelve;
var P: TTitlePointsSet; Lines: TStrings; Skipped: integer;
begin
  Lines := Csv([
    'Date,Close',
    '01/02/2024,100',
    '03/15/2024,101']);       //  the 15 is in the SECOND position
  P := Load(Lines, xmDateSerial, vcClose, Skipped);
  try
    AssertEquals('two bars', 2, P.PointsCount);
    AssertEquals('01/02 read as 1 February',
      EncodeDate(2024, 1, 2), P.PointXCoord[0], 1e-9);
    AssertEquals('03/15 read as 15 March',
      EncodeDate(2024, 3, 15), P.PointXCoord[1], 1e-9);
  finally
    P.Free;
  end;
end;

{ A column that proves BOTH layouts is not one layout with an odd row - it is a
  file whose dates cannot all be right. Refusing beats loading half of it. }
procedure TOhlcLoaderTest.ContradictoryDateColumnIsRefused;
var Lines: TStrings; Raised: boolean; Skipped: integer; P: TTitlePointsSet;
begin
  Lines := Csv([
    'Date,Close',
    '15/03/2024,100',        //  proves day-first
    '03/15/2024,101']);      //  proves month-first
  Raised := False;
  try
    P := Load(Lines, xmDateSerial, vcClose, Skipped);
    P.Free;
  except
    on E: EInvalidFileFormat do
    begin
      Raised := True;
      AssertTrue('the error says the column is inconsistent',
        Pos('not consistent', E.Message) > 0);
    end;
  end;
  AssertTrue('a contradictory date column is refused', Raised);
end;

{ The message must name the problem AND the ways out. Before this the user saw
  only "no usable rows were found", which hides the real cause and suggests the
  file is empty. }
procedure TOhlcLoaderTest.TheAmbiguousDateErrorExplainsBothRemedies;
var Lines: TStrings; Msg: string; Skipped: integer; P: TTitlePointsSet;
begin
  Lines := Csv([
    'Date,Close',
    '01/02/2024,100',
    '02/03/2024,101']);
  Msg := '';
  try
    P := Load(Lines, xmDateSerial, vcClose, Skipped);
    P.Free;
  except
    on E: EInvalidFileFormat do Msg := E.Message;
  end;
  AssertTrue('names the ambiguity', Pos('ambiguous', LowerCase(Msg)) > 0);
  AssertTrue('says loading was stopped',
    Pos('stopped', LowerCase(Msg)) > 0);
  AssertTrue('offers the ISO remedy', Pos('yyyy-mm-dd', Msg) > 0);
  AssertTrue('offers the bar-index remedy',
    Pos('bar-index', LowerCase(Msg)) > 0);
end;

procedure TOhlcLoaderTest.AFileWithNoUsableRowsIsAnError;
var Lines: TStrings; Raised: boolean; Skipped: integer; P: TTitlePointsSet;
begin
  Lines := Csv([
    'Date,Open,High,Low,Close',
    'not,a,number,at,all']);
  Raised := False;
  try
    P := Load(Lines, xmBarIndex, vcClose, Skipped);
    P.Free;
  except
    on EInvalidFileFormat do Raised := True;
  end;
  AssertTrue('a file with nothing usable fails loudly', Raised);
end;

{ The fixture the user guide's worked example is built on: it must keep loading,
  or the documentation stops being reproducible. }

function TOhlcSampleFileTest.Load(const APath: string; AXMode: TOhlcXMode;
  AColumn: TOhlcValueColumn; out ASkipped: integer): TTitlePointsSet;
var
  Loader: TOHLCFileLoader;
begin
  Loader := TOHLCFileLoader.Create(nil);
  try
    Loader.XMode := AXMode;
    Loader.ValueColumn := AColumn;
    Loader.LoadDataSet(APath);
    ASkipped := Loader.SkippedRows;
    Result := Loader.GetPointsSetCopy;
  finally
    Loader.Free;
  end;
end;

{ THE ONE TEST THAT REALLY NEEDS A FILE, and therefore the only one here that is
  an integration test. It checks that the CSV shipped in Data/ still loads - which
  is about the file existing and being reachable from the binary, not about the
  parser, and the parser is covered above without it. }
procedure TOhlcSampleFileTest.TheBundledSampleLoads;
var P: TTitlePointsSet; Skipped: integer; Path: string;
begin
  Path := ExtractFilePath(ParamStr(0)) + '../Data/sample-ohlc.csv';
  if not FileExists(Path) then
  begin
    Ignore('sample-ohlc.csv not reachable from the test binary');
    Exit;
  end;
  P := Load(Path, xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('every bar loaded', 20, P.PointsCount);
    AssertEquals('nothing skipped', 0, Skipped);
    AssertEquals('first close', 100.80, P.PointYCoord[0], 1e-9);
    AssertEquals('last close', 117.90, P.PointYCoord[19], 1e-9);
  finally
    P.Free;
  end;
end;

{ ---- what a malformed file is told ----------------------------------------- }

{ EVERY ONE OF THESE IS A REFUSAL WITH A REMEDY IN IT, and that is what is being
  pinned as much as the refusal itself. A loader that stops without saying what to
  change leaves the user with a file that "does not work" and nothing to do about
  it - and these are the paths a user meets with somebody else's export, which is
  most files this program will ever see. }

function RefusalFromLoading(ALines: TStrings; AXMode: TOhlcXMode;
  AColumn: TOhlcValueColumn): string;
var
  Loader: TOHLCFileLoader;
begin
  Result := '';
  Loader := TOHLCFileLoader.Create(nil);
  try
    Loader.XMode := AXMode;
    Loader.ValueColumn := AColumn;
    try
      Loader.LoadFromLines(ALines);
    except
      on E: EInvalidFileFormat do
        Result := E.Message;
    end;
  finally
    Loader.Free;
  end;
end;

procedure TOhlcLoaderTest.AnEmptyFileIsRefused;
var
  Msg: string;
begin
  //  Nothing at all: not a crash, and not an empty chart either - an empty chart
  //  is indistinguishable from a file that loaded and contained nothing.
  Msg := RefusalFromLoading(Csv([]), xmBarIndex, vcClose);
  AssertTrue('refused: ' + Msg, Msg <> '');
  AssertTrue('and says the file is empty', Pos('empty', LowerCase(Msg)) > 0);
end;

procedure TOhlcLoaderTest.AHeaderlessFileTooNarrowToGuessIsRefused;
var
  Msg: string;
begin
  //  With a header the columns are named; without one the loader assumes the
  //  usual open-high-low-close-volume order, which needs five columns to be that
  //  order at all. Three columns and no header is a file it cannot read, and the
  //  remedy - add a header naming a close column - is the message.
  Msg := RefusalFromLoading(Csv(['1,2,3', '4,5,6']), xmBarIndex, vcClose);
  AssertTrue('refused: ' + Msg, Msg <> '');
  AssertTrue('it says why', Pos('five', LowerCase(Msg)) > 0);
  AssertTrue('and what to add', Pos('header', LowerCase(Msg)) > 0);
end;

procedure TOhlcLoaderTest.AskingForDatesWhenThereAreNoneIsRefused;
var
  Msg: string;
begin
  //  The chronological abscissa was asked for and the file has no date column.
  //  Refused rather than falling back to the bar index silently: the x axis would
  //  be a different quantity from the one the user asked for, and nothing on the
  //  chart would say so.
  Msg := RefusalFromLoading(Csv(['close', '10', '11', '12']),
    xmDateSerial, vcClose);
  AssertTrue('refused: ' + Msg, Msg <> '');
  AssertTrue('and names the way out',
    Pos('bar-index', LowerCase(Msg)) > 0);
end;

procedure TOhlcLoaderTest.AskingForAColumnTheFileHasNotIsRefused;
var
  Msg: string;
begin
  //  The file is fine; the column asked for is not in it.
  Msg := RefusalFromLoading(Csv(['date,close', '2020-01-01,10',
    '2020-01-02,11']), xmBarIndex, vcHigh);
  AssertTrue('refused: ' + Msg, Msg <> '');
  AssertTrue('and says which kind of thing is missing',
    Pos('column', LowerCase(Msg)) > 0);
end;

procedure TOhlcLoaderTest.ATabSeparatedFileIsRead;
var
  Got: TTitlePointsSet;
  Skipped: integer;
begin
  //  THE THIRD DELIMITER. Comma and semicolon have tests; tab is what a
  //  spreadsheet gives you when you copy a range and paste it into a text file,
  //  which is how a good many of these files are made.
  Got := Load(Csv([ 'date' + #9 + 'close',
                    '2020-01-01' + #9 + '10',
                    '2020-01-02' + #9 + '11',
                    '2020-01-03' + #9 + '12' ]),
              xmBarIndex, vcClose, Skipped);
  try
    AssertEquals('every row read', 3, Got.PointsCount);
    AssertEquals('and the values are the close column', 10.0,
      Got.PointYCoord[0], 1e-9);
    AssertEquals('none skipped', 0, Skipped);
  finally
    Got.Free;
  end;
end;

initialization
  //  UNIT: every assertion above drives the parser over lines held in memory.
  //  The class used to write a temporary CSV for each one, which made all
  //  seventeen integration tests - so roughly two hundred lines of format
  //  decisions ran only in the slow half and counted toward no coverage at all.
  RegisterTest('unit', TOhlcLoaderTest);
  RegisterTest('integration', TOhlcSampleFileTest);
end.
