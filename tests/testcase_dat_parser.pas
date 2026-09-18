// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the DAT parser makes of a line, without a file being involved.)

The format is deliberately loose - "the first column is x, the second is y, and
the separator is any symbol that is not a digit, a point or a comma" - which
means the parser makes several decisions silently. Those are the ones worth
pinning: a loader that guesses wrong here produces a profile that looks plausible
and is wrong, and nothing downstream can tell.

testcase_dat_loader still checks that the fixture in Data/ loads, which is about
the file being reachable. This is about the format, and it reaches the same parser
through LoadFromLines rather than through a path - so it is a unit test and its
lines count.
}
unit testcase_dat_parser;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_loader, dat_file_loader, title_points_set;

type
    TDatParserTest = class(TTestCase)
    private
        { Parses the given lines and returns the points. Caller frees. }
        function Parse(const ALines: array of string): TTitlePointsSet;
    published
        procedure TwoNumbersBecomeAPoint;
        procedure AnySeparatorWorks;
        procedure ADecimalCommaIsAcceptedLikeAPoint;
        procedure ColumnsBeyondTheSecondAreIgnored;
        procedure ALineWithOneNumberContributesNothing;
        procedure BlankAndNonNumericLinesAreSkipped;
        procedure ARepeatedArgumentIsIgnored;
        procedure ANegativeNumberKeepsItsSign;
        procedure AMinusBetweenNumbersSeparatesThem;
        procedure AnExponentIsPartOfItsNumber;
        procedure ALetterEAfterANumberIsNotAnExponent;
        procedure ACommaBetweenTwoNumbersSeparatesThem;
        procedure ACommaSeparatesColumnsOfDecimalPointNumbers;
        procedure AMalformedNumberSkipsItsLine;
        procedure ALonePunctuationMarkDoesNotRefuseTheFile;
        procedure AnEmptyInputLoadsNothing;
    end;

implementation

function TDatParserTest.Parse(const ALines: array of string): TTitlePointsSet;
var
    Loader: TDATFileLoader;
    L: TStringList;
    i: integer;
begin
    L := TStringList.Create;
    try
        for i := Low(ALines) to High(ALines) do
            L.Add(ALines[i]);
        Loader := TDATFileLoader.Create(nil);
        try
            //  The same parser a real load runs; only the source of the lines
            //  differs, so this cannot pass over code the application does not use.
            Loader.LoadFromLines(L);
            Result := Loader.GetPointsSetCopy;
        finally
            Loader.Free;
        end;
    finally
        L.Free;
    end;
end;

procedure TDatParserTest.TwoNumbersBecomeAPoint;
var
    P: TTitlePointsSet;
begin
    P := Parse(['1.5 2.5']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('x is the first column', 1.5, P.PointXCoord[0], 1e-9);
        AssertEquals('y is the second', 2.5, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.AnySeparatorWorks;
var
    P: TTitlePointsSet;
begin
    //  The documented rule is "any symbol except a digit, a point or a comma", and
    //  real files arrive tab-separated, semicolon-separated and worse.
    P := Parse(['1 10', '2' + #9 + '20', '3;30', '4|40', '5   50']);
    try
        AssertEquals('every line parsed', 5, P.PointsCount);
        AssertEquals('tab', 20.0, P.PointYCoord[1], 1e-9);
        AssertEquals('semicolon', 30.0, P.PointYCoord[2], 1e-9);
        AssertEquals('pipe', 40.0, P.PointYCoord[3], 1e-9);
        AssertEquals('runs of spaces', 50.0, P.PointYCoord[4], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ADecimalCommaIsAcceptedLikeAPoint;
var
    P: TTitlePointsSet;
begin
    //  Continental exports write 1,5 for one and a half. MyStrToFloat maps both
    //  separators onto the locale's, so the file loads whatever the machine's
    //  locale happens to be - which is the point of it doing the mapping at all.
    P := Parse(['1,5;2,5']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('comma is a decimal point', 1.5, P.PointXCoord[0], 1e-9);
        AssertEquals('in both columns', 2.5, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ColumnsBeyondTheSecondAreIgnored;
var
    P: TTitlePointsSet;
begin
    //  Diffractometer exports carry error columns, counts and flags after y.
    P := Parse(['3.0 3377.0 58.1 1 extra']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('x', 3.0, P.PointXCoord[0], 1e-9);
        AssertEquals('y is the SECOND number, not the last', 3377.0,
            P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ALineWithOneNumberContributesNothing;
var
    P: TTitlePointsSet;
begin
    //  A point needs both halves. Taking x with a default y would put a
    //  zero-valued sample into the profile, and the fit would try to match it.
    P := Parse(['42', '1 10']);
    try
        AssertEquals('only the complete line became a point', 1, P.PointsCount);
        AssertEquals('and it is the right one', 1.0, P.PointXCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.BlankAndNonNumericLinesAreSkipped;
var
    P: TTitlePointsSet;
begin
    //  Headers, comments and trailing blank lines are normal in these files, and
    //  none of them is an error.
    P := Parse(['# a comment', '', '   ', 'Angle Intensity', '1 10', '']);
    try
        AssertEquals('one usable line', 1, P.PointsCount);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ARepeatedArgumentIsIgnored;
var
    P: TTitlePointsSet;
begin
    //  Two samples at one x cannot both be evaluated by a curve, so the first
    //  wins and the duplicate is dropped rather than overwriting it.
    P := Parse(['1 10', '1 999', '2 20']);
    try
        AssertEquals('the duplicate did not add a point', 2, P.PointsCount);
        AssertEquals('and the first value survived', 10.0, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ANegativeNumberKeepsItsSign;
var
    P: TTitlePointsSet;
begin
    //  A minus sign used to read as a separator, so "-1 -10" loaded as the point
    //  (1, 10): a background-subtracted profile, a difference curve or any
    //  signal that crosses zero came in folded up into positive values, looking
    //  plausible and being wrong.
    P := Parse(['-1 -10', '+2 +20']);
    try
        AssertEquals('two points', 2, P.PointsCount);
        AssertEquals('x keeps its sign', -1.0, P.PointXCoord[0], 1e-9);
        AssertEquals('y keeps its sign', -10.0, P.PointYCoord[0], 1e-9);
        AssertEquals('a plus sign is accepted too', 20.0, P.PointYCoord[1], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.AMinusBetweenNumbersSeparatesThem;
var
    P: TTitlePointsSet;
begin
    //  A sign belongs to a number only where a number can start. Written right
    //  after a digit it is a separator, as it always was.
    P := Parse(['10-20']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('x', 10.0, P.PointXCoord[0], 1e-9);
        AssertEquals('y', 20.0, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.AnExponentIsPartOfItsNumber;
var
    P: TTitlePointsSet;
begin
    //  "1.5e3" was read as the two numbers 1.5 and 3, so a line in scientific
    //  notation loaded as x=1.5, y=3 whatever its second column said.
    P := Parse(['1.5e3 2E-2', '3,5E+1;4']);
    try
        AssertEquals('two points', 2, P.PointsCount);
        AssertEquals('an exponent', 1500.0, P.PointXCoord[0], 1e-9);
        AssertEquals('a negative exponent, capital E', 0.02, P.PointYCoord[0], 1e-12);
        AssertEquals('with a decimal comma and a plus', 35.0, P.PointXCoord[1], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ALetterEAfterANumberIsNotAnExponent;
var
    P: TTitlePointsSet;
begin
    //  Only an e followed by digits is an exponent; "3e" in a unit or a word is
    //  a separator after the number.
    P := Parse(['3eV 7']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('x', 3.0, P.PointXCoord[0], 1e-9);
        AssertEquals('y', 7.0, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ACommaBetweenTwoNumbersSeparatesThem;
var
    P: TTitlePointsSet;
begin
    //  "12,40" was one number, 12.4, and the line - a plain comma-separated
    //  pair - was dropped for having no second column. A comma is a decimal
    //  mark only when that still leaves two columns.
    P := Parse(['12,40', '1,5;2,5']);
    try
        AssertEquals('two points', 2, P.PointsCount);
        AssertEquals('a comma-separated x', 12.0, P.PointXCoord[0], 1e-9);
        AssertEquals('and y', 40.0, P.PointYCoord[0], 1e-9);
        AssertEquals('while a decimal comma still reads as one', 1.5,
            P.PointXCoord[1], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ACommaSeparatesColumnsOfDecimalPointNumbers;
var
    P: TTitlePointsSet;
begin
    //  A line that writes its decimals with points uses its commas to separate.
    P := Parse(['1.5,2.5,9.9']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('x', 1.5, P.PointXCoord[0], 1e-9);
        AssertEquals('y', 2.5, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.AMalformedNumberSkipsItsLine;
var
    P: TTitlePointsSet;
begin
    //  "1.2.3" is not a number. It used to become 1.2 followed by 3 - a point
    //  built from digits nobody wrote as a column. A line that holds one is not
    //  a data line, like a header.
    P := Parse(['1.2.3 4.5', '1 10']);
    try
        AssertEquals('only the good line', 1, P.PointsCount);
        AssertEquals('and it is the right one', 10.0, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ALonePunctuationMarkDoesNotRefuseTheFile;
var
    P: TTitlePointsSet;
begin
    //  A full stop at the end of a comment line used to be taken for the start
    //  of a number, fail to convert, and refuse the WHOLE file as not a DAT
    //  file.
    P := Parse(['# Measured on 3 May. Counts, not rates.', '. , .', '1 10']);
    try
        AssertEquals('the data line loaded', 1, P.PointsCount);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.AnEmptyInputLoadsNothing;
var
    P: TTitlePointsSet;
begin
    //  Empty, not an exception: an empty file is a file with no points in it, and
    //  whether that is worth refusing is the caller's decision, not the parser's.
    P := Parse([]);
    try
        AssertEquals('no points', 0, P.PointsCount);
    finally
        P.Free;
    end;
end;

initialization
    //  A unit test: the parser over string literals, no file anywhere.
    RegisterTest('unit', TDatParserTest);
end.
