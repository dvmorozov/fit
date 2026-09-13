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
        procedure NegativeSignIsNotPartOfANumber;
        procedure ASecondDecimalPointEndsTheNumber;
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

procedure TDatParserTest.NegativeSignIsNotPartOfANumber;
var
    P: TTitlePointsSet;
begin
    //  CHARACTERISATION, not endorsement. A minus sign is not in the accepted set,
    //  so it reads as a separator: "-1 -10" yields x=1, y=10 rather than the
    //  negative pair a reader would expect. The format has always been positive
    //  angles and positive counts, so nothing has hit it - but the behaviour is
    //  surprising enough that it must be written down rather than discovered.
    P := Parse(['-1 -10']);
    try
        AssertEquals('one point', 1, P.PointsCount);
        AssertEquals('the sign was treated as a separator', 1.0,
            P.PointXCoord[0], 1e-9);
        AssertEquals('for both columns', 10.0, P.PointYCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TDatParserTest.ASecondDecimalPointEndsTheNumber;
var
    P: TTitlePointsSet;
begin
    //  CHARACTERISATION, not endorsement. A second point or comma inside one
    //  number is not an error and does not abandon the line - it ends the
    //  NUMBER, and the digits after it start the next column. So "1.2.3 4.5" is
    //  read as x=1.2, y=3, and the 4.5 that a reader sees as the second column
    //  is never looked at: the line was complete four characters before it.
    //  Pinned because this is the one path the parser's goto served, and a
    //  restructuring that landed anywhere else would change what a malformed
    //  number means without failing anything.
    P := Parse(['1.2.3 4.5', '9,8,7;6']);
    try
        AssertEquals('one point per line', 2, P.PointsCount);
        AssertEquals('the number stops at the second point', 1.2,
            P.PointXCoord[0], 1e-9);
        AssertEquals('and the digits after it are the second column', 3.0,
            P.PointYCoord[0], 1e-9);
        //  The comma arm of the same test, since either symbol is a decimal
        //  point here and either one repeated ends the number.
        AssertEquals('a second comma ends it too', 9.8, P.PointXCoord[1], 1e-9);
        AssertEquals('and again the rest is a new column', 7.0,
            P.PointYCoord[1], 1e-9);
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
