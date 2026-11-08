// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The line scanner every line-based numeric format reads through.)

WHY IT IS TESTED HERE AS WELL AS THROUGH A LOADER. testcase_dat_parser pins what
the DAT loader makes of a line, and it is what proves this scanner still behaves
as it did before it was moved out of that loader. What it cannot reach is the
scanner's own edges - a malformed run, an exponent that is not one - separately
from the loader's rule that a line needs two numbers. A second caller now exists
(the JCAMP-DX reader in a module), so those edges are contract, not detail.
}
unit testcase_text_numbers;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, text_numbers;

type
    TTextNumbersTest = class(TTestCase)
    published
        procedure NumbersAreReadLeftToRight;
        procedure AnySeparatorEndsANumber;
        procedure ASignStartsANumberOnlyWhereOneMayStart;
        procedure AnExponentIsPartOfTheNumber;
        procedure ALetterAfterDigitsIsNotAnExponent;
        procedure ACommaIsADecimalMarkOnlyWhenAsked;
        procedure TwoDecimalMarksInOneRunAreMalformed;
        procedure PunctuationWithNoDigitIsNotANumber;
        procedure AnEmptyLineHoldsNoNumbers;
    end;

implementation

function Scan(const ALine: string; ACommaIsDecimal: boolean;
    out AValid: boolean): TLineNumbers;
begin
    Result := LineNumbers(ALine, ACommaIsDecimal, AValid);
end;

procedure TTextNumbersTest.NumbersAreReadLeftToRight;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('10 20 30', False, Valid);
    AssertTrue('a line of plain numbers is well formed', Valid);
    AssertEquals('three numbers', 3, Length(N));
    AssertEquals('first', 10, N[0], 1E-9);
    AssertEquals('last', 30, N[2], 1E-9);
end;

procedure TTextNumbersTest.AnySeparatorEndsANumber;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    //  The formats this serves separate columns with spaces, tabs, semicolons
    //  or nothing more than a letter, and a reader that insisted on one of them
    //  would refuse files that are plainly readable.
    N := Scan('1;2'#9'3|4', False, Valid);
    AssertTrue('well formed', Valid);
    AssertEquals('four numbers', 4, Length(N));
end;

procedure TTextNumbersTest.ASignStartsANumberOnlyWhereOneMayStart;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('-10 20', False, Valid);
    AssertEquals('a leading sign belongs to its number', -10, N[0], 1E-9);
    //  Between two digits the sign is a separator: "10-20" is a range written
    //  without spaces, which is how some files write two columns.
    N := Scan('10-20', False, Valid);
    AssertEquals('two numbers', 2, Length(N));
    AssertEquals('the second keeps no sign', 20, N[1], 1E-9);
end;

procedure TTextNumbersTest.AnExponentIsPartOfTheNumber;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('1.5e3 2E-2', False, Valid);
    AssertTrue('well formed', Valid);
    AssertEquals('positive exponent', 1500, N[0], 1E-9);
    AssertEquals('negative exponent', 0.02, N[1], 1E-12);
end;

procedure TTextNumbersTest.ALetterAfterDigitsIsNotAnExponent;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    //  "3eV" is a number and a unit. Reading the e as an exponent would give a
    //  number that is plausible and wrong, which is the whole hazard here.
    N := Scan('3eV 4', False, Valid);
    AssertEquals('two numbers', 2, Length(N));
    AssertEquals('the unit is not part of the value', 3, N[0], 1E-9);
end;

procedure TTextNumbersTest.ACommaIsADecimalMarkOnlyWhenAsked;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('1,5 2,5', True, Valid);
    AssertEquals('as a decimal mark, two numbers', 2, Length(N));
    AssertEquals('one and a half', 1.5, N[0], 1E-9);

    N := Scan('1,5 2,5', False, Valid);
    AssertEquals('as a separator, four numbers', 4, Length(N));
end;

procedure TTextNumbersTest.TwoDecimalMarksInOneRunAreMalformed;
var
    Valid: boolean;
begin
    //  "1.2.3" is a version, a date or a damaged file - never a number - and
    //  the caller skips the whole line rather than keeping a guess from it.
    Scan('1.2.3 4', False, Valid);
    AssertFalse('a run with two marks makes the line malformed', Valid);
end;

procedure TTextNumbersTest.PunctuationWithNoDigitIsNotANumber;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('Intensity vs. angle', False, Valid);
    AssertTrue('a sentence is not malformed, it simply holds no numbers', Valid);
    AssertEquals('no numbers', 0, Length(N));
end;

procedure TTextNumbersTest.AnEmptyLineHoldsNoNumbers;
var
    N: TLineNumbers;
    Valid: boolean;
begin
    N := Scan('', False, Valid);
    AssertTrue('well formed', Valid);
    AssertEquals('no numbers', 0, Length(N));
end;

initialization
    RegisterTest('unit', TTextNumbersTest);
end.
