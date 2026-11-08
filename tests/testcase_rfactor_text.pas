// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How an R-factor reads, everywhere it is shown.)

A GOOD FIT DRIVES THE R-FACTOR TOWARDS ZERO, and a fixed eight-decimal format
then showed 0.00000568 - a row of zeros with the only information in the last
digits, and nothing at all once the value fell below 1e-8. The status bar, the
progress header and the REST stats each formatted it themselves, so fixing one
would have left the others disagreeing with it.
}
unit testcase_rfactor_text;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, rfactor_text;

type
    TRFactorTextTest = class(TTestCase)
    published
        procedure AnOrdinaryValueReadsAsADecimal;
        procedure ATinyValueKeepsItsDigits;
        procedure AValueBelowAnyFixedFormatIsNotZero;
        procedure RoundingUpDoesNotAddADigit;
        procedure ZeroIsZero;
        procedure AHugeValueDoesNotFillThePanel;
        procedure TheTextParsesBackToTheValue;
        procedure ASlowFitStillChangesWhatIsShown;
        procedure AWholeNumberIsNeverCutShort;
    end;

implementation

procedure TRFactorTextTest.AnOrdinaryValueReadsAsADecimal;
begin
    AssertEquals('six significant digits', '0.0421345', RFactorText(0.04213451));
    AssertEquals('above one', '1.50000', RFactorText(1.5));
    AssertEquals('at the switch', '0.00123456', RFactorText(0.001234561));
end;

procedure TRFactorTextTest.ATinyValueKeepsItsDigits;
begin
    //  THE CASE REPORTED: 0.00000568 is four zeros and three digits.
    AssertEquals('scientific', '5.68412E-6', RFactorText(0.000005684123));
end;

procedure TRFactorTextTest.AValueBelowAnyFixedFormatIsNotZero;
begin
    AssertEquals('1e-12', '1.23457E-12', RFactorText(1.2345678E-12));
end;

procedure TRFactorTextTest.RoundingUpDoesNotAddADigit;
begin
    //  The magnitude is taken AFTER rounding, so 0.09999999 does not read as
    //  0.1000000 with a seventh digit, nor 0.0009999999 as a decimal.
    AssertEquals('0.1', '0.100000', RFactorText(0.09999999));
    AssertEquals('0.001', '0.00100000', RFactorText(0.0009999999));
end;

procedure TRFactorTextTest.ZeroIsZero;
begin
    AssertEquals('0', '0.00000', RFactorText(0));
end;

procedure TRFactorTextTest.AHugeValueDoesNotFillThePanel;
begin
    //  A sum-of-squares loss on raw counts runs to millions.
    AssertEquals('1e7', '1.23457E7', RFactorText(12345678));
end;

procedure TRFactorTextTest.TheTextParsesBackToTheValue;
var
    V: double;
begin
    //  The project file reads the reported text back as a number.
    AssertTrue('parses', TryStrToFloat(RFactorText(0.000005684123), V));
    AssertEquals('to the value shown', 5.68412E-6, V, 1E-15);
end;

procedure TRFactorTextTest.ASlowFitStillChangesWhatIsShown;
begin
    //  THE CASE REPORTED: a long simplex run improving by a few parts in ten
    //  thousand per second read "1.323E-6" for minutes and looked stopped.
    AssertTrue('a change in the fifth digit shows',
        RFactorText(1.32312E-6) <> RFactorText(1.32348E-6));
end;

procedure TRFactorTextTest.AWholeNumberIsNeverCutShort;
begin
    //  FEWER DIGITS THAN ITS WHOLE PART, as an old project's figure is read at
    //  (fit_project_provenance): the decimals stop at none, and the whole part
    //  is written out rather than rounded to a different number.
    AssertEquals('123457', RFactorText(123456.7, 3));
end;

initialization
    RegisterTest('unit', TRFactorTextTest);
end.
