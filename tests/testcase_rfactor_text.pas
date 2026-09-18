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
    end;

implementation

procedure TRFactorTextTest.AnOrdinaryValueReadsAsADecimal;
begin
    AssertEquals('four significant digits', '0.04213', RFactorText(0.0421345));
    AssertEquals('above one', '1.500', RFactorText(1.5));
    AssertEquals('at the switch', '0.001234', RFactorText(0.001234));
end;

procedure TRFactorTextTest.ATinyValueKeepsItsDigits;
begin
    //  THE CASE REPORTED: 0.00000568 is four zeros and three digits.
    AssertEquals('scientific', '5.684E-6', RFactorText(0.0000056841));
end;

procedure TRFactorTextTest.AValueBelowAnyFixedFormatIsNotZero;
begin
    AssertEquals('1e-12', '1.235E-12', RFactorText(1.2345678E-12));
end;

procedure TRFactorTextTest.RoundingUpDoesNotAddADigit;
begin
    //  The magnitude is taken AFTER rounding, so 0.099999 does not read as
    //  0.10000 with a fifth digit, nor 0.00099999 as a decimal.
    AssertEquals('0.1', '0.1000', RFactorText(0.099999));
    AssertEquals('0.001', '0.001000', RFactorText(0.00099999));
end;

procedure TRFactorTextTest.ZeroIsZero;
begin
    AssertEquals('0', '0.000', RFactorText(0));
end;

procedure TRFactorTextTest.AHugeValueDoesNotFillThePanel;
begin
    //  A sum-of-squares loss on raw counts runs to millions.
    AssertEquals('1e7', '1.235E7', RFactorText(12345678));
end;

procedure TRFactorTextTest.TheTextParsesBackToTheValue;
var
    V: double;
begin
    //  The project file reads the reported text back as a number.
    AssertTrue('parses', TryStrToFloat(RFactorText(0.0000056841), V));
    AssertEquals('to the value shown', 5.684E-6, V, 1E-12);
end;

initialization
    RegisterTest('unit', TRFactorTextTest);
end.
