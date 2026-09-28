// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for how a number is written into an exported table.

  WHY THESE EXIST. The export wrote the text the table shows: four decimals,
  so 0.000012 became 0.0000 and a fitted width lost the digits a publication
  needs. An exported number is written so that reading it back gives the
  same double, and with a full stop whatever the machine's locale, because a
  file travels to machines whose locale is another. }
unit testcase_table_text;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, table_text;

type
    TTableTextTest = class(TTestCase)
    private
        function ReadBack(const AText: string): double;
    published
        procedure AThirdReadsBackExactly;
        procedure ATinyValueKeepsItsDigits;
        procedure AShortValueIsWrittenShort;
        procedure TheDecimalMarkIsAFullStopWhateverTheLocale;
    end;

implementation

function TTableTextTest.ReadBack(const AText: string): double;
var
    F: TFormatSettings;
begin
    F := DefaultFormatSettings;
    F.DecimalSeparator := '.';
    Result := StrToFloat(AText, F);
end;

procedure TTableTextTest.AThirdReadsBackExactly;
var
    Third: double;
begin
    //  A double, not the literal: 1 / 3 untyped is extended (AGENTS.md).
    Third := 1 / 3;
    AssertTrue(ExportedNumber(Third), ReadBack(ExportedNumber(Third)) = Third);
end;

procedure TTableTextTest.ATinyValueKeepsItsDigits;
var
    Tiny: double;
begin
    Tiny := 0.0000123456789;
    AssertTrue(ExportedNumber(Tiny), ReadBack(ExportedNumber(Tiny)) = Tiny);
end;

procedure TTableTextTest.AShortValueIsWrittenShort;
begin
    //  Exact is not the same as long: 0.1 read back from '0.1' is the same
    //  double, and seventeen digits of it would only be noise to a reader.
    AssertEquals('0.1', ExportedNumber(0.1));
    AssertEquals('773', ExportedNumber(773));
end;

procedure TTableTextTest.TheDecimalMarkIsAFullStopWhateverTheLocale;
var
    Saved: char;
begin
    Saved := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := ',';
    try
        AssertEquals('2.5', ExportedNumber(2.5));
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
end;

initialization
    RegisterTest('unit', TTableTextTest);
end.
