// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A number as a user typed it.)

EVERY BOX THAT TAKES A NUMBER asks the same two questions - what a full stop
means on a keyboard laid out for a comma, and what happens when what was typed is
not a number - and they were answered separately at each box. One of the answers
swapped the process-wide decimal separator around a call to `StrToFloat`, which
raises: on a typo the separator was never put back, and the exception reached the
top-level handler, which logs at Fatal and stops the server poll.

So a typo in the wavelength box left the whole application reading numbers with
the wrong separator AND disconnected the user from the compute server. Neither is
visible in the line that caused it.
}
unit testcase_typed_number;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, typed_number;

type
    TTypedNumberTest = class(TTestCase)
    published
        //  Reading.
        procedure AWholeNumberIsRead;
        procedure AFractionIsRead;
        procedure ANegativeNumberIsRead;
        procedure ExponentialFormIsRead;
        procedure SurroundingSpaceIsIgnored;

        //  The separator.
        procedure AFullStopIsTheSeparatorWhateverTheLocale;
        procedure ACommaIsNotASeparator;
        procedure TheProcessWideSeparatorIsNotTouched;

        //  Refusing.
        procedure ATypoIsRefusedRatherThanRaised;
        procedure AnEmptyBoxIsRefused;
        procedure ALetterIsRefused;
        procedure AValueWithUnitsIsRefused;
        procedure ARefusedValueIsZeroNotRubbish;

        //  Falling back.
        procedure AReadableValueIsUsed;
        procedure AnUnreadableValueFallsBack;

        //  Positivity.
        procedure APositiveNumberIsPositive;
        procedure ZeroIsNot;
        procedure ANegativeNumberIsNot;
        procedure ATypoIsNot;
    end;

implementation

{ ---- reading --------------------------------------------------------------- }

procedure TTypedNumberTest.AWholeNumberIsRead;
var
    V: double;
begin
    AssertTrue('read', TryTypedNumber('42', V));
    AssertEquals('the value', 42.0, V, 1E-12);
end;

procedure TTypedNumberTest.AFractionIsRead;
var
    V: double;
begin
    //  1.5406 angstrom - the copper K-alpha wavelength, and the number this box
    //  exists to take.
    AssertTrue('read', TryTypedNumber('1.5406', V));
    AssertEquals('the value', 1.5406, V, 1E-12);
end;

procedure TTypedNumberTest.ANegativeNumberIsRead;
var
    V: double;
begin
    //  READ, though most boxes will then refuse it. Reading and judging are
    //  different jobs, and folding them together means every box that legally
    //  takes a negative has to parse for itself.
    AssertTrue('read', TryTypedNumber('-3.25', V));
    AssertEquals('the value', -3.25, V, 1E-12);
end;

procedure TTypedNumberTest.ExponentialFormIsRead;
var
    V: double;
begin
    //  What this program's own JSON writes doubles as, so a value pasted back
    //  from a reply has to be readable.
    AssertTrue('read', TryTypedNumber('3.75E-001', V));
    AssertEquals('the value', 0.375, V, 1E-12);
end;

procedure TTypedNumberTest.SurroundingSpaceIsIgnored;
var
    V: double;
begin
    //  What a paste produces.
    AssertTrue('read', TryTypedNumber('  1.5  ', V));
    AssertEquals('the value', 1.5, V, 1E-12);
end;

{ ---- the separator --------------------------------------------------------- }

procedure TTypedNumberTest.AFullStopIsTheSeparatorWhateverTheLocale;
var
    Saved: char;
    V: double;
begin
    //  EVERY formula, data file and wire format this program touches writes
    //  numbers with a full stop. A box that followed the user's locale would
    //  accept a value the rest of the program then reads as a different one.
    Saved := DefaultFormatSettings.DecimalSeparator;
    try
        DefaultFormatSettings.DecimalSeparator := ',';
        AssertTrue('still read', TryTypedNumber('1.5406', V));
        AssertEquals('and still the same value', 1.5406, V, 1E-12);
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
end;

procedure TTypedNumberTest.ACommaIsNotASeparator;
var
    V: double;
begin
    //  Refused rather than read as something else. '1,5' parsed as fifteen is
    //  the failure this program cannot afford: it is a plausible wavelength.
    AssertFalse('refused', TryTypedNumber('1,5', V));
end;

procedure TTypedNumberTest.TheProcessWideSeparatorIsNotTouched;
var
    Saved: char;
    V: double;
begin
    //  THE DEFECT THIS REPLACED. The old code assigned the global separator,
    //  called something that raises, and restored it afterwards - so a typo
    //  left the whole application reading numbers with a separator it had not
    //  asked for, for the rest of the session.
    Saved := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := ',';
    try
        TryTypedNumber('1.5', V);
        TryTypedNumber('rubbish', V);
        AssertEquals('unchanged after a good read and a bad one', ',',
            DefaultFormatSettings.DecimalSeparator);
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
end;

{ ---- refusing -------------------------------------------------------------- }

procedure TTypedNumberTest.ATypoIsRefusedRatherThanRaised;
var
    V: double;
    Raised: boolean;
begin
    //  A FUNCTION THAT RAISES makes every caller wrap it, and the one that did
    //  not wrapped it in a menu handler - where the exception reached the
    //  top-level handler, was logged as fatal, and stopped the server poll. A
    //  typo in a text box should not disconnect the user from the server.
    Raised := False;
    try
        AssertFalse('refused', TryTypedNumber('1..5', V));
    except
        on Exception do
            Raised := True;
    end;
    AssertFalse('and nothing was raised', Raised);
end;

procedure TTypedNumberTest.AnEmptyBoxIsRefused;
var
    V: double;
begin
    //  A dialog accepted with nothing typed. Reading it as zero would set a
    //  wavelength of zero, which is what "not set" already means.
    AssertFalse('refused', TryTypedNumber('', V));
    AssertFalse('and whitespace too', TryTypedNumber('   ', V));
end;

procedure TTypedNumberTest.ALetterIsRefused;
var
    V: double;
begin
    AssertFalse('refused', TryTypedNumber('abc', V));
end;

procedure TTypedNumberTest.AValueWithUnitsIsRefused;
var
    V: double;
begin
    //  What a paste from a paper or a spreadsheet cell looks like.
    AssertFalse('refused', TryTypedNumber('1.5406 A', V));
end;

procedure TTypedNumberTest.ARefusedValueIsZeroNotRubbish;
var
    V: double;
begin
    //  An out parameter left as whatever the stack held is how a refusal
    //  becomes a plausible number in a caller that forgot to check.
    V := 999;
    AssertFalse('refused', TryTypedNumber('abc', V));
    AssertEquals('and cleared', 0.0, V, 1E-12);
end;

{ ---- falling back ---------------------------------------------------------- }

procedure TTypedNumberTest.AReadableValueIsUsed;
begin
    AssertEquals('read', 2.5, TypedNumberOr('2.5', 99), 1E-12);
end;

procedure TTypedNumberTest.AnUnreadableValueFallsBack;
begin
    //  For the boxes where an empty entry has a meaning - and only those. The
    //  wavelength is not one of them.
    AssertEquals('the default', 99.0, TypedNumberOr('abc', 99), 1E-12);
    AssertEquals('and for an empty box', 99.0, TypedNumberOr('', 99), 1E-12);
end;

{ ---- positivity ------------------------------------------------------------ }

procedure TTypedNumberTest.APositiveNumberIsPositive;
begin
    AssertTrue('positive', TypedNumberIsPositive('1.5406'));
end;

procedure TTypedNumberTest.ZeroIsNot;
begin
    //  Zero is what "no wavelength" already means to the client, so accepting it
    //  from the box would silently do nothing at all.
    AssertFalse('zero', TypedNumberIsPositive('0'));
    AssertFalse('and zero written out', TypedNumberIsPositive('0.000'));
end;

procedure TTypedNumberTest.ANegativeNumberIsNot;
begin
    AssertFalse('negative', TypedNumberIsPositive('-1.5'));
end;

procedure TTypedNumberTest.ATypoIsNot;
begin
    //  Unreadable and non-positive have to give the same answer here, or a
    //  caller checking only this one lets a typo through.
    AssertFalse('a typo', TypedNumberIsPositive('abc'));
    AssertFalse('and an empty box', TypedNumberIsPositive(''));
end;

initialization
    //  A unit test: text in, a number and a verdict out.
    RegisterTest('unit', TTypedNumberTest);
end.
