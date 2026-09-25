// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The numbers on one line of JCAMP-DX data, whichever way they are written.)

JCAMP-DX WRITES NUMBERS IN FOUR WAYS, and a reader that understands only the
plain one reads a third of the files in existence and silently mis-reads some of
the rest. All four are here because they are one decision - what does this
character begin? - and separating them would mean deciding it twice:

  AFFN  ordinary numbers separated by spaces:      450.0 221 151 188
  PAC   packed, the sign IS the separator:         450.0+221+151-188
  SQZ   the first digit carries the sign:          450.0C21A51 - @=0, A..I=1..9,
        a..i=-1..-9, so 'C21' is 321 and 'a51' is -151
  DIF   each value is a DIFFERENCE from the one before it: %=0, J..R=+1..+9,
        j..r=-1..-9
  DUP   the previous value again, that many times: S..Z and s mean 1..9

WHY THE DECODER IS ITS OWN UNIT. It is a rule over a string with no file, no
header and no spectrum in it, so every form above is reachable from a test with
a string literal - and the forms that matter most are the ones no sample file in
this repository happens to use.

THE PLAIN NUMBER IS STILL text_numbers' JOB: this unit decides where a token
starts and what its first character means, and hands AFFN runs to the framework's
scanner rather than keeping a second opinion about exponents and decimal marks.
}
unit jcamp_asdf;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TJcampValues = array of double;

{ The values on one data line, decoded. ADifferenceRun says whether the line
  ENDED inside a run of differences, which the caller needs because a DIF-coded
  file repeats the last value of a line as the first value of the next one as a
  check - and counting it twice shifts the whole spectrum. }
function DecodeJcampLine(const ALine: string;
    out AEndedInDifference: boolean): TJcampValues;

{ What one ASDF character means, exposed so the table itself can be tested
  rather than inferred from decoded lines. Returns False for a character that
  begins no number. }
function AsdfDigit(AChar: char; out AValue: longint; out AIsDifference,
    AIsDuplicate: boolean): boolean;

implementation

uses
    text_numbers;

function AsdfDigit(AChar: char; out AValue: longint; out AIsDifference,
    AIsDuplicate: boolean): boolean;
begin
    AValue := 0;
    AIsDifference := False;
    AIsDuplicate := False;
    Result := True;
    case AChar of
        '@': AValue := 0;
        'A'..'I': AValue := Ord(AChar) - Ord('A') + 1;
        'a'..'i': AValue := -(Ord(AChar) - Ord('a') + 1);
        '%': begin AValue := 0; AIsDifference := True; end;
        'J'..'R': begin AValue := Ord(AChar) - Ord('J') + 1; AIsDifference := True; end;
        'j'..'r': begin AValue := -(Ord(AChar) - Ord('j') + 1); AIsDifference := True; end;
        'S'..'Z': begin AValue := Ord(AChar) - Ord('S') + 1; AIsDuplicate := True; end;
        's': begin AValue := 9; AIsDuplicate := True; end;
        else
            Result := False;
    end;
end;

{ Whether AChar can START a plain number. '+' and '-' can, which is what makes
  PAC work: the sign both separates and belongs to what follows. }
function StartsPlainNumber(AChar: char): boolean;
begin
    Result := ((AChar >= '0') and (AChar <= '9')) or (AChar = '+') or
        (AChar = '-') or (AChar = '.');
end;

function DecodeJcampLine(const ALine: string;
    out AEndedInDifference: boolean): TJcampValues;
var
    i, Len, Start, Digit, Count: longint;
    Token: string;
    Value, Previous, LastDifference: double;
    IsDifference, IsDuplicate, WasDifference: boolean;
    Numbers: TLineNumbers;
    Valid: boolean;

    procedure Emit(AValue: double);
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := AValue;
        Previous := AValue;
    end;

begin
    Result := nil;
    AEndedInDifference := False;
    WasDifference := False;
    Previous := 0;
    LastDifference := 0;
    Len := Length(ALine);
    i := 1;
    while i <= Len do
    begin
        //  SPACE, COMMA, SEMICOLON and TAB all separate values, and a peak
        //  table writes its pairs as '39,100 50,20' - so a reader that knew
        //  only the space read the first number of the line and stopped.
        if (ALine[i] = ' ') or (ALine[i] = ',') or (ALine[i] = ';') or
            (ALine[i] = #9) then
        begin
            Inc(i);
            Continue;
        end;

        if AsdfDigit(ALine[i], Digit, IsDifference, IsDuplicate) then
        begin
            //  The first character carries the sign (and, for DIF and DUP, the
            //  meaning); the digits after it are ordinary.
            Inc(i);
            Token := '';
            while (i <= Len) and (ALine[i] >= '0') and (ALine[i] <= '9') do
            begin
                Token := Token + ALine[i];
                Inc(i);
            end;
            if IsDuplicate then
            begin
                //  A DUP COUNT INCLUDES THE VALUE IT REPEATS: 'W' after one
                //  value means five in all, so four more are emitted. Getting
                //  this off by one shifts every later point of the spectrum
                //  and still draws a plausible curve.
                Count := StrToIntDef(IntToStr(Digit) + Token, Digit);
                while Count > 1 do
                begin
                    if WasDifference then
                        //  Inside a run of differences the repeat is of the
                        //  DIFFERENCE, not of the value: that is what makes a
                        //  flat stretch compress to two characters.
                        Emit(Previous + LastDifference)
                    else
                        Emit(Previous);
                    Dec(Count);
                end;
                Continue;
            end;

            Value := StrToFloatDef(IntToStr(Abs(Digit)) + Token, Abs(Digit));
            if Digit < 0 then
                Value := -Value;
            if IsDifference then
            begin
                LastDifference := Value;
                Emit(Previous + Value);
                WasDifference := True;
            end
            else
            begin
                Emit(Value);
                WasDifference := False;
            end;
            Continue;
        end;

        if StartsPlainNumber(ALine[i]) then
        begin
            Start := i;
            if (ALine[i] = '+') or (ALine[i] = '-') then
                Inc(i);
            while (i <= Len) and (((ALine[i] >= '0') and (ALine[i] <= '9')) or
                (ALine[i] = '.') or (ALine[i] = 'e') or (ALine[i] = 'E') or
                (((ALine[i] = '+') or (ALine[i] = '-')) and (i > Start) and
                ((ALine[i - 1] = 'e') or (ALine[i - 1] = 'E')))) do
                Inc(i);
            Token := Copy(ALine, Start, i - Start);
            //  THE FRAMEWORK'S OWN SCANNER decides what a plain number is, so
            //  the exponent and decimal-mark rules have one implementation.
            Numbers := LineNumbers(Token, False, Valid);
            if Valid and (Length(Numbers) = 1) then
            begin
                Emit(Numbers[0]);
                WasDifference := False;
            end;
            Continue;
        end;

        //  Anything else - a comment marker, a stray letter - ends the line's
        //  data. JCAMP puts $$ comments after the numbers.
        Break;
    end;
    AEndedInDifference := WasDifference;
end;

end.
