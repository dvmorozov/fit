// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a formula editor's buttons do to the text and the caret.)

THE FORMULA IS THE MODEL. A user-defined curve type is nothing but the expression
the user types, so the calculator-style keypad beside the edit box - forty-odd
buttons inserting a function name, an operator or a digit - is how that model gets
written. Where the caret lands after each insertion decides what the next button
appends to, and a caret one character out silently builds a different formula:
`Sin()` with the caret after the closing bracket turns the next digit into
`Sin()7` rather than `Sin(7)`.

None of it could be tested. Both insertions were three lines inside a `with
EditExpression do` in an LCL dialog, and the forty button handlers each call one
of them - so a mistake in either is a mistake in every button.

WHAT AN EDIT BOX IS, HERE. Text, a caret position, and how much is selected. The
dialog reads those three off the control, asks for the result, and writes them
back; nothing in this unit knows what a `TEdit` is.
}
unit formula_editing;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { The three things about an edit box that an insertion depends on and
      changes. SelStart is zero-based, as the control reports it. }
    TEditState = record
        Text: string;
        SelStart: longint;
        SelLength: longint;
    end;

{ An edit box holding AText with the caret at ASelStart and ASelLength
  characters selected. }
function EditState(const AText: string;
    ASelStart: longint; ASelLength: longint = 0): TEditState;

{ Inserts a function call and leaves the caret BETWEEN ITS BRACKETS, which is
  where the argument goes - that placement is the whole convenience the keypad
  offers, and without it every function press needs a manual click to fix.

  Whatever was selected is replaced, so selecting a sub-expression and pressing
  a function wraps it... except that it does not: the selection is replaced by
  an empty call, and what was selected is gone. That is the behaviour as it
  stands and it is pinned as such; wrapping a selection would be a better
  editor and is a change to make deliberately. }
function InsertFunction(const AState: TEditState;
    const AName: string): TEditState;

{ Inserts a literal - an operator, a digit, a bracket - and leaves the caret
  after it. }
function InsertSymbol(const AState: TEditState;
    const ASymbol: string): TEditState;

{ What a typed character becomes.

  A COMMA BECOMES A FULL STOP, always. The expression parser reads numbers with
  a full stop whatever the user's locale is, and a keyboard laid out for a
  comma-decimal locale puts a comma on the numeric keypad - so without this the
  formula a user types on their own keyboard does not parse, and the error names
  the expression rather than the key. }
function TypedCharacter(AKey: char): char;

implementation

function EditState(const AText: string;
    ASelStart: longint; ASelLength: longint = 0): TEditState;
begin
    Result.Text := AText;
    Result.SelStart := ASelStart;
    Result.SelLength := ASelLength;
end;

{ Replaces the selection with AInsert and returns the new text, leaving the
  caret at ACaretOffset characters past where the selection began.

  Clamped at both ends: a caret reported past the end of the text, or a
  selection running past it, must not produce a Copy with a negative length or
  an insertion at a position that does not exist. }
function ReplaceSelection(const AState: TEditState;
    const AInsert: string; ACaretOffset: longint): TEditState;
var
    Start, Len: longint;
begin
    Start := AState.SelStart;
    if Start < 0 then
        Start := 0;
    if Start > Length(AState.Text) then
        Start := Length(AState.Text);

    Len := AState.SelLength;
    if Len < 0 then
        Len := 0;
    if Start + Len > Length(AState.Text) then
        Len := Length(AState.Text) - Start;

    //  Copy is one-based; SelStart is not.
    Result.Text := Copy(AState.Text, 1, Start) + AInsert +
        Copy(AState.Text, Start + Len + 1, MaxInt);
    Result.SelStart := Start + ACaretOffset;
    Result.SelLength := 0;
end;

function InsertFunction(const AState: TEditState;
    const AName: string): TEditState;
begin
    //  Name + '()' , caret between the brackets: past the name and past the
    //  opening bracket, which is Length(AName) + 1.
    Result := ReplaceSelection(AState, AName + '()', Length(AName) + 1);
end;

function InsertSymbol(const AState: TEditState;
    const ASymbol: string): TEditState;
begin
    Result := ReplaceSelection(AState, ASymbol, Length(ASymbol));
end;

function TypedCharacter(AKey: char): char;
begin
    if AKey = ',' then
        Result := '.'
    else
        Result := AKey;
end;

end.
