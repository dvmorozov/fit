// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a formula editor's keypad does to the text and the caret.)

THE FORMULA IS THE MODEL. A user-defined curve type is nothing but the expression
the user types, and the calculator-style keypad beside the edit box is how it
gets written. Where the caret lands after each press decides what the next press
appends to - `Sin()` with the caret after the closing bracket turns the next
digit into `Sin()7` instead of `Sin(7)`, and the user gets a formula they did not
write and a parse error naming the whole expression.

Forty button handlers call one of two three-line methods, so a mistake in either
is a mistake in every button - and both lived inside a `with EditExpression do`
in an LCL dialog, where nothing could reach them.
}
unit testcase_formula_editing;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, formula_editing;

type
    TFormulaEditingTest = class(TTestCase)
    published
        //  Inserting a function.
        procedure AFunctionIsInsertedWithItsBrackets;
        procedure TheCaretLandsBetweenTheBrackets;
        procedure TwoFunctionsNestWhenPressedInTurn;
        procedure AFunctionAtTheStartOfTheTextGoesFirst;
        procedure AFunctionInTheMiddleSplitsTheText;
        procedure AFunctionAtTheEndAppends;

        //  Inserting a symbol.
        procedure ASymbolIsInsertedWhereTheCaretIs;
        procedure TheCaretLandsAfterTheSymbol;
        procedure DigitsPressedInTurnBuildANumber;
        procedure AMultiCharacterSymbolMovesTheCaretPastAllOfIt;

        //  A selection.
        procedure ASelectionIsReplacedBySymbol;
        procedure ASelectionIsReplacedByAnEmptyCall;

        //  Nonsense from the control.
        procedure ACaretPastTheEndIsClampedToIt;
        procedure ANegativeCaretIsTreatedAsTheStart;
        procedure ASelectionRunningPastTheEndIsTruncated;
        procedure ANegativeSelectionLengthSelectsNothing;
        procedure InsertingIntoAnEmptyBoxWorks;

        //  What a typed character becomes.
        procedure ACommaBecomesAFullStop;
        procedure EveryOtherCharacterIsItself;
    end;

implementation

{ ---- inserting a function -------------------------------------------------- }

procedure TFormulaEditingTest.AFunctionIsInsertedWithItsBrackets;
var
    R: TEditState;
begin
    //  The brackets come with the name, because a function without them is not
    //  an expression and the user would have to type both by hand every time.
    R := InsertFunction(EditState('', 0), 'Sin');
    AssertEquals('name and brackets', 'Sin()', R.Text);
end;

procedure TFormulaEditingTest.TheCaretLandsBetweenTheBrackets;
var
    R: TEditState;
begin
    //  THE WHOLE CONVENIENCE THE KEYPAD OFFERS. One character out and the next
    //  press lands outside the call.
    R := InsertFunction(EditState('', 0), 'Sin');
    AssertEquals('after Sin(', 4, R.SelStart);
end;

procedure TFormulaEditingTest.TwoFunctionsNestWhenPressedInTurn;
var
    R: TEditState;
begin
    //  Press Sin, press Cos: the second lands inside the first, which is what
    //  the caret placement is for. This is the assertion that would fail if the
    //  offset were computed from the wrong end.
    R := InsertFunction(EditState('', 0), 'Sin');
    R := InsertFunction(R, 'Cos');
    AssertEquals('nested', 'Sin(Cos())', R.Text);
    AssertEquals('and the caret is inside the inner call', 8, R.SelStart);
end;

procedure TFormulaEditingTest.AFunctionAtTheStartOfTheTextGoesFirst;
var
    R: TEditState;
begin
    R := InsertFunction(EditState('x+1', 0), 'Abs');
    AssertEquals('prepended', 'Abs()x+1', R.Text);
end;

procedure TFormulaEditingTest.AFunctionInTheMiddleSplitsTheText;
var
    R: TEditState;
begin
    //  The caret is between the '+' and the '1'.
    R := InsertFunction(EditState('x+1', 2), 'Abs');
    AssertEquals('split', 'x+Abs()1', R.Text);
    AssertEquals('caret inside the call', 6, R.SelStart);
end;

procedure TFormulaEditingTest.AFunctionAtTheEndAppends;
var
    R: TEditState;
begin
    R := InsertFunction(EditState('x+', 2), 'Ln');
    AssertEquals('appended', 'x+Ln()', R.Text);
    AssertEquals('caret inside', 5, R.SelStart);
end;

{ ---- inserting a symbol ---------------------------------------------------- }

procedure TFormulaEditingTest.ASymbolIsInsertedWhereTheCaretIs;
var
    R: TEditState;
begin
    R := InsertSymbol(EditState('x1', 1), '+');
    AssertEquals('between them', 'x+1', R.Text);
end;

procedure TFormulaEditingTest.TheCaretLandsAfterTheSymbol;
var
    R: TEditState;
begin
    //  After, not before: an operator pressed twice would otherwise come out
    //  reversed, and a digit pressed twice would build the number backwards.
    R := InsertSymbol(EditState('x1', 1), '+');
    AssertEquals('past the plus', 2, R.SelStart);
end;

procedure TFormulaEditingTest.DigitsPressedInTurnBuildANumber;
var
    R: TEditState;
begin
    //  Three presses of the keypad. Reversed digits is what a caret left before
    //  the insertion produces, and '321' looks exactly as much like a number as
    //  '123' does.
    R := InsertSymbol(EditState('', 0), '1');
    R := InsertSymbol(R, '2');
    R := InsertSymbol(R, '3');
    AssertEquals('in order', '123', R.Text);
end;

procedure TFormulaEditingTest.AMultiCharacterSymbolMovesTheCaretPastAllOfIt;
var
    R: TEditState;
begin
    //  The brackets button inserts two characters.
    R := InsertSymbol(EditState('x', 1), '()');
    AssertEquals('inserted', 'x()', R.Text);
    AssertEquals('past both', 3, R.SelStart);
end;

{ ---- a selection ----------------------------------------------------------- }

procedure TFormulaEditingTest.ASelectionIsReplacedBySymbol;
var
    R: TEditState;
begin
    //  Select '+1' and press '-': the selection goes, as it does in any editor.
    R := InsertSymbol(EditState('x+1', 1, 2), '-');
    AssertEquals('replaced', 'x-', R.Text);
    AssertEquals('caret after it', 2, R.SelStart);
end;

procedure TFormulaEditingTest.ASelectionIsReplacedByAnEmptyCall;
var
    R: TEditState;
begin
    //  ASSERTED AS IT BEHAVES. Selecting a sub-expression and pressing a
    //  function looks as though it should wrap the selection, and it does not -
    //  the selection is replaced by an empty call and what was selected is lost.
    //  Wrapping would be a better editor and is a change to make deliberately,
    //  not inside an extraction.
    R := InsertFunction(EditState('x+1', 0, 3), 'Sin');
    AssertEquals('the selection is gone', 'Sin()', R.Text);
end;

{ ---- nonsense from the control --------------------------------------------- }

procedure TFormulaEditingTest.ACaretPastTheEndIsClampedToIt;
var
    R: TEditState;
begin
    //  A control can report a caret past the text after the text was replaced
    //  from elsewhere. Inserting at a position that does not exist would either
    //  fault or silently drop the insertion.
    R := InsertSymbol(EditState('ab', 99), '+');
    AssertEquals('appended', 'ab+', R.Text);
end;

procedure TFormulaEditingTest.ANegativeCaretIsTreatedAsTheStart;
var
    R: TEditState;
begin
    //  -1 is what an edit box with no focus reports on some widget sets.
    R := InsertSymbol(EditState('ab', -1), '+');
    AssertEquals('prepended', '+ab', R.Text);
end;

procedure TFormulaEditingTest.ASelectionRunningPastTheEndIsTruncated;
var
    R: TEditState;
begin
    R := InsertSymbol(EditState('abc', 1, 99), 'X');
    AssertEquals('to the end', 'aX', R.Text);
end;

procedure TFormulaEditingTest.ANegativeSelectionLengthSelectsNothing;
var
    R: TEditState;
begin
    R := InsertSymbol(EditState('abc', 1, -5), 'X');
    AssertEquals('nothing replaced', 'aXbc', R.Text);
end;

procedure TFormulaEditingTest.InsertingIntoAnEmptyBoxWorks;
var
    R: TEditState;
begin
    //  The state every formula starts from.
    R := InsertSymbol(EditState('', 0), 'x');
    AssertEquals('the first character', 'x', R.Text);
    AssertEquals('and the caret after it', 1, R.SelStart);
end;

{ ---- what a typed character becomes ---------------------------------------- }

procedure TFormulaEditingTest.ACommaBecomesAFullStop;
begin
    //  ALWAYS. The parser reads numbers with a full stop whatever the locale is,
    //  and a keyboard laid out for a comma-decimal locale puts a comma on the
    //  numeric keypad - so without this, a user typing on their own keyboard
    //  gets a formula that does not parse and an error naming the expression
    //  rather than the key they pressed.
    AssertEquals('converted', '.', TypedCharacter(','));
end;

procedure TFormulaEditingTest.EveryOtherCharacterIsItself;
var
    C: char;
begin
    //  The filter must touch nothing else: a keystroke silently changed is
    //  worse than one refused.
    for C := ' ' to '~' do
        if C <> ',' then
            AssertEquals('character ' + C + ' is unchanged', C,
                TypedCharacter(C));
end;

initialization
    //  A unit test: three values in, three values out. No dialog and no edit box.
    RegisterTest('unit', TFormulaEditingTest);
end.
