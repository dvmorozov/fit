// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What editing a cell of the profile table means.)

THE ONE PLACE DATA IS TYPED IN. The profile grid is editable, and what the user
types there replaces a measured point. Three decisions govern it, and all three
lived inside a grid's editing-done handler:

  * a cell counts as filled in once its text DIFFERS from what was in it when
    editing began - which is how the row knows the user has been through it;
  * a point whose either cell changed is moved - the unchanged cell states the
    rest - while a row typed from nothing waits for both of its cells;
  * the text becomes a number through StrToFloatDef with a default of zero.

THE THIRD ONE WAS A TRAP, and RowEditRefusal now closes it. A cell holding anything that is not a
number - a typo, a stray letter, a value pasted with its units - reads as zero,
and the point moves to the origin without a word. The default exists to handle an
EMPTY cell, which is a different thing and a legitimate one. Both are pinned
below, and the difference between them is now something a caller can ask about
rather than something buried in a call to StrToFloatDef.
}
unit grid_edit;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

{ True when editing changed the cell, which is what marks it filled in.

  Comparing TEXT rather than value: '1.0' and '1.00' are the same number and the
  user did type something, and a row whose cells never mark themselves filled is
  a row whose edits are never applied. }
function CellWasEdited(const ASavedText, ANewText: string): boolean;

{ Whether an edit of a point's row is to be applied, given the row's two cells
  as they were when editing began and as they are now.

  A POINT THAT EXISTS IS FULLY STATED: changing one of its cells moves it, the
  other cell saying the rest. The earlier rule - apply only once every cell has
  been edited - made the ordinary correction, one amplitude, do nothing while
  the table showed the new number. A row being typed from nothing still waits
  for both halves. }
function RowEditApplies(const ASavedX, ASavedY, ANewX, ANewY: string): boolean;

{ Why a row whose new cells are ANewX and ANewY cannot be applied, in words
  naming what was typed, or '' when both are numbers.

  THE TRAP IS CLOSED HERE. EditedValue reads anything that is not a number as
  zero, and the edit handler used to apply that zero - a typo moved the point to
  the origin without a word. A row with such a cell is refused instead. }
function RowEditRefusal(const ANewX, ANewY: string): string;

{ The number a cell's text stands for. Zero for anything unreadable - see the
  unit comment. }
function EditedValue(const AText: string): double;

{ Whether the text is a number at all.

  Not consulted by the grid handler today: it is here so that the difference
  between "empty" and "nonsense" is askable, which is the first thing anyone
  fixing the silent zero will need. }
function EditedValueIsReadable(const AText: string): boolean;

{ True when the cell is empty - blank or whitespace. Distinguished from
  unreadable because an empty cell is a legitimate state of a row being typed
  and a stray letter is not. }
function EditedValueIsEmpty(const AText: string): boolean;

implementation

function CellWasEdited(const ASavedText, ANewText: string): boolean;
begin
    Result := ASavedText <> ANewText;
end;

function RowEditApplies(const ASavedX, ASavedY, ANewX, ANewY: string): boolean;
begin
    Result := (CellWasEdited(ASavedX, ANewX) or CellWasEdited(ASavedY, ANewY)) and
        not EditedValueIsEmpty(ANewX) and not EditedValueIsEmpty(ANewY);
end;

function RowEditRefusal(const ANewX, ANewY: string): string;
begin
    Result := '';
    if not EditedValueIsReadable(ANewX) then
        Result := '"' + Trim(ANewX) + '" is not a number, so the point was ' +
            'not moved. Type the position as a number.'
    else if not EditedValueIsReadable(ANewY) then
        Result := '"' + Trim(ANewY) + '" is not a number, so the point was ' +
            'not moved. Type the amplitude as a number.';
end;

function EditedValueIsEmpty(const AText: string): boolean;
begin
    Result := Trim(AText) = '';
end;

function EditedValueIsReadable(const AText: string): boolean;
var
    Value: double;
begin
    Result := (not EditedValueIsEmpty(AText)) and
        TryStrToFloat(Trim(AText), Value);
end;

function EditedValue(const AText: string): double;
begin
    Result := StrToFloatDef(AText, 0);
end;

end.
