// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What editing a cell of the profile table means.)

THE ONE PLACE DATA IS TYPED IN. The profile grid is editable, and what the user
types there replaces a measured point. Three decisions govern it, and all three
lived inside a grid's editing-done handler:

  * a cell counts as filled in once its text DIFFERS from what was in it when
    editing began - which is how the row knows the user has been through it;
  * the point is replaced only when EVERY data cell of the row is filled in, so
    that a half-typed row does not move a point to a position the user has not
    finished stating;
  * the text becomes a number through StrToFloatDef with a default of zero.

THE THIRD ONE IS A TRAP AND IS KEPT. A cell holding anything that is not a
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

{ True when every data cell of the row has been filled in.

  ALL OF THEM, because a point is a pair. Applying a row with only its abscissa
  entered would move the point to an ordinate the user has not stated. }
function RowIsComplete(const AFilled: array of boolean): boolean;

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

function RowIsComplete(const AFilled: array of boolean): boolean;
var
    i: longint;
begin
    //  An empty row is not complete: with no cells there is nothing the user
    //  stated, and reporting completeness would apply an edit nobody made.
    Result := Length(AFilled) > 0;
    for i := 0 to High(AFilled) do
        if not AFilled[i] then
            Exit(False);
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
