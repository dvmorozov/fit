// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The custom axis dialog's four boxes, answered when nobody is there.)

WHY. Custom Position, Custom Value and View > R-factor Scale > Custom R-factor
ask for a name, a unit and two formulas in a dialog of their own, and with a run
nobody watches that dialog stopped the run: nothing could answer it. So the
window's own check-ui could not choose a custom scale either, and its captions
and what choosing one does to the chart went unchecked. Each box is now a
question with an id, answered through answer_source like every other question
the window asks - the same seam a script uses.

A BOX NOT ANSWERED KEEPS WHAT IT SHOWS, as an untouched box in the window does
(OptionalAnswer): the dialog opens on the axis in force, or on the identity on a
first use, so every box has something in it. What the answers make is then
judged by the same rule the window's OK is (custom_axis.CustomAxisProblem).
}
unit custom_axis_answers;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, answer_source, custom_axis;

const
    { The id of each box, in the order the dialog shows them. }
    CustomAxisQuestion: array[TCustomAxisField] of string = (
        'custom-axis.name', 'custom-axis.unit', 'custom-axis.forward',
        'custom-axis.inverse');

{ What the dialog would hand back, had the run typed its answers into it: each
  answered box replaced, the rest as AShown has them, and whitespace trimmed as
  the window's OK trims it. }
function AnsweredCustomAxis(
    const AShown: TCustomAxisDefinition): TCustomAxisDefinition;

implementation

function AnsweredCustomAxis(
    const AShown: TCustomAxisDefinition): TCustomAxisDefinition;
var
    Value: string;
begin
    Result := AShown;
    if OptionalAnswer(CustomAxisQuestion[cafName], Value) then
        Result.Name := Value;
    if OptionalAnswer(CustomAxisQuestion[cafUnit], Value) then
        Result.Units := Value;
    if OptionalAnswer(CustomAxisQuestion[cafForward], Value) then
        Result.Forward_ := Value;
    if OptionalAnswer(CustomAxisQuestion[cafInverse], Value) then
        Result.Inverse := Value;
    Result := TrimmedCustomAxis(Result);
end;

initialization
    RegisterQuestion(CustomAxisQuestion[cafName], qkText,
        'A custom axis or R-factor scale (Custom Position, Custom Value or ' +
        'View > R-factor Scale > Custom R-factor): the name the chart shows.');
    RegisterQuestion(CustomAxisQuestion[cafUnit], qkText,
        'A custom axis or R-factor scale: its unit, shown after the name.');
    RegisterQuestion(CustomAxisQuestion[cafForward], qkText,
        'A custom axis or R-factor scale: f(x), the value shown, as a formula ' +
        'of the stored value x - ln(x), say.');
    RegisterQuestion(CustomAxisQuestion[cafInverse], qkText,
        'A custom axis or R-factor scale: g(x), the inverse of f(x) - exp(x) ' +
        'for ln(x). Each box not answered keeps what the dialog shows.');
end.
