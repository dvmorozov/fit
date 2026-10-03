// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The user-defined argument axis: what it starts as, and when it is
usable.)

WHAT IT IS. The user can define an axis of their own by giving two formulas in
terms of x: f(x) to display a value, and g(x) to get back from a displayed value
to the one the data holds. Both are needed, because the chart converts in both
directions - a click has to become an abscissa, and an abscissa a position.

WHY THAT IS NOT OBVIOUS TO A USER, and why the seeding matters. The dialog opens
empty on a first use, and an empty pair of boxes gives no clue that what belongs
in them is a formula written in x. Seeding them with the identity - f(x)=x,
g(x)=x - is the whole of the instruction: the user sees what a valid answer looks
like, and the axis it defines is the one they already have, so accepting it
unchanged does nothing surprising.

WHAT IS NOT CHECKED, and is worth knowing. Whether g really inverts f is nobody's
business here: `g(f(x)) = x` could be sampled and is not, so a user who writes
f(x)=ln(x) with g(x)=log10(x) gets an axis that maps positions to the wrong place
in one direction only. It is recorded in findings.md rather than fixed, because
rejecting input the program accepts today is a change to make deliberately.
}
unit custom_axis;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, axis_mode_registry;

type
    { The four things a custom axis is made of. }
    TCustomAxisDefinition = record
        { What the axis is called on the chart. }
        Name: string;
        { Its unit, or '' - not every axis has one. }
        Units: string;
        { f(x): the value the user reads, from the value the data holds. }
        Forward_: string;
        { g(x): back again. }
        Inverse: string;
    end;

    { Why a definition cannot be used. }
    TCustomAxisProblem = (
        capNone,
        { f(x) is missing, so nothing can be displayed. }
        capNoForward,
        { g(x) is missing, so nothing on the chart can be turned back into an
          abscissa - which is what a click is. }
        capNoInverse
        );

{ The definition a first use starts from: the identity, named. }
function DefaultCustomAxis: TCustomAxisDefinition;

{ True when this definition has never been filled in, so the dialog should be
  seeded rather than reopened on what is there. }
function CustomAxisIsUnset(const ADefinition: TCustomAxisDefinition): boolean;

{ Whitespace trimmed from every field, which is what the dialog hands back and
  what a formula parser will not tolerate. }
function TrimmedCustomAxis(
    const ADefinition: TCustomAxisDefinition): TCustomAxisDefinition;

{ Why the definition cannot be used, or capNone. }
function CustomAxisProblem(
    const ADefinition: TCustomAxisDefinition): TCustomAxisProblem;

{ What to tell the user about a problem. Empty for capNone. }
function CustomAxisProblemMessage(AProblem: TCustomAxisProblem): string;

type
    { The four boxes of the Custom axis dialog, in the order they are shown. }
    TCustomAxisField = (cafName, cafUnit, cafForward, cafInverse);

const
    { Always visible at the top of the dialog - more discoverable than tooltips
      alone: what the axis does, and why both a formula and its inverse. }
    CustomAxisIntro =
        'A custom axis only changes how positions are shown — it never ' +
        'changes your data or the fit.' + LineEnding +
        'f(x) converts the stored value x to the value displayed. g(x) is ' +
        'its inverse (displayed value back to x), used when you read or ' +
        'edit positions.';
    CustomAxisFieldCaption: array[TCustomAxisField] of string = (
        'Display name:', 'Unit:', 'Displayed value  f(x):', 'Inverse  g(x):');
    CustomAxisFieldHint: array[TCustomAxisField] of string = (
        'A short label for the axis.',
        'Optional unit shown next to the name.',
        'Value shown on the axis as a formula of the stored value x. ' +
            'Example: ln(x)',
        'Converts a displayed value back to the stored x — the inverse of ' +
            'f(x). Example: exp(x)');

{ What the dialog opens on for ADefinition: the definition itself, or the
  identity on a first use - two empty boxes give no clue that what belongs in
  them is a formula in x, and accepting f(x)=x unchanged does nothing
  surprising. }
function CustomAxisToEdit(const ADefinition: TAxisDefinition): TCustomAxisDefinition;

{ The axis definition an accepted dialog gives. }
function AxisDefinitionOf(const AAxis: TCustomAxisDefinition): TAxisDefinition;

type
    { The dialog's spacing, already in the pixels of the display it opens on.
      Scaling is the caller's: the dialog is built by hand, and a hand-built
      form is scaled BEFORE any control is added to it, so a design pixel
      passed through unscaled stays design-sized. }
    TAxisDialogMetrics = record
        Margin, Gap, RowGap: integer;
        EditHeight, LabelWidth: integer;
        ButtonWidth, ButtonHeight: integer;
        Width, IntroHeight: integer;
    end;

    { Where the dialog puts things. }
    TAxisDialogLayout = record
        IntroWidth: integer;
        EditLeft, EditWidth: integer;
        FirstRowTop, RowStep: integer;
        ButtonTop, OkLeft, CancelLeft: integer;
        ClientHeight: integer;
    end;

{ Where everything goes in a dialog of ARows boxes: an explanation across the
  top, a caption column and an edit column below it, and OK and Cancel
  right-aligned under the last row. }
function CustomAxisDialogLayout(const AMetrics: TAxisDialogMetrics;
    ARows: integer): TAxisDialogLayout;

implementation

function DefaultCustomAxis: TCustomAxisDefinition;
begin
    Result.Name := 'Custom';
    //  No unit: the identity axis is in whatever the data is in, and inventing
    //  one would label the chart with something untrue.
    Result.Units := '';
    Result.Forward_ := 'x';
    Result.Inverse := 'x';
end;

function CustomAxisIsUnset(const ADefinition: TCustomAxisDefinition): boolean;
begin
    //  THE FORWARD FORMULA IS THE TEST, because it is the one field that cannot
    //  be legitimately empty. A name or a unit left blank is a choice.
    Result := Trim(ADefinition.Forward_) = '';
end;

function TrimmedCustomAxis(
    const ADefinition: TCustomAxisDefinition): TCustomAxisDefinition;
begin
    Result.Name := Trim(ADefinition.Name);
    Result.Units := Trim(ADefinition.Units);
    Result.Forward_ := Trim(ADefinition.Forward_);
    Result.Inverse := Trim(ADefinition.Inverse);
end;

function CustomAxisProblem(
    const ADefinition: TCustomAxisDefinition): TCustomAxisProblem;
var
    D: TCustomAxisDefinition;
begin
    D := TrimmedCustomAxis(ADefinition);
    if D.Forward_ = '' then
        Result := capNoForward
    else if D.Inverse = '' then
        Result := capNoInverse
    else
        Result := capNone;
end;

function CustomAxisProblemMessage(AProblem: TCustomAxisProblem): string;
begin
    case AProblem of
        capNoForward, capNoInverse:
            //  ONE MESSAGE FOR BOTH, and it names both formulas: a user who
            //  left one blank has very likely not understood that two are
            //  wanted, and telling them only about the one they missed does not
            //  explain why.
            Result := 'Both the display formula f(x) and its inverse g(x) are ' +
                'required, each written in terms of x ' +
                '(e.g. f(x)=ln(x), g(x)=exp(x)).';
        else
            Result := '';
    end;
end;

function CustomAxisToEdit(const ADefinition: TAxisDefinition): TCustomAxisDefinition;
begin
    Result.Name := ADefinition.Name;
    Result.Units := ADefinition.UnitName;
    Result.Forward_ := ADefinition.Forward;
    Result.Inverse := ADefinition.Inverse;
    if CustomAxisIsUnset(Result) then
        Result := DefaultCustomAxis;
end;

function AxisDefinitionOf(const AAxis: TCustomAxisDefinition): TAxisDefinition;
begin
    Result := Default(TAxisDefinition);
    Result.Name := AAxis.Name;
    Result.UnitName := AAxis.Units;
    Result.Forward := AAxis.Forward_;
    Result.Inverse := AAxis.Inverse;
end;

function CustomAxisDialogLayout(const AMetrics: TAxisDialogMetrics;
    ARows: integer): TAxisDialogLayout;
begin
    with AMetrics do
    begin
        Result.IntroWidth := Width - 2 * Margin;
        Result.EditLeft := Margin + LabelWidth + Gap;
        Result.EditWidth := Width - Result.EditLeft - Margin;
        Result.FirstRowTop := Margin + IntroHeight + RowGap;
        Result.RowStep := EditHeight + RowGap;
        Result.ButtonTop := Result.FirstRowTop + ARows * Result.RowStep + RowGap;
        Result.CancelLeft := Width - Margin - ButtonWidth;
        Result.OkLeft := Result.CancelLeft - Gap - ButtonWidth;
        Result.ClientHeight := Result.ButtonTop + ButtonHeight + Margin;
    end;
end;

end.
