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
    SysUtils;

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

end.
