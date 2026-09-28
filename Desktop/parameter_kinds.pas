// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a parameter is treated, in the terms the user reads.)

WHY THE ENGINE'S TYPES ARE NOT THE USER'S. TParameterType makes eight
distinctions, and most of them are about how the engine seeds and constrains a
value - whether it was initialised from the data peak or from the fitting
interval, whether it is the abscissa. To someone reading the parameter table
those are the same thing: the fit moves it. What the reader needs to know is
narrower and different - is this value being fitted, is it held to one value
across an interval, was it fixed when the curve was placed, or does it follow
from the others.

Four kinds, then, and the mapping between them is a decision. It lived in the
main form beside the colours that paint it, which meant it could only be reached
by opening a window - and the parameter table's colouring is the user's ONLY
indication of which numbers the fit is free to move. A value shown as fitted that
is actually fixed is a fit that appears not to have worked.

WHAT STAYS IN THE FORM. The colours. A tint is a widget-set value and painting is
a window's business; what a kind IS, and what it is called, is not.
}
unit parameter_kinds;

{$mode objfpc}{$H+}

interface

uses
    special_curve_parameter;

type
    { What the user needs to know about a value, which is narrower than what the
      engine distinguishes. Types that differ only in what the ENGINE does with
      them are one kind here. }
    TParameterKind = (
        { Varied by the fit. The ordinary case, and most of the table. }
        pkFitted,
        { Varied, but held to one value across the curves of an interval. }
        pkShared,
        { Set when the curve was placed and not varied. }
        pkFixed,
        { Not fitted and not entered: it follows from the others. }
        pkComputed
        );

const
    ParameterKindCaption: array[TParameterKind] of string = (
        'Fitted', 'Shared', 'Fixed', 'Computed');

    { The sentence lives on the hint rather than in the legend row: four
      explanations side by side is a paragraph, and a paragraph under a table is
      read once and then never again. }
    ParameterKindHint: array[TParameterKind] of string = (
        'Varied by the fit to match the data.',
        'Varied by the fit, but held to one value across the curves ' +
            'of an interval.',
        'Set when the curve was placed and not varied - for a wave pattern, ' +
            'the points you picked.',
        'Not fitted and not entered: it follows from the other parameters, ' +
            'and is recomputed whenever they change.');

{ Which of the four a parameter type belongs to.

  The types the engine distinguishes but the user does not are folded together
  here, in the one place that decides it. }
function KindOfParameter(AType: TParameterType): TParameterKind;

implementation

function KindOfParameter(AType: TParameterType): TParameterKind;
begin
    case AType of
        Shared:
            Result := pkShared;
        Calculated:
            Result := pkComputed;
        InvariablePosition:
            Result := pkFixed;
        else
            //  Variable, VariablePosition, Amplitude, Width and Argument - all
            //  moved by the fit, which is the only distinction this key makes.
            Result := pkFitted;
    end;
end;

end.
