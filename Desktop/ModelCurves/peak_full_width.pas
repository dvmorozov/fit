// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How wide a curve is at half its maximum, measured from the curve.)

For a curve type whose width per unit of a parameter has no closed form - a
formula the user wrote - the only way to know is to look. This samples the curve
around where it is placed, finds its highest point, and bisects each side for
where it falls to half of that.

A curve that is not a peak - one still at half its height at either end of the
span sampled, or with no positive maximum - has no width at half maximum, and
the answer is 0: the caller then counts its width as a full width, which is
what it did before anything was measured (TUserPointsSet.FullWidthPerUnit).
}
unit peak_full_width;

{$mode objfpc}{$H+}

interface

type
    { The curve's value at AX. }
    TCurveValueAt = function(const AX: double): double of object;

{ The full width at half maximum of AValueAt, looked for within ASpan either
  side of ACentre, or 0 when it is not a peak there. }
function MeasuredFullWidth(AValueAt: TCurveValueAt;
    const ACentre, ASpan: double): double;

implementation

const
    { Samples across the whole span, and bisection steps at each crossing. }
    SAMPLES = 400;
    BISECTIONS = 50;

function MeasuredFullWidth(AValueAt: TCurveValueAt;
    const ACentre, ASpan: double): double;
var
    i, Top: longint;
    Step, Highest, Half, Y: double;

    { Where AValueAt falls through Half between AInside (above) and AOutside. }
    function Crossing(AInside, AOutside: double): double;
    var
        k: longint;
        Mid: double;
    begin
        for k := 1 to BISECTIONS do
        begin
            Mid := (AInside + AOutside) / 2;
            if AValueAt(Mid) >= Half then
                AInside := Mid
            else
                AOutside := Mid;
        end;
        Result := (AInside + AOutside) / 2;
    end;

    function At(AIndex: longint): double;
    begin
        Result := ACentre - ASpan + AIndex * Step;
    end;

var
    Left, Right: longint;
begin
    Result := 0;
    if not (ASpan > 0) then
        Exit;
    Step := 2 * ASpan / SAMPLES;
    Top := 0;
    Highest := AValueAt(At(0));
    for i := 1 to SAMPLES do
    begin
        Y := AValueAt(At(i));
        if Y > Highest then
        begin
            Highest := Y;
            Top := i;
        end;
    end;
    if not (Highest > 0) then
        Exit;
    Half := Highest / 2;
    //  Not a peak if it is still at half its height at either end.
    if (AValueAt(At(0)) >= Half) or (AValueAt(At(SAMPLES)) >= Half) then
        Exit;
    Left := Top;
    while AValueAt(At(Left - 1)) >= Half do
        Dec(Left);
    Right := Top;
    while AValueAt(At(Right + 1)) >= Half do
        Inc(Right);
    Result := Crossing(At(Right), At(Right + 1)) - Crossing(At(Left), At(Left - 1));
end;

end.
