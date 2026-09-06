// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the user is told next while picking, and when a gesture is over.)

PICKING IS A MULTI-CLICK GESTURE and the status line is the only thing that says
how far through it the user is. Selecting an area takes two clicks; characterising
a peak takes three; the background and the curve positions take as many as the
user likes; the fitting intervals take them in pairs, so the guidance alternates.
The prompt after each click is what distinguishes "pick another" from "you are
done, now choose the menu entry".

WHAT WENT WRONG WITHOUT IT. The whole of this lived in nested `case` statements
inside a chart click handler - two levels deep, one branch per mode, one branch
per count within it - reachable only by clicking on a chart. A prompt asking for
a third point in a two-point gesture, or a gesture that never reports itself
finished, is invisible to every kind of testing this project had.

WHEN A GESTURE IS OVER matters for more than the prompt: the mode ends, its
markers come off the chart, and the entry unticks. A mode that outlives what it
was entered for turns the next stray click into a pick nobody meant to make.
}
unit pick_guidance;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_client;

const
    { What the status line says while a gesture is under way. Kept here with the
      rule that chooses them: a prompt and the count it belongs to are one fact,
      and they were three hundred lines apart. }
    HintFirst = 'Now you can pick a first point';
    HintSecondFinish = 'Now you can pick a second point - "FINISH"';
    HintSecondPeak = 'Now you can pick a second point - "PEAK"';
    HintThirdFinish = 'Now you can pick a third point - "FINISH"';
    HintSelectProfileInterval =
        'Now you can pick the menu item "Select Area"';
    HintMovePeak = 'Now you can pick the menu item "Move Peak to Results"';
    HintNextPoint =
        'Now you can pick a next point or the menu item "Minimize Difference"';
    HintNextBackPoint =
        'Now you can pick a next point or the menu item "Remove Background"';
    HintNextPointOdd = 'Now you can pick a left point of peak';
    HintNextPointEven = 'Now you can pick a right point of peak';

{ How many picks make one complete gesture in this mode, or 0 when the mode has
  no natural end and runs until the user leaves it.

  The open-ended modes are the ones that collect a SET - the background points,
  the curve positions, the fitting intervals. The bounded ones describe a single
  thing: an area has two ends, a peak has three points. }
function PicksPerGesture(ASelection: TSelMode): longint;

{ True when APicksSoFar completes the gesture, so further clicks are ignored. }
function GestureIsComplete(ASelection: TSelMode; APicksSoFar: longint): boolean;

{ What to tell the user after their APicksSoFar-th pick, or '' when there is
  nothing to say - which is what a completed bounded gesture returns.

  APicksSoFar is the count BEFORE this click is added, because that is what the
  chart handler knows when it has to speak: the click has been accepted and the
  point has not been added yet. }
function PickHint(ASelection: TSelMode; APicksSoFar: longint): string;

implementation

function PicksPerGesture(ASelection: TSelMode): longint;
begin
    case ASelection of
        ModeSelectIntervalBounds: Result := 2;
        ModeSelectCurveBounds: Result := 2;
        ModeSelectCharacteristicPoints: Result := 3;
        else
            //  Open-ended: the background, the positions, the intervals, a
            //  module's own set, and "no mode at all".
            Result := 0;
    end;
end;

function GestureIsComplete(ASelection: TSelMode; APicksSoFar: longint): boolean;
var
    Needed: longint;
begin
    Needed := PicksPerGesture(ASelection);
    Result := (Needed > 0) and (APicksSoFar >= Needed);
end;

function PickHint(ASelection: TSelMode; APicksSoFar: longint): string;
begin
    Result := '';
    case ASelection of
        ModeSelectIntervalBounds:
            //  Two ends, and then the user is sent to the menu: picking the
            //  area is a separate act from marking where it is.
            case APicksSoFar of
                0: Result := HintSecondFinish;
                1: Result := HintSelectProfileInterval;
            end;

        ModeSelectCharacteristicPoints:
            //  Three: the two flanks and the peak between them.
            case APicksSoFar of
                0: Result := HintSecondPeak;
                1: Result := HintThirdFinish;
                2: Result := HintMovePeak;
            end;

        ModeSelectCurveBounds:
            case APicksSoFar of
                0: Result := HintSecondFinish;
                1: Result := HintMovePeak;
            end;

        ModeSelectBackground:
            //  Open-ended, so the prompt only changes once: from "start" to
            //  "carry on, or finish through the menu".
            if APicksSoFar > 0 then
                Result := HintNextBackPoint
            else
                Result := HintFirst;

        ModeSelectCurvePositions:
            if APicksSoFar > 0 then
                Result := HintNextPoint
            else
                Result := HintFirst;

        ModeSelectRFactorBounds:
            //  IN PAIRS, and the prompt names which end of the peak comes next -
            //  an interval half-marked looks exactly like one fully marked until
            //  the fit uses it.
            //
            //  THE PARITY READS BACKWARDS and is kept exactly as it was: with an
            //  even number picked the user is asked for a RIGHT point. Whether
            //  that is a slip or whether the first pick of a pair is the left one
            //  by convention is a question about the interface, not about this
            //  function, so it is preserved and recorded rather than corrected.
            if Odd(APicksSoFar) then
                Result := HintNextPointOdd
            else
                Result := HintNextPointEven;
    end;
end;

end.
