// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which point of which series a click on the chart is a pick of.)

WHY THIS IS NOT IN THE WINDOW. The chart's crosshair snaps to the nearest point
of ANY visible series, and the window then compared the series it snapped to with
the one being marked up. That comparison is a decision, and it was wrong in a way
nothing could see: as soon as a model was drawn over the data - which is what
marking the first bounded pattern does - a click landed on a model series, failed
the comparison and was dropped with no pick, no message and no log line. The user
saw a mode that had stopped accepting clicks.

The decision is therefore taken here, over plain numbers, where it can be tested
in milliseconds (see tests/testcase_pick_target.pas). The window keeps only the
part that needs widgets: reading the pixels out of the chart.
}
unit pick_target;

{$mode objfpc}{$H+}

interface

const
    { No point at all - an empty series. }
    NO_POINT = -1;

{ Whether a click of this button places a pick.

  PICKING IS A LEFT-CLICK GESTURE and always has been. What was missing is that
  NOTHING CHECKED: TTAChart.MouseDown and MouseUp both take the button and never
  read it, the window's own handlers ignore it, and OnChartClick's gate tests
  whether the crosshair was drawn - which comes from MouseMove and so is
  button-independent. A right-click that did not move between press and release
  therefore satisfied every condition and added a point. Nobody chose that; it is
  what falls out of never asking.

  Asked of a boolean rather than of the LCL's TMouseButton so that this unit
  names no widget set - the caller translates, which is one line at the one place
  a button arrives. }
function ClickPlacesAPick(AIsLeftButton: boolean): boolean;

{ Whether a click that snapped to AClickedSerie is already aimed at something a
  pick may come from: the series being marked up (the active one), or the set
  the picks are collected into, a click on which takes a pick back.

  Anything else is a curve drawn OVER the data - a model, a difference, a
  module's overlay. A click on one of those is still a click on the data
  underneath, so the caller re-aims it rather than dropping it. }
function IsPickableSerie(AClickedSerie, AActiveSerie: longint;
    AClickedIsCollectedSet: boolean): boolean;

{ The point of a series nearest to a click, all in image (pixel) coordinates.
  AX and AY are one series' points and must be the same length.

  Distance is measured in BOTH coordinates, as the chart's own crosshair
  measures it, so that the point picked is the one under the pointer rather than
  the one merely at the same x. Ties go to the earlier point: two points at the
  same distance are equally right, and picking deterministically is what makes
  the gesture reproducible. }
function NearestPointIndex(const AX, AY: array of longint;
    AClickX, AClickY: longint): longint;

implementation

uses
    checks;

function ClickPlacesAPick(AIsLeftButton: boolean): boolean;
begin
    //  ONLY THE LEFT BUTTON. Every other one is free for what a button is
    //  ordinarily for - a context menu - and until this existed there was none,
    //  because opening one would also have placed a point.
    Result := AIsLeftButton;
end;

function IsPickableSerie(AClickedSerie, AActiveSerie: longint;
    AClickedIsCollectedSet: boolean): boolean;
begin
    Result := (AClickedSerie = AActiveSerie) or AClickedIsCollectedSet;
end;

function NearestPointIndex(const AX, AY: array of longint;
    AClickX, AClickY: longint): longint;
var
    i: longint;
    Dist, Best: int64;
    DX, DY: int64;
begin
    //  Two arrays that disagree describe no series at all, and silently taking
    //  the shorter would pick a point from the wrong place.
    CheckThat(Length(AX) = Length(AY),
        'the x and y coordinates of a series must come in pairs');

    Result := NO_POINT;
    Best := 0;
    for i := 0 to High(AX) do
    begin
        //  Squared distance, and in int64: the comparison is all that is
        //  wanted, and a square root would cost precision and time for nothing.
        //  Pixel coordinates of points far off-canvas are large enough that the
        //  square overflows a longint.
        DX := int64(AClickX) - AX[i];
        DY := int64(AClickY) - AY[i];
        Dist := DX * DX + DY * DY;
        if (Result = NO_POINT) or (Dist < Best) then
        begin
            Best := Dist;
            Result := i;
        end;
    end;
end;

end.
