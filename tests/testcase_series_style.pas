// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How each series on the chart is drawn.)

TEN METHODS, EACH SETTING SEVEN PROPERTIES BY HAND, none of them reachable by a
test: creating one series meant creating a chart, a form and the widget set. What
they set is not decoration - it is how the user tells the measured data from the
model, a requested curve position from the one the fit achieved, and an interval
bound from a picked point.

The test worth having here is not "the residual is green". It is the pair of
sweeps at the end: every series has a style, and every series that is drawn
without lines is exempt from the markers toggle. That second rule is the trap the
extraction was for - the chart asks whether a series has lines to decide whether
"View markers" reaches it, so a marker-only series that forgets to say so has its
only content switched off by a menu item that was never meant to touch it.
}
unit testcase_series_style;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, module_view_types, series_style;

type
    TSeriesStyleTest = class(TTestCase)
    published
        procedure TheMeasuredDataIsAProfileWithHollowMarkers;
        procedure TheSelectionSharesTheDataColour;
        procedure AModelCurveTakesItsColourFromThePalette;
        procedure ProfileSeriesFollowTheMarkersToggle;
        procedure PositionSeriesIgnoreTheMarkersToggle;
        procedure RequestedAndAchievedPositionsDifferByShape;
        procedure BoundsAndPickedPointsPointOppositeWays;
        procedure TheBackgroundIsJoinedUnlikeTheOtherMarkerSeries;
        procedure EverySeriesHasAStyle;
        procedure EverySizeIsStated;
        procedure OnlyLineSeriesAnswerToTheMarkersToggle;
        procedure EveryModuleShapeIsMapped;
        procedure AnUnmappedModuleShapeIsRefusedRatherThanGuessed;
    end;

implementation

procedure TSeriesStyleTest.TheMeasuredDataIsAProfileWithHollowMarkers;
var
    Style: TSeriesStyle;
begin
    Style := FitSeriesStyle(fskExperimentalProfile, True);
    AssertTrue('joined, because it is a curve through data', Style.ShowLines);
    AssertTrue('hollow markers', Style.Hollow);
    AssertTrue('the experiment colour',
        Style.ColorRole = crExperiment);
    AssertTrue('square markers', Style.Marker = smRectangle);
end;

procedure TSeriesStyleTest.TheSelectionSharesTheDataColour;
var
    Data, Selection: TSeriesStyle;
begin
    //  A selected interval is part of the measured data, not a thing of its own,
    //  and is drawn as such - it is told apart by being drawn on top, not by
    //  colour.
    Data := FitSeriesStyle(fskExperimentalProfile, True);
    Selection := FitSeriesStyle(fskSelectedInterval, True);
    AssertTrue('the same colour role', Selection.ColorRole = Data.ColorRole);
    AssertTrue('and the same marker', Selection.Marker = Data.Marker);
    AssertTrue('and hollow like it', Selection.Hollow);
end;

procedure TSeriesStyleTest.AModelCurveTakesItsColourFromThePalette;
var
    Style: TSeriesStyle;
begin
    //  WHICH of the sixteen is series_palette's arithmetic; that this series is
    //  the one asking is the claim here.
    Style := FitSeriesStyle(fskModelCurve, True);
    AssertTrue('from the palette', Style.ColorRole = crModelCurve);
    AssertTrue('joined', Style.ShowLines);
    AssertTrue('filled, so it reads over the hollow data markers',
        not Style.Hollow);
end;

procedure TSeriesStyleTest.ProfileSeriesFollowTheMarkersToggle;
var
    Kind: TFitSeriesKind;
begin
    for Kind := fskExperimentalProfile to fskResidual do
    begin
        AssertTrue('markers on with the toggle on',
            FitSeriesStyle(Kind, True).ShowPoints);
        AssertTrue('markers off with the toggle off',
            not FitSeriesStyle(Kind, False).ShowPoints);
    end;
end;

procedure TSeriesStyleTest.PositionSeriesIgnoreTheMarkersToggle;
var
    Kind: TFitSeriesKind;
begin
    //  THE POINT OF THE FAMILY. These series are markers and nothing else, so
    //  the toggle must not be able to leave them blank.
    for Kind := fskIntervalBounds to fskPickedPoints do
    begin
        AssertTrue('drawn with the toggle on',
            FitSeriesStyle(Kind, True).ShowPoints);
        AssertTrue('and still drawn with it off',
            FitSeriesStyle(Kind, False).ShowPoints);
    end;
end;

procedure TSeriesStyleTest.RequestedAndAchievedPositionsDifferByShape;
var
    Asked, Got: TSeriesStyle;
begin
    Asked := FitSeriesStyle(fskRequestedPositions, True);
    Got := FitSeriesStyle(fskAchievedPositions, True);
    //  After a good fit the two sit almost on top of each other, so the
    //  difference between them cannot be colour.
    AssertTrue('the same colour role', Got.ColorRole = Asked.ColorRole);
    AssertTrue('and the same size', Got.MarkerSize = Asked.MarkerSize);
    AssertTrue('but different shapes', Got.Marker <> Asked.Marker);
end;

procedure TSeriesStyleTest.BoundsAndPickedPointsPointOppositeWays;
begin
    //  Both are vertical lines at an x with no meaningful y, so the only thing
    //  that separates them on screen is which end they hang from.
    AssertTrue('bounds hang from the top',
        FitSeriesStyle(fskIntervalBounds, True).Marker = smVertLineTB);
    AssertTrue('picked points rise from the bottom',
        FitSeriesStyle(fskPickedPoints, True).Marker = smVertLineBT);
end;

procedure TSeriesStyleTest.TheBackgroundIsJoinedUnlikeTheOtherMarkerSeries;
var
    Style: TSeriesStyle;
begin
    //  The background is the one member of the marker family drawn with a line,
    //  because it is a curve the fit subtracts: its shape between the points is
    //  what the user is judging.
    Style := FitSeriesStyle(fskBackground, True);
    AssertTrue('joined', Style.ShowLines);
    AssertTrue('markers too', Style.ShowPoints);
    AssertTrue('its own colour', Style.ColorRole = crBackground);
end;

procedure TSeriesStyleTest.EverySeriesHasAStyle;
var
    Kind: TFitSeriesKind;
    Style: TSeriesStyle;
begin
    //  A SWEEP RATHER THAN A LIST, so a series added to the enum and forgotten
    //  in the table fails here instead of being drawn with whatever the record
    //  happened to contain. An unassigned Result is not detectable directly, so
    //  the assertion is on the one field no series may leave at zero.
    for Kind := Low(TFitSeriesKind) to High(TFitSeriesKind) do
    begin
        Style := FitSeriesStyle(Kind, True);
        AssertTrue('kind ' + IntToStr(Ord(Kind)) + ' has no size, so it has no' +
            ' entry in the table', Style.MarkerSize > 0);
    end;
end;

procedure TSeriesStyleTest.EverySizeIsStated;
var
    Kind: TFitSeriesKind;
begin
    //  Nothing is left to the plotting component's default: a default is a value
    //  that changes when somebody else's code changes, and the chart is a fork
    //  of a 2005 component scheduled to be replaced.
    for Kind := Low(TFitSeriesKind) to High(TFitSeriesKind) do
        AssertTrue('a plausible marker size',
            (FitSeriesStyle(Kind, True).MarkerSize >= 2) and
            (FitSeriesStyle(Kind, True).MarkerSize <= 8));
end;

procedure TSeriesStyleTest.OnlyLineSeriesAnswerToTheMarkersToggle;
var
    Kind: TFitSeriesKind;
    Style: TSeriesStyle;
begin
    //  THE RULE THE EXTRACTION EXISTS FOR, checked over every series at once:
    //  the toggle reaches a series exactly when there is a line left to see
    //  after the markers go. Stated the other way round, no series can be left
    //  drawing nothing.
    for Kind := Low(TFitSeriesKind) to High(TFitSeriesKind) do
    begin
        Style := FitSeriesStyle(Kind, False);
        //  The view applies the style's ShowLines to both flags, so this is
        //  what it would ask.
        if MarkersToggleApplies(Style.ShowLines, Style.ShowLines) then
            AssertTrue('kind ' + IntToStr(Ord(Kind)) +
                ' would be blank with markers off', Style.ShowLines)
        else
            AssertTrue('kind ' + IntToStr(Ord(Kind)) +
                ' has no lines, so its markers must survive the toggle',
                Style.ShowPoints);
    end;
end;

procedure TSeriesStyleTest.EveryModuleShapeIsMapped;
var
    Shape: TModuleMarkerShape;
    Marker: TSeriesMarker;
begin
    //  The module vocabulary is a published contract, so every shape in it must
    //  draw as something. A module that asks for a shape this chart forgot would
    //  otherwise get a wrong picture with no error.
    for Shape := Low(TModuleMarkerShape) to High(TModuleMarkerShape) do
        AssertTrue('shape ' + IntToStr(Ord(Shape)) + ' is not mapped',
            ChartMarkerForModuleShape(Shape, Marker));
end;

procedure TSeriesStyleTest.AnUnmappedModuleShapeIsRefusedRatherThanGuessed;
var
    Marker: TSeriesMarker;
begin
    //  Reached by casting past the end of the enum, which is what a shape added
    //  later and not mapped here would amount to. The refusal is what lets the
    //  caller say so; the marker it leaves behind must still be a real one, so
    //  that a caller which reports and carries on draws something rather than
    //  reading an uninitialised value.
    Marker := smVertLineTB;
    AssertTrue('an unknown shape is refused',
        not ChartMarkerForModuleShape(
            TModuleMarkerShape(Ord(High(TModuleMarkerShape)) + 1), Marker));
    AssertTrue('and a usable marker is still returned',
        Marker = smCircle);
end;

initialization
    RegisterTest('unit', TSeriesStyleTest);
end.
