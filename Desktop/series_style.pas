// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How each of the chart's series is drawn - the decisions, without the
chart.)

WHAT WAS WRONG WITH THE OLD ARRANGEMENT. Ten methods of TFitViewer each created a
series and then set six or seven of its properties inline. Every one of those
settings is a decision - what this series IS, told apart from the others by shape,
size, colour and whether its points are joined - and not one of them was reachable
by a test, because reaching it meant owning a chart, a form and the widget set.

They also could not be compared with each other, which is how they drifted. Two
families are plainly intended:

  A PROFILE-LIKE series is a curve through data: rectangle markers, points joined
  by a line, and its markers appear only while the user has "View markers" on.
  The experimental profile, the model curves, the computed profile, the residual.

  A MARKER-ONLY series is a set of positions, not a path: its own shape and size,
  no line, and its markers are always drawn - they are the whole content, so the
  markers toggle must not reach them. Interval bounds, curve positions, picked
  points.

That second sentence is a real invariant with a real trap behind it, and it was
being maintained by hand in each method. The chart decides whether the toggle
reaches a series by asking whether it is drawn with lines, consulting both the
current flag and the InitShowLines the series was created with (see
TFitViewer.ViewAllMarkers). So a marker-only series had to remember to set the
Init flags as well as the plain ones - five of the ten methods did, the rest
relied on the component's defaults happening to agree. A new series added by
copying the wrong neighbour would have its markers silently switched off by a
toggle that was never meant to apply to it.

Here it is one table, the two families are visible side by side, and
MarkersToggleApplies states the rule once.

WHY COLOUR IS A ROLE HERE AND NOT A VALUE. A colour is a widget-set value, and
this unit is compiled by the light test suite, which has no widget set on purpose.
So each series names the ROLE its colour plays and the view maps the roles onto
the palette at the one place it creates a series - the same division
series_palette already makes for the same reason: which colour a curve gets is
arithmetic and lives there, what the sixteen colours are stays with the chart.

WHAT THIS UNIT DELIBERATELY DOES NOT DECIDE: whether a series is sorted before it
is drawn. That belongs to the points set, not to how it looks, and at least one
series must not be sorted at all (see TFitViewer.PlotResultedCurvePositions);
moving the sort here would put an invariant about DATA behind a record about
appearance.
}
unit series_style;

{$mode objfpc}{$H+}

interface

uses
    module_view_types;

type
    { The marker shapes the framework's own series use.

      A separate enum from TModuleMarkerShape, which is a module's vocabulary and
      is deliberately kept to shapes any plotting component can be expected to
      offer. These two are not: a vertical line from the top of the plot down to
      the point, and one from the bottom up, are how this application draws
      interval bounds and picked points, and a module has no business asking for
      them. ChartMarkerForModuleShape maps the narrow set into this one. }
    TSeriesMarker = (smRectangle, smCircle, smDiagCross, smVertLineTB,
                     smVertLineBT);

    { What a series' colour MEANS. The values live with the chart. }
    TSeriesColorRole = (
        crExperiment,       //  the measured data, and a selection over it
        crModelCurve,       //  one of the model's curves: from the palette, by index
        crComputed,         //  the sum of the curves
        crResidual,         //  measured minus computed
        crBackground,       //  the subtracted background
        crIntervalBound,    //  where an R-factor interval ends
        crPosition,         //  where a curve sits, asked for or achieved
        crPickedPoint       //  a point the user marked
    );

    { Everything about how one series is drawn, and nothing about what it holds. }
    TSeriesStyle = record
        Marker:     TSeriesMarker;
        { In pixels. Always stated, including where it equals the component's own
          default - a style that leaves a field to the default is a style that
          changes when the default does. }
        MarkerSize: integer;
        ColorRole:  TSeriesColorRole;
        { Draw the marker as an outline. Used where markers sit densely enough
          that filled ones merge into a band. }
        Hollow:     boolean;
        { Join the points. Also decides whether the markers toggle reaches this
          series - see MarkersToggleApplies. }
        ShowLines:  boolean;
        ShowPoints: boolean;
    end;

    { Every series the framework itself draws. A module's series is not here: it
      brings its own style across the view contract. }
    TFitSeriesKind = (
        fskExperimentalProfile,
        fskSelectedInterval,
        fskModelCurve,
        fskComputedProfile,
        fskResidual,
        fskBackground,
        fskIntervalBounds,
        fskRequestedPositions,
        fskAchievedPositions,
        fskPickedPoints
    );

{ How the given series is drawn, with the markers toggle already applied.

  AViewMarkers is the state of "View markers": it decides ShowPoints for the
  profile-like series and is ignored by the marker-only ones, which is the
  distinction this unit exists to make legible. }
function FitSeriesStyle(AKind: TFitSeriesKind;
    AViewMarkers: boolean): TSeriesStyle;

{ Whether the "View markers" toggle may switch a series' markers on and off.

  THE RULE, STATED ONCE AND ASKED BY THE VIEW: it may, exactly when the series is
  drawn with lines. Turning markers off on a series that has no lines would leave
  nothing drawn at all, so a marker-only series is exempt - and it is exempt by
  virtue of how it is drawn rather than by a list somebody has to keep.

  Takes the two flags rather than a style because that is what the caller has: the
  chart holds the current flag and the one the series was created with, and both
  count - a series created with lines has answered to the toggle ever since, even
  if the toggle itself has since turned its lines off. }
function MarkersToggleApplies(AShowLines, AInitShowLines: boolean): boolean;

{ The framework marker a module's requested shape becomes.

  False when the shape is not mapped, so the caller can report it: an unmapped
  shape would otherwise be drawn as whatever the first branch happens to be,
  which is a wrong picture with no error. }
function ChartMarkerForModuleShape(AShape: TModuleMarkerShape;
    out AMarker: TSeriesMarker): boolean;

implementation

{ The two families, as two constructors, so that a new series is added by
  choosing which of them it belongs to rather than by copying six lines from a
  neighbour and changing some of them. }

function ProfileLike(AColor: TSeriesColorRole; AViewMarkers: boolean;
    AHollow: boolean = False): TSeriesStyle;
begin
    Result.Marker := smRectangle;
    //  The component's own default, stated rather than left out.
    Result.MarkerSize := 2;
    Result.ColorRole := AColor;
    Result.Hollow := AHollow;
    Result.ShowLines := True;
    //  The whole point of the family: the user's toggle governs these.
    Result.ShowPoints := AViewMarkers;
end;

function MarkerOnly(AMarker: TSeriesMarker; ASize: integer;
    AColor: TSeriesColorRole; AShowLines: boolean = False): TSeriesStyle;
begin
    Result.Marker := AMarker;
    Result.MarkerSize := ASize;
    Result.ColorRole := AColor;
    Result.Hollow := False;
    //  Normally none: the markers ARE the series. The background is the one
    //  exception and says so where it is declared.
    Result.ShowLines := AShowLines;
    //  Always drawn, whatever the toggle says.
    Result.ShowPoints := True;
end;

function FitSeriesStyle(AKind: TFitSeriesKind;
    AViewMarkers: boolean): TSeriesStyle;
begin
    case AKind of
        //  ---- profile-like: a curve through data --------------------------
        fskExperimentalProfile:
            //  Hollow, because a dense profile of filled squares reads as a
            //  solid band and the model curve drawn over it disappears.
            Result := ProfileLike(crExperiment, AViewMarkers, True);
        fskSelectedInterval:
            //  The same colour as the data it is part of, hollow for the same
            //  reason.
            Result := ProfileLike(crExperiment, AViewMarkers, True);
        fskModelCurve:
            //  WHICH colour is series_palette's arithmetic, applied by the view.
            Result := ProfileLike(crModelCurve, AViewMarkers);
        fskComputedProfile:
            Result := ProfileLike(crComputed, AViewMarkers);
        fskResidual:
            Result := ProfileLike(crResidual, AViewMarkers);

        //  ---- marker-only: a set of positions -----------------------------
        fskBackground:
            //  JOINED, alone in this family: the background is a curve the fit
            //  subtracts, so its shape between the points is the meaning. Being
            //  joined, it is also the one member the markers toggle reaches -
            //  which is consistent, because with the line still drawn there is
            //  something left to see.
            Result := MarkerOnly(smCircle, 3, crBackground, True);
        fskIntervalBounds:
            //  Full-height lines from the top: a bound is a position on the x
            //  axis and says nothing about y, so a marker at some y would be a
            //  claim the data does not make.
            Result := MarkerOnly(smVertLineTB, 3, crIntervalBound);
        fskRequestedPositions:
            //  Crosses: where the user asked for a curve.
            Result := MarkerOnly(smDiagCross, 5, crPosition);
        fskAchievedPositions:
            //  Circles at the same size: where the fit put it. The two sets sit
            //  close together after a good fit, so they have to be separable by
            //  shape rather than by colour alone.
            Result := MarkerOnly(smCircle, 5, crPosition);
        fskPickedPoints:
            //  Lines from the bottom up, so they are not read as interval
            //  bounds, which come from the top down.
            Result := MarkerOnly(smVertLineBT, 3, crPickedPoint);
    end;
end;

function MarkersToggleApplies(AShowLines, AInitShowLines: boolean): boolean;
begin
    Result := AShowLines or AInitShowLines;
end;

function ChartMarkerForModuleShape(AShape: TModuleMarkerShape;
    out AMarker: TSeriesMarker): boolean;
begin
    Result := True;
    case AShape of
        msCircle:    AMarker := smCircle;
        //  A MODULE ASKING FOR AN UPRIGHT CROSS GETS A DIAGONAL ONE, and for a
        //  star likewise. Stated because it is a substitution, not a mapping:
        //  the framework's series only ever use the diagonal cross, so those are
        //  the shapes this chart draws. Better a marker of the right size and
        //  colour in the wrong shape than a refusal a module cannot act on.
        msCross:     AMarker := smDiagCross;
        msDiagCross: AMarker := smDiagCross;
        msSquare:    AMarker := smRectangle;
        msStar:      AMarker := smDiagCross;
    else
        //  Not mapped at all - a shape added to the module vocabulary and
        //  forgotten here. Reported by the caller, which knows how to report;
        //  this unit must stay free of anything that does.
        AMarker := smCircle;
        Result := False;
    end;
end;

end.
