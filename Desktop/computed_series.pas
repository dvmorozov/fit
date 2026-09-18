// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the computed series is named, and which is drawn, when it
comes back empty.)

FIVE SERIES ARE FETCHED AFTER EVERY RECOMPUTE and each is handled by the same
four steps: drop the old one, ask the service for the new one, give it a title,
draw it. What differs between them is only when the last two steps happen, and
that difference is a rule with a defect behind it - which is why it is here
rather than repeated five times inside a hundred-line routine where no test can
reach it.

THE RULE IS ABOUT WHO PICKS INTO THE SERIES.

  * The curve positions, the fitted positions and the fit intervals are the sets
    the user's next pick is ADDED TO. The title is what names the series on the
    chart, so it has to be there before there is anything in it: naming only the
    non-empty ones left a set that came back empty - after a reset, or before
    anything had been picked - to be drawn later under a blank name. They are
    therefore named whether or not they carry points.

  * The computed profile and the difference are OUTPUTS. Nobody picks into them,
    they exist only once there is a model, and an empty one is nothing at all -
    so there is no series to name yet.

DRAWING IS THE SAME QUESTION FOR ALL FIVE: a series with no points has nothing
to draw, and asking the chart to draw it puts an entry in the legend for a curve
that is not there.

AND THREE OF THEM ARE ONLY FETCHED ON REQUEST. The caller passes ShowExtraData -
false during the rapid refreshes of a running fit, where the model is what
matters and the pick markers would only be redrawn on top of themselves.
}
unit computed_series;

{$MODE Delphi}

interface

type
    { The series a recompute refreshes, in the order the client fetches them. }
    TComputedSeries = (
        { The summed model, and the residual between it and the data. }
        csComputedProfile,
        csDeltaProfile,
        { Where the user put the curves, where the fit put them, and the
          stretches each sub-task fits. }
        csCurvePositions,
        csFittedPositions,
        csRFactorBounds);

{ True for a series fetched only when the caller asked for the extra data. }
function SeriesIsExtraData(ASeries: TComputedSeries): boolean;

{ Whether the series is given its title, having come back with APointsCount
  points. See the rule at the top: a set the user picks into is named while
  still empty, because the title is what names the series it will appear as. }
function SeriesIsNamed(ASeries: TComputedSeries;
    APointsCount: longint): boolean;

{ Whether the series is drawn. Nothing with no points is ever drawn. }
function SeriesIsPlotted(ASeries: TComputedSeries;
    APointsCount: longint): boolean;

implementation

function SeriesIsExtraData(ASeries: TComputedSeries): boolean;
begin
    Result := ASeries in [csCurvePositions, csFittedPositions, csRFactorBounds];
end;

function SeriesIsNamed(ASeries: TComputedSeries;
    APointsCount: longint): boolean;
begin
    //  THE SAME THREE, and that is not a coincidence worth hiding behind a
    //  second list: a series is named while empty exactly when it is one the
    //  user picks into, and those are exactly the ones fetched on request.
    //  Written as one condition so the two cannot drift apart.
    Result := SeriesIsExtraData(ASeries) or (APointsCount > 0);
end;

function SeriesIsPlotted(ASeries: TComputedSeries;
    APointsCount: longint): boolean;
begin
    Result := APointsCount > 0;
end;

end.
