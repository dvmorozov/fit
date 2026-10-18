// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the engine finds the peak search: the curve positions and fit
intervals Compute Automatically proposes, and step 2 of Fit > Automatically.)

THE SEARCH IS THE SCIENTIFIC MODULE'S (peak_search, moved there on
2026-10-18, its code unchanged): it walks down from the highest point taking
peaks, which is a peak shape's notion of where curves go. The engine still
decides WHEN to search - for a type placed at curve positions - and asks this
registry for the search, under the name it always called, as it asks
background_algorithms for the background arithmetic.

REGISTERED WHEN LINKED: peak_search registers itself from its unit's
initialization, and the module's server front door names it.

A BUILD WITHOUT IT REFUSES IN WORDS. No product without the scientific module
asks: a type placed at curve positions is the module's, and without one the
window offers no Compute Automatically for positions (build_capabilities).
}
unit peak_search_registry;

{$mode objfpc}{$H+}

interface

uses
    points_set, title_points_set, named_points_set;

type
    { The points of every peak in AData, highest first - lowest first when
      ASearchMinimums - as peak_search finds them, down to the background
      fraction the module keeps for the problem AHost serves. AHost is the
      engine itself, the object the module's session was made for. }
    TFindPeakPoints = function(AData: TPointsSet; ASearchMinimums: boolean;
        AHost: TObject): TTitlePointsSet;

    { Each peak's top - or trough's bottom, or both, as AMode asks - added to
      APositions: Model > Curve Positions > Compute Automatically. }
    TFindCurvePositions = procedure(AData: TPointsSet; AMode: TExtremumMode;
        AHost: TObject; APositions: TPointsSet);
    { The bounds of each peak, added to ABounds: Model > Fit Intervals >
      Compute Automatically for a type placed at curve positions. }
    TFindPeakBounds = procedure(AData: TPointsSet; AHost: TObject;
        ABounds: TPointsSet);

{ Registers the search. The last registration wins; nil unregisters. }
procedure RegisterPeakSearch(AFind: TFindPeakPoints;
    APositions: TFindCurvePositions; ABounds: TFindPeakBounds);

{ The registered search. Raises EUserException in a build that registered
  none. }
function FindPeakPoints(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
procedure FindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
procedure FindPeakBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
{ Whether this build has a peak search: where it is only a fallback - a
  module's own search found nothing - the engine leaves the intervals alone
  instead of refusing. }
function PeakSearchAvailable: boolean;

implementation

uses
    MyExceptions;

var
    Find: TFindPeakPoints;
    Positions: TFindCurvePositions;
    Bounds: TFindPeakBounds;

const
    NoSearch = 'This build has no peak search: curve positions and fit ' +
        'intervals are found automatically by a module it does not contain.';

procedure RegisterPeakSearch(AFind: TFindPeakPoints;
    APositions: TFindCurvePositions; ABounds: TFindPeakBounds);
begin
    Find := AFind;
    Positions := APositions;
    Bounds := ABounds;
end;

function FindPeakPoints(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
begin
    if not Assigned(Find) then
        raise EUserException.Create(NoSearch);
    Result := Find(AData, ASearchMinimums, AHost);
end;

function PeakSearchAvailable: boolean;
begin
    Result := Assigned(Bounds);
end;

procedure FindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
begin
    if not Assigned(Positions) then
        raise EUserException.Create(NoSearch);
    Positions(AData, AMode, AHost, APositions);
end;

procedure FindPeakBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
begin
    if not Assigned(Bounds) then
        raise EUserException.Create(NoSearch);
    Bounds(AData, AHost, ABounds);
end;

end.
