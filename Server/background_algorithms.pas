// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the engine finds the background algorithms: proposing a
profile's background points, and taking them off.)

THE ALGORITHMS ARE THE SCIENTIFIC MODULE'S (background_search, moved there on
2026-10-17): they assume the concave background of a diffractogram, which is
that field's. The engine still holds the background POINT SET - one of the
four point sets of a problem, on the wire and in the project - and the verbs
that act on it; it asks this registry for the arithmetic, under the same
three names it always called, so its code is the code it had. A restructuring
only, at the user's direction.

REGISTERED WHEN LINKED: background_search registers itself from its unit's
initialization, as a curve type does, and the module's server front door
names it, so a server - or a test binary - that links it has it.

A BUILD WITHOUT THEM REFUSES IN WORDS. No product without the scientific
module asks: its window offers no background command, and its automatic run
leaves for its module's own before the copy below would be taken.
}
unit background_algorithms;

{$mode objfpc}{$H+}

interface

uses
    points_set;

type
    TProposeBackgroundPoints = function(AData: TPointsSet): TPointsSet;
    TSubtractPiecewiseLinear = procedure(AData, ABackground: TPointsSet);
    TBackgroundRemovedCopy = function(AData: TPointsSet): TPointsSet;

{ Registers the three algorithms. The last registration wins. }
procedure RegisterBackgroundAlgorithms(APropose: TProposeBackgroundPoints;
    ASubtract: TSubtractPiecewiseLinear; ARemovedCopy: TBackgroundRemovedCopy);

{ The registered algorithms, as background_search declares them. Each raises
  EUserException in a build that registered none. }
function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);
function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;

implementation

uses
    MyExceptions;

var
    Propose: TProposeBackgroundPoints;
    Subtract: TSubtractPiecewiseLinear;
    RemovedCopy: TBackgroundRemovedCopy;

const
    NoAlgorithm = 'This build has no background algorithm: background points ' +
        'are proposed and subtracted by a module it does not contain.';

procedure RegisterBackgroundAlgorithms(APropose: TProposeBackgroundPoints;
    ASubtract: TSubtractPiecewiseLinear; ARemovedCopy: TBackgroundRemovedCopy);
begin
    Propose := APropose;
    Subtract := ASubtract;
    RemovedCopy := ARemovedCopy;
end;

function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
begin
    if not Assigned(Propose) then
        raise EUserException.Create(NoAlgorithm);
    Result := Propose(AData);
end;

procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);
begin
    if not Assigned(Subtract) then
        raise EUserException.Create(NoAlgorithm);
    Subtract(AData, ABackground);
end;

function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;
begin
    if not Assigned(RemovedCopy) then
        raise EUserException.Create(NoAlgorithm);
    Result := RemovedCopy(AData);
end;

end.
