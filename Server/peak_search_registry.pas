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

ONE CONTRACT OF implementation_registry, 'peak-search'. What registers through
RegisterPeakSearch is the NATIVE implementation - the default and the
fallback - and an alternative from any provider registers beside it through
RegisterPeakSearchImplementation; the search asked for is the one this
machine chose (ImplementationInForce), or the native one when that cannot run
here. It used to be one slot, the last registration winning, which left no
room for an alternative at all.

HOW A CHOICE REACHES A SERVER IN ANOTHER PROCESS is decided with the first
alternative: today there is one implementation, so the choice is the native
one wherever it is asked.

A BUILD WITHOUT IT REFUSES IN WORDS. No product without the scientific module
asks: a type placed at curve positions is the module's, and without one the
window offers no Compute Automatically for positions (build_capabilities).
}
unit peak_search_registry;

{$mode objfpc}{$H+}

interface

uses
    points_set, title_points_set, named_points_set, implementation_registry;

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

const
    { The contract the search is registered under in implementation_registry. }
    PeakSearchContract = 'peak-search';
    { The native implementation's id. }
    NativePeakSearchId = 'native';

{ Registers the NATIVE search, replacing one registered before - a test puts
  the real one back this way. nil for AFind takes every implementation out:
  the build has no peak search. }
procedure RegisterPeakSearch(AFind: TFindPeakPoints;
    APositions: TFindCurvePositions; ABounds: TFindPeakBounds);
{ Registers an alternative beside the native one: AInfo says what it is and
  the provider it runs on (its Contract is set here). Refused, as
  implementation_registry refuses, before a native one is registered. }
procedure RegisterPeakSearchImplementation(const AInfo: TImplementationInfo;
    AFind: TFindPeakPoints; APositions: TFindCurvePositions;
    ABounds: TFindPeakBounds);

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

type
    { The three entry points of one implementation, by its id. }
    TSearch = record
        Id: string;
        Find: TFindPeakPoints;
        Positions: TFindCurvePositions;
        Bounds: TFindPeakBounds;
    end;

var
    Searches: array of TSearch;

const
    NoSearch = 'This build has no peak search: curve positions and fit ' +
        'intervals are found automatically by a module it does not contain.';

procedure AddSearch(const AInfo: TImplementationInfo; AFind: TFindPeakPoints;
    APositions: TFindCurvePositions; ABounds: TFindPeakBounds);
var
    Info: TImplementationInfo;
begin
    Info := AInfo;
    Info.Contract := PeakSearchContract;
    RegisterImplementation(Info);
    SetLength(Searches, Length(Searches) + 1);
    Searches[High(Searches)].Id := Info.Id;
    Searches[High(Searches)].Find := AFind;
    Searches[High(Searches)].Positions := APositions;
    Searches[High(Searches)].Bounds := ABounds;
end;

procedure RegisterPeakSearch(AFind: TFindPeakPoints;
    APositions: TFindCurvePositions; ABounds: TFindPeakBounds);
var
    Info: TImplementationInfo;
begin
    //  REPLACED, NOT ADDED: the native one is the default, and a second
    //  registration of it - a test putting the real search back - is the
    //  same implementation again. Its alternatives went with it, since none
    //  may stand without the native one beside it.
    UnregisterContract(PeakSearchContract);
    Searches := nil;
    if not Assigned(AFind) then
        Exit;
    Info := Default(TImplementationInfo);
    Info.Id := NativePeakSearchId;
    Info.Name := 'Peak search (native)';
    Info.Description := 'Finds the curve positions and fit intervals Compute ' +
        'Automatically proposes, in the compute server itself, by the search ' +
        'the module this build links provides. Needs nothing installed.';
    Info.Provider := NativeProvider;
    AddSearch(Info, AFind, APositions, ABounds);
end;

procedure RegisterPeakSearchImplementation(const AInfo: TImplementationInfo;
    AFind: TFindPeakPoints; APositions: TFindCurvePositions;
    ABounds: TFindPeakBounds);
begin
    AddSearch(AInfo, AFind, APositions, ABounds);
end;

{ The search this machine's choice resolves to, False when there is none. }
function SearchInForce(out ASearch: TSearch): boolean;
var
    Id: string;
    i: longint;
begin
    ASearch := Default(TSearch);
    Id := ImplementationInForce(PeakSearchContract).Id;
    for i := 0 to High(Searches) do
        if Searches[i].Id = Id then
        begin
            ASearch := Searches[i];
            Exit(True);
        end;
    Result := False;
end;

function FindPeakPoints(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
var
    Search: TSearch;
begin
    if not (SearchInForce(Search) and Assigned(Search.Find)) then
        raise EUserException.Create(NoSearch);
    Result := Search.Find(AData, ASearchMinimums, AHost);
end;

function PeakSearchAvailable: boolean;
var
    Search: TSearch;
begin
    Result := SearchInForce(Search) and Assigned(Search.Bounds);
end;

procedure FindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
var
    Search: TSearch;
begin
    if not (SearchInForce(Search) and Assigned(Search.Positions)) then
        raise EUserException.Create(NoSearch);
    Search.Positions(AData, AMode, AHost, APositions);
end;

procedure FindPeakBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
var
    Search: TSearch;
begin
    if not (SearchInForce(Search) and Assigned(Search.Bounds)) then
        raise EUserException.Create(NoSearch);
    Search.Bounds(AData, AHost, ABounds);
end;

end.
