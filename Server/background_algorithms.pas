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

ONE CONTRACT OF implementation_registry, 'background', as the peak search is
(peak_search_registry, which says why): RegisterBackgroundAlgorithms registers
the NATIVE implementation, the default and the fallback, and an alternative
from any provider registers beside it through
RegisterBackgroundImplementation; the arithmetic asked for is the one this
machine chose, or the native one when that cannot run here. It used to be
one slot, the last registration winning.

A BUILD WITHOUT THEM REFUSES IN WORDS. No product without the scientific
module asks: its window offers no background command, and its automatic run
leaves for its module's own before the copy below would be taken.
}
unit background_algorithms;

{$mode objfpc}{$H+}

interface

uses
    points_set, implementation_registry;

type
    TProposeBackgroundPoints = function(AData: TPointsSet): TPointsSet;
    TSubtractPiecewiseLinear = procedure(AData, ABackground: TPointsSet);
    TBackgroundRemovedCopy = function(AData: TPointsSet): TPointsSet;

const
    { The contract the arithmetic is registered under in implementation_registry. }
    BackgroundContract = 'background';
    { The native implementation's id. }
    NativeBackgroundId = 'native';

{ Registers the NATIVE algorithms, replacing ones registered before - a test
  puts the real ones back this way. nil for APropose takes every
  implementation out: the build has none. }
procedure RegisterBackgroundAlgorithms(APropose: TProposeBackgroundPoints;
    ASubtract: TSubtractPiecewiseLinear; ARemovedCopy: TBackgroundRemovedCopy);
{ Registers an alternative beside the native one: AInfo says what it is and
  the provider it runs on (its Contract is set here). }
procedure RegisterBackgroundImplementation(const AInfo: TImplementationInfo;
    APropose: TProposeBackgroundPoints; ASubtract: TSubtractPiecewiseLinear;
    ARemovedCopy: TBackgroundRemovedCopy);

{ The registered algorithms, as background_search declares them. Each raises
  EUserException in a build that registered none. }
function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);
function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;

{ Raises the same EUserException as the three above when the arithmetic in
  force is missing, and does nothing otherwise. Asked by a command BEFORE it
  changes anything: a command refused half-way had already cleared the
  background points and moved the engine's state, while telling the user it
  did nothing. }
procedure RefuseWithoutBackgroundArithmetic;

implementation

uses
    MyExceptions;

type
    { The three entry points of one implementation, by its id. }
    TAlgorithms = record
        Id: string;
        Propose: TProposeBackgroundPoints;
        Subtract: TSubtractPiecewiseLinear;
        RemovedCopy: TBackgroundRemovedCopy;
    end;

var
    Registered: array of TAlgorithms;

const
    NoAlgorithm = 'This build has no background algorithm: background points ' +
        'are proposed and subtracted by a module it does not contain.';

procedure AddAlgorithms(const AInfo: TImplementationInfo;
    APropose: TProposeBackgroundPoints; ASubtract: TSubtractPiecewiseLinear;
    ARemovedCopy: TBackgroundRemovedCopy);
var
    Info: TImplementationInfo;
begin
    Info := AInfo;
    Info.Contract := BackgroundContract;
    RegisterImplementation(Info);
    SetLength(Registered, Length(Registered) + 1);
    Registered[High(Registered)].Id := Info.Id;
    Registered[High(Registered)].Propose := APropose;
    Registered[High(Registered)].Subtract := ASubtract;
    Registered[High(Registered)].RemovedCopy := ARemovedCopy;
end;

procedure RegisterBackgroundAlgorithms(APropose: TProposeBackgroundPoints;
    ASubtract: TSubtractPiecewiseLinear; ARemovedCopy: TBackgroundRemovedCopy);
var
    Info: TImplementationInfo;
begin
    //  REPLACED, NOT ADDED, as the peak search's native one is.
    UnregisterContract(BackgroundContract);
    Registered := nil;
    if not Assigned(APropose) then
        Exit;
    Info := Default(TImplementationInfo);
    Info.Id := NativeBackgroundId;
    Info.Name := 'Background arithmetic (native)';
    Info.Description := 'Proposes a profile''s background points and takes ' +
        'the background off, in the compute server itself, by the algorithms ' +
        'the module this build links provides. Needs nothing installed.';
    Info.Provider := NativeProvider;
    AddAlgorithms(Info, APropose, ASubtract, ARemovedCopy);
end;

procedure RegisterBackgroundImplementation(const AInfo: TImplementationInfo;
    APropose: TProposeBackgroundPoints; ASubtract: TSubtractPiecewiseLinear;
    ARemovedCopy: TBackgroundRemovedCopy);
begin
    AddAlgorithms(AInfo, APropose, ASubtract, ARemovedCopy);
end;

{ The algorithms this machine's choice resolves to; all nil when none. }
function InForce: TAlgorithms;
var
    Id: string;
    i: longint;
begin
    Result := Default(TAlgorithms);
    Id := ImplementationInForce(BackgroundContract).Id;
    for i := 0 to High(Registered) do
        if Registered[i].Id = Id then
            Exit(Registered[i]);
end;

function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
var
    A: TAlgorithms;
begin
    A := InForce;
    if not Assigned(A.Propose) then
        raise EUserException.Create(NoAlgorithm);
    Result := A.Propose(AData);
end;

procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);
var
    A: TAlgorithms;
begin
    A := InForce;
    if not Assigned(A.Subtract) then
        raise EUserException.Create(NoAlgorithm);
    A.Subtract(AData, ABackground);
end;

procedure RefuseWithoutBackgroundArithmetic;
var
    A: TAlgorithms;
begin
    A := InForce;
    if not (Assigned(A.Propose) and Assigned(A.Subtract) and
        Assigned(A.RemovedCopy)) then
        raise EUserException.Create(NoAlgorithm);
end;

function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;
var
    A: TAlgorithms;
begin
    A := InForce;
    if not Assigned(A.RemovedCopy) then
        raise EUserException.Create(NoAlgorithm);
    Result := A.RemovedCopy(AData);
end;

end.
