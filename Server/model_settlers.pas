// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a module does to its curves before they are computed.)

SOME CURVES ARE PLACED BY OTHERS. A module's nested component sits on a leg of
its parent: the builder puts it there when the model is built. A fit changes
the parent on every evaluation and rebuilds nothing, so without this the
component stayed where it was first put - the fitted model broke the module's
own rule, and reopening the project, which rebuilds, measured a different model
(findings.md, "A fitted model did not reopen as it was saved").

So a module registers a SETTLER, and every task calls each settler on its
curves before it computes them (TFitTask.ComputeProfile) - in a fit, on every
evaluation. A settler acts only on its own module's curves and leaves the rest;
it must not log per call at a tier a release writes. The framework names no
module, and a model with nothing to settle pays a loop over an empty list.

Registered once, at start-up or when a module's unit is linked; called from the
fit workers side by side, so the list is read-only while fits run.
}
unit model_settlers;

{$mode objfpc}{$H+}

interface

uses
    self_copied_component;

type
    { Puts ACurves in the relation its module requires, before they are
      computed. }
    TModelSettler = procedure(ACurves: TSelfCopiedCompList);

{ Adds ASettler; adding it twice keeps one. }
procedure RegisterModelSettler(ASettler: TModelSettler);
{ Removes ASettler - for a test that registered its own. }
procedure UnregisterModelSettler(ASettler: TModelSettler);
{ Calls every registered settler on ACurves. }
procedure SettleModel(ACurves: TSelfCopiedCompList);

implementation

var
    Settlers: array of TModelSettler;

procedure RegisterModelSettler(ASettler: TModelSettler);
var
    i: longint;
begin
    for i := 0 to High(Settlers) do
        if Settlers[i] = ASettler then
            Exit;
    SetLength(Settlers, Length(Settlers) + 1);
    Settlers[High(Settlers)] := ASettler;
end;

procedure UnregisterModelSettler(ASettler: TModelSettler);
var
    i, j: longint;
begin
    for i := 0 to High(Settlers) do
        if Settlers[i] = ASettler then
        begin
            for j := i to High(Settlers) - 1 do
                Settlers[j] := Settlers[j + 1];
            SetLength(Settlers, Length(Settlers) - 1);
            Exit;
        end;
end;

procedure SettleModel(ACurves: TSelfCopiedCompList);
var
    i: longint;
begin
    for i := 0 to High(Settlers) do
        Settlers[i](ACurves);
end;

end.
