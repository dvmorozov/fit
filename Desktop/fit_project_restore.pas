// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The order a project is restored in, as data.)

RESTORING A PROJECT IS NOT "SET EVERY FIELD". Six of the engine's setters clear
something another one has just written, so the sequence decides whether a project
comes back or comes back empty - and every one of those failures is silent: the
file is fine, every call succeeds, and the model on screen is missing something.

  * PUT profile RESETS THE PROBLEM. Entering ProfileWaiting frees every point
    set, the curve attributes, the identity registry and every module session.
    Anything sent before it is gone.
  * SetCurveType calls MarkFitted([]) whenever the type actually changes, so
    setting it after the handles are adopted wipes every restored fitted flag -
    and that flag is the only thing that distinguishes a restored FIT from a
    restored SEED.
  * CreateTasks refuses to build the user-defined curve type with no formula, so
    the formula must be in place before the first rebuild - which the picks
    trigger.
  * CreateTasks resolves every pick against the SELECTED interval when one is in
    force, so selecting it after the picks windows them against the wrong data,
    and a pick naming an x the active data does not hold fails an internal check
    at the NEXT edit rather than during the restore.
  * SetRFactorBounds CLEARS the curve attributes - where the fitted values live
    - so the bounds must precede both the picks and the values.
  * every step above clears or regenerates the curve attributes, so the values
    can only be written last. It is the rebuild that write triggers which hands
    each rebuilt instance the values stored under its handle.

WHY THE PLAN IS DATA AND NOT A PROCEDURE. Expressed as an ordered list derived
from "which sections does this document hold", the rules above become a pure
function assertable with no service, no socket and no engine - the shape
pick_target and close_query already have. What APPLIES the steps is then a loop
over the list, and the thing worth testing is not in the loop.

THE STEPS ARE NOT THE VERBS. A step says WHICH PART OF THE DOCUMENT goes next,
not which method to call; the caller owns the mapping, because it owns the
service. That is what keeps this unit free of IFitService and therefore free of
the engine.
}
unit fit_project_restore;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_project_document;

type
    { One thing that has to happen, in the order the type declares them.

      THE DECLARATION ORDER IS THE RESTORE ORDER. Everything below reads the
      enumeration rather than repeating the sequence, so there is exactly one
      place the order is stated and a step inserted in the wrong position is
      wrong once rather than in three lists that must agree. }
    TRestoreStep = (
        { Resets the problem, so nothing may precede it. }
        rsProfile,
        { Before any handle is adopted: changing the curve type clears every
          fitted flag. }
        rsSettings,
        { Before the first rebuild, or building the user-defined type raises. }
        rsUserCurve,
        { Before the picks: it moves the state, and after them it moves it back. }
        rsBackground,
        { Before the picks are resolved against it. }
        rsSelectInterval,
        { Before the picks and the values: writing bounds clears the curve
          attributes. }
        rsBounds,
        { The pivot: the picks with their handles. }
        rsPositions,
        { After the profile that reset every module session, and after the picks,
          because a module's markup contributes fit readiness per interval. }
        rsModules,
        { Last: every step above clears or regenerates the store these live in. }
        rsCurveValues);

    TRestorePlan = array of TRestoreStep;

{ The steps ADoc needs, in order - and only for the sections it actually holds.

  A step for an absent section would push an empty set at an engine that has
  nothing there either: harmless for some, and for the profile a reset of
  everything already restored. }
function PlanRestore(const ADoc: TProjectDocument): TRestorePlan;

{ Whether ADoc holds anything for AStep. }
function ProjectHasStep(const ADoc: TProjectDocument;
    AStep: TRestoreStep): boolean;

{ A stable name for a step, for a log line and for a test's failure message. }
function RestoreStepName(AStep: TRestoreStep): string;

implementation

function ProjectHasStep(const ADoc: TProjectDocument;
    AStep: TRestoreStep): boolean;
begin
    case AStep of
        rsProfile:
            Result := Length(ADoc.Profile.X) > 0;
        //  ALWAYS. A project's settings are what the user chose - the curve
        //  type, the objective, the weighting - and restoring nothing would
        //  leave whatever the process last had, silently.
        rsSettings:
            Result := True;
        rsUserCurve:
            Result := ADoc.HasUserCurve;
        rsBackground:
            Result := Length(ADoc.Background.X) > 0;
        rsSelectInterval:
            Result := ADoc.SelectedIntervalInForce;
        rsBounds:
            Result := Length(ADoc.Bounds.X) > 0;
        rsPositions:
            Result := Length(ADoc.Positions.X) > 0;
        rsModules:
            Result := Length(ADoc.ModuleDocuments) > 0;
        rsCurveValues:
            Result := Length(ADoc.Curves) > 0;
    else
        Result := False;
    end;
end;

function PlanRestore(const ADoc: TProjectDocument): TRestorePlan;
var
    Step: TRestoreStep;
    n: longint;
begin
    Result := nil;
    n := 0;
    //  Over the enumeration, in its own declaration order - which is the one
    //  statement of the sequence in this program.
    for Step := Low(TRestoreStep) to High(TRestoreStep) do
        if ProjectHasStep(ADoc, Step) then
        begin
            SetLength(Result, n + 1);
            Result[n] := Step;
            Inc(n);
        end;
end;

function RestoreStepName(AStep: TRestoreStep): string;
begin
    case AStep of
        rsProfile:        Result := 'profile';
        rsSettings:       Result := 'settings';
        rsUserCurve:      Result := 'userCurve';
        rsBackground:     Result := 'background';
        rsSelectInterval: Result := 'selectInterval';
        rsBounds:         Result := 'bounds';
        rsPositions:      Result := 'positions';
        rsModules:        Result := 'modules';
        rsCurveValues:    Result := 'curveValues';
    else
        Result := 'unknown';
    end;
end;

end.
