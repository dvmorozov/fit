// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The order a project is restored in, which is load-bearing and every
part of which is a way to lose the project silently.)

WHY THE ORDER IS A THING WITH ITS OWN TESTS. Restoring a project is not "set
every field". Six of the engine's setters clear something another one has just
written, so the sequence decides whether a project comes back or comes back
empty - and every one of these failures is silent: the file is fine, the calls
all succeed, and the model on screen is missing something.

  * PUT profile RESETS THE PROBLEM. Its state transition frees every point set,
    the curve attributes, the identity registry and the module sessions.
    Anything sent before it is gone.
  * SetCurveType calls MarkFitted([]) whenever the type actually changes, so
    setting it after the handles are adopted wipes every restored fitted flag -
    and the flag is what stops a rebuild re-seeding.
  * CreateTasks refuses to build the user-defined curve type with no formula, so
    the formula has to be in place before the first rebuild.
  * CreateTasks resolves every pick against the SELECTED interval when one is in
    force, so selecting it after the picks windows them against the wrong data.
  * SetRFactorBounds CLEARS the curve attributes - the store the fitted values
    live in - so bounds after values destroys the fit.
  * every step above clears or regenerates the curve attributes, so the values
    can only be written last.

THE PLAN IS DATA, and that is the point of the unit under test. The rules above
become a pure function of "which sections does this document hold", assertable
with no service, no socket and no engine - the shape pick_target and close_query
already have. What applies the steps is a separate thing, and can be a loop.
}
unit testcase_project_restore_order;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_project_document, fit_project_restore;

type
    TProjectRestoreOrderTest = class(TTestCase)
    private
        { A document holding every section, so every step is emitted. }
        function AFullDocument: TProjectDocument;
        { The plan for ADoc, as step names in order, comma-separated. }
        function PlanOf(const ADoc: TProjectDocument): string;
        { Where AStep appears in the plan for ADoc, or -1. }
        function PositionOf(const ADoc: TProjectDocument;
            AStep: TRestoreStep): longint;
        { Asserts AFirst is planned before ASecond. }
        procedure AssertBefore(const AWhy: string;
            AFirst, ASecond: TRestoreStep);
    published
        procedure TheProfileIsPushedBeforeAnythingElse;
        procedure TheCurveTypeIsSetBeforeAnyHandleIsAdopted;
        procedure TheFormulaIsSetBeforeTheFirstRebuild;
        procedure TheIntervalIsSelectedBeforeThePicksAreResolvedAgainstIt;
        procedure TheBoundsArePushedBeforeThePicks;
        procedure TheValuesAreWrittenLast;
        procedure TheModuleDocumentsComeAfterTheProfileThatResetsThem;

        procedure NoStepIsEmittedForASectionTheDocumentDoesNotHold;
        procedure AProjectWithNoFitPlansNoValueWrite;
        procedure AProjectWithNoUserCurvePlansNoFormula;
        procedure AProjectWithNoSelectedIntervalPlansNoSelection;
        procedure AnEmptyDocumentStillPlansTheProfileAndTheSettings;
        procedure EveryStepThePlanCanEmitIsEmittedForAFullDocument;
    end;

implementation

function TProjectRestoreOrderTest.AFullDocument: TProjectDocument;
begin
    Result := EmptyProjectDocument;
    SetLength(Result.Profile.X, 2);
    SetLength(Result.Profile.Y, 2);
    SetLength(Result.Background.X, 1);
    SetLength(Result.Background.Y, 1);
    SetLength(Result.Bounds.X, 2);
    SetLength(Result.Bounds.Y, 2);
    SetLength(Result.Positions.X, 1);
    SetLength(Result.Positions.Y, 1);
    Result.SelectedIntervalInForce := True;
    Result.HasUserCurve := True;
    Result.UserCurveExpression := 'A*x';
    SetLength(Result.Curves, 1);
    Result.Curves[0].Id := 'a';
    SetLength(Result.ModuleDocuments, 1);
    Result.ModuleDocuments[0].Module := 'sample';
    Result.ModuleDocuments[0].Content := '{}';
end;

function TProjectRestoreOrderTest.PlanOf(const ADoc: TProjectDocument): string;
var
    Plan: TRestorePlan;
    i: longint;
begin
    Result := '';
    Plan := PlanRestore(ADoc);
    for i := 0 to High(Plan) do
    begin
        if Result <> '' then
            Result := Result + ',';
        Result := Result + RestoreStepName(Plan[i]);
    end;
end;

function TProjectRestoreOrderTest.PositionOf(const ADoc: TProjectDocument;
    AStep: TRestoreStep): longint;
var
    Plan: TRestorePlan;
    i: longint;
begin
    Result := -1;
    Plan := PlanRestore(ADoc);
    for i := 0 to High(Plan) do
        if Plan[i] = AStep then
            Exit(i);
end;

procedure TProjectRestoreOrderTest.AssertBefore(const AWhy: string;
    AFirst, ASecond: TRestoreStep);
var
    Doc: TProjectDocument;
    A, B: longint;
begin
    Doc := AFullDocument;
    A := PositionOf(Doc, AFirst);
    B := PositionOf(Doc, ASecond);
    AssertTrue(RestoreStepName(AFirst) + ' is planned', A >= 0);
    AssertTrue(RestoreStepName(ASecond) + ' is planned', B >= 0);
    AssertTrue(AWhy, A < B);
end;

procedure TProjectRestoreOrderTest.TheProfileIsPushedBeforeAnythingElse;
var
    Plan: TRestorePlan;
begin
    //  IT RESETS THE PROBLEM. Setting a profile passes through ProfileWaiting,
    //  which frees every point set, the curve attributes, the identity registry
    //  and every module session. Anything sent before it is destroyed, and
    //  nothing anywhere reports that.
    Plan := PlanRestore(AFullDocument);
    AssertTrue('there is a plan', Length(Plan) > 0);
    AssertEquals('the profile goes first', 'profile', RestoreStepName(Plan[0]));
end;

procedure TProjectRestoreOrderTest.TheCurveTypeIsSetBeforeAnyHandleIsAdopted;
begin
    //  SetCurveType calls MarkFitted([]) whenever the type actually changes. Run
    //  after the picks, it wipes every fitted flag the restore just established
    //  - and the flag is the only thing that distinguishes a restored fit from
    //  a restored seed, so the model would silently re-seed on the next edit.
    AssertBefore('the settings precede the picks that carry the handles',
        rsSettings, rsPositions);
end;

procedure TProjectRestoreOrderTest.TheFormulaIsSetBeforeTheFirstRebuild;
begin
    //  CreateTasks refuses to build the user-defined curve type with no
    //  formula. The first rebuild is triggered by the picks, so the formula has
    //  to be there before them or the restore raises and leaves an empty model.
    AssertBefore('the formula precedes anything that rebuilds',
        rsUserCurve, rsPositions);
end;

procedure TProjectRestoreOrderTest.
    TheIntervalIsSelectedBeforeThePicksAreResolvedAgainstIt;
begin
    //  CreateTasks resolves every pick against the selected area when one is in
    //  force. Selecting it afterwards windows the picks against the wrong data,
    //  and a pick naming an x the active data does not contain fails an
    //  internal check at the NEXT edit rather than here.
    AssertBefore('the interval precedes the picks',
        rsSelectInterval, rsPositions);
end;

procedure TProjectRestoreOrderTest.TheBoundsArePushedBeforeThePicks;
begin
    //  SetRFactorBounds CLEARS the curve attributes, which is where the fitted
    //  values live. Bounds after the picks throws away the report they built;
    //  bounds after the values throws away the fit.
    AssertBefore('the bounds precede the picks', rsBounds, rsPositions);
    AssertBefore('and certainly precede the values', rsBounds, rsCurveValues);
end;

procedure TProjectRestoreOrderTest.TheValuesAreWrittenLast;
var
    Plan: TRestorePlan;
begin
    //  EVERY step above clears or regenerates the curve attributes, so this is
    //  the only position from which the values survive - and it is the rebuild
    //  this write triggers that hands each instance the values stored under its
    //  handle.
    Plan := PlanRestore(AFullDocument);
    AssertEquals('the values go last', 'curveValues',
        RestoreStepName(Plan[High(Plan)]));
end;

procedure TProjectRestoreOrderTest.
    TheModuleDocumentsComeAfterTheProfileThatResetsThem;
begin
    //  Setting a profile calls Reset on every module session, so a module's own
    //  document has to be posted after it. It also has to come after the picks:
    //  a module's markup contributes fit readiness and is sliced per interval.
    AssertBefore('after the profile', rsProfile, rsModules);
    AssertBefore('and after the picks', rsPositions, rsModules);
end;

procedure TProjectRestoreOrderTest.NoStepIsEmittedForASectionTheDocumentDoesNotHold;
var
    Doc: TProjectDocument;
begin
    //  A step for a section that is not there would push an empty set over an
    //  engine that had nothing there either - harmless in some cases and, for
    //  the profile, a reset of everything already restored.
    Doc := EmptyProjectDocument;
    AssertEquals('nothing to restore but the settings', 'settings',
        PlanOf(Doc));
end;

procedure TProjectRestoreOrderTest.AProjectWithNoFitPlansNoValueWrite;
var
    Doc: TProjectDocument;
begin
    //  A project saved after placing picks and before pressing Fit. Writing an
    //  empty value set would be a rebuild for nothing.
    Doc := AFullDocument;
    Doc.Curves := nil;
    AssertEquals('no value write is planned', -1,
        PositionOf(Doc, rsCurveValues));
end;

procedure TProjectRestoreOrderTest.AProjectWithNoUserCurvePlansNoFormula;
var
    Doc: TProjectDocument;
begin
    //  Pushing a formula for a problem whose curve type is a built-in one would
    //  leave the engine holding a user curve nobody asked for - and deleting a
    //  user curve is a distinct operation with its own meaning.
    Doc := AFullDocument;
    Doc.HasUserCurve := False;
    AssertEquals('no formula is planned', -1, PositionOf(Doc, rsUserCurve));
end;

procedure TProjectRestoreOrderTest.AProjectWithNoSelectedIntervalPlansNoSelection;
var
    Doc: TProjectDocument;
begin
    //  No selected interval means the whole profile, which is the engine's own
    //  default - not something to be asked for.
    Doc := AFullDocument;
    Doc.SelectedIntervalInForce := False;
    AssertEquals('no selection is planned', -1,
        PositionOf(Doc, rsSelectInterval));
end;

procedure TProjectRestoreOrderTest.AnEmptyDocumentStillPlansTheProfileAndTheSettings;
var
    Doc: TProjectDocument;
begin
    //  A NEW, EMPTY PROJECT still has settings - a curve type, an objective, a
    //  weighting - and they are what the user chose. Restoring nothing at all
    //  would silently put them back to whatever the process last had.
    Doc := EmptyProjectDocument;
    AssertTrue('the settings are always restored',
        PositionOf(Doc, rsSettings) >= 0);
    AssertEquals('but there is no profile to push', -1,
        PositionOf(Doc, rsProfile));
end;

procedure TProjectRestoreOrderTest.EveryStepThePlanCanEmitIsEmittedForAFullDocument;
var
    Step: TRestoreStep;
    Doc: TProjectDocument;
begin
    //  SELF-ENFORCING. A step added to the enumeration without a section to
    //  drive it, or without a place in the order, fails here rather than being
    //  quietly absent from every restore.
    Doc := AFullDocument;
    for Step := Low(TRestoreStep) to High(TRestoreStep) do
        AssertTrue(RestoreStepName(Step) + ' has a place in the plan',
            PositionOf(Doc, Step) >= 0);
end;

initialization
    //  A unit test: a pure function of a record. No engine, no socket, no file.
    RegisterTest('unit', TProjectRestoreOrderTest);
end.
