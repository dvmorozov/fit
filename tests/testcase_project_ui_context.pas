// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The window's half of a project: what is captured, and how much of it
comes back.)

WHY THIS FILE EXISTS, and it is worth stating because it is a second instance of
the same lesson. The assembly this tests was inside the window, where nothing can
reach it and coverage deliberately does not look - and three things were missing
from it for as long as it lived there:

  * the SELECTED INTERVAL was never captured, so a project saved while a
    sub-interval was in force reopened over the whole profile;
  * the USER-DEFINED FORMULA was never captured, so a project using that curve
    type saved no formula - and the engine refuses to build that type without
    one, so the model came back empty;
  * the working context was WRITTEN AND NEVER READ, so the axis, the tab and the
    picking mode did not restore at all, while the user guide said they did.

Not one of those is a crash, and no test failed. They are the failure this
codebase keeps producing: a path the user takes that no test does. The first
three tests below are each one of them.
}
unit testcase_project_ui_context;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    mscr_specimen_list, fit_project_document, fit_project_session,
    project_ui_context;

type
    TProjectUiContextTest = class(TTestCase)
    private
        { Owned by the fixture, because BuildProjectContext only reads it. }
        FList: TMSCRCurveList;
        { A context as the window would build one, with everything set. }
        function AContext: TProjectClientContext;
        { A document whose working context is worth putting back. }
        function ADocumentWithUi: TProjectDocument;
    protected
        procedure TearDown; override;
    published
        //  The three that were missing.
        procedure TheSelectedIntervalIsCaptured;
        procedure TheUserDefinedFormulaIsCaptured;
        procedure TheWorkingContextIsCapturedForPuttingBack;

        //  The rest of the capture.
        procedure TheAxisAndTheTabAndThePickingModeAreCaptured;
        procedure WhereTheDataCameFromIsCarriedWithTheAppVersion;
        procedure NoIntervalInForceCapturesNone;
        procedure AUserCurveWithNoFormulaIsNotAUserCurve;

        //  What comes back.
        procedure AnAxisTheUserChoseIsPutBack;
        procedure AnAxisTheUserNeverChoseIsLeftAlone;
        procedure ATabThisBuildDoesNotHaveIsNotAskedFor;
        procedure ATabThisBuildHasIsPutBack;
        procedure ACurveTheModelNoLongerHoldsIsNotSelected;
        procedure ACurveTheModelStillHoldsIsSelected;
        procedure ThePickingModeComesBack;
        procedure AProjectWithNoWorkingContextChangesNothing;
        procedure WithNoCurveListThereIsNoAxisToCapture;
    end;

implementation

procedure TProjectUiContextTest.TearDown;
begin
    FreeAndNil(FList);
end;

function TProjectUiContextTest.AContext: TProjectClientContext;
var
    Prov: TProjectProvenance;
begin
    Prov := Default(TProjectProvenance);
    Prov.SourcePath := 'Data/2.dat';
    Prov.SourceHash := 'abc123';
    FList := TMSCRCurveList.Create;
    FList.FViewMode := 2;
    Result := BuildProjectContext(
        FList, True,       //  the axis is the curve list's; chosen by the user
        3, 1,              //  picking mode, tab
        '0a0a0a0a-1111-2222-3333-444444444444',
        'd', 'A', 'x*2', 'x/2',
        True, 12, 88,      //  a sub-interval is in force
        True, 'A*exp(-x/tau)',
        Prov, '1.2.0.1731');
end;

function TProjectUiContextTest.ADocumentWithUi: TProjectDocument;
begin
    Result := EmptyProjectDocument;
    Result.HasUi := True;
    Result.Ui.ViewMode := 2;
    Result.Ui.ViewModeChosenByUser := True;
    Result.Ui.SelectionMode := 3;
    Result.Ui.ActiveTab := 1;
    Result.Ui.SelectedCurveId := '0a0a0a0a-1111-2222-3333-444444444444';
    Result.Ui.CustomAxisName := 'd';
    Result.Ui.CustomAxisForward := 'x*2';
end;

procedure TProjectUiContextTest.TheSelectedIntervalIsCaptured;
var
    C: TProjectClientContext;
begin
    //  MISSING FOR AS LONG AS THIS LIVED IN THE WINDOW. Without it the restore
    //  plan emits no selection step, and a project saved while the user was
    //  working on one peak reopens across the whole profile - no error, just
    //  the wrong thing.
    C := AContext;
    AssertTrue('in force', C.SelectedIntervalInForce);
    AssertEquals('from', 12, C.SelectedIntervalFrom);
    AssertEquals('to', 88, C.SelectedIntervalTo);
end;

procedure TProjectUiContextTest.TheUserDefinedFormulaIsCaptured;
var
    C: TProjectClientContext;
begin
    //  ALSO MISSING. The server does not report the formula it is fitting, so
    //  the client is the only side that has it - and the engine refuses to build
    //  the user-defined type without one, so a project using it came back empty.
    C := AContext;
    AssertTrue('there is one', C.HasUserCurve);
    AssertEquals('A*exp(-x/tau)', C.UserCurveExpression);
end;

procedure TProjectUiContextTest.TheWorkingContextIsCapturedForPuttingBack;
var
    Plan: TProjectUiPlan;
begin
    //  THE THIRD: it was captured and written to the file, and nothing ever
    //  read it back. This is the half that says what to put back.
    Plan := PlanUiRestore(ADocumentWithUi, 4, True);
    AssertTrue('the axis', Plan.ApplyAxis);
    AssertTrue('the tab', Plan.ApplyTab);
    AssertTrue('the picking mode', Plan.ApplySelectionMode);
    AssertTrue('the selected curve', Plan.ApplySelectedCurve);
end;

procedure TProjectUiContextTest.TheAxisAndTheTabAndThePickingModeAreCaptured;
var
    C: TProjectClientContext;
begin
    C := AContext;
    AssertTrue('', C.HasUi);
    AssertEquals('the axis', 2, C.Ui.ViewMode);
    AssertTrue('and that the user chose it', C.Ui.ViewModeChosenByUser);
    AssertEquals('the picking mode', 3, C.Ui.SelectionMode);
    AssertEquals('the tab in front', 1, C.Ui.ActiveTab);
    AssertEquals('and which curve was selected, by handle',
        '0a0a0a0a-1111-2222-3333-444444444444', C.Ui.SelectedCurveId);
    AssertEquals('the user-defined axis', 'd', C.Ui.CustomAxisName);
    AssertEquals('', 'x*2', C.Ui.CustomAxisForward);
    AssertEquals('', 'x/2', C.Ui.CustomAxisInverse);
end;

procedure TProjectUiContextTest.WhereTheDataCameFromIsCarriedWithTheAppVersion;
var
    C: TProjectClientContext;
begin
    C := AContext;
    AssertEquals('Data/2.dat', C.Provenance.SourcePath);
    AssertEquals('abc123', C.Provenance.SourceHash);
    AssertEquals('the build that wrote it', '1.2.0.1731',
        C.Provenance.AppVersion);
end;

procedure TProjectUiContextTest.NoIntervalInForceCapturesNone;
var
    C: TProjectClientContext;
    Prov: TProjectProvenance;
begin
    //  No selected interval means the whole profile, which is the engine's own
    //  default rather than something to ask for.
    Prov := Default(TProjectProvenance);
    C := BuildProjectContext(nil, False, 0, 0, '', '', '', '', '',
        False, 0, 0, False, '', Prov, '1.0');
    AssertFalse('none', C.SelectedIntervalInForce);
end;

procedure TProjectUiContextTest.AUserCurveWithNoFormulaIsNotAUserCurve;
var
    C: TProjectClientContext;
    Prov: TProjectProvenance;
begin
    //  A curve saved without its formula is an entry that cannot become a
    //  curve - curve_type_menu already refuses to select one. Recording it as a
    //  user curve would make the restore push an empty formula and then fail to
    //  build the type, which is a worse way to say the same thing.
    Prov := Default(TProjectProvenance);
    C := BuildProjectContext(nil, False, 0, 0, '', '', '', '', '',
        False, 0, 0, True, '   ', Prov, '1.0');
    AssertFalse('not one', C.HasUserCurve);
    AssertEquals('and nothing to push', '', C.UserCurveExpression);
end;

procedure TProjectUiContextTest.AnAxisTheUserChoseIsPutBack;
var
    Plan: TProjectUiPlan;
begin
    Plan := PlanUiRestore(ADocumentWithUi, 4, True);
    AssertTrue('applied', Plan.ApplyAxis);
    AssertEquals('', 2, Plan.ViewMode);
    AssertEquals('with its definition', 'd', Plan.CustomAxisName);
    AssertEquals('', 'x*2', Plan.CustomAxisForward);
end;

procedure TProjectUiContextTest.AnAxisTheUserNeverChoseIsLeftAlone;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    //  A project that never had one chosen carries whatever mode was in force.
    //  Forcing that on reopening would move someone onto an axis they never
    //  picked - which is exactly the distinction ViewModeChosenByUser exists to
    //  make, and it has to hold here too.
    Doc := ADocumentWithUi;
    Doc.Ui.ViewModeChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertFalse('left alone', Plan.ApplyAxis);
end;

procedure TProjectUiContextTest.ATabThisBuildDoesNotHaveIsNotAskedFor;
var
    Plan: TProjectUiPlan;
begin
    //  A project written by a build with a module's tab in it, opened by one
    //  without. Asking for the fifth of three is how a restore turns into a
    //  range error in the widget set.
    Plan := PlanUiRestore(ADocumentWithUi, 1, True);
    AssertFalse('not asked for', Plan.ApplyTab);
end;

procedure TProjectUiContextTest.ATabThisBuildHasIsPutBack;
var
    Plan: TProjectUiPlan;
begin
    Plan := PlanUiRestore(ADocumentWithUi, 2, True);
    AssertTrue('put back', Plan.ApplyTab);
    AssertEquals('', 1, Plan.ActiveTab);
end;

procedure TProjectUiContextTest.ACurveTheModelNoLongerHoldsIsNotSelected;
var
    Plan: TProjectUiPlan;
begin
    //  It may be gone: a curve type this build does not have, or a module that
    //  is not in it. Selecting a row for a curve that is not there is a lookup
    //  failure several gestures later.
    Plan := PlanUiRestore(ADocumentWithUi, 4, False);
    AssertFalse('not selected', Plan.ApplySelectedCurve);
end;

procedure TProjectUiContextTest.ACurveTheModelStillHoldsIsSelected;
var
    Plan: TProjectUiPlan;
begin
    Plan := PlanUiRestore(ADocumentWithUi, 4, True);
    AssertTrue('selected', Plan.ApplySelectedCurve);
    AssertEquals('by handle, not by row',
        '0a0a0a0a-1111-2222-3333-444444444444', Plan.SelectedCurveId);
end;

procedure TProjectUiContextTest.ThePickingModeComesBack;
var
    Plan: TProjectUiPlan;
begin
    //  A half-finished pick is work: someone who saved while placing background
    //  points comes back to the same tool in their hand.
    Plan := PlanUiRestore(ADocumentWithUi, 4, True);
    AssertTrue('', Plan.ApplySelectionMode);
    AssertEquals('', 3, Plan.SelectionMode);
end;

procedure TProjectUiContextTest.AProjectWithNoWorkingContextChangesNothing;
var
    Plan: TProjectUiPlan;
begin
    //  A project saved from a session that never touched the chart. An ordinary
    //  file, not a damaged one - and nothing about the window should move.
    Plan := PlanUiRestore(EmptyProjectDocument, 4, True);
    AssertFalse('the axis', Plan.ApplyAxis);
    AssertFalse('the tab', Plan.ApplyTab);
    AssertFalse('the picking mode', Plan.ApplySelectionMode);
    AssertFalse('the selected curve', Plan.ApplySelectedCurve);
end;

procedure TProjectUiContextTest.WithNoCurveListThereIsNoAxisToCapture;
var
    C: TProjectClientContext;
    Prov: TProjectProvenance;
begin
    //  A window that has never had a model has no curve list, and asking one
    //  for its axis is how a capture faults on the first save of a fresh
    //  session.
    Prov := Default(TProjectProvenance);
    C := BuildProjectContext(nil, False, 0, 0, '', '', '', '', '',
        False, 0, 0, False, '', Prov, '1.0');
    AssertEquals('the default axis', 0, C.Ui.ViewMode);
    AssertEquals('and no curve selected', '', C.Ui.SelectedCurveId);
end;

initialization
    //  A unit test: plain values in, records out. No window.
    RegisterTest('unit', TProjectUiContextTest);
end.
