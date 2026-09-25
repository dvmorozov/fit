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
    coordinate_axis, axis_mode_registry, axis_mode_registration, axis_choice,
    diffraction_axis_modes, chart_axes,
    fit_project_document, fit_project_session,
    project_ui_context;

type
    TProjectUiContextTest = class(TTestCase)
    private
        { Owned by the fixture, because BuildProjectContext only reads it. }
        FAxes: TChartAxes;
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
        procedure WithNoAxesThereIsNoAxisToCapture;
        //  The value axis, and what the data said.
        procedure TheValueAxisIsCapturedBesideTheArgument;
        procedure WhatTheDataSaidIsCaptured;
        procedure WhatTheDataSaidIsAlwaysPutBack;
        procedure TheBarsDatesAreCaptured;
        procedure TheBarsDatesAreAlwaysPutBack;
        procedure TheValueAxisIsPutBackOnItsOwn;
        //  A file written before axis ids.
        procedure AFormerDisplayModeIsPutBackAsItsMode;
        procedure AnUnchosenFormerModeIsStillLeftAlone;
    end;

implementation

procedure TProjectUiContextTest.TearDown;
begin
    FreeAndNil(FAxes);
end;

function TwoBarDates: TAxisDates;
begin
    SetLength(Result, 2);
    Result[0] := EncodeDate(2020, 9, 24);
    Result[1] := EncodeDate(2020, 9, 25);
end;

function TProjectUiContextTest.AContext: TProjectClientContext;
var
    Prov: TProjectProvenance;
    Definition: TAxisDefinition;
begin
    Prov := Default(TProjectProvenance);
    Prov.SourcePath := 'Data/2.dat';
    Prov.SourceHash := 'abc123';
    RegisterAllAxisModes;
    FreeAndNil(FAxes);
    FAxes := TChartAxes.Create;
    Definition.Name := 'd';
    Definition.UnitName := 'A';
    Definition.Forward := 'x*2';
    Definition.Inverse := 'x/2';
    FAxes.SetWaveLength(1.54);
    FAxes.SetDefinition(adArgument, Definition);
    //  Chosen by the user, as the window's menu handler does it.
    FAxes.Choose(adArgument, SinThetaOverLambdaAxisModeId);
    FAxes.Choose(adValue, LogarithmicAxisModeId);
    Result := BuildProjectContext(
        FAxes, ThetaAxisModeId, IntensityAxisModeId,   //  what the data said
        TwoBarDates,       //  and the day of each bar
        3, 1,              //  picking mode, tab
        '0a0a0a0a-1111-2222-3333-444444444444',
        True, 12, 88,      //  a sub-interval is in force
        True, 'A*exp(-x/tau)',
        Prov, '1.2.0.1731');
end;

function TProjectUiContextTest.ADocumentWithUi: TProjectDocument;
begin
    Result := EmptyProjectDocument;
    SetLength(Result.Ui.ArgumentDates, 2);
    Result.Ui.ArgumentDates[0] := EncodeDate(2020, 9, 24);
    Result.Ui.ArgumentDates[1] := EncodeDate(2020, 9, 25);
    Result.HasUi := True;
    Result.Ui.Argument.Mode := SinThetaOverLambdaAxisModeId;
    Result.Ui.Argument.ChosenByUser := True;
    Result.Ui.Argument.DataMode := ThetaAxisModeId;
    Result.Ui.Value.Mode := LogarithmicAxisModeId;
    Result.Ui.Value.ChosenByUser := True;
    Result.Ui.Value.DataMode := IntensityAxisModeId;
    Result.Ui.SelectionMode := 3;
    Result.Ui.ActiveTab := 1;
    Result.Ui.SelectedCurveId := '0a0a0a0a-1111-2222-3333-444444444444';
    Result.Ui.Argument.CustomName := 'd';
    Result.Ui.Argument.CustomForward := 'x*2';
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
    AssertTrue('the axis', Plan.Axes[adArgument].Apply);
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
    AssertEquals('the axis', SinThetaOverLambdaAxisModeId, C.Ui.Argument.Mode);
    AssertTrue('and that the user chose it', C.Ui.Argument.ChosenByUser);
    AssertEquals('the picking mode', 3, C.Ui.SelectionMode);
    AssertEquals('the tab in front', 1, C.Ui.ActiveTab);
    AssertEquals('and which curve was selected, by handle',
        '0a0a0a0a-1111-2222-3333-444444444444', C.Ui.SelectedCurveId);
    AssertEquals('the user-defined axis', 'd', C.Ui.Argument.CustomName);
    AssertEquals('', 'x*2', C.Ui.Argument.CustomForward);
    AssertEquals('', 'x/2', C.Ui.Argument.CustomInverse);
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
    C := BuildProjectContext(nil, '', '', nil, 0, 0, '',
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
    C := BuildProjectContext(nil, '', '', nil, 0, 0, '',
        False, 0, 0, True, '   ', Prov, '1.0');
    AssertFalse('not one', C.HasUserCurve);
    AssertEquals('and nothing to push', '', C.UserCurveExpression);
end;

procedure TProjectUiContextTest.AnAxisTheUserChoseIsPutBack;
var
    Plan: TProjectUiPlan;
begin
    Plan := PlanUiRestore(ADocumentWithUi, 4, True);
    AssertTrue('applied', Plan.Axes[adArgument].Apply);
    AssertEquals('', SinThetaOverLambdaAxisModeId, Plan.Axes[adArgument].Mode);
    AssertEquals('with its definition', 'd', Plan.Axes[adArgument].Definition.Name);
    AssertEquals('', 'x*2', Plan.Axes[adArgument].Definition.Forward);
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
    Doc.Ui.Argument.ChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertFalse('left alone', Plan.Axes[adArgument].Apply);
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
    AssertFalse('the axis', Plan.Axes[adArgument].Apply);
    AssertFalse('nor the value''s', Plan.Axes[adValue].Apply);
    AssertFalse('the tab', Plan.ApplyTab);
    AssertFalse('the picking mode', Plan.ApplySelectionMode);
    AssertFalse('the selected curve', Plan.ApplySelectedCurve);
end;

procedure TProjectUiContextTest.WithNoAxesThereIsNoAxisToCapture;
var
    C: TProjectClientContext;
    Prov: TProjectProvenance;
begin
    //  A capture before the window has its axes must not fault on the first
    //  save of a fresh session.
    Prov := Default(TProjectProvenance);
    C := BuildProjectContext(nil, '', '', nil, 0, 0, '',
        False, 0, 0, False, '', Prov, '1.0');
    AssertEquals('no axis', '', C.Ui.Argument.Mode);
    AssertFalse('chosen by nobody', C.Ui.Argument.ChosenByUser);
    AssertEquals('and no curve selected', '', C.Ui.SelectedCurveId);
end;

procedure TProjectUiContextTest.TheValueAxisIsCapturedBesideTheArgument;
var
    C: TProjectClientContext;
begin
    C := AContext;
    AssertEquals(LogarithmicAxisModeId, C.Ui.Value.Mode);
    AssertTrue(C.Ui.Value.ChosenByUser);
end;

procedure TProjectUiContextTest.WhatTheDataSaidIsCaptured;
var
    C: TProjectClientContext;
begin
    //  The project keeps the points, not the file: nothing will read the file
    //  again to find out the argument was bars and the value a price.
    C := AContext;
    AssertEquals(ThetaAxisModeId, C.Ui.Argument.DataMode);
    AssertEquals(IntensityAxisModeId, C.Ui.Value.DataMode);
end;

procedure TProjectUiContextTest.WhatTheDataSaidIsAlwaysPutBack;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    //  Not a choice, so not subject to one: a fact about the profile.
    Doc := ADocumentWithUi;
    Doc.Ui.Argument.ChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertEquals(ThetaAxisModeId, Plan.Axes[adArgument].DataMode);
    AssertEquals(IntensityAxisModeId, Plan.Axes[adValue].DataMode);
end;

procedure TProjectUiContextTest.TheBarsDatesAreCaptured;
var
    C: TProjectClientContext;
begin
    C := AContext;
    AssertEquals(2, Length(C.Ui.ArgumentDates));
    AssertEquals(EncodeDate(2020, 9, 25), C.Ui.ArgumentDates[1], 0);
end;

procedure TProjectUiContextTest.TheBarsDatesAreAlwaysPutBack;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    //  A fact about the profile, like what the data said: not a choice.
    Doc := ADocumentWithUi;
    Doc.Ui.Argument.ChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertEquals(2, Length(Plan.ArgumentDates));
    AssertEquals(EncodeDate(2020, 9, 24), Plan.ArgumentDates[0], 0);
end;

procedure TProjectUiContextTest.TheValueAxisIsPutBackOnItsOwn;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    Doc := ADocumentWithUi;
    Doc.Ui.Argument.ChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertFalse('the argument is left alone', Plan.Axes[adArgument].Apply);
    AssertTrue('the value is not', Plan.Axes[adValue].Apply);
    AssertEquals(LogarithmicAxisModeId, Plan.Axes[adValue].Mode);
end;

procedure TProjectUiContextTest.AFormerDisplayModeIsPutBackAsItsMode;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    //  A project saved before axis ids: 'viewMode' 2 was Sin(Theta)/Lambda.
    Doc := EmptyProjectDocument;
    Doc.HasUi := True;
    Doc.Ui.HasLegacyViewMode := True;
    Doc.Ui.LegacyViewMode := 2;
    Doc.Ui.Argument.ChosenByUser := True;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertTrue(Plan.Axes[adArgument].Apply);
    AssertEquals(SinThetaOverLambdaAxisModeId, Plan.Axes[adArgument].Mode);
    AssertEquals('the value was never remembered', AutomaticAxisModeId,
        Plan.Axes[adValue].Mode);
end;

procedure TProjectUiContextTest.AnUnchosenFormerModeIsStillLeftAlone;
var
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
begin
    Doc := EmptyProjectDocument;
    Doc.HasUi := True;
    Doc.Ui.HasLegacyViewMode := True;
    Doc.Ui.LegacyViewMode := 0;
    Doc.Ui.Argument.ChosenByUser := False;
    Plan := PlanUiRestore(Doc, 4, True);
    AssertFalse(Plan.Axes[adArgument].Apply);
end;

initialization
    //  A unit test: plain values in, records out. No window.
    RegisterTest('unit', TProjectUiContextTest);
end.
