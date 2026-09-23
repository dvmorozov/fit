// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Assembling the window's half of a project, and deciding how much of it
to put back.)

WHY IT IS NOT IN THE WINDOW, and this is the second time the answer has been
demonstrated rather than argued. The assembly WAS in the window - and three
things were silently missing from it for exactly as long, because the window is
excluded from the coverage target and nothing tests it:

  * the selected interval was never captured, so a project saved while a
    sub-interval was in force came back over the whole profile;
  * the user-defined formula was never captured, so a project using that curve
    type saved no formula - and the engine refuses to build that type without
    one, so the model came back empty;
  * the working context was written to the file and never read back, so the
    axis, the tab and the picking mode did not restore at all, while the user
    guide said they did.

None of those is a crash. All three are the failure this codebase keeps
producing: a path the user takes that no test does. So the assembly takes plain
values and lives here, and the window reads its widgets and hands them over.

WHAT THE APPLY SIDE DECIDES, which is why it is a plan rather than a setter:

  * an axis is applied only when the user CHOSE it. A project that never had one
    chosen carries whatever mode was in force, and forcing that on reopening
    would move a user onto an axis they never picked - the same distinction
    Settings_v1.ViewModeChosenByUser exists to make;
  * a tab is applied only when the window still has one at that index, because a
    build with fewer tabs must not be asked for the fifth of three;
  * a selected curve is applied only when the model still holds that handle. It
    may not: a curve type that is gone, or a module this build lacks.
}
unit project_ui_context;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, mscr_specimen_list, fit_project_document,
    fit_project_session;

type
    { What the WINDOW should put back from a project, and what it should leave
      alone.

      THE WINDOW'S ONLY. The selected interval and the provenance were here once
      and are not any more: they belong to the document rather than to any
      widget, so project_workflow adopts them straight from the document it
      opened. Carrying them here as well left four fields that were filled in and
      read by nothing - the exact shape this feature produced five times, and
      found the sixth time by asking of every field what writes it and what reads
      it (findings.md). }
    TProjectUiPlan = record
        ApplyAxis: boolean;
        ViewMode: longint;
        CustomAxisName: string;
        CustomAxisUnit: string;
        CustomAxisForward: string;
        CustomAxisInverse: string;

        ApplyTab: boolean;
        ActiveTab: longint;

        ApplySelectedCurve: boolean;
        SelectedCurveId: string;

        ApplySelectionMode: boolean;
        SelectionMode: longint;
    end;

{ The window's half of a project, from values it reads off its own widgets.

  Every field is a parameter rather than being fetched here, which is what keeps
  this unit free of the widget set - and what let the three fields that used to
  be missing become visible as missing. }
function BuildProjectContext(
    ACurveList: TMSCRCurveList; AViewModeChosenByUser: boolean;
    ASelectionMode, AActiveTab: longint;
    const ASelectedCurveId: string;
    const ACustomAxisName, ACustomAxisUnit: string;
    const ACustomAxisForward, ACustomAxisInverse: string;
    AIntervalInForce: boolean; AIntervalFrom, AIntervalTo: longint;
    AHasUserCurve: boolean; const AUserCurveExpression: string;
    const AProvenance: TProjectProvenance;
    const AAppVersion: string): TProjectClientContext;

{ How much of ADoc's working context this window can put back.

  ATabCount is how many tabs it has; AModelHoldsCurve says whether the model
  still holds the handle the project recorded as selected. }
function PlanUiRestore(const ADoc: TProjectDocument; ATabCount: longint;
    AModelHoldsCurve: boolean): TProjectUiPlan;

implementation

function BuildProjectContext(
    ACurveList: TMSCRCurveList; AViewModeChosenByUser: boolean;
    ASelectionMode, AActiveTab: longint;
    const ASelectedCurveId: string;
    const ACustomAxisName, ACustomAxisUnit: string;
    const ACustomAxisForward, ACustomAxisInverse: string;
    AIntervalInForce: boolean; AIntervalFrom, AIntervalTo: longint;
    AHasUserCurve: boolean; const AUserCurveExpression: string;
    const AProvenance: TProjectProvenance;
    const AAppVersion: string): TProjectClientContext;
begin
    Result := EmptyProjectClientContext;

    Result.HasUi := True;
    //  THE AXIS IS THE CURVE LIST'S, not a widget's - it is where ApplyViewMode
    //  puts the mode that is actually drawn, and it is a model object rather
    //  than part of the widget set, so reading it here is honest and saves the
    //  window a nil-guard it would not be able to test.
    if Assigned(ACurveList) then
        Result.Ui.ViewMode := ACurveList.FViewMode;
    Result.Ui.ViewModeChosenByUser := AViewModeChosenByUser;
    Result.Ui.SelectionMode := ASelectionMode;
    Result.Ui.ActiveTab := AActiveTab;
    //  BY HANDLE, which is the only thing that still names the same curve after
    //  an edit: the model's order is derived, so a row index would name another
    //  one. Captured at all because it was not - the plan decided whether to put
    //  it back and nothing ever put it there.
    Result.Ui.SelectedCurveId := ASelectedCurveId;
    Result.Ui.CustomAxisName := ACustomAxisName;
    Result.Ui.CustomAxisUnit := ACustomAxisUnit;
    Result.Ui.CustomAxisForward := ACustomAxisForward;
    Result.Ui.CustomAxisInverse := ACustomAxisInverse;

    //  AS INDICES. The profile they index may have been smoothed, so a
    //  coordinate need not name a sample any more.
    Result.SelectedIntervalInForce := AIntervalInForce;
    Result.SelectedIntervalFrom := AIntervalFrom;
    Result.SelectedIntervalTo := AIntervalTo;

    //  THE FORMULA IS THE CLIENT'S. The server does not report the expression it
    //  is fitting, and without it a restore cannot rebuild the user-defined type
    //  at all - CreateTasks refuses to.
    //
    //  An EMPTY formula is not a user curve, whatever the flag says: a curve
    //  saved without one is an entry that cannot become a curve, which
    //  curve_type_menu already refuses to select.
    Result.HasUserCurve := AHasUserCurve and (Trim(AUserCurveExpression) <> '');
    if Result.HasUserCurve then
        Result.UserCurveExpression := AUserCurveExpression;

    Result.Provenance := AProvenance;
    Result.Provenance.AppVersion := AAppVersion;
end;

function PlanUiRestore(const ADoc: TProjectDocument; ATabCount: longint;
    AModelHoldsCurve: boolean): TProjectUiPlan;
begin
    Result := Default(TProjectUiPlan);

    //  A project saved from a session that never touched the chart has no
    //  working context, and that is an ordinary file rather than a damaged one.
    if not ADoc.HasUi then
        Exit;

    //  ONLY WHEN THE USER CHOSE IT. A project that never had an axis chosen
    //  carries whatever was in force; forcing that on reopening would move
    //  someone onto an axis they never picked, which is the distinction
    //  ViewModeChosenByUser was added to make.
    Result.ApplyAxis := ADoc.Ui.ViewModeChosenByUser;
    Result.ViewMode := ADoc.Ui.ViewMode;
    Result.CustomAxisName := ADoc.Ui.CustomAxisName;
    Result.CustomAxisUnit := ADoc.Ui.CustomAxisUnit;
    Result.CustomAxisForward := ADoc.Ui.CustomAxisForward;
    Result.CustomAxisInverse := ADoc.Ui.CustomAxisInverse;

    //  A build with fewer tabs must not be asked for the fifth of three - and a
    //  project written by a build with a module's tab in it is exactly that.
    Result.ApplyTab := (ADoc.Ui.ActiveTab >= 0) and
        (ADoc.Ui.ActiveTab < ATabCount);
    Result.ActiveTab := ADoc.Ui.ActiveTab;

    //  The model may no longer hold it: a curve type that is gone, or a module
    //  this build does not have.
    Result.ApplySelectedCurve := (ADoc.Ui.SelectedCurveId <> '') and
        AModelHoldsCurve;
    Result.SelectedCurveId := ADoc.Ui.SelectedCurveId;

    //  RESTORED, because a half-finished pick is work: someone who saved while
    //  placing background points comes back to the same tool in their hand.
    Result.ApplySelectionMode := True;
    Result.SelectionMode := ADoc.Ui.SelectionMode;
end;

end.
