// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which modules get a report tab, and when a report tab can be shown.)

AN OPT-IN, and the decisions here are what make it one. A module that reports
nothing gets no tab, so a build of modules that judge nothing carries none; a
module that does report names its own tab, because what a report is called is
its field's word. Kept out of the form so each rule is a function a test calls.

HIDDEN UNTIL THERE IS SOMETHING TO READ. A report tab that shows before its
module has judged anything is an empty tab, and an empty tab reads as a broken
one.

RESTORING A TAB. A saved project names the tab that was in front by index. A
report tab is appended after every designed tab and is hidden while its module
has not reported - which, on opening a project, is still true at the moment the
tab would be restored. So a restore asks whether the tab it names is shown
(project_ui_context.PlanUiRestore). It asked for a count of the tabs before
the first hidden one until a build hid its FIRST tab - the background points',
with no background shape - and so restored none. FirstShownTab is where the
window opens when the tab it was designed to open on is hidden.
}
unit report_tabs;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_ui_host, module_report_types;

const
    { What a report tab says before its module has sent anything. The tab is
      hidden then, so this is only ever seen if a module shows an empty report
      on purpose - and it must still not be a blank box. }
    ReportTabEmptyText = 'Nothing has been checked yet.';

type
    TReportTab = record
        Module: string;
        Caption: string;
    end;

    TReportTabs = array of TReportTab;

{ One tab per module whose ReportCaption is not blank, in registration order, with
  the caption trimmed. }
function ReportTabsFor(const AModules: TUiModuleArray): TReportTabs;

{ The index of AModule's tab, or -1 when it declared none. }
function ReportTabIndex(const ATabs: TReportTabs; const AModule: string): longint;

{ The log line for a report from AModule, which declared no tab: a defect in
  the module, not something to show the user, and named so it can be fixed. }
function UndeclaredReportWarning(const AModule: string): string;

{ Whether a report tab showing AReport is shown at all. }
function ReportTabVisible(const AReport: TModuleReport): boolean;

{ The first tab AShown shows, or -1 when it shows none. }
function FirstShownTab(const AShown: array of boolean): longint;

implementation

function ReportTabsFor(const AModules: TUiModuleArray): TReportTabs;
var
    i, n: longint;
    Caption: string;
begin
    Result := nil;
    n := 0;
    for i := 0 to High(AModules) do
    begin
        Caption := Trim(AModules[i].ReportCaption);
        if Caption = '' then
            Continue;
        SetLength(Result, n + 1);
        Result[n].Module := AModules[i].Name;
        Result[n].Caption := Caption;
        Inc(n);
    end;
end;

function ReportTabIndex(const ATabs: TReportTabs; const AModule: string): longint;
var
    i: longint;
begin
    Result := -1;
    if AModule = '' then
        Exit;
    for i := 0 to High(ATabs) do
        if ATabs[i].Module = AModule then
            Exit(i);
end;

function UndeclaredReportWarning(const AModule: string): string;
begin
    Result := Format('ui: a report from module "%s", which declared no report ' +
        'tab, was not shown', [AModule]);
end;

function ReportTabVisible(const AReport: TModuleReport): boolean;
begin
    Result := not ReportIsEmpty(AReport);
end;

function FirstShownTab(const AShown: array of boolean): longint;
var
    i: longint;
begin
    for i := 0 to High(AShown) do
        if AShown[i] then
            Exit(i);
    Result := -1;
end;

end.
