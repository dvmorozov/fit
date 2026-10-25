// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every UI module the build links answers the window's questions the way
the window reads them.)

WHAT IS WALKED. The window asks every registered module, through module_menu
and report_tabs, which panel it fills, what it offers over a row and what its
report tab is called. A module that fills no panel and has no report answers
blank - and those answers were reached by no test at all, so a module answering
a panel id it does not own, a row menu over a curve it never placed, or a blank
report caption with spaces in it, would have gone unseen until the window
showed it. So this walks THE BUILD'S OWN MODULES, registered as each host
registers them (RegisterAppModules), through the functions the window calls.

NAMES NO MODULE. The framework may not; whatever this build links is walked,
and a bare core with none passes trivially.

WHY A UNIT TEST. It crosses no process, touches no file and fits nothing. The
UI module registry is process-wide and has no unregister, and other tests add
mock modules of their own to it, so nothing here assumes a count.
}
unit testcase_ui_module_answers;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_ui_host, module_menu, report_tabs, model_outline, app_modules;

type
    TUiModuleAnswersTest = class(TTestCase)
    private
        FModules: TUiModuleArray;
    protected
        procedure SetUp; override;
    published
        procedure EveryDeclaredPanelHasOneOwnerAndWordsItsEmptyState;
        procedure OverACurveNoModulePlacedNoModuleOffersARowMenu;
        procedure EveryReportTabIsOneModulesOwnCaption;
    end;

implementation

const
    //  A curve handle no module issued: the framework's rows are curves of the
    //  model, and a module may answer only for the ones it placed.
    HandleNobodyIssued = '{6F0D7C3A-2B1E-4C59-9A8D-000000000000}';

procedure TUiModuleAnswersTest.SetUp;
begin
    //  What both hosts call before any menu is built. Idempotent.
    RegisterAppModules;
    FModules := RegisteredUiModules;
end;

procedure TUiModuleAnswersTest.EveryDeclaredPanelHasOneOwnerAndWordsItsEmptyState;
var
    i: longint;
    Id: string;
    Text: TModulePanelText;
begin
    for i := 0 to High(FModules) do
    begin
        Id := FModules[i].PanelId;
        if Id = '' then
            //  Fills no panel: the window then never asks it for panel words
            //  (PanelTextFor matches no empty id).
            Continue;
        AssertEquals(FModules[i].Name + ' is the one module that owns panel ' +
            Id, i, ModuleIndexOfPanel(FModules, Id));
        AssertTrue(Id + ' is not the framework''s own panel',
            Id <> FrameworkStructureId);
        Text := PanelTextFor(FModules, Id);
        AssertTrue('the window finds the words for ' + Id, Text.Found);
        AssertTrue(FModules[i].Name + ' words its empty panel: a blank box ' +
            'reads as a broken one', Trim(Text.EmptyText) <> '');
    end;
end;

procedure TUiModuleAnswersTest.OverACurveNoModulePlacedNoModuleOffersARowMenu;
var
    Asked: TModuleIndexes;
    Owner: longint;
    Offer: TUiMenuDeclArray;
begin
    Asked := RowMenuModulesToAsk(FModules, FrameworkStructureId);
    AssertEquals('over the framework''s rows every module is asked',
        Length(FModules), Length(Asked));
    Offer := FirstRowMenuOffer(FModules, Asked, HandleNobodyIssued, Owner);
    AssertEquals('no module claims a curve it did not place', -1, Owner);
    AssertEquals('so nothing is offered over it', 0, Length(Offer));
end;

procedure TUiModuleAnswersTest.EveryReportTabIsOneModulesOwnCaption;
var
    Tabs: TReportTabs;
    Seen: TStringList;
    i, t: longint;
    Caption: string;
begin
    Tabs := ReportTabsFor(FModules);
    Seen := TStringList.Create;
    try
        for t := 0 to High(Tabs) do
        begin
            AssertTrue('a tab is captioned', Tabs[t].Caption <> '');
            AssertEquals('one tab per module: ' + Tabs[t].Module, -1,
                Seen.IndexOf(Tabs[t].Module));
            Seen.Add(Tabs[t].Module);
        end;
        //  And a module answering blank - spaces included - has no tab: an empty
        //  tab is what a build must not carry for a module that judges nothing.
        for i := 0 to High(FModules) do
        begin
            Caption := Trim(FModules[i].ReportCaption);
            AssertEquals(FModules[i].Name + ' has a tab exactly when it ' +
                'captions one', Caption <> '',
                Seen.IndexOf(FModules[i].Name) >= 0);
        end;
    finally
        Seen.Free;
    end;
end;

initialization
    RegisterTest('unit', TUiModuleAnswersTest);
end.
