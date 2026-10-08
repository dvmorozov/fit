// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for which modules get a report tab, and when a report tab shows.

  WHY THESE EXIST. A report tab is a module's opt-in: a build with no module that
  reports must not carry an empty tab the framework invented words for, and each
  module that does report is captioned in its own words. The tab is hidden while
  its module has nothing to say, so a restored project may name a tab that is not
  there to be selected yet - and asking the widget set for a hidden page is how a
  restore becomes a range error or a blank strip. }
unit testcase_report_tabs;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_ui_host, module_report_types,
    report_tabs, mock_ui_module;

type
    TReportTabsTest = class(TTestCase)
    private
        FA, FB, FC: TMockUiModule;
        function Modules: TUiModuleArray;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AModuleWithNoCaptionGetsNoTab;
        procedure NoModuleThatReportsMeansNoTab;
        procedure EachDeclaringModuleGetsItsOwnTabInItsOwnWords;
        procedure ABlankCaptionIsNoCaption;
        procedure ATabIsFoundByItsModule;
        procedure AModuleThatDeclaredNoTabHasNone;
        procedure AReportFromAModuleWithNoTabIsWarnedAboutByName;
        procedure AnEmptyReportHidesTheTab;
        procedure AReportWithSomethingInItShowsTheTab;
        procedure EveryTabUpToTheFirstHiddenOneCanBeRestored;
        procedure WithNoTabHiddenEveryTabCanBeRestored;
    end;

implementation

procedure TReportTabsTest.SetUp;
begin
    FA := TMockUiModule.Create('alpha');
    FB := TMockUiModule.Create('beta');
    FC := TMockUiModule.Create('gamma');
end;

procedure TReportTabsTest.TearDown;
begin
    FA.Free;
    FB.Free;
    FC.Free;
end;

function TReportTabsTest.Modules: TUiModuleArray;
begin
    SetLength(Result, 3);
    Result[0] := FA;
    Result[1] := FB;
    Result[2] := FC;
end;

procedure TReportTabsTest.AModuleWithNoCaptionGetsNoTab;
begin
    FB.SetReportCaption('Beta Analysis');
    AssertEquals(1, Length(ReportTabsFor(Modules)));
end;

procedure TReportTabsTest.NoModuleThatReportsMeansNoTab;
begin
    AssertEquals(0, Length(ReportTabsFor(Modules)));
    AssertEquals(0, Length(ReportTabsFor(nil)));
end;

procedure TReportTabsTest.EachDeclaringModuleGetsItsOwnTabInItsOwnWords;
var
    Tabs: TReportTabs;
begin
    FC.SetReportCaption('Gamma Checks');
    FA.SetReportCaption('Alpha Analysis');
    Tabs := ReportTabsFor(Modules);
    AssertEquals(2, Length(Tabs));
    //  In registration order, which is the order the menus are in.
    AssertEquals('alpha', Tabs[0].Module);
    AssertEquals('Alpha Analysis', Tabs[0].Caption);
    AssertEquals('gamma', Tabs[1].Module);
    AssertEquals('Gamma Checks', Tabs[1].Caption);
end;

procedure TReportTabsTest.ABlankCaptionIsNoCaption;
var
    Tabs: TReportTabs;
begin
    FA.SetReportCaption('   ');
    FB.SetReportCaption('  Beta  ');
    Tabs := ReportTabsFor(Modules);
    AssertEquals(1, Length(Tabs));
    AssertEquals('Beta', Tabs[0].Caption);
end;

procedure TReportTabsTest.ATabIsFoundByItsModule;
var
    Tabs: TReportTabs;
begin
    FA.SetReportCaption('A');
    FC.SetReportCaption('C');
    Tabs := ReportTabsFor(Modules);
    AssertEquals(0, ReportTabIndex(Tabs, 'alpha'));
    AssertEquals(1, ReportTabIndex(Tabs, 'gamma'));
end;

procedure TReportTabsTest.AModuleThatDeclaredNoTabHasNone;
begin
    FA.SetReportCaption('A');
    AssertEquals(-1, ReportTabIndex(ReportTabsFor(Modules), 'beta'));
    AssertEquals(-1, ReportTabIndex(ReportTabsFor(Modules), ''));
end;

procedure TReportTabsTest.AReportFromAModuleWithNoTabIsWarnedAboutByName;
var
    Warning: string;
begin
    //  A defect in the module, not something to show the user - but a warning
    //  that does not name the module cannot be acted on.
    Warning := UndeclaredReportWarning('beta');
    AssertTrue(Warning, Pos('"beta"', Warning) > 0);
    AssertTrue(Warning, Pos('declared no report tab', Warning) > 0);
end;

procedure TReportTabsTest.AnEmptyReportHidesTheTab;
var
    R: TModuleReport;
begin
    R := Default(TModuleReport);
    R.Module := 'alpha';
    R.Title := 'Nothing yet';
    AssertFalse(ReportTabVisible(R));
end;

procedure TReportTabsTest.AReportWithSomethingInItShowsTheTab;
var
    R: TModuleReport;
begin
    R := Default(TModuleReport);
    SetLength(R.Sections, 1);
    AssertTrue(ReportTabVisible(R));
end;

procedure TReportTabsTest.EveryTabUpToTheFirstHiddenOneCanBeRestored;
begin
    AssertEquals(5, RestorableTabCount([True, True, True, True, True, False, True]));
    AssertEquals(0, RestorableTabCount([False, True]));
end;

procedure TReportTabsTest.WithNoTabHiddenEveryTabCanBeRestored;
begin
    AssertEquals(3, RestorableTabCount([True, True, True]));
    AssertEquals(0, RestorableTabCount([]));
end;

initialization
    RegisterTest('unit', TReportTabsTest);
end.
