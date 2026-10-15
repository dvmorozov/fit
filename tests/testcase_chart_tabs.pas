// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The tabs of the chart area, and what the spectrum tab draws.)
}
unit testcase_chart_tabs;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, chart_tabs, residual_randomness;

type
    TChartTabsTest = class(TTestCase)
    published
        procedure ThereAreThreeTabsModelFirst;
        procedure EveryTabHasACaptionAndATopic;
        procedure ASavedTabThatNoLongerExistsOpensTheModel;
    end;

implementation

procedure TChartTabsTest.ThereAreThreeTabsModelFirst;
begin
    AssertEquals(3, ChartTabCount);
    AssertEquals('the model first, where the window opens', 'Model',
        ChartTabCaption(ctModel));
    AssertEquals('R-factor', ChartTabCaption(ctRFactor));
    AssertEquals('Residual Spectrum', ChartTabCaption(ctSpectrum));
end;

procedure TChartTabsTest.EveryTabHasACaptionAndATopic;
var
    T: TChartTab;
begin
    for T := Low(TChartTab) to High(TChartTab) do
    begin
        AssertTrue('a caption', ChartTabCaption(T) <> '');
        AssertTrue('a topic', ChartTabTopic(T) <> '');
    end;
end;

procedure TChartTabsTest.ASavedTabThatNoLongerExistsOpensTheModel;
begin
    AssertTrue('as saved', RestoredChartTab(2) = ctSpectrum);
    AssertTrue('out of range', RestoredChartTab(7) = ctModel);
    AssertTrue('never saved', RestoredChartTab(-1) = ctModel);
end;

initialization
    RegisterTest('unit', TChartTabsTest);
end.
