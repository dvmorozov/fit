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
        { THE RESIDUAL SPECTRUM IS A MODULE'S TO OFFER (the scientific one's):
          a product without it has the model's tab and the R-factor's. }
        procedure TheModelAndTheRFactorAreEveryBuilds;
        procedure TheSpectrumIsOnlyWhereAModuleOffersIt;
        procedure ASavedTabThisBuildDoesNotOfferOpensTheModel;
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
    //  As saved, in a build that offers it.
    OfferChartTab(ctSpectrum);
    AssertTrue('as saved', RestoredChartTab(2) = ctSpectrum);
    AssertTrue('out of range', RestoredChartTab(7) = ctModel);
    AssertTrue('never saved', RestoredChartTab(-1) = ctModel);
end;

procedure TChartTabsTest.TheModelAndTheRFactorAreEveryBuilds;
begin
    AssertTrue(ChartTabOfferedIn(ctModel, []));
    AssertTrue(ChartTabOfferedIn(ctRFactor, []));
    AssertTrue(ctModel in OfferedChartTabs);
    AssertTrue(ctRFactor in OfferedChartTabs);
end;

procedure TChartTabsTest.TheSpectrumIsOnlyWhereAModuleOffersIt;
begin
    AssertFalse(ChartTabOfferedIn(ctSpectrum, []));
    AssertTrue(ChartTabOfferedIn(ctSpectrum, [ctSpectrum]));
    OfferChartTab(ctSpectrum);
    AssertTrue('offered once a module offers it', ctSpectrum in OfferedChartTabs);
end;

procedure TChartTabsTest.ASavedTabThisBuildDoesNotOfferOpensTheModel;
begin
    AssertTrue(RestoredChartTabIn(2, [ctModel, ctRFactor]) = ctModel);
    AssertTrue(RestoredChartTabIn(2, [ctModel, ctRFactor, ctSpectrum]) = ctSpectrum);
    AssertTrue(RestoredChartTabIn(1, []) = ctRFactor);
end;

initialization
    RegisterTest('unit', TChartTabsTest);
end.
