// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The line under an exported chart says where the data came from, and
sizes nobody could read are refused.)
}
unit testcase_chart_image;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_project_document, chart_image;

type
    TChartImageTest = class(TTestCase)
    published
        procedure AFetchedSeriesSaysItsSourceAndTheDayItWasFetched;
        procedure AFileFromThisComputerSaysNothing;
        procedure TheQueryIsLeftOutWhenThereWasNone;
        procedure ASizeNobodyCouldReadIsRefused;
        procedure TheDefaultSizeIsAllowed;
        procedure ASizeIsReadAsWidthByHeight;
    end;

implementation

procedure TChartImageTest.AFetchedSeriesSaysItsSourceAndTheDayItWasFetched;
var
    P: TProjectProvenance;
begin
    P := Default(TProjectProvenance);
    P.OriginSourceTitle := 'FRED (St. Louis Fed)';
    P.OriginQuery := 'DCOILBRENTEU';
    P.OriginRetrievedAt := '2026-10-19T09:30:00';
    AssertEquals('Source: FRED (St. Louis Fed), DCOILBRENTEU, retrieved 2026-10-19',
        ChartImageCaption(P));
end;

procedure TChartImageTest.AFileFromThisComputerSaysNothing;
var
    P: TProjectProvenance;
begin
    P := Default(TProjectProvenance);
    P.SourcePath := '/home/u/x.dat';
    AssertEquals('', ChartImageCaption(P));
end;

procedure TChartImageTest.TheQueryIsLeftOutWhenThereWasNone;
var
    P: TProjectProvenance;
begin
    P := Default(TProjectProvenance);
    P.OriginSourceTitle := 'European Central Bank';
    AssertEquals('Source: European Central Bank', ChartImageCaption(P));
end;

procedure TChartImageTest.ASizeNobodyCouldReadIsRefused;
begin
    AssertTrue(ChartImageSizeRefusal(10, 900) <> '');
    AssertTrue(ChartImageSizeRefusal(1600, 90000) <> '');
end;

procedure TChartImageTest.TheDefaultSizeIsAllowed;
begin
    AssertEquals('', ChartImageSizeRefusal(DefaultChartImageWidth,
        DefaultChartImageHeight));
end;

procedure TChartImageTest.ASizeIsReadAsWidthByHeight;
var
    W, H: integer;
begin
    AssertTrue(TryParseChartImageSize('1200x630', W, H));
    AssertEquals(1200, W);
    AssertEquals(630, H);
    AssertTrue('either case', TryParseChartImageSize(' 800X600 ', W, H));
    AssertFalse(TryParseChartImageSize('big', W, H));
    AssertFalse(TryParseChartImageSize('x600', W, H));
end;

initialization
    RegisterTest('unit', TChartImageTest);
end.
