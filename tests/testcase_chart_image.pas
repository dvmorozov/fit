// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The line under an exported chart says where the data came from, and
sizes nobody could read are refused.)
}
unit testcase_chart_image;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_project_document, module_view_types,
    series_style, chart_image;

type
    TChartImageTest = class(TTestCase)
    published
        procedure AFetchedSeriesSaysItsSourceAndTheDayItWasFetched;
        procedure AFileFromThisComputerSaysNothing;
        procedure TheQueryIsLeftOutWhenThereWasNone;
        procedure ASizeNobodyCouldReadIsRefused;
        procedure TheDefaultSizeIsAllowed;
        procedure ASizeIsReadAsWidthByHeight;
        procedure NoAnswerAtAllIsTodaysPicture;
        procedure TheDefaultAnswersReadAsTheDefaultOptions;
        procedure AScaleIsANumberWithAPointOrAComma;
        procedure AutoGrowsLinesAndTextWithThePicture;
        procedure AutoScalesByTheSideThatGrewLess;
        procedure AScaleOutsideTheRangeOrNotANumberIsRefused;
        procedure TheLayersAreAnsweredYesOrNo;
        procedure AnAnswerThatIsNeitherYesNorNoIsRefused;
        procedure ASizeThatCannotBeReadOrDrawnIsRefused;
        procedure AnAnswerThatIsRefusedLeavesTheDefaults;
        procedure LeavingOutTheFitIntervalsHidesTheirBoundsOnly;
        procedure LeavingOutTheDifferenceHidesItAndItsNonRandomStretches;
        procedure EveryOtherSeriesIsAlwaysDrawn;
        procedure AYesOrNoReadsBackAsItWasWritten;
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

function Answers(const ASize, AScale, AIntervals,
    ADifference: string): TChartImageAnswers;
begin
    Result.Size := ASize;
    Result.Scale := AScale;
    Result.FitIntervals := AIntervals;
    Result.Difference := ADifference;
end;

procedure TChartImageTest.NoAnswerAtAllIsTodaysPicture;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', '', '', ''), O));
    AssertEquals(DefaultChartImageWidth, O.Width);
    AssertEquals(DefaultChartImageHeight, O.Height);
    AssertEquals('drawn as the window draws', 1.0, O.Scale, 0);
    AssertTrue(O.DrawFitIntervals);
    AssertTrue(O.DrawDifference);
end;

procedure TChartImageTest.TheDefaultAnswersReadAsTheDefaultOptions;
var
    O, D: TChartImageOptions;
    A: TChartImageAnswers;
begin
    A := DefaultChartImageAnswers;
    AssertEquals('a dialog starts from the size in words', '1600x900', A.Size);
    AssertEquals('', ReadChartImageAnswers(A, O));
    D := DefaultChartImageOptions;
    AssertEquals(D.Width, O.Width);
    AssertEquals(D.Height, O.Height);
    AssertEquals(D.Scale, O.Scale, 0);
    AssertEquals(D.DrawFitIntervals, O.DrawFitIntervals);
    AssertEquals(D.DrawDifference, O.DrawDifference);
end;

procedure TChartImageTest.AScaleIsANumberWithAPointOrAComma;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', '3', '', ''), O));
    AssertEquals(3.0, O.Scale, 0);
    AssertEquals('', ReadChartImageAnswers(Answers('', ' 2.5 ', '', ''), O));
    AssertEquals(2.5, O.Scale, 0);
    AssertEquals('', ReadChartImageAnswers(Answers('', '1,5', '', ''), O));
    AssertEquals(1.5, O.Scale, 0);
end;

procedure TChartImageTest.AutoGrowsLinesAndTextWithThePicture;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('4800x2700', 'Auto', '', ''), O));
    AssertEquals('three times the default size reads like it enlarged', 3.0,
        O.Scale, 1e-12);
    AssertEquals('', ReadChartImageAnswers(Answers('', 'auto', '', ''), O));
    AssertEquals('at the default size, as the window draws', 1.0, O.Scale, 0);
end;

procedure TChartImageTest.AutoScalesByTheSideThatGrewLess;
begin
    AssertEquals('wider only', 1.0, ChartImageAutoScale(3200, 900), 1e-12);
    AssertEquals('taller only', 1.0, ChartImageAutoScale(1600, 1800), 1e-12);
    AssertEquals(2.0, ChartImageAutoScale(4800, 1800), 1e-12);
    AssertEquals('smaller', 0.5, ChartImageAutoScale(800, 450), 1e-12);
end;

procedure TChartImageTest.AScaleOutsideTheRangeOrNotANumberIsRefused;
var
    O: TChartImageOptions;
begin
    AssertTrue('a word', Pos('scale', ReadChartImageAnswers(
        Answers('', 'big', '', ''), O)) > 0);
    AssertTrue('too small', ReadChartImageAnswers(Answers('', '0.1', '', ''), O) <> '');
    AssertTrue('too large', ReadChartImageAnswers(Answers('', '11', '', ''), O) <> '');
    AssertTrue('auto below the smallest scale',
        ReadChartImageAnswers(Answers('200x200', 'auto', '', ''), O) <> '');
end;

procedure TChartImageTest.TheLayersAreAnsweredYesOrNo;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', '', 'no', 'NO'), O));
    AssertFalse(O.DrawFitIntervals);
    AssertFalse(O.DrawDifference);
    AssertEquals('', ReadChartImageAnswers(Answers('', '', 'false', 'yes'), O));
    AssertFalse(O.DrawFitIntervals);
    AssertTrue(O.DrawDifference);
    AssertEquals('', ReadChartImageAnswers(Answers('', '', ' True ', ''), O));
    AssertTrue(O.DrawFitIntervals);
end;

procedure TChartImageTest.AnAnswerThatIsNeitherYesNorNoIsRefused;
var
    O: TChartImageOptions;
    Said: string;
begin
    Said := ReadChartImageAnswers(Answers('', '', 'maybe', ''), O);
    AssertTrue(Said, Pos('maybe', Said) > 0);
    AssertTrue(Said, Pos('fit intervals', Said) > 0);
    Said := ReadChartImageAnswers(Answers('', '', '', '1'), O);
    AssertTrue(Said, Pos('difference', Said) > 0);
end;

procedure TChartImageTest.ASizeThatCannotBeReadOrDrawnIsRefused;
var
    O: TChartImageOptions;
    Said: string;
begin
    Said := ReadChartImageAnswers(Answers('large', '', '', ''), O);
    AssertTrue(Said, Pos('WIDTHxHEIGHT', Said) > 0);
    Said := ReadChartImageAnswers(Answers('10x10', '', '', ''), O);
    AssertEquals(ChartImageSizeRefusal(10, 10), Said);
    AssertEquals('', ReadChartImageAnswers(Answers('1200x630', '', '', ''), O));
    AssertEquals(1200, O.Width);
    AssertEquals(630, O.Height);
end;

procedure TChartImageTest.AnAnswerThatIsRefusedLeavesTheDefaults;
var
    O: TChartImageOptions;
begin
    AssertTrue(ReadChartImageAnswers(Answers('1200x630', '2', 'no', 'perhaps'),
        O) <> '');
    AssertEquals(DefaultChartImageWidth, O.Width);
    AssertEquals(1.0, O.Scale, 0);
    AssertTrue(O.DrawFitIntervals);
end;

function Only(AIntervals, ADifference: boolean): TChartImageOptions;
begin
    Result := DefaultChartImageOptions;
    Result.DrawFitIntervals := AIntervals;
    Result.DrawDifference := ADifference;
end;

procedure TChartImageTest.LeavingOutTheFitIntervalsHidesTheirBoundsOnly;
begin
    AssertFalse(ChartImageDrawsSeries(RoleColorSource(crIntervalBound, 0),
        Only(False, True)));
    AssertTrue(ChartImageDrawsSeries(RoleColorSource(crIntervalBound, 0),
        Only(True, False)));
    AssertTrue('the difference stays', ChartImageDrawsSeries(
        RoleColorSource(crResidual, 0), Only(False, True)));
end;

procedure TChartImageTest.LeavingOutTheDifferenceHidesItAndItsNonRandomStretches;
begin
    AssertFalse(ChartImageDrawsSeries(RoleColorSource(crResidual, 0),
        Only(True, False)));
    AssertFalse('a verdict on the difference goes with it', ChartImageDrawsSeries(
        RoleColorSource(crNonRandom, 0), Only(True, False)));
    AssertTrue(ChartImageDrawsSeries(RoleColorSource(crResidual, 0),
        Only(False, True)));
end;

procedure TChartImageTest.EveryOtherSeriesIsAlwaysDrawn;
var
    R: TSeriesColorRole;
    C: TModuleColor;
begin
    for R := Low(TSeriesColorRole) to High(TSeriesColorRole) do
        if not (R in [crIntervalBound, crResidual, crNonRandom]) then
            AssertTrue(IntToStr(Ord(R)), ChartImageDrawsSeries(
                RoleColorSource(R, 1), Only(False, False)));
    C := Default(TModuleColor);
    AssertTrue('a module''s series', ChartImageDrawsSeries(ModuleColorSource(C),
        Only(False, False)));
    AssertTrue('a fixed colour', ChartImageDrawsSeries(
        Default(TSeriesColorSource), Only(False, False)));
end;

procedure TChartImageTest.AYesOrNoReadsBackAsItWasWritten;
var
    Yes: boolean;
begin
    AssertEquals('yes', ChartImageYesNo(True));
    AssertEquals('no', ChartImageYesNo(False));
    AssertTrue(TryReadChartImageYesNo(ChartImageYesNo(True), Yes));
    AssertTrue(Yes);
    AssertTrue(TryReadChartImageYesNo(ChartImageYesNo(False), Yes));
    AssertFalse(Yes);
    AssertTrue(TryReadChartImageYesNo('FALSE', Yes));
    AssertFalse(Yes);
    AssertFalse('a blank is no answer', TryReadChartImageYesNo('', Yes));
    AssertFalse(TryReadChartImageYesNo('y', Yes));
end;

initialization
    RegisterTest('unit', TChartImageTest);
end.
