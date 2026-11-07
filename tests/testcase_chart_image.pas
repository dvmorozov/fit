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
        procedure NoAnswerAtAllIsTodaysPicture;
        procedure TheDefaultAnswersReadAsTheDefaultOptions;
        procedure AScaleIsANumberWithAPointOrAComma;
        procedure AutoGrowsLinesAndTextWithThePicture;
        procedure AutoScalesByTheSideThatGrewLess;
        procedure AScaleOutsideTheRangeOrNotANumberIsRefused;
        procedure ASizeThatCannotBeReadOrDrawnIsRefused;
        procedure AnAnswerThatIsRefusedLeavesTheDefaults;
        procedure TheSourceLineIsDrawnUnlessTheAnswerSaysNo;
        procedure TheSourceLineIsLeftOutWhenTheAnswerIsNo;
        procedure ASourceAnswerThatIsNotYesOrNoIsRefused;
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

function Answers(const ASize, AScale: string): TChartImageAnswers;
begin
    Result.Size := ASize;
    Result.Scale := AScale;
end;

procedure TChartImageTest.NoAnswerAtAllIsTodaysPicture;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', ''), O));
    AssertEquals(DefaultChartImageWidth, O.Width);
    AssertEquals(DefaultChartImageHeight, O.Height);
    AssertEquals('drawn as the window draws', 1.0, O.Scale, 0);
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
end;

procedure TChartImageTest.AScaleIsANumberWithAPointOrAComma;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', '3'), O));
    AssertEquals(3.0, O.Scale, 0);
    AssertEquals('', ReadChartImageAnswers(Answers('', ' 2.5 '), O));
    AssertEquals(2.5, O.Scale, 0);
    AssertEquals('', ReadChartImageAnswers(Answers('', '1,5'), O));
    AssertEquals(1.5, O.Scale, 0);
end;

procedure TChartImageTest.AutoGrowsLinesAndTextWithThePicture;
var
    O: TChartImageOptions;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('4800x2700', 'Auto'), O));
    AssertEquals('three times the default size reads like it enlarged', 3.0,
        O.Scale, 1e-12);
    AssertEquals('', ReadChartImageAnswers(Answers('', 'auto'), O));
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
        Answers('', 'big'), O)) > 0);
    AssertTrue('too small', ReadChartImageAnswers(Answers('', '0.1'), O) <> '');
    AssertTrue('too large', ReadChartImageAnswers(Answers('', '11'), O) <> '');
    AssertTrue('auto below the smallest scale',
        ReadChartImageAnswers(Answers('200x200', 'auto'), O) <> '');
end;

procedure TChartImageTest.ASizeThatCannotBeReadOrDrawnIsRefused;
var
    O: TChartImageOptions;
    Said: string;
begin
    Said := ReadChartImageAnswers(Answers('large', ''), O);
    AssertTrue(Said, Pos('WIDTHxHEIGHT', Said) > 0);
    Said := ReadChartImageAnswers(Answers('10x10', ''), O);
    AssertEquals(ChartImageSizeRefusal(10, 10), Said);
    AssertEquals('', ReadChartImageAnswers(Answers('1200x630', ''), O));
    AssertEquals(1200, O.Width);
    AssertEquals(630, O.Height);
end;

procedure TChartImageTest.AnAnswerThatIsRefusedLeavesTheDefaults;
var
    O: TChartImageOptions;
begin
    AssertTrue(ReadChartImageAnswers(Answers('1200x630', 'huge'), O) <> '');
    AssertEquals(DefaultChartImageWidth, O.Width);
    AssertEquals(1.0, O.Scale, 0);
end;

procedure TChartImageTest.TheSourceLineIsDrawnUnlessTheAnswerSaysNo;
var
    O: TChartImageOptions;
    A: TChartImageAnswers;
begin
    AssertEquals('', ReadChartImageAnswers(Answers('', ''), O));
    AssertTrue('no answer keeps today''s picture', O.DrawSource);
    A := Answers('', '');
    A.Source := ' Yes ';
    AssertEquals('', ReadChartImageAnswers(A, O));
    AssertTrue(O.DrawSource);
    AssertTrue('the dialog starts from yes',
        DefaultChartImageAnswers.Source = 'yes');
    AssertTrue(DefaultChartImageOptions.DrawSource);
end;

procedure TChartImageTest.TheSourceLineIsLeftOutWhenTheAnswerIsNo;
var
    O: TChartImageOptions;
    A: TChartImageAnswers;
begin
    A := Answers('', '');
    A.Source := 'no';
    AssertEquals('', ReadChartImageAnswers(A, O));
    AssertFalse(O.DrawSource);
    A.Source := ' NO ';
    AssertEquals('', ReadChartImageAnswers(A, O));
    AssertFalse('either case', O.DrawSource);
end;

procedure TChartImageTest.ASourceAnswerThatIsNotYesOrNoIsRefused;
var
    O: TChartImageOptions;
    A: TChartImageAnswers;
    Said: string;
begin
    A := Answers('', '');
    A.Source := 'maybe';
    Said := ReadChartImageAnswers(A, O);
    AssertTrue(Said, Pos('yes or no', Said) > 0);
    AssertTrue('the defaults are left', O.DrawSource);
end;

initialization
    RegisterTest('unit', TChartImageTest);
end.
