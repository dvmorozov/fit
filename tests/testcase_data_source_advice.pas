// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the wizard refuses, and whether it says anything useful about it.)

fit_advice's SHAPE AND fit_advice's REASON: the function that decides whether a
button works is the function that says why it does not, so the two cannot drift
apart. Every case here asserts the REASON as well as the verdict, because a
refusal that says only "cannot continue" is the failure this unit exists to
prevent - and a test asserting only the boolean would not notice it happening.
}
unit testcase_data_source_advice;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, data_source_advice;

type
    TDataSourceAdviceTest = class(TTestCase)
    private
        procedure AssertRefused(const AVerdict: TDataSourceVerdict;
            const AExpectedWord, AWhat: string);
    published
        procedure AnAnsweredQueryCanBeSearched;
        procedure AnEmptyRequiredFieldNamesTheField;
        procedure AContainerSaysWhatItIs;
        procedure AFileWithNoReaderNamesItsKind;
        procedure AReadableFileCanBeChosen;
        procedure NothingDownloadedYetCannotBecomeAProject;
        procedure AnUnreadableDownloadGivesTheReadersOwnWords;
        procedure AnEmptyProfileIsRefusedRatherThanShown;
        procedure APreviewedFileWithPointsCanBecomeAProject;
    end;

implementation

procedure TDataSourceAdviceTest.AssertRefused(const AVerdict: TDataSourceVerdict;
    const AExpectedWord, AWhat: string);
begin
    AssertFalse(AWhat + ' must be refused', AVerdict.Allowed);
    AssertTrue(AWhat + ' must say why, and name what it saw: got "' +
        AVerdict.Reason + '"',
        Pos(LowerCase(AExpectedWord), LowerCase(AVerdict.Reason)) > 0);
end;

procedure TDataSourceAdviceTest.AnAnsweredQueryCanBeSearched;
var
    V: TDataSourceVerdict;
begin
    V := AdviseSearch('');
    AssertTrue('an answered query on a reachable source searches', V.Allowed);
    AssertEquals('an allowed action explains nothing', '', V.Reason);
end;

procedure TDataSourceAdviceTest.AnEmptyRequiredFieldNamesTheField;
begin
    //  The field is NAMED, not described: "Enter Symbol to search" says which
    //  box, where "fill in the fields" does not.
    AssertRefused(AdviseSearch('Symbol'), 'symbol',
        'a search with an empty symbol');
end;

procedure TDataSourceAdviceTest.AContainerSaysWhatItIs;
begin
    AssertRefused(AdviseChoose(False, False, ''), 'record',
        'a record rather than a file');
end;

procedure TDataSourceAdviceTest.AFileWithNoReaderNamesItsKind;
begin
    AssertRefused(AdviseChoose(True, False, '.XLSX'), '.xlsx',
        'a file no reader handles');
end;

procedure TDataSourceAdviceTest.AReadableFileCanBeChosen;
begin
    AssertTrue('a readable file can be taken forward',
        AdviseChoose(True, True, '.XY').Allowed);
end;

procedure TDataSourceAdviceTest.NothingDownloadedYetCannotBecomeAProject;
begin
    AssertRefused(AdviseCreate(False, False, 0, ''), 'downloaded',
        'creating a project before anything was fetched');
end;

procedure TDataSourceAdviceTest.AnUnreadableDownloadGivesTheReadersOwnWords;
begin
    //  The reader knows what it expected and what it found; this function
    //  would only make that vaguer.
    AssertRefused(AdviseCreate(True, False, 0, 'The CSV file is empty.'),
        'csv file is empty', 'a file the reader refused');
end;

procedure TDataSourceAdviceTest.AnEmptyProfileIsRefusedRatherThanShown;
begin
    //  The quiet failure that matters most: a downloaded error page parses to
    //  nothing and becomes an empty chart that looks like a working import.
    AssertRefused(AdviseCreate(True, True, 0, ''), 'no data points',
        'a download that parsed to nothing');
end;

procedure TDataSourceAdviceTest.APreviewedFileWithPointsCanBecomeAProject;
begin
    AssertTrue('a previewed file with data becomes a project',
        AdviseCreate(True, True, 1254, '').Allowed);
end;

initialization
    RegisterTest('unit', TDataSourceAdviceTest);
end.
