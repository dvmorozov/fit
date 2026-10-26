// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Workbooks read as tables: the first sheet of an .xlsx or an .ods.)

Two halves. The XML of a sheet turned into lines is a rule over text, tested
with the XML written here (unit). A workbook file built here with FPC's zipper
and read by the loader the import uses is a file on disk (integration).
}
unit testcase_spreadsheet;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    spreadsheet_sheet, spreadsheet_file_loader, data_loader, title_points_set;

type
    TSpreadsheetSheetTest = class(TTestCase)
    published
        procedure AReferenceNamesItsColumn;
        procedure AnXlsxSheetIsItsRowsOfCells;
        procedure AStringCellIsLookedUpInTheSharedStrings;
        procedure AnInlineStringIsItsOwnText;
        procedure ACellTheSheetLeavesOutKeepsItsPlace;
        procedure ARowTheSheetLeavesOutKeepsItsPlace;
        procedure ACellsTabIsNotASeparator;
        procedure TheFirstSheetIsFoundThroughTheWorkbook;
        procedure WithNoWorkbookTheFirstSheetIsSheetOne;
        procedure AnOdsTableIsItsRowsOfCells;
        procedure AnOdsNumberIsItsValueNotItsText;
        procedure RepeatedOdsCellsAreWrittenOut;
        procedure ASheetEndingInAMillionEmptyRowsEndsAtOne;
        procedure OnlyTheFirstOdsTableIsRead;
        //  A whole workbook, built in memory.
        procedure AnXlsxWorkbookIsItsFirstSheet;
        procedure AnOdsWorkbookIsItsFirstTable;
        procedure AZipThatIsNoWorkbookIsRefusedInWords;
        procedure BytesThatAreNoZipAreRefusedInWords;
        procedure AWorkbookNamingAMissingSheetIsRefused;
    end;

    TSpreadsheetFileTest = class(TTestCase)
    private
        FDir: string;
        function Workbook(const AName: string;
            const ANamesAndTexts: array of string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnXlsxBecomesAProfileWithItsColumnsNamed;
        procedure AnOdsBecomesAProfile;
        procedure AZipThatIsNoWorkbookIsRefusedSayingSo;
        procedure AFileThatIsNoZipIsRefusedSayingSo;
    end;

implementation

uses
    zipper, sample_columns;

function S(const AText: string): TStringStream;
begin
    Result := TStringStream.Create(AText);
end;

const
    SheetHead = '<?xml version="1.0" encoding="UTF-8"?><worksheet ' +
        'xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">' +
        '<sheetData>';
    SheetTail = '</sheetData></worksheet>';
    OdsHead = '<?xml version="1.0" encoding="UTF-8"?><office:document-content ' +
        'xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" ' +
        'xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" ' +
        'xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">' +
        '<office:body><office:spreadsheet>';
    OdsTail = '</office:spreadsheet></office:body></office:document-content>';

function XlsxLines(const ARows, AShared: string): TStringList;
var
    Sheet, Shared: TStringStream;
begin
    Sheet := S(SheetHead + ARows + SheetTail);
    Shared := nil;
    if AShared <> '' then
        Shared := S('<?xml version="1.0"?><sst xmlns="http://schemas.' +
            'openxmlformats.org/spreadsheetml/2006/main">' + AShared + '</sst>');
    try
        Result := XlsxSheetLines(Sheet, Shared);
    finally
        Shared.Free;
        Sheet.Free;
    end;
end;

function OdsLines(const ATables: string): TStringList;
var
    Content: TStringStream;
begin
    Content := S(OdsHead + ATables + OdsTail);
    try
        Result := OdsSheetLines(Content);
    finally
        Content.Free;
    end;
end;

{ ---------------------------------- sheets ----------------------------------- }

procedure TSpreadsheetSheetTest.AReferenceNamesItsColumn;
begin
    AssertEquals(0, ColumnOfReference('A1'));
    AssertEquals(2, ColumnOfReference('C12'));
    AssertEquals(27, ColumnOfReference('AB7'));
end;

procedure TSpreadsheetSheetTest.AnXlsxSheetIsItsRowsOfCells;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1"><v>10</v></c><c r="B1"><v>1.5</v></c></row>' +
        '<row r="2"><c r="A2"><v>11</v></c><c r="B2"><v>2.5</v></c></row>', '');
    try
        AssertEquals(2, L.Count);
        AssertEquals('10'#9'1.5', L[0]);
        AssertEquals('11'#9'2.5', L[1]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AStringCellIsLookedUpInTheSharedStrings;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1" t="s"><v>0</v></c>' +
        '<c r="B1" t="s"><v>1</v></c></row>',
        '<si><t>angle</t></si><si><r><t>inten</t></r><r><t>sity</t></r></si>');
    try
        AssertEquals('angle'#9'intensity', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AnInlineStringIsItsOwnText;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1" t="inlineStr"><is><t>x</t></is></c></row>', '');
    try
        AssertEquals('x', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.ACellTheSheetLeavesOutKeepsItsPlace;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1"><v>1</v></c><c r="C1"><v>3</v></c></row>', '');
    try
        AssertEquals('1'#9#9'3', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.ARowTheSheetLeavesOutKeepsItsPlace;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1"><v>1</v></c></row>' +
        '<row r="3"><c r="A3"><v>3</v></c></row>', '');
    try
        AssertEquals(3, L.Count);
        AssertEquals('', L[1]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.ACellsTabIsNotASeparator;
var
    L: TStringList;
begin
    L := XlsxLines('<row r="1"><c r="A1" t="inlineStr"><is><t>a' + #9 + 'b</t></is></c>' +
        '<c r="B1"><v>2</v></c></row>', '');
    try
        AssertEquals('a b'#9'2', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.TheFirstSheetIsFoundThroughTheWorkbook;
var
    Book, Rels: TStringStream;
begin
    Book := S('<?xml version="1.0"?><workbook xmlns="http://schemas.' +
        'openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.' +
        'openxmlformats.org/officeDocument/2006/relationships"><sheets>' +
        '<sheet name="Data" sheetId="1" r:id="rId3"/>' +
        '<sheet name="Notes" sheetId="2" r:id="rId4"/></sheets></workbook>');
    Rels := S('<?xml version="1.0"?><Relationships xmlns="http://schemas.' +
        'openxmlformats.org/package/2006/relationships">' +
        '<Relationship Id="rId4" Target="worksheets/sheet1.xml"/>' +
        '<Relationship Id="rId3" Target="worksheets/sheet7.xml"/></Relationships>');
    try
        AssertEquals('xl/worksheets/sheet7.xml', XlsxFirstSheetPath(Book, Rels));
    finally
        Rels.Free;
        Book.Free;
    end;
end;

procedure TSpreadsheetSheetTest.WithNoWorkbookTheFirstSheetIsSheetOne;
begin
    AssertEquals('xl/worksheets/sheet1.xml', XlsxFirstSheetPath(nil, nil));
end;

procedure TSpreadsheetSheetTest.AnOdsTableIsItsRowsOfCells;
var
    L: TStringList;
begin
    L := OdsLines('<table:table table:name="Sheet1">' +
        '<table:table-row><table:table-cell office:value-type="string">' +
        '<text:p>x</text:p></table:table-cell><table:table-cell ' +
        'office:value-type="string"><text:p>y</text:p></table:table-cell>' +
        '</table:table-row><table:table-row><table:table-cell ' +
        'office:value-type="float" office:value="1"><text:p>1</text:p>' +
        '</table:table-cell><table:table-cell office:value-type="float" ' +
        'office:value="2"><text:p>2</text:p></table:table-cell>' +
        '</table:table-row></table:table>');
    try
        AssertEquals(2, L.Count);
        AssertEquals('x'#9'y', L[0]);
        AssertEquals('1'#9'2', L[1]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AnOdsNumberIsItsValueNotItsText;
var
    L: TStringList;
begin
    L := OdsLines('<table:table><table:table-row><table:table-cell ' +
        'office:value-type="float" office:value="1.5"><text:p>1.50 kV</text:p>' +
        '</table:table-cell></table:table-row></table:table>');
    try
        AssertEquals('1.5', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.RepeatedOdsCellsAreWrittenOut;
var
    L: TStringList;
begin
    L := OdsLines('<table:table><table:table-row><table:table-cell ' +
        'table:number-columns-repeated="3" office:value-type="float" ' +
        'office:value="7"/></table:table-row></table:table>');
    try
        AssertEquals('7'#9'7'#9'7', L[0]);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.ASheetEndingInAMillionEmptyRowsEndsAtOne;
var
    L: TStringList;
begin
    L := OdsLines('<table:table><table:table-row><table:table-cell ' +
        'office:value-type="float" office:value="1"/></table:table-row>' +
        '<table:table-row table:number-rows-repeated="1048575">' +
        '<table:table-cell/></table:table-row></table:table>');
    try
        AssertEquals(2, L.Count);
    finally
        L.Free;
    end;
end;

procedure TSpreadsheetSheetTest.OnlyTheFirstOdsTableIsRead;
var
    L: TStringList;
begin
    L := OdsLines('<table:table><table:table-header-rows><table:table-row>' +
        '<table:table-cell><text:p>a</text:p></table:table-cell></table:table-row>' +
        '</table:table-header-rows></table:table><table:table><table:table-row>' +
        '<table:table-cell><text:p>b</text:p></table:table-cell></table:table-row>' +
        '</table:table>');
    try
        AssertEquals(1, L.Count);
        AssertEquals('a', L[0]);
    finally
        L.Free;
    end;
end;

{ A zip of the names and contents given in pairs, in memory. }
function ZipStream(const ANamesAndTexts: array of string): TMemoryStream;
var
    Zip: TZipper;
    Streams: TList;
    Each: TStringStream;
    i: longint;
begin
    Result := TMemoryStream.Create;
    Zip := TZipper.Create;
    Streams := TList.Create;
    try
        i := 0;
        while i < High(ANamesAndTexts) do
        begin
            Each := TStringStream.Create(ANamesAndTexts[i + 1]);
            Streams.Add(Each);
            Zip.Entries.AddFileEntry(Each, ANamesAndTexts[i]);
            Inc(i, 2);
        end;
        Zip.SaveToStream(Result);
    finally
        for i := 0 to Streams.Count - 1 do
            TObject(Streams[i]).Free;
        Streams.Free;
        Zip.Free;
    end;
end;

function WorkbookMessage(AData: TStream): string;
var
    L: TStringList;
begin
    Result := '';
    try
        L := WorkbookLines(AData, 'book');
        L.Free;
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TSpreadsheetSheetTest.AnXlsxWorkbookIsItsFirstSheet;
var
    Data: TMemoryStream;
    L: TStringList;
begin
    Data := ZipStream(['xl/workbook.xml', '<?xml version="1.0"?><workbook ' +
        'xmlns:r="r"><sheets><sheet name="A" r:id="rId1"/></sheets></workbook>',
        'xl/_rels/workbook.xml.rels', '<?xml version="1.0"?><Relationships>' +
        '<Relationship Id="rId1" Target="worksheets/data.xml"/></Relationships>',
        'xl/sharedStrings.xml', '<?xml version="1.0"?><sst><si><t>x</t></si></sst>',
        'xl/worksheets/data.xml', SheetHead + '<row r="1"><c r="A1" t="s"><v>0' +
        '</v></c></row><row r="2"><c r="A2"><v>5</v></c></row>' + SheetTail]);
    try
        L := WorkbookLines(Data, 'book.xlsx');
        try
            AssertEquals(2, L.Count);
            AssertEquals('x', L[0]);
            AssertEquals('5', L[1]);
        finally
            L.Free;
        end;
    finally
        Data.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AnOdsWorkbookIsItsFirstTable;
var
    Data: TMemoryStream;
    L: TStringList;
begin
    Data := ZipStream(['content.xml', OdsHead + '<table:table><table:table-row>' +
        '<table:table-cell office:value-type="float" office:value="3"/>' +
        '</table:table-row></table:table>' + OdsTail]);
    try
        L := WorkbookLines(Data, 'book.ods');
        try
            AssertEquals('3', L[0]);
        finally
            L.Free;
        end;
    finally
        Data.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AZipThatIsNoWorkbookIsRefusedInWords;
var
    Data: TMemoryStream;
begin
    Data := ZipStream(['readme.txt', 'hello']);
    try
        AssertTrue(Pos('not a workbook', WorkbookMessage(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpreadsheetSheetTest.BytesThatAreNoZipAreRefusedInWords;
var
    Data: TStringStream;
begin
    Data := TStringStream.Create('<html>a login page</html>');
    try
        AssertTrue(WorkbookMessage(Data),
            Pos('could not be opened as a workbook', WorkbookMessage(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpreadsheetSheetTest.AWorkbookNamingAMissingSheetIsRefused;
var
    Data: TMemoryStream;
begin
    Data := ZipStream(['xl/workbook.xml', '<?xml version="1.0"?><workbook/>']);
    try
        AssertTrue(WorkbookMessage(Data),
            Pos('xl/worksheets/sheet1.xml', WorkbookMessage(Data)) > 0);
    finally
        Data.Free;
    end;
end;

{ ---------------------------------- files ------------------------------------ }

procedure TSpreadsheetFileTest.SetUp;
begin
    FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-spreadsheet-test';
    ForceDirectories(FDir);
end;

procedure TSpreadsheetFileTest.TearDown;
var
    Found: TSearchRec;
begin
    if FindFirst(IncludeTrailingPathDelimiter(FDir) + '*', faAnyFile, Found) = 0 then
    begin
        repeat
            DeleteFile(IncludeTrailingPathDelimiter(FDir) + Found.Name);
        until FindNext(Found) <> 0;
        FindClose(Found);
    end;
    RemoveDir(FDir);
end;

function TSpreadsheetFileTest.Workbook(const AName: string;
    const ANamesAndTexts: array of string): string;
var
    Zip: TZipper;
    Streams: TList;
    Each: TStringStream;
    i: longint;
begin
    Result := IncludeTrailingPathDelimiter(FDir) + AName;
    Zip := TZipper.Create;
    Streams := TList.Create;
    try
        i := 0;
        while i < High(ANamesAndTexts) do
        begin
            Each := TStringStream.Create(ANamesAndTexts[i + 1]);
            Streams.Add(Each);
            Zip.Entries.AddFileEntry(Each, ANamesAndTexts[i]);
            Inc(i, 2);
        end;
        Zip.FileName := Result;
        Zip.ZipAllFiles;
    finally
        for i := 0 to Streams.Count - 1 do
            TObject(Streams[i]).Free;
        Streams.Free;
        Zip.Free;
    end;
end;

procedure TSpreadsheetFileTest.AnXlsxBecomesAProfileWithItsColumnsNamed;
var
    Loader: TSpreadsheetFileLoader;
    P: TTitlePointsSet;
    Values: TDoubleArray;
begin
    Loader := TSpreadsheetFileLoader.Create(nil);
    try
        Loader.LoadDataSet(Workbook('run.xlsx', [
            'xl/workbook.xml', '<?xml version="1.0"?><workbook xmlns="x" ' +
            'xmlns:r="r"><sheets><sheet name="A" r:id="rId1"/></sheets></workbook>',
            'xl/_rels/workbook.xml.rels', '<?xml version="1.0"?><Relationships>' +
            '<Relationship Id="rId1" Target="worksheets/sheet1.xml"/></Relationships>',
            'xl/sharedStrings.xml', '<?xml version="1.0"?><sst><si><t>angle</t>' +
            '</si><si><t>intensity</t></si><si><t>error</t></si></sst>',
            'xl/worksheets/sheet1.xml', SheetHead +
            '<row r="1"><c r="A1" t="s"><v>0</v></c><c r="B1" t="s"><v>1</v></c>' +
            '<c r="C1" t="s"><v>2</v></c></row>' +
            '<row r="2"><c r="A2"><v>10</v></c><c r="B2"><v>100</v></c><c r="C2"><v>1</v></c></row>' +
            '<row r="3"><c r="A3"><v>11</v></c><c r="B3"><v>120</v></c><c r="C3"><v>2</v></c></row>' +
            SheetTail]));
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals(2, P.PointsCount);
            AssertEquals(120, P.PointYCoord[1], 1e-12);
        finally
            P.Free;
        end;
        AssertEquals('intensity', Loader.ValueName);
        AssertTrue(SampleColumnNamed(Loader.SampleColumns, 'error', Values));
    finally
        Loader.Free;
    end;
end;

procedure TSpreadsheetFileTest.AnOdsBecomesAProfile;
var
    Loader: TSpreadsheetFileLoader;
    P: TTitlePointsSet;
begin
    Loader := TSpreadsheetFileLoader.Create(nil);
    try
        Loader.LoadDataSet(Workbook('run.ods', ['mimetype',
            'application/vnd.oasis.opendocument.spreadsheet', 'content.xml',
            OdsHead + '<table:table><table:table-row><table:table-cell ' +
            'office:value-type="float" office:value="1"/><table:table-cell ' +
            'office:value-type="float" office:value="5"/></table:table-row>' +
            '<table:table-row><table:table-cell office:value-type="float" ' +
            'office:value="2"/><table:table-cell office:value-type="float" ' +
            'office:value="6"/></table:table-row></table:table>' + OdsTail]));
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals(2, P.PointsCount);
            AssertEquals(6, P.PointYCoord[1], 1e-12);
        finally
            P.Free;
        end;
    finally
        Loader.Free;
    end;
end;

procedure TSpreadsheetFileTest.AZipThatIsNoWorkbookIsRefusedSayingSo;
var
    Loader: TSpreadsheetFileLoader;
    Message_: string;
begin
    Loader := TSpreadsheetFileLoader.Create(nil);
    try
        Message_ := '';
        try
            Loader.LoadDataSet(Workbook('odd.xlsx', ['readme.txt', 'hello']));
        except
            on E: Exception do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('not a workbook', Message_) > 0);
    finally
        Loader.Free;
    end;
end;

procedure TSpreadsheetFileTest.AFileThatIsNoZipIsRefusedSayingSo;
var
    Loader: TSpreadsheetFileLoader;
    Path, Message_: string;
    Lines: TStringList;
begin
    Path := IncludeTrailingPathDelimiter(FDir) + 'page.xlsx';
    Lines := TStringList.Create;
    try
        Lines.Text := '<html>a login page</html>';
        Lines.SaveToFile(Path);
    finally
        Lines.Free;
    end;
    Loader := TSpreadsheetFileLoader.Create(nil);
    try
        Message_ := '';
        try
            Loader.LoadDataSet(Path);
        except
            on E: Exception do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('could not be opened as a workbook', Message_) > 0);
    finally
        Loader.Free;
    end;
end;

initialization
    RegisterTest('unit', TSpreadsheetSheetTest);
    RegisterTest('integration', TSpreadsheetFileTest);
end.
