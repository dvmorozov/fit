// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Tables of numbers in text, and the reader of .csv and .tsv files
over them.)

Every case is a shape a public repository actually publishes: a header naming
the columns, a comma or a tab or a semicolon, a decimal comma, quoted cells, a
units line, a column of labels or dates beside the numbers. Nothing reads a
file: the reader is driven through LoadFromLines, the same parse a file takes.
}
unit testcase_text_table;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    text_table, csv_file_loader, data_loader, sample_columns, title_points_set;

type
    TTextTableTest = class(TTestCase)
    private
        function Lines(const AText: array of string): TStringList;
        function TableOf(const AText: array of string): TTextTable;
    published
        procedure CellsAreSplitAtTheDelimiter;
        procedure AQuotedCellMayHoldTheDelimiter;
        procedure SpacesSplitWhenNoDelimiterIsUsed;
        procedure TheDelimiterIsTheOneThatSplitsTheTableAlike;
        procedure ATabBeatsACommaInsideNumbers;
        procedure ASemicolonTableWithDecimalCommasIsRead;
        procedure ACommaIsNotADecimalMarkWhereAFullStopIsUsed;
        procedure TheHeaderNamesTheColumns;
        procedure AUnitsLineUnderTheNamesIsNotTheNames;
        procedure WithNoHeaderTheColumnsAreNumbered;
        procedure AColumnOfLabelsIsNotNumeric;
        procedure ADateIsNotANumber;
        procedure AStrayGapCostsOnlyItsCell;
        procedure CommentsAndBlankLinesAreSkipped;
    end;

    TCsvLoaderTest = class(TTestCase)
    private
        FLoader: TCSVFileLoader;
        procedure Load(const AText: array of string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheFirstTwoNumericColumnsAreTheProfile;
        procedure TheValueIsNamedByItsHeader;
        procedure FurtherColumnsAreKeptBesideIt;
        procedure ARowWithoutBothNumbersIsSkipped;
        procedure ARepeatedArgumentKeepsTheFirst;
        procedure FewerThanTwoNumericColumnsIsRefusedNamingWhatWasFound;
    end;

implementation

{ ---------------------------------- table ----------------------------------- }

function TTextTableTest.Lines(const AText: array of string): TStringList;
var
    i: longint;
begin
    Result := TStringList.Create;
    for i := 0 to High(AText) do
        Result.Add(AText[i]);
end;

function TTextTableTest.TableOf(const AText: array of string): TTextTable;
var
    L: TStringList;
begin
    L := Lines(AText);
    try
        Result := ParseTextTable(L);
    finally
        L.Free;
    end;
end;

procedure TTextTableTest.CellsAreSplitAtTheDelimiter;
var
    Cells: TStringArray;
begin
    Cells := SplitTableLine(' 1.5 , 2 ,x', ',');
    AssertEquals(3, Length(Cells));
    AssertEquals('1.5', Cells[0]);
    AssertEquals('2', Cells[1]);
    AssertEquals('x', Cells[2]);
end;

procedure TTextTableTest.AQuotedCellMayHoldTheDelimiter;
var
    Cells: TStringArray;
begin
    Cells := SplitTableLine('"Smith, J.",3,"say ""hi"""', ',');
    AssertEquals(3, Length(Cells));
    AssertEquals('Smith, J.', Cells[0]);
    AssertEquals('say "hi"', Cells[2]);
end;

procedure TTextTableTest.SpacesSplitWhenNoDelimiterIsUsed;
var
    Cells: TStringArray;
begin
    Cells := SplitTableLine('  10.0   200    3', #0);
    AssertEquals(3, Length(Cells));
    AssertEquals('200', Cells[1]);
end;

procedure TTextTableTest.TheDelimiterIsTheOneThatSplitsTheTableAlike;
var
    L: TStringList;
begin
    L := Lines(['angle,intensity', '10,100', '11,120', '12,90']);
    try
        AssertEquals(',', DetectTableDelimiter(L));
    finally
        L.Free;
    end;
    L := Lines(['10 100', '11 120']);
    try
        AssertEquals('no delimiter splits it: spaces', #0, DetectTableDelimiter(L));
    finally
        L.Free;
    end;
end;

procedure TTextTableTest.ATabBeatsACommaInsideNumbers;
var
    L: TStringList;
begin
    L := Lines(['x'#9'y', '1,5'#9'2,5', '2,5'#9'3,5']);
    try
        AssertEquals(#9, DetectTableDelimiter(L));
    finally
        L.Free;
    end;
end;

procedure TTextTableTest.ASemicolonTableWithDecimalCommasIsRead;
var
    T: TTextTable;
begin
    T := TableOf(['Winkel;Intensität', '1,5;2,5', '2,5;3,5']);
    AssertEquals(2, Length(T.Columns));
    AssertEquals(1.5, T.Columns[0][0], 1e-12);
    AssertEquals(3.5, T.Columns[1][1], 1e-12);
end;

procedure TTextTableTest.ACommaIsNotADecimalMarkWhereAFullStopIsUsed;
var
    T: TTextTable;
begin
    T := TableOf(['a;b;c', '1.5;2;3', '2.5;3;4']);
    AssertEquals(3, Length(T.Columns));
    AssertEquals(2.5, T.Columns[0][1], 1e-12);
end;

procedure TTextTableTest.TheHeaderNamesTheColumns;
var
    T: TTextTable;
begin
    T := TableOf(['wavelength,absorbance', '400,0.1', '401,0.2']);
    AssertEquals('wavelength', T.Names[0]);
    AssertEquals('absorbance', T.Names[1]);
    AssertEquals(2, T.RowCount);
end;

procedure TTextTableTest.AUnitsLineUnderTheNamesIsNotTheNames;
var
    T: TTextTable;
begin
    T := TableOf(['2theta,counts', 'deg,1/s', '10,5', '11,6']);
    AssertEquals('2theta', T.Names[0]);
    AssertEquals('counts', T.Names[1]);
end;

procedure TTextTableTest.WithNoHeaderTheColumnsAreNumbered;
var
    T: TTextTable;
begin
    T := TableOf(['10,5', '11,6']);
    AssertEquals('Column 1', T.Names[0]);
    AssertEquals('Column 2', T.Names[1]);
end;

procedure TTextTableTest.AColumnOfLabelsIsNotNumeric;
var
    T: TTextTable;
begin
    T := TableOf(['sample,x,y', 'A,1,2', 'B,2,3', 'C,3,4']);
    AssertEquals('the labels are left out', 2, Length(T.Columns));
    AssertEquals('x', T.Names[0]);
    AssertEquals(3, T.CellCount);
end;

procedure TTextTableTest.ADateIsNotANumber;
var
    T: TTextTable;
    V: double;
begin
    //  "2024-03-01" is all digits and dashes, and it is not a number: read as
    //  one, a date column would become the argument.
    AssertFalse(TryTableNumber('2024-03-01', False, V));
    T := TableOf(['date,open,close', '2024-03-01,1,2', '2024-03-02,2,3']);
    AssertEquals(2, Length(T.Columns));
    AssertEquals('open', T.Names[0]);
end;

procedure TTextTableTest.AStrayGapCostsOnlyItsCell;
var
    T: TTextTable;
begin
    T := TableOf(['x,y,z', '1,2,3', '2,n/a,4', '3,4,5']);
    AssertEquals(3, Length(T.Columns));
    AssertTrue('the gap is a gap', IsNan(T.Columns[1][1]));
    AssertEquals(4, T.Columns[2][1], 1e-12);
end;

procedure TTextTableTest.CommentsAndBlankLinesAreSkipped;
var
    T: TTextTable;
begin
    T := TableOf(['# recorded 2024', '', 'x,y', '1,2', '% note', '2,3']);
    AssertEquals(2, T.RowCount);
    AssertEquals('x', T.Names[0]);
end;

{ --------------------------------- loader ----------------------------------- }

procedure TCsvLoaderTest.SetUp;
begin
    FLoader := TCSVFileLoader.Create(nil);
end;

procedure TCsvLoaderTest.TearDown;
begin
    FreeAndNil(FLoader);
end;

procedure TCsvLoaderTest.Load(const AText: array of string);
var
    L: TStringList;
    i: longint;
begin
    L := TStringList.Create;
    try
        for i := 0 to High(AText) do
            L.Add(AText[i]);
        FLoader.LoadFromLines(L);
    finally
        L.Free;
    end;
end;

procedure TCsvLoaderTest.TheFirstTwoNumericColumnsAreTheProfile;
var
    P: TTitlePointsSet;
begin
    Load(['id,angle,intensity', 'a,10,100', 'b,11,120']);
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals(2, P.PointsCount);
        AssertEquals('the argument', 10, P.PointXCoord[0], 1e-12);
        AssertEquals('the value', 120, P.PointYCoord[1], 1e-12);
    finally
        P.Free;
    end;
end;

procedure TCsvLoaderTest.TheValueIsNamedByItsHeader;
begin
    Load(['angle,intensity,error', '10,100,1', '11,120,2']);
    AssertEquals('intensity', FLoader.ValueName);
end;

procedure TCsvLoaderTest.FurtherColumnsAreKeptBesideIt;
var
    Columns: TSampleColumns;
    Values: TDoubleArray;
begin
    Load(['angle,intensity,error', '10,100,1', '11,120,2']);
    Columns := FLoader.SampleColumns;
    AssertEquals(1, Length(Columns));
    AssertTrue(SampleColumnNamed(Columns, 'error', Values));
    AssertEquals(2, Values[1], 1e-12);
end;

procedure TCsvLoaderTest.ARowWithoutBothNumbersIsSkipped;
var
    P: TTitlePointsSet;
    Values: TDoubleArray;
begin
    Load(['x,y,z', '1,2,3', '2,,4', '3,4,5']);
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals(2, P.PointsCount);
        AssertEquals(3, P.PointXCoord[1], 1e-12);
    finally
        P.Free;
    end;
    //  The columns beside it stay aligned with the points that were kept.
    AssertTrue(SampleColumnNamed(FLoader.SampleColumns, 'z', Values));
    AssertEquals(2, Length(Values));
    AssertEquals(5, Values[1], 1e-12);
end;

procedure TCsvLoaderTest.ARepeatedArgumentKeepsTheFirst;
var
    P: TTitlePointsSet;
begin
    Load(['x,y', '1,2', '1,9', '2,3']);
    P := FLoader.GetPointsSetCopy;
    try
        AssertEquals(2, P.PointsCount);
        AssertEquals(2, P.PointYCoord[0], 1e-12);
    finally
        P.Free;
    end;
end;

procedure TCsvLoaderTest.FewerThanTwoNumericColumnsIsRefusedNamingWhatWasFound;
var
    Message_: string;
begin
    Message_ := '';
    try
        Load(['name,value', 'a,1', 'b,2']);
    except
        on E: Exception do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('two columns of numbers', Message_) > 0);
    AssertTrue('and what it found: ' + Message_,
        Pos('no line', Message_) > 0);
end;

initialization
    RegisterTest('unit', TTextTableTest);
    RegisterTest('unit', TCsvLoaderTest);
end.
