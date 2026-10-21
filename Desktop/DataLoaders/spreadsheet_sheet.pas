// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The first sheet of an .xlsx or .ods workbook, as lines of
tab-separated cells.)

WHY SPREADSHEETS. They are the third commonest kind of file in Zenodo's
datasets - some 95,000 records hold an .xlsx (counted 2026-10-20) - and a
measurement exported from an instrument's software or typed up for a paper is
as often a workbook as a CSV.

WHY NOT A SPREADSHEET LIBRARY. FPSpreadsheet reads these and many more, and was
the first plan (2026-10-20). It was set aside when the work was done: it is a
package every machine that builds Fit would have to fetch - three platforms, the
coverage container, the published snapshot's own build - for what is, for a
profile, one thing: the cells of the first sheet. Both formats are a zip of XML
that FPC reads with its own zipper and DOM, so this is that, and nothing to
install. What it gives up is the legacy binary .xls, which only such a library
reads; that is recorded as a limitation where the user meets it.

THE SHEET BECOMES TEXT, and text_table reads it - the same answer to "which
columns are the profile" as a CSV, rather than a second one. A cell's own tabs
and line breaks become spaces, so a cell is one cell.

NOTHING HERE OPENS A FILE: the XML parts arrive as streams, so every rule is
reachable from a test that writes the XML itself.
}
unit spreadsheet_sheet;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ The cells of an Office Open XML worksheet (xl/worksheets/sheetN.xml) as
  tab-separated lines, a string cell's text looked up in the shared strings
  (xl/sharedStrings.xml, or nil when the workbook has none). A gap between
  cells - a row or a column the sheet leaves out - is kept as an empty cell or
  line, so columns stay where they are. }
function XlsxSheetLines(ASheet, ASharedStrings: TStream): TStringList;

{ Which part of an .xlsx holds its first sheet: read from xl/workbook.xml and
  its relationships, 'xl/worksheets/sheet1.xml' when either says nothing. }
function XlsxFirstSheetPath(AWorkbook, ARelationships: TStream): string;

{ The first table of an OpenDocument spreadsheet's content.xml as tab-separated
  lines, repeated rows and cells written out as often as they repeat - up to a
  bound, since a sheet ends in a million empty repeated rows. }
function OdsSheetLines(AContent: TStream): TStringList;

{ The column a reference names, counted from 0: 'A1' is 0, 'AB7' is 27. }
function ColumnOfReference(const AReference: string): longint;

implementation

uses
    DOM, XMLRead, StrUtils;

const
    { The most repeated rows or cells written out: past the data a sheet often
      repeats an empty row a million times to its edge. }
    MaxRepeat = 10000;

{ ACell's text as one cell: its own tabs and breaks are not separators. }
function CellText(const AText: string): string;
begin
    Result := StringReplace(AText, #9, ' ', [rfReplaceAll]);
    Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
    Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
end;

{ The element's name without its namespace prefix. }
function LocalName(ANode: TDOMNode): string;
var
    P: longint;
begin
    Result := ANode.NodeName;
    P := Pos(':', Result);
    if P > 0 then
        Result := Copy(Result, P + 1, MaxInt);
end;

{ The value of the attribute whose name, without its prefix, is AName. }
function AttributeOf(ANode: TDOMNode; const AName: string): string;
var
    i: longint;
    Attr: TDOMNode;
begin
    Result := '';
    if (ANode = nil) or (ANode.Attributes = nil) then
        Exit;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
        Attr := ANode.Attributes[i];
        if LocalName(Attr) = AName then
            Exit(Attr.NodeValue);
    end;
end;

{ All the text inside ANode, its descendants' included. }
function AllText(ANode: TDOMNode): string;
begin
    Result := '';
    if ANode <> nil then
        Result := ANode.TextContent;
end;

function Parsed(AStream: TStream): TXMLDocument;
begin
    AStream.Position := 0;
    ReadXMLFile(Result, AStream);
end;

{ The first descendant of ANode called AName (without its prefix). }
function FirstNamed(ANode: TDOMNode; const AName: string): TDOMNode;
var
    Child: TDOMNode;
begin
    Result := nil;
    if ANode = nil then
        Exit;
    Child := ANode.FirstChild;
    while Child <> nil do
    begin
        if (Child.NodeType = ELEMENT_NODE) and (LocalName(Child) = AName) then
            Exit(Child);
        Result := FirstNamed(Child, AName);
        if Result <> nil then
            Exit;
        Child := Child.NextSibling;
    end;
end;

function ColumnOfReference(const AReference: string): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 1 to Length(AReference) do
    begin
        if not (UpCase(AReference[i]) in ['A'..'Z']) then
            Break;
        Result := Result * 26 + (Ord(UpCase(AReference[i])) - Ord('A') + 1);
    end;
    Dec(Result);
end;

{ ---------------------------------- xlsx ------------------------------------- }

function SharedStrings(AStream: TStream): TStringList;
var
    Doc: TXMLDocument;
    Item: TDOMNode;
begin
    Result := TStringList.Create;
    if AStream = nil then
        Exit;
    Doc := Parsed(AStream);
    try
        Item := FirstNamed(Doc.DocumentElement, 'si');
        while Item <> nil do
        begin
            //  A rich string is several runs; its text is all of them.
            if (Item.NodeType = ELEMENT_NODE) and (LocalName(Item) = 'si') then
                Result.Add(CellText(AllText(Item)));
            Item := Item.NextSibling;
        end;
    finally
        Doc.Free;
    end;
end;

{ The rows of ASheet into ALines, a string cell's text looked up in
  AStrings. }
procedure ReadXlsxRows(ASheet: TStream; AStrings, ALines: TStringList);
var
    Doc: TXMLDocument;
    Data, Row, Cell: TDOMNode;
    Cells: TStringArray;
    Kind, Value, Ref: string;
    Column, RowNumber, Index, i: longint;
begin
    Doc := Parsed(ASheet);
    try
        Data := FirstNamed(Doc.DocumentElement, 'sheetData');
        if Data = nil then
            Exit;
        Row := Data.FirstChild;
        while Row <> nil do
        begin
            if (Row.NodeType = ELEMENT_NODE) and (LocalName(Row) = 'row') then
            begin
                //  A ROW THE SHEET LEAVES OUT is an empty line, so the
                //  rows after it keep their place.
                RowNumber := StrToIntDef(AttributeOf(Row, 'r'), 0);
                while (RowNumber > 0) and (ALines.Count < RowNumber - 1) do
                    ALines.Add('');
                Cells := nil;
                Cell := Row.FirstChild;
                while Cell <> nil do
                begin
                    if (Cell.NodeType = ELEMENT_NODE) and (LocalName(Cell) = 'c') then
                    begin
                        Ref := AttributeOf(Cell, 'r');
                        Column := Length(Cells);
                        if Ref <> '' then
                            Column := ColumnOfReference(Ref);
                        Kind := AttributeOf(Cell, 't');
                        if Kind = 'inlineStr' then
                            Value := AllText(FirstNamed(Cell, 'is'))
                        else
                            Value := AllText(FirstNamed(Cell, 'v'));
                        if Kind = 's' then
                        begin
                            Index := StrToIntDef(Value, -1);
                            if (Index >= 0) and (Index < AStrings.Count) then
                                Value := AStrings[Index]
                            else
                                Value := '';
                        end;
                        if Column >= 0 then
                        begin
                            while Length(Cells) <= Column do
                            begin
                                SetLength(Cells, Length(Cells) + 1);
                                Cells[High(Cells)] := '';
                            end;
                            Cells[Column] := CellText(Value);
                        end;
                    end;
                    Cell := Cell.NextSibling;
                end;
                Value := '';
                for i := 0 to High(Cells) do
                begin
                    if i > 0 then
                        Value := Value + #9;
                    Value := Value + Cells[i];
                end;
                ALines.Add(Value);
            end;
            Row := Row.NextSibling;
        end;
    finally
        Doc.Free;
    end;
end;

function XlsxSheetLines(ASheet, ASharedStrings: TStream): TStringList;
var
    Strings: TStringList;
begin
    Result := TStringList.Create;
    try
        Strings := SharedStrings(ASharedStrings);
        try
            ReadXlsxRows(ASheet, Strings, Result);
        finally
            Strings.Free;
        end;
    except
        Result.Free;
        raise;
    end;
end;

function XlsxFirstSheetPath(AWorkbook, ARelationships: TStream): string;
var
    Doc: TXMLDocument;
    Sheet, Rel: TDOMNode;
    Id, Target: string;
begin
    Result := 'xl/worksheets/sheet1.xml';
    if (AWorkbook = nil) or (ARelationships = nil) then
        Exit;
    Id := '';
    Doc := Parsed(AWorkbook);
    try
        Sheet := FirstNamed(Doc.DocumentElement, 'sheet');
        if Sheet <> nil then
            Id := AttributeOf(Sheet, 'id');
    finally
        Doc.Free;
    end;
    if Id = '' then
        Exit;
    Doc := Parsed(ARelationships);
    try
        Rel := FirstNamed(Doc.DocumentElement, 'Relationship');
        while Rel <> nil do
        begin
            if (Rel.NodeType = ELEMENT_NODE) and (AttributeOf(Rel, 'Id') = Id) then
            begin
                Target := AttributeOf(Rel, 'Target');
                //  Relative to xl/, or absolute from the package root.
                if StartsStr('/', Target) then
                    Result := Copy(Target, 2, MaxInt)
                else
                    Result := 'xl/' + Target;
                Exit;
            end;
            Rel := Rel.NextSibling;
        end;
    finally
        Doc.Free;
    end;
end;

{ ----------------------------------- ods ------------------------------------- }

{ The cells of one table-row as a tab-separated line. }
function OdsRowLine(ARow: TDOMNode): string;
var
    Cell: TDOMNode;
    Value: string;
    CellRepeat, c: longint;
    First: boolean;
begin
    Result := '';
    First := True;
    Cell := ARow.FirstChild;
    while Cell <> nil do
    begin
        if (Cell.NodeType = ELEMENT_NODE) and
            ((LocalName(Cell) = 'table-cell') or
            (LocalName(Cell) = 'covered-table-cell')) then
        begin
            //  A NUMBER IS ITS VALUE, not its shown text: "1.50 kV" is shown,
            //  1.5 is stored.
            if (AttributeOf(Cell, 'value-type') = 'float') or
                (AttributeOf(Cell, 'value-type') = 'percentage') then
                Value := AttributeOf(Cell, 'value')
            else
                Value := CellText(AllText(Cell));
            CellRepeat := StrToIntDef(
                AttributeOf(Cell, 'number-columns-repeated'), 1);
            if (CellRepeat < 1) or (CellRepeat > MaxRepeat) then
                CellRepeat := 1;
            for c := 1 to CellRepeat do
            begin
                if not First then
                    Result := Result + #9;
                Result := Result + Value;
                First := False;
            end;
        end;
        Cell := Cell.NextSibling;
    end;
    //  Trailing empty cells say nothing.
    Result := TrimRightSet(Result, [#9]);
end;

{ Every row under ANode, in order: rows sit in the table itself, and in its
  header-rows and row groups. }
procedure WalkOdsRows(ANode: TDOMNode; ALines: TStringList);
var
    Child: TDOMNode;
    Line, Name_: string;
    RowRepeat, r: longint;
begin
    Child := ANode.FirstChild;
    while Child <> nil do
    begin
        if Child.NodeType = ELEMENT_NODE then
        begin
            Name_ := LocalName(Child);
            if Name_ = 'table-row' then
            begin
                Line := OdsRowLine(Child);
                RowRepeat := StrToIntDef(
                    AttributeOf(Child, 'number-rows-repeated'), 1);
                //  A SHEET ENDS IN A MILLION EMPTY ROWS repeated to its edge;
                //  an empty row repeated is one empty row.
                if (RowRepeat < 1) or (RowRepeat > MaxRepeat) or (Line = '') then
                    RowRepeat := 1;
                for r := 1 to RowRepeat do
                    ALines.Add(Line);
            end
            else if (Name_ = 'table-header-rows') or (Name_ = 'table-row-group') or
                (Name_ = 'table-rows') then
                WalkOdsRows(Child, ALines);
        end;
        Child := Child.NextSibling;
    end;
end;

function OdsSheetLines(AContent: TStream): TStringList;
var
    Doc: TXMLDocument;
    Table: TDOMNode;
begin
    Result := TStringList.Create;
    try
        Doc := Parsed(AContent);
        try
            //  The first table only: the next one is the next sheet.
            Table := FirstNamed(Doc.DocumentElement, 'table');
            if Table <> nil then
                WalkOdsRows(Table, Result);
        finally
            Doc.Free;
        end;
    except
        Result.Free;
        raise;
    end;
end;

end.
