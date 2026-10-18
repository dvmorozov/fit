// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A data file's text, whatever the program that wrote it put in front
of it.)

Spreadsheets save CSV with a UTF-8 byte-order mark, and some trading platforms
export UTF-16. Read byte for byte, the mark would become part of the first
column's name - "Date" matching nothing - and UTF-16 one long line of NULs.
TStringList.LoadFromFile, which TDataLoader.LoadDataSetActually reads every
file with, already decodes both by their mark (FPC 3.2.2); these hold that,
so a read replaced by a raw one fails here rather than in a user's file.
A decoder of our own was written first and deleted: nothing needed it.
}
unit testcase_text_file_encoding;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, data_loader;

type
    { Integration: writes a file. }
    TTextFileEncodingFileTest = class(TTestCase)
    published
        procedure ALoaderSeesTheFirstColumnNamedAsWritten;
        procedure AUtf16FileIsReadAsText;
    end;

implementation

type
    { A loader whose parser only keeps the lines it is given. }
    TLineKeepingLoader = class(TDataLoader)
    public
        Lines: TStringList;
        destructor Destroy; override;
    protected
        procedure ParseLines(ALines: TStrings); override;
    end;

destructor TLineKeepingLoader.Destroy;
begin
    Lines.Free;
    inherited Destroy;
end;

procedure TLineKeepingLoader.ParseLines(ALines: TStrings);
begin
    FreeAndNil(Lines);
    Lines := TStringList.Create;
    Lines.Assign(ALines);
end;

function Bytes(const AValues: array of byte): RawByteString;
var
    i: integer;
begin
    Result := '';
    SetLength(Result, Length(AValues));
    for i := 0 to High(AValues) do
        Result[i + 1] := Chr(AValues[i]);
end;

{ THROUGH THE READ EVERY LOADER INHERITS: a file saved with a mark, opened the
  way File > Open opens it. }
procedure TTextFileEncodingFileTest.ALoaderSeesTheFirstColumnNamedAsWritten;
var
    Path: string;
    F: TFileStream;
    Text: RawByteString;
    L: TLineKeepingLoader;
begin
    Path := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-bom-' +
        IntToStr(GetProcessID) + '.csv';
    Text := Bytes([$EF, $BB, $BF]) + 'Date,Close' + LineEnding + '2024-01-02,1';
    F := TFileStream.Create(Path, fmCreate);
    try
        F.WriteBuffer(Text[1], Length(Text));
    finally
        F.Free;
    end;
    L := TLineKeepingLoader.Create(nil);
    try
        L.LoadDataSet(Path);
        AssertEquals(2, L.Lines.Count);
        AssertEquals('Date,Close', L.Lines[0]);
    finally
        L.Free;
        DeleteFile(Path);
    end;
end;

procedure TTextFileEncodingFileTest.AUtf16FileIsReadAsText;
var
    Path: string;
    F: TFileStream;
    Text: RawByteString;
    L: TLineKeepingLoader;
begin
    Path := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-utf16-' +
        IntToStr(GetProcessID) + '.csv';
    //  "<DATE>" CR LF "1" in UTF-16LE, after its mark.
    Text := Bytes([$FF, $FE, Ord('<'), 0, Ord('D'), 0, Ord('A'), 0, Ord('T'), 0,
        Ord('E'), 0, Ord('>'), 0, 13, 0, 10, 0, Ord('1'), 0]);
    F := TFileStream.Create(Path, fmCreate);
    try
        F.WriteBuffer(Text[1], Length(Text));
    finally
        F.Free;
    end;
    L := TLineKeepingLoader.Create(nil);
    try
        L.LoadDataSet(Path);
        AssertEquals(2, L.Lines.Count);
        AssertEquals('<DATE>', L.Lines[0]);
        AssertEquals('1', L.Lines[1]);
    finally
        L.Free;
        DeleteFile(Path);
    end;
end;

initialization
    RegisterTest('integration', TTextFileEncodingFileTest);
end.
