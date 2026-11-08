// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A source shaped like a repository - a question, records, files -
that names no real service.)

WHY THE FRAMEWORK'S TESTS NEED ONE. The wizard's container steps - a record
found alone opened by Next, a record among several chosen and opened, Back out
of one - were driven through the DOI source while it was the framework's. It is
the published-data module's since 2026-10-20, and a framework test naming a
module is what the leak gate fails: in a build without that module the test
cannot even compile. So the framework's tests drive the same steps through
this, and the module tests its own source against the real services' shapes.

IT ASKS ITS WEB CLIENT, and that is deliberate: failures, re-asking a source
after Back, and the address a file is fetched from are all observed through
the mock web client's log and canned failures, exactly as they were.

THE WIRE FORMAT IS ITS OWN AND TRIVIAL - a line per record "id|title", a line
per file "name|size|address" - because what is under test is the wizard, not a
parser.

Never registered, for the reason the probes in testcase_data_source_wizard are
not: the registry walk is another test's subject. Tests reach it through
ChooseSourceClass.
}
unit mock_record_source;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, data_source;

const
    RecordSourceId = 'record-probe';
    RecordFieldId = 'q';
    RecordFieldCaption = 'Record or words';
    { Where the probe asks. A test answers these through the mock web client. }
    RecordSearchUrl = 'https://records.example.org/search?q=';
    RecordFilesUrl = 'https://records.example.org/records/';

type
    TRecordSource = class(TDataSource)
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Children(const AItem: TDataSourceItem): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

{ What the probe's search answers, one "id|title" per record. }
function RecordLines(const AIdsAndTitles: array of string): string;

{ What a record holds, one "name|size|address" per file. }
function FileLines(const ANamesSizesAndUrls: array of string): string;

implementation

function RecordLines(const AIdsAndTitles: array of string): string;
var
    i: longint;
begin
    Result := '';
    i := 0;
    while i < High(AIdsAndTitles) do
    begin
        Result := Result + AIdsAndTitles[i] + '|' + AIdsAndTitles[i + 1] + LineEnding;
        Inc(i, 2);
    end;
end;

function FileLines(const ANamesSizesAndUrls: array of string): string;
var
    i: longint;
begin
    Result := '';
    i := 0;
    while i + 2 <= High(ANamesSizesAndUrls) do
    begin
        Result := Result + ANamesSizesAndUrls[i] + '|' + ANamesSizesAndUrls[i + 1] +
            '|' + ANamesSizesAndUrls[i + 2] + LineEnding;
        Inc(i, 3);
    end;
end;

class function TRecordSource.Info: TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := RecordSourceId;
    Result.Title := 'Record probe';
    Result.Category := 'General';
    Result.Summary := 'Records holding files, for the wizard''s tests.';
    Result.ProducesExtensions := '.XY';
    Result.NeedsNetwork := True;
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := TextField(RecordFieldId, RecordFieldCaption,
        'a record, or words to search for');
    Result.HasContainers := True;
end;

function Fields(const ALine: string): TStringArray;
begin
    Result := ALine.Split(['|']);
end;

function TRecordSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Lines: TStringList;
    Parts: TStringArray;
    i: longint;
begin
    Result := nil;
    Lines := TStringList.Create;
    try
        Lines.Text := FWeb.GetText(RecordSearchUrl +
            UrlEncoded(QueryValue(AQuery, RecordFieldId)));
        for i := 0 to Lines.Count - 1 do
        begin
            Parts := Fields(Lines[i]);
            if Length(Parts) < 2 then
                Continue;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)].Id := Parts[0];
            Result[High(Result)].Title := Parts[1];
            Result[High(Result)].Ref := Parts[0];
            Result[High(Result)].IsLeaf := False;
        end;
    finally
        Lines.Free;
    end;
    if Length(Result) = 0 then
        raise EDataSourceError.Create('Nothing was found for "' +
            QueryValue(AQuery, RecordFieldId) + '".');
end;

function TRecordSource.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Lines: TStringList;
    Parts: TStringArray;
    i: longint;
begin
    Result := nil;
    Lines := TStringList.Create;
    try
        Lines.Text := FWeb.GetText(RecordFilesUrl + AItem.Ref);
        for i := 0 to Lines.Count - 1 do
        begin
            Parts := Fields(Lines[i]);
            if Length(Parts) < 3 then
                Continue;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)].Id := AItem.Ref + '/' + Parts[0];
            Result[High(Result)].Title := Parts[0];
            Result[High(Result)].FileName := Parts[0];
            Result[High(Result)].Size := StrToInt64Def(Parts[1], 0);
            Result[High(Result)].Ref := Parts[2];
            Result[High(Result)].IsLeaf := True;
        end;
    finally
        Lines.Free;
    end;
    if Length(Result) = 0 then
        raise EDataSourceError.Create('That record holds no files.');
end;

function TRecordSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    FWeb.Download(AItem.Ref, ADest);
    Result := AItem.FileName;
end;

end.
