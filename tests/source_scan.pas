// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Reading this repository's own Pascal sources, for the tests that assert
something about the code as it is written rather than about what it does.)

One implementation of the three things every such test needs - where the
repository is, which files are its sources, and what a source says once its
comments and string literals are out of the way - so two scans cannot disagree
about any of them.
}
unit source_scan;

{$MODE Delphi}

interface

uses
    Classes, SysUtils;

{ The repository root, with a trailing separator, found by walking up from the
  test binary; '' when it is not found. A scan of nothing reports success, so a
  caller must fail on ''. }
function RepoRoot: string;

{ ASource with comments and string literals blanked to spaces. Blanked, not
  removed, and line breaks kept, so nothing is joined that was apart and a
  position still has the line number it had. }
function StripCommentsAndStrings(const ASource: string): string;

{ Appends the full path of every .pas, .lpr and .inc file under ADir to AFiles.
  Build output, the Lazarus backup directory and .git are skipped: they carry
  generated copies of the same sources, and a scan would report each twice. }
procedure CollectPascalSources(const ADir: string; AFiles: TStrings);

implementation

function RepoRoot: string;
var
    Dir: string;
    i: integer;
begin
    //  The binary lives in <root>/tests, but it is not assumed: walk up until a
    //  directory carries both markers.
    Dir := ExtractFilePath(ExpandFileName(ParamStr(0)));
    for i := 0 to 5 do
    begin
        if FileExists(IncludeTrailingPathDelimiter(Dir) + 'AGENTS.md') and
            DirectoryExists(IncludeTrailingPathDelimiter(Dir) + 'Packages') then
        begin
            Result := IncludeTrailingPathDelimiter(Dir);
            Exit;
        end;
        Dir := ExpandFileName(IncludeTrailingPathDelimiter(Dir) + '..');
    end;
    Result := '';
end;

function StripCommentsAndStrings(const ASource: string): string;
var
    i, Len: integer;

    procedure Blank;
    begin
        if not (Result[i] in [#10, #13]) then
            Result[i] := ' ';
        Inc(i);
    end;

begin
    Result := ASource;
    i := 1;
    Len := Length(ASource);
    while i <= Len do
    begin
        if (ASource[i] = '/') and (i < Len) and (ASource[i + 1] = '/') then
        begin
            while (i <= Len) and not (ASource[i] in [#10, #13]) do
                Blank;
        end
        else if ASource[i] = '{' then
        begin
            while (i <= Len) and (ASource[i] <> '}') do
                Blank;
            if i <= Len then
                Blank;
        end
        else if (ASource[i] = '(') and (i < Len) and (ASource[i + 1] = '*') then
        begin
            while (i <= Len) and
                not ((ASource[i] = '*') and (i < Len) and (ASource[i + 1] = ')')) do
                Blank;
            if i <= Len then
                Blank;
            if i <= Len then
                Blank;
        end
        else if ASource[i] = '''' then
        begin
            Blank;
            while (i <= Len) and (ASource[i] <> '''') do
                Blank;
            if i <= Len then
                Blank;
        end
        else
            Inc(i);
    end;
end;

procedure CollectPascalSources(const ADir: string; AFiles: TStrings);
var
    Rec: TSearchRec;
    Name, Ext: string;
begin
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, Rec) = 0 then
    try
        repeat
            Name := Rec.Name;
            if (Name = '.') or (Name = '..') then Continue;
            if (Rec.Attr and faDirectory) <> 0 then
            begin
                if (LowerCase(Name) = 'o') or (LowerCase(Name) = 'lib') or
                    (LowerCase(Name) = 'backup') or (Name = '.git') then Continue;
                CollectPascalSources(IncludeTrailingPathDelimiter(ADir) + Name, AFiles);
            end
            else
            begin
                Ext := LowerCase(ExtractFileExt(Name));
                if (Ext = '.pas') or (Ext = '.lpr') or (Ext = '.inc') then
                    AFiles.Add(IncludeTrailingPathDelimiter(ADir) + Name);
            end;
        until FindNext(Rec) <> 0;
    finally
        FindClose(Rec);
    end;
end;

end.
