// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(That no Pascal source in the build declares a label or jumps to one.)

THE DEFECT THIS DEFENDS AGAINST. Free Pascal emits a user-declared `label` as a
GLOBAL symbol, and clang refuses a global symbol between `.cfi_startproc` and
`.cfi_endproc` - which on x86_64-darwin it always sits between, because those
blocks are unwind information rather than debug information and no compiler flag
removes them. One `goto` therefore makes its unit unassemblable on Intel macOS,
and the compiler reports it at a line one PAST THE END of the file, naming
nothing that exists. Two of them - in TAGraph.pas and dat_file_loader.pas - cost
three release cycles and the Intel macOS download before anyone looked at the
source rather than at the toolchain.

WHY A SOURCE SCAN rather than a test of the code that replaced them. The failure
is not that the drawing is wrong; it is that a construct is PRESENT. Nothing
about the value of a variable can detect it, and the platform that rejects it is
not the platform CI compiles on most of the time - so the thing to assert is the
absence itself, on every platform, every run.

Comments and string literals are stripped before the scan, so the prose in this
file and the explanatory comments left where the labels used to be do not trip
it. See docs/contributing/building.md.
}
unit testcase_no_pascal_labels;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry;

type
    TNoPascalLabelsTest = class(TTestCase)
    private
        //  The repository root, found by walking up from this binary.
        function RepoRoot: string;
        //  Pascal source with comments and string literals blanked out.
        function StripCommentsAndStrings(const ASource: string): string;
        procedure ScanTree(const ADir: string; AOffenders: TStrings);
        procedure ScanFile(const APath: string; AOffenders: TStrings);
    published
        procedure NoSourceFileDeclaresALabelOrUsesGoto;
    end;

implementation

function TNoPascalLabelsTest.RepoRoot: string;
var
    Dir: string;
    i: integer;
begin
    //  The binary lives in <root>/tests, but it is not assumed: walk up until a
    //  directory carries both markers. Failing to find it must FAIL rather than
    //  pass quietly - a scan that examines nothing reports success.
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

function TNoPascalLabelsTest.StripCommentsAndStrings(const ASource: string): string;
var
    i, Len: integer;
begin
    //  Blanked, not removed, so nothing is joined that was apart.
    Result := ASource;
    i := 1;
    Len := Length(ASource);
    while i <= Len do
    begin
        if (ASource[i] = '/') and (i < Len) and (ASource[i + 1] = '/') then
        begin
            while (i <= Len) and not (ASource[i] in [#10, #13]) do
            begin
                Result[i] := ' ';
                Inc(i);
            end;
        end
        else if ASource[i] = '{' then
        begin
            while (i <= Len) and (ASource[i] <> '}') do
            begin
                Result[i] := ' ';
                Inc(i);
            end;
            if i <= Len then
            begin
                Result[i] := ' ';
                Inc(i);
            end;
        end
        else if (ASource[i] = '(') and (i < Len) and (ASource[i + 1] = '*') then
        begin
            while (i <= Len) and
                not ((ASource[i] = '*') and (i < Len) and (ASource[i + 1] = ')')) do
            begin
                Result[i] := ' ';
                Inc(i);
            end;
            if i <= Len then
            begin
                Result[i] := ' ';
                Inc(i);
            end;
            if i <= Len then
            begin
                Result[i] := ' ';
                Inc(i);
            end;
        end
        else if ASource[i] = '''' then
        begin
            Result[i] := ' ';
            Inc(i);
            while (i <= Len) and (ASource[i] <> '''') do
            begin
                Result[i] := ' ';
                Inc(i);
            end;
            if i <= Len then
            begin
                Result[i] := ' ';
                Inc(i);
            end;
        end
        else
            Inc(i);
    end;
end;

procedure TNoPascalLabelsTest.ScanFile(const APath: string; AOffenders: TStrings);
var
    Lines: TStringList;
    Text, Word1: string;
    i, p: integer;

    function LooksLikeAJump(const ALine: string; const AKeyword: string): boolean;
    var
        s: string;
        k, n: integer;
    begin
        //  "<keyword> <identifier>" followed by ';' or ',' - the statement and
        //  declaration forms. Anything else is not one.
        Result := False;
        s := LowerCase(Trim(ALine));
        k := Pos(AKeyword + ' ', s);
        if k = 0 then Exit;
        n := k + Length(AKeyword) + 1;
        while (n <= Length(s)) and (s[n] = ' ') do Inc(n);
        if (n > Length(s)) or not (s[n] in ['a'..'z', '_']) then Exit;
        while (n <= Length(s)) and (s[n] in ['a'..'z', '0'..'9', '_']) do Inc(n);
        while (n <= Length(s)) and (s[n] = ' ') do Inc(n);
        Result := (n <= Length(s)) and (s[n] in [';', ',']);
    end;

begin
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(APath);
        Text := StripCommentsAndStrings(Lines.Text);
        Lines.Text := Text;
        for i := 0 to Lines.Count - 1 do
        begin
            Word1 := Lines[i];
            if LooksLikeAJump(Word1, 'goto') or LooksLikeAJump(Word1, 'label') then
            begin
                p := i + 1;
                AOffenders.Add(Format('%s:%d: %s', [APath, p, Trim(Word1)]));
            end;
        end;
    finally
        Lines.Free;
    end;
end;

procedure TNoPascalLabelsTest.ScanTree(const ADir: string; AOffenders: TStrings);
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
                //  Build output and the Lazarus cache carry generated copies of
                //  the same sources; scanning them reports each twice.
                if (LowerCase(Name) = 'o') or (LowerCase(Name) = 'lib') or
                    (LowerCase(Name) = 'backup') or (Name = '.git') then Continue;
                ScanTree(IncludeTrailingPathDelimiter(ADir) + Name, AOffenders);
            end
            else
            begin
                Ext := LowerCase(ExtractFileExt(Name));
                if (Ext = '.pas') or (Ext = '.lpr') or (Ext = '.inc') then
                    ScanFile(IncludeTrailingPathDelimiter(ADir) + Name, AOffenders);
            end;
        until FindNext(Rec) <> 0;
    finally
        FindClose(Rec);
    end;
end;

procedure TNoPascalLabelsTest.NoSourceFileDeclaresALabelOrUsesGoto;
var
    Root: string;
    Offenders: TStringList;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found - a scan of nothing is not a pass',
        Root <> '');
    Offenders := TStringList.Create;
    try
        ScanTree(Root, Offenders);
        AssertEquals('Pascal labels are not allowed here, see AGENTS.md: ' +
            Offenders.Text, 0, Offenders.Count);
    finally
        Offenders.Free;
    end;
end;

initialization
    //  A unit test: it reads source files that are already on disk beside it,
    //  starts no process and touches nothing the product writes.
    RegisterTest('unit', TNoPascalLabelsTest);
end.
