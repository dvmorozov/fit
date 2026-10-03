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

Comments and string literals are stripped before the scan (source_scan), so the
prose in this file and the explanatory comments left where the labels used to be
do not trip it. See docs/contributing/building.md.
}
unit testcase_no_pascal_labels;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, source_scan;

type
    TNoPascalLabelsTest = class(TTestCase)
    private
        procedure ScanFile(const APath: string; AOffenders: TStrings);
    published
        procedure NoSourceFileDeclaresALabelOrUsesGoto;
    end;

implementation

procedure TNoPascalLabelsTest.ScanFile(const APath: string; AOffenders: TStrings);
var
    Lines: TStringList;
    Word1: string;
    i: integer;

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
        Lines.Text := StripCommentsAndStrings(Lines.Text);
        for i := 0 to Lines.Count - 1 do
        begin
            Word1 := Lines[i];
            if LooksLikeAJump(Word1, 'goto') or LooksLikeAJump(Word1, 'label') then
                AOffenders.Add(Format('%s:%d: %s', [APath, i + 1, Trim(Word1)]));
        end;
    finally
        Lines.Free;
    end;
end;

procedure TNoPascalLabelsTest.NoSourceFileDeclaresALabelOrUsesGoto;
var
    Root: string;
    Files, Offenders: TStringList;
    i: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found - a scan of nothing is not a pass',
        Root <> '');
    Files := TStringList.Create;
    Offenders := TStringList.Create;
    try
        CollectPascalSources(Root, Files);
        for i := 0 to Files.Count - 1 do
            ScanFile(Files[i], Offenders);
        AssertEquals('Pascal labels are not allowed here, see AGENTS.md: ' +
            Offenders.Text, 0, Offenders.Count);
    finally
        Offenders.Free;
        Files.Free;
    end;
end;

initialization
    //  A unit test: it reads source files that are already on disk beside it,
    //  starts no process and touches nothing the product writes.
    RegisterTest('unit', TNoPascalLabelsTest);
end.
