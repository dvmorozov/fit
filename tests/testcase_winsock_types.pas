// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(That no source beside WinSock names a socket type without its unit.)

THE DEFECT THIS DEFENDS AGAINST. Two units declare a TSocket, and they disagree:
the Sockets unit's is a signed longint, WinSock2's an unsigned UINT_PTR. A unit
that uses both gets whichever was used LAST for a bare TSocket - so in
readiness_channel the fields were Sockets' and the "no socket" constant was
WinSock2's. -1 in a longint never equalled $FFFFFFFFFFFFFFFF, the wait skipped
accept and selected on socket -1, and on Windows every started server, launcher
and Python sidecar was reported as never having said it was listening. The
worker tests then failed in SetUp, left their fit_server running, and the
release job waited half an hour on the pipe it held - twice.

WHY A SOURCE SCAN. WinSock2 exists only on Windows, so no test run on Linux or
macOS can reach the mismatch, and CI tests on Windows only when it releases.
What can be asserted everywhere is the construct: in a source that mentions
WinSock, TSocket is written with its unit's name.
}
unit testcase_winsock_types;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, source_scan;

type
    TWinSockTypesTest = class(TTestCase)
    published
        procedure ABareTSocketBesideWinSockIsReported;
        procedure ASocketTypeNamedByItsUnitIsNot;
        procedure WithoutWinSockABareTSocketIsFine;
        procedure AMentionInACommentIsNot;
        procedure NoSourceNamesAnAmbiguousSocketType;
    end;

{ Appends to ALines the line number of every bare TSocket in ASource, when
  ASource mentions WinSock or WinSock2 anywhere outside comments and strings.
  Public so the rule itself is tested, not only its verdict on this tree. }
procedure BareSocketTypeLines(const ASource: string; ALines: TStrings);

implementation

procedure BareSocketTypeLines(const ASource: string; ALines: TStrings);
var
    Text, Word: string;
    i, j, Len, Line: integer;
    UsesWinSock: boolean;
    Bare: TStringList;
begin
    Text := StripCommentsAndStrings(ASource);
    Len := Length(Text);
    i := 1;
    Line := 1;
    UsesWinSock := False;
    Bare := TStringList.Create;
    try
        while i <= Len do
        begin
            if Text[i] = #10 then
            begin
                Inc(Line);
                Inc(i);
            end
            else if Text[i] in ['A'..'Z', 'a'..'z', '_'] then
            begin
                j := i;
                while (j <= Len) and (Text[j] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
                    Inc(j);
                Word := LowerCase(Copy(Text, i, j - i));
                if (Word = 'winsock') or (Word = 'winsock2') then
                    UsesWinSock := True
                //  "Sockets.TSocket" and "WinSock2.TSocket" say which one they mean.
                else if (Word = 'tsocket') and ((i = 1) or (Text[i - 1] <> '.')) then
                    Bare.Add(IntToStr(Line));
                i := j;
            end
            else
                Inc(i);
        end;
        if UsesWinSock then
            ALines.AddStrings(Bare);
    finally
        Bare.Free;
    end;
end;

procedure TWinSockTypesTest.ABareTSocketBesideWinSockIsReported;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        //  The shape readiness_channel had.
        BareSocketTypeLines('uses Sockets;' + #10 +
            'implementation' + #10 +
            'uses WinSock2;' + #10 +
            'const NoSocket = TSocket(-1);', Lines);
        AssertEquals('one bare TSocket', 1, Lines.Count);
        AssertEquals('on the line it is on', '4', Lines[0]);
    finally
        Lines.Free;
    end;
end;

procedure TWinSockTypesTest.ASocketTypeNamedByItsUnitIsNot;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        BareSocketTypeLines('uses Sockets, WinSock2;' + #10 +
            'const NoSocket = Sockets.TSocket(-1);' + #10 +
            'var S: WinSock2.TSocket;', Lines);
        AssertEquals('nothing reported', 0, Lines.Count);
    finally
        Lines.Free;
    end;
end;

procedure TWinSockTypesTest.WithoutWinSockABareTSocketIsFine;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        //  With one unit in scope there is only one TSocket to mean.
        BareSocketTypeLines('uses Sockets;' + #10 + 'var S: TSocket;', Lines);
        AssertEquals('nothing reported', 0, Lines.Count);
    finally
        Lines.Free;
    end;
end;

procedure TWinSockTypesTest.AMentionInACommentIsNot;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        BareSocketTypeLines('uses WinSock2;' + #10 +
            '{ a bare TSocket explained in a comment }' + #10 +
            '// and TSocket here' + #10 +
            'const S = ''TSocket'';', Lines);
        AssertEquals('nothing reported', 0, Lines.Count);
    finally
        Lines.Free;
    end;
end;

procedure TWinSockTypesTest.NoSourceNamesAnAmbiguousSocketType;
var
    Root: string;
    Files, Source, Lines, Offenders: TStringList;
    i, k: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found - a scan of nothing is not a pass',
        Root <> '');
    Files := TStringList.Create;
    Source := TStringList.Create;
    Lines := TStringList.Create;
    Offenders := TStringList.Create;
    try
        CollectPascalSources(Root, Files);
        AssertTrue('there were sources to scan', Files.Count > 0);
        for i := 0 to Files.Count - 1 do
        begin
            Source.LoadFromFile(Files[i]);
            Lines.Clear;
            BareSocketTypeLines(Source.Text, Lines);
            for k := 0 to Lines.Count - 1 do
                Offenders.Add(Files[i] + ':' + Lines[k]);
        end;
        AssertEquals('Write Sockets.TSocket or WinSock2.TSocket beside WinSock - ' +
            'the two types differ: ' + Offenders.Text, 0, Offenders.Count);
    finally
        Offenders.Free;
        Lines.Free;
        Source.Free;
        Files.Free;
    end;
end;

initialization
    //  A unit test: it reads source files already on disk and starts nothing.
    RegisterTest('unit', TWinSockTypesTest);
end.
