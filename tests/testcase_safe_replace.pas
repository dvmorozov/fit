// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for replacing a file so that a failed write leaves the old one.

  WHY THESE EXIST. Saving a project opened the target with fmCreate, which
  truncates it on the spot, and then wrote into it - so a save that failed half
  way (a full disk, a lost network share) left neither the old project nor the
  new one. The unit that did it said, in a comment, that it did not. What these
  check is the promise: until the new content is complete, the old file is
  exactly as it was. }
unit testcase_safe_replace;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, safe_replace;

type
    { A stream that fails after handing out a few bytes, the way a write that
      runs out of space fails part way. }
    TFailingStream = class(TStringStream)
    public
        function Read(var Buffer; Count: longint): longint; override;
    end;

    TSafeReplaceTest = class(TTestCase)
    private
        FDir: string;
        function PathOf(const AName: string): string;
        function ContentOf(const APath: string): string;
        procedure WriteFile(const APath, AText: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ANewFileIsWritten;
        procedure AnExistingFileIsReplaced;
        procedure AFailedWriteLeavesTheOldFileAsItWas;
        procedure AndLeavesNothingBehindBesideIt;
        procedure AFailureIsReportedInWords;
    end;

implementation

function TFailingStream.Read(var Buffer; Count: longint): longint;
begin
    if Position >= 4 then
        raise EStreamError.Create('no space left on device');
    if Count > 4 - Position then
        Count := 4 - Position;
    Result := inherited Read(Buffer, Count);
end;

procedure TSafeReplaceTest.SetUp;
begin
    FDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
        'fit-safe-replace-' + IntToStr(GetTickCount64) + PathDelim;
    ForceDirectories(FDir);
end;

procedure TSafeReplaceTest.TearDown;
var
    Info: TSearchRec;
begin
    if FindFirst(FDir + '*', faAnyFile, Info) = 0 then
    begin
        repeat
            if (Info.Attr and faDirectory) = 0 then
                DeleteFile(FDir + Info.Name);
        until FindNext(Info) <> 0;
        FindClose(Info);
    end;
    RemoveDir(FDir);
end;

function TSafeReplaceTest.PathOf(const AName: string): string;
begin
    Result := FDir + AName;
end;

function TSafeReplaceTest.ContentOf(const APath: string): string;
var
    S: TStringStream;
begin
    S := TStringStream.Create('');
    try
        S.LoadFromFile(APath);
        Result := S.DataString;
    finally
        S.Free;
    end;
end;

procedure TSafeReplaceTest.WriteFile(const APath, AText: string);
var
    S: TStringStream;
begin
    S := TStringStream.Create(AText);
    try
        S.SaveToFile(APath);
    finally
        S.Free;
    end;
end;

procedure TSafeReplaceTest.ANewFileIsWritten;
var
    Content: TStringStream;
    Fault: string;
begin
    Content := TStringStream.Create('new project');
    try
        AssertTrue('written: ' + Fault,
            ReplaceFileWith(PathOf('a.fitproj'), Content, Fault));
    finally
        Content.Free;
    end;
    AssertEquals('new project', ContentOf(PathOf('a.fitproj')));
end;

procedure TSafeReplaceTest.AnExistingFileIsReplaced;
var
    Content: TStringStream;
    Fault: string;
begin
    WriteFile(PathOf('a.fitproj'), 'old project');
    Content := TStringStream.Create('new project');
    try
        AssertTrue('written: ' + Fault,
            ReplaceFileWith(PathOf('a.fitproj'), Content, Fault));
    finally
        Content.Free;
    end;
    AssertEquals('new project', ContentOf(PathOf('a.fitproj')));
end;

procedure TSafeReplaceTest.AFailedWriteLeavesTheOldFileAsItWas;
var
    Content: TFailingStream;
    Fault: string;
begin
    WriteFile(PathOf('a.fitproj'), 'old project');
    Content := TFailingStream.Create('new project, which never all arrives');
    try
        AssertFalse('not written', ReplaceFileWith(PathOf('a.fitproj'),
            Content, Fault));
    finally
        Content.Free;
    end;
    AssertEquals('the old project, whole', 'old project',
        ContentOf(PathOf('a.fitproj')));
end;

procedure TSafeReplaceTest.AndLeavesNothingBehindBesideIt;
var
    Content: TFailingStream;
    Fault: string;
    Info: TSearchRec;
    Count: longint;
begin
    WriteFile(PathOf('a.fitproj'), 'old project');
    Content := TFailingStream.Create('new project, which never all arrives');
    try
        ReplaceFileWith(PathOf('a.fitproj'), Content, Fault);
    finally
        Content.Free;
    end;
    Count := 0;
    if FindFirst(FDir + '*', faAnyFile, Info) = 0 then
    begin
        repeat
            if (Info.Attr and faDirectory) = 0 then
                Inc(Count);
        until FindNext(Info) <> 0;
        FindClose(Info);
    end;
    AssertEquals('only the project itself', 1, Count);
end;

procedure TSafeReplaceTest.AFailureIsReportedInWords;
var
    Content: TFailingStream;
    Fault: string;
begin
    Content := TFailingStream.Create('never all arrives');
    try
        ReplaceFileWith(PathOf('a.fitproj'), Content, Fault);
    finally
        Content.Free;
    end;
    AssertTrue('says why: ' + Fault, Pos('no space left', Fault) > 0);
end;

initialization
    RegisterTest('unit', TSafeReplaceTest);
end.
