// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which downloaded files were worth keeping, and which were not.)

WHAT THIS ENDS. Every look at a data source writes a file: the preview has to
read something, so the bytes go to disk before anyone has decided anything. Most
of those decisions are "no" - the wrong series, the wrong record, a file that
turned out to hold a web page - and each "no" used to leave a file behind
FOREVER, under a stepped-over name. 'SP500 (9).csv' is what that looks like
after a morning's work, and the user never asked for one of them.

THE RULE. A file that became a project is the user's, and stays: their project's
provenance points at it and Reload profile reads it. A file that did not is
rubbish this program made, and it goes when the program does.

WHERE IT DOES NOT APPLY. A file the user MOVED somewhere of their own (the
wizard's Save in) is theirs by that act, whatever they did next - so only the
application's own downloads directory is ever cleaned. Deleting something out of
somebody's Documents folder because they changed their mind about a chart is not
tidiness.

NOTHING HERE TOUCHES A DISK. Deleting is passed in, so every rule above is
reachable from a test that creates no file at all.
}
unit download_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TStringArray = array of string;

    { Deletes one file and says whether it went. Passed in, so the rules here
      are testable without a disk. }
    TDeleteFile = function(const APath: string): boolean;

    TDownloadSession = class(TObject)
    private
        FWritten: TStringList;
        FKept: TStringList;
        FRoot: string;
    public
        constructor Create;
        destructor Destroy; override;

        { The directory this session may clean. Anything outside it is left
          alone, whatever happened to it - see the unit header. }
        property CleanableRoot: string read FRoot write FRoot;

        { A file this session wrote. Called by whatever saved it. }
        procedure Wrote(const APath: string);
        { A file that became a project, and is therefore the user's. }
        procedure Kept(const APath: string);
        { What would be deleted now, in the order it was written. }
        function Rubbish: TStringArray;
        { Deletes it, and answers how many went. The session forgets what it
          has thrown away, so a second call deletes nothing twice. }
        function Discard(ADelete: TDeleteFile): longint;
    end;

{ The session this application is in. One per process: the wizard writes into
  it, the import marks what was kept, and the window empties it on the way out. }
function CurrentDownloadSession: TDownloadSession;

{ Deletes a file. The application's own, passed to Discard. }
function DefaultDeleteFile(const APath: string): boolean;

implementation

var
    Session: TDownloadSession = nil;

function DefaultDeleteFile(const APath: string): boolean;
begin
    Result := DeleteFile(APath);
end;

function CurrentDownloadSession: TDownloadSession;
begin
    if Session = nil then
        Session := TDownloadSession.Create;
    Result := Session;
end;

constructor TDownloadSession.Create;
begin
    inherited Create;
    FWritten := TStringList.Create;
    FKept := TStringList.Create;
end;

destructor TDownloadSession.Destroy;
begin
    FKept.Free;
    FWritten.Free;
    inherited Destroy;
end;

procedure TDownloadSession.Wrote(const APath: string);
begin
    if Trim(APath) = '' then
        Exit;
    if FWritten.IndexOf(APath) < 0 then
        FWritten.Add(APath);
end;

procedure TDownloadSession.Kept(const APath: string);
begin
    if Trim(APath) = '' then
        Exit;
    //  Recorded even when this session did not write it - a project made from
    //  a file fetched in an earlier session keeps that file, and saying so
    //  costs one line and removes a way to get this wrong.
    if FKept.IndexOf(APath) < 0 then
        FKept.Add(APath);
end;

{ Whether APath is inside ARoot. Compared as text on a normalised path: both
  come from this program, and a downloaded file's name is sanitised before it
  is ever used. }
function Inside(const ARoot, APath: string): boolean;
var
    Root: string;
begin
    Root := Trim(ARoot);
    if Root = '' then
        //  NOTHING IS CLEANABLE when no root is stated. A session that does
        //  not know where its own cache is must not delete anything.
        Exit(False);
    Root := IncludeTrailingPathDelimiter(ExpandFileName(Root));
    Result := Pos(LowerCase(Root), LowerCase(ExpandFileName(APath))) = 1;
end;

function TDownloadSession.Rubbish: TStringArray;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to FWritten.Count - 1 do
    begin
        if FKept.IndexOf(FWritten[i]) >= 0 then
            Continue;
        if not Inside(FRoot, FWritten[i]) then
            Continue;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FWritten[i];
    end;
end;

function TDownloadSession.Discard(ADelete: TDeleteFile): longint;
var
    Going: TStringArray;
    i: longint;
begin
    Result := 0;
    if not Assigned(ADelete) then
        Exit;
    Going := Rubbish;
    for i := 0 to High(Going) do
    begin
        if ADelete(Going[i]) then
            Inc(Result);
        //  FORGOTTEN EITHER WAY. A file that would not delete - open in
        //  another program, on a disk that has gone away - is not worth
        //  trying again and again at every close.
        FWritten.Delete(FWritten.IndexOf(Going[i]));
    end;
end;

finalization
    Session.Free;
end.
