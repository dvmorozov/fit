// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A project on disk: capture and write, read and apply.)

THE ONLY PLACE A PROJECT MEETS THE FILE SYSTEM. Everything under it - the
container, the document, the sections, the capture, the restore order - works
over records and streams and is covered by unit tests. This unit is the pair of
wrappers that turn a path into a stream, and it decides almost nothing, which is
the point: what is left to go wrong here is a missing file or a full disk, and
both of those are reported rather than raised.

WHY A FAILURE IS A MESSAGE AND NOT AN EXCEPTION. Both of these are reached from a
menu command. What the user needs is a sentence saying which part of their
project did not come back, or why it could not be written; an exception escaping
into the LCL's handler gives them a class name and a stack.

WHAT AN OPEN DOES NOT DO. It does not touch the window. The document it returns
carries the working context - the axis, the tab, the selected curve - and
applying THAT is the window's business, because only the window has the widgets.
Restoring the problem itself is done here because it goes through IFitService,
which the window does not own either.
}
unit fit_project_file;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    int_fit_service, fit_project_document, fit_project_json,
    fit_project_session;

{ Captures AService plus AContext and writes the result to APath.

  APREVIOUS is the document being replaced, so that parts this build did not
  understand survive the save - see CaptureProject.

  False with AFault set - never an exception - for anything the file system
  refuses: no such directory, no permission, no space. }
function SaveProjectFile(AService: IFitService;
    const AContext: TProjectClientContext; const APrevious: TProjectDocument;
    const APath: string; out AFault: string): boolean;

{ Reads APath and applies the problem it describes to AService.

  ADoc comes back so the caller can apply the working context, which needs
  widgets this unit must not name. AService must be a fresh problem: a restore
  does not undo what a previous document left behind. }
function OpenProjectFile(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;

implementation

function SaveProjectFile(AService: IFitService;
    const AContext: TProjectClientContext; const APrevious: TProjectDocument;
    const APath: string; out AFault: string): boolean;
var
    Doc: TProjectDocument;
    S: TFileStream;
begin
    AFault := '';
    Result := False;
    try
        Doc := CaptureProject(AService, AContext, APrevious);
        //  Written whole, then closed. Not written over the existing file in
        //  place: a save that fails half way would leave the user with neither
        //  the old project nor the new one, and this is the one operation whose
        //  whole purpose is not losing work.
        S := TFileStream.Create(APath, fmCreate);
        try
            Result := WriteProjectToStream(Doc, S);
            if not Result then
                AFault := 'The project could not be written to "' + APath + '".';
        finally
            S.Free;
        end;
    except
        on E: Exception do
        begin
            Result := False;
            AFault := 'The project could not be saved to "' + APath + '": ' +
                E.Message;
        end;
    end;
end;

function OpenProjectFile(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;
var
    S: TFileStream;
begin
    ADoc := EmptyProjectDocument;
    AFault := '';
    Result := False;
    if not FileExists(APath) then
    begin
        AFault := 'There is no project at "' + APath + '".';
        Exit;
    end;
    try
        S := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
        try
            if not ReadProjectFromStream(S, ADoc, AFault) then
                Exit;
        finally
            S.Free;
        end;
    except
        on E: Exception do
        begin
            AFault := 'The project at "' + APath + '" could not be read: ' +
                E.Message;
            Exit;
        end;
    end;
    //  Read first, applied second, and only if the read succeeded: a file that
    //  turns out not to be a project must leave the problem the user already
    //  had exactly as it was.
    Result := ApplyProject(AService, ADoc, AFault);
end;

end.
