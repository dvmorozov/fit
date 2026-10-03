// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What happens between a previewed download and a project holding it.)

IT REUSES THE IMPORT THAT EXISTS. A downloaded file is a file: it goes through
the same LoadDataFile the File menu uses, which goes through TFitClient's
loader, its profile and its PUT to the server. There is deliberately no second
way in - a parallel path would be a second answer to what importing means, and
every "worked in tests, not in the app" defect in findings.md was exactly that.

THE ORDER IS THE WHOLE OF THIS UNIT, and each step is here because leaving it
out is a defect somebody would have to find:

  1. ASK BEFORE DISCARDING. A download is not worth losing an unsaved fit for.
  2. START A NEW PROJECT, so the downloaded data does not arrive inside a
     document that was about something else - the user asked for data, and what
     they get is a project of it.
  3. IMPORT THE FILE by the ordinary path.
  4. RECORD WHERE IT CAME FROM. Provenance is filled by the import itself with
     the file's own path, which for a download is a name in a cache directory
     and says nothing about the service, the query or the day - so the origin
     is recorded after it, and only then.

WHY A HOST INTERFACE. Steps 1 to 4 are the window's to perform and this unit's
to order; the seam is what lets the order be tested without a window, which is
where the decision would otherwise be unreachable.
}
unit data_source_import;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source_wizard;

type
    { Raised when this unit is asked to import with something missing that the
      caller was supposed to supply. A programming error in the window, not a
      user error, so it says which of them it was. }
    EImportHostMissing = class(Exception);

    { What the window does on this unit's behalf. }
    IImportHost = interface
        { Whether the current document may be replaced - asks the user when
          there is unsaved work. False means they said no. }
        function MayReplaceDocument: boolean;
        { Starts an empty project. }
        procedure NewProject;
        { Imports a data file exactly as File > Import Profile does. }
        procedure ImportProfileFile(const APath: string);
        { Records where the imported file came from. }
        procedure RememberOrigin(const AOrigin: TDownloadOrigin);
        { The folder the user last chose to keep downloads in, or '' when they
          have never chosen one - and then the per-user default is used. Asked
          of the window because it is what remembers things between sessions. }
        function DownloadFolder: string;
        procedure RememberDownloadFolder(const APath: string);
    end;

{ Turns the file the wizard downloaded into a project. False when the user
  declined to discard what was open; the wizard then stays as it was, with the
  download still in hand, so answering "no" costs nothing that was fetched. }
function ImportDownload(AHost: IImportHost; const APath: string;
    const AOrigin: TDownloadOrigin): boolean;

{ The origin as one line, for a window title, a log or a report. Public because
  the same sentence is what a user has to quote when they publish a result. }
function OriginText(const AOrigin: TDownloadOrigin): string;

implementation

uses
    download_session;

function ImportDownload(AHost: IImportHost; const APath: string;
    const AOrigin: TDownloadOrigin): boolean;
begin
    if AHost = nil then
        raise EImportHostMissing.Create(
            'nothing was given to import the download into');
    Result := False;
    if Trim(APath) = '' then
        raise EImportHostMissing.Create(
            'no downloaded file to import');

    //  ASKED BEFORE ANYTHING IS TOUCHED. A user who says no must find the
    //  document exactly as they left it.
    if not AHost.MayReplaceDocument then
        Exit;

    AHost.NewProject;
    AHost.ImportProfileFile(APath);
    //  AFTER the import: the import fills provenance from the file it read,
    //  which for a download is a path in a cache directory.
    AHost.RememberOrigin(AOrigin);
    //  AND THE FILE IS NOW THE USER'S. A project points at it, and Reload
    //  profile reads it, so it survives the close that throws away everything
    //  downloaded and not used.
    CurrentDownloadSession.Kept(APath);
    Result := True;
end;

function OriginText(const AOrigin: TDownloadOrigin): string;
begin
    if AOrigin.SourceId = '' then
        Exit('');
    Result := AOrigin.SourceTitle;
    if AOrigin.Query <> '' then
        Result := Result + ' (' + AOrigin.Query + ')';
    if AOrigin.Address <> '' then
        Result := Result + ', ' + AOrigin.Address;
    if AOrigin.RetrievedAt > 0 then
        Result := Result + ', fetched ' +
            FormatDateTime('yyyy-mm-dd hh:nn', AOrigin.RetrievedAt);
end;

end.
