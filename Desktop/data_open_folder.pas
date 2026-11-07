// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which folder File > Import Profile's dialog opens in.)

WHY THIS IS DECIDED AT ALL. The dialog used to open in the program's own folder
every time - where nobody keeps data, and on an installed copy a folder the user
may not even be allowed to write to - so every import began by climbing out of
it. A dialog should open where the user's data is.

THE PRECEDENCE, and why each step is above the next:
  * the folder of the data the open project was made from, when it was chosen
    on this computer: that is where this work's data lives, and the likeliest
    place for the next file of it. NOT when the data was fetched by a data
    source - its file then sits in the program's downloaded-data folder, which
    is a cache the user never chose and should not have to browse.
  * the folder of the data file chosen in this dialog last, this session: the
    user's own most recent answer to the same question, for a project that has
    no data of its own on this computer.
  * the folder of the project file itself, for a project whose data was fetched
    or whose data folder is gone: data is commonly kept beside the project.
  * the user's home folder: somewhere the user's files are, unlike the
    program's.
A folder that does not exist (a drive not mounted, a folder renamed) is passed
over rather than handed to the dialog, which would open somewhere arbitrary.

Passed an existence check, like recent_project, so it touches no file system and
every step is reachable from a test.
}
unit data_open_folder;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_project_document;

type
    { Whether a folder is there. }
    TFolderExists = function(const AFolder: string): boolean;

    { What the window knows when the dialog is about to open. }
    TDataOpenContext = record
        { The file the open project's data was read from, or ''. }
        ProjectDataFile: string;
        { True when that file was fetched by a data source rather than chosen
          on this computer. }
        ProjectDataFetched: boolean;
        { The data file this dialog last chose, this session, or ''. }
        LastChosenFile: string;
        { The project file, or '' for a project never saved. }
        ProjectFile: string;
        { The user's home folder. }
        HomeFolder: string;
    end;

{ What the window knows, from the open project's provenance and path, the
  dialog's last choice and the user's home folder. }
function DataOpenContextOf(const AProvenance: TProjectProvenance;
    const AProjectFile, ALastChosenFile, AHomeFolder: string): TDataOpenContext;

{ The folder the dialog opens in; '' leaves it to the dialog. }
function DataOpenFolder(const AContext: TDataOpenContext;
    AFolderExists: TFolderExists): string;
{ The same, asking the file system. }
function DataOpenFolder(const AContext: TDataOpenContext): string;

{ DirectoryExists, with its signature written out: the window is compiled in a
  mode where taking an RTL routine's address binds whichever overload it finds
  (fit's AGENTS.md). }
function DefaultFolderExists(const AFolder: string): boolean;

implementation

function DataOpenContextOf(const AProvenance: TProjectProvenance;
    const AProjectFile, ALastChosenFile, AHomeFolder: string): TDataOpenContext;
begin
    Result.ProjectDataFile := AProvenance.SourcePath;
    //  A data source records itself; a file chosen here leaves this empty.
    Result.ProjectDataFetched := Trim(AProvenance.OriginSourceId) <> '';
    Result.LastChosenFile := ALastChosenFile;
    Result.ProjectFile := AProjectFile;
    Result.HomeFolder := AHomeFolder;
end;

function DataOpenFolder(const AContext: TDataOpenContext;
    AFolderExists: TFolderExists): string;

    function FolderOf(const AFile: string; out AFolder: string): boolean;
    begin
        AFolder := ExtractFilePath(Trim(AFile));
        Result := (AFolder <> '') and AFolderExists(AFolder);
    end;

begin
    if not AContext.ProjectDataFetched and
        FolderOf(AContext.ProjectDataFile, Result) then
        Exit;
    if FolderOf(AContext.LastChosenFile, Result) then
        Exit;
    if FolderOf(AContext.ProjectFile, Result) then
        Exit;
    //  Not checked: the dialog falls back on its own if even home is gone,
    //  and there is nowhere better left to suggest.
    Result := AContext.HomeFolder;
end;

function DataOpenFolder(const AContext: TDataOpenContext): string;
begin
    Result := DataOpenFolder(AContext, @DefaultFolderExists);
end;

function DefaultFolderExists(const AFolder: string): boolean;
begin
    Result := DirectoryExists(AFolder);
end;

end.
