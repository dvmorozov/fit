// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the projects opened on this machine are remembered: the one to
reopen at start-up and the list behind File > Open Recent.)

WHY THIS MACHINE'S SETTINGS. A path names a file on ONE machine. The list was
once kept in config.xml, which a build kept in the checkout (FIT_PROFILE_DIR) -
and a checkout is copied, synced and shared: a Mac offered a Linux machine's
/mnt/data paths in File > Open Recent, every one of them a line that opened
nothing. On Windows that file roamed with the account and carried the list to
every machine it signs in to. It then had a file of its own,
recent-projects.txt, in this machine's folder; it is now the 'recent' section
of settings.json (machine_settings), which is in that same folder and which no
profile folder moves.

WRITTEN THE MOMENT A PROJECT IS OPENED, not when the window closes: a session
that ends in a crash still remembers what it opened.

THE LIST IS A JSON ARRAY in the file, and recent_project's separated string
here: what the list MEANS - promote, de-duplicate, trim, forget - is
recent_project's, over that string. This unit only keeps it.
}
unit recent_project_store;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, machine_settings;

const
    { The section of this machine's settings.json the list is kept in. }
    RecentSection = 'recent';

type
    TRecentProjectStore = class
    private
        FSettings: TMachineSettings;
        FLast, FRecent: string;
        procedure Save;
    public
        { Reads the list from ASettings, which it writes to and does not own. }
        constructor Create(ASettings: TMachineSettings);
        { The window is showing the document at APath ('' for one never
          saved), and the file is written at once. }
        procedure Showing(const APath: string);
        { The project to reopen is gone: neither reopened nor offered. }
        procedure ForgetLast;
        { Takes over a list kept somewhere else - ALast to reopen, ARecent as
          recent_project stores it - and writes it. What an earlier version's
          own file held, carried in once (legacy_settings_import). }
        procedure Adopt(const ALast, ARecent: string);
        property LastProject: string read FLast;
        { As recent_project stores it: RecentMenu and the rest take this. }
        property Recent: string read FRecent;
    end;

implementation

uses
    fpjson, recent_project;

const
    LastKey = 'last';
    ProjectsKey = 'projects';

constructor TRecentProjectStore.Create(ASettings: TMachineSettings);
var
    Section: TJSONObject;
    Projects: TJSONData;
    i: longint;
begin
    inherited Create;
    FSettings := ASettings;
    FLast := FSettings.Str(RecentSection, LastKey);
    Section := FSettings.Section(RecentSection);
    try
        //  In any case, as every key in the file is read.
        Projects := ValueIn(Section, ProjectsKey);
        if Projects is TJSONArray then
            for i := 0 to Projects.Count - 1 do
                //  Only paths: a hand edit's number or null is no project.
                if Projects.Items[i] is TJSONString then
                begin
                    if FRecent <> '' then
                        FRecent := FRecent + RecentSeparator;
                    FRecent := FRecent + Projects.Items[i].AsString;
                end;
    finally
        Section.Free;
    end;
end;

procedure TRecentProjectStore.Save;
var
    Projects: TJSONArray;
    Path: string;
begin
    Projects := TJSONArray.Create;
    for Path in RecentProjects(FRecent) do
        Projects.Add(Path);
    FSettings.ReplaceSection(RecentSection,
        TJSONObject.Create([LastKey, FLast, ProjectsKey, Projects]));
end;

procedure TRecentProjectStore.Showing(const APath: string);
begin
    //  No file system a project is opened from names a file with a line
    //  break, and a menu line cannot show one.
    if (Pos(#10, APath) > 0) or (Pos(#13, APath) > 0) then
        Exit;
    FLast := RememberedAfterShowing(FLast, APath);
    FRecent := RecentAfterOpening(FRecent, APath);
    Save;
end;

procedure TRecentProjectStore.ForgetLast;
begin
    //  OUT OF THE LIST TOO: an entry that opens nothing is a line the user can
    //  only be disappointed by.
    FRecent := RecentWithout(FRecent, FLast);
    FLast := '';
    Save;
end;

procedure TRecentProjectStore.Adopt(const ALast, ARecent: string);
begin
    FLast := ALast;
    FRecent := ARecent;
    Save;
end;

end.
