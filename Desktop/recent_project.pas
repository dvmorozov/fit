// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the application opens when it starts.)

THE DECISION HAS FOUR INPUTS and one of them is a file system, which is why this
is a function taking an existence check rather than a block inside the start-up
sequence. Every branch is then reachable without writing a file, and the rules
below are readable as rules instead of being spread through `Fit.lpr`.

THE PRECEDENCE, and why each step is above the next:

  * an explicit /PROJECT= is the user saying which project, NOW. Nothing
    overrides it, a remembered one least of all: a convenience never overrides
    an instruction.
  * /INFILE= means "start fresh with this data", which is what it has meant
    since before projects existed and must go on meaning. Below /PROJECT= and
    above the remembered project - because opening the last project and then
    loading data into it would silently modify a document the user never asked
    to open.
  * only then the last project, which is a convenience.

A MISSING FILE IS A WARNING, NOT A FAILURE. The remembered project may have been
deleted, renamed, or be on a drive that is not mounted, and the application still
has to start - refusing would leave the user with no way in at all. It must not
pass in silence either: a project simply not appearing is indistinguishable from
a broken auto-open, which is the same reasoning /INFILE has always carried.

AND A FILE NAMED ON THE COMMAND LINE IS NEVER SILENTLY SUBSTITUTED. If the user
asked for a project that is not there, falling back to the remembered one would
open a DIFFERENT document from the one asked for, with only the window title to
say so.
}
unit recent_project;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Types;

type
    { What to open. }
    TStartupChoice = (
        { An empty window. Either nothing was asked for, or what was asked for
          is not there - the warning says which. }
        scNothing,
        { A project file. }
        scProject,
        { A data file, into a new empty project. }
        scDataFile);

    TStartupPlan = record
        Choice: TStartupChoice;
        Path: string;
        { What to tell the user, or '' when there is nothing to say. Never a
          reason to stop starting up. }
        Warning: string;
    end;

    { Whether APath names something that can be opened. Passed in so this unit
      touches no file system and every branch stays reachable from a test. }
    TPathExists = function(const APath: string): boolean;

const
    { How many projects File > Open Recent offers. Long enough to reach last
      week's work, short enough that the menu is read rather than scanned. }
    RecentProjectLimit = 8;
    { What separates the entries in the one string the settings file holds. A
      character no file system allows in a name, so a path can never be read
      back as two. }
    RecentSeparator = '|';

{ The list as the settings file holds it, after APath has been opened.

  MOST RECENT FIRST, one entry per project, at most RecentProjectLimit of them.
  Opening a project already in the list MOVES it rather than repeating it: a
  menu offering the same file twice offers a choice that is not one, and pushes
  something real off the end to do it.

  A path that is empty, or that contains the separator, leaves the list
  untouched - New Project has no path, and a name carrying the separator would
  be written out and read back as two files that do not exist. }
function RecentAfterOpening(const AStored, APath: string): string;

{ The projects a stored list names, most recent first. }
function RecentProjects(const AStored: string): TStringDynArray;

{ The same list without APath - what the application does with a project that
  is gone, or that would not open. Matched the way RecentAfterOpening matches:
  the same file under another spelling is the same entry. An empty path, or one
  the list does not hold, leaves it exactly as it was. }
function RecentWithout(const AStored, APath: string): string;

{ Whether APath is really there: the check the application itself passes in.

  A NAMED FUNCTION RATHER THAN @FileExists, and this is not a stylistic
  preference. The RTL has no one-argument FileExists - both overloads take a
  FollowLink parameter with a default, which serves the CALL and does nothing
  for the ADDRESS. Fit.lpr compiled in Delphi syntax mode, where @Routine is
  assignment-compatible with ANY procedural variable, so the wrong address was
  taken in silence: the two-argument UnicodeString overload, called with an
  AnsiString. The last project was reported as no longer there and the
  application started empty, on every run.

  Under {$mode objfpc} the compiler refuses that line and prints both
  signatures. Fit.lpr now declares it, and tools/build-tests/syntax_mode.tests.ps1
  keeps every program file that way.

  The same trap took DefaultSourceNotice earlier in this feature. A wrapper with
  the exact signature is what makes the compiler check it again, and it is
  reachable from a test, which @FileExists never was. }
function DefaultPathExists(const APath: string): boolean;

{ What to open, given the two command-line switches, the remembered project, and
  a way to ask whether a path is there. }
function PlanStartup(const AProjectSwitch, AInFileSwitch, ALastProject: string;
    AExists: TPathExists): TStartupPlan;

implementation

function DefaultPathExists(const APath: string): boolean;
begin
    Result := FileExists(APath);
end;

function RecentProjects(const AStored: string): TStringDynArray;
var
    Rest, One: string;
    n, p: longint;
begin
    SetLength(Result, 0);
    Rest := AStored;
    n := 0;
    while Rest <> '' do
    begin
        p := Pos(RecentSeparator, Rest);
        if p = 0 then
        begin
            One := Rest;
            Rest := '';
        end
        else
        begin
            One := Copy(Rest, 1, p - 1);
            Rest := Copy(Rest, p + 1, Length(Rest));
        end;
        //  An empty entry is what a trailing separator or a settings file
        //  edited by hand leaves behind. It names no project.
        if Trim(One) = '' then
            Continue;
        SetLength(Result, n + 1);
        Result[n] := One;
        Inc(n);
    end;
end;

function RecentWithout(const AStored, APath: string): string;
var
    Names: TStringDynArray;
    i: longint;
begin
    Result := AStored;
    if Trim(APath) = '' then
        Exit;
    Names := RecentProjects(AStored);
    Result := '';
    for i := 0 to High(Names) do
    begin
        if SameText(Names[i], APath) then
            Continue;
        if Result <> '' then
            Result := Result + RecentSeparator;
        Result := Result + Names[i];
    end;
end;

function RecentAfterOpening(const AStored, APath: string): string;
var
    Names: TStringDynArray;
    Kept: string;
    i, n: longint;
begin
    Result := AStored;
    if Trim(APath) = '' then
        Exit;
    //  See the header: the separator is the one character a path may not carry.
    if Pos(RecentSeparator, APath) > 0 then
        Exit;

    Names := RecentProjects(AStored);
    Result := APath;
    n := 1;
    for i := 0 to High(Names) do
    begin
        if n >= RecentProjectLimit then
            Break;
        Kept := Names[i];
        //  THE SAME FILE UNDER ANOTHER SPELLING is the same entry. Windows and
        //  macOS both open A.fitproj and a.fitproj as one file; on a case
        //  sensitive system the cost of this is a merged entry rather than a
        //  lost project, which is the cheaper way to be wrong.
        if SameText(Kept, APath) then
            Continue;
        Result := Result + RecentSeparator + Kept;
        Inc(n);
    end;
end;

{ Surrounding whitespace comes from the command line and is not part of a name;
  anything inside it is, because a path with a space in it is ordinary
  everywhere this runs. A switch with nothing after it is "not asked for"
  rather than a file named by the empty string. }
function Given(const APath: string): string;
begin
    Result := Trim(APath);
end;

function PlanStartup(const AProjectSwitch, AInFileSwitch, ALastProject: string;
    AExists: TPathExists): TStartupPlan;
var
    Asked: string;
begin
    Result.Choice := scNothing;
    Result.Path := '';
    Result.Warning := '';

    //  1. The project the user named. Never substituted.
    Asked := Given(AProjectSwitch);
    if Asked <> '' then
    begin
        if AExists(Asked) then
        begin
            Result.Choice := scProject;
            Result.Path := Asked;
        end
        else
            Result.Warning := 'The project "' + Asked +
                '" could not be found, so Fit started with nothing open.';
        Exit;
    end;

    //  2. The data file the user named: start fresh with it.
    Asked := Given(AInFileSwitch);
    if Asked <> '' then
    begin
        if AExists(Asked) then
        begin
            Result.Choice := scDataFile;
            Result.Path := Asked;
        end
        else
            Result.Warning := 'The data file "' + Asked +
                '" could not be found, so Fit started with nothing open.';
        Exit;
    end;

    //  3. The project last open, as a convenience.
    Asked := Given(ALastProject);
    if Asked = '' then
        Exit;
    if AExists(Asked) then
    begin
        Result.Choice := scProject;
        Result.Path := Asked;
    end
    else
        //  NAMED, so the reader is not left looking for which project. It is
        //  ordinary for this one to have gone - the file may have been moved
        //  since the last session - so the wording says what happened rather
        //  than reporting a fault.
        Result.Warning := 'The project last open, "' + Asked +
            '", is no longer there, so Fit started with nothing open.';
end;

end.
