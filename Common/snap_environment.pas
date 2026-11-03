// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A Fit process started inside some snap's environment gives that
environment back before anything reads it.)

THE DEFECT. File > Open Project froze the window - "Application is not
responding" - with nothing in either log after the dialog was asked for. The
main thread was entirely in KDE:

    TOpenDialog.Execute -> QFileDialog_selectNameFilter
      -> KDirOperator::checkPreviewSupport -> KSycoca::ensureCacheValid
      -> KBuildSycoca::recreate

The client had been started from the terminal of an application installed as a
snap (VS Code), and a snap's children inherit the snap's environment: its
XDG_DATA_HOME and XDG_DATA_DIRS under ~/snap/<name>/<rev> and /snap/<name>/<rev>,
its GTK, GIO and locale paths, and SNAP itself. KDE keys its service cache on the
data directories, and for the snap's set it could never be built
(kbuildsycoca5 ran two and a half minutes and died on std::bad_alloc) - so every
file dialog rebuilt it on the GUI thread, forever. With the desktop's own
directories the same rebuild takes 0.2 s. The server has the same exposure in a
quieter form: it looks for the Python sidecar under XDG_DATA_HOME.

WHY IN THE PROGRAM AND NOT IN A LAUNCHER. The first fix repaired the environment
in the build script's `-Task run`, from VS Code's own record of what it replaced
(<NAME>_VSCODE_SNAP_ORIG). That fixed one editor on one path: an installed Fit
started from any snap's terminal still froze. Every snap exports the same SNAP*
variables and roots its private directories under them, so the rule below is
stated over those and needs no editor's bookkeeping.

NOT BY THE SNAP VARIABLE. The first version of this rule acted only when SNAP
was set, was tested from a shell that carried it, and did nothing at all in the
terminal the user actually ran: VS Code's terminal REMOVES SNAP and every SNAP_*
and leaves the snap's directories in everything else. So a snap's directories
are recognised by where snapd puts them - /snap/<name>, /var/lib/snapd/snap/<name>
(where /snap is not the mount point), /var/snap/<name> and ~/snap/<name> - and
the SNAP* values, when present, only add to that list.

THE RULE. Unless this executable is itself inside a snap (then the environment
is its own):
  - SNAP and every SNAP_* variable go - Qt reads SNAP as "sandboxed" and routes
    dialogs through the portal, and a process outside any snap is not in one;
  - a variable whose value lies inside a snap's directories goes: the snap set
    it from nothing, so the desktop's value is "unset";
  - a colon-separated list keeps its entries outside them, in order, and goes
    when none is left.
/snap/bin and /var/lib/snapd/desktop are the HOST's - launchers and menu entries
for every installed snap, present in a session with no snap running - and stay.
SNAP_REAL_HOME and SNAP_LIBRARY_PATH are not roots either: they name the user's
home and the host's GL libraries, and taking them for the snap's would empty HOME.

WHAT CANNOT BE REPAIRED HERE: LD_LIBRARY_PATH and LD_PRELOAD have done their work
before the first line of Pascal runs.
}
unit snap_environment;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { One variable to rewrite (Value) or to remove (Remove). }
    TEnvironmentChange = record
        Name: string;
        Value: string;
        Remove: boolean;
    end;
    TEnvironmentChanges = array of TEnvironmentChange;

{ What to change in AEnvironment (NAME=VALUE lines) so that a process at
  AExecutable no longer runs in a snap's environment it merely inherited. Empty
  outside a snap. Only ever names variables AEnvironment holds: nothing is added. }
function SnapLeakRepairs(AEnvironment: TStrings;
    const AExecutable: string): TEnvironmentChanges;

{ Makes AChanges in this process, where both libc (Qt, KDE, and every child's
  execve) and the Pascal RTL (GetEnvironmentVariable, TProcess) read them. Does
  nothing off Linux, the only system snaps exist on. }
procedure ApplyEnvironmentChanges(const AChanges: TEnvironmentChanges);

{ Reads this process's environment, repairs it, and says what it changed. What
  the initialization section below runs. }
function RepairEnvironmentLeakedFromSnap: TEnvironmentChanges;

{ One log line; empty when nothing was changed. }
function DescribeEnvironmentChanges(const AChanges: TEnvironmentChanges): string;

var
    { What the start-up repair changed, kept for the programs to log once their
      log is open - the repair runs before it is. }
    EnvironmentRepairedAtStartup: TEnvironmentChanges;

implementation

{$IFDEF LINUX}
uses
    ctypes;

function setenv(AName, AValue: PChar; AOverwrite: cint): cint; cdecl; external 'c';
function unsetenv(AName: PChar): cint; cdecl; external 'c';
function malloc(ASize: csize_t): Pointer; cdecl; external 'c';
function strdup(AText: PChar): PChar; cdecl; external 'c';
procedure free(APointer: Pointer); cdecl; external 'c';

var
    environ: PPChar; cvar; external;
    //  The copy System.envp was last pointed at, freed when replaced.
    OwnCopy: PPChar = nil;
{$ENDIF}

const
    //  The snap's OWN directories. Not SNAP_REAL_HOME, not SNAP_LIBRARY_PATH:
    //  see the header.
    SnapRootNames: array[0..4] of string =
        ('SNAP', 'SNAP_DATA', 'SNAP_COMMON', 'SNAP_USER_DATA', 'SNAP_USER_COMMON');

function IsUnder(const APath, ARoot: string): boolean;
begin
    //  A parent, not a prefix: /snap/code/2630 is not inside /snap/code/263.
    Result := (ARoot <> '') and ((APath = ARoot) or
        (Copy(APath, 1, Length(ARoot) + 1) = ARoot + '/'));
end;

{ Inside <APrefix><name>, for a name that is a snap's: `bin` beside the snaps
  is the host's launcher directory. }
function IsUnderSnapsIn(const APath, APrefix: string): boolean;
var
    Rest, Name: string;
    Slash: integer;
begin
    Result := False;
    if Copy(APath, 1, Length(APrefix)) <> APrefix then
        Exit;
    Rest := Copy(APath, Length(APrefix) + 1, MaxInt);
    Slash := Pos('/', Rest);
    if Slash > 0 then
        Name := Copy(Rest, 1, Slash - 1)
    else
        Name := Rest;
    Result := (Name <> '') and (Name <> 'bin');
end;

{ Inside some snap's own directories, by snapd's layout or by a root the
  environment named. }
function IsSnapPath(const APath, AHome: string; const ARoots: TStringArray): boolean;
var
    Root: string;
begin
    if IsUnderSnapsIn(APath, '/snap/') or
        IsUnderSnapsIn(APath, '/var/lib/snapd/snap/') or
        IsUnderSnapsIn(APath, '/var/snap/') or
        ((AHome <> '') and IsUnderSnapsIn(APath, ExcludeTrailingPathDelimiter(AHome) + '/snap/')) then
        Exit(True);
    for Root in ARoots do
        if IsUnder(APath, Root) then
            Exit(True);
    Result := False;
end;

procedure AddChange(var AChanges: TEnvironmentChanges; const AName, AValue: string;
    ARemove: boolean);
var
    n: integer;
begin
    n := Length(AChanges);
    SetLength(AChanges, n + 1);
    AChanges[n].Name := AName;
    AChanges[n].Value := AValue;
    AChanges[n].Remove := ARemove;
end;

function SnapLeakRepairs(AEnvironment: TStrings;
    const AExecutable: string): TEnvironmentChanges;
var
    Roots, Entries: TStringArray;
    Kept, Name, Value, Entry, RootName, Home: string;
    i: integer;
    Dropped: boolean;
begin
    Result := nil;
    Home := AEnvironment.Values['HOME'];

    Roots := nil;
    for RootName in SnapRootNames do
        if AEnvironment.Values[RootName] <> '' then
        begin
            SetLength(Roots, Length(Roots) + 1);
            Roots[High(Roots)] := AEnvironment.Values[RootName];
        end;
    //  A Fit that is itself a snap: the environment is its own.
    if IsSnapPath(AExecutable, Home, Roots) then
        Exit;

    for i := 0 to AEnvironment.Count - 1 do
    begin
        Name := AEnvironment.Names[i];
        if Name = '' then
            Continue;
        Value := AEnvironment.ValueFromIndex[i];
        if (Name = 'SNAP') or (Copy(Name, 1, 5) = 'SNAP_') then
        begin
            AddChange(Result, Name, '', True);
            Continue;
        end;
        //  Every value is read as a list; one without a colon is a list of one,
        //  and one whose parts lie nowhere near the snap (DISPLAY=:1, a URL)
        //  keeps every part and is not touched.
        Entries := Value.Split([':']);
        Kept := '';
        Dropped := False;
        for Entry in Entries do
            if IsSnapPath(Entry, Home, Roots) then
                Dropped := True
            else if Kept = '' then
                Kept := Entry
            else
                Kept := Kept + ':' + Entry;
        if not Dropped then
            Continue;
        if Kept = '' then
            AddChange(Result, Name, '', True)
        else
            AddChange(Result, Name, Kept, False);
    end;
end;

{$IFDEF LINUX}
{ FPC's copy of the environment, re-pointed at a snapshot of libc's.

  TWO COPIES OF ONE ENVIRONMENT. libc's `environ` is what Qt, KDE and every
  exec'd child read; System.envp is what GetEnvironmentVariable and TProcess
  read. They start as the same array, and setenv/unsetenv on names that already
  exist edit that array in place - but anything that ever ADDED a variable
  before this ran has already given libc an array of its own, and then only one
  side would see the repair.

  A COPY, NOT libc's POINTER: libc reallocates and frees its array whenever a
  variable is added later (Qt does add some), and an envp left pointing at it
  would dangle. The copy lives for the rest of the process - it IS the
  environment - and is taken from libc's allocator, as libc's own is, so the
  leak check sees a C runtime's memory rather than a Pascal object nobody freed.
  Only a copy this unit made is ever freed, when a later repair replaces it. }
procedure ResyncPascalEnvironment;
var
    Count, i: integer;
    Copied, Previous: PPChar;
begin
    Count := 0;
    while environ[Count] <> nil do
        Inc(Count);
    Copied := malloc((Count + 1) * SizeOf(PChar));
    for i := 0 to Count - 1 do
        Copied[i] := strdup(environ[i]);
    Copied[Count] := nil;
    Previous := OwnCopy;
    System.envp := Copied;
    OwnCopy := Copied;
    if Previous <> nil then
    begin
        i := 0;
        while Previous[i] <> nil do
        begin
            free(Previous[i]);
            Inc(i);
        end;
        free(Previous);
    end;
end;
{$ENDIF}

procedure ApplyEnvironmentChanges(const AChanges: TEnvironmentChanges);
{$IFDEF LINUX}
var
    Change: TEnvironmentChange;
{$ENDIF}
begin
{$IFDEF LINUX}
    if Length(AChanges) = 0 then
        Exit;
    for Change in AChanges do
        if Change.Remove then
            unsetenv(PChar(Change.Name))
        else
            setenv(PChar(Change.Name), PChar(Change.Value), 1);
    ResyncPascalEnvironment;
{$ENDIF}
end;

function ProcessEnvironment: TStringList;
{$IFDEF LINUX}
var
    p: PPChar;
{$ELSE}
var
    i: integer;
{$ENDIF}
begin
    Result := TStringList.Create;
{$IFDEF LINUX}
    //  libc's, not the RTL's: it is the one Qt will read, and the one a
    //  variable set after start-up (by a test, or by a library) lands in.
    p := environ;
    while p^ <> nil do
    begin
        Result.Add(string(p^));
        Inc(p);
    end;
{$ELSE}
    for i := 1 to GetEnvironmentVariableCount do
        Result.Add(GetEnvironmentString(i));
{$ENDIF}
end;

function RepairEnvironmentLeakedFromSnap: TEnvironmentChanges;
var
    Env: TStringList;
begin
    Env := ProcessEnvironment;
    try
        Result := SnapLeakRepairs(Env, ParamStr(0));
    finally
        Env.Free;
    end;
    ApplyEnvironmentChanges(Result);
end;

function DescribeEnvironmentChanges(const AChanges: TEnvironmentChanges): string;
var
    Removed, Trimmed: string;
    Change: TEnvironmentChange;
begin
    Result := '';
    if Length(AChanges) = 0 then
        Exit;
    Removed := '';
    Trimmed := '';
    for Change in AChanges do
        if Change.Remove then
            Removed := Removed + ' ' + Change.Name
        else
            Trimmed := Trimmed + ' ' + Change.Name;
    Result := 'environment: started inside a snap''s environment, which is not ' +
        'this program''s - removed' + Removed;
    if Trimmed <> '' then
        Result := Result + '; kept only the desktop''s entries of' + Trimmed;
end;

initialization
    //  HERE, AND THIS UNIT FIRST IN EACH PROGRAM'S USES: the LCL's Interfaces
    //  unit creates the QApplication in its own initialization, and the
    //  platform theme decides what it is from the environment it finds then.
    EnvironmentRepairedAtStartup := RepairEnvironmentLeakedFromSnap;
end.
