// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A Fit process started from inside some snap's environment gives that
environment back.)

THE DEFECT. File > Open Project froze the window for good when Fit was started
from the integrated terminal of VS Code installed as a snap. The backtrace was
entirely KDE's - TOpenDialog.Execute -> QFileDialog_selectNameFilter ->
KDirOperator::checkPreviewSupport -> KSycoca::ensureCacheValid ->
KBuildSycoca::recreate - because the client had inherited the snap's
XDG_DATA_HOME and XDG_DATA_DIRS, KDE keys its service cache on those, and for the
snap's set it could never be built (kbuildsycoca5: two and a half minutes, then
std::bad_alloc). So it was rebuilt on the GUI thread every time.

Nothing here is about VS Code, and nothing waits for SNAP: the first rule did,
and the terminal the user ran had had SNAP and every SNAP_* taken out while the
snap's directories stayed in everything else. A snap's directories are known by
where snapd puts them, so any snap is recognised, announced or not.
}
unit testcase_snap_environment;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, snap_environment;

type
    TSnapEnvironmentTest = class(TTestCase)
    private
        function Env(const ALines: array of string): TStringList;
        function Find(const AChanges: TEnvironmentChanges; const AName: string): integer;
        procedure AssertRemoved(const AChanges: TEnvironmentChanges; const AName: string);
        procedure AssertUntouched(const AChanges: TEnvironmentChanges; const AName: string);
        function VSCodeTerminal: TStringList;
    published
        procedure NothingChangesOutsideASnap;
        procedure TheDesktopsDataDirectoriesSurviveAndTheSnapsAreDropped;
        procedure AVariableRootedInTheSnapIsRemoved;
        procedure TheSnapsOwnVariablesAreRemoved;
        procedure APathListWithNothingButTheSnapIsRemoved;
        procedure TheRestOfTheSessionIsLeftAlone;
        procedure AnySnapIsRecognisedNotOneEditor;
        procedure ALookAlikeDirectoryIsNotASnaps;
        procedure ATerminalThatHidSnapStillHasItsDirectoriesTakenOut;
        procedure TheHostsOwnSnapDirectoriesStay;
        procedure AFitThatIsItselfASnapKeepsItsEnvironment;
        procedure NothingIsEverAdded;
        procedure TheLogLineNamesWhatWasRemovedAndWhatWasTrimmed;
{$IFDEF LINUX}
        procedure AnAppliedRepairIsSeenByLibcAndByPascal;
        procedure TheStartUpRepairTakesTheSnapOut;
{$ENDIF}
    end;

implementation

{$IFDEF LINUX}
uses
    ctypes;

function setenv(AName, AValue: PChar; AOverwrite: cint): cint; cdecl; external 'c';
function getenv(AName: PChar): PChar; cdecl; external 'c';
{$ENDIF}

const
    VSCodeExe = '/mnt/data/Fit/Desktop/o/x86_64-linux/Fit-x86_64-linux';

function TSnapEnvironmentTest.Env(const ALines: array of string): TStringList;
var
    Line: string;
begin
    Result := TStringList.Create;
    for Line in ALines do
        Result.Add(Line);
end;

function TSnapEnvironmentTest.Find(const AChanges: TEnvironmentChanges;
    const AName: string): integer;
var
    i: integer;
begin
    for i := 0 to High(AChanges) do
        if AChanges[i].Name = AName then
            Exit(i);
    Result := -1;
end;

procedure TSnapEnvironmentTest.AssertRemoved(const AChanges: TEnvironmentChanges;
    const AName: string);
var
    i: integer;
begin
    i := Find(AChanges, AName);
    AssertTrue(AName + ' was left in place', i >= 0);
    AssertTrue(AName + ' was changed, not removed', AChanges[i].Remove);
end;

procedure TSnapEnvironmentTest.AssertUntouched(const AChanges: TEnvironmentChanges;
    const AName: string);
begin
    AssertEquals(AName + ' was touched', -1, Find(AChanges, AName));
end;

{ What the VS Code snap's terminal carried when the dialog froze, one of each
  kind of variable. }
function TSnapEnvironmentTest.VSCodeTerminal: TStringList;
begin
    Result := Env([
        'HOME=/home/u',
        'PATH=/usr/local/bin:/usr/bin:/snap/bin',
        'DISPLAY=:1',
        'XDG_CURRENT_DESKTOP=KDE',
        'SNAP=/snap/code/263',
        'SNAP_NAME=code',
        'SNAP_REAL_HOME=/home/u',
        'SNAP_DATA=/var/snap/code/263',
        'SNAP_COMMON=/var/snap/code/common',
        'SNAP_USER_DATA=/home/u/snap/code/263',
        'SNAP_USER_COMMON=/home/u/snap/code/common',
        'SNAP_LIBRARY_PATH=/var/lib/snapd/lib/gl:/var/lib/snapd/lib/gl32',
        'XDG_DATA_HOME=/home/u/snap/code/263/.local/share',
        'XDG_DATA_DIRS=/home/u/snap/code/263/.local/share/flatpak/exports/share:' +
            '/home/u/snap/code/263/.local/share:/home/u/snap/code/263:' +
            '/snap/code/263/usr/share:/var/lib/snapd/desktop:/usr/share/plasma:' +
            '/usr/local/share/:/usr/share/',
        'XDG_CONFIG_DIRS=/etc/xdg',
        'GTK_PATH=/snap/code/263/usr/lib/x86_64-linux-gnu/gtk-3.0',
        'GIO_MODULE_DIR=/home/u/snap/code/common/.cache/gio-modules',
        'LOCPATH=/snap/code/263/usr/lib/locale',
        'GTK_IM_MODULE=ibus'
    ]);
end;

procedure TSnapEnvironmentTest.NothingChangesOutsideASnap;
var
    E: TStringList;
begin
    E := Env(['HOME=/home/u', 'XDG_DATA_DIRS=/usr/share',
        'XDG_DATA_HOME=/home/u/.local/share', 'PATH=/usr/bin:/snap/bin']);
    try
        AssertEquals(0, Length(SnapLeakRepairs(E, VSCodeExe)));
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.TheDesktopsDataDirectoriesSurviveAndTheSnapsAreDropped;
var
    E: TStringList;
    C: TEnvironmentChanges;
    i: integer;
begin
    //  THE ONE THAT FROZE THE DIALOG: KDE keys its service cache on these. The
    //  desktop's entries survive IN THEIR ORDER - it is a precedence list.
    E := VSCodeTerminal;
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        i := Find(C, 'XDG_DATA_DIRS');
        AssertTrue('XDG_DATA_DIRS was not repaired', i >= 0);
        AssertFalse(C[i].Remove);
        AssertEquals('/var/lib/snapd/desktop:/usr/share/plasma:/usr/local/share/:/usr/share/',
            C[i].Value);
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.AVariableRootedInTheSnapIsRemoved;
var
    E: TStringList;
    C: TEnvironmentChanges;
begin
    //  Set by the snap from nothing, so the desktop's value is "unset" - which
    //  for XDG_DATA_HOME means ~/.local/share, exactly what a user expects.
    E := VSCodeTerminal;
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        AssertRemoved(C, 'XDG_DATA_HOME');
        AssertRemoved(C, 'GTK_PATH');
        AssertRemoved(C, 'GIO_MODULE_DIR');
        AssertRemoved(C, 'LOCPATH');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.TheSnapsOwnVariablesAreRemoved;
var
    E: TStringList;
    C: TEnvironmentChanges;
begin
    //  Qt reads SNAP as "running sandboxed" and routes file dialogs through the
    //  desktop portal; a process that is not in the snap must not claim to be.
    E := VSCodeTerminal;
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        AssertRemoved(C, 'SNAP');
        AssertRemoved(C, 'SNAP_NAME');
        AssertRemoved(C, 'SNAP_USER_DATA');
        AssertRemoved(C, 'SNAP_REAL_HOME');
        AssertRemoved(C, 'SNAP_LIBRARY_PATH');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.APathListWithNothingButTheSnapIsRemoved;
var
    E: TStringList;
    C: TEnvironmentChanges;
begin
    //  Emptied, a list is not "no directories" but "the default ones", which is
    //  what removing it says; an empty value would say something else.
    E := Env(['SNAP=/snap/code/263', 'SNAP_USER_DATA=/home/u/snap/code/263',
        'GTK_EXE_PATHS=/snap/code/263/usr:/home/u/snap/code/263/bin']);
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        AssertRemoved(C, 'GTK_EXE_PATHS');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.TheRestOfTheSessionIsLeftAlone;
var
    E: TStringList;
    C: TEnvironmentChanges;
begin
    //  HOME equals SNAP_REAL_HOME: the real home is the user's, not the snap's,
    //  and must never be taken for one of its directories. DISPLAY carries a
    //  colon and is no path list. /snap/bin in PATH is the host's.
    E := VSCodeTerminal;
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        AssertUntouched(C, 'HOME');
        AssertUntouched(C, 'PATH');
        AssertUntouched(C, 'DISPLAY');
        AssertUntouched(C, 'XDG_CURRENT_DESKTOP');
        AssertUntouched(C, 'XDG_CONFIG_DIRS');
        AssertUntouched(C, 'GTK_IM_MODULE');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.AnySnapIsRecognisedNotOneEditor;
var
    E: TStringList;
    C: TEnvironmentChanges;
    i: integer;
begin
    //  No editor's private bookkeeping (VS Code's *_VSCODE_SNAP_ORIG) is relied
    //  on: the rule is stated over what every snap exports.
    E := Env(['SNAP=/snap/konsole/77', 'SNAP_USER_DATA=/home/u/snap/konsole/77',
        'XDG_DATA_HOME=/home/u/snap/konsole/77/.local/share',
        'XDG_DATA_DIRS=/snap/konsole/77/usr/share:/usr/share']);
    try
        C := SnapLeakRepairs(E, '/usr/bin/fit');
        AssertRemoved(C, 'XDG_DATA_HOME');
        i := Find(C, 'XDG_DATA_DIRS');
        AssertTrue(i >= 0);
        AssertEquals('/usr/share', C[i].Value);
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.ALookAlikeDirectoryIsNotASnaps;
var
    E: TStringList;
begin
    //  A prefix is not a parent: /snapshots and ~/snapshots hold nobody's snap.
    E := Env(['HOME=/home/u', 'SNAP=/snap/code/263',
        'FIT_DATA=/snapshots/data', 'FIT_MORE=/home/u/snapshots/x']);
    try
        AssertUntouched(SnapLeakRepairs(E, VSCodeExe), 'FIT_DATA');
        AssertUntouched(SnapLeakRepairs(E, VSCodeExe), 'FIT_MORE');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.ATerminalThatHidSnapStillHasItsDirectoriesTakenOut;
var
    E: TStringList;
    C: TEnvironmentChanges;
    i: integer;
begin
    //  THE SECOND REPORT, and the environment is the hung client's own, read
    //  from /proc. VS Code's terminal had REMOVED SNAP and every SNAP_* - and
    //  left the snap's directories in everything else. The first rule waited
    //  for SNAP, so it did nothing here; it had only ever been tried from a
    //  shell that still carried it. A snap's directories are recognised by
    //  where snapd puts them, not by a variable that says so.
    E := Env([
        'HOME=/home/u',
        'PATH=/opt/microsoft/powershell/7:/usr/local/bin:/usr/bin:/snap/bin',
        'XDG_DATA_HOME=/home/u/snap/code/263/.local/share',
        'XDG_DATA_HOME_VSCODE_SNAP_ORIG=',
        'XDG_DATA_DIRS=/home/u/snap/code/263/.local/share/flatpak/exports/share:' +
            '/home/u/snap/code/263/.local/share:/home/u/snap/code/263:' +
            '/snap/code/263/usr/share:/var/lib/snapd/desktop:/usr/share/plasma:' +
            '/usr/share/gnome:/home/u/.local/share/flatpak/exports/share:' +
            '/var/lib/flatpak/exports/share:/usr/local/share/:/usr/share/:' +
            '/var/lib/snapd/desktop',
        'GTK_PATH=/snap/code/263/usr/lib/x86_64-linux-gnu/gtk-3.0',
        'GTK_EXE_PREFIX=/snap/code/263/usr',
        'GTK_IM_MODULE_FILE=/home/u/snap/code/common/.cache/immodules/immodules.cache',
        'GIO_MODULE_DIR=/home/u/snap/code/common/.cache/gio-modules',
        'GSETTINGS_SCHEMA_DIR=/home/u/snap/code/263/.local/share/glib-2.0/schemas',
        'LOCPATH=/snap/code/263/usr/lib/locale',
        'GTK_RC_FILES=/etc/gtk/gtkrc:/home/u/.gtkrc:/home/u/.config/gtkrc',
        'TERM_PROGRAM=vscode'
    ]);
    try
        C := SnapLeakRepairs(E, '/mnt/data/fit-pro/Desktop/o/x86_64-linux/Fit-x86_64-linux');
        i := Find(C, 'XDG_DATA_DIRS');
        AssertTrue('XDG_DATA_DIRS was not repaired', i >= 0);
        AssertEquals('/var/lib/snapd/desktop:/usr/share/plasma:/usr/share/gnome:' +
            '/home/u/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:' +
            '/usr/local/share/:/usr/share/:/var/lib/snapd/desktop', C[i].Value);
        AssertRemoved(C, 'XDG_DATA_HOME');
        AssertRemoved(C, 'GTK_PATH');
        AssertRemoved(C, 'GTK_EXE_PREFIX');
        AssertRemoved(C, 'GTK_IM_MODULE_FILE');
        AssertRemoved(C, 'GIO_MODULE_DIR');
        AssertRemoved(C, 'GSETTINGS_SCHEMA_DIR');
        AssertRemoved(C, 'LOCPATH');
        AssertUntouched(C, 'PATH');
        AssertUntouched(C, 'HOME');
        AssertUntouched(C, 'GTK_RC_FILES');
        AssertUntouched(C, 'TERM_PROGRAM');
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.TheHostsOwnSnapDirectoriesStay;
var
    E: TStringList;
begin
    //  /snap/bin holds the host's launchers for installed snaps, and
    //  /var/lib/snapd/desktop the host's menu entries for them: the DESKTOP's,
    //  belonging to no one snap - so a snap-free desktop session has them too.
    E := Env(['HOME=/home/u', 'PATH=/usr/bin:/snap/bin',
        'XDG_DATA_DIRS=/usr/share:/var/lib/snapd/desktop',
        'OTHER_BIN=/var/lib/snapd/snap/bin']);
    try
        AssertEquals(0, Length(SnapLeakRepairs(E, VSCodeExe)));
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.AFitThatIsItselfASnapKeepsItsEnvironment;
var
    E: TStringList;
begin
    //  Then the environment is its own, not leaked, and taking it away would
    //  break the package - whichever snap it is.
    E := VSCodeTerminal;
    try
        AssertEquals(0, Length(SnapLeakRepairs(E, '/snap/code/263/usr/bin/fit')));
        AssertEquals(0, Length(SnapLeakRepairs(E, '/snap/fit/12/bin/Fit')));
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.NothingIsEverAdded;
var
    E: TStringList;
    C: TEnvironmentChanges;
    i: integer;
begin
    //  An INVARIANT ApplyEnvironmentChanges leans on: only names already in
    //  the environment are rewritten or removed.
    E := VSCodeTerminal;
    try
        C := SnapLeakRepairs(E, VSCodeExe);
        AssertTrue(Length(C) > 0);
        for i := 0 to High(C) do
            AssertTrue(C[i].Name + ' was not in the environment',
                E.IndexOfName(C[i].Name) >= 0);
    finally
        E.Free;
    end;
end;

procedure TSnapEnvironmentTest.TheLogLineNamesWhatWasRemovedAndWhatWasTrimmed;
var
    C: TEnvironmentChanges;
    S: string;
begin
    SetLength(C, 2);
    C[0].Name := 'SNAP';
    C[0].Remove := True;
    C[1].Name := 'XDG_DATA_DIRS';
    C[1].Value := '/usr/share';
    S := DescribeEnvironmentChanges(C);
    AssertTrue(S, Pos('SNAP', S) > 0);
    AssertTrue(S, Pos('XDG_DATA_DIRS', S) > 0);
    AssertEquals('', DescribeEnvironmentChanges(nil));
end;

{$IFDEF LINUX}
procedure TSnapEnvironmentTest.AnAppliedRepairIsSeenByLibcAndByPascal;
var
    C: TEnvironmentChanges;
begin
    //  TWO COPIES OF ONE ENVIRONMENT. Qt and KDE read libc's; FPC's
    //  GetEnvironmentVariable and TProcess read System.envp. A repair only one
    //  of them sees leaves the server looking for Python under the snap's
    //  XDG_DATA_HOME while the window is fine - or the reverse.
    setenv('FIT_SNAPTEST_KEEP', '/snap/x/1/share:/usr/share', 1);
    setenv('FIT_SNAPTEST_GONE', '/snap/x/1', 1);
    SetLength(C, 2);
    C[0].Name := 'FIT_SNAPTEST_KEEP';
    C[0].Value := '/usr/share';
    C[1].Name := 'FIT_SNAPTEST_GONE';
    C[1].Remove := True;
    ApplyEnvironmentChanges(C);
    try
        AssertEquals('libc', '/usr/share', string(getenv('FIT_SNAPTEST_KEEP')));
        AssertTrue('libc', getenv('FIT_SNAPTEST_GONE') = nil);
        AssertEquals('Pascal', '/usr/share', GetEnvironmentVariable('FIT_SNAPTEST_KEEP'));
        AssertEquals('Pascal', '', GetEnvironmentVariable('FIT_SNAPTEST_GONE'));
    finally
        C[0].Remove := True;
        ApplyEnvironmentChanges(C);
    end;
end;

procedure TSnapEnvironmentTest.TheStartUpRepairTakesTheSnapOut;
var
    C: TEnvironmentChanges;
begin
    //  THROUGH THE ROUTINE THE PROGRAMS' INITIALIZATION CALLS, reading the
    //  process's own environment - not a list handed to the pure function.
    setenv('SNAP', '/snap/fittest/1', 1);
    setenv('FIT_SNAPTEST_DIR', '/snap/fittest/1/lib', 1);
    C := RepairEnvironmentLeakedFromSnap;
    AssertTrue('nothing was repaired', Length(C) >= 2);
    AssertTrue('libc still has SNAP', getenv('SNAP') = nil);
    AssertTrue('libc still has the snap directory', getenv('FIT_SNAPTEST_DIR') = nil);
    AssertEquals('Pascal still has SNAP', '', GetEnvironmentVariable('SNAP'));
    AssertEquals('', GetEnvironmentVariable('FIT_SNAPTEST_DIR'));
end;
{$ENDIF}

initialization
    RegisterTest('unit', TSnapEnvironmentTest);
end.
