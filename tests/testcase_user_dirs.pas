// SPDX-License-Identifier: GPL-3.0-or-later
{ Where Fit keeps a user's settings and logs, and what it does with the folder
  earlier versions kept them in.

  The defect these defend against: every Fit process - the window, the compute
  server, the Python sidecar's log, and every test run - wrote into a folder
  called Fit in the HOME folder itself on Linux and macOS. A developer's home
  filled with 100 MB of logs from building and testing, and an installed copy
  put a folder there that no platform's conventions ask for. }
unit testcase_user_dirs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Process, fpcunit, testregistry, app_data_root;

type
    { The rules, over an environment given as a record: no file is touched and
      no variable of this process is read. }
    TUserDirsTest = class(TTestCase)
    private
        { A user with a home folder and nothing else set. }
        function PlainUser: TUserDirsEnvironment;
    published
        procedure TheSettingsAreInThePlatformsSettingsFolder;
        procedure TheLogsAreInThePlatformsLogFolder;
        procedure TheLogsAreNeverAmongTheSettings;
        procedure NothingIsKeptInTheHomeFolderItself;
        procedure AnXdgVariableMovesTheFolderItNames;
        procedure WithNoHomeThereIsNoFolder;
        procedure AProfileFolderDoesNotHoldTheCurveTypes;
        procedure AProfileFolderKeepsTheLogsInAFolderOfTheirOwn;
        procedure AProfileFolderHoldsTheDataToo;
        procedure TheOldFolderIsTheOneEarlierVersionsUsed;
        procedure WithAProfileFolderNothingIsAdopted;
        procedure TheSettingsAndCurveTypesBelongInTheSettingsFolder;
        procedure EveryLogAndEveryCopyOfOneBelongsInTheLogFolder;
        procedure AFileThatIsNotOursStaysWhereItIs;
        procedure SettingsAlreadyInTheirFolderStay;
        procedure WithNoFolderToGoToAFileStays;
        //  What is true of one machine only: the projects opened on it.
        procedure WhatBelongsToThisMachineIsInItsOwnPlatformFolder;
        procedure OnWindowsItIsNotTheRoamingFolderTheSettingsAreIn;
        procedure AProfileFolderDoesNotHoldWhatBelongsToTheMachine;
        procedure AnXdgStateVariableMovesWhatBelongsToTheMachine;
        procedure WithNoHomeThereIsNoMachineFolder;
    end;

    { Moving what an earlier version left in its folder: real files, in a
      temporary directory. }
    TLegacyUserDirTest = class(TTestCase)
    private
        FRoot, FLegacy, FConfig, FLogs: string;
        procedure Touch(const APath, AText: string);
        function ReadText(const APath: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheSettingsAndCurveTypesMoveToTheSettingsFolder;
        procedure TheLogsMoveToTheLogFolder;
        procedure TheOldFolderGoesOnceItIsEmpty;
        procedure AFileThatIsNotOursKeepsTheOldFolder;
        procedure AFileAlreadyInPlaceIsNotOverwritten;
        procedure WhereTheSettingsFolderIsTheOldOneOnlyTheLogsMove;
    end;

    { The surface the build reaches: a compute server started with
      FIT_PROFILE_DIR writes its log inside that folder. }
    TProfileDirProcessTest = class(TTestCase)
    private
        FRoot: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AServerStartedWithAProfileFolderLogsInsideIt;
    end;

implementation

uses
    worker_process_harness;

function EmptyEnvironment: TUserDirsEnvironment;
begin
    Result := Default(TUserDirsEnvironment);
end;

function TUserDirsTest.PlainUser: TUserDirsEnvironment;
begin
    Result := EmptyEnvironment;
{$IFDEF WINDOWS}
    Result.Home := 'C:\Users\u';
    Result.AppData := 'C:\Users\u\AppData\Roaming';
    Result.LocalAppData := 'C:\Users\u\AppData\Local';
{$ELSE}
    Result.Home := '/home/u';
{$ENDIF}
end;

procedure TUserDirsTest.TheSettingsAreInThePlatformsSettingsFolder;
begin
    //  Each platform names one place for a program's settings, and a user who
    //  goes looking for them looks there.
{$IFDEF WINDOWS}
    //  The ROAMING profile: settings are what should follow an account.
    AssertEquals('C:\Users\u\AppData\Roaming\Fit', AppConfigDirIn(PlainUser));
{$ELSE}
{$IFDEF DARWIN}
    AssertEquals('/home/u/Library/Application Support/Fit', AppConfigDirIn(PlainUser));
{$ELSE}
    AssertEquals('/home/u/.config/fit', AppConfigDirIn(PlainUser));
{$ENDIF}
{$ENDIF}
end;

procedure TUserDirsTest.TheLogsAreInThePlatformsLogFolder;
begin
{$IFDEF WINDOWS}
    //  LOCAL, not roaming: logs are large and say something about one machine.
    AssertEquals('C:\Users\u\AppData\Local\Fit\Logs', AppLogDirIn(PlainUser));
{$ELSE}
{$IFDEF DARWIN}
    //  Where Console lists every application's logs.
    AssertEquals('/home/u/Library/Logs/Fit', AppLogDirIn(PlainUser));
{$ELSE}
    //  The XDG state directory, which the specification names for logs.
    AssertEquals('/home/u/.local/state/fit', AppLogDirIn(PlainUser));
{$ENDIF}
{$ENDIF}
end;

procedure TUserDirsTest.TheLogsAreNeverAmongTheSettings;
var
    E: TUserDirsEnvironment;
begin
    //  A clean removes the logs and keeps the settings; a bug report attaches
    //  the logs and not the settings. Neither works if they share a folder.
    E := PlainUser;
    AssertTrue('a plain user', AppLogDirIn(E) <> AppConfigDirIn(E));
    E.Profile := '/repo/var/profile';
    AssertTrue('a profile folder', AppLogDirIn(E) <> AppConfigDirIn(E));
end;

procedure TUserDirsTest.NothingIsKeptInTheHomeFolderItself;
var
    E: TUserDirsEnvironment;
    Dirs: array[0..2] of string;
    Dir: string;
begin
    E := PlainUser;
    Dirs[0] := AppConfigDirIn(E);
    Dirs[1] := AppLogDirIn(E);
    Dirs[2] := AppDataRootIn(E);
    for Dir in Dirs do
    begin
        AssertFalse('not the home folder: ' + Dir,
            ExcludeTrailingPathDelimiter(Dir) = E.Home);
{$IFDEF WINDOWS}
        //  Except the settings: earlier versions already kept them where
        //  Windows says to, APPDATA\Fit, so on Windows that folder IS the
        //  settings folder and only the logs moved out of it
        //  (LegacyFileDestination, TheSettingsAreInThePlatformsSettingsFolder).
        if Dir = Dirs[0] then
            Continue;
{$ENDIF}
        AssertFalse('not the folder earlier versions put there: ' + Dir,
            ExcludeTrailingPathDelimiter(Dir) = LegacyUserDirIn(E));
    end;
end;

procedure TUserDirsTest.AnXdgVariableMovesTheFolderItNames;
var
    E: TUserDirsEnvironment;
begin
    E := PlainUser;
    E.XdgConfigHome := '/cfg';
    E.XdgStateHome := '/state';
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
    //  Not this platform's convention, so not this program's business.
    AssertEquals(AppConfigDirIn(PlainUser), AppConfigDirIn(E));
    AssertEquals(AppLogDirIn(PlainUser), AppLogDirIn(E));
{$ELSE}
    AssertEquals('/cfg/fit', AppConfigDirIn(E));
    AssertEquals('/state/fit', AppLogDirIn(E));
{$ENDIF}
end;

procedure TUserDirsTest.WithNoHomeThereIsNoFolder;
begin
    //  '' rather than a relative name: that resolves against whatever folder
    //  the program was started in, which is how a log.txt appeared one level
    //  above a checkout.
    AssertEquals('settings', '', AppConfigDirIn(EmptyEnvironment));
    AssertEquals('logs', '', AppLogDirIn(EmptyEnvironment));
    AssertEquals('the old folder', '', LegacyUserDirIn(EmptyEnvironment));
end;

procedure TUserDirsTest.AProfileFolderDoesNotHoldTheCurveTypes;
var
    E: TUserDirsEnvironment;
begin
    //  AN INSTALLED COPY AND ONE STARTED FROM A CHECKOUT USE THE SAME THINGS.
    //  The profile folder is for what a build's runs produce - logs, downloaded
    //  data - and the curve types a user defined are the user's, wherever Fit
    //  was started from. They were in var/profile under a build, so the two
    //  copies on one machine each had a library of their own.
    E := PlainUser;
    E.Profile := '/repo/var/profile';
    AssertEquals(AppConfigDirIn(PlainUser), AppConfigDirIn(E));
end;

procedure TUserDirsTest.AProfileFolderKeepsTheLogsInAFolderOfTheirOwn;
var
    E: TUserDirsEnvironment;
begin
    E := PlainUser;
    E.Profile := '/repo/var/profile';
    AssertEquals('/repo/var/profile' + PathDelim + 'logs', AppLogDirIn(E));
end;

procedure TUserDirsTest.AProfileFolderHoldsTheDataToo;
var
    E: TUserDirsEnvironment;
begin
    //  The download cache: a test or a run from the build that fetched a file
    //  would otherwise leave it in the user's data folder.
    E := PlainUser;
    E.Profile := '/repo/var/profile';
    AssertEquals('/repo/var/profile' + PathDelim + 'data', AppDataRootIn(E));
end;

procedure TUserDirsTest.TheOldFolderIsTheOneEarlierVersionsUsed;
begin
{$IFDEF WINDOWS}
    AssertEquals('C:\Users\u\AppData\Roaming\Fit', LegacyUserDirIn(PlainUser));
{$ELSE}
    AssertEquals('/home/u/Fit', LegacyUserDirIn(PlainUser));
{$ENDIF}
end;

procedure TUserDirsTest.WithAProfileFolderNothingIsAdopted;
var
    E: TUserDirsEnvironment;
begin
    //  A test run must not move an installed copy's settings into a checkout.
    E := PlainUser;
    E.Profile := '/repo/var/profile';
    AssertEquals('', LegacyUserDirIn(E));
end;

{ THE DEFECT: the recent list was kept with the settings, and a build keeps
  its settings in the checkout (FIT_PROFILE_DIR). A checkout copied from a
  Linux machine to a Mac brought its File > Open Recent along - every line a
  /mnt/data path nothing on the Mac has. And on Windows the settings ROAM,
  which carries the same list to every machine the account signs in to. }
procedure TUserDirsTest.WhatBelongsToThisMachineIsInItsOwnPlatformFolder;
begin
{$IFDEF WINDOWS}
    AssertEquals('C:\Users\u\AppData\Local\Fit', AppMachineStateDirIn(PlainUser));
{$ELSE}
{$IFDEF DARWIN}
    AssertEquals('/home/u/Library/Application Support/Fit',
        AppMachineStateDirIn(PlainUser));
{$ELSE}
    //  XDG names its state directory for exactly this: "recently used files",
    //  data that should persist but is not portable between machines.
    AssertEquals('/home/u/.local/state/fit', AppMachineStateDirIn(PlainUser));
{$ENDIF}
{$ENDIF}
end;

procedure TUserDirsTest.OnWindowsItIsNotTheRoamingFolderTheSettingsAreIn;
begin
{$IFDEF WINDOWS}
    AssertFalse('the roaming profile carries a folder to every machine',
        AppMachineStateDirIn(PlainUser) = AppConfigDirIn(PlainUser));
{$ELSE}
    //  Neither Library/Application Support nor ~/.local/state is synchronised
    //  by the platform, so sharing a folder with the settings costs nothing.
    AssertTrue(AppMachineStateDirIn(PlainUser) <> '');
{$ENDIF}
end;

procedure TUserDirsTest.AProfileFolderDoesNotHoldWhatBelongsToTheMachine;
var
    E: TUserDirsEnvironment;
begin
    //  THE PROFILE FOLDER IS IN A CHECKOUT, and a checkout is copied, synced
    //  and shared between machines. What names this machine's files may not
    //  go where another machine will read it.
    E := PlainUser;
    E.Profile := '/repo/var/profile';
    AssertEquals(AppMachineStateDirIn(PlainUser), AppMachineStateDirIn(E));
end;

procedure TUserDirsTest.AnXdgStateVariableMovesWhatBelongsToTheMachine;
var
    E: TUserDirsEnvironment;
begin
    E := PlainUser;
    E.XdgStateHome := '/state';
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
    AssertEquals(AppMachineStateDirIn(PlainUser), AppMachineStateDirIn(E));
{$ELSE}
    AssertEquals('/state/fit', AppMachineStateDirIn(E));
{$ENDIF}
end;

procedure TUserDirsTest.WithNoHomeThereIsNoMachineFolder;
begin
    AssertEquals('', AppMachineStateDirIn(EmptyEnvironment));
end;

procedure TUserDirsTest.TheSettingsAndCurveTypesBelongInTheSettingsFolder;
begin
    AssertEquals('/cfg', LegacyFileDestination('config.xml', '/old', '/cfg', '/logs'));
    AssertEquals('/cfg', LegacyFileDestination('module-preferences.txt', '/old', '/cfg', '/logs'));
    AssertEquals('a curve type the user defined', '/cfg',
        LegacyFileDestination('63918950573061.cpr', '/old', '/cfg', '/logs'));
end;

procedure TUserDirsTest.EveryLogAndEveryCopyOfOneBelongsInTheLogFolder;
const
    LogNames: array[0..7] of string = ('log.txt', 'log.txt.1', 'fit_client.log',
        'fit_client.log.before-check-ui', 'fit_client.log.before-capture',
        'fit_server_log.txt', 'fit_server_log.txt.1', 'fit_sidecar_log.txt');
var
    Name: string;
begin
    for Name in LogNames do
        AssertEquals(Name, '/logs', LegacyFileDestination(Name, '/old', '/cfg', '/logs'));
end;

procedure TUserDirsTest.AFileThatIsNotOursStaysWhereItIs;
begin
    //  The build kept vm.json there; anything else is somebody's own.
    AssertEquals('', LegacyFileDestination('vm.json', '/old', '/cfg', '/logs'));
    AssertEquals('', LegacyFileDestination('notes.txt', '/old', '/cfg', '/logs'));
end;

procedure TUserDirsTest.SettingsAlreadyInTheirFolderStay;
begin
    //  Windows: the old folder IS the settings folder, and only the logs move.
    AssertEquals('', LegacyFileDestination('config.xml', '/old', '/old/', '/logs'));
    AssertEquals('/logs', LegacyFileDestination('log.txt', '/old', '/old/', '/logs'));
end;

procedure TUserDirsTest.WithNoFolderToGoToAFileStays;
begin
    AssertEquals('', LegacyFileDestination('config.xml', '/old', '', '/logs'));
    AssertEquals('', LegacyFileDestination('log.txt', '/old', '/cfg', ''));
end;

{ TLegacyUserDirTest }

procedure TLegacyUserDirTest.SetUp;
begin
    FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fit-user-dirs-' +
        IntToStr(GetProcessID);
    FLegacy := FRoot + PathDelim + 'Fit';
    FConfig := FRoot + PathDelim + 'config';
    FLogs := FRoot + PathDelim + 'state';
    ForceDirectories(FLegacy);
end;

procedure DeleteTree(const ADir: string);
var
    S: TSearchRec;
begin
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, S) = 0 then
    try
        repeat
            if (S.Name = '.') or (S.Name = '..') then
                Continue;
            if (S.Attr and faDirectory) <> 0 then
                DeleteTree(IncludeTrailingPathDelimiter(ADir) + S.Name)
            else
                DeleteFile(IncludeTrailingPathDelimiter(ADir) + S.Name);
        until FindNext(S) <> 0;
    finally
        FindClose(S);
    end;
    RemoveDir(ADir);
end;

procedure TLegacyUserDirTest.TearDown;
begin
    DeleteTree(FRoot);
end;

procedure TLegacyUserDirTest.Touch(const APath, AText: string);
var
    L: TStringList;
begin
    L := TStringList.Create;
    try
        L.Text := AText;
        L.SaveToFile(APath);
    finally
        L.Free;
    end;
end;

function TLegacyUserDirTest.ReadText(const APath: string): string;
var
    L: TStringList;
begin
    L := TStringList.Create;
    try
        L.LoadFromFile(APath);
        Result := Trim(L.Text);
    finally
        L.Free;
    end;
end;

procedure TLegacyUserDirTest.TheSettingsAndCurveTypesMoveToTheSettingsFolder;
begin
    //  Moved, not copied and not started afresh: a user-defined curve type is
    //  the user's own work, and an upgrade that forgot it would lose it.
    Touch(FLegacy + PathDelim + 'config.xml', 'settings');
    Touch(FLegacy + PathDelim + 'module-preferences.txt', 'preferences');
    Touch(FLegacy + PathDelim + '63918950573061.cpr', 'curve');
    MoveLegacyUserFiles(FLegacy, FConfig, FLogs);
    AssertEquals('settings', 'settings', ReadText(FConfig + PathDelim + 'config.xml'));
    AssertEquals('preferences', 'preferences',
        ReadText(FConfig + PathDelim + 'module-preferences.txt'));
    AssertEquals('curve type', 'curve', ReadText(FConfig + PathDelim + '63918950573061.cpr'));
    AssertFalse('nothing left behind', FileExists(FLegacy + PathDelim + 'config.xml'));
end;

procedure TLegacyUserDirTest.TheLogsMoveToTheLogFolder;
const
    LogNames: array[0..6] of string = ('log.txt', 'log.txt.1', 'fit_client.log',
        'fit_client.log.before-check-ui', 'fit_server_log.txt',
        'fit_server_log.txt.1', 'fit_sidecar_log.txt');
var
    Name: string;
begin
    //  Every generation and every copy set aside, of every process's log.
    for Name in LogNames do
        Touch(FLegacy + PathDelim + Name, Name);
    MoveLegacyUserFiles(FLegacy, FConfig, FLogs);
    for Name in LogNames do
    begin
        AssertEquals(Name, Name, ReadText(FLogs + PathDelim + Name));
        AssertFalse(Name + ' is not among the settings',
            FileExists(FConfig + PathDelim + Name));
    end;
end;

procedure TLegacyUserDirTest.TheOldFolderGoesOnceItIsEmpty;
begin
    //  The point of the move: no folder left in the home folder.
    Touch(FLegacy + PathDelim + 'config.xml', 'settings');
    Touch(FLegacy + PathDelim + 'log.txt', 'log');
    MoveLegacyUserFiles(FLegacy, FConfig, FLogs);
    AssertFalse(DirectoryExists(FLegacy));
end;

procedure TLegacyUserDirTest.AFileThatIsNotOursKeepsTheOldFolder;
begin
    //  Something else put it there, and it is not this program's to move or
    //  to delete.
    Touch(FLegacy + PathDelim + 'notes.txt', 'mine');
    Touch(FLegacy + PathDelim + 'config.xml', 'settings');
    MoveLegacyUserFiles(FLegacy, FConfig, FLogs);
    AssertEquals('mine', ReadText(FLegacy + PathDelim + 'notes.txt'));
    AssertTrue('the settings moved all the same',
        FileExists(FConfig + PathDelim + 'config.xml'));
end;

procedure TLegacyUserDirTest.AFileAlreadyInPlaceIsNotOverwritten;
begin
    //  Settings already in the new folder are newer than the old ones: a
    //  version that has run there wrote them.
    ForceDirectories(FConfig);
    Touch(FConfig + PathDelim + 'config.xml', 'newer');
    Touch(FLegacy + PathDelim + 'config.xml', 'older');
    MoveLegacyUserFiles(FLegacy, FConfig, FLogs);
    AssertEquals('newer', ReadText(FConfig + PathDelim + 'config.xml'));
    AssertEquals('the old one is left where it was', 'older',
        ReadText(FLegacy + PathDelim + 'config.xml'));
end;

procedure TLegacyUserDirTest.WhereTheSettingsFolderIsTheOldOneOnlyTheLogsMove;
begin
    //  Windows: the settings were already in the roaming profile, and only the
    //  logs go - to the local one.
    Touch(FLegacy + PathDelim + 'config.xml', 'settings');
    Touch(FLegacy + PathDelim + 'fit_client.log', 'log');
    MoveLegacyUserFiles(FLegacy, FLegacy, FLogs);
    AssertEquals('settings', ReadText(FLegacy + PathDelim + 'config.xml'));
    AssertEquals('log', ReadText(FLogs + PathDelim + 'fit_client.log'));
end;

{ TProfileDirProcessTest }

const
    ProfileTestPort = 8812;

procedure TProfileDirProcessTest.SetUp;
begin
    FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fit-profile-' +
        IntToStr(GetProcessID);
    ForceDirectories(FRoot);
end;

procedure TProfileDirProcessTest.TearDown;
begin
    DeleteTree(FRoot);
end;

procedure TProfileDirProcessTest.AServerStartedWithAProfileFolderLogsInsideIt;
var
    P: TProcess;
    i: integer;
    Log: string;
begin
    AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));
    KillStaleWorker(ProfileTestPort);
    Log := FRoot + PathDelim + 'logs' + PathDelim + 'fit_server_log.txt';
    P := TProcess.Create(nil);
    try
        P.Executable := WorkerServerPath;
        P.Parameters.Add('--port');
        P.Parameters.Add(IntToStr(ProfileTestPort));
        //  The whole environment, with one variable changed: setting
        //  Environment replaces it, and a server without HOME or PATH is not
        //  the one the build starts.
        for i := 1 to GetEnvironmentVariableCount do
            if Pos('FIT_PROFILE_DIR=', GetEnvironmentString(i)) <> 1 then
                P.Environment.Add(GetEnvironmentString(i));
        P.Environment.Add('FIT_PROFILE_DIR=' + FRoot);
        P.Options := [poNoConsole];
        P.Execute;
        try
            //  The banner is written before the server listens.
            for i := 1 to 150 do
            begin
                if FileExists(Log) then
                    Break;
                Sleep(100);
            end;
        finally
            P.Terminate(0);
            P.WaitOnExit;
        end;
    finally
        P.Free;
    end;
    AssertTrue('the server logged into ' + Log, FileExists(Log));
end;

initialization
    RegisterTest('unit', TUserDirsTest);
    RegisterTest('integration', TLegacyUserDirTest);
    RegisterTest('integration', TProfileDirProcessTest);
end.
