// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the desktop launcher decides to start the compute server - asserted
without starting anything.)

WHAT THIS DEFENDS. Fit is two programs, and the client has no engine: a package
that installs both and starts neither gives the user a window that cannot fit
anything. The launcher is the answer on every platform - a wrapper script on
Linux and inside the macOS bundle, fit_launcher.exe on Windows - and every one of
them states the same four things: the port, the loopback health URL, how long to
wait for the server to bind, and that a server already answering is REUSED rather
than started a second time.

Those four are the whole feature, and none of them can be observed once the
launcher has run: a second server started on a taken port fails to bind and the
user sees "the server cannot start" for a server that was running perfectly.
That is why the rules are a unit and this is a test of the unit.

The other half is the switch shape. Fit's own command line accepts only
/PROJECT= and /INFILE= (Desktop/Fit.lpr), so a file handed over by the desktop -
Explorer's "%1", macOS's Open With - arrives as a bare path that the client
silently ignores. The launcher translates, and this is where the translation is
checked.
}
unit testcase_launcher_rules;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, launcher_rules;

type
    TLauncherRulesTest = class(TTestCase)
    private
        FPaths: TStringList;
        FArgs: TStringList;
        { The value following AName in FArgs, or '' when AName is absent. }
        function ValueAfter(const AName: string): string;
        function AnyEndsWith(const ATail: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Where the server is reached.
        procedure TheServerIsOnLoopbackOnly;
        procedure TheHealthRouteSharesTheBase;
        procedure ThePortReachesTheUrl;

        //  Which port, and where it comes from.
        procedure NothingConfiguredMeansTheDefaultPort;
        procedure TheEnvironmentOverridesThePort;
        procedure GarbageInTheEnvironmentIsTheDefaultPort;
        procedure APortOutsideTheRangeIsTheDefaultPort;

        //  What the server is started with.
        procedure ThePortIsPassedAsAnArgument;
        procedure NothingIsPassedTwice;

        //  The decision itself.
        procedure AServerThatAnswersIsReused;
        procedure NothingAnsweringStartsOne;
        procedure TheWaitIsTenSeconds;

        //  Where the two binaries are looked for.
        procedure EveryServerCandidateEndsInTheServerName;
        procedure EveryClientCandidateEndsInTheClientName;
        procedure TheCandidatesCoverTheInstalledAndDevelopmentLayouts;
        procedure TheTwoBinariesAreLookedForBesideTheLauncherFirst;

        //  What a file handed over by the desktop becomes.
        procedure AProjectPathBecomesTheProjectSwitch;
        procedure AnyOtherFileBecomesTheInFileSwitch;
        procedure AProjectExtensionIsRecognisedWhateverItsCase;
        procedure ASwitchIsPassedThroughUntouched;
        procedure AnAbsoluteUnixPathIsAFileAndNotASwitch;
        procedure AnEmptyArgumentStaysEmpty;
        procedure TheSwitchIsNotQuoted;
    end;

implementation

const
    { A port that is not the server's own, so a hard-coded 8787 anywhere in the
      URL construction shows up as a mismatch. }
    SomePort = 9137;

procedure TLauncherRulesTest.SetUp;
begin
    FPaths := TStringList.Create;
    FArgs := TStringList.Create;
end;

procedure TLauncherRulesTest.TearDown;
begin
    FPaths.Free;
    FPaths := nil;
    FArgs.Free;
    FArgs := nil;
end;

function TLauncherRulesTest.ValueAfter(const AName: string): string;
var
    i: integer;
begin
    Result := '';
    i := FArgs.IndexOf(AName);
    if (i >= 0) and (i + 1 < FArgs.Count) then
        Result := FArgs[i + 1];
end;

function TLauncherRulesTest.AnyEndsWith(const ATail: string): boolean;
var
    i: integer;
begin
    Result := False;
    for i := 0 to FPaths.Count - 1 do
        if (Length(FPaths[i]) >= Length(ATail)) and
            (Copy(FPaths[i], Length(FPaths[i]) - Length(ATail) + 1,
            Length(ATail)) = ATail) then
            Exit(True);
end;

{ ---- the address ----------------------------------------------------------- }

procedure TLauncherRulesTest.TheServerIsOnLoopbackOnly;
begin
    //  The launcher starts a server for THIS user on THIS machine. Probing
    //  anything but loopback would let a machine on the network answer for the
    //  health check and leave the local client with no engine.
    AssertEquals('loopback', 'http://127.0.0.1:9137', ServerBaseUrl(SomePort));
end;

procedure TLauncherRulesTest.TheHealthRouteSharesTheBase;
begin
    AssertEquals('health', ServerBaseUrl(SomePort) + '/health',
        ServerHealthUrl(SomePort));
end;

procedure TLauncherRulesTest.ThePortReachesTheUrl;
begin
    //  A URL built from a constant instead of the argument would pass every
    //  other assertion here.
    AssertTrue('the port is not ignored',
        ServerBaseUrl(SomePort) <> ServerBaseUrl(SomePort + 1));
end;

{ ---- the port -------------------------------------------------------------- }

procedure TLauncherRulesTest.NothingConfiguredMeansTheDefaultPort;
begin
    //  The same 8787 the client defaults to (Desktop/http_fit_service.pas) and
    //  the server binds (Worker/fit_server.lpr). Three statements of one number;
    //  this is the one a test can see.
    AssertEquals('default', DefaultLauncherPort, LauncherPort(''));
    AssertEquals('and it is the documented one', 8787, DefaultLauncherPort);
end;

procedure TLauncherRulesTest.TheEnvironmentOverridesThePort;
begin
    //  FIT_PORT, exactly as the shell launchers read it.
    AssertEquals('overridden', 9001, LauncherPort('9001'));
end;

procedure TLauncherRulesTest.GarbageInTheEnvironmentIsTheDefaultPort;
begin
    //  A launcher that refused to start over a typo in an environment variable
    //  would be a program that cannot be started at all, with no window to say
    //  why. The default is the recoverable answer.
    AssertEquals('nonsense', DefaultLauncherPort, LauncherPort('nonsense'));
    AssertEquals('half a number', DefaultLauncherPort, LauncherPort('87a7'));
    AssertEquals('blank', DefaultLauncherPort, LauncherPort('   '));
end;

procedure TLauncherRulesTest.APortOutsideTheRangeIsTheDefaultPort;
begin
    //  0 is "any free port" to the OS, and the client would have nowhere to look;
    //  above 65535 does not exist at all.
    AssertEquals('zero', DefaultLauncherPort, LauncherPort('0'));
    AssertEquals('too big', DefaultLauncherPort, LauncherPort('70000'));
    AssertEquals('negative', DefaultLauncherPort, LauncherPort('-1'));
end;

{ ---- the command line ------------------------------------------------------ }

procedure TLauncherRulesTest.ThePortIsPassedAsAnArgument;
begin
    //  The server takes --port (Worker/fit_server.lpr ParseArgs). Started without
    //  it, it binds its own default - which is the same number today and would
    //  stop being it the moment FIT_PORT is set, leaving the client probing a
    //  port nothing is on.
    BuildServerArgs(FArgs, SomePort);
    AssertEquals('--port', '9137', ValueAfter('--port'));
end;

procedure TLauncherRulesTest.NothingIsPassedTwice;
var
    i, j, Repeats: integer;
begin
    BuildServerArgs(FArgs, SomePort);
    Repeats := 0;
    for i := 0 to FArgs.Count - 1 do
    begin
        if Copy(FArgs[i], 1, 2) <> '--' then
            continue;
        for j := i + 1 to FArgs.Count - 1 do
            if FArgs[j] = FArgs[i] then
                Inc(Repeats);
    end;
    AssertEquals('no option appears twice: ' + FArgs.CommaText, 0, Repeats);
end;

{ ---- the decision ---------------------------------------------------------- }

procedure TLauncherRulesTest.AServerThatAnswersIsReused;
begin
    //  THE RULE THE WHOLE FEATURE TURNS ON. A second launch must not start a
    //  second server: the port is taken, the new one fails to bind, and what the
    //  user is told is that the server could not start - about a server that is
    //  running and fitting. It is also what makes a server the user runs by hand,
    //  or one on another machine reached through the same port, survive a launch.
    AssertTrue('reused', StepBeforeStartingTheClient(True) =
        lsReuseTheRunningServer);
end;

procedure TLauncherRulesTest.NothingAnsweringStartsOne;
begin
    AssertTrue('started', StepBeforeStartingTheClient(False) = lsStartOne);
end;

procedure TLauncherRulesTest.TheWaitIsTenSeconds;
begin
    //  The client probes the server as it starts up, so the launcher has to wait
    //  for the port to be bound rather than race it - otherwise the first window
    //  after an install says no server answered, and it is wrong.
    //
    //  Ten seconds, stated once here and asserted against the shell launchers by
    //  the packaging tests: three copies of this number is how the Windows and
    //  Linux launchers would come to disagree about what "too slow" means.
    AssertEquals('budget', 10000, LauncherWaitBudgetMs);
    AssertEquals('tries', 50, LauncherWaitTries);
    AssertEquals('interval', 200, LauncherWaitIntervalMs);
end;

{ ---- where the binaries are ------------------------------------------------ }

procedure TLauncherRulesTest.EveryServerCandidateEndsInTheServerName;
var
    i: integer;
begin
    ServerBinaryCandidates(FPaths, '/opt/fit/');
    AssertTrue('there are candidates', FPaths.Count > 0);
    for i := 0 to FPaths.Count - 1 do
        AssertEquals('every candidate is the server',
            ServerExeName, ExtractFileName(FPaths[i]));
end;

procedure TLauncherRulesTest.EveryClientCandidateEndsInTheClientName;
var
    i: integer;
begin
    //  NOT one name: the client is Fit in an installed tree and
    //  Fit-<cpu>-<os> in a build, because that is what the project writes
    //  (Desktop/o/$(TargetCPU)-$(TargetOS)/Fit-...). Both start with Fit.
    ClientBinaryCandidates(FPaths, '/opt/fit/');
    AssertTrue('there are candidates', FPaths.Count > 0);
    for i := 0 to FPaths.Count - 1 do
        AssertEquals(FPaths[i] + ' names the client', 'Fit',
            Copy(ExtractFileName(FPaths[i]), 1, 3));
end;

procedure TLauncherRulesTest.TheCandidatesCoverTheInstalledAndDevelopmentLayouts;
begin
    //  Installed, both binaries sit beside the launcher. In the development tree
    //  they are two directories apart - Desktop/o/... and Worker/o - and a
    //  launcher that only knew the installed layout could never be run from a
    //  build, which is the only way it gets tried before a release.
    ServerBinaryCandidates(FPaths, '/src/fit/Worker/o/');
    AssertTrue('beside the launcher', AnyEndsWith('/' + ServerExeName));
    FPaths.Clear;
    ClientBinaryCandidates(FPaths, '/src/fit/Worker/o/');
    AssertTrue('the client is looked for under Desktop as well: ' +
        FPaths.CommaText, Pos('Desktop', FPaths.Text) > 0);
end;

procedure TLauncherRulesTest.TheTwoBinariesAreLookedForBesideTheLauncherFirst;
begin
    //  THE INSTALLED LAYOUT WINS. A development tree left on a machine that also
    //  has Fit installed must not make the installed launcher run the built
    //  binaries - the user would be running a different program than the one
    //  they installed, and nothing would say so.
    ServerBinaryCandidates(FPaths, '/opt/fit/');
    AssertEquals('first', '/opt/fit/' + ServerExeName, FPaths[0]);
    FPaths.Clear;
    ClientBinaryCandidates(FPaths, '/opt/fit/');
    AssertEquals('first', '/opt/fit/' + ClientExeName, FPaths[0]);
end;

{ ---- what a file handed over becomes --------------------------------------- }

procedure TLauncherRulesTest.AProjectPathBecomesTheProjectSwitch;
begin
    //  THE DEFECT THIS EXISTS TO STOP. Fit.lpr accepts an argument only when its
    //  first character is / or \ (CmdLineParamFound), so double-clicking a
    //  project - which hands over a bare path - opens the window with nothing in
    //  it and reports no error anywhere.
    AssertEquals('project', '/PROJECT=C:\a b\p.fitproj',
        SwitchForArgument('C:\a b\p.fitproj'));
end;

procedure TLauncherRulesTest.AnyOtherFileBecomesTheInFileSwitch;
begin
    //  Data files are what the sample set is made of, and /INFILE is how the
    //  client is asked to load one.
    AssertEquals('data', '/INFILE=/home/u/spectrum.dat',
        SwitchForArgument('/home/u/spectrum.dat'));
end;

procedure TLauncherRulesTest.AProjectExtensionIsRecognisedWhateverItsCase;
begin
    //  Windows hands over the name as it is stored, and a project saved as
    //  .FITPROJ is the same file.
    AssertEquals('upper', '/PROJECT=P.FITPROJ', SwitchForArgument('P.FITPROJ'));
end;

procedure TLauncherRulesTest.ASwitchIsPassedThroughUntouched;
begin
    //  Someone running the launcher by hand passes the client's own switches;
    //  wrapping one in another switch would hide it.
    AssertEquals('project switch', '/PROJECT=a.fitproj',
        SwitchForArgument('/PROJECT=a.fitproj'));
    AssertEquals('log level', '/LOG_LEVEL=warning',
        SwitchForArgument('/LOG_LEVEL=warning'));
end;

procedure TLauncherRulesTest.AnAbsoluteUnixPathIsAFileAndNotASwitch;
begin
    //  THE TRAP IN THE OBVIOUS RULE. "starts with a slash" means "is a switch"
    //  on Windows and "is an absolute path" on Unix, so a launcher that used it
    //  would hand /home/u/x.dat to the client untranslated - the exact silence
    //  the translation exists to remove. A switch has a name and an equals sign.
    AssertEquals('a project', '/PROJECT=/home/u/p.fitproj',
        SwitchForArgument('/home/u/p.fitproj'));
    //  And an equals sign somewhere down a path is still a path.
    AssertEquals('an odd directory name', '/INFILE=/home/a=b/x.dat',
        SwitchForArgument('/home/a=b/x.dat'));
end;

procedure TLauncherRulesTest.AnEmptyArgumentStaysEmpty;
begin
    //  An empty ParamStr is possible - a shell expanding an unset variable - and
    //  '/INFILE=' would ask the client to load a file with no name.
    AssertEquals('empty', '', SwitchForArgument(''));
end;

procedure TLauncherRulesTest.TheSwitchIsNotQuoted;
begin
    //  NOT QUOTED HERE, deliberately. This value goes into TProcess.Parameters,
    //  which quotes each argument itself; a quote added here would reach the
    //  client as part of the file name and the file would not be found. The
    //  quoting that IS needed lives in the installer's registry command, where
    //  the desktop substitutes %1 into a command line rather than an array.
    AssertTrue('no quotes: ' + SwitchForArgument('C:\a b\p.fitproj'),
        Pos('"', SwitchForArgument('C:\a b\p.fitproj')) = 0);
end;

initialization
    RegisterTest('unit', TLauncherRulesTest);

end.
