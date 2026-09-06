// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the Python sidecar is addressed and started - asserted without
starting it.)

WHAT THIS DEFENDS. The sidecar's command line cannot be observed once the child
is running: a missing --modules produces a sidecar that starts, reports healthy,
and answers 404 for every route the module was supposed to add. The only place
that argument can be checked is before it is handed to TProcess, which is why
sidecar_launch exists as a unit of its own.

The URL helpers look trivial and are not: the port and the loopback address are
agreed between this process and a Python one, and the two halves are in different
languages.
}
unit testcase_sidecar_launch;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, sidecar_launch;

type
    TSidecarLaunchTest = class(TTestCase)
    private
        FArgs: TStringList;
        { The value following AName in FArgs, or '' when AName is absent. }
        function ValueAfter(const AName: string): string;
        function Names(const AName: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Where the sidecar is reached.
        procedure TheSidecarIsOnLoopbackOnly;
        procedure TheHealthAndFitRoutesShareTheBase;
        procedure ThePortReachesTheUrl;

        //  What it is started with.
        procedure TheScriptComesFirst;
        procedure ThePortIsPassedAsAnArgument;
        procedure TheParentPidIsPassedSoTheChildCanOutliveNothing;
        procedure TheLogFileIsPassed;
        procedure NoModulesMeansNoModulesArgument;
        procedure RegisteredModulesArePassedThrough;
        procedure NothingIsPassedTwice;

        //  Which module packages are registered.
        procedure AnEmptyNameRegistersNothing;
        procedure TheFirstNameNeedsNoSeparator;
        procedure TwoNamesArePathSeparated;
        procedure TheSameNameTwiceIsRegisteredOnce;
        procedure ANameThatIsAPrefixOfAnotherIsStillItsOwnModule;
        procedure MembershipIsByWholeName;

        //  Where the script and the interpreter are looked for.
        procedure EveryCandidateEndsInTheScriptName;
        procedure TheCandidatesCoverTheDevelopmentAndInstalledLayouts;
        procedure TheVirtualenvIsInsideTheScriptDirectory;
        procedure TheFallbackInterpreterIsNotAPath;
    end;

implementation

const
    { A port that is not the sidecar's own, so a hard-coded 8788 anywhere in the
      URL construction shows up as a mismatch. }
    SomePort = 9137;

procedure TSidecarLaunchTest.SetUp;
begin
    FArgs := TStringList.Create;
end;

procedure TSidecarLaunchTest.TearDown;
begin
    FArgs.Free;
    FArgs := nil;
end;

function TSidecarLaunchTest.ValueAfter(const AName: string): string;
var
    i: integer;
begin
    Result := '';
    i := FArgs.IndexOf(AName);
    if (i >= 0) and (i + 1 < FArgs.Count) then
        Result := FArgs[i + 1];
end;

function TSidecarLaunchTest.Names(const AName: string): boolean;
begin
    Result := FArgs.IndexOf(AName) >= 0;
end;

{ ---- the address ----------------------------------------------------------- }

procedure TSidecarLaunchTest.TheSidecarIsOnLoopbackOnly;
begin
    //  NOT reachable from another machine. fit_server owns the sidecar; the
    //  desktop client talks only to fit_server, and a sidecar bound to 0.0.0.0
    //  would put an unauthenticated fitting service on the network.
    AssertEquals('loopback', 'http://127.0.0.1:9137',
        SidecarBaseUrl(SomePort));
end;

procedure TSidecarLaunchTest.TheHealthAndFitRoutesShareTheBase;
begin
    AssertEquals('health', SidecarBaseUrl(SomePort) + '/health',
        SidecarHealthUrl(SomePort));
    AssertEquals('fit', SidecarBaseUrl(SomePort) + '/fit',
        SidecarFitUrl(SomePort));
end;

procedure TSidecarLaunchTest.ThePortReachesTheUrl;
begin
    //  Two different ports must give two different URLs. A URL built from a
    //  constant instead of the field would pass every other assertion here.
    AssertTrue('the port is not ignored',
        SidecarBaseUrl(SomePort) <> SidecarBaseUrl(SomePort + 1));
end;

{ ---- the command line ------------------------------------------------------ }

procedure TSidecarLaunchTest.TheScriptComesFirst;
begin
    //  The interpreter is the executable, so the script has to be argument zero -
    //  anywhere else and Python reads it as an option.
    BuildSidecarArgs(FArgs, '/opt/fit/py/fit_backend.py', SomePort, 4242,
        '/var/log/sidecar.txt', '');
    AssertTrue('there are arguments', FArgs.Count > 0);
    AssertEquals('the script', '/opt/fit/py/fit_backend.py', FArgs[0]);
end;

procedure TSidecarLaunchTest.ThePortIsPassedAsAnArgument;
begin
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt', '');
    AssertEquals('--port', '9137', ValueAfter('--port'));
end;

procedure TSidecarLaunchTest.TheParentPidIsPassedSoTheChildCanOutliveNothing;
begin
    //  The sidecar watches this pid and exits when it goes. Without it, a server
    //  killed without a clean shutdown leaves a Python process holding the port,
    //  and the next server start reuses a worker nobody owns.
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt', '');
    AssertEquals('--parent-pid', '4242', ValueAfter('--parent-pid'));
end;

procedure TSidecarLaunchTest.TheLogFileIsPassed;
begin
    //  Its stderr is detached so the pipe cannot fill and block, which makes this
    //  file the only record of what the Python side computed.
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242,
        '/home/u/.fit/fit_sidecar_log.txt', '');
    AssertEquals('--log-file', '/home/u/.fit/fit_sidecar_log.txt',
        ValueAfter('--log-file'));
end;

procedure TSidecarLaunchTest.NoModulesMeansNoModulesArgument;
begin
    //  THE PUBLIC BUILD. Passing an empty --modules would make the sidecar try to
    //  import a package named '' and fail to start at all.
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt', '');
    AssertFalse('no --modules at all: ' + FArgs.CommaText, Names('--modules'));
end;

procedure TSidecarLaunchTest.RegisteredModulesArePassedThrough;
begin
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt',
        'patterns' + PathSeparator + 'other');
    AssertEquals('--modules', 'patterns' + PathSeparator + 'other',
        ValueAfter('--modules'));
end;

procedure TSidecarLaunchTest.NothingIsPassedTwice;
var
    i, j, Repeats: integer;
begin
    //  A duplicated option is how a merge of two argument-building branches
    //  fails, and argparse takes the last one - so the wrong value wins silently.
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt',
        'patterns');
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

{ ---- the module list ------------------------------------------------------- }

procedure TSidecarLaunchTest.AnEmptyNameRegistersNothing;
begin
    //  A module that declares no Python package registers ''. Appending it would
    //  put a bare separator in the list and the sidecar would import nothing
    //  under an empty name.
    AssertEquals('from empty', '', AddSidecarModule('', ''));
    AssertEquals('from non-empty', 'patterns',
        AddSidecarModule('patterns', ''));
end;

procedure TSidecarLaunchTest.TheFirstNameNeedsNoSeparator;
begin
    AssertEquals('no leading separator', 'patterns',
        AddSidecarModule('', 'patterns'));
end;

procedure TSidecarLaunchTest.TwoNamesArePathSeparated;
begin
    AssertEquals('separated', 'patterns' + PathSeparator + 'other',
        AddSidecarModule('patterns', 'other'));
end;

procedure TSidecarLaunchTest.TheSameNameTwiceIsRegisteredOnce;
begin
    //  ORDINARY, not an error: every host that starts a sidecar registers its own
    //  modules, and one process can construct more than one host.
    AssertEquals('unchanged', 'patterns', AddSidecarModule('patterns', 'patterns'));
    AssertEquals('and in the middle of a list',
        'a' + PathSeparator + 'b',
        AddSidecarModule('a' + PathSeparator + 'b', 'a'));
end;

procedure TSidecarLaunchTest.ANameThatIsAPrefixOfAnotherIsStillItsOwnModule;
begin
    //  THE CASE A SUBSTRING TEST GETS WRONG, and this rule was a substring test
    //  before it was extracted: registering 'pat' after 'patterns' found 'pat'
    //  inside it and dropped the module, which then imports nothing and answers
    //  404 for its own routes.
    AssertEquals('the shorter name is added',
        'patterns' + PathSeparator + 'pat',
        AddSidecarModule('patterns', 'pat'));
    AssertEquals('and so is the longer one',
        'pat' + PathSeparator + 'patterns',
        AddSidecarModule('pat', 'patterns'));
end;

procedure TSidecarLaunchTest.MembershipIsByWholeName;
begin
    AssertTrue('a listed name', SidecarModuleListed('a' + PathSeparator + 'b', 'b'));
    AssertFalse('a name that only occurs inside one',
        SidecarModuleListed('patterns', 'pat'));
    AssertFalse('nothing is in an empty list', SidecarModuleListed('', 'a'));
    AssertFalse('and an empty name is in no list',
        SidecarModuleListed('a', ''));
end;

{ ---- where it is looked for ------------------------------------------------ }

procedure TSidecarLaunchTest.EveryCandidateEndsInTheScriptName;
var
    i: integer;
begin
    SidecarScriptCandidates(FArgs, '/opt/fit/bin/');
    AssertTrue('there are candidates', FArgs.Count > 0);
    for i := 0 to FArgs.Count - 1 do
        AssertTrue('candidate names the script: ' + FArgs[i],
            Pos('fit_backend.py', FArgs[i]) > 0);
end;

procedure TSidecarLaunchTest.TheCandidatesCoverTheDevelopmentAndInstalledLayouts;
var
    Joined: string;
begin
    //  FOUR LAYOUTS, and each one is a real place this binary runs from: the
    //  server in Worker/o, the same in Worker/o/<arch>, an installed tree with
    //  py/ beside the binary, and the test binary in tests/. Losing one of them
    //  makes the sidecar simply unavailable in that layout, with no error that
    //  names the reason.
    SidecarScriptCandidates(FArgs, '/tree/Worker/o/');
    Joined := FArgs.Text;
    AssertTrue('one level up (Worker/o)',
        Pos('/tree/Worker/o/../py/', Joined) > 0);
    AssertTrue('two levels up (Worker/o/<arch>)',
        Pos('/tree/Worker/o/../../py/', Joined) > 0);
    AssertTrue('beside the binary (installed)',
        Pos('/tree/Worker/o/py/', Joined) > 0);
    AssertTrue('from a sibling of Worker (the test binary)',
        Pos('/tree/Worker/o/../Worker/py/', Joined) > 0);
end;

procedure TSidecarLaunchTest.TheVirtualenvIsInsideTheScriptDirectory;
begin
    //  Relative to the script, not to the binary: the venv is created beside
    //  fit_backend.py by the build instructions, and a server installed
    //  elsewhere must still find it.
    AssertTrue('under the script directory',
        Pos('/py/', VenvPython('/tree/Worker/py/')) > 0);
    AssertTrue('and it is a venv',
        Pos('.venv', VenvPython('/tree/Worker/py/')) > 0);
end;

procedure TSidecarLaunchTest.TheFallbackInterpreterIsNotAPath;
begin
    //  A BARE NAME, so it is resolved through PATH. An absolute path would work
    //  on the machine it was written for and nowhere else.
    AssertTrue('no directory in it',
        ExtractFilePath(SystemPython) = '');
    AssertTrue('and it is a python', Pos('python', SystemPython) > 0);
end;

initialization
    //  A unit test: no process is started, no port is bound and no file is
    //  touched - which is exactly what made these decisions untestable before
    //  they were moved out of python_sidecar.
    RegisterTest('unit', TSidecarLaunchTest);
end.
