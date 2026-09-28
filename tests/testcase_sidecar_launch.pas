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
        procedure TheLifelinePortIsPassedSoTheChildCanOutliveNothing;
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
        procedure ARepositoryBesideThisOneIsLookedInToo;
        procedure TheUmbrellaIsLookedForAboveTheRepository;
        procedure AnExplicitSettingOutranksThePerUserLocation;
        procedure AnEnvironmentThatSaysNothingGivesNoHome;
        procedure ThePerUserLocationIsWhereThisPlatformKeepsApplicationData;
        procedure TheSharedEnvironmentIsTriedBeforeTheInTreeOne;
        procedure TheInTreeEnvironmentIsStillOffered;
        procedure TheMissingHomeDropsOutOfTheList;
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

procedure TSidecarLaunchTest.TheLifelinePortIsPassedSoTheChildCanOutliveNothing;
begin
    //  THE LIFELINE, NOT A PROCESS ID. The sidecar connects to this port, says
    //  when it is listening, and exits when fit_server's end of the connection
    //  closes - a server killed without a clean shutdown included - instead of
    //  asking every two seconds whether a pid still exists.
    BuildSidecarArgs(FArgs, 'fit_backend.py', SomePort, 4242, 'log.txt', '');
    AssertEquals('--lifeline-port', '4242', ValueAfter('--lifeline-port'));
    AssertEquals('and no process id to poll', -1, FArgs.IndexOf('--parent-pid'));
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

procedure TSidecarLaunchTest.ARepositoryBesideThisOneIsLookedInToo;
var
    Repos: TStringList;
begin
    //  WHERE A MODULE BUILD'S SERVER RUNS FROM. The pack is its own repository
    //  and keeps no file in the framework's, so its fit_server sits in
    //  <pack>/Worker/o with no fit_backend.py anywhere above it - the script is
    //  in the framework repository CHECKED OUT BESIDE the pack. Without this the
    //  sidecar is simply unavailable in every private build, and Detect answers
    //  "this feature needs the Python component" on a machine that has it.
    //
    //  The same rule the sidecar itself applies when it looks for a module's
    //  <name>_routes.py: each repository beside the one it is in.
    Repos := TStringList.Create;
    try
        Repos.Add('fit');
        Repos.Add('fit-pack');
        SidecarSiblingCandidates(FArgs, '/work/', Repos);
        AssertEquals('one candidate per repository', 2, FArgs.Count);
        AssertEquals('the script under the first repository''s Worker/py',
            '/work/fit/Worker/py/fit_backend.py', FArgs[0]);
        AssertEquals('and under the second, in the order given',
            '/work/fit-pack/Worker/py/fit_backend.py', FArgs[1]);
    finally
        Repos.Free;
    end;
end;

procedure TSidecarLaunchTest.TheUmbrellaIsLookedForAboveTheRepository;
var
    Joined: string;
begin
    //  EVERY DEPTH a binary here runs from: the test binary in tests/, the
    //  server in Worker/o, and the server in Worker/o/<arch>. Two, three and
    //  four levels up reach the directory the repositories sit in - and the
    //  tests/ one is why the pack's decompose tests can find the sidecar at all
    //  instead of ignoring themselves.
    SidecarUmbrellaCandidates(FArgs, '/work/tree/Worker/o/');
    Joined := FArgs.Text;
    AssertTrue('above the repository (Worker/o): ' + Joined,
        Pos('/work/tree/Worker/o/../../../', Joined) > 0);
    AssertTrue('and one deeper (Worker/o/<arch>): ' + Joined,
        Pos('/work/tree/Worker/o/../../../../', Joined) > 0);
    AssertTrue('and one shallower, for a binary in tests/: ' + Joined,
        Pos('/work/tree/Worker/o/../../' + LineEnding, Joined) > 0);
    //  Shallowest first: a depth that is too deep lands among unrelated
    //  checkouts, which is the direction that can match the wrong tree.
    AssertEquals('the shallowest is tried first',
        '/work/tree/Worker/o/../../', FArgs[0]);
end;

procedure TSidecarLaunchTest.AnExplicitSettingOutranksThePerUserLocation;
begin
    //  FIT_PY_HOME IS HOW A MACHINE SAYS "not there, here" - a CI job with the
    //  environment on a cache volume, a developer with a full home partition -
    //  and it is the same variable the build script reads, so an override that
    //  moved only one of the two would point the server at an environment the
    //  prerequisites step never fills. It wins whatever else is set, which is
    //  the only way to be sure it is what took effect.
    AssertEquals('the explicit setting, not the per-user location',
        '/mnt/cache/py',
        SidecarPyHomeFrom('/mnt/cache/py', 'C:\Users\u\AppData\Local',
            '/home/u/.local/share', '/home/u'));
end;

procedure TSidecarLaunchTest.AnEnvironmentThatSaysNothingGivesNoHome;
begin
    //  A CONTAINER with neither HOME nor LOCALAPPDATA set. Nothing is the right
    //  answer: a path built from an empty base would be probed, fail, and be
    //  reported as "no interpreter on this machine" rather than as an
    //  environment that could not be located - and SidecarPythonCandidates
    //  drops an empty home from the list rather than offering it.
    AssertEquals('no home at all', '', SidecarPyHomeFrom('', '', '', ''));
end;

procedure TSidecarLaunchTest.ThePerUserLocationIsWhereThisPlatformKeepsApplicationData;
var
    Home: string;
begin
{$IFDEF WINDOWS}
    //  LOCALAPPDATA rather than APPDATA: the roaming one would carry compiled
    //  extensions between machines, where they are built for one Python minor
    //  version on one platform and mean nothing.
    Home := SidecarPyHomeFrom('', 'C:\Users\u\AppData\Local', '', '');
    AssertEquals('under the local application data',
        'C:\Users\u\AppData\Local\Fit\py', Home);
{$ELSE}
    //  XDG first, then HOME - and the HOME case has to build the .local/share
    //  leg itself, which is the half a machine without XDG_DATA_HOME takes and
    //  most of them do not set it.
    AssertEquals('XDG_DATA_HOME when it is set', '/xdg/fit/py',
        SidecarPyHomeFrom('', '', '/xdg', '/home/u'));
    Home := SidecarPyHomeFrom('', '', '', '/home/u');
    AssertEquals('and the default beneath HOME when it is not',
        '/home/u/.local/share/fit/py', Home);
{$ENDIF}
end;

procedure TSidecarLaunchTest.TheSharedEnvironmentIsTriedBeforeTheInTreeOne;
var
    L: TStringList;
begin
    //  ORDER IS THE POINT. The environment moved out of the checkout, and a
    //  machine mid-move has both: the one the prerequisites step now builds and
    //  an in-tree .venv it has not been run to remove yet. The shared one is the
    //  current one, so it answers first - otherwise a stale tree of pins would
    //  go on serving every fit on that machine.
    L := TStringList.Create;
    try
        SidecarPythonCandidates(L, '/home/u/.local/share/fit/py', '/tree/Worker/py/');
        AssertEquals('both places are offered', 2, L.Count);
        //  Separator-agnostic: the home is handed in with forward slashes and
        //  the interpreter is appended with whatever PathDelim is here, so
        //  asserting on a whole path would pass on one platform only.
        AssertTrue('under the shared home', Pos('/fit/py', L[0]) > 0);
        AssertTrue('in its sidecar environment', Pos('sidecar', L[0]) > 0);
        AssertTrue('and it is an interpreter', Pos('python', L[0]) > 0);
        AssertTrue('not the in-tree one', Pos('.venv', L[0]) = 0);
    finally
        L.Free;
    end;
end;

procedure TSidecarLaunchTest.TheInTreeEnvironmentIsStillOffered;
var
    L: TStringList;
begin
    //  Relative to the script, not to the binary: that is where the venv used to
    //  be created, and a checkout that has not run the prerequisites step since
    //  the move still has a working one there.
    L := TStringList.Create;
    try
        SidecarPythonCandidates(L, '/home/u/.local/share/fit/py', '/tree/Worker/py/');
        AssertTrue('under the script directory', Pos('/py/', L[1]) > 0);
        AssertTrue('and it is a venv', Pos('.venv', L[1]) > 0);
    finally
        L.Free;
    end;
end;

procedure TSidecarLaunchTest.TheMissingHomeDropsOutOfTheList;
var
    L: TStringList;
begin
    //  An environment that cannot say where it keeps its data - no HOME, no
    //  LOCALAPPDATA - produces no candidate rather than a path rooted at
    //  nothing, which would be probed, fail, and read as "no sidecar here".
    L := TStringList.Create;
    try
        SidecarPythonCandidates(L, '', '/tree/Worker/py/');
        AssertEquals('only the in-tree one is left', 1, L.Count);
        AssertTrue('and it is that one', Pos('.venv', L[0]) > 0);
    finally
        L.Free;
    end;
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
