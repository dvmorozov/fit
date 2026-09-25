// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Python sidecar is started with, and where it is reached -
decided without starting anything.)

WHY IT IS ITS OWN UNIT. Every decision here used to sit inside
python_sidecar.EnsureRunning, between a TProcess and a socket, and so could only
be exercised by starting a real Python interpreter. The unit measured zero
covered lines while owning the command line the sidecar is launched with, the
rule that keeps a module from being imported twice, and the loopback URL both
halves have to agree on. A wrong argument here does not fail loudly: the sidecar
starts, does not import a module, and answers 404 for its routes.

NOTHING HERE TOUCHES THE WORLD. No process, no socket, no FileExists. The
candidate paths are produced as a list and the probing is left to the caller,
because which paths to try in which order is a decision and asking the disk is
not.
}
unit sidecar_launch;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ The sidecar's base URL. Loopback, always: fit_server owns the sidecar and the
  desktop client must never be able to reach it. }
function SidecarBaseUrl(APort: word): string;
{ Where the sidecar answers that it is alive. }
function SidecarHealthUrl(APort: word): string;
{ Where a fit problem is posted. }
function SidecarFitUrl(APort: word): string;

{ True when APackage is already named in the path-separated AList.

  BY WHOLE NAME, not as a substring: two modules whose names share a prefix are
  different modules, and a substring test silently refuses to register the
  longer one. }
function SidecarModuleListed(const AList, APackage: string): boolean;
{ AList with APackage appended. An empty name and an exact repeat both return
  AList unchanged - a repeat is ordinary, because every host that starts a
  sidecar registers its own modules. }
function AddSidecarModule(const AList, APackage: string): string;

{ The command line the sidecar is started with, appended to ADest.

  --modules is passed ONLY when a module registered. A build with no module -
  which is the public build - starts a sidecar that imports nothing and answers
  only the routes the generic backend declares itself.

  An ARGUMENT rather than an environment variable, throughout: setting one
  variable on TProcess replaces the whole environment, and the child would lose
  PATH and the virtualenv with it. }
procedure BuildSidecarArgs(ADest: TStrings; const AScript: string;
    APort: word; ALifelinePort: word; const ALogFile, AModules: string);

{ Where to look for the sidecar script, given the directory of the running
  binary, appended to ADest in the order they should be tried.

  The order is the point, and it is the order this has always used - see the
  comment on the implementation. }
procedure SidecarScriptCandidates(ADest: TStrings; const ABinDir: string);

{ Where a repository checked out BESIDE this one would keep the sidecar script,
  one candidate per name in ANames, appended to ADest in the order given.
  AUmbrella ends in a separator, as ABinDir does above.

  A module pack is its own repository and keeps no file in the framework's, so
  its fit_server runs from <pack>/Worker/o and the script is not above it at all
  - it is in the framework repository next door. The sidecar already applies this
  same rule to find a module's <name>_routes.py; this is the other half of it. }
procedure SidecarSiblingCandidates(ADest: TStrings; const AUmbrella: string;
    ANames: TStrings);

{ The directories that may hold this repository and its siblings, given the
  directory of the running binary - what to list to get the names above. }
procedure SidecarUmbrellaCandidates(ADest: TStrings; const ABinDir: string);

{ Where the virtual environments live, given what the environment says. The
  directory holding 'sidecar', or '' when none of the arguments can say.

  A PURE FUNCTION OF ITS ARGUMENTS, and the reason the one below is two lines
  long. The precedence is the decision here - an explicit setting outranks the
  per-user location, and a machine that can name neither gets nothing rather
  than a path rooted at '' - and a decision that can only be reached by setting
  variables in the test process is a decision nothing states. Each argument is
  one environment variable, empty when it is unset; which ones are read on this
  platform is SidecarPyHome's business, and the unused ones arrive empty. }
function SidecarPyHomeFrom(const AExplicit, ALocalAppData, AXdgData,
    AHome: string): string;

{ Where the virtual environments live on this machine - the directory holding
  'sidecar', or '' when the environment cannot say.

  THE ONE THING HERE THAT READS THE ENVIRONMENT, and it reads it as an INPUT
  rather than as a probe: no file is opened and no directory is listed, so the
  unit's rule still holds. It holds nothing back for itself - every choice it
  makes is in SidecarPyHomeFrom above, so what is left is the reading. }
function SidecarPyHome: string;

{ The interpreters to try for a sidecar whose script is in APyDir, appended to
  ADest in the order they should be tried. APyHome is what SidecarPyHome
  answered; APyDir ends in a separator.

  TWO PLACES, newest first. The environment is per MACHINE and shared by every
  checkout on it (tools/build-lib/core.ps1, Get-PyHome), because one per checkout
  meant ~9.6k files copied between machines for every clone. The in-tree
  Worker/py/.venv is where it USED to go, and is still tried: a checkout that has
  not run the prerequisites step since the move has one, and it works. }
procedure SidecarPythonCandidates(ADest: TStrings; const APyHome, APyDir: string);
{ The interpreter to fall back on when there is no virtualenv. It may still have
  the libraries; the sidecar reports its own failure if it does not. }
function SystemPython: string;

implementation

uses
    app_data_root;

const
    { The sidecar's entry point, wherever it is found. }
    SIDECAR_SCRIPT = 'fit_backend.py';
    { Not 'localhost': that resolves, and on some hosts resolves to something
      other than the interface the sidecar bound. }
    LOOPBACK = '127.0.0.1';

function SidecarBaseUrl(APort: word): string;
begin
    Result := Format('http://%s:%d', [LOOPBACK, APort]);
end;

function SidecarHealthUrl(APort: word): string;
begin
    Result := SidecarBaseUrl(APort) + '/health';
end;

function SidecarFitUrl(APort: word): string;
begin
    Result := SidecarBaseUrl(APort) + '/fit';
end;

function SidecarModuleListed(const AList, APackage: string): boolean;
var
    Names: TStringList;
begin
    Result := False;
    if (AList = '') or (APackage = '') then
        Exit;
    Names := TStringList.Create;
    try
        Names.Delimiter := PathSeparator;
        Names.StrictDelimiter := True;
        Names.DelimitedText := AList;
        Result := Names.IndexOf(APackage) >= 0;
    finally
        Names.Free;
    end;
end;

function AddSidecarModule(const AList, APackage: string): string;
begin
    Result := AList;
    if APackage = '' then
        Exit;
    if SidecarModuleListed(AList, APackage) then
        Exit;
    if Result <> '' then
        Result := Result + PathSeparator;
    Result := Result + APackage;
end;

procedure BuildSidecarArgs(ADest: TStrings; const AScript: string;
    APort: word; ALifelinePort: word; const ALogFile, AModules: string);
begin
    ADest.Add(AScript);
    ADest.Add('--port');
    ADest.Add(IntToStr(APort));
    //  THE LIFELINE: the sidecar connects here, says when it is listening, and
    //  exits when fit_server's end of the connection closes - on a clean
    //  shutdown or a kill alike (readiness_channel).
    ADest.Add('--lifeline-port');
    ADest.Add(IntToStr(ALifelinePort));
    //  Its stderr is detached, so it needs a log file of its own - the durable
    //  record of every Python fit's numerics.
    ADest.Add('--log-file');
    ADest.Add(ALogFile);
    if AModules <> '' then
    begin
        ADest.Add('--modules');
        ADest.Add(AModules);
    end;
end;

procedure SidecarScriptCandidates(ADest: TStrings; const ABinDir: string);
const
    SCRIPT = SIDECAR_SCRIPT;
begin
    //  Development first, because that is the order this has always had and the
    //  layouts are disjoint in practice - no deployment has a ../py beside the
    //  binary. Kept rather than tidied: reordering these changes which script a
    //  server runs, which is not a change to make while extracting.
    ADest.Add(ABinDir + '../py/' + SCRIPT);
    ADest.Add(ABinDir + '../../py/' + SCRIPT);
    //  Installed beside the binary.
    ADest.Add(ABinDir + 'py/' + SCRIPT);
    //  From a sibling of Worker/ - the test binary in tests/, say.
    ADest.Add(ABinDir + '../Worker/py/' + SCRIPT);
end;

procedure SidecarSiblingCandidates(ADest: TStrings; const AUmbrella: string;
    ANames: TStrings);
var
    i: integer;
begin
    for i := 0 to ANames.Count - 1 do
        if ANames[i] <> '' then
            //  Forward slashes throughout, as every other candidate here: the
            //  RTL accepts them on Windows too, and mixing separators would make
            //  the same path read differently in a log.
            ADest.Add(AUmbrella + ANames[i] + '/Worker/py/' + SIDECAR_SCRIPT);
end;

procedure SidecarUmbrellaCandidates(ADest: TStrings; const ABinDir: string);
begin
    //  ONE ENTRY PER LAYOUT the script candidates already name, counted out to
    //  the directory the repositories sit in: a test binary in tests/ is two
    //  levels below it, a server in Worker/o three, one in Worker/o/<arch> four.
    //
    //  SHALLOWEST FIRST, because a depth that is wrong for the binary at hand
    //  lands INSIDE the repository - where nothing matches - while a depth that
    //  is too deep lands above the umbrella, among unrelated checkouts that
    //  could match something. Being wrong upwards is the expensive direction.
    ADest.Add(ABinDir + '../../');
    ADest.Add(ABinDir + '../../../');
    ADest.Add(ABinDir + '../../../../');
end;

{ The interpreter inside a virtual environment, wherever that environment is. }
function VenvInterpreter(const AVenv: string): string;
begin
{$IFDEF WINDOWS}
    Result := IncludeTrailingPathDelimiter(AVenv) + 'Scripts\python.exe';
{$ELSE}
    Result := IncludeTrailingPathDelimiter(AVenv) + 'bin/python';
{$ENDIF}
end;

function SidecarPyHomeFrom(const AExplicit, ALocalAppData, AXdgData,
    AHome: string): string;
begin
    //  An explicit setting wins outright: it is how a CI job or a machine with
    //  an unusual layout says where the environment is, and it is the same
    //  variable the build script reads.
    if AExplicit <> '' then
        Exit(AExplicit);
    //  Everything below the explicit setting is "where does this application
    //  keep per-user data?", which is not a question about Python - the data
    //  source cache asks it too. app_data_root answers it once.
    Result := AppDataRootFrom(ALocalAppData, AXdgData, AHome);
    if Result <> '' then
        Result := IncludeTrailingPathDelimiter(Result) + 'py';
end;

function SidecarPyHome: string;
begin
{$IFDEF WINDOWS}
    Result := SidecarPyHomeFrom(GetEnvironmentVariable('FIT_PY_HOME'),
        GetEnvironmentVariable('LOCALAPPDATA'), '', '');
{$ELSE}
    Result := SidecarPyHomeFrom(GetEnvironmentVariable('FIT_PY_HOME'), '',
        GetEnvironmentVariable('XDG_DATA_HOME'), GetEnvironmentVariable('HOME'));
{$ENDIF}
end;

procedure SidecarPythonCandidates(ADest: TStrings; const APyHome, APyDir: string);
begin
    if APyHome <> '' then
        ADest.Add(VenvInterpreter(IncludeTrailingPathDelimiter(APyHome) + 'sidecar'));
    //  APyDir already ends in a separator - it is an ExtractFilePath result, as
    //  every other candidate procedure in this unit takes.
    if APyDir <> '' then
        ADest.Add(VenvInterpreter(APyDir + '.venv'));
end;

function SystemPython: string;
begin
{$IFDEF WINDOWS}
    Result := 'python.exe';
{$ELSE}
    Result := 'python3';
{$ENDIF}
end;

end.
