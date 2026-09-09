// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the launcher does before the desktop client starts - decided
without starting anything.)

WHY THERE IS A LAUNCHER AT ALL. Fit is a client and a compute server, and the
client has no engine of its own. Installed side by side, clicking the menu entry
gives a window that cannot fit anything until the user knows to start a second
program - which is not a package "installing the app". So every package puts a
launcher in front of the client: /usr/bin/fit on Linux, Contents/MacOS/fit inside
the macOS bundle, fit_launcher.exe on Windows. Each one asks the same question -
is a server answering? - and starts one only when nothing is.

THE SERVER OUTLIVES THE WINDOW, deliberately. Killing it on exit would be tidier
for a single window and wrong for two: the second client reuses the first's
server, and closing the first would take the engine away from the second
mid-fit. One idle server per session, reused by every later launch, is the lesser
cost - and it makes every start after the first immediate.

NOTHING HERE TOUCHES THE WORLD. No socket, no process, no FileExists: the
candidate paths are produced as a list and the probing is left to the caller,
exactly as sidecar_launch does for the Python sidecar, and for the same reason -
a launcher that can only be exercised by starting two programs is a launcher
nothing checks.

THE NUMBERS ARE STATED ONCE HERE. The port, the loopback URL and the wait budget
are also written into the shell launchers that packaging generates, which is
three copies of one rule; the packaging tests read those numbers back out of the
shell text and compare them with the constants below. That comparison is what
stops a Windows launcher that waits ten seconds from shipping beside a macOS one
that waits two.
}
unit launcher_rules;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

const
    { The port the client looks at unless it has been told otherwise
      (Desktop/http_fit_service.DEFAULT_SERVER_URL), and the one the server binds
      unless it is passed --port (Worker/fit_server.DEFAULT_PORT). }
    DefaultLauncherPort = 8787;

    { How long to wait for a freshly started server to bind, as tries and the
      pause between them.

      WHY WAIT AT ALL. The client probes the server while it starts up. Racing
      that probe means the first window after an install says no server answered
      - which is wrong, and is the single most confusing thing this program can
      say. }
    LauncherWaitTries = 50;
    LauncherWaitIntervalMs = 200;

    { How long to wait for the server to ANSWER one probe. Short: a server that
      is up answers immediately, and this is on the path of every launch. }
    LauncherProbeTimeoutMs = 2000;

    { The environment variable that moves the whole arrangement to another port.
      The shell launchers read the same variable, falling back to the same
      default. }
    LauncherPortVariable = 'FIT_PORT';

type
    { What to do about the compute server before the client is started. }
    TLaunchStep = (
        { Something is already answering on the port. It is used as it is - see
          the comment on the unit. }
        lsReuseTheRunningServer,
        { Nothing answered, so this launcher starts one. }
        lsStartOne
    );

{ The names the two binaries are installed under. }
function ClientExeName: string;
function ServerExeName: string;

{ The compute server's base URL. Loopback, always: the launcher starts a server
  for this user on this machine, and a probe that could be answered from the
  network would leave the local client with no engine. }
function ServerBaseUrl(APort: word): string;
{ Where the server answers that it is alive. }
function ServerHealthUrl(APort: word): string;

{ The port to use, given the value of FIT_PORT (or '' when it is unset).

  Anything that is not a usable port number - a typo, a blank, 0, something above
  65535 - is the default rather than an error: a launcher that refused to start
  would be a program that cannot be started at all, with no window to say why. }
function LauncherPort(const AEnvValue: string): word;

{ The command line the server is started with, appended to ADest.

  --port is passed even when it is the default, because the default is agreed in
  three places and the moment FIT_PORT moves one of them the others must follow. }
procedure BuildServerArgs(ADest: TStrings; APort: word);

{ Where to look for the server and the client, given the directory the launcher
  itself is in, appended to ADest in the order they should be tried.

  BESIDE THE LAUNCHER FIRST. A development tree on a machine that also has Fit
  installed must not make the installed launcher run the built binaries: the user
  would be running a different program than the one they installed, and nothing
  would say so. }
procedure ServerBinaryCandidates(ADest: TStrings; const ALauncherDir: string);
procedure ClientBinaryCandidates(ADest: TStrings; const ALauncherDir: string);

{ What to do once the port has been probed. }
function StepBeforeStartingTheClient(AServerAnswered: boolean): TLaunchStep;

{ The whole time a launcher will wait for a server it started. }
function LauncherWaitBudgetMs: integer;

{ One argument of the launcher's own command line, as the client will accept it.

  THE TRANSLATION THIS UNIT EXISTS FOR, after the decision itself. Fit accepts an
  argument only when its first character is / or \ (Desktop/Fit.lpr,
  CmdLineParamFound); a file handed over by the desktop - Explorer substituting
  %1, macOS opening a document - arrives as a bare path, which the client
  silently ignores. So a bare path becomes the switch that names it, and anything
  that is already a switch is passed through untouched.

  NOT QUOTED: the result goes into TProcess.Parameters, which quotes each
  argument itself. A quote added here would reach the client as part of the file
  name. }
function SwitchForArgument(const AArg: string): string;

implementation

const
    { Not 'localhost': that resolves, and on some hosts resolves to something
      other than the interface the server bound. }
    LOOPBACK = '127.0.0.1';
    { Desktop/project_commands.ProjectExtension - the one extension that is Fit's
      own and nobody else's. }
    PROJECT_EXTENSION = '.fitproj';
{$IFDEF WINDOWS}
    EXE_SUFFIX = '.exe';
{$ELSE}
    EXE_SUFFIX = '';
{$ENDIF}
    { What the client project writes into Desktop/o: the binary is named for the
      target it was built for, so a build tree holds one per target. }
    BUILD_TARGET = {$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%};

function ClientExeName: string;
begin
    Result := 'Fit' + EXE_SUFFIX;
end;

function ServerExeName: string;
begin
    Result := 'fit_server' + EXE_SUFFIX;
end;

function ServerBaseUrl(APort: word): string;
begin
    Result := Format('http://%s:%d', [LOOPBACK, APort]);
end;

function ServerHealthUrl(APort: word): string;
begin
    Result := ServerBaseUrl(APort) + '/health';
end;

function LauncherPort(const AEnvValue: string): word;
var
    Value: longint;
begin
    Result := DefaultLauncherPort;
    Value := StrToIntDef(Trim(AEnvValue), 0);
    //  0 is "any free port" to the OS, and the client would have nowhere to
    //  look; above 65535 is not a port at all.
    if (Value > 0) and (Value <= High(word)) then
        Result := word(Value);
end;

procedure BuildServerArgs(ADest: TStrings; APort: word);
begin
    ADest.Add('--port');
    ADest.Add(IntToStr(APort));
end;

procedure ServerBinaryCandidates(ADest: TStrings; const ALauncherDir: string);
begin
    //  Installed: both binaries and the launcher in one directory.
    ADest.Add(ALauncherDir + ServerExeName);
    //  A launcher built into Worker/o beside the server it starts - which is
    //  where a developer runs it from, and the only way it is tried before a
    //  release.
    ADest.Add(ALauncherDir + '../Worker/o/' + ServerExeName);
    ADest.Add(ALauncherDir + '../../Worker/o/' + ServerExeName);
end;

procedure ClientBinaryCandidates(ADest: TStrings; const ALauncherDir: string);
begin
    ADest.Add(ALauncherDir + ClientExeName);
    //  In a build the client is under Desktop/o, named for its target.
    ADest.Add(ALauncherDir + '../../Desktop/o/' + BUILD_TARGET + '/Fit-' +
        BUILD_TARGET + EXE_SUFFIX);
    ADest.Add(ALauncherDir + '../Desktop/o/' + BUILD_TARGET + '/Fit-' +
        BUILD_TARGET + EXE_SUFFIX);
end;

function StepBeforeStartingTheClient(AServerAnswered: boolean): TLaunchStep;
begin
    if AServerAnswered then
        //  THE RULE THE WHOLE FEATURE TURNS ON. A second server would fail to
        //  bind a taken port, and the user would be told the server cannot start
        //  - about a server that is running and fitting.
        Result := lsReuseTheRunningServer
    else
        Result := lsStartOne;
end;

function LauncherWaitBudgetMs: integer;
begin
    Result := LauncherWaitTries * LauncherWaitIntervalMs;
end;

{ True when AArg is one of the client's own switches rather than a file name.

  NOT "starts with a slash": on Unix every absolute path does, and a data file
  passed to the launcher would then be handed to the client unchanged - which is
  precisely the silence this unit exists to remove. A switch is a slash, a name
  with no directory separator in it, and an equals sign. }
function IsClientSwitch(const AArg: string): boolean;
var
    Head: string;
    Eq: integer;
begin
    Result := False;
    if (AArg = '') or ((AArg[1] <> '/') and (AArg[1] <> '\')) then
        Exit;
    Eq := Pos('=', AArg);
    if Eq < 3 then
        Exit;
    Head := Copy(AArg, 2, Eq - 2);
    Result := (Pos('/', Head) = 0) and (Pos('\', Head) = 0);
end;

function SwitchForArgument(const AArg: string): string;
begin
    Result := AArg;
    if Result = '' then
        Exit;
    //  Already a switch - including one this launcher does not know, which is
    //  the client's business and not its own.
    if IsClientSwitch(Result) then
        Exit;
    if SameText(ExtractFileExt(Result), PROJECT_EXTENSION) then
        Result := '/PROJECT=' + Result
    else
        Result := '/INFILE=' + Result;
end;

end.
