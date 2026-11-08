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
launcher in front of the client: /usr/bin/fit on Linux, Contents/MacOS/fit_launcher
inside the macOS bundle, fit_launcher.exe on Windows. Each one asks the same question -
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
    Classes, SysUtils, product_identity;

const
    { The port the client looks at unless it has been told otherwise, and the
      one the server binds unless it is passed --port.

      THE PRODUCT'S OWN, not one number for every program built from this
      framework: products built from it are installed side by side, and
      with one shared port the second one started would find the first one's
      server answering and use it - a different product's routes, silently.
      Every place that used to write a fixed number (the server, both clients, the
      launcher, the installer's shutdown) reads product_identity instead. }
    DefaultLauncherPort = ProductServerPort;

    { HOW LONG A LAUNCHER WAITS for a server it started to say it is listening:
      ten seconds, stated once. The server says so over the readiness channel,
      so the wait ends the moment it is up - or the moment it dies - rather than
      after some number of probes.

      WHY WAIT AT ALL. The client probes the server while it starts up. Racing
      that probe means the first window after an install says no server answered
      - which is wrong, and is the single most confusing thing this program can
      say. }
    LauncherReadyBudgetMs = 10000;

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
        lsStartOne,
        { Something answers on the port, but it is another product's server (or
          one that does not say whose it is). Not used and not replaced: it is
          somebody else's, and the user is told. }
        lsRefuseAnotherProductsServer
    );

{ The names the binaries are installed under: this product's own, so that two
  products installed side by side never share a file name (and a kill by name
  never takes the other one's server). }
function ClientExeName: string;
function ServerExeName: string;
function LauncherExeName: string;

{ The product a server names in its /health reply, or '' when the reply names
  none (an older server, or something that is not a Fit server at all). }
function ServerProductOf(const AHealthReply: string): string;

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

{ The server's base URL when nothing else is configured: this product's port, or
  the one FIT_PORT names. What a window, a backend and an installer's shutdown
  all use, so none of them keeps a port of its own. }
function DefaultServerBaseUrl: string;

{ The command line the server is started with, appended to ADest.

  --port is passed even when it is the default, because the default is agreed in
  three places and the moment FIT_PORT moves one of them the others must follow.
  --ready-port names where the server says it is listening; 0 passes none. }
procedure BuildServerArgs(ADest: TStrings; APort, AReadyPort: word);

{ Where to look for the server and the client, given the directory the launcher
  itself is in, appended to ADest in the order they should be tried.

  BESIDE THE LAUNCHER FIRST. A development tree on a machine that also has Fit
  installed must not make the installed launcher run the built binaries: the user
  would be running a different program than the one they installed, and nothing
  would say so. }
procedure ServerBinaryCandidates(ADest: TStrings; const ALauncherDir: string);
procedure ClientBinaryCandidates(ADest: TStrings; const ALauncherDir: string);

{ What to do once the port has been probed. AAnsweringProduct is what the
  answering server said it is (ServerProductOf); it is not looked at when
  nothing answered. }
function StepBeforeStartingTheClient(AServerAnswered: boolean;
    const AAnsweringProduct: string): TLaunchStep;

{ Whether a server that says it is AAnsweringProduct is one this product may use.
  The ONE rule, for the launcher and for the window alike: only its own. An
  empty answer is not its own - a server that does not say whose it is cannot
  be told apart from another product's. }
function ServerIsThisProducts(const AAnsweringProduct: string): boolean;

{ What the user is told when the server on a port is another product's: whose
  it is, whose this is, and what to do. Registered in the explanation registry
  (client_explanations) under the same words, so the refusal explains itself. }
function AnotherProductsServerNotice(const AUrl,
    AAnsweringProduct: string): string;

{ One argument of the launcher's own command line, as the client will accept it.

  THE TRANSLATION THIS UNIT EXISTS FOR, after the decision itself. Fit reads
  options, --name=value (Desktop/command_line_options); a file handed over by the
  desktop - Explorer substituting %1, macOS opening a document - arrives as a
  bare path, which names no option. So a bare path becomes the option that names
  it, and anything that is already an option - in the current spelling or the
  old /NAME=value one, which the client still reads for one release - is passed
  through untouched.

  NOT QUOTED: the result goes into TProcess.Parameters, which quotes each
  argument itself. A quote added here would reach the client as part of the file
  name. }
function SwitchForArgument(const AArg: string): string;

implementation

uses
    fpjson, jsonparser;

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
    Result := ProductClientExe + EXE_SUFFIX;
end;

function ServerExeName: string;
begin
    Result := ProductServerExe + EXE_SUFFIX;
end;

function LauncherExeName: string;
begin
    Result := ProductLauncherExe + EXE_SUFFIX;
end;

function ServerProductOf(const AHealthReply: string): string;
var
    Parsed: TJSONData;
begin
    Result := '';
    try
        Parsed := GetJSON(AHealthReply);
    except
        //  Not JSON: something else holds the port. No product, which is the
        //  answer that is refused.
        Exit;
    end;
    try
        if Parsed is TJSONObject then
            Result := TJSONObject(Parsed).Get('product', '');
    finally
        Parsed.Free;
    end;
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

function DefaultServerBaseUrl: string;
begin
    Result := ServerBaseUrl(LauncherPort(GetEnvironmentVariable(LauncherPortVariable)));
end;

procedure BuildServerArgs(ADest: TStrings; APort, AReadyPort: word);
begin
    ADest.Add('--port');
    ADest.Add(IntToStr(APort));
    if AReadyPort > 0 then
    begin
        ADest.Add('--ready-port');
        ADest.Add(IntToStr(AReadyPort));
    end;
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
    ADest.Add(ALauncherDir + '../../Desktop/o/' + BUILD_TARGET + '/' +
        ProductClientExe + '-' + BUILD_TARGET + EXE_SUFFIX);
    ADest.Add(ALauncherDir + '../Desktop/o/' + BUILD_TARGET + '/' +
        ProductClientExe + '-' + BUILD_TARGET + EXE_SUFFIX);
end;

function ServerIsThisProducts(const AAnsweringProduct: string): boolean;
begin
    Result := AAnsweringProduct = ProductSlug;
end;

function StepBeforeStartingTheClient(AServerAnswered: boolean;
    const AAnsweringProduct: string): TLaunchStep;
begin
    if not AServerAnswered then
        Result := lsStartOne
    else if ServerIsThisProducts(AAnsweringProduct) then
        //  THE RULE THE WHOLE FEATURE TURNS ON. A second server would fail to
        //  bind a taken port, and the user would be told the server cannot start
        //  - about a server that is running and fitting.
        Result := lsReuseTheRunningServer
    else
        //  The port is taken, so starting one fails too - but using it would
        //  mean fitting on another product's routes without a word. Said
        //  instead.
        Result := lsRefuseAnotherProductsServer;
end;

function AnotherProductsServerNotice(const AUrl,
    AAnsweringProduct: string): string;
var
    Whose: string;
begin
    if AAnsweringProduct = '' then
        Whose := 'a server that does not say which product it belongs to'
    else
        Whose := 'the server of ' + AAnsweringProduct;
    Result := 'The server at ' + AUrl + ' is ' + Whose + ', and this is ' +
        ProductProjectName + ', which needs its own. ' + LineEnding +
        'Close the other program''s server, or move one of them to another ' +
        'port with the environment variable ' + LauncherPortVariable + '.';
end;

{ True when AArg is one of the client's own switches rather than a file name.

  NOT "starts with a slash": on Unix every absolute path does, and a data file
  passed to the launcher would then be handed to the client unchanged - which is
  precisely the silence this unit exists to remove. An option is two dashes and
  a name; the old spelling is a slash, a name with no directory separator in
  it, and an equals sign. }
function IsClientSwitch(const AArg: string): boolean;
var
    Head: string;
    Eq: integer;
begin
    Result := False;
    if (Length(AArg) > 2) and (Copy(AArg, 1, 2) = '--') then
        Exit(True);
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
        Result := '--project=' + Result
    else
        Result := '--infile=' + Result;
end;

end.
