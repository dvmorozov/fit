// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

  Fit's Windows launcher: makes sure a compute server is answering, then starts
  the desktop client.

  WHAT THE START MENU POINTS AT. Fit is a client and a compute server, and the
  client has no engine of its own; an entry that started the client alone would
  give the user a window that cannot fit anything until they knew to start a
  second program. So the installed shortcut - and the .fitproj association -
  point here. The Linux packages install a shell wrapper that does exactly this;
  this is the same wrapper where there is no shell to write it in.

  WHY A PROGRAM AND NOT A SCRIPT. A .cmd flashes a console window on every start
  and cannot wait on an HTTP endpoint without help; the installer's own [Run]
  step fires once, at install time, and says nothing about the second launch.
  This is a GUI-subsystem binary (fpc -WG), so nothing appears but the client's
  own window.

  IT DECIDES NOTHING ITSELF. The port, the health URL, the wait budget, where the
  two binaries are looked for and how a file name becomes a switch are all in
  launcher_rules, which the test suite asserts. What is left here is the part
  that touches the world: one HTTP probe, one detached child, one wait loop.
}
program fit_launcher;

{$mode objfpc}{$H+}

uses
    SysUtils, Classes, fphttpclient, process, launcher_rules;

{ True when something answers the health route. Any answer at all counts: a
  server that replies is a server that bound the port, and the client's own
  probe asks the same question the same way. }
function ServerAnswers(APort: word): boolean;
var
    C: TFPHTTPClient;
begin
    Result := False;
    C := TFPHTTPClient.Create(nil);
    try
        C.ConnectTimeout := LauncherProbeTimeoutMs;
        try
            C.Get(ServerHealthUrl(APort));
            Result := True;
        except
            Result := False;
        end;
    finally
        C.Free;
    end;
end;

{ The first candidate that is on disk, or '' when none is.

  The ORDER is the decision and it belongs to launcher_rules; asking the disk is
  not a decision, which is why it is here. }
function FirstThatExists(ACandidates: TStringList): string;
var
    i: integer;
begin
    Result := '';
    for i := 0 to ACandidates.Count - 1 do
        if FileExists(ACandidates[i]) then
            Exit(ExpandFileName(ACandidates[i]));
end;

function LocateServer(const ADir: string): string;
var
    Candidates: TStringList;
begin
    Candidates := TStringList.Create;
    try
        ServerBinaryCandidates(Candidates, ADir);
        Result := FirstThatExists(Candidates);
    finally
        Candidates.Free;
    end;
end;

function LocateClient(const ADir: string): string;
var
    Candidates: TStringList;
begin
    Candidates := TStringList.Create;
    try
        ClientBinaryCandidates(Candidates, ADir);
        Result := FirstThatExists(Candidates);
    finally
        Candidates.Free;
    end;
end;

{ Starts the compute server and leaves it running.

  DETACHED, and it outlives this process and the window: the second client
  reuses the first's server, and stopping it when one window closes would take
  the engine away from another mid-fit. Its output goes nowhere - the server
  keeps its own log file - because a pipe nobody reads eventually fills and
  blocks the child. }
procedure StartServer(const AExe: string; APort: word);
var
    P: TProcess;
begin
    P := TProcess.Create(nil);
    try
        P.Executable := AExe;
        BuildServerArgs(P.Parameters, APort);
        P.CurrentDirectory := ExtractFilePath(AExe);
        //  No wait option among them: the server runs until the session ends, so
        //  waiting on it would mean no window ever appeared.
        P.Options := [];
        P.ShowWindow := swoHIDE;
        try
            P.Execute;
        except
            //  Nothing to say and nowhere to say it: this is a GUI binary with no
            //  console. The client is started regardless and reports the server
            //  it could not reach, naming the address - which is the message the
            //  user can act on.
        end;
    finally
        P.Free;
    end;
end;

{ Waits for a freshly started server to bind, up to the budget the rules state. }
procedure WaitForServer(APort: word);
var
    i: integer;
begin
    for i := 1 to LauncherWaitTries do
    begin
        if ServerAnswers(APort) then
            Exit;
        Sleep(LauncherWaitIntervalMs);
    end;
end;

{ Starts the client, translating every argument this launcher was given.

  A file the desktop opened with Fit arrives as a bare path, which the client
  ignores - see launcher_rules.SwitchForArgument. }
procedure StartClient(const AExe: string);
var
    P: TProcess;
    i: integer;
    Switch: string;
begin
    P := TProcess.Create(nil);
    try
        P.Executable := AExe;
        for i := 1 to ParamCount do
        begin
            Switch := SwitchForArgument(ParamStr(i));
            if Switch <> '' then
                P.Parameters.Add(Switch);
        end;
        P.CurrentDirectory := ExtractFilePath(AExe);
        P.Options := [];
        P.Execute;
    finally
        P.Free;
    end;
end;

var
    Dir, ServerExe, ClientExe: string;
    Port: word;
begin
    Dir := ExtractFilePath(ExpandFileName(ParamStr(0)));
    Port := LauncherPort(GetEnvironmentVariable(LauncherPortVariable));

    if StepBeforeStartingTheClient(ServerAnswers(Port)) = lsStartOne then
    begin
        ServerExe := LocateServer(Dir);
        if ServerExe <> '' then
        begin
            StartServer(ServerExe, Port);
            WaitForServer(Port);
        end;
    end;

    ClientExe := LocateClient(Dir);
    //  Nothing to fall back on if the client is not there: this launcher IS the
    //  application as far as the desktop is concerned, and an installation
    //  missing its client is broken in a way no message here would mend.
    if ClientExe <> '' then
        StartClient(ClientExe);
end.
