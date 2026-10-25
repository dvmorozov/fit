// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Opening a web page in the user's browser, the one way this program
does it.)

WHY NOT THE LCL'S OpenURL. On Linux it finds a browser launcher (xdg-open and
the like) and starts it through the LCL's own process code - a bare TProcess,
which on Unix hands the child every descriptor this process has open that is
not close-on-exec, and the RTL marks none. xdg-open starts the browser, and the
browser keeps the copies for as long as it runs: a window's sockets, its
server's pipe, a listener that should have closed with the window. That is the
defect child_process exists to prevent, reached by a path that names no
TProcess, so its script test could not see it. And it parses the command line
it builds, so a URL with a quote or a space in it was split.

ONE FUNCTION, OpenInBrowser, and tools/build-tests/child_process.tests.ps1
refuses OpenURL and OpenDocument anywhere else.

PER PLATFORM:
  * Linux and the other Unixes: xdg-open, the freedesktop.org launcher every
    desktop provides, started as a TChildProcess - which hands it its standard
    streams and nothing else - with the URL as one argument, never parsed.
  * macOS: open, the system's own launcher, the same way. The LCL's Cocoa
    OpenURL asks NSWorkspace instead, which starts no child of ours; open is
    chosen anyway so both Unixes take one path, testable without Cocoa (the
    test build for macOS uses the nogui widget set, which has none).
  * Windows: the LCL's OpenURL, which is ShellExecute. The shell starts the
    browser, not this process, so nothing of ours is inherited, and the system
    decides which browser - the platform behaviour, kept.

NOT WAITED FOR. The launcher hands the page to a browser and exits, but on a
desktop without one it may run the browser itself in the foreground; waiting
would freeze the window until the browser closed.
}
unit browser_launch;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, child_process;

const
    { The program that opens a URL on this platform, found on the PATH. Empty
      on Windows, where the shell does it. }
{$IFDEF DARWIN}
    BrowserLauncher = 'open';
{$ELSE}
  {$IFDEF UNIX}
    BrowserLauncher = 'xdg-open';
  {$ELSE}
    BrowserLauncher = '';
  {$ENDIF}
{$ENDIF}

{ The process that has ALauncherPath open AUrl - built, not started: the URL is
  its one argument, and nothing is waited for or captured. The caller frees it. }
function NewBrowserProcess(const ALauncherPath, AUrl: string): TChildProcess;

{ Opens AUrl in the user's browser. False when no launcher was found or it could
  not be started; nothing is raised. }
function OpenInBrowser(const AUrl: string): boolean;

implementation

{$IFDEF MSWINDOWS}
uses
    LCLIntf;
{$ENDIF}

function NewBrowserProcess(const ALauncherPath, AUrl: string): TChildProcess;
begin
    Result := TChildProcess.Create(nil);
    Result.Executable := ALauncherPath;
    Result.Parameters.Add(AUrl);
    //  No pipes: the launcher's output is nobody's, and on Windows a child
    //  with no pipes inherits no handle (child_process).
    Result.Options := [];
end;

function OpenInBrowser(const AUrl: string): boolean;
{$IFDEF MSWINDOWS}
begin
    Result := OpenURL(AUrl);
end;
{$ELSE}
var
    Launcher: string;
    P: TChildProcess;
begin
    Result := False;
    Launcher := ExeSearch(BrowserLauncher, GetEnvironmentVariable('PATH'));
    if Launcher = '' then
        Exit;
    P := NewBrowserProcess(Launcher, AUrl);
    try
        try
            P.Execute;
            Result := True;
        except
            //  A launcher that cannot be started costs the page, never the
            //  window: the caller says so.
            Result := False;
        end;
    finally
        P.Free;
    end;
end;
{$ENDIF}

end.
