// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A web page is opened by a child that holds nothing of ours, with the
address as one argument.)

The process is built and inspected, never started: starting it would open a
browser on the machine the tests run on. That the class hands the child nothing
but its standard streams is testcase_child_process's; that nothing else opens a
page is child_process.tests.ps1's.

ONLY WHERE A CHILD IS STARTED. On Windows the page is opened by the shell (the
LCL's OpenURL, which is ShellExecute): no process of ours is built, so there is
nothing to inspect, and EachPlatformNamesItsOwnLauncher is all that is said
there - that this program names no launcher of its own.
}
unit testcase_browser_launch;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, process, child_process,
    browser_launch;

type
    TBrowserLaunchTest = class(TTestCase)
    published
{$IFNDEF MSWINDOWS}
        procedure TheAddressIsOneArgumentHoweverItIsWritten;
        procedure NothingIsCapturedOrWaitedFor;
{$ENDIF}
        procedure EachPlatformNamesItsOwnLauncher;
    end;

implementation

{$IFNDEF MSWINDOWS}
{ The LCL built a command line and parsed it back, so an address with a space,
  a quote or an ampersand reached the launcher in pieces. }
procedure TBrowserLaunchTest.TheAddressIsOneArgumentHoweverItIsWritten;
const
    Url = 'https://example.org/a b?q="x y"&r=''1'';ls';
var
    P: TChildProcess;
begin
    P := NewBrowserProcess('/usr/bin/xdg-open', Url);
    try
        AssertEquals('/usr/bin/xdg-open', P.Executable);
        AssertEquals('one argument', 1, P.Parameters.Count);
        AssertEquals(Url, P.Parameters[0]);
    finally
        P.Free;
    end;
end;

{ A launcher may run the browser in the foreground on a desktop without one:
  waited for, it would freeze the window until the browser closed. What the
  child is then handed is TChildProcess's decision, made as it starts
  (testcase_child_process). }
procedure TBrowserLaunchTest.NothingIsCapturedOrWaitedFor;
var
    P: TChildProcess;
begin
    P := NewBrowserProcess('/usr/bin/xdg-open', 'https://example.org/');
    try
        AssertFalse('not waited for', poWaitOnExit in P.Options);
        AssertFalse('nothing captured', poUsePipes in P.Options);
    finally
        P.Free;
    end;
end;
{$ENDIF}

procedure TBrowserLaunchTest.EachPlatformNamesItsOwnLauncher;
begin
{$IFDEF DARWIN}
    AssertEquals('open', BrowserLauncher);
{$ELSE}
  {$IFDEF UNIX}
    AssertEquals('xdg-open', BrowserLauncher);
  {$ELSE}
    AssertEquals('the shell opens it', '', BrowserLauncher);
  {$ENDIF}
{$ENDIF}
end;

initialization
    RegisterTest('unit', TBrowserLaunchTest);
end.
