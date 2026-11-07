// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a run nobody watches may ask of the window: run a command by its
id, wait until it has finished, and close with an exit code.)

A FIELD-NEUTRAL SEAM. The window's commands are already rows of one table with
stable ids (ui_commands), and a click on any of them runs its action's Execute
or its module's Command. A module that drives the window - a script runner -
needs exactly that and no more: the same Execute, by id, and to know when what
it started has finished (a fit runs asynchronously; most commands do not). It
must not reach the window's controls, which is why this is an interface and the
window implements it.

Questions the commands ask are answered through answer_source, not here.
}
unit int_automation;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_ui_host;

type
    { What happened when a command was asked to run. }
    TCommandRun = (
        { It ran and has finished. }
        crDone,
        { It started something that is still running - a fit, a computation.
          WhenIdle says when it has ended. }
        crRunning,
        { No row of the window's command table has that id. }
        crUnknown,
        { The command exists but is not offered now - nothing open, a fit
          running - exactly as its menu entry would be greyed. }
        crUnavailable);

    TIdleEvent = procedure of object;

    IAutomationHost = interface
        ['{5C2F8E1A-7B3D-4E6F-A1C2-9D8E7F6A5B42}']
        { Runs the command the row AId names: the framework's row id
          (NewFromDataSource, DoAllAutomatically) or a module's
          (module.command, or the command's own id when only one module has
          it), through the same Execute a click runs. }
        function RunCommand(const AId: string): TCommandRun;
        { Whether something a command started is still running. }
        function CommandRunning: boolean;
        { Calls AThen from the window's own loop once nothing is running - at
          once (queued) when nothing is. One pending call at a time; a second
          replaces the first. }
        procedure WhenIdle(AThen: TIdleEvent);
        { Ends the application with AExitCode, without asking to save. }
        procedure Quit(AExitCode: integer);
        { The ids RunCommand accepts in this window, in the table's order: what
          a script is checked against before anything runs. }
        function CommandIds: TStringArray;
        { The window as modules always see it - for a licensed module's
          LicenceAllowsUse, which asks through it. }
        function UiHost: IUiHost;
    end;

implementation

end.
