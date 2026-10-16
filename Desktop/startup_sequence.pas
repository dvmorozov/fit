// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the application does when it starts: the decision of
recent_project, and the acting on it.)

WHY THIS IS NOT IN Fit.lpr, WHERE IT WAS. A program file is linked by no test.
recent_project decides what to open and is covered thirteen times over; the
lines that read the window, called that decision and acted on it were in Fit.lpr
- and one of them was wrong. It passed `@FileExists` as the existence check;
there is no one-argument FileExists, the address taken was the two-argument
UnicodeString overload, and the project last open could never be found again.
Every test passed throughout, because every test supplied its own check.

So the sequence lives here, where a test drives it exactly as the application
does, and Fit.lpr is a host implementation and one call.

THE HOST IS THREE THINGS THE WINDOW OWNS and one this unit will not do. Opening
a project and loading a data file need the window; what the last session left
behind is the window's settings; and saying so belongs to whoever is listening,
which is the log in the application and an assertion in a test. Nothing here
touches any of them directly.

TWO ENTRY POINTS, and the difference is the whole lesson of this unit:

  * RunStartup(project, infile, host) is what the APPLICATION calls. It supplies
    its own existence check, so there is no argument at the call site left to
    get wrong - which is what went wrong.
  * RunStartup(project, infile, host, exists) takes one, so a test can reach
    every branch without a disk.

The second is a convenience for tests and the first is the contract. Both are
covered, and the first is covered against files that are really there: a stub
cannot find the defect that was in supplying the real thing.
}
unit startup_sequence;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, recent_project;

type
    { What the start-up sequence needs from the application.

      A CORBA INTERFACE (-SIcorba, as everywhere here): no reference counting,
      so whoever creates the implementation frees it. }
    IStartupHost = interface
        ['{2F5C8A61-7D34-4E92-B1A8-6C0D93E4F157}']

        { What the last session left behind, or '' when there is none. The
          window holds it, in the settings it writes at shutdown. }
        function LastProject: string;

        { Open this project. False when it could not be opened - the workflow
          has already told the user why, so the answer is wanted here only to
          decide whether the project is worth offering again. }
        function OpenProject(const APath: string): boolean;
        { Start fresh with this data file. }
        procedure LoadDataFile(const APath: string);

        { Something the user should be able to find out about: a project that is
          no longer where it was. Never a reason to stop starting up. }
        procedure Warn(const AMessage: string);
        { The ordinary record of what happened, for someone reading a log
          afterwards. }
        procedure Note(const AMessage: string);

        { Stop offering the project last open.

          A CONVENIENCE THAT FAILED STOPS BEING ONE: a remembered project that
          is gone, or that will not open, would otherwise produce the same
          warning on every start-up for ever, and there is nothing the user can
          do about it from there. Only ever the REMEMBERED one - a project named
          on the command line is an instruction, and a mistyped switch must not
          clear a setting the user never mentioned. }
        procedure ForgetLastProject;
    end;

{ Does what the two command-line switches and the remembered project say, using
  the real file system. What the application calls. }
procedure RunStartup(const AProjectSwitch, AInFileSwitch: string;
    AHost: IStartupHost);

{ The same, with the existence check supplied - which is what makes every branch
  reachable from a test with no disk. }
procedure RunStartup(const AProjectSwitch, AInFileSwitch: string;
    AHost: IStartupHost; AExists: TPathExists);

implementation

procedure RunStartup(const AProjectSwitch, AInFileSwitch: string;
    AHost: IStartupHost);
begin
    RunStartup(AProjectSwitch, AInFileSwitch, AHost, @DefaultPathExists);
end;

procedure RunStartup(const AProjectSwitch, AInFileSwitch: string;
    AHost: IStartupHost; AExists: TPathExists);
var
    Remembered: string;
    Plan: TStartupPlan;
begin
    Remembered := AHost.LastProject;

    //  THE THREE INPUTS, BEFORE THE DECISION. Without this a start-up that
    //  opens nothing says nothing, and "no project was remembered" reads
    //  exactly like "the auto-open is broken" - which is how the one defect
    //  this sequence has had was reported.
    AHost.Note('start-up: /PROJECT="' + AProjectSwitch + '" /INFILE="' +
        AInFileSwitch + '" last project "' + Remembered + '"');

    //  THE DECISION IS NOT HERE. Which of the three wins, and what a missing
    //  file means, is recent_project's - so it is covered on its own, and this
    //  reads as a sequence rather than as a rule.
    Plan := PlanStartup(AProjectSwitch, AInFileSwitch, Remembered, AExists);

    if Plan.Warning <> '' then
        AHost.Warn(Plan.Warning);

    //  THE REMEMBERED PROJECT IS GONE. Nothing was asked for on the command
    //  line and the plan opens nothing, so the warning above is about the one
    //  the settings hold - and it is the last time it will be given.
    if (Plan.Choice = scNothing) and (Plan.Warning <> '') and
        (Trim(AProjectSwitch) = '') and (Trim(AInFileSwitch) = '') then
        AHost.ForgetLastProject;

    //  ONE ACTION. A case that fell through to a second branch would load data
    //  into a document nobody asked to open.
    case Plan.Choice of
        scProject:
        begin
            AHost.Note('opening project ' + Plan.Path);
            //  AND IF IT WOULD NOT OPEN, it is not offered again either - but
            //  only when it was the remembered one rather than an instruction.
            if not AHost.OpenProject(Plan.Path) and
                (Trim(AProjectSwitch) = '') then
                AHost.ForgetLastProject;
        end;
        scDataFile:
        begin
            AHost.Note('/INFILE: opening ' + Plan.Path);
            AHost.LoadDataFile(Plan.Path);
        end;
        //  scNothing: an empty window, and the note above already says why.
    end;
end;

end.
