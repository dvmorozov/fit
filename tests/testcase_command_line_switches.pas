// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client accepts on its command line - the other half of what
the launcher produces.)

WHY THIS IS A UNIT TEST AND NOT A NOTE IN A COMMENT. The rule was written inside
Fit.lpr, as a function nested in the program's main block: a program file is
linked by no test, which is where its one shipped defect lived, and this rule is
one nothing would notice being wrong. An argument the client does not recognise
is not refused - it is passed over in silence. So a desktop that hands over a
bare path, which is what Explorer's %1 and macOS's Open With both do, opens an
empty window and reports nothing at all.

The launcher exists to translate, and launcher_rules.SwitchForArgument is the
translation. This is the assertion that the two halves meet: what the launcher
produces is what the client reads back.
}
unit testcase_command_line_switches;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, command_line_switches,
    launcher_rules;

type
    TCommandLineSwitchesTest = class(TTestCase)
    private
        FArgs: TStringList;
        { The value of AName in FArgs, or '' when it is not there. }
        function ValueOf(const AName: string): string;
        function Found(const AName: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ASwitchIsFoundByName;
        procedure TheValueIsWhatFollowsTheEquals;
        procedure APathWithSpacesArrivesWhole;
        procedure ABackslashStartsASwitchToo;
        procedure ASwitchWithNoValueIsStillFound;
        procedure AnAbsentSwitchIsNotFound;
        procedure ABarePathIsNotASwitch;
        procedure AnEmptyArgumentIsSurvived;
        procedure TheFirstOfTwoWins;

        //  The two halves, meeting.
        procedure WhatTheLauncherMakesOfAProjectIsWhatTheClientReads;
        procedure WhatTheLauncherMakesOfADataFileIsWhatTheClientReads;
    end;

implementation

procedure TCommandLineSwitchesTest.SetUp;
begin
    FArgs := TStringList.Create;
end;

procedure TCommandLineSwitchesTest.TearDown;
begin
    FArgs.Free;
    FArgs := nil;
end;

function TCommandLineSwitchesTest.ValueOf(const AName: string): string;
begin
    if not SwitchFound(FArgs, AName, Result) then
        Result := '';
end;

function TCommandLineSwitchesTest.Found(const AName: string): boolean;
var
    Ignored: string;
begin
    Result := SwitchFound(FArgs, AName, Ignored);
end;

procedure TCommandLineSwitchesTest.ASwitchIsFoundByName;
begin
    FArgs.Add('/PROJECT=a.fitproj');
    AssertTrue('found', Found('PROJECT'));
end;

procedure TCommandLineSwitchesTest.TheValueIsWhatFollowsTheEquals;
begin
    FArgs.Add('/INFILE=spectrum.dat');
    AssertEquals('value', 'spectrum.dat', ValueOf('INFILE'));
end;

procedure TCommandLineSwitchesTest.APathWithSpacesArrivesWhole;
begin
    //  The quoting is the shell's business and is gone by the time the argument
    //  is one string; splitting on a space here would lose half of every path
    //  under Program Files or Documents.
    FArgs.Add('/PROJECT=C:\a b\p.fitproj');
    AssertEquals('whole', 'C:\a b\p.fitproj', ValueOf('PROJECT'));
end;

procedure TCommandLineSwitchesTest.ABackslashStartsASwitchToo;
begin
    //  What the IDE's run parameters have always allowed.
    FArgs.Add('\INFILE=x.dat');
    AssertEquals('value', 'x.dat', ValueOf('INFILE'));
end;

procedure TCommandLineSwitchesTest.ASwitchWithNoValueIsStillFound;
begin
    FArgs.Add('/PROJECT');
    AssertTrue('found', Found('PROJECT'));
    AssertEquals('no value', '', ValueOf('PROJECT'));
end;

procedure TCommandLineSwitchesTest.AnAbsentSwitchIsNotFound;
begin
    FArgs.Add('/LOG_LEVEL=warning');
    AssertFalse('not there', Found('PROJECT'));
end;

procedure TCommandLineSwitchesTest.ABarePathIsNotASwitch;
begin
    //  THE DEFECT THE LAUNCHER EXISTS TO PREVENT, asserted from the client's
    //  side: this is what a desktop hands over when a file is opened with Fit,
    //  and the client cannot see it. Nothing here is wrong - the rule is
    //  deliberate - but it is the reason nothing may point a file association
    //  straight at the client.
    FArgs.Add('C:\data\p.fitproj');
    AssertFalse('a bare path names no switch', Found('PROJECT'));
end;

procedure TCommandLineSwitchesTest.AnEmptyArgumentIsSurvived;
begin
    //  A shell expanding an unset variable produces one, and reading its first
    //  character is how this rule used to be written.
    FArgs.Add('');
    FArgs.Add('/PROJECT=a.fitproj');
    AssertEquals('still found', 'a.fitproj', ValueOf('PROJECT'));
end;

procedure TCommandLineSwitchesTest.TheFirstOfTwoWins;
begin
    //  Stated rather than discovered: the search stops at the first match, so a
    //  repeated switch takes its earliest value.
    FArgs.Add('/INFILE=first.dat');
    FArgs.Add('/INFILE=second.dat');
    AssertEquals('first', 'first.dat', ValueOf('INFILE'));
end;

procedure TCommandLineSwitchesTest.WhatTheLauncherMakesOfAProjectIsWhatTheClientReads;
begin
    //  THE ROUND TRIP. The launcher turns the desktop's bare path into a switch;
    //  this is the client reading that switch back and getting the same path. A
    //  change to either side that the other does not follow fails here.
    FArgs.Add(SwitchForArgument('C:\a b\p.fitproj'));
    AssertEquals('the project', 'C:\a b\p.fitproj', ValueOf('PROJECT'));
end;

procedure TCommandLineSwitchesTest.WhatTheLauncherMakesOfADataFileIsWhatTheClientReads;
begin
    FArgs.Add(SwitchForArgument('/home/u/spectrum.dat'));
    AssertEquals('the data file', '/home/u/spectrum.dat', ValueOf('INFILE'));
end;

initialization
    //  A unit test: it reads a list of strings and nothing else.
    RegisterTest('unit', TCommandLineSwitchesTest);
end.
