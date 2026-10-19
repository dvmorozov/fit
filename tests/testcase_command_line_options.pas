// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client accepts on its command line, read the GNU way:
--name=value.)

WHY THE RULE CHANGED. The old one (command_line_switches, now gone) matched a
switch's name ANYWHERE in an argument, its value included, and stopped at the
first match. So /INFILE=/tmp/PROJECT/x.dat also counted as /PROJECT, a longer
switch whose name began with a shorter one counted as both, and a switch given
twice was read once. Scripting needs a name read whole and an option that may
repeat (--param), which is what this suite asserts first.

WHAT IS KEPT FROM THE OLD SUITE, because each was a defect once: a path with
spaces arrives whole, a bare path names no option (the launcher translates it),
an empty argument is survived, and what the launcher makes of a desktop's path
is what the client reads back.
}
unit testcase_command_line_options;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, command_line_options,
    launcher_rules, int_automation;

type
    TCommandLineOptionsTest = class(TTestCase)
    private
        FArgs: TStringList;
        FTable: TCommandLineOptions;
        FLine: TCommandLine;
        { FArgs read against FTable; freed in TearDown. }
        function Read: TCommandLine;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnOptionIsFoundByItsWholeName;
        procedure TheValueIsEverythingAfterTheFirstEquals;
        procedure APathWithSpacesArrivesWhole;
        procedure AFlagIsFoundWithoutAValue;
        procedure ANameInsideAnotherOptionsValueIsNotThatOption;
        procedure ANameThatBeginsALongerNameIsNotThatOption;
        procedure ARepeatableOptionKeepsEveryValueInOrder;
        procedure AnOptionGivenTwiceThatMayNotRepeatIsAnError;
        procedure AnUnknownOptionIsNamedWithTheWayToTheList;
        procedure AFlagGivenAValueIsAnError;
        procedure AnOptionThatNeedsAValueWithoutOneIsAnError;
        procedure EveryMistakeIsReportedAtOnce;
        procedure ABarePathIsNotAnOption;
        procedure AnEmptyArgumentIsSurvived;
        procedure TheToolkitsSingleDashArgumentsAreLeftAlone;
        procedure TheDebugLoggersOwnOptionsAreLeftAlone;

        //  The old spelling, for one release.
        procedure TheOldSpellingStillWorksAndSaysWhatReplacesIt;
        procedure ABackslashStartsAnOldSpellingToo;
        procedure AnOldSpellingIsMatchedWholeNotInsideALongerName;
        procedure AnAbsoluteUnixPathIsNotAnOldSpelling;
        procedure TheOldNameOfAnOptionIsItsNameInCapitals;

        //  What the help lists.
        procedure TheHelpListsEveryOptionWithItsValueAndItsText;

        //  What the client does with it.
        procedure AnInteractiveStartWithAMistakeStillStarts;
        procedure AnUnattendedRunWithAMistakeIsRefused;
        procedure AskingForHelpWinsOverAMistake;
        procedure AskingForTheVersionCloses;

        //  What starts a run nobody watches, and what runs after start-up.
        procedure AnUnattendedOptionMakesTheRunUnattended;
        procedure AnOptionThatStartsFreshMakesTheRunStartFresh;
        procedure TheWindowCheckingItselfIsUnattended;
        procedure AStartupActionRunsOnlyWhenItsOptionIsGiven;

        //  The client's own table.
        procedure TheClientKnowsEveryOptionItHasAlwaysRead;

        //  The two halves, meeting.
        procedure WhatTheLauncherMakesOfAProjectIsWhatTheClientReads;
        procedure WhatTheLauncherMakesOfADataFileIsWhatTheClientReads;
    end;

implementation

procedure TCommandLineOptionsTest.SetUp;
begin
    FArgs := TStringList.Create;
    //  A TABLE OF ITS OWN, not the client's: what the parser does must not
    //  depend on which options a build happens to register.
    FTable := TCommandLineOptions.Create;
    FTable.Add('project', 'file', 'Opens a project.');
    FTable.Add('infile', 'file', 'Opens a data file.');
    FTable.Add('check-ui', '', 'Checks the window.');
    FTable.Add('script', 'file', 'Runs a script.');
    FTable.Add('script-check', 'file', 'Checks a script.');
    FTable.Add('param', 'name=value', 'Sets a parameter.', True);
    FLine := nil;
end;

procedure TCommandLineOptionsTest.TearDown;
begin
    FLine.Free;
    FLine := nil;
    FTable.Free;
    FTable := nil;
    FArgs.Free;
    FArgs := nil;
end;

function TCommandLineOptionsTest.Read: TCommandLine;
begin
    FLine.Free;
    FLine := TCommandLine.Create(FArgs, FTable);
    Result := FLine;
end;

procedure TCommandLineOptionsTest.AnOptionIsFoundByItsWholeName;
begin
    FArgs.Add('--project=a.fitproj');
    AssertTrue('found', Read.Has('project'));
    AssertEquals('value', 'a.fitproj', FLine.Value('project'));
    AssertFalse('not another', FLine.Has('infile'));
    AssertEquals('no errors', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.TheValueIsEverythingAfterTheFirstEquals;
begin
    //  An equals sign down a path is still the path.
    FArgs.Add('--infile=/home/a=b/x.dat');
    AssertEquals('value', '/home/a=b/x.dat', Read.Value('infile'));
end;

procedure TCommandLineOptionsTest.APathWithSpacesArrivesWhole;
begin
    //  The quoting is the shell's business and is gone by the time the argument
    //  is one string; splitting on a space would lose half of every path under
    //  Program Files or Documents.
    FArgs.Add('--project=C:\a b\p.fitproj');
    AssertEquals('whole', 'C:\a b\p.fitproj', Read.Value('project'));
end;

procedure TCommandLineOptionsTest.AFlagIsFoundWithoutAValue;
begin
    FArgs.Add('--check-ui');
    AssertTrue('found', Read.Has('check-ui'));
    AssertEquals('no value', '', FLine.Value('check-ui'));
    AssertEquals('no errors', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.ANameInsideAnotherOptionsValueIsNotThatOption;
begin
    //  THE OLD RULE'S DEFECT: a name was matched anywhere in the argument, so
    //  this one opened a data file AND counted as a project.
    FArgs.Add('--infile=/tmp/project/x.dat');
    AssertTrue('the data file', Read.Has('infile'));
    AssertFalse('no project', FLine.Has('project'));
end;

procedure TCommandLineOptionsTest.ANameThatBeginsALongerNameIsNotThatOption;
begin
    //  --script-check must not also run the script it was asked only to check.
    FArgs.Add('--script-check=s.json');
    AssertTrue('the check', Read.Has('script-check'));
    AssertFalse('not a run', FLine.Has('script'));
end;

procedure TCommandLineOptionsTest.ARepeatableOptionKeepsEveryValueInOrder;
var
    Values: TStringArray;
begin
    //  The old rule read the first of two and dropped the rest.
    FArgs.Add('--param=series=DGS10');
    FArgs.Add('--param=range=5y');
    FArgs.Add('--param=out=/w/a.png');
    Values := Read.Values('param');
    AssertEquals('count', 3, Length(Values));
    AssertEquals('first', 'series=DGS10', Values[0]);
    AssertEquals('second', 'range=5y', Values[1]);
    AssertEquals('third', 'out=/w/a.png', Values[2]);
    AssertEquals('no errors', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.AnOptionGivenTwiceThatMayNotRepeatIsAnError;
begin
    //  Which of two projects to open has no right answer, so neither is guessed.
    FArgs.Add('--project=a.fitproj');
    FArgs.Add('--project=b.fitproj');
    AssertEquals('one error', 1, Read.Errors.Count);
    AssertTrue('names it', Pos('--project', FLine.Errors[0]) > 0);
end;

procedure TCommandLineOptionsTest.AnUnknownOptionIsNamedWithTheWayToTheList;
begin
    FArgs.Add('--porject=a.fitproj');
    AssertEquals('one error', 1, Read.Errors.Count);
    AssertTrue('names it', Pos('--porject', FLine.Errors[0]) > 0);
    AssertTrue('says where the list is', Pos('--help', FLine.Errors[0]) > 0);
    AssertFalse('not taken as anything', FLine.Has('project'));
end;

procedure TCommandLineOptionsTest.AFlagGivenAValueIsAnError;
begin
    FArgs.Add('--check-ui=yes');
    AssertEquals('one error', 1, Read.Errors.Count);
    AssertTrue('names it', Pos('--check-ui', FLine.Errors[0]) > 0);
end;

procedure TCommandLineOptionsTest.AnOptionThatNeedsAValueWithoutOneIsAnError;
begin
    FArgs.Add('--project');
    AssertEquals('one error', 1, Read.Errors.Count);
    //  And it shows the form that works.
    AssertTrue('shows the form', Pos('--project=<file>', FLine.Errors[0]) > 0);
end;

procedure TCommandLineOptionsTest.EveryMistakeIsReportedAtOnce;
begin
    //  A script's command line is fixed in one go, not one mistake per run.
    FArgs.Add('--porject=a.fitproj');
    FArgs.Add('--check-ui=yes');
    FArgs.Add('--infile');
    AssertEquals('all three', 3, Read.Errors.Count);
end;

procedure TCommandLineOptionsTest.ABarePathIsNotAnOption;
begin
    //  What a desktop hands over when a file is opened with Fit. It names no
    //  option - which is why nothing may point a file association straight at
    //  the client, and the launcher translates.
    FArgs.Add('C:\data\p.fitproj');
    AssertFalse('no project', Read.Has('project'));
    AssertEquals('and no complaint', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.AnEmptyArgumentIsSurvived;
begin
    //  A shell expanding an unset variable produces one.
    FArgs.Add('');
    FArgs.Add('--project=a.fitproj');
    AssertEquals('still found', 'a.fitproj', Read.Value('project'));
    AssertEquals('no errors', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.TheToolkitsSingleDashArgumentsAreLeftAlone;
begin
    //  macOS adds -psn_<number> to a program the Finder starts, and Qt reads
    //  -platform and -style of its own. The double dash is ours; the single
    //  one is not, and refusing it would refuse a start nobody can change.
    FArgs.Add('-psn_0_1234567');
    FArgs.Add('-platform');
    FArgs.Add('offscreen');
    AssertEquals('no errors', 0, Read.Errors.Count);
end;

procedure TCommandLineOptionsTest.TheDebugLoggersOwnOptionsAreLeftAlone;
begin
    //  The LCL's debug logger reads --debug-log and --debug-enable itself.
    FArgs.Add('--debug-log=/tmp/lcl.log');
    FArgs.Add('--debug-enable=LCL_LAYOUT');
    AssertEquals('no errors', 0, Read.Errors.Count);
end;

procedure TCommandLineOptionsTest.TheOldSpellingStillWorksAndSaysWhatReplacesIt;
begin
    //  For one release: a shortcut, a script or an IDE run parameter written
    //  the old way keeps working, and the log says what to write instead.
    FArgs.Add('/INFILE=spectrum.dat');
    AssertEquals('value', 'spectrum.dat', Read.Value('infile'));
    AssertEquals('one note', 1, FLine.Notes.Count);
    AssertTrue('names the old', Pos('/INFILE', FLine.Notes[0]) > 0);
    AssertTrue('names the new', Pos('--infile', FLine.Notes[0]) > 0);
    AssertEquals('not an error', 0, FLine.Errors.Count);
end;

procedure TCommandLineOptionsTest.ABackslashStartsAnOldSpellingToo;
begin
    //  What the IDE's run parameters have always allowed.
    FArgs.Add('\PROJECT=x.fitproj');
    AssertEquals('value', 'x.fitproj', Read.Value('project'));
end;

procedure TCommandLineOptionsTest.AnOldSpellingIsMatchedWholeNotInsideALongerName;
begin
    FArgs.Add('/SCRIPT_CHECK=s.json');
    AssertTrue('the check', Read.Has('script-check'));
    AssertFalse('not a run', FLine.Has('script'));
end;

procedure TCommandLineOptionsTest.AnAbsoluteUnixPathIsNotAnOldSpelling;
begin
    //  On Unix every absolute path starts with a slash.
    FArgs.Add('/home/u/PROJECT');
    FArgs.Add('/PROJECTS/x.dat');
    AssertFalse('no project', Read.Has('project'));
    AssertEquals('no complaint', 0, FLine.Errors.Count);
    AssertEquals('no note', 0, FLine.Notes.Count);
end;

procedure TCommandLineOptionsTest.TheOldNameOfAnOptionIsItsNameInCapitals;
begin
    AssertEquals('check-ui', 'CHECK_UI', OldSpellingOf('check-ui'));
    AssertEquals('infile', 'INFILE', OldSpellingOf('infile'));
end;

procedure TCommandLineOptionsTest.TheHelpListsEveryOptionWithItsValueAndItsText;
var
    Help: string;
begin
    Help := CommandLineHelp('Fit', FTable);
    AssertTrue('a valued option', Pos('--project=<file>', Help) > 0);
    AssertTrue('its text', Pos('Opens a project.', Help) > 0);
    AssertTrue('a flag', Pos('--check-ui', Help) > 0);
    AssertFalse('a flag takes no value', Pos('--check-ui=', Help) > 0);
    AssertTrue('a repeatable one says so', Pos('may be given more than once', Help) > 0);
end;

procedure TCommandLineOptionsTest.AnInteractiveStartWithAMistakeStillStarts;
begin
    //  The option was mistyped, not the program: the window opens, and the log
    //  says what was not understood - where the old rule said nothing.
    FArgs.Add('--porject=a.fitproj');
    AssertTrue(CommandLineVerdict(Read, False) = cvStart);
end;

procedure TCommandLineOptionsTest.AnUnattendedRunWithAMistakeIsRefused;
begin
    //  Nobody reads a warning in an unattended run, and the caller needs an
    //  exit code that says it did not do what was asked.
    FArgs.Add('--porject=a.fitproj');
    AssertTrue(CommandLineVerdict(Read, True) = cvRefuse);
    FArgs.Clear;
    FArgs.Add('--project=a.fitproj');
    AssertTrue('and without one it runs', CommandLineVerdict(Read, True) = cvStart);
end;

procedure TCommandLineOptionsTest.AskingForHelpWinsOverAMistake;
var
    Table: TCommandLineOptions;
begin
    Table := TCommandLineOptions.Create;
    try
        Table.Add('help', '', 'Prints this list.');
        Table.Add('version', '', 'Prints the version.');
        FArgs.Add('--porject=a.fitproj');
        FArgs.Add('--help');
        FLine.Free;
        FLine := TCommandLine.Create(FArgs, Table);
        AssertTrue(CommandLineVerdict(FLine, True) = cvHelp);
    finally
        FLine.Free;
        FLine := nil;
        Table.Free;
    end;
end;

procedure TCommandLineOptionsTest.AskingForTheVersionCloses;
var
    Table: TCommandLineOptions;
begin
    Table := TCommandLineOptions.Create;
    try
        Table.Add('version', '', 'Prints the version.');
        FArgs.Add('--version');
        FLine.Free;
        FLine := TCommandLine.Create(FArgs, Table);
        AssertTrue(CommandLineVerdict(FLine, False) = cvVersion);
    finally
        FLine.Free;
        FLine := nil;
        Table.Free;
    end;
end;

procedure TCommandLineOptionsTest.AnUnattendedOptionMakesTheRunUnattended;
begin
    FTable.Add('run-me', 'file', 'Runs something nobody watches.', False, True);
    FArgs.Add('--project=a.fitproj');
    AssertFalse('an ordinary start', UnattendedRequested(Read, FTable));
    FArgs.Add('--run-me=x');
    AssertTrue('a run nobody watches', UnattendedRequested(Read, FTable));
end;

{ A script must do the same on every machine, so it may not begin in whatever
  project that machine last had open: the scenario suite's synthetic run began
  in the previous scenario's peak project, and its curve type was refused. }
procedure TCommandLineOptionsTest.AnOptionThatStartsFreshMakesTheRunStartFresh;
begin
    FTable.Add('record-me', 'dir', 'Records the session as it was left.', False, True);
    FTable.Add('script-me', 'file', 'Runs steps from nothing.', False, True, True);
    FArgs.Add('--record-me=x');
    AssertFalse('unattended is not fresh: a recording shows the last project',
        FreshStartRequested(Read, FTable));
    FArgs.Add('--script-me=y');
    AssertTrue('a run that must start from nothing',
        FreshStartRequested(Read, FTable));
end;

procedure TCommandLineOptionsTest.TheWindowCheckingItselfIsUnattended;
var
    Args: TStringList;
    Line: TCommandLine;
begin
    //  What Fit.lpr asks to decide whether notices, the update check and the
    //  end-of-run dialogs are skipped.
    Args := TStringList.Create;
    try
        Args.Add('--check-ui');
        Line := TCommandLine.Create(Args, ClientOptions);
        try
            AssertTrue(UnattendedRequested(Line, ClientOptions));
        finally
            Line.Free;
        end;
    finally
        Args.Free;
    end;
end;

var
    StartupActionRuns: integer = 0;

procedure CountStartupAction(ALine: TCommandLine; AHost: IAutomationHost);
begin
    Inc(StartupActionRuns);
end;

procedure TCommandLineOptionsTest.AStartupActionRunsOnlyWhenItsOptionIsGiven;
var
    Line: TCommandLine;
begin
    //  The global registry, as a module uses it, under an option no build
    //  has: nothing in production can ever give it.
    RegisterCommandLineOption('unit-test-startup', '', 'Only a test gives this.');
    RegisterStartupAction('unit-test-startup', @CountStartupAction);
    StartupActionRuns := 0;
    FArgs.Add('--project=a.fitproj');
    Line := TCommandLine.Create(FArgs, ClientOptions);
    try
        AssertEquals('not given, not run', 0, RunStartupActions(Line, nil));
    finally
        Line.Free;
    end;
    FArgs.Add('--unit-test-startup');
    Line := TCommandLine.Create(FArgs, ClientOptions);
    try
        AssertEquals('given, run once', 1, RunStartupActions(Line, nil));
        AssertEquals(1, StartupActionRuns);
    finally
        Line.Free;
    end;
end;

procedure TCommandLineOptionsTest.TheClientKnowsEveryOptionItHasAlwaysRead;
const
    //  TYPED, not an inline [...] of literals: FPC sizes such a list to its
    //  first element, and 'log-level' arrived as 'log-lev'.
    Always: array[0..7] of string = ('project', 'infile', 'log-level', 'dpi',
        'check-ui', 'write-params-log', 'help', 'version');
var
    Name: string;
begin
    //  THE CLIENT'S OWN TABLE: every switch Fit.lpr has read, so none of them
    //  stops working silently when the reading moved here.
    for Name in Always do
        AssertTrue(Name, ClientOptions.IndexOf(Name) >= 0);
end;

procedure TCommandLineOptionsTest.WhatTheLauncherMakesOfAProjectIsWhatTheClientReads;
begin
    //  THE ROUND TRIP. The launcher turns the desktop's bare path into an
    //  option; this is the client reading it back and getting the same path.
    FArgs.Add(SwitchForArgument('C:\a b\p.fitproj'));
    AssertEquals('the project', 'C:\a b\p.fitproj', Read.Value('project'));
    AssertEquals('in the current spelling', 0, FLine.Notes.Count);
end;

procedure TCommandLineOptionsTest.WhatTheLauncherMakesOfADataFileIsWhatTheClientReads;
begin
    FArgs.Add(SwitchForArgument('/home/u/spectrum.dat'));
    AssertEquals('the data file', '/home/u/spectrum.dat', Read.Value('infile'));
    AssertEquals('in the current spelling', 0, FLine.Notes.Count);
end;

initialization
    //  A unit test: it reads lists of strings and nothing else.
    RegisterTest('unit', TCommandLineOptionsTest);
end.
