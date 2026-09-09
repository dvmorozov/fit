// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the application opens when it starts, and what a project that is
no longer there means.)

THE DECISION HAS FOUR INPUTS and one of them is a file system, which is exactly
why it is a function rather than a block inside the startup sequence: the
existence check is passed in, so every branch below is reachable without writing
a file, and the whole thing is a unit test.

WHY THE PRECEDENCE IS WHAT IT IS.

  * an explicit /PROJECT= is the user saying which project, now. Nothing may
    override it, including a remembered one.
  * /INFILE= means "start fresh with this data" - it has meant that since before
    projects existed, and it must go on meaning it. Opening the last project and
    then loading a data file into it would silently modify a document the user
    did not ask to open.
  * only then the last project, which is a convenience and not an instruction.

AND WHY A MISSING FILE IS A WARNING RATHER THAN A FAILURE. The last project may
have been deleted, renamed, or be on a drive that is not mounted; the application
still has to start, or the user has no way in at all. But it must not pass in
silence either - a project simply not appearing is indistinguishable from a
broken auto-open, which is the same reasoning /INFILE already carries.
}
unit testcase_recent_project;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, fpcunit, testregistry, recent_project;

type
    TRecentProjectTest = class(TTestCase)
    private
        function Plan(const AProjectSwitch, AInFile, ALast: string): TStartupPlan;
    published
        procedure AnExplicitProjectSwitchWins;
        procedure ItWinsEvenOverARememberedProject;
        procedure InfileMeansStartFreshWithThatData;
        procedure AndInfileWinsOverARememberedProject;
        procedure TheLastProjectIsOpenedWhenNothingElseWasAsked;
        procedure NothingRememberedOpensNothing;

        procedure AProjectThatIsNoLongerThereIsReportedAndStartupContinues;
        procedure AnInfileThatIsNoLongerThereIsReportedAndStartupContinues;
        procedure AnAskedForProjectThatIsMissingIsReportedNotSilentlyIgnored;
        procedure AWarningNamesTheFileItIsAbout;

        procedure APathIsTakenAsGivenRatherThanTrimmedIntoSomethingElse;
        procedure AWhitespaceOnlyPathIsNoPathAtAll;
    end;

    { The list behind File > Open Recent. }
    TRecentListTest = class(TTestCase)
    published
        procedure AProjectJustOpenedIsFirst;
        procedure TheOneOpenedBeforeItComesNext;
        procedure OpeningOneAgainMovesItToTheFrontRatherThanRepeatingIt;
        procedure TheSamePathInAnotherCaseIsTheSameProject;
        procedure TheListStopsAtItsLimit;
        procedure AndItIsTheOLDESTThatFallsOffTheEnd;
        procedure NothingIsRememberedForAnEmptyPath;
        procedure AStoredListReadsBackAsTheProjectsItNames;
        procedure AnEmptyStoreIsAnEmptyList;
        procedure APathWithTheSeparatorInItCannotSplitTheList;

        procedure AProjectThatIsGoneIsTakenOutOfTheList;
        procedure AndOnlyThatOne;
        procedure TakingOutSomethingThatIsNotThereChangesNothing;
        procedure TakingOutNothingChangesNothing;
    end;

    { The same decision against a REAL file system.

      WHY THIS IS NOT COVERED BY THE SUITE ABOVE, and it is the whole reason
      this class exists: that one passes its own existence check in, so every
      branch is reachable with no disk - and the check the APPLICATION passes in
      was therefore never called by anything. It was `@FileExists`, and the RTL
      has no one-argument FileExists: both overloads take a second FollowLink
      parameter with a default, which serves the CALL and does nothing for the
      ADDRESS. Fit.lpr compiled in Delphi syntax mode, where @Routine is
      assignment-compatible with any procedural variable, so the address taken
      was the two-argument UnicodeString overload - called with an AnsiString.
      The last project was reported as no longer there and the application
      started empty, on every run. This unit, in objfpc mode, would not have
      compiled that line at all.

      So the check itself is now a named function in recent_project, and this
      suite calls it against files that really are and really are not there. }
    TStartupOnDiskTest = class(TTestCase)
    private
        FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFileThatIsThereIsReportedAsThere;
        procedure AFileThatIsNotThereIsReportedAsNotThere;
        procedure TheRememberedProjectIsOpenedWhenItReallyExists;
        procedure ARememberedProjectThatIsReallyGoneIsReported;
    end;

implementation

{ Only these two paths exist. Everything else the test asks about is absent, so
  every "the file is gone" branch is reachable without touching a disk. }
function ExistsForTest(const APath: string): boolean;
begin
    Result := (APath = 'here.fitproj') or (APath = 'here.dat');
end;

function TRecentProjectTest.Plan(const AProjectSwitch, AInFile,
    ALast: string): TStartupPlan;
begin
    Result := PlanStartup(AProjectSwitch, AInFile, ALast, @ExistsForTest);
end;

procedure TRecentProjectTest.AnExplicitProjectSwitchWins;
var
    P: TStartupPlan;
begin
    P := Plan('here.fitproj', '', '');
    AssertEquals('a project is opened', Ord(scProject), Ord(P.Choice));
    AssertEquals('the one that was named', 'here.fitproj', P.Path);
    AssertEquals('and nothing to warn about', '', P.Warning);
end;

procedure TRecentProjectTest.ItWinsEvenOverARememberedProject;
var
    P: TStartupPlan;
begin
    //  The user said which one. A remembered project is a convenience, and a
    //  convenience never overrides an instruction.
    P := Plan('here.fitproj', '', 'somewhere/else.fitproj');
    AssertEquals('', Ord(scProject), Ord(P.Choice));
    AssertEquals('the one that was asked for', 'here.fitproj', P.Path);
end;

procedure TRecentProjectTest.InfileMeansStartFreshWithThatData;
var
    P: TStartupPlan;
begin
    //  /INFILE has meant "start fresh with this data" since before projects
    //  existed, and it goes on meaning it.
    P := Plan('', 'here.dat', '');
    AssertEquals('a data file', Ord(scDataFile), Ord(P.Choice));
    AssertEquals('', 'here.dat', P.Path);
end;

procedure TRecentProjectTest.AndInfileWinsOverARememberedProject;
var
    P: TStartupPlan;
begin
    //  Otherwise the data would be loaded INTO the remembered project, silently
    //  modifying a document the user never asked to open.
    P := Plan('', 'here.dat', 'here.fitproj');
    AssertEquals('the data file, not the project', Ord(scDataFile),
        Ord(P.Choice));
    AssertEquals('', 'here.dat', P.Path);
end;

procedure TRecentProjectTest.TheLastProjectIsOpenedWhenNothingElseWasAsked;
var
    P: TStartupPlan;
begin
    P := Plan('', '', 'here.fitproj');
    AssertEquals('', Ord(scProject), Ord(P.Choice));
    AssertEquals('', 'here.fitproj', P.Path);
    AssertEquals('', '', P.Warning);
end;

procedure TRecentProjectTest.NothingRememberedOpensNothing;
var
    P: TStartupPlan;
begin
    //  A first run. An empty window, and nothing to say about it.
    P := Plan('', '', '');
    AssertEquals('', Ord(scNothing), Ord(P.Choice));
    AssertEquals('', '', P.Path);
    AssertEquals('nothing worth warning about either', '', P.Warning);
end;

procedure TRecentProjectTest.AProjectThatIsNoLongerThereIsReportedAndStartupContinues;
var
    P: TStartupPlan;
begin
    //  Deleted, renamed, or on a drive that is not mounted. The application has
    //  to start - refusing to would leave the user with no way in at all - but
    //  it must not pass in silence, because a project simply not appearing is
    //  indistinguishable from a broken auto-open.
    P := Plan('', '', 'gone.fitproj');
    AssertEquals('nothing is opened', Ord(scNothing), Ord(P.Choice));
    AssertTrue('and it says so', P.Warning <> '');
end;

procedure TRecentProjectTest.AnInfileThatIsNoLongerThereIsReportedAndStartupContinues;
var
    P: TStartupPlan;
begin
    P := Plan('', 'gone.dat', '');
    AssertEquals('', Ord(scNothing), Ord(P.Choice));
    AssertTrue('reported', P.Warning <> '');
end;

procedure TRecentProjectTest.AnAskedForProjectThatIsMissingIsReportedNotSilentlyIgnored;
var
    P: TStartupPlan;
begin
    //  The user named this one on the command line. Falling back to the
    //  remembered project would open a DIFFERENT document than the one asked
    //  for, and the window title is the only thing that would say so.
    P := Plan('gone.fitproj', '', 'here.fitproj');
    AssertEquals('no silent substitution', Ord(scNothing), Ord(P.Choice));
    AssertTrue('reported', P.Warning <> '');
end;

procedure TRecentProjectTest.AWarningNamesTheFileItIsAbout;
var
    P: TStartupPlan;
begin
    //  A refusal that explains itself. "Could not open the last project" sends
    //  the reader looking for which one.
    P := Plan('', '', 'gone.fitproj');
    AssertTrue('the message names the file',
        Pos('gone.fitproj', P.Warning) > 0);
end;

procedure TRecentProjectTest.APathIsTakenAsGivenRatherThanTrimmedIntoSomethingElse;
var
    P: TStartupPlan;
begin
    //  Surrounding whitespace comes from a command line and is not part of the
    //  name; anything inside it is. A path with a space in it is ordinary on
    //  every platform this runs on.
    P := Plan('  here.fitproj  ', '', '');
    AssertEquals('', 'here.fitproj', P.Path);
end;

procedure TRecentProjectTest.AWhitespaceOnlyPathIsNoPathAtAll;
var
    P: TStartupPlan;
begin
    //  /PROJECT= with nothing after it. Treated as "not asked for" rather than
    //  as a file named by the empty string, which would warn about nothing.
    P := Plan('   ', '', 'here.fitproj');
    AssertEquals('falls through to the remembered one', Ord(scProject),
        Ord(P.Choice));
    AssertEquals('', 'here.fitproj', P.Path);
    AssertEquals('and says nothing about the empty switch', '', P.Warning);
end;

{ ---- against a real file system -------------------------------------------- }

procedure TStartupOnDiskTest.SetUp;
var
    F: TFileStream;
begin
    //  A real file, with the real extension, in the temp directory: the point
    //  of this suite is that the answer comes from the file system rather than
    //  from a stub.
    FPath := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-startup-test.fitproj';
    F := TFileStream.Create(FPath, fmCreate);
    try
        F.WriteByte(0);
    finally
        F.Free;
    end;
end;

procedure TStartupOnDiskTest.TearDown;
begin
    if (FPath <> '') and FileExists(FPath) then
        DeleteFile(FPath);
end;

procedure TStartupOnDiskTest.AFileThatIsThereIsReportedAsThere;
begin
    AssertTrue('the file this fixture just wrote', DefaultPathExists(FPath));
end;

procedure TStartupOnDiskTest.AFileThatIsNotThereIsReportedAsNotThere;
begin
    //  THE OTHER ANSWER, because a check that always says True would pass the
    //  test above and offer a project that is not there.
    AssertFalse('a name nothing has',
        DefaultPathExists(FPath + '.no-such-thing'));
end;

procedure TStartupOnDiskTest.TheRememberedProjectIsOpenedWhenItReallyExists;
var
    P: TStartupPlan;
begin
    //  THE USE CASE: the application starts with no switches and the project
    //  from the last session is where it was left.
    P := PlanStartup('', '', FPath, @DefaultPathExists);
    AssertEquals('the remembered project is offered', Ord(scProject),
        Ord(P.Choice));
    AssertEquals('', FPath, P.Path);
    AssertEquals('and nothing is warned about', '', P.Warning);
end;

procedure TStartupOnDiskTest.ARememberedProjectThatIsReallyGoneIsReported;
var
    P: TStartupPlan;
begin
    DeleteFile(FPath);
    P := PlanStartup('', '', FPath, @DefaultPathExists);
    AssertEquals('nothing is opened', Ord(scNothing), Ord(P.Choice));
    AssertTrue('and it is said, naming the file', Pos(FPath, P.Warning) > 0);
end;

{ ---- the recent list ------------------------------------------------------- }

procedure TRecentListTest.AProjectJustOpenedIsFirst;
begin
    AssertEquals('a.fitproj', RecentAfterOpening('', 'a.fitproj'));
end;

procedure TRecentListTest.TheOneOpenedBeforeItComesNext;
var
    L: string;
begin
    //  MOST RECENT FIRST, which is the only order this menu can have: it is
    //  read top to bottom and the top is where the eye starts.
    L := RecentAfterOpening('', 'a.fitproj');
    L := RecentAfterOpening(L, 'b.fitproj');
    AssertEquals('b.fitproj' + RecentSeparator + 'a.fitproj', L);
end;

procedure TRecentListTest.OpeningOneAgainMovesItToTheFrontRatherThanRepeatingIt;
var
    L: string;
begin
    //  A list with the same project twice offers the user a choice that is not
    //  one, and pushes something real off the end to do it.
    L := RecentAfterOpening('', 'a.fitproj');
    L := RecentAfterOpening(L, 'b.fitproj');
    L := RecentAfterOpening(L, 'a.fitproj');
    AssertEquals('a.fitproj' + RecentSeparator + 'b.fitproj', L);
end;

procedure TRecentListTest.TheSamePathInAnotherCaseIsTheSameProject;
var
    L: string;
begin
    //  Windows and macOS both open C:\Work\A.fitproj and c:\work.fitproj as
    //  one file, and a list that shows both is wrong on every platform this
    //  runs on - on Linux it is merely unlikely, and the cost of being wrong
    //  the other way is a duplicate entry rather than a lost one.
    L := RecentAfterOpening('', 'Work' + PathDelim + 'A.fitproj');
    L := RecentAfterOpening(L, 'work' + PathDelim + 'a.fitproj');
    AssertEquals('one entry, the newest spelling',
        'work' + PathDelim + 'a.fitproj', L);
end;

procedure TRecentListTest.TheListStopsAtItsLimit;
var
    L: string;
    i: longint;
begin
    L := '';
    for i := 1 to RecentProjectLimit + 5 do
        L := RecentAfterOpening(L, 'p' + IntToStr(i) + '.fitproj');
    AssertEquals('no longer than the menu it fills', RecentProjectLimit,
        Length(RecentProjects(L)));
end;

procedure TRecentListTest.AndItIsTheOLDESTThatFallsOffTheEnd;
var
    L: string;
    i: longint;
    Names: TStringDynArray;
begin
    L := '';
    for i := 1 to RecentProjectLimit + 1 do
        L := RecentAfterOpening(L, 'p' + IntToStr(i) + '.fitproj');
    Names := RecentProjects(L);
    AssertEquals('the newest is first',
        'p' + IntToStr(RecentProjectLimit + 1) + '.fitproj', Names[0]);
    AssertEquals('and the first one opened is gone', 'p2.fitproj',
        Names[High(Names)]);
end;

procedure TRecentListTest.NothingIsRememberedForAnEmptyPath;
begin
    //  New Project has no path, and a blank entry in the menu opens nothing.
    AssertEquals('a.fitproj', RecentAfterOpening('a.fitproj', ''));
    AssertEquals('a.fitproj', RecentAfterOpening('a.fitproj', '   '));
end;

procedure TRecentListTest.AStoredListReadsBackAsTheProjectsItNames;
var
    Names: TStringDynArray;
begin
    Names := RecentProjects('a.fitproj' + RecentSeparator + 'b.fitproj');
    AssertEquals(2, Length(Names));
    AssertEquals('a.fitproj', Names[0]);
    AssertEquals('b.fitproj', Names[1]);
end;

procedure TRecentListTest.AnEmptyStoreIsAnEmptyList;
begin
    //  A first run, and every run before this existed.
    AssertEquals(0, Length(RecentProjects('')));
end;

procedure TRecentListTest.APathWithTheSeparatorInItCannotSplitTheList;
var
    L: string;
begin
    //  THE STORE IS ONE STRING in a settings file, so the separator is the one
    //  character a path must not contain. A path that does is refused entry
    //  rather than being written out and read back as two files that do not
    //  exist.
    L := RecentAfterOpening('a.fitproj', 'we' + RecentSeparator + 'ird.fitproj');
    AssertEquals('the list is untouched', 'a.fitproj', L);
end;

procedure TRecentListTest.AProjectThatIsGoneIsTakenOutOfTheList;
begin
    //  AN ENTRY THAT OPENS NOTHING is a line the user can only be disappointed
    //  by, and the application already knows it is gone - it just refused to
    //  start on it.
    AssertEquals('b.fitproj',
        RecentWithout('a.fitproj' + RecentSeparator + 'b.fitproj', 'a.fitproj'));
end;

procedure TRecentListTest.AndOnlyThatOne;
var
    L: string;
begin
    L := RecentWithout('a.fitproj' + RecentSeparator + 'b.fitproj' +
        RecentSeparator + 'c.fitproj', 'b.fitproj');
    AssertEquals('the others keep their order',
        'a.fitproj' + RecentSeparator + 'c.fitproj', L);
end;

procedure TRecentListTest.TakingOutSomethingThatIsNotThereChangesNothing;
begin
    AssertEquals('a.fitproj', RecentWithout('a.fitproj', 'z.fitproj'));
end;

procedure TRecentListTest.TakingOutNothingChangesNothing;
begin
    //  ForgetLastProject runs when there may be no remembered project at all.
    AssertEquals('a.fitproj', RecentWithout('a.fitproj', ''));
end;

initialization
    //  A unit test: the existence check is passed in, so no file is touched.
    RegisterTest('unit', TRecentProjectTest);
    //  And an integration one beside it: this half is the file system.
    RegisterTest('unit', TRecentListTest);
    RegisterTest('integration', TStartupOnDiskTest);
end.
