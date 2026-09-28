// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the application actually does when it starts: the decision, and
the acting on it.)

WHY THIS EXISTS BESIDE testcase_recent_project, which already has thirteen tests
over the same decision. Those tests pass their own existence check in, so every
branch is reachable with no disk - and the sequence AROUND the decision lived in
Fit.lpr, a program file no test links. That is where the defect was: the check
handed to PlanStartup was `@FileExists`, which is not a one-argument function,
and the last project could never be reopened while the whole suite was green.

So the sequence is a unit now, and this suite drives it exactly as Fit.lpr does.
Two halves, deliberately:

  * the UNIT half stubs the existence check, because that is what makes every
    branch - opened, not there, nothing asked for - reachable in microseconds;
    and
  * the INTEGRATION half calls the production entry point, the one that supplies
    its own check, against a file that is really on disk. That is the half the
    defect was in, and a stub can never find it: the rule this codebase now
    carries is that the red test enters where the application enters, and where
    a seam has to be stubbed to get there, the production argument to that seam
    needs its own test calling it the way the application does.
}
unit testcase_startup_sequence;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    recent_project, startup_sequence;

type
    { Records what the application was told to do, and answers for the window.

      A PLAIN TObject, not TInterfacedObject: the interfaces here are CORBA
      (-SIcorba), which have no reference counting, so the fixture owns this and
      frees it. }
    TMockStartupHost = class(TObject, IStartupHost)
    public
        Remembered: string;
        Opened: string;
        Loaded: string;
        Warnings: string;
        Notes: string;
        { What OpenProject answers, and whether it was told to forget. }
        OpenWorks: boolean;
        Forgotten: boolean;
        { Every call, in order, so "it opened the project" is distinguishable
          from "it opened the project and then loaded a file over it". }
        Log: string;
        function LastProject: string;
        function OpenProject(const APath: string): boolean;
        procedure LoadDataFile(const APath: string);
        procedure Warn(const AMessage: string);
        procedure Note(const AMessage: string);
        procedure ForgetLastProject;
    end;

    { The sequence, with the existence check stubbed - every branch, no disk. }
    TStartupSequenceTest = class(TTestCase)
    private
        FHost: TMockStartupHost;
        FStub: IStartupHost;
        procedure Run(const AProjectSwitch, AInFileSwitch: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheRememberedProjectIsOpened;
        procedure NothingRememberedOpensNothing;
        procedure AProjectNamedOnTheCommandLineWins;
        procedure ADataFileIsLoadedRatherThanOpenedAsAProject;
        procedure AProjectAndADataFileAreNeverBothActedOn;
        procedure ARememberedProjectThatIsGoneIsReportedAndNothingIsOpened;
        procedure TheWindowIsAskedWhatItRemembers;
        procedure WhatItWasStartedWithIsSaidBeforeAnythingIsDecided;

        //  What happens to a remembered project that did not work out
        procedure ARememberedProjectThatIsGoneIsNotOfferedAgain;
        procedure ARememberedProjectThatWillNotOpenIsNotOfferedAgain;
        procedure OneThatOpensIsStillRemembered;
        procedure AProjectNamedOnTheCommandLineIsNeverForgottenForTheUser;
    end;

    { The production entry point, against a real file - the half that was
      missing, and the half the defect was in. }
    TStartupSequenceOnDiskTest = class(TTestCase)
    private
        FHost: TMockStartupHost;
        FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheRememberedProjectIsOpenedWhenItReallyExists;
        procedure ARememberedProjectThatIsReallyGoneIsNotOpened;
        procedure ADataFileThatIsReallyThereIsLoaded;
    end;

implementation

const
    { Two paths the stub says are there. Anything else is absent, so the "it is
      gone" branches need no arrangement. }
    RealProject = 'kept.fitproj';
    RealData = 'kept.dat';

function StubExists(const APath: string): boolean;
begin
    Result := (APath = RealProject) or (APath = RealData);
end;

{ ---- the mock ---------------------------------------------------------- }

function TMockStartupHost.LastProject: string;
begin
    Log := Log + 'last;';
    Result := Remembered;
end;

function TMockStartupHost.OpenProject(const APath: string): boolean;
begin
    Log := Log + 'open(' + APath + ');';
    Opened := APath;
    Result := OpenWorks;
end;

procedure TMockStartupHost.ForgetLastProject;
begin
    Log := Log + 'forget;';
    Forgotten := True;
end;

procedure TMockStartupHost.LoadDataFile(const APath: string);
begin
    Log := Log + 'load(' + APath + ');';
    Loaded := APath;
end;

procedure TMockStartupHost.Warn(const AMessage: string);
begin
    Log := Log + 'warn;';
    Warnings := Warnings + AMessage;
end;

procedure TMockStartupHost.Note(const AMessage: string);
begin
    Notes := Notes + AMessage + '|';
end;

{ ---- the sequence, stubbed --------------------------------------------- }

procedure TStartupSequenceTest.SetUp;
begin
    FHost := TMockStartupHost.Create;
    //  Opening works unless a test says otherwise: the ordinary case.
    FHost.OpenWorks := True;
    FStub := FHost;
end;

procedure TStartupSequenceTest.TearDown;
begin
    //  The interface first, then the object: a CORBA interface holds no count,
    //  so the other order leaves a live reference to freed memory.
    FStub := nil;
    FreeAndNil(FHost);
end;

procedure TStartupSequenceTest.Run(const AProjectSwitch, AInFileSwitch: string);
begin
    RunStartup(AProjectSwitch, AInFileSwitch, FStub, @StubExists);
end;

procedure TStartupSequenceTest.TheRememberedProjectIsOpened;
begin
    //  THE USE CASE, and the one that did not work: no switches, and the
    //  project from the last session is where it was left.
    FHost.Remembered := RealProject;
    Run('', '');
    AssertEquals('opened', RealProject, FHost.Opened);
    AssertEquals('and nothing was warned about', '', FHost.Warnings);
end;

procedure TStartupSequenceTest.NothingRememberedOpensNothing;
begin
    Run('', '');
    AssertEquals('nothing opened', '', FHost.Opened);
    AssertEquals('nothing loaded', '', FHost.Loaded);
    //  NOT A WARNING EITHER. A first run has nothing to remember, which is
    //  ordinary rather than a fault.
    AssertEquals('and nothing to report', '', FHost.Warnings);
end;

procedure TStartupSequenceTest.AProjectNamedOnTheCommandLineWins;
begin
    FHost.Remembered := 'something-else.fitproj';
    Run(RealProject, '');
    AssertEquals('the one that was asked for', RealProject, FHost.Opened);
end;

procedure TStartupSequenceTest.ADataFileIsLoadedRatherThanOpenedAsAProject;
begin
    Run('', RealData);
    AssertEquals('loaded as data', RealData, FHost.Loaded);
    AssertEquals('and not opened as a project', '', FHost.Opened);
end;

procedure TStartupSequenceTest.AProjectAndADataFileAreNeverBothActedOn;
begin
    //  ONE ACTION, and this is what the sequence adds to the decision: a case
    //  that fell through to a second branch would load data into a document
    //  the user never asked to open.
    FHost.Remembered := RealProject;
    Run('', RealData);
    AssertEquals('the data file', RealData, FHost.Loaded);
    AssertEquals('and the remembered project is left alone', '', FHost.Opened);
end;

procedure TStartupSequenceTest.ARememberedProjectThatIsGoneIsReportedAndNothingIsOpened;
begin
    FHost.Remembered := 'moved-away.fitproj';
    Run('', '');
    AssertEquals('nothing opened', '', FHost.Opened);
    AssertTrue('and it is said, naming the file',
        Pos('moved-away.fitproj', FHost.Warnings) > 0);
end;

procedure TStartupSequenceTest.TheWindowIsAskedWhatItRemembers;
begin
    //  THE JOIN. What the window remembered has to reach the decision, and it
    //  is the link nothing tested: the settings field, the window's getter and
    //  PlanStartup were each covered and were connected by a program file.
    FHost.Remembered := RealProject;
    Run('', '');
    AssertTrue('the host was asked at all', Pos('last;', FHost.Log) > 0);
    AssertTrue('before anything was opened',
        Pos('last;', FHost.Log) < Pos('open(', FHost.Log));
end;

procedure TStartupSequenceTest.WhatItWasStartedWithIsSaidBeforeAnythingIsDecided;
begin
    //  A start-up that opens nothing must not be silent about why: "no project
    //  was remembered" and "the auto-open is broken" are the same silence
    //  otherwise, which is how the defect was reported.
    Run('', '');
    AssertTrue('the inputs are on the record', Pos('start-up:', FHost.Notes) > 0);
end;

procedure TStartupSequenceTest.ARememberedProjectThatIsGoneIsNotOfferedAgain;
begin
    //  A CONVENIENCE THAT FAILED STOPS BEING ONE. The file has been deleted or
    //  moved; offering it again next time is a warning the user cannot act on,
    //  once per start-up, for ever.
    FHost.Remembered := 'moved-away.fitproj';
    Run('', '');
    AssertTrue('the application forgot it', FHost.Forgotten);
end;

procedure TStartupSequenceTest.ARememberedProjectThatWillNotOpenIsNotOfferedAgain;
begin
    //  IT IS THERE AND IT IS NOT A PROJECT - a file replaced by something else,
    //  or truncated by a full disk. The workflow has already told the user what
    //  went wrong; what must not happen is the same failure every start-up.
    FHost.Remembered := RealProject;
    FHost.OpenWorks := False;
    Run('', '');
    AssertEquals('it was tried', RealProject, FHost.Opened);
    AssertTrue('and then forgotten', FHost.Forgotten);
end;

procedure TStartupSequenceTest.OneThatOpensIsStillRemembered;
begin
    FHost.Remembered := RealProject;
    Run('', '');
    AssertFalse('nothing was forgotten', FHost.Forgotten);
end;

procedure TStartupSequenceTest.AProjectNamedOnTheCommandLineIsNeverForgottenForTheUser;
begin
    //  /PROJECT= IS AN INSTRUCTION, not a convenience, and it is not what the
    //  settings remember. A mistyped switch must not silently clear the project
    //  the user would otherwise have been offered next time.
    FHost.Remembered := RealProject;
    Run('typo.fitproj', '');
    AssertFalse('the remembered project is left alone', FHost.Forgotten);
    AssertEquals('and nothing was opened', '', FHost.Opened);
end;

{ ---- the sequence, against a real file system --------------------------- }

procedure TStartupSequenceOnDiskTest.SetUp;
var
    F: TFileStream;
begin
    FHost := TMockStartupHost.Create;
    FHost.OpenWorks := True;
    FPath := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-startup-seq.fitproj';
    F := TFileStream.Create(FPath, fmCreate);
    try
        F.WriteByte(0);
    finally
        F.Free;
    end;
end;

procedure TStartupSequenceOnDiskTest.TearDown;
begin
    if (FPath <> '') and FileExists(FPath) then
        DeleteFile(FPath);
    FreeAndNil(FHost);
end;

procedure TStartupSequenceOnDiskTest.TheRememberedProjectIsOpenedWhenItReallyExists;
var
    Host: IStartupHost;
begin
    //  THE PRODUCTION OVERLOAD - no check passed in, exactly as Fit.lpr calls
    //  it. This is the test the defect could not have survived: with
    //  @FileExists in there, the file below is on disk and the project is still
    //  not opened.
    FHost.Remembered := FPath;
    Host := FHost;
    try
        RunStartup('', '', Host);
    finally
        Host := nil;
    end;
    AssertEquals('opened', FPath, FHost.Opened);
    AssertEquals('and nothing warned about', '', FHost.Warnings);
end;

procedure TStartupSequenceOnDiskTest.ARememberedProjectThatIsReallyGoneIsNotOpened;
var
    Host: IStartupHost;
begin
    DeleteFile(FPath);
    FHost.Remembered := FPath;
    Host := FHost;
    try
        RunStartup('', '', Host);
    finally
        Host := nil;
    end;
    AssertEquals('nothing opened', '', FHost.Opened);
    AssertTrue('and it is said', Pos(FPath, FHost.Warnings) > 0);
end;

procedure TStartupSequenceOnDiskTest.ADataFileThatIsReallyThereIsLoaded;
var
    Host: IStartupHost;
begin
    //  /INFILE, through the same production entry point: it takes the same
    //  check, so it had the same defect and nothing covered it either.
    Host := FHost;
    try
        RunStartup('', FPath, Host);
    finally
        Host := nil;
    end;
    AssertEquals('loaded', FPath, FHost.Loaded);
end;

initialization
    //  A unit test: the existence check is stubbed, so no file is touched.
    RegisterTest('unit', TStartupSequenceTest);
    //  And the production entry point beside it, which is the file system.
    RegisterTest('integration', TStartupSequenceOnDiskTest);
end.
