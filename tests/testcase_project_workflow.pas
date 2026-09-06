// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(New, Open, Save, Save As, and what closing does about unsaved work.)

WHY THESE ARE WORTH TESTS. Every one of them is a short sequence whose steps have
to happen in an order that is not obvious, and getting the order wrong is silent:
a window titled after a document that was never written, a project offered at the
next start-up that does not exist, a project dirty the instant it is opened, a
close that goes ahead after a save that did not happen.

The last of those is the only path in this program where it overrides what the
user asked for, and it is the one worth being sure about: they said save, the
save did not happen, and closing anyway destroys precisely the work they asked to
keep.

NOTHING HERE TOUCHES A DISK. The window is faked because a dialog cannot be
opened headlessly, and the file layer is faked because these tests are about the
sequences rather than about writing bytes - which is also what keeps them in the
half the coverage figure is measured over. Writing the bytes is
fit_project_file's, and the integration tests drive that.
}
unit testcase_project_workflow;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, title_points_set, close_query,
    fit_project_archive, fit_project_document, fit_project_session,
    project_commands, project_workflow, int_project_host, mock_project_host,
    gauss_points_set;

type
    TProjectWorkflowTest = class(TTestCase)
    private
        FService: TFitService;
        FHostObj: TMockProjectHost;
        FHost: IProjectHost;
        FFlow: TProjectWorkflow;
        function PathIn(const AName: string): string;
        procedure GivenAProblem;
        { A problem with CURVES in it, which is what an import discards. }
        procedure GivenAModel;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  New
        procedure StartingANewProjectClearsEverythingAndForgetsThePath;

        //  Save / Save As
        procedure TheFirstSaveAsksForAName;
        procedure ASavedProjectIsShownAndRememberedOnlyAfterItIsWritten;
        procedure ASecondSaveDoesNotAskAgain;
        procedure SaveAsAlwaysAsksAndStartsFromTheCurrentPath;
        procedure CancellingTheNameDialogWritesNothingAndChangesNothing;
        procedure ANameWithNoExtensionIsSavedWithOne;
        procedure ASaveThatFailsIsReportedAndChangesNothing;

        //  Overwriting
        procedure SavingOverAnExistingProjectAsksFirst;
        procedure ARefusedOverwriteWritesNothingAndChangesNothing;
        procedure AnAcceptedOverwriteWrites;
        procedure NothingIsAskedWhenNoFileIsThere;
        procedure TheQuestionIsAboutTheNameThatWillBeWritten;
        procedure TheQuestionComesBeforeTheWrite;

        //  Importing a profile over an open project
        procedure ImportingIntoAnEmptySessionAsksNothing;
        procedure ImportingOverAModelAsksFirst;
        procedure ARefusedImportIsRefusedRatherThanReported;
        procedure UnsavedWorkIsAlsoWorthAskingAbout;
        procedure TheImportQuestionNamesTheFileBeingLoaded;
        procedure ReloadingAsksTheSameQuestionAboutTheFileItCameFrom;

        //  Open
        procedure OpeningAProjectShowsItAndRefreshesTheWindow;
        procedure AProjectIsMarkedSavedOnlyAfterTheRefresh;
        procedure CancellingTheOpenDialogDoesNothing;
        procedure AFileThatIsNotAProjectIsReportedAndNothingIsShown;
        procedure OpeningPutsTheWindowsOwnContextBack;
        procedure ItIsPutBackAfterTheRefreshThatRebuildsTheModel;
        procedure AFailedOpenPutsNoContextBack;
        procedure ASourceThatHasMovedOnIsReportedOnOpening;
        procedure ASourceThatIsUnchangedSaysNothing;
        procedure StartingANewProjectClearsWhatTheWindowRemembers;
        procedure AGestureInTheWindowIsRememberedForTheDocument;
        procedure WhatWasOpenedIsWhatTheNextSaveReplaces;
        procedure AfterNewThereIsNothingBeingReplaced;
        procedure WithNoSeamsGivenItUsesTheRealOnes;

        //  Closing
        procedure WithNothingUnsavedClosingAsksNothing;
        procedure DecliningToSaveStillCloses;
        procedure CancellingTheQuestionStopsTheClose;
        procedure ChoosingToSaveWritesTheProjectAndCloses;
        procedure ACancelledSaveStopsTheClose;
    end;

implementation

var
    { What the fake file layer did and was told to do. Unit-level because a plain
      function pointer cannot close over a fixture - the seam is deliberately the
      simplest thing that removes the file system. }
    SavedTo: string;
    SaveWorks: boolean;
    OpenWorks: boolean;
    SaveCount: longint;
    { The unknown part the save was handed as "what this replaces". }
    ReplacedPart: string;
    { The document the fake open hands back, so a test can say what working
      context the file carried. }
    OpenedUi: TProjectDocument;
    { What the fake notice answers, and what was logged. }
    NoticeText: string;
    NoticedProvenance: string;
    { The paths the fake file system says are already there, separated by ';'.
      A string rather than a list because a plain function pointer cannot close
      over a fixture - the same reason the seams above are unit-level. }
    ExistingPaths: string;

function FakeSave(AService: IFitService;
    const AContext: TProjectClientContext; const APrevious: TProjectDocument;
    const APath: string; out AFault: string): boolean;
begin
    Inc(SaveCount);
    //  What the save was told it is replacing, so a test can see that the
    //  document being written carries the one it opened.
    ReplacedPart := '';
    PartContent(APrevious.AsRead, 'future/recipe.json', ReplacedPart);
    AFault := '';
    Result := SaveWorks;
    if Result then
        SavedTo := APath
    else
        AFault := 'no such directory';
end;

function FakeNotice(const AProvenance: TProjectProvenance): string;
begin
    NoticedProvenance := AProvenance.SourcePath;
    Result := NoticeText;
end;

function FakeExists(const APath: string): boolean;
begin
    Result := Pos(';' + APath + ';', ';' + ExistingPaths + ';') > 0;
end;

function FakeOpen(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;
begin
    ADoc := OpenedUi;
    AFault := '';
    Result := OpenWorks;
    if not Result then
        AFault := 'that is not a project';
end;

procedure TProjectWorkflowTest.SetUp;
begin
    FService := TFitService.Create;
    FHostObj := TMockProjectHost.Create;
    FHost := FHostObj;
    FFlow := TProjectWorkflow.Create(FService, FHost, @FakeSave, @FakeOpen,
        @FakeNotice, @FakeExists);
    NoticeText := '';
    NoticedProvenance := '';
    SavedTo := '';
    SaveWorks := True;
    OpenWorks := True;
    SaveCount := 0;
    ReplacedPart := '';
    OpenedUi := EmptyProjectDocument;
    //  NOTHING IS THERE by default, so a test that does not arrange a file is
    //  never asked about one.
    ExistingPaths := '';
end;

procedure TProjectWorkflowTest.TearDown;
begin
    FreeAndNil(FFlow);
    //  The interface goes first: everything compiles -SIcorba, so a live
    //  reference over a freed object is a use-after-free that happens to work.
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FService);
end;

procedure TProjectWorkflowTest.GivenAModel;
var
    B, Picks: TTitlePointsSet;
    Svc: IFitService;
begin
    //  A PROBLEM WITH CURVES IN IT, which is what an import discards - and what
    //  GivenAProblem deliberately does not build: most tests here are about
    //  documents rather than models.
    GivenAProblem;
    Svc := FService;
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(10, 0);
    FService.SetRFactorBounds(B);
    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(4, 20);
    FService.SetCurvePositions(Picks);
end;

function TProjectWorkflowTest.PathIn(const AName: string): string;
begin
    //  A path, not a place: nothing here writes one.
    Result := 'C:' + PathDelim + 'work' + PathDelim + AName;
end;

procedure TProjectWorkflowTest.GivenAProblem;
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 10 do
        P.AddNewPoint(i, 10 + i);
    FService.SetProfilePointsSet(P);
end;

procedure TProjectWorkflowTest.StartingANewProjectClearsEverythingAndForgetsThePath;
begin
    FFlow.Path := PathIn('a.fitproj');
    FFlow.NewProject;
    AssertEquals('the path is forgotten, so the next Save asks', '',
        FFlow.Path);
    AssertTrue('the window was cleared',
        FHostObj.Log.Saw('ClearEverything'));
    AssertEquals('and shows no document', '', FHostObj.Shown);
end;

procedure TProjectWorkflowTest.TheFirstSaveAsksForAName;
begin
    //  A Save that silently picks a name puts the user's work somewhere they
    //  did not choose.
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    AssertTrue('saved', FFlow.SaveProject);
    AssertTrue('it asked', FHostObj.Log.Saw('AskProjectToSaveAs'));
end;

procedure TProjectWorkflowTest.ASavedProjectIsShownAndRememberedOnlyAfterItIsWritten;
begin
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    AssertTrue('saved', FFlow.SaveProject);
    AssertEquals('written where it was asked to be',
        PathIn('a.fitproj'), SavedTo);
    AssertEquals('the window shows it', PathIn('a.fitproj'), FHostObj.Shown);
    AssertEquals('and it is what a later Save writes to',
        PathIn('a.fitproj'), FFlow.Path);
end;

procedure TProjectWorkflowTest.ASecondSaveDoesNotAskAgain;
begin
    //  Asking every time is how people stop reading the dialog.
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    FFlow.SaveProject;
    AssertEquals('asked once', 1,
        FHostObj.Log.CountOf('AskProjectToSaveAs'));
    AssertTrue('saved again', FFlow.SaveProject);
    AssertEquals('and still only once', 1,
        FHostObj.Log.CountOf('AskProjectToSaveAs'));
end;

procedure TProjectWorkflowTest.SaveAsAlwaysAsksAndStartsFromTheCurrentPath;
begin
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    FFlow.SaveProject;
    FHostObj.ScriptSaveAs(PathIn('b.fitproj'));
    AssertTrue('saved under the new name', FFlow.SaveProjectAs);
    AssertEquals('the dialog started from where it was',
        PathIn('a.fitproj'), FHostObj.Suggested);
    AssertEquals('and the document moved', PathIn('b.fitproj'), FFlow.Path);
    AssertEquals('and it was written there', PathIn('b.fitproj'), SavedTo);
end;

procedure TProjectWorkflowTest.CancellingTheNameDialogWritesNothingAndChangesNothing;
begin
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'), False);
    AssertFalse('not saved', FFlow.SaveProject);
    AssertEquals('nothing was written', '', SavedTo);
    AssertEquals('no document is claimed', '', FFlow.Path);
    AssertEquals('and nothing was reported - cancelling is not an error', '',
        FHostObj.Problem);
end;

procedure TProjectWorkflowTest.ANameWithNoExtensionIsSavedWithOne;
begin
    //  A file with no extension opens in nothing. The name is settled BEFORE
    //  the write, so what is remembered is the file that actually exists.
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('noext'));
    AssertTrue('saved', FFlow.SaveProject);
    AssertEquals('with an extension', PathIn('noext.fitproj'), SavedTo);
    AssertEquals('and that is what is remembered',
        PathIn('noext.fitproj'), FFlow.Path);
end;

procedure TProjectWorkflowTest.ASaveThatFailsIsReportedAndChangesNothing;
begin
    //  A directory that is not there. The user is told; the document is not
    //  retitled after a file that was never written.
    GivenAProblem;
    SaveWorks := False;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    AssertFalse('not saved', FFlow.SaveProject);
    AssertTrue('reported', FHostObj.Problem <> '');
    AssertEquals('and no document is claimed', '', FFlow.Path);
    AssertEquals('nor shown', '', FHostObj.Shown);
end;

procedure TProjectWorkflowTest.OpeningAProjectShowsItAndRefreshesTheWindow;
begin
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    FFlow.SaveProject;
    FFlow.NewProject;

    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    AssertTrue('opened', FFlow.OpenProject);
    AssertEquals('shown', PathIn('a.fitproj'), FHostObj.Shown);
    AssertTrue('and the window redrew from the engine',
        FHostObj.Log.Saw('RefreshFromEngine'));
end;

procedure TProjectWorkflowTest.AProjectIsMarkedSavedOnlyAfterTheRefresh;
var
    Seq: string;
begin
    //  THE ORDER MATTERS AND IS NOT OBVIOUS. Filling the tables is what marks
    //  the window modified, so marking it saved first leaves a project dirty
    //  the instant it is opened - and every close then asks about work nobody
    //  did, which is how people learn to dismiss the question.
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    FFlow.SaveProject;
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    FFlow.OpenProject;
    //  Only the OPEN leg: the save before it marks the project saved too, and
    //  the whole log would find that one first.
    Seq := FHostObj.Log.Sequence;
    Seq := Copy(Seq, Pos('AskProjectToOpen', Seq), MaxInt);
    AssertTrue('both happened: ' + Seq,
        (Pos('RefreshFromEngine', Seq) > 0) and (Pos('MarkSaved', Seq) > 0));
    AssertTrue('and the refresh came first: ' + Seq,
        Pos('RefreshFromEngine', Seq) < Pos('MarkSaved', Seq));
end;

procedure TProjectWorkflowTest.CancellingTheOpenDialogDoesNothing;
begin
    FHostObj.ScriptOpen(PathIn('a.fitproj'), False);
    AssertFalse('nothing opened', FFlow.OpenProject);
    AssertEquals('nothing shown', '', FHostObj.Shown);
    AssertEquals('and nothing reported', '', FHostObj.Problem);
end;

procedure TProjectWorkflowTest.AFileThatIsNotAProjectIsReportedAndNothingIsShown;
begin
    //  The user picked a data file by mistake. Reported, and the problem they
    //  already had is left alone - nothing is shown and no document is claimed,
    //  so a failed open cannot be mistaken for a successful one.
    OpenWorks := False;
    FHostObj.ScriptOpen(PathIn('rubbish.dat'));
    AssertFalse('not opened', FFlow.OpenProject);
    AssertTrue('reported', FHostObj.Problem <> '');
    AssertEquals('nothing shown', '', FHostObj.Shown);
    AssertEquals('and no document is claimed', '', FFlow.Path);
end;

procedure TProjectWorkflowTest.OpeningPutsTheWindowsOwnContextBack;
begin
    //  IT WAS WRITTEN TO THE FILE AND NEVER READ. The axis, the tab and the
    //  picking mode did not restore at all, while the user guide said they
    //  did - no error, no failing test, just a feature that was not there.
    OpenedUi.HasUi := True;
    OpenedUi.Ui.ViewModeChosenByUser := True;
    OpenedUi.Ui.ViewMode := 2;
    OpenedUi.Ui.ActiveTab := 1;
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    AssertTrue('opened', FFlow.OpenProject);
    AssertTrue('the window was told', FHostObj.Log.Saw('ApplyWorkingContext'));
    AssertTrue('the axis', FHostObj.Applied.ApplyAxis);
    AssertEquals('', 2, FHostObj.Applied.ViewMode);
    AssertTrue('and the tab', FHostObj.Applied.ApplyTab);
end;

procedure TProjectWorkflowTest.ItIsPutBackAfterTheRefreshThatRebuildsTheModel;
var
    Seq: string;
begin
    //  The selected curve is looked up in the model, and the refresh is what
    //  rebuilt it. Asking first would find nothing and silently select nothing.
    OpenedUi.HasUi := True;
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    FFlow.OpenProject;
    Seq := FHostObj.Log.Sequence;
    AssertTrue('both happened: ' + Seq,
        (Pos('RefreshFromEngine', Seq) > 0) and
        (Pos('ApplyWorkingContext', Seq) > 0));
    AssertTrue('the refresh came first: ' + Seq,
        Pos('RefreshFromEngine', Seq) < Pos('ApplyWorkingContext', Seq));
end;

procedure TProjectWorkflowTest.AFailedOpenPutsNoContextBack;
begin
    //  Nothing was opened, so nothing about the window may move - the problem
    //  the user already had is left exactly as it was.
    OpenWorks := False;
    FHostObj.ScriptOpen(PathIn('rubbish.dat'));
    AssertFalse('not opened', FFlow.OpenProject);
    AssertFalse('and nothing was applied',
        FHostObj.Log.Saw('ApplyWorkingContext'));
end;

procedure TProjectWorkflowTest.ASourceThatHasMovedOnIsReportedOnOpening;
begin
    //  THE DATA FILE THE PROJECT CAME FROM NO LONGER SAYS WHAT IT SAID. The
    //  project is not wrong - it carries its own profile - so this is a notice
    //  rather than a refusal, and it is asked about the provenance the file
    //  recorded rather than about anything this session happens to hold.
    OpenedUi.Provenance.SourcePath := 'runs/7.dat';
    NoticeText := 'the file is not the file it was';
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    AssertTrue('opened anyway', FFlow.OpenProject);
    AssertEquals('asked about the project''s own source', 'runs/7.dat',
        NoticedProvenance);
end;

procedure TProjectWorkflowTest.ASourceThatIsUnchangedSaysNothing;
begin
    //  Silence is the ordinary case, and it has to be: a notice on every open
    //  is one nobody reads by the time one matters.
    NoticeText := '';
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    AssertTrue('opened', FFlow.OpenProject);
    AssertEquals('nothing was shown to the user', '', FHostObj.Problem);
end;

procedure TProjectWorkflowTest.StartingANewProjectClearsWhatTheWindowRemembers;
begin
    //  THE WINDOW REMEMBERS TWO THINGS ON BEHALF OF THE DOCUMENT - the selected
    //  interval and where the data came from - and nothing else holds either.
    //  Left alone, a new project claimed the previous file's provenance and
    //  would have saved an interval nobody was in.
    OpenedUi.Provenance.SourcePath := 'runs/7.dat';
    OpenedUi.SelectedIntervalInForce := True;
    OpenedUi.SelectedIntervalFrom := 4;
    OpenedUi.SelectedIntervalTo := 16;
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    FFlow.OpenProject;
    AssertEquals('adopted on opening', 'runs/7.dat',
        FFlow.ContextToSave.Provenance.SourcePath);
    AssertTrue('so the next save keeps the interval',
        FFlow.ContextToSave.SelectedIntervalInForce);
    AssertEquals('', 4, FFlow.ContextToSave.SelectedIntervalFrom);

    FFlow.NewProject;
    AssertEquals('and cleared on New', '',
        FFlow.ContextToSave.Provenance.SourcePath);
    AssertFalse('', FFlow.ContextToSave.SelectedIntervalInForce);
end;

procedure TProjectWorkflowTest.AGestureInTheWindowIsRememberedForTheDocument;
begin
    //  The engine holds the windowed data, not the indices it was windowed by,
    //  so nothing but this remembers which stretch the user chose - and a save
    //  that did not ask it wrote "no interval" over a project that had one.
    FFlow.RememberInterval(3, 17);
    AssertTrue('in force', FFlow.ContextToSave.SelectedIntervalInForce);
    AssertEquals('', 3, FFlow.ContextToSave.SelectedIntervalFrom);
    AssertEquals('', 17, FFlow.ContextToSave.SelectedIntervalTo);

    FFlow.ForgetInterval;
    AssertFalse('going back to the whole profile forgets it',
        FFlow.ContextToSave.SelectedIntervalInForce);
end;

procedure TProjectWorkflowTest.WithNoSeamsGivenItUsesTheRealOnes;
var
    Plain: TProjectWorkflow;
begin
    //  THE PRODUCTION WIRING. Everywhere else here injects fakes, so the three
    //  "nothing was given, use the real one" branches were reached only by the
    //  application - which is precisely the arrangement that lets a constructor
    //  wire something to the wrong function and no test notice.
    //
    //  Opening a path that is not there exercises the real open and its
    //  refusal without writing anything: the file layer answers False, the
    //  workflow reports it, and nothing about the window moves.
    Plain := TProjectWorkflow.Create(FService, FHost);
    try
        AssertFalse('nothing to open',
            Plain.OpenProjectAt(PathIn('not-there.fitproj')));
        AssertTrue('and it said so', FHostObj.Problem <> '');
        AssertEquals('no document is claimed', '', Plain.Path);
    finally
        Plain.Free;
    end;
end;

procedure TProjectWorkflowTest.WhatWasOpenedIsWhatTheNextSaveReplaces;
begin
    //  THE FORMAT'S ONE REAL PROMISE, over the path a user actually takes:
    //  open a project written by a newer build, save it, and the section that
    //  build added is still there. The preservation lives in the writer, but it
    //  can only preserve what the save is TOLD it is replacing.
    OpenedUi.AsRead := WithPart(nil, 'future/recipe.json', '{"steps":[1,2]}');
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    FFlow.OpenProject;

    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    AssertTrue('saved', FFlow.SaveProject);
    AssertEquals('the newer build''s section went into the save',
        '{"steps":[1,2]}', ReplacedPart);
end;

procedure TProjectWorkflowTest.AfterNewThereIsNothingBeingReplaced;
begin
    //  The counterpart. A project saved from a session that opened none
    //  replaces nothing, so it must not carry the last one''s sections into a
    //  file that has nothing to do with them.
    OpenedUi.AsRead := WithPart(nil, 'future/recipe.json', '{"steps":[1,2]}');
    FHostObj.ScriptOpen(PathIn('a.fitproj'));
    FFlow.OpenProject;
    FFlow.NewProject;

    FHostObj.ScriptSaveAs(PathIn('b.fitproj'));
    AssertTrue('saved', FFlow.SaveProject);
    AssertEquals('nothing carried over', '', ReplacedPart);
end;

procedure TProjectWorkflowTest.WithNothingUnsavedClosingAsksNothing;
begin
    //  Being asked to save something you did not change is how people learn to
    //  dismiss the question without reading it.
    FHostObj.ScriptUnsavedWork(False);
    AssertTrue('closes', FFlow.MayClose);
    AssertFalse('nothing was asked',
        FHostObj.Log.Saw('AskSaveBeforeClosing'));
end;

procedure TProjectWorkflowTest.DecliningToSaveStillCloses;
begin
    FHostObj.ScriptUnsavedWork(True);
    FHostObj.ScriptCloseAnswer(saNo);
    AssertTrue('closes', FFlow.MayClose);
    AssertEquals('and nothing was written', '', SavedTo);
end;

procedure TProjectWorkflowTest.CancellingTheQuestionStopsTheClose;
begin
    //  Cancel cancels the CLOSE, not the save.
    FHostObj.ScriptUnsavedWork(True);
    FHostObj.ScriptCloseAnswer(saCancel);
    AssertFalse('stays open', FFlow.MayClose);
end;

procedure TProjectWorkflowTest.ChoosingToSaveWritesTheProjectAndCloses;
begin
    GivenAProblem;
    FHostObj.ScriptUnsavedWork(True);
    FHostObj.ScriptCloseAnswer(saYes);
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'));
    AssertTrue('closes', FFlow.MayClose);
    AssertEquals('having written it', PathIn('a.fitproj'), SavedTo);
end;

procedure TProjectWorkflowTest.ACancelledSaveStopsTheClose;
begin
    //  THE ONE PATH WHERE THIS PROGRAM OVERRIDES WHAT THE USER ASKED FOR. They
    //  said save; the save did not happen; closing anyway destroys exactly the
    //  work they asked to keep. A cancelled name dialog is that case, and it is
    //  the easy one to get wrong, because nothing failed.
    GivenAProblem;
    FHostObj.ScriptUnsavedWork(True);
    FHostObj.ScriptCloseAnswer(saYes);
    FHostObj.ScriptSaveAs(PathIn('a.fitproj'), False);
    AssertFalse('stays open', FFlow.MayClose);
    AssertEquals('and nothing was written', '', SavedTo);
end;

{ ---- overwriting --------------------------------------------------------- }

procedure TProjectWorkflowTest.SavingOverAnExistingProjectAsksFirst;
begin
    //  THE ONE QUESTION WHOSE WRONG ANSWER DESTROYS WORK THE USER DID NOT OFFER
    //  UP, and it was not being put at all: Save As wrote over whatever was
    //  there without a word. The export conversation has asked this since
    //  before projects existed (table_export.eqFileExists); the document that
    //  matters more than any export did not.
    GivenAProblem;
    ExistingPaths := PathIn('taken.fitproj');
    FHostObj.ScriptSaveAs(PathIn('taken.fitproj'));
    FHostObj.ScriptConfirmAnswer(True);

    AssertTrue('saved', FFlow.SaveProjectAs);
    AssertTrue('and the user was asked about that file: ' + FHostObj.Asked,
        Pos('taken.fitproj', FHostObj.Asked) > 0);
end;

procedure TProjectWorkflowTest.ARefusedOverwriteWritesNothingAndChangesNothing;
begin
    //  NO IS NO, all the way: nothing written, nothing retitled, nothing
    //  remembered as the current document. "Cancelled" and "failed" differ, and
    //  neither is "saved".
    GivenAProblem;
    ExistingPaths := PathIn('taken.fitproj');
    FHostObj.ScriptSaveAs(PathIn('taken.fitproj'));
    FHostObj.ScriptConfirmAnswer(False);

    AssertFalse('not saved', FFlow.SaveProjectAs);
    AssertEquals('nothing was written', '', SavedTo);
    AssertEquals('the write was not even attempted', 0, SaveCount);
    AssertEquals('nothing was retitled', '', FHostObj.Shown);
    AssertEquals('and no document was adopted', '', FFlow.Path);
    AssertEquals('and it is not reported as a failure', '', FHostObj.Problem);
end;

procedure TProjectWorkflowTest.AnAcceptedOverwriteWrites;
begin
    GivenAProblem;
    ExistingPaths := PathIn('taken.fitproj');
    FHostObj.ScriptSaveAs(PathIn('taken.fitproj'));
    FHostObj.ScriptConfirmAnswer(True);

    AssertTrue('saved', FFlow.SaveProjectAs);
    AssertEquals('over the file the user chose', PathIn('taken.fitproj'),
        SavedTo);
    AssertEquals('and it is the document now', PathIn('taken.fitproj'),
        FFlow.Path);
end;

procedure TProjectWorkflowTest.NothingIsAskedWhenNoFileIsThere;
begin
    //  The ordinary save. A question here would be one the user learns to
    //  dismiss without reading, which is how the question that matters stops
    //  being read.
    GivenAProblem;
    FHostObj.ScriptSaveAs(PathIn('fresh.fitproj'));

    AssertTrue('saved', FFlow.SaveProjectAs);
    AssertEquals('nothing was asked', '', FHostObj.Asked);
end;

procedure TProjectWorkflowTest.TheQuestionIsAboutTheNameThatWillBeWritten;
begin
    //  THE EXTENSION IS SETTLED FIRST, and this is why the widget set's own
    //  overwrite prompt cannot be the whole answer: the user types "taken", the
    //  dialog sees a name nothing has, and the file this program then writes is
    //  taken.fitproj - which does exist.
    GivenAProblem;
    ExistingPaths := PathIn('taken.fitproj');
    FHostObj.ScriptSaveAs(PathIn('taken'));
    FHostObj.ScriptConfirmAnswer(False);

    AssertFalse('refused', FFlow.SaveProjectAs);
    AssertTrue('asked about the file that would have been written: ' +
        FHostObj.Asked, Pos('taken.fitproj', FHostObj.Asked) > 0);
end;

procedure TProjectWorkflowTest.TheQuestionComesBeforeTheWrite;
var
    Ask, Write_: longint;
begin
    //  A question put after the file has been replaced is not a question.
    GivenAProblem;
    ExistingPaths := PathIn('taken.fitproj');
    FHostObj.ScriptSaveAs(PathIn('taken.fitproj'));
    FHostObj.ScriptConfirmAnswer(True);
    FFlow.SaveProjectAs;

    Ask := Pos('Confirm', FHostObj.Log.Sequence);
    Write_ := Pos('ShowDocument', FHostObj.Log.Sequence);
    AssertTrue('the user was asked', Ask > 0);
    AssertTrue('before the document was replaced and retitled',
        Ask < Write_);
end;

{ ---- importing over an open project --------------------------------------- }

procedure TProjectWorkflowTest.ImportingIntoAnEmptySessionAsksNothing;
begin
    //  Nothing built, nothing edited: there is nothing to lose, and a question
    //  here is one the user learns to dismiss without reading.
    AssertTrue('goes ahead', FFlow.MayImportProfile(PathIn('two.dat')));
    AssertEquals('and nothing was asked', '', FHostObj.Asked);
end;

procedure TProjectWorkflowTest.ImportingOverAModelAsksFirst;
begin
    //  THE REPORT THIS COMES FROM: opening a data file inside a project threw
    //  the whole model away without a word.
    GivenAModel;
    FHostObj.ScriptConfirmAnswer(True);
    AssertTrue('the user agreed, so it goes ahead',
        FFlow.MayImportProfile(PathIn('two.dat')));
    AssertTrue('and was asked: ' + FHostObj.Asked, FHostObj.Asked <> '');
end;

procedure TProjectWorkflowTest.ARefusedImportIsRefusedRatherThanReported;
begin
    GivenAModel;
    FHostObj.ScriptConfirmAnswer(False);
    AssertFalse('not imported', FFlow.MayImportProfile(PathIn('two.dat')));
    //  Cancelled is not failed: nothing is reported to the user, who has just
    //  said what they wanted.
    AssertEquals('', '', FHostObj.Problem);
end;

procedure TProjectWorkflowTest.UnsavedWorkIsAlsoWorthAskingAbout;
begin
    //  NO MODEL AND STILL SOMETHING TO LOSE: picks placed, tables edited, and
    //  nothing built from them yet. The import takes those too.
    FHostObj.ScriptUnsavedWork(True);
    FHostObj.ScriptConfirmAnswer(False);
    AssertFalse('asked, and refused', FFlow.MayImportProfile(PathIn('two.dat')));
    AssertTrue('the question was put', FHostObj.Asked <> '');
end;

procedure TProjectWorkflowTest.TheImportQuestionNamesTheFileBeingLoaded;
begin
    GivenAModel;
    FHostObj.ScriptConfirmAnswer(True);
    FFlow.MayImportProfile(PathIn('two.dat'));
    AssertTrue('names it: ' + FHostObj.Asked,
        Pos('two.dat', FHostObj.Asked) > 0);
end;

procedure TProjectWorkflowTest.ReloadingAsksTheSameQuestionAboutTheFileItCameFrom;
begin
    //  RE-READING DISCARDS THE MODEL exactly as importing another file does,
    //  and asking about one and not the other teaches a user that the answer
    //  does not matter. The name comes from the document's own provenance.
    GivenAModel;
    FFlow.RememberSource(PathIn('two.dat'), 'dat');
    FHostObj.ScriptConfirmAnswer(False);
    AssertFalse('refused', FFlow.MayReloadProfile);
    AssertTrue('and it named the file it would re-read: ' + FHostObj.Asked,
        Pos('two.dat', FHostObj.Asked) > 0);
end;

initialization
    //  A UNIT TEST. It was an integration one, because the file calls were
    //  inline - and the whole unit then measured 0 % with eighteen tests over
    //  it, since only the unit half is measured. That is the trap testing.md
    //  names: tests sitting in the half nobody counts. The file layer is a seam
    //  now, so nothing here touches a disk; fit_project_file is what the
    //  integration tests drive.
    RegisterTest('unit', TProjectWorkflowTest);
end.
