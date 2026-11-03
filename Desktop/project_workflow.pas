// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(New, Open, Save, Save As and what closing does about unsaved work -
the sequences, with the dialogs left to the window.)

WHY IT IS HERE RATHER THAN IN THE FORM. It was in the form, and the coverage gate
said what that cost: the excluded UI-wrapper group grew by eighty-one lines, which
is eighty-one lines of decision that stopped being counted. That group is a debt
register whose total may only shrink, so the decisions come out and the window
keeps the dialogs.

WHAT IS A DECISION HERE, and every one of them has a way of going wrong quietly:

  * Save with no path behaves as Save As. A Save that silently picks a name puts
    the user's work somewhere they did not choose.
  * A cancelled dialog is not a failed save and not a successful one - it is
    "nothing happened", and at closing time it has to stop the close, because the
    user has neither kept their work nor said to discard it.
  * The path is only remembered, and the window only retitled, AFTER the write
    succeeds. Doing it first names a document that is not there.
  * A project is marked saved after the REFRESH on open, not before: filling the
    tables is what sets the modified flags, so the other order leaves a project
    dirty the instant it is opened and asks about work nobody did.
  * A failed open leaves the problem the user already had alone. It is reported,
    not raised: they chose a file, and choosing the wrong one is ordinary.
}
unit project_workflow;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_fit_service, close_query, project_commands, coordinate_axis,
    fit_project_document, fit_project_session, fit_project_file,
    project_ui_context, fit_project_provenance, log, int_project_host,
    //  For TProjectParts: the baseline is kept part by part so that a document
    //  that differs from its file can say which part differs.
    fit_project_archive,
    //  What a project mixing modules is told on opening.
    fit_advice,
    recent_project,
    //  The models the document has been through: it is the document's, so it
    //  lives with the rest of the document's memory, here.
    model_history, model_history_session, model_history_json, history_rows,
    fit_progress_json,
    //  What each step of an open cost.
    client_log;

type
    { How a project is written and read.

      SEAMS, and they carry this comment so they can be re-checked later. The
      sequences below decide things that go wrong silently - what order to
      retitle in, what a cancelled dialog means at closing time - and with the
      file calls inline none of that was reachable without writing a file, which
      by this project's rule puts it in the half that is not measured. It showed
      up as this unit sitting at 0 % with eighteen tests over it.

      The syscalls stay behind these two; the decisions come out in front.
      Callers pass nothing and get the real ones. }
    TSaveProjectFunc = function(AService: IFitService;
        const AContext: TProjectClientContext;
        const APrevious: TProjectDocument; const APath: string;
        out AWritten: TProjectDocument; out AFault: string): boolean;
    TOpenProjectFunc = function(AService: IFitService; const APath: string;
        out ADoc: TProjectDocument; out AFault: string): boolean;
    { What to say about the data file, or '' when there is nothing to say.

      A SEAM for the same reason as the two above: whether there IS anything to
      say can only be answered by reading the file, and the decision worth
      testing is what happens when there is. }
    TSourceNoticeFunc = function(
        const AProvenance: TProjectProvenance): string;

    { Drives the document commands over a service and a window.

      Holds the one piece of state the commands share - where the open document
      lives - because that is what makes Save different from Save As. }
    TProjectWorkflow = class
    private
        FService: IFitService;
        FHost: IProjectHost;
        FPath: string;
        { THE DOCUMENT'S OWN MEMORY, and it lives here rather than in the window
          because it belongs to the document rather than to any widget: which
          stretch of the profile is selected (as indices - the engine holds the
          windowed data, not the indices it was windowed by) and where the data
          came from. It was in the window, and being there meant a new project
          claimed the previous file's provenance and a reopened project lost its
          interval on the next save. }
        FIntervalInForce: boolean;
        FIntervalFrom, FIntervalTo: longint;
        FProvenance: TProjectProvenance;
        { THE DOCUMENT THIS ONE REPLACES, kept for one reason: a part - or a
          member inside a part - written by a newer build survives a save only
          if the writer starts from what was READ. Empty until a project is
          opened, which is right: a project saved from a session that opened
          none replaces nothing. }
        FOpened: TProjectDocument;
        FSave: TSaveProjectFunc;
        FOpen: TOpenProjectFunc;
        FNotice: TSourceNoticeFunc;
        { Whether a path is already taken. A seam for the reason the three above
          are: this unit decides whether to ask before replacing a file, and a
          test has to reach both answers without writing one. }
        FExists: TPathExists;
        { The document as it stood when it last matched a file - opened, saved,
          imported or started empty - as ProjectFingerprint reduces it. '' is
          unknown. }
        FBaseline: string;
        { THE SAME BASELINE, PART BY PART, kept for one purpose: when the
          comparison says there is unsaved work, saying WHICH part of the
          document moved. "Project has been modified" with nothing modified is
          otherwise a report nobody can act on - the difference is a few bytes
          inside a section the log truncates. Hashes, not contents, so this
          costs a line per part rather than a second copy of the project. }
        FBaselineParts: TProjectParts;
        { WHAT COULD HAVE MOVED IT, as it stood when the baseline was taken:
          the writes the service had sent, the history's revision, and what
          only the window holds. While all three are as they were, nothing
          the file holds can differ, and the baseline is not compared at all -
          see HasUnsavedWork. }
        FBaselineChanges: longint;
        { False when a write was sent while the baseline was being read - the
          problem made by its first request, a run's own thread - so it is not
          known which side of the baseline the write fell, and the next close
          compares. }
        FBaselineChangesKnown: boolean;
        FBaselineHistory: longint;
        FBaselineClient: string;
        { A baseline being read in the background (a TBaselineReader), or
          nil - see StartBaselineInBackground. }
        FBaselineReader: TThread;
        { The background read could not give a baseline to trust: anything
          that moved since makes a close ask. }
        FBaselineUnusable: boolean;
        { Set by the constructor: the work that touches no widget - reading and
          restoring a file, writing a save, reading the model back to compare
          it, an open's baseline - runs on a thread of its own while the window
          is let answer (IProjectHost.KeepResponding). What the window makes;
          a test of the sequences runs it all on its own thread. }
        FInBackground: boolean;
        { A close the user asked for while a run was going, held until the run
          has ended - see MayClose. }
        FCloseHeld: boolean;
        { THE MODELS THIS DOCUMENT HAS BEEN THROUGH. Owned here for the reason
          the interval and the provenance are: it belongs to the document, is
          saved with it, read back with it and cleared with it, and no widget
          outlives the gesture that shows it. }
        FHistory: TModelHistory;
        procedure Rebase;
        { Waits for a background baseline and throws it away: something else
          is taking the baseline now. }
        procedure DropPendingBaseline;
        { Runs AJob (a TWorkflowJob) - on its own thread when FInBackground,
          letting the window answer until it has finished; on this one
          otherwise. The caller reads what it found and frees it. }
        procedure Await(AJob: TThread);
        { ProjectFingerprintParts, through Await. }
        function ReadBackFingerprint(
            const AContext: TProjectClientContext): TProjectParts;
        { OpenProjectAt's work, under its busy state: what is to be reported
          is added to AProblems, for the caller to say once that state is
          over. }
        function OpenAt(const APath: string;
            var AProblems: TStringArray): boolean;
        { Writes the document to APath and adopts it, under the busy state;
          a failure is reported once that state is over. }
        function WriteTo(const APath: string): boolean;
        { Whether nothing has been sent, recorded or chosen since the baseline
          that a file could hold. }
        function NothingCouldHaveMoved(const AContext: TProjectClientContext;
            AChanges: longint): boolean;
        { Takes what could move the baseline as it stands now. }
        procedure NoteWhatCouldMoveIt(const AContext: TProjectClientContext;
            AChangesBefore, AChangesAfter: longint);
    public
        constructor Create(AService: IFitService; AHost: IProjectHost;
            ASave: TSaveProjectFunc = nil; AOpen: TOpenProjectFunc = nil;
            ANotice: TSourceNoticeFunc = nil; AExists: TPathExists = nil;
            AInBackground: boolean = False);

        { What is saved: the window's answer plus this object's memory. }
        function ContextToSave: TProjectClientContext;
        { Closes what is open and starts empty - after the question closing the
          window asks, when there is unsaved work. }
        procedure NewProject;
        { Closes what is open and starts empty WITHOUT asking: for a caller
          that has just asked MayReplaceDocument itself - data_source_import,
          whose second question was the same one again. }
        procedure StartEmpty;
        { Asks about unsaved work, then for a project, and opens it. False when
          nothing was opened - cancelled, or the file was not a project. }
        function OpenProject: boolean;
        { File > Open Recent: the same question, then APath. }
        function OpenRecentProject(const APath: string): boolean;
        { Opens APath without asking anything, which is what the command line
          does: nothing is open yet to lose. }
        function OpenProjectAt(const APath: string): boolean;
        { Saves; asks for a name when there is not one yet. }
        function SaveProject: boolean;
        { Always asks for a name. }
        function SaveProjectAs: boolean;
        { Whether the window may close, having asked about unsaved work and
          saved it if that is what the user chose.

          NOT WHILE A RUN IS GOING: the run is stopped, as Stop stops it, and
          the close is held until it has ended - see CloseOnceRunEnds. }
        function MayClose: boolean;
        { The window has taken in the end of a run: whether a close held for
          it should go ahead now. Answers True once per held close. }
        function CloseOnceRunEnds: boolean;
        { The same decision for anything else that replaces the open document:
          New Project, Open Project, Open Recent. They used to replace it
          without a word while closing the window asked - the same loss, and
          only one of the two ways to it was guarded. }
        function MayReplaceDocument: boolean;
        { Whether there is work here that no file holds: the window's own flags,
          or anything that would be saved having changed since the document
          last matched a file. The flags alone missed whatever reaches neither
          table - points picked before any model exists. }
        function HasUnsavedWork: boolean;

        { Whether a data file may replace the data this project is built on.

          IT IS NOT A SAVE QUESTION. Importing a profile starts the model again
          - the curves, the picks and the parameters describe data that is being
          replaced - and that is right, but it is invisible from a menu item
          called "Import Profile". So it is asked about when there is something
          to lose, and not otherwise: a question on an empty session is one the
          user learns to dismiss without reading, which is how the question that
          matters stops being read. }
        function MayImportProfile(const APath: string): boolean;
        { File > Reload Profile: re-reads the data file the OPEN DOCUMENT says
          it came from, after the same question an import asks - re-reading
          discards the model in exactly the same way.

          FROM THE PROVENANCE, NOT FROM THE LAST IMPORT. The client used to
          re-read whichever file its loader had last read in this session, so
          after importing one file and opening a project built from another the
          question named the project's file and the command read the other;
          with no import at all it tripped an internal check. A document that
          names no file, or names one that is gone, is said so and nothing is
          read. }
        procedure ReloadProfile;

        { The user has selected a stretch of the profile, or gone back to all of
          it. Told rather than asked, because it is a gesture in the window and
          nothing else observes it. }
        procedure RememberInterval(AFrom, ATo: longint);
        procedure ForgetInterval;
        { A data file has been loaded: describe it while it is certainly there
          and certainly the one these numbers came from. }
        procedure RememberSource(const APath, ALoaderName: string);
        { Whether APath is not the file the document's data came from - which
          makes it new data rather than the same data read again (Reload
          Profile), and so a reason to start its axes again on Automatic. }
        function IsNewSource(const APath: string): boolean;
        { And where that file was FETCHED from, when it was fetched rather than
          chosen on this computer.

          WHY IT IS NOT ENOUGH TO KEEP THE PATH. A downloaded file's path is a
          name in a cache directory, which says nothing about the service, the
          query or the day - and that is exactly what has to be quoted when a
          result computed from it is published.

          Called AFTER RememberSource, which describes the file itself; this
          adds to what that recorded rather than replacing it. Empty for a file
          the user opened themselves, and then the project carries exactly the
          keys it always carried. }
        { Where the open project's data came from, as it will be saved -
          what an exported chart cites (chart_image). }
        property Provenance: TProjectProvenance read FProvenance;
        procedure RememberOrigin(const ASourceId, ASourceTitle, AQuery,
            AAddress, ARetrievedAt: string);
        { Where Save As starts: the open document's file, or for a project
          that has none, its DATA FILE'S NAME as a project - beside the data
          file when the user chose it, and the name alone when it was fetched,
          whose folder is a cache that is emptied. '' with neither. }
        function SaveAsSuggestion: string;
        { What the project is called where it is named to the user - the
          unsaved-work question: its file's name, or the name Save As would
          offer, without the extension; Untitled with neither. }
        function DocumentName: string;

        { Where the open document lives, or empty when it has never been
          saved. }
        property Path: string read FPath write FPath;

        { ---- the model history ---------------------------------------------

          Each of these is a gesture the window forwards; the window then
          redraws the History tab from History. }

        { A run of ARunKind has ended (AStopped: cut short by Stop): records
          the model it reached. False when that model is the current entry's -
          the run changed nothing. }
        function RecordRun(ARunKind: THistoryRunKind; AStopped: boolean;
            const ARunLog: TRunEvents = nil;
            const ARFactor: TFitProgressSamples = nil): boolean;
        { Makes the entry AId the live model and redraws the window from it.
          Reported, not raised, when it cannot be done. }
        function MakeHistoryEntryCurrent(const AId: string): boolean;
        { Asks, then removes the entry AId. False when the user said no. The
          question names the model as its row does, on ALossAxis - the scale
          View > R-factor Scale put in force, or nil for the R-factor as it
          is. }
        function DeleteHistoryEntry(const AId: string;
            ALossAxis: TCoordinateAxis): boolean;
        procedure RenameHistoryEntry(const AId, AName: string);
        property History: TModelHistory read FHistory;

        { Takes the baseline on a thread of its own: what an open does when
          the workflow was made with AInBackground. What only the window
          holds, the history's revision and the writes sent are noted at once,
          so a close with nothing changed never waits for it. }
        procedure StartBaselineInBackground;
        { Waits for a baseline being taken in the background, and adopts it -
          or, when a write went out or the history moved while it was read,
          marks it unusable, so that the next close asks. }
        procedure WaitForBaseline;

        destructor Destroy; override;
    end;

implementation

uses
    //  LocalTimeToUniversal: FPC 3.2.4 also has it in SysUtils, 3.2.2 (the
    //  CI and release toolchain) only here - so a local 3.2.4 build hides
    //  its absence.
    DateUtils;

type
    { A piece of the document's work that touches no widget, run off the
      window's thread when the workflow is the window's (Await). Run is the
      work; a fault in it is caught and kept, never raised on a thread nobody
      is listening to. }
    TWorkflowJob = class(TThread)
    public
        Failure: string;
        procedure Run; virtual; abstract;
    protected
        procedure Execute; override;
    end;

    { Reads a project file and restores it into the engine (TOpenProjectFunc). }
    TOpenJob = class(TWorkflowJob)
    public
        Open: TOpenProjectFunc;
        Service: IFitService;
        Path: string;
        Doc: TProjectDocument;
        Fault: string;
        Opened: boolean;
        procedure Run; override;
    end;

    { Captures the model and writes it (TSaveProjectFunc). }
    TSaveJob = class(TWorkflowJob)
    public
        Save: TSaveProjectFunc;
        Service: IFitService;
        Context: TProjectClientContext;
        Previous: TProjectDocument;
        Path: string;
        Written: TProjectDocument;
        Fault: string;
        Saved: boolean;
        procedure Run; override;
    end;

    { Reads the model back and reduces it to what closing compares. }
    TFingerprintJob = class(TWorkflowJob)
    public
        Service: IFitService;
        Context: TProjectClientContext;
        Parts: TProjectParts;
        procedure Run; override;
    end;

procedure TWorkflowJob.Execute;
begin
    try
        Run;
    except
        on E: Exception do
            Failure := E.Message;
    end;
end;

procedure TOpenJob.Run;
begin
    Opened := Open(Service, Path, Doc, Fault);
end;

procedure TSaveJob.Run;
begin
    Saved := Save(Service, Context, Previous, Path, Written, Fault);
end;

procedure TFingerprintJob.Run;
begin
    Parts := ProjectFingerprintParts(Service, Context);
end;

function TProjectWorkflow.ReadBackFingerprint(
    const AContext: TProjectClientContext): TProjectParts;
var
    Job: TFingerprintJob;
begin
    //  OFF THE WINDOW'S THREAD (Await): reading the model back is reads only.
    Job := TFingerprintJob.Create(True);
    try
        Job.Service := FService;
        Job.Context := AContext;
        Await(Job);
        Result := Job.Parts;
    finally
        Job.Free;
    end;
end;

procedure TProjectWorkflow.Await(AJob: TThread);
var
    Job: TWorkflowJob;
begin
    Job := AJob as TWorkflowJob;
    if not FInBackground then
    begin
        //  On this thread, caught as the thread would catch it.
        Job.Execute;
        Exit;
    end;
    Job.FreeOnTerminate := False;
    Job.Start;
    //  ANSWERING WHILE IT WORKS: the window repaints, can be moved, and says
    //  how long it has been - and holds the user's commands back meanwhile.
    while not Job.Finished do
        FHost.KeepResponding;
    Job.WaitFor;
end;

constructor TProjectWorkflow.Create(AService: IFitService; AHost: IProjectHost;
    ASave: TSaveProjectFunc; AOpen: TOpenProjectFunc;
    ANotice: TSourceNoticeFunc; AExists: TPathExists;
    AInBackground: boolean);
begin
    inherited Create;
    FService := AService;
    FHost := AHost;
    FPath := '';
    FOpened := EmptyProjectDocument;
    FHistory := TModelHistory.Create;
    FSave := ASave;
    if not Assigned(FSave) then
        FSave := @WriteProjectFile;
    FOpen := AOpen;
    if not Assigned(FOpen) then
        FOpen := @OpenProjectFile;
    FNotice := ANotice;
    if not Assigned(FNotice) then
        FNotice := @SourceChangeNotice;
    FInBackground := AInBackground;
    FExists := AExists;
    if not Assigned(FExists) then
        //  recent_project's, and NOT @FileExists: there is no one-argument
        //  FileExists, and taking its address is how the last project stopped
        //  reopening (findings.md, "The sixth gap").
        FExists := @DefaultPathExists;
end;

procedure TProjectWorkflow.RememberInterval(AFrom, ATo: longint);
begin
    FIntervalInForce := True;
    FIntervalFrom := AFrom;
    FIntervalTo := ATo;
end;

procedure TProjectWorkflow.ForgetInterval;
begin
    FIntervalInForce := False;
end;

procedure TProjectWorkflow.NoteWhatCouldMoveIt(
    const AContext: TProjectClientContext;
    AChangesBefore, AChangesAfter: longint);
begin
    FBaselineChanges := AChangesAfter;
    FBaselineChangesKnown := AChangesBefore = AChangesAfter;
    FBaselineHistory := FHistory.Revision;
    FBaselineClient := ClientHalfFingerprint(AContext);
end;

function TProjectWorkflow.NothingCouldHaveMoved(
    const AContext: TProjectClientContext; AChanges: longint): boolean;
begin
    Result := FBaselineChangesKnown and (AChanges = FBaselineChanges) and
        (FHistory.Revision = FBaselineHistory) and
        (ClientHalfFingerprint(AContext) = FBaselineClient);
end;

procedure TProjectWorkflow.Rebase;
var
    Context: TProjectClientContext;
    Changes: longint;
begin
    DropPendingBaseline;
    //  COUNTED BEFORE AND AFTER THE CAPTURE: a write sent while it reads -
    //  from a run's own thread - leaves it unknown which side of the baseline
    //  the write fell, and the next comparison is made rather than skipped.
    Changes := FService.ChangeCount;
    Context := ContextToSave;
    //  ASKED ONCE. Each capture reads the whole problem back from the engine,
    //  which over HTTP is some hundreds of requests; the fingerprint is that
    //  same answer reduced, not a second reading of it.
    FBaselineParts := ProjectFingerprintParts(FService, Context);
    FBaseline := FingerprintOfParts(FBaselineParts);
    FBaselineUnusable := False;
    NoteWhatCouldMoveIt(Context, Changes, FService.ChangeCount);
end;

function TProjectWorkflow.HasUnsavedWork: boolean;
var
    NowParts: TProjectParts;
    Now_, Moved: string;
    Context: TProjectClientContext;
    Changes: longint;
begin
    Result := FHost.HasUnsavedWork;
    if Result then
        Exit;
    //  UNKNOWN - the engine could not be read when the baseline was taken -
    //  is left to the window's flags, as it always was; a baseline still
    //  being read in the background is not unknown.
    if (FBaseline = '') and not Assigned(FBaselineReader) and
        not FBaselineUnusable then
        Exit;
    Changes := FService.ChangeCount;
    Context := ContextToSave;
    //  NOTHING SENT, RECORDED OR CHOSEN SINCE THE BASELINE: then nothing a file
    //  holds can differ from it, and the problem is not read back. Reading it
    //  back - the profile, every curve, a module's state, the history written
    //  out as the file holds it - was the 36 seconds an untouched S&P 500
    //  project took to close (2026-10-24). Rejected: counting any write at all
    //  as a change, which would ask about a run that changed nothing.
    if NothingCouldHaveMoved(Context, Changes) then
        Exit(False);
    //  UNDER THE BUSY CURSOR, and over before the question is put.
    FHost.BeginBusy(CheckingForChangesNotice(DocumentName));
    try
        //  THE BASELINE FIRST, if it is still being read: it is now needed.
        WaitForBaseline;
        if FBaselineUnusable then
            NowParts := nil
        else
            NowParts := ReadBackFingerprint(Context);
    finally
        FHost.EndBusy;
    end;
    //  NOTHING TO COMPARE WITH, and something moved: asked about, rather than
    //  risk a change nobody is asked about.
    if FBaselineUnusable then
    begin
        WriteLog('unsaved work: something changed while the baseline was ' +
            'read, so it is not known what, and the close asks', Debug);
        Exit(True);
    end;
    Now_ := FingerprintOfParts(NowParts);
    Result := (Now_ <> '') and (Now_ <> FBaseline);
    //  FOUND AS IT WAS: what was sent changed nothing the file holds, so the
    //  next close need not look again until something else is.
    if (Now_ <> '') and not Result then
        NoteWhatCouldMoveIt(Context, Changes, FService.ChangeCount);
    if not Result then
        Exit;
    //  SAID IN THE LOG, not just answered. This is the line that turns "it
    //  asked me to save a project I never touched" from a report that can only
    //  be guessed at into one that names the section which moved.
    Moved := DifferingPartNames(FBaselineParts, NowParts);
    if Moved = '' then
        Moved := '(the parts agree - the difference is in how they are ' +
            'reduced)';
    WriteLog('unsaved work: the document differs from the file in ' + Moved,
        Debug);
end;

function TProjectWorkflow.IsNewSource(const APath: string): boolean;
begin
    Result := (FProvenance.SourcePath = '') or
        not SameFileName(ExpandFileName(APath),
            ExpandFileName(FProvenance.SourcePath));
end;

procedure TProjectWorkflow.RememberSource(const APath, ALoaderName: string);
begin
    //  DESCRIBED NOW, while the file is certainly there and certainly the one
    //  these numbers came from - by save time it may be gone.
    DescribeSourceFile(APath, ALoaderName, FProvenance);
    //  A FRESH IMPORT IS NOT WORK: the file it came from still holds it.
    //
    //  AND THE WINDOW HAS TO BE TOLD SO. Loading a data file fills the tables,
    //  and filling a table is what raises the window's two modified flags -
    //  they say a table was drawn, not that anyone changed anything. Those
    //  flags are read BEFORE the comparison below and short-circuit it, so
    //  rebasing alone left every --infile session and every File > Import
    //  Profile asking "Project has been modified. Save?" at closing time about
    //  work nobody had done. Nothing is lost by clearing them here: an import
    //  that would replace real work has already asked about it (MayImportProfile).
    FHost.MarkSaved;
    Rebase;
end;

procedure TProjectWorkflow.RememberOrigin(const ASourceId, ASourceTitle,
    AQuery, AAddress, ARetrievedAt: string);
begin
    FProvenance.OriginSourceId := ASourceId;
    FProvenance.OriginSourceTitle := ASourceTitle;
    FProvenance.OriginQuery := AQuery;
    FProvenance.OriginAddress := AAddress;
    FProvenance.OriginRetrievedAt := ARetrievedAt;
    //  UNSAVED WORK, unlike a file the user chose, and deliberately not
    //  rebased: the origin is part of what is saved, so the document now
    //  differs from the baseline RememberSource took - and the download it was
    //  read from is deleted once the import has run, so no file holds it.
end;

function TProjectWorkflow.DocumentName: string;
begin
    Result := ChangeFileExt(ExtractFileName(SaveAsSuggestion), '');
    if Result = '' then
        Result := UntitledProjectName;
end;

function TProjectWorkflow.SaveAsSuggestion: string;
begin
    if FPath <> '' then
        Exit(FPath);
    if FProvenance.SourcePath = '' then
        Exit('');
    Result := ChangeFileExt(FProvenance.SourcePath, ProjectExtension);
    if FProvenance.OriginSourceId <> '' then
        Result := ExtractFileName(Result);
end;

{ What the window holds, plus what this object remembers for the document. }
function TProjectWorkflow.ContextToSave: TProjectClientContext;
begin
    Result := FHost.CurrentContext;
    Result.SelectedIntervalInForce := FIntervalInForce;
    Result.SelectedIntervalFrom := FIntervalFrom;
    Result.SelectedIntervalTo := FIntervalTo;
    //  The window's answer carries the running build's version and nothing else
    //  about where the data came from; that is this object's to say.
    FProvenance.AppVersion := Result.Provenance.AppVersion;
    Result.Provenance := FProvenance;
    Result.History := FHistory;
end;

procedure TProjectWorkflow.NewProject;
begin
    if not MayReplaceDocument then
        Exit;
    StartEmpty;
end;

procedure TProjectWorkflow.StartEmpty;
begin
    FPath := '';
    FIntervalInForce := False;
    FProvenance := Default(TProjectProvenance);
    //  Nothing is being replaced any more, so nothing is carried forward.
    FOpened := EmptyProjectDocument;
    FHistory.Clear;
    FHost.ClearEverything;
    //  AN EMPTY DOCUMENT'S PLAN, which is how the window's document-scoped
    //  memory is cleared: the selected interval and where the data came from.
    //  Without this a new project claimed the previous file's provenance, and
    //  saved an interval nobody was in.
    FHost.ApplyWorkingContext(PlanUiRestore(EmptyProjectDocument,
        FHost.TabsShown, False));
    FHost.ShowDocument('');
    FHost.MarkSaved;
    Rebase;
end;

function TProjectWorkflow.OpenProjectAt(const APath: string): boolean;
var
    Problems: TStringArray;
    i: longint;
begin
    Problems := nil;
    //  UNDER THE BUSY CURSOR, saying what is opened: a large project takes
    //  seconds to read, restore, draw and compare with its file.
    FHost.BeginBusy(OpeningProjectNotice(ChangeFileExt(ExtractFileName(APath),
        '')));
    try
        Result := OpenAt(APath, Problems);
        //  AND THE WINDOW FINISHED WHILE THE PROGRESS IS STILL UP: the
        //  redraws the open queued ran after it, and the window stood frozen
        //  for seconds once the progress had gone. When the busy state ends,
        //  the window is ready (the user, 2026-10-25).
        if Result then
        begin
            FHost.BusyStage(FinishingWindowStage);
            FHost.FinishDrawing;
        end;
    finally
        FHost.EndBusy;
    end;
    //  SAID ONCE THE CURSOR IS BACK, in the order they were found: a message
    //  box under a busy cursor reads as one that cannot be answered yet.
    for i := 0 to High(Problems) do
        FHost.ReportProblem(Problems[i]);
end;

function TProjectWorkflow.OpenAt(const APath: string;
    var AProblems: TStringArray): boolean;

    procedure Say(const AMessage: string);
    begin
        SetLength(AProblems, Length(AProblems) + 1);
        AProblems[High(AProblems)] := AMessage;
    end;

var
    Doc: TProjectDocument;
    Fault, Notice: string;
    Started: QWord;
    Job: TOpenJob;
begin
    //  EACH STEP TIMED, as a run's end is (client_log.LogClientPhase): opening
    //  the S&P 500 project took two minutes, and the log could account for
    //  little more than half of them.
    //  NOT READ ACROSS A RESTORE: the problem is about to be replaced.
    DropPendingBaseline;
    Started := GetTickCount64;
    //  OFF THE WINDOW'S THREAD (Await): reading the file and restoring it into
    //  the engine touch no widget, and the window answers meanwhile.
    FHost.BusyStage(ReadingProjectStage);
    Job := TOpenJob.Create(True);
    try
        Job.Open := FOpen;
        Job.Service := FService;
        Job.Path := APath;
        Await(Job);
        Result := Job.Opened and (Job.Failure = '');
        Doc := Job.Doc;
        Fault := Job.Fault;
        //  A FAULT IN THE WORK is said in its own words, as a refused file is:
        //  the problem the user had is left as it was.
        if Job.Failure <> '' then
            Fault := 'This project could not be opened: ' + Job.Failure;
    finally
        Job.Free;
    end;
    LogClientPhase('opening: the file read and restored', Started);
    if not Result then
    begin
        //  REPORTED, NOT RAISED, and the problem the user already had is left
        //  exactly as it was: they chose a file, and choosing the wrong one is
        //  an ordinary mistake rather than a fault in the program.
        Say(Fault);
        Exit;
    end;
    //  A PROJECT OPENED WITHOUT ITS MODEL - its curve type is not in this
    //  build - IS A NEW ONE: saved over its own file it would lose the model
    //  the file holds and this build cannot make. So Save asks for a name,
    //  while the title still says which file it came from.
    if ModelTakenOnOpen(Doc) then
        FPath := APath
    else
        FPath := '';
    FHost.ShowDocument(APath);
    //  THE FILE'S HISTORY, from the parts as read - BEFORE the refresh, which
    //  is what redraws the History tab. A damaged history reads as none rather
    //  than refusing the project (HistoryFromParts). NONE for a project opened
    //  without its model: every model in it is of the type this build lacks,
    //  so making one current would open it without its curves too.
    HistoryFromParts(Doc.AsRead, FHistory);
    if not ModelTakenOnOpen(Doc) then
        FHistory.Clear;
    //  WHAT CAME WITH THE DATA FIRST - the bars' dates, the columns read
    //  beside the value - so the refresh draws with them, once
    //  (IProjectHost.ApplyDataContext).
    FHost.ApplyDataContext(PlanUiRestore(Doc, FHost.TabsShown, False));
    Started := GetTickCount64;
    //  ON THE WINDOW'S THREAD, being widgets: the window answers between the
    //  steps the refresh says it has ended (client_log.SetPhaseObserver).
    FHost.BusyStage(DrawingModelStage);
    FHost.RefreshFromEngine;
    LogClientPhase('opening: the window refreshed', Started);
    //  AND THE WINDOW'S OWN HALF. It was written to the file and never read
    //  back, so the axis, the tab and the picking mode did not restore at all
    //  while the user guide said they did. After the refresh, because the
    //  selected curve is looked up in the model the refresh rebuilt.
    //  ADOPTED HERE, not by the window: the interval and the provenance belong
    //  to the document, and this object is what outlives one gesture.
    FOpened := Doc;
    FIntervalInForce := Doc.SelectedIntervalInForce;
    FIntervalFrom := Doc.SelectedIntervalFrom;
    FIntervalTo := Doc.SelectedIntervalTo;
    FProvenance := Doc.Provenance;
    Started := GetTickCount64;
    FHost.BusyStage(RestoringWindowStage);
    FHost.ApplyWorkingContext(PlanUiRestore(Doc, FHost.TabsShown,
        FHost.ModelHoldsCurve(Doc.Ui.SelectedCurveId)));
    LogClientPhase('opening: the window''s own half put back', Started);
    Started := GetTickCount64;
    FHost.BusyStage(CheckingOpenedStage);

    //  AND SAY SO IF THE DATA FILE HAS MOVED ON. Logged rather than shown: the
    //  project carries its own profile, so nothing is wrong - what has happened
    //  is worth being able to find out months later, and not worth a dialog
    //  every time someone reorganises a data directory.
    Notice := FNotice(Doc.Provenance);
    if Notice <> '' then
        WriteLog(Notice, Warning);

    //  AND SAY SO IF THE FIT DOES NOT REPRODUCE. There was deliberately no
    //  check here while a restore did not measure anything - the engine read
    //  "Not calculated" until something was fitted, so it could never fire.
    //  The restore now ends by measuring a model that was measured when saved
    //  (fit_project_restore.rsEvaluate), so the comparison means something.
    //  Logged, not shown, for the reason the data-file notice is.
    Notice := RFactorReproductionNotice(Doc.RFactor, FService.GetRFactorStr);
    if Notice <> '' then
        WriteLog(Notice, Warning);
    //  AND SAY SO, OUT LOUD, IF THE MODEL MIXES MODULES - which only a project
    //  saved before one model held one module's curves can. It opens and fits
    //  as saved; what changed is that nothing more of another module may join
    //  it, and the user is told why and how to make it one module's.
    if FService.ModelMixesModules then
        Say(MixedModelWarning);
    //  AND IF A SAVED VALUE IS HELD AT ANOTHER - a project saved before a limit
    //  existed (non-negotiable 7). Through the same channel, so the window
    //  needs nothing new.
    Notice := AdviseValuesHeldOnOpen(ValuesHeldOtherThanSaved(FService, Doc));
    if Notice <> '' then
        Say(Notice);
    //  AND IF PART OF IT COULD NOT BE TAKEN - a project a pack wrote, opened
    //  without the pack. It opens with all it can (fit_project_session), and
    //  says so rather than leaving it to the log (non-negotiable 7; the free
    //  build and the S&P 500 project, 2026-10-15).
    Notice := NotTakenOnOpen(Doc);
    if Notice <> '' then
        Say(Notice);
    LogClientPhase('opening: what did not come back looked for', Started);
    //  AFTER the refresh, not before: filling the tables is what sets the
    //  modified flags, so the other order leaves a project dirty the instant it
    //  is opened and every close asks about work nobody did.
    FHost.MarkSaved;
    Started := GetTickCount64;
    //  IN THE BACKGROUND where the window asked for it: four seconds of every
    //  open of the S&P 500 project, for a comparison only a close may need.
    if FInBackground then
        StartBaselineInBackground
    else
        Rebase;
    LogClientPhase('opening: the baseline taken', Started);
end;
function TProjectWorkflow.OpenProject: boolean;
var
    Chosen: string;
begin
    Result := False;
    //  ASKED BEFORE THE DIALOG: a user who says Cancel here has not yet been
    //  made to choose a file for nothing.
    if not MayReplaceDocument then
        Exit;
    if not FHost.AskProjectToOpen(Chosen) then
        //  Cancelled. Nothing opened, nothing reported: choosing not to choose
        //  is not an error.
        Exit;
    Result := OpenProjectAt(Chosen);
end;

function TProjectWorkflow.OpenRecentProject(const APath: string): boolean;
begin
    Result := MayReplaceDocument and OpenProjectAt(APath);
end;

function TProjectWorkflow.SaveProjectAs: boolean;
var
    Chosen: string;
begin
    Result := False;
    if not FHost.AskProjectToSaveAs(SaveAsSuggestion, Chosen) then
        //  CANCELLED IS NOT FAILED, but it is not saved either - and at closing
        //  time the difference does not matter: nothing was kept.
        Exit;
    //  The extension is settled BEFORE the write, so what is reported and what
    //  is remembered are the name that actually exists.
    Chosen := ProjectFileName(Chosen);
    //  AND BEFORE THE QUESTION, which is the whole reason the question is here
    //  rather than left to the file dialog: the user types "taken", the widget
    //  set sees a name nothing has, and the file this would write is
    //  "taken.fitproj" - which does exist. Overwriting is the one answer that
    //  destroys work nobody offered up.
    if FExists(Chosen) and not FHost.Confirm(OverwriteQuestion(Chosen)) then
        //  Cancelled, exactly as cancelling the dialog is: nothing written,
        //  nothing reported, nothing changed.
        Exit;
    Result := WriteTo(Chosen);
end;

function TProjectWorkflow.WriteTo(const APath: string): boolean;
var
    Fault: string;
    Written: TProjectDocument;
    Context: TProjectClientContext;
    Changes: longint;
    Job: TSaveJob;
begin
    //  UNDER THE BUSY CURSOR, saying what is written: reading a large project
    //  back from the server, encoding it and compressing it takes seconds.
    FHost.BeginBusy(SavingProjectNotice(ChangeFileExt(ExtractFileName(APath),
        '')));
    try
        DropPendingBaseline;
        Changes := FService.ChangeCount;
        Context := ContextToSave;
        //  OFF THE WINDOW'S THREAD (Await): capturing the model, encoding and
        //  writing it touch no widget - the window's half was taken above.
        FHost.BusyStage(WritingProjectStage);
        Job := TSaveJob.Create(True);
        try
            Job.Save := FSave;
            Job.Service := FService;
            Job.Context := Context;
            Job.Previous := FOpened;
            Job.Path := APath;
            Await(Job);
            Result := Job.Saved and (Job.Failure = '');
            Written := Job.Written;
            Fault := Job.Fault;
            if Job.Failure <> '' then
                Fault := 'The project could not be saved to "' + APath +
                    '": ' + Job.Failure;
        finally
            Job.Free;
        end;
        if Result then
        begin
            //  ONLY AFTER IT WORKED. Retitling first names a document that is
            //  not there, and remembering it first offers it at the next
            //  start-up.
            FPath := APath;
            FHost.ShowDocument(APath);
            FHost.MarkSaved;
            //  FROM WHAT WAS WRITTEN, not read back a second time: the file now
            //  holds exactly that, and reading it again was the second half of
            //  every save of a large project.
            FBaselineParts := FingerprintPartsOf(Written, Context);
            FBaseline := FingerprintOfParts(FBaselineParts);
            FBaselineUnusable := False;
            NoteWhatCouldMoveIt(Context, Changes, FService.ChangeCount);
        end;
    finally
        FHost.EndBusy;
    end;
    //  SAID ONCE THE CURSOR IS BACK: a message box under a busy cursor reads as
    //  one that cannot be answered yet.
    if not Result then
        FHost.ReportProblem(Fault);
end;

function TProjectWorkflow.SaveProject: boolean;
begin
    //  The first Save is a Save As, which is what every application does and
    //  what stops a Save from choosing a name on the user's behalf.
    if SaveTargetFor(FPath) = stAskForPath then
        Exit(SaveProjectAs);
    Result := WriteTo(FPath);
end;

function TProjectWorkflow.MayImportProfile(const APath: string): boolean;
begin
    //  WHAT COUNTS AS SOMETHING TO LOSE. A model, first - that is what the
    //  import discards. And unsaved work besides, because a session can have
    //  picks and edited tables before anything has been built from them, and
    //  those go too.
    if (FService.GetCurveCount = 0) and not HasUnsavedWork then
        Exit(True);
    Result := FHost.Confirm(DiscardModelQuestion(APath));
end;

procedure TProjectWorkflow.ReloadProfile;
var
    Source: string;
begin
    Source := FProvenance.SourcePath;
    if Source = '' then
    begin
        FHost.ReportProblem(NoSourceToReloadNotice);
        Exit;
    end;
    if not FExists(Source) then
    begin
        FHost.ReportProblem(SourceGoneNotice(Source));
        Exit;
    end;
    if MayImportProfile(Source) then
        FHost.LoadProfile(Source);
end;

function TProjectWorkflow.MayClose: boolean;
begin
    //  NOT DURING A RUN. Whether there is unsaved work is answered by reading
    //  the model from the server, on routes that wait for the problem - which
    //  the run holds. Asked during a long fit, the question waited for the
    //  fit on the window's thread, and closing the window hung it for as long
    //  as the fit had left to go.
    //
    //  STOPPED, NOT ABANDONED. The run is stopped as Stop stops it and ends the
    //  way a stopped run always ends, so the questions are then put about the
    //  model it reached - which the user may well want to save. Rejected:
    //  closing without asking (the run's result would be lost unasked), and
    //  waiting for the end here (a nested message loop inside the close,
    //  re-entered by the very handler it waits for).
    if FHost.RunInProgress then
    begin
        FCloseHeld := True;
        FHost.StopRun(StoppingBeforeClosingNotice);
        Result := False;
        Exit;
    end;
    Result := MayReplaceDocument;
end;

function TProjectWorkflow.CloseOnceRunEnds: boolean;
begin
    //  ONCE. A run started after a close that was then cancelled at the save
    //  question must not close the window when it ends.
    Result := FCloseHeld;
    FCloseHeld := False;
end;

function TProjectWorkflow.MayReplaceDocument: boolean;
var
    Action: TCloseAction;
begin
    Action := caCarryOn;
    if HasUnsavedWork then
        //  ONE QUESTION, ABOUT THE DOCUMENT. What the rules are - that an
        //  unmodified document is never asked about, that "No" does not clear
        //  the modified flag, that Cancel cancels the close rather than the
        //  save - is close_query's, and stays there.
        Action := ActionForDocument(True,
            FHost.AskSaveBeforeClosing(DocumentName));
    if Action = caSaveFirst then
        //  A FAILED OR CANCELLED SAVE BLOCKS THE CLOSE. The user asked for the
        //  work to be kept; closing after failing to keep it destroys exactly
        //  what they asked to save.
        Action := ActionAfterSaving(SaveProject);
    Result := Action <> caStayAndShow;
end;

destructor TProjectWorkflow.Destroy;
begin
    DropPendingBaseline;
    FHistory.Free;
    inherited Destroy;
end;

{ Now, in UTC: the history stores every time in it. }
function NowUtc: TDateTime;
begin
    Result := LocalTimeToUniversal(Now);
end;

function TProjectWorkflow.RecordRun(ARunKind: THistoryRunKind;
    AStopped: boolean; const ARunLog: TRunEvents;
    const ARFactor: TFitProgressSamples): boolean;
begin
    Result := model_history_session.RecordRun(FService, ContextToSave, FHistory,
        ARunKind, AStopped, NowUtc, NewHistoryId, ARunLog, ARFactor);
end;

function TProjectWorkflow.MakeHistoryEntryCurrent(const AId: string): boolean;
var
    Fault: string;
    Model: TProjectDocument;
begin
    Result := model_history_session.MakeHistoryEntryCurrent(FService,
        ContextToSave, FHistory, AId, NowUtc, NewHistoryId, Fault);
    //  REDRAWN EITHER WAY: a restore that failed half way has still changed
    //  the model, and a window showing the one before would hide that.
    FHost.RefreshFromEngine;
    if not Result then
    begin
        FHost.ReportProblem(Fault);
        Exit;
    end;
    //  THE INTERVAL IS THE DOCUMENT'S MEMORY, and the model just put back was
    //  fitted over the one it recorded - the next save must say so.
    if FHistory.ModelToRestore(FHistory.IndexOf(AId), Model) then
    begin
        FIntervalInForce := Model.SelectedIntervalInForce;
        FIntervalFrom := Model.SelectedIntervalFrom;
        FIntervalTo := Model.SelectedIntervalTo;
        //  AND SAY SO IF IT IS HELD OTHERWISE, as opening a project does: an
        //  entry recorded before a limit existed is put back by the same
        //  restore and held the same way (non-negotiable 7).
        Fault := AdviseValuesHeldOnOpen(ValuesHeldOtherThanSaved(FService, Model));
        if Fault <> '' then
            FHost.ReportProblem(Fault);
    end;
end;

function TProjectWorkflow.DeleteHistoryEntry(const AId: string;
    ALossAxis: TCoordinateAxis): boolean;
var
    Index: longint;
begin
    Index := FHistory.IndexOf(AId);
    //  ASKED, because nothing brings a deleted model back - the live model is
    //  untouched, but the record of how it was reached is not.
    Result := (Index >= 0) and FHost.Confirm(HistoryDeleteQuestion(FHistory,
        Index, -GetLocalTimeOffset, ALossAxis));
    if Result then
        FHistory.Delete(AId);
end;

type
    { Reads the problem back for a baseline, off the window's thread. Reads
      only: the HTTP service takes reads from any thread, each with its own
      read batch (THttpFitService.BeginReadBatch). }
    TBaselineReader = class(TThread)
    public
        Service: IFitService;
        Context: TProjectClientContext;
        Doc: TProjectDocument;
        Read: boolean;
        ChangesAfter: longint;
    protected
        procedure Execute; override;
    end;

procedure TBaselineReader.Execute;
begin
    try
        Doc := CaptureProject(Service, Context, EmptyProjectDocument);
        Read := True;
    except
        //  An engine that cannot be reached leaves the baseline unusable,
        //  which makes the next close ask rather than claim nothing changed.
        Read := False;
    end;
    ChangesAfter := Service.ChangeCount;
end;

procedure TProjectWorkflow.StartBaselineInBackground;
var
    Reader: TBaselineReader;
    Context: TProjectClientContext;
    Changes: longint;
begin
    DropPendingBaseline;
    Changes := FService.ChangeCount;
    Context := ContextToSave;
    //  WHAT COULD MOVE IT, noted now: a close that finds none of it moved
    //  answers without the baseline, and so without waiting for it.
    NoteWhatCouldMoveIt(Context, Changes, Changes);
    FBaseline := '';
    FBaselineParts := nil;
    FBaselineUnusable := False;
    Reader := TBaselineReader.Create(True);
    Reader.FreeOnTerminate := False;
    Reader.Service := FService;
    Reader.Context := Context;
    FBaselineReader := Reader;
    Reader.Start;
end;

procedure TProjectWorkflow.WaitForBaseline;
var
    Reader: TBaselineReader;
begin
    if not Assigned(FBaselineReader) then
        Exit;
    Reader := TBaselineReader(FBaselineReader);
    FBaselineReader := nil;
    try
        //  ANSWERING WHILE IT FINISHES, as every wait of the window's is.
        while not Reader.Finished do
            FHost.KeepResponding;
        Reader.WaitFor;
        //  USABLE ONLY IF NOTHING MOVED WHILE IT WAS READ: a write that went
        //  out, or a run recorded, may or may not be in what was read, and a
        //  baseline that already holds a change would hide it at closing time.
        if Reader.Read and (Reader.ChangesAfter = FBaselineChanges) and
            (FHistory.Revision = FBaselineHistory) then
        begin
            FBaselineParts := FingerprintPartsOf(Reader.Doc, Reader.Context);
            FBaseline := FingerprintOfParts(FBaselineParts);
        end;
        FBaselineUnusable := FBaseline = '';
    finally
        Reader.Free;
    end;
end;

procedure TProjectWorkflow.DropPendingBaseline;
var
    Reader: TThread;
begin
    //  WAITED FOR, NOT ABANDONED: it reads through the service this object
    //  holds, and must not outlive it - nor read one problem into the baseline
    //  of the next.
    if not Assigned(FBaselineReader) then
        Exit;
    Reader := FBaselineReader;
    FBaselineReader := nil;
    Reader.WaitFor;
    Reader.Free;
    //  NONE TAKEN NOW: until something takes one, a change is asked about.
    FBaselineUnusable := True;
end;

procedure TProjectWorkflow.RenameHistoryEntry(const AId, AName: string);
begin
    FHistory.Rename(AId, Trim(AName));
end;

end.
