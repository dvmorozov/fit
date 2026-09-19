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
    SysUtils, int_fit_service, close_query, project_commands,
    fit_project_document, fit_project_session, fit_project_file,
    project_ui_context, fit_project_provenance, log, int_project_host,
    recent_project;

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
        out AFault: string): boolean;
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
        procedure Rebase;
    public
        constructor Create(AService: IFitService; AHost: IProjectHost;
            ASave: TSaveProjectFunc = nil; AOpen: TOpenProjectFunc = nil;
            ANotice: TSourceNoticeFunc = nil; AExists: TPathExists = nil);

        { What is saved: the window's answer plus this object's memory. }
        function ContextToSave: TProjectClientContext;
        { Closes what is open and starts empty - after the question closing the
          window asks, when there is unsaved work. }
        procedure NewProject;
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
          saved it if that is what the user chose. }
        function MayClose: boolean;
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

        { Where the open document lives, or empty when it has never been
          saved. }
        property Path: string read FPath write FPath;
    end;

implementation

constructor TProjectWorkflow.Create(AService: IFitService; AHost: IProjectHost;
    ASave: TSaveProjectFunc; AOpen: TOpenProjectFunc;
    ANotice: TSourceNoticeFunc; AExists: TPathExists);
begin
    inherited Create;
    FService := AService;
    FHost := AHost;
    FPath := '';
    FOpened := EmptyProjectDocument;
    FSave := ASave;
    if not Assigned(FSave) then
        FSave := @SaveProjectFile;
    FOpen := AOpen;
    if not Assigned(FOpen) then
        FOpen := @OpenProjectFile;
    FNotice := ANotice;
    if not Assigned(FNotice) then
        FNotice := @SourceChangeNotice;
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

procedure TProjectWorkflow.Rebase;
begin
    FBaseline := ProjectFingerprint(FService, ContextToSave);
end;

function TProjectWorkflow.HasUnsavedWork: boolean;
var
    Now_: string;
begin
    Result := FHost.HasUnsavedWork;
    if Result or (FBaseline = '') then
        Exit;
    Now_ := ProjectFingerprint(FService, ContextToSave);
    Result := (Now_ <> '') and (Now_ <> FBaseline);
end;

procedure TProjectWorkflow.RememberSource(const APath, ALoaderName: string);
begin
    //  DESCRIBED NOW, while the file is certainly there and certainly the one
    //  these numbers came from - by save time it may be gone.
    DescribeSourceFile(APath, ALoaderName, FProvenance);
    //  A FRESH IMPORT IS NOT WORK: the file it came from still holds it.
    Rebase;
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
end;

procedure TProjectWorkflow.NewProject;
begin
    if not MayReplaceDocument then
        Exit;
    FPath := '';
    FIntervalInForce := False;
    FProvenance := Default(TProjectProvenance);
    //  Nothing is being replaced any more, so nothing is carried forward.
    FOpened := EmptyProjectDocument;
    FHost.ClearEverything;
    //  AN EMPTY DOCUMENT'S PLAN, which is how the window's document-scoped
    //  memory is cleared: the selected interval and where the data came from.
    //  Without this a new project claimed the previous file's provenance, and
    //  saved an interval nobody was in.
    FHost.ApplyWorkingContext(PlanUiRestore(EmptyProjectDocument,
        FHost.TabCount, False));
    FHost.ShowDocument('');
    FHost.MarkSaved;
    Rebase;
end;

function TProjectWorkflow.OpenProjectAt(const APath: string): boolean;
var
    Doc: TProjectDocument;
    Fault, Notice: string;
begin
    Result := FOpen(FService, APath, Doc, Fault);
    if not Result then
    begin
        //  REPORTED, NOT RAISED, and the problem the user already had is left
        //  exactly as it was: they chose a file, and choosing the wrong one is
        //  an ordinary mistake rather than a fault in the program.
        FHost.ReportProblem(Fault);
        Exit;
    end;
    FPath := APath;
    FHost.ShowDocument(APath);
    FHost.RefreshFromEngine;
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
    FHost.ApplyWorkingContext(PlanUiRestore(Doc, FHost.TabCount,
        FHost.ModelHoldsCurve(Doc.Ui.SelectedCurveId)));

    //  AND SAY SO IF THE DATA FILE HAS MOVED ON. Logged rather than shown: the
    //  project carries its own profile, so nothing is wrong - what has happened
    //  is worth being able to find out months later, and not worth a dialog
    //  every time someone reorganises a data directory.
    Notice := FNotice(Doc.Provenance);
    if Notice <> '' then
        WriteLog(Notice, Warning);

    //  THERE IS DELIBERATELY NO COMPARISON OF THE R-FACTOR HERE, and the reason
    //  is worth keeping: restoring a project does not recompute one. The engine
    //  reports "Not calculated" until something is fitted, so a check at this
    //  moment could never fire - it would be a diagnostic that looks like a
    //  safeguard and is not one. The figure the project recorded stays in the
    //  file, where the user can read it.
    //  AFTER the refresh, not before: filling the tables is what sets the
    //  modified flags, so the other order leaves a project dirty the instant it
    //  is opened and every close asks about work nobody did.
    FHost.MarkSaved;
    Rebase;
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
    Chosen, Fault: string;
begin
    Result := False;
    if not FHost.AskProjectToSaveAs(FPath, Chosen) then
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
    Result := FSave(FService, ContextToSave, FOpened, Chosen, Fault);
    if not Result then
    begin
        FHost.ReportProblem(Fault);
        Exit;
    end;
    //  ONLY AFTER IT WORKED. Retitling first names a document that is not
    //  there, and remembering it first offers it at the next start-up.
    FPath := Chosen;
    FHost.ShowDocument(Chosen);
    FHost.MarkSaved;
    Rebase;
end;

function TProjectWorkflow.SaveProject: boolean;
var
    Fault: string;
begin
    //  The first Save is a Save As, which is what every application does and
    //  what stops a Save from choosing a name on the user's behalf.
    if SaveTargetFor(FPath) = stAskForPath then
        Exit(SaveProjectAs);

    Result := FSave(FService, ContextToSave, FOpened, FPath, Fault);
    if not Result then
    begin
        FHost.ReportProblem(Fault);
        Exit;
    end;
    FHost.ShowDocument(FPath);
    FHost.MarkSaved;
    Rebase;
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
    Result := MayReplaceDocument;
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
            FHost.AskSaveBeforeClosing(ProjectDocumentName));
    if Action = caSaveFirst then
        //  A FAILED OR CANCELLED SAVE BLOCKS THE CLOSE. The user asked for the
        //  work to be kept; closing after failing to keep it destroys exactly
        //  what they asked to save.
        Action := ActionAfterSaving(SaveProject);
    Result := Action <> caStayAndShow;
end;

end.
