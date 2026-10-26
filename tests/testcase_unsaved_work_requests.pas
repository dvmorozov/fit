// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Closing a project nobody changed reads nothing back from the server.)

FOUND IN USE: closing the S&P 500 project, untouched since it was opened, showed
the busy cursor for 36 seconds (the client log, 2026-10-24: "closing: may the
window close 36250 ms"). To learn that nothing had changed, the window read the
whole problem back - the profile, every curve, the statistics, 5.5 MB of a
module's state - and wrote the whole project out again as the file would hold
it, history included, to compare hashes with the ones taken when it was opened.

What is asked first now is whether anything COULD have changed: a write sent to
the server (IFitService.ChangeCount), a change to the history
(TModelHistory.Revision), or a change to what only the window holds. Only when
one of them moved is the problem read back - and then exactly as before, so a
run that changed nothing still asks nothing.

A UNIT test: the window is a double and the transport is a table of replies.
It enters where the window does, at MayClose, over the HTTP service the window
uses - the one that counts what it sends.
}
unit testcase_unsaved_work_requests;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, mock_http_transport,
    int_fit_service,
    mock_project_host, int_project_host, close_query, project_workflow,
    fit_project_session, fit_project_document, fit_points_json, model_history,
    project_commands, fit_project_file;

type
    TUnsavedWorkRequestsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FHostObj: TMockProjectHost;
        FHost: IProjectHost;
        FFlow: TProjectWorkflow;
        { How many reads the service has made since its log was cleared. }
        function Reads: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheProblemCanBeReadBackAtAll;
        procedure AnUntouchedProjectClosesWithoutReadingTheModelBack;
        procedure AWriteSinceTheBaselineIsLookedFor;
        procedure AndAWriteThatChangedNothingStillAsksNothing;
        procedure AChangeOnlyTheWindowHoldsIsStillSeen;
        procedure AChangeToTheHistoryIsStillSeen;
        procedure TheFirstCloseAfterABaselineTakenWhileAWriteWentOutLooks;
        //  A save reads the problem once: the baseline after it is what it
        //  wrote, not a second reading.
        procedure ASaveReadsTheProblemBackOnce;

        //  The baseline an open takes is read in the background: on the
        //  S&P 500 project four seconds added to every open.
        procedure AnOpensBaselineIsReadOffTheWindowsThread;
        procedure AnUntouchedCloseDoesNotWaitForIt;
        procedure AWriteWhileItIsReadMakesTheCloseAsk;
        procedure ASaveWhileItIsReadTakesItsOwn;
        procedure AWorkflowFreedWhileItIsReadWaitsForIt;

        //  Whether a module took its part of the project is known from the
        //  restore that handed it over; reading the module's whole state back
        //  to ask again was 2.8 s of every open of the S&P 500 project.
        procedure AnOpenDoesNotReadAModulesStateBack;
    private
        FReadOffThread: boolean;
        FWriteWhileReading: boolean;
        FReaderFinishedItsRead: boolean;
        { Passes over the reads made on this thread (the open's own) and notes
          the first made on another. }
        procedure NoteTheReadingThread(Sender: TObject);
        { Passes over the reads made on this thread; on another, holds the
          read a while and then notes that it finished. }
        procedure HoldTheBackgroundRead(Sender: TObject);
        procedure WriteWhileReading(Sender: TObject);
        function OpenedInTheBackground: TProjectWorkflow;
    end;

implementation

{ A save that writes nothing: the capture a real save makes, and the document
  it would have written. }
function CapturingSave(AService: IFitService;
    const AContext: TProjectClientContext; const APrevious: TProjectDocument;
    const APath: string; out AWritten: TProjectDocument;
    out AFault: string): boolean;
begin
    AFault := '';
    AWritten := CaptureProject(AService, AContext, APrevious);
    Result := True;
end;

{ An open that restores a project carrying one module's part, as
  OpenProjectFile does once the file is read. }
function OpenWithAModulesPart(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;
begin
    ADoc := EmptyProjectDocument;
    SetLength(ADoc.ModuleDocuments, 1);
    ADoc.ModuleDocuments[0].Module := 'acme';
    ADoc.ModuleDocuments[0].Content := '{}';
    Result := OpenProjectDocument(AService, ADoc, AFault);
end;

{ An open that reads a file of nothing and restores nothing. }
function OpenNothing(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;
begin
    ADoc := EmptyProjectDocument;
    AFault := '';
    Result := True;
end;

procedure TUnsavedWorkRequestsTest.NoteTheReadingThread(Sender: TObject);
begin
    //  ARMED AGAIN for a read on this thread: the open makes reads of its own
    //  here, and the first read the baseline makes is the one asked about.
    if GetCurrentThreadId = MainThreadID then
        FSvc.OnNextFetch := @NoteTheReadingThread
    else
        FReadOffThread := True;
end;

procedure TUnsavedWorkRequestsTest.HoldTheBackgroundRead(Sender: TObject);
begin
    if GetCurrentThreadId = MainThreadID then
        FSvc.OnNextFetch := @HoldTheBackgroundRead
    else
    begin
        //  Long enough that a Free not waiting for it returns well before.
        Sleep(300);
        FReaderFinishedItsRead := True;
    end;
end;

procedure TUnsavedWorkRequestsTest.WriteWhileReading(Sender: TObject);
begin
    FWriteWhileReading := True;
    FSvc.SetMaxRFactor(0.25);
end;

function TUnsavedWorkRequestsTest.OpenedInTheBackground: TProjectWorkflow;
begin
    Result := TProjectWorkflow.Create(FSvc, FHost, @CapturingSave, @OpenNothing,
        nil, nil, True);
    AssertTrue('opened', Result.OpenProjectAt('a.fitproj'));
end;

procedure TUnsavedWorkRequestsTest.SetUp;
begin
    FSvc := TMockHttpService.Create('http://localhost:8080');
    //  A model of no curves - a reply the capture can read, as the server's
    //  is. Without it the baseline cannot be taken at all, and every close
    //  answers "nothing unsaved" whatever was done: the first draft of these
    //  tests passed for exactly that reason.
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.5}');
    FSvc.Reply('stats', '{"ok":true,"rFactor":"0.1"}');
    FHostObj := TMockProjectHost.Create;
    FHost := FHostObj;
    FFlow := TProjectWorkflow.Create(FSvc, FHost);
    //  THE PROBLEM MADE FIRST, as it is in any session with something to
    //  close: making it is a write, and one sent while the baseline is read
    //  makes the next close look (TheFirstCloseAfterABaselineTakenWhile...).
    FSvc.GetMaxRFactor;
    //  What every session starts from: an empty document, matched to a file
    //  of nothing - the baseline every later comparison is made against.
    FFlow.StartEmpty;
    FSvc.Log.Clear;
end;

procedure TUnsavedWorkRequestsTest.TearDown;
begin
    FreeAndNil(FFlow);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FSvc);
end;

function TUnsavedWorkRequestsTest.Reads: longint;
var
    Lines: TStringList;
    i: longint;
begin
    Result := 0;
    Lines := TStringList.Create;
    try
        Lines.Text := FSvc.Log.AsText;
        for i := 0 to Lines.Count - 1 do
            if Lines[i].StartsWith('GET(') then
                Inc(Result);
    finally
        Lines.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.TheProblemCanBeReadBackAtAll;
begin
    //  WHAT EVERY OTHER TEST HERE STANDS ON: a baseline that could not be
    //  taken reads as "unknown", and an unknown baseline asks nothing.
    AssertTrue('the fixture''s server can be read back',
        ProjectFingerprint(FSvc, EmptyProjectClientContext) <> '');
end;

procedure TUnsavedWorkRequestsTest.AnUntouchedProjectClosesWithoutReadingTheModelBack;
begin
    AssertTrue('it closes', FFlow.MayClose);
    AssertFalse('without a question', FHostObj.Log.Saw('AskSaveBeforeClosing'));
    AssertEquals('and without reading the problem back: ' + FSvc.Log.AsText,
        0, Reads);
    AssertFalse('so there is nothing to be busy with',
        FHostObj.Log.Saw('BeginBusy'));
end;

procedure TUnsavedWorkRequestsTest.AWriteSinceTheBaselineIsLookedFor;
begin
    FSvc.SetMaxRFactor(0.25);
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.25}');
    FSvc.Log.Clear;
    FHostObj.ScriptCloseAnswer(saCancel);
    AssertFalse('the change is found, and Cancel keeps the window',
        FFlow.MayClose);
    AssertTrue('by reading the problem back', Reads > 0);
    AssertTrue('saying so while it reads: ' + FHostObj.BusyWith,
        Pos('Looking for changes', FHostObj.BusyWith) = 1);
    AssertTrue('and asking', FHostObj.Log.Saw('AskSaveBeforeClosing'));
    AssertFalse('once it has finished reading', FHostObj.DialogWhileBusy);
    AssertEquals('and the busy state over', 0, FHostObj.BusyDepth);
end;

procedure TUnsavedWorkRequestsTest.AndAWriteThatChangedNothingStillAsksNothing;
begin
    //  The same value written again: sent, so the problem is read back - and
    //  found as it was, so nothing is asked, exactly as before.
    FSvc.SetMaxRFactor(0.5);
    FSvc.Log.Clear;
    AssertTrue('it closes', FFlow.MayClose);
    AssertTrue('having looked', Reads > 0);
    AssertFalse('without a question', FHostObj.Log.Saw('AskSaveBeforeClosing'));
end;

procedure TUnsavedWorkRequestsTest.AChangeOnlyTheWindowHoldsIsStillSeen;
var
    Context: TProjectClientContext;
begin
    //  An axis is the project's, and choosing one sends nothing to the server.
    Context := EmptyProjectClientContext;
    Context.HasUi := True;
    Context.Ui.Argument.Mode := 'two-theta';
    Context.Ui.Argument.ChosenByUser := True;
    FHostObj.ScriptContext(Context);
    FHostObj.ScriptCloseAnswer(saCancel);
    AssertFalse('the change is found', FFlow.MayClose);
    AssertTrue('and asked about', FHostObj.Log.Saw('AskSaveBeforeClosing'));
end;

procedure TUnsavedWorkRequestsTest.AChangeToTheHistoryIsStillSeen;
var
    Entry: THistoryEntry;
    Profile: TPointsData;
begin
    Entry := Default(THistoryEntry);
    Entry.Id := 'a';
    Entry.ContentHash := 'm1';
    Entry.Model := EmptyProjectDocument;
    Profile := Default(TPointsData);
    AssertTrue('recorded', FFlow.History.Add(Entry, Profile));
    FHostObj.ScriptCloseAnswer(saCancel);
    AssertFalse('the change is found', FFlow.MayClose);
    AssertTrue('and asked about', FHostObj.Log.Saw('AskSaveBeforeClosing'));
end;

procedure TUnsavedWorkRequestsTest.TheFirstCloseAfterABaselineTakenWhileAWriteWentOutLooks;
var
    Svc: TMockHttpService;
    Flow: TProjectWorkflow;
begin
    //  A service whose problem is made by the baseline's own first read: a
    //  write sent while it read, so which side of it the write fell is not
    //  known. The first close looks; finding nothing, the next does not.
    Svc := TMockHttpService.Create('http://localhost:8080');
    Flow := TProjectWorkflow.Create(Svc, FHost);
    try
        Svc.Reply('curves', '{"ok":true,"curves":[]}');
        Flow.StartEmpty;
        Svc.Log.Clear;
        AssertTrue('it closes', Flow.MayClose);
        AssertTrue('having looked', Svc.Log.AsText <> '');
        Svc.Log.Clear;
        AssertTrue('and again', Flow.MayClose);
        AssertEquals('without looking now: ' + Svc.Log.AsText, '',
            Svc.Log.AsText);
    finally
        Flow.Free;
        Svc.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.ASaveReadsTheProblemBackOnce;
var
    Flow: TProjectWorkflow;
    Lines: TStringList;
    i, Stats: longint;
begin
    Flow := TProjectWorkflow.Create(FSvc, FHost, @CapturingSave);
    try
        Flow.StartEmpty;
        FHostObj.ScriptSaveAs(IncludeTrailingPathDelimiter(GetTempDir) +
            'fit-no-such-dir-' + IntToStr(GetTickCount64) +
            PathDelim + 'a.fitproj');
        FSvc.Log.Clear;
        AssertTrue('saved', Flow.SaveProject);
        Stats := 0;
        Lines := TStringList.Create;
        try
            Lines.Text := FSvc.Log.AsText;
            for i := 0 to Lines.Count - 1 do
                if Lines[i].StartsWith('GET(') and Lines[i].EndsWith('/stats)') then
                    Inc(Stats);
        finally
            Lines.Free;
        end;
        AssertEquals('the statistics read once, by the save: ' +
            FSvc.Log.AsText, 1, Stats);
        FSvc.Log.Clear;
        AssertTrue('and it closes', Flow.MayClose);
        AssertEquals('without reading anything: ' + FSvc.Log.AsText, '',
            FSvc.Log.AsText);
    finally
        Flow.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.AnOpensBaselineIsReadOffTheWindowsThread;
var
    Flow: TProjectWorkflow;
begin
    //  THE OPEN'S OWN BASELINE, not one started by hand: the open is what
    //  must not read it on the window's thread. Its file is read and
    //  restored by OpenNothing, which reads nothing, so the only read made
    //  off this thread is the baseline's.
    FReadOffThread := False;
    FSvc.OnNextFetch := @NoteTheReadingThread;
    Flow := TProjectWorkflow.Create(FSvc, FHost, @CapturingSave, @OpenNothing,
        nil, nil, True);
    try
        AssertTrue('opened', Flow.OpenProjectAt('a.fitproj'));
        Flow.WaitForBaseline;
        FSvc.OnNextFetch := nil;
        AssertTrue('the open''s baseline read on a thread of its own',
            FReadOffThread);
        FSvc.Log.Clear;
        AssertTrue('and once it is taken an untouched close', Flow.MayClose);
        AssertEquals('reads nothing: ' + FSvc.Log.AsText, '', FSvc.Log.AsText);
    finally
        Flow.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.AnUntouchedCloseDoesNotWaitForIt;
var
    Flow: TProjectWorkflow;
begin
    Flow := OpenedInTheBackground;
    try
        AssertTrue('it closes', Flow.MayClose);
        AssertFalse('without a question',
            FHostObj.Log.Saw('AskSaveBeforeClosing'));
        AssertFalse('and without the busy state a wait would show',
            FHostObj.Log.Saw('BeginBusy(' +
            CheckingForChangesNotice(Flow.DocumentName)));
    finally
        Flow.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.AWriteWhileItIsReadMakesTheCloseAsk;
var
    Flow: TProjectWorkflow;
begin
    Flow := TProjectWorkflow.Create(FSvc, FHost, @CapturingSave, @OpenNothing,
        nil, nil, True);
    try
        AssertTrue('opened', Flow.OpenProjectAt('a.fitproj'));
        Flow.WaitForBaseline;
        //  A WRITE WHILE IT READS - the same value, so a comparison against a
        //  baseline read after it would find nothing. Which side of the write
        //  the baseline fell is not known, so the close asks rather than risk
        //  a change nobody is asked about.
        FWriteWhileReading := False;
        FSvc.OnNextFetch := @WriteWhileReading;
        Flow.StartBaselineInBackground;
        Flow.WaitForBaseline;
        AssertTrue('the write went out while it read', FWriteWhileReading);
        FHostObj.ScriptCloseAnswer(saCancel);
        AssertFalse('the close asks', Flow.MayClose);
        AssertTrue('asked', FHostObj.Log.Saw('AskSaveBeforeClosing'));
    finally
        Flow.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.ASaveWhileItIsReadTakesItsOwn;
var
    Flow: TProjectWorkflow;
begin
    Flow := OpenedInTheBackground;
    try
        AssertTrue('saved', Flow.SaveProject);
        FSvc.Log.Clear;
        AssertTrue('it closes', Flow.MayClose);
        AssertEquals('from the save''s baseline, reading nothing: ' +
            FSvc.Log.AsText, '', FSvc.Log.AsText);
    finally
        Flow.Free;
    end;
end;

procedure TUnsavedWorkRequestsTest.AWorkflowFreedWhileItIsReadWaitsForIt;
var
    Flow: TProjectWorkflow;
    Waited: longint;
    Calls: longint;
begin
    //  Freed at once, while the open's baseline is held in its first read:
    //  the reading thread must not outlive what it reads for.
    FReaderFinishedItsRead := False;
    FSvc.OnNextFetch := @HoldTheBackgroundRead;
    Flow := OpenedInTheBackground;
    try
        Flow.Free;
        FSvc.OnNextFetch := nil;
        AssertTrue('the free waited for the read it was making',
            FReaderFinishedItsRead);
        Calls := FSvc.Log.Calls.Count;
        Sleep(100);
        AssertEquals('and nothing reads after it: ' + FSvc.Log.AsText, Calls,
            FSvc.Log.Calls.Count);
    finally
        //  A FREE THAT DID NOT WAIT left the thread reading the service the
        //  fixture is about to free: given time to finish here, so a failure
        //  is this assertion and not a fault on another thread later.
        Waited := 0;
        while not FReaderFinishedItsRead and (Waited < 5000) do
        begin
            Sleep(10);
            Inc(Waited, 10);
        end;
        if Waited > 0 then
            Sleep(500);
    end;
end;

procedure TUnsavedWorkRequestsTest.AnOpenDoesNotReadAModulesStateBack;
var
    Flow: TProjectWorkflow;
    Lines: TStringList;
    i, StateReads: longint;
begin
    Flow := TProjectWorkflow.Create(FSvc, FHost, @CapturingSave,
        @OpenWithAModulesPart);
    try
        FSvc.Log.Clear;
        AssertTrue('opened', Flow.OpenProjectAt('a.fitproj'));
        StateReads := 0;
        Lines := TStringList.Create;
        try
            Lines.Text := FSvc.Log.AsText;
            for i := 0 to Lines.Count - 1 do
                if Lines[i].StartsWith('GET(') and
                    (Pos('/modules/acme/', Lines[i]) > 0) then
                    Inc(StateReads);
        finally
            Lines.Free;
        end;
        AssertEquals('the module''s state read back: ' + FSvc.Log.AsText, 0,
            StateReads);
        AssertFalse('and nothing reported as not taken: ' + FHostObj.Problem,
            FHostObj.Log.Saw('ReportProblem'));
    finally
        Flow.Free;
    end;
end;

initialization
    //  A UNIT test: the transport is a double, no server runs.
    RegisterTest('unit', TUnsavedWorkRequestsTest);
end.
