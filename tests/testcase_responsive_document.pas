// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Opening, saving and comparing a large project leave the window
answering, and say what they are doing.)

FOUND IN USE: opening the S&P 500 project, the window stood white and "Not
Responding" for a minute, and closing it showed only a busy cursor (the user,
2026-10-24): "it's not a problem that the app needs some time to open a project,
the problem is that it is not responsive and doesn't display what's going on".

So the work that touches no widget - reading the file and restoring it into the
engine, capturing the model and writing a save, reading the model back to
compare it with the file - runs on a thread of its own, and the window is let
answer the system while it waits (IProjectHost.KeepResponding). Each stage is
named as it begins (IProjectHost.BusyStage), so the window can say what it is
doing. What has to be on the window's thread - drawing the model, filling the
tables - stays there, and is named too.

A UNIT test: the window is a double and the transport is a table of replies.
It enters where the window does - OpenProjectAt, SaveProject, MayClose - with
the workflow made as the window makes it, its work in the background.
}
unit testcase_responsive_document;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, mock_http_transport,
    int_fit_service, mock_project_host, int_project_host, close_query,
    project_workflow, fit_project_session, fit_project_document,
    project_commands;

type
    TResponsiveDocumentTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FHostObj: TMockProjectHost;
        FHost: IProjectHost;
        FFlow: TProjectWorkflow;
        FComparedOffThread: boolean;
        procedure NoteTheComparingThread(Sender: TObject);
        { Holds the first read made off the window's thread - the baseline's -
          long enough for the window to be waiting on it; re-armed by every
          read made on the window's thread before it. }
        procedure SlowTheFirstBackgroundRead(Sender: TObject);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Opening.
        procedure AnOpenReadsTheFileOffTheWindowsThread;
        procedure AndTheWindowAnswersWhileItWaits;
        procedure AnOpenNamesEachStageInTurn;
        procedure AnOpenThatRaisesInTheBackgroundIsReportedNotRaised;
        procedure AnOpenFinishesTheWindowsDrawingBeforeItsBusyStateEnds;
        //  Saving.
        procedure ASaveWritesOffTheWindowsThread;
        procedure ASaveThatRaisesInTheBackgroundIsReportedNotRaised;
        //  The comparison's baseline.
        procedure WaitingForTheBaselineLetsTheWindowAnswer;
        //  Closing.
        procedure TheCloseComparisonReadsOffTheWindowsThread;
    end;

implementation

var
    { Which thread the fake open and save ran on, and how long they take. }
    OpenThread, SaveThread: TThreadID;
    OpenRaises, SaveRaises: boolean;

function SlowOpen(AService: IFitService; const APath: string;
    out ADoc: TProjectDocument; out AFault: string): boolean;
begin
    OpenThread := GetCurrentThreadId;
    if OpenRaises then
        raise Exception.Create('the disk went away');
    //  Long enough that the window is let answer at least once.
    Sleep(60);
    ADoc := EmptyProjectDocument;
    AFault := '';
    Result := True;
end;

function SlowSave(AService: IFitService;
    const AContext: TProjectClientContext; const APrevious: TProjectDocument;
    const APath: string; out AWritten: TProjectDocument;
    out AFault: string): boolean;
begin
    SaveThread := GetCurrentThreadId;
    if SaveRaises then
        raise Exception.Create('the disk is full');
    Sleep(60);
    AFault := '';
    AWritten := CaptureProject(AService, AContext, APrevious);
    Result := True;
end;

procedure TResponsiveDocumentTest.SetUp;
begin
    OpenThread := 0;
    SaveThread := 0;
    OpenRaises := False;
    SaveRaises := False;
    FSvc := TMockHttpService.Create('http://localhost:8080');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.5}');
    FSvc.Reply('stats', '{"ok":true,"rFactor":"0.1"}');
    FHostObj := TMockProjectHost.Create;
    FHost := FHostObj;
    //  AS THE WINDOW MAKES IT: its work in the background.
    FFlow := TProjectWorkflow.Create(FSvc, FHost, @SlowSave, @SlowOpen, nil,
        nil, True);
    FSvc.GetMaxRFactor;
end;

procedure TResponsiveDocumentTest.TearDown;
begin
    FreeAndNil(FFlow);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FSvc);
end;

procedure TResponsiveDocumentTest.NoteTheComparingThread(Sender: TObject);
begin
    FComparedOffThread := GetCurrentThreadId <> MainThreadID;
end;

procedure TResponsiveDocumentTest.SlowTheFirstBackgroundRead(Sender: TObject);
begin
    if GetCurrentThreadId = MainThreadID then
        FSvc.OnNextFetch := @SlowTheFirstBackgroundRead
    else
        Sleep(150);
end;

procedure TResponsiveDocumentTest.ASaveThatRaisesInTheBackgroundIsReportedNotRaised;
begin
    SaveRaises := True;
    FHostObj.ScriptSaveAs(IncludeTrailingPathDelimiter(GetTempDir) + 'a.fitproj');
    AssertFalse('not saved', FFlow.SaveProject);
    AssertTrue('said, with where and why: ' + FHostObj.Problem,
        (Pos('could not be saved', FHostObj.Problem) > 0) and
        (Pos('the disk is full', FHostObj.Problem) > 0));
    AssertEquals('and the busy state over', 0, FHostObj.BusyDepth);
end;

procedure TResponsiveDocumentTest.WaitingForTheBaselineLetsTheWindowAnswer;
var
    Before: longint;
begin
    FSvc.OnNextFetch := @SlowTheFirstBackgroundRead;
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    Before := FHostObj.Responses;
    FFlow.WaitForBaseline;
    AssertTrue('let answer while the baseline was read: ' +
        IntToStr(FHostObj.Responses - Before), FHostObj.Responses > Before);
end;

procedure TResponsiveDocumentTest.AnOpenReadsTheFileOffTheWindowsThread;
begin
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    AssertTrue('the file was read', OpenThread <> 0);
    AssertTrue('on a thread of its own', OpenThread <> MainThreadID);
end;

procedure TResponsiveDocumentTest.AndTheWindowAnswersWhileItWaits;
begin
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    AssertTrue('let answer while the file was read: ' +
        IntToStr(FHostObj.Responses), FHostObj.Responses > 0);
    AssertEquals('and the busy state over', 0, FHostObj.BusyDepth);
end;

procedure TResponsiveDocumentTest.AnOpenNamesEachStageInTurn;
var
    S: string;
begin
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    S := FHostObj.Stages.Text;
    AssertTrue('reading: ' + S,
        FHostObj.Stages.IndexOf(ReadingProjectStage) >= 0);
    AssertTrue('drawing: ' + S,
        FHostObj.Stages.IndexOf(DrawingModelStage) >
        FHostObj.Stages.IndexOf(ReadingProjectStage));
    AssertTrue('putting the window back: ' + S,
        FHostObj.Stages.IndexOf(RestoringWindowStage) >
        FHostObj.Stages.IndexOf(DrawingModelStage));
    AssertTrue('checking what came back: ' + S,
        FHostObj.Stages.IndexOf(CheckingOpenedStage) >
        FHostObj.Stages.IndexOf(RestoringWindowStage));
end;

procedure TResponsiveDocumentTest.AnOpenThatRaisesInTheBackgroundIsReportedNotRaised;
begin
    OpenRaises := True;
    AssertFalse('not opened', FFlow.OpenProjectAt('a.fitproj'));
    AssertTrue('said, in its own words: ' + FHostObj.Problem,
        Pos('the disk went away', FHostObj.Problem) > 0);
    AssertEquals('and the busy state over', 0, FHostObj.BusyDepth);
end;

procedure TResponsiveDocumentTest.AnOpenFinishesTheWindowsDrawingBeforeItsBusyStateEnds;
var
    Seq: string;
begin
    //  FOUND IN USE: the progress window closed and the window then stood
    //  frozen for seconds more, drawing what it had put off - the user asked
    //  that when the progress is gone, everything is ready.
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    Seq := FHostObj.Log.Sequence;
    AssertTrue('the drawing finished: ' + Seq, Pos('FinishDrawing', Seq) > 0);
    AssertTrue('inside the busy state: ' + Seq,
        Pos('FinishDrawing', Seq) < Pos('EndBusy', Seq));
    AssertEquals('named, as the last stage', FinishingWindowStage,
        FHostObj.Stages[FHostObj.Stages.Count - 1]);
end;

procedure TResponsiveDocumentTest.ASaveWritesOffTheWindowsThread;
begin
    FHostObj.ScriptSaveAs(IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-no-such-dir-' + IntToStr(GetTickCount64) + PathDelim +
        'a.fitproj');
    AssertTrue('saved', FFlow.SaveProject);
    AssertTrue('written on a thread of its own',
        (SaveThread <> 0) and (SaveThread <> MainThreadID));
    AssertTrue('named: ' + FHostObj.Stages.Text,
        FHostObj.Stages.IndexOf(WritingProjectStage) >= 0);
end;

procedure TResponsiveDocumentTest.TheCloseComparisonReadsOffTheWindowsThread;
begin
    AssertTrue('opened', FFlow.OpenProjectAt('a.fitproj'));
    FFlow.WaitForBaseline;
    //  Something sent, so the close has to read the model back.
    FSvc.SetMaxRFactor(0.25);
    FComparedOffThread := False;
    FSvc.OnNextFetch := @NoteTheComparingThread;
    FHostObj.ScriptCloseAnswer(saCancel);
    FFlow.MayClose;
    AssertTrue('read back on a thread of its own', FComparedOffThread);
end;

initialization
    //  A UNIT test: the transport is a double, no server runs.
    RegisterTest('unit', TResponsiveDocumentTest);
end.
