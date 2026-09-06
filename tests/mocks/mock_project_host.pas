// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A window for the document commands to talk to, that has no window in
it.)

Follows the rule mock_support states: a plain TObject - never TInterfacedObject,
whose refcounting is inert under -SIcorba and reads as a lifetime guarantee that
does not exist - owned by the fixture, which nils the interface first and frees
the object after.

IT RECORDS AND IT ANSWERS WHAT IT WAS SCRIPTED TO; it never asserts. A mock that
failed inside its own callback would report from whatever called it rather than
from the expectation, and would name the mock rather than the test.
}
unit mock_project_host;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, close_query, mock_support, int_project_host,
    project_ui_context, fit_project_session, fit_project_document;

type
    TMockProjectHost = class(TMockBase, IProjectHost)
    private
        FOpenPath: string;
        FOpenAccepted: boolean;
        FSavePath: string;
        FSaveAccepted: boolean;
        FSaveAnswer: TSaveAnswer;
        FUnsaved: boolean;
        FShown: string;
        FProblem: string;
        FSuggested: string;
        FApplied: TProjectUiPlan;
        FHoldsCurve: boolean;
        FTabCount: longint;
        FConfirmAnswer: boolean;
        FAsked: string;
    public
        constructor Create; override;

        //  ---- scripting
        { What the Open dialog answers. Accepted=False is a cancel. }
        procedure ScriptOpen(const APath: string; AAccepted: boolean = True);
        { What the Save As dialog answers. }
        procedure ScriptSaveAs(const APath: string; AAccepted: boolean = True);
        { What the user says to a yes/no question. False by default: a mock
          that silently said yes would let a test pass over the answer that
          destroys somebody's work. }
        procedure ScriptConfirmAnswer(AAllowed: boolean);
        { The question that was put, or '' if none was. }
        property Asked: string read FAsked;
        { What the user says when asked about unsaved work. }
        procedure ScriptCloseAnswer(AAnswer: TSaveAnswer);
        procedure ScriptUnsavedWork(AHasSome: boolean);

        //  ---- what was recorded
        { The document the window was last told to show; '' means none. }
        property Shown: string read FShown;
        { The last message the user was shown, or ''. }
        property Problem: string read FProblem;
        { The path the Save As dialog was started from. }
        property Suggested: string read FSuggested;
        { The working context the window was last told to put back. }
        property Applied: TProjectUiPlan read FApplied;
        { Whether the model is to claim it still holds the selected curve. }
        procedure ScriptModelHoldsCurve(AHolds: boolean);
        procedure ScriptTabCount(ACount: longint);

        //  IProjectHost
        function AskProjectToOpen(out APath: string): boolean;
        function AskProjectToSaveAs(const ASuggested: string;
            out APath: string): boolean;
        function AskSaveBeforeClosing(const AWhat: string): TSaveAnswer;
        function Confirm(const AQuestion: string): boolean;
        procedure ReportProblem(const AMessage: string);
        procedure ShowDocument(const APath: string);
        procedure RefreshFromEngine;
        procedure ApplyWorkingContext(const APlan: TProjectUiPlan);
        function TabCount: longint;
        function ModelHoldsCurve(const AHandle: string): boolean;
        procedure ClearEverything;
        function CurrentContext: TProjectClientContext;
        function HasUnsavedWork: boolean;
        procedure MarkSaved;
    end;

implementation

constructor TMockProjectHost.Create;
begin
    inherited Create;
    FOpenAccepted := False;
    FSaveAccepted := False;
    FSaveAnswer := saCancel;
    FConfirmAnswer := False;
    FAsked := '';
    FUnsaved := False;
    FShown := '';
    FProblem := '';
    FSuggested := '';
    FApplied := Default(TProjectUiPlan);
    FHoldsCurve := True;
    //  Enough that an ordinary project's tab is inside it; a test that cares
    //  says otherwise.
    FTabCount := 8;
end;

procedure TMockProjectHost.ScriptOpen(const APath: string; AAccepted: boolean);
begin
    FOpenPath := APath;
    FOpenAccepted := AAccepted;
end;

procedure TMockProjectHost.ScriptSaveAs(const APath: string;
    AAccepted: boolean);
begin
    FSavePath := APath;
    FSaveAccepted := AAccepted;
end;

procedure TMockProjectHost.ScriptCloseAnswer(AAnswer: TSaveAnswer);
begin
    FSaveAnswer := AAnswer;
end;

procedure TMockProjectHost.ScriptUnsavedWork(AHasSome: boolean);
begin
    FUnsaved := AHasSome;
end;

function TMockProjectHost.AskProjectToOpen(out APath: string): boolean;
begin
    FLog.Note('AskProjectToOpen');
    APath := FOpenPath;
    Result := FOpenAccepted;
end;

procedure TMockProjectHost.ScriptConfirmAnswer(AAllowed: boolean);
begin
    FConfirmAnswer := AAllowed;
end;

function TMockProjectHost.Confirm(const AQuestion: string): boolean;
begin
    //  LOGGED IN ORDER, because when it is asked matters as much as whether:
    //  a question put after the thing has already happened is not a question.
    FLog.Note('Confirm', AQuestion);
    FAsked := AQuestion;
    Result := FConfirmAnswer;
end;

function TMockProjectHost.AskProjectToSaveAs(const ASuggested: string;
    out APath: string): boolean;
begin
    FLog.Note('AskProjectToSaveAs', ASuggested);
    FSuggested := ASuggested;
    APath := FSavePath;
    Result := FSaveAccepted;
end;

function TMockProjectHost.AskSaveBeforeClosing(const AWhat: string): TSaveAnswer;
begin
    FLog.Note('AskSaveBeforeClosing', AWhat);
    Result := FSaveAnswer;
end;

procedure TMockProjectHost.ReportProblem(const AMessage: string);
begin
    FLog.Note('ReportProblem', AMessage);
    FProblem := AMessage;
end;

procedure TMockProjectHost.ShowDocument(const APath: string);
begin
    FLog.Note('ShowDocument', APath);
    FShown := APath;
end;

procedure TMockProjectHost.RefreshFromEngine;
begin
    FLog.Note('RefreshFromEngine');
end;

procedure TMockProjectHost.ApplyWorkingContext(const APlan: TProjectUiPlan);
begin
    FLog.Note('ApplyWorkingContext');
    FApplied := APlan;
end;

function TMockProjectHost.TabCount: longint;
begin
    Result := FTabCount;
end;

function TMockProjectHost.ModelHoldsCurve(const AHandle: string): boolean;
begin
    Result := FHoldsCurve;
end;

procedure TMockProjectHost.ScriptModelHoldsCurve(AHolds: boolean);
begin
    FHoldsCurve := AHolds;
end;

procedure TMockProjectHost.ScriptTabCount(ACount: longint);
begin
    FTabCount := ACount;
end;

procedure TMockProjectHost.ClearEverything;
begin
    FLog.Note('ClearEverything');
end;

function TMockProjectHost.CurrentContext: TProjectClientContext;
begin
    FLog.Note('CurrentContext');
    Result := EmptyProjectClientContext;
end;

function TMockProjectHost.HasUnsavedWork: boolean;
begin
    FLog.Note('HasUnsavedWork');
    Result := FUnsaved;
end;

procedure TMockProjectHost.MarkSaved;
begin
    FLog.Note('MarkSaved');
    FUnsaved := False;
end;

end.
