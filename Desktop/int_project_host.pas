// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the document commands need from the window, and nothing else.)

WHY THIS EXISTS, and it is not testability alone. The New/Open/Save/Save-As
sequences were written as methods of TFormMain, and the coverage gate reported
what that meant: the excluded UI-wrapper group grew by eighty-one lines, which is
eighty-one lines of decision that stopped being counted. `wrappers.txt` calls that
group a debt register rather than an amnesty, and its total may only shrink.

So the sequences move to a counted module and reach the window through this. Each
member is one thing the window can do and nothing else can: open a modal dialog,
show a message, ask the user a question, retitle itself, refresh what is drawn.
None of them decides anything - the deciding is what moved out.

CORBA INTERFACES, like everything else here: no reference counting, so an
implementor is an ordinary object whose lifetime its owner manages.
}
unit int_project_host;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, close_query, fit_project_session, project_ui_context;

type
    IProjectHost = interface
        ['{6B1F4C27-9A38-4D5E-B0C6-3E7A82D14F95}']

        //  ---- asking the user
        { The project to open, or False when the dialog was cancelled. }
        function AskProjectToOpen(out APath: string): boolean;
        { Where to save, starting from ASuggested. False when cancelled. }
        function AskProjectToSaveAs(const ASuggested: string;
            out APath: string): boolean;
        { Whether to save AWhat before closing. }
        function AskSaveBeforeClosing(const AWhat: string): TSaveAnswer;
        { Puts a yes/no question the user must answer before something
          irreversible happens, and answers True for yes.

          THE QUESTION IS PASSED IN, and every wording is project_commands' -
          which of them to ask, and whether to ask at all, are decisions, and
          the window is where nothing can be tested. There were two callers on
          the day this was written: replacing a file that already exists, and
          replacing the data a project is built on. }
        function Confirm(const AQuestion: string): boolean;
        { Something went wrong, in words the user can act on. }
        procedure ReportProblem(const AMessage: string);
        { A step that can take seconds has begun - reading, writing or
          comparing a large project - and AWhat says which: the window shows it
          under the busy cursor until the matching EndBusy. Pairs nest; only
          the outermost is shown. Ended BEFORE anything is reported or asked,
          so no dialog opens under a busy cursor. }
        procedure BeginBusy(const AWhat: string);
        procedure EndBusy;
        { What the busy step is doing now, in the user's words - a stage of
          it ("Drawing the model"). Shown beside the step's own name. }
        procedure BusyStage(const AWhat: string);
        { Lets the window answer the system now - repaint, be moved, show the
          seconds go by - while a busy step waits for its work, with the user's
          commands held back until EndBusy. Called often; the window decides
          how often it actually answers. }
        procedure KeepResponding;
        { Does now, still inside the busy step, what the window put off while
          the step ran - the redraws and the state refresh it queued, and the
          painting of every pane, the slow ones too - so that when the busy
          state ends the window is whole and the user waits for nothing more.
          Opens no dialog and starts no command: what would is put off again,
          to after EndBusy. }
        procedure FinishDrawing;

        //  ---- telling the window
        { The document changed: retitle, and remember it for next time. APath is
          empty when there is no document. }
        procedure ShowDocument(const APath: string);
        { Redraw everything from what the engine now holds. }
        procedure RefreshFromEngine;
        { Put back as much of a project's working context as APlan says to.

          A PLAN RATHER THAN THE CONTEXT ITSELF: which parts may be put back is
          a decision - an axis the user never chose, a tab this build does not
          have, a curve the model no longer holds - and it is made in
          project_ui_context, where a test can reach it. This only sets what it
          is told to set. }
        procedure ApplyWorkingContext(const APlan: TProjectUiPlan);
        { The part of APlan that comes with the data - each bar's date, the
          columns read beside the value - put back BEFORE the refresh, so what
          the refresh draws is drawn with them once, rather than drawn and then
          drawn again by ApplyWorkingContext. Draws nothing itself. }
        procedure ApplyDataContext(const APlan: TProjectUiPlan);
        { Which of this window's tabs it shows, and whether the model still
          holds AHandle - the two things the plan above cannot know. }
        function TabsShown: TTabsShown;
        function ModelHoldsCurve(const AHandle: string): boolean;
        { Start again with nothing loaded. }
        procedure ClearEverything;
        { Read the data file at APath into a fresh problem, as File > Import
          Profile does once its question has been answered. }
        procedure LoadProfile(const APath: string);

        //  ---- what only the window knows
        { The working context to save: the axis, the picking mode, the tab in
          front, where the data came from. The engine has never been told any of
          it. }
        function CurrentContext: TProjectClientContext;
        { Whether there is work here that the file on disk does not have. }
        function HasUnsavedWork: boolean;
        { Says the window and the file now agree. }
        procedure MarkSaved;

        //  ---- the run in progress
        { Whether a long operation - a fit, an automatic run, a computation -
          has been started and the window has not yet taken in its end. }
        function RunInProgress: boolean;
        { Stops it exactly as the Stop command does, and shows ANotice while
          it ends. The run then ends the way every stopped run ends, through
          the window's own end-of-run handling, so what it reached is the
          model. }
        procedure StopRun(const ANotice: string);
    end;

implementation

end.
