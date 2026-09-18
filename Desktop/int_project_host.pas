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
        { How many tabs this window has, and whether the model still holds
          AHandle - the two things the plan above cannot know. }
        function TabCount: longint;
        function ModelHoldsCurve(const AHandle: string): boolean;
        { Start again with nothing loaded. }
        procedure ClearEverything;

        //  ---- what only the window knows
        { The working context to save: the axis, the picking mode, the tab in
          front, where the data came from. The engine has never been told any of
          it. }
        function CurrentContext: TProjectClientContext;
        { Whether there is work here that the file on disk does not have. }
        function HasUnsavedWork: boolean;
        { Says the window and the file now agree. }
        procedure MarkSaved;
    end;

implementation

end.
