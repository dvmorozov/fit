// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What closing the window does about work the user has not saved.)

THE LAST THING THE PROGRAM DOES FOR THE USER, and it was two copies of the same
thirty lines in an LCL close handler - one for the parameter table, one for the
summary table - where no test could reach any of it. Two copies is how the
second one comes to differ from the first: the parameter block ends its Cancel
branch with an Exit and the datasheet block does not, which happens not to matter
only because the datasheet is the last thing asked about.

WHAT THE RULES ARE, in one place now:

  * An unmodified table is not asked about. Being asked to save something you did
    not change is how people learn to dismiss the question without reading it.

  * Each modified table is asked about SEPARATELY, and saying no to one says
    nothing about the other.

  * "No" closes without saving, and does NOT clear the modified flag - nothing
    was saved, so nothing about the document changed.

  * "Cancel" means cancel the CLOSE, not cancel the save: the window stays open
    and the table in question is brought to the front, so the answer to "which
    one?" is on screen rather than in the message the user just dismissed.

  * A SAVE THAT FAILS BLOCKS THE CLOSE, on the same terms as Cancel. This is the
    case worth having a test for: the user said save, the save did not happen, and
    closing anyway would destroy exactly the work they asked to keep. It is also
    the only path where the program overrides what the user asked for, so it has
    to be deliberate rather than a fallthrough.

WHY A LIST AND NOT TWO BLOCKS. The window has two editable tables today. Adding a
third meant copying the block again - and a copy is what this unit exists to stop
being necessary: the caller walks a list of pending documents, and a new one is an
entry in it.
}
unit close_query;

{$mode objfpc}{$H+}

interface

type
    { What the user answered when asked whether to save. Named rather than the
      widget set's mrYes/mrNo/mrCancel, so this unit stays testable without one -
      the caller translates. }
    TSaveAnswer = (saYes, saNo, saCancel);

    { What the window should do next about one pending document. }
    TCloseAction = (
        { Nothing to save, or the user chose not to. Go on to the next document,
          and close if it was the last. }
        caCarryOn,
        { The user asked for it to be saved. Save, then ask this unit again what
          the result means - see ActionAfterSaving. }
        caSaveFirst,
        { Do not close. The document is brought to the front so that the user can
          see which one is being talked about. }
        caStayAndShow
    );

{ What to do about a document, given whether it has unsaved changes and - when it
  has - what the user answered.

  AAnswer is not consulted for an unmodified document, because an unmodified
  document is never asked about. }
function ActionForDocument(AModified: boolean;
    AAnswer: TSaveAnswer): TCloseAction;

{ What the outcome of the save the user asked for means for the close.

  A FAILED SAVE STOPS THE CLOSE. The user asked for the work to be kept; closing
  after failing to keep it would discard precisely what they asked to save. }
function ActionAfterSaving(ASaved: boolean): TCloseAction;

{ Whether the document should still be considered modified after a save attempt.

  Only a successful save clears it. Nothing else does - and in particular "No"
  does not, which is why the flag is not touched on that path: the user declined
  to save, they did not declare the document clean. }
function StillModifiedAfterSaving(ASaved: boolean): boolean;

{ The question to put to the user about one document.

  AWhat names the document in the user's terms - the caller owns that vocabulary,
  because it is the same wording that appears on the tab they will be shown. }
function SaveQuestion(const AWhat: string): string;

implementation

function ActionForDocument(AModified: boolean;
    AAnswer: TSaveAnswer): TCloseAction;
begin
    //  Never asked about, so nothing to decide.
    if not AModified then
    begin
        Result := caCarryOn;
        Exit;
    end;

    case AAnswer of
        saYes:    Result := caSaveFirst;
        //  Declined, and that is a complete answer: the close goes ahead and the
        //  document is left exactly as it was, modified flag included.
        saNo:     Result := caCarryOn;
        saCancel: Result := caStayAndShow;
    else
        //  No fourth answer exists. Treated as Cancel rather than as consent,
        //  because the safe reading of an answer this cannot understand is not
        //  to throw the work away.
        Result := caStayAndShow;
    end;
end;

function ActionAfterSaving(ASaved: boolean): TCloseAction;
begin
    if ASaved then
        Result := caCarryOn
    else
        //  THE CASE THAT MATTERS. Not a fallthrough: the user asked to keep the
        //  work, the program could not, and closing now would lose it.
        Result := caStayAndShow;
end;

function StillModifiedAfterSaving(ASaved: boolean): boolean;
begin
    Result := not ASaved;
end;

function SaveQuestion(const AWhat: string): string;
begin
    //  Two lines, as the two hand-written copies were: what changed, then the
    //  question. The name is the caller's, so it matches the tab the user is
    //  shown if they cancel.
    Result := AWhat + ' has been modified.' + #13#10 + 'Save?';
end;

end.
