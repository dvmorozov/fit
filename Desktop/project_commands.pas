// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the File menu's document commands decide, with the dialogs left
to the window.)

WHY THESE ARE HERE AND NOT IN THE HANDLERS. Everything below is a decision - what
Save means when the document has never been saved, what name a file gets when the
user types none, what the unsaved-work question is about - and a decision inside
an event handler is unreachable by any test, because an LCL descendant cannot be
instantiated headlessly. The window is left reading controls and opening dialogs.
It is the same split table_export made for the one export this program used to
have, and close_query for the conversation at shutdown.

SAVE AND EXPORT ARE DIFFERENT THINGS, and the naming here is the whole point.
Save writes the document, and the document can be opened again. Export writes a
table for something else to read, and nothing can open it. Conflating them is how
`Save as Text File...` came to mean "write whichever grid is in front" - a command
whose label could not say what it would do.
}
unit project_commands;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

const
    { The extension a project gets when the user types none. A file with no
      extension opens in nothing - the same reasoning table_export states for
      the .txt it adds. }
    ProjectExtension = '.fitproj';

    { What the unsaved-work question calls the document. The user's own word for
      it, because it is the word on the menu they just used. }
    ProjectDocumentName = 'Project';

    { The Open/Save dialog filter. One entry plus "all files": a project is the
      only thing either dialog opens, and a user who renamed one still has to be
      able to find it. }
    ProjectDialogFilter =
        'Fit project (*' + ProjectExtension + ')|*' + ProjectExtension +
        '|All files (*.*)|*.*';

type
    { What Save has to do before it can write anything. }
    TSaveTarget = (
        { The document already has a path; write there without asking. }
        stUsePath,
        { It has none - it has never been saved - so ask for one. Save then
          behaves as Save As, which is what every application does and what the
          user expects; the alternative is a Save that silently picks a name. }
        stAskForPath);

{ Whether Save can write straight away, given the path the document has. }
function SaveTargetFor(const ACurrentPath: string): TSaveTarget;

{ APath with a project extension, added only when the user typed none.

  NOT FORCED: a user who types their own extension meant it, and rewriting it
  would put a file where they were not looking for it. This only fills a gap,
  exactly as table_export.ExportFileName does for a table. }
function ProjectFileName(const APath: string): string;

{ The title bar text for a document at APath. Empty gives the application's own
  name, which is what a window with nothing open shows. }
{ What to ask before writing over a file that is already there.

  THE WORDING IS HERE rather than in the window for the reason every other rule
  in this unit is: a question the user has to answer is part of the command, and
  the window is where nothing can be tested. }
function OverwriteQuestion(const APath: string): string;

{ What to ask before a data file replaces what is open.

  IMPORTING A PROFILE STARTS THE MODEL AGAIN - the curves, the picks and the
  parameters all go, because they describe data that is being replaced. That is
  the right behaviour and it is not obvious from the menu, so it is asked about
  rather than done quietly: the answer is one click either way, and the cost of
  the wrong one is a session's work. }
function DiscardModelQuestion(const APath: string): string;

function ProjectTitle(const APath: string): string;

implementation

function SaveTargetFor(const ACurrentPath: string): TSaveTarget;
begin
    //  Trimmed, because a path made of spaces is not a path: writing to it
    //  would fail somewhere far away from the command the user gave.
    if Trim(ACurrentPath) = '' then
        Result := stAskForPath
    else
        Result := stUsePath;
end;

function ProjectFileName(const APath: string): string;
begin
    Result := Trim(APath);
    if Result = '' then
        Exit;
    if ExtractFileExt(Result) = '' then
        Result := Result + ProjectExtension;
end;

function ProjectTitle(const APath: string): string;
begin
    //  The document first, which is what the window's own title rule already
    //  does for a data file - the name matters more than the application's,
    //  because the application is the same in every window.
    Result := '';
    if Trim(APath) = '' then
        Exit;
    Result := ExtractFileName(Trim(APath));
end;

function OverwriteQuestion(const APath: string): string;
begin
    //  NAMED, and by the file rather than the path: the user chose it a moment
    //  ago in a dialog that showed them a folder, and what they need confirmed
    //  is which file they are about to lose.
    Result := 'The file "' + ExtractFileName(APath) + '" already exists.' +
        LineEnding + 'Replace it?';
end;

function DiscardModelQuestion(const APath: string): string;
var
    Named: string;
begin
    //  SAYS WHAT IS LOST, not just what is about to happen. "Are you sure?" is
    //  a question nobody can answer; this one can be answered without knowing
    //  how the program works.
    //
    //  A NAMELESS FILE IS POSSIBLE: a project opened in this session has its
    //  source path from the document, and a document written before provenance
    //  was recorded carries none. 'Loading ""' would then be the one part of
    //  the sentence the user cannot make sense of.
    Named := ExtractFileName(APath);
    if Named = '' then
        Named := 'another data file';
    Result := 'Loading ' + Named +
        ' replaces the data in this project.' + LineEnding +
        'The current model - its curves, picks and fitted parameters - will be ' +
        'discarded.' + LineEnding + 'Continue?';
end;

end.
