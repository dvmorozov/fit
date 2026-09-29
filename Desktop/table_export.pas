// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a table leaves this program as text.)

THE ONLY EXPORT THERE IS. Everything the program computes can be read on screen
and, through this, written to a file - so the shape of what comes out is the
whole of what anyone can do with the results elsewhere.

Two decisions, both of which were inside a method that opens a save dialog and
writes a file:

  * the name gets a `.txt` extension when the user typed none, because a file
    with no extension opens in nothing;
  * the cells of a row are separated by tabs, with NO tab after the last one -
    a trailing separator is an extra empty column in every spreadsheet that
    reads the file, and it is invisible in the file itself.

Neither could be tested, and the second is the kind of thing nobody notices
until a column of numbers turns out to be one place to the right of its heading.
}
unit table_export;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

const
    { What a table is saved as when the user names no extension. }
    DefaultTableExtension = '.txt';
    { What separates one cell from the next. A tab rather than a comma: the
      values are decimal numbers written with a full stop, and a comma
      separator makes a file that a spreadsheet in a comma-decimal locale reads
      as twice as many columns. }
    CellSeparator = #9;

type
    { WHAT STILL HAS TO BE ASKED about the name the user chose. Saving a table
      is a conversation, not a call: the file dialog answers once and can leave
      two questions behind, and which one is asked decides what the buttons on
      it mean. }
    TExportQuestion = (
        //  The user closed the file dialog without choosing. Nothing to ask.
        eqCancelled,
        //  A name that is blank or nothing but spaces. Asked because it is
        //  almost always a slip rather than a decision, and the alternative -
        //  failing silently - leaves the user believing the table was saved.
        eqNameIsEmpty,
        //  Something is already there. The one question whose wrong answer
        //  destroys data the user did not offer up.
        eqFileExists,
        //  Nothing in the way.
        eqNone);

    { What the user said to whichever question was asked. }
    TExportAnswer = (eaYes, eaNo, eaCancel);

    { What the caller does next. }
    TExportStep = (
        //  Write the file.
        esWrite,
        //  Put the file dialog up again. The user is correcting a name, not
        //  abandoning the save, so the conversation starts over rather than the
        //  save failing.
        esChooseAgain,
        //  Stop, and report that nothing was written.
        esGiveUp);

{ Which question the name the user chose still raises. AExists is what the
  filesystem says about the name AFTER an extension has been supplied - see
  ExportFileName - because that is the file that would be written. }
function QuestionAbout(AChosen: boolean; const AName: string;
    AExists: boolean): TExportQuestion;

{ What to do, given the question and the answer to it.

  THE THREE ANSWERS ARE NOT INTERCHANGEABLE, and that is the reason this is a
  table rather than a pair of ifs. "No" to overwriting means choose another
  name; "cancel" to the same question means abandon the save. Treating them
  alike either loses a file the user meant to keep or throws away the export
  they were in the middle of. }
function StepFor(AQuestion: TExportQuestion; AAnswer: TExportAnswer): TExportStep;

{ The name to write to, given what the user chose.

  An extension is APPENDED, never replaced: a user who typed one meant it, and
  silently changing `results.dat` to `results.txt` writes the file somewhere
  they did not ask for. }
function ExportFileName(const AChosen: string): string;

{ One row of a table, tab-separated, with no trailing separator. }
function TabSeparatedRow(const ACells: array of string): string;


type
    { How the caller asks the user for a name. False when they gave up. }
    TAskExportPath = function(out APath: string): boolean of object;
    { How the caller puts one of this unit's questions to the user. }
    TAskExportQuestion = function(AQuestion: TExportQuestion;
        const APath: string): TExportAnswer of object;
    { Whether APath is already there. Passed in, so this unit touches no file
      system and every branch below stays reachable from a test. }
    TExportPathExists = function(const APath: string): boolean of object;

{ Runs the whole name-choosing conversation and answers the path to write to, or
  '' when the user gave up.

  WHY IT IS HERE. This was a repeat loop inside the window - and before that, a
  loop with a label and two gotos - so the one thing worth checking about it
  could not be: that "No" to Overwrite asks again while Cancel does not, and that
  cancelling the file dialog ends it rather than looping. The questions and what
  each answer means were already in this unit; only the loop around them was out
  of reach.

  The extension is settled BEFORE the file is asked about, or the user is warned
  about a different file from the one that would be written. }
function ChooseExportPath(AAsk: TAskExportPath;
    AQuestion: TAskExportQuestion; AExists: TExportPathExists): string;

implementation

function QuestionAbout(AChosen: boolean; const AName: string;
    AExists: boolean): TExportQuestion;
begin
    if not AChosen then
        Exit(eqCancelled);
    //  TRIMMED. A name of spaces is not a name, and the filesystem would take
    //  it or refuse it depending on the platform - which is not a difference
    //  the user should meet.
    if Trim(AName) = '' then
        Exit(eqNameIsEmpty);
    if AExists then
        Exit(eqFileExists);
    Result := eqNone;
end;

function StepFor(AQuestion: TExportQuestion;
    AAnswer: TExportAnswer): TExportStep;
begin
    case AQuestion of
        eqCancelled:
            //  Nothing was asked and nothing is written. Not a failure - the
            //  user changed their mind, which is what the dialog's Cancel is
            //  for.
            Result := esGiveUp;
        eqNone:
            Result := esWrite;
        eqNameIsEmpty:
            //  "Select file again?" - yes reopens the dialog, no ends it. There
            //  is no third button, and a cancel arriving here means the same as
            //  no.
            if AAnswer = eaYes then
                Result := esChooseAgain
            else
                Result := esGiveUp;
        else
            //  eqFileExists: "Overwrite?" - yes writes over it, NO goes back for
            //  another name, and CANCEL abandons the save. Three outcomes from
            //  three buttons; collapsing no and cancel is how a user who meant
            //  "not that file" ends up with no export at all.
            case AAnswer of
                eaYes: Result := esWrite;
                eaNo: Result := esChooseAgain;
                else Result := esGiveUp;
            end;
    end;
end;

function ExportFileName(const AChosen: string): string;
begin
    Result := AChosen;
    if Result = '' then
        Exit;
    if ExtractFileExt(Result) = '' then
        Result := Result + DefaultTableExtension;
end;

function TabSeparatedRow(const ACells: array of string): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(ACells) do
    begin
        if i > 0 then
            Result := Result + CellSeparator;
        Result := Result + ACells[i];
    end;
end;

function ChooseExportPath(AAsk: TAskExportPath;
    AQuestion: TAskExportQuestion; AExists: TExportPathExists): string;
var
    Chosen: string;
    Question: TExportQuestion;
    Step: TExportStep;
begin
    repeat
        Chosen := '';
        if AAsk(Chosen) then
        begin
            if Trim(Chosen) <> '' then
                Chosen := ExportFileName(Chosen);
            Question := QuestionAbout(True, Chosen, AExists(Chosen));
        end
        else
            //  The dialog was cancelled. Nothing was chosen, and nothing is
            //  asked about it.
            Question := QuestionAbout(False, '', False);

        Step := StepFor(Question, AQuestion(Question, Chosen));
    until Step <> esChooseAgain;

    if Step = esWrite then
        Result := Chosen
    else
        Result := '';
end;

end.
