// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the File menu's document commands decide, and what separates Save
from Export.)

SAVE WRITES THE DOCUMENT AND EXPORT WRITES A TABLE. The distinction is the reason
these commands were reworked: `Save as Text File...` wrote whichever grid was in
front, could not be opened again by anything, and cleared the table's modified
flag as though the work had been kept. Once Save means the project, a command
that writes a one-way text file has to say Export - and each export has to name
its own table, because a label that cannot say what it will do is a decision made
from state the user cannot see.

These are the decisions behind those commands. The dialogs stay in the window,
which is where they have to be and where nothing can test them.
}
unit testcase_project_commands;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, project_commands;

type
    TProjectCommandsTest = class(TTestCase)
    published
        procedure SavingADocumentThatHasNeverBeenSavedAsksForAName;
        procedure SavingOneThatHasAPathWritesStraightToIt;
        procedure APathOfSpacesIsNoPathAtAll;

        procedure ANameWithNoExtensionGetsTheProjectOne;
        procedure ANameTheUserGaveAnExtensionToIsLeftAlone;
        procedure AnEmptyNameIsNotTurnedIntoAnExtension;

        procedure TheTitleIsTheDocumentRatherThanItsWholePath;
        procedure WithNoDocumentThereIsNoTitleToAdd;

        procedure TheFilterOffersProjectsAndStillLetsAnyFileBeChosen;

        //  The two questions the document commands put
        procedure TheOverwriteQuestionNamesTheFileAndNotThePath;
        procedure TheDiscardQuestionSaysWhatIsAboutToBeLost;
        procedure AndNamesTheFileBeingLoaded;
        procedure AFileWithNoNameIsStillAReadableQuestion;
    end;

implementation

procedure TProjectCommandsTest.SavingADocumentThatHasNeverBeenSavedAsksForAName;
begin
    //  Save behaves as Save As the first time, which is what every application
    //  does. The alternative is a Save that silently picks a name, and then the
    //  user's work is somewhere they did not choose.
    AssertEquals(Ord(stAskForPath), Ord(SaveTargetFor('')));
end;

procedure TProjectCommandsTest.SavingOneThatHasAPathWritesStraightToIt;
begin
    //  And no dialog. Asking every time is how people stop reading the dialog.
    AssertEquals(Ord(stUsePath), Ord(SaveTargetFor('C:\work\run7.fitproj')));
end;

procedure TProjectCommandsTest.APathOfSpacesIsNoPathAtAll;
begin
    //  It would otherwise be written to, and fail somewhere a long way from the
    //  command the user gave.
    AssertEquals(Ord(stAskForPath), Ord(SaveTargetFor('   ')));
end;

procedure TProjectCommandsTest.ANameWithNoExtensionGetsTheProjectOne;
begin
    //  A file with no extension opens in nothing - the same reasoning
    //  table_export states for the .txt it adds to a table.
    AssertEquals('run7' + ProjectExtension, ProjectFileName('run7'));
end;

procedure TProjectCommandsTest.ANameTheUserGaveAnExtensionToIsLeftAlone;
begin
    //  NOT FORCED. Someone who typed an extension meant it, and rewriting it
    //  would put the file somewhere they will not look for it.
    AssertEquals('run7.zip', ProjectFileName('run7.zip'));
    AssertEquals('already right', 'run7' + ProjectExtension,
        ProjectFileName('run7' + ProjectExtension));
end;

procedure TProjectCommandsTest.AnEmptyNameIsNotTurnedIntoAnExtension;
begin
    //  Otherwise cancelling a dialog produces a file called ".fitproj", which
    //  is hidden on two of the three platforms this runs on.
    AssertEquals('', ProjectFileName(''));
    AssertEquals('', ProjectFileName('   '));
end;

procedure TProjectCommandsTest.TheTitleIsTheDocumentRatherThanItsWholePath;
begin
    //  A title bar is narrow and a path is long; the name is the part that
    //  tells one window from another.
    AssertEquals('run7.fitproj',
        ProjectTitle('C:\some\deep\folder\run7.fitproj'));
end;

procedure TProjectCommandsTest.WithNoDocumentThereIsNoTitleToAdd;
begin
    AssertEquals('', ProjectTitle(''));
    AssertEquals('', ProjectTitle('  '));
end;

procedure TProjectCommandsTest.TheFilterOffersProjectsAndStillLetsAnyFileBeChosen;
begin
    //  A user who renamed a project still has to be able to open it, so the
    //  filter is not a gate.
    AssertTrue('projects are offered',
        Pos('*' + ProjectExtension, ProjectDialogFilter) > 0);
    AssertTrue('and so is everything else', Pos('*.*', ProjectDialogFilter) > 0);
end;

procedure TProjectCommandsTest.TheOverwriteQuestionNamesTheFileAndNotThePath;
var
    Q: string;
begin
    Q := OverwriteQuestion('C:' + PathDelim + 'work' + PathDelim + 'run7.fitproj');
    AssertTrue('names the file: ' + Q, Pos('run7.fitproj', Q) > 0);
    AssertEquals('and not the folder it is in', 0, Pos('work', Q));
end;

procedure TProjectCommandsTest.TheDiscardQuestionSaysWhatIsAboutToBeLost;
var
    Q: string;
begin
    //  NOT "Are you sure?", which is a question nobody can answer. What the
    //  user has to weigh is that the model goes - and that is exactly the part
    //  that is invisible from a menu item called "Import Profile".
    Q := DiscardModelQuestion('two.dat');
    AssertTrue('says the model goes: ' + Q, Pos('model', LowerCase(Q)) > 0);
    AssertTrue('and what the model is made of',
        (Pos('curves', LowerCase(Q)) > 0) and (Pos('picks', LowerCase(Q)) > 0));
end;

procedure TProjectCommandsTest.AndNamesTheFileBeingLoaded;
begin
    AssertTrue('the file is named',
        Pos('two.dat', DiscardModelQuestion('two.dat')) > 0);
end;

procedure TProjectCommandsTest.AFileWithNoNameIsStillAReadableQuestion;
var
    Q: string;
begin
    //  A project written before provenance was recorded carries no source path,
    //  and 'Loading ""' is the one part of the sentence a user cannot make
    //  sense of.
    Q := DiscardModelQuestion('');
    AssertEquals('no empty quotes', 0, Pos('""', Q));
    AssertTrue('and it still says what is lost', Pos('model', LowerCase(Q)) > 0);
end;

initialization
    //  A unit test: strings in, strings out. The dialogs are the window's.
    RegisterTest('unit', TProjectCommandsTest);
end.
