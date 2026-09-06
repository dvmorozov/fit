// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What editing a cell of the profile table means, and how a table
leaves this program as text.)

THE PROFILE GRID IS THE ONE PLACE DATA IS TYPED IN, and the export is the only
way anything computed here reaches another program. Both were methods that took
a widget - one an editing-done handler, one a method that opens a save dialog and
writes a file - so neither could be exercised by anything but a person with a
mouse.

The failures are quiet ones. A row applied before it is complete moves a point to
an ordinate the user has not stated. A trailing tab is an extra empty column in
every spreadsheet that reads the file, and invisible in the file itself. A typo
in a cell reads as zero and moves the point to the origin without a word.
}
unit testcase_grid_edit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, grid_edit, table_export;

type
    TGridEditTest = class(TTestCase)
    published
        //  When a cell counts as edited.
        procedure AChangedCellCountsAsEdited;
        procedure AnUnchangedCellDoesNot;
        procedure TheSameNumberWrittenDifferentlyCountsAsEdited;
        procedure ClearingACellCountsAsEditing;

        //  When a row may be applied.
        procedure ARowWithEveryCellFilledIsComplete;
        procedure ARowMissingOneCellIsNot;
        procedure ARowWithNoCellsIsNotComplete;

        //  What a cell's text means.
        procedure ANumberIsItsValue;
        procedure ANegativeAndAFractionAreRead;
        procedure AnEmptyCellIsZero;
        procedure AnUnreadableCellIsAlsoZeroWhichIsTheTrap;
        procedure EmptyAndUnreadableAreDistinguishable;

        //  The exported file's name.
        procedure ANameWithNoExtensionGetsOne;
        procedure ANameWithAnExtensionKeepsIt;
        procedure AnExtensionIsNeverReplaced;
        procedure AnEmptyNameIsLeftAlone;
        procedure ADottedDirectoryIsNotAnExtension;

        //  The exported file's rows.
        procedure CellsAreSeparatedByTabs;
        procedure ThereIsNoTrailingSeparator;
        procedure ASingleCellRowHasNoSeparatorAtAll;
        procedure AnEmptyRowIsAnEmptyLine;
        procedure AnEmptyCellStillTakesItsColumn;

        //  Saving one: which question the chosen name still raises...
        procedure ACancelledDialogAsksNothing;
        procedure AnEmptyNameIsQuestioned;
        procedure ANameOfSpacesIsAlsoEmpty;
        procedure AnExistingFileIsQuestioned;
        procedure AGoodNameRaisesNothing;

        //  ...and what the answer to it means.
        procedure NothingChosenMeansGiveUp;
        procedure NothingInTheWayMeansWrite;
        procedure ChoosingAgainAfterAnEmptyName;
        procedure DecliningAfterAnEmptyNameGivesUp;
        procedure OverwritingWrites;
        procedure RefusingToOverwriteChoosesAgain;
        procedure CancellingTheOverwriteGivesUp;
        procedure RefusingAndCancellingAreNotTheSame;
    end;

implementation

{ ---- when a cell counts as edited ------------------------------------------ }

procedure TGridEditTest.AChangedCellCountsAsEdited;
begin
    AssertTrue('changed', CellWasEdited('1.0', '2.0'));
end;

procedure TGridEditTest.AnUnchangedCellDoesNot;
begin
    //  Clicking into a cell and out again is not an edit, and marking it filled
    //  would let a row be applied that the user only looked at.
    AssertFalse('untouched', CellWasEdited('1.0', '1.0'));
end;

procedure TGridEditTest.TheSameNumberWrittenDifferentlyCountsAsEdited;
begin
    //  COMPARED AS TEXT, deliberately. '1.0' and '1.00' are the same number, and
    //  the user did type something - a row whose cells never mark themselves
    //  filled is a row whose edits are never applied at all.
    AssertTrue('retyped', CellWasEdited('1.0', '1.00'));
end;

procedure TGridEditTest.ClearingACellCountsAsEditing;
begin
    AssertTrue('emptied', CellWasEdited('1.0', ''));
end;

{ ---- when a row may be applied --------------------------------------------- }

procedure TGridEditTest.ARowWithEveryCellFilledIsComplete;
begin
    AssertTrue('both', RowIsComplete([True, True]));
end;

procedure TGridEditTest.ARowMissingOneCellIsNot;
begin
    //  A POINT IS A PAIR. Applying a row with only its abscissa entered moves
    //  the point to an ordinate the user has not stated.
    AssertFalse('no ordinate', RowIsComplete([True, False]));
    AssertFalse('no abscissa', RowIsComplete([False, True]));
    AssertFalse('neither', RowIsComplete([False, False]));
end;

procedure TGridEditTest.ARowWithNoCellsIsNotComplete;
begin
    //  Vacuously true is the wrong answer here: with no cells there is nothing
    //  the user stated, and reporting completeness applies an edit nobody made.
    AssertFalse('nothing', RowIsComplete([]));
end;

{ ---- what a cell's text means ---------------------------------------------- }

procedure TGridEditTest.ANumberIsItsValue;
begin
    AssertEquals('read', 12.5, EditedValue('12.5'), 1E-9);
end;

procedure TGridEditTest.ANegativeAndAFractionAreRead;
begin
    AssertEquals('negative', -3.25, EditedValue('-3.25'), 1E-9);
    AssertEquals('small', 0.001, EditedValue('0.001'), 1E-9);
end;

procedure TGridEditTest.AnEmptyCellIsZero;
begin
    //  What the default was put there for: a row being typed has empty cells in
    //  it, and reading one must not raise in the middle of an edit.
    AssertEquals('empty', 0.0, EditedValue(''), 1E-9);
end;

procedure TGridEditTest.AnUnreadableCellIsAlsoZeroWhichIsTheTrap;
begin
    //  ASSERTED AS IT BEHAVES, AND IT IS A DEFECT. A typo, a stray letter, a
    //  value pasted with its units - all read as zero, and the point moves to
    //  the origin without a word. The default is right for an empty cell and
    //  wrong for this, and the two are indistinguishable to StrToFloatDef.
    //  See findings.md.
    AssertEquals('a typo', 0.0, EditedValue('12..5'), 1E-9);
    AssertEquals('a letter', 0.0, EditedValue('abc'), 1E-9);
    AssertEquals('a value with units', 0.0, EditedValue('12.5 keV'), 1E-9);
end;

procedure TGridEditTest.EmptyAndUnreadableAreDistinguishable;
begin
    //  The first thing anyone fixing the silent zero needs: the difference is
    //  askable now, even though the grid handler does not yet ask.
    AssertTrue('empty is empty', EditedValueIsEmpty(''));
    AssertTrue('and so is whitespace', EditedValueIsEmpty('   '));
    AssertFalse('a typo is not empty', EditedValueIsEmpty('abc'));

    AssertTrue('a number is readable', EditedValueIsReadable('12.5'));
    AssertFalse('a typo is not', EditedValueIsReadable('abc'));
    AssertFalse('and neither is an empty cell',
        EditedValueIsReadable(''));
end;

{ ---- the exported file's name ---------------------------------------------- }

procedure TGridEditTest.ANameWithNoExtensionGetsOne;
begin
    //  A file with no extension opens in nothing.
    AssertEquals('appended', 'results.txt', ExportFileName('results'));
end;

procedure TGridEditTest.ANameWithAnExtensionKeepsIt;
begin
    AssertEquals('unchanged', 'results.txt', ExportFileName('results.txt'));
end;

procedure TGridEditTest.AnExtensionIsNeverReplaced;
begin
    //  A user who typed one meant it. Silently turning results.dat into
    //  results.txt writes the file somewhere they did not ask for, and they
    //  then cannot find it.
    AssertEquals('kept', 'results.dat', ExportFileName('results.dat'));
    AssertEquals('and an unusual one', 'results.csv',
        ExportFileName('results.csv'));
end;

procedure TGridEditTest.AnEmptyNameIsLeftAlone;
begin
    //  The caller refuses an empty name with its own message; producing '.txt'
    //  here would turn a refusal into a hidden file.
    AssertEquals('empty', '', ExportFileName(''));
end;

procedure TGridEditTest.ADottedDirectoryIsNotAnExtension;
begin
    //  A full stop in a directory name is not an extension on the file, and a
    //  name ending in a directory separator has no file part at all.
    AssertEquals('the file still has none',
        '/home/u/my.data/results' + DefaultTableExtension,
        ExportFileName('/home/u/my.data/results'));
end;

{ ---- the exported file's rows ---------------------------------------------- }

procedure TGridEditTest.CellsAreSeparatedByTabs;
begin
    AssertEquals('separated', 'a' + CellSeparator + 'b' + CellSeparator + 'c',
        TabSeparatedRow(['a', 'b', 'c']));
end;

procedure TGridEditTest.ThereIsNoTrailingSeparator;
begin
    //  A TRAILING TAB IS AN EXTRA EMPTY COLUMN in every spreadsheet that reads
    //  the file - and it is invisible in the file, so the report comes back as
    //  "your export has a blank column" and nobody can see why.
    AssertEquals('nothing after the last cell', 'c',
        Copy(TabSeparatedRow(['a', 'b', 'c']), 5, MaxInt));
end;

procedure TGridEditTest.ASingleCellRowHasNoSeparatorAtAll;
begin
    AssertEquals('bare', 'only', TabSeparatedRow(['only']));
end;

procedure TGridEditTest.AnEmptyRowIsAnEmptyLine;
begin
    AssertEquals('empty', '', TabSeparatedRow([]));
end;

procedure TGridEditTest.AnEmptyCellStillTakesItsColumn;
begin
    //  A blank cell in the middle of a row is data - it says this value is not
    //  there - and dropping it would shift every column after it one to the
    //  left, which is the worst kind of wrong an exported table can be.
    AssertEquals('the column is kept',
        'a' + CellSeparator + CellSeparator + 'c',
        TabSeparatedRow(['a', '', 'c']));
end;

{ ---- saving a table: the questions ----------------------------------------- }

procedure TGridEditTest.ACancelledDialogAsksNothing;
begin
    //  The user closed the file dialog. Not a failure and not a question - it
    //  is what Cancel is for.
    AssertTrue('nothing to ask',
        QuestionAbout(False, '', False) = eqCancelled);
end;

procedure TGridEditTest.AnEmptyNameIsQuestioned;
begin
    //  ASKED RATHER THAN FAILED. An empty name is almost always a slip, and
    //  failing silently leaves the user believing the table was saved.
    AssertTrue('asked', QuestionAbout(True, '', False) = eqNameIsEmpty);
end;

procedure TGridEditTest.ANameOfSpacesIsAlsoEmpty;
begin
    //  TRIMMED, because the filesystem would take it or refuse it depending on
    //  the platform - which is not a difference the user should meet.
    AssertTrue('asked', QuestionAbout(True, '   ', False) = eqNameIsEmpty);
end;

procedure TGridEditTest.AnExistingFileIsQuestioned;
begin
    //  THE ONE QUESTION WHOSE WRONG ANSWER DESTROYS DATA the user did not offer
    //  up.
    AssertTrue('asked',
        QuestionAbout(True, 'results.txt', True) = eqFileExists);
end;

procedure TGridEditTest.AGoodNameRaisesNothing;
begin
    AssertTrue('nothing in the way',
        QuestionAbout(True, 'results.txt', False) = eqNone);
end;

{ ---- saving a table: what the answers mean --------------------------------- }

procedure TGridEditTest.NothingChosenMeansGiveUp;
begin
    AssertTrue('stop', StepFor(eqCancelled, eaCancel) = esGiveUp);
end;

procedure TGridEditTest.NothingInTheWayMeansWrite;
begin
    AssertTrue('write', StepFor(eqNone, eaCancel) = esWrite);
end;

procedure TGridEditTest.ChoosingAgainAfterAnEmptyName;
begin
    //  "Select file again?" - yes reopens the dialog, so the user is correcting
    //  a name rather than abandoning the save.
    AssertTrue('ask again', StepFor(eqNameIsEmpty, eaYes) = esChooseAgain);
end;

procedure TGridEditTest.DecliningAfterAnEmptyNameGivesUp;
begin
    AssertTrue('stop', StepFor(eqNameIsEmpty, eaNo) = esGiveUp);
end;

procedure TGridEditTest.OverwritingWrites;
begin
    AssertTrue('write', StepFor(eqFileExists, eaYes) = esWrite);
end;

procedure TGridEditTest.RefusingToOverwriteChoosesAgain;
begin
    //  NO MEANS "NOT THAT FILE", not "forget it". The user still wants the
    //  export; they want it somewhere else.
    AssertTrue('ask again', StepFor(eqFileExists, eaNo) = esChooseAgain);
end;

procedure TGridEditTest.CancellingTheOverwriteGivesUp;
begin
    AssertTrue('stop', StepFor(eqFileExists, eaCancel) = esGiveUp);
end;

procedure TGridEditTest.RefusingAndCancellingAreNotTheSame;
begin
    //  THE WHOLE REASON THIS IS A TABLE and not a pair of ifs. Three buttons,
    //  three outcomes: collapsing no into cancel leaves a user who meant "not
    //  that file" with no export at all, and collapsing it the other way
    //  overwrites a file they declined to overwrite.
    AssertTrue('they differ',
        StepFor(eqFileExists, eaNo) <> StepFor(eqFileExists, eaCancel));
end;

initialization
    //  A unit test: strings in, strings out. No grid, no dialog, no file.
    RegisterTest('unit', TGridEditTest);
end.
