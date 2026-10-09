// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The model history's own rules: what is a new model, what deleting does
to the lineage, which entry is the best, and when an entry can be made current.)

A unit test: records in, records out. Capturing a model from an engine and
putting one back are model_history_session's, and are tested there through a
real service; nothing here needs one, because nothing here talks to one.
}
unit testcase_model_history;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_points_json, fit_project_document, model_history;

type
    TModelHistoryTest = class(TTestCase)
    private
        FHistory: TModelHistory;
        { An entry with AId, holding a model described by AContent, fitted to
          the profile AProfile, with ARFactor. }
        function AnEntry(const AId, AContent: string; ARFactor: double;
            const AProfile: string = 'p1'): THistoryEntry;
        function ProfileNamed(const AHash: string): TPointsData;
        { Records an entry and asserts it was recorded. }
        procedure Record_(const AId, AContent: string; ARFactor: double;
            const AProfile: string = 'p1');
        function ParentOf(const AId: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Recording.
        procedure AFirstRunIsARootAndCurrent;
        procedure EachRunDescendsFromTheModelItStartedFrom;
        procedure ARunThatChangedNothingRecordsNothing;
        procedure ButTheSameModelReachedFromElsewhereIsRecorded;
        procedure FittingFromAnOlderModelBranches;
        procedure AProfileIsStoredOnceHoweverManyEntriesShareIt;

        //  Deleting.
        procedure DeletingAnEntryHandsItsChildrenToItsParent;
        procedure DeletingTheRootMakesItsChildrenRoots;
        procedure DeletingTheCurrentEntryLeavesNothingCurrent;
        procedure AProfileNoEntryUsesAnyMoreIsDropped;
        procedure DeletingAnUnknownEntryChangesNothing;

        //  Naming.
        procedure AnEntryCanBeNamed;

        //  Restoring.
        procedure AModelIsRestoredWithTheProfileItWasFittedTo;
        procedure AnEntryWhoseProfileIsMissingCannotBeMadeCurrent;

        //  The best.
        procedure TheBestIsTheLowestRFactor;
        procedure AModelNeverFittedIsNeverTheBest;
        procedure OnlyModelsOfTheSameDataAreCompared;
        procedure OnlyModelsWithTheSameObjectiveAreCompared;
        procedure WithNothingCurrentTheNewestIsTheReference;
        procedure AnEmptyHistoryHasNoBest;

        //  Reading back.
        procedure LoadingPutsBackEntriesProfilesAndTheCurrentOne;
        procedure ACurrentIdNamingNoEntryIsNotCurrent;
        procedure ClearingEmptiesEverything;

        procedure EveryRunKindHasAName;
    end;

implementation

procedure TModelHistoryTest.SetUp;
begin
    FHistory := TModelHistory.Create;
end;

procedure TModelHistoryTest.TearDown;
begin
    FreeAndNil(FHistory);
end;

function TModelHistoryTest.AnEntry(const AId, AContent: string;
    ARFactor: double; const AProfile: string): THistoryEntry;
begin
    Result := Default(THistoryEntry);
    Result.Id := AId;
    Result.CreatedUtc := '2026-10-02T12:00:00Z';
    Result.ContentHash := AContent;
    Result.ProfileHash := AProfile;
    Result.Model := EmptyProjectDocument;
    Result.Model.RFactor := ARFactor;
    Result.Model.Settings.LossKind := 1;
end;

function TModelHistoryTest.ProfileNamed(const AHash: string): TPointsData;
begin
    Result := Default(TPointsData);
    //  A profile its hash can be told apart by in an assertion.
    SetLength(Result.X, 1);
    SetLength(Result.Y, 1);
    Result.X[0] := Length(AHash);
    Result.Y[0] := Ord(AHash[Length(AHash)]);
end;

procedure TModelHistoryTest.Record_(const AId, AContent: string;
    ARFactor: double; const AProfile: string);
begin
    AssertTrue(AId + ' is recorded', FHistory.Add(
        AnEntry(AId, AContent, ARFactor, AProfile), ProfileNamed(AProfile)));
end;

function TModelHistoryTest.ParentOf(const AId: string): string;
begin
    AssertTrue(AId + ' is there', FHistory.IndexOf(AId) >= 0);
    Result := FHistory[FHistory.IndexOf(AId)].ParentId;
end;

{ ---- recording -------------------------------------------------------------- }

procedure TModelHistoryTest.AFirstRunIsARootAndCurrent;
begin
    Record_('a', 'm1', 0.1);
    AssertEquals('one entry', 1, FHistory.Count);
    AssertEquals('descending from nothing', '', ParentOf('a'));
    AssertEquals('and it is the model now', 'a', FHistory.CurrentId);
end;

procedure TModelHistoryTest.EachRunDescendsFromTheModelItStartedFrom;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    AssertEquals('b started from a', 'a', ParentOf('b'));
    AssertEquals('and is now current', 'b', FHistory.CurrentId);
end;

procedure TModelHistoryTest.ARunThatChangedNothingRecordsNothing;
begin
    //  Pressing Fit on a converged model: the engine finds what it already
    //  had. A row per press would bury the models that differ.
    Record_('a', 'm1', 0.1);
    AssertFalse('the same model again is not recorded',
        FHistory.Add(AnEntry('b', 'm1', 0.1), ProfileNamed('p1')));
    AssertEquals('still one entry', 1, FHistory.Count);
    AssertEquals('and the current one is unchanged', 'a', FHistory.CurrentId);
end;

procedure TModelHistoryTest.ButTheSameModelReachedFromElsewhereIsRecorded;
begin
    //  ONLY THE CURRENT ENTRY IS COMPARED. Two branches converging on one
    //  model is a fact about the lineage, and recording it is what shows it.
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    FHistory.SetCurrent('a');
    Record_('c', 'm2', 0.08);
    AssertEquals('c is its own entry', 3, FHistory.Count);
    AssertEquals('descending from where it started', 'a', ParentOf('c'));
end;

procedure TModelHistoryTest.FittingFromAnOlderModelBranches;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    FHistory.SetCurrent('a');
    Record_('c', 'm3', 0.07);
    AssertEquals('b is still a child of a', 'a', ParentOf('b'));
    AssertEquals('and so is c - a branch', 'a', ParentOf('c'));
end;

procedure TModelHistoryTest.AProfileIsStoredOnceHoweverManyEntriesShareIt;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    Record_('c', 'm3', 0.07, 'p2');
    AssertEquals('two profiles for three entries', 2,
        Length(FHistory.Profiles));
end;

{ ---- deleting --------------------------------------------------------------- }

procedure TModelHistoryTest.DeletingAnEntryHandsItsChildrenToItsParent;
begin
    //  a <- b <- c, and b goes: c descends from a, not from nothing. An entry
    //  claiming to start from nothing would draw as a second history.
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    Record_('c', 'm3', 0.07);
    FHistory.Delete('b');
    AssertEquals('two left', 2, FHistory.Count);
    AssertEquals('c now descends from a', 'a', ParentOf('c'));
    AssertEquals('c is still current', 'c', FHistory.CurrentId);
end;

procedure TModelHistoryTest.DeletingTheRootMakesItsChildrenRoots;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    FHistory.Delete('a');
    AssertEquals('b descends from nothing now', '', ParentOf('b'));
end;

procedure TModelHistoryTest.DeletingTheCurrentEntryLeavesNothingCurrent;
begin
    //  NOT the parent made current: the live model is still the deleted
    //  entry's, and marking another entry current would claim it was that one.
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08);
    FHistory.Delete('b');
    AssertEquals('nothing is current', '', FHistory.CurrentId);
end;

procedure TModelHistoryTest.AProfileNoEntryUsesAnyMoreIsDropped;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.08, 'p2');
    FHistory.Delete('b');
    AssertEquals('only the profile still in use', 1, Length(FHistory.Profiles));
    AssertEquals('which is a''s', 'p1', FHistory.Profiles[0].Hash);
end;

procedure TModelHistoryTest.DeletingAnUnknownEntryChangesNothing;
begin
    Record_('a', 'm1', 0.1);
    FHistory.Delete('nobody');
    AssertEquals('still there', 1, FHistory.Count);
    AssertEquals('still current', 'a', FHistory.CurrentId);
end;

{ ---- naming ----------------------------------------------------------------- }

procedure TModelHistoryTest.AnEntryCanBeNamed;
begin
    Record_('a', 'm1', 0.1);
    FHistory.Rename('a', 'two peaks, free widths');
    AssertEquals('', 'two peaks, free widths', FHistory[0].Name);
end;

{ ---- restoring -------------------------------------------------------------- }

procedure TModelHistoryTest.AModelIsRestoredWithTheProfileItWasFittedTo;
var
    Doc: TProjectDocument;
begin
    Record_('a', 'm1', 0.1, 'p1');
    Record_('b', 'm2', 0.08, 'p22');
    AssertTrue('restorable', FHistory.IsRestorable(1));
    AssertTrue('restored', FHistory.ModelToRestore(1, Doc));
    AssertEquals('with its own profile - not the first one', 3.0,
        Doc.Profile.X[0], 0);
    AssertEquals('the model itself', 0.08, Doc.RFactor, 0);
end;

procedure TModelHistoryTest.AnEntryWhoseProfileIsMissingCannotBeMadeCurrent;
var
    Entries: THistoryEntries;
    Doc: TProjectDocument;
begin
    //  A file damaged by hand: the entry is kept and shown, and refused -
    //  restoring it onto whatever profile is loaded would fit a model to data
    //  it was never fitted to.
    SetLength(Entries, 1);
    Entries[0] := AnEntry('a', 'm1', 0.1, 'gone');
    FHistory.Load(Entries, nil, 'a');
    AssertEquals('kept', 1, FHistory.Count);
    AssertFalse('not restorable', FHistory.IsRestorable(0));
    AssertFalse('and not restored', FHistory.ModelToRestore(0, Doc));
end;

{ ---- the best --------------------------------------------------------------- }

procedure TModelHistoryTest.TheBestIsTheLowestRFactor;
begin
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', 0.05);
    Record_('c', 'm3', 0.07);
    AssertEquals('b', 1, FHistory.BestIndex);
end;

procedure TModelHistoryTest.AModelNeverFittedIsNeverTheBest;
begin
    //  -1 is "no fit has run", not the lowest figure on the list.
    Record_('a', 'm1', 0.1);
    Record_('b', 'm2', -1);
    AssertEquals('a', 0, FHistory.BestIndex);
end;

procedure TModelHistoryTest.OnlyModelsOfTheSameDataAreCompared;
begin
    //  The current model is fitted to p2 - a smoothed profile, say. A lower
    //  R-factor over the raw data says nothing about which model fits this.
    Record_('a', 'm1', 0.01, 'p1');
    Record_('b', 'm2', 0.08, 'p2');
    Record_('c', 'm3', 0.07, 'p2');
    AssertEquals('c, among the models of p2', 2, FHistory.BestIndex);
end;

procedure TModelHistoryTest.OnlyModelsWithTheSameObjectiveAreCompared;
var
    E: THistoryEntry;
begin
    Record_('a', 'm1', 0.01);
    E := AnEntry('b', 'm2', 0.08);
    E.Model.Settings.LossKind := 2;
    AssertTrue(FHistory.Add(E, ProfileNamed('p1')));
    AssertEquals('b alone shares its own objective', 1, FHistory.BestIndex);
end;

procedure TModelHistoryTest.WithNothingCurrentTheNewestIsTheReference;
begin
    Record_('a', 'm1', 0.01, 'p1');
    Record_('b', 'm2', 0.08, 'p2');
    Record_('c', 'm3', 0.07, 'p2');
    FHistory.SetCurrent('');
    AssertEquals('compared with the newest, c', 2, FHistory.BestIndex);
end;

procedure TModelHistoryTest.AnEmptyHistoryHasNoBest;
begin
    AssertEquals('', -1, FHistory.BestIndex);
end;

{ ---- reading back ----------------------------------------------------------- }

procedure TModelHistoryTest.LoadingPutsBackEntriesProfilesAndTheCurrentOne;
var
    Entries: THistoryEntries;
    Profiles: THistoryProfiles;
begin
    SetLength(Entries, 2);
    Entries[0] := AnEntry('a', 'm1', 0.1);
    Entries[1] := AnEntry('b', 'm2', 0.08);
    Entries[1].ParentId := 'a';
    SetLength(Profiles, 1);
    Profiles[0].Hash := 'p1';
    Profiles[0].Points := ProfileNamed('p1');
    FHistory.Load(Entries, Profiles, 'a');
    AssertEquals('both entries', 2, FHistory.Count);
    AssertEquals('their lineage', 'a', ParentOf('b'));
    AssertEquals('the current one', 'a', FHistory.CurrentId);
    AssertTrue('and restorable', FHistory.IsRestorable(1));
end;

procedure TModelHistoryTest.ACurrentIdNamingNoEntryIsNotCurrent;
var
    Entries: THistoryEntries;
begin
    SetLength(Entries, 1);
    Entries[0] := AnEntry('a', 'm1', 0.1);
    FHistory.Load(Entries, nil, 'deleted-by-hand');
    AssertEquals('nothing current', '', FHistory.CurrentId);
end;

procedure TModelHistoryTest.ClearingEmptiesEverything;
begin
    Record_('a', 'm1', 0.1);
    FHistory.Clear;
    AssertEquals('no entries', 0, FHistory.Count);
    AssertEquals('no profiles', 0, Length(FHistory.Profiles));
    AssertEquals('nothing current', '', FHistory.CurrentId);
end;

procedure TModelHistoryTest.EveryRunKindHasAName;
var
    K: THistoryRunKind;
begin
    //  SELF-ENFORCING: a kind added without a name would show as a blank row.
    for K := Low(THistoryRunKind) to High(THistoryRunKind) do
        AssertTrue(IntToStr(Ord(K)) + ' is named',
            HistoryRunKindName(K) <> '');
end;

initialization
    //  A unit test: records in, records out. No engine, no file.
    RegisterTest('unit', TModelHistoryTest);
end.
