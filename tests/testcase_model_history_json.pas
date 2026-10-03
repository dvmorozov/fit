// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The model history in the project file: what it writes, what it reads
back, what it carries through for a newer build, and when two models are the
same model.)

A unit test: parts in memory, no archive on disk and no engine.
}
unit testcase_model_history_json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson,
    fit_points_json, fit_project_archive, fit_project_document,
    fit_project_json, model_history, model_history_json;

type
    TModelHistoryJsonTest = class(TTestCase)
    private
        FHistory: TModelHistory;
        FRead: TModelHistory;
        function AProfileOf(AFirst: double): TPointsData;
        { A captured model: picks with handles, a fitted curve, a module's
          document, and the window's context a capture also carries. }
        function ACapture(ASigma: double; const AProfile: TPointsData):
            TProjectDocument;
        { Records a run ending in a model with ASigma, fitted to AProfile. }
        procedure Run(const AId: string; ASigma: double;
            const AProfile: TPointsData);
        function HistoryPartNames(const AParts: TProjectParts): string;
        function RoundTrip: TProjectParts;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What is written.
        procedure AProjectWithNoHistoryHasNoHistoryPart;
        procedure EachEntryAndEachProfileIsItsOwnPart;
        procedure AProfileSharedByEntriesIsWrittenOnce;
        procedure TheOtherPartsOfTheProjectAreUntouched;
        procedure ADeletedEntryLeavesTheFile;

        //  What is read back.
        procedure TheLineageNamesAndTheCurrentEntryComeBack;
        procedure TheModelComesBackAsTheProjectsOwnSectionsHoldIt;
        procedure TheContentHashIsRecomputedOnReading;
        procedure AnEntryWhoseModelIsMissingIsLeftOut;
        procedure AnEntryWhoseProfileIsMissingIsKeptButNotRestorable;
        procedure AnUnreadableIndexLeavesNoHistory;
        procedure HistoryPartsAreNotMistakenForAModulesDocument;

        //  What a newer build wrote.
        procedure ARecordedModelIsWrittenBackExactlyAsItWasRead;
        procedure AnIndexMemberThisBuildDoesNotKnowSurvives;
        procedure ARunKindThisBuildDoesNotKnowSurvives;

        //  When two models are the same model.
        procedure TheSameProfileHashesTheSame;
        procedure OnePointMovedIsAnotherProfile;
        procedure AMeasurementIsNotPartOfTheModel;
        procedure NorIsWhenOrWhereItWasRecorded;
        procedure AValueIsPartOfTheModel;
        procedure SoIsTheProfileItWasFittedTo;

        //  A new entry.
        procedure ANewEntryNamesItsProfileAndHoldsNoCopy;
        procedure ANewEntryKeepsNothingOfTheWindow;
        procedure ANewEntrySaysWhenInUtc;
    end;

implementation

procedure TModelHistoryJsonTest.SetUp;
begin
    FHistory := TModelHistory.Create;
    FRead := TModelHistory.Create;
end;

procedure TModelHistoryJsonTest.TearDown;
begin
    FreeAndNil(FHistory);
    FreeAndNil(FRead);
end;

function TModelHistoryJsonTest.AProfileOf(AFirst: double): TPointsData;
var
    i: longint;
begin
    Result := Default(TPointsData);
    SetLength(Result.X, 5);
    SetLength(Result.Y, 5);
    for i := 0 to 4 do
    begin
        Result.X[i] := i;
        Result.Y[i] := 10 + i * i;
    end;
    Result.Y[0] := AFirst;
end;

function TModelHistoryJsonTest.ACapture(ASigma: double;
    const AProfile: TPointsData): TProjectDocument;
begin
    Result := EmptyProjectDocument;
    Result.Profile := AProfile;
    SetLength(Result.Bounds.X, 2);
    SetLength(Result.Bounds.Y, 2);
    Result.Bounds.X[1] := 4;
    SetLength(Result.Positions.X, 1);
    SetLength(Result.Positions.Y, 1);
    SetLength(Result.Positions.Ids, 1);
    Result.Positions.X[0] := 2;
    Result.Positions.Y[0] := 14;
    Result.Positions.Ids[0] := '{0A0A0A0A-1111-2222-3333-444444444444}';
    Result.Settings.CurveTypeId := '{11111111-2222-3333-4444-555555555555}';
    Result.Settings.LossKind := 1;
    Result.Settings.Stated := AllProjectSettings;
    SetLength(Result.Curves, 1);
    Result.Curves[0].Id := '{0A0A0A0A-1111-2222-3333-444444444444}';
    Result.Curves[0].Fitted := True;
    SetLength(Result.Curves[0].Params, 1);
    Result.Curves[0].Params[0].Name := 'sigma';
    Result.Curves[0].Params[0].Value := ASigma;
    Result.Curves[0].Params[0].Error := 0.01;
    Result.RFactor := 0.05;
    SetLength(Result.ModuleDocuments, 1);
    Result.ModuleDocuments[0].Module := 'sample';
    Result.ModuleDocuments[0].Content := '{"markup":[1,2,3]}';
    //  What the window adds to a capture, and a history entry must not keep.
    Result.HasUi := True;
    Result.Ui.ActiveTab := 3;
    Result.Provenance.SourcePath := '/data/source.dat';
    Result.CreatedUtc := '2026-01-01T00:00:00Z';
    Result.ModifiedUtc := '2026-01-02T00:00:00Z';
end;

procedure TModelHistoryJsonTest.Run(const AId: string; ASigma: double;
    const AProfile: TPointsData);
var
    Profile: TPointsData;
begin
    AssertTrue(AId + ' recorded', FHistory.Add(
        NewHistoryEntry(ACapture(ASigma, AProfile), AId,
            hrkMinimizeDifference, False, EncodeDate(2026, 10, 2), Profile),
        Profile));
end;

function TModelHistoryJsonTest.HistoryPartNames(
    const AParts: TProjectParts): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AParts) do
        if Pos(HistoryPartPrefix, AParts[i].Name) = 1 then
            Result := Result + AParts[i].Name + ';';
end;

function TModelHistoryJsonTest.RoundTrip: TProjectParts;
begin
    Result := HistoryToParts(nil, FHistory);
    HistoryFromParts(Result, FRead);
end;

{ ---- what is written -------------------------------------------------------- }

procedure TModelHistoryJsonTest.AProjectWithNoHistoryHasNoHistoryPart;
var
    Parts: TProjectParts;
begin
    //  A project from a session that never fitted is the file it always was.
    Parts := WithPart(nil, 'problem.json', '{}');
    Parts := HistoryToParts(Parts, FHistory);
    AssertEquals('no history part', '', HistoryPartNames(Parts));
    AssertEquals('and nil writes none either', '',
        HistoryPartNames(HistoryToParts(Parts, nil)));
end;

procedure TModelHistoryJsonTest.EachEntryAndEachProfileIsItsOwnPart;
var
    Names: string;
begin
    Run('a', 1.5, AProfileOf(10));
    Names := HistoryPartNames(HistoryToParts(nil, FHistory));
    AssertTrue('the index: ' + Names, Pos(HistoryIndexPart, Names) > 0);
    AssertTrue('the entry: ' + Names,
        Pos(HistoryEntryPartName('a'), Names) > 0);
    AssertTrue('the profile: ' + Names, Pos(HistoryProfilePartName(
        PointsHash(AProfileOf(10))), Names) > 0);
end;

procedure TModelHistoryJsonTest.AProfileSharedByEntriesIsWrittenOnce;
var
    Parts: TProjectParts;
    i, Profiles: longint;
begin
    Run('a', 1.5, AProfileOf(10));
    Run('b', 1.7, AProfileOf(10));
    Run('c', 1.9, AProfileOf(11));
    Parts := HistoryToParts(nil, FHistory);
    Profiles := 0;
    for i := 0 to High(Parts) do
        if Pos('history/profiles/', Parts[i].Name) = 1 then
            Inc(Profiles);
    AssertEquals('two profiles for three models', 2, Profiles);
end;

procedure TModelHistoryJsonTest.TheOtherPartsOfTheProjectAreUntouched;
var
    Parts: TProjectParts;
    Content: string;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := WithPart(nil, 'problem.json', '{"x":1}');
    Parts := WithPart(Parts, 'modules/sample.json', '{"y":2}');
    Parts := HistoryToParts(Parts, FHistory);
    AssertTrue(PartContent(Parts, 'problem.json', Content));
    AssertEquals('the problem', '{"x":1}', Content);
    AssertTrue(PartContent(Parts, 'modules/sample.json', Content));
    AssertEquals('a module''s document', '{"y":2}', Content);
end;

procedure TModelHistoryJsonTest.ADeletedEntryLeavesTheFile;
var
    Parts: TProjectParts;
    Names: string;
begin
    //  THE PARTS AS READ ARE WHERE A SAVE STARTS, so a part for an entry the
    //  user deleted is still in them - and written back, the entry would
    //  reappear the next time the project is opened.
    Run('a', 1.5, AProfileOf(10));
    Run('b', 1.7, AProfileOf(11));
    Parts := HistoryToParts(nil, FHistory);
    FHistory.Delete('b');
    Names := HistoryPartNames(HistoryToParts(Parts, FHistory));
    AssertEquals('b''s model is gone: ' + Names, 0,
        Pos(HistoryEntryPartName('b'), Names));
    AssertEquals('and so is the profile only b used: ' + Names, 0,
        Pos(HistoryProfilePartName(PointsHash(AProfileOf(11))), Names));
    AssertTrue('a stays', Pos(HistoryEntryPartName('a'), Names) > 0);
end;

{ ---- what is read back ------------------------------------------------------ }

procedure TModelHistoryJsonTest.TheLineageNamesAndTheCurrentEntryComeBack;
begin
    Run('a', 1.5, AProfileOf(10));
    Run('b', 1.7, AProfileOf(10));
    FHistory.Rename('a', 'first try');
    FHistory.SetCurrent('a');
    RoundTrip;
    AssertEquals('both', 2, FRead.Count);
    AssertEquals('in order', 'a', FRead[0].Id);
    AssertEquals('the lineage', 'a', FRead[1].ParentId);
    AssertEquals('the name', 'first try', FRead[0].Name);
    AssertEquals('the current one', 'a', FRead.CurrentId);
    AssertEquals('the run', Ord(hrkMinimizeDifference), Ord(FRead[1].RunKind));
    AssertEquals('when', FHistory[1].CreatedUtc, FRead[1].CreatedUtc);
end;

procedure TModelHistoryJsonTest.TheModelComesBackAsTheProjectsOwnSectionsHoldIt;
var
    Doc: TProjectDocument;
begin
    Run('a', 1.5, AProfileOf(10));
    RoundTrip;
    AssertTrue('restorable', FRead.ModelToRestore(0, Doc));
    AssertEquals('the profile it was fitted to', 10.0, Doc.Profile.Y[0], 0);
    AssertEquals('the pick', 2.0, Doc.Positions.X[0], 0);
    AssertEquals('with its handle', '{0A0A0A0A-1111-2222-3333-444444444444}',
        Doc.Positions.Ids[0]);
    AssertEquals('the fitted value', 1.5, Doc.Curves[0].Params[0].Value, 0);
    AssertTrue('still fitted', Doc.Curves[0].Fitted);
    AssertEquals('the R-factor it measured', 0.05, Doc.RFactor, 0);
    AssertEquals('the curve type', '{11111111-2222-3333-4444-555555555555}',
        Doc.Settings.CurveTypeId);
    AssertEquals('the module''s document', '{"markup":[1,2,3]}',
        Doc.ModuleDocuments[0].Content);
end;

procedure TModelHistoryJsonTest.TheContentHashIsRecomputedOnReading;
begin
    //  NOT STORED, so it cannot disagree with the model it describes - and a
    //  model made current must compare equal to the entry it came from.
    Run('a', 1.5, AProfileOf(10));
    RoundTrip;
    AssertEquals(FHistory[0].ContentHash, FRead[0].ContentHash);
    AssertTrue('and is not empty', FRead[0].ContentHash <> '');
end;

procedure TModelHistoryJsonTest.AnEntryWhoseModelIsMissingIsLeftOut;
var
    Parts, Kept: TProjectParts;
    i: longint;
begin
    Run('a', 1.5, AProfileOf(10));
    Run('b', 1.7, AProfileOf(10));
    Parts := HistoryToParts(nil, FHistory);
    Kept := nil;
    for i := 0 to High(Parts) do
        if Parts[i].Name <> HistoryEntryPartName('a') then
            Kept := WithPart(Kept, Parts[i].Name, Parts[i].Content);
    HistoryFromParts(Kept, FRead);
    AssertEquals('only b', 1, FRead.Count);
    AssertEquals('b', 'b', FRead[0].Id);
end;

procedure TModelHistoryJsonTest.AnEntryWhoseProfileIsMissingIsKeptButNotRestorable;
var
    Parts, Kept: TProjectParts;
    i: longint;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := HistoryToParts(nil, FHistory);
    Kept := nil;
    for i := 0 to High(Parts) do
        if Pos('history/profiles/', Parts[i].Name) <> 1 then
            Kept := WithPart(Kept, Parts[i].Name, Parts[i].Content);
    HistoryFromParts(Kept, FRead);
    AssertEquals('kept', 1, FRead.Count);
    AssertFalse('not restorable', FRead.IsRestorable(0));
end;

procedure TModelHistoryJsonTest.AnUnreadableIndexLeavesNoHistory;
begin
    //  NEVER A REASON NOT TO OPEN THE PROJECT: the model in it is still the
    //  user's work, whatever happened to its history.
    Run('a', 1.5, AProfileOf(10));
    HistoryFromParts(WithPart(HistoryToParts(nil, FHistory), HistoryIndexPart,
        'not json'), FRead);
    AssertEquals('nothing', 0, FRead.Count);
end;

procedure TModelHistoryJsonTest.HistoryPartsAreNotMistakenForAModulesDocument;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Fault: string;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := ProjectToParts(EmptyProjectDocument);
    Parts := HistoryToParts(Parts, FHistory);
    AssertTrue('a project: ' + Fault, ProjectFromParts(Parts, Doc, Fault));
    AssertEquals('and no module document came from the history', 0,
        Length(Doc.ModuleDocuments));
end;

{ ---- what a newer build wrote ----------------------------------------------- }

procedure TModelHistoryJsonTest.ARecordedModelIsWrittenBackExactlyAsItWasRead;
var
    Parts: TProjectParts;
    Newer, Back: string;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := HistoryToParts(nil, FHistory);
    AssertTrue(PartContent(Parts, HistoryEntryPartName('a'), Newer));
    //  A member a newer build added to a recorded model.
    Newer := Copy(Newer, 1, Length(Newer) - 1) + ', "fromANewerBuild" : 7 }';
    Parts := WithPart(Parts, HistoryEntryPartName('a'), Newer);
    HistoryFromParts(Parts, FRead);
    AssertTrue(PartContent(HistoryToParts(Parts, FRead),
        HistoryEntryPartName('a'), Back));
    AssertEquals('byte for byte', Newer, Back);
end;

procedure TModelHistoryJsonTest.AnIndexMemberThisBuildDoesNotKnowSurvives;
var
    Parts: TProjectParts;
    Index, Back: string;
    O: TJSONObject;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := HistoryToParts(nil, FHistory);
    AssertTrue(PartContent(Parts, HistoryIndexPart, Index));
    O := AsObject(Index);
    try
        O.Add('newerTop', 'kept');
        TJSONObject(O.Arrays['entries'].Items[0]).Add('newerInEntry', 'kept');
        Parts := WithPart(Parts, HistoryIndexPart, O.AsJSON);
    finally
        O.Free;
    end;
    HistoryFromParts(Parts, FRead);
    //  AND RENAMED, so the index really is rewritten rather than copied.
    FRead.Rename('a', 'renamed');
    AssertTrue(PartContent(HistoryToParts(Parts, FRead), HistoryIndexPart,
        Back));
    AssertTrue('at the top: ' + Back, Pos('newerTop', Back) > 0);
    AssertTrue('inside the entry: ' + Back, Pos('newerInEntry', Back) > 0);
    AssertTrue('and the rename took: ' + Back, Pos('renamed', Back) > 0);
end;

procedure TModelHistoryJsonTest.ARunKindThisBuildDoesNotKnowSurvives;
var
    Parts: TProjectParts;
    Index, Back: string;
begin
    Run('a', 1.5, AProfileOf(10));
    Parts := HistoryToParts(nil, FHistory);
    AssertTrue(PartContent(Parts, HistoryIndexPart, Index));
    Index := StringReplace(Index, '"minimize-difference"', '"bayesian-sweep"',
        []);
    Parts := WithPart(Parts, HistoryIndexPart, Index);
    HistoryFromParts(Parts, FRead);
    AssertEquals('read as a run it cannot name', Ord(hrkOther),
        Ord(FRead[0].RunKind));
    AssertTrue(PartContent(HistoryToParts(Parts, FRead), HistoryIndexPart,
        Back));
    AssertTrue('and written back as it was: ' + Back,
        Pos('"bayesian-sweep"', Back) > 0);
end;

{ ---- when two models are the same model ------------------------------------- }

procedure TModelHistoryJsonTest.TheSameProfileHashesTheSame;
begin
    AssertEquals(PointsHash(AProfileOf(10)), PointsHash(AProfileOf(10)));
    AssertTrue('and is not empty', PointsHash(AProfileOf(10)) <> '');
end;

procedure TModelHistoryJsonTest.OnePointMovedIsAnotherProfile;
begin
    //  A smoothed profile differs from the raw one by small amounts at every
    //  point - and is a different profile.
    AssertFalse(PointsHash(AProfileOf(10)) = PointsHash(AProfileOf(10.000001)));
end;

procedure TModelHistoryJsonTest.AMeasurementIsNotPartOfTheModel;
var
    A, B: TProjectDocument;
begin
    A := ACapture(1.5, AProfileOf(10));
    B := A;
    B.RFactor := 0.0500000001;
    B.Statistics.ChiSquare := 12.5;
    AssertEquals('measured again after a restore, still the same model',
        ModelContentHash(A, 'p'), ModelContentHash(B, 'p'));
end;

procedure TModelHistoryJsonTest.NorIsWhenOrWhereItWasRecorded;
var
    A, B: TProjectDocument;
begin
    A := ACapture(1.5, AProfileOf(10));
    B := A;
    B.ModifiedUtc := '2027-01-01T00:00:00Z';
    B.Provenance.SourcePath := '/elsewhere.dat';
    B.HasUi := False;
    AssertEquals(ModelContentHash(A, 'p'), ModelContentHash(B, 'p'));
end;

procedure TModelHistoryJsonTest.AValueIsPartOfTheModel;
begin
    AssertFalse(ModelContentHash(ACapture(1.5, AProfileOf(10)), 'p') =
        ModelContentHash(ACapture(1.5000001, AProfileOf(10)), 'p'));
end;

procedure TModelHistoryJsonTest.SoIsTheProfileItWasFittedTo;
begin
    AssertFalse(ModelContentHash(ACapture(1.5, AProfileOf(10)), 'p1') =
        ModelContentHash(ACapture(1.5, AProfileOf(10)), 'p2'));
end;

{ ---- a new entry ------------------------------------------------------------ }

procedure TModelHistoryJsonTest.ANewEntryNamesItsProfileAndHoldsNoCopy;
var
    E: THistoryEntry;
    Profile: TPointsData;
begin
    E := NewHistoryEntry(ACapture(1.5, AProfileOf(10)), 'a',
        hrkAutomatically, True, Now, Profile);
    AssertEquals('named by its hash', PointsHash(AProfileOf(10)),
        E.ProfileHash);
    AssertEquals('handed over', 10.0, Profile.Y[0], 0);
    AssertEquals('and not kept in the model', 0, Length(E.Model.Profile.X));
    AssertEquals('the run', Ord(hrkAutomatically), Ord(E.RunKind));
    AssertTrue('stopped', E.Stopped);
    AssertEquals('its id', 'a', E.Id);
    AssertTrue('its content', E.ContentHash <> '');
end;

procedure TModelHistoryJsonTest.ANewEntryKeepsNothingOfTheWindow;
var
    E: THistoryEntry;
    Profile: TPointsData;
begin
    //  A MODEL, not a session: making it current must not move the window's
    //  tab or claim another data file.
    E := NewHistoryEntry(ACapture(1.5, AProfileOf(10)), 'a',
        hrkMinimizeDifference, False, Now, Profile);
    AssertFalse('no window context', E.Model.HasUi);
    AssertEquals('no provenance', '', E.Model.Provenance.SourcePath);
    AssertEquals('no stamps', '', E.Model.ModifiedUtc);
    AssertEquals('no parts as read', 0, Length(E.Model.AsRead));
end;

procedure TModelHistoryJsonTest.ANewEntrySaysWhenInUtc;
var
    E: THistoryEntry;
    Profile: TPointsData;
begin
    E := NewHistoryEntry(ACapture(1.5, AProfileOf(10)), 'a',
        hrkMinimizeDifference, False,
        EncodeDate(2026, 10, 2) + EncodeTime(12, 33, 56, 0), Profile);
    AssertEquals('2026-10-02T12:33:56Z', E.CreatedUtc);
end;

initialization
    //  A unit test: parts in memory, no archive and no engine.
    RegisterTest('unit', TModelHistoryJsonTest);
end.
