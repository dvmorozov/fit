// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A frame of a running fit is sent as the intervals that moved.)

WHY. A model of a thousand curves is a frame of twenty megabytes, and a window
polling it twice a second was sent all of it each time: the server needed ten
seconds under load, the client gave up after two and asked again without
advancing, and nothing was drawn. The curves of a frame belong to the fit
intervals that drew them, and each interval carries the revision of the frame it
last changed in - so a poll that names the revision it holds is sent only the
intervals that moved since, and puts them over what it holds.

WHAT IS PINNED HERE is the rule on plain records, where it can be exhaustive:
which curves a poll is sent, how the rest is put back, what is refused, what the
text cache keeps apart, and what the progress log records of each interval's
revision. Through the routes the window polls it is pinned in testcase_parallel_fit
(the server) and testcase_client_fit_progress (the client).
}
unit testcase_progress_changes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_points_json, fit_progress_json, fit_progress_log, difference_structure;

type
    TProgressChangesTest = class(TTestCase)
    private
        FCache: TSnapshotTextCache;
        { Five curves in three groups, as of revision 5: group 0 (a, b) last
          changed at 3, group 1 (c) and group 2 (d, e) at 5. }
        function Frame: TFitProgressSnapshot;
        function Curve(const AId: string; AGroup: longint): TFitProgressCurve;
        function Ids(const ASnapshot: TFitProgressSnapshot): string;
        function WithGroup(const ASnapshot: TFitProgressSnapshot;
            AGroup, ARevision: longint; const AIds: array of string):
            TFitProgressSnapshot;
        function ReportOf(const ASnapshot: TFitProgressSnapshot): TFitProgressReport;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Which curves a poll is sent.
        procedure APollHoldingAnOlderFrameIsSentOnlyTheGroupsThatChangedSince;
        procedure APollFromNothingIsSentTheWholeFrame;
        procedure AFrameInWhichEverythingMovedIsSentWhole;
        procedure AFrameThatIsNotGroupedIsAlwaysSentWhole;
        //  Putting them back.
        procedure TheGroupsThatMovedAreLaidOverTheOnesTheReaderHolds;
        procedure AGroupThatMovedToNoCurvesEmptiesItsPlace;
        procedure AGroupThatIsGoneIsDropped;
        procedure AReaderThatDoesNotHoldWhatIsUnchangedIsToldSo;
        procedure AFrameThatIsWholeIsLeftAlone;
        //  The wire.
        procedure APartialFrameRoundTripsWithItsGroupsAndItsBase;
        procedure AFrameThatIsNotGroupedCarriesNoGroupFields;
        procedure ThePartialReplyIsSmallerThanTheWholeOne;
        procedure TheCacheKeepsTheWholeFrameAndTheChangesApart;
        //  What the log records.
        procedure AGroupKeepsTheRevisionOfTheFrameItLastMovedIn;
        procedure AFrameWithNoGroupFlagsIsNotGrouped;
        procedure AnOperationStartForgetsTheGroups;
    end;

implementation

function TProgressChangesTest.Curve(const AId: string;
    AGroup: longint): TFitProgressCurve;
begin
    Result := Default(TFitProgressCurve);
    Result.Id := AId;
    Result.Group := AGroup;
    SetLength(Result.Points.X, 3);
    SetLength(Result.Points.Y, 3);
    Result.Points.X[1] := 1;
    Result.Points.Y[1] := 2;
end;

function TProgressChangesTest.Frame: TFitProgressSnapshot;
begin
    Result := Default(TFitProgressSnapshot);
    Result.Seq := 4;
    Result.Revision := 5;
    SetLength(Result.ComputedProfile.X, 2);
    SetLength(Result.ComputedProfile.Y, 2);
    SetLength(Result.Curves, 5);
    Result.Curves[0] := Curve('a', 0);
    Result.Curves[1] := Curve('b', 0);
    Result.Curves[2] := Curve('c', 1);
    Result.Curves[3] := Curve('d', 2);
    Result.Curves[4] := Curve('e', 2);
    SetLength(Result.Groups, 3);
    Result.Groups[0].Id := 0;
    Result.Groups[0].Revision := 3;
    Result.Groups[0].Count := 2;
    Result.Groups[1].Id := 1;
    Result.Groups[1].Revision := 5;
    Result.Groups[1].Count := 1;
    Result.Groups[2].Id := 2;
    Result.Groups[2].Revision := 5;
    Result.Groups[2].Count := 2;
end;

{ THE FRAME with group AGroup changed to hold AIds at ARevision, and the frame's
  own revision moved to it. }
function TProgressChangesTest.WithGroup(const ASnapshot: TFitProgressSnapshot;
    AGroup, ARevision: longint; const AIds: array of string):
    TFitProgressSnapshot;
var
    i, k: longint;
    Curves: TFitProgressCurves;
begin
    Result := ASnapshot;
    Result.Revision := ARevision;
    Curves := nil;
    //  The group keeps its place in the order the curves are listed in.
    for i := 0 to High(ASnapshot.Groups) do
    begin
        if ASnapshot.Groups[i].Id = AGroup then
        begin
            for k := 0 to High(AIds) do
            begin
                SetLength(Curves, Length(Curves) + 1);
                Curves[High(Curves)] := Curve(AIds[k], AGroup);
            end;
            Result.Groups[i].Revision := ARevision;
            Result.Groups[i].Count := Length(AIds);
        end
        else
            for k := 0 to High(ASnapshot.Curves) do
                if ASnapshot.Curves[k].Group = ASnapshot.Groups[i].Id then
                begin
                    SetLength(Curves, Length(Curves) + 1);
                    Curves[High(Curves)] := ASnapshot.Curves[k];
                end;
    end;
    Result.Curves := Curves;
end;

function TProgressChangesTest.Ids(const ASnapshot: TFitProgressSnapshot): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(ASnapshot.Curves) do
        Result := Result + ASnapshot.Curves[i].Id;
end;

function TProgressChangesTest.ReportOf(const ASnapshot: TFitProgressSnapshot):
    TFitProgressReport;
begin
    Result := Default(TFitProgressReport);
    Result.Busy := True;
    Result.NextSeq := 5;
    Result.HasSnapshot := True;
    Result.Snapshot := ASnapshot;
end;

procedure TProgressChangesTest.SetUp;
begin
    FCache := TSnapshotTextCache.Create;
end;

procedure TProgressChangesTest.TearDown;
begin
    FreeAndNil(FCache);
end;

procedure TProgressChangesTest.APollHoldingAnOlderFrameIsSentOnlyTheGroupsThatChangedSince;
var
    S: TFitProgressSnapshot;
begin
    //  The poll holds revision 4: group 0 (revision 3) is what it has already.
    S := SnapshotChangesSince(Frame, 4);
    AssertTrue('partial', S.Partial);
    AssertEquals('relative to the frame it holds', 4, S.BaseRevision);
    AssertEquals('the curves of groups 1 and 2', 'cde', Ids(S));
    AssertEquals('every group still listed', 3, Length(S.Groups));
    AssertEquals('the frame is this one', 5, S.Revision);
end;

procedure TProgressChangesTest.APollFromNothingIsSentTheWholeFrame;
var
    S: TFitProgressSnapshot;
begin
    S := SnapshotChangesSince(Frame, 0);
    AssertFalse(S.Partial);
    AssertEquals('abcde', Ids(S));
end;

procedure TProgressChangesTest.AFrameInWhichEverythingMovedIsSentWhole;
var
    S: TFitProgressSnapshot;
begin
    //  Holding revision 2, every group changed after it.
    S := SnapshotChangesSince(Frame, 2);
    AssertFalse('nothing to leave out', S.Partial);
    AssertEquals('abcde', Ids(S));
end;

procedure TProgressChangesTest.AFrameThatIsNotGroupedIsAlwaysSentWhole;
var
    F, S: TFitProgressSnapshot;
begin
    F := Frame;
    F.Groups := nil;
    S := SnapshotChangesSince(F, 4);
    AssertFalse(S.Partial);
    AssertEquals('abcde', Ids(S));
end;

procedure TProgressChangesTest.TheGroupsThatMovedAreLaidOverTheOnesTheReaderHolds;
var
    Held, Sent: TFitProgressSnapshot;
begin
    Held := Frame;
    //  Group 1 moves at revision 7 and now holds c and f; the others do not.
    Sent := SnapshotChangesSince(WithGroup(Frame, 1, 7, ['c', 'f']), 5);
    AssertTrue('only group 1 moved: ' + Ids(Sent), Sent.Partial);
    AssertEquals('cf', Ids(Sent));
    AssertTrue(MergeSnapshotChanges(Held, Sent));
    AssertFalse('whole now', Sent.Partial);
    AssertEquals('in the order the groups are listed', 'abcfde', Ids(Sent));
    AssertEquals('and the revision is the frame''s', 7, Sent.Revision);
end;

procedure TProgressChangesTest.AGroupThatMovedToNoCurvesEmptiesItsPlace;
var
    Held, Sent: TFitProgressSnapshot;
begin
    Held := Frame;
    //  Every curve of group 2 was removed: it changed, and holds none.
    Sent := SnapshotChangesSince(WithGroup(Frame, 2, 7, []), 5);
    AssertTrue(Sent.Partial);
    AssertEquals('nothing of it is sent', '', Ids(Sent));
    AssertTrue(MergeSnapshotChanges(Held, Sent));
    AssertEquals('and what the reader held of it is gone', 'abc', Ids(Sent));
end;

procedure TProgressChangesTest.AGroupThatIsGoneIsDropped;
var
    Held, Sent: TFitProgressSnapshot;
begin
    Held := Frame;
    Sent := WithGroup(Frame, 1, 7, ['c']);
    //  A later frame has only two groups.
    SetLength(Sent.Groups, 2);
    Sent := SnapshotChangesSince(Sent, 5);
    AssertTrue(MergeSnapshotChanges(Held, Sent));
    AssertEquals('group 2 is no longer in the frame', 'abc', Ids(Sent));
end;

procedure TProgressChangesTest.AReaderThatDoesNotHoldWhatIsUnchangedIsToldSo;
var
    Held, Sent: TFitProgressSnapshot;
begin
    Sent := SnapshotChangesSince(WithGroup(Frame, 1, 7, ['c', 'f']), 5);
    //  The reader holds only group 0's first curve: it is not what the server
    //  thinks it holds.
    Held := Frame;
    SetLength(Held.Curves, 1);
    AssertFalse(MergeSnapshotChanges(Held, Sent));
    AssertTrue('and the frame is left as it was', Sent.Partial);
    AssertEquals('cf', Ids(Sent));
end;

procedure TProgressChangesTest.AFrameThatIsWholeIsLeftAlone;
var
    Held, Sent: TFitProgressSnapshot;
begin
    Held := Frame;
    Sent := Frame;
    AssertTrue(MergeSnapshotChanges(Held, Sent));
    AssertEquals('abcde', Ids(Sent));
end;

procedure TProgressChangesTest.APartialFrameRoundTripsWithItsGroupsAndItsBase;
var
    R, Back: TFitProgressReport;
    O: TJSONObject;
begin
    R := ReportOf(SnapshotChangesSince(Frame, 4));
    O := FitProgressToJson(R);
    try
        AssertTrue(FitProgressFromJson(O, Back));
    finally
        O.Free;
    end;
    AssertTrue(Back.HasSnapshot);
    AssertTrue('partial', Back.Snapshot.Partial);
    AssertEquals('base', 4, Back.Snapshot.BaseRevision);
    AssertEquals('groups', 3, Length(Back.Snapshot.Groups));
    AssertEquals('group 0 revision', 3, Back.Snapshot.Groups[0].Revision);
    AssertEquals('group 0 count', 2, Back.Snapshot.Groups[0].Count);
    AssertEquals('only the moved curves', 'cde', Ids(Back.Snapshot));
    AssertEquals('each names its group', 2, Back.Snapshot.Curves[1].Group);
end;

procedure TProgressChangesTest.AFrameThatIsNotGroupedCarriesNoGroupFields;
var
    F: TFitProgressSnapshot;
    Text: string;
begin
    F := Frame;
    F.Groups := nil;
    Text := FitProgressToJson(ReportOf(F)).AsJSON;
    AssertEquals('no groups', 0, Pos('"groups"', Text));
    AssertEquals('no group on a curve', 0, Pos('"group"', Text));
    AssertEquals('not partial', 0, Pos('"partial"', Text));
end;

procedure TProgressChangesTest.ThePartialReplyIsSmallerThanTheWholeOne;
var
    Whole, Partial: string;
begin
    Whole := FitProgressToResponse(ReportOf(Frame), FCache, 0);
    Partial := FitProgressToResponse(ReportOf(Frame), FCache, 4);
    AssertTrue(Format('%d bytes against %d', [Length(Partial), Length(Whole)]),
        Length(Partial) < Length(Whole));
end;

procedure TProgressChangesTest.TheCacheKeepsTheWholeFrameAndTheChangesApart;
var
    Whole, Partial, WholeAgain, PartialAgain: string;
begin
    //  The same revision, asked for two ways: one text must not answer both.
    Whole := FitProgressToResponse(ReportOf(Frame), FCache, 0);
    Partial := FitProgressToResponse(ReportOf(Frame), FCache, 4);
    AssertTrue('different texts', Whole <> Partial);
    PartialAgain := FitProgressToResponse(ReportOf(Frame), FCache, 4);
    AssertEquals('the changes, again, from the cache', Partial, PartialAgain);
    AssertEquals('turned into text once for each of the two', 2,
        FCache.Serializations);
    WholeAgain := FitProgressToResponse(ReportOf(Frame), FCache, 0);
    AssertEquals('the whole frame is still the whole frame', Whole, WholeAgain);
end;

procedure TProgressChangesTest.AGroupKeepsTheRevisionOfTheFrameItLastMovedIn;
var
    Log: TFitProgressLog;
    Curves: TFitProgressCurves;
    Empty: TPointsData;
    Report: TFitProgressReport;
    Flags: TFrameGroupFlags;
begin
    Log := TFitProgressLog.Create;
    try
        Log.Start(0);
        Empty := Default(TPointsData);
        SetLength(Curves, 3);
        Curves[0] := Curve('a', 0);
        Curves[1] := Curve('b', 1);
        Curves[2] := Curve('c', 1);
        SetLength(Flags, 2);
        //  The first frame: both groups are new.
        Flags[0] := False;
        Flags[1] := False;
        Log.StoreSnapshot(1, Empty, Empty, Curves, Empty,
            Default(TDifferenceStructure), False, Flags);
        //  The second: only group 1 moved.
        Flags[1] := True;
        Log.StoreSnapshot(2, Empty, Empty, Curves, Empty,
            Default(TDifferenceStructure), False, Flags);
        Report := Log.Report(2, 0, True, 0);
        AssertTrue(Report.HasSnapshot);
        AssertEquals('two groups', 2, Length(Report.Snapshot.Groups));
        AssertEquals('group 0 stayed at the first frame', 1,
            Report.Snapshot.Groups[0].Revision);
        AssertEquals('group 1 moved in the second', 2,
            Report.Snapshot.Groups[1].Revision);
        AssertEquals('group 0 holds one curve', 1, Report.Snapshot.Groups[0].Count);
        AssertEquals('group 1 holds two', 2, Report.Snapshot.Groups[1].Count);
    finally
        Log.Free;
    end;
end;

procedure TProgressChangesTest.AFrameWithNoGroupFlagsIsNotGrouped;
var
    Log: TFitProgressLog;
    Curves: TFitProgressCurves;
    Empty: TPointsData;
    Report: TFitProgressReport;
begin
    Log := TFitProgressLog.Create;
    try
        Log.Start(0);
        Empty := Default(TPointsData);
        SetLength(Curves, 1);
        Curves[0] := Curve('a', 0);
        Log.StoreSnapshot(1, Empty, Empty, Curves, Empty,
            Default(TDifferenceStructure));
        Report := Log.Report(1, 0, True, 0);
        AssertTrue(Report.HasSnapshot);
        AssertEquals('no groups', 0, Length(Report.Snapshot.Groups));
    finally
        Log.Free;
    end;
end;

procedure TProgressChangesTest.AnOperationStartForgetsTheGroups;
var
    Log: TFitProgressLog;
    Curves: TFitProgressCurves;
    Empty: TPointsData;
    Report: TFitProgressReport;
    Flags: TFrameGroupFlags;
    First: longint;
begin
    Log := TFitProgressLog.Create;
    try
        Log.Start(0);
        Empty := Default(TPointsData);
        SetLength(Curves, 1);
        Curves[0] := Curve('a', 0);
        SetLength(Flags, 1);
        Log.StoreSnapshot(1, Empty, Empty, Curves, Empty,
            Default(TDifferenceStructure), False, Flags);
        First := Log.Report(1, 0, True, 0).Snapshot.Groups[0].Revision;
        //  A new operation: its group 0 is not the last one's, whatever the
        //  flag says.
        Log.Finish(2);
        Log.Start(3);
        Log.StoreSnapshot(4, Empty, Empty, Curves, Empty,
            Default(TDifferenceStructure), False, Flags);
        Report := Log.Report(4, 0, True, 0);
        AssertTrue('a later revision than the last operation''s frame',
            Report.Snapshot.Groups[0].Revision > First);
    finally
        Log.Free;
    end;
end;

initialization
    //  Plain records and the log: nothing outside this process.
    RegisterTest('unit', TProgressChangesTest);
end.
