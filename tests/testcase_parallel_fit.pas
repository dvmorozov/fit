// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Fit intervals fitted side by side, through the verb the window calls.)

FIT INTERVALS ARE INDEPENDENT BY CONSTRUCTION, so they are fitted at once on a
bounded pool (docs/internal/fit-performance.md, "parallel fit intervals"; job_pool). What that
must not change is the answer: a fit on several workers reaches exactly the
model the same fit reaches on one, bit for bit, because no interval reads
another's state while it fits. And what it must change is the time - which is
why the server says how many workers the last fit used, and a test can tell a
parallel run from one that only claims to be.

`fitThreads` is the setting: 0, the default, means one worker per processor
(never more than the intervals); 1 is the old loop exactly.
}
unit testcase_parallel_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    fit_rest_api, test_curve_types, fit_progress_json, int_fit_service;

type
    TIntList = array of longint;

    { Runs one request on a thread of its own, as the server's connection
      threads do: the test polls the progress while it is answered. }
    TRequestRunner = class(TThread)
    private
        FApi: TFitRestApi;
        FPath: string;
    protected
        procedure Execute; override;
    public
        Code: longint;
        constructor Create(AApi: TFitRestApi; const APath: string);
    end;

    TParallelFitTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        function Num(const AValue: double): string;
        { Three peaks, a pick on each, an interval round each, fitted on at
          most AThreads workers - watched, with AWatched, by a client asking
          for frames, as an animating window does. Answers the problem. }
        function FitThreePeaks(AThreads: longint;
            AWatched: boolean = False): longint;
        { The three peaks picked, then run automatically - the intervals and
          the reduction the engine's own - on at most AThreads. Picked, because
          with nothing placed the run seeds a curve at every sample, which
          takes minutes and tests nothing more here. }
        function AutomaticThreePeaks(AThreads: longint): longint;
        function NewThreePeaks(AThreads: longint): longint;
        function FittedValues(AId: longint): TStringList;
        procedure ReadFrame(const AReply: string; out HasFrame, Partial: boolean;
            out Revision, Base: longint;
            out CurveGroups, GroupRevs, GroupCounts: TIntList);
        function WorkersUsed(AId: longint): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure SideBySideReachesExactlyTheModelOneByOneDoes;
        procedure OneThreadIsTheOldLoop;
        procedure NoMoreWorkersThanIntervals;
        procedure AnAutomaticRunSideBySideReachesTheModelOneByOneDoes;
        //  A frame drawn by a worker carries the intervals and the strip,
        //  from the copy taken as the step was set up.
        procedure AFrameOfASideBySideFitCarriesItsIntervalsAndItsStrip;
        //  Several pollers of one frame cost one serialisation.
        procedure PollersOfOneFrameShareOneSerialisation;
        //  A window that refreshes once a second is not fed a frame more often.
        procedure AWatchedFitMakesAFrameNoMoreOftenThanOnceASecond;
        //  A reply carries the intervals that moved since the poll's frame, each
        //  named with the revision it last changed at - never the whole model
        //  again (a reply of twenty megabytes froze the window).
        procedure AWatchedFitRepliesNameTheirIntervalsAndCarryOnlyThoseThatMoved;
        //  A fault on a worker is the run's failure, answered with what it
        //  was - never a dead server, a lost reply or a problem left busy.
        procedure AFaultOnAWorkerIsAnsweredAsAFailedRun;
    end;

implementation

constructor TRequestRunner.Create(AApi: TFitRestApi; const APath: string);
begin
    FApi := AApi;
    FPath := APath;
    inherited Create(False);
end;

procedure TRequestRunner.Execute;
var
    Resp: string;
begin
    FApi.Handle('POST', FPath, '', Code, Resp);
end;

procedure TParallelFitTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TParallelFitTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TParallelFitTest.Call(const M, P, B: string;
    out Code: longint): TJSONObject;
var
    Resp: string;
    D: TJSONData;
begin
    FApi.Handle(M, P, B, Code, Resp);
    D := GetJSON(Resp);
    if D is TJSONObject then
        Result := TJSONObject(D)
    else
    begin
        D.Free;
        Result := TJSONObject.Create;
    end;
end;

function TParallelFitTest.Num(const AValue: double): string;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStrF(AValue, ffGeneral, 17, 0, FS);
end;

const
    CENTRES: array[0..2] of double = (10, 30, 50);
    { A pick one sample off a peak, at about the profile's height there: a pick
      seeds its curve's amplitude, and one at zero starts a flat curve that no
      engine moves - so two runs would agree whether or not they fitted. }
    PICK_Y = '90';

function TParallelFitTest.NewThreePeaks(AThreads: longint): longint;
var
    Code, i, k: longint;
    R: TJSONObject;
    XS, YS: string;
    Y: double;
begin
    R := Call('POST', '/problems', '', Code);
    try
        Result := R.Get('id', 0);
    finally
        R.Free;
    end;
    XS := '';
    YS := '';
    for i := 0 to 60 do
    begin
        Y := 0;
        for k := 0 to 2 do
            //  A little structure the seeds do not have, so each fit has work
            //  - under the peaks only, so the stretches between them are empty
            //  and the automatic interval search finds three.
            Y := Y + (100 + 3 * Sin(i * 0.7)) * Exp(-Sqr((i - CENTRES[k]) / 3));
        //  A floor that rises every other sample: the peak search walks down
        //  each peak while the values fall, so on a smooth profile the three
        //  walks would meet between the peaks and make one interval.
        Y := Y + 0.1 * (i mod 2);
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + Num(Y);
    end;
    R := Call('PUT', Format('/problems/%d/profile', [Result]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code);
    R.Free;
    //  Asked for by name: the selection is process-wide, and a suite that
    //  fits Gaussians must not depend on which suite ran before it.
    R := Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"fitThreads":%d,"curveType":"%s"}',
        [AThreads, GUIDToString(TTestGaussPointsSet.GetCurveTypeId)]), Code);
    try
        AssertEquals('the thread count is a setting (' + R.Get('error', '') +
            ')', 200, Code);
    finally
        R.Free;
    end;
end;

function TParallelFitTest.AutomaticThreePeaks(AThreads: longint): longint;
var
    Code, k: longint;
    R: TJSONObject;
begin
    Result := NewThreePeaks(AThreads);
    for k := 0 to 2 do
    begin
        R := Call('POST', Format('/problems/%d/points/positions', [Result]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 1), PICK_Y]), Code);
        R.Free;
    end;
    R := Call('POST', Format('/problems/%d/actions/do-all-automatically',
        [Result]), '', Code);
    try
        AssertEquals('run automatically (' + R.Get('error', '') + ')', 200,
            Code);
    finally
        R.Free;
    end;
end;

function TParallelFitTest.FitThreePeaks(AThreads: longint;
    AWatched: boolean): longint;
var
    Code, k: longint;
    R: TJSONObject;
    BX, BY: string;
begin
    Result := NewThreePeaks(AThreads);
    BX := '';
    BY := '';
    for k := 0 to 2 do
    begin
        if k > 0 then
        begin
            BX := BX + ',';
            BY := BY + ',';
        end;
        BX := BX + Num(CENTRES[k] - 9) + ',' + Num(CENTRES[k] + 9);
        BY := BY + '0,0';
        R := Call('POST', Format('/problems/%d/points/positions', [Result]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 1), PICK_Y]), Code);
        R.Free;
    end;
    R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        Format('{"x":[%s],"y":[%s]}', [BX, BY]), Code);
    R.Free;
    AssertEquals('the intervals are set', 200, Code);
    if AWatched then
        Call('GET', Format('/problems/%d/progress?snapshot=1', [Result]), '',
            Code).Free;
    R := Call('POST', Format('/problems/%d/actions/minimize-difference',
        [Result]), '', Code);
    try
        AssertEquals('fitted (' + R.Get('error', '') + ')', 200, Code);
    finally
        R.Free;
    end;
end;

procedure TParallelFitTest.AFrameOfASideBySideFitCarriesItsIntervalsAndItsStrip;
var
    Id, Code, i, k: longint;
    R: TJSONObject;
    Report: TFitProgressReport;
    Inside: boolean;
begin
    Id := FitThreePeaks(3, True);
    AssertEquals('side by side', 3, WorkersUsed(Id));
    R := Call('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '',
        Code);
    try
        AssertTrue('the report reads', FitProgressFromJson(R, Report));
    finally
        R.Free;
    end;
    AssertTrue('a frame', Report.HasSnapshot);
    AssertEquals('of the three intervals', 6,
        Length(Report.Snapshot.FitBounds.X));
    for k := 0 to 2 do
    begin
        AssertEquals('from', CENTRES[k] - 9, Report.Snapshot.FitBounds.X[2 * k], 0);
        AssertEquals('to', CENTRES[k] + 9,
            Report.Snapshot.FitBounds.X[2 * k + 1], 0);
    end;
    AssertTrue('and the strip', Report.Snapshot.HasStructure);
    for i := 0 to High(Report.Snapshot.Structure.X) do
    begin
        Inside := False;
        for k := 0 to 2 do
            if Abs(Report.Snapshot.Structure.X[i] - CENTRES[k]) <= 9 then
                Inside := True;
        AssertTrue(Format('strip sample %g inside an interval',
            [Report.Snapshot.Structure.X[i]]), Inside);
    end;
end;

procedure TParallelFitTest.PollersOfOneFrameShareOneSerialisation;
var
    Id, Code, i: longint;
    Before: longint;
    A, B: string;
begin
    Id := FitThreePeaks(3, True);
    Before := FApi.Sessions.Find(Id).SnapshotCache.Serializations;
    //  Through the route the window polls, three times, the frame unchanged:
    //  the fit is over, so nothing makes another.
    A := '';
    for i := 1 to 3 do
    begin
        FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '',
            Code, B);
        AssertEquals(200, Code);
        if i = 1 then
            A := B;
        AssertEquals('every poll reads the same frame', A, B);
    end;
    AssertEquals('turned into text once for the three',
        Before + 1, FApi.Sessions.Find(Id).SnapshotCache.Serializations);
end;

procedure TParallelFitTest.AWatchedFitMakesAFrameNoMoreOftenThanOnceASecond;
var
    Id, Code, k, x, Seen: longint;
    Runner: TRequestRunner;
    R: TJSONObject;
    Report: TFitProgressReport;
    Resp: string;
    Revisions: array of longint;
    BX, BY: string;
    From: QWord;
    Seconds: double;
    Known, Stopped: boolean;
    i: longint;
begin
    //  A CURVE ON EVERY SAMPLE of each interval, as the automatic run seeds,
    //  so the fit lasts far longer than the few seconds it is watched for -
    //  and is stopped, as a user does, once it has been.
    Id := NewThreePeaks(3);
    BX := '';
    BY := '';
    for k := 0 to 2 do
    begin
        if k > 0 then
        begin
            BX := BX + ',';
            BY := BY + ',';
        end;
        BX := BX + Num(CENTRES[k] - 9) + ',' + Num(CENTRES[k] + 9);
        BY := BY + '0,0';
        for x := Round(CENTRES[k]) - 9 to Round(CENTRES[k]) + 9 do
            Call('POST', Format('/problems/%d/points/positions', [Id]),
                Format('{"x":%d,"y":%s}', [x, PICK_Y]), Code).Free;
    end;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        Format('{"x":[%s],"y":[%s]}', [BX, BY]), Code).Free;
    AssertEquals('the intervals are set', 200, Code);
    //  Somebody is watching, as an animating window is.
    Call('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '', Code).Free;

    Revisions := nil;
    Stopped := False;
    From := GetTickCount64;
    Runner := TRequestRunner.Create(FApi,
        Format('/problems/%d/actions/minimize-difference', [Id]));
    try
        while not Runner.Finished do
        begin
            if (not Stopped) and (GetTickCount64 - From >= 4000) then
            begin
                Call('POST', Format('/problems/%d/actions/stop', [Id]), '',
                    Code).Free;
                Stopped := True;
            end;
            FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1', [Id]),
                '', Code, Resp);
            R := TJSONObject(GetJSON(Resp));
            try
                if FitProgressFromJson(R, Report) and Report.HasSnapshot then
                begin
                    Known := False;
                    for i := 0 to High(Revisions) do
                        if Revisions[i] = Report.Snapshot.Revision then
                            Known := True;
                    if not Known then
                    begin
                        SetLength(Revisions, Length(Revisions) + 1);
                        Revisions[High(Revisions)] := Report.Snapshot.Revision;
                    end;
                end;
            finally
                R.Free;
            end;
            Sleep(40);
        end;
        Runner.WaitFor;
        AssertEquals('fitted', 200, Runner.Code);
    finally
        Runner.Free;
    end;
    Seconds := (GetTickCount64 - From) / 1000;
    Seen := Length(Revisions);
    //  A fit over before it was stopped would pass whatever the cadence.
    AssertTrue(Format('the fit lasted %.1f s: too light a model to judge a ' +
        'cadence by', [Seconds]), Stopped);
    AssertTrue(Format('%d frames in %.1f s of a fit watched by a poll every ' +
        '40 ms', [Seen, Seconds]), Seen <= Seconds + 2);
    AssertTrue('and some frames were made', Seen >= 1);
end;

{ What a reply's frame carries, read off the wire as a client reads it: the
  ids of its curves, the intervals it names with the revision each last
  changed at and how many curves each holds, and whether it is only the changes
  since the frame the poll named. }
procedure TParallelFitTest.ReadFrame(const AReply: string;
    out HasFrame, Partial: boolean; out Revision, Base: longint;
    out CurveGroups, GroupRevs, GroupCounts: TIntList);
var
    R, Snap, C: TJSONObject;
    D: TJSONData;
    Curves, Groups: TJSONArray;
    i: longint;
begin
    HasFrame := False;
    Partial := False;
    Revision := 0;
    Base := 0;
    CurveGroups := nil;
    GroupRevs := nil;
    GroupCounts := nil;
    D := GetJSON(AReply);
    try
        R := TJSONObject(D);
        Snap := TJSONObject(R.Find('snapshot'));
        if Snap = nil then
            Exit;
        HasFrame := True;
        Revision := Snap.Get('revision', 0);
        Partial := Snap.Get('partial', False);
        Base := Snap.Get('base', 0);
        Curves := TJSONArray(Snap.Find('curves'));
        SetLength(CurveGroups, Curves.Count);
        for i := 0 to Curves.Count - 1 do
        begin
            C := TJSONObject(Curves.Items[i]);
            CurveGroups[i] := C.Get('group', -1);
        end;
        D := Snap.Find('groups');
        if D is TJSONArray then
        begin
            Groups := TJSONArray(D);
            SetLength(GroupRevs, Groups.Count);
            SetLength(GroupCounts, Groups.Count);
            for i := 0 to Groups.Count - 1 do
            begin
                GroupRevs[i] := TJSONObject(Groups.Items[i]).Get('rev', 0);
                GroupCounts[i] := TJSONObject(Groups.Items[i]).Get('n', 0);
            end;
        end;
    finally
        R.Free;
    end;
end;

procedure TParallelFitTest.AWatchedFitRepliesNameTheirIntervalsAndCarryOnlyThoseThatMoved;
var
    Id, Code, k, x, i, g, Expected, Polls: longint;
    Runner: TRequestRunner;
    Resp: string;
    BX, BY: string;
    From: QWord;
    Stopped, Has, Partial: boolean;
    Revision, Base, Held: longint;
    CurveGroups, GroupRevs, GroupCounts: TIntList;
    Framed: longint;
begin
    //  The cadence test's model: a curve on every sample of three intervals, so
    //  the fit outlasts the few seconds it is watched for.
    Id := NewThreePeaks(3);
    BX := '';
    BY := '';
    for k := 0 to 2 do
    begin
        if k > 0 then
        begin
            BX := BX + ',';
            BY := BY + ',';
        end;
        BX := BX + Num(CENTRES[k] - 9) + ',' + Num(CENTRES[k] + 9);
        BY := BY + '0,0';
        for x := Round(CENTRES[k]) - 9 to Round(CENTRES[k]) + 9 do
            Call('POST', Format('/problems/%d/points/positions', [Id]),
                Format('{"x":%d,"y":%s}', [x, PICK_Y]), Code).Free;
    end;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        Format('{"x":[%s],"y":[%s]}', [BX, BY]), Code).Free;
    Call('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '', Code).Free;

    Held := 0;
    Framed := 0;
    Stopped := False;
    From := GetTickCount64;
    Runner := TRequestRunner.Create(FApi,
        Format('/problems/%d/actions/minimize-difference', [Id]));
    try
        Polls := 0;
        while not Runner.Finished do
        begin
            if (not Stopped) and (GetTickCount64 - From >= 4000) then
            begin
                Call('POST', Format('/problems/%d/actions/stop', [Id]), '',
                    Code).Free;
                Stopped := True;
            end;
            //  AS THE WINDOW POLLS: naming the frame it holds.
            FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1' +
                '&snapshotSince=%d', [Id, Held]), '', Code, Resp);
            Inc(Polls);
            ReadFrame(Resp, Has, Partial, Revision, Base, CurveGroups,
                GroupRevs, GroupCounts);
            //  The frame taken before the fit starts is not grouped, and is
            //  sent whole; the builder's frames are.
            if Has and (Length(GroupRevs) = 0) then
            begin
                AssertFalse('a frame with no groups is whole', Partial);
                Held := Revision;
            end
            else if Has then
            begin
                Inc(Framed);
                //  WHAT IT CARRIES IS WHAT MOVED: the curves of the intervals
                //  whose revision is later than the frame the poll named - all
                //  of them when the reply is not partial.
                Expected := 0;
                for g := 0 to High(GroupRevs) do
                    if (not Partial) or (GroupRevs[g] > Base) then
                        Inc(Expected, GroupCounts[g]);
                AssertEquals('the curves it carries', Expected,
                    Length(CurveGroups));
                for i := 0 to High(CurveGroups) do
                    AssertTrue('each curve names an interval of the frame',
                        (CurveGroups[i] >= 0) and
                        (CurveGroups[i] <= High(GroupRevs)));
                if Partial then
                    AssertEquals('a partial frame is relative to the one the ' +
                        'poll named', Held, Base);
                Held := Revision;
            end;
            Sleep(40);
        end;
        Runner.WaitFor;
    finally
        Runner.Free;
    end;
    AssertTrue('the watched fit made frames of named intervals', Framed >= 1);
    //  A POLL FROM NOTHING GETS EVERYTHING, whatever the intervals' revisions.
    FApi.Handle('GET', Format('/problems/%d/progress?snapshot=1', [Id]), '',
        Code, Resp);
    ReadFrame(Resp, Has, Partial, Revision, Base, CurveGroups, GroupRevs,
        GroupCounts);
    AssertTrue('the last frame is still there', Has);
    AssertFalse('and a first poll is not partial', Partial);
    //  Grouped, it carries every curve of every interval it names.
    if Length(GroupCounts) > 0 then
    begin
        Expected := 0;
        for g := 0 to High(GroupCounts) do
            Inc(Expected, GroupCounts[g]);
        AssertEquals('every interval''s curves', Expected, Length(CurveGroups));
    end;
end;

function TParallelFitTest.FittedValues(AId: longint): TStringList;
var
    Code, i, j: longint;
    R: TJSONObject;
    Curves, Params: TJSONArray;
    P: TJSONObject;
begin
    Result := TStringList.Create;
    R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    try
        Curves := TJSONArray(R.Find('curves'));
        for i := 0 to Curves.Count - 1 do
        begin
            Params := TJSONArray(TJSONObject(Curves.Items[i]).Find('params'));
            for j := 0 to Params.Count - 1 do
            begin
                P := TJSONObject(Params.Items[j]);
                if P.Get('kind', '') = 'text' then
                    Continue;
                Result.Add(Num(P.Get('value', NaN)));
            end;
        end;
    finally
        R.Free;
    end;
end;

function TParallelFitTest.WorkersUsed(AId: longint): longint;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/stats', [AId]), '', Code);
    try
        Result := R.Get('fitWorkers', -1);
    finally
        R.Free;
    end;
end;

procedure TParallelFitTest.SideBySideReachesExactlyTheModelOneByOneDoes;
var
    One, Three: longint;
    A, B: TStringList;
    i: longint;
begin
    One := FitThreePeaks(1);
    Three := FitThreePeaks(3);
    AssertEquals('one by one', 1, WorkersUsed(One));
    AssertEquals('side by side', 3, WorkersUsed(Three));
    A := FittedValues(One);
    B := FittedValues(Three);
    try
        AssertTrue('a model was fitted', A.Count > 0);
        AssertEquals('as many values', A.Count, B.Count);
        for i := 0 to A.Count - 1 do
            AssertEquals(Format('value %d, bit for bit', [i]), A[i], B[i]);
    finally
        A.Free;
        B.Free;
    end;
end;

procedure TParallelFitTest.OneThreadIsTheOldLoop;
begin
    AssertEquals(1, WorkersUsed(FitThreePeaks(1)));
end;

procedure TParallelFitTest.NoMoreWorkersThanIntervals;
begin
    //  Asked for eight; three intervals are three jobs.
    AssertEquals(3, WorkersUsed(FitThreePeaks(8)));
end;

{ A RUN OF STAGES: the automatic run reduces the curves, then fits what is left,
  and the second stage starts from what the first one collected. So each stage
  must end - collected, remembered - before the next begins, side by side as
  one by one; a stage that only the operation's own end collected would hand the
  last stage a model the reduction never reached. }
procedure TParallelFitTest.AnAutomaticRunSideBySideReachesTheModelOneByOneDoes;
var
    One, Three: longint;
    A, B: TStringList;
    i: longint;
begin
    One := AutomaticThreePeaks(1);
    Three := AutomaticThreePeaks(3);
    AssertEquals('one by one', 1, WorkersUsed(One));
    AssertTrue('side by side', WorkersUsed(Three) > 1);
    A := FittedValues(One);
    B := FittedValues(Three);
    try
        AssertTrue('a model was fitted', A.Count > 0);
        AssertEquals('as many values', A.Count, B.Count);
        for i := 0 to A.Count - 1 do
            AssertEquals(Format('value %d, bit for bit', [i]), A[i], B[i]);
    finally
        A.Free;
        B.Free;
    end;
end;

{ A FAULT ON A WORKER THREAD IS THE RUN'S FAILURE. A worker is a thread of its
  own, and an exception that escapes one is caught by nothing anyone reads - it
  would end the thread, leave the fit "done" over intervals never fitted, and
  answer success. So the pool keeps the first one and raises it on the request
  thread after the join (job_pool), the operation restores the problem's state
  (RecreateMainCalcThread), and the route answers 500 naming it and logs its
  class and message (TFitRestApi.Handle). Asked when the publishing pipeline's
  compute server died with exit code 217 during a run (2026-10-27): that was
  not this - an exception the run time could not raise at all, from a heap
  already overrun (findings.md) - and this held; the test keeps it held. }
procedure TParallelFitTest.AFaultOnAWorkerIsAnsweredAsAFailedRun;
var
    Id, Code, k: longint;
    R: TJSONObject;
    BX, BY: string;
begin
    Id := NewThreePeaks(3);
    BX := '';
    BY := '';
    for k := 0 to 2 do
    begin
        if k > 0 then
        begin
            BX := BX + ',';
            BY := BY + ',';
        end;
        BX := BX + Num(CENTRES[k] - 9) + ',' + Num(CENTRES[k] + 9);
        BY := BY + '0,0';
        Call('POST', Format('/problems/%d/points/positions', [Id]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 1), PICK_Y]),
            Code).Free;
    end;
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        Format('{"x":[%s],"y":[%s]}', [BX, BY]), Code).Free;
    AssertEquals('the intervals are set', 200, Code);

    TestGaussFailsOffTheMainThread := True;
    try
        R := Call('POST', Format('/problems/%d/actions/minimize-difference',
            [Id]), '', Code);
    finally
        TestGaussFailsOffTheMainThread := False;
    end;
    try
        AssertEquals('answered as a fault: ' + R.AsJSON, 500, Code);
        AssertTrue('naming it: ' + R.Get('error', ''),
            Pos(TEST_GAUSS_FAULT, R.Get('error', '')) > 0);
    finally
        R.Free;
    end;
    AssertEquals('on the workers it was given', 3, WorkersUsed(Id));

    R := Call('GET', Format('/problems/%d/state', [Id]), '', Code);
    try
        AssertEquals('the problem still answers', 200, Code);
        AssertTrue('and is not left busy',
            R.Get('state', -1) <> Ord(AsyncOperation));
    finally
        R.Free;
    end;
    //  And fits again, now that nothing fails.
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code).Free;
    AssertEquals('and fits again', 200, Code);
end;

initialization
    RegisterTest('integration', TParallelFitTest);
end.
