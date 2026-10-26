// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The wire form of a fit's live progress.)

WHAT CROSSES. While a fit runs the client polls GET /problems/<id>/progress and
draws what comes back: the loss the engine has reached so far, sample by sample,
and - for a client that animates - a snapshot of the model as it stood at one of
those samples. Both ends read this one codec, so a field the server writes and
the client never reads is a test failure here rather than a chart that silently
stays flat.

WHY THE SNAPSHOT IS FORGIVEN AND THE SAMPLES ARE NOT. Progress is a display: a
reply whose snapshot cannot be read still carries a perfectly good loss curve,
and refusing the whole reply would blank the chart over a decoration. A reply
whose samples cannot be read carries nothing at all, so it is refused.
}
unit testcase_fit_progress_json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_points_json, fit_progress_json;

type
    TFitProgressJsonTest = class(TTestCase)
    private
        function TwoSamples: TFitProgressReport;
        function Decode(const AJson: string; out R: TFitProgressReport): boolean;
        { ACount samples of a loss that only falls, one a second. }
        function FallingSamples(ACount: longint): TFitProgressSamples;
    published
        //  How many of a run's R-factor samples are kept: the server's log,
        //  the window watching the run and the History entry recording it all
        //  keep them by this one rule.
        procedure ACurveWithinItsBoundIsKeptWhole;
        procedure ALongCurveIsKeptWithinItsBoundWithItsEnds;
        procedure ATinyBoundStillKeepsTheFirstAndTheLatest;
        //  A SPIKE OUTLIVES THE THINNING. Every second sample was dropped, so a
        //  one-sample jump in the R-factor went at even odds each time the log
        //  was halved, and a long run's chart lost the spikes it had shown
        //  minutes earlier (reported 2026-10-25).
        procedure ASpikeSurvivesEveryHalving;
        procedure ADipSurvivesEveryHalvingToo;
        //  AND THE EARLY RUN IS NOT THINNED MORE THAN THE LATE ONE. Halving the
        //  whole log each time it filled halved its oldest part again and
        //  again, so the first minutes of a long fit became two points and a
        //  straight line, and every peak in them went (reported 2026-10-26).
        //  Each stretch of time keeps its lowest and its highest sample, as a
        //  bar chart of a squeezed range does.
        procedure ARunFedOneSampleAtATimeKeepsEveryPeakAndDip;
        procedure TheKeptSamplesCoverTheRunEvenly;
        procedure TheEventsRoundTripWithEveryField;
        procedure AReplyFromBeforeEventsHasNone;
        //  What is being minimised, and by what.
        procedure TheObjectiveAndTheEngineSurviveTheRoundTrip;
        procedure TheStageSurvivesTheRoundTrip;
        procedure AndIsLeftOutWhenThereIsNone;
        procedure ASnapshotWithoutCurvesIsRefused;
        procedure SoIsOneWhoseCurveIsNotAnObject;
        procedure SoIsOneWhoseCurvePointsAreMalformed;
        procedure AndNothingAtAllIsNotAReport;
        procedure AndAReplyWithoutThemReadsThemAsUnknown;
        procedure TheSamplesRoundTripInOrder;
        procedure TheOperationStateRoundTrips;
        procedure AReportWithoutASnapshotSendsNone;
        procedure ASnapshotRoundTripsWithEveryCurveAndItsHandle;
        procedure ASnapshotCarriesTheDifferenceProfileToo;
        procedure AReplyWithoutSamplesIsRefused;
        procedure ASampleThatIsNotAnObjectIsRefused;
        procedure ASampleMissingItsValueIsRefused;
        procedure AnUnreadableSnapshotIsDroppedAndTheSamplesKept;
        procedure SomethingThatIsNotJsonIsRefused;
        procedure AnEmptyReplyIsANothingReportNotAFault;
        procedure TheChangedProfileAndItsRevisionRoundTrip;
        procedure AReportWithoutAChangedProfileSendsNone;
    end;

implementation

function TFitProgressJsonTest.TwoSamples: TFitProgressReport;
begin
    Result := Default(TFitProgressReport);
    Result.Busy := True;
    Result.Elapsed := 2.5;
    Result.NextSeq := 8;
    SetLength(Result.Samples, 2);
    Result.Samples[0].Seq := 6;
    Result.Samples[0].Elapsed := 1.25;
    Result.Samples[0].Value := 0.5;
    Result.Samples[1].Seq := 7;
    Result.Samples[1].Elapsed := 2.0;
    Result.Samples[1].Value := 0.125;
end;

function TFitProgressJsonTest.Decode(const AJson: string;
    out R: TFitProgressReport): boolean;
begin
    Result := FitProgressFromJsonString(AJson, R);
end;

function TFitProgressJsonTest.FallingSamples(ACount: longint):
    TFitProgressSamples;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, ACount);
    for i := 0 to ACount - 1 do
    begin
        Result[i].Seq := i;
        Result[i].Elapsed := i;
        Result[i].Value := 1 / (i + 1);
    end;
end;

procedure TFitProgressJsonTest.ACurveWithinItsBoundIsKeptWhole;
var
    Kept: TFitProgressSamples;
    i: longint;
begin
    Kept := BoundedSamples(FallingSamples(10), 10);
    AssertEquals('every sample', 10, Length(Kept));
    for i := 0 to 9 do
        AssertEquals('in order', i, Kept[i].Seq);
    AssertEquals('none at all is none', 0, Length(BoundedSamples(nil, 10)));
    AssertEquals('whatever the bound', 0, Length(BoundedSamples(nil, 1)));
end;

procedure TFitProgressJsonTest.ATinyBoundStillKeepsTheFirstAndTheLatest;
var
    Kept: TFitProgressSamples;
begin
    //  A bound below four is taken as four: room for the two ends and one
    //  stretch's lowest and highest between them.
    Kept := BoundedSamples(FallingSamples(9), 1);
    AssertTrue('within four', Length(Kept) <= 4);
    AssertEquals(0, Kept[0].Seq);
    AssertEquals(8, Kept[High(Kept)].Seq);
end;

{ FallingSamples with sample AAt put at AValue - a spike above the curve or
  a dip below it. }
function WithOutlier(const ASamples: TFitProgressSamples; AAt: longint;
    AValue: double): TFitProgressSamples;
begin
    Result := Copy(ASamples);
    Result[AAt].Value := AValue;
end;

function KeepsSeq(const ASamples: TFitProgressSamples; ASeq: longint): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(ASamples) do
        if ASamples[i].Seq = ASeq then
            Exit(True);
end;

procedure TFitProgressJsonTest.ASpikeSurvivesEveryHalving;
var
    Kept: TFitProgressSamples;
    At: longint;
begin
    //  At every place in the run, odd or even: whichever of a pair the old
    //  rule kept, some halving dropped it.
    for At := 1 to 98 do
    begin
        Kept := BoundedSamples(WithOutlier(FallingSamples(100), At, 5), 8);
        AssertTrue('within the bound', Length(Kept) <= 8);
        AssertTrue('the spike at ' + IntToStr(At) + ' is kept', KeepsSeq(Kept, At));
    end;
end;

procedure TFitProgressJsonTest.ADipSurvivesEveryHalvingToo;
var
    Kept: TFitProgressSamples;
    At: longint;
begin
    for At := 1 to 98 do
    begin
        Kept := BoundedSamples(WithOutlier(FallingSamples(100), At, -5), 8);
        AssertTrue('the dip at ' + IntToStr(At) + ' is kept', KeepsSeq(Kept, At));
    end;
end;

{ A long run as the engine reports it, one sample every hundredth of a
  second: a falling loss with a spike (five times up) and a dip (five times
  down) in every 3001 samples. }
function SpikyRun(ACount: longint): TFitProgressSamples;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, ACount);
    for i := 0 to ACount - 1 do
    begin
        Result[i].Seq := i;
        Result[i].Elapsed := i * 0.01;
        Result[i].Value := 1e-5 * Exp(-i / 20000) + 1e-6;
        if i mod 3001 = 1500 then
            Result[i].Value := Result[i].Value * 5
        else if i mod 3001 = 700 then
            Result[i].Value := Result[i].Value / 5;
    end;
end;

procedure TFitProgressJsonTest.ARunFedOneSampleAtATimeKeepsEveryPeakAndDip;
var
    Fed, Kept: TFitProgressSamples;
    Log: TSampleLog;
    i: longint;
begin
    Fed := SpikyRun(100000);
    SampleLogStart(Log, 4000);
    for i := 0 to High(Fed) do
        SampleLogAdd(Log, Fed[i]);
    Kept := SampleLogSamples(Log);
    AssertTrue('within the bound: ' + IntToStr(Length(Kept)), Length(Kept) <= 4000);
    for i := 0 to High(Fed) do
        if (i mod 3001 = 1500) or (i mod 3001 = 700) then
            AssertTrue('the peak or dip at ' + IntToStr(i) + ' is kept',
                KeepsSeq(Kept, i));
    AssertEquals('the first', 0, Kept[0].Seq);
    AssertEquals('the latest', High(Fed), Kept[High(Kept)].Seq);
    for i := 1 to High(Kept) do
        AssertTrue('in order', Kept[i].Seq > Kept[i - 1].Seq);
end;

procedure TFitProgressJsonTest.TheKeptSamplesCoverTheRunEvenly;
var
    Fed, Kept: TFitProgressSamples;
    Log: TSampleLog;
    i: longint;
    Gap: double;
begin
    //  1000 s in at most 4000 samples: no stretch of the run, the first
    //  minutes included, goes without one for more than a few seconds.
    Fed := SpikyRun(100000);
    SampleLogStart(Log, 4000);
    for i := 0 to High(Fed) do
        SampleLogAdd(Log, Fed[i]);
    Kept := SampleLogSamples(Log);
    for i := 1 to High(Kept) do
    begin
        Gap := Kept[i].Elapsed - Kept[i - 1].Elapsed;
        AssertTrue(Format('%.1f s without a sample after %.1f s', [Gap,
            Kept[i - 1].Elapsed]), Gap <= 4.0);
    end;
end;

procedure TFitProgressJsonTest.ALongCurveIsKeptWithinItsBoundWithItsEnds;
var
    Kept: TFitProgressSamples;
    i: longint;
begin
    //  An hour-long run reported 322 923 improvements, every one of them kept
    //  and saved: a 28 MB History index the project then re-encoded on every
    //  close (the S&P 500 project, 2026-10-24).
    Kept := BoundedSamples(FallingSamples(100001), 4000);
    AssertTrue('within the bound', Length(Kept) <= 4000);
    AssertTrue('and not thinned further than it has to be',
        Length(Kept) > 2000);
    AssertEquals('the first, which the improvement is measured from', 0,
        Kept[0].Seq);
    AssertEquals('the latest, which is what the run reached', 100000,
        Kept[High(Kept)].Seq);
    for i := 1 to High(Kept) do
        AssertTrue('in order', Kept[i].Seq > Kept[i - 1].Seq);
end;

procedure TFitProgressJsonTest.TheEventsRoundTripWithEveryField;
var
    Sent, R: TFitProgressReport;
begin
    Sent := TwoSamples;
    SetLength(Sent.Events, 1);
    Sent.Events[0].Seq := 9;
    Sent.Events[0].Elapsed := 3.5;
    Sent.Events[0].Source := 'framework';
    Sent.Events[0].Section := 'Reduction';
    Sent.Events[0].Topic := 'fitting/how-many-curves';
    Sent.Events[0].Title := 'Gaussian at 12.0 removed';
    Sent.Events[0].Detail := 'BIC 401 with it, 389 without.';
    AssertTrue('decoded', Decode(FitProgressToJsonString(Sent), R));
    AssertEquals(1, Length(R.Events));
    AssertEquals(9, R.Events[0].Seq);
    AssertEquals(3.5, R.Events[0].Elapsed, 1e-12);
    AssertEquals('framework', R.Events[0].Source);
    AssertEquals('Reduction', R.Events[0].Section);
    AssertEquals('fitting/how-many-curves', R.Events[0].Topic);
    AssertEquals('Gaussian at 12.0 removed', R.Events[0].Title);
    AssertEquals('BIC 401 with it, 389 without.', R.Events[0].Detail);
end;

procedure TFitProgressJsonTest.AReplyFromBeforeEventsHasNone;
var
    R: TFitProgressReport;
begin
    AssertTrue('decoded', Decode(FitProgressToJsonString(TwoSamples), R));
    AssertEquals(0, Length(R.Events));
end;

procedure TFitProgressJsonTest.TheSamplesRoundTripInOrder;
var
    R: TFitProgressReport;
begin
    AssertTrue('decoded', Decode(FitProgressToJsonString(TwoSamples), R));
    AssertEquals('both samples', 2, Length(R.Samples));
    AssertEquals('first seq', 6, R.Samples[0].Seq);
    AssertEquals('first time', 1.25, R.Samples[0].Elapsed, 1e-12);
    AssertEquals('first value', 0.5, R.Samples[0].Value, 1e-12);
    AssertEquals('second seq', 7, R.Samples[1].Seq);
    AssertEquals('second value', 0.125, R.Samples[1].Value, 1e-12);
end;

procedure TFitProgressJsonTest.TheOperationStateRoundTrips;
var
    R: TFitProgressReport;
begin
    AssertTrue('decoded', Decode(FitProgressToJsonString(TwoSamples), R));
    AssertTrue('busy', R.Busy);
    AssertEquals('elapsed', 2.5, R.Elapsed, 1e-12);
    //  What the client asks from next time. Losing it would have every poll
    //  resend the whole history.
    AssertEquals('next seq', 8, R.NextSeq);
end;

procedure TFitProgressJsonTest.AReportWithoutASnapshotSendsNone;
var
    O: TJSONObject;
    R: TFitProgressReport;
begin
    //  ABSENT, not empty. Most polls carry no snapshot - nobody animates by
    //  default - and an empty profile in every one of them would read on the
    //  client as a model with no points.
    O := FitProgressToJson(TwoSamples);
    try
        AssertTrue('no snapshot field', O.Find('snapshot') = nil);
    finally
        O.Free;
    end;
    AssertTrue('decoded', Decode(FitProgressToJsonString(TwoSamples), R));
    AssertFalse('and none is read back', R.HasSnapshot);
end;

procedure TFitProgressJsonTest.ASnapshotRoundTripsWithEveryCurveAndItsHandle;
var
    Sent, R: TFitProgressReport;
begin
    Sent := TwoSamples;
    Sent.HasSnapshot := True;
    Sent.Snapshot.Seq := 7;
    Sent.Snapshot.Revision := 11;
    Sent.Snapshot.ComputedProfile.Title := 'computed';
    SetLength(Sent.Snapshot.ComputedProfile.X, 2);
    SetLength(Sent.Snapshot.ComputedProfile.Y, 2);
    Sent.Snapshot.ComputedProfile.X[1] := 3;
    Sent.Snapshot.ComputedProfile.Y[1] := 4;
    SetLength(Sent.Snapshot.Curves, 2);
    Sent.Snapshot.Curves[0].Id := 'AAAA';
    Sent.Snapshot.Curves[0].Points.Title := 'Gaussian';
    SetLength(Sent.Snapshot.Curves[0].Points.X, 1);
    SetLength(Sent.Snapshot.Curves[0].Points.Y, 1);
    Sent.Snapshot.Curves[0].Points.Y[0] := 9;
    Sent.Snapshot.Curves[1].Id := 'BBBB';
    Sent.Snapshot.Curves[1].Points.Title := 'Lorentzian';

    AssertTrue('decoded', Decode(FitProgressToJsonString(Sent), R));
    AssertTrue('a snapshot', R.HasSnapshot);
    AssertEquals('taken at', 7, R.Snapshot.Seq);
    //  What a client asks the next poll from, so it is sent only once.
    AssertEquals('under its revision', 11, R.Snapshot.Revision);
    AssertEquals('the profile', 2, Length(R.Snapshot.ComputedProfile.X));
    AssertEquals('its values', 4.0, R.Snapshot.ComputedProfile.Y[1], 1e-12);
    AssertEquals('every curve', 2, Length(R.Snapshot.Curves));
    //  THE HANDLE, because two curves of one type differ only in where they
    //  sit, and the chart keys what it draws for a curve by it.
    AssertEquals('first handle', 'AAAA', R.Snapshot.Curves[0].Id);
    AssertEquals('first type', 'Gaussian', R.Snapshot.Curves[0].Points.Title);
    AssertEquals('first values', 9.0, R.Snapshot.Curves[0].Points.Y[0], 1e-12);
    AssertEquals('second handle', 'BBBB', R.Snapshot.Curves[1].Id);
    AssertEquals('second type', 'Lorentzian', R.Snapshot.Curves[1].Points.Title);
end;

procedure TFitProgressJsonTest.ASnapshotCarriesTheDifferenceProfileToo;
var
    Sent, R: TFitProgressReport;
begin
    //  An animated chart draws what a finished one draws, and a finished one
    //  draws the difference beside the model.
    Sent := TwoSamples;
    Sent.HasSnapshot := True;
    SetLength(Sent.Snapshot.DeltaProfile.X, 3);
    SetLength(Sent.Snapshot.DeltaProfile.Y, 3);
    Sent.Snapshot.DeltaProfile.Y[2] := -1.5;
    AssertTrue('decoded', Decode(FitProgressToJsonString(Sent), R));
    AssertEquals('the difference', 3, Length(R.Snapshot.DeltaProfile.Y));
    AssertEquals('its values', -1.5, R.Snapshot.DeltaProfile.Y[2], 1e-12);
end;

procedure TFitProgressJsonTest.AReplyWithoutSamplesIsRefused;
var
    R: TFitProgressReport;
begin
    AssertFalse('refused',
        Decode('{"busy":true,"elapsed":1,"nextSeq":0}', R));
end;

procedure TFitProgressJsonTest.ASampleThatIsNotAnObjectIsRefused;
var
    R: TFitProgressReport;
begin
    AssertFalse('refused', Decode(
        '{"busy":true,"elapsed":1,"nextSeq":1,"samples":[3]}', R));
end;

procedure TFitProgressJsonTest.ASampleMissingItsValueIsRefused;
var
    R: TFitProgressReport;
begin
    //  Read as zero it would be a perfect fit on the chart - the one reading of
    //  a missing number that is certainly wrong.
    AssertFalse('refused', Decode(
        '{"busy":true,"elapsed":1,"nextSeq":1,"samples":[{"seq":0,"t":0.5}]}',
        R));
end;

procedure TFitProgressJsonTest.AnUnreadableSnapshotIsDroppedAndTheSamplesKept;
var
    R: TFitProgressReport;
begin
    AssertTrue('the reply is still read', Decode(
        '{"busy":true,"elapsed":1,"nextSeq":1,' +
        '"samples":[{"seq":0,"t":0.5,"value":0.25}],' +
        '"snapshot":{"seq":0,"profile":"garbage"}}', R));
    AssertFalse('without its snapshot', R.HasSnapshot);
    AssertEquals('but with its sample', 1, Length(R.Samples));
end;

procedure TFitProgressJsonTest.SomethingThatIsNotJsonIsRefused;
var
    R: TFitProgressReport;
begin
    AssertFalse('refused', Decode('this is not json', R));
    AssertFalse('nor is an array', Decode('[]', R));
end;

procedure TFitProgressJsonTest.AnEmptyReplyIsANothingReportNotAFault;
var
    R: TFitProgressReport;
begin
    //  What an idle problem answers: nothing running, nothing recorded. It is
    //  an ordinary answer, and must decode as one.
    AssertTrue('decoded', Decode(
        FitProgressToJsonString(Default(TFitProgressReport)), R));
    AssertFalse('not busy', R.Busy);
    AssertEquals('no samples', 0, Length(R.Samples));
end;


procedure TFitProgressJsonTest.TheObjectiveAndTheEngineSurviveTheRoundTrip;
var
    R, Back: TFitProgressReport;
begin
    //  THE HEADER NAMES BOTH, so that what the user is watching is not a
    //  mystery number: the same fit run under a different objective or a
    //  different engine converges differently, and the line above the chart is
    //  where that is visible.
    R := Default(TFitProgressReport);
    R.Busy := True;
    R.LossName := 'Sum of squares';
    R.EngineName := 'Python lmfit';
    AssertTrue(FitProgressFromJsonString(FitProgressToJsonString(R), Back));
    AssertEquals('the objective', 'Sum of squares', Back.LossName);
    AssertEquals('the engine', 'Python lmfit', Back.EngineName);
end;

procedure TFitProgressJsonTest.TheStageSurvivesTheRoundTrip;
var
    R, Back: TFitProgressReport;
begin
    //  What the run is doing before the engine reports - starting it - is the
    //  only thing the header can say during a start of minutes.
    R := Default(TFitProgressReport);
    R.Busy := True;
    R.Stage := 'Starting the Python engine';
    AssertTrue(FitProgressFromJsonString(FitProgressToJsonString(R), Back));
    AssertEquals('the stage', 'Starting the Python engine', Back.Stage);
end;

procedure TFitProgressJsonTest.AndIsLeftOutWhenThereIsNone;
begin
    //  ABSENT, NOT EMPTY, like the names: an older client never sees the key,
    //  and a reader cannot mistake "no stage" for a stage named nothing.
    AssertEquals('no key', 0,
        Pos('stage', FitProgressToJsonString(Default(TFitProgressReport))));
end;

procedure TFitProgressJsonTest.AndAReplyWithoutThemReadsThemAsUnknown;
var
    Back: TFitProgressReport;
begin
    //  An older server says nothing about either, and that is not a fault: the
    //  header then says what it always said.
    AssertTrue(FitProgressFromJsonString(
        '{"ok":true,"busy":true,"elapsed":1.0,"nextSeq":0,"samples":[]}', Back));
    AssertEquals('no objective', '', Back.LossName);
    AssertEquals('no engine', '', Back.EngineName);
end;


procedure TFitProgressJsonTest.ASnapshotWithoutCurvesIsRefused;
var
    R: TFitProgressReport;
begin
    //  A SNAPSHOT IS DROPPED, NOT PATCHED UP. Half a frame drawn over the data
    //  is a picture of a model that never existed; the samples in the same
    //  reply are still kept, so the loss chart goes on.
    AssertTrue(FitProgressFromJsonString('{"ok":true,"busy":true,' +
        '"elapsed":1.0,"nextSeq":1,"samples":[],' +
        '"snapshot":{"seq":1,"profile":{"x":[1],"y":[1]}}}', R));
    AssertFalse('no snapshot', R.HasSnapshot);
end;

procedure TFitProgressJsonTest.SoIsOneWhoseCurveIsNotAnObject;
var
    R: TFitProgressReport;
begin
    AssertTrue(FitProgressFromJsonString('{"ok":true,"busy":true,' +
        '"elapsed":1.0,"nextSeq":1,"samples":[],' +
        '"snapshot":{"seq":1,"profile":{"x":[1],"y":[1]},"curves":[7]}}', R));
    AssertFalse('no snapshot', R.HasSnapshot);
end;

procedure TFitProgressJsonTest.SoIsOneWhoseCurvePointsAreMalformed;
var
    R: TFitProgressReport;
begin
    AssertTrue(FitProgressFromJsonString('{"ok":true,"busy":true,' +
        '"elapsed":1.0,"nextSeq":1,"samples":[],"snapshot":{"seq":1,' +
        '"profile":{"x":[1],"y":[1]},"curves":[{"id":"a"}]}}', R));
    AssertFalse('no snapshot', R.HasSnapshot);
end;

procedure TFitProgressJsonTest.AndNothingAtAllIsNotAReport;
var
    R: TFitProgressReport;
begin
    //  The reader is handed whatever the transport parsed, which for a body
    //  that is not an object at all is nothing.
    AssertFalse(FitProgressFromJson(nil, R));
end;

procedure TFitProgressJsonTest.TheChangedProfileAndItsRevisionRoundTrip;
var
    Sent, R: TFitProgressReport;
begin
    Sent := TwoSamples;
    Sent.HasProfile := True;
    Sent.ProfileRevision := 2;
    SetLength(Sent.Profile.X, 2);
    SetLength(Sent.Profile.Y, 2);
    Sent.Profile.X[1] := 5;
    Sent.Profile.Y[1] := 6;
    AssertTrue('decoded', Decode(FitProgressToJsonString(Sent), R));
    AssertTrue('a profile', R.HasProfile);
    AssertEquals('its revision', 2, R.ProfileRevision);
    AssertEquals('its points', 2, Length(R.Profile.X));
    AssertEquals('its values', 6.0, R.Profile.Y[1], 1e-12);
end;

procedure TFitProgressJsonTest.AReportWithoutAChangedProfileSendsNone;
var
    O: TJSONObject;
    R: TFitProgressReport;
begin
    //  ABSENT, like the snapshot: an empty one would read as a profile with no
    //  points and take the data off the chart.
    O := FitProgressToJson(TwoSamples);
    try
        AssertTrue('no profile field', O.Find('experimentalProfile') = nil);
    finally
        O.Free;
    end;
    AssertTrue('decoded', Decode(FitProgressToJsonString(TwoSamples), R));
    AssertFalse('and none is read back', R.HasProfile);
end;

initialization
    //  A codec over strings: nothing outside this process.
    RegisterTest('unit', TFitProgressJsonTest);
end.
