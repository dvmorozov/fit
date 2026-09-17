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
    published
        //  What is being minimised, and by what.
        procedure TheObjectiveAndTheEngineSurviveTheRoundTrip;
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

initialization
    //  A codec over strings: nothing outside this process.
    RegisterTest('unit', TFitProgressJsonTest);
end.
