// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the data stand above the model's background curve, found
together with that curve's starting values.)
}
unit testcase_background_clipping;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, test_curve_types,
    points_set, background_clipping;

type
    TBackgroundClippingTest = class(TTestCase)
    private
        FCurve: TTestLinearBackgroundPointsSet;
        FX, FY: array of double;
        FFound: TBackgroundClipping;
        { 0..100 in steps of 0.5: ALevel + ASlope x, a peak of 200 at 30 and
          one of 120 at 70 (sigma 1), and noise of about sqrt(ALevel). }
        procedure GivenAProfile(ALevel, ASlope: double);
        procedure Clip;
        function IntervalHolding(AX: double): longint;
        function Level: double;
        function Slope: double;
        { The seeded line against the floor AAt + ASlope x, within half a
          noise unit, at both ends and in the middle. }
        procedure AssertOnTheFloor(AAt, ASlope: double);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheSeedIsTheFloorNotTheMeanOfEverything;
        procedure OnASlopeTooTheSeedFollowsTheFloor;
        procedure EachPeakIsAnIntervalOfItsOwn;
        procedure AnIntervalEndsWhereThePeakHasSunkIntoTheNoise;
        procedure DeadSamplesAtTheStartPullNothingDown;
        procedure BareDataHasNoInterval;
        procedure ASingleNoisySampleIsNoInterval;
        procedure ThePicksAreEverySampleInTheIntervalsAndNoneOutside;
        procedure ThePicksAreTheHeightsAboveTheFloorAndNeverBelowOneNoiseUnit;
        procedure ADipFarUnderTheFloorGetsNoPick;
        procedure TheFloorScoreCountsAPeakAsOneMissHoweverTallItIs;
        procedure TheFloorScoreChargesEveryTermItsLogOfTheSamples;
        procedure ABackgroundThatFollowsAHumpScoresBetterThanOneThatCrossesIt;
    end;

implementation

const
    STEP = 0.5;
    LAST_X = 100.0;

{ The same noise every run: a fixed sequence, roughly uniform, scaled to a
  standard deviation of ASigma. }
function Noise(AIndex: longint; ASigma: double): double;
var
    Seed: longword;
begin
    Seed := longword(AIndex) * 2654435761 + 12345;
    Seed := Seed xor (Seed shr 13);
    Seed := Seed * 1274126177;
    Result := ((Seed mod 10000) / 10000 - 0.5) * Sqrt(12) * ASigma;
end;

function Gauss(AAmplitude, ACentre, AX: double): double;
begin
    Result := AAmplitude * Exp(-Sqr(AX - ACentre) / 2);
end;

procedure TBackgroundClippingTest.SetUp;
begin
    FCurve := TTestLinearBackgroundPointsSet.Create(nil);
end;

procedure TBackgroundClippingTest.TearDown;
begin
    FreeAndNil(FCurve);
end;

procedure TBackgroundClippingTest.GivenAProfile(ALevel, ASlope: double);
var
    i, N: longint;
    x, Floor: double;
begin
    N := Round(LAST_X / STEP) + 1;
    SetLength(FX, N);
    SetLength(FY, N);
    for i := 0 to N - 1 do
    begin
        x := i * STEP;
        Floor := ALevel + ASlope * x;
        FX[i] := x;
        FY[i] := Floor + Gauss(200, 30, x) + Gauss(120, 70, x) +
            Noise(i, Sqrt(Floor));
    end;
end;

procedure TBackgroundClippingTest.Clip;
begin
    FFound := ClipToBackground(FCurve, FX, FY);
end;

function TBackgroundClippingTest.IntervalHolding(AX: double): longint;
var
    k: longint;
begin
    Result := -1;
    for k := 0 to High(FFound.Intervals) do
        if (FX[FFound.Intervals[k].First] <= AX) and
           (AX <= FX[FFound.Intervals[k].Last]) then
            Exit(k);
end;

function TBackgroundClippingTest.Level: double;
begin
    Result := FCurve.ValuesByName['b0'];
end;

function TBackgroundClippingTest.Slope: double;
begin
    Result := FCurve.ValuesByName['b1'];
end;

procedure TBackgroundClippingTest.AssertOnTheFloor(AAt, ASlope: double);
const
    WHERE: array[0..2] of double = (0, 50, 100);
var
    k: longint;
    x: double;
begin
    for k := 0 to High(WHERE) do
    begin
        x := WHERE[k];
        AssertEquals(Format('the floor at %g', [x]), AAt + ASlope * x,
            Level + Slope * x, 0.5 * Sqrt(AAt + ASlope * x));
    end;
end;

procedure TBackgroundClippingTest.TheSeedIsTheFloorNotTheMeanOfEverything;
begin
    //  The mean of the data with its two peaks in it is about 56; the floor is
    //  50, and the noise there about 7.
    GivenAProfile(50, 0);
    Clip;
    AssertOnTheFloor(50, 0);
end;

procedure TBackgroundClippingTest.OnASlopeTooTheSeedFollowsTheFloor;
begin
    GivenAProfile(20, 0.5);
    Clip;
    AssertOnTheFloor(20, 0.5);
end;

procedure TBackgroundClippingTest.EachPeakIsAnIntervalOfItsOwn;
begin
    GivenAProfile(50, 0);
    Clip;
    AssertEquals('two intervals', 2, Length(FFound.Intervals));
    AssertTrue('one holding the peak at 30', IntervalHolding(30) >= 0);
    AssertTrue('and one the peak at 70', IntervalHolding(70) >= 0);
    AssertTrue('not the same one', IntervalHolding(30) <> IntervalHolding(70));
end;

procedure TBackgroundClippingTest.AnIntervalEndsWhereThePeakHasSunkIntoTheNoise;
var
    k: longint;
begin
    //  A sigma-1 peak of 200 over noise of 7 is under one noise unit 3.3
    //  sigma out: the interval reaches about that far, and not to the floor
    //  of the whole profile.
    GivenAProfile(50, 0);
    Clip;
    k := IntervalHolding(30);
    AssertTrue('the peak has an interval', k >= 0);
    AssertTrue(Format('it starts on the floor, at %g', [FX[FFound.Intervals[k].First]]),
        (FX[FFound.Intervals[k].First] <= 30 - 2.5) and
        (FX[FFound.Intervals[k].First] >= 30 - 6));
    AssertTrue(Format('and ends on it, at %g', [FX[FFound.Intervals[k].Last]]),
        (FX[FFound.Intervals[k].Last] >= 30 + 2.5) and
        (FX[FFound.Intervals[k].Last] <= 30 + 6));
end;

procedure TBackgroundClippingTest.DeadSamplesAtTheStartPullNothingDown;
var
    i: longint;
begin
    //  A detector that counted nothing below 10 (Tb2Fe17 below 2 degrees):
    //  those samples are no background, and no peak either.
    GivenAProfile(50, 0);
    for i := 0 to High(FX) do
        if FX[i] < 10 then
            FY[i] := 0;
    Clip;
    //  Judged where it was measured: at 0 the line is an extrapolation.
    AssertEquals('the floor that was measured, at 50', 50, Level + 50 * Slope,
        0.5 * Sqrt(50));
    AssertEquals('still two intervals', 2, Length(FFound.Intervals));
end;

procedure TBackgroundClippingTest.BareDataHasNoInterval;
var
    i: longint;
begin
    GivenAProfile(50, 0);
    for i := 0 to High(FX) do
        FY[i] := 50 + Noise(i, Sqrt(50));
    Clip;
    AssertEquals('nothing stands above the floor', 0, Length(FFound.Intervals));
end;

procedure TBackgroundClippingTest.ASingleNoisySampleIsNoInterval;
var
    i: longint;
begin
    GivenAProfile(50, 0);
    for i := 0 to High(FX) do
        FY[i] := 50 + Noise(i, Sqrt(50));
    FY[100] := 50 + 6 * Sqrt(50);
    Clip;
    AssertEquals('one sample high is noise, not a peak', 0,
        Length(FFound.Intervals));
end;

procedure TBackgroundClippingTest.ThePicksAreEverySampleInTheIntervalsAndNoneOutside;
var
    Floor: array of double;
    Picks: TPointsSet;
    i, k, Inside: longint;
begin
    //  One starting curve on every sample of every interval, so the intervals
    //  and the picks cannot disagree about what is a peak.
    GivenAProfile(50, 0);
    Clip;
    SetLength(Floor, Length(FX));
    for i := 0 to High(Floor) do
        Floor[i] := FCurve.PointYCoord[i];
    Picks := TPointsSet.Create(nil);
    try
        PicksInIntervals(FFound, FX, FY, Floor, Picks);
        Inside := 0;
        for k := 0 to High(FFound.Intervals) do
            Inc(Inside, FFound.Intervals[k].Last - FFound.Intervals[k].First + 1);
        AssertEquals('one pick per sample inside the intervals', Inside,
            Picks.PointsCount);
        for i := 0 to High(FX) do
        begin
            k := IntervalHolding(FX[i]);
            AssertEquals(Format('the sample at %g is picked exactly when it is ' +
                'inside an interval', [FX[i]]), k >= 0,
                Picks.IndexOfValueX(FX[i]) >= 0);
        end;
    finally
        Picks.Free;
    end;
end;

procedure TBackgroundClippingTest.ThePicksAreTheHeightsAboveTheFloorAndNeverBelowOneNoiseUnit;
var
    Floor: array of double;
    Picks: TPointsSet;
    i, At: longint;
begin
    //  A pick's height seeds a curve's amplitude: a padded sample that dipped
    //  under the floor must not seed zero or a negative one.
    GivenAProfile(50, 0);
    Clip;
    SetLength(Floor, Length(FX));
    for i := 0 to High(Floor) do
        Floor[i] := FCurve.PointYCoord[i];
    Picks := TPointsSet.Create(nil);
    try
        PicksInIntervals(FFound, FX, FY, Floor, Picks);
        for i := 0 to Picks.PointsCount - 1 do
            AssertTrue(Format('the pick at %g seeds a positive amplitude',
                [Picks.PointXCoord[i]]), Picks.PointYCoord[i] >= FFound.Noise);
        At := 0;
        while FX[At] < 30 do
            Inc(At);
        AssertEquals('the top of the peak: the data above the floor',
            FY[At] - Floor[At], Picks.PointYCoord[Picks.IndexOfValueX(FX[At])],
            1e-9);
    finally
        Picks.Free;
    end;
end;

procedure TBackgroundClippingTest.ADipFarUnderTheFloorGetsNoPick;
var
    Clipping: TBackgroundClipping;
    X, Y, Floor: array of double;
    Picks: TPointsSet;
    i: longint;
begin
    //  An interval made by hand over samples 2..6 of a flat floor of 10 and a
    //  noise unit of 1: one sample 60 under the floor is a dip, one 2 under it
    //  is ordinary noise and one 5 over it is a hump.
    SetLength(X, 9);
    SetLength(Y, 9);
    SetLength(Floor, 9);
    for i := 0 to 8 do
    begin
        X[i] := i;
        Y[i] := 10;
        Floor[i] := 10;
    end;
    Y[3] := 10 - 60;
    Y[4] := 10 - 2;
    Y[5] := 10 + 5;
    Clipping := Default(TBackgroundClipping);
    SetLength(Clipping.Intervals, 1);
    Clipping.Intervals[0].First := 2;
    Clipping.Intervals[0].Last := 6;
    Clipping.Noise := 1;
    Picks := TPointsSet.Create(nil);
    try
        PicksInIntervals(Clipping, X, Y, Floor, Picks);
        AssertEquals('the dip is no peak', -1, Picks.IndexOfValueX(3));
        AssertEquals('the noise under the floor is picked, at one noise unit',
            1, Picks.PointYCoord[Picks.IndexOfValueX(4)], 1e-9);
        AssertEquals('the hump is picked at its height', 5,
            Picks.PointYCoord[Picks.IndexOfValueX(5)], 1e-9);
        AssertEquals('the others of the interval too', 4, Picks.PointsCount);
    finally
        Picks.Free;
    end;
end;

procedure TBackgroundClippingTest.TheFloorScoreCountsAPeakAsOneMissHoweverTallItIs;
var
    Y, Floor: array of double;
    Small, Huge: double;
    i: longint;
begin
    //  A hundred samples on the floor and one off it: 10 noise units, then a
    //  thousand. Capped, both cost the same.
    SetLength(Y, 100);
    SetLength(Floor, 100);
    for i := 0 to 99 do
    begin
        Y[i] := 5;
        Floor[i] := 5;
    end;
    Y[50] := 5 + 10;
    Small := FloorFitBic(Y, Floor, 1, 0);
    Y[50] := 5 + 1000;
    Huge := FloorFitBic(Y, Floor, 1, 0);
    AssertEquals('the cap', Sqr(CLIP_UNITS), Small, 1e-9);
    AssertEquals('however tall', Small, Huge, 1e-9);
end;

procedure TBackgroundClippingTest.TheFloorScoreChargesEveryTermItsLogOfTheSamples;
var
    Y, Floor: array of double;
    i: longint;
begin
    SetLength(Y, 200);
    SetLength(Floor, 200);
    for i := 0 to 199 do
    begin
        Y[i] := 3;
        Floor[i] := 3;
    end;
    AssertEquals('no misses, no terms', 0, FloorFitBic(Y, Floor, 1, 0), 1e-9);
    AssertEquals('ln 200 a term', 7 * Ln(200), FloorFitBic(Y, Floor, 1, 7), 1e-9);
end;

procedure TBackgroundClippingTest.ABackgroundThatFollowsAHumpScoresBetterThanOneThatCrossesIt;
var
    Y, Flat, Follows: array of double;
    i: longint;
begin
    //  A hundred samples on a hump twenty noise units high, in four hundred.
    SetLength(Y, 400);
    SetLength(Flat, 400);
    SetLength(Follows, 400);
    for i := 0 to 399 do
    begin
        Flat[i] := 10;
        Follows[i] := 10;
        Y[i] := 10;
        if (i >= 100) and (i < 200) then
        begin
            Y[i] := 30;
            Follows[i] := 30;
        end;
    end;
    //  Twenty more terms are worth it when they take the hump; none is worth
    //  its charge when they leave it where it was.
    AssertTrue('following it pays for its terms',
        FloorFitBic(Y, Follows, 1, 32) < FloorFitBic(Y, Flat, 1, 12));
    AssertTrue('and extra terms that take nothing do not',
        FloorFitBic(Y, Flat, 1, 12) < FloorFitBic(Y, Flat, 1, 32));
end;

initialization
    RegisterTest('unit', TBackgroundClippingTest);
end.
