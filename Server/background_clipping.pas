// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the data stand above the model's background curve - the fit
intervals - found together with that curve's starting values.)

ITERATIVE CLIPPING. The background curve is fitted by least squares through
the samples (TNamedPointsSet.SeedFromBaseline, the shape's own solve); the
samples further than CLIP_UNITS noise units from it are dropped, and it is
fitted again through the rest, until no sample changes side. Peaks stand only
above a background, so their samples go and the curve settles on the floor;
samples far BELOW it go too - a detector that counted nothing at the start of
a scan is neither background nor peak. This is the family of the modified
polynomial fit (Lieber and Mahadevan-Jansen, Appl. Spectrosc. 57, 1363,
2003; Zhao et al., Appl. Spectrosc. 61, 1225, 2007) and of Sonneveld and
Visser's iterative background (J. Appl. Cryst. 8, 1, 1975). Ours fits the
background SHAPE the user chose instead of a plain polynomial.

THREE STARTS, the one that ends with the most samples on the curve winning: the
lower half of the samples by height, the bowl walk's points, and - for a series
shape of CLIP_FROM_ALL_TERMS or more - every sample. The last is what lets a
hump the curve has the terms to follow stay in the floor: begun below it, a hump
is clipped as a peak however wide it is. The intervals are what is left above
the curve this ends with, so they are a by-product of fitting the background:
nothing about them is set before it.

THE NOISE is measured once, from the data alone, before any curve: 1.4826
times the median absolute second difference, over the square root of 6 - the
standard deviation of normal noise, read where a smooth peak barely bends the
data. It was the spread of the residuals at first, and on 2.dat, whose 51
samples are mostly two peaks, the peaks were the spread: nothing was ever
clipped and the curve went through them. One figure for the whole profile -
this software's choice; noise that grows with the level (counts) is judged by
its typical size.

THE INTERVALS are the stretches of samples more than CLIP_UNITS above the
curve, each widened to where the data sink to one noise unit above it - where
the peak has gone into the noise - joined when fewer than JOIN_GAP samples
apart, and padded by PAD samples. A stretch with fewer than MIN_ABOVE samples
above CLIP_UNITS is a noisy sample, not a peak. Every one of these numbers is
this software's choice.
}
unit background_clipping;

{$mode objfpc}{$H+}

interface

uses
    points_set, named_points_set;

const
    { How many noise units from the curve a sample may lie and still be
      background. }
    CLIP_UNITS = 3.0;
    { The most rounds of fitting and clipping. }
    CLIP_ROUNDS = 100;
    { Samples above the clip a stretch needs to be a peak's. }
    MIN_ABOVE = 2;
    { Stretches fewer samples apart than this are one interval. }
    JOIN_GAP = 3;
    { Samples added at each end of an interval. }
    PAD = 2;
    { The fewest samples a round may keep: fewer, and the curve stays as it is. }
    MIN_KEPT = 4;
    { A series background with at least this many terms is also clipped from
      every sample (ClipToBackground): it can follow a broad hump, and a curve
      begun below the hump never would. }
    CLIP_FROM_ALL_TERMS = 12;

type
    { The samples AFirst..ALast of the data, both included. }
    TSampleInterval = record
        First, Last: longint;
    end;
    TSampleIntervals = array of TSampleInterval;
    { Values at the samples clipped, in their order. }
    TClippedValues = array of double;

    TBackgroundClipping = record
        { The stretches standing above the background, in order. }
        Intervals: TSampleIntervals;
        { The noise the samples were judged by. }
        Noise: double;
        { Rounds it took. }
        Rounds: longint;
        { The terms the curve had (TNamedPointsSet.TermCount; 0 for a shape
          with none to choose), and how many of the samples lie on it - within
          CLIP_UNITS noise units - as it ended. }
        Terms, Kept: longint;
    end;

{ Seeds ACurve - a background shape sampled at AX already, or with no samples
  yet - on the background of AX, AY (AX ascending) and answers where the data
  stand above it. ACurve is left sampled at AX and holding the starting
  values. }
function ClipToBackground(ACurve: TNamedPointsSet;
    const AX, AY: array of double): TBackgroundClipping;

{ THE SCORE OF A BACKGROUND AGAINST THE FLOOR OF THE DATA: the squared
  residual over the noise, each sample's part CAPPED at CLIP_UNITS squared, plus
  ln(samples) for every one of ATerms terms - a BIC that counts a peak as one
  miss however tall it is. Capped, because a background is not asked to match
  the peaks: they would otherwise outweigh everything and favour whatever curve
  passes nearest them. A hump the background does not follow costs every one of
  its samples the cap; one it follows costs about one each, so a curve that
  can take the hump pays for its terms, and one that cannot is not credited
  for trying. The same data and noise for every number of terms, so the scores
  compare. }
function FloorFitBic(const AY, AFloor: array of double; ANoise: double;
    ATerms: longint): double;

{ One starting curve on EVERY SAMPLE inside the intervals of AClipping, none
  outside them: a pick at each, the sample's own x and its height above the
  floor AFloor, which is the amplitude its curve starts from. The intervals and
  the picks are one rule, so a hump the clipping calls a peak can never be left
  without a curve - the peak search it replaces cut at a fraction of the
  highest point and left low humps bare inside intervals. A height is never
  under AClipping.Noise: a padded sample that dipped below the floor would seed
  a curve with no amplitude, which the reduction deletes at once. A sample more
  than CLIP_UNITS noise units BELOW the floor is a dip, not a peak, and gets no
  pick - the clipping drops such samples from the floor for the same reason -
  and neither does a sample whose height would be zero (data without noise,
  lying on the floor). }
procedure PicksInIntervals(const AClipping: TBackgroundClipping;
    const AX, AY, AFloor: array of double; APicks: TPointsSet);

implementation

uses
    SysUtils, Math, background_algorithms, MyExceptions;

function FloorFitBic(const AY, AFloor: array of double; ANoise: double;
    ATerms: longint): double;
var
    i: longint;
    R, Cap: double;
begin
    Cap := Sqr(CLIP_UNITS);
    Result := 0;
    for i := 0 to Min(High(AY), High(AFloor)) do
    begin
        R := Sqr((AY[i] - AFloor[i]) / Max(ANoise, 1e-300));
        Result := Result + Min(R, Cap);
    end;
    Result := Result + Ln(Max(Length(AY), 1)) * ATerms;
end;

procedure PicksInIntervals(const AClipping: TBackgroundClipping;
    const AX, AY, AFloor: array of double; APicks: TPointsSet);
var
    k, i: longint;
begin
    for k := 0 to High(AClipping.Intervals) do
        for i := AClipping.Intervals[k].First to AClipping.Intervals[k].Last do
            //  AX ascends, so a repeated x is the sample before: a pick set
            //  holds one pick per abscissa. Not a search of the picks made -
            //  that is quadratic over a profile of thousands of samples.
            if ((i = 0) or (AX[i] <> AX[i - 1])) and
               (AFloor[i] - AY[i] <= CLIP_UNITS * AClipping.Noise) and
               (Max(AY[i] - AFloor[i], AClipping.Noise) > 0) then
                APicks.AddNewPoint(AX[i], Max(AY[i] - AFloor[i],
                    AClipping.Noise));
end;

function MedianOf(var AValues: array of double): double;
var
    N: longint;
    Sorted: array of double;
    i, j, Gap: longint;
    T: double;
begin
    N := Length(AValues);
    if N = 0 then
        Exit(0);
    SetLength(Sorted, N);
    for i := 0 to N - 1 do
        Sorted[i] := AValues[i];
    //  A shell sort: a profile may hold tens of thousands of samples, and this
    //  runs once a round.
    Gap := N div 2;
    while Gap > 0 do
    begin
        for i := Gap to N - 1 do
        begin
            T := Sorted[i];
            j := i;
            while (j >= Gap) and (Sorted[j - Gap] > T) do
            begin
                Sorted[j] := Sorted[j - Gap];
                Dec(j, Gap);
            end;
            Sorted[j] := T;
        end;
        Gap := Gap div 2;
    end;
    if Odd(N) then
        Result := Sorted[N div 2]
    else
        Result := (Sorted[N div 2 - 1] + Sorted[N div 2]) / 2;
end;

type
    TKeptSamples = array of boolean;
    TResiduals = array of double;

{ Rounds of fitting and clipping from AStart; answers how many samples the
  curve ends with and leaves ACurve fitted through them, ANoise wide. }
function ClipFrom(ACurve: TNamedPointsSet; const AX, AY: array of double;
    ANoise: double; const AStart: TKeptSamples; out AResidual: TResiduals;
    out ARounds: longint): longint;
var
    N, i, Count: longint;
    Kept, NowKept: TKeptSamples;
    KeptX, KeptY: array of double;
    Changed: boolean;
begin
    N := Length(AX);
    Kept := Copy(AStart);
    SetLength(NowKept, N);
    SetLength(AResidual, N);
    ARounds := 0;
    repeat
        Inc(ARounds);
        Count := 0;
        SetLength(KeptX, N);
        SetLength(KeptY, N);
        for i := 0 to N - 1 do
            if Kept[i] then
            begin
                KeptX[Count] := AX[i];
                KeptY[Count] := AY[i];
                Inc(Count);
            end;
        SetLength(KeptX, Count);
        SetLength(KeptY, Count);
        ACurve.SeedFromBaseline(KeptX, KeptY);
        ACurve.ComputeUnlifted;
        for i := 0 to N - 1 do
            AResidual[i] := AY[i] - ACurve.PointYCoord[i];
        Changed := False;
        Count := 0;
        for i := 0 to N - 1 do
        begin
            NowKept[i] := Abs(AResidual[i]) <= CLIP_UNITS * ANoise;
            if NowKept[i] then
                Inc(Count);
            if NowKept[i] <> Kept[i] then
                Changed := True;
        end;
        //  TOO FEW LEFT to say more than the curve already does: it stays as
        //  the last round fitted it.
        if Count < MIN_KEPT then
            Break;
        for i := 0 to N - 1 do
            Kept[i] := NowKept[i];
    until (not Changed) or (ARounds >= CLIP_ROUNDS);
    Result := Count;
end;

{ The first start: the lower half of the samples, by height. }
function LowerHalf(const AY: array of double): TKeptSamples;
var
    Heights: array of double;
    Middle: double;
    i: longint;
begin
    SetLength(Heights, Length(AY));
    for i := 0 to High(AY) do
        Heights[i] := AY[i];
    Middle := MedianOf(Heights);
    SetLength(Result, Length(AY));
    for i := 0 to High(AY) do
        Result[i] := AY[i] <= Middle;
end;

{ The second start: the samples the bowl walk proposes as background
  (background_algorithms), or none in a build without it. }
function BowlWalk(const AX, AY: array of double): TKeptSamples;
var
    Data, Walk: TPointsSet;
    i, k: longint;
begin
    SetLength(Result, Length(AX));
    for i := 0 to High(Result) do
        Result[i] := False;
    Data := TPointsSet.Create(nil);
    try
        for i := 0 to High(AX) do
            Data.AddNewPoint(AX[i], AY[i]);
        try
            Walk := ProposeBackgroundPoints(Data);
        except
            on EUserException do
                Exit;
        end;
        try
            for k := 0 to Walk.PointsCount - 1 do
            begin
                i := Data.IndexOfValueX(Walk.PointXCoord[k]);
                if i >= 0 then
                    Result[i] := True;
            end;
        finally
            Walk.Free;
        end;
    finally
        Data.Free;
    end;
end;

function ClipToBackground(ACurve: TNamedPointsSet;
    const AX, AY: array of double): TBackgroundClipping;
var
    N, i, k, Above, Start, FromHalf, FromWalk, FromAll, Best, Rounds: longint;
    Residual: TResiduals;
    InRun: boolean;
    Scale, Floor: double;
    Bends: array of double;
    Runs: TSampleIntervals;
    Half, Walk, All, BestStart: TKeptSamples;
begin
    Result := Default(TBackgroundClipping);
    N := Length(AX);
    if (N = 0) or (Length(AY) <> N) then
        Exit;
    if ACurve.PointsCount = 0 then
        for i := 0 to N - 1 do
            ACurve.AddSample(AX[i]);
    if ACurve.PointsCount <> N then
        raise Exception.Create('a background is clipped onto the samples it ' +
            'is drawn at');
    //  A NOISE FLOOR for data with little or none: a hundredth of the data's
    //  typical size. A millionth of its span judged a noise-free falling
    //  background so strictly that the curve's own bend dropped samples, and
    //  a six-term Chebyshev started 17 counts off a floor of 157.
    SetLength(Bends, N);
    for i := 0 to N - 1 do
        Bends[i] := Abs(AY[i]);
    Scale := MedianOf(Bends);
    Floor := Max(Scale * 1e-2, 1e-300);
    Result.Noise := Floor;
    if N >= 3 then
    begin
        SetLength(Bends, N - 2);
        for i := 1 to N - 2 do
            Bends[i - 1] := Abs(AY[i - 1] - 2 * AY[i] + AY[i + 1]);
        Result.Noise := Max(1.4826 * MedianOf(Bends) / Sqrt(6), Floor);
    end;

    //  TWO STARTS, AND THE ONE THAT ENDS WITH MORE SAMPLES ON THE CURVE.
    //  The lower half of the samples by height: through all of them, the
    //  first curve on 2.dat - 51 samples, mostly two peaks - dipped below zero,
    //  was drawn lifted to zero, and kept no sample. But on a background that
    //  falls steeply the lower half is one side of the profile, and the curve
    //  through it is wild on the other: there the bowl walk's points, which
    //  trace a falling or rising floor, start it right. On Tb2Fe17 the walk
    //  starts at dead samples and stays under the floor, and the lower half
    //  wins.
    Half := LowerHalf(AY);
    Walk := BowlWalk(AX, AY);
    FromHalf := ClipFrom(ACurve, AX, AY, Result.Noise, Half, Residual, Rounds);
    Best := FromHalf;
    BestStart := Half;
    for i := 0 to N - 1 do
        if Walk[i] then
        begin
            FromWalk := ClipFrom(ACurve, AX, AY, Result.Noise, Walk, Residual,
                Rounds);
            if FromWalk > Best then
            begin
                Best := FromWalk;
                BestStart := Walk;
            end;
            Break;
        end;
    //  A THIRD START FOR A CURVE FLEXIBLE ENOUGH TO FOLLOW A BROAD FEATURE:
    //  every sample, as the modified polynomial fit starts (Lieber and
    //  Mahadevan-Jansen, cited above). Begun from the lower half or the walk, a
    //  hump above them is above the curve for good: it is clipped as a peak
    //  however wide it is, and no number of terms can bring it back. Begun from
    //  all of them, the curve rises towards the hump as far as its terms let it,
    //  the peaks narrower than that are clipped, and what is left is as smooth
    //  as the curve is flexible - the number of terms says what a background
    //  is. It wins only by ending with more samples on the curve than the other
    //  starts. Only for a curve with this many terms: a stiffer one would be
    //  drawn through the peaks, and it is how the clipping found its intervals
    //  before there were terms to choose.
    if ACurve.TermCount >= CLIP_FROM_ALL_TERMS then
    begin
        SetLength(All, N);
        for i := 0 to N - 1 do
            All[i] := True;
        FromAll := ClipFrom(ACurve, AX, AY, Result.Noise, All, Residual, Rounds);
        if FromAll > Best then
        begin
            Best := FromAll;
            BestStart := All;
        end;
    end;
    //  The winner fitted last, so the curve is left as it chose.
    ClipFrom(ACurve, AX, AY, Result.Noise, BestStart, Residual, Rounds);
    Result.Rounds := Rounds;
    Result.Terms := ACurve.TermCount;
    Result.Kept := 0;
    for i := 0 to N - 1 do
        if Abs(Residual[i]) <= CLIP_UNITS * Result.Noise then
            Inc(Result.Kept);

    //  THE STRETCHES ABOVE, widened to where the data sink into the noise.
    Runs := nil;
    i := 0;
    while i < N do
    begin
        if Residual[i] <= CLIP_UNITS * Result.Noise then
        begin
            Inc(i);
            Continue;
        end;
        Start := i;
        Above := 0;
        while (i < N) and (Residual[i] > Result.Noise) do
        begin
            if Residual[i] > CLIP_UNITS * Result.Noise then
                Inc(Above);
            Inc(i);
        end;
        while (Start > 0) and (Residual[Start - 1] > Result.Noise) do
            Dec(Start);
        if Above >= MIN_ABOVE then
        begin
            SetLength(Runs, Length(Runs) + 1);
            Runs[High(Runs)].First := Max(0, Start - PAD);
            Runs[High(Runs)].Last := Min(N - 1, i - 1 + PAD);
        end;
    end;
    //  Joined where they nearly touch.
    InRun := False;
    for k := 0 to High(Runs) do
    begin
        if InRun and (Runs[k].First - Result.Intervals[High(Result.Intervals)].Last
            <= JOIN_GAP) then
        begin
            Result.Intervals[High(Result.Intervals)].Last :=
                Max(Result.Intervals[High(Result.Intervals)].Last, Runs[k].Last);
            Continue;
        end;
        SetLength(Result.Intervals, Length(Result.Intervals) + 1);
        Result.Intervals[High(Result.Intervals)] := Runs[k];
        InRun := True;
    end;
end;

end.
