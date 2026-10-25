// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Is the difference between the data and the model only noise?)

A model that explains the data leaves a difference that is noise: no sample
of it says anything about the next. What is left otherwise - a wave the model
missed, a peak it left out - shows as the difference following itself. This
unit asks that question, of every model in every build, the same way.

THE TEST: LJUNG-BOX (Ljung & Box 1978, Biometrika 65:297). The sum of the
difference's first h autocorrelations squared, weighted so that for noise it
follows a chi-square of h degrees of freedom. h = min(10, n/5): Hyndman &
Athanasopoulos's rule (Forecasting: Principles and Practice) - enough lags to
see a slow wave, few enough that each is estimated from many pairs. Canonical; its limits: it sees
correlation, not every departure from noise (a step in the variance passes).

STANDARDISED FIRST. Each sample is divided by the noise expected at it, so a
quiet stretch and a loud one weigh alike. This software's choice. THE NOISE IS
THE FIELD'S, given by the caller: the variance the noise rule of the model's
curve types measures (TNamedPointsSet.NoiseRule, through ModelResidualNoise -
the noise the fit's figures and a reduction's BIC are weighed by). This unit
knows no field's noise, so with none given nothing is judged. Judged by the
noise of counts whatever the type said, a price - whose noise grows with its
level, where counts' grows with its square root - had the noise of its quiet
years put twenty times too low and those years named as structure, and its
loud years read as noise (the S&P 500 over a century, 2026-10-25).

WHERE: THE WAVELET TRANSFORM, sample by sample (residual_wavelet), each fit
interval transformed on its own and every sample judged together. It was
windows of fifty samples, each by Ljung-Box, which named the whole of a window
around a burst a few samples long (2026-10-13); one mechanism now
says where, for the shading, the chart's curve and the scalogram alike.

THE VERDICT: each interval tested whole, the intervals' p-values combined by
Fisher's method (Fisher 1925, Statistical Methods for Research Workers, 21.1);
random when that p is at least 5 % and no stretch is named.

A TEST IS NOT A PROOF: a difference read as random may still hide structure
the test cannot see, and a noise estimate that is wrong moves every verdict.
}
unit residual_randomness;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Types, Math, fpjson, sample_coverage;

const
    { The significance every verdict here is taken at. }
    RANDOMNESS_SIGNIFICANCE = 0.05;
    { The most lags Ljung-Box is taken over. }
    LJUNG_BOX_MAX_LAGS = 10;
    { Fewer samples than this are not tested: two lags at the least. }
    LJUNG_BOX_MIN_SAMPLES = 10;

type
    TDoubleArray = array of double;

    { Samples ALo..AHi of the arrays judged, inclusive. }
    TSampleSpan = record
        Lo, Hi: longint;
    end;
    TSampleSpans = array of TSampleSpan;

    TLjungBox = record
        Valid: boolean;
        Q: double;
        Lags: longint;
        P: double;
    end;

    { The verdict on a difference. Tested is False where nothing could be
      tested - too few samples, or a reply from before this existed. }
    TResidualRandomness = record
        Tested: boolean;
        { The intervals' Ljung-Box p-values, combined. }
        P: double;
        { How many samples were judged for where the difference is not
          noise (residual_wavelet). }
        Samples: longint;
        { The windows judged not random, merged where they touch. }
        NonRandomRanges: TSampleRanges;
    end;


function SampleSpan(ALo, AHi: longint): TSampleSpan;

{ Ljung-Box over AR, with min(10, n/5) lags. Not Valid below
  LJUNG_BOX_MIN_SAMPLES. }
function LjungBox(const AR: array of double): TLjungBox;

{ Fisher's combination of independent p-values: the chance that
  -2 sum ln p reaches what it does when every hypothesis holds. }
function FisherCombined(const AP: array of double): double;

{ Which of AP Benjamini-Hochberg rejects at false discovery rate AQ. }
function BenjaminiHochberg(const AP: array of double;
    AQ: double): TBooleanDynArray;

{ (data - model) / sqrt(ANoise), sample by sample: ANoise the variance of the
  noise at each sample, as the model's noise rule measures it. Empty when
  ANoise does not hold one per sample: a variance for some other series would
  weigh samples by another sample's noise. }
function StandardisedResiduals(const AData, AModel,
    ANoise: array of double): TDoubleArray;

{ The verdict on AData - AModel over the fit intervals AIntervals (spans into
  the arrays, which hold the fitting window's samples), AX naming where each
  sample is, the difference standardised by ANoise as StandardisedResiduals
  takes it. Not tested when ANoise does not hold one variance per sample. }
function JudgeResiduals(const AX, AData, AModel: array of double;
    const AIntervals: array of TSampleSpan;
    const ANoise: array of double): TResidualRandomness;

{ WHETHER REMOVING A CURVE WOULD LEAVE A PATTERN: the standardised difference
  over the curve's samples is random with it (AWith) and not without it
  (AWithout), by Ljung-Box at the significance. A reduction refuses such a
  removal whatever BIC says - beside BIC, not instead of it: where the
  difference is not random already, the curve is not what made it so, and
  BIC decides as before. }
function RemovalBreaksRandomness(const AWith, AWithout: array of double): boolean;

{ Whether the difference reads as random: tested, combined p at least the
  significance, and no stretch named. }
function ResidualIsRandom(const AR: TResidualRandomness): boolean;

{ THE VERDICT IN ONE PHRASE, said alike on the status bar and in History:
  'Difference: random (p = 0.31)', 'Difference: not random in 3 stretches',
  or, with no stretch named, 'Difference: not random (p = 0.0004)'. Empty
  when nothing was judged. }
function ResidualVerdictText(const AR: TResidualRandomness): string;


{ Adds the verdict to AObject; nothing when it was not tested. }
procedure AddRandomnessJson(AObject: TJSONObject; const AR: TResidualRandomness);
{ Reads what AddRandomnessJson wrote; absent, it reads as not tested - how a
  reply or a project file from before this reads. }
procedure ReadRandomnessJson(AObject: TJSONObject; out AR: TResidualRandomness);

implementation

uses
    special_functions, residual_wavelet;

function SampleSpan(ALo, AHi: longint): TSampleSpan;
begin
    Result.Lo := ALo;
    Result.Hi := AHi;
end;

function LjungBox(const AR: array of double): TLjungBox;
var
    n, k, t: longint;
    Mean, Den, Num: double;
begin
    Result := Default(TLjungBox);
    n := Length(AR);
    if n < LJUNG_BOX_MIN_SAMPLES then
        Exit;
    Result.Valid := True;
    Result.Lags := Min(LJUNG_BOX_MAX_LAGS, n div 5);
    Mean := 0;
    for t := 0 to n - 1 do
        Mean := Mean + AR[t];
    Mean := Mean / n;
    Den := 0;
    for t := 0 to n - 1 do
        Den := Den + Sqr(AR[t] - Mean);
    //  A difference of nothing at all follows nothing.
    if Den <= 0 then
    begin
        Result.P := 1;
        Exit;
    end;
    for k := 1 to Result.Lags do
    begin
        Num := 0;
        for t := 0 to n - 1 - k do
            Num := Num + (AR[t] - Mean) * (AR[t + k] - Mean);
        Result.Q := Result.Q + Sqr(Num / Den) / (n - k);
    end;
    Result.Q := Result.Q * n * (n + 2);
    Result.P := ChiSquareSurvival(Result.Q, Result.Lags);
end;

function FisherCombined(const AP: array of double): double;
var
    i: longint;
    X: double;
begin
    if Length(AP) = 0 then
        Exit(1);
    X := 0;
    for i := 0 to High(AP) do
        X := X - 2 * Ln(Max(AP[i], 1e-300));
    Result := ChiSquareSurvival(X, 2 * Length(AP));
end;

{ Sinks AValues[AAt] into the max-heap of the first ACount values. }
procedure SiftDown(var AValues: TDoubleArray; AAt, ACount: longint);
var
    Child: longint;
    t: double;
begin
    t := AValues[AAt];
    Child := 2 * AAt + 1;
    while Child < ACount do
    begin
        if (Child + 1 < ACount) and (AValues[Child + 1] > AValues[Child]) then
            Inc(Child);
        if AValues[Child] <= t then
            Break;
        AValues[AAt] := AValues[Child];
        AAt := Child;
        Child := 2 * AAt + 1;
    end;
    AValues[AAt] := t;
end;

function BenjaminiHochberg(const AP: array of double;
    AQ: double): TBooleanDynArray;
var
    Sorted: TDoubleArray;
    i, m: longint;
    Cut, t: double;
begin
    m := Length(AP);
    SetLength(Result, m);
    SetLength(Sorted, m);
    for i := 0 to m - 1 do
        Sorted[i] := AP[i];
    //  HEAPSORT, m log m. It was an insertion sort, "a few hundred windows at
    //  most" - and then the wavelet judged every (sample, octave) of the
    //  series: 300 000 p-values for 24 816 bars, a quarter of a minute of the
    //  window's thread on every frame of a run (findings.md, "The Difference
    //  structure strip froze the window on a long series").
    for i := m div 2 - 1 downto 0 do
        SiftDown(Sorted, i, m);
    for i := m - 1 downto 1 do
    begin
        t := Sorted[0];
        Sorted[0] := Sorted[i];
        Sorted[i] := t;
        SiftDown(Sorted, 0, i);
    end;
    Cut := -1;
    for i := m - 1 downto 0 do
        if Sorted[i] <= (i + 1) / m * AQ then
        begin
            Cut := Sorted[i];
            Break;
        end;
    for i := 0 to m - 1 do
        Result[i] := AP[i] <= Cut;
end;

function StandardisedResiduals(const AData, AModel,
    ANoise: array of double): TDoubleArray;
var
    i: longint;
begin
    Result := nil;
    //  OUT OF STEP IS NOT USED, as ModelResidualNoise refuses it.
    if Length(ANoise) <> Length(AData) then
        Exit;
    SetLength(Result, Length(AData));
    for i := 0 to High(AData) do
        Result[i] := (AData[i] - AModel[i]) / Sqrt(ANoise[i]);
end;

function JudgeResiduals(const AX, AData, AModel: array of double;
    const AIntervals: array of TSampleSpan;
    const ANoise: array of double): TResidualRandomness;
var
    R: TDoubleArray;
    IntervalP: TDoubleArray;
    Where: TWaveletStructure;
    s, i, n, Lo: longint;
    LB: TLjungBox;
    Open: boolean;
begin
    Result := Default(TResidualRandomness);
    //  NO NOISE, NO VERDICT: nothing to judge the difference against.
    if (Length(AData) = 0) or (Length(ANoise) <> Length(AData)) then
        Exit;
    R := StandardisedResiduals(AData, AModel, ANoise);
    //  WHETHER: each interval's Ljung-Box, combined.
    IntervalP := nil;
    for s := 0 to High(AIntervals) do
    begin
        Lo := AIntervals[s].Lo;
        n := AIntervals[s].Hi - Lo + 1;
        LB := LjungBox(Copy(R, Lo, n));
        if not LB.Valid then
            Continue;
        SetLength(IntervalP, Length(IntervalP) + 1);
        IntervalP[High(IntervalP)] := LB.P;
    end;
    if Length(IntervalP) = 0 then
        Exit;
    Result.Tested := True;
    Result.P := FisherCombined(IntervalP);
    //  WHERE: the wavelet's, every interval on its own and every sample of
    //  them judged together (residual_wavelet). It was 50-sample windows,
    //  each by Ljung-Box, which named a whole window around a burst a few
    //  samples long.
    Where := WaveletStructureOver(R, AIntervals);
    for s := 0 to High(AIntervals) do
    begin
        if AIntervals[s].Hi - AIntervals[s].Lo + 1 >= WAVELET_MIN_SAMPLES then
            Inc(Result.Samples, AIntervals[s].Hi - AIntervals[s].Lo + 1);
        //  Runs of named samples, never across the gap between intervals.
        Open := False;
        for i := AIntervals[s].Lo to AIntervals[s].Hi do
        begin
            if not Where.Named[i] then
            begin
                Open := False;
                Continue;
            end;
            n := Length(Result.NonRandomRanges);
            if not Open then
            begin
                SetLength(Result.NonRandomRanges, n + 1);
                Result.NonRandomRanges[n].FromX := AX[i];
                Result.NonRandomRanges[n].Count := 0;
                Inc(n);
                Open := True;
            end;
            Result.NonRandomRanges[n - 1].ToX := AX[i];
            Inc(Result.NonRandomRanges[n - 1].Count);
        end;
    end;
end;

function RemovalBreaksRandomness(const AWith, AWithout: array of double): boolean;
var
    LBWith, LBWithout: TLjungBox;
begin
    LBWith := LjungBox(AWith);
    LBWithout := LjungBox(AWithout);
    Result := LBWith.Valid and LBWithout.Valid and
        (LBWith.P >= RANDOMNESS_SIGNIFICANCE) and
        (LBWithout.P < RANDOMNESS_SIGNIFICANCE);
end;

function ResidualIsRandom(const AR: TResidualRandomness): boolean;
begin
    Result := AR.Tested and (AR.P >= RANDOMNESS_SIGNIFICANCE) and
        (Length(AR.NonRandomRanges) = 0);
end;

function ResidualVerdictText(const AR: TResidualRandomness): string;
var
    n: longint;
begin
    if not AR.Tested then
        Exit('');
    n := Length(AR.NonRandomRanges);
    if ResidualIsRandom(AR) then
        Result := Format('Difference: random (p = %.2g)', [AR.P])
    else if n = 1 then
        Result := 'Difference: not random in 1 stretch'
    else if n > 1 then
        Result := Format('Difference: not random in %d stretches', [n])
    else
        Result := Format('Difference: not random (p = %.2g)', [AR.P]);
end;

procedure AddRandomnessJson(AObject: TJSONObject; const AR: TResidualRandomness);
var
    A: TJSONArray;
    O: TJSONObject;
    i: longint;
begin
    if not AR.Tested then
        Exit;
    AObject.Add('residualP', AR.P);
    AObject.Add('residualSamples', AR.Samples);
    A := TJSONArray.Create;
    for i := 0 to High(AR.NonRandomRanges) do
    begin
        O := TJSONObject.Create;
        O.Add('from', AR.NonRandomRanges[i].FromX);
        O.Add('to', AR.NonRandomRanges[i].ToX);
        O.Add('count', AR.NonRandomRanges[i].Count);
        A.Add(O);
    end;
    AObject.Add('nonRandomRanges', A);
end;

procedure ReadRandomnessJson(AObject: TJSONObject; out AR: TResidualRandomness);
var
    D: TJSONData;
    A: TJSONArray;
    i: longint;
begin
    AR := Default(TResidualRandomness);
    D := AObject.Find('residualP');
    if not (D is TJSONNumber) then
        Exit;
    AR.Tested := True;
    AR.P := D.AsFloat;
    //  'residualWindows', the count of 50-sample windows a project saved
    //  before the wavelet replaced it holds, is left unread: it counted something else.
    AR.Samples := AObject.Get('residualSamples', 0);
    D := AObject.Find('nonRandomRanges');
    if not (D is TJSONArray) then
        Exit;
    A := TJSONArray(D);
    SetLength(AR.NonRandomRanges, A.Count);
    for i := 0 to A.Count - 1 do
    begin
        AR.NonRandomRanges[i].FromX := A.Objects[i].Get('from', 0.0);
        AR.NonRandomRanges[i].ToX := A.Objects[i].Get('to', 0.0);
        AR.NonRandomRanges[i].Count := A.Objects[i].Get('count', 0);
    end;
end;

end.
