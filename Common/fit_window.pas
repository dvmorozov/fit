// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The samples a fit is measured over, and which interval each is in.)

ONE ANSWER, ASKED TWICE: the server's statistics (fit_service_statistics) and
the window's spectrum of the difference (TFitClient.ResidualSpectrum) both read
the difference over the fit intervals, and a sample counted by one and not the
other would make the two disagree about the same model. The bounds are
consecutive (start, end) pairs, each end included; a sample in no pair is
outside the window.
}
unit fit_window;

{$mode objfpc}{$H+}

interface

uses
    residual_randomness;

type
    TFitWindow = record
        { The samples inside the fit intervals, in order: where, the data, and
          the model there. }
        X, Data, Fit: TDoubleArray;
        { Each interval's samples, as indices into the three. }
        Spans: TSampleSpans;
        { The variance of the noise at each of them, as the model's curve
          types measure it (named_points_set.ModelResidualNoise) - or empty,
          the data being counts, for the framework's own measure
          (residual_randomness.StandardisedResiduals). Picked from the whole
          series' noise exactly as the data are, so each difference meets the
          variance measured at its own sample. }
        Noise: TDoubleArray;
    end;

{ The samples of AX (with AData and AFit beside them) inside the intervals
  ABoundsX gives as consecutive (start, end) pairs. NO INTERVAL MEANS THE WHOLE
  PROFILE, one span, as the engine fits it (TFitService.CreateTasks): read as
  nothing, the window judged nothing through a run the engine was fitting
  every sample of (2026-10-18). }
function FitWindowOf(const AX, AData, AFit, ABoundsX: array of double): TFitWindow;
    overload;
{ The same, with ANoise - the noise's variance at each sample of AX, over the
  whole series - picked beside them. ANoise not one per sample leaves the
  window's Noise empty: counts. }
function FitWindowOf(const AX, AData, AFit, ANoise,
    ABoundsX: array of double): TFitWindow; overload;

{ The pair of ABoundsX that AX falls in, counted from 0; -1 for none. }
function IntervalIndexOf(const ABoundsX: array of double; AX: double): longint;

implementation

function IntervalIndexOf(const ABoundsX: array of double; AX: double): longint;
var
    i: longint;
begin
    Result := -1;
    i := 0;
    while i + 1 <= High(ABoundsX) do
    begin
        if (AX >= ABoundsX[i]) and (AX <= ABoundsX[i + 1]) then
            Exit(i div 2);
        Inc(i, 2);
    end;
end;

function FitWindowOf(const AX, AData, AFit, ABoundsX: array of double): TFitWindow;
begin
    Result := FitWindowOf(AX, AData, AFit, [], ABoundsX);
end;

function FitWindowOf(const AX, AData, AFit, ANoise,
    ABoundsX: array of double): TFitWindow;
var
    i, n, Interval, LastInterval: longint;
    HasNoise: boolean;
begin
    Result := Default(TFitWindow);
    SetLength(Result.X, Length(AX));
    SetLength(Result.Data, Length(AX));
    SetLength(Result.Fit, Length(AX));
    HasNoise := (Length(AX) > 0) and (Length(ANoise) = Length(AX));
    if HasNoise then
        SetLength(Result.Noise, Length(AX));
    LastInterval := -1;
    n := 0;
    for i := 0 to High(AX) do
    begin
        if Length(ABoundsX) = 0 then
            Interval := 0
        else
            Interval := IntervalIndexOf(ABoundsX, AX[i]);
        if Interval < 0 then
            Continue;
        if Interval <> LastInterval then
        begin
            SetLength(Result.Spans, Length(Result.Spans) + 1);
            Result.Spans[High(Result.Spans)].Lo := n;
            LastInterval := Interval;
        end;
        Result.Spans[High(Result.Spans)].Hi := n;
        Result.X[n] := AX[i];
        Result.Data[n] := AData[i];
        Result.Fit[n] := AFit[i];
        if HasNoise then
            Result.Noise[n] := ANoise[i];
        Inc(n);
    end;
    SetLength(Result.X, n);
    SetLength(Result.Data, n);
    SetLength(Result.Fit, n);
    if HasNoise then
        SetLength(Result.Noise, n);
end;

end.
