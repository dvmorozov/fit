// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The "Difference structure" curve the model chart draws under the
difference.)

WHERE THE MODEL FALLS SHORT, ONE NUMBER A SAMPLE (Stage I3, the user's
request: "the user should see places where the model is not perfect"). At each
sample of the fit intervals, how much rhythm the difference holds there against
what noise would hold - its strongest octave's wavelet power over noise's
(residual_wavelet) - and the level above which a sample is named.

ONE MECHANISM, NOT A SECOND OPINION. The same samples (fit_window.FitWindowOf),
the same standardising (StandardisedResiduals) and the same judgement
(WaveletStructureOver over the same spans) the statistics shade stretches
with: the curve reaches its level at exactly the shaded samples, and a test
says so. Computed by the window rather than sent by the server, as the
spectrum is: a few milliseconds over what the window already holds, and no
array per sample added to every reply and every History entry.

Kept out of the form so a test calls it.
}
unit difference_structure;

{$mode objfpc}{$H+}

interface

uses
    residual_randomness, fit_window;

type
    TDifferenceStructure = record
        { The samples of the fit intervals, in order. }
        X: TDoubleArray;
        { At each: the strongest octave's power over noise's; one for noise. }
        Power: TDoubleArray;
        { A sample at or above it is named; Infinity when nothing was judged. }
        Level: double;
        { The data less the model at each, drawn on the strip's right axis. }
        Difference: TDoubleArray;
        { Per sample, the fit interval it lies in. }
        SpanOf: array of longint;
    end;

{ The curve over the samples of AX (the data AData, the model AFit beside them)
  inside the fit intervals ABoundsX gives as (start, end) pairs. Empty when
  there is nothing to judge - no sample, or intervals too short. }
function DifferenceStructure(const AX, AData, AFit,
    ABoundsX: array of double): TDifferenceStructure;
{ The same over a window already taken - what the client holds. }
function DifferenceStructureOf(const AWindow: TFitWindow): TDifferenceStructure;
{ AY over the samples of AStructure as the strip draws it: a point with no
  value (NaN, which the chart leaves undrawn) between two fit intervals, so no
  line joins them across samples nobody judged. }
procedure DrawnByInterval(const AStructure: TDifferenceStructure;
    const AY: TDoubleArray; out AX, AYDrawn: TDoubleArray);

implementation

uses
    Math, residual_wavelet;

function DifferenceStructure(const AX, AData, AFit,
    ABoundsX: array of double): TDifferenceStructure;
var
    W: TFitWindow;
begin
    Result := Default(TDifferenceStructure);
    Result.Level := Infinity;
    if Length(AX) = 0 then
        Exit;
    W := FitWindowOf(AX, AData, AFit, ABoundsX);
    Result := DifferenceStructureOf(W);
end;

function DifferenceStructureOf(const AWindow: TFitWindow): TDifferenceStructure;
var
    S: TWaveletStructure;
    i, k: longint;
begin
    Result := Default(TDifferenceStructure);
    Result.Level := Infinity;
    if Length(AWindow.X) = 0 then
        Exit;
    S := WaveletStructureOver(StandardisedResiduals(AWindow.Data, AWindow.Fit),
        AWindow.Spans);
    //  NOTHING JUDGED, NOTHING DRAWN: intervals too short to hold one octave
    //  leave every sample at zero, and a flat line of zeros would read as
    //  noise everywhere (found by the window check on its small model).
    if IsInfinite(S.Level) then
        Exit;
    Result.X := AWindow.X;
    SetLength(Result.SpanOf, Length(AWindow.X));
    for k := 0 to High(AWindow.Spans) do
        for i := AWindow.Spans[k].Lo to AWindow.Spans[k].Hi do
            Result.SpanOf[i] := k;
    Result.Power := S.Power;
    Result.Level := S.Level;
    SetLength(Result.Difference, Length(AWindow.X));
    for i := 0 to High(AWindow.X) do
        Result.Difference[i] := AWindow.Data[i] - AWindow.Fit[i];
end;

procedure DrawnByInterval(const AStructure: TDifferenceStructure;
    const AY: TDoubleArray; out AX, AYDrawn: TDoubleArray);
var
    i, n: longint;
begin
    SetLength(AX, 2 * Length(AStructure.X));
    SetLength(AYDrawn, Length(AX));
    n := 0;
    for i := 0 to High(AStructure.X) do
    begin
        //  THE BREAK, halfway across the gap: placed inside it so the chart
        //  orders it there, and with no value so nothing is drawn through it.
        if (i > 0) and (AStructure.SpanOf[i] <> AStructure.SpanOf[i - 1]) then
        begin
            AX[n] := (AStructure.X[i - 1] + AStructure.X[i]) / 2;
            AYDrawn[n] := NaN;
            Inc(n);
        end;
        AX[n] := AStructure.X[i];
        AYDrawn[n] := AY[i];
        Inc(n);
    end;
    SetLength(AX, n);
    SetLength(AYDrawn, n);
end;

end.
