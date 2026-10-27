// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The "Difference structure" curve the model chart draws under the
difference.)

WHERE THE MODEL FALLS SHORT, ONE NUMBER A SAMPLE (the user's
request: "the user should see places where the model is not perfect"). At each
sample of the fit intervals, how much rhythm the difference holds there against
what noise would hold - its strongest octave's wavelet power over noise's
(residual_wavelet) - and the level above which a sample is named.

ONE MECHANISM, NOT A SECOND OPINION. The same samples (fit_window.FitWindowOf),
the same standardising (StandardisedResiduals, by the noise the model's curve
types measure when one does - the window's Noise) and the same judgement
(WaveletStructureOver over the same spans) the statistics shade stretches
with: the curve reaches its level at exactly the shaded samples, and a test
says so.

COMPUTED BY THE SERVER, DRAWN BY THE WINDOW (the maintainer, 2026-10-27;
AGENTS.md, non-negotiable 1). The window computed this itself, from the
copies it holds for drawing, to keep an array per sample out of the replies -
and during a run those copies held no fit interval, so the strip covered the
whole profile while the server judged the intervals. The server computes it
over its own fit window (fit_service_statistics.ModelFitWindow) and sends it
on its own route, GET /difference-structure, and with every animated frame;
the window draws what it is sent. Its own route rather than /stats, so the
statistics reply and a History entry stay as they were.

Engine-free, so both hosts link it: the server to compute and send it, the
client to read it and draw it (DrawnByInterval).
}
unit difference_structure;

{$mode objfpc}{$H+}

interface

uses
    fpjson, residual_randomness, fit_window;

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

{ The curve over a fit window - the samples of the fit intervals, the data,
  the model and the noise of the model's rule beside them, as the server takes
  them (fit_service_statistics.ModelFitWindow). Empty when there is nothing to
  judge - no sample, no noise, or intervals too short. }
function DifferenceStructureOf(const AWindow: TFitWindow): TDifferenceStructure;
{ AY over the samples of AStructure as the strip draws it: a point with no
  value (NaN, which the chart leaves undrawn) between two fit intervals, so no
  line joins them across samples nobody judged. }
procedure DrawnByInterval(const AStructure: TDifferenceStructure;
    const AY: TDoubleArray; out AX, AYDrawn: TDoubleArray);

{ AStructure as the wire carries it: x, power, difference and spanOf, one per
  sample, and level - ABSENT when nothing was judged, since JSON has no
  infinity. The caller frees. }
function DifferenceStructureToJson(const AStructure: TDifferenceStructure):
    TJSONObject;
{ And back. False, with an empty structure, for anything that is not one:
  arrays of different lengths, or a member that is not an array of numbers. A
  structure with no samples reads as one that judged nothing. }
function DifferenceStructureFromJson(AData: TJSONData;
    out AStructure: TDifferenceStructure): boolean;

implementation

uses
    Math, residual_wavelet;

function DifferenceStructureOf(const AWindow: TFitWindow): TDifferenceStructure;
var
    S: TWaveletStructure;
    i, k: longint;
begin
    Result := Default(TDifferenceStructure);
    Result.Level := Infinity;
    //  NO RULE'S NOISE, NOTHING DRAWN: the model holds no curve whose type
    //  names a noise rule, and nothing else says what noise is.
    if (Length(AWindow.X) = 0) or (Length(AWindow.Noise) <> Length(AWindow.X)) then
        Exit;
    S := WaveletStructureOver(StandardisedResiduals(AWindow.Data, AWindow.Fit,
        AWindow.Noise), AWindow.Spans);
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

function NumbersJson(const AValues: array of double): TJSONArray;
var
    i: longint;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(AValues) do
        Result.Add(AValues[i]);
end;

function DifferenceStructureToJson(const AStructure: TDifferenceStructure):
    TJSONObject;
var
    Spans: TJSONArray;
    i: longint;
begin
    Result := TJSONObject.Create;
    Result.Add('x', NumbersJson(AStructure.X));
    Result.Add('power', NumbersJson(AStructure.Power));
    Result.Add('difference', NumbersJson(AStructure.Difference));
    Spans := TJSONArray.Create;
    for i := 0 to High(AStructure.SpanOf) do
        Spans.Add(AStructure.SpanOf[i]);
    Result.Add('spanOf', Spans);
    if not IsInfinite(AStructure.Level) and not IsNan(AStructure.Level) then
        Result.Add('level', AStructure.Level);
end;

{ The numbers of member AName of AObject; False when it is missing or holds
  anything else. }
function NumbersFrom(AObject: TJSONObject; const AName: string;
    out AValues: TDoubleArray): boolean;
var
    D: TJSONData;
    i: longint;
begin
    AValues := nil;
    D := AObject.Find(AName);
    Result := D is TJSONArray;
    if not Result then
        Exit;
    SetLength(AValues, TJSONArray(D).Count);
    for i := 0 to High(AValues) do
    begin
        if TJSONArray(D).Items[i].JSONType <> jtNumber then
        begin
            AValues := nil;
            Exit(False);
        end;
        AValues[i] := TJSONArray(D).Items[i].AsFloat;
    end;
end;

function DifferenceStructureFromJson(AData: TJSONData;
    out AStructure: TDifferenceStructure): boolean;
var
    O: TJSONObject;
    Spans: TDoubleArray;
    D: TJSONData;
    i: longint;
begin
    AStructure := Default(TDifferenceStructure);
    AStructure.Level := Infinity;
    Result := False;
    if not (AData is TJSONObject) then
        Exit;
    O := TJSONObject(AData);
    if not (NumbersFrom(O, 'x', AStructure.X) and
        NumbersFrom(O, 'power', AStructure.Power) and
        NumbersFrom(O, 'difference', AStructure.Difference) and
        NumbersFrom(O, 'spanOf', Spans)) or
       (Length(AStructure.Power) <> Length(AStructure.X)) or
       (Length(AStructure.Difference) <> Length(AStructure.X)) or
       (Length(Spans) <> Length(AStructure.X)) then
    begin
        AStructure := Default(TDifferenceStructure);
        AStructure.Level := Infinity;
        Exit;
    end;
    SetLength(AStructure.SpanOf, Length(Spans));
    for i := 0 to High(Spans) do
        AStructure.SpanOf[i] := Round(Spans[i]);
    D := O.Find('level');
    if (D <> nil) and (D.JSONType = jtNumber) then
        AStructure.Level := D.AsFloat;
    Result := True;
end;

end.
