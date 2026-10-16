// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The heat map the Residual Spectrum tab draws: where, and at which
rhythm, the difference holds more than noise.)

WHY IT REPLACED THE PERIODOGRAM (Stage I4, the user: "I don't understand what
is displayed on this chart... the user should see places where the model is not
perfect"). Bartlett's cumulative periodogram said whether the whole difference
was noise and nothing about where. The wavelet scalogram (Torrence & Compo
1998) draws the difference's power over noise at every point and every rhythm:
the model's x across, the period in samples upwards, and a patch where the
model left a rhythm out.

THE SAME SAMPLES AND THE SAME TRANSFORM as the shaded stretches and the
Difference structure strip: the fit window (fit_window), standardised over it
(StandardisedResiduals), each fit interval transformed on its own
(residual_wavelet.WaveletScalogram). A cell is strong where its power reaches
ln 20 outside the cone of influence - Torrence & Compo's pointwise 95 % test -
but only the judgement of every cell together names one (below).

WHAT A CELL SAYS (the user, 2026-10-14: "totally unclear what it means"):
one of five things, each in one colour from the palette in force (app_theme)
and named in the map's key (ScalogramKey). The shaded stretches' colour is
EXACTLY the cells the judgement behind the stretches named (residual_wavelet
NamedAt) - not every cell over the pointwise level, which noise reaches in one
cell of twenty and which made the map claim more than the stretches did. Those
strong cells keep a grey of their own; noise is the background; the ends of an
interval, where its edges shape what is seen, and the gaps between intervals
are a disabled cell's grey, "not judged".

Kept out of the form so a test calls it.
}
unit difference_scalogram;

{$mode objfpc}{$H+}

interface

uses
    Types, residual_randomness, fit_window, app_theme;

type
    { The heat map's cells: a column per sample of the fit intervals, a row per
      period. }
    TScalogramGrid = record
        { The samples' x, ascending, and the interval each lies in. }
        X: TDoubleArray;
        SpanOf: array of longint;
        { The periods, in samples, ascending - the longest interval's. }
        Periods: TDoubleArray;
        { [row][column]: power over noise; NaN where that interval is too
          short to hold the period. }
        Power: array of TDoubleArray;
        { [row][column]: in an octave the judgement named there - what the
          shaded stretches are drawn from (residual_wavelet NamedAt). }
        Named: array of TBooleanDynArray;
        { The power a cell must reach: ln 20. }
        Level: double;
        { Per column, the period beyond which the interval's edges shape what
          is seen (the cone of influence). }
        Cone: TDoubleArray;
    end;

    { WHAT A CELL SAYS, most telling first - the order of the key. }
    TScalogramCell = (scMissed, scStrongAlone, scAboveAverage, scNoise,
        scNotJudged);

    TScalogramKeyItem = record
        Cell: TScalogramCell;
        Color: TThemeColor;
        Text: string;
    end;
    TScalogramKey = array of TScalogramKeyItem;

function ScalogramGridOf(const AWindow: TFitWindow): TScalogramGrid;
{ The cell at AX and APeriod: its power over noise, or NaN outside the
  intervals, between them, and beyond the periods held. }
function ScalogramValueAt(const AGrid: TScalogramGrid;
    AX, APeriod: double): double;
{ What the cell at AX and APeriod says. }
function ScalogramCellAt(const AGrid: TScalogramGrid;
    AX, APeriod: double): TScalogramCell;
{ Its colour, from palette AP. }
function ScalogramColorOf(const AP: TThemePalette;
    ACell: TScalogramCell): TThemeColor;
{ Every kind of cell, in order, its colour and its words: the map's key. }
function ScalogramKey(const AP: TThemePalette): TScalogramKey;

{ THE PERIOD AXIS is log2 of the period, so an octave is one unit and a
  rhythm of 16 samples sits as far above one of 8 as that one above 4: the
  marks are whole octaves, read as the period they stand for. }
procedure ScalogramPeriodMarks(AMin, AMax: double; out AStart, AStep: double);
function ScalogramPeriodMarkText(AValue: double): string;
{ The period, in samples, at position AValue of that axis. }
function ScalogramPeriodAt(AValue: double): double;

implementation

uses
    SysUtils, Math, residual_wavelet;

const
    { How far towards the axis colour the two shades of noise go: the faint
      one for noise above its average, the deeper one for a cell as strong as
      noise is one time in twenty. Short of the stretches' colour, and apart
      from a disabled cell's grey. This software's choice. }
    AboveAverageShade = 0.18;
    StrongAloneShade = 0.42;

function ScalogramGridOf(const AWindow: TFitWindow): TScalogramGrid;
var
    R, Part: TDoubleArray;
    Grids: array of TWaveletScalogram;
    Judged: TWaveletStructure;
    s, i, j, Rows, Longest, Lo, Len, Col: longint;
begin
    Result := Default(TScalogramGrid);
    Result.Level := -Ln(RANDOMNESS_SIGNIFICANCE);
    if Length(AWindow.X) = 0 then
        Exit;
    R := StandardisedResiduals(AWindow.Data, AWindow.Fit);
    SetLength(Grids, Length(AWindow.Spans));
    Rows := 0;
    Longest := -1;
    for s := 0 to High(AWindow.Spans) do
    begin
        Lo := AWindow.Spans[s].Lo;
        Len := AWindow.Spans[s].Hi - Lo + 1;
        SetLength(Part, Len);
        for i := 0 to Len - 1 do
            Part[i] := R[Lo + i];
        Grids[s] := WaveletScalogram(Part);
        if Length(Grids[s].Periods) > Rows then
        begin
            Rows := Length(Grids[s].Periods);
            Longest := s;
        end;
    end;
    Result.X := AWindow.X;
    SetLength(Result.SpanOf, Length(AWindow.X));
    SetLength(Result.Cone, Length(AWindow.X));
    if Longest >= 0 then
        Result.Periods := Grids[Longest].Periods;
    //  THE SAME JUDGEMENT AS THE SHADED STRETCHES, over the same samples and
    //  spans: so a cell in their colour is one of theirs, and none is missing.
    Judged := WaveletStructureOver(R, AWindow.Spans);
    Result.Named := Judged.NamedAt;
    SetLength(Result.Power, Rows);
    for j := 0 to Rows - 1 do
    begin
        SetLength(Result.Power[j], Length(AWindow.X));
        for i := 0 to High(Result.Power[j]) do
            Result.Power[j][i] := NaN;
    end;
    for s := 0 to High(AWindow.Spans) do
        for i := 0 to AWindow.Spans[s].Hi - AWindow.Spans[s].Lo do
        begin
            Col := AWindow.Spans[s].Lo + i;
            Result.SpanOf[Col] := s;
            Result.Cone[Col] := Grids[s].Cone[i];
            for j := 0 to High(Grids[s].Periods) do
            begin
                Result.Power[j][Col] := Grids[s].Power[j][i];
            end;
        end;
end;

{ The column and row nearest AX and APeriod; False outside the intervals,
  between them and beyond the periods held. }
function Locate(const AGrid: TScalogramGrid; AX, APeriod: double;
    out Col, Row: longint): boolean;
var
    Lo, Hi, Mid, j: longint;
    Half: double;
begin
    Result := False;
    Col := -1;
    Row := -1;
    if (Length(AGrid.X) = 0) or (Length(AGrid.Periods) = 0) or
       (AX < AGrid.X[0]) or (AX > AGrid.X[High(AGrid.X)]) or (APeriod <= 0) then
        Exit;
    //  The last column at or before AX.
    Lo := 0;
    Hi := High(AGrid.X);
    while Lo < Hi do
    begin
        Mid := (Lo + Hi + 1) div 2;
        if AGrid.X[Mid] <= AX then
            Lo := Mid
        else
            Hi := Mid - 1;
    end;
    Col := Lo;
    if (Col < High(AGrid.X)) and (AX > AGrid.X[Col]) then
    begin
        //  BETWEEN TWO INTERVALS nothing was judged.
        if AGrid.SpanOf[Col] <> AGrid.SpanOf[Col + 1] then
            Exit;
        if AGrid.X[Col + 1] - AX < AX - AGrid.X[Col] then
            Inc(Col);
    end;
    //  The nearest period, on the ladder's own log scale.
    Half := WAVELET_DJ / 2;
    if (Log2(APeriod) < Log2(AGrid.Periods[0]) - Half) or
       (Log2(APeriod) > Log2(AGrid.Periods[High(AGrid.Periods)]) + Half) then
        Exit;
    Row := 0;
    for j := 1 to High(AGrid.Periods) do
        if Abs(Log2(AGrid.Periods[j]) - Log2(APeriod)) <
           Abs(Log2(AGrid.Periods[Row]) - Log2(APeriod)) then
            Row := j;
    Result := True;
end;

function ScalogramValueAt(const AGrid: TScalogramGrid;
    AX, APeriod: double): double;
var
    Col, Row: longint;
begin
    Result := NaN;
    if Locate(AGrid, AX, APeriod, Col, Row) then
        Result := AGrid.Power[Row][Col];
end;

function ScalogramCellAt(const AGrid: TScalogramGrid;
    AX, APeriod: double): TScalogramCell;
var
    Col, Row: longint;
    V: double;
begin
    Result := scNotJudged;
    if not Locate(AGrid, AX, APeriod, Col, Row) then
        Exit;
    V := AGrid.Power[Row][Col];
    //  INSIDE THE CONE the interval's ends shape what is seen, and nothing
    //  there is named (residual_wavelet): shown as not judged, rather than
    //  in colours that would read as a finding. The dashed cone line this
    //  replaced was what nobody could read (the user, 2026-10-14).
    if IsNan(V) or (AGrid.Periods[Row] > AGrid.Cone[Col]) then
        Exit;
    if AGrid.Named[Row][Col] then
        Result := scMissed
    //  STRONG ALONE: over the pointwise 95 % level, but not named when every
    //  cell is judged together. Noise passes there one cell in twenty; in the
    //  stretches' colour such cells made the map claim more than the
    //  stretches did.
    else if V >= AGrid.Level then
        Result := scStrongAlone
    //  Noise averages one (residual_wavelet).
    else if V >= 1 then
        Result := scAboveAverage
    else
        Result := scNoise;
end;

function ScalogramColorOf(const AP: TThemePalette;
    ACell: TScalogramCell): TThemeColor;
begin
    case ACell of
        scMissed: Result := AP.NonRandom;
        scStrongAlone: Result := Mix(AP.Background, AP.Axis, StrongAloneShade);
        scAboveAverage: Result := Mix(AP.Background, AP.Axis, AboveAverageShade);
        scNoise: Result := AP.Background;
    else
        //  The grids' own colour for a cell that holds nothing to read.
        Result := AP.DisabledCell;
    end;
end;

function ScalogramKey(const AP: TThemePalette): TScalogramKey;
const
    Words: array[TScalogramCell] of string = (
        'The model misses a wave here',
        'Strong, but could be chance',
        'Noise, above its average',
        'Noise',
        'Not judged: an interval''s end, or no data');
var
    c: TScalogramCell;
begin
    SetLength(Result, Ord(High(TScalogramCell)) + 1);
    for c := Low(TScalogramCell) to High(TScalogramCell) do
    begin
        Result[Ord(c)].Cell := c;
        Result[Ord(c)].Color := ScalogramColorOf(AP, c);
        Result[Ord(c)].Text := Words[c];
    end;
end;

procedure ScalogramPeriodMarks(AMin, AMax: double; out AStart, AStep: double);
begin
    AStart := Ceil(AMin);
    AStep := 1;
end;

function ScalogramPeriodMarkText(AValue: double): string;
begin
    Result := IntToStr(Round(ScalogramPeriodAt(AValue)));
end;

function ScalogramPeriodAt(AValue: double): double;
begin
    Result := Power(2, AValue);
end;

end.
