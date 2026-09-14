// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the datasheet says, decided from the model alone.)

WHAT IT IS. The summary table is the only place the user can read the fit as
numbers: for every sample inside every fitting interval, the abscissa, the
measured value, the model's value, the difference, and each curve's contribution
under the row its own abscissa names. It is a pure function of five point sets,
and it used to be written directly into a `TStringGrid`, one `Cells[i, j] :=`
at a time, inside a unit that uses `Forms` and reaches into the main form by
name.

So none of it could be tested, and it is not simple. The number of columns is the
largest number of curves any one interval holds. The number of rows is the
samples in every interval plus a subheading each. A curve belongs to an interval
by containment - a rule that changed once already, when curves stopped spanning
their whole interval - and a curve's values are written under the profile rows
its own abscissae match, so a curve covering half an interval leaves the rest of
its column blank, which is the honest picture of where it exists.

Every one of those is a decision that is wrong quietly: a table with a column too
few silently drops a curve, and a row index off by one shifts a column against
the profile beside it. Both look like data.

WHAT STAYS IN THE VIEWER. Setting ColCount and RowCount on a grid, copying the
strings in, and making the tab visible. This class does not know what a grid is.
}
unit summary_table;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, points_set, title_points_set, curve_points_set,
    self_copied_component,
    //  For the four column captions. SINGLE-SOURCED with the series titles on
    //  the chart deliberately: the column a user reads and the curve they are
    //  looking at are the same thing, and two copies of the words would drift.
    fit_client;

type
    { Why a table could not be built. Distinguished because they are not the
      same event: an incomplete model is a caller that asked too early, and no
      intervals is the ordinary state of a fit whose first interval the user has
      not closed yet - which must empty the table rather than leave the last
      fit's numbers on screen. }
    TSummaryOutcome = (
        soBuilt,
        soModelIncomplete,
        soNoIntervals
        );

    { The datasheet's contents as text, addressed by column and row. }
    TSummaryTable = class(TObject)
    private
        //  [column, row]. Column-major because the grid is, and because a
        //  curve fills one column across many rows.
        FCells: array of array of string;
        FColCount: longint;
        FRowCount: longint;
        FIntervals: longint;
        procedure SetSize(ACols, ARows: longint);
    public
        constructor Create;

        { Builds the table from the model. Anything but soBuilt leaves the table
          empty, which is what the caller should then show. }
        function Build(AProfile: TTitlePointsSet; ACurves: TSelfCopiedCompList;
            AComputed, ADelta, ABounds: TTitlePointsSet): TSummaryOutcome;
        { Empties it. }
        procedure Clear;

        { The cell, or '' for any position outside the table - so a caller
          walking a grid that is larger than this cannot fault. }
        function CellAt(ACol, ARow: longint): string;

        property ColCount: longint read FColCount;
        property RowCount: longint read FRowCount;
        { How many fitting intervals the table covers. }
        property IntervalCount: longint read FIntervals;
    end;

{ How a number reads in the table.

  ffFixed rather than ffGeneral, for two reasons that are easy to lose: ffGeneral
  shows anything below 0.00001 in exponential form, and it prints 10000000.999999
  with every digit - and a column whose numbers have differing numbers of decimals
  is hard to read. ffFixed rounds the very small to zero by itself, which is
  accepted. }
function CurveValueText(AValue: double): string;

{ The largest number of curves any one interval holds - which is how many curve
  columns the table needs. Fewer would drop a curve from the table without
  saying so. }
function MaxCurvesInAnyInterval(ACurves: TSelfCopiedCompList;
    ABounds: TTitlePointsSet): longint;

{ How many profile samples fall inside the intervals, summed. }
function PointsInBounds(AProfile, ABounds: TTitlePointsSet): longint;

{ Whether a curve belongs to the interval starting at AStartX and ending at
  AFinishX.

  BY CONTAINMENT OF ITS FIRST ABSCISSA, not by "it starts exactly where the
  interval starts". That earlier test held only while every curve spanned its
  whole interval; a curve now carries just the samples it covers, so its first x
  is its own. The comparison is exact and that is safe rather than lucky: a
  curve's x values are COPIED from the profile when its window is made, so they
  are the same doubles and not merely equal ones. }
function CurveIsInInterval(ACurve: TCurvePointsSet;
    AStartX, AFinishX: double): boolean;

const
    { The four columns every table has, before the curve columns. }
    FixedColumnCount = 4;
    { What a subheading row says in its second and third columns. }
    IntervalHeading = 'Fit interval';
    IntervalHeadingNumber = 'number';

implementation

{ Whether there is anything to tabulate: measured data, and a computed profile
  and a difference of the same length as it.

  ASSIGNED IS NOT THE SAME AS FILLED, and the difference was a crash. The table
  reads the computed profile and the difference sample by sample AGAINST the
  measured one; all three exist from the moment a file is open and the last two
  hold nothing until something has been computed. So an interval placed before
  the first fit had this reading index 0 of an empty set, and the client aborted.
  A shorter one overruns the same way, one index later. }
function ModelIsBuilt(AProfile, AComputed, ADelta: TTitlePointsSet): boolean;
begin
    Result := (AProfile.PointsCount > 0) and
        (AComputed.PointsCount = AProfile.PointsCount) and
        (ADelta.PointsCount = AProfile.PointsCount);
end;

function CurveValueText(AValue: double): string;
begin
    Result := FloatToStrF(AValue, ffFixed, 8, 4);
end;

function CurveIsInInterval(ACurve: TCurvePointsSet;
    AStartX, AFinishX: double): boolean;
begin
    Result := Assigned(ACurve) and (ACurve.PointsCount > 0) and
        (ACurve.PointXCoord[0] >= AStartX) and
        (ACurve.PointXCoord[0] <= AFinishX);
end;

function MaxCurvesInAnyInterval(ACurves: TSelfCopiedCompList;
    ABounds: TTitlePointsSet): longint;
var
    i, j, CurCount: longint;
    StartX, FinishX: double;
begin
    Result := 0;
    if (not Assigned(ACurves)) or (not Assigned(ABounds)) then
        Exit;

    j := 0;
    while j + 1 < ABounds.PointsCount do
    begin
        CurCount := 0;
        StartX := ABounds.PointXCoord[j];
        FinishX := ABounds.PointXCoord[j + 1];
        for i := 0 to ACurves.Count - 1 do
            if CurveIsInInterval(TCurvePointsSet(ACurves.Items[i]),
                StartX, FinishX) then
                Inc(CurCount);
        if CurCount > Result then
            Result := CurCount;
        j := j + 2;
    end;
end;

function PointsInBounds(AProfile, ABounds: TTitlePointsSet): longint;
var
    j, StartIndex, EndIndex: longint;
begin
    Result := 0;
    if (not Assigned(AProfile)) or (not Assigned(ABounds)) then
        Exit;

    j := 0;
    while j + 1 < ABounds.PointsCount do
    begin
        StartIndex := AProfile.IndexOfValueX(ABounds.PointXCoord[j]);
        EndIndex := AProfile.IndexOfValueX(ABounds.PointXCoord[j + 1]);
        Result := Result + EndIndex - StartIndex + 1;
        j := j + 2;
    end;
end;

constructor TSummaryTable.Create;
begin
    inherited Create;
    Clear;
end;

procedure TSummaryTable.Clear;
begin
    SetSize(0, 0);
    FIntervals := 0;
end;

procedure TSummaryTable.SetSize(ACols, ARows: longint);
var
    i: longint;
begin
    if ACols < 0 then
        ACols := 0;
    if ARows < 0 then
        ARows := 0;
    FColCount := ACols;
    FRowCount := ARows;
    SetLength(FCells, ACols);
    for i := 0 to ACols - 1 do
    begin
        SetLength(FCells[i], ARows);
    end;
end;

function TSummaryTable.CellAt(ACol, ARow: longint): string;
begin
    //  OUT OF RANGE IS BLANK, not a fault: the grid this fills outlives any one
    //  table and may be larger than the current one while it is being resized.
    if (ACol < 0) or (ACol >= FColCount) or (ARow < 0) or (ARow >= FRowCount) then
        Result := ''
    else
        Result := FCells[ACol][ARow];
end;

function TSummaryTable.Build(AProfile: TTitlePointsSet;
    ACurves: TSelfCopiedCompList; AComputed, ADelta,
    ABounds: TTitlePointsSet): TSummaryOutcome;
var
    i, j, k: longint;
    LeftIndex, RightIndex, RowIndex, ColIndex, CurveRow: longint;
    Curve: TCurvePointsSet;
    StartX, FinishX: double;
begin
    Clear;

    if (not Assigned(AProfile)) or (not Assigned(ACurves)) or
        (not Assigned(AComputed)) or (not Assigned(ADelta)) or
        (not Assigned(ABounds)) then
        Exit(soModelIncomplete);

    //  ASSIGNED IS NOT THE SAME AS FILLED - see ModelIsBuilt.
    if not ModelIsBuilt(AProfile, AComputed, ADelta) then
        Exit(soModelIncomplete);

    //  AN ODD NUMBER OF BOUNDS IS A GESTURE IN PROGRESS, not a broken
    //  invariant, and this used to raise on it.
    //
    //  It cost a crash the user could reach in two clicks: picking fit
    //  intervals adds ONE bound per click (TFitClient.AddPointToRFactorBounds),
    //  and every one of them re-reads the model and refills this table - so the
    //  first click of every interval presents an odd count here. It raised, the
    //  exception reached the top level, and the client aborted. Nothing caught
    //  it because both halves are right on their own: a fit does need paired
    //  bounds, and a click does add one at a time.
    //
    //  So the RULE MOVES to where it belongs - a fit consuming bounds - and a
    //  table asked to draw a half-placed interval draws nothing, which is the
    //  same answer it already gives for a model that is not built yet.
    if ABounds.PointsCount mod 2 <> 0 then
        Exit(soModelIncomplete);

    if ABounds.PointsCount = 0 then
        Exit(soNoIntervals);

    FIntervals := ABounds.PointsCount div 2;
    SetSize(FixedColumnCount + MaxCurvesInAnyInterval(ACurves, ABounds),
        //  One heading row, one subheading per interval, and every sample.
        1 + PointsInBounds(AProfile, ABounds) + FIntervals);

    FCells[0][0] := PositionName;
    FCells[1][0] := AmplitudeName;
    FCells[2][0] := TotalAmplitudeName;
    FCells[3][0] := DifferenceName;

    i := 0;
    RowIndex := 1;
    while i + 1 < ABounds.PointsCount do
    begin
        StartX := ABounds.PointXCoord[i];

        //  The interval's own subheading row.
        FCells[1][RowIndex] := IntervalHeading;
        FCells[2][RowIndex] := IntervalHeadingNumber;
        FCells[3][RowIndex] := IntToStr(i div 2 + 1);
        Inc(RowIndex);

        LeftIndex := AProfile.IndexOfValueX(ABounds.PointXCoord[i]);
        RightIndex := AProfile.IndexOfValueX(ABounds.PointXCoord[i + 1]);
        FinishX := AProfile.PointXCoord[RightIndex];

        for j := LeftIndex to RightIndex do
        begin
            FCells[0][RowIndex + j - LeftIndex] :=
                CurveValueText(AProfile.PointXCoord[j]);
            FCells[1][RowIndex + j - LeftIndex] :=
                CurveValueText(AProfile.PointYCoord[j]);
            FCells[2][RowIndex + j - LeftIndex] :=
                CurveValueText(AComputed.PointYCoord[j]);
            FCells[3][RowIndex + j - LeftIndex] :=
                CurveValueText(ADelta.PointYCoord[j]);
        end;

        ColIndex := FixedColumnCount;
        for j := 0 to ACurves.Count - 1 do
        begin
            Curve := TCurvePointsSet(ACurves.Items[j]);
            if not CurveIsInInterval(Curve, StartX, FinishX) then
                Continue;

            FCells[ColIndex][0] := Curve.FTitle;
            //  Written under the row its own x names, so a curve that covers
            //  part of the interval lines up with the profile beside it and the
            //  rest of the column stays blank.
            for k := 0 to Curve.PointsCount - 1 do
            begin
                CurveRow := AProfile.IndexOfValueX(Curve.PointXCoord[k]);
                if (CurveRow >= LeftIndex) and (CurveRow <= RightIndex) then
                    FCells[ColIndex][RowIndex + CurveRow - LeftIndex] :=
                        CurveValueText(Curve.PointYCoord[k]);
            end;
            Inc(ColIndex);
        end;

        i := i + 2;
        RowIndex := RowIndex + RightIndex - LeftIndex + 1;
    end;

    Result := soBuilt;
end;

end.
