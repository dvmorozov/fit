// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The peak search: the points of every peak in a profile, highest
first, down to a fraction of the highest.)

MOVED HERE FROM THE ENGINE on 2026-10-18 (TFitService.
ComputeCurvePositionsActual), its code unchanged but for taking the data and
the fraction as arguments: it is a peak shape's idea of where curves go, so
it is this module's, and a product without peaks carries none of it. The
engine asks for it by name (peak_search_registry); linking this unit
registers it.
}
unit peak_search;

{$mode objfpc}{$H+}

interface

uses
    points_set, title_points_set, named_points_set;

function FindPeakPoints(AData: TPointsSet; SearchMinimums: boolean;
    BackFactor: double): TTitlePointsSet;

{ The same, down to the background fraction this module keeps for the problem
  AHost serves (scientific_app_module.BackFactorFor): what the engine is
  given (peak_search_registry). }
function FindPeakPointsFor(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
{ The curve positions in AData for a type that wants AMode - each peak's top,
  each trough's bottom, or both - added to APositions, down to 1/ABackFactor of
  the highest. Moved from TFitService.ComputeCurvePositionsAlg, unchanged. }
procedure FindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    ABackFactor: double; APositions: TPointsSet);
{ The bounds of each peak in AData, added to ABounds. Moved from
  TFitService.ComputeCurveBoundsAlg, unchanged. }
procedure FindPeakBounds(AData: TPointsSet; ABackFactor: double;
    ABounds: TPointsSet);

{ The two above at the background fraction this module keeps for the problem
  AHost serves: what the engine is given. }
procedure FindCurvePositionsFor(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
procedure FindPeakBoundsFor(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);

implementation

uses
    checks, peak_search_registry, scientific_app_module;

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1E100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1E100;

function FindPeakPoints(AData: TPointsSet; SearchMinimums: boolean;
    BackFactor: double): TTitlePointsSet;
var
    Data: TPointsSet;
    ExtremumValue: double;
    ExtremumX0: double;
    ExtremumIndex: longint;
    ExtremumFound: boolean;
    LeftIndex, RightIndex: longint;
    // LeftIndex2, RightIndex2: LongInt;
    LeftX0, RightX0, Temp: double;
    i:    longint;
    GlobalExtremum: double;

    function GetBoundaryValue: double;
    begin
        if SearchMinimums then
            Result := MAX_VALUE
        else
            Result := MIN_VALUE;
    end;

    function IsFirstBetter(Value1, Value2: double): boolean;
    begin
        if SearchMinimums then
            Result := Value1 < Value2
        else
            Result := Value1 > Value2;
    end;

begin
    Data := AData;

    CheckAssigned(Data, 'the data record');
    Data.Sort;

    Result := TTitlePointsSet.Create(nil);
    try
        //  Repeatedly takes the lowest point among those not yet assigned to a
        //  peak.
        repeat
            ExtremumValue := GetBoundaryValue;
            ExtremumFound := False;

            for i := 0 to Data.PointsCount - 1 do
                if IsFirstBetter(Data.PointYCoord[i], ExtremumValue) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    ExtremumValue := Data.PointYCoord[i];
                    ExtremumX0    := Data.PointXCoord[i];
                    ExtremumIndex := i;
                    ExtremumFound := True;
                end;
            //  Prevents an endless loop when every point of the profile has
            //  been taken, which can happen when the lower bound of the peak
            //  search is set too small. THE EXIT MUST COME BEFORE THE POINT IS
            //  ADDED.
            if not ExtremumFound then
                Break;
            // pervyy naydennyy maksimum yavlyaetsya global'nym
            if Result.PointsCount = 0 then
                GlobalExtremum := ExtremumValue;
            Result.AddNewPoint(ExtremumX0, Data.PointYCoord[ExtremumIndex]);

            //  The peak's bounds, for computing the R-factor.
            Temp      := ExtremumValue;
            LeftIndex := ExtremumIndex;
            LeftX0    := ExtremumX0;
            // !!! trebuetsya zaschita ot dubley inache budet sboy sortirovki !!!
            for i := ExtremumIndex - 1 downto 0 do
                if not IsFirstBetter(Data.PointYCoord[i], Temp) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    Temp      := Data.PointYCoord[i];
                    LeftIndex := i;
                    LeftX0    := Data.PointXCoord[i];
                    Result.AddNewPoint(LeftX0, Data.PointYCoord[LeftIndex]);
                end
                else
                    Break//  THE COMPARISON MUST BE <, otherwise the peaks run
            //  into one another.
            ;
            //  Widening the peak to the left, artificially.
            (*
              if LeftIndex < 10 then LeftIndex2 := 0
              else LeftIndex2 := LeftIndex - 10;
              for i := LeftIndex - 1 downto LeftIndex2 do
              begin
              if (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
              begin
              LeftIndex := i;
              LeftX0 := Data.PointXCoord[i];
              Result.AddNewPoint(LeftX0, Data.PointYCoord[LeftIndex]);
              end
              end;
            *)
            Temp    := ExtremumValue;
            RightIndex := ExtremumIndex;
            RightX0 := ExtremumX0;
            for i := ExtremumIndex + 1 to Data.PointsCount - 1 do
                if not IsFirstBetter(Data.PointYCoord[i], Temp) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    Temp    := Data.PointYCoord[i];
                    RightIndex := i;
                    RightX0 := Data.PointXCoord[i];
                    Result.AddNewPoint(RightX0, Data.PointYCoord[RightIndex]);
                end
                else
                    Break;
            //  THE COMPARISON MUST BE <, otherwise the peaks run into one
            //  another.
            //  Widening the peak to the right, artificially.
            (*
              if RightIndex + 10 > Data.PointsCount - 1 then
              RightIndex2 := Data.PointsCount - 1
              else RightIndex2 := RightIndex + 10;
              for i := RightIndex + 1 to RightIndex2 do
              begin
              if (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
              begin
              RightIndex := i;
              RightX0 := Data.PointXCoord[i];
              Result.AddNewPoint(RightX0, Data.PointYCoord[RightIndex]);
              end
              else Break;
              end;
            *)

            //  ??? The noise in the background should be estimated and the
            //  threshold set as a fraction of the maximum that exceeds it, with a
            //  way to enter that value by hand. What is wanted here is not a
            //  fraction of the maximum but an absolute value - a background may
            //  be present - and it should vary along the profile. (Simply put:
            //  the background should be removed first.)
        until ExtremumValue < GlobalExtremum / BackFactor;
    except
        Result.Free;
        raise;
    end;
end;

procedure FindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    ABackFactor: double; APositions: TPointsSet);


    procedure SearchExtremums(Minimums: boolean);
    var
        Peaks: TPointsSet;

        procedure SelectExtremums;
        var
            i:    longint;
            Data: TPointsSet;
            PrevValue, CurValue: double;
            PeakFound: boolean;
            X:    double;
            LastPoint: boolean;

            function DerivativeChanged: boolean;
            begin
                if Minimums then
                    Result := CurValue > PrevValue
                else
                    Result := CurValue < PrevValue;
            end;

        begin
            CheckAssigned(Peaks, 'the peaks found in the profile');
            { Peaks collecton contains all data points having values different
              from some estimated average by defined value. }
            CheckThat(Peaks.PointsCount >= 3, 'even the narrowest peak needs three points to be located');

            Peaks.Sort; // !!!

            Data := AData;
            CheckAssigned(Data, 'the data record');
            Data.Sort;
            //  Of the Peaks that ComputeCurvePositionsActual returned, only the
            //  local maxima within each peak are kept.
            PeakFound := False;
            PrevValue := Peaks.PointYCoord[0];
            for i := 1 to Peaks.PointsCount - 1 do
            begin
                CurValue := Peaks.PointYCoord[i];
                if not PeakFound then
                begin
                    { Inflection point is searched. Last point is included
                      if data are going in the given direction. }
                    LastPoint := i = Peaks.PointsCount - 1;
                    if DerivativeChanged or (not DerivativeChanged and LastPoint)
                    then
                    begin
                        if LastPoint then
                        begin
                            X := Peaks.PointXCoord[i];
                            //  Guards against duplicates.
                            if APositions.IndexOfValueX(X) = -1 then
                                APositions.AddNewPoint(X, CurValue);
                        end
                        else
                        begin
                            X := Peaks.PointXCoord[i - 1];
                            //  Guards against duplicates.
                            if APositions.IndexOfValueX(X) = -1 then
                                APositions.AddNewPoint(X, PrevValue);
                        end;
                        PeakFound := True;
                    end;
                end
                else
                if not DerivativeChanged then
                    PeakFound := False;
                //  Look for a downward inflection.
                PrevValue     := CurValue;
            end;
        end;

    begin
        Peaks := FindPeakPoints(AData, Minimums, ABackFactor);
        try
            SelectExtremums;
        except
            Peaks.Free;
            raise;
        end;
        Peaks.Free;
    end;


begin
    case AMode of
        OnlyMaximums:
            SearchExtremums(False);
        OnlyMinimums:
            SearchExtremums(True);
        MaximumsAndMinimums:
        begin
            SearchExtremums(False);
            SearchExtremums(True);
        end;
    end;
end;

procedure FindPeakBounds(AData: TPointsSet; ABackFactor: double;
    ABounds: TPointsSet);
var
    i:     longint;
    Data:  TPointsSet;
    First: boolean;
    Peaks: TPointsSet;
    X:     double;
begin
    Peaks := FindPeakPoints(AData, False, ABackFactor);
    try
        CheckAssigned(Peaks, 'the peaks found in the profile');
        //  Even the narrowest peak should have at least 3 points.
        //  TODO: every case must be handled.
        // CheckThat(Peaks.PointsCount >= 3, 'even the narrowest peak needs three points to be located');
        if Peaks.PointsCount = 0 then
            Exit;
        Peaks.Sort; // !!!

        Data := AData;
        CheckAssigned(Data, 'the data record');

        Data.Sort;
        //  Of the Peaks that ComputeCurvePositionsActual returned, only the
        //  points bounding each peak are kept.
        First := False;
        for i := 0 to Data.PointsCount - 1 do
            if Peaks.IndexOfValueX(Data.PointXCoord[i]) <> -1 then
            begin
                //  A peak point.
                if not First then
                begin
                    //  The first is the left bound.
                    First := True;
                    //  Guards against duplicate points.
                    X     := Data.PointXCoord[i];
                    if ABounds.IndexOfValueX(X) = -1 then
                        ABounds.AddNewPoint(X, Data.PointYCoord[i]);
                end;
                //  The rest are skipped.
            end
            else
            if First then
            begin
                //  The previous point is the right bound.
                X := Data.PointXCoord[i - 1];
                if ABounds.IndexOfValueX(X) = -1 then
                    ABounds.AddNewPoint(X,
                        Data.PointYCoord[i - 1]);
                First := False;
            end;
            //  Not a peak point.
        if First then
        begin
            //  Every point was walked and no right bound was found, so the last
            //  point becomes one.
            X := Data.PointXCoord[i];
            //  Guards against duplicates.
            if ABounds.IndexOfValueX(X) = -1 then
                ABounds.AddNewPoint(X, Data.PointYCoord[i]);
            First := False;
        end;
    finally
        Peaks.Free;
    end;
end;


procedure FindCurvePositionsFor(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
begin
    FindCurvePositions(AData, AMode, BackFactorFor(AHost), APositions);
end;

procedure FindPeakBoundsFor(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
begin
    FindPeakBounds(AData, BackFactorFor(AHost), ABounds);
end;

function FindPeakPointsFor(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
begin
    Result := FindPeakPoints(AData, ASearchMinimums, BackFactorFor(AHost));
end;

initialization
    //  THE ENGINE ASKS FOR IT BY NAME (peak_search_registry): linking this unit
    //  is what gives a server - or a test binary - the peak search. The
    //  module's server front door names it (scientific_server_module).
    RegisterPeakSearch(@peak_search.FindPeakPointsFor,
        @peak_search.FindCurvePositionsFor, @peak_search.FindPeakBoundsFor);
end.
