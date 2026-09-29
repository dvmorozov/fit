// SPDX-License-Identifier: GPL-3.0-or-later
unit position_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, points_set, special_curve_parameter, SysUtils;

type
    { The abciss coordinate of curve position (middle point). }
    TPositionCurveParameter = class(TSpecialCurveParameter)
    private
        { X0 variation boundaries. }
        Fx0Low, Fx0High: double;
        { The points the boundaries are to be read off, kept because they do not
          exist yet when this parameter is constructed - see SetValue. Borrowed;
          it is the curve that owns this parameter. }
        FPoints: TPointsSet;
        { The position the curve was seeded at, which is the centre the window is
          measured from. Kept for the same reason as FPoints: it is known at
          construction and cannot be used until then. }
        FSeed: double;
        { True once the boundaries have been read off real points. }
        FBoundsFixed: boolean;

        constructor Create; overload;
        procedure SetBoundaries(x0: double; PointsSet: TPointsSet);

    protected
        procedure SetValue(AValue: double); override;

    public
        constructor Create(x0: double; PointsSet: TPointsSet); overload;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
        { SetValue clamps x0 to [Fx0Low, Fx0High] - the samples either side of
          where the curve was seeded - so a fit, native or bounded backend,
          keeps each peak in the window the design allows.

          THOSE BOUNDS USED TO BE A SINGLE POINT, and so no fit could move a
          peak at all. See the comment on SetValue for what was wrong and what
          it cost; these two answer whatever the bounds currently are, which is
          unbounded until the curve has points and a position. }
        function GetMinValue: double; override;
        function GetMaxValue: double; override;

    end;

implementation

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1e100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1e100;

constructor TPositionCurveParameter.Create;
begin
    inherited Create;
    FName      := 'x0';
    FType      := VariablePosition;
    Fx0Low     := MIN_VALUE;
    Fx0High    := MAX_VALUE;
end;

constructor TPositionCurveParameter.Create(x0: double; PointsSet: TPointsSet);
begin
    inherited Create;
    FName      := 'x0';
    FType      := VariablePosition;
    Fx0Low     := MIN_VALUE;
    Fx0High    := MAX_VALUE;
    //  KEPT, NOT READ YET. Every curve type constructs this from inside its own
    //  constructor and passes ITSELF as the points set, which at that moment
    //  holds no points at all - so reading the boundaries here is what made
    //  them a single point. They are read on the first assignment instead,
    //  which is when the curve has both its window and its seed position.
    FPoints := PointsSet;
    FSeed   := x0;
    if Assigned(FPoints) and (FPoints.PointsCount > 0) then
    begin
        //  A caller that did hand over real points gets the boundaries now
        //  rather than later.
        SetBoundaries(FSeed, FPoints);
        FBoundsFixed := True;
    end;
end;

procedure TPositionCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TPositionCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TPositionCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TPositionCurveParameter.Create;
    CopyTo(Result);
end;

procedure TPositionCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    inherited;
    TPositionCurveParameter(Dest).Fx0Low := Fx0Low;
    TPositionCurveParameter(Dest).Fx0High := Fx0High;
    //  The copy inherits the WINDOW and the fact that it is settled, and NOT the
    //  points: it belongs to a different curve, and a copy that recomputed its
    //  bounds off the original's points would widen or narrow them behind the
    //  caller's back.
    TPositionCurveParameter(Dest).FBoundsFixed := FBoundsFixed;
    TPositionCurveParameter(Dest).FSeed := FSeed;
    TPositionCurveParameter(Dest).FPoints := nil;
end;

procedure TPositionCurveParameter.SetBoundaries(x0: double; PointsSet: TPointsSet);
var
    i: longint;
    TempDouble: double;
    Highindex, Lowindex: longint;
begin
    Fx0Low    := MIN_VALUE;
    Fx0High   := MAX_VALUE;
    Highindex := -1;
    Lowindex  := -1;

    { Searches of curve points closest to the given position
      x0 and return them as  boundaries of variation. }
    for i := 0 to PointsSet.PointsCount - 1 do
    begin
        TempDouble := PointsSet.PointXCoord[i];
        if TempDouble < x0 then
        begin
            if Abs(TempDouble - x0) < Abs(Fx0Low - x0) then
                Fx0Low := TempDouble;
            Lowindex   := i;
        end;
        if TempDouble > x0 then
        begin
            if Abs(TempDouble - x0) < Abs(Fx0High - x0) then
                Fx0High := TempDouble;
            Highindex   := i;
        end;
    end;

    if Lowindex = -1 then
        Fx0Low := x0;
    if Highindex = -1 then
        Fx0High := x0;
end;

procedure TPositionCurveParameter.SetValue(AValue: double);
begin
    //  THE FIRST ASSIGNMENT IS WHAT FIXES THE WINDOW, and it has to be, because
    //  a curve gets its points before it gets its position: the engine calls
    //  SetWindow and only then assigns x0. So this is the earliest moment at
    //  which both halves exist.
    //
    //  WHAT THIS REPAIRS. The boundaries were read in the constructor, from the
    //  curve itself, which held no points yet - so SetBoundaries found no sample
    //  either side of x0 and its own fallback collapsed both bounds onto x0. The
    //  clamp below was therefore to a single value, permanently, and NO FIT
    //  COULD MOVE A PEAK: the optimiser spent a dimension on a coordinate that
    //  always handed back its seed, and a position fitted by a backend was
    //  discarded in silence. A peak sat wherever the user clicked and nothing
    //  said so. See docs/contributing/findings.md.
    //
    //  Measured from the SEED and fixed ONCE, not from the incoming value and
    //  not again afterwards. Both matter. A window recomputed around each
    //  incoming value would bracket that value and so clamp nothing at all,
    //  and a window measured from whichever value happened to arrive first
    //  would depend on whether the caller assigns the seed before the
    //  optimiser's first trial - which the native and backend paths do not
    //  agree on.
    if (not FBoundsFixed) and Assigned(FPoints) and (FPoints.PointsCount > 0) then
    begin
        SetBoundaries(FSeed, FPoints);
        FBoundsFixed := True;
    end;

    FValue := AValue;
    { Checks boundary conditions. }
    if FValue < Fx0Low then
    begin
        FValue := Fx0Low;
        Exit;
    end;
    if FValue > Fx0High then
    begin
        FValue := Fx0High;
        Exit;
    end;
    FValue := AValue;

    WriteValueToLog(AValue);
end;

function TPositionCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

function TPositionCurveParameter.GetMinValue: double;
begin
    Result := Fx0Low;
end;

function TPositionCurveParameter.GetMaxValue: double;
begin
    Result := Fx0High;
end;

end.
