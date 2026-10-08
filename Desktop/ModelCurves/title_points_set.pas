// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class representing point set having title.)

Copyright (C) Dmitry Morozov
}
unit title_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, neutron_points_set, SysUtils;

type
    { Point set with title. TODO: must implement functionality of argument
      recalculation. }
    TTitlePointsSet = class(TNeutronPointsSet)
    public
        { FTitle which is displayed in chart legend. }
        FTitle: string;
        { What each point is DRAWN ON (TPointsData.Baseline), parallel to the
          points; empty - every set but a module's nested component - is zero.
          DISPLAY ONLY: PointYCoord stays the value the model computes, and
          only the chart reads this. Filled on the server from the curve's
          DrawnBaselineIn and on the client from the wire. }
        FDrawnBaseline: array of double;
        { Where point AIndex is drawn: its value, plus what it rests on when
          the baseline is in step with the points. }
        function DrawnY(AIndex: longint): double;

        procedure CopyParameters(Dest: TObject); override;
    end;

{ Where point AIndex of APoints is drawn: DrawnY for a set that can carry a
  baseline, its value for any other. The chart's one question. }
function DrawnYOf(APoints: TNeutronPointsSet; AIndex: longint): double;

implementation

{============================ TTitlePointsSet =================================}

procedure TTitlePointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TTitlePointsSet(Dest).FTitle := FTitle;
    TTitlePointsSet(Dest).FDrawnBaseline := Copy(FDrawnBaseline);
end;

function DrawnYOf(APoints: TNeutronPointsSet; AIndex: longint): double;
begin
    if APoints is TTitlePointsSet then
        Result := TTitlePointsSet(APoints).DrawnY(AIndex)
    else
        Result := APoints.PointYCoord[AIndex];
end;

function TTitlePointsSet.DrawnY(AIndex: longint): double;
begin
    Result := PointYCoord[AIndex];
    //  Out of step is drawn on zero rather than on a neighbour's level: the
    //  codec refuses such a baseline, and nothing else should produce one.
    if Length(FDrawnBaseline) = PointsCount then
        Result := Result + FDrawnBaseline[AIndex];
end;

end.
