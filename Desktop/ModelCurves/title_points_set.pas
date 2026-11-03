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
    Classes, points_set, SysUtils;

type
    { A profile, or any editable set drawn under a title. The title and the
      drawn baseline it held are TSampledValues' now, which a curve shares
      without being editable; this name is kept because every profile, pick
      set and bound list is declared with it. }
    TTitlePointsSet = class(TPointsSet)
    end;

{ Where point AIndex of APoints is drawn (TSampledValues.DrawnY). The chart's
  one question. }
function DrawnYOf(APoints: TSampledValues; AIndex: longint): double;

implementation

function DrawnYOf(APoints: TSampledValues; AIndex: longint): double;
begin
    Result := APoints.DrawnY(AIndex);
end;

end.
