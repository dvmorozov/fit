// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the background's four coefficients the optimiser is varying.)

THE BACKGROUND IS A SHIFTED QUADRATIC - `CalcPolinom2(A, B, C, x0, x)` - and when
the fit is allowed to vary it, the optimiser addresses those four coefficients BY
NUMBER, as it addresses everything else. So there is a mapping from an index to a
coefficient, and it is written twice: once to read the current value and once to
write a proposed one.

TWO COPIES OF ONE ORDERING is the whole reason this exists. Written out at the two
sites, a reordering applied to one and not the other has the optimiser reading the
curvature and writing the offset: the fit runs, each step lands on the wrong
coefficient, and the background comes out as a shape nobody asked for with no
error anywhere. Neither copy is wrong on its own, which is why nothing catches it.

AND THE TWO SIDES ARE DELIBERATELY NOT SYMMETRIC. Two of the four are stored as
magnitudes, so a negative proposal is folded; the reader hands back what was
stored. That means the optimiser's step is reflected rather than refused for those
two, which is a property of the parameterisation and not a mistake - see
BackgroundParameterIsFolded.

The coefficients stay where they are, as four fields on the task: they are read
per point per evaluation, and a record would move them for no gain. What moves
here is the ORDER and the FOLDING, which is what a test can hold still.
}
unit background_parameters;

{$mode objfpc}{$H+}

interface

const
    { A, B, C and x0. The task checks a proposed index against this. }
    BACKGROUND_PARAMETER_COUNT = 4;

    { The indices, named. The optimiser knows only the numbers, so these exist
      for the two mapping sites and for anything that has to say which
      coefficient it means. }
    BACKGROUND_CURVATURE = 0;
    BACKGROUND_SLOPE = 1;
    BACKGROUND_OFFSET = 2;
    BACKGROUND_CENTRE = 3;

{ Whether the coefficient at AIndex is stored as a magnitude, so that a negative
  proposal is folded rather than kept.

  CURVATURE AND OFFSET ARE FOLDED. A background that curves downwards or sits
  below zero is not a background - it would be subtracted from the data and add
  signal that was never measured - so those two are held non-negative. The slope
  and the centre are signed, because a background may perfectly well fall across
  the range or be centred anywhere.

  The consequence for the optimiser is that a step into the folded half comes
  back reflected rather than refused, which is why this is exported: a caller
  reasoning about why a simplex stalled needs to know which coordinates are
  mirrors. }
function BackgroundParameterIsFolded(AIndex: longint): boolean;

{ The coefficient at AIndex, or 0 for an index outside the four.

  ZERO RATHER THAN A RAISE, because the caller has already checked the index
  against BACKGROUND_PARAMETER_COUNT and this runs inside the objective. }
function BackgroundParameter(AIndex: longint;
    const AA, AB, AC, AX0: double): double;

{ Sets the coefficient at AIndex to AValue, folding it when the index calls for
  that. An index outside the four changes nothing. }
procedure SetBackgroundParameter(AIndex: longint; AValue: double;
    var AA, AB, AC, AX0: double);

implementation

uses
    Math;

function BackgroundParameterIsFolded(AIndex: longint): boolean;
begin
    Result := (AIndex = BACKGROUND_CURVATURE) or (AIndex = BACKGROUND_OFFSET);
end;

function BackgroundParameter(AIndex: longint;
    const AA, AB, AC, AX0: double): double;
begin
    case AIndex of
        BACKGROUND_CURVATURE: Result := AA;
        BACKGROUND_SLOPE: Result := AB;
        BACKGROUND_OFFSET: Result := AC;
        BACKGROUND_CENTRE: Result := AX0;
        else
            Result := 0;
    end;
end;

procedure SetBackgroundParameter(AIndex: longint; AValue: double;
    var AA, AB, AC, AX0: double);
begin
    //  FOLDED HERE AND NOT IN THE READER, deliberately: what is stored is the
    //  magnitude, so the reader has nothing left to fold and folding it twice
    //  would hide a negative that had got in some other way.
    if BackgroundParameterIsFolded(AIndex) then
        AValue := Abs(AValue);
    case AIndex of
        BACKGROUND_CURVATURE: AA := AValue;
        BACKGROUND_SLOPE: AB := AValue;
        BACKGROUND_OFFSET: AC := AValue;
        BACKGROUND_CENTRE: AX0 := AValue;
    end;
end;

end.
