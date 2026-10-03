// SPDX-License-Identifier: GPL-3.0-or-later
{ One definition of "how good is this fit", shared by the tests that compare the
  native Downhill Simplex against the gradient (Python/trf) minimizer.

  It is the same Poisson-weighted chi-square the application shows in its status bar
  (fit_service_statistics computes it the same way for either backend), so a test
  comparing the two engines measures exactly what the user sees. }
unit test_fit_quality;
{$mode objfpc}{$H+}
interface

uses fit_task;

{ The Poisson-weighted chi-square of the task's current calculated profile against
  its experimental profile. Not reduced: the raw weighted sum of squared residuals,
  so it does not depend on a parameter count and two engines are directly comparable. }
function WeightedChiSquare(Task: TFitTask): double;

implementation

uses points_set, fit_statistics;

function WeightedChiSquare(Task: TFitTask): double;
var
  Exp, Calc: TPointsSet;
  DataY, FitY: array of double;
  i: integer;
  Stats: TFitStatistics;
begin
  Exp := Task.ExpProfile;
  Calc := Task.GetCalcProfile;
  SetLength(DataY, Exp.PointsCount);
  SetLength(FitY, Exp.PointsCount);
  for i := 0 to Exp.PointsCount - 1 do
  begin
    DataY[i] := Exp.PointYCoord[i];
    FitY[i] := Calc.PointYCoord[i];
  end;
  //  ChiSquare is the weighted sum of squared residuals and does not depend on the
  //  varying-parameter count (that only scales the *reduced* value), so 0 is fine.
  Stats := ComputeFitStatistics(DataY, FitY, 0, WeightPoisson);
  Result := Stats.ChiSquare;
end;

end.
