// SPDX-License-Identifier: GPL-3.0-or-later
{ The two-branch pseudo-Voigt curve computes what SimpMath defines, each side
  from its own width and mix.

  WHY THIS EXISTS. The curve's calculation and its three branch getters were
  covered only because some other test happened to fit with this type, and when
  that stopped they went dark without any test failing - fifteen lines, 98 % to
  77 %. Every sibling lineshape has a test of its own that calls ReCalc; this
  one had only the expression check, which never computes a point. }
unit testcase_two_branches_pseudo_voigt;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  SimpMath, two_branches_pseudo_voigt_points_set;
type
  TTwoBranchesPseudoVoigtTest = class(TTestCase)
  private
    function Computed(x: double): double;
  published
    procedure EachSideIsDrawnWithItsOwnWidthAndMix;
    procedure BothSidesReachTheHeightAtThePosition;
    procedure TheBranchParametersReadBackWhatWasSet;
  end;

implementation

const
  X0 = 10.0;
  PEAK_A = 100.0;
  { Left and right differ in both width and mix, so a branch computed from the
    other side's parameters cannot pass. }
  LEFT_SIGMA = 1.5;
  LEFT_ETA = 0.2;
  RIGHT_SIGMA = 3.0;
  RIGHT_ETA = 0.7;

procedure SetUpCurve(C: T2BranchesPseudoVoigtPointsSet);
begin
  C.ValuesByName['x0'] := X0;
  C.ValuesByName['A'] := PEAK_A;
  C.ValuesByName['sigma'] := LEFT_SIGMA;
  C.ValuesByName['eta'] := LEFT_ETA;
  C.ValuesByName['sigmaright'] := RIGHT_SIGMA;
  C.ValuesByName['etaright'] := RIGHT_ETA;
end;

function TTwoBranchesPseudoVoigtTest.Computed(x: double): double;
var
  C: T2BranchesPseudoVoigtPointsSet;
begin
  C := T2BranchesPseudoVoigtPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    SetUpCurve(C);
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

procedure TTwoBranchesPseudoVoigtTest.EachSideIsDrawnWithItsOwnWidthAndMix;
begin
  AssertEquals('left of the position, sigma and eta',
    TwoBranchesPseudoVoigtPoint(PEAK_A, LEFT_SIGMA, LEFT_ETA, RIGHT_SIGMA,
      RIGHT_ETA, X0, 8.5), Computed(8.5), 1e-9);
  AssertEquals('right of the position, sigmaright and etaright',
    TwoBranchesPseudoVoigtPoint(PEAK_A, LEFT_SIGMA, LEFT_ETA, RIGHT_SIGMA,
      RIGHT_ETA, X0, 11.5), Computed(11.5), 1e-9);
  //  The wider right branch is the higher one at the same distance, which is
  //  what the user sees and what a swapped pair of branches would get wrong.
  AssertTrue('the wider side falls more slowly', Computed(11.5) > Computed(8.5));
end;

procedure TTwoBranchesPseudoVoigtTest.BothSidesReachTheHeightAtThePosition;
begin
  AssertEquals('A is the height at x0', PEAK_A, Computed(X0), 1e-9);
end;

procedure TTwoBranchesPseudoVoigtTest.TheBranchParametersReadBackWhatWasSet;
var
  C: T2BranchesPseudoVoigtPointsSet;
begin
  C := T2BranchesPseudoVoigtPointsSet.Create(nil, X0);
  try
    SetUpCurve(C);
    AssertEquals('eta', LEFT_ETA, C.Eta, 1e-12);
    AssertEquals('sigmaright', RIGHT_SIGMA, C.SigmaRight, 1e-12);
    AssertEquals('etaright', RIGHT_ETA, C.EtaRight, 1e-12);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TTwoBranchesPseudoVoigtTest);
end.
