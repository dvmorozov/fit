// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_minimizer;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, downhill_simplex_minimizer, DownhillSimplexServer, CombEnumerator, SimpMath;
type
  TParabola = class(TComponent)
  public
    P: array[0..1] of double;
    Step: array[0..1] of double;
    Idx: longint;
    function GetFunc: double;
    procedure ComputeFunc;
    function GetVariationStep: double;
    procedure SetVariationStep(NewStep: double);
    procedure SetFirstParam;
    procedure SetNextParam;
    function GetParam: double;
    procedure SetParam(NewVal: double);
    function EndOfCycle: boolean;
    procedure ShowCurMin;
  end;
  TMinimizerTest = class(TTestCase)
  published
    procedure FindsParabolaMinimum;
    procedure AParameterIsAddressedByItsIndex;
    procedure TheMinimizerIsOneDiscreteValueThatCannotBeMoved;
    procedure SphericalAndCartesianAgreeBothWays;
  end;
implementation
function TParabola.GetFunc: double; begin Result := Sqr(P[0]-3) + Sqr(P[1]-5); end;
procedure TParabola.ComputeFunc; begin end;
function TParabola.GetVariationStep: double; begin Result := Step[Idx]; end;
procedure TParabola.SetVariationStep(NewStep: double); begin Step[Idx] := NewStep; end;
procedure TParabola.SetFirstParam; begin Idx := 0; end;
procedure TParabola.SetNextParam; begin Inc(Idx); end;
function TParabola.GetParam: double; begin Result := P[Idx]; end;
procedure TParabola.SetParam(NewVal: double); begin P[Idx] := NewVal; end;
function TParabola.EndOfCycle: boolean; begin Result := Idx >= 2; end;
procedure TParabola.ShowCurMin; begin end;
procedure TMinimizerTest.FindsParabolaMinimum;
var M: TDownhillSimplexMinimizer; F: TParabola; ErrorCode: longint;
begin
  F := TParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    F.P[0]:=0; F.P[1]:=0; F.Step[0]:=1.0; F.Step[1]:=1.0; F.Idx:=0;
    M.OnGetFunc:=@F.GetFunc; M.OnComputeFunc:=@F.ComputeFunc;
    M.OnGetVariationStep:=@F.GetVariationStep; M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam; M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnGetParam:=@F.GetParam; M.OnSetParam:=@F.SetParam;
    M.OnEndOfCycle:=@F.EndOfCycle; M.OnShowCurMin:=@F.ShowCurMin;
    ErrorCode:=0; M.Minimize(ErrorCode);
    AssertEquals('p0 -> 3', 3.0, F.P[0], 1e-3);
    AssertEquals('p1 -> 5', 5.0, F.P[1], 1e-3);
  finally M.Free; F.Free; end;
end;
{ ADDRESSING ONE PARAMETER BY INDEX, which is how the caller reads and writes a
  single variation step without knowing how the minimizer walks its parameters.

  The pair had never been called. It matters because the walk is stateful - the
  minimizer selects a parameter by stepping from the first until the index is
  reached - so a getter that left the cursor somewhere else would make the NEXT
  read answer about a different parameter. Reading index 1 and then index 0 is
  what catches that, and reading a step back after writing it is what catches a
  setter addressing its neighbour. }
procedure TMinimizerTest.AParameterIsAddressedByItsIndex;
var M: TDownhillSimplexMinimizer; F: TParabola;
    Params: IDownhillRealParameters;
begin
  F := TParabola.Create(nil);
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    F.Step[0]:=1.0; F.Step[1]:=2.0; F.Idx:=0;
    M.OnGetVariationStep:=@F.GetVariationStep;
    M.OnSetVariationStep:=@F.SetVariationStep;
    M.OnSetNextParam:=@F.SetNextParam;
    M.OnSetFirstParam:=@F.SetFirstParam;
    M.OnEndOfCycle:=@F.EndOfCycle;

    //  THROUGH THE INTERFACE, because that is the whole surface: the optimiser
    //  reaches its host's parameters this way and no other, so the indexed
    //  property is the thing under test rather than a method on a class.
    Params := M;

    //  Read out of order, so a cursor left behind by the first read would show
    //  up in the second.
    AssertEquals('the second step', 2.0, Params.VariationStep[1], 1e-12);
    AssertEquals('the first step', 1.0, Params.VariationStep[0], 1e-12);

    //  Written by index, and read back by index.
    Params.VariationStep[1] := 7.5;
    AssertEquals('the second was written', 7.5, F.Step[1], 1e-12);
    AssertEquals('and the first was left alone', 1.0, F.Step[0], 1e-12);
    AssertEquals('read back', 7.5, Params.VariationStep[1], 1e-12);
  finally M.Free; F.Free; end;
end;

{ THE MINIMIZER AS A DISCRETE VALUE. The optimiser's enumerator drives every
  quantity it knows through IDiscretValue - "how many values have you, which is
  selected, select this one" - and this minimizer has exactly one, itself. So the
  answers are fixed: one value, index zero, and any attempt to select another is
  refused rather than quietly ignored.

  It reads like boilerplate and is not: an enumerator that believed there were two
  would walk a position that does not exist, and one that accepted a non-zero
  index would leave this object claiming to be something it is not. }
procedure TMinimizerTest.TheMinimizerIsOneDiscreteValueThatCannotBeMoved;
var M: TDownhillSimplexMinimizer; V: IDownhillRealParameters;
    Refused: boolean;
begin
  M := TDownhillSimplexMinimizer.Create(nil);
  try
    V := M;
    AssertEquals('one value', 1, V.NumberOfValues);
    AssertEquals('and it is the one selected', 0, V.ValueIndex);
    //  Selecting the only value it has is fine.
    V.ValueIndex := 0;
    AssertEquals('still the same', 0, V.ValueIndex);
    Refused := False;
    try
      V.ValueIndex := 1;
    except
      on E: Exception do
        Refused := True;
    end;
    AssertTrue('selecting a value it does not have is refused', Refused);
  finally M.Free; end;
end;

{ THE COORDINATE CONVERSION the optimiser's geometry rests on, both ways and over
  the cases the formula treats separately: z = 0, where the polar angle cannot be
  derived from a ratio and is taken as a right angle; and x = 0, where the azimuth
  is a right angle whose SIGN comes from y. Those two branches are the whole of
  the function's difficulty, and neither had been executed. }
procedure TMinimizerTest.SphericalAndCartesianAgreeBothWays;
var Theta, Phi, R, X, Y, Z: double;
begin
  //  An ordinary point: out and back.
  ConvertDekartToSpherical(1, 2, 3, Theta, Phi, R);
  AssertEquals('the radius', Sqrt(1.0 + 4.0 + 9.0), R, 1e-12);
  ConvertSphericalToDekart(Theta, Phi, R, X, Y, Z);
  AssertEquals('x survives the round trip', 1.0, X, 1e-9);
  AssertEquals('y survives it', 2.0, Y, 1e-9);
  AssertEquals('z survives it', 3.0, Z, 1e-9);

  //  z = 0: in the equatorial plane, which the formula answers directly rather
  //  than by a ratio it cannot form.
  ConvertDekartToSpherical(1, 0, 0, Theta, Phi, R);
  AssertEquals('a right angle from the pole', pi / 2, Theta, 1e-12);
  AssertEquals('and no azimuth', 0.0, Phi, 1e-12);

  //  x = 0: the azimuth is a right angle, and its SIGN is y's. Getting that
  //  wrong reflects the point through the origin.
  ConvertDekartToSpherical(0, 2, 0, Theta, Phi, R);
  AssertEquals('positive y looks one way', pi / 2, Phi, 1e-12);
  ConvertDekartToSpherical(0, -2, 0, Theta, Phi, R);
  AssertEquals('negative y the other', -pi / 2, Phi, 1e-12);
end;

initialization RegisterTest('unit', TMinimizerTest);
end.
