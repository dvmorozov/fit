// SPDX-License-Identifier: GPL-3.0-or-later
{ Layer-3 smoke test: verifies the curve-type registry is wired up and that the
  cross-platform "User defined" curve type self-registers. This is the class of
  wiring defect that unit/engine tests cannot see - it regressed several times
  while the user-curve subsystem was Windows-only / compiled out. Using the
  curve-type units here forces their initialization (self-registration) to run. }
unit testcase_user_curve;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  curve_types_singleton, int_curve_type_iterator,
  //  Referencing these units links them, so their registration runs.
  gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
  asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
  user_points_set;

type
  TUserCurveRegistryTest = class(TTestCase)
  private
    function RegisteredNames: TStringList;
  published
    procedure UserDefinedTypeIsRegistered;
    procedure StandardTypesAreRegistered;
  end;

implementation

{ Collects the names of every registered curve type via the public iterator. }
function TUserCurveRegistryTest.RegisteredNames: TStringList;
var
  Iter: ICurveTypeIterator;
begin
  Result := TStringList.Create;
  Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
  Iter.FirstCurveType;
  while True do
  begin
    Result.Add(Iter.GetCurveTypeName);
    if Iter.EndCurveType then Break
    else Iter.NextCurveType;
  end;
end;

procedure TUserCurveRegistryTest.UserDefinedTypeIsRegistered;
var
  Names: TStringList;
begin
  Names := RegisteredNames;
  try
    AssertTrue('"User defined" curve type is registered',
      Names.IndexOf('User Defined') >= 0);
    //  The name the menu shows must match the class' declared name.
    AssertEquals('declared name', 'User Defined',
      TUserPointsSet.GetCurveTypeName);
  finally
    Names.Free;
  end;
end;

procedure TUserCurveRegistryTest.StandardTypesAreRegistered;
var
  Names: TStringList;
begin
  Names := RegisteredNames;
  try
    AssertTrue('Gaussian registered', Names.IndexOf('Gaussian') >= 0);
    AssertTrue('Lorentzian registered', Names.IndexOf('Lorentzian') >= 0);
    AssertTrue('at least the five built-in types plus user-defined',
      Names.Count >= 6);
  finally
    Names.Free;
  end;
end;

initialization
  RegisterTest('unit', TUserCurveRegistryTest);
end.
