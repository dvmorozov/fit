// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This module's curve types as the framework's registry holds them:
declared, owned, built and measured.)

The framework's own registry tests (testcase_curve_type_registration,
testcase_noise_rule) walk whatever is registered and name no module's type, so
the framework suite builds without any module. What they used to assert of
THESE types by name is asserted here, where the types are: that the module's
front door declares them, that they are Standard, that each peak shape built
through its class is the one its own constructor builds, and that a peak's
noise is the noise of counts.

Entered where the application enters: the window calls RegisterAppModules,
which in free Fit calls RegisterScientificModule.
}
unit testcase_scientific_curve_types;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    named_points_set, curve_points_set, curve_type_registration, noise_rule,
    int_curve_factory,
    fit_reduction, scientific_module,
    gauss_points_set, voigt_points_set, user_points_set,
    lorentz_points_set, pseudo_voigt_points_set, asym_pseudo_voigt_points_set,
    two_branches_pseudo_voigt_points_set, doniach_sunjic_points_set, emg_points_set,
    skewed_gaussian_points_set, moffat_points_set, pearson7_points_set, step_points_set,
    counts_noise_rule;

type
    TScientificCurveTypesTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        { The front door's ExpectCurveTypes names the ordinary peaks, the
          line shapes added later and the formula-defined user curve. }
        procedure ItDeclaresItsShapesAndTheUsersFormula;
        { Declared under the owner the framework calls Standard, so a peak, a
          background and the user's formula still go in one model. }
        procedure ItsTypesAreStandard;
        { THE REGISTRY BUILDS A TYPE THROUGH ITS CLASS (TFitTask's generic
          path: Create(nil), then the position) - so a peak shape must come out
          of that exactly as its own (owner, x0) constructor builds it, or the
          engine's list naming every peak class cannot go. }
        procedure APeakBuiltThroughItsClassIsTheOneItsOwnConstructorBuilds;
        { A diffraction peak's noise is the straight line in the level the
          framework's tool measures, and the figures weigh as counts. }
        procedure APeakOfCountsIsMeasuredByTheNoiseOfCounts;
    end;

implementation

procedure TScientificCurveTypesTest.SetUp;
begin
    RegisterScientificModule;
end;

procedure TScientificCurveTypesTest.ItDeclaresItsShapesAndTheUsersFormula;
var
    Names: TStringList;
begin
    Names := ExpectedCurveTypeNames;
    try
        AssertTrue('the ordinary peaks are covered',
            Names.IndexOf('TGaussPointsSet') >= 0);
        AssertTrue('and the formula-defined user curve',
            Names.IndexOf('TUserPointsSet') >= 0);
        AssertTrue('and a line shape added later',
            Names.IndexOf('TVoigtPointsSet') >= 0);
    finally
        Names.Free;
    end;
end;

procedure TScientificCurveTypesTest.ItsTypesAreStandard;
begin
    AssertEquals('a peak type', STANDARD_CURVE_OWNER,
        CurveTypeOwner(TGaussPointsSet.GetCurveTypeId));
    AssertEquals('the user''s own', STANDARD_CURVE_OWNER,
        CurveTypeOwner(TUserPointsSet.GetCurveTypeId));
end;

procedure TScientificCurveTypesTest.APeakBuiltThroughItsClassIsTheOneItsOwnConstructorBuilds;
const
    X0 = 3.25;

    procedure Same(AClass: TCurveClass; AExpected: TCurvePointsSet);
    var
        Built: TCurvePointsSet;
        i: longint;
        Name: string;
    begin
        Name := AClass.GetCurveTypeName;
        Built := AClass.Create(nil) as TCurvePointsSet;
        try
            AssertTrue(Name + ' built through its class is placed by a position',
                Built.Hasx0);
            Built.x0 := X0;
            AssertEquals(Name + ': the same parameters',
                AExpected.Parameters.Count, Built.Parameters.Count);
            for i := 0 to AExpected.Parameters.Count - 1 do
            begin
                AssertEquals(Name + ': parameter ' + IntToStr(i),
                    AExpected.Parameters[i].Name, Built.Parameters[i].Name);
                AssertEquals(Name + ': ' + AExpected.Parameters[i].Name,
                    AExpected.Parameters[i].Value, Built.Parameters[i].Value, 0);
                AssertTrue(Name + ': ' + AExpected.Parameters[i].Name + '''s kind',
                    AExpected.Parameters[i].Type_ = Built.Parameters[i].Type_);
            end;
        finally
            Built.Free;
            AExpected.Free;
        end;
    end;

    { The (owner, x0) constructor, then the position assigned as the engine
      assigns it to every curve it builds. }
    function Placed(ACurve: TCurvePointsSet): TCurvePointsSet;
    begin
        ACurve.x0 := X0;
        Result := ACurve;
    end;

begin
    Same(TGaussPointsSet, Placed(TGaussPointsSet.Create(nil, X0)));
    Same(TLorentzPointsSet, Placed(TLorentzPointsSet.Create(nil, X0)));
    Same(TPseudoVoigtPointsSet, Placed(TPseudoVoigtPointsSet.Create(nil, X0)));
    Same(TAsymPseudoVoigtPointsSet, Placed(TAsymPseudoVoigtPointsSet.Create(nil, X0)));
    Same(T2BranchesPseudoVoigtPointsSet, Placed(T2BranchesPseudoVoigtPointsSet.Create(nil, X0)));
    Same(TVoigtPointsSet, Placed(TVoigtPointsSet.Create(nil, X0)));
    Same(TDoniachSunjicPointsSet, Placed(TDoniachSunjicPointsSet.Create(nil, X0)));
    Same(TEmgPointsSet, Placed(TEmgPointsSet.Create(nil, X0)));
    Same(TSkewedGaussianPointsSet, Placed(TSkewedGaussianPointsSet.Create(nil, X0)));
    Same(TMoffatPointsSet, Placed(TMoffatPointsSet.Create(nil, X0)));
    Same(TPearson7PointsSet, Placed(TPearson7PointsSet.Create(nil, X0)));
    Same(TStepPointsSet, Placed(TStepPointsSet.Create(nil, X0)));
end;

procedure TScientificCurveTypesTest.APeakOfCountsIsMeasuredByTheNoiseOfCounts;
const
    Y: array[0..9] of double = (10, 12, 11, 30, 80, 120, 75, 28, 13, 11);
var
    V, Line: TNoiseVariances;
    i: longint;
begin
    AssertTrue('a Gaussian''s', TGaussPointsSet.NoiseRule = TCountsNoiseRule);
    AssertTrue('a Voigt''s', TVoigtPointsSet.NoiseRule = TCountsNoiseRule);
    AssertTrue('figures as counts', TCountsNoiseRule.WeighsFiguresAsCounts);
    AssertEquals('noise-rule/counts', NoiseRuleTopic(TCountsNoiseRule));
    V := TCountsNoiseRule.Variances(Y);
    Line := NoiseVariancesOf(Y);
    for i := 0 to High(Y) do
        AssertEquals(Format('at %d', [i]), Line[i], V[i], 0);
end;

initialization
    RegisterTest('unit', TScientificCurveTypesTest);
end.
