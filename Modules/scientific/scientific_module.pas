// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's front door: the peak shapes and the
background shapes a measured profile is decomposed into.)

A MODULE, NOT THE FRAMEWORK'S OWN. The framework is a host for fitting any
data to any class of models; these shapes are one class - the decomposition of
a spectrum or a diffraction profile into peaks on a baseline - and a product for
another field leaves them out. They used to be named by the
engine, the window and the curve-type list; each now reaches them only through
the registry (TFitTask.CreatePatternInstance builds every registered type,
DefaultBackgroundCurveTypeId is a capability), so linking this unit is all it
takes to add them, and leaving it out is all it takes to have none.

THEIR OWNER STAYS 'Standard' (curve_type_registration.STANDARD_CURVE_OWNER),
the framework's own name: a model is one module's (fit_advice), and a peak, a
background and the user's formula - all three this module's since
2026-10-17 - have always gone in one model together.
}
unit scientific_module;

{$mode objfpc}{$H+}

interface

{ Registers the module: the diffraction angles and '.DAT' as a diffraction
  profile, the Residual Spectrum, its chapter of the guide, and the check that its curve types linked into this binary - each
  registers itself from its unit's initialization, which naming it below is
  what runs. Idempotent. }
procedure RegisterScientificModule;

implementation

uses
    curve_type_registration,
    //  The profile the shapes decompose: the angles of a diffraction pattern,
    //  and '.DAT' read as one.
    diffraction_profile, chart_tabs, explanation_registry, scientific_guide,
    curve_type_menu_extension, user_formula_menu,
    module_registry, scientific_app_module, axis_mode_registry,
    axis_parameter_store, wavelength_store,
    gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
    asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
    voigt_points_set, doniach_sunjic_points_set, emg_points_set,
    skewed_gaussian_points_set, moffat_points_set, pearson7_points_set,
    step_points_set,
    //  The background shapes: curves with no position, one per fit interval
    //  under the peaks (TNamedPointsSet.IsBackground).
    polynomial_background_points_set, exponential_background_points_set,
    power_law_background_points_set,
    //  The user's formula: a shape written as an expression, built and
    //  configured by the engine through its capability (IsDefinedByFormula).
    user_points_set;

procedure RegisterScientificModule;
begin
    //  The coordinates BEFORE anything that names one, and before the window
    //  restores the axis it last showed; the format before the framework's.
    RegisterDiffractionProfile;
    //  And the map of where, and at which rhythm, the difference between the
    //  data and a model of peaks on a baseline is not noise: the window's
    //  Residual Spectrum tab, which a build without this module does not show.
    OfferChartTab(ctSpectrum);
    //  And what all of it is: the chapter that explains the background, the
    //  angles, the wavelength and the map - the core guide names none of them.
    RegisterExplanationProvider(ScientificGuideProvider);
    //  The formulas the user saved, under Model > Curve Type > User - read
    //  from the settings folder only when that menu is first built, so the
    //  compute server, which registers this module too, never reads it.
    RegisterCurveTypeMenuExtension(UserFormulas);
    //  The wavelength the diffraction angles divide by: this module's state for
    //  each problem (its session, and its part of the project file), and where
    //  the client reads and writes it.
    RegisterAppModule(ScientificModule);
    RegisterAxisParameterStore(apWaveLength, WavelengthStore);
    ExpectCurveTypes(STANDARD_CURVE_OWNER, [
        TGaussPointsSet, TLorentzPointsSet, TPseudoVoigtPointsSet,
        TAsymPseudoVoigtPointsSet, T2BranchesPseudoVoigtPointsSet,
        TVoigtPointsSet, TDoniachSunjicPointsSet, TEMGPointsSet,
        TSkewedGaussianPointsSet, TMoffatPointsSet, TPearson7PointsSet,
        TStepPointsSet,
        TConstantBackgroundPointsSet, TLinearBackgroundPointsSet,
        TQuadraticBackgroundPointsSet, TExponentialBackgroundPointsSet,
        TPowerLawBackgroundPointsSet,
        TUserPointsSet]);
end;

end.
