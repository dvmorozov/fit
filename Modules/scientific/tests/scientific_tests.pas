// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's tests, in one list.)

Linking a test unit registers its fixture. Every product that links the module
links this list through the module_tests the build writes for it (module.json
names it). The framework's own suite links none of it: a test whose subject is
this module's shapes, background, formula or diffraction data lives here, and
runs wherever the module is linked.
}
unit scientific_tests;

{$mode objfpc}{$H+}

interface

uses
    testcase_scientific_module,
    //  The diffraction angles: their arithmetic, their captions and the
    //  preference the peak shapes state for them.
    testcase_diffraction_axes,
    //  The wavelength and the background fraction through a project's round
    //  trip, in the module's own part and from a file written before.
    testcase_scientific_project_session,
    //  Reads and writes files in a folder of its own, so an integration test.
    testcase_user_formula_menu,
    //  The wavelength round trip, through the REST router and a real engine.
    testcase_wavelength_round_trip,
    //  The saved formulas' gestures in memory, and the module's small seams.
    testcase_user_formula_gestures, testcase_scientific_seams,
    //  The diffractometers' own formats; one real file read whole.
    testcase_diffraction_formats,
    //  Each peak shape: its arithmetic, its parameters, and the formula it
    //  hands a formula-based backend.
    testcase_pearson7, testcase_moffat, testcase_doniach_sunjic, testcase_emg,
    testcase_voigt, testcase_two_branches_pseudo_voigt,
    testcase_skewed_gaussian, testcase_step, testcase_curve_expression,
    //  The Moffat's width cap, moving with m, as the sidecar is asked again.
    testcase_moffat_sidecar_cap,
    //  What each shape says about itself, and how wide each one's peak is.
    testcase_curve_type_explanation_content, testcase_peak_widths,
    //  The background: its shapes, the search under the peaks, and the
    //  background fitted and subtracted through the REST router and the client.
    testcase_background_search, testcase_peak_search,
    testcase_background_shapes, testcase_background_model,
    //  The heat map of the Residual Spectrum tab.
    testcase_difference_scalogram,
    //  The user's own formula: the curve, its configuration, the dialogs'
    //  sequence (behind doubles), the stored library and the fit.
    testcase_user_points_set, testcase_curve_configuration,
    testcase_user_curve_wizard, testcase_user_curve_flow,
    testcase_curve_type_factory, testcase_user_curve_library,
    testcase_user_curve, testcase_user_curve_fit,
    //  Every shape through the native engine and the Python sidecar, and the
    //  real diffraction profiles (Data/2.dat): the losses, the Python
    //  minimiser and the automatic decomposition measured on them.
    testcase_python_backend_process, testcase_python_real_data,
    testcase_loss_real_data, testcase_auto_decomposition,
    //  Fits through a started compute server, which builds the model from a
    //  curve type only a module gives it: posted whole, stopped mid-run,
    //  animated, kept in the history, and a project opened against it.
    testcase_worker_process_peaks, testcase_live_stop, testcase_live_animation,
    testcase_model_history_live, testcase_live_project_open,
    //  The whole workflow through the client's transport against a started
    //  server, choosing between this module's peak shapes.
    testcase_http_fit_service,
    //  What used to sit among the framework's client and settings tests: the
    //  saved formula's component, the background fraction at the module's
    //  resource, and every menu command named in a guide holding this
    //  module's chapter.
    testcase_saved_formula_model, testcase_background_fraction_client,
    testcase_guide_menu_paths,
    //  This module's types as the framework's registry holds them: declared
    //  by the front door, Standard, built alike through their class, and
    //  measured by the noise of counts.
    testcase_scientific_curve_types;

implementation

end.
