// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's tests, in one list.)

Linking a test unit registers its fixture. Every product that links the module
links this list through the module_tests the build writes for it (module.json
names it).
}
unit scientific_tests;

{$mode objfpc}{$H+}

interface

uses
    testcase_scientific_module,
    //  Reads and writes files in a folder of its own, so an integration test.
    testcase_user_formula_menu,
    //  The wavelength round trip, through the REST router and a real engine.
    testcase_wavelength_round_trip,
    //  The saved formulas' gestures in memory, and the module's small seams.
    testcase_user_formula_gestures, testcase_scientific_seams,
    //  The diffractometers' own formats; one real file read whole.
    testcase_diffraction_formats;

implementation

end.
