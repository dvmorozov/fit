// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This module's copy of the suite's module-test list.)

Overrides tests/no-modules/module_tests.pas the same way app_modules overrides
the framework's: by sitting earlier on the suite's unit search path. Naming a
test unit here links it, and linking it registers its fixture.
}
unit module_tests;

{$mode objfpc}{$H+}

interface

uses
    testcase_linear_ramp;

implementation

end.
