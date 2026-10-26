// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The suite's counterpart to app_modules: which module tests are in it.)

The same extension point, applied to the test suite. A test unit registers its
fixture from its own initialization section, so it runs only if it is LINKED,
and this unit's uses clause is the only thing that links a module's tests.

A module ships its own copy of this unit beside its tests and puts that directory
first on the suite's unit search path. This one sits in a subdirectory of its
own, because a project's own directory is searched first and a copy in tests/
could never be overridden. The public suite then runs the public
tests and nothing else, and neither suite depends on the other's fixtures.
}
unit module_tests;

{$mode objfpc}{$H+}

interface

implementation

//  The published framework has no module, so there are no module tests to link.

end.
