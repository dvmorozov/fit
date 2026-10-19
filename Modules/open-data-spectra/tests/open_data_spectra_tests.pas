// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This module's tests, in one list.)

Linking a test unit registers its fixture, so naming it here is what puts it in
a suite. Named after the module rather than module_tests, which is a build's
list: the module's own build (module_tests.pas beside it) and a product of
several modules (its module_tests, written from module.json) both link this one.
}
unit open_data_spectra_tests;

{$mode objfpc}{$H+}

interface

uses
    testcase_jcamp_dx, testcase_nist_source;

implementation

end.
