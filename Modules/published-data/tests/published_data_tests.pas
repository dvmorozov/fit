// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This module's tests, in one list.)

Linking a test unit registers its fixture, so naming it here is what puts it in
a suite. Named after the module rather than module_tests, which is a build's
list: a product of several modules writes its module_tests from module.json,
and that one links this.
}
unit published_data_tests;

{$mode objfpc}{$H+}

interface

uses
    testcase_doi_source, testcase_repository_filter, testcase_archives,
    testcase_published_search, testcase_catalog_source,
    //  Through the wizard to a real server: an integration test.
    testcase_published_e2e;

implementation

end.
