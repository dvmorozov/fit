// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The build mode: what the tests are built as.)

A build from the build menu is a development build - FIT_DEV_BUILD - in which a
module may run every feature without a licence. The tests are never one: they
are built without the define, so a module's licence rules are tested as they
ship rather than as a development build skips them.
}
unit testcase_build_mode;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, build_mode;

type
    TBuildModeTest = class(TTestCase)
    published
        procedure TheTestsAreBuiltAsWhatShips;
    end;

implementation

procedure TBuildModeTest.TheTestsAreBuiltAsWhatShips;
begin
    AssertFalse('the suite is not a development build', DevelopmentBuild);
    AssertEquals('and says nothing about being one', '', DevelopmentBuildNote);
end;

initialization
    RegisterTest('unit', TBuildModeTest);
end.
