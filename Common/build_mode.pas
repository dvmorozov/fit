// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Whether this is a development build: one in which every feature
runs without a licence.)

THE BUILD MENU MAKES DEVELOPMENT BUILDS (FIT_DEV_BUILD); only a package is
built without it. A module that sells its features behind a licence asks
DevelopmentBuild and lets everything run when it is True, so the application
can be built, run, checked and recorded from the sources whatever a licence -
or a store that does not exist yet - would say. The framework names no module
and decides nothing about licences: this says only what kind of build it is.

A DEVELOPMENT BUILD CANNOT BE SHIPPED BY ACCIDENT. DevelopmentBuildNote's text
is compiled in only under FIT_DEV_BUILD, and the published packaging step
(scripts/build-app.ps1, Test-DevelopmentBuildBinary) refuses a client binary
that carries its marker - so a development binary left beside the sources is
refused by a packaging step that forgot to rebuild it. The tests are built
without the define (testcase_build_mode), so a licence is tested as it ships.
}
unit build_mode;

{$mode objfpc}{$H+}

interface

const
    { True in a build from the build menu, False in a package and in the tests. }
    DevelopmentBuild = {$IFDEF FIT_DEV_BUILD}True{$ELSE}False{$ENDIF};

{ What a development build says about itself, wherever a feature runs that a
  licence would otherwise hold back; '' in any other build. }
function DevelopmentBuildNote: string;

implementation

function DevelopmentBuildNote: string;
begin
{$IFDEF FIT_DEV_BUILD}
    //  THE MARKER packaging looks for, '(FIT_DEV_BUILD)': in this text and
    //  nowhere else in any source (dev_build.tests.ps1 holds that), so only a
    //  development build's binary can carry it.
    Result := 'This is a development build (FIT_DEV_BUILD): every feature runs ' +
        'without a licence. Packages are built without it.';
{$ELSE}
    Result := '';
{$ENDIF}
end;

end.
