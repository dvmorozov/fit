// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place a build says which modules it contains.)

THIS IS THE EXTENSION POINT. A module is a directory of units plus a front-door
procedure that registers everything the module contributes. Nothing links that
directory until something names it, and this unit is the only thing that does.

TO ADD A MODULE to a build, do not edit any framework source. Put your own copy
of this unit in your module's directory, calling your front door:

    uses my_module;
    procedure RegisterAppModules;
    begin
        RegisterMyModule;
    end;

and put that directory FIRST on the project's unit search path. The project file
changes by one search-path entry; no other file changes at all, which is what
lets a private module live in its own repository and share no file with this one.

WHY THIS UNIT LIVES IN Common AND NOT BESIDE THE HOSTS. A project's own directory
is searched before its search path, so a copy sitting next to Fit.lpr could never
be overridden. It is deliberately somewhere no application project calls home.

IF THE PATH IS WRONG the framework's copy is linked instead and the module is
silently absent - so its front door ends with ExpectCurveTypes, which raises at
startup naming what did not link. A whole vertical was once dead in the compute
server for exactly this reason, with every test passing.
}
unit app_modules;

{$mode objfpc}{$H+}

interface

{ Registers every module this build contains. Called once by each host - the
  desktop client and the compute server - before any menu is built and before
  anything can create a curve. }
procedure RegisterAppModules;

implementation

procedure RegisterAppModules;
begin
    //  The published framework contains no module. This is not a placeholder for
    //  work that is missing: a build with no module is the ordinary case, and
    //  everything downstream - menus, panels, routes, the curve-type list - is
    //  derived from what registered, so nothing here needs to know.
end;

end.
