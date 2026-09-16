// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This module's copy of the framework's module list.)

Overrides Common/app_modules.pas by sitting EARLIER on the project's unit search
path. Adding this directory to Fit.lpi and fit_server.lpi is the entire
difference between a build with this module and one without; no framework file
changes. See the framework's copy for why the extension point has this shape.
}
unit app_modules;

{$mode objfpc}{$H+}

interface

procedure RegisterAppModules;

implementation

uses
    example_module;

procedure RegisterAppModules;
begin
    RegisterExampleModule;
end;

end.
