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
    scientific_module, open_data_spectra_module;

procedure RegisterAppModules;
begin
    //  FREE FIT'S MODULES FIRST: this module's own build is free Fit with this
    //  module added, so the framework's suite it runs finds what free Fit has
    //  (products/fit lists them) - and a spectrum is fitted with its peaks.
    RegisterScientificModule;
    RegisterOpenDataSpectraModule;
end;

end.
