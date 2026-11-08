// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What this module's own build adds to the desktop client alone.)

This module's own build is free Fit with this module added (see its
app_modules), so its client links free Fit's windows for the scientific
module as free Fit's does (products/fit/client_modules.pas). This module
itself has no window.
}
unit client_modules;

{$mode objfpc}{$H+}

interface

procedure RegisterClientModules;

implementation

uses
    scientific_forms;

procedure RegisterClientModules;
begin
    RegisterScientificForms;
end;

end.
