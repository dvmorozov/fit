// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Registers what this product's modules add to the desktop client alone: the client's windows for them.)

WRITTEN BY THE BUILD from the product's packaging/identity.json (build.modules)
and each module's module.json (fit/tools/build-lib/product_modules.ps1) - do
not edit it: change the module list and build.
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
