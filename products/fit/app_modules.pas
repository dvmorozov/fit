// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Registers every module this product contains, in both hosts.)

WRITTEN BY THE BUILD from the product's packaging/identity.json (build.modules)
and each module's module.json (fit/tools/build-lib/product_modules.ps1) - do
not edit it: change the module list and build.
}
unit app_modules;

{$mode objfpc}{$H+}

interface

procedure RegisterAppModules;

implementation

uses
    scientific_module;

procedure RegisterAppModules;
begin
    RegisterScientificModule;
end;

end.
