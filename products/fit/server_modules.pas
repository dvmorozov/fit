// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Registers what this product's modules add to the compute server alone.)

WRITTEN BY THE BUILD from the product's packaging/identity.json (build.modules)
and each module's module.json (fit/tools/build-lib/product_modules.ps1) - do
not edit it: change the module list and build.
}
unit server_modules;

{$mode objfpc}{$H+}

interface

procedure RegisterServerModules;

implementation

uses
    scientific_server_module;

procedure RegisterServerModules;
begin
    RegisterScientificServer;
end;

end.
