// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Links the tests of every module this product contains.)

WRITTEN BY THE BUILD from the product's packaging/identity.json (build.modules)
and each module's module.json (fit/tools/build-lib/product_modules.ps1) - do
not edit it: change the module list and build.
}
unit module_tests;

{$mode objfpc}{$H+}

interface

uses
    scientific_tests, published_data_tests;

implementation

end.
