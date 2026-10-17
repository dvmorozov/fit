// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(This product's identity, as the program states it.)

WRITTEN BY THE BUILD from packaging/identity.json (fit/tools/build-lib/
module_identity.ps1) - do not edit it: edit that file and build. It is
committed so that a fresh tree builds, and rewritten whenever the file changes.
}
unit product_identity;

{$mode objfpc}{$H+}

interface

const
    ProductProjectName = 'Fit';
    ProductHomepageUrl = 'https://dvmorozov.github.io/fit/';
    ProductSourceUrl = 'https://github.com/dvmorozov/fit';
    ProductSupportUrl = 'https://github.com/dvmorozov/fit/issues';
    ProductLicenseId = 'GPL-3.0-or-later';
    ProductLicenseUrl = 'https://www.gnu.org/licenses/gpl-3.0.html';
    ProductReadmeFile = 'README.md';
    ProductUpdateProduct = 'Fit';
    ProductUpdateFeedUrl = 'https://github.com/dvmorozov/fit/releases/latest/download/';
    ProductMaintainerName = 'Dmitry Morozov';
    ProductMaintainerId = 'dvmorozov';
    ProductMaintainerAddress = '7559490+dvmorozov@users.noreply.github.com';

implementation

end.
