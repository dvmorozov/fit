// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The application icon a build links: the framework's own.)

THE ICON AS A UNIT, NOT AS A $R DIRECTIVE IN Fit.lpr. Free Pascal resolves a $R
against the folder of the source that names it, and nowhere else - so the
one in Fit.lpr is Desktop/fit_icon.res in every project that compiles
Fit.lpr, and a product with an icon of its own could never link it. A unit is
looked for along the search path instead, where a product's composition folder
comes first: a product that carries its own icon has the build write its own
copy of this unit there (Sync-ProductIconUnit, tools/build-lib/icons.ps1),
naming the resource in its own folder. A product that carries none links this
one, and keeps Fit's icon.

WHY THIS UNIT LIVES IN Common AND NOT BESIDE Fit.lpr, as app_modules does: the
main source's own folder is searched before the search path, so a copy beside
Fit.lpr could never be shadowed. The resource itself stays in Desktop/, where
-Task set-icon writes it beside the Fit.ico it is compiled from.

ONE MAINICON. The projects embed no icon of their own (no <Icon> in the .lpi),
so this is the only one in the binary; two would be a duplicate resource.
}
unit app_icon;

{$mode objfpc}{$H+}

interface

implementation

{$R ../Desktop/fit_icon.res}

end.
