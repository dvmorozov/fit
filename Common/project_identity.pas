// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The project's public identity, as the program states it.)

THE SAME VALUES AS packaging/identity.json, which is where they are decided:
the package metadata, the installer and the site read that file directly. The
program cannot - the file is not shipped, and a desktop application that found
its identity beside its sources would work only where it was built - so they
are constants here, and tools/build-tests/identity.tests.ps1 fails when one
differs from the file.

No address: questions and reports go to the issue tracker.

Copyright (C) Dmitry Morozov
}
unit project_identity;

{$mode objfpc}{$H+}

interface

const
    ProjectName = 'Fit';
    ProjectSourceUrl = 'https://github.com/dvmorozov/fit';
    ProjectSupportUrl = 'https://github.com/dvmorozov/fit/issues';
    ProjectMaintainer = 'Dmitry Morozov';
    ProjectLicenseUrl = 'https://www.gnu.org/licenses/gpl-3.0.html';

implementation

end.
