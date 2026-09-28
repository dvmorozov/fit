// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the About box says about the licence: the notices GPLv3
section 5(d) asks an interactive program to show.)

"If the work has interactive user interfaces, each must display Appropriate
Legal Notices" - a copyright notice, that there is no warranty, that the work
may be conveyed under this licence and how to view it. The text is built here,
where it can be read, and the dialog only shows it.
}
unit testcase_legal_notices;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, legal_notices, project_identity;

type
    TLegalNoticesTest = class(TTestCase)
    published
        procedure TheCopyrightIsTheMaintainers;
        procedure TheLicenceIsNamedWithItsVersion;
        procedure ThereIsNoWarrantyAndItSaysSo;
        procedure ItSaysWhereTheSourceAndTheLicenceAre;
        procedure ItSaysWhereTheOtherComponentsLicencesAre;
        procedure ItNamesTheFrameworkNotTheWindow;
    end;

implementation

procedure TLegalNoticesTest.TheCopyrightIsTheMaintainers;
begin
    AssertTrue(Pos('Copyright (C) ' + ProjectMaintainer, LegalNotices) > 0);
end;

procedure TLegalNoticesTest.TheLicenceIsNamedWithItsVersion;
begin
    AssertTrue(Pos('GNU General Public License', LegalNotices) > 0);
    AssertTrue('the version, and the "or later" the SPDX line says',
        Pos('either version 3 of the License, or (at your option) any later version',
        LegalNotices) > 0);
end;

procedure TLegalNoticesTest.ThereIsNoWarrantyAndItSaysSo;
begin
    AssertTrue(Pos('WITHOUT ANY WARRANTY', LegalNotices) > 0);
end;

procedure TLegalNoticesTest.ItSaysWhereTheSourceAndTheLicenceAre;
begin
    AssertTrue('the source', Pos(ProjectSourceUrl, LegalNotices) > 0);
    AssertTrue('the licence text', Pos(ProjectLicenseUrl, LegalNotices) > 0);
end;

procedure TLegalNoticesTest.ItSaysWhereTheOtherComponentsLicencesAre;
begin
    AssertTrue(Pos('THIRD-PARTY.md', LegalNotices) > 0);
end;

{ THE FRAMEWORK'S LICENCE, BY ITS OWN NAME: a build carrying a module under
  other terms shows the same window with another title, and "<title> is free
  software" would then be untrue of it. }
procedure TLegalNoticesTest.ItNamesTheFrameworkNotTheWindow;
begin
    AssertTrue(Pos(ProjectName + ' is free software', LegalNotices) > 0);
end;

initialization
    RegisterTest('unit', TLegalNoticesTest);
end.
