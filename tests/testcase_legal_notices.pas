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
    Classes, SysUtils, fpcunit, testregistry, legal_notices, project_identity,
    explanation, notice_registry;

type
    TLegalNoticesTest = class(TTestCase)
    published
        procedure TheCopyrightIsTheMaintainers;
        procedure TheLicenceIsNamedWithItsVersion;
        procedure ThereIsNoWarrantyAndItSaysSo;
        procedure ItSaysWhereTheSourceAndTheLicenceAre;
        procedure ItSaysWhereTheOtherComponentsLicencesAre;
        procedure TheComponentsUnderMplAreNamedWithTheirSource;
        procedure ItNamesTheFrameworkNotTheWindow;
        procedure AModulesTermsComeBeforeTheFrameworks;
        procedure ANoticeWithNoWordsIsLeftOut;
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

{ MPL-2.0 SECTION 3.2(a): whoever distributes the executable form must tell
  its recipients how to obtain the source of the MPL-covered files. The
  minimizer and the grids are linked into the client and are MPL-2.0, not the
  framework's GPL; a reader of the About box should not have to infer that. }
procedure TLegalNoticesTest.TheComponentsUnderMplAreNamedWithTheirSource;
begin
    AssertTrue('the licence', Pos('Mozilla Public License 2.0', LegalNotices) > 0);
    AssertTrue('fitminimizers',
        Pos('https://github.com/dvmorozov/fitminimizers', LegalNotices) > 0);
    AssertTrue('fitgrids',
        Pos('https://github.com/dvmorozov/fitgrids', LegalNotices) > 0);
    AssertTrue('where its text is installed (Get-PackageDocs)',
        Pos('LICENSE-MPL-2.0', LegalNotices) > 0);
end;

{ THE FRAMEWORK'S LICENCE, BY ITS OWN NAME: a build carrying a module under
  other terms shows the same window with another title, and "<title> is free
  software" would then be untrue of it. }
procedure TLegalNoticesTest.ItNamesTheFrameworkNotTheWindow;
begin
    AssertTrue(Pos(ProjectName + ' is free software', LegalNotices) > 0);
end;

function FindPackNotice(const ATopic: string; out AExplanation: TExplanation): boolean;
begin
    Result := ATopic = 'pack/terms';
    if Result then
    begin
        AExplanation := NewExplanation('pack/terms', 'Pack terms',
            'The pack is licensed separately.', esModelChoice);
        AddParagraph(AExplanation, 'It gives no advice.');
    end;
end;

function Notice(const ATopic: string): TNotices;
begin
    SetLength(Result, 1);
    Result[0].Topic := ATopic;
    Result[0].RequiresAcknowledgement := True;
end;

{ A MODULE'S TERMS COME FIRST. They were placed after the framework's, on the
  reasoning that the framework's are true of every build; but a window titled
  with a product's name that opens on "Fit is free software ... GNU General
  Public License" reads as a GPL grant for the product, and was reported as
  exactly that. The terms of the build in hand are read first; the framework's,
  which are true of part of it, follow. }
procedure TLegalNoticesTest.AModulesTermsComeBeforeTheFrameworks;
var
    Text: string;
begin
    Text := AboutNotices(Notice('pack/terms'), @FindPackNotice);
    AssertEquals('the module''s first', 1, Pos('Pack terms', Text));
    AssertTrue(Pos('It gives no advice.', Text) > 0);
    AssertTrue('then the framework''s',
        Pos(LegalNotices, Text) > Pos('It gives no advice.', Text));
end;

procedure TLegalNoticesTest.ANoticeWithNoWordsIsLeftOut;
begin
    AssertEquals(LegalNotices, AboutNotices(Notice('pack/missing'), @FindPackNotice));
end;

initialization
    RegisterTest('unit', TLegalNoticesTest);
end.
