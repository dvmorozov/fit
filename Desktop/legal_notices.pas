// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The legal notices the About box shows.)

GPLv3 SECTION 5(d): "If the work has interactive user interfaces, each must
display Appropriate Legal Notices" - a copyright notice, that there is no
warranty, that the work may be conveyed under the licence, and how to view the
licence. The wording is the licence's own recommended notice ("How to Apply
These Terms to Your New Programs"), not a paraphrase: a paraphrase of a legal
text is a second legal text.

THE FRAMEWORK BY ITS OWN NAME, not by the window's title. A build that carries
a module under other terms shows the same dialog with another title, and
"<title> is free software" would then be untrue of it; a module states its own
terms ABOVE these (AboutNotices).

THE MPL COMPONENTS ARE NAMED HERE, not only in THIRD-PARTY.md: the minimizer
and the grids are linked into the client under MPL-2.0, whose section 3.2(a)
asks whoever distributes the executable to say where their source is.

Built here rather than in the dialog so it can be read by a test: the dialog
only shows it (about_box_dialog).

Copyright (C) Dmitry Morozov
}
unit legal_notices;

{$mode objfpc}{$H+}

interface

uses
    explanation, notice_registry;

type
    TFindExplanation = function(const ATopic: string;
        out AExplanation: TExplanation): boolean;

{ The notices, as paragraphs separated by blank lines. }
function LegalNotices: string;

{ What the About box shows: each of ANotices whose topic AFind resolves, in
  registration order, then the framework's notices. A module's terms come
  FIRST: they were once placed after the framework's, as the ones true of
  fewer builds, and a window titled with a product's name that opened on "Fit
  is free software" read as a GPL grant for the product. A notice whose topic
  resolves nowhere is left out here and reported by NoticeFindings. }
function AboutNotices(const ANotices: TNotices; AFind: TFindExplanation): string;

implementation

uses
    project_identity;

function LegalNotices: string;
const
    Para = LineEnding + LineEnding;
begin
    Result :=
        'Copyright (C) ' + ProjectMaintainer + Para +
        ProjectName + ' is free software: you can redistribute it and/or ' +
        'modify it under the terms of the GNU General Public License as ' +
        'published by the Free Software Foundation, either version 3 of the ' +
        'License, or (at your option) any later version.' + Para +
        ProjectName + ' is distributed in the hope that it will be useful, ' +
        'but WITHOUT ANY WARRANTY; without even the implied warranty of ' +
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU ' +
        'General Public License for more details: ' + ProjectLicenseUrl + Para +
        'Source code: ' + ProjectSourceUrl + Para +
        'Its downhill simplex minimizer (fitminimizers) and its table grids ' +
        '(fitgrids) are licensed separately, under the Mozilla Public License ' +
        '2.0: https://www.mozilla.org/en-US/MPL/2.0/, whose text is installed ' +
        'with the program as LICENSE-MPL-2.0. Their source code: ' +
        'https://github.com/dvmorozov/fitminimizers and ' +
        'https://github.com/dvmorozov/fitgrids' + Para +
        'The other components it is built with are listed with their own ' +
        'licences in THIRD-PARTY.md, beside the source.';
end;

function AboutNotices(const ANotices: TNotices; AFind: TFindExplanation): string;
const
    Rule = LineEnding + LineEnding + '----' + LineEnding + LineEnding;
var
    i: integer;
    E: TExplanation;
begin
    Result := '';
    for i := 0 to High(ANotices) do
        if AFind(ANotices[i].Topic, E) then
            Result := Result + NoticeText(E) + Rule;
    Result := Result + LegalNotices;
end;

end.
