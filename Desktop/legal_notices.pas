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
terms beside these.

Built here rather than in the dialog so it can be read by a test: the dialog
only shows it (about_box_dialog).

Copyright (C) Dmitry Morozov
}
unit legal_notices;

{$mode objfpc}{$H+}

interface

{ The notices, as paragraphs separated by blank lines. }
function LegalNotices: string;

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
        'The components it is built with are listed with their own licences ' +
        'in THIRD-PARTY.md, beside the source.';
end;

end.
