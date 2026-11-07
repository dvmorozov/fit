// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Reading the links and the title out of a page that was written for a
person, however its markup happens to be written.)

The NIST WebBook search is read from its pages (html_scan's header says why
that is a liability). A page's markup is not ours to choose: an anchor may
have no href, an href may be unquoted, a closing tag may be missing. Each case
here is one a page has, and each must give either the right link or none - a
wrong one is a substance the user did not ask for.
}
unit testcase_html_scan;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, html_scan;

type
    THtmlScanTest = class(TTestCase)
    published
        procedure AnAnchorWithoutAnHrefIsNoLink;
        procedure AnUnquotedHrefEndsAtItsSpaceOrAtTheTag;
        procedure AnAnchorNeverClosedKeepsItsHrefAndHasNoText;
        procedure ATitleNeverClosedIsNoTitle;
        procedure AParameterTheUrlLacksIsEmpty;
    end;

implementation

procedure THtmlScanTest.AnAnchorWithoutAnHrefIsNoLink;
var
    Links: THtmlLinks;
begin
    //  A named anchor, then a link, every link asked for: only the link -
    //  the anchor is not one with an href borrowed from the next tag along.
    Links := LinksContaining('<a name="top">Top</a> <a href="/cgi/cbook.cgi?ID=C64175">' +
        'Ethanol</a>', '');
    AssertEquals(1, Length(Links));
    AssertEquals('/cgi/cbook.cgi?ID=C64175', Links[0].Href);
    AssertEquals('Ethanol', Links[0].Text);
end;

procedure THtmlScanTest.AnUnquotedHrefEndsAtItsSpaceOrAtTheTag;
var
    Links: THtmlLinks;
begin
    Links := LinksContaining('<a href=/cgi/cbook.cgi?ID=C64175&amp;Units=SI ' +
        'class=x>Ethanol</a><a href=/cgi/cbook.cgi?ID=C71432>Benzene</a>', 'cbook');
    AssertEquals(2, Length(Links));
    AssertEquals('up to the space, entity undone',
        '/cgi/cbook.cgi?ID=C64175&Units=SI', Links[0].Href);
    AssertEquals('up to the tag', '/cgi/cbook.cgi?ID=C71432', Links[1].Href);
    AssertEquals('Benzene', Links[1].Text);
end;

procedure THtmlScanTest.AnAnchorNeverClosedKeepsItsHrefAndHasNoText;
var
    Links: THtmlLinks;
begin
    //  The rest of the page is not its text: where the link's words end is
    //  not known, and a page's tail as a substance's name would be shown.
    Links := LinksContaining('<a href="/cgi/cbook.cgi?ID=C64175">Ethanol and ' +
        'everything after it', 'cbook');
    AssertEquals(1, Length(Links));
    AssertEquals('/cgi/cbook.cgi?ID=C64175', Links[0].Href);
    AssertEquals('no text it could be sure of', '', Links[0].Text);
end;

procedure THtmlScanTest.ATitleNeverClosedIsNoTitle;
begin
    //  Rather than the rest of the page as a title.
    AssertEquals('', PageTitle('<html><title>Ethanol<body>everything else'));
end;

procedure THtmlScanTest.AParameterTheUrlLacksIsEmpty;
begin
    AssertEquals('', ParameterOf('/cgi/cbook.cgi?Name=ethanol', 'ID'));
end;

initialization
    RegisterTest('unit', THtmlScanTest);
end.
