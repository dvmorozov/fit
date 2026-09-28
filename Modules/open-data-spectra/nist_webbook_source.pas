// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Infrared, mass and UV-visible spectra from the NIST Chemistry WebBook.)

WHAT THE USER GETS. A substance by name, the spectra NIST holds for it, and the
chosen one downloaded as JCAMP-DX and fitted like any other profile. Doing it by
hand means a search page, a species page, a spectrum page and a download link -
four pages in which the wrong species is easy to pick up.

THREE SHAPES OF ANSWER, and telling them apart is most of this unit:

  * the name matches ONE substance, and the search lands straight on its page;
  * the name matches SEVERAL, and the answer is a list;
  * the name matches none, and the page says so - which is a refusal here, not
    an empty result, because an empty list reads as "this build is broken".

READ FROM PAGES, NOT FROM AN API. The WebBook publishes no machine interface, so
the species pages are scanned for the anchors that name each kind of spectrum.
That is a liability and is stated as a limitation of this source rather than
hidden: when NIST changes its markup, this stops finding spectra and says so.

WHAT IS NOT DONE HERE. No identification, no matching a measured spectrum
against the library, no interpretation - the boundary the framework's data
access draws, and this module keeps it.
}
unit nist_webbook_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    TNistWebBookSource = class(TDataSource)
    private
        function SpeciesFrom(const APage: string): TDataSourceItems;
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Children(const AItem: TDataSourceItem): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

const
    NistSourceId = 'nist-webbook';
    NistTopic = 'spectra/source-nist';
    NistNameFieldId = 'name';
    NistBase = 'https://webbook.nist.gov/cgi/cbook.cgi';
    { How a species is named in an item's Ref. }
    NistSpeciesPrefix = 'nist:';

type
    { The kinds of spectrum this source offers, and how each is addressed. The
      anchor is what a species page carries when NIST holds that kind. }
    TNistSpectrumKind = record
        Anchor: string;
        TypeParameter: string;
        Title: string;
        Units: string;
    end;

const
    NistSpectrumKinds: array[0..2] of TNistSpectrumKind = (
        (Anchor: '#IR-Spec'; TypeParameter: 'IR';
         Title: 'Infrared spectrum'; Units: 'absorbance against 1/cm'),
        (Anchor: '#Mass-Spec'; TypeParameter: 'Mass';
         Title: 'Mass spectrum'; Units: 'relative intensity against m/z'),
        (Anchor: '#UV-Vis-Spec'; TypeParameter: 'UVVis';
         Title: 'UV-visible spectrum'; Units: 'absorbance against nm'));

{ The species id a WebBook page names, or '' - 'C71432' for benzene. }
function NistSpeciesId(const APage: string): string;

{ Whether the page is the "no such substance" answer. }
function NistFoundNothing(const APage: string): boolean;

{ Whether the page is a LIST of matches rather than one substance's page. }
function NistIsResultList(const APage: string): boolean;

{ Where a spectrum of this kind is downloaded from. }
function NistJcampUrl(const ASpeciesId, ATypeParameter: string): string;

implementation

uses
    html_scan, StrUtils;

function NistFoundNothing(const APage: string): boolean;
begin
    Result := Pos('not found', LowerCase(PageTitle(APage))) > 0;
end;

function NistIsResultList(const APage: string): boolean;
begin
    //  The WebBook titles its list of matches 'Search Results'. A single match
    //  is titled with the substance's own name, which is exactly the
    //  distinction that has to be made before anything is read from the page.
    Result := Pos('search results', LowerCase(PageTitle(APage))) > 0;
end;

function NistSpeciesId(const APage: string): string;
var
    Links: THtmlLinks;
    i: longint;
begin
    Result := '';
    Links := LinksContaining(APage, 'ID=');
    for i := 0 to High(Links) do
    begin
        Result := ParameterOf(Links[i].Href, 'ID');
        if Result <> '' then
            Exit;
    end;
end;

function NistJcampUrl(const ASpeciesId, ATypeParameter: string): string;
begin
    Result := NistBase + '?JCAMP=' + ASpeciesId + '&Index=0&Type=' +
        ATypeParameter;
end;

class function TNistWebBookSource.Info: TDataSourceInfo;
var
    Field: TInputFieldDecl;
begin
    Result.Id := NistSourceId;
    Result.Title := 'NIST Chemistry WebBook';
    Result.Category := 'Spectra';
    Result.Summary :=
        'Infrared, mass and UV-visible spectra of chemical substances.';
    Result.Topic := NistTopic;
    //  JCAMP-DX, which this module's own loader reads. The completeness walk
    //  checks that the loader is registered in whatever build offers this.
    Result.ProducesExtensions := '.JDX';
    Result.NeedsNetwork := True;
    //  A NAME, NOT AN IDENTIFIER: the WebBook knows hundreds of thousands of
    //  substances, so this cannot be a closed list - but the common ones are
    //  offered, so the first use needs nothing typed and nothing looked up.
    Field := SuggestedField(NistNameFieldId, 'Substance',
        'Choose one, or type any name the WebBook knows.',
        Choices([
            'Benzene', 'benzene',
            'Toluene', 'toluene',
            'Ethanol', 'ethanol',
            'Methanol', 'methanol',
            'Acetone', 'acetone',
            'Acetic acid', 'acetic acid',
            'Water', 'water',
            'Carbon dioxide', 'carbon dioxide',
            'Ammonia', 'ammonia',
            'Phenol', 'phenol',
            'Chloroform', 'chloroform',
            'Hexane', 'hexane']));
    Field.Topic := NistTopic;
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := Field;
    //  A substance holds several spectra, and which one is wanted is the
    //  user's choice.
    Result.HasContainers := True;
end;

function TNistWebBookSource.SpeciesFrom(const APage: string): TDataSourceItems;
var
    Links: THtmlLinks;
    Item: TDataSourceItem;
    Id, Seen: string;
    i: longint;
begin
    Result := nil;
    Seen := ';';
    Links := LinksContaining(APage, 'ID=C');
    for i := 0 to High(Links) do
    begin
        Id := ParameterOf(Links[i].Href, 'ID');
        if (Id = '') or (Pos(';' + Id + ';', Seen) > 0) then
            Continue;
        //  A species page links its own id many times over, once per section;
        //  a list links each match once. One rule reads both.
        Seen := Seen + Id + ';';
        Item := Default(TDataSourceItem);
        Item.Id := NistSpeciesPrefix + Id;
        Item.Ref := Item.Id;
        Item.Title := Links[i].Text;
        if Trim(Item.Title) = '' then
            Item.Title := Id;
        Item.Details := Id;
        Item.IsLeaf := False;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Item;
    end;
end;

function TNistWebBookSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Name_, Page: string;
    Item: TDataSourceItem;
    Id: string;
begin
    Result := nil;
    Name_ := Trim(QueryValue(AQuery, NistNameFieldId));
    if Name_ = '' then
        raise EDataSourceError.Create('Enter the name of a substance.');

    Page := FWeb.GetText(NistBase + '?Name=' + UrlEncoded(Name_) + '&Units=SI');

    if NistFoundNothing(Page) then
        //  A refusal, not an empty list: an empty list reads as "this build
        //  cannot search", which sends the user to look in the wrong place.
        raise EDataSourceError.Create('The NIST WebBook knows no substance ' +
            'called "' + Name_ + '". Try its chemical name - benzene rather ' +
            'than benzol - or a formula.');

    if NistIsResultList(Page) then
    begin
        Result := SpeciesFrom(Page);
        if Length(Result) = 0 then
            raise EDataSourceError.Create('The NIST WebBook answered with a ' +
                'list of matches that this version of Fit could not read. The ' +
                'site''s pages may have changed.');
        Exit;
    end;

    //  One substance: the search landed on its own page.
    Id := NistSpeciesId(Page);
    if Id = '' then
        raise EDataSourceError.Create('The NIST WebBook answered with a page ' +
            'naming no substance. The site''s pages may have changed.');
    Item := Default(TDataSourceItem);
    Item.Id := NistSpeciesPrefix + Id;
    Item.Ref := Item.Id;
    Item.Title := PageTitle(Page);
    Item.Details := Id;
    Item.IsLeaf := False;
    SetLength(Result, 1);
    Result[0] := Item;
end;

function TNistWebBookSource.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Id, Page: string;
    i: longint;
    Spectrum: TDataSourceItem;
begin
    Result := nil;
    Id := AItem.Ref;
    if StartsStr(NistSpeciesPrefix, Id) then
        Id := Copy(Id, Length(NistSpeciesPrefix) + 1, MaxInt);

    Page := FWeb.GetText(NistBase + '?ID=' + Id + '&Units=SI');
    for i := Low(NistSpectrumKinds) to High(NistSpectrumKinds) do
    begin
        //  THE PAGE'S OWN ANCHORS say which kinds NIST holds. Offering all
        //  three regardless would download three "Spectrum not found" files
        //  and blame the reader for them.
        if Pos(LowerCase(NistSpectrumKinds[i].Anchor), LowerCase(Page)) = 0 then
            Continue;
        Spectrum := Default(TDataSourceItem);
        Spectrum.Id := Id + '-' + NistSpectrumKinds[i].TypeParameter;
        Spectrum.Title := NistSpectrumKinds[i].Title;
        Spectrum.Details := NistSpectrumKinds[i].Units;
        Spectrum.FileName := Id + '-' + NistSpectrumKinds[i].TypeParameter + '.jdx';
        Spectrum.Ref := NistJcampUrl(Id, NistSpectrumKinds[i].TypeParameter);
        Spectrum.IsLeaf := True;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Spectrum;
    end;

    if Length(Result) = 0 then
        raise EDataSourceError.Create('The NIST WebBook holds no infrared, ' +
            'mass or UV-visible spectrum for ' + AItem.Title + '. It may hold ' +
            'other data for it, which Fit does not fetch.');
end;

function TNistWebBookSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    FWeb.Download(AItem.Ref, ADest);
    //  The name this source chose, which carries the '.jdx' that decides the
    //  reader: the address ends in a query and names nothing.
    Result := AItem.FileName;
end;

end.
