// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What this module puts in front of a user, and what each thing is.)

THE MODULE'S CHAPTER OF THE GUIDE. The DOI source was the core Data chapter's
until 2026-10-20, when it left the framework for this module: a product with no
use for published research data - a price-analysis product - read about a
source it did not have. Its topic moved with it ('data/source-doi' became
'published-data/source-doi'): a topic is routed by its namespace to the one
provider that owns it, so a module cannot answer inside a core chapter.

ITS TOPICS LINK INTO THE CORE CHAPTERS, NEVER THE OTHER WAY, for the reason
scientific_guide gives: a core topic linking here would link nowhere in a
product without this module.
}
unit published_data_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, explanation_registry, static_explanations;

const
    PublishedDataNamespace = 'published-data';
    DoiTopic = 'published-data/source-doi';
    CatalogTopic = 'published-data/catalog';

function PublishedDataExplanations: TExplanations;
procedure RegisterPublishedDataExplanations;

implementation

const
    //  The core chapter this one links into, written out rather than taken
    //  from its unit: a chapter does not use another chapter's unit.
    DataSourcesTopic = 'data/data-sources';

var
    Provider: TStaticExplanationProvider = nil;

function DoiSource: TExplanation;
begin
    Result := NewExplanation(DoiTopic, 'Published data (DOI)',
        'The DOI source opens a Zenodo or figshare record by its DOI and lists ' +
        'the files it publishes.', esModelChoice);
    AddParagraph(Result, 'Paste a DOI - 10.5281/zenodo.1234567, or the same ' +
        'thing as a doi.org link - and Fit asks the DOI system where it ' +
        'resolves to, then asks that repository which files the record holds. ' +
        'Next goes straight to that list: choose a file and it is previewed ' +
        'and imported like any other.');
    AddParagraph(Result, 'Words rather than a DOI search Zenodo and figshare ' +
        'for datasets whose description matches them - datasets only, not ' +
        'papers or software, and only those holding a file Fit can read. Each ' +
        'row says which repository it is in, when it was published and how ' +
        'many of its files Fit can read. Choose one and press Next to see its ' +
        'files; Back returns to the records.');
    AddParagraph(Result, 'A page holds up to 25 records from Zenodo - the most it ' +
        'gives without an account - and ten ' +
        'from figshare, whose records are each asked for their files before ' +
        'they are listed. A row "More from Zenodo..." or "More from ' +
        'figshare..." at the end opens that repository''s next page.');
    AddParagraph(Result, 'When one repository cannot be reached, the other''s ' +
        'records are still listed, with a row saying which could not be ' +
        'searched; opening that row says why. Only both failing stops the ' +
        'search.');
    AddLimitation(Result, 'Only Zenodo and figshare publish a file list Fit ' +
        'can read. A DOI that resolves anywhere else is refused, naming where ' +
        'it went, so that the file can be fetched with the Web address source ' +
        'instead.');
    AddParagraph(Result, 'Only what Fit can read is listed. A record''s ' +
        'papers, pictures and anything else no reader here understands are ' +
        'left out, so the list is the data you can fit; a record holding ' +
        'nothing of the kind says how many files it did hold, rather than ' +
        'showing an empty list. Which files count follows the readers this ' +
        'build has: a module that adds a format adds its files here too.');
    AddParagraph(Result, 'An archive - .zip, .gz, .tar, .tar.gz or .tgz - is ' +
        'listed as a folder. Opening it downloads it once and lists the files ' +
        'inside that Fit can read, an archive inside it as another folder; ' +
        'choosing one takes that file alone, named as the archive names it. ' +
        'Nothing is unpacked onto the disk.');
    AddLimitation(Result, 'An archive is unpacked in memory, and a file in ' +
        'it larger than 256 MB is refused rather than unpacked. A .zip that ' +
        'is encrypted, or split over several files, cannot be opened.');
    AddLimitation(Result, 'A record may publish its data in a form no reader ' +
        'here understands - an instrument''s own binary format, say. Such ' +
        'files are not listed; open the record in a browser to see everything ' +
        'it holds.');
    AddRelated(Result, CatalogTopic);
    AddRelated(Result, DataSourcesTopic);
end;

function Catalog: TExplanation;
begin
    Result := NewExplanation(CatalogTopic, 'Data catalog',
        'The Data catalog browses the newest datasets of Zenodo and figshare ' +
        'by the kind of data Fit can read, like folders, with nothing to type.',
        esModelChoice);
    AddParagraph(Result, 'Choosing it lists one row for each kind of file ' +
        'this build reads - a diffractometer''s scans, spectra, tables of ' +
        'numbers, workbooks and the rest, each with its extensions - and a ' +
        'row for archives. Opening a row lists the newest datasets on both ' +
        'repositories holding such a file; opening a dataset lists the files ' +
        'Fit can read, and an archive among them opens like a folder. A ' +
        '"More from..." row at the end opens the next page. Back goes up a ' +
        'level.');
    AddParagraph(Result, 'Nothing here is a list kept by Fit: the rows are ' +
        'the readers this build has, so a module that adds a format adds a ' +
        'row, and everything under them is asked of the repositories when it ' +
        'is opened.');
    AddLimitation(Result, 'Plain .txt files are no row of their own, although ' +
        'they are read: nearly every dataset holds a README.txt, and the row ' +
        'would list them all. A .txt is still listed inside a dataset, and a ' +
        'word search with Published data (DOI) still finds datasets by them.');
    AddLimitation(Result, 'A dataset is listed for the name of a file it ' +
        'holds; whether that file is a measurement is seen in the preview.');
    AddLimitation(Result, 'How many datasets a row holds is not shown: ' +
        'counting would ask both repositories once for every row before the ' +
        'list could be drawn.');
    AddRelated(Result, DoiTopic);
    AddRelated(Result, DataSourcesTopic);
end;

function PublishedDataExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, DoiSource);
    AppendExplanation(Result, Catalog);
end;

procedure RegisterPublishedDataExplanations;
begin
    if not Assigned(Provider) then
        Provider := TStaticExplanationProvider.Create(PublishedDataNamespace,
            @PublishedDataExplanations);
    RegisterExplanationProvider(Provider);
end;

finalization
    Provider.Free;
end.
