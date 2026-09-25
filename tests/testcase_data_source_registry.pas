// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The source registry's own rules, and the walk that keeps a build honest.)

WHAT THIS BINARY CAN AND CANNOT CHECK, as testcase_data_loader_registry states
it: this binary links every source unit, so "the sources are registered" is true
by construction here and would be true in a build whose application never called
RegisterAllDataSources. So what is tested is the registry's OWN rules, plus the
completeness walk - which is the part that keeps working for sources written
after this file, in module repositories this one has never heard of.
}
unit testcase_data_source_registry;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_registry, data_source_registration,
    data_source_findings, data_loader_registration, explanation,
    explanation_registry, static_explanations, guide_data,
    int_web_client, samples_source, url_source, doi_source;

type
    TDataSourceRegistryTest = class(TTestCase)
    published
        procedure AKnownIdResolvesToItsSource;
        procedure AnUnknownIdResolvesToNothing;
        procedure TheSameSourceRegisteredAgainIsANoOp;
        procedure TwoSourcesCannotClaimOneId;
        procedure ASourceWithNoClassIsRefused;
        procedure ASourceWithNoIdIsRefused;
        procedure CategoriesAreDerivedFromWhatRegistered;
        //  The walk.
        procedure EveryRegisteredSourceIsComplete;
        procedure ASourceWithoutAnExplanationIsReported;
        procedure ASourceProducingUnreadableFilesIsReported;
        procedure ASourceWithANamelessQueryFieldIsReported;
        procedure ASourceWithNoTitleIsReported;
        procedure ASourceWithNoCategoryIsReported;
        procedure ASourceWithNoSummaryIsReported;
        procedure ASourceNamingATopicNothingExplainsIsReported;
        procedure ASourceSayingNothingAboutWhatItProducesIsReported;
        procedure AChoiceWithNoNameWouldShowAnIdentifierAndIsReported;
        procedure AFieldAskedWhenAFieldNobodyDeclaresIsReported;
        procedure AFieldAskedForAChoiceNobodyOffersIsReported;
        procedure TwoSourcesUnderOneIdAreReportedByTheWalkToo;
        procedure ASourceWithNoIdIsReportedByTheWalkToo;
    end;

implementation

type
    { Sources that exist only to be registered wrongly, or walked. They never
      search: the registration rules are what is under test. }
    TNamelessSource = class(TDataSource)
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string; override;
    end;

    { Claims the id the samples source already has. }
    TImpostorSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TUnexplainedSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TUnreadableSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TBadFieldSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    { Each of these is complete APART FROM ONE THING, so the finding it
      produces names that thing and nothing else. }
    TNamelessTitleSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TNoCategorySource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TNoSummarySource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TUnknownTopicSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TSilentAboutFilesSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    TNamelessChoiceSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    { Dates asked only while a field that does not exist says so - a field
      greyed for good. }
    TBoundToNothingSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

    { Dates asked only for a range nobody can choose. }
    TBoundToNoChoiceSource = class(TNamelessSource)
    public
        class function Info: TDataSourceInfo; override;
    end;

class function TNamelessSource.Info: TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := '';
end;

function TNamelessSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
begin
    Result := nil;
end;

function TNamelessSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    Result := '';
end;

class function TImpostorSource.Info: TDataSourceInfo;
begin
    Result := inherited Info;
    Result.Id := SamplesSourceId;
end;

class function TUnexplainedSource.Info: TDataSourceInfo;
begin
    Result := inherited Info;
    Result.Id := 'test-unexplained';
    Result.Title := 'Unexplained';
    Result.Category := 'Test';
    Result.Summary := 'Registered with no explanation topic.';
    Result.Topic := '';
    Result.ProducesExtensions := '.DAT';
end;

class function TUnreadableSource.Info: TDataSourceInfo;
begin
    Result := inherited Info;
    Result.Id := 'test-unreadable';
    Result.Title := 'Unreadable';
    Result.Category := 'Test';
    Result.Summary := 'Produces a kind of file nothing here reads.';
    Result.Topic := 'data/data-sources';
    Result.ProducesExtensions := '.HDF5';
end;

class function TBadFieldSource.Info: TDataSourceInfo;
begin
    Result := inherited Info;
    Result.Id := 'test-bad-field';
    Result.Title := 'Bad field';
    Result.Category := 'Test';
    Result.Summary := 'Declares a query field with no caption.';
    Result.Topic := 'data/data-sources';
    Result.ProducesExtensions := '.DAT';
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := TextField('', '', 'no id and no caption');
end;

var
    DataChapter: TStaticExplanationProvider = nil;

{ The guide's Data chapter, where the framework's own source topics live.

  NOT REGISTERED IN THE PROCESS REGISTRY, and that is deliberate: registering it
  here would put the Data chapter in the registry BEFORE the application's own
  call does, and the index is built in registration order - so this test would
  silently reorder the user guide. (It did: "Data comes after the chapter before
  it" failed in testcase_client_explanations until this was a local lookup.)
  Nothing about a source's completeness needs the global registry. }
procedure EnsureDataChapter;
begin
    if DataChapter <> nil then
        Exit;
    DataChapter := TStaticExplanationProvider.Create(DataNamespace,
        @DataExplanations);
end;

function DataChapterProviders: TExplanationProviders;
begin
    EnsureDataChapter;
    Result := nil;
    SetLength(Result, 1);
    Result[0] := DataChapter;
end;

{ A source that is complete, as the ones below start from. }
function ASoundInfo: TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := 'probe';
    Result.Title := 'A probe';
    Result.Category := 'Test';
    Result.Summary := 'Registered only to be walked.';
    Result.Topic := 'data/data-sources';
    Result.ProducesExtensions := '.DAT';
end;

class function TNamelessTitleSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-no-title';
    Result.Title := '';
end;

class function TNoCategorySource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-no-category';
    Result.Category := '';
end;

class function TNoSummarySource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-no-summary';
    Result.Summary := '';
end;

class function TUnknownTopicSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-unknown-topic';
    Result.Topic := 'data/no-such-topic';
end;

class function TSilentAboutFilesSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-silent';
    Result.ProducesExtensions := '';
end;

class function TNamelessChoiceSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-nameless-choice';
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := ChoiceField('bars', 'Bars', 'how long a bar is',
        Choices(['', 'Monthly']), '');
end;

class function TBoundToNothingSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-bound-to-nothing';
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := EnabledWhen(DateField('from', 'From', 'the start'),
        'range', 'custom');
end;

class function TBoundToNoChoiceSource.Info: TDataSourceInfo;
begin
    Result := ASoundInfo;
    Result.Id := 'test-bound-to-no-choice';
    SetLength(Result.QueryFields, 2);
    Result.QueryFields[0] := ChoiceField('range', 'Range', 'how much',
        Choices(['Whole series', '', 'Last year', '1y']), '');
    Result.QueryFields[1] := EnabledWhen(DateField('from', 'From', 'the start'),
        'range', 'custom');
end;

function TopicResolves(const ATopic: string): boolean;
var
    Explanation_: TExplanation;
begin
    Result := FindExplanationIn(DataChapterProviders, ATopic, Explanation_);
end;

{ What the walk says about this build as it stands. }
function FindingsOfThisBuild: string;
begin
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    //  The topics live in the guide's Data chapter, looked up directly - see
    //  EnsureDataChapter for why it is not registered.
    EnsureDataChapter;
    Result := JoinedFindings(DataSourceFindings(@TopicResolves));
end;

{ What the walk says about SEVERAL sources, without registering any of them. }
function FindingsAboutAll(const ASources: array of TDataSourceClass): string;
var
    Probe: TDataSourceClasses;
    i: longint;
begin
    RegisterAllDataLoaders;
    EnsureDataChapter;
    SetLength(Probe, Length(ASources));
    for i := 0 to High(ASources) do
        Probe[i] := ASources[i];
    Result := JoinedFindings(FindingsFor(Probe, @TopicResolves));
end;

{ What the walk says about ONE source, WITHOUT registering it.

  The registry has no way to take a source back out, and nothing in the
  application needs one - so a probe registered here would stay in the registry
  for every later test in this process, and a later walk would report a fault
  nobody put in the build. That is not hypothetical: it failed a module's suite
  over three probes this file had left behind. }
function FindingsAbout(ASource: TDataSourceClass): string;
var
    Probe: TDataSourceClasses;
begin
    RegisterAllDataLoaders;
    EnsureDataChapter;
    SetLength(Probe, 1);
    Probe[0] := ASource;
    Result := JoinedFindings(FindingsFor(Probe, @TopicResolves));
end;

procedure TDataSourceRegistryTest.AKnownIdResolvesToItsSource;
begin
    RegisterAllDataSources;
    AssertTrue('the samples source must resolve by its id',
        FindDataSourceClass(SamplesSourceId) = TSamplesSource);
    AssertTrue('and so must the web address source',
        FindDataSourceClass(UrlSourceId) = TUrlSource);
end;

procedure TDataSourceRegistryTest.AnUnknownIdResolvesToNothing;
begin
    RegisterAllDataSources;
    //  Nil rather than an exception: a project remembering a source this build
    //  does not contain is ordinary - the module may simply not be built in -
    //  and the caller says so in its own words.
    AssertTrue('an unknown source id resolves to nothing',
        FindDataSourceClass('no-such-source') = nil);
end;

procedure TDataSourceRegistryTest.TheSameSourceRegisteredAgainIsANoOp;
var
    Before: longint;
begin
    RegisterAllDataSources;
    Before := DataSourceCount;
    //  A module's front door registers its sources and may be called twice.
    RegisterDataSource(TSamplesSource);
    AssertEquals('registering the same source again must add nothing',
        Before, DataSourceCount);
end;

procedure TDataSourceRegistryTest.TwoSourcesCannotClaimOneId;
var
    Raised: boolean;
begin
    RegisterAllDataSources;
    Raised := False;
    try
        RegisterDataSource(TImpostorSource);
    except
        on E: EDataSourceRegistration do
            Raised := True;
    end;
    //  Which one the wizard offered would otherwise depend on the order of two
    //  uses clauses, and a project's provenance would name an id that resolves
    //  to a different source in the next build.
    AssertTrue('a second claim on an id must be refused', Raised);
end;

procedure TDataSourceRegistryTest.ASourceWithNoClassIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterDataSource(nil);
    except
        on E: EDataSourceRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TDataSourceRegistryTest.ASourceWithNoIdIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterDataSource(TNamelessSource);
    except
        on E: EDataSourceRegistration do
            Raised := True;
    end;
    //  Nothing could name it: not the wizard, and not a project recording
    //  where its data came from.
    AssertTrue('a source with no id must be refused', Raised);
end;

procedure TDataSourceRegistryTest.CategoriesAreDerivedFromWhatRegistered;
var
    Categories: TStringList;
begin
    RegisterAllDataSources;
    Categories := DataSourceCategories;
    try
        //  Derived, never declared: a category exists because a source uses
        //  it, so a module adds one by using it and edits nothing.
        AssertTrue('the framework''s own sources are general',
            Categories.IndexOf('General') >= 0);
    finally
        Categories.Free;
    end;
end;

procedure TDataSourceRegistryTest.EveryRegisteredSourceIsComplete;
var
    Findings: string;
begin
    Findings := FindingsOfThisBuild;
    //  The whole report, so a failure reads as the rule that was broken rather
    //  than as a count that changed.
    AssertEquals('every registered source must explain itself and produce ' +
        'files this build can read', '', Findings);
end;

procedure TDataSourceRegistryTest.ASourceWithoutAnExplanationIsReported;
begin
    AssertTrue('a source with no topic must be named in the findings',
        Pos('TUnexplainedSource', FindingsAbout(TUnexplainedSource)) > 0);
end;

procedure TDataSourceRegistryTest.ASourceProducingUnreadableFilesIsReported;
var
    Findings: string;
begin
    Findings := FindingsAbout(TUnreadableSource);
    //  A source that can only deliver files nothing reads is a dead end the
    //  user would meet after searching, downloading and waiting.
    AssertTrue('a source producing files no loader reads must be reported',
        Pos('.HDF5', Findings) > 0);
end;

procedure TDataSourceRegistryTest.ASourceWithANamelessQueryFieldIsReported;
var
    Findings: string;
begin
    Findings := FindingsAbout(TBadFieldSource);
    AssertTrue('a query field with no caption must be reported',
        Pos('TBadFieldSource', Findings) > 0);
end;

procedure TDataSourceRegistryTest.ASourceWithNoTitleIsReported;
begin
    //  The wizard would offer a nameless row.
    AssertTrue('reported',
        Pos('no title', FindingsAbout(TNamelessTitleSource)) > 0);
end;

procedure TDataSourceRegistryTest.ASourceWithNoCategoryIsReported;
begin
    //  Categories are derived from what registered, so a source with none is
    //  grouped under nothing and disappears from the list.
    AssertTrue('reported',
        Pos('no category', FindingsAbout(TNoCategorySource)) > 0);
end;

procedure TDataSourceRegistryTest.ASourceWithNoSummaryIsReported;
begin
    //  A user would choose it with nothing to go on.
    AssertTrue('reported',
        Pos('no summary', FindingsAbout(TNoSummarySource)) > 0);
end;

procedure TDataSourceRegistryTest.ASourceNamingATopicNothingExplainsIsReported;
var
    Findings: string;
begin
    //  A TOPIC THAT RESOLVES NOWHERE is worse than none: the source looks
    //  explained until somebody asks it to explain itself.
    Findings := FindingsAbout(TUnknownTopicSource);
    AssertTrue(Findings, Pos('no-such-topic', Findings) > 0);
    AssertTrue(Findings, Pos('resolves to no explanation', Findings) > 0);
end;

procedure TDataSourceRegistryTest.ASourceSayingNothingAboutWhatItProducesIsReported;
var
    Findings: string;
begin
    //  Without it nothing can tell whether this build can read what the
    //  source fetches - which is the check that keeps a user from searching,
    //  downloading and only then being told no reader handles it.
    Findings := FindingsAbout(TSilentAboutFilesSource);
    AssertTrue(Findings, Pos('what kind of file', Findings) > 0);
end;

procedure TDataSourceRegistryTest.AChoiceWithNoNameWouldShowAnIdentifierAndIsReported;
var
    Findings: string;
begin
    //  The whole point of captions: a user chooses "Monthly", not a value a
    //  service happens to use. A choice with no caption would show the value.
    Findings := FindingsAbout(TNamelessChoiceSource);
    AssertTrue(Findings, Pos('with no name', Findings) > 0);
    AssertTrue('and says what would be shown: ' + Findings,
        Pos('Monthly', Findings) > 0);
end;

procedure TDataSourceRegistryTest.AFieldAskedWhenAFieldNobodyDeclaresIsReported;
var
    Findings: string;
begin
    //  It would be greyed for good, with a reason naming nothing on screen.
    Findings := FindingsAbout(TBoundToNothingSource);
    AssertTrue(Findings, Pos('"from"', Findings) > 0);
    AssertTrue('and names what is missing: ' + Findings,
        Pos('"range"', Findings) > 0);
end;

procedure TDataSourceRegistryTest.AFieldAskedForAChoiceNobodyOffersIsReported;
var
    Findings: string;
begin
    Findings := FindingsAbout(TBoundToNoChoiceSource);
    AssertTrue(Findings, Pos('"custom"', Findings) > 0);
end;

procedure TDataSourceRegistryTest.TwoSourcesUnderOneIdAreReportedByTheWalkToo;
var
    Findings: string;
begin
    //  THE REGISTRY REFUSES THIS AT REGISTRATION, and the walk says it as
    //  well - because a build can be composed of modules that were written
    //  apart, and a module's own suite walks what IT has rather than what a
    //  host would have refused.
    Findings := FindingsAboutAll([TUnexplainedSource, TUnexplainedSource]);
    AssertTrue(Findings, Pos('another source already uses', Findings) > 0);
end;

procedure TDataSourceRegistryTest.ASourceWithNoIdIsReportedByTheWalkToo;
begin
    //  Nothing could name it - not the wizard, and not a project recording
    //  where its data came from.
    AssertTrue('reported',
        Pos('declares no id', FindingsAbout(TNamelessSource)) > 0);
end;

initialization
    RegisterTest('unit', TDataSourceRegistryTest);

finalization
    DataChapter.Free;
end.
