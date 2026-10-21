// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The two sources the framework itself ships, over canned replies.)

NO TEST HERE TOUCHES THE NETWORK, and that is a rule rather than a convenience:
a test that reached a public service would fail when the service is slow or
rate-limits it, and pass tomorrow for reasons nobody here decided. The replies
are recorded shapes of what each service answers, cut down to what the source
reads.

WHAT IS WORTH PINNING is what each source ASKS FOR and what it MAKES OF THE
ANSWER - a source quietly addressing the wrong endpoint is a defect that looks
exactly like a service with nothing to say.
}
unit testcase_builtin_sources;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, samples_source, url_source,
    data_loader_registry,
    int_web_client, mock_web_client;

type
    TBuiltinSourcesTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        function QueryOf(const AId, AValue: string): TDataSourceQuery;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Web address.
        procedure AnAddressBecomesOneFileToFetch;
        procedure TheNameComesFromTheAddressWithoutItsQuery;
        procedure APageAddressOnThisComputerIsRefusedWithAdvice;
        procedure SomethingThatIsNotAnAddressIsRefused;
        procedure TheFileIsFetchedFromTheAddressGiven;
        //  Samples.
        procedure TheSamplesAreLookedForWhereTheyAreInstalled;
        procedure TheBundleKeepsThemWhereMacOsPutsResources;
        procedure APackageUnderItsOwnNameIsLookedUpUnderThatName;
        procedure AnInstallationWithoutSamplesSaysSo;
        procedure TheSamplesAreWhateverThisBuildReads;
    end;

implementation

uses
    data_loader_registration;

function TBuiltinSourcesTest.QueryOf(const AId, AValue: string): TDataSourceQuery;
begin
    Result := nil;
    Result := WithQueryValue(Result, AId, AValue);
end;

procedure TBuiltinSourcesTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
end;

procedure TBuiltinSourcesTest.TearDown;
begin
    //  The interface first, then the object: a corba interface is a bare
    //  pointer and nothing counts it.
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

{ --------------------------------- URL -------------------------------------- }

procedure TBuiltinSourcesTest.AnAddressBecomesOneFileToFetch;
var
    Source: TUrlSource;
    Items: TDataSourceItems;
begin
    Source := TUrlSource.Create(FWeb);
    try
        Items := Source.Search(QueryOf(UrlFieldId,
            'https://example.org/data/spectrum.xy'));
        AssertEquals('one address is one result', 1, Length(Items));
        AssertTrue('and it is a file rather than a record', Items[0].IsLeaf);
        //  NOTHING IS FETCHED to answer the query: a mistyped address costs
        //  nothing, and the wizard's steps mean the same for every source.
        AssertEquals('nothing was downloaded to list it', 0,
            FMockObject.Log.CountOf('Download'));
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.TheNameComesFromTheAddressWithoutItsQuery;
begin
    AssertEquals('spectrum.xy',
        FileNameFromUrl('https://example.org/d/spectrum.xy?token=9#top'));
    //  An address whose file is chosen by its query names no file at all -
    //  and neither does one ending in a word that names an endpoint, which is
    //  what leaves the server's own suggestion free to say what the file is.
    AssertEquals('', FileNameFromUrl('https://example.org/download/?id=SP500'));
    AssertEquals('', FileNameFromUrl('https://example.org/download?id=SP500'));
end;

procedure TBuiltinSourcesTest.APageAddressOnThisComputerIsRefusedWithAdvice;
var
    Source: TUrlSource;
    Message_: string;
begin
    Source := TUrlSource.Create(FWeb);
    try
        Message_ := '';
        try
            Source.Search(QueryOf(UrlFieldId, 'file:///home/u/data.dat'));
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        //  Not a snub: the command that opens a local file is in the same menu.
        AssertTrue('a local address is answered with the command that opens ' +
            'one: got "' + Message_ + '"', Pos('Import Profile', Message_) > 0);
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.SomethingThatIsNotAnAddressIsRefused;
begin
    AssertTrue('a bare word is not an address',
        Pos('http', UrlRefusal('quartz spectra')) > 0);
    AssertEquals('a full address is accepted', '',
        UrlRefusal('https://example.org/a.xy'));
end;

procedure TBuiltinSourcesTest.TheFileIsFetchedFromTheAddressGiven;
var
    Source: TUrlSource;
    Items: TDataSourceItems;
    Dest: TStringStream;
    Name_: string;
begin
    FMockObject.Reply('example.org/data/spectrum.xy', '1 2'#10'3 4');
    Source := TUrlSource.Create(FWeb);
    Dest := TStringStream.Create('');
    try
        Items := Source.Search(QueryOf(UrlFieldId,
            'https://example.org/data/spectrum.xy'));
        Name_ := Source.Download(Items[0], Dest);
        AssertEquals('the bytes at that address', '1 2'#10'3 4', Dest.DataString);
        AssertEquals('saved under the name the address gave', 'spectrum.xy',
            Name_);
    finally
        Dest.Free;
        Source.Free;
    end;
end;

{ ------------------------------- Samples ------------------------------------ }

{ A machine with nothing installed anywhere. }
function NothingExists(const APath: string): boolean;
begin
    Result := False;
end;

{ An absolute path as this platform spells it. The candidates above the program
  are resolved with ExpandFileName, which on Windows gives '/opt/fit/share' a
  drive and backslashes - so a literal Unix path, compared as a string, fails
  there for a rule that holds. Spelling the fixture natively, not repeating the
  rule, is what keeps the test about the layout. }
function Native(const APath: string): string;
begin
    Result := ExpandFileName(APath);
end;

procedure TBuiltinSourcesTest.TheSamplesAreLookedForWhereTheyAreInstalled;
var
    Candidates: TStringList;
begin
    Candidates := SampleDirectoryCandidatesFor(Native('/opt/fit/bin/'), 'fit');
    try
        //  A source tree, a portable archive, a bundle and an installed
        //  package put them in different places, and all of them are right.
        AssertTrue('beside the program',
            Candidates.IndexOf(Native('/opt/fit/bin/Data')) >= 0);
        AssertTrue('and where a package puts read-only program data',
            Candidates.IndexOf(Native('/opt/fit/share/fit/Data')) >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.TheBundleKeepsThemWhereMacOsPutsResources;
var
    Candidates: TStringList;
begin
    //  A macOS bundle has the binary in Contents/MacOS and its read-only data
    //  in Contents/Resources - which is where the packaging puts it. Looking
    //  only one directory up finds Contents, and the samples silently are not
    //  there on the one platform whose layout differs.
    Candidates := SampleDirectoryCandidates(
        Native('/Applications/Fit.app/Contents/MacOS/'));
    try
        AssertTrue('the bundle''s resources are among the candidates',
            Candidates.IndexOf(
                Native('/Applications/Fit.app/Contents/Resources/Data')) >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.APackageUnderItsOwnNameIsLookedUpUnderThatName;
var
    Candidates: TStringList;
begin
    //  A Linux package installs its data under ITS OWN NAME - /usr/share/fit
    //  for one product and /usr/share/fit-pro for the other - so the directory
    //  is derived from the program's own file name rather than written out.
    //  Without this the second product finds the first's samples, or none.
    Candidates := SampleDirectoryCandidatesFor('/usr/bin/', 'fit-pro');
    try
        AssertTrue('its own share directory',
            Candidates.IndexOf('/usr/share/fit-pro/Data') >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.AnInstallationWithoutSamplesSaysSo;
var
    Nowhere: string;
begin
    //  The existence check is passed in rather than taken from the disk, and
    //  this is why: one candidate is the absolute /usr/share/fit/Data, so on a
    //  machine that HAS Fit installed a test asking the real disk would be
    //  answering a question about that machine instead of about this rule.
    Nowhere := SamplesDirectoryFor('/no/such/place/', @NothingExists);
    //  An empty list would read as "this build has no samples in it", which is
    //  a different and more alarming thing than "they were not installed".
    AssertEquals('nothing is found where nothing is installed', '', Nowhere);
end;

procedure TBuiltinSourcesTest.TheSamplesAreWhateverThisBuildReads;
var
    Declared: TStringList;
    Loaders: TDataLoaderInfoArray;
    Own: TStringList;
    i, j: longint;
begin
    //  DERIVED, NOT LISTED. The samples are chosen from the installed folder by
    //  whether this build reads them, so what the source declares must be
    //  exactly that: a fixed list named .CSV in a build with no price reader,
    //  which the registry walk then refused.
    RegisterAllDataLoaders;
    Declared := TStringList.Create;
    Own := TStringList.Create;
    try
        Declared.Delimiter := ';';
        Declared.StrictDelimiter := True;
        Declared.DelimitedText := TSamplesSource.Info.ProducesExtensions;
        Own.Delimiter := ';';
        Own.StrictDelimiter := True;
        Loaders := RegisteredDataLoaders;
        for i := 0 to High(Loaders) do
        begin
            Own.DelimitedText := Loaders[i].Extensions;
            for j := 0 to Own.Count - 1 do
                AssertTrue(Own[j] + ' is read here, so it is offered',
                    Declared.IndexOf(Own[j]) >= 0);
        end;
        for i := 0 to Declared.Count - 1 do
            AssertTrue(Declared[i] + ' is offered, so something reads it',
                FindDataLoaderClass('x' + Declared[i]) <> nil);
    finally
        Own.Free;
        Declared.Free;
    end;
end;

initialization
    RegisterTest('unit', TBuiltinSourcesTest);
end.
