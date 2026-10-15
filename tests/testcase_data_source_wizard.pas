// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which steps a source needs, and what the wizard refuses at each one.)

THE STEPS ARE DERIVED FROM WHAT A SOURCE DECLARED, so the truth table below is
the contract a module writes its source against: declare a question and you get
a Find step, declare that results may hold other things and you get a Choose
step. Nothing anywhere lists which source needs which, and this is the test that
keeps that true for sources written after it.
}
unit testcase_data_source_wizard;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_wizard, data_source_advice, data_source_registry,
    data_source_registration, data_loader_registration,
    samples_source, url_source, doi_source,
    int_web_client, mock_web_client;

type
    TDataSourceWizardTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        function InfoOf(AAsks, AHasContainers: boolean): TDataSourceInfo;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ASourceThatAsksNothingIsBrowsed;
        procedure ASeriesFeedIsAskedAndPreviewed;
        procedure ARepositoryNeedsEveryStep;
        procedure NextSkipsTheStepsASourceDoesNotNeed;
        procedure BackSkipsThemToo;
        procedure AStepThisSourceHasNotIsNotReachable;
        procedure NothingChosenIsRefusedWithTheReason;
        procedure AnUnknownSourceIdSaysItMayBeAMissingModule;
        procedure AnEmptyRequiredFieldStopsTheSearch;
        procedure OpeningARecordReplacesTheListWithItsFiles;
        procedure ClosingARecordGoesBackToTheResults;
        procedure AFileNoReaderHandlesCannotBeSelected;
        //  What a user is shown, and what the service is asked for.
        procedure AChoiceIsShownByNameAndSentAsItsValue;
        procedure ChoicesAreDeclaredInPairsOrRefused;
        procedure AValueNothingOffersIsShownAsItself;
        //  A field asked only while another holds a value.
        procedure AFieldBoundToAChoiceIsAskedOnlyWhileItIsMade;
        procedure AFieldWithNoConditionIsAlwaysAsked;
        procedure AGreyedFieldSaysWhatWouldAskIt;
        procedure AGreyedFieldIsNotPartOfTheQuery;
        procedure AGreyedRequiredFieldDoesNotStopTheSearch;
        procedure TheWizardGreysByTheAnswersSoFar;
        procedure TheSourceIsNotAskedWhatWasGreyed;
        //  What the breadcrumb over the source's panel may say, and where it
        //  may go.
        procedure AStepShowsWhatWasChosenThereOnceItIsBehindYou;
        procedure AStepNotYetReachedCannotBeJumpedTo;
        procedure AndTheRefusalNamesWhatToFinishFirst;
        procedure AStepAlreadyVisitedCanBeGoneBackTo;
        procedure TheFurthestStepReachedIsRemembered;
        procedure ASourceThatAsksNothingListsItselfOnEnteringTheStep;
        procedure ChoosingAnotherSourceStartsItsOwnStepsAgain;
        procedure ChoosingTheSourceIsNotOneOfTheSteps;
        procedure EverySourceCountsItsOwnStepsFromOne;
    end;

implementation
type
    { A source that asks NOTHING and has two things to offer - the shape the
      bundled samples have. It is never registered: the registry walk is
      another test's subject, and a probe left in the global list would fail
      it by name. }
    TListingSource = class(TDataSource)
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string; override;
    end;

class function TListingSource.Info: TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := 'listing';
    Result.Title := 'Listing';
    Result.Category := 'General';
    Result.ProducesExtensions := '.DAT';
end;

function TListingSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    i: longint;
begin
    SetLength(Result, 2);
    for i := 0 to 1 do
    begin
        Result[i].Id := 'file' + IntToStr(i);
        Result[i].Title := 'sample' + IntToStr(i) + '.dat';
        Result[i].FileName := Result[i].Title;
        Result[i].Ref := Result[i].Title;
        Result[i].IsLeaf := True;
    end;
end;

function TListingSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    Result := AItem.FileName;
end;


type
    { A range choice and the dates it guards - the shape a series feed has.
      Never registered, for the reason TListingSource is not. }
    TRangeSource = class(TDataSource)
    public
        class var LastQuery: TDataSourceQuery;
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string; override;
    end;

function RangeFields: TInputFieldDecls;
begin
    SetLength(Result, 3);
    Result[0] := ChoiceField('range', 'Range', 'how much',
        Choices(['Whole series', '', 'Custom dates', 'custom']), '');
    Result[0].Required := False;
    Result[1] := EnabledWhen(DateField('from', 'From', 'the first day'),
        'range', 'custom');
    Result[2] := EnabledWhen(TextField('note', 'Note', 'required when custom'),
        'range', 'custom');
end;

class function TRangeSource.Info: TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := 'range-probe';
    Result.Title := 'Range probe';
    Result.Category := 'General';
    Result.ProducesExtensions := '.DAT';
    Result.QueryFields := RangeFields;
end;

function TRangeSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
begin
    LastQuery := Copy(AQuery);
    Result := nil;
end;

function TRangeSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    Result := '';
end;

function TDataSourceWizardTest.InfoOf(AAsks, AHasContainers: boolean): TDataSourceInfo;
begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := 'probe';
    Result.HasContainers := AHasContainers;
    if AAsks then
    begin
        SetLength(Result.QueryFields, 1);
        Result.QueryFields[0] := TextField('q', 'Query', 'anything');
    end;
end;

procedure TDataSourceWizardTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
end;

procedure TDataSourceWizardTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

procedure TDataSourceWizardTest.ASourceThatAsksNothingIsBrowsed;
var
    Steps: TWizardStepSet;
begin
    //  The samples: nothing to ask, so the list of what there is IS the step.
    Steps := WizardSteps(InfoOf(False, False));
    AssertTrue('a source that asks nothing has no Find step',
        not (wsFind in Steps));
    AssertTrue('but it must be browsed', wsChoose in Steps);
    AssertTrue('and previewed', wsPreview in Steps);
end;

procedure TDataSourceWizardTest.ASeriesFeedIsAskedAndPreviewed;
var
    Steps: TWizardStepSet;
begin
    //  A feed answering with the one series it was asked for: making the user
    //  choose from a list of one would be confirming their own typing.
    Steps := WizardSteps(InfoOf(True, False));
    AssertTrue('it asks', wsFind in Steps);
    AssertTrue('it does not make the user choose from one result',
        not (wsChoose in Steps));
    AssertTrue('it is previewed', wsPreview in Steps);
end;

procedure TDataSourceWizardTest.ARepositoryNeedsEveryStep;
var
    Steps: TWizardStepSet;
begin
    Steps := WizardSteps(InfoOf(True, True));
    AssertTrue('source', wsSource in Steps);
    AssertTrue('find', wsFind in Steps);
    AssertTrue('choose', wsChoose in Steps);
    AssertTrue('preview', wsPreview in Steps);
end;

procedure TDataSourceWizardTest.NextSkipsTheStepsASourceDoesNotNeed;
begin
    //  The web address source asks one question and answers with one file.
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    AssertTrue('starts at the source list', FWizard.Step = wsSource);
    FWizard.GoNext;
    AssertTrue('then asks', FWizard.Step = wsFind);
    FWizard.GoNext;
    //  Choose is not in this source's set at all, so it is not stepped through.
    AssertTrue('and goes straight to the preview', FWizard.Step = wsPreview);
end;

procedure TDataSourceWizardTest.BackSkipsThemToo;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.GoNext;
    FWizard.GoNext;
    FWizard.GoBack;
    AssertTrue('back from the preview is the question it asked',
        FWizard.Step = wsFind);
    FWizard.GoBack;
    AssertTrue('and back again is the source list', FWizard.Step = wsSource);
end;

procedure TDataSourceWizardTest.AStepThisSourceHasNotIsNotReachable;
var
    Refused: boolean;
begin
    FWizard.ChooseSource(UrlSourceId);
    Refused := False;
    try
        FWizard.GoTo_(wsChoose);
    except
        on E: EDataSourceError do
            Refused := True;
    end;
    AssertTrue('a step this source does not have is not reachable', Refused);
end;

procedure TDataSourceWizardTest.NothingChosenIsRefusedWithTheReason;
var
    Verdict: TDataSourceVerdict;
begin
    Verdict := FWizard.CanGoNext;
    AssertFalse('no source chosen yet', Verdict.Allowed);
    AssertTrue('and it says what to do: "' + Verdict.Reason + '"',
        Pos('Choose a data source', Verdict.Reason) > 0);
end;

procedure TDataSourceWizardTest.AnUnknownSourceIdSaysItMayBeAMissingModule;
var
    Message_: string;
begin
    Message_ := '';
    try
        FWizard.ChooseSource('not-built-in-here');
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    //  This is what a remembered import of a source from a module that is not
    //  installed looks like, and the user can act on that sentence.
    AssertTrue('the refusal names the module possibility: "' + Message_ + '"',
        Pos('module', Message_) > 0);
end;

procedure TDataSourceWizardTest.AnEmptyRequiredFieldStopsTheSearch;
var
    Verdict: TDataSourceVerdict;
begin
    FWizard.ChooseSource(UrlSourceId);
    Verdict := FWizard.SearchVerdict;
    AssertFalse('an empty address cannot be searched for', Verdict.Allowed);
    AssertTrue('and the field is named: "' + Verdict.Reason + '"',
        Pos('Address', Verdict.Reason) > 0);
end;

procedure TDataSourceWizardTest.OpeningARecordReplacesTheListWithItsFiles;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://zenodo.org/record/1234567"}}]}');
    FMockObject.Reply('zenodo.org/api/records/1234567',
        '{"files":[{"key":"quartz.xy","size":10,"links":{"self":' +
        '"https://zenodo.org/api/records/1234567/files/quartz.xy/content"}}]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.1234567');
    FWizard.Search;
    AssertEquals('the DOI found its record', 1, FWizard.ItemCount);
    FWizard.Open(0);
    AssertEquals('which holds one file', 1, FWizard.ItemCount);
    AssertEquals('quartz.xy', FWizard.Item(0).Title);
    AssertTrue('and we are inside it', FWizard.InsideContainer);
end;

procedure TDataSourceWizardTest.ClosingARecordGoesBackToTheResults;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://zenodo.org/record/1234567"}}]}');
    FMockObject.Reply('zenodo.org/api/records/1234567',
        '{"files":[{"key":"quartz.xy","size":10,"links":{"self":"x"}}]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.1234567');
    FWizard.Search;
    FWizard.Open(0);
    FWizard.CloseContainer;
    AssertFalse('out of the record', FWizard.InsideContainer);
    AssertEquals('back among the results', 1, FWizard.ItemCount);
    AssertFalse('which are records, not files', FWizard.Item(0).IsLeaf);
end;

procedure TDataSourceWizardTest.AFileNoReaderHandlesCannotBeSelected;
var
    Verdict: TDataSourceVerdict;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://zenodo.org/record/7"}}]}');
    FMockObject.Reply('zenodo.org/api/records/7',
        '{"files":[{"key":"notes.pdf","size":10,"links":{"self":"x"}}]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.7');
    FWizard.Search;
    FWizard.Open(0);
    Verdict := FWizard.ChooseVerdict(0);
    //  Listed, visible, and refused by name: what the record contains is part
    //  of what the user came to see.
    AssertFalse('a PDF is not data this build reads', Verdict.Allowed);
    AssertTrue('and the refusal names the kind: "' + Verdict.Reason + '"',
        Pos('.pdf', Verdict.Reason) > 0);
end;

procedure TDataSourceWizardTest.AFieldBoundToAChoiceIsAskedOnlyWhileItIsMade;
var
    Q: TDataSourceQuery;
begin
    Q := WithQueryValue(DefaultQuery(RangeFields), 'range', '');
    AssertFalse('the whole series asks no dates',
        FieldIsEnabled(RangeFields, 'from', Q));
    Q := WithQueryValue(Q, 'range', 'custom');
    AssertTrue('custom dates do', FieldIsEnabled(RangeFields, 'from', Q));
end;

procedure TDataSourceWizardTest.AFieldWithNoConditionIsAlwaysAsked;
begin
    AssertTrue(FieldIsEnabled(RangeFields, 'range', nil));
    AssertTrue('and one nothing declares is not greyed by this',
        FieldIsEnabled(RangeFields, 'nowhere', nil));
    AssertEquals('', FieldCondition(RangeFields, 'range'));
end;

procedure TDataSourceWizardTest.AGreyedFieldSaysWhatWouldAskIt;
begin
    //  BY CAPTION, both of them: the user sees "Range" and "Custom dates",
    //  never 'range' and 'custom'.
    AssertEquals('Used only when Range is "Custom dates".',
        FieldCondition(RangeFields, 'from'));
end;

procedure TDataSourceWizardTest.AGreyedFieldIsNotPartOfTheQuery;
var
    Q: TDataSourceQuery;
begin
    //  A date typed, then the range changed to the whole series: the date is
    //  still in the box and must not travel.
    Q := WithQueryValue(DefaultQuery(RangeFields), 'from', '2020-01-01');
    AssertEquals('', QueryValue(EffectiveQuery(RangeFields, Q), 'from'));
    Q := WithQueryValue(Q, 'range', 'custom');
    AssertEquals('asked again, it travels', '2020-01-01',
        QueryValue(EffectiveQuery(RangeFields, Q), 'from'));
end;

procedure TDataSourceWizardTest.AGreyedRequiredFieldDoesNotStopTheSearch;
var
    Q: TDataSourceQuery;
begin
    Q := DefaultQuery(RangeFields);
    AssertEquals('a field nobody is asked cannot be missing', '',
        MissingRequiredField(RangeFields, Q));
    Q := WithQueryValue(Q, 'range', 'custom');
    AssertEquals('Note', MissingRequiredField(RangeFields, Q));
end;

procedure TDataSourceWizardTest.TheWizardGreysByTheAnswersSoFar;
begin
    FWizard.ChooseSourceClass(TRangeSource);
    AssertFalse(FWizard.FieldEnabled('from'));
    FWizard.SetField('range', 'custom');
    AssertTrue(FWizard.FieldEnabled('from'));
end;

procedure TDataSourceWizardTest.TheSourceIsNotAskedWhatWasGreyed;
begin
    FWizard.ChooseSourceClass(TRangeSource);
    FWizard.SetField('from', '2020-01-01');
    TRangeSource.LastQuery := nil;
    FWizard.Search;
    AssertEquals('', QueryValue(TRangeSource.LastQuery, 'from'));
end;

procedure TDataSourceWizardTest.AChoiceIsShownByNameAndSentAsItsValue;
var
    Catalogue: TInputChoices;
begin
    //  NOBODY SHOULD HAVE TO KNOW AN IDENTIFIER. The list shows what a user
    //  calls the thing; what travels is what the service calls it.
    Catalogue := Choices(['S&P 500', 'SP500', '10-year Treasury yield', 'DGS10']);
    AssertEquals('the name is turned into the id', 'DGS10',
        ValueOfChoice(Catalogue, '10-year Treasury yield'));
    AssertEquals('and the id back into the name', 'S&P 500',
        CaptionOfChoice(Catalogue, 'SP500'));
end;

procedure TDataSourceWizardTest.ChoicesAreDeclaredInPairsOrRefused;
var
    Refused: boolean;
begin
    Refused := False;
    try
        //  An odd count means the pairs have slipped, which would show an
        //  identifier to a user as though it were a name - the one thing
        //  captions exist to prevent.
        Choices(['S&P 500', 'SP500', 'Dow Jones']);
    except
        on E: EDataSourceError do
            Refused := True;
    end;
    AssertTrue('a half-written list of choices is refused', Refused);
end;

procedure TDataSourceWizardTest.AValueNothingOffersIsShownAsItself;
begin
    //  A query remembered from a build whose catalogue has since changed.
    //  Showing the value is honest, and still recognisable.
    AssertEquals('WILL5000PR',
        CaptionOfChoice(Choices(['S&P 500', 'SP500']), 'WILL5000PR'));
end;

procedure TDataSourceWizardTest.AStepShowsWhatWasChosenThereOnceItIsBehindYou;
begin
    FWizard.ChooseSource(UrlSourceId);
    //  WHILE THE CHOICE IS ON SCREEN there is nothing to repeat: the source
    //  list is right there, and naming the source in the breadcrumb as well
    //  said the same thing twice, in two places at once.
    AssertEquals('the source is named as the choice of its own step',
        'Web address', FWizard.ChoiceAt(wsSource));
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    //  And the question's answer belongs to the step that asked it.
    AssertTrue(FWizard.ChoiceAt(wsFind),
        Pos('example.org', FWizard.ChoiceAt(wsFind)) > 0);
    AssertEquals('a step with nothing chosen yet says nothing', '',
        FWizard.ChoiceAt(wsPreview));
end;

procedure TDataSourceWizardTest.AStepNotYetReachedCannotBeJumpedTo;
begin
    FWizard.ChooseSource(UrlSourceId);
    //  A breadcrumb is a place to go BACK from: jumping forward over a question
    //  arrives at a page whose answer nothing has.
    AssertFalse('the preview has not been reached',
        FWizard.CanGoTo(wsPreview).Allowed);
end;

procedure TDataSourceWizardTest.AndTheRefusalNamesWhatToFinishFirst;
var
    Verdict: TDataSourceVerdict;
begin
    FWizard.ChooseSource(UrlSourceId);
    Verdict := FWizard.CanGoTo(wsPreview);
    //  Not "you cannot do that": what has to happen first.
    AssertTrue(Verdict.Reason, Pos('source', LowerCase(Verdict.Reason)) > 0);
    AssertTrue(Verdict.Reason, Pos('first', LowerCase(Verdict.Reason)) > 0);
end;

procedure TDataSourceWizardTest.AStepAlreadyVisitedCanBeGoneBackTo;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.GoNext;
    AssertTrue('back to where we came from',
        FWizard.CanGoTo(wsSource).Allowed);
    FWizard.GoTo_(wsSource);
    AssertTrue('and it went', FWizard.Step = wsSource);
end;

procedure TDataSourceWizardTest.TheFurthestStepReachedIsRemembered;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.GoNext;
    FWizard.GoTo_(wsSource);
    //  GOING BACK DOES NOT UNDO HAVING BEEN THERE: the breadcrumb still lets
    //  the user return to the question they had already answered.
    AssertTrue('still reached', FWizard.Reached = wsFind);
    AssertTrue('so it is reachable again', FWizard.CanGoTo(wsFind).Allowed);
end;


{ THE SOURCE IS NOT A STEP. It is what the window's left-hand list is FOR, and
  that list is on screen the whole time - so counting it made the sequence
  depend on the thing that chose the sequence, and the steps on the left
  changed under a selection made on the right. }
procedure TDataSourceWizardTest.ChoosingTheSourceIsNotOneOfTheSteps;
begin
    AssertEquals('with no source chosen there is no sequence yet', 0,
        FWizard.StageCount);
    FWizard.ChooseSource(UrlSourceId);
    AssertEquals('the source list is not a stage', 0,
        FWizard.StageOf(wsSource));
end;

procedure TDataSourceWizardTest.EverySourceCountsItsOwnStepsFromOne;
begin
    FWizard.ChooseSource(UrlSourceId);
    AssertEquals('a web address asks, then previews', 2, FWizard.StageCount);
    AssertEquals('its question is step one', 1, FWizard.StageOf(wsFind));
    AssertEquals('its preview is step two', 2, FWizard.StageOf(wsPreview));

    FWizard.ChooseSource(DoiSourceId);
    AssertEquals('a repository asks, lists and previews', 3,
        FWizard.StageCount);
    AssertEquals('its file list is step two', 2, FWizard.StageOf(wsChoose));
    AssertEquals('and the preview is step three', 3,
        FWizard.StageOf(wsPreview));

    FWizard.ChooseSource(SamplesSourceId);
    AssertEquals('bundled samples ask nothing, so listing them is step one',
        1, FWizard.StageOf(wsChoose));
    AssertEquals('and there are two of them', 2, FWizard.StageCount);
end;

{ NOTHING TO ASK STILL MEANS SOMETHING TO SHOW. The samples declare no
  question, so no step runs a search before the list appears - and the list
  was drawn from what the source had never been asked for, which is an empty
  list with nothing wrong reported. Entering the step is what asks. }
procedure TDataSourceWizardTest.ASourceThatAsksNothingListsItselfOnEnteringTheStep;
begin
    FWizard.ChooseSourceClass(TListingSource);
    FWizard.GoNext;
    AssertTrue('the step entered is the list', FWizard.Step = wsChoose);
    AssertEquals('and it holds what the source has', 2, FWizard.ItemCount);
end;

{ THE SOURCE LIST IS ON SCREEN THE WHOLE TIME, so a source can be changed
  from any step - and the step the user was on belonged to the source they
  have just left. Keeping it would show the new source's name over the old
  one's page, with answers nothing had been asked for. }
procedure TDataSourceWizardTest.ChoosingAnotherSourceStartsItsOwnStepsAgain;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.GoNext;
    FWizard.GoNext;
    AssertTrue('the web address is at its preview',
        FWizard.Step = wsPreview);

    FWizard.ChooseSource(DoiSourceId);
    AssertTrue('another source starts at its own first step',
        FWizard.Step = wsSource);
    AssertFalse('and nothing further has been reached',
        FWizard.CanGoTo(wsPreview).Allowed);
end;
initialization
    RegisterTest('unit', TDataSourceWizardTest);
end.
