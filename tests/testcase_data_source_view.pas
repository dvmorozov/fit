// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the data source window shows at each step.)

THESE RULES LIVED IN THE WINDOW, where nothing could run them: which panel is on
screen, what Back and Create may do and why not, what a breadcrumb repeats, how
a caption chosen in a list travels as the value behind it, and what the status
line says while a service is slow. data_source_view holds them now, and the
window only draws the answers.

THE WIZARD HERE IS THE REAL ONE over the registered web-address source - the
same calls the window makes, in its order - so the view is tested against the
wizard the user drives rather than a record filled in by hand.
}
unit testcase_data_source_view;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry,
    data_source, data_source_advice, data_source_wizard, data_source_view,
    data_source_registration, data_loader_registration, url_source,
    title_points_set, int_web_client, mock_web_client;

type
    TDataSourceViewTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Which editor a declared field is answered in.
        procedure ADateIsPickedFromACalendar;
        procedure AChoiceWithChoicesIsAClosedList;
        procedure TextWithSuggestionsIsAnOpenList;
        procedure TextWithNothingToSuggestIsTyped;
        procedure AChoiceWithNothingToChooseIsTyped;
        //  A closed list's starting row.
        procedure AClosedListStartsOnTheRememberedValue;
        procedure AValueNothingOffersStartsOnTheFirstRow;
        //  The line under a field.
        procedure AGreyableFieldsHintSaysWhatWouldAskIt;
        procedure AFieldAlwaysAskedShowsItsHintAlone;
        procedure TheChoicesOfAFieldAreFoundByItsId;
        //  What travels.
        procedure AnEmptyDateStaysEmpty;
        procedure ADateTravelsInIsoWhateverTheMachineShows;
        procedure ACaptionTravelsAsItsValue;
        procedure FreeTextInAnOpenListIsTheAnswer;
        procedure AClosedListHoldingNoOfferedCaptionHasNoAnswer;
        //  The Choose step's rows.
        procedure ARowThatCanBeTakenShowsItsDetails;
        procedure ARowThatCannotSaysWhyInTheirPlace;
        procedure ASizeIsInKilobytesAndAnUnknownOneIsBlank;
        //  The preview.
        procedure APreviewNamesTheFileAndWhereItCameFrom;
        procedure APreviewStatesHowManyPointsAndTheirRange;
        procedure AFileReadToNoPointsSaysSoWithoutARange;
        procedure APreviewThatFailedSaysWhy;
        //  What is on screen.
        procedure WithNoSourceChosenThePromptIsShownAndNothingAdvances;
        procedure AndTheStatusSaysWhatToDo;
        procedure TheFirstStepOfASourceHasNoBack;
        procedure TheQuestionIsOnScreenWhileItIsAsked;
        //  The breadcrumb.
        procedure WithNoSourceThereAreNoCrumbs;
        procedure EveryStageOfTheSourceIsACrumbNumberedFromOne;
        procedure OnlyCrumbsAfterTheFirstHaveASeparator;
        procedure TheStepOnScreenIsCurrentAndTheRestAreFuture;
        procedure ACrumbNotYetReachedCannotBeClicked;
        //  The wait.
        procedure BeforeAnythingArrivesItSaysItIsAsking;
        procedure WithNoLengthItCountsWhatArrived;
        procedure WithALengthItCountsAgainstIt;
        procedure AfterAFewSecondsItSaysHowLongAndHowToStop;
        procedure TheSpinnerTurnsWithEachFrame;
        //  The outcome.
        procedure StoppingIsSaidInTheStatusLine;
        procedure FailingIsPutInFrontOfTheUser;
        procedure SucceedingShowsNothing;
    end;

implementation

function RangeFields: TInputFieldDecls;
begin
    SetLength(Result, 2);
    Result[0] := ChoiceField('range', 'Range', 'how much',
        Choices(['Whole series', 'all', 'Custom dates', 'custom']), 'all');
    Result[1] := EnabledWhen(DateField('from', 'From', 'the first day'),
        'range', 'custom');
end;

procedure TDataSourceViewTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
end;

procedure TDataSourceViewTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

{ ---- which editor ---------------------------------------------------------- }

procedure TDataSourceViewTest.ADateIsPickedFromACalendar;
begin
    AssertTrue(FieldEditorKind(DateField('d', 'D', '')) = fekDate);
end;

procedure TDataSourceViewTest.AChoiceWithChoicesIsAClosedList;
begin
    AssertTrue(FieldEditorKind(RangeFields[0]) = fekClosedList);
end;

procedure TDataSourceViewTest.TextWithSuggestionsIsAnOpenList;
begin
    AssertTrue(FieldEditorKind(SuggestedField('s', 'Series', '',
        Choices(['S&P 500', 'SP500']))) = fekOpenList);
end;

procedure TDataSourceViewTest.TextWithNothingToSuggestIsTyped;
begin
    AssertTrue(FieldEditorKind(TextField('q', 'Query', '')) = fekText);
end;

procedure TDataSourceViewTest.AChoiceWithNothingToChooseIsTyped;
var
    Field: TInputFieldDecl;
begin
    //  A closed list of nothing could not be answered at all.
    Field := RangeFields[0];
    Field.Choices := nil;
    AssertTrue(FieldEditorKind(Field) = fekText);
end;

{ ---- a closed list's starting row ------------------------------------------ }

procedure TDataSourceViewTest.AClosedListStartsOnTheRememberedValue;
begin
    AssertEquals(1, InitialChoiceIndex(RangeFields[0].Choices, 'custom'));
end;

procedure TDataSourceViewTest.AValueNothingOffersStartsOnTheFirstRow;
begin
    AssertEquals(0, InitialChoiceIndex(RangeFields[0].Choices, 'gone'));
end;

{ ---- the line under a field ------------------------------------------------ }

procedure TDataSourceViewTest.AGreyableFieldsHintSaysWhatWouldAskIt;
var
    Text_: string;
begin
    Text_ := FieldHintText(RangeFields, 'from');
    AssertTrue(Text_, Pos('the first day', Text_) = 1);
    AssertTrue(Text_, Pos('Custom dates', Text_) > 0);
end;

procedure TDataSourceViewTest.AFieldAlwaysAskedShowsItsHintAlone;
begin
    AssertEquals('how much', FieldHintText(RangeFields, 'range'));
end;

procedure TDataSourceViewTest.TheChoicesOfAFieldAreFoundByItsId;
begin
    AssertEquals(2, Length(FieldChoices(RangeFields, 'range')));
    AssertEquals(0, Length(FieldChoices(RangeFields, 'from')));
    AssertEquals(0, Length(FieldChoices(RangeFields, 'nobody')));
end;

{ ---- what travels ---------------------------------------------------------- }

procedure TDataSourceViewTest.AnEmptyDateStaysEmpty;
begin
    //  The picker's Date is a real day even when the box is empty.
    AssertEquals('', WireDate('', EncodeDate(2026, 9, 23)));
    AssertEquals('', WireDate('   ', EncodeDate(2026, 9, 23)));
end;

procedure TDataSourceViewTest.ADateTravelsInIsoWhateverTheMachineShows;
begin
    AssertEquals('2026-09-23', WireDate('23.9.26', EncodeDate(2026, 9, 23)));
end;

procedure TDataSourceViewTest.ACaptionTravelsAsItsValue;
begin
    AssertEquals('custom', AnswerFromChoiceText(RangeFields[0].Choices,
        'Custom dates', True));
end;

procedure TDataSourceViewTest.FreeTextInAnOpenListIsTheAnswer;
begin
    AssertEquals('NASDAQCOM', AnswerFromChoiceText(
        Choices(['S&P 500', 'SP500']), 'NASDAQCOM', False));
end;

procedure TDataSourceViewTest.AClosedListHoldingNoOfferedCaptionHasNoAnswer;
begin
    AssertEquals('', AnswerFromChoiceText(RangeFields[0].Choices,
        'typed anyway', True));
end;

{ ---- the Choose step's rows ------------------------------------------------ }

function Row(const ADetails: string; ASize: int64): TDataSourceItem;
begin
    Result := Default(TDataSourceItem);
    Result.Title := 'quartz.xy';
    Result.Details := ADetails;
    Result.Size := ASize;
end;

function Verdict(AAllowed: boolean; const AReason: string): TDataSourceVerdict;
begin
    Result.Allowed := AAllowed;
    Result.Reason := AReason;
end;

procedure TDataSourceViewTest.ARowThatCanBeTakenShowsItsDetails;
begin
    AssertEquals('CC-BY', ResultRowText(Row('CC-BY', 0),
        Verdict(True, '')).Details);
end;

procedure TDataSourceViewTest.ARowThatCannotSaysWhyInTheirPlace;
begin
    AssertEquals('No reader for .zip',
        ResultRowText(Row('CC-BY', 0), Verdict(False, 'No reader for .zip')).Details);
end;

procedure TDataSourceViewTest.ASizeIsInKilobytesAndAnUnknownOneIsBlank;
begin
    AssertEquals('2 KB', ResultRowText(Row('', 2048), Verdict(True, '')).Size);
    AssertEquals('', ResultRowText(Row('', 0), Verdict(True, '')).Size);
end;

{ ---- the preview ----------------------------------------------------------- }

procedure TDataSourceViewTest.APreviewNamesTheFileAndWhereItCameFrom;
var
    Lines: TStringArray;
begin
    Lines := PreviewLines('/tmp/cache/quartz.xy', 'Zenodo', nil, '');
    AssertEquals(2, Length(Lines));
    AssertEquals('File: quartz.xy', Lines[0]);
    AssertEquals('From: Zenodo', Lines[1]);
end;

procedure TDataSourceViewTest.APreviewStatesHowManyPointsAndTheirRange;
var
    Points: TTitlePointsSet;
    Lines: TStringArray;
begin
    Points := TTitlePointsSet.Create(nil);
    try
        Points.AddNewPoint(10, 1);
        Points.AddNewPoint(20, 2);
        Points.AddNewPoint(30, 3);
        Lines := PreviewLines('a.xy', 'here', Points, 'ignored');
    finally
        Points.Free;
    end;
    AssertEquals(4, Length(Lines));
    AssertEquals('3 points, x from 10 to 30', Lines[3]);
end;

procedure TDataSourceViewTest.AFileReadToNoPointsSaysSoWithoutARange;
var
    Points: TTitlePointsSet;
    Lines: TStringArray;
begin
    Points := TTitlePointsSet.Create(nil);
    try
        Lines := PreviewLines('a.xy', 'here', Points, '');
    finally
        Points.Free;
    end;
    AssertEquals('0 points', Lines[High(Lines)]);
end;

procedure TDataSourceViewTest.APreviewThatFailedSaysWhy;
var
    Lines: TStringArray;
begin
    Lines := PreviewLines('a.xy', 'here', nil, 'Not a profile');
    AssertEquals(4, Length(Lines));
    AssertEquals('Not a profile', Lines[3]);
end;

{ ---- what is on screen ----------------------------------------------------- }

procedure TDataSourceViewTest.WithNoSourceChosenThePromptIsShownAndNothingAdvances;
var
    View: TWizardView;
begin
    View := WizardView(FWizard);
    AssertTrue('prompt', View.PromptVisible);
    AssertFalse('header', View.HeaderVisible);
    AssertFalse('fields', View.FieldsVisible);
    AssertFalse('Next', View.NextEnabled);
    AssertFalse('Create', View.CreateEnabled);
    AssertFalse('Back', View.BackEnabled);
end;

procedure TDataSourceViewTest.AndTheStatusSaysWhatToDo;
var
    View: TWizardView;
begin
    View := WizardView(FWizard);
    AssertTrue(View.Status, Pos('Choose a data source', View.Status) > 0);
end;

procedure TDataSourceViewTest.TheFirstStepOfASourceHasNoBack;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    //  Back out of it would un-choose the source, and the list is on screen.
    AssertFalse(WizardView(FWizard).BackEnabled);
end;

procedure TDataSourceViewTest.TheQuestionIsOnScreenWhileItIsAsked;
var
    View: TWizardView;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    View := WizardView(FWizard);
    AssertTrue('fields', View.FieldsVisible);
    AssertTrue('header', View.HeaderVisible);
    AssertFalse('prompt', View.PromptVisible);
    AssertFalse('results', View.ResultsVisible);
    AssertFalse('preview', View.PreviewVisible);
    AssertFalse('folder: nothing fetched', View.FolderVisible);
    AssertTrue('Next', View.NextEnabled);
    AssertFalse('Create', View.CreateEnabled);
end;

{ ---- the breadcrumb -------------------------------------------------------- }

procedure TDataSourceViewTest.WithNoSourceThereAreNoCrumbs;
begin
    AssertEquals(0, Length(WizardCrumbs(FWizard)));
end;

procedure TDataSourceViewTest.EveryStageOfTheSourceIsACrumbNumberedFromOne;
var
    Crumbs: TWizardCrumbs;
begin
    //  A web address is asked and previewed; it has no Choose step, and
    //  choosing the source is not a stage.
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    Crumbs := WizardCrumbs(FWizard);
    AssertEquals(2, Length(Crumbs));
    AssertEquals('1. Find', Crumbs[0].Text);
    AssertEquals('2. Preview', Crumbs[1].Text);
end;

procedure TDataSourceViewTest.OnlyCrumbsAfterTheFirstHaveASeparator;
var
    Crumbs: TWizardCrumbs;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    Crumbs := WizardCrumbs(FWizard);
    AssertFalse(Crumbs[0].SeparatorBefore);
    AssertTrue(Crumbs[1].SeparatorBefore);
end;

procedure TDataSourceViewTest.TheStepOnScreenIsCurrentAndTheRestAreFuture;
var
    Crumbs: TWizardCrumbs;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    Crumbs := WizardCrumbs(FWizard);
    AssertTrue(Crumbs[0].State = csCurrent);
    AssertTrue(Crumbs[1].State = csFuture);
end;

procedure TDataSourceViewTest.ACrumbNotYetReachedCannotBeClicked;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.EnterChosenSource;
    AssertFalse(WizardCrumbs(FWizard)[1].Clickable);
end;

{ ---- the wait -------------------------------------------------------------- }

procedure TDataSourceViewTest.BeforeAnythingArrivesItSaysItIsAsking;
begin
    AssertEquals('|  Asking the service...', DownloadStatusText(0, 0, 0, 0));
end;

procedure TDataSourceViewTest.WithNoLengthItCountsWhatArrived;
begin
    AssertEquals('|  5 KB received', DownloadStatusText(0, 5 * 1024, 0, 0));
end;

procedure TDataSourceViewTest.WithALengthItCountsAgainstIt;
begin
    AssertEquals('|  5 of 20 KB',
        DownloadStatusText(0, 5 * 1024, 20 * 1024, 0));
end;

procedure TDataSourceViewTest.AfterAFewSecondsItSaysHowLongAndHowToStop;
begin
    AssertEquals('|  Asking the service...', DownloadStatusText(0, 0, 0, 2));
    AssertEquals('|  Asking the service...  (3 s - Stop to give up)',
        DownloadStatusText(0, 0, 0, 3));
end;

procedure TDataSourceViewTest.TheSpinnerTurnsWithEachFrame;
begin
    AssertEquals('/', DownloadStatusText(1, 0, 0, 0)[1]);
    AssertEquals('-', DownloadStatusText(2, 0, 0, 0)[1]);
    AssertEquals('\', DownloadStatusText(3, 0, 0, 0)[1]);
    AssertEquals('|', DownloadStatusText(4, 0, 0, 0)[1]);
end;

{ ---- the outcome ----------------------------------------------------------- }

procedure TDataSourceViewTest.StoppingIsSaidInTheStatusLine;
begin
    //  Cancelled wins over the error text it carries.
    AssertTrue(FailureShown(True, 'Stopped.') = fsStatusLine);
end;

procedure TDataSourceViewTest.FailingIsPutInFrontOfTheUser;
begin
    AssertTrue(FailureShown(False, 'The service did not answer.') = fsDialog);
end;

procedure TDataSourceViewTest.SucceedingShowsNothing;
begin
    AssertTrue(FailureShown(False, '') = fsNothing);
end;

initialization
    RegisterTest('unit', TDataSourceViewTest);
end.
