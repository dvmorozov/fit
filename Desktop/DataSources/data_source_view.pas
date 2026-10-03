// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the data source window shows at each step, decided where a test
can reach it.)

THE WINDOW DRAWS; THIS DECIDES. data_source_window is an LCL descendant, and a
decision taken inside one is unreachable by any test - so which panel is on
screen, which buttons are pressable, what the breadcrumb says, how an answer is
read back out of a box and what a download's status line reads were all rules
nothing ran. They are here now, over plain records and the wizard, and the
window reads the answers into its controls without deciding anything itself.

WHAT STAYS IN THE WINDOW: which LCL control stands for which kind of field,
fonts and colours, and the wait loop that pumps messages while a download runs.
Those are drawing, and drawing is what the window is for.

NOTHING HERE NAMES A WIDGET. A field is answered by a kind of editor
(TFieldEditorKind), a crumb has a state rather than a font, and a failure is
shown in a place rather than by a dialog class - so a second front end could
draw the same decisions differently.
}
unit data_source_view;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, data_source, data_source_advice, data_source_wizard,
    title_points_set;

const
    { What a date looks like in this window, and on the wire. One constant, so
      the box, the calendar and the query cannot disagree. }
    IsoDateFormat = 'yyyy-mm-dd';

type
    { Which editor a declared field is answered in. }
    TFieldEditorKind = (
        fekText,        //  free text, nothing to choose from
        fekDate,        //  a calendar
        fekClosedList,  //  one of the choices and nothing else
        fekOpenList     //  the common answers offered, any other taken
        );

    { The two text columns of one row in the Choose step's list. }
    TResultRowText = record
        Details: string;
        Size: string;
    end;

    { What is on screen, and what may be pressed, at the wizard's step. }
    TWizardView = record
        PromptVisible: boolean;
        HeaderVisible: boolean;
        FieldsVisible: boolean;
        ResultsVisible: boolean;
        PreviewVisible: boolean;
        FolderVisible: boolean;
        BackEnabled: boolean;
        NextEnabled: boolean;
        CreateEnabled: boolean;
        { Why Create is not pressable, or ''. }
        Status: string;
    end;

    { Where a crumb's step lies relative to the one on screen. }
    TCrumbState = (csPast, csCurrent, csFuture);

    TWizardCrumb = record
        Step: TWizardStep;
        Text: string;
        { Whether clicking it may go there (TDataSourceWizard.CanGoTo). }
        Clickable: boolean;
        { Whether a '>' separates it from the crumb before it. }
        SeparatorBefore: boolean;
        State: TCrumbState;
    end;

    TWizardCrumbs = array of TWizardCrumb;

    { Where a finished download's failure is told. }
    TFailureShown = (
        fsNothing,      //  nothing failed
        fsStatusLine,   //  the user stopped it: said in the line, no box
        fsDialog        //  it failed: put in front of the user
        );

{ The editor AField is answered in. A DATE IS PICKED FROM A CALENDAR, because a
  typed date is a format to get wrong. A field that declares choices is a list
  of their captions - CLOSED when the field is a choice and nothing else, OPEN
  when it offers the common answers and still takes any other, which is what a
  name needs where the service knows thousands. Anything else is typed. }
function FieldEditorKind(const AField: TInputFieldDecl): TFieldEditorKind;

{ The row a closed list starts on: the one whose caption AValue has, or the
  first when nothing offers it - a closed list with nothing selected is an
  answer nobody gave. }
function InitialChoiceIndex(const AChoices: TInputChoices;
    const AValue: string): longint;

{ The line under field AId: its hint, WITH ITS CONDITION when it has one - a
  greyed field says what would ask it, in the words on screen. }
function FieldHintText(const AFields: TInputFieldDecls; const AId: string): string;

{ The choices field AId declared, or none when no field has that id. }
function FieldChoices(const AFields: TInputFieldDecls;
    const AId: string): TInputChoices;

{ A date box's answer as the wire takes it. Empty stays EMPTY - an empty date
  means "unbounded", and the picker's Date would answer with a day nobody
  chose; anything else is ADate in ONE FORM whatever this machine shows: the
  services these sources ask take ISO dates, and the picker shows the user's
  own order. }
function WireDate(const AText: string; ADate: TDateTime): string;

{ A list box's answer as the source understands it. A caption that is one of
  AChoices becomes its value; free text typed into an open list is the answer
  itself; a closed list holding no caption it offers has no answer. }
function AnswerFromChoiceText(const AChoices: TInputChoices;
    const AText: string; AClosed: boolean): string;

{ The columns beside a found item's title. A row that cannot be taken forward
  stays VISIBLE and says why where its details would be: what a record holds is
  part of what the user came to see. A size the catalogue did not give is
  blank, not "0 KB". }
function ResultRowText(const ARow: TDataSourceItem;
    const AVerdict: TDataSourceVerdict): TResultRowText;

{ What the preview says about the file at AFilePath: its name, where it came
  from, then what was read from it - or why nothing could be. }
function PreviewLines(const AFilePath, AOriginText: string;
    APoints: TTitlePointsSet; const APreviewError: string): TStringArray;

{ What is on screen at AWizard's step. The source list is not a page: it is the
  left-hand side and it never hides, so with no source chosen the right-hand
  side is a prompt. }
function WizardView(AWizard: TDataSourceWizard): TWizardView;

{ The breadcrumb over the chosen source's panel: one crumb per stage of its own
  sequence (choosing the source is not one), numbered from one. A stage BEHIND
  YOU repeats what you answered there, since it is no longer on screen
  anywhere else. }
function WizardCrumbs(AWizard: TDataSourceWizard): TWizardCrumbs;

{ The status line while a download runs. A SPINNER, NOT A SILENT WAIT: a service
  can take tens of seconds, and a window that does not visibly move during it
  IS a hung window to anyone looking at it. AFrame turns the spinner; what has
  arrived is shown against the total when the service sent one; and after the
  first few seconds, how long it has been - a slow service and a stuck one look
  the same until something counts. }
function DownloadStatusText(AFrame: longint; ABytes, ATotal: int64;
    ASeconds: longint): string;

{ Where a finished download's outcome is told. STOPPING IS NOT FAILING: a
  download the user cancelled is said in the status line and no box appears. }
function FailureShown(ACancelled: boolean; const AError: string): TFailureShown;

implementation

function FieldEditorKind(const AField: TInputFieldDecl): TFieldEditorKind;
begin
    if AField.Kind = ifDate then
        Result := fekDate
    else if Length(AField.Choices) = 0 then
        //  Before Kind: a choice with nothing to choose from can only be typed.
        Result := fekText
    else if AField.Kind = ifChoice then
        Result := fekClosedList
    else
        Result := fekOpenList;
end;

function InitialChoiceIndex(const AChoices: TInputChoices;
    const AValue: string): longint;
var
    Caption_: string;
    i: longint;
begin
    Caption_ := CaptionOfChoice(AChoices, AValue);
    for i := 0 to High(AChoices) do
        if AChoices[i].Caption = Caption_ then
            Exit(i);
    Result := 0;
end;

function FieldHintText(const AFields: TInputFieldDecls; const AId: string): string;
var
    i: longint;
    Hint: string;
begin
    Hint := '';
    for i := 0 to High(AFields) do
        if AFields[i].Id = AId then
        begin
            Hint := AFields[i].Hint;
            Break;
        end;
    Result := Trim(Hint + ' ' + FieldCondition(AFields, AId));
end;

function FieldChoices(const AFields: TInputFieldDecls;
    const AId: string): TInputChoices;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(AFields) do
        if AFields[i].Id = AId then
            Exit(AFields[i].Choices);
end;

function WireDate(const AText: string; ADate: TDateTime): string;
begin
    if Trim(AText) = '' then
        Result := ''
    else
        Result := FormatDateTime(IsoDateFormat, ADate);
end;

function AnswerFromChoiceText(const AChoices: TInputChoices;
    const AText: string; AClosed: boolean): string;
begin
    Result := ValueOfChoice(AChoices, AText);
    if Result <> '' then
        Exit;
    if AClosed then
        Result := ''
    else
        Result := AText;
end;

function ResultRowText(const ARow: TDataSourceItem;
    const AVerdict: TDataSourceVerdict): TResultRowText;
begin
    if AVerdict.Allowed then
        Result.Details := ARow.Details
    else
        Result.Details := AVerdict.Reason;
    if ARow.Size > 0 then
        Result.Size := IntToStr(ARow.Size div 1024) + ' KB'
    else
        Result.Size := '';
end;

function PreviewLines(const AFilePath, AOriginText: string;
    APoints: TTitlePointsSet; const APreviewError: string): TStringArray;

    procedure Add(const ALine: string);
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := ALine;
    end;

begin
    Result := nil;
    Add('File: ' + ExtractFileName(AFilePath));
    Add('From: ' + AOriginText);
    if APoints <> nil then
    begin
        Add('');
        //  A file read to no points has no range to state, and indexing its
        //  first point would raise in the middle of drawing the page.
        if APoints.PointsCount = 0 then
            Add('0 points')
        else
            Add(Format('%d points, x from %g to %g',
                [APoints.PointsCount, APoints.PointXCoord[0],
                APoints.PointXCoord[APoints.PointsCount - 1]]));
    end
    else if APreviewError <> '' then
    begin
        Add('');
        Add(APreviewError);
    end;
end;

function WizardView(AWizard: TDataSourceWizard): TWizardView;
var
    Step: TWizardStep;
    Chosen: boolean;
    Verdict: TDataSourceVerdict;
begin
    Step := AWizard.Step;
    Chosen := AWizard.SourceClass <> nil;
    Result.PromptVisible := not Chosen;
    Result.HeaderVisible := Chosen;
    Result.FieldsVisible := Step = wsFind;
    Result.ResultsVisible := Step = wsChoose;
    Result.PreviewVisible := Step = wsPreview;
    //  Where the file is kept, once there is a file.
    Result.FolderVisible := (Step = wsPreview) and (AWizard.FilePath <> '');
    //  BACK WALKS THIS SOURCE'S OWN STEPS. Going back out of the first one
    //  would mean un-choosing the source, and the list that chose it is on
    //  screen - so there is nothing for Back to do there.
    Result.BackEnabled := AWizard.StageOf(Step) > 1;
    Result.NextEnabled := Chosen and (Step <> wsPreview);
    Verdict := AWizard.CanGoNext;
    Result.CreateEnabled := (Step = wsPreview) and Verdict.Allowed;
    //  THE REASON, BESIDE THE BUTTON IT DISABLES.
    if Verdict.Allowed then
        Result.Status := ''
    else
        Result.Status := Verdict.Reason;
end;

function WizardCrumbs(AWizard: TDataSourceWizard): TWizardCrumbs;
var
    Step: TWizardStep;
    Steps: TWizardStepSet;
    Crumb: TWizardCrumb;
    Choice: string;
begin
    Result := nil;
    if AWizard.SourceClass = nil then
        Exit;
    Steps := AWizard.Steps;
    for Step := Low(TWizardStep) to High(TWizardStep) do
    begin
        if not (Step in Steps) or (Step = wsSource) then
            Continue;
        Crumb.Step := Step;
        Crumb.SeparatorBefore := Length(Result) > 0;
        Crumb.Text := Format('%d. %s', [AWizard.StageOf(Step), StepCaption(Step)]);
        if Step < AWizard.Step then
        begin
            Crumb.State := csPast;
            Choice := AWizard.ChoiceAt(Step);
            if Choice <> '' then
                Crumb.Text := Crumb.Text + ': ' + Choice;
        end
        else if Step = AWizard.Step then
            Crumb.State := csCurrent
        else
            Crumb.State := csFuture;
        Crumb.Clickable := AWizard.CanGoTo(Step).Allowed;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Crumb;
    end;
end;

function DownloadStatusText(AFrame: longint; ABytes, ATotal: int64;
    ASeconds: longint): string;
const
    Spinner: array[0..3] of string = ('|', '/', '-', '\');
var
    Turn: string;
begin
    Turn := Spinner[Abs(AFrame) mod 4];
    if ATotal > 0 then
        Result := Format('%s  %d of %d KB', [Turn, ABytes div 1024,
            ATotal div 1024])
    else if ABytes > 0 then
        //  Most services send no length, so there is no percentage to show -
        //  what there is, is how much has arrived.
        Result := Format('%s  %d KB received', [Turn, ABytes div 1024])
    else
        Result := Format('%s  Asking the service...', [Turn]);
    if ASeconds >= 3 then
        Result := Result + Format('  (%d s - Stop to give up)', [ASeconds]);
end;

function FailureShown(ACancelled: boolean; const AError: string): TFailureShown;
begin
    if ACancelled then
        Result := fsStatusLine
    else if AError <> '' then
        Result := fsDialog
    else
        Result := fsNothing;
end;

end.
