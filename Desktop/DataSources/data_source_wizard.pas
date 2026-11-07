// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The steps of finding data, decided here rather than in a window.)

THE WHOLE DIALOG'S BEHAVIOUR IS IN THIS UNIT and none of it is in the form,
because a decision made inside an LCL descendant is unreachable by any test: a
TStringGrid with no parent raises as soon as it is sized. The form reads
controls and forwards them; everything below - which steps exist, when the next
one may be entered, what is refused and why, what the preview says - is here and
is covered.

THE STEPS ARE DERIVED, NEVER LISTED. WizardSteps asks the source what it
declared - does it ask questions, can its results hold other things - and
answers which steps that implies. There is no table of "source X needs step Y"
anywhere, which is what lets a module add a source without touching this unit.

  a series feed   asks a question, answers with one file    Source, Find, Preview
  the samples     ask nothing, answer with files            Source, Choose, Preview
  a repository    asks a question, answers with records     all four

THE PREVIEW IS THE IMPORT. The file is downloaded once, into the cache, and
parsed by the very loader the import will use - so what the user approves is
what the project gets, and pressing Create fetches nothing again. Every
"worked in tests, not in the app" defect in findings.md was a second path doing
what the first had already done, slightly differently.
}
unit data_source_wizard;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, data_source_registry, data_source_advice,
    download_cache, download_session, data_loader, data_loader_registry,
    data_source_failure, int_web_client, title_points_set, sample_columns;

type
    TWizardStep = (
        wsSource,   //  which source
        wsFind,     //  the source's own questions
        wsChoose,   //  which of what was found
        wsPreview   //  what was downloaded, before it becomes a project
        );

    TWizardStepSet = set of TWizardStep;

    { Where a downloaded file came from, kept with the project so that a result
      can be traced back to what it was computed from. }
    TDownloadOrigin = record
        SourceId: string;
        SourceTitle: string;
        { What was asked for, as the user typed it - one line, for a human. }
        Query: string;
        { The address the bytes came from, or the file they were copied from. }
        Address: string;
        RetrievedAt: TDateTime;
    end;

    TDataSourceWizard = class(TObject)
    private
        FWeb: IWebClient;
        FSourceClass: TDataSourceClass;
        FSource: TDataSource;
        FQuery: TDataSourceQuery;
        FItems: TDataSourceItems;
        { The containers opened to get here, so Back means something. }
        FOpened: TDataSourceItems;
        FSelected: TDataSourceItem;
        FHasSelection: boolean;
        { The row of the list now shown that the user is on, file or record,
          or -1. A RECORD IS GONE INTO WITH NEXT, like any other step forward,
          so the wizard has to know which one - Select only ever holds a file. }
        FPointed: longint;
        { Whether the record shown was opened by the wizard itself because the
          search found nothing else - a DOI names one record. Back then returns
          to the question rather than to a list of that one record. }
        FOpenedAlone: boolean;
        { Whether the source has been asked since it was chosen. }
        FSearched: boolean;
        FStep: TWizardStep;
        { The furthest step this wizard has been at. }
        FReached: TWizardStep;
        FCacheRoot: string;
        FFilePath: string;
        FPreview: TTitlePointsSet;
        FPreviewError: string;
        FOrigin: TDownloadOrigin;
        FLogPath: string;
        { WHICH COLUMN IS THE VALUE, for a file holding more than one: the
          loader's own choice first, then every other column it kept beside
          the profile (sample_columns), by name. }
        FValueColumns: TStringArray;
        FOtherColumns: TSampleColumns;
        FChosenColumn: longint;
        { What the loader read, kept while another column is shown, so that
          going back to its own choice reads nothing again. }
        FLoaded: TTitlePointsSet;
        { The file as it was downloaded; FFilePath is a file of the chosen
          column when one was chosen. }
        FDownloadedPath: string;
        FDownloadedQuery: string;
        function Info: TDataSourceInfo;
        { EVERY CALL INTO A SOURCE GOES THROUGH THESE, so that each is logged -
          what was asked and how many items came back - and each failure is
          logged with what was being done, and worded by data_source_failure's
          rules. A call made directly would be the one nobody can explain
          afterwards. }
        function AskSearch(const AQuery: TDataSourceQuery): TDataSourceItems;
        function AskChildren(const AItem: TDataSourceItem): TDataSourceItems;
        function AskDownload(const AItem: TDataSourceItem; ADest: TStream): string;
        { Logs E as a failure of AOperation on AItem and rewords it where the
          rules say so; the caller re-raises the same object, so its class -
          a stop is still a stop - reaches the window unchanged. }
        procedure KeepColumns(ALoader: TDataLoader);
        procedure NoteFailure(AOperation: TSourceOperation;
            const AItem: TDataSourceItem; E: Exception);
        { The search as an operation: a source that asks nothing is listing. }
        function SearchOperation: TSourceOperation;
        procedure FreeSource;
        procedure ForgetDownload;
        function QueryAsText: string;
        { Whether the user is on a record of the list now shown. }
        function PointsAtRecord: boolean;
        { Opens the only row of the list when it is a record: there is nothing
          to choose between. }
        procedure OpenALoneRecord;
    protected
        { Writes what was downloaded where it will be kept. Virtual so a test
          can drive the whole wizard without a disk; the production path is the
          one sentence below it. }
        function SaveDownload(const APath: string; AData: TStream): string; virtual;
    public
        constructor Create(AWeb: IWebClient);
        destructor Destroy; override;

        { Which steps this source needs. }
        class function StepsFor(const AInfo: TDataSourceInfo): TWizardStepSet;

        { Step 1. Raises when nothing is registered under AId - which is what a
          remembered import of a source this build no longer contains looks
          like. }
        procedure ChooseSource(const AId: string);
        { The same, by the class itself - which is what ChooseSource does once
          the registry has answered. A module holding a source class already
          has no reason to look its own id up, and a test has no reason to put
          a probe in the global registry to reach this. }
        procedure ChooseSourceClass(ASourceClass: TDataSourceClass);

        { Step 2. }
        procedure SetField(const AId, AValue: string);
        function FieldValue(const AId: string): string;
        { Whether the field AId is asked now - what the window greys by. }
        function FieldEnabled(const AId: string): boolean;
        { Whether Search may run, and why not. }
        function SearchVerdict: TDataSourceVerdict;
        procedure Search;

        { Step 3. }
        function ItemCount: longint;
        function Item(AIndex: longint): TDataSourceItem;
        { Whether this row can be taken forward, and what it is when it cannot. }
        function ChooseVerdict(AIndex: longint): TDataSourceVerdict;
        { Opens a container: its children replace the list. What Next does on
          a record (Advance), and what the wizard does by itself for a record
          found alone. }
        procedure Open(AIndex: longint);
        { Goes back up one container, or to the search results. }
        procedure CloseContainer;
        function InsideContainer: boolean;
        procedure Select(AIndex: longint);

        { Step 4. Downloads the selected file into the cache and parses it with
          the registered loader for its kind. }
        procedure Download;
        function CreateVerdict: TDataSourceVerdict;

        { Which steps exist for the chosen source, and where we are. }
        function Steps: TWizardStepSet;
        function CanGoNext: TDataSourceVerdict;
        procedure GoNext;
        { WHAT THE WINDOW'S NEXT DOES, which is more than GoNext: a question
          is searched before it is left, and a source with no Choose step
          takes the one file it answered with - then the next step is
          entered. It was the window's, where no test could reach it. }
        procedure Advance;
        { Enters the chosen source's first step, which choosing it in the list
          does: the list is the choice and stays on screen, so there is nothing
          left to confirm with a Next. Nothing happens to a source already
          past its first step. }
        procedure EnterChosenSource;
        { Whether the step on screen is a preview with nothing fetched yet.
          ENTERING THE PREVIEW IS WHAT DOWNLOADS: one fetch, and what is
          previewed is what is imported. }
        function NeedsDownload: boolean;
        { The user is on row AIndex: selects it when it is a file that can be
          taken forward. False when it cannot, and then only where the user is
          changes: a row that cannot be taken stays on screen saying why
          (ChooseVerdict), and a record is gone into by Next (Advance). }
        function SelectIfAllowed(AIndex: longint): boolean;
        { The row of the list now shown that holds the selected file - or the
          record the user is on - or -1.
          WHAT THE WINDOW HIGHLIGHTS AFTER REDRAWING THE LIST: every click
          redraws it, and a redraw clears the highlight, so without this the
          click that chose a file showed nothing at all. Matched by Id, which
          is stable while a row is on screen, so a list that no longer holds
          the choice (another container opened) highlights nothing. }
        function SelectedIndex: longint;
        procedure GoBack;
        { Returns to a step already completed - what clicking the
          breadcrumb does. A step not yet reached is refused: a breadcrumb is
          a place to go
          BACK from, and jumping forward over a question would arrive at a
          page whose answer nothing has. }
        procedure GoTo_(AStep: TWizardStep);
        { Whether that step can be gone to, and why not. }
        function CanGoTo(AStep: TWizardStep): TDataSourceVerdict;
        { The furthest step reached so far, which is what makes a step
          "completed" rather than merely earlier in the list. }
        function Reached: TWizardStep;
        { Which stage of the chosen source's own sequence AStep is, counting
          from one - and how many there are.

          CHOOSING THE SOURCE IS NOT A STAGE. It is what the window's left-hand
          list is FOR, and it is on screen the whole time; counting it as a
          step made the sequence depend on the thing that chose the sequence,
          which is what looked wrong. So the stages are the steps AFTER it,
          and a source with nothing to ask has one. }
        function StageOf(AStep: TWizardStep): longint;
        function StageCount: longint;
        { What was chosen at a completed step - the source's name, the file's
          name - or '' where there is nothing to show. The window decides how
          to write it; which choice belongs to which step is decided here. }
        function ChoiceAt(AStep: TWizardStep): string;
        function Step: TWizardStep;

        property HasSelection: boolean read FHasSelection;
        property Selected: TDataSourceItem read FSelected;
        { The profile the preview parsed, or nil. Owned here. }
        property Preview: TTitlePointsSet read FPreview;
        property PreviewError: string read FPreviewError;
        property FilePath: string read FFilePath;
        property Origin: TDownloadOrigin read FOrigin;
        { Where downloads are kept. Set by the caller so a test can point it
          somewhere harmless. }
        property CacheRoot: string read FCacheRoot write FCacheRoot;
        { The log file a failure's message points at: the one this process
          writes (GetLogFilePath) unless the caller names another. }
        property LogPath: string read FLogPath write FLogPath;

        { THE VALUE COLUMN, for a downloaded table with more than one: what
          the preview offers to plot against the argument. Empty for a file
          that has only the one. The first is the reader's own choice. }
        function ValueColumns: TStringArray;
        function ChosenValueColumn: longint;
        { Plots column AIndex of ValueColumns as the value. THE PREVIEW IS
          STILL THE IMPORT: a column other than the reader's is written as a
          two-column file of its own, in the same cache, and that file is what
          Create Project imports - so the project gets exactly what the
          preview shows, and its origin says which column it was. }
        procedure ChooseValueColumn(AIndex: longint);
        property SourceClass: TDataSourceClass read FSourceClass;
    end;

{ The steps a source with these declared facts needs. Free function as well as
  a class method so the truth table can be tested without a wizard. }
function WizardSteps(const AInfo: TDataSourceInfo): TWizardStepSet;

{ What one step is called in the breadcrumb. }
function StepCaption(AStep: TWizardStep): string;

implementation

uses
    Math, log, web_client;

function WizardSteps(const AInfo: TDataSourceInfo): TWizardStepSet;
begin
    //  Source and Preview always: something must be chosen, and nothing becomes
    //  a project unseen.
    Result := [wsSource, wsPreview];
    //  A source that declares no question has nothing to ask.
    if Length(AInfo.QueryFields) > 0 then
        Include(Result, wsFind);
    //  Choosing is needed when what comes back may be several things: a source
    //  whose answer is always the one file it was asked for would make the user
    //  confirm their own typing.
    if AInfo.HasContainers or (Length(AInfo.QueryFields) = 0) then
        Include(Result, wsChoose);
end;

function StepCaption(AStep: TWizardStep): string;
begin
    case AStep of
        wsSource: Result := 'Source';
        wsFind: Result := 'Find';
        wsChoose: Result := 'Choose';
        else
            Result := 'Preview';
    end;
end;

constructor TDataSourceWizard.Create(AWeb: IWebClient);
begin
    inherited Create;
    FWeb := AWeb;
    FStep := wsSource;
    FPointed := -1;
    FCacheRoot := DownloadsRoot;
    FLogPath := GetLogFilePath;
end;

function TDataSourceWizard.SearchOperation: TSourceOperation;
begin
    if Length(Info.QueryFields) = 0 then
        Result := soList
    else
        Result := soSearch;
end;

procedure TDataSourceWizard.NoteFailure(AOperation: TSourceOperation;
    const AItem: TDataSourceItem; E: Exception);
var
    Line: string;
begin
    Line := SourceFailureLogLine(AOperation, Info.Id, Info.Title, AItem.Title,
        QueryAsText, AItem.Ref, E.ClassName, E.Message);
    if E is EWebCancelled then
        //  Nothing is wrong: the user stopped it, and is told so in a line.
        WriteLog(Line, Notification)
    else if (E is EDataSourceError) and not (E is EDataSourceFault) then
        //  AN ANSWER, worded by the source - "nothing found" is not a fault,
        //  and sending the user to a log for it would make it read as one.
        WriteLog(Line, Notification)
    else
    begin
        WriteLog(Line, Warning);
        if (E is EWebError) or (E is EDataSourceFault) then
            //  Already worded for the user: what happened and what to try.
            E.Message := WithLogPointer(E.Message, FLogPath)
        else
            //  Worded by nobody - a defect, a reply shaped as nothing here
            //  expected. What was being done is what makes it reportable.
            E.Message := UnexpectedFailureMessage(AOperation, Info.Title,
                AItem.Title, E.Message, FLogPath);
    end;
end;

function TDataSourceWizard.AskSearch(
    const AQuery: TDataSourceQuery): TDataSourceItems;
var
    NoItem: TDataSourceItem;
begin
    NoItem := Default(TDataSourceItem);
    try
        Result := FSource.Search(AQuery);
        WriteLog(SourceSuccessLogLine(SearchOperation, Info.Id, Info.Title, '',
            QueryAsText, Length(Result)), Debug);
    except
        on E: Exception do
        begin
            NoteFailure(SearchOperation, NoItem, E);
            raise;
        end;
    end;
end;

function TDataSourceWizard.AskChildren(
    const AItem: TDataSourceItem): TDataSourceItems;
begin
    try
        Result := FSource.Children(AItem);
        WriteLog(SourceSuccessLogLine(soOpen, Info.Id, Info.Title, AItem.Title,
            QueryAsText, Length(Result)), Debug);
    except
        on E: Exception do
        begin
            NoteFailure(soOpen, AItem, E);
            raise;
        end;
    end;
end;

function TDataSourceWizard.AskDownload(const AItem: TDataSourceItem;
    ADest: TStream): string;
begin
    try
        Result := FSource.Download(AItem, ADest);
        WriteLog(SourceSuccessLogLine(soDownload, Info.Id, Info.Title,
            AItem.Title, QueryAsText, 1) + ', ' + IntToStr(ADest.Size) +
            ' bytes', Debug);
    except
        on E: Exception do
        begin
            NoteFailure(soDownload, AItem, E);
            raise;
        end;
    end;
end;

destructor TDataSourceWizard.Destroy;
begin
    ForgetDownload;
    FreeSource;
    inherited Destroy;
end;

procedure TDataSourceWizard.FreeSource;
begin
    FreeAndNil(FSource);
    FSourceClass := nil;
end;

procedure TDataSourceWizard.ForgetDownload;
begin
    FreeAndNil(FPreview);
    FreeAndNil(FLoaded);
    FPreviewError := '';
    FFilePath := '';
    FDownloadedPath := '';
    FValueColumns := nil;
    FOtherColumns := nil;
    FChosenColumn := 0;
end;

function TDataSourceWizard.ValueColumns: TStringArray;
begin
    Result := Copy(FValueColumns);
end;

function TDataSourceWizard.ChosenValueColumn: longint;
begin
    Result := FChosenColumn;
end;

{ What the loader read, and the columns it kept beside the value, as the
  choices the preview offers. }
procedure TDataSourceWizard.KeepColumns(ALoader: TDataLoader);
var
    i: longint;
    Name_: string;
begin
    //  ONLY A FORMAT THAT NAMES ITS VALUE COLUMN offers the others against
    //  it: a table. A format whose columns beside the value MEAN something -
    //  a price bar's open, high and low - keeps them for its own drawing, and
    //  plotting one of them alone would lose what the file was.
    Name_ := ALoader.ValueName;
    if Name_ = '' then
        Exit;
    FOtherColumns := ALoader.SampleColumns;
    if Length(FOtherColumns) = 0 then
        Exit;
    SetLength(FValueColumns, Length(FOtherColumns) + 1);
    FValueColumns[0] := Name_;
    for i := 0 to High(FOtherColumns) do
        FValueColumns[i + 1] := FOtherColumns[i].Name;
    FLoaded := TTitlePointsSet.CreateFromPoints(nil, FPreview);
    FDownloadedQuery := FOrigin.Query;
end;

procedure TDataSourceWizard.ChooseValueColumn(AIndex: longint);
var
    Column: TSampleColumn;
    Points: TTitlePointsSet;
    Text: TStringList;
    Data: TStringStream;
    i: longint;
    Settings: TFormatSettings;
begin
    if (AIndex < 0) or (AIndex > High(FValueColumns)) then
        raise EDataSourceError.Create('There is no column number ' +
            IntToStr(AIndex + 1) + ' to plot.');
    if AIndex = FChosenColumn then
        Exit;
    if AIndex = 0 then
    begin
        //  The reader's own choice: what it read, and the file as it came.
        FreeAndNil(FPreview);
        FPreview := TTitlePointsSet.CreateFromPoints(nil, FLoaded);
        FFilePath := FDownloadedPath;
        FOrigin.Query := FDownloadedQuery;
        FChosenColumn := 0;
        Exit;
    end;

    Column := FOtherColumns[AIndex - 1];
    Settings := DefaultFormatSettings;
    Settings.DecimalSeparator := '.';
    Points := TTitlePointsSet.Create(nil);
    Text := TStringList.Create;
    try
        for i := 0 to FLoaded.PointsCount - 1 do
        begin
            //  A cell that held no number is no point: the column's gaps are
            //  not zeros.
            if (i > High(Column.Values)) or IsNan(Column.Values[i]) then
                Continue;
            Points.AddNewPoint(FLoaded.PointXCoord[i], Column.Values[i]);
            Text.Add(FloatToStr(FLoaded.PointXCoord[i], Settings) + #9 +
                FloatToStr(Column.Values[i], Settings));
        end;
        if Points.PointsCount = 0 then
            raise EDataSourceError.Create('The column "' + Column.Name +
                '" holds no numbers beside the argument, so there is nothing ' +
                'to plot.');
        Data := TStringStream.Create(Text.Text);
        try
            //  A FILE OF ITS OWN, beside the download: what is previewed is
            //  what is imported, and the import reads a file.
            FFilePath := SaveDownload(CachePath(FCacheRoot, Info.Id,
                ChangeFileExt(ExtractFileName(FDownloadedPath), '') + ' - ' +
                Column.Name + '.xy'), Data);
        finally
            Data.Free;
        end;
        FreeAndNil(FPreview);
        FPreview := Points;
        Points := nil;
        FChosenColumn := AIndex;
        FOrigin.Query := FDownloadedQuery;
        if FOrigin.Query <> '' then
            FOrigin.Query := FOrigin.Query + ', ';
        FOrigin.Query := FOrigin.Query + 'value column ' + Column.Name;
    finally
        Text.Free;
        Points.Free;
    end;
end;

function TDataSourceWizard.Info: TDataSourceInfo;
begin
    if FSourceClass = nil then
        raise EDataSourceError.Create('No data source has been chosen.');
    Result := FSourceClass.Info;
end;

class function TDataSourceWizard.StepsFor(const AInfo: TDataSourceInfo): TWizardStepSet;
begin
    Result := WizardSteps(AInfo);
end;

procedure TDataSourceWizard.ChooseSource(const AId: string);
var
    Found: TDataSourceClass;
begin
    Found := FindDataSourceClass(AId);
    if Found = nil then
        //  What a remembered import of a source this build does not contain
        //  looks like: the module may simply not be built in, and saying which
        //  id was wanted is the whole of what the user can act on.
        raise EDataSourceError.Create('This build has no data source called "' +
            AId + '". It may come from a module that is not installed.');
    ChooseSourceClass(Found);
end;

procedure TDataSourceWizard.ChooseSourceClass(ASourceClass: TDataSourceClass);
begin
    if ASourceClass = nil then
        raise EDataSourceError.Create('No data source was chosen.');
    if ASourceClass = FSourceClass then
        Exit;
    FreeSource;
    ForgetDownload;
    FItems := nil;
    FOpened := nil;
    FOpenedAlone := False;
    FPointed := -1;
    FHasSelection := False;
    FSearched := False;
    //  BACK TO ITS OWN FIRST STEP. The source list is on screen the whole
    //  time, so this can be reached from any step - and the step reached
    //  belonged to the source being left. Keeping it would put the new
    //  source's name over the old one's page.
    FStep := wsSource;
    FReached := wsSource;
    FSourceClass := ASourceClass;
    FSource := ASourceClass.Create(FWeb);
    FQuery := DefaultQuery(ASourceClass.Info.QueryFields);
end;


procedure TDataSourceWizard.SetField(const AId, AValue: string);
begin
    FQuery := WithQueryValue(FQuery, AId, AValue);
end;

function TDataSourceWizard.FieldValue(const AId: string): string;
begin
    Result := QueryValue(FQuery, AId);
end;

function TDataSourceWizard.FieldEnabled(const AId: string): boolean;
begin
    Result := FieldIsEnabled(Info.QueryFields, AId, FQuery);
end;

function TDataSourceWizard.QueryAsText: string;
var
    i: longint;
    Fields: TInputFieldDecls;
    Value: string;
begin
    Result := '';
    Fields := Info.QueryFields;
    for i := 0 to High(Fields) do
    begin
        //  What was ASKED: a date left in a greyed box was not.
        Value := Trim(QueryValue(EffectiveQuery(Fields, FQuery), Fields[i].Id));
        if Value = '' then
            Continue;
        if Result <> '' then
            Result := Result + ', ';
        Result := Result + Fields[i].Caption + ' ' + Value;
    end;
end;

function TDataSourceWizard.SearchVerdict: TDataSourceVerdict;
begin
    Result := AdviseSearch(MissingRequiredField(Info.QueryFields, FQuery));
end;

procedure TDataSourceWizard.Search;
var
    Verdict: TDataSourceVerdict;
begin
    Verdict := SearchVerdict;
    if not Verdict.Allowed then
        //  The refusal the button already shows, raised in the same words: one
        //  decision, whether it is reached by a disabled button or by Enter.
        raise EDataSourceError.Create(Verdict.Reason);
    FOpened := nil;
    FOpenedAlone := False;
    FPointed := -1;
    FHasSelection := False;
    ForgetDownload;
    //  WHAT WAS ASKED, not what is in the boxes: a field greyed after it was
    //  filled in still holds its text, and must not travel.
    FItems := AskSearch(EffectiveQuery(Info.QueryFields, FQuery));
    FSearched := True;
end;

function TDataSourceWizard.ItemCount: longint;
begin
    Result := Length(FItems);
end;

function TDataSourceWizard.Item(AIndex: longint): TDataSourceItem;
begin
    if (AIndex < 0) or (AIndex > High(FItems)) then
        raise EDataSourceError.Create('There is no result number ' +
            IntToStr(AIndex + 1) + '.');
    Result := FItems[AIndex];
end;

function TDataSourceWizard.ChooseVerdict(AIndex: longint): TDataSourceVerdict;
var
    Row: TDataSourceItem;
begin
    Row := Item(AIndex);
    Result := AdviseChoose(Row.IsLeaf,
        FindDataLoaderClass(Row.FileName) <> nil,
        ExtractFileExt(Row.FileName));
end;

procedure TDataSourceWizard.Open(AIndex: longint);
var
    Row: TDataSourceItem;
    Verdict: TDataSourceVerdict;
begin
    Row := Item(AIndex);
    Verdict := AdviseOpen(Row.IsLeaf);
    if not Verdict.Allowed then
        raise EDataSourceError.Create(Verdict.Reason);
    SetLength(FOpened, Length(FOpened) + 1);
    FOpened[High(FOpened)] := Row;
    FItems := AskChildren(Row);
    FHasSelection := False;
    FPointed := -1;
end;

function TDataSourceWizard.PointsAtRecord: boolean;
begin
    Result := (FPointed >= 0) and (FPointed <= High(FItems)) and
        not FItems[FPointed].IsLeaf;
end;

procedure TDataSourceWizard.OpenALoneRecord;
begin
    if (Length(FOpened) = 0) and (Length(FItems) = 1) and not FItems[0].IsLeaf then
    begin
        Open(0);
        FOpenedAlone := True;
    end;
end;

procedure TDataSourceWizard.CloseContainer;
begin
    if Length(FOpened) = 0 then
        Exit;
    SetLength(FOpened, Length(FOpened) - 1);
    FHasSelection := False;
    FPointed := -1;
    if Length(FOpened) = 0 then
        //  Back at the top: re-run the search rather than keeping a stale copy
        //  of what it answered, which may be minutes old.
        FItems := AskSearch(FQuery)
    else
        FItems := AskChildren(FOpened[High(FOpened)]);
end;

function TDataSourceWizard.InsideContainer: boolean;
begin
    Result := Length(FOpened) > 0;
end;

procedure TDataSourceWizard.Select(AIndex: longint);
var
    Verdict: TDataSourceVerdict;
begin
    Verdict := ChooseVerdict(AIndex);
    if not Verdict.Allowed then
        raise EDataSourceError.Create(Verdict.Reason);
    FSelected := Item(AIndex);
    FHasSelection := True;
    ForgetDownload;
end;

function TDataSourceWizard.SaveDownload(const APath: string;
    AData: TStream): string;
var
    Output: TFileStream;
begin
    //  EVERY WAY A DISK REFUSES, in one sentence naming the place: a full
    //  disk, a read-only home, a folder somebody has removed underneath us.
    //  Without this the user is shown the run-time library's own words about
    //  a handle, which name nothing they can act on - and the download that
    //  succeeded looks like a download that failed.
    try
        ForceDirectories(ExtractFileDir(APath));
        Result := FreeCachePath(APath, @DefaultPathExists);
        Output := TFileStream.Create(Result, fmCreate);
        try
            AData.Position := 0;
            Output.CopyFrom(AData, AData.Size);
        finally
            Output.Free;
        end;
        //  WRITTEN, AND NOT YET WANTED. Every look at a source writes a file,
        //  because the preview has to read something; most of them are
        //  answered "no". The session throws them away - the one that became
        //  a project as soon as the import has run, the rest when the
        //  application closes. Its root is THIS wizard's cache, which is
        //  where every file it writes goes: there is no other folder.
        CurrentDownloadSession.CleanableRoot := FCacheRoot;
        CurrentDownloadSession.Wrote(Result);
    except
        on E: Exception do
            raise EDataSourceError.Create(
                SaveFailureMessage(ExtractFileDir(APath), E.Message));
    end;
end;

procedure TDataSourceWizard.Download;
var
    Buffer: TMemoryStream;
    Loader: TDataLoader;
    LoaderClass: TDataLoaderClass;
    Name_, Path: string;
begin
    if not FHasSelection then
        raise EDataSourceError.Create('Choose a file first.');
    if FCacheRoot = '' then
        raise EDataSourceError.Create('This computer has nowhere to keep ' +
            'downloaded files: no user data directory could be found.');

    ForgetDownload;
    Buffer := TMemoryStream.Create;
    try
        Name_ := AskDownload(FSelected, Buffer);
        Name_ := DownloadFileName(FSelected.FileName, Name_,
            Info.ProducesExtensions);
        Path := CachePath(FCacheRoot, Info.Id, Name_);
        FFilePath := SaveDownload(Path, Buffer);
    finally
        Buffer.Free;
    end;

    FOrigin.SourceId := Info.Id;
    FOrigin.SourceTitle := Info.Title;
    FOrigin.Query := QueryAsText;
    FOrigin.Address := FSelected.Ref;
    FOrigin.RetrievedAt := Now;

    //  THE PREVIEW IS READ BY THE LOADER THE IMPORT WILL USE. Anything else
    //  would be a second reader whose agreement with the first is a matter of
    //  hope - and the disagreement would appear after the project was created.
    LoaderClass := FindDataLoaderClass(FFilePath);
    if LoaderClass = nil then
    begin
        FPreviewError := 'No reader in this build handles ' +
            LowerCase(ExtractFileExt(FFilePath)) + ' files.';
        Exit;
    end;
    Loader := LoaderClass.Create(nil);
    try
        try
            Loader.LoadDataSet(FFilePath);
            FPreview := Loader.GetPointsSetCopy;
            FDownloadedPath := FFilePath;
            if FPreview.PointsCount > 0 then
                KeepColumns(Loader);
        except
            on E: Exception do
            begin
                //  THE READER'S OWN WORDS, kept rather than replaced: it knows
                //  what it expected and what it found. A service answering a
                //  bad request with a web page lands here, and "the file is
                //  not a data file" would lose which part of it was wrong.
                //  Not re-raised: a file that cannot be read is an ordinary
                //  outcome of asking for the wrong thing, and the preview is
                //  where the user sees that and chooses again. LOGGED, though:
                //  a file that passed every filter and still could not be read
                //  is what a new format's reader is written from.
                WriteLog(SourceFailureLogLine(soRead, Info.Id, Info.Title,
                    FSelected.Title, QueryAsText, FFilePath, E.ClassName,
                    E.Message), Warning);
                FPreviewError := E.Message;
            end;
        end;
    finally
        Loader.Free;
    end;
    if (FPreview <> nil) and (FPreview.PointsCount = 0) then
    begin
        //  Parsed, and holds nothing. Freed here so that what the preview
        //  shows and what CreateVerdict says cannot disagree.
        FreeAndNil(FPreview);
        if FPreviewError = '' then
            FPreviewError := 'The file was read, and holds no data points.';
    end;
end;

function TDataSourceWizard.CreateVerdict: TDataSourceVerdict;
var
    Points: longint;
begin
    Points := 0;
    if FPreview <> nil then
        Points := FPreview.PointsCount;
    Result := AdviseCreate(FFilePath <> '', FPreview <> nil, Points,
        FPreviewError);
end;

function TDataSourceWizard.Steps: TWizardStepSet;
begin
    if FSourceClass = nil then
        Result := [wsSource]
    else
        Result := WizardSteps(Info);
end;

function TDataSourceWizard.Step: TWizardStep;
begin
    Result := FStep;
end;

function TDataSourceWizard.CanGoNext: TDataSourceVerdict;
begin
    Result.Allowed := True;
    Result.Reason := '';
    case FStep of
        wsSource:
            if FSourceClass = nil then
            begin
                Result.Allowed := False;
                Result.Reason := 'Choose a data source.';
            end;
        wsFind:
            Result := SearchVerdict;
        wsChoose:
            //  A RECORD IS A STEP FORWARD TOO: Next goes into it.
            if not FHasSelection and not PointsAtRecord then
            begin
                Result.Allowed := False;
                Result.Reason := 'Choose a file.';
            end;
        wsPreview:
            Result := CreateVerdict;
    end;
end;

procedure TDataSourceWizard.GoNext;
var
    Next: TWizardStep;
    Verdict: TDataSourceVerdict;
begin
    Verdict := CanGoNext;
    if not Verdict.Allowed then
        raise EDataSourceError.Create(Verdict.Reason);
    if FStep = wsPreview then
        Exit;
    Next := FStep;
    repeat
        Next := Succ(Next);
        //  Steps this source does not need are not stepped through: they are
        //  not in its set at all, which is what makes the breadcrumb show a series
        //  feed three steps and a repository four.
    until (Next in Steps) or (Next = wsPreview);
    //  A SOURCE THAT ASKS NOTHING IS ASKED HERE. With no Find step nothing
    //  ever ran the search, and the list of what there is was drawn from a
    //  source that had never been asked - an empty list, reported as nothing
    //  wrong. The window cannot do this instead: it would be a decision taken
    //  inside an LCL descendant, where no test reaches it.
    //
    //  BEFORE the step changes, so a source that refuses - no samples
    //  directory, nothing readable in it - leaves the user where they were,
    //  looking at the list they chose from, rather than at an empty page.
    if (Next = wsChoose) and not FSearched then
        Search;
    FStep := Next;
    //  NOTHING TO CHOOSE BETWEEN: a DOI names one record, and the list the
    //  user came for is its files. This was an Open button beside a list of
    //  one, outside the wizard's sequence and offering no alternative (the
    //  user, 2026-10-20).
    if FStep = wsChoose then
        OpenALoneRecord;
    if FStep > FReached then
        FReached := FStep;
end;

procedure TDataSourceWizard.Advance;
begin
    //  NEXT ON A RECORD GOES INTO IT, and stays on the list: its files are
    //  what is chosen from.
    if (FStep = wsChoose) and PointsAtRecord then
    begin
        Open(FPointed);
        Exit;
    end;
    if FStep = wsFind then
    begin
        Search;
        //  A source answering with exactly one file has no Choose step, so the
        //  result it found IS the choice: there is no list to pick it from.
        if not (wsChoose in Steps) and (ItemCount = 1) then
            Select(0);
    end;
    GoNext;
end;

procedure TDataSourceWizard.EnterChosenSource;
begin
    if FStep = wsSource then
        GoNext;
end;

function TDataSourceWizard.NeedsDownload: boolean;
begin
    Result := (FStep = wsPreview) and (FFilePath = '');
end;

function TDataSourceWizard.SelectIfAllowed(AIndex: longint): boolean;
begin
    //  The row the user is on, whatever it is: Next goes into a record.
    FPointed := AIndex;
    Result := ChooseVerdict(AIndex).Allowed;
    if Result then
        Select(AIndex);
end;

function TDataSourceWizard.SelectedIndex: longint;
var
    i: longint;
begin
    //  A RECORD THE USER IS ON is shown as theirs too: Next goes into it, and
    //  a highlight cleared by the redraw would say nothing was chosen.
    if PointsAtRecord then
        Exit(FPointed);
    Result := -1;
    if not FHasSelection then
        Exit;
    for i := 0 to High(FItems) do
        if FItems[i].Id = FSelected.Id then
            Exit(i);
end;

procedure TDataSourceWizard.GoBack;
var
    Previous: TWizardStep;
begin
    if FStep = wsSource then
        Exit;
    //  BACK OUT OF A RECORD the user went into, to the records they chose it
    //  from. One the wizard opened by itself had no list before it, so Back
    //  leaves the step, as it would anywhere else.
    if (FStep = wsChoose) and InsideContainer and
        not (FOpenedAlone and (Length(FOpened) = 1)) then
    begin
        CloseContainer;
        Exit;
    end;
    Previous := FStep;
    repeat
        Previous := Pred(Previous);
    until (Previous in Steps) or (Previous = wsSource);
    FStep := Previous;
end;

function TDataSourceWizard.Reached: TWizardStep;
begin
    Result := FReached;
    if FStep > Result then
        Result := FStep;
end;

function TDataSourceWizard.StageCount: longint;
var
    Each: TWizardStep;
    Steps_: TWizardStepSet;
begin
    Result := 0;
    if FSourceClass = nil then
        Exit;
    Steps_ := Steps;
    for Each := Low(TWizardStep) to High(TWizardStep) do
        if (Each in Steps_) and (Each <> wsSource) then
            Inc(Result);
end;

function TDataSourceWizard.StageOf(AStep: TWizardStep): longint;
var
    Each: TWizardStep;
    Steps_: TWizardStepSet;
begin
    Result := 0;
    if (FSourceClass = nil) or (AStep = wsSource) then
        Exit;
    Steps_ := Steps;
    for Each := Low(TWizardStep) to High(TWizardStep) do
    begin
        if (Each in Steps_) and (Each <> wsSource) then
            Inc(Result);
        if Each = AStep then
            Exit;
    end;
    Result := 0;
end;

function TDataSourceWizard.CanGoTo(AStep: TWizardStep): TDataSourceVerdict;
begin
    Result.Allowed := False;
    if not (AStep in Steps) then
    begin
        Result.Reason := 'This source has no ' +
            LowerCase(StepCaption(AStep)) + ' step.';
        Exit;
    end;
    if AStep > Reached then
    begin
        //  NOT "you cannot do that": what has to happen first, which is the
        //  step they are on.
        Result.Reason := 'Finish the ' + LowerCase(StepCaption(FStep)) +
            ' step first.';
        Exit;
    end;
    Result.Allowed := True;
    Result.Reason := '';
end;

function TDataSourceWizard.ChoiceAt(AStep: TWizardStep): string;
begin
    Result := '';
    case AStep of
        wsSource:
            if FSourceClass <> nil then
                Result := Info.Title;
        wsFind:
            Result := QueryAsText;
        wsChoose:
            if FHasSelection then
                Result := FSelected.Title;
        wsPreview:
            if FPreview <> nil then
                Result := IntToStr(FPreview.PointsCount) + ' points';
    end;
end;

procedure TDataSourceWizard.GoTo_(AStep: TWizardStep);
var
    Verdict: TDataSourceVerdict;
begin
    Verdict := CanGoTo(AStep);
    if not Verdict.Allowed then
        raise EDataSourceError.Create(Verdict.Reason);
    FStep := AStep;
end;

end.
