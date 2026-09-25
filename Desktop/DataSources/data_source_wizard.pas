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
    int_web_client, title_points_set;

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
        function Info: TDataSourceInfo;
        procedure FreeSource;
        procedure ForgetDownload;
        function QueryAsText: string;
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
        { Opens a container: its children replace the list. }
        procedure Open(AIndex: longint);
        { Goes back up one container, or to the search results. }
        procedure CloseContainer;
        function InsideContainer: boolean;
        procedure Select(AIndex: longint);

        { Step 4. Downloads the selected file into the cache and parses it with
          the registered loader for its kind. }
        procedure Download;
        function CreateVerdict: TDataSourceVerdict;

        { Puts the file that was downloaded into AFolder and keeps it there.

          WHY THE FILE MOVES RATHER THAN BEING FETCHED AGAIN: it is the file
          the user has just looked at, and asking the service twice for one
          answer is both slower and a different answer when the series has
          moved on. The preview stays exactly as it was. }
        procedure KeepIn(const AFolder: string);

        { Which steps exist for the chosen source, and where we are. }
        function Steps: TWizardStepSet;
        function CanGoNext: TDataSourceVerdict;
        procedure GoNext;
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
        property SourceClass: TDataSourceClass read FSourceClass;
    end;

{ The steps a source with these declared facts needs. Free function as well as
  a class method so the truth table can be tested without a wizard. }
function WizardSteps(const AInfo: TDataSourceInfo): TWizardStepSet;

{ What one step is called in the breadcrumb. }
function StepCaption(AStep: TWizardStep): string;

implementation

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
    FCacheRoot := DownloadsRoot;
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
    FPreviewError := '';
    FFilePath := '';
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
    FHasSelection := False;
    ForgetDownload;
    //  WHAT WAS ASKED, not what is in the boxes: a field greyed after it was
    //  filled in still holds its text, and must not travel.
    FItems := FSource.Search(EffectiveQuery(Info.QueryFields, FQuery));
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
begin
    Row := Item(AIndex);
    if Row.IsLeaf then
        raise EDataSourceError.Create(Row.Title + ' is a file, not a record.');
    SetLength(FOpened, Length(FOpened) + 1);
    FOpened[High(FOpened)] := Row;
    FItems := FSource.Children(Row);
    FHasSelection := False;
end;

procedure TDataSourceWizard.CloseContainer;
begin
    if Length(FOpened) = 0 then
        Exit;
    SetLength(FOpened, Length(FOpened) - 1);
    FHasSelection := False;
    if Length(FOpened) = 0 then
        //  Back at the top: re-run the search rather than keeping a stale copy
        //  of what it answered, which may be minutes old.
        FItems := FSource.Search(FQuery)
    else
        FItems := FSource.Children(FOpened[High(FOpened)]);
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
        //  answered "no". The session knows which were kept and throws the
        //  rest away when the application closes.
        CurrentDownloadSession.CleanableRoot := DownloadsRoot;
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
        Name_ := FSource.Download(FSelected, Buffer);
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
        except
            on E: Exception do
                //  THE READER'S OWN WORDS, kept rather than replaced: it knows
                //  what it expected and what it found. A service answering a
                //  bad request with a web page lands here, and "the file is
                //  not a data file" would lose which part of it was wrong.
                //  Not re-raised: a file that cannot be read is an ordinary
                //  outcome of asking for the wrong thing, and the preview is
                //  where the user sees that and chooses again.
                FPreviewError := E.Message;
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

procedure TDataSourceWizard.KeepIn(const AFolder: string);
var
    Target: string;
begin
    //  Moving it out of the cache is the user saying where it belongs, so the
    //  session stops calling it rubbish - Rubbish only ever names files that
    //  are still inside the application's own downloads directory.
    if (Trim(AFolder) = '') or (FFilePath = '') then
        Exit;
    Target := PathInFolder(AFolder, FFilePath);
    if SameText(Target, FFilePath) then
        Exit;
    try
        ForceDirectories(AFolder);
        //  NOT OVERWRITTEN: a file of that name already there is somebody
        //  else's download, and the stepped-over name is what the rest of the
        //  cache does.
        Target := FreeCachePath(Target, @DefaultPathExists);
        if not RenameFile(FFilePath, Target) then
        begin
            //  A rename fails across devices, which is ordinary when the
            //  chosen folder is on another disk: copy, then drop the original.
            CopyFileTo(FFilePath, Target);
            DeleteFile(FFilePath);
        end;
    except
        on E: Exception do
            raise EDataSourceError.Create(
                SaveFailureMessage(AFolder, E.Message));
    end;
    FFilePath := Target;
    FCacheRoot := AFolder;
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
            if not FHasSelection then
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
    if FStep > FReached then
        FReached := FStep;
end;

procedure TDataSourceWizard.GoBack;
var
    Previous: TWizardStep;
begin
    if FStep = wsSource then
        Exit;
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
