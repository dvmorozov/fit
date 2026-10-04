// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(File > New Project from Data Source: the window over the wizard.)

A UI WRAPPER AND NOTHING ELSE. Which steps exist, when the next one may be
entered, what is refused and in what words, what the preview found - all of it
is data_source_wizard's, and none of it is here. This window reads controls,
calls the wizard and draws what it answers. A decision taken inside an LCL
descendant is unreachable by any test, which is why there is none.

Created in code rather than from a .lfm, as explain_everything_window is: the
designed form stays untouched, and the controls are laid out from what the
chosen source declared rather than from anything a designer could have drawn.

MASTER AND DETAIL, NOT A STEP RAIL. The sources are a list on the left, on
screen the whole time, and the right-hand side is the chosen source's own
sequence: its question, its files, its preview. The earlier arrangement put
the STEPS on the left and the SOURCE LIST on the right, so the left-hand list
changed under a selection made on the right - which is the strangeness this
window was rebuilt to remove. What decides the sequence is now what is beside
it, permanently, and switching source is one click rather than a walk back.

NOTHING CHOSEN IS HIDDEN. Where a wizard puts everything behind a Back button,
the steps of the chosen source are a breadcrumb over its own panel, each
completed one showing what was answered there and clickable to return to it -
in the same panel as the answer, not in another one.

THE FIELDS ARE BUILT FROM DECLARATIONS. A source says it wants text with
suggestions, a choice or a date; this builds an edit, a combo box or an edit
with a hint. A source names no widget, so adding one changes nothing here.
}
unit data_source_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, EditBtn,
    Graphics,
    data_source, data_source_registry, data_source_advice, data_source_wizard,
    data_source_import, download_cache, download_job, int_web_client,
    web_client;

type
    TDataSourceWindow = class(TForm)
    private
        FWizard: TDataSourceWizard;
        FWebObject: TWebClient;
        FWeb: IWebClient;

        { The permanent left-hand list: what to fit from, always on screen. }
        FSidebar: TPanel;
        FSources: TTreeView;
        FSourceSummary: TLabel;
        { The right-hand side: the chosen source's own sequence. }
        FPages: TPanel;
        FHeader: TPanel;
        FSourceTitle: TLabel;
        FCrumbs: TPanel;
        FPrompt: TLabel;
        FStatus: TLabel;
        FBack: TButton;
        FNext: TButton;
        FCreate: TButton;
        FCancel: TButton;

        //  The chosen source's question.
        { SCROLLS: a source may ask more than the window has room for, and a
          field below the edge - or a hint cut off mid-sentence - is one the
          user never sees. FRED's fifth field was. }
        FFieldPanel: TScrollBox;
        FFieldEditors: TStringList;   //  field id -> the control holding it
        //  Its files.
        FResults: TListView;
        FOpenButton: TButton;
        FUpButton: TButton;
        //  What it fetched.
        FPreviewInfo: TMemo;
        FFolderEdit: TEdit;
        FFolderButton: TButton;
        { Where downloads are kept, as the caller remembers it between
          sessions. }
        FFolder: string;
        { Who remembers where downloads are kept between sessions. }
        FHost: IImportHost;

        { The job fetching a file now, or nil. }
        FJob: TDownloadJob;
        FImported: boolean;
        FImportedPath: string;
        FImportedOrigin: TDownloadOrigin;
        { What the job last reported, written from its thread and drawn by the
          wait on this one. }
        FProgressBytes: int64;
        FProgressTotal: int64;

        procedure BuildSidebar;
        procedure BuildHeader;
        procedure ShowHeader;
        procedure BuildFindPage;
        procedure BuildChoosePage;
        procedure BuildPreviewPage;
        procedure ShowStep;
        procedure ShowSources;
        procedure ShowFields;
        procedure ShowResults;
        procedure ShowPreview;
        procedure ReadFields;
        { Greys every field the answers so far do not ask (FieldIsEnabled). }
        procedure ShowFieldStates;
        procedure FieldChange(Sender: TObject);
        function SelectedResultIndex: longint;
        procedure Complain(const AMessage: string);
        { What the window does with anything raised while it works. }
        procedure Report(E: Exception);

        { Going back to a step already answered, from the breadcrumb over the
          panel that answered it. }
        procedure CrumbClick(Sender: TObject);
        procedure SourcesChange(Sender: TObject; ANode: TTreeNode);
        procedure ResultsSelect(Sender: TObject; AItem: TListItem;
            ASelected: boolean);
        procedure ResultsDoubleClick(Sender: TObject);
        procedure OpenClick(Sender: TObject);
        procedure UpClick(Sender: TObject);
        procedure BackClick(Sender: TObject);
        { Cancel means "stop the download" while one runs, and "close the
          window" otherwise - which is what the caption says at the time. }
        procedure CancelClick(Sender: TObject);
        { Chooses where downloaded data is kept, and moves what was just
          fetched into it. }
        procedure FolderClick(Sender: TObject);
        procedure NextClick(Sender: TObject);
        procedure CreateClick(Sender: TObject);
        { Fetches the chosen file WITHOUT the window stopping: the job runs in
          its own thread and this waits on it, drawing what it reports and
          leaving Cancel pressable. }
        procedure DownloadWhileWaiting;
        { Told how far the download has got, from the job's thread. }
        procedure DownloadProgress(ABytes, ATotal: int64);
    public
        constructor CreateWizard(AOwner: TComponent; AHost: IImportHost);
        destructor Destroy; override;

        { What the window ended with: whether a file was downloaded and
          approved, and what it was. The caller performs the import, because
          starting a project is the window's business and not this dialog's. }
        property Imported: boolean read FImported;
        property ImportedPath: string read FImportedPath;
        property ImportedOrigin: TDownloadOrigin read FImportedOrigin;
    end;

{ Runs the wizard over AOwner. When the user finished it, imports what they
  chose through AHost - the same four steps File > Import Profile takes, in the
  order data_source_import states. }
procedure NewProjectFromDataSource(AOwner: TComponent; AHost: IImportHost);

implementation

uses
    Dialogs, DateUtils, data_source_view;

constructor TDataSourceWindow.CreateWizard(AOwner: TComponent;
    AHost: IImportHost);
var
    Buttons: TPanel;
begin
    inherited CreateNew(AOwner);
    FHost := AHost;
    Caption := 'New Project from Data Source';
    Width := Scale96ToFont(760);
    Height := Scale96ToFont(520);
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;

    FWebObject := TWebClient.Create;
    FWeb := FWebObject;
    FWizard := TDataSourceWizard.Create(FWeb);
    //  WHERE THE USER LAST CHOSE TO KEEP DOWNLOADS, or the per-user default.
    //  Asked of the host because it is what survives a session.
    if FHost <> nil then
        FFolder := FHost.DownloadFolder;
    FFolder := ChosenDownloadsRoot(FFolder);
    FWizard.CacheRoot := FFolder;
    FFieldEditors := TStringList.Create;

    Buttons := TPanel.Create(Self);
    Buttons.Parent := Self;
    Buttons.Align := alBottom;
    Buttons.Height := Scale96ToFont(44);
    Buttons.BevelOuter := bvNone;

    //  EVERY BUTTON HERE SIZES TO ITS OWN CAPTION. A TButton's default width
    //  fits "Cancel" and clips "Create Project" - which is what the window's
    //  own caption check reported the first time it measured this form, at the
    //  font this machine happens to use. AutoSize is also what keeps that true
    //  in another language.
    //  CREATED IN READING ORDER. With alRight the first control created is the
    //  LEFTMOST, so creating Cancel first put it at the left end and left the
    //  row reading Cancel, Create Project, Next, Back - backwards.
    FBack := TButton.Create(Self);
    FBack.Parent := Buttons;
    FBack.Caption := '< Back';
    FBack.Align := alRight;
    FBack.BorderSpacing.Around := Scale96ToFont(6);
    FBack.AutoSize := True;
    FBack.OnClick := @BackClick;

    FNext := TButton.Create(Self);
    FNext.Parent := Buttons;
    FNext.Caption := 'Next >';
    FNext.Align := alRight;
    FNext.BorderSpacing.Around := Scale96ToFont(6);
    FNext.AutoSize := True;
    FNext.Default := True;
    FNext.OnClick := @NextClick;

    FCreate := TButton.Create(Self);
    FCreate.Parent := Buttons;
    FCreate.Caption := 'Create Project';
    FCreate.Align := alRight;
    FCreate.BorderSpacing.Around := Scale96ToFont(6);
    FCreate.AutoSize := True;
    FCreate.OnClick := @CreateClick;

    FCancel := TButton.Create(Self);
    FCancel.Parent := Buttons;
    FCancel.Caption := 'Cancel';
    FCancel.ModalResult := mrCancel;
    FCancel.Cancel := True;
    FCancel.OnClick := @CancelClick;
    FCancel.Align := alRight;
    FCancel.BorderSpacing.Around := Scale96ToFont(6);
    FCancel.AutoSize := True;

    //  THE REASON, BESIDE THE BUTTON IT DISABLES. A disabled button with its
    //  explanation somewhere else is the refusal this project's rules exist to
    //  prevent.
    FStatus := TLabel.Create(Self);
    FStatus.Parent := Buttons;
    FStatus.Align := alClient;
    FStatus.Layout := tlCenter;
    FStatus.BorderSpacing.Around := Scale96ToFont(8);

    BuildSidebar;

    FPages := TPanel.Create(Self);
    FPages.Parent := Self;
    FPages.Align := alClient;
    FPages.BevelOuter := bvNone;

    BuildHeader;
    BuildFindPage;
    BuildChoosePage;
    BuildPreviewPage;

    ShowSources;
    ShowStep;
end;

destructor TDataSourceWindow.Destroy;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FWebObject);
    FreeAndNil(FFieldEditors);
    inherited Destroy;
end;

{ THE LEFT-HAND SIDE IS THE SOURCES, and only the sources. It never changes
  under anything chosen on the right: it is what chooses. }
procedure TDataSourceWindow.BuildSidebar;
var
    Caption_: TLabel;
begin
    FSidebar := TPanel.Create(Self);
    FSidebar.Parent := Self;
    FSidebar.Align := alLeft;
    //  Wide enough for the longest source title the framework ships.
    FSidebar.Width := Scale96ToFont(240);
    FSidebar.BevelOuter := bvNone;

    Caption_ := TLabel.Create(Self);
    Caption_.Parent := FSidebar;
    Caption_.Align := alTop;
    Caption_.Caption := 'Data source';
    Caption_.Font.Style := [fsBold];
    Caption_.BorderSpacing.Around := Scale96ToFont(8);

    //  WHAT THE SELECTED SOURCE IS, under the list that selected it. A
    //  summary belongs beside its source and not in the panel the source
    //  decides the contents of.
    FSourceSummary := TLabel.Create(Self);
    FSourceSummary.Parent := FSidebar;
    FSourceSummary.Align := alBottom;
    FSourceSummary.AutoSize := False;
    FSourceSummary.Height := Scale96ToFont(64);
    FSourceSummary.WordWrap := True;
    FSourceSummary.Layout := tlTop;
    FSourceSummary.BorderSpacing.Around := Scale96ToFont(8);

    FSources := TTreeView.Create(Self);
    FSources.Parent := FSidebar;
    FSources.Align := alClient;
    FSources.BorderSpacing.Around := Scale96ToFont(8);
    FSources.ReadOnly := True;
    FSources.OnChange := @SourcesChange;
end;

{ THE BREADCRUMB OVER THE PANEL IT DESCRIBES: the source's name, then its own
  steps. Built empty; ShowHeader fills it, because which steps there are
  depends on the source and that is read from the wizard every time. }
procedure TDataSourceWindow.BuildHeader;
begin
    FHeader := TPanel.Create(Self);
    FHeader.Parent := FPages;
    FHeader.Align := alTop;
    FHeader.BevelOuter := bvNone;
    FHeader.AutoSize := False;
    FHeader.Height := Scale96ToFont(56);

    FSourceTitle := TLabel.Create(Self);
    FSourceTitle.Parent := FHeader;
    FSourceTitle.Align := alTop;
    FSourceTitle.Font.Style := [fsBold];
    FSourceTitle.BorderSpacing.Around := Scale96ToFont(8);

    FCrumbs := TPanel.Create(Self);
    FCrumbs.Parent := FHeader;
    FCrumbs.Align := alClient;
    FCrumbs.BevelOuter := bvNone;

    //  WHAT TO DO WHEN NOTHING IS CHOSEN YET, said where the answer will
    //  appear rather than left as an empty panel.
    FPrompt := TLabel.Create(Self);
    FPrompt.Parent := FPages;
    FPrompt.Align := alTop;
    FPrompt.WordWrap := True;
    FPrompt.BorderSpacing.Around := Scale96ToFont(12);
    FPrompt.Caption := 'Choose a data source on the left. ' +
        'What it asks for, and what it offers, appears here.';
end;

procedure TDataSourceWindow.BuildFindPage;
begin
    FFieldPanel := TScrollBox.Create(Self);
    FFieldPanel.Parent := FPages;
    FFieldPanel.Align := alClient;
    FFieldPanel.BorderStyle := bsNone;
    FFieldPanel.HorzScrollBar.Visible := False;
    FFieldPanel.VertScrollBar.Tracking := True;
    FFieldPanel.Visible := False;
end;

procedure TDataSourceWindow.BuildChoosePage;
var
    Bar: TPanel;
begin
    FResults := TListView.Create(Self);
    FResults.Parent := FPages;
    FResults.Align := alClient;
    FResults.ViewStyle := vsReport;
    FResults.ReadOnly := True;
    FResults.RowSelect := True;
    FResults.Visible := False;
    FResults.OnSelectItem := @ResultsSelect;
    FResults.OnDblClick := @ResultsDoubleClick;
    FResults.Columns.Add.Caption := 'Name';
    FResults.Columns.Add.Caption := 'Details';
    FResults.Columns.Add.Caption := 'Size';
    FResults.Column[0].Width := Scale96ToFont(240);
    FResults.Column[1].Width := Scale96ToFont(220);
    FResults.Column[2].Width := Scale96ToFont(90);

    Bar := TPanel.Create(Self);
    Bar.Parent := FPages;
    Bar.Align := alBottom;
    Bar.Height := Scale96ToFont(36);
    Bar.BevelOuter := bvNone;
    Bar.Visible := False;
    Bar.Name := 'ChooseBar';

    FOpenButton := TButton.Create(Self);
    FOpenButton.Parent := Bar;
    FOpenButton.Caption := 'Open';
    FOpenButton.Align := alLeft;
    FOpenButton.BorderSpacing.Around := Scale96ToFont(4);
    FOpenButton.AutoSize := True;
    FOpenButton.OnClick := @OpenClick;

    FUpButton := TButton.Create(Self);
    FUpButton.Parent := Bar;
    FUpButton.Caption := 'Back to results';
    FUpButton.Align := alLeft;
    FUpButton.BorderSpacing.Around := Scale96ToFont(4);
    FUpButton.AutoSize := True;
    FUpButton.OnClick := @UpClick;
end;

procedure TDataSourceWindow.BuildPreviewPage;
var
    Bar: TPanel;
    Caption_: TLabel;
begin
    //  WHERE THE FILE IS KEPT, SHOWN AND CHANGEABLE. A download that lands
    //  somewhere the user cannot name is a file they cannot find again - and
    //  "it saved it somewhere" is what they are left saying about their own
    //  data. The folder is remembered between sessions, so this is a row to
    //  read rather than a question to answer every time.
    Bar := TPanel.Create(Self);
    Bar.Parent := FPages;
    Bar.Align := alBottom;
    Bar.Height := Scale96ToFont(40);
    Bar.BevelOuter := bvNone;
    Bar.Visible := False;
    Bar.Name := 'FolderBar';

    Caption_ := TLabel.Create(Self);
    Caption_.Parent := Bar;
    Caption_.Align := alLeft;
    Caption_.Layout := tlCenter;
    Caption_.AutoSize := False;
    Caption_.Width := Scale96ToFont(60);
    Caption_.BorderSpacing.Around := Scale96ToFont(8);
    Caption_.Caption := 'Save in';

    FFolderButton := TButton.Create(Self);
    FFolderButton.Parent := Bar;
    FFolderButton.Align := alRight;
    FFolderButton.AutoSize := True;
    FFolderButton.BorderSpacing.Around := Scale96ToFont(6);
    FFolderButton.Caption := 'Change...';
    FFolderButton.OnClick := @FolderClick;

    FFolderEdit := TEdit.Create(Self);
    FFolderEdit.Parent := Bar;
    FFolderEdit.Align := alClient;
    FFolderEdit.BorderSpacing.Around := Scale96ToFont(6);
    //  Shown, not typed: a path typed by hand is a path that does not exist.
    FFolderEdit.ReadOnly := True;

    FPreviewInfo := TMemo.Create(Self);
    FPreviewInfo.Parent := FPages;
    FPreviewInfo.Align := alClient;
    FPreviewInfo.ReadOnly := True;
    FPreviewInfo.ScrollBars := ssAutoVertical;
    FPreviewInfo.Visible := False;
end;

procedure TDataSourceWindow.FolderClick(Sender: TObject);
var
    Chosen: string;
begin
    Chosen := FFolder;
    if not SelectDirectory('Keep downloaded data in', Chosen, Chosen) then
        Exit;
    try
        //  The file the user has just looked at is MOVED there: asking the
        //  service again for one answer is slower and may answer differently.
        FWizard.KeepIn(Chosen);
        FFolder := Chosen;
        if FHost <> nil then
            FHost.RememberDownloadFolder(Chosen);
        ShowPreview;
        ShowStep;
    except
        on E: Exception do
            Report(E);
    end;
end;

procedure TDataSourceWindow.ShowSources;
var
    Categories: TStringList;
    Parent_: TTreeNode;
    Titles: TStringArray;
    i, j: longint;
begin
    FSources.Items.Clear;
    Categories := DataSourceCategories;
    try
        for i := 0 to Categories.Count - 1 do
        begin
            Parent_ := FSources.Items.Add(nil, Categories[i]);
            //  A row carries its source's TITLE; FindDataSourceByTitle says why.
            Titles := DataSourceTitlesIn(Categories[i]);
            for j := 0 to High(Titles) do
                FSources.Items.AddChild(Parent_, Titles[j]);
            Parent_.Expand(True);
        end;
    finally
        Categories.Free;
    end;
end;

procedure TDataSourceWindow.SourcesChange(Sender: TObject; ANode: TTreeNode);
var
    Source: TDataSourceClass;
begin
    if (ANode = nil) or (ANode.Parent = nil) then
        Exit;
    Source := FindDataSourceByTitle(ANode.Text);
    if Source = nil then
        Exit;
    FWizard.ChooseSource(Source.Info.Id);
    FSourceSummary.Caption := Source.Info.Summary;
    try
        FWizard.EnterChosenSource;
    except
        on E: Exception do
            //  A source that cannot offer anything - no samples directory,
            //  nothing readable in it - says so and leaves the list
            //  selectable, which is where the user must go next.
            Report(E);
    end;
    ShowStep;
end;

{ THE FIELDS A SOURCE DECLARED, laid out so they fit whatever window they are
  in, and answered by CHOOSING wherever the answers can be listed.

  NOBODY SHOULD HAVE TO KNOW AN IDENTIFIER. A service calls a series 'SP500' and
  a substance 'C71432'; a user knows "S&P 500" and "benzene". So a field that
  declares choices is a drop-down of their CAPTIONS, and the value behind the
  chosen one is what the source is asked for - the identifier never appears.
  A field with no choices is one nobody can enumerate: an address, a DOI.

  A DATE IS PICKED FROM A CALENDAR (TDateEdit, which is the LCL's own), because
  a typed date is a format to get wrong - and the format differs by country,
  which the picker settles by showing a month. }
procedure TDataSourceWindow.ShowFields;
var
    Fields: TInputFieldDecls;
    i, j: longint;
    Row, Right_: TPanel;
    Caption_, Hint_: TLabel;
    Edit: TEdit;
    Picker: TDateEdit;
    Combo: TComboBox;
    Editor: TControl;
    Value: string;
    FieldColumn: longint;
    Kind: TFieldEditorKind;
begin
    //  Where every editor starts: one column for the captions, the same in
    //  every row, so "To" does not begin further left than "Series".
    FieldColumn := Scale96ToFont(140);
    FFieldPanel.DestroyComponents;
    FFieldEditors.Clear;
    if FWizard.SourceClass = nil then
        Exit;
    Fields := FWizard.SourceClass.Info.QueryFields;
    for i := 0 to High(Fields) do
    begin
        Row := TPanel.Create(FFieldPanel);
        Row.Parent := FFieldPanel;
        Row.Align := alTop;
        //  The declared order, for the same reason the hint states its own.
        Row.Top := i * Scale96ToFont(1000);
        //  Tall enough for the editor and TWO lines of its hint: a hint that
        //  is cut off mid-sentence is worse than none, and these say formats.
        Row.Height := Scale96ToFont(88);
        Row.BorderSpacing.Top := Scale96ToFont(4);
        Row.BevelOuter := bvNone;

        Right_ := TPanel.Create(Row);
        Right_.Parent := Row;
        Right_.Align := alClient;
        Right_.BevelOuter := bvNone;
        //  A margin on the right, or the editor's own border draws in the
        //  window's edge - which is what the first version did.
        Right_.BorderSpacing.Right := Scale96ToFont(12);

        Value := FWizard.FieldValue(Fields[i].Id);

        Kind := FieldEditorKind(Fields[i]);
        if Kind = fekDate then
        begin
            Picker := TDateEdit.Create(Right_);
            Picker.Parent := Right_;
            Picker.Align := alTop;
            Picker.Top := 0;
            Picker.BorderSpacing.Around := Scale96ToFont(6);
            Picker.BorderSpacing.Left := FieldColumn;
            //  Empty until something is chosen: an empty date means "as far
            //  back as there is", and defaulting to today would silently ask
            //  for one day of a series.
            Picker.DefaultToday := False;
            //  ISO, AND SAID SO IN THE BOX. Left to itself the picker shows
            //  the machine's own short date - '23-9-26', which is three
            //  different days depending on where you are, and which hides
            //  whether 26 is a year or a day. yyyy-mm-dd is unambiguous
            //  everywhere, sorts as it reads, and is the form every service
            //  these sources ask actually takes - so what is shown, what is
            //  typed and what travels are one thing.
            Picker.DateOrder := doNone;
            Picker.DateFormat := IsoDateFormat;
            Picker.Text := Value;
            Editor := Picker;
        end
        else if Kind in [fekClosedList, fekOpenList] then
        begin
            Combo := TComboBox.Create(Right_);
            Combo.Parent := Right_;
            Combo.Align := alTop;
            Combo.Top := 0;
            Combo.BorderSpacing.Around := Scale96ToFont(6);
            Combo.BorderSpacing.Left := FieldColumn;
            if Kind = fekClosedList then
                Combo.Style := csDropDownList
            else
                Combo.Style := csDropDown;
            for j := 0 to High(Fields[i].Choices) do
                Combo.Items.Add(Fields[i].Choices[j].Caption);
            //  A CHOICE MAY DECIDE WHICH OTHER FIELDS ARE ASKED - a Range of
            //  "Custom dates" and the dates under it - so every change of one
            //  re-reads the answers and greys the rest again.
            Combo.OnChange := @FieldChange;
            Combo.Text := CaptionOfChoice(Fields[i].Choices, Value);
            if Kind = fekClosedList then
                Combo.ItemIndex := InitialChoiceIndex(Fields[i].Choices, Value);
            Editor := Combo;
        end
        else
        begin
            Edit := TEdit.Create(Right_);
            Edit.Parent := Right_;
            Edit.Align := alTop;
            Edit.Top := 0;
            Edit.BorderSpacing.Around := Scale96ToFont(6);
            Edit.BorderSpacing.Left := FieldColumn;
            Edit.Text := Value;
            Editor := Edit;
        end;
        FFieldEditors.AddObject(Fields[i].Id, Editor);

        //  THE CAPTION IS ANCHORED TO THE MIDDLE OF ITS EDITOR, in the editor's
        //  own parent. It was a column of the row, placed by a fixed offset
        //  from the row's top - which put it level with the editor only where
        //  an editor is as tall as that offset assumed, and on macOS every
        //  caption sat below its box. The row is as tall as the editor AND two
        //  lines of hint, so centring on the row is wrong too: it reads as the
        //  hint's label.
        Caption_ := TLabel.Create(Right_);
        Caption_.Parent := Right_;
        Caption_.Left := Scale96ToFont(8);
        Caption_.AnchorSideTop.Control := Editor;
        Caption_.AnchorSideTop.Side := asrCenter;
        Caption_.Anchors := [akLeft, akTop];
        Caption_.Caption := Fields[i].Caption;

        //  THE HINT UNDER THE EDITOR, not inside it: a placeholder disappears
        //  the moment anything is typed, which is when a format is most often
        //  wanted.
        Hint_ := TLabel.Create(Right_);
        Hint_.Parent := Right_;
        Hint_.Align := alTop;
        //  BELOW the editor, and said so: controls aligned to the top are
        //  ordered by their Top, not by the order they were created in, so the
        //  hint drew ABOVE the box it describes until it said where it goes.
        Hint_.Top := Scale96ToFont(1000);
        Hint_.BorderSpacing.Left := FieldColumn + Scale96ToFont(8);
        //  WITH ITS CONDITION, when it has one: a greyed field says what would
        //  ask it, in the words on screen.
        Hint_.Caption := FieldHintText(Fields, Fields[i].Id);
        Hint_.Font.Color := clGrayText;
        //  Long hints wrap rather than run off the edge, and the row is tall
        //  enough for two lines of one.
        Hint_.WordWrap := True;
        Hint_.AutoSize := False;
        Hint_.Height := Scale96ToFont(40);
    end;
    ShowFieldStates;
end;

procedure TDataSourceWindow.ShowFieldStates;
var
    i: longint;
begin
    for i := 0 to FFieldEditors.Count - 1 do
        TControl(FFieldEditors.Objects[i]).Enabled :=
            FWizard.FieldEnabled(FFieldEditors[i]);
end;

procedure TDataSourceWindow.FieldChange(Sender: TObject);
begin
    ReadFields;
    ShowFieldStates;
end;

{ What the user answered, as the SOURCE understands it: the caption chosen is
  translated back to the value behind it, so what travels is the identifier the
  service knows and what was shown never was. }
procedure TDataSourceWindow.ReadFields;
var
    Fields: TInputFieldDecls;
    i: longint;
    Control: TObject;
begin
    if FWizard.SourceClass = nil then
        Exit;
    Fields := FWizard.SourceClass.Info.QueryFields;
    for i := 0 to FFieldEditors.Count - 1 do
    begin
        Control := FFieldEditors.Objects[i];
        if Control is TDateEdit then
            //  The box's text as well as the picker's date: empty must stay
            //  empty, and Date would answer with a day nobody chose.
            FWizard.SetField(FFieldEditors[i], WireDate(TDateEdit(Control).Text,
                TDateEdit(Control).Date))
        else if Control is TComboBox then
            FWizard.SetField(FFieldEditors[i], AnswerFromChoiceText(
                FieldChoices(Fields, FFieldEditors[i]), TComboBox(Control).Text,
                TComboBox(Control).Style = csDropDownList))
        else if Control is TEdit then
            FWizard.SetField(FFieldEditors[i], TEdit(Control).Text);
    end;
end;

procedure TDataSourceWindow.ShowResults;
var
    i: longint;
    Row: TDataSourceItem;
    Entry: TListItem;
    Columns: TResultRowText;
begin
    FResults.Items.Clear;
    for i := 0 to FWizard.ItemCount - 1 do
    begin
        Row := FWizard.Item(i);
        Entry := FResults.Items.Add;
        Entry.Caption := Row.Title;
        Columns := ResultRowText(Row, FWizard.ChooseVerdict(i));
        Entry.SubItems.Add(Columns.Details);
        Entry.SubItems.Add(Columns.Size);
    end;
    FUpButton.Enabled := FWizard.InsideContainer;
end;

procedure TDataSourceWindow.ShowPreview;
var
    Lines: TStringArray;
    i: longint;
begin
    FFolderEdit.Text := ExtractFileDir(FWizard.FilePath);
    Lines := PreviewLines(FWizard.FilePath, OriginText(FWizard.Origin),
        FWizard.Preview, FWizard.PreviewError);
    FPreviewInfo.Lines.Clear;
    for i := 0 to High(Lines) do
        FPreviewInfo.Lines.Add(Lines[i]);
end;

procedure TDataSourceWindow.ShowStep;
var
    View: TWizardView;
begin
    View := WizardView(FWizard);
    FPrompt.Visible := View.PromptVisible;
    FHeader.Visible := View.HeaderVisible;
    FFieldPanel.Visible := View.FieldsVisible;
    FResults.Visible := View.ResultsVisible;
    FOpenButton.Parent.Visible := View.ResultsVisible;
    FPreviewInfo.Visible := View.PreviewVisible;
    FFolderEdit.Parent.Visible := View.FolderVisible;

    case FWizard.Step of
        wsFind: ShowFields;
        wsChoose: ShowResults;
        wsPreview: ShowPreview;
    end;

    FBack.Enabled := View.BackEnabled;
    FNext.Enabled := View.NextEnabled;
    FCreate.Enabled := View.CreateEnabled;
    FStatus.Caption := View.Status;
    ShowHeader;
end;

{ WHAT THE BREADCRUMB SHOWS, and why it is here rather than on the left.

  IT DESCRIBES ONE SOURCE'S SEQUENCE, so it lives over that source's panel. On
  the left it was a list of steps that changed whenever a source was picked on
  the right: the thing that decided the sequence and the sequence itself were
  in different halves of the window, and the half that did not move was the one
  that changed.

  A STEP BEHIND YOU SHOWS WHAT YOU ANSWERED THERE and can be clicked to go back
  to it, so no choice is hidden behind a Back button. A step not yet reached is
  greyed: clicking it would arrive at a page whose question nothing answered. }
procedure TDataSourceWindow.ShowHeader;
var
    Crumbs: TWizardCrumbs;
    Crumb: TLabel;
    Left_, i: longint;

    function Add(const ACaption: string; AStep: TWizardStep;
        AClickable: boolean): TLabel;
    begin
        Result := TLabel.Create(FCrumbs);
        Result.Parent := FCrumbs;
        Result.AutoSize := True;
        Result.Left := Left_;
        Result.Top := Scale96ToFont(4);
        Result.Caption := ACaption;
        Result.Tag := Ord(AStep);
        if AClickable then
        begin
            //  The desktop's own way of saying that a word can be clicked,
            //  rather than a colour chosen here that some themes make
            //  unreadable.
            Result.Font.Style := [fsUnderline];
            Result.Font.Color := clHotLight;
            Result.Cursor := crHandPoint;
            Result.OnClick := @CrumbClick;
        end;
        Left_ := Left_ + Result.Width + Scale96ToFont(6);
    end;

begin
    FCrumbs.DestroyComponents;
    if FWizard.SourceClass = nil then
    begin
        FSourceTitle.Caption := '';
        Exit;
    end;
    FSourceTitle.Caption := FWizard.SourceClass.Info.Title;

    Left_ := Scale96ToFont(8);
    Crumbs := WizardCrumbs(FWizard);
    for i := 0 to High(Crumbs) do
    begin
        if Crumbs[i].SeparatorBefore then
            Add('>', Crumbs[i].Step, False).Font.Color := clGrayText;
        Crumb := Add(Crumbs[i].Text, Crumbs[i].Step, Crumbs[i].Clickable);
        case Crumbs[i].State of
            //  WHERE YOU ARE, in the one thing every widget set draws the same
            //  way: weight. A colour says it only in themes that have one.
            csCurrent:
            begin
                Crumb.Font.Style := [fsBold];
                Crumb.Font.Color := clWindowText;
            end;
            csFuture:
                Crumb.Font.Color := clGrayText;
        end;
    end;
end;

procedure TDataSourceWindow.Complain(const AMessage: string);
begin
    //  The refusal in the words the rule chose. The window never rewords one:
    //  every one of them was written to say what happened and what to do, and
    //  a window that summarised them would lose exactly that.
    MessageDlg('Data source', AMessage, mtInformation, [mbOK], 0);
end;

procedure TDataSourceWindow.Report(E: Exception);
begin
    //  STOPPING IS NOT FAILING. A download the user cancelled has nothing
    //  wrong with it, so it is said in the status line and no box appears;
    //  everything else is put in front of them.
    if E is EWebCancelled then
        FStatus.Caption := E.Message
    else
        Complain(E.Message);
end;

function TDataSourceWindow.SelectedResultIndex: longint;
begin
    Result := -1;
    if FResults.Selected <> nil then
        Result := FResults.Selected.Index;
end;

procedure TDataSourceWindow.CrumbClick(Sender: TObject);
var
    Step: TWizardStep;
    Verdict: TDataSourceVerdict;
begin
    Step := TWizardStep((Sender as TLabel).Tag);
    if Step = FWizard.Step then
        Exit;
    //  A BREADCRUMB IS A PLACE TO GO BACK FROM. Jumping forward over a
    //  question would arrive at a page whose answer nothing has, so it is
    //  refused - in the status line rather than a box, because clicking a step
    //  you have not reached is a guess, not a mistake.
    Verdict := FWizard.CanGoTo(Step);
    if not Verdict.Allowed then
    begin
        FStatus.Caption := Verdict.Reason;
        Exit;
    end;
    FWizard.GoTo_(Step);
    ShowStep;
end;

procedure TDataSourceWindow.ResultsSelect(Sender: TObject; AItem: TListItem;
    ASelected: boolean);
var
    Index: longint;
begin
    if not ASelected then
        Exit;
    Index := SelectedResultIndex;
    if Index < 0 then
        Exit;
    FWizard.SelectIfAllowed(Index);
    ShowStep;
end;

procedure TDataSourceWindow.ResultsDoubleClick(Sender: TObject);
var
    Index: longint;
begin
    Index := SelectedResultIndex;
    if Index < 0 then
        Exit;
    if FWizard.Item(Index).IsLeaf then
        NextClick(Sender)
    else
        OpenClick(Sender);
end;

procedure TDataSourceWindow.OpenClick(Sender: TObject);
var
    Index: longint;
begin
    Index := SelectedResultIndex;
    if Index < 0 then
        Exit;
    Screen.Cursor := crHourGlass;
    try
        try
            FWizard.Open(Index);
            ShowStep;
        except
            on E: Exception do
                Report(E);
        end;
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TDataSourceWindow.UpClick(Sender: TObject);
begin
    try
        FWizard.CloseContainer;
        ShowStep;
    except
        on E: Exception do
            Report(E);
    end;
end;

procedure TDataSourceWindow.CancelClick(Sender: TObject);
begin
    if FJob = nil then
        Exit;
    //  The window stays open: the user stopped a download, not the wizard.
    ModalResult := mrNone;
    FWeb.Cancel;
    FStatus.Caption := 'Stopping...';
end;

procedure TDataSourceWindow.BackClick(Sender: TObject);
begin
    FWizard.GoBack;
    ShowStep;
end;

procedure TDataSourceWindow.DownloadProgress(ABytes, ATotal: int64);
begin
    //  CALLED FROM THE JOB'S THREAD, so it only remembers; the wait below is
    //  what draws it, on the window's own thread.
    FProgressBytes := ABytes;
    FProgressTotal := ATotal;
end;

procedure TDataSourceWindow.DownloadWhileWaiting;
var
    Started: TDateTime;
    Frame: longint;
begin
    FProgressBytes := 0;
    FProgressTotal := 0;
    FWeb.SetProgress(@DownloadProgress);
    FJob := TDownloadJob.Create(FWizard);
    Started := Now;
    Frame := 0;
    try
        //  Cancel is the one thing that stays pressable, and it means "stop
        //  the download" while one is running rather than "close the window".
        FCancel.Caption := 'Stop';
        FBack.Enabled := False;
        FNext.Enabled := False;
        FCreate.Enabled := False;
        FStatus.Caption := DownloadStatusText(Frame, 0, 0, 0);
        Application.ProcessMessages;
        FJob.Start;
        while not FJob.Finished do
        begin
            //  THE WINDOW KEEPS LIVING: it repaints, it answers the desktop,
            //  and the button below can be pressed. Without this the whole
            //  application is frozen for as long as the service takes.
            Application.ProcessMessages;
            Inc(Frame);
            FStatus.Caption := DownloadStatusText(Frame, FProgressBytes,
                FProgressTotal, SecondsBetween(Now, Started));
            Sleep(120);
        end;
        FJob.WaitFor;
        case FailureShown(FJob.Cancelled, FJob.Error) of
            fsStatusLine: FStatus.Caption := FJob.Error;
            fsDialog: Complain(FJob.Error);
        end;
    finally
        FWeb.SetProgress(nil);
        FCancel.Caption := 'Cancel';
        FreeAndNil(FJob);
    end;
end;

procedure TDataSourceWindow.NextClick(Sender: TObject);
begin
    //  While a download runs, this button is disabled and the click that
    //  reaches here is the one that started it.
    if FJob <> nil then
        Exit;
    Screen.Cursor := crHourGlass;
    try
        try
            //  The question's boxes, while they are the page on screen.
            if FWizard.Step = wsFind then
                ReadFields;
            FWizard.Advance;
        except
            on E: Exception do
            begin
                Report(E);
                ShowStep;
                Exit;
            end;
        end;
    finally
        Screen.Cursor := crDefault;
    end;

    //  ENTERING THE PREVIEW IS WHAT DOWNLOADS: one fetch, and what is
    //  previewed is what is imported. Outside the cursor above, because it
    //  runs in its own thread and this window stays alive while it does.
    if FWizard.NeedsDownload then
        DownloadWhileWaiting;
    ShowStep;
end;

procedure TDataSourceWindow.CreateClick(Sender: TObject);
var
    Verdict: TDataSourceVerdict;
begin
    Verdict := FWizard.CreateVerdict;
    if not Verdict.Allowed then
    begin
        Complain(Verdict.Reason);
        Exit;
    end;
    FImported := True;
    FImportedPath := FWizard.FilePath;
    FImportedOrigin := FWizard.Origin;
    ModalResult := mrOk;
end;

procedure NewProjectFromDataSource(AOwner: TComponent; AHost: IImportHost);
var
    Window: TDataSourceWindow;
begin
    Window := TDataSourceWindow.CreateWizard(AOwner, AHost);
    try
        Window.ShowModal;
        if not Window.Imported then
            Exit;
        try
            //  The import itself is data_source_import's order, through the
            //  host - the same four steps whatever started them.
            ImportDownload(AHost, Window.ImportedPath, Window.ImportedOrigin);
        except
            on E: Exception do
                //  THE LAST STEP CAN STILL FAIL: the file was fetched and read
                //  a moment ago, and between then and here it can be gone, the
                //  disk can be unreadable, or the engine can be unreachable.
                //  Without this the user meets an unhandled exception at the
                //  end of a wizard that had worked.
                MessageDlg('Data source', 'The data was fetched, but the ' +
                    'project could not be started from it: ' + E.Message,
                    mtError, [mbOK], 0);
        end;
    finally
        Window.Free;
    end;
end;

end.
