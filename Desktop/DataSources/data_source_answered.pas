// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(File > New Project from Data Source when nobody is there to click: the same
wizard, given its answers.)

WHY THIS IS NOT A SECOND IMPORT. The window (data_source_window) is a modal
form, so an unattended run cannot get past it - and every decision the window
makes is already TDataSourceWizard's: which step comes next, whether a search
may run, whether a row can be taken, what Create would import. This unit makes
the window's three gestures from answers instead of clicks - choose the source,
fill its fields and press Next, take a row - and ends in the same
data_source_import.ImportDownload the window ends in. Nothing here decides
anything the wizard does not.

THE ANSWERS (answer_source): data-source is the source's id; data-source.<field>
fills that field, and a field not answered keeps its default, as an untouched
box in the window does; data-source-item is which found row to take - by
number from 1, or by name - and the first when not given.

THE DOWNLOAD RUNS HERE, on the calling thread. The window runs it on a thread
so it can repaint; a run nobody watches has nothing to repaint, and waiting on
the same wizard.Download directly is the one call TDownloadJob makes.
}
unit data_source_answered;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, data_source_import, int_web_client;

{ Runs File > New Project from Data Source from the installed answers. True when the
  project was started from the data; False with the reason when the run was
  stopped - a question not answered, a source that refused, a download that
  failed - and nothing was imported. }
function ImportFromAnswers(AHost: IImportHost; AWeb: IWebClient;
    out AWhy: string): boolean;

implementation

uses
    answer_source, data_source, data_source_advice, data_source_wizard,
    download_cache;

const
    { The wizard has four steps; more passes than that is a loop. }
    MaxPasses = 8;

{ The row ASpec names: a number from 1, or a row's title, file name or id; -1
  when none does. }
function RowOf(AWizard: TDataSourceWizard; const ASpec: string): longint;
var
    N: longint;
begin
    if TryStrToInt(Trim(ASpec), N) then
    begin
        if (N >= 1) and (N <= AWizard.ItemCount) then
            Exit(N - 1);
        Exit(-1);
    end;
    //  By what the window shows - the title - or the file it would download,
    //  or the source's own id for it.
    for Result := 0 to AWizard.ItemCount - 1 do
        if SameText(AWizard.Item(Result).Title, Trim(ASpec)) or
            SameText(AWizard.Item(Result).FileName, Trim(ASpec)) or
            SameText(AWizard.Item(Result).Id, Trim(ASpec)) then
            Exit;
    Result := -1;
end;

function ImportFromAnswers(AHost: IImportHost; AWeb: IWebClient;
    out AWhy: string): boolean;
var
    Wizard: TDataSourceWizard;
    SourceId, Value, ItemSpec: string;
    Fields: TInputFieldDecls;
    Verdict: TDataSourceVerdict;
    I, Row, Pass: longint;
begin
    Result := False;
    AWhy := '';
    if ScriptedAnswer(QuestionDataSource, 'Which data source?', SourceId) <>
        arAnswered then
    begin
        AWhy := 'no data source was named (' + QuestionDataSource + ')';
        Exit;
    end;
    Wizard := TDataSourceWizard.Create(AWeb);
    try
        Wizard.CacheRoot := ChosenDownloadsRoot(AHost.DownloadFolder);
        try
            //  Choosing it in the window's list.
            Wizard.ChooseSource(Trim(SourceId));
            Wizard.EnterChosenSource;
            //  Its boxes, as answered; the rest keep their defaults.
            Fields := Wizard.SourceClass.Info.QueryFields;
            for I := 0 to High(Fields) do
                if (Fields[I].Id <> '') and
                    (ScriptedAnswer(QuestionDataSourceField + Fields[I].Id,
                        Fields[I].Caption, Value) = arAnswered) then
                    Wizard.SetField(Fields[I].Id, Value);
            //  A missing answer means the first row.
            ItemSpec := '1';
            //  NEXT, as often as the wizard has steps.
            for Pass := 1 to MaxPasses do
            begin
                if Wizard.Step = wsPreview then
                    Break;
                if Wizard.Step = wsChoose then
                begin
                    if ScriptedAnswer(QuestionDataSourceItem,
                        'Which of the items found?', Value) = arAnswered then
                        ItemSpec := Value;
                    Row := RowOf(Wizard, ItemSpec);
                    if Row < 0 then
                    begin
                        AWhy := Format('no item %s among the %d found',
                            [ItemSpec, Wizard.ItemCount]);
                        Exit;
                    end;
                    if not Wizard.SelectIfAllowed(Row) then
                    begin
                        AWhy := Wizard.ChooseVerdict(Row).Reason;
                        Exit;
                    end;
                end;
                Verdict := Wizard.CanGoNext;
                if not Verdict.Allowed then
                begin
                    AWhy := Verdict.Reason;
                    Exit;
                end;
                Wizard.Advance;
            end;
            if Wizard.NeedsDownload then
                Wizard.Download;
            Verdict := Wizard.CreateVerdict;
            if not Verdict.Allowed then
            begin
                AWhy := Verdict.Reason;
                if Wizard.PreviewError <> '' then
                    AWhy := AWhy + ': ' + Wizard.PreviewError;
                Exit;
            end;
            //  THE SAME LAST STEP the window's Create takes.
            Result := ImportDownload(AHost, Wizard.FilePath, Wizard.Origin);
            if not Result then
                AWhy := 'the open project was kept, so nothing was imported';
        except
            on E: Exception do
                AWhy := E.Message;
        end;
    finally
        Wizard.Free;
    end;
end;

end.
