// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a question the window would ask a person is answered when
nobody is there.)

WHY. A run nobody watches - a script, a module's batch - is meant to go through
the same commands a click does, and those commands ask: which file, overwrite
it, save first, which data source. Answering them by skipping the commands
would be a second path through the program, which is the bypass this codebase
forbids (AGENTS.md, non-negotiable 6). So each place that asks keeps asking,
through the seam it already has (IUiHost, IProjectHost, table_export's
callbacks, the data-source wizard), and that seam first asks THIS unit.

THREE ANSWERS, never a guess. With no source installed (every interactive
session) the question goes to the person, exactly as before. With one, an
answered question gets its value, and an unanswered one STOPS the run, naming
the question: a modal dialog in an unattended process is a process that never
ends, and a guessed yes may overwrite a file.

THE QUESTIONS ARE REGISTERED with what each asks, because the schema a script
is checked against is drawn from this list before anything runs. A question
asked without being registered could never be answered.
}
unit answer_source;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { What answers questions in a run nobody watches. CORBA, like every
      interface here: the installer keeps the object alive. }
    IAnswerSource = interface
        ['{6B1E0F4C-2D5A-4C3B-9E7F-1A2B3C4D5E61}']
        { True with the value when the run has an answer for AQuestionId. }
        function Answer(const AQuestionId: string; out AValue: string): boolean;
        { The run has no answer for this question, and stops. }
        procedure NotAnswered(const AQuestionId, AQuestionText: string);
        { What the window would have shown in a message box - a refusal, an
          error - and so the command did not do what it was asked: the run
          stops with this as the reason. }
        procedure Problem(const AText: string);
    end;

    TAnswerResult = (
        { Nobody installed a source: ask the person, as always. }
        arAskThePerson,
        { Use the value. }
        arAnswered,
        { Stop what was being done; the source has been told why. }
        arStop);

    TQuestionKind = (qkYesNo, qkText, qkPath, qkChoice);

    TQuestion = record
        Id: string;
        Kind: TQuestionKind;
        { What it asks, for the schema and the guide. }
        Help: string;
    end;

const
    { Any yes-or-no question a command asks before going on: Clear Model's
      confirmation, fitting with samples no curve covers. }
    QuestionConfirm = 'confirm';
    { A line of text a module asks for (IUiHost.AskText). }
    QuestionText = 'text';
    { Where File > Save Project As writes the project. }
    QuestionProjectPath = 'project-path';
    { Replace a file that is already there. }
    QuestionOverwrite = 'overwrite';
    { Save the open project before another replaces it: yes, no or cancel. }
    QuestionSaveFirst = 'save-first';
    { Where an export (summary table, curve parameters) is written. }
    QuestionExportPath = 'export-path';
    { The curve type the model is built with: its name as Model > Curve Type
      shows it, or its id. }
    QuestionCurveType = 'curve-type';
    { The data file File > Import Profile opens. }
    QuestionProfilePath = 'profile-path';
    { File > New Project from Data Source: which source, by its id. }
    QuestionDataSource = 'data-source';
    { File > New Project from Data Source: a field of the chosen source, as
      data-source.<field id>. }
    QuestionDataSourceField = 'data-source.';
    { File > New Project from Data Source: which of the found items, by number from
      one, or by its title, file name or id; the first when not given. }
    QuestionDataSourceItem = 'data-source-item';

    { Every question the framework's window asks, for the test that keeps the
      registry complete. }
    WindowQuestionIds: array[0..10] of string = (QuestionConfirm, QuestionText,
        QuestionProjectPath, QuestionOverwrite, QuestionSaveFirst,
        QuestionExportPath, QuestionProfilePath, QuestionCurveType,
        QuestionDataSource, QuestionDataSourceField, QuestionDataSourceItem);

{ Installs ASource for the questions that follow; nil puts every question back
  to the person. }
procedure UseAnswerSource(ASource: IAnswerSource);
{ Whether a source is installed - a run nobody watches. }
function AnsweringUnattended: boolean;

{ The answer to a question with a value. }
function ScriptedAnswer(const AQuestionId, AQuestionText: string;
    out AValue: string): TAnswerResult;
{ The answer to a box that keeps its default when left alone: True with the
  value when a run answers AQuestionId. }
function OptionalAnswer(const AQuestionId: string; out AValue: string): boolean;
{ The answer to a yes-or-no question: yes, no, true or false in any case. Any
  other answer stops the run - a typo is not a decision. }
function ScriptedYesNo(const AQuestionId, AQuestionText: string;
    out AYes: boolean): TAnswerResult;

{ A message box the window would show. False with no source installed: show
  it, as always. True with one: it was handed to the source as the run's
  problem, and must not be shown - nobody is there to close it. }
function UnattendedMessage(const ATitle, AText: string): boolean;

{ A dialog that asks for something no question covers - a number in a form of
  its own, a server address. False with no source installed: open it. True with
  one: the run stops, saying the command cannot be scripted, and the dialog must
  not open. }
function UnattendedDialog(const ADialog: string): boolean;

{ The registered questions. A module asking a question of its own registers it. }
procedure RegisterQuestion(const AId: string; AKind: TQuestionKind;
    const AHelp: string);
function QuestionCount: integer;
function Question(AIndex: integer): TQuestion;
function IndexOfQuestion(const AId: string): integer;

implementation

var
    Source: IAnswerSource = nil;
    Questions: array of TQuestion;

procedure UseAnswerSource(ASource: IAnswerSource);
begin
    Source := ASource;
end;

function AnsweringUnattended: boolean;
begin
    Result := Source <> nil;
end;

function ScriptedAnswer(const AQuestionId, AQuestionText: string;
    out AValue: string): TAnswerResult;
begin
    AValue := '';
    if Source = nil then
        Exit(arAskThePerson);
    if Source.Answer(AQuestionId, AValue) then
        Exit(arAnswered);
    Source.NotAnswered(AQuestionId, AQuestionText);
    Result := arStop;
end;

function OptionalAnswer(const AQuestionId: string; out AValue: string): boolean;
begin
    //  NOT A THIRD ANSWER: a box with a default is not a question the run has
    //  to answer, so leaving it alone neither asks the person nor stops the
    //  run - it keeps what it shows, as an untouched box in the window does.
    AValue := '';
    Result := (Source <> nil) and Source.Answer(AQuestionId, AValue);
end;

function ScriptedYesNo(const AQuestionId, AQuestionText: string;
    out AYes: boolean): TAnswerResult;
var
    Value: string;
begin
    AYes := False;
    Result := ScriptedAnswer(AQuestionId, AQuestionText, Value);
    if Result <> arAnswered then
        Exit;
    Value := LowerCase(Trim(Value));
    if (Value = 'yes') or (Value = 'true') then
        AYes := True
    else if (Value = 'no') or (Value = 'false') then
        AYes := False
    else
    begin
        Source.NotAnswered(AQuestionId, Format('%s (answered "%s", which is ' +
            'neither yes nor no)', [AQuestionText, Value]));
        Result := arStop;
    end;
end;

function UnattendedMessage(const ATitle, AText: string): boolean;
begin
    Result := Source <> nil;
    if not Result then
        Exit;
    if ATitle = '' then
        Source.Problem(AText)
    else
        Source.Problem(ATitle + ': ' + AText);
end;

function UnattendedDialog(const ADialog: string): boolean;
begin
    Result := Source <> nil;
    if Result then
        Source.Problem(Format('%s asks in a window of its own, which a script ' +
            'cannot answer', [ADialog]));
end;

procedure RegisterQuestion(const AId: string; AKind: TQuestionKind;
    const AHelp: string);
var
    I: integer;
begin
    I := IndexOfQuestion(AId);
    if I < 0 then
    begin
        I := Length(Questions);
        SetLength(Questions, I + 1);
    end;
    Questions[I].Id := AId;
    Questions[I].Kind := AKind;
    Questions[I].Help := AHelp;
end;

function QuestionCount: integer;
begin
    Result := Length(Questions);
end;

function Question(AIndex: integer): TQuestion;
begin
    Result := Questions[AIndex];
end;

function IndexOfQuestion(const AId: string): integer;
begin
    for Result := 0 to High(Questions) do
        if Questions[Result].Id = AId then
            Exit;
    Result := -1;
end;

initialization
    RegisterQuestion(QuestionConfirm, qkYesNo,
        'Any yes-or-no question a command asks before going on - Model > ' +
        'Clear Model''s confirmation, replacing a project file, or fitting with ' +
        'samples no curve covers.');
    RegisterQuestion(QuestionText, qkText,
        'A line of text a command asks for, such as a name.');
    RegisterQuestion(QuestionProjectPath, qkPath,
        'The project file: where File > Save Project As writes it, or which ' +
        'one File > Open Project opens.');
    RegisterQuestion(QuestionOverwrite, qkYesNo,
        'Whether an export replaces a file that is already there.');
    RegisterQuestion(QuestionSaveFirst, qkChoice,
        'Whether to save the open project before another replaces it: yes, ' +
        'no or cancel.');
    RegisterQuestion(QuestionExportPath, qkPath,
        'Where File > Export Summary Table or Model > Export Curve ' +
        'Parameters writes its table.');
    RegisterQuestion(QuestionCurveType, qkChoice,
        'The curve type the model is built with (ChooseCurveType): its name ' +
        'as Model > Curve Type shows it, or its id.');
    RegisterQuestion(QuestionProfilePath, qkPath,
        'The data file File > Import Profile opens.');
    RegisterQuestion(QuestionDataSource, qkChoice,
        'File > New Project from Data Source: the source, by its id (fred, ecb, ...).');
    RegisterQuestion(QuestionDataSourceField, qkText,
        'File > New Project from Data Source: a field of the chosen source, written ' +
        'data-source.<field id> - data-source.series for FRED''s series. A field ' +
        'not answered keeps its default, as an untouched box in the window does.');
    RegisterQuestion(QuestionDataSourceItem, qkChoice,
        'File > New Project from Data Source: which of the items found, by number ' +
        'from 1 or by its title, file name or id; the first when not given.');
end.
