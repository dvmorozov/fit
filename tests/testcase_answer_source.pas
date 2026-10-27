// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a question the window would ask a person is answered when
nobody is there.)

WHAT THIS DEFENDS. A run nobody watches - a script - goes through the same
commands a click does, and those commands ask questions: which file, overwrite
it, save first. Each question has an id, and a run that has an answer for it
gets that answer; a run that has none STOPS, naming the question, because a
modal dialog in an unattended run is a process that never ends. With no source
installed every question goes to the person, exactly as before.
}
unit testcase_answer_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, answer_source;

type
    { Answers from a list of id=value lines; remembers the questions it could
      not answer. A plain object, freed by the fixture. }
    TListAnswers = class(TObject, IAnswerSource)
    public
        Answers: TStringList;
        Unanswered: TStringList;
        Problems: TStringList;
        constructor Create;
        destructor Destroy; override;
        function Answer(const AQuestionId: string; out AValue: string): boolean;
        procedure NotAnswered(const AQuestionId, AQuestionText: string);
        procedure Problem(const AText: string);
    end;

    TAnswerSourceTest = class(TTestCase)
    private
        FAnswers: TListAnswers;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure WithNoSourceEveryQuestionGoesToThePerson;
        procedure AnAnsweredQuestionIsAnsweredWithItsValue;
        procedure AnUnansweredQuestionStopsTheRunAndIsNamed;
        procedure ABoxWithADefaultLeftUnansweredKeepsItsDefault;
        procedure AYesOrNoAnswerIsReadEitherWay;
        procedure AYesOrNoAnswerThatIsNeitherStopsTheRun;
        procedure EveryQuestionTheWindowAsksIsRegisteredWithItsMeaning;
        procedure AQuestionIdIsRegisteredOnce;
        procedure AMessageIsShownToAPersonAndHandedToARun;
        procedure ADialogNoQuestionCoversOpensForAPersonAndStopsARun;
    end;

implementation

constructor TListAnswers.Create;
begin
    inherited Create;
    Answers := TStringList.Create;
    Unanswered := TStringList.Create;
    Problems := TStringList.Create;
end;

destructor TListAnswers.Destroy;
begin
    Problems.Free;
    Unanswered.Free;
    Answers.Free;
    inherited Destroy;
end;

function TListAnswers.Answer(const AQuestionId: string; out AValue: string): boolean;
begin
    Result := Answers.IndexOfName(AQuestionId) >= 0;
    AValue := Answers.Values[AQuestionId];
end;

procedure TListAnswers.NotAnswered(const AQuestionId, AQuestionText: string);
begin
    Unanswered.Add(AQuestionId + ': ' + AQuestionText);
end;

procedure TListAnswers.Problem(const AText: string);
begin
    Problems.Add(AText);
end;

procedure TAnswerSourceTest.SetUp;
begin
    FAnswers := TListAnswers.Create;
end;

procedure TAnswerSourceTest.TearDown;
begin
    //  CORBA: the interface first, then the object.
    UseAnswerSource(nil);
    FAnswers.Free;
    FAnswers := nil;
end;

procedure TAnswerSourceTest.WithNoSourceEveryQuestionGoesToThePerson;
var
    Value: string;
begin
    UseAnswerSource(nil);
    AssertTrue(ScriptedAnswer(QuestionProjectPath, 'Save the project as', Value) =
        arAskThePerson);
end;

procedure TAnswerSourceTest.AnAnsweredQuestionIsAnsweredWithItsValue;
var
    Value: string;
begin
    FAnswers.Answers.Values[QuestionProjectPath] := '/w/oil.fitproj';
    UseAnswerSource(FAnswers);
    AssertTrue(ScriptedAnswer(QuestionProjectPath, 'Save the project as', Value) =
        arAnswered);
    AssertEquals('/w/oil.fitproj', Value);
    AssertEquals('nothing left unanswered', 0, FAnswers.Unanswered.Count);
end;

procedure TAnswerSourceTest.AnUnansweredQuestionStopsTheRunAndIsNamed;
var
    Value: string;
begin
    UseAnswerSource(FAnswers);
    AssertTrue(ScriptedAnswer(QuestionExportPath, 'Export the table to', Value) =
        arStop);
    AssertEquals(1, FAnswers.Unanswered.Count);
    AssertTrue('names the id', Pos(QuestionExportPath, FAnswers.Unanswered[0]) = 1);
    AssertTrue('and the question', Pos('Export the table to', FAnswers.Unanswered[0]) > 0);
end;

{ A BOX THAT HAS A DEFAULT IS NOT A QUESTION THE RUN MUST ANSWER: an untouched
  box in the window keeps what it shows. The data source's own fields are such
  boxes - FRED's dates, asked only for a custom range - and the pipeline's
  first run stopped on the first one it did not name (2026-10-22). }
procedure TAnswerSourceTest.ABoxWithADefaultLeftUnansweredKeepsItsDefault;
var
    Value: string;
begin
    UseAnswerSource(nil);
    AssertFalse('a person fills the box themselves',
        OptionalAnswer(QuestionDataSourceField + 'from', Value));
    UseAnswerSource(FAnswers);
    AssertFalse('not answered', OptionalAnswer(QuestionDataSourceField + 'from', Value));
    AssertEquals('and the run is not stopped for it', 0, FAnswers.Unanswered.Count);
    FAnswers.Answers.Values[QuestionDataSourceField + 'range'] := '5y';
    AssertTrue('answered', OptionalAnswer(QuestionDataSourceField + 'range', Value));
    AssertEquals('5y', Value);
end;

procedure TAnswerSourceTest.AYesOrNoAnswerIsReadEitherWay;
var
    Yes: boolean;
begin
    UseAnswerSource(FAnswers);
    FAnswers.Answers.Values[QuestionConfirm] := 'yes';
    AssertTrue(ScriptedYesNo(QuestionConfirm, 'Clear the model?', Yes) = arAnswered);
    AssertTrue(Yes);
    FAnswers.Answers.Values[QuestionConfirm] := 'No';
    AssertTrue(ScriptedYesNo(QuestionConfirm, 'Clear the model?', Yes) = arAnswered);
    AssertFalse(Yes);
end;

procedure TAnswerSourceTest.AYesOrNoAnswerThatIsNeitherStopsTheRun;
var
    Yes: boolean;
begin
    //  'maybe' is a typo in a script, not a decision: guessing either way could
    //  overwrite a file or keep one the run was asked to replace.
    UseAnswerSource(FAnswers);
    FAnswers.Answers.Values[QuestionOverwrite] := 'maybe';
    AssertTrue(ScriptedYesNo(QuestionOverwrite, 'Replace it?', Yes) = arStop);
    AssertEquals(1, FAnswers.Unanswered.Count);
end;

procedure TAnswerSourceTest.EveryQuestionTheWindowAsksIsRegisteredWithItsMeaning;
var
    Id: string;
    I: integer;
begin
    //  THE SCHEMA A SCRIPT IS CHECKED AGAINST is drawn from this list, so a
    //  question asked without being listed could never be answered.
    for Id in WindowQuestionIds do
    begin
        I := IndexOfQuestion(Id);
        AssertTrue(Id + ' is registered', I >= 0);
        AssertTrue(Id + ' says what it asks', Question(I).Help <> '');
    end;
end;

procedure TAnswerSourceTest.AQuestionIdIsRegisteredOnce;
var
    I, J: integer;
begin
    for I := 0 to QuestionCount - 1 do
        for J := I + 1 to QuestionCount - 1 do
            AssertFalse(Question(I).Id, Question(I).Id = Question(J).Id);
end;

procedure TAnswerSourceTest.AMessageIsShownToAPersonAndHandedToARun;
begin
    UseAnswerSource(nil);
    AssertFalse('a person sees it', UnattendedMessage('Refused', 'No points'));
    UseAnswerSource(FAnswers);
    AssertTrue('a run does not', UnattendedMessage('Refused', 'No points'));
    AssertEquals('it is the run''s problem', 'Refused: No points',
        Trim(FAnswers.Problems.Text));
end;

procedure TAnswerSourceTest.ADialogNoQuestionCoversOpensForAPersonAndStopsARun;
begin
    UseAnswerSource(nil);
    AssertFalse('it opens', UnattendedDialog('Fit > Set Maximum R-factor'));
    UseAnswerSource(FAnswers);
    AssertTrue('it does not', UnattendedDialog('Fit > Set Maximum R-factor'));
    AssertTrue('and says which and why',
        Pos('Fit > Set Maximum R-factor asks in a window', FAnswers.Problems[0]) = 1);
end;

initialization
    RegisterTest('unit', TAnswerSourceTest);
end.
