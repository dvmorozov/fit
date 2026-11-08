// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The custom axis dialog's boxes, answered when nobody is there.)

WHAT THIS DEFENDS. Choosing a custom axis or R-factor scale opens a dialog of
four boxes, and with a run nobody watches - a script, or the window's own
check-ui - that dialog stopped the run, so nothing could choose a custom scale
without a person. Each box is a question with an id now; these tests pin that
the ids are registered, which is what lets a script name them, and what the
answers make of the dialog.
}
unit testcase_custom_axis_answers;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, answer_source, custom_axis,
    custom_axis_answers, testcase_answer_source;

type
    TCustomAxisAnswersTest = class(TTestCase)
    private
        FAnswers: TListAnswers;
        function Shown: TCustomAxisDefinition;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EveryBoxOfTheDialogIsAQuestionARunCanAnswer;
        procedure AnAnsweredBoxTakesTheAnswer;
        procedure ABoxNotAnsweredKeepsWhatItShows;
        procedure AnswersAreTrimmedAsTheWindowsOKTrimsThem;
    end;

implementation

function TCustomAxisAnswersTest.Shown: TCustomAxisDefinition;
begin
    Result.Name := 'Custom';
    Result.Units := 'eV';
    Result.Forward_ := 'x';
    Result.Inverse := 'x';
end;

procedure TCustomAxisAnswersTest.SetUp;
begin
    FAnswers := TListAnswers.Create;
    UseAnswerSource(FAnswers);
end;

procedure TCustomAxisAnswersTest.TearDown;
begin
    //  CORBA: the interface first, then the object.
    UseAnswerSource(nil);
    FAnswers.Free;
    FAnswers := nil;
end;

{ A question that is not registered can never be answered: a script is checked
  against the registered questions before anything runs. }
procedure TCustomAxisAnswersTest.EveryBoxOfTheDialogIsAQuestionARunCanAnswer;
var
    F: TCustomAxisField;
    I: integer;
begin
    for F := Low(TCustomAxisField) to High(TCustomAxisField) do
    begin
        I := IndexOfQuestion(CustomAxisQuestion[F]);
        AssertTrue(CustomAxisQuestion[F] + ' is registered', I >= 0);
        AssertTrue(CustomAxisQuestion[F] + ' says what it asks',
            Trim(Question(I).Help) <> '');
    end;
end;

procedure TCustomAxisAnswersTest.AnAnsweredBoxTakesTheAnswer;
var
    A: TCustomAxisDefinition;
begin
    FAnswers.Answers.Values[CustomAxisQuestion[cafName]] := 'Twice';
    FAnswers.Answers.Values[CustomAxisQuestion[cafUnit]] := 'nm';
    FAnswers.Answers.Values[CustomAxisQuestion[cafForward]] := '2*x';
    FAnswers.Answers.Values[CustomAxisQuestion[cafInverse]] := 'x/2';
    A := AnsweredCustomAxis(Shown);
    AssertEquals('Twice', A.Name);
    AssertEquals('nm', A.Units);
    AssertEquals('2*x', A.Forward_);
    AssertEquals('x/2', A.Inverse);
    AssertEquals('a box is not a question the run must answer', 0,
        FAnswers.Unanswered.Count);
end;

procedure TCustomAxisAnswersTest.ABoxNotAnsweredKeepsWhatItShows;
var
    A: TCustomAxisDefinition;
begin
    FAnswers.Answers.Values[CustomAxisQuestion[cafForward]] := 'ln(x)';
    A := AnsweredCustomAxis(Shown);
    AssertEquals('ln(x)', A.Forward_);
    AssertEquals('Custom', A.Name);
    AssertEquals('eV', A.Units);
    AssertEquals('x', A.Inverse);
    AssertEquals('nothing stops the run', 0, FAnswers.Unanswered.Count);
end;

procedure TCustomAxisAnswersTest.AnswersAreTrimmedAsTheWindowsOKTrimsThem;
begin
    FAnswers.Answers.Values[CustomAxisQuestion[cafInverse]] := '  exp(x) ';
    AssertEquals('exp(x)', AnsweredCustomAxis(Shown).Inverse);
end;

initialization
    RegisterTest('unit', TCustomAxisAnswersTest);
end.
