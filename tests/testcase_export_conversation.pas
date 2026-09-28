// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The conversation that gets a name out of the user before a table is
exported.)

THE LOOP, WHICH NOTHING COULD REACH BEFORE. Which question to ask, and what each
answer to it means, have been in table_export and tested for some time; the loop
AROUND them was a `repeat` inside the window - and before that a loop with a
label and two gotos - so the one thing worth checking about it could not be
checked at all.

WHAT IS WORTH CHECKING is that the two "no" answers are different. "No" to
Overwrite means pick another name, so the dialog comes back; Cancel means give
up, so it does not. Getting those the same way round is a loop that either
cannot be escaped or throws the user out on their first mistake, and neither is
visible in the pieces - only in the loop.

The three things the window does - open the dialog, put a question, look on
disk - are passed in, so nothing here opens a window or touches a file.
}
unit testcase_export_conversation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, table_export;

type
    TExportConversationTest = class(TTestCase)
    private
        { What the fake dialog answers, in order: one entry per time it is
          opened. An empty entry is a cancel. }
        FOffers: array of string;
        FOfferAt: longint;
        { What the user answers each question, in order. }
        FAnswers: array of TExportAnswer;
        FAnswerAt: longint;
        { Which paths the fake disk already holds. }
        FExisting: string;
        { What was actually asked, so an ordering can be read back. }
        FAsked: string;

        function Ask(out APath: string): boolean;
        function Answer(AQuestion: TExportQuestion;
            const APath: string): TExportAnswer;
        function Exists(const APath: string): boolean;

        procedure Offers(const APaths: array of string);
        procedure Answers(const AAnswers: array of TExportAnswer);
        function Run: string;
    protected
        procedure SetUp; override;
    published
        procedure AFreshNameIsWrittenWithoutAskingAnything;
        procedure ANameWithNoExtensionGetsOne;
        procedure OverwritingIsAskedAboutAndAcceptedNamesTheFile;
        procedure DecliningToOverwriteAsksForAnotherName;
        procedure CancellingTheOverwriteQuestionGivesUp;
        procedure CancellingTheFileDialogGivesUpWithoutAsking;
        procedure AnEmptyNameIsQueriedRatherThanWritten;
        procedure TheFileAskedAboutIsTheOneThatWouldBeWritten;
    end;

implementation

procedure TExportConversationTest.SetUp;
begin
    FOffers := nil;
    FAnswers := nil;
    FOfferAt := 0;
    FAnswerAt := 0;
    FExisting := '';
    FAsked := '';
end;

procedure TExportConversationTest.Offers(const APaths: array of string);
var
    i: longint;
begin
    SetLength(FOffers, Length(APaths));
    for i := Low(APaths) to High(APaths) do
        FOffers[i] := APaths[i];
end;

procedure TExportConversationTest.Answers(const AAnswers: array of TExportAnswer);
var
    i: longint;
begin
    SetLength(FAnswers, Length(AAnswers));
    for i := Low(AAnswers) to High(AAnswers) do
        FAnswers[i] := AAnswers[i];
end;

function TExportConversationTest.Ask(out APath: string): boolean;
begin
    APath := '';
    Result := False;
    if FOfferAt > High(FOffers) then
        Exit;
    APath := FOffers[FOfferAt];
    Inc(FOfferAt);
    //  An empty offer is the dialog being cancelled.
    Result := APath <> '';
end;

function TExportConversationTest.Answer(AQuestion: TExportQuestion;
    const APath: string): TExportAnswer;
begin
    if AQuestion <> eqNone then
    begin
        if FAsked <> '' then
            FAsked := FAsked + ';';
        FAsked := FAsked + APath;
    end;
    Result := eaCancel;
    if FAnswerAt > High(FAnswers) then
        Exit;
    Result := FAnswers[FAnswerAt];
    Inc(FAnswerAt);
end;

function TExportConversationTest.Exists(const APath: string): boolean;
begin
    Result := (APath <> '') and (Pos(APath, FExisting) > 0);
end;

function TExportConversationTest.Run: string;
begin
    Result := ChooseExportPath(@Ask, @Answer, @Exists);
end;

procedure TExportConversationTest.AFreshNameIsWrittenWithoutAskingAnything;
begin
    //  Nothing to warn about: a name was given and no file is in the way.
    Offers(['report.txt']);
    AssertEquals('report.txt', Run);
    AssertEquals('and the user was asked nothing', '', FAsked);
end;

procedure TExportConversationTest.ANameWithNoExtensionGetsOne;
begin
    //  A file with no extension opens in nothing.
    Offers(['report']);
    AssertEquals('report' + DefaultTableExtension, Run);
end;

procedure TExportConversationTest.OverwritingIsAskedAboutAndAcceptedNamesTheFile;
begin
    FExisting := 'report.txt';
    Offers(['report.txt']);
    Answers([eaYes]);
    AssertEquals('report.txt', Run);
end;

procedure TExportConversationTest.DecliningToOverwriteAsksForAnotherName;
begin
    //  "No" MEANS PICK ANOTHER NAME, so the dialog comes back. Treated as a
    //  give-up it would throw the user out for declining to destroy a file.
    FExisting := 'old.txt';
    Offers(['old.txt', 'new.txt']);
    Answers([eaNo]);
    AssertEquals('the second name is the one used', 'new.txt', Run);
end;

procedure TExportConversationTest.CancellingTheOverwriteQuestionGivesUp;
begin
    //  AND CANCEL DOES NOT ASK AGAIN. This is the half that makes "No" safe:
    //  if the two behaved alike, the loop either could not be escaped or threw
    //  the user out on their first mistake.
    FExisting := 'old.txt';
    Offers(['old.txt', 'new.txt']);
    Answers([eaCancel]);
    AssertEquals('nothing is written', '', Run);
    AssertEquals('and the dialog was opened once', 1, FOfferAt);
end;

procedure TExportConversationTest.CancellingTheFileDialogGivesUpWithoutAsking;
begin
    //  Cancelling the file dialog is a complete answer on its own; putting a
    //  question about a name the user declined to give is nonsense.
    Offers(['']);
    AssertEquals('', Run);
    AssertEquals('nothing was asked about', '', FAsked);
end;

procedure TExportConversationTest.AnEmptyNameIsQueriedRatherThanWritten;
begin
    //  A dialog that returns a blank name. Writing to it fails a long way from
    //  the command the user gave, so it is queried here.
    Offers([' ', 'report.txt']);
    Answers([eaYes]);
    AssertEquals('the second attempt is used', 'report.txt', Run);
end;

procedure TExportConversationTest.TheFileAskedAboutIsTheOneThatWouldBeWritten;
begin
    //  THE EXTENSION IS SETTLED BEFORE THE QUESTION. Asked about "report" while
    //  "report.txt" is what would be overwritten, the user is warned about the
    //  wrong file - or not warned at all.
    FExisting := 'report' + DefaultTableExtension;
    Offers(['report']);
    Answers([eaYes]);
    AssertEquals('report' + DefaultTableExtension, Run);
    AssertEquals('and that is what they were asked about',
        'report' + DefaultTableExtension, FAsked);
end;

initialization
    //  A unit test: the dialog, the question and the disk are all passed in.
    RegisterTest('unit', TExportConversationTest);
end.
