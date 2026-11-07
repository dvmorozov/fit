// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The words a module gives the framework's generic workflow: how they are
registered, which one is in force, and that every registration in this build is
sound.)

A MODULE SAYS A TERM DIFFERENTLY BY REGISTERING IT, and the framework's own
words stand for every term it does not. The registration is judged on a
vocabulary of the test's own, so it leaves the program's alone; what this build
registered is judged by the last test, which walks it and so fails by name for
the next module that registers two terms alike.
}
unit testcase_module_vocabulary;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, module_vocabulary;

type
    TModuleVocabularyTest = class(TTestCase)
    published
        procedure ASilentModuleGetsTheFrameworksWords;
        procedure AModuleSaysOnlyWhatItRegistered;
        procedure AnotherModuleIsUnaffected;
        procedure TheSameWordTwiceIsOneRegistration;
        procedure ADifferentWordForTheSameTermIsRefused;
        procedure AnEmptyWordIsRefused;
        procedure TwoTermsReadingAlikeAreFoundByName;
        procedure AModuleWithNoCurveTypeIsFoundByName;
        procedure EveryTermHasDefaultWordsOfItsOwn;
        procedure EveryVocabularyThisBuildRegisteredIsSound;
    end;

implementation

uses
    curve_type_registration, curve_types_singleton, int_curve_type_iterator;

procedure TModuleVocabularyTest.ASilentModuleGetsTheFrameworksWords;
var
    V: TModuleVocabulary;
    T: TVocabularyTerm;
begin
    V := TModuleVocabulary.Create;
    try
        for T := Low(TVocabularyTerm) to High(TVocabularyTerm) do
            AssertEquals(FrameworkWords(T), V.WordsFor('Nobody', T));
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.AModuleSaysOnlyWhatItRegistered;
var
    V: TModuleVocabulary;
begin
    V := TModuleVocabulary.Create;
    try
        V.Say('Waves', vtCurvePositions, 'Wave positions');
        AssertEquals('Wave positions', V.WordsFor('Waves', vtCurvePositions));
        AssertEquals('the rest stay the framework''s',
            FrameworkWords(vtFitIntervals), V.WordsFor('Waves', vtFitIntervals));
        AssertEquals('and the module''s name is not case-sensitive',
            'Wave positions', V.WordsFor('waves', vtCurvePositions));
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.AnotherModuleIsUnaffected;
var
    V: TModuleVocabulary;
begin
    V := TModuleVocabulary.Create;
    try
        V.Say('Waves', vtCurvePositions, 'Wave positions');
        AssertEquals(FrameworkWords(vtCurvePositions),
            V.WordsFor('Peaks', vtCurvePositions));
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.TheSameWordTwiceIsOneRegistration;
var
    V: TModuleVocabulary;
begin
    //  A front door may be called twice.
    V := TModuleVocabulary.Create;
    try
        V.Say('Waves', vtCurvePositions, 'Wave positions');
        V.Say('Waves', vtCurvePositions, 'Wave positions');
        AssertEquals(1, Length(V.Modules));
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.ADifferentWordForTheSameTermIsRefused;
var
    V: TModuleVocabulary;
    Refused: boolean;
begin
    V := TModuleVocabulary.Create;
    try
        V.Say('Waves', vtCurvePositions, 'Wave positions');
        Refused := False;
        try
            V.Say('Waves', vtCurvePositions, 'Wave starts');
        except
            on EArgumentException do
                Refused := True;
        end;
        AssertTrue('two meanings for one term', Refused);
        AssertEquals('and the first stands', 'Wave positions',
            V.WordsFor('Waves', vtCurvePositions));
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.AnEmptyWordIsRefused;
var
    V: TModuleVocabulary;
    Refused: boolean;
begin
    V := TModuleVocabulary.Create;
    try
        Refused := False;
        try
            V.Say('Waves', vtCurvePositions, '  ');
        except
            on EArgumentException do
                Refused := True;
        end;
        AssertTrue(Refused);
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.TwoTermsReadingAlikeAreFoundByName;
var
    V: TModuleVocabulary;
    Found: TVocabularyStrings;
begin
    //  A legend caption stands for every row that carries it, so two terms
    //  with one caption would be hidden by one script command.
    V := TModuleVocabulary.Create;
    try
        V.Say('Waves', vtCurvePositions, 'Positions');
        V.Say('Waves', vtFittedPositions, 'positions');
        Found := V.Findings(['Waves']);
        AssertEquals(1, Length(Found));
        AssertTrue(Found[0], Pos('Waves', Found[0]) > 0);
        AssertTrue(Found[0], Pos('alike', Found[0]) > 0);
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.AModuleWithNoCurveTypeIsFoundByName;
var
    V: TModuleVocabulary;
    Found: TVocabularyStrings;
begin
    //  Words registered under a name no curve type is registered under would
    //  never be in force: the owner of the selected type is the only key.
    V := TModuleVocabulary.Create;
    try
        V.Say('Wavs', vtCurvePositions, 'Wave positions');
        Found := V.Findings(['Waves']);
        AssertEquals(1, Length(Found));
        AssertTrue(Found[0], Pos('Wavs', Found[0]) > 0);
    finally
        V.Free;
    end;
end;

procedure TModuleVocabularyTest.EveryTermHasDefaultWordsOfItsOwn;
var
    A, B: TVocabularyTerm;
begin
    for A := Low(TVocabularyTerm) to High(TVocabularyTerm) do
    begin
        AssertTrue('words for term ' + IntToStr(Ord(A)), FrameworkWords(A) <> '');
        for B := Low(TVocabularyTerm) to High(TVocabularyTerm) do
            if A <> B then
                AssertFalse(FrameworkWords(A) + ' and ' + FrameworkWords(B) +
                    ' read alike', SameText(FrameworkWords(A), FrameworkWords(B)));
    end;
end;

{ The registry walk: the vocabularies every module's front door registered into
  this binary, judged against the owners the registered curve types have. }
procedure TModuleVocabularyTest.EveryVocabularyThisBuildRegisteredIsSound;
var
    Owners: array of string;
    Iter: ICurveTypeIterator;
    Found: TVocabularyStrings;
    F: string;
begin
    Owners := nil;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        try
            SetLength(Owners, Length(Owners) + 1);
            Owners[High(Owners)] := CurveTypeOwner(Iter.GetCurveTypeId);
        except
            on EListError do
            begin
                SetLength(Owners, Length(Owners) - 1);
                Break;
            end;
        end;
        if Iter.EndCurveType then
            Break;
        Iter.NextCurveType;
    end;
    Found := ProgramVocabulary.Findings(Owners);
    for F in Found do
        Fail(F);
end;

initialization
    RegisterTest('unit', TModuleVocabularyTest);

end.
