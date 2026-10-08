// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Notices a module registers: listed in the About box, and the ones that
require it acknowledged once per major version.)

A notice is an explanation topic - its words live where every other explanation
lives - that a module declares to be terms the user should see. The decisions
here are plain functions over plain values: which notices are still to be
acknowledged, what storing an acknowledgement does, what the About box writes.
}
unit testcase_notice_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, notice_registry;

type
    TNoticeRegistryTest = class(TTestCase)
    private
        function Two: TNotices;
    published
        procedure AMajorVersionIsTheFirstNumber;
        procedure ANoticeNeverAcknowledgedIsAsked;
        procedure ANoticeThatAsksNothingIsNeverAsked;
        procedure AnAcknowledgementHoldsForItsMajorVersion;
        procedure ANewMajorVersionAsksAgain;
        procedure StoringAnAcknowledgementKeepsTheOthers;
        procedure StoringItAgainReplacesTheVersion;
        procedure AStoredListFromANewerBuildIsReadAsItCan;
        procedure ANoticeReadsAsItsTitleSummaryAndParagraphs;
        procedure RegisteringTwiceRegistersOnce;
        procedure ANoticeWhoseTopicResolvesNowhereIsAFinding;
        procedure AcceptingEveryNoticeStoresEachAndGoesOn;
        procedure DecliningOneStopsAndStoresNothingForIt;
        procedure ANoticeWithNoWordsIsNotAskedAndDoesNotStop;
    end;

implementation

function TNoticeRegistryTest.Two: TNotices;
begin
    SetLength(Result, 2);
    Result[0].Topic := 'pack/disclaimer';
    Result[0].RequiresAcknowledgement := True;
    Result[1].Topic := 'pack/credits';
    Result[1].RequiresAcknowledgement := False;
end;

procedure TNoticeRegistryTest.AMajorVersionIsTheFirstNumber;
begin
    AssertEquals('1', MajorVersionOf('1.2.0.1980'));
    AssertEquals('12', MajorVersionOf('12.0'));
    AssertEquals('a build that cannot name itself', '', MajorVersionOf(''));
end;

procedure TNoticeRegistryTest.ANoticeNeverAcknowledgedIsAsked;
var
    Ask: TStringArray;
begin
    Ask := NoticesToAcknowledge(Two, '', '1');
    AssertEquals(1, Length(Ask));
    AssertEquals('pack/disclaimer', Ask[0]);
end;

procedure TNoticeRegistryTest.ANoticeThatAsksNothingIsNeverAsked;
var
    N: TNotices;
begin
    N := Two;
    N[0].RequiresAcknowledgement := False;
    AssertEquals(0, Length(NoticesToAcknowledge(N, '', '1')));
end;

procedure TNoticeRegistryTest.AnAcknowledgementHoldsForItsMajorVersion;
begin
    AssertEquals(0, Length(NoticesToAcknowledge(Two, 'pack/disclaimer=1', '1')));
end;

{ New terms come with a new major version; a new build of the same one does not
  ask again. }
procedure TNoticeRegistryTest.ANewMajorVersionAsksAgain;
begin
    AssertEquals(1, Length(NoticesToAcknowledge(Two, 'pack/disclaimer=1', '2')));
end;

procedure TNoticeRegistryTest.StoringAnAcknowledgementKeepsTheOthers;
var
    S: string;
begin
    S := WithAcknowledgement('other/terms=3', 'pack/disclaimer', '1');
    AssertEquals(0, Length(NoticesToAcknowledge(Two, S, '1')));
    AssertTrue('the other kept', Pos('other/terms=3', S) > 0);
end;

procedure TNoticeRegistryTest.StoringItAgainReplacesTheVersion;
var
    S: string;
begin
    S := WithAcknowledgement('pack/disclaimer=1', 'pack/disclaimer', '2');
    AssertEquals('pack/disclaimer=2', S);
end;

{ Settings are read by whatever build is running; an entry it cannot make sense
  of is skipped, not fatal, and a notice it names is simply asked again. }
procedure TNoticeRegistryTest.AStoredListFromANewerBuildIsReadAsItCan;
begin
    AssertEquals(0, Length(NoticesToAcknowledge(Two,
        'garbage;;=;pack/disclaimer=1;x', '1')));
end;

procedure TNoticeRegistryTest.ANoticeReadsAsItsTitleSummaryAndParagraphs;
var
    E: TExplanation;
    Text: string;
begin
    E := NewExplanation('pack/disclaimer', 'Not advice', 'It measures a count.',
        esModelChoice);
    AddParagraph(E, 'No forecasts.');
    Text := NoticeText(E);
    AssertTrue(Pos('Not advice', Text) = 1);
    AssertTrue(Pos('It measures a count.', Text) > 0);
    AssertTrue(Pos('No forecasts.', Text) > Pos('It measures a count.', Text));
end;

procedure TNoticeRegistryTest.RegisteringTwiceRegistersOnce;
var
    Before: integer;
begin
    RegisterNotice('test-notices/once', False);
    Before := Length(RegisteredNotices);
    RegisterNotice('test-notices/once', True);
    AssertEquals(Before, Length(RegisteredNotices));
end;

function NeverResolves(const ATopic: string): boolean;
begin
    Result := False;
end;

function AlwaysResolves(const ATopic: string): boolean;
begin
    Result := True;
end;

procedure TNoticeRegistryTest.ANoticeWhoseTopicResolvesNowhereIsAFinding;
begin
    AssertEquals(2, Length(NoticeFindings(Two, @NeverResolves)));
    AssertEquals(0, Length(NoticeFindings(Two, @AlwaysResolves)));
end;

var
    Asked: TStringList;
    DeclineTitle: string;

function AskAndRecord(const ATitle, AText: string): boolean;
begin
    Asked.Add(ATitle);
    Result := ATitle <> DeclineTitle;
end;

function FindTwo(const ATopic: string; out AExplanation: TExplanation): boolean;
begin
    Result := (ATopic = 'pack/disclaimer') or (ATopic = 'pack/terms');
    if Result then
        AExplanation := NewExplanation(ATopic, 'Title of ' + ATopic,
            'A summary.', esModelChoice);
end;

function TwoToAsk: TNotices;
begin
    SetLength(Result, 2);
    Result[0].Topic := 'pack/disclaimer';
    Result[0].RequiresAcknowledgement := True;
    Result[1].Topic := 'pack/terms';
    Result[1].RequiresAcknowledgement := True;
end;

procedure TNoticeRegistryTest.AcceptingEveryNoticeStoresEachAndGoesOn;
var
    Stored: string;
begin
    Asked := TStringList.Create;
    try
        DeclineTitle := '';
        Stored := '';
        AssertTrue(AcknowledgeNotices(TwoToAsk, '1', @FindTwo, @AskAndRecord, Stored));
        AssertEquals('both asked', 2, Asked.Count);
        AssertEquals('both stored', 0, Length(NoticesToAcknowledge(TwoToAsk, Stored, '1')));
    finally
        FreeAndNil(Asked);
    end;
end;

{ Declining ends it: the application closes, and the next start asks again. }
procedure TNoticeRegistryTest.DecliningOneStopsAndStoresNothingForIt;
var
    Stored: string;
begin
    Asked := TStringList.Create;
    try
        DeclineTitle := 'Title of pack/disclaimer';
        Stored := '';
        AssertFalse(AcknowledgeNotices(TwoToAsk, '1', @FindTwo, @AskAndRecord, Stored));
        AssertEquals('nothing asked after it', 1, Asked.Count);
        AssertEquals('asked again next time', 2,
            Length(NoticesToAcknowledge(TwoToAsk, Stored, '1')));
    finally
        FreeAndNil(Asked);
    end;
end;

{ A notice with no words cannot be accepted, and a user cannot be stopped by
  it; NoticeFindings is where it fails, by name, before a release. }
procedure TNoticeRegistryTest.ANoticeWithNoWordsIsNotAskedAndDoesNotStop;
var
    N: TNotices;
    Stored: string;
begin
    Asked := TStringList.Create;
    try
        N := TwoToAsk;
        N[1].Topic := 'pack/missing';
        DeclineTitle := '';
        Stored := '';
        AssertTrue(AcknowledgeNotices(N, '1', @FindTwo, @AskAndRecord, Stored));
        AssertEquals(1, Asked.Count);
    finally
        FreeAndNil(Asked);
    end;
end;

initialization
    RegisterTest('unit', TNoticeRegistryTest);
end.
