// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the report a module contributes: its verdicts, as data.

  WHY THESE EXIST. A report is what a user reads to learn whether what they built
  follows the rules of their field. Two failures would be silent. A finding that
  says nothing about its status must not read as a verdict - so zero is
  information, never a pass or a failure. And a failure or a warning that links
  to no explanation, or to one that does not exist, tells the user THAT something
  is wrong but never why; the completeness check names every such finding so a
  module cannot ship one. }
unit testcase_module_report;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, module_report_types;

type
    TModuleReportTest = class(TTestCase)
    published
        procedure AZeroInitialisedFindingIsInformationNotAVerdict;
        procedure AReportWithNothingInItIsEmpty;
        procedure AHeaderFindingAloneMakesAReport;
        procedure ASectionAloneMakesAReport;
        procedure EveryStatusIsNamedInItsOwnWord;
        procedure NewReportFindingFillsWhatItIsGiven;
        procedure TopicsAreListedOnceInTheOrderTheyAppear;
        procedure AFailingFindingWithNoTopicIsNamed;
        procedure AWarningWithNoTopicIsNamed;
        procedure APassOrInformationNeedsNoTopic;
        procedure ATopicThatDoesNotResolveIsNamed;
        procedure ASectionTopicThatDoesNotResolveIsNamed;
        procedure AWellFormedReportHasNoFindings;
        procedure NoResolverChecksOnlyTheMissingTopics;
    end;

implementation

function KnownOnly(const ATopic: string): boolean;
begin
    Result := Copy(ATopic, 1, 6) = 'known/';
end;

function Finding(AStatus: TReportStatus; const ATopic: string): TReportFinding;
begin
    Result := NewReportFinding(AStatus, 'rule', 'Checked thing',
        'What was measured.', ATopic);
end;

function WithSection(const AFindings: array of TReportFinding;
    const ASectionTopic: string = ''): TModuleReport;
var
    i: integer;
begin
    Result := Default(TModuleReport);
    Result.Module := 'sample';
    SetLength(Result.Sections, 1);
    Result.Sections[0].Title := 'First part';
    Result.Sections[0].Topic := ASectionTopic;
    SetLength(Result.Sections[0].Findings, Length(AFindings));
    for i := 0 to High(AFindings) do
        Result.Sections[0].Findings[i] := AFindings[i];
end;

function Contains(const AItems: TStringArray; const AText: string): boolean;
var
    i: integer;
begin
    Result := False;
    for i := 0 to High(AItems) do
        if Pos(AText, AItems[i]) > 0 then
            Exit(True);
end;

procedure TModuleReportTest.AZeroInitialisedFindingIsInformationNotAVerdict;
begin
    AssertTrue(Default(TReportFinding).Status = rpInfo);
    AssertTrue(Default(TModuleReport).Verdict = rpInfo);
end;

procedure TModuleReportTest.AReportWithNothingInItIsEmpty;
var
    R: TModuleReport;
begin
    R := Default(TModuleReport);
    R.Module := 'sample';
    R.Title := 'A title is not content';
    AssertTrue(ReportIsEmpty(R));
end;

procedure TModuleReportTest.AHeaderFindingAloneMakesAReport;
var
    R: TModuleReport;
begin
    R := Default(TModuleReport);
    SetLength(R.Header, 1);
    AssertFalse(ReportIsEmpty(R));
end;

procedure TModuleReportTest.ASectionAloneMakesAReport;
begin
    AssertFalse(ReportIsEmpty(WithSection([])));
end;

procedure TModuleReportTest.EveryStatusIsNamedInItsOwnWord;
var
    A, B: TReportStatus;
begin
    for A := Low(TReportStatus) to High(TReportStatus) do
    begin
        AssertTrue(ReportStatusCaption(A) <> '');
        for B := Low(TReportStatus) to High(TReportStatus) do
            if A <> B then
                AssertFalse('two statuses read alike',
                    ReportStatusCaption(A) = ReportStatusCaption(B));
    end;
end;

procedure TModuleReportTest.NewReportFindingFillsWhatItIsGiven;
var
    F: TReportFinding;
begin
    F := NewReportFinding(rpFail, 'rule', 'T', 'D', 'known/t');
    AssertTrue(F.Status = rpFail);
    AssertEquals('rule', F.Kind);
    AssertEquals('T', F.Title);
    AssertEquals('D', F.Detail);
    AssertEquals('known/t', F.Topic);
    AssertEquals('', F.Measured);
    AssertEquals('', F.Limit);
    AssertEquals('', F.RowId);
end;

procedure TModuleReportTest.TopicsAreListedOnceInTheOrderTheyAppear;
var
    R: TModuleReport;
    T: TStringArray;
begin
    R := WithSection([Finding(rpPass, 'known/b'), Finding(rpFail, 'known/a'),
        Finding(rpPass, 'known/b'), Finding(rpInfo, '')], 'known/s');
    SetLength(R.Header, 1);
    R.Header[0] := Finding(rpWarning, 'known/h');
    T := ReportTopics(R);
    AssertEquals(4, Length(T));
    AssertEquals('known/h', T[0]);
    AssertEquals('known/s', T[1]);
    AssertEquals('known/b', T[2]);
    AssertEquals('known/a', T[3]);
end;

procedure TModuleReportTest.AFailingFindingWithNoTopicIsNamed;
var
    Found: TStringArray;
begin
    Found := ModuleReportFindings(WithSection([Finding(rpFail, '')]), @KnownOnly);
    AssertEquals(1, Length(Found));
    AssertTrue(Found[0], Contains(Found, 'First part'));
    AssertTrue(Found[0], Contains(Found, 'no explanation'));
end;

procedure TModuleReportTest.AWarningWithNoTopicIsNamed;
begin
    AssertEquals(1, Length(ModuleReportFindings(
        WithSection([Finding(rpWarning, '')]), @KnownOnly)));
end;

procedure TModuleReportTest.APassOrInformationNeedsNoTopic;
begin
    AssertEquals(0, Length(ModuleReportFindings(
        WithSection([Finding(rpPass, ''), Finding(rpInfo, '')]), @KnownOnly)));
end;

procedure TModuleReportTest.ATopicThatDoesNotResolveIsNamed;
var
    Found: TStringArray;
    R: TModuleReport;
begin
    R := WithSection([Finding(rpPass, 'gone/x')]);
    SetLength(R.Header, 1);
    R.Header[0] := Finding(rpInfo, 'gone/h');
    Found := ModuleReportFindings(R, @KnownOnly);
    AssertEquals(2, Length(Found));
    AssertTrue(Contains(Found, 'gone/x'));
    AssertTrue(Contains(Found, 'gone/h'));
end;

procedure TModuleReportTest.ASectionTopicThatDoesNotResolveIsNamed;
begin
    AssertTrue(Contains(ModuleReportFindings(
        WithSection([], 'gone/s'), @KnownOnly), 'gone/s'));
end;

procedure TModuleReportTest.AWellFormedReportHasNoFindings;
begin
    AssertEquals(0, Length(ModuleReportFindings(WithSection(
        [Finding(rpFail, 'known/a'), Finding(rpPass, '')], 'known/s'),
        @KnownOnly)));
end;

procedure TModuleReportTest.NoResolverChecksOnlyTheMissingTopics;
begin
    AssertEquals(1, Length(ModuleReportFindings(WithSection(
        [Finding(rpFail, ''), Finding(rpPass, 'gone/x')]), nil)));
end;

initialization
    RegisterTest('unit', TModuleReportTest);
end.
