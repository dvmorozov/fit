// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Run Log says of each stage of a run, run or skipped.)
}
unit testcase_run_stages;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_progress_json,
    run_event_text, explanation, guide_fitting;

type
    TRunStagesTest = class(TTestCase)
    published
        procedure EveryStageHasAName;
        procedure AStageThatRanIsNamedUnderTheStagesHeading;
        procedure ASkippedStageSaysSoAndWhy;
        procedure ASkippedStageWithoutAReasonIsRefused;
        procedure TheTopicItNamesIsInTheGuide;
    end;

implementation

procedure TRunStagesTest.EveryStageHasAName;
var
    S: TRunStage;
begin
    for S := Low(TRunStage) to High(TRunStage) do
        AssertTrue('stage ' + IntToStr(Ord(S)) + ' has a name',
            RunStageName(S) <> '');
end;

procedure TRunStagesTest.AStageThatRanIsNamedUnderTheStagesHeading;
var
    E: TRunEvent;
begin
    E := StageRanEvent(rsIntervalSearch, '45 intervals above the background');
    AssertEquals(RunSectionStages, E.Section);
    AssertEquals(RunStageName(rsIntervalSearch), E.Title);
    AssertEquals('45 intervals above the background', E.Detail);
    AssertEquals(RunEventFramework, E.Source);
end;

procedure TRunStagesTest.ASkippedStageSaysSoAndWhy;
var
    E: TRunEvent;
begin
    E := StageSkippedEvent(rsPeakSearch, 'the model already has its picks');
    AssertTrue('the title says skipped: ' + E.Title, Pos('skipped', E.Title) > 0);
    AssertTrue('and names the stage', Pos(RunStageName(rsPeakSearch), E.Title) = 1);
    AssertEquals('the model already has its picks', E.Detail);
end;

procedure TRunStagesTest.ASkippedStageWithoutAReasonIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        StageSkippedEvent(rsReduction, '  ');
    except
        Raised := True;
    end;
    AssertTrue('a stage left out says why', Raised);
end;

procedure TRunStagesTest.TheTopicItNamesIsInTheGuide;
var
    All: TExplanations;
    i: longint;
begin
    //  Written twice - the engine names it, the guide explains it, and no
    //  unit of one may use the other's - so tied here.
    AssertEquals(guide_fitting.RunStagesTopic, run_event_text.RunStagesTopic);
    All := FittingExplanations;
    for i := 0 to High(All) do
        if All[i].Topic = run_event_text.RunStagesTopic then
            Exit;
    Fail('the fitting chapter explains ' + run_event_text.RunStagesTopic);
end;

initialization
    RegisterTest('unit', TRunStagesTest);
end.
