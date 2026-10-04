// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The rules of recording a fit: the switch, and what the frames are
called.)

The recording runs inside the application, over a real window, and nothing
checks it but a person watching the video. So everything about it that is a rule
rather than a pixel lives in screen_recording and is asserted here.
}
unit testcase_screen_recording;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, screen_recording,
    command_line_switches;

type
    TScreenRecordingTest = class(TTestCase)
    published
        procedure FramesAreNumberedForFfmpeg;
        procedure TheSwitchIsNotMistakenForAnother;
        procedure ARecordingRunsFitAutomaticallyUnlessToldOtherwise;
        procedure ARecordingRunsTheFitCommandItIsNamed;
        procedure AnUnknownRunIsRefusedNotGuessed;
        procedure OnlyTheAutomaticRunStartsFromAnEmptyModel;
        procedure TheRunSwitchIsNotMistakenForTheRecordingSwitch;
        procedure ACommandOfTheWindowCanRunBeforeTheFit;
        procedure TheFitCommandAloneIsAStepListOfOne;
        procedure ARecordingEndsWithAFitOrIsRefused;
        procedure AnEmptyStepIsRefused;
    end;

implementation

procedure TScreenRecordingTest.FramesAreNumberedForFfmpeg;
begin
    //  The pattern the encoder is given is frame_%06d.png, counted from one.
    AssertEquals(IncludeTrailingPathDelimiter('out') + 'frame_000001.png',
        FrameFileName('out', 1));
    AssertEquals(IncludeTrailingPathDelimiter('out') + 'frame_012345.png',
        FrameFileName('out', 12345));
end;

procedure TScreenRecordingTest.TheSwitchIsNotMistakenForAnother;
var
    Args: TStringList;
    Value: string;
begin
    //  A SWITCH IS MATCHED AS A SUBSTRING of the argument, so the recording
    //  switch and the ones the client already reads must not contain each
    //  other.
    Args := TStringList.Create;
    try
        Args.Add('/' + RecordSwitch + '=frames');
        AssertTrue(SwitchFound(Args, RecordSwitch, Value));
        AssertEquals('frames', Value);
        AssertFalse(SwitchFound(Args, 'CHECK_UI', Value));
        AssertFalse(SwitchFound(Args, 'PROJECT', Value));
        AssertFalse(SwitchFound(Args, 'INFILE', Value));
        Args.Clear;
        Args.Add('/PROJECT=a.fitproj');
        AssertFalse(SwitchFound(Args, RecordSwitch, Value));
    finally
        Args.Free;
    end;
end;

procedure TScreenRecordingTest.ARecordingRunsFitAutomaticallyUnlessToldOtherwise;
var
    Chosen: TRecordRun;
begin
    //  What every recording of free Fit has been: no switch, no change.
    AssertTrue(RecordRunOf('', Chosen));
    AssertTrue(Chosen = rrAutomatically);
end;

procedure TScreenRecordingTest.ARecordingRunsTheFitCommandItIsNamed;
var
    Chosen: TRecordRun;
begin
    //  Named after the Fit menu's commands, so a capture says in the words
    //  the window uses what it will show.
    AssertTrue(RecordRunOf('automatically', Chosen));
    AssertTrue(Chosen = rrAutomatically);
    AssertTrue(RecordRunOf('minimize-difference', Chosen));
    AssertTrue(Chosen = rrMinimizeDifference);
    AssertTrue(RecordRunOf('Minimize-Difference', Chosen));
    AssertTrue(Chosen = rrMinimizeDifference);
end;

procedure TScreenRecordingTest.AnUnknownRunIsRefusedNotGuessed;
var
    Chosen: TRecordRun;
begin
    //  A misspelt run recorded as the automatic one would replace the
    //  site's pictures with a fit nobody asked for.
    AssertFalse(RecordRunOf('fit', Chosen));
    AssertFalse(RecordRunOf('minimise-difference', Chosen));
end;

procedure TScreenRecordingTest.OnlyTheAutomaticRunStartsFromAnEmptyModel;
begin
    //  Fit > Automatically builds its own model, so the recording clears the
    //  one the project holds. Minimize Difference fits the model as it is -
    //  a module's markup, a count marked by hand - and clearing it would leave
    //  nothing to fit.
    AssertTrue(RecordRunClearsModel(rrAutomatically));
    AssertFalse(RecordRunClearsModel(rrMinimizeDifference));
end;

procedure TScreenRecordingTest.TheRunSwitchIsNotMistakenForTheRecordingSwitch;
var
    Args: TStringList;
    Value: string;
begin
    Args := TStringList.Create;
    try
        Args.Add('/' + RecordRunSwitch + '=minimize-difference');
        AssertTrue(SwitchFound(Args, RecordRunSwitch, Value));
        AssertEquals('minimize-difference', Value);
        AssertFalse(SwitchFound(Args, RecordSwitch, Value));
        Args.Clear;
        Args.Add('/' + RecordSwitch + '=frames');
        AssertFalse(SwitchFound(Args, RecordRunSwitch, Value));
    finally
        Args.Free;
    end;
end;

procedure TScreenRecordingTest.ACommandOfTheWindowCanRunBeforeTheFit;
var
    Steps: TStringArray;
    Chosen: TRecordRun;
begin
    //  A module's own command - Detect Waves, which decomposes the series into
    //  a count - runs first, by the id its row has in the window's command
    //  table, and the recorded fit refines what it made.
    AssertTrue(RecordStepsOf('example.decompose, minimize-difference',
        Steps, Chosen));
    AssertEquals(1, Length(Steps));
    AssertEquals('example.decompose', Steps[0]);
    AssertTrue(Chosen = rrMinimizeDifference);
end;

procedure TScreenRecordingTest.TheFitCommandAloneIsAStepListOfOne;
var
    Steps: TStringArray;
    Chosen: TRecordRun;
begin
    AssertTrue(RecordStepsOf('minimize-difference', Steps, Chosen));
    AssertEquals(0, Length(Steps));
    AssertTrue(Chosen = rrMinimizeDifference);
    AssertTrue(RecordStepsOf('', Steps, Chosen));
    AssertEquals(0, Length(Steps));
    AssertTrue(Chosen = rrAutomatically);
end;

procedure TScreenRecordingTest.ARecordingEndsWithAFitOrIsRefused;
var
    Steps: TStringArray;
    Chosen: TRecordRun;
begin
    //  The recording ends when its fit does; a list ending in anything else
    //  would never end.
    AssertFalse(RecordStepsOf('example.decompose', Steps, Chosen));
    AssertFalse(RecordStepsOf('minimize-difference,example.decompose', Steps, Chosen));
end;

procedure TScreenRecordingTest.AnEmptyStepIsRefused;
var
    Steps: TStringArray;
    Chosen: TRecordRun;
begin
    AssertFalse(RecordStepsOf(',minimize-difference', Steps, Chosen));
    AssertFalse(RecordStepsOf('a,,minimize-difference', Steps, Chosen));
end;

initialization
    RegisterTest('unit', TScreenRecordingTest);
end.
