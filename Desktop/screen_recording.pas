// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The rules of recording a fit: the switch that turns it on, the rate
its stills are counted in, and what the frames are called.)

THE ORDINARY APPLICATION, RECORDING ITSELF. /RECORD_FIT=<dir> changes nothing
about what the window does: it opens the project it would open anyway, and the
steps that follow - Model > Clear Model, View > Animation Mode, Fit >
Automatically - are those actions' own Execute, run inside Application.Run.
The one addition is that each time the chart has been updated, the window is
rendered into a numbered PNG (window_recorder). The capture task makes the video
from those frames and picks the screenshots out of them.

ONE FRAME PER UPDATE OF THE CHART, not per stretch of time. What the video is
for is the sequence the fit went through, and a window that draws slowly - a VM
painting in software draws a fraction of the frames a desktop does - still
draws every step of it. Timed to the clock, those steps were spread over
repeats of whatever was on the chart, and most of the video was one picture.
}
unit screen_recording;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

const
    { /RECORD_FIT=<dir>: record the fit of the project opened at start-up. }
    RecordSwitch = 'RECORD_FIT';
    { The rate the video plays at, which the stills before and after the fit
      are counted in: two seconds of the project are 2 * RecordPlayFps frames. }
    RecordPlayFps = 12;
    { /RECORD_RUN=[<command>,...]<fit command>: the window's commands the
      recording runs, the last a Fit command whose end ends the recording.
      Without it, Fit > Automatically, as every recording of free Fit has been. }
    RecordRunSwitch = 'RECORD_RUN';

type
    { The Fit menu's commands a recording can run, named as the menu names
      them (automatically, minimize-difference). A module product's own site
      records its markup being fitted, which Fit > Automatically would clear
      and rebuild as peaks - so the capture task says which, from what the
      module's identity file declares. }
    TRecordRun = (rrAutomatically, rrMinimizeDifference);

{ Frame AIndex (from 1) in ADir, in the pattern the encoder reads:
  frame_%06d.png. }
function FrameFileName(const ADir: string; AIndex: longint): string;

{ The run AValue names; an empty value is Fit > Automatically. False for a name
  that is not one of them: a misspelt run is refused rather than recorded as
  another, which would replace the site's pictures with a fit nobody asked for. }
function RecordRunOf(const AValue: string; out ARun: TRecordRun): boolean;

{ Whether the recording clears the model before it runs: only Fit >
  Automatically, which builds a model of its own. Minimize Difference fits the
  model the project holds, and would have nothing to fit after a clear. }
function RecordRunClearsModel(ARun: TRecordRun): boolean;

{ The steps AValue names: the commands run first, each by the id its row has in
  the window's command table (a module's is <module>.<command>, so two modules
  cannot collide), and the Fit command last. A MODULE PRODUCT'S RECORDING IS OF
  ITS OWN WORK - Fit Pro's runs Detect Waves, which decomposes the series into a
  count, and the fit then refines it - and the framework names no module, so the
  module's identity file says which. False for a list that does not end in a
  Fit command (the recording ends when its fit does, so it would never end) or
  that has an empty step. }
function RecordStepsOf(const AValue: string; out ACommands: TStringArray;
    out ARun: TRecordRun): boolean;

implementation

function FrameFileName(const ADir: string; AIndex: longint): string;
begin
    Result := IncludeTrailingPathDelimiter(ADir) + Format('frame_%.6d.png', [AIndex]);
end;

function RecordRunOf(const AValue: string; out ARun: TRecordRun): boolean;
var
    Name: string;
begin
    Name := LowerCase(Trim(AValue));
    ARun := rrAutomatically;
    Result := True;
    if (Name = '') or (Name = 'automatically') then
        ARun := rrAutomatically
    else if Name = 'minimize-difference' then
        ARun := rrMinimizeDifference
    else
        Result := False;
end;

function RecordRunClearsModel(ARun: TRecordRun): boolean;
begin
    Result := ARun = rrAutomatically;
end;

function RecordStepsOf(const AValue: string; out ACommands: TStringArray;
    out ARun: TRecordRun): boolean;
var
    Parts: TStringArray;
    i: longint;
begin
    ACommands := nil;
    ARun := rrAutomatically;
    if Trim(AValue) = '' then
        Exit(True);
    Parts := AValue.Split([',']);
    for i := 0 to High(Parts) do
    begin
        Parts[i] := Trim(Parts[i]);
        if Parts[i] = '' then
            Exit(False);
    end;
    Result := RecordRunOf(Parts[High(Parts)], ARun);
    if Result then
        ACommands := Copy(Parts, 0, High(Parts));
end;

end.
