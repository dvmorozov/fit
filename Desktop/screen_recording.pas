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

{ Frame AIndex (from 1) in ADir, in the pattern the encoder reads:
  frame_%06d.png. }
function FrameFileName(const ADir: string; AIndex: longint): string;

implementation

function FrameFileName(const ADir: string; AIndex: longint): string;
begin
    Result := IncludeTrailingPathDelimiter(ADir) + Format('frame_%.6d.png', [AIndex]);
end;

end.
