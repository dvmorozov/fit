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

initialization
    RegisterTest('unit', TScreenRecordingTest);
end.
