// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Renders the window into numbered PNG frames while a fit is recorded.)

CALLED WHERE THE CHART HAS JUST BEEN UPDATED. The window asks for a frame after
each repaint of its chart (TFormMain.ChartPaintTiming), so a frame is a picture
of what the user would be looking at that moment - nothing drives the window
from here. One frame per update: see screen_recording for why not per second.

THE WINDOW ONLY DRAWS; A THREAD WRITES. Encoding a 1440x900 PNG costs more than
drawing it, and both used to run in the window's own loop: the recorder took
forty per cent of it, the window's progress timer fired a fraction as often as
it should, and the fit it was recording was drawn in fewer frames because it was
being recorded. So Capture paints into a bitmap and hands it over; the writer
thread encodes it. The queue is bounded - a frame that cannot be handed over is
not taken at all, which costs one frame rather than the window's memory.

RENDERED, NOT GRABBED. TWinControl.PaintTo is QWidget::render on Qt, which draws
every child, so it works where a Wayland session refuses a screen grab. Captures
are taken on Linux only - on Windows PaintTo draws the controls Fit paints
itself blank (tools/build-lib/capture.ps1, Test-CaptureHere).
}
unit window_recorder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, SyncObjs, Forms, Graphics, IntfGraphics,
    screen_recording;

type
    TWindowRecorder = class;

    { Encodes what the window painted, off the window's thread. }
    TFrameWriter = class(TThread)
    private
        FOwner: TWindowRecorder;
    protected
        procedure Execute; override;
    public
        constructor Create(AOwner: TWindowRecorder);
    end;

    TWindowRecorder = class
    private
        FForm: TCustomForm;
        FDir: string;
        FFrames: longint;
        FLastFrame: string;
        FRendering: boolean;
        FRenders: longint;
        FPaintMs: int64;
        FSaveMs: int64;
        FDropped: longint;
        { The frames painted and not yet written, and their file names. }
        FLock: TCriticalSection;
        FWaiting: TEventObject;
        FQueue: TList;
        FNames: TStringList;
        FWriter: TFrameWriter;
        procedure Save(AImage: TLazIntfImage; const APath: string);
        { The next frame to write, or nil. Called by the writer thread. }
        function TakeQueued(out APath: string): TLazIntfImage;
        function QueueLength: longint;
    public
        constructor Create(AForm: TCustomForm; const ADir: string);
        destructor Destroy; override;

        { A frame of the window as it is now: painted here, written by the
          thread. Skipped when the writer is that far behind. }
        procedure Capture;
        { AFrames more of the last picture, as a still: the cleared project
          before the fit, the result after it. }
        procedure Hold(AFrames: longint);
        { Waits until every frame handed over has been written. }
        procedure Finish;

        { True while the window is being painted into a frame - its chart
          repaints then too, and that repaint must not ask for another frame. }
        property Rendering: boolean read FRendering;
        property FramesWritten: longint read FFrames;
        property Renders: longint read FRenders;
        { What a frame costs: drawing it in the window's thread, and encoding
          it in the writer's. }
        property PaintMs: int64 read FPaintMs;
        property SaveMs: int64 read FSaveMs;
        { Frames not taken because the writer was behind. }
        property Dropped: longint read FDropped;
    end;

implementation

uses
    DateUtils, FileUtil, ZStream;

const
    { How many painted frames may wait to be written. Each is the size of the
      window in 24-bit pixels - about four megabytes. }
    QueueLimit = 12;

{ ----------------------------- the writer ---------------------------------- }

constructor TFrameWriter.Create(AOwner: TWindowRecorder);
begin
    FOwner := AOwner;
    inherited Create(False);
end;

procedure TFrameWriter.Execute;
var
    Img: TLazIntfImage;
    Path: string;
begin
    while True do
    begin
        Img := FOwner.TakeQueued(Path);
        if not Assigned(Img) then
        begin
            if Terminated then
                Break;
            //  Nothing to write: sleep until the window hands something over,
            //  and wake at least often enough to notice Terminate.
            FOwner.FWaiting.WaitFor(50);
            Continue;
        end;
        try
            FOwner.Save(Img, Path);
        finally
            Img.Free;
        end;
    end;
end;

{ ---------------------------- the recorder --------------------------------- }

constructor TWindowRecorder.Create(AForm: TCustomForm; const ADir: string);
begin
    inherited Create;
    FForm := AForm;
    FDir := ADir;
    ForceDirectories(ADir);
    FLock := TCriticalSection.Create;
    FWaiting := TEventObject.Create(nil, False, False, '');
    FQueue := TList.Create;
    FNames := TStringList.Create;
    FWriter := TFrameWriter.Create(Self);
end;

destructor TWindowRecorder.Destroy;
var
    i: longint;
begin
    if Assigned(FWriter) then
    begin
        FWriter.Terminate;
        FWaiting.SetEvent;
        FWriter.WaitFor;
        FWriter.Free;
    end;
    for i := 0 to FQueue.Count - 1 do
        TLazIntfImage(FQueue[i]).Free;
    FQueue.Free;
    FNames.Free;
    FWaiting.Free;
    FLock.Free;
    inherited Destroy;
end;

function TWindowRecorder.QueueLength: longint;
begin
    FLock.Acquire;
    try
        Result := FQueue.Count;
    finally
        FLock.Release;
    end;
end;

function TWindowRecorder.TakeQueued(out APath: string): TLazIntfImage;
begin
    Result := nil;
    APath := '';
    FLock.Acquire;
    try
        if FQueue.Count = 0 then
            Exit;
        Result := TLazIntfImage(FQueue[0]);
        APath := FNames[0];
        FQueue.Delete(0);
        FNames.Delete(0);
    finally
        FLock.Release;
    end;
end;

procedure TWindowRecorder.Save(AImage: TLazIntfImage; const APath: string);
var
    Writer: TLazWriterPNG;
    Began: TDateTime;
begin
    Began := Now;
    //  THE FASTEST DEFLATE: ffmpeg re-encodes the frames anyway, and this runs
    //  while the window is drawing the next one.
    Writer := TLazWriterPNG.Create;
    try
        Writer.CompressionLevel := clfastest;
        Writer.UseAlpha := False;
        AImage.SaveToFile(APath, Writer);
    finally
        Writer.Free;
    end;
    Inc(FSaveMs, MilliSecondsBetween(Now, Began));
end;

procedure TWindowRecorder.Capture;
var
    Bmp: TBitmap;
    Img: TLazIntfImage;
    Began: TDateTime;
    Path: string;
begin
    //  NOT TAKEN AT ALL when the writer is behind: a frame held here would cost
    //  the window the memory, and the next one is a moment away.
    if QueueLength >= QueueLimit then
    begin
        Inc(FDropped);
        Exit;
    end;
    Began := Now;
    FRendering := True;
    Bmp := TBitmap.Create;
    try
        Bmp.PixelFormat := pf24bit;
        //  The whole window, menu bar included, is what the widget set paints.
        Bmp.SetSize(FForm.Width, FForm.Height);
        Bmp.Canvas.Brush.Color := clWindow;
        Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
        FForm.PaintTo(Bmp.Canvas, 0, 0);
        //  HANDED OVER AS PIXELS, not as a bitmap. A TBitmap belongs to the
        //  widget set that made it, and freeing one on the writer's thread
        //  faulted the process - an access violation in the destructor, which
        //  ended a recording a run in four. The image below is plain memory.
        Img := Bmp.CreateIntfImage;
    finally
        Bmp.Free;
        FRendering := False;
    end;
    Inc(FFrames);
    Path := FrameFileName(FDir, FFrames);
    FLastFrame := Path;
    FLock.Acquire;
    try
        FQueue.Add(Img);
        FNames.Add(Path);
    finally
        FLock.Release;
    end;
    FWaiting.SetEvent;
    Inc(FRenders);
    Inc(FPaintMs, MilliSecondsBetween(Now, Began));
end;

procedure TWindowRecorder.Finish;
begin
    //  Every frame handed over is on disk before the caller reads the count -
    //  the task encodes them as soon as the process ends.
    while QueueLength > 0 do
    begin
        FWaiting.SetEvent;
        Sleep(20);
    end;
end;

procedure TWindowRecorder.Hold(AFrames: longint);
var
    i: longint;
begin
    if FLastFrame = '' then
        Exit;
    //  The still is a copy of the last frame, so it waits for it to be written.
    Finish;
    for i := 1 to AFrames do
    begin
        Inc(FFrames);
        CopyFile(FLastFrame, FrameFileName(FDir, FFrames));
    end;
end;

end.
