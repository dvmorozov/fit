// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the update's progress window says, and the one place the
checker's thread leaves it for the window's.)

WHY THIS EXISTS. Install now used to download a 65 MB installer with nothing on
the screen at all: the window went on as if nothing had been asked until, a
minute later, a message said how it ended. An updater that shows nothing reads
as one that did nothing, and a user who cannot tell a slow download from a hung
one cannot decide to wait or to give up.

SO THE WAIT IS SHOWN THE WAY DESKTOP UPDATERS SHOW IT: a small window of its
own, not modal, saying what is happening in words ("Downloading version
1.2.0.2131"), how far it has got in bytes and as a bar, roughly how long is
left, and a Cancel that works while the bytes are still arriving. A stage with
no measure - asking the feed, comparing the digest - draws a moving bar rather
than a frozen one. Cancel is offered only while it can still change something:
once the digest is being compared the download is over, and once the installer
is handed over it is no longer this program's.

EVERY WORD AND NUMBER IS DECIDED HERE, over plain values, so a test reaches all
of it; the window (Desktop/Forms/update_progress_window.pas) reads a box and
writes controls.

THE BOX, AND WHY NOT SYNCHRONIZE. The checker reports from its own thread, as
often as curl hands over a chunk - more than a thousand times for one
installer. Synchronizing each report would stall the download behind the
window's message loop; a TThread.Queue per report would pile up a thousand
redraws. The checker instead posts the latest state into a box under a lock,
and the window reads it on a timer, ten times a second, whatever the rate the
bytes arrive at.
}
unit update_progress;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, SyncObjs, update_check;

type
    TTickSource = function: QWord;

    { The latest progress, handed from the checker's thread to the window's. }
    TUpdateProgressBox = class
    private
        FLock: TCriticalSection;
        FLatest: TUpdateProgress;
        FPosted: boolean;
        FStageStarted: QWord;
        FTicks: TTickSource;
    public
        constructor Create;
        destructor Destroy; override;
        { Keeps AProgress as the latest; the stage's clock starts again when
          the stage is a new one. }
        procedure Post(const AProgress: TUpdateProgress);
        { The latest progress and how long its stage has run; False when
          nothing has been posted yet. }
        function Read(out AProgress: TUpdateProgress;
            out AElapsedMs: int64): boolean;
        { What the stage's clock reads; GetTickCount64 unless a test says. }
        property Ticks: TTickSource read FTicks write FTicks;
    end;

{ A length of data in the unit a person reads it in: bytes, KB or MB. }
function ByteSizeText(ABytes: int64): string;

{ The one line saying what is happening. }
function UpdateProgressTitle(const AProgress: TUpdateProgress): string;

{ The line under it: how far, and how long is left when that can be told. }
function UpdateProgressDetail(const AProgress: TUpdateProgress;
    AElapsedMs: int64): string;

{ How full the bar is, 0..100, or -1 when there is nothing to measure and
  the bar should move by itself. }
function UpdateProgressPercent(const AProgress: TUpdateProgress): integer;

{ Whether Cancel can still change anything. }
function UpdateProgressCancellable(const AProgress: TUpdateProgress): boolean;

implementation

constructor TUpdateProgressBox.Create;
begin
    inherited Create;
    FLock := TCriticalSection.Create;
    FTicks := @GetTickCount64;
end;

destructor TUpdateProgressBox.Destroy;
begin
    FLock.Free;
    inherited Destroy;
end;

procedure TUpdateProgressBox.Post(const AProgress: TUpdateProgress);
begin
    FLock.Enter;
    try
        if (not FPosted) or (AProgress.Stage <> FLatest.Stage) then
            FStageStarted := FTicks();
        FLatest := AProgress;
        FPosted := True;
    finally
        FLock.Leave;
    end;
end;

function TUpdateProgressBox.Read(out AProgress: TUpdateProgress;
    out AElapsedMs: int64): boolean;
begin
    FLock.Enter;
    try
        Result := FPosted;
        AProgress := FLatest;
        AElapsedMs := 0;
        if FPosted then
            AElapsedMs := int64(FTicks() - FStageStarted);
    finally
        FLock.Leave;
    end;
end;

const
    KB = int64(1024);
    MB = KB * 1024;
    { Before this long a rate is the first chunk's, which says more about
      the connection's start than about the download. }
    EstimateAfterMs = 2000;

{ AVALUE with one decimal and a full stop, whatever the user's locale says:
  every other figure the program writes reads the same way. }
function OneDecimal(AValue: double): string;
var
    Fmt: TFormatSettings;
begin
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Result := FormatFloat('0.0', AValue, Fmt);
end;

function ByteSizeText(ABytes: int64): string;
begin
    if ABytes = 1 then
        Result := '1 byte'
    else if ABytes < KB then
        Result := IntToStr(ABytes) + ' bytes'
    else if ABytes < MB then
        Result := OneDecimal(ABytes / KB) + ' KB'
    else
        Result := OneDecimal(ABytes / MB) + ' MB';
end;

function UpdateProgressTitle(const AProgress: TUpdateProgress): string;
begin
    case AProgress.Stage of
        usChecking: Result := 'Checking for updates';
        usDownloading: Result := 'Downloading version ' + AProgress.Version;
        usVerifying: Result := 'Checking the download';
        else Result := 'Starting the installer';
    end;
end;

{ How long the rest of a download will take at the rate so far, in words. }
function TimeLeftText(ABytes, ATotal, AElapsedMs: int64): string;
var
    Seconds: int64;
begin
    //  Rounded up: "0 s left" while bytes are still arriving reads as stuck.
    Seconds := ((ATotal - ABytes) * AElapsedMs div ABytes + 999) div 1000;
    if Seconds <= 90 then
        Result := 'about ' + IntToStr(Seconds) + ' s left'
    else
        Result := 'about ' + IntToStr((Seconds + 30) div 60) + ' min left';
end;

function UpdateProgressDetail(const AProgress: TUpdateProgress;
    AElapsedMs: int64): string;
begin
    case AProgress.Stage of
        usChecking:
            Result := 'Asking the update feed for the newest version...';
        usDownloading:
            if AProgress.Bytes <= 0 then
                Result := 'Connecting...'
            else if AProgress.Total <= 0 then
                //  NO LENGTH TO MEASURE AGAINST - a feed from before sizes
                //  were published - so what there is, is how much has come.
                Result := ByteSizeText(AProgress.Bytes) + ' received'
            else
            begin
                Result := ByteSizeText(AProgress.Bytes) + ' of ' +
                    ByteSizeText(AProgress.Total);
                if (AElapsedMs >= EstimateAfterMs) and
                    (AProgress.Bytes < AProgress.Total) then
                    Result := Result + ' - ' + TimeLeftText(AProgress.Bytes,
                        AProgress.Total, AElapsedMs);
            end;
        usVerifying:
            Result := 'Comparing it with the checksum the release publishes...';
        else
            Result := 'The program closes so that the installer can replace it.';
    end;
end;

function UpdateProgressPercent(const AProgress: TUpdateProgress): integer;
begin
    //  Only a download has a measure, and only when its length is known; the
    //  other stages are short or wait on something that says nothing.
    if (AProgress.Stage <> usDownloading) or (AProgress.Total <= 0) then
        Exit(-1);
    Result := AProgress.Bytes * 100 div AProgress.Total;
    //  A server may send more than the feed said; the digest then refuses
    //  the file, and the bar must not run off its end first.
    if Result > 100 then
        Result := 100;
end;

function UpdateProgressCancellable(const AProgress: TUpdateProgress): boolean;
begin
    Result := AProgress.Stage in [usChecking, usDownloading];
end;

end.
