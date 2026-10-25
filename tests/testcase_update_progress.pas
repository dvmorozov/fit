// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the update's progress window says while the user waits.)

Every word, number and bar position the window shows is decided in
update_progress over plain values, so each is pinned here; the window only
reads the box on a timer and writes its controls.
}
unit testcase_update_progress;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, update_check, update_progress;

type
    TUpdateProgressTest = class(TTestCase)
    published
        procedure SizesAreSaidInTheUnitAPersonReads;
        procedure EachStageSaysWhatIsHappening;
        procedure ADownloadSaysHowFarAgainstTheWhole;
        procedure WithoutALengthItSaysHowMuchHasArrived;
        procedure TimeLeftIsEstimatedOnceThereIsARateToGoOn;
        procedure TheBarMeasuresOnlyWhatCanBeMeasured;
        procedure CancelIsOfferedOnlyWhileItCanChangeSomething;
        procedure TheBoxHandsOverTheLatestAndHowLongItsStageHasRun;
    end;

implementation

var
    TickValue: QWord;

function TestTicks: QWord;
begin
    Result := TickValue;
end;

function P(AStage: TUpdateStage; ABytes: int64 = 0; ATotal: int64 = 0): TUpdateProgress;
begin
    Result := Default(TUpdateProgress);
    Result.Stage := AStage;
    Result.Version := '1.3.0.2100';
    Result.Bytes := ABytes;
    Result.Total := ATotal;
end;

const
    MB = int64(1024) * 1024;

procedure TUpdateProgressTest.SizesAreSaidInTheUnitAPersonReads;
begin
    AssertEquals('1 byte', ByteSizeText(1));
    AssertEquals('900 bytes', ByteSizeText(900));
    AssertEquals('1.5 KB', ByteSizeText(1536));
    //  The installer that could not be downloaded (2026-10-21).
    AssertEquals('65.5 MB', ByteSizeText(68646041));
    AssertEquals('whatever the locale''s separator', '2.0 MB', ByteSizeText(2 * MB));
end;

procedure TUpdateProgressTest.EachStageSaysWhatIsHappening;
begin
    AssertEquals('Checking for updates', UpdateProgressTitle(P(usChecking)));
    AssertEquals('Downloading version 1.3.0.2100',
        UpdateProgressTitle(P(usDownloading)));
    AssertEquals('Checking the download', UpdateProgressTitle(P(usVerifying)));
    AssertEquals('Starting the installer', UpdateProgressTitle(P(usInstalling)));
    AssertTrue('every stage has a line under it',
        (UpdateProgressDetail(P(usChecking), 0) <> '') and
        (UpdateProgressDetail(P(usVerifying), 0) <> '') and
        (UpdateProgressDetail(P(usInstalling), 0) <> ''));
end;

procedure TUpdateProgressTest.ADownloadSaysHowFarAgainstTheWhole;
begin
    AssertEquals('nothing yet', 'Connecting...',
        UpdateProgressDetail(P(usDownloading, 0, 48 * MB), 500));
    AssertEquals('12.0 MB of 48.0 MB',
        UpdateProgressDetail(P(usDownloading, 12 * MB, 48 * MB), 500));
end;

procedure TUpdateProgressTest.WithoutALengthItSaysHowMuchHasArrived;
begin
    AssertEquals('12.0 MB received',
        UpdateProgressDetail(P(usDownloading, 12 * MB, 0), 4000));
end;

procedure TUpdateProgressTest.TimeLeftIsEstimatedOnceThereIsARateToGoOn;
begin
    //  A quarter in four seconds: twelve more.
    AssertEquals('12.0 MB of 48.0 MB - about 12 s left',
        UpdateProgressDetail(P(usDownloading, 12 * MB, 48 * MB), 4000));
    //  One in a hundred in four seconds: six and a half minutes.
    AssertEquals('1.0 MB of 100.0 MB - about 7 min left',
        UpdateProgressDetail(P(usDownloading, MB, 100 * MB), 4000));
    AssertEquals('not before two seconds: a rate from the first chunk lies',
        '12.0 MB of 48.0 MB',
        UpdateProgressDetail(P(usDownloading, 12 * MB, 48 * MB), 1999));
    AssertEquals('done is not "0 s left"', '48.0 MB of 48.0 MB',
        UpdateProgressDetail(P(usDownloading, 48 * MB, 48 * MB), 4000));
end;

procedure TUpdateProgressTest.TheBarMeasuresOnlyWhatCanBeMeasured;
begin
    AssertEquals(25, UpdateProgressPercent(P(usDownloading, 12 * MB, 48 * MB)));
    AssertEquals('never past full', 100,
        UpdateProgressPercent(P(usDownloading, 50 * MB, 48 * MB)));
    AssertEquals('no length: it moves by itself', -1,
        UpdateProgressPercent(P(usDownloading, 12 * MB, 0)));
    AssertEquals(-1, UpdateProgressPercent(P(usChecking)));
    AssertEquals(-1, UpdateProgressPercent(P(usVerifying, 48 * MB, 48 * MB)));
    AssertEquals(-1, UpdateProgressPercent(P(usInstalling)));
end;

procedure TUpdateProgressTest.CancelIsOfferedOnlyWhileItCanChangeSomething;
begin
    AssertTrue(UpdateProgressCancellable(P(usChecking)));
    AssertTrue(UpdateProgressCancellable(P(usDownloading)));
    AssertFalse('the download is over', UpdateProgressCancellable(P(usVerifying)));
    AssertFalse('the installer has it', UpdateProgressCancellable(P(usInstalling)));
end;

procedure TUpdateProgressTest.TheBoxHandsOverTheLatestAndHowLongItsStageHasRun;
var
    Box: TUpdateProgressBox;
    Got: TUpdateProgress;
    Elapsed: int64;
begin
    Box := TUpdateProgressBox.Create;
    try
        AssertTrue('the system''s clock by default', Box.Ticks = @GetTickCount64);
        Box.Ticks := @TestTicks;
        TickValue := 1000;
        AssertFalse('nothing posted yet', Box.Read(Got, Elapsed));
        Box.Post(P(usDownloading, 0, 48 * MB));
        TickValue := 3000;
        Box.Post(P(usDownloading, 12 * MB, 48 * MB));
        TickValue := 5000;
        AssertTrue(Box.Read(Got, Elapsed));
        AssertEquals('the latest', 12 * MB, Got.Bytes);
        AssertEquals('the stage''s clock runs on', 4000, Elapsed);
        Box.Post(P(usVerifying, 48 * MB, 48 * MB));
        TickValue := 5500;
        AssertTrue(Box.Read(Got, Elapsed));
        AssertTrue(Got.Stage = usVerifying);
        AssertEquals('and starts again with a new stage', 500, Elapsed);
    finally
        Box.Free;
    end;
end;

initialization
    RegisterTest('unit', TUpdateProgressTest);
end.
