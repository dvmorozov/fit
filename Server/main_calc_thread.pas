// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of thread class executing server methods.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit main_calc_thread;

interface

uses
    Classes, log, SysUtils, checks;

type
    { Must contain counterparts of IClientCallback methods without parameters
      for synchronous call from UI thread. }
    //  TODO: catch exceptions and store the message in the thread object, to be
    //  read afterwards.
    TMainCalcThread = class(TThread)
    private
        FTask:    TThreadMethod;
        { These methods are synchronized with UI thread. }
        FShowCurMin: TThreadMethod;
        FShowProfile: TThreadMethod;
        FDone:    TThreadMethod;
        FComputeCurveBoundsDone: TThreadMethod;
        FComputeBackgroundPointsDone: TThreadMethod;
        FComputeCurvePositionsDone: TThreadMethod;
        FAllDone: TThreadMethod;

    public
        procedure Execute; override;

        procedure ShowCurMin;
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        procedure SetSyncMethods(ATask, AShowCurMin, AShowProfile,
            ADone, AComputeCurveBoundsDone, AComputeBackgroundPointsDone,
            AComputeCurvePositionsDone, AAllDone: TThreadMethod);
    private
        { Message of an exception raised by the task, shown to the user after the
          task finishes (empty when the task succeeded). }
        FErrorMessage: string;
        procedure ShowError;
    end;

var
    { Optional UI hook: displays a fatal calculation error to the user. Set by the
      desktop app so this (LCL-free) unit stays decoupled from the UI; the worker
      thread invokes it synchronized on the main thread. }
    OnCalcError: procedure(const AMessage: string) of object;

implementation

//  `uses app` REMOVED, not tidied. app.pas uses Forms and its
//  initialization constructs a desktop client application object plus an
//  HTTP client aimed at the default server URL. This unit referenced
//  neither identifier app.pas exports, so the clause bought nothing and
//  cost the LCL - and, in the compute server, a client of itself built on
//  every start-up. See docs/contributing/findings.md.

procedure TMainCalcThread.SetSyncMethods(
    ATask, AShowCurMin, AShowProfile, ADone, AComputeCurveBoundsDone,
    AComputeBackgroundPointsDone, AComputeCurvePositionsDone, AAllDone: TThreadMethod);
begin
    CheckThat(Assigned(ATask), 'ATask is missing when it is required');
    CheckThat(Assigned(AShowCurMin), 'AShowCurMin is missing when it is required');
    CheckThat(Assigned(AShowProfile), 'AShowProfile is missing when it is required');
    CheckThat(Assigned(ADone), 'ADone is missing when it is required');
    CheckThat(Assigned(AComputeCurveBoundsDone), 'AComputeCurveBoundsDone is missing when it is required');
    CheckThat(Assigned(AComputeBackgroundPointsDone), 'AComputeBackgroundPointsDone is missing when it is required');
    CheckThat(Assigned(AComputeCurvePositionsDone), 'AComputeCurvePositionsDone is missing when it is required');
    CheckThat(Assigned(AAllDone), 'AAllDone is missing when it is required');

    FTask    := ATask;
    FShowCurMin := AShowCurMin;
    FShowProfile := AShowProfile;
    FDone    := ADone;
    FComputeCurveBoundsDone := AComputeCurveBoundsDone;
    //  THIS LINE WAS MISSING. The argument was validated as required and then
    //  dropped, so ComputeBackgroundPointsDone synchronized a nil method and the
    //  client was never told the background had been computed. See
    //  docs/contributing/findings.md.
    FComputeBackgroundPointsDone := AComputeBackgroundPointsDone;
    FComputeCurvePositionsDone := AComputeCurvePositionsDone;
    FAllDone := AAllDone;
end;

procedure TMainCalcThread.Execute;
begin
    //  THIS HANDLING STAYS AS IT IS because it runs on another thread.
    FErrorMessage := '';
    try
        FTask;
    except
        on E: Exception do
        begin
            FErrorMessage := E.Message;
            WriteLog(E.Message, Fatal);
        end;
    end;
    Synchronize(FAllDone);
    //  Surface a fatal error to the user (previously it was only logged, so the
    //  calculation appeared to hang on "Please wait").
    if (FErrorMessage <> '') and Assigned(OnCalcError) then
        Synchronize(ShowError);
end;

procedure TMainCalcThread.ShowError;
begin
    OnCalcError(FErrorMessage);
end;

procedure TMainCalcThread.ShowCurMin;
begin
    Synchronize(FShowCurMin);
end;

procedure TMainCalcThread.ShowProfile;
begin
    Synchronize(FShowProfile);
end;

procedure TMainCalcThread.Done;
begin
    Synchronize(FDone);
end;

procedure TMainCalcThread.ComputeCurveBoundsDone;
begin
    Synchronize(FComputeCurveBoundsDone);
end;

procedure TMainCalcThread.ComputeBackgroundPointsDone;
begin
    Synchronize(FComputeBackgroundPointsDone);
end;

procedure TMainCalcThread.ComputeCurvePositionsDone;
begin
    Synchronize(FComputeCurvePositionsDone);
end;

end.
