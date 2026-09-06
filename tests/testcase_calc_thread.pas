// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The thread the in-process engine computes on, and the eight methods it
was handed.)

WHAT IT IS. TMainCalcThread runs one task off the main thread and marshals every
progress report back onto it. It is eight assignments, seven one-line forwards and
one try/except - and it measured zero covered lines, because the only thing that
had ever driven it was a real fit.

WHY IT CAN BE TESTED WITHOUT A THREAD RUNNING. TThread.Synchronize called from
the main thread executes the method directly rather than queueing it (FPC's
ThreadQueueAppend takes that branch when the caller is the main thread). So the
forwards can be driven inline, on this thread, with nothing started - which is
also the only way to observe them: what the thread does after Start is by
construction not observable from here without a message loop.

The thread is created suspended and never started. Freeing one is safe: the
destructor sets Terminated before releasing it, and the thread function then
returns without entering Execute.
}
unit testcase_calc_thread;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, main_calc_thread, checks;

type
    TCalcThreadTest = class(TTestCase)
    private
        FThread: TMainCalcThread;
        { Which callbacks arrived, in order. }
        FSeen: TStringList;
        { What the task should do when run. }
        FRaiseInTask: boolean;
        FErrorShown: string;
        FErrorShownCount: longint;

        procedure Task;
        procedure OnShowCurMin;
        procedure OnShowProfile;
        procedure OnDone;
        procedure OnCurveBounds;
        procedure OnBackgroundPoints;
        procedure OnCurvePositions;
        procedure OnAllDone;
        procedure CalcError(const AMessage: string);
        { Hands the thread a complete set of callbacks. }
        procedure WireEverything;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What it refuses to be set up with.
        procedure EveryCallbackIsRequired;
        procedure ACompleteSetIsAccepted;

        //  Where each callback goes. One test per callback, because the failure
        //  is a forward wired to its neighbour and a single test asserting "some
        //  callback arrived" would pass through it.
        procedure ShowCurMinGoesToItsOwnCallback;
        procedure ShowProfileGoesToItsOwnCallback;
        procedure DoneGoesToItsOwnCallback;
        procedure CurveBoundsGoesToItsOwnCallback;
        procedure BackgroundPointsGoesToItsOwnCallback;
        procedure CurvePositionsGoesToItsOwnCallback;
        procedure NoCallbackIsWiredToAnother;

        //  Running the task.
        procedure TheTaskRunsAndTheEndIsReported;
        procedure ATaskThatRaisesStillReportsTheEnd;
        procedure ATaskThatRaisesReachesTheUserWithItsMessage;
        procedure ASuccessfulTaskShowsNoError;
        procedure WithNoErrorHookAFailureIsNotFatal;
    end;

implementation

var
    { The unit's UI hook is a global, so a fixture cannot own it. Saved and
      restored around each test that sets it. }
    SavedOnCalcError: procedure(const AMessage: string) of object;

procedure TCalcThreadTest.SetUp;
begin
    FSeen := TStringList.Create;
    FRaiseInTask := False;
    FErrorShown := '';
    FErrorShownCount := 0;
    SavedOnCalcError := OnCalcError;
    OnCalcError := nil;
    //  Suspended: nothing here wants the thread running, and every method under
    //  test executes inline when called from this thread.
    FThread := TMainCalcThread.Create(True);
end;

procedure TCalcThreadTest.TearDown;
begin
    OnCalcError := SavedOnCalcError;
    FreeAndNil(FThread);
    FreeAndNil(FSeen);
end;

procedure TCalcThreadTest.Task;
begin
    FSeen.Add('task');
    if FRaiseInTask then
        raise Exception.Create('the task went wrong');
end;

procedure TCalcThreadTest.OnShowCurMin;
begin
    FSeen.Add('cur-min');
end;

procedure TCalcThreadTest.OnShowProfile;
begin
    FSeen.Add('profile');
end;

procedure TCalcThreadTest.OnDone;
begin
    FSeen.Add('done');
end;

procedure TCalcThreadTest.OnCurveBounds;
begin
    FSeen.Add('curve-bounds');
end;

procedure TCalcThreadTest.OnBackgroundPoints;
begin
    FSeen.Add('background-points');
end;

procedure TCalcThreadTest.OnCurvePositions;
begin
    FSeen.Add('curve-positions');
end;

procedure TCalcThreadTest.OnAllDone;
begin
    FSeen.Add('all-done');
end;

procedure TCalcThreadTest.CalcError(const AMessage: string);
begin
    FErrorShown := AMessage;
    Inc(FErrorShownCount);
end;

procedure TCalcThreadTest.WireEverything;
begin
    FThread.SetSyncMethods(Task, OnShowCurMin, OnShowProfile, OnDone,
        OnCurveBounds, OnBackgroundPoints, OnCurvePositions, OnAllDone);
end;

{ ---- being set up ---------------------------------------------------------- }

procedure TCalcThreadTest.EveryCallbackIsRequired;
var
    Refusals, i: longint;

    { Wires everything except the argument at position AOmit (1-based). }
    procedure WireWithout(AOmit: longint);
    var
        M: array[1..8] of TThreadMethod;
    begin
        M[1] := Task;
        M[2] := OnShowCurMin;
        M[3] := OnShowProfile;
        M[4] := OnDone;
        M[5] := OnCurveBounds;
        M[6] := OnBackgroundPoints;
        M[7] := OnCurvePositions;
        M[8] := OnAllDone;
        M[AOmit] := nil;
        FThread.SetSyncMethods(M[1], M[2], M[3], M[4], M[5], M[6], M[7], M[8]);
    end;

begin
    //  ALL EIGHT, each on its own. A nil callback is not detected when it is
    //  stored - it is detected when the engine reaches that point in a fit and
    //  synchronizes nothing, which is a fit that appears to hang.
    Refusals := 0;
    for i := 1 to 8 do
        try
            WireWithout(i);
            Fail(Format('argument %d was accepted as nil', [i]));
        except
            on E: EInternalCheckFailed do
                Inc(Refusals);
        end;
    AssertEquals('all eight are required', 8, Refusals);
end;

procedure TCalcThreadTest.ACompleteSetIsAccepted;
begin
    WireEverything;
    AssertEquals('nothing ran yet', 0, FSeen.Count);
end;

{ ---- where each callback goes ---------------------------------------------- }

procedure TCalcThreadTest.ShowCurMinGoesToItsOwnCallback;
begin
    WireEverything;
    FThread.ShowCurMin;
    AssertEquals('one callback', 1, FSeen.Count);
    AssertEquals('and it is the right one', 'cur-min', FSeen[0]);
end;

procedure TCalcThreadTest.ShowProfileGoesToItsOwnCallback;
begin
    WireEverything;
    FThread.ShowProfile;
    AssertEquals('profile', 'profile', FSeen.CommaText);
end;

procedure TCalcThreadTest.DoneGoesToItsOwnCallback;
begin
    WireEverything;
    FThread.Done;
    AssertEquals('done', 'done', FSeen.CommaText);
end;

procedure TCalcThreadTest.CurveBoundsGoesToItsOwnCallback;
begin
    WireEverything;
    FThread.ComputeCurveBoundsDone;
    AssertEquals('curve-bounds', 'curve-bounds', FSeen.CommaText);
end;

procedure TCalcThreadTest.BackgroundPointsGoesToItsOwnCallback;
begin
    //  THE ONE THAT WAS BROKEN. SetSyncMethods validated this argument and then
    //  never stored it, so this method synchronized a nil and the client was
    //  never told the background had been computed. See findings.md.
    WireEverything;
    FThread.ComputeBackgroundPointsDone;
    AssertEquals('background-points', 'background-points', FSeen.CommaText);
end;

procedure TCalcThreadTest.CurvePositionsGoesToItsOwnCallback;
begin
    WireEverything;
    FThread.ComputeCurvePositionsDone;
    AssertEquals('curve-positions', 'curve-positions', FSeen.CommaText);
end;

procedure TCalcThreadTest.NoCallbackIsWiredToAnother;
begin
    //  Six calls, six distinct arrivals, in the order made. Two forwards wired to
    //  one callback is invisible in a per-method test if the two methods happen to
    //  be tested against the same expectation, so the whole sequence is asserted
    //  once as well.
    WireEverything;
    FThread.ShowCurMin;
    FThread.ShowProfile;
    FThread.ComputeCurveBoundsDone;
    FThread.ComputeBackgroundPointsDone;
    FThread.ComputeCurvePositionsDone;
    FThread.Done;
    AssertEquals('each to its own, in order',
        'cur-min,profile,curve-bounds,background-points,curve-positions,done',
        FSeen.CommaText);
end;

{ ---- running the task ------------------------------------------------------ }

procedure TCalcThreadTest.TheTaskRunsAndTheEndIsReported;
begin
    //  Execute is called here directly, on this thread. What it does is run the
    //  task and then report the end - and the report is what re-enables the
    //  interface, so a path that skips it leaves the application disabled.
    WireEverything;
    FThread.Execute;
    AssertEquals('the task then the end', 'task,all-done', FSeen.CommaText);
end;

procedure TCalcThreadTest.ATaskThatRaisesStillReportsTheEnd;
begin
    //  THE IMPORTANT ONE. A failed calculation that does not report the end is
    //  the "Please wait" that never goes away - which is the defect the error
    //  handling here was written for.
    WireEverything;
    FRaiseInTask := True;
    FThread.Execute;
    AssertEquals('the end was still reported', 'task,all-done',
        FSeen.CommaText);
end;

procedure TCalcThreadTest.ATaskThatRaisesReachesTheUserWithItsMessage;
begin
    //  The message, not a generic failure: it is the only thing that says what
    //  went wrong, and before this hook existed it was written to the log alone.
    WireEverything;
    OnCalcError := CalcError;
    FRaiseInTask := True;
    FThread.Execute;
    AssertEquals('shown once', 1, FErrorShownCount);
    AssertEquals('with the message the task raised', 'the task went wrong',
        FErrorShown);
end;

procedure TCalcThreadTest.ASuccessfulTaskShowsNoError;
begin
    //  A stale error message shown after a calculation that worked is worse than
    //  none: the user is told a fit failed when it did not.
    WireEverything;
    OnCalcError := CalcError;
    FThread.Execute;
    AssertEquals('nothing was shown', 0, FErrorShownCount);
end;

procedure TCalcThreadTest.WithNoErrorHookAFailureIsNotFatal;
begin
    //  The hook is optional - the compute server sets none, having no user to
    //  show anything to - so a failure there must be logged and let go, not
    //  dereferenced.
    WireEverything;
    OnCalcError := nil;
    FRaiseInTask := True;
    FThread.Execute;
    AssertEquals('the end was reported anyway', 'task,all-done',
        FSeen.CommaText);
end;

initialization
    //  A unit test: the thread is never started, and Synchronize called from the
    //  main thread runs its method inline.
    RegisterTest('unit', TCalcThreadTest);
end.
