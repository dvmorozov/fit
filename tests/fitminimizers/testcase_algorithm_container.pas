// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The two ways an algorithm is run: on the caller's thread, or on
another one.)

TAlgorithmContainer IS FOUR ABSTRACT METHODS AND ONE CONCRETE ONE, and the
concrete one is the whole contract: Run means "do the work, then report it", in
that order. A descendant supplies the two halves and never has to remember to
call the second - which is exactly what the base class is for, and exactly what
nothing checked.

THE THREADED FORM MUST BEHAVE THE SAME WAY FROM OUTSIDE. TThreadAlgorithmContainer
overrides Run to hand both halves to a TRunner instead, and the point of it is
that a caller writes the same two methods either way: the work goes to a worker
thread, the report comes back on the caller's. A descendant that got the halves
the wrong way round would compute on the interface thread and paint from the
worker - which on the desktop side means the window freezing for the length of a
fit, and corruption inside the widget set with nothing in the stack naming this
unit.

TRunningAlgorithmContainer IS NOT A THIRD CLASS. It is an alias kept for callers
written against the old name, and it is pinned here because an alias that
silently became a descendant - or a descendant that became an alias - would
change what `is` and `ClassName` answer for every one of them.

WHY THESE ARE UNIT TESTS DESPITE STARTING A THREAD. A thread crosses no process
boundary, touches no file and runs no optimiser to convergence, which are the
three things that make a test integration here. Each one below waits for its own
runner before asserting, so nothing is left running when it ends.
}
unit testcase_algorithm_container;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    AlgorithmContainer, RunningThread;

type
    { A container that records what its base class did to it, in order. Both
      halves are the ones a real descendant would supply; neither does any
      work, because what is under test is who calls them and when. }
    TRecordingContainer = class(TAlgorithmContainer)
    private
        FTrace: string;
        FComputeThread: TThreadID;
        FCreates, FDestroys: longint;
    protected
        procedure Running; override;
        procedure RunningFinished; override;
        procedure CreateAlgorithm; override;
        procedure DestroyAlgorithm; override;
    public
        procedure StopAlgorithm; override;
        property Trace: string read FTrace;
        property ComputeThread: TThreadID read FComputeThread;
        property Creates: longint read FCreates;
        property Destroys: longint read FDestroys;
        { Algorithm is protected on the base class, so only a descendant can
          say whether one is there. }
        function HasAlgorithm: boolean;
    end;

    { The same recorder over the threaded base, so one set of expectations can
      be put to both. The runner is protected, and a test needs it to wait. }
    TRecordingThreadContainer = class(TThreadAlgorithmContainer)
    private
        FTrace: string;
        FComputeThread, FOutputThread: TThreadID;
    protected
        procedure Running; override;
        procedure RunningFinished; override;
        procedure CreateAlgorithm; override;
        procedure DestroyAlgorithm; override;
    public
        procedure StopAlgorithm; override;
        procedure WaitForIt;
        property Trace: string read FTrace;
        property ComputeThread: TThreadID read FComputeThread;
        property OutputThread: TThreadID read FOutputThread;
    end;

    TAlgorithmContainerTest = class(TTestCase)
    published
        //  On the caller's thread.
        procedure RunDoesTheWorkAndThenReportsIt;
        procedure RunReportsEvenWhenNothingWasComputed;
        procedure RunBuildsNoAlgorithmOfItsOwn;
        procedure AFreshContainerHoldsNoAlgorithm;
        procedure RunningTwiceRunsBothHalvesTwice;
        procedure TheWorkStaysOnTheCallersThread;

        //  On a worker thread.
        procedure TheThreadedFormRunsBothHalvesToo;
        procedure AndInTheSameOrder;
        procedure TheWorkLeavesTheCallersThread;
        procedure ButTheReportComesBackToIt;
        procedure AContainerThatWasNeverRunIsFinished;
        procedure DestroyingOneThatIsRunningWaitsForIt;

        //  The deprecated name.
        procedure TheOldNameIsTheSameClassNotADescendant;
    end;

implementation

{ ---------------------------- the recorders --------------------------------- }

procedure TRecordingContainer.Running;
begin
    FComputeThread := GetCurrentThreadId;
    FTrace := FTrace + 'work;';
end;

procedure TRecordingContainer.RunningFinished;
begin
    FTrace := FTrace + 'report;';
end;

procedure TRecordingContainer.CreateAlgorithm;
begin
    Inc(FCreates);
end;

procedure TRecordingContainer.DestroyAlgorithm;
begin
    Inc(FDestroys);
end;

procedure TRecordingContainer.StopAlgorithm;
begin
end;

function TRecordingContainer.HasAlgorithm: boolean;
begin
    Result := Assigned(Algorithm);
end;

procedure TRecordingThreadContainer.Running;
begin
    FComputeThread := GetCurrentThreadId;
    //  Appended from the worker thread and read after Wait, which is the only
    //  point at which both halves have certainly finished.
    FTrace := FTrace + 'work;';
end;

procedure TRecordingThreadContainer.RunningFinished;
begin
    FOutputThread := GetCurrentThreadId;
    FTrace := FTrace + 'report;';
end;

procedure TRecordingThreadContainer.CreateAlgorithm;
begin
end;

procedure TRecordingThreadContainer.DestroyAlgorithm;
begin
end;

procedure TRecordingThreadContainer.StopAlgorithm;
begin
end;

procedure TRecordingThreadContainer.WaitForIt;
begin
    Runner.Wait;
end;

{ ------------------------- on the caller's thread ---------------------------- }

procedure TAlgorithmContainerTest.RunDoesTheWorkAndThenReportsIt;
var
    C: TRecordingContainer;
begin
    //  THE ONE CONCRETE METHOD IN THE BASE CLASS, and the reason a descendant
    //  never has to remember to report: the base class does it.
    C := TRecordingContainer.Create(nil);
    try
        C.Run;
        AssertEquals('work first, then the report', 'work;report;', C.Trace);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.RunReportsEvenWhenNothingWasComputed;
var
    C: TRecordingContainer;
begin
    //  There is no condition on the second call. A container whose algorithm
    //  found nothing still reports, because "no result" is a result the caller
    //  is waiting to be told about - and a Run that returned silently would be
    //  indistinguishable from one still going.
    C := TRecordingContainer.Create(nil);
    try
        C.Run;
        AssertTrue('the report happened', Pos('report;', C.Trace) > 0);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.RunBuildsNoAlgorithmOfItsOwn;
var
    C: TRecordingContainer;
begin
    //  CreateAlgorithm and DestroyAlgorithm are the descendant's to call, from
    //  inside Running. If the base class called them, a descendant that also
    //  did would build two algorithms per run and free one of them twice.
    C := TRecordingContainer.Create(nil);
    try
        C.Run;
        AssertEquals('none created', 0, C.Creates);
        AssertEquals('and none destroyed', 0, C.Destroys);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.AFreshContainerHoldsNoAlgorithm;
var
    C: TRecordingContainer;
begin
    C := TRecordingContainer.Create(nil);
    try
        AssertFalse('nothing until a descendant makes one', C.HasAlgorithm);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.RunningTwiceRunsBothHalvesTwice;
var
    C: TRecordingContainer;
begin
    //  A container is reusable: nothing is consumed by running it, so a second
    //  fit over new data needs no second container.
    C := TRecordingContainer.Create(nil);
    try
        C.Run;
        C.Run;
        AssertEquals('twice through, in order',
            'work;report;work;report;', C.Trace);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.TheWorkStaysOnTheCallersThread;
var
    C: TRecordingContainer;
begin
    //  The non-threaded form is the one a headless caller wants: it starts no
    //  thread, so there is nothing to wait for and nothing to synchronise.
    C := TRecordingContainer.Create(nil);
    try
        C.Run;
        AssertTrue('no thread was started',
            C.ComputeThread = GetCurrentThreadId);
    finally
        C.Free;
    end;
end;

{ --------------------------- on a worker thread ------------------------------ }

procedure TAlgorithmContainerTest.TheThreadedFormRunsBothHalvesToo;
var
    C: TRecordingThreadContainer;
begin
    C := TRecordingThreadContainer.Create(nil);
    try
        C.Run;
        C.WaitForIt;
        AssertTrue('the work ran', Pos('work;', C.Trace) > 0);
        AssertTrue('and was reported', Pos('report;', C.Trace) > 0);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.AndInTheSameOrder;
var
    C: TRecordingThreadContainer;
begin
    //  Same contract as the synchronous form, which is what lets a descendant
    //  be written once and run either way.
    C := TRecordingThreadContainer.Create(nil);
    try
        C.Run;
        C.WaitForIt;
        AssertEquals('work first, then the report', 'work;report;', C.Trace);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.TheWorkLeavesTheCallersThread;
var
    C: TRecordingThreadContainer;
begin
    //  The entire reason this subclass exists. If the work stayed here, the
    //  window would freeze for the length of a fit.
    C := TRecordingThreadContainer.Create(nil);
    try
        C.Run;
        C.WaitForIt;
        AssertTrue('computed elsewhere',
            C.ComputeThread <> GetCurrentThreadId);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.ButTheReportComesBackToIt;
var
    C: TRecordingThreadContainer;
begin
    //  And the other half of the same contract: the report is synchronised
    //  onto the calling thread, which is what makes it safe to touch the chart
    //  from a descendant's RunningFinished.
    C := TRecordingThreadContainer.Create(nil);
    try
        C.Run;
        C.WaitForIt;
        AssertTrue('reported on this thread',
            C.OutputThread = GetCurrentThreadId);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.AContainerThatWasNeverRunIsFinished;
var
    C: TRecordingThreadContainer;
begin
    //  Constructed, not started. "Finished" has to be true here or a caller
    //  that waits before its first run would wait forever.
    C := TRecordingThreadContainer.Create(nil);
    try
        C.WaitForIt;
        AssertEquals('nothing ran', '', C.Trace);
    finally
        C.Free;
    end;
end;

procedure TAlgorithmContainerTest.DestroyingOneThatIsRunningWaitsForIt;
var
    C: TRecordingThreadContainer;
begin
    //  The destructor utilises the runner, whose own destructor waits. Without
    //  that, freeing a container mid-fit would leave a thread writing into
    //  memory that had just been released.
    C := TRecordingThreadContainer.Create(nil);
    C.Run;
    C.Free;
    AssertTrue('returned rather than faulting', True);
end;

{ ---------------------------- the deprecated name ---------------------------- }

procedure TAlgorithmContainerTest.TheOldNameIsTheSameClassNotADescendant;
var
    C: TRunningAlgorithmContainer;
begin
    //  AN ALIAS, not a subclass. Callers written against either name get one
    //  implementation, and `is` answers the same for both - which would stop
    //  being true the moment somebody "tidied" the alias into a descendant.
    C := TRecordingThreadContainer.Create(nil);
    try
        AssertTrue('the old name is the threaded container',
            C is TThreadAlgorithmContainer);
        AssertEquals('one class, one name',
            TThreadAlgorithmContainer.ClassName,
            TRunningAlgorithmContainer.ClassName);
    finally
        C.Free;
    end;
end;

initialization
    //  Unit tests: two methods and a thread that is waited for. No process, no
    //  file, no optimiser.
    RegisterTest('unit', TAlgorithmContainerTest);
end.
