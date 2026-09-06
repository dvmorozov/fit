// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a long server operation is run off the main thread, and what
happens to it when it fails.)

WHY THIS EXISTS. TServerCallThread is how EVERY long operation reaches the
compute server - a fit, a background computation, a bounds proposal - and until
this suite no test had ever entered its constructor. The coverage gap scan says
so in one line, in the only category it is certain about: a method no test
enters.

WHAT IT WOULD HAVE CAUGHT. The comment inside Finished records a defect this
code has already had: an exception escaping the completion handler is re-raised
in the WORKER thread, after the except block that would have logged it has
already run, and dies there with the thread - so the window is left half
refreshed and the log says nothing at all. That is what "the curves stopped
being drawn after every fit" looked like from the outside, and the fix is only
as good as something asserting it.

A REAL THREAD, and the pumping that goes with it. The completion runs through
Synchronize, which parks the worker until the MAIN thread executes the queued
method - in an application the widget set does that, and here nothing does
unless a test calls CheckSynchronize. So each test pumps until the work reports
itself finished or a deadline passes, and the deadline is what turns "it never
finished" into a failing assertion rather than a suite that hangs.

It is registered `unit` by the suite's own rule: a thread is inside this
process, and nothing here touches a socket, a file or an optimiser.
}
unit testcase_client_async;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, main_calc_thread;

type
    { RunAsync is PROTECTED, and virtual so that a test can run the operation in
      place - which is how every other client test avoids the thread. This one
      is about the thread, so it reaches the real one through a forwarder rather
      than overriding it away. }
    TThreadedClient = class(TFitClient)
    public
        procedure CallRunAsync(AOp: TServerOp; ADone: TThreadMethod);
    end;

    TClientAsyncTest = class(TTestCase)
    private
        FClient: TThreadedClient;
        { What the operation and the completion handler recorded. }
        FOpRan: boolean;
        FDoneRan: boolean;
        { The thread the operation was run on, so "off the main thread" is
          asserted rather than assumed. }
        FOpThread: TThreadID;
        FDoneThread: TThreadID;

        { The operation the thread runs. }
        function QuietOperation: string;
        function FailingOperation: string;
        { The completion handler. }
        procedure Completed;
        procedure CompletedBadly;
        { Pumps Synchronize until the completion handler has run, or gives up. }
        function WaitForCompletion: boolean;
        { What OnCalcError was told. A method, because that hook is one. }
        procedure RememberError(const AMessage: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheOperationRunsAndTheCompletionIsReported;
        procedure TheOperationRunsOffTheMainThread;
        procedure AndTheCompletionRunsOnIt;
        procedure AnOperationThatRaisesIsReportedRatherThanLost;
        procedure AndTheWindowIsStillToldTheWorkIsOver;
        procedure AFailureInTheCompletionIsReportedToo;
    end;

implementation

var
    { What the hook was told. Unit-level because OnCalcError is process-global:
      a fixture field would be read after the fixture is gone if a thread ever
      outlived a test, and that is the one failure a test must not have. }
    ReportedError: string;

procedure TThreadedClient.CallRunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    RunAsync(AOp, ADone);
end;

procedure TClientAsyncTest.RememberError(const AMessage: string);
begin
    ReportedError := AMessage;
end;

procedure TClientAsyncTest.SetUp;
begin
    FClient := TThreadedClient.Create;
    FOpRan := False;
    FDoneRan := False;
    FOpThread := 0;
    FDoneThread := 0;
    ReportedError := '';
    main_calc_thread.OnCalcError := @Self.RememberError;
end;

procedure TClientAsyncTest.TearDown;
begin
    //  UNHOOKED FIRST. The hook is process-global, and a test that left it
    //  pointing here would have every later suite report into a freed fixture.
    main_calc_thread.OnCalcError := nil;
    FreeAndNil(FClient);
end;

function TClientAsyncTest.QuietOperation: string;
begin
    FOpRan := True;
    FOpThread := GetCurrentThreadId;
    Result := '';
end;

function TClientAsyncTest.FailingOperation: string;
begin
    FOpRan := True;
    FOpThread := GetCurrentThreadId;
    Result := '';
    raise Exception.Create('the compute server said no');
end;

procedure TClientAsyncTest.Completed;
begin
    FDoneThread := GetCurrentThreadId;
    FDoneRan := True;
end;

procedure TClientAsyncTest.CompletedBadly;
begin
    FDoneThread := GetCurrentThreadId;
    FDoneRan := True;
    raise Exception.Create('the window could not be refreshed');
end;

function TClientAsyncTest.WaitForCompletion: boolean;
var
    Deadline: TDateTime;
begin
    //  Five seconds is far more than this needs and far less than a suite can
    //  afford to hang for. Reaching it is a failing assertion in the caller.
    Deadline := Now + 5 / (24 * 60 * 60);
    while (not FDoneRan) and (Now < Deadline) do
        //  The main thread's half of Synchronize. Without it the worker parks
        //  for ever holding its queued method, which is a hang rather than a
        //  failure - see the unit header.
        CheckSynchronize(10);
    Result := FDoneRan;
end;

procedure TClientAsyncTest.TheOperationRunsAndTheCompletionIsReported;
begin
    FClient.CallRunAsync(@QuietOperation, @Completed);
    AssertTrue('the completion was reported', WaitForCompletion);
    AssertTrue('and the operation itself ran', FOpRan);
    AssertEquals('with nothing to report', '', ReportedError);
end;

procedure TClientAsyncTest.TheOperationRunsOffTheMainThread;
begin
    //  THE WHOLE POINT of running it this way: a blocking call to the compute
    //  server must not be made on the thread that draws the window.
    FClient.CallRunAsync(@QuietOperation, @Completed);
    AssertTrue('finished', WaitForCompletion);
    AssertTrue('the operation was not run on the main thread',
        FOpThread <> MainThreadID);
end;

procedure TClientAsyncTest.AndTheCompletionRunsOnIt;
begin
    //  ...and the completion must be, because it touches the window. That is
    //  what Synchronize is for, and it is the half a test can most easily
    //  believe without checking.
    FClient.CallRunAsync(@QuietOperation, @Completed);
    AssertTrue('finished', WaitForCompletion);
    AssertEquals('the completion ran on the main thread', MainThreadID,
        FDoneThread);
end;

procedure TClientAsyncTest.AnOperationThatRaisesIsReportedRatherThanLost;
begin
    //  A server that refuses, or a connection that drops, must reach the user.
    //  An exception on a worker thread reaches nobody by default.
    FClient.CallRunAsync(@FailingOperation, @Completed);
    AssertTrue('finished', WaitForCompletion);
    AssertTrue('the failure was reported: ' + ReportedError,
        Pos('the compute server said no', ReportedError) > 0);
end;

procedure TClientAsyncTest.AndTheWindowIsStillToldTheWorkIsOver;
begin
    //  THE OTHER HALF, and the one that matters more: a failed operation must
    //  still run the completion, or the window stays busy for ever with no way
    //  back except restarting the application.
    FClient.CallRunAsync(@FailingOperation, @Completed);
    AssertTrue('the completion ran anyway', WaitForCompletion);
end;

procedure TClientAsyncTest.AFailureInTheCompletionIsReportedToo;
begin
    //  THE DEFECT THE CODE'S OWN COMMENT RECORDS. An exception escaping the
    //  completion handler is re-raised on the WORKER thread, after the handler
    //  that would have logged it has already run, and dies with the thread -
    //  leaving a half-refreshed window and an empty log. It is caught, and the
    //  user is told.
    FClient.CallRunAsync(@QuietOperation, @CompletedBadly);
    AssertTrue('the completion ran', WaitForCompletion);
    //  Pumped once more: the report is made inside the same Synchronize call
    //  that set the flag above, so it is already there - this asserts the
    //  message rather than the timing.
    AssertTrue('and its failure was reported: ' + ReportedError,
        Pos('the window could not be refreshed', ReportedError) > 0);
end;

initialization
    //  A thread, and nothing outside this process: a unit test by the rule in
    //  testcase_suite_split.
    RegisterTest('unit', TClientAsyncTest);
end.
