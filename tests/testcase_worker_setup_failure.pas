// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A worker fixture whose SetUp fails leaves no worker running.)

THE DEFECT THIS DEFENDS AGAINST. fpcunit runs SetUp outside the try that
guarantees TearDown, so a SetUp that fails never reaches TearDown. The worker
fixture started fit_server and THEN asserted that it had said it was listening;
when that assertion failed - every time, on Windows, while the readiness channel
was broken there - the server it had started was never stopped. That process held
the output pipe it inherited, so the suite finished and the release job went on
waiting for the pipe to close until it was cancelled half an hour later.

HOW SETUP IS MADE TO FAIL with a live worker: a fixture that gives the worker no
time at all to say it is listening. A process that has only just been started
cannot have loaded, bound a port and announced within a zero budget.
}
unit testcase_worker_setup_failure;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, worker_process_harness;

type
    TWorkerSetupFailureTest = class(TTestCase)
    published
        procedure ASetUpThatFailsStopsTheWorkerItStarted;
    end;

implementation

type
    { The fixture under test, impatient: its SetUp starts a real worker and fails. }
    TImpatientWorkerFixture = class(TWorkerProcessTest)
    protected
        function ReadyBudgetMs: longint; override;
    public
        procedure TrySetUp;
        procedure CleanUp;
        function WorkerLeftRunning: boolean;
    end;

function TImpatientWorkerFixture.ReadyBudgetMs: longint;
begin
    Result := 0;
end;

procedure TImpatientWorkerFixture.TrySetUp;
begin
    SetUp;
end;

procedure TImpatientWorkerFixture.CleanUp;
begin
    TearDown;
end;

function TImpatientWorkerFixture.WorkerLeftRunning: boolean;
begin
    Result := Assigned(FProc) and FProc.Running;
end;

procedure TWorkerSetupFailureTest.ASetUpThatFailsStopsTheWorkerItStarted;
var
    Fixture: TImpatientWorkerFixture;
    Failed: boolean;
begin
    //  Without a server binary SetUp fails BEFORE starting anything, and the
    //  assertion below would pass over a worker that never existed.
    AssertTrue('the server binary exists: ' + WorkerServerPath,
        FileExists(WorkerServerPath));
    Fixture := TImpatientWorkerFixture.Create;
    try
        Failed := False;
        try
            Fixture.TrySetUp;
        except
            on EAssertionFailedError do
                Failed := True;
        end;
        AssertTrue('SetUp failed, as it must for a worker given no time to say ' +
            'it is listening', Failed);
        AssertFalse('the worker that SetUp started is not left running',
            Fixture.WorkerLeftRunning);
    finally
        //  What fpcunit would not do: stop the worker if the assertion above
        //  found it running, so this test leaves nothing behind either.
        Fixture.CleanUp;
        Fixture.Free;
    end;
end;

initialization
    //  Integration: it starts a real fit_server process.
    RegisterTest('integration', TWorkerSetupFailureTest);
end.
