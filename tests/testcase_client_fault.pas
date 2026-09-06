// SPDX-License-Identifier: GPL-3.0-or-later
{ What the desktop client does with an exception that reached the top level.

  One decision decides it: is the exception a failed OPERATION, which the client
  reports and survives, or a memory FAULT, after which nothing the process does
  can be trusted - including reporting it. The second case ends the process,
  because the client that tried to report one through the widget set that raised
  it froze holding the X pointer grab (see client_log.EndProcessAfterFault).

  Expressed over exception classes alone, so it is exhaustively testable here
  rather than only reachable by faulting a live GUI. }
unit testcase_client_fault;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry, client_log,
  //  EUserException: the model declining an operation and saying why.
  MyExceptions;

type
  TClientFaultTest = class(TTestCase)
  published
    procedure MemoryFaultsEndTheProcess;
    procedure FailedOperationsDoNot;
    procedure ArithmeticFaultsAreOperations;
    procedure TheModuleMapIsWrittenWithoutRaising;

    //  The second decision: a refusal is not a fault, and must not cost the poll.
    procedure ARefusalIsRecognisedAsOne;
    procedure AnOrdinaryFailureIsNot;
    procedure NorIsAMemoryFault;
  end;

implementation

{ Every class here means "the memory of this process is not what the code
  believes": a bad pointer dereferenced, a bad pointer freed, a fault the OS
  reported. None of them says which operation failed, because by then the
  faulting code has been abandoned half-done. }
procedure TClientFaultTest.MemoryFaultsEndTheProcess;
begin
  AssertTrue('EAccessViolation',
    FaultLeavesProcessUnsound(EAccessViolation.Create('x')));
  AssertTrue('EInvalidPointer',
    FaultLeavesProcessUnsound(EInvalidPointer.Create('x')));
  AssertTrue('EExternalException',
    FaultLeavesProcessUnsound(EExternalException.Create('x')));
end;

{ The everyday failures: a refused request, an unreachable server, a file that
  is not what it claimed. The client must keep running and say so - killing the
  process over a bad data file would be a far worse bug than the one this
  guards. }
procedure TClientFaultTest.FailedOperationsDoNot;
begin
  AssertFalse('Exception', FaultLeavesProcessUnsound(Exception.Create('refused')));
  AssertFalse('EInOutError', FaultLeavesProcessUnsound(EInOutError.Create('x')));
  AssertFalse('EConvertError',
    FaultLeavesProcessUnsound(EConvertError.Create('x')));
  AssertFalse('EListError', FaultLeavesProcessUnsound(EListError.Create('x')));
end;

{ Deliberately on the survivable side, and the reason the decision names classes
  instead of testing for EExternal: the arithmetic faults share that base, and a
  fit over pathological data can raise one without any memory being wrong. }
procedure TClientFaultTest.ArithmeticFaultsAreOperations;
begin
  AssertFalse('EDivByZero', FaultLeavesProcessUnsound(EDivByZero.Create('x')));
  AssertFalse('EOverflow', FaultLeavesProcessUnsound(EOverflow.Create('x')));
  AssertFalse('EUnderflow', FaultLeavesProcessUnsound(EUnderflow.Create('x')));
end;

{ A REFUSAL MUST NOT COST THE STATE POLL, and this is the decision that keeps it
  from doing so.

  The last-resort handler treated every exception alike: it logged at Fatal and
  stopped the poll. So the server declining a delete - "this curve carries no
  handle, so it cannot be removed on its own" - left the window frozen with
  "Server polling has been stopped" stapled to a message that was perfectly
  correct, and the user read it as the compute server having crashed. It had
  not. }
procedure TClientFaultTest.ARefusalIsRecognisedAsOne;
begin
  AssertTrue('raised in this process',
    IsRefusalRatherThanFault(EUserException.Create('the model declines')));
end;

{ An unreachable server, a bad data file, a conversion that failed: those ARE
  faults of a kind - nobody chose them - and the poll stopping is the honest
  response, because the next poll would raise the same thing. }
procedure TClientFaultTest.AnOrdinaryFailureIsNot;
begin
  AssertFalse('Exception', IsRefusalRatherThanFault(Exception.Create('x')));
  AssertFalse('EInOutError', IsRefusalRatherThanFault(EInOutError.Create('x')));
  AssertFalse('EConvertError',
    IsRefusalRatherThanFault(EConvertError.Create('x')));
end;

{ And a memory fault is never a refusal, whatever it says: the two decisions
  must not be able to disagree, because one of them ends the process. }
procedure TClientFaultTest.NorIsAMemoryFault;
begin
  AssertFalse('EAccessViolation',
    IsRefusalRatherThanFault(EAccessViolation.Create('x')));
  AssertTrue('and it still ends the process',
    FaultLeavesProcessUnsound(EAccessViolation.Create('x')));
end;

{ It runs on the way out of a crash, so it must not add a second one - on a
  platform with no /proc, with the file unreadable, or with the map absent it
  has to be a no-op. }
procedure TClientFaultTest.TheModuleMapIsWrittenWithoutRaising;
begin
  LogClientModuleMap;
  AssertTrue('LogClientModuleMap returned', True);
end;

initialization
  RegisterTest('unit', TClientFaultTest);
end.
