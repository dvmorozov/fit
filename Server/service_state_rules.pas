// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which of the service's states admit which operation, and what the user
is told when one does not.)

FOUR RULES, WRITTEN ABOUT THIRTY TIMES. The compute service decided all of this
inline, at every entry point: whether an operation in flight has to be aborted
before this one can run, whether the profile has to be loaded first, whether
picked points can be accepted, whether an abort has anything to abort. The
profile rule alone appeared at twenty separate raise sites, spelled out in full
each time.

THIRTY COPIES IS NOT A TIDINESS PROBLEM. It is why the rules could only be
reached by driving the whole service - which needs the optimiser, so by this
project's own rule those are integration tests, and the service sits at two
thirds covered with its decisions among the uncovered third. Each rule here is a
function of the state enum and nothing else, so all of them can be stated
exhaustively over its seven values with no service, no profile and no fit.

THE CLIENT'S HALF IS HERE TOO, for the same enum: the window has to decide
whether to OFFER a fit where the service decides whether to ACCEPT one, and the
two answers differ. `FitIsOffered` and `ProfileRefusal` sit next to each other
now so that the difference is one readable pair rather than a rule restated in
another process, and `testcase_service_state_rules` walks both over every state.

THE DIFFERENCE THAT MATTERS IS AT BackNotRemoved, and it is a workflow choice
rather than a disagreement to be resolved. (The pair reads differently at
AsyncOperation too, and that one is incidental: a fit entry point aborts what is
running and goes ahead, while the window keeps the command dark for the length of
the operation. Same intent, said twice.) The service accepts a manual fit there and completes
the missing data itself - it says so where the refusal used to be, "Instead of an
error, the data that is needed is created" - so a REST caller may fit a profile
whose background has not been subtracted. The window does not offer the two
manual fits there, and offers "do all automatically" instead, which subtracts and
then fits. Fitting curves to a profile that still carries its background is a
thing to be asked for deliberately, not a thing to be reached by clicking the
ordinary command, and that is what the pair below encodes. Neither side is
wrong; what was wrong was that the intent lived in a commented-out guard in one
process and an unexplained condition in the other.

A REFUSAL IS A USER ERROR, NOT A FAULT, which is why RefuseIf raises
EUserException specifically: the REST layer maps that class to 400 and anything
else to 500, so the class is the difference between "your request was wrong for
this state" and "the server broke". That mapping has been wrong here before; see
findings.md.
}
unit service_state_rules;

{$MODE Delphi}

interface

uses
    int_fit_service;

type
    { Why the service will not do what was asked. }
    TServiceRefusal = (
        { It will. }
        rfNone,
        { No profile has been loaded, so there is nothing to operate on. }
        rfDataMustBeSet,
        { An operation is in flight and this one cannot join it. }
        rfNowCalculating,
        { Nothing is running, so there is nothing to abort. }
        rfCalcNotStarted,
        { No fit has completed, so there is no result to read. }
        rfFitNotDone);

{ True when a command must abort the operation in flight before doing its own
  work.

  ABORTING IS NOT REFUSING. The command goes ahead; what the caller gets back is
  a note saying the previous calculation was cancelled to make room for it. That
  distinction is the reason this is a separate question from the refusals below -
  a state that has to be cleared is not a state that says no. }
function MustAbortRunningOperation(AState: TFitServerState): boolean;

{ Why an operation that needs the loaded profile is refused, or rfNone. }
function ProfileRefusal(AState: TFitServerState): TServiceRefusal;

{ Whether the WINDOW offers a manual fit in this state - which is not the same
  question as whether the service would accept one; see the note at the top of
  this unit for why they differ and where.

  ReadyForAutoFit counts: the data such a fit still needs is completed
  automatically, so refusing here would disable the button that does it. }
function FitIsOffered(AState: TFitServerState): boolean;

{ Why accepting picked points is refused, or rfNone.

  Stricter than ProfileRefusal by one state: a pick that arrived while a
  calculation was running would be applied to a model being rebuilt underneath
  it, so it is refused rather than aborting the calculation - the user did not
  ask for the fit to stop, they clicked on the chart. }
function PickRefusal(AState: TFitServerState): TServiceRefusal;

{ Why an operation that must not run alongside a calculation is refused, or
  rfNone.

  The difference from MustAbortRunningOperation is the whole of it: these
  operations REFUSE while one is running, where the others abort it and carry
  on. Which of the two a command does is the command's own choice, and the
  service makes it differently for different commands on purpose - a fit
  replaces a running fit, a read of the model does not interrupt one. }
function BusyRefusal(AState: TFitServerState): TServiceRefusal;

{ Why an abort is refused, or rfNone. }
function AbortRefusal(AState: TFitServerState): TServiceRefusal;

{ Why reading a completed fit's result is refused, or rfNone. }
function ResultRefusal(AFitDone: boolean): TServiceRefusal;

{ The sentence the user reads, or '' for rfNone. Every refusal opens with the
  same first line and then says which one it is: the opening line is what makes
  it recognisable as a refusal rather than a fault, and the second is the only
  part that tells the user what to do. }
function ServiceRefusalText(ARefusal: TServiceRefusal): string;

{ Raises ARefusal as a user error unless it is rfNone. }
procedure RefuseIf(ARefusal: TServiceRefusal);

const
    { The first line of every refusal. }
    InadmissibleServerState = 'This operation is not available right now.';
    { The second lines. }
    DataMustBeSet = 'Load data before running this operation.';
    NowCalculation = 'A calculation is already in progress.';
    CalcNotStarted = 'The calculation not started.';
    StillNotDone = 'The calculation still not accomplished.';
    { What a command reports when it aborted a calculation to run. }
    CalcAborted = 'Calculation aborted.';
    { The separator between a refusal's two lines. #13#10 rather than
      LineEnding, which is what the service has always used: the text travels to
      a desktop client that need not be on the same platform as the server, so
      the sequence is fixed rather than taken from whichever machine composed
      the message. }
    REFUSAL_CRLF = #13#10;

implementation

uses
    SysUtils, MyExceptions;

function MustAbortRunningOperation(AState: TFitServerState): boolean;
begin
    Result := AState = AsyncOperation;
end;

function FitIsOffered(AState: TFitServerState): boolean;
begin
    //  NAMED AS THE THREE, not derived from ProfileRefusal, because it is not a
    //  narrowing of it: the states it leaves out are states in which the service
    //  would go ahead. Spelling them out is what keeps that visible.
    Result := AState in [ReadyForFit, ReadyForAutoFit, Finished];
end;

function ProfileRefusal(AState: TFitServerState): TServiceRefusal;
begin
    //  ONE STATE ONLY, and deliberately not more. Every other state has a
    //  profile, and an operation that needs one can complete whatever else is
    //  missing - which is what the service does rather than refusing, and why
    //  a stricter rule here would disable most of the program after loading a
    //  file.
    if AState = ProfileWaiting then
        Result := rfDataMustBeSet
    else
        Result := rfNone;
end;

function PickRefusal(AState: TFitServerState): TServiceRefusal;
begin
    if AState = ProfileWaiting then
        Result := rfDataMustBeSet
    else if AState = AsyncOperation then
        Result := rfNowCalculating
    else
        Result := rfNone;
end;

function BusyRefusal(AState: TFitServerState): TServiceRefusal;
begin
    if AState = AsyncOperation then
        Result := rfNowCalculating
    else
        Result := rfNone;
end;

function AbortRefusal(AState: TFitServerState): TServiceRefusal;
begin
    //  The one rule of the five that refuses everything EXCEPT the running
    //  state, which is why it is written as its own function rather than as a
    //  negation of MustAbortRunningOperation at each call site.
    if AState = AsyncOperation then
        Result := rfNone
    else
        Result := rfCalcNotStarted;
end;

function ResultRefusal(AFitDone: boolean): TServiceRefusal;
begin
    if AFitDone then
        Result := rfNone
    else
        Result := rfFitNotDone;
end;

function ServiceRefusalText(ARefusal: TServiceRefusal): string;
begin
    case ARefusal of
        rfDataMustBeSet:  Result := InadmissibleServerState + REFUSAL_CRLF +
                                    DataMustBeSet;
        rfNowCalculating: Result := InadmissibleServerState + REFUSAL_CRLF +
                                    NowCalculation;
        rfCalcNotStarted: Result := InadmissibleServerState + REFUSAL_CRLF +
                                    CalcNotStarted;
        rfFitNotDone:     Result := InadmissibleServerState + REFUSAL_CRLF +
                                    StillNotDone;
        else              Result := '';
    end;
end;

procedure RefuseIf(ARefusal: TServiceRefusal);
begin
    if ARefusal = rfNone then
        Exit;
    raise EUserException.Create(ServiceRefusalText(ARefusal));
end;

end.
