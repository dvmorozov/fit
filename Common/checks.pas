// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Internal consistency checks that behave the same in release as in debug.)

WHY NOT `Assert`. `Assert` is compiled out of a release build. That means the
checks a developer relies on to catch a broken invariant are exactly the checks
that are absent from the build users run - so a violated invariant that would
stop the program on a developer's machine instead runs on, producing wrong output
quietly, in the one situation where diagnosing it is hardest. A check that only
runs when it is not needed is not a check.

The rule for this codebase (Stage 3E): **release makes the same checks debug
does, and every failure is logged where it happens.** These routines do both -
they log at the point of failure, with the message written for whoever reads the
log, and then raise. Nothing here is conditional on a build flag.

WHY LOG *AND* RAISE, when the raise carries the message anyway. Because the two
have different audiences and different survival rates. An exception can be caught
and reworded by a caller several layers up, or swallowed by a handler that was
written for a different failure; the log entry is written before any of that can
happen, and it is the record that reaches a user's bug report. Where the two
disagree, the log is what was actually true at the point of failure.

WHAT THESE ARE NOT FOR. Not for invalid user input, and not for conditions the
program is expected to encounter - a missing file, an unreachable server, a
number the user typed wrongly. Those are ordinary outcomes and deserve an
ordinary message aimed at the user (EUserException). These are for statements the
code believes must be true about ITSELF, where being wrong means a defect.
}
unit checks;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, log, MyExceptions;

type
    { Raised when an internal invariant does not hold. Distinct from
      EUserException so a handler can tell "the user did something we do not
      support" from "this program is wrong about itself" - and so a catch-all
      written for the first can be found and narrowed.

      AN ALIAS, NOT A SECOND CLASS. The minimizer package declares this class
      because it makes the same checks and cannot name this unit; two distinct
      classes of the same name would be the worst of both, since `on E:
      EInternalCheckFailed` would then catch whichever one the uses clause
      happened to resolve and silently miss the other. There is one class: a
      broken invariant is a broken invariant whichever side of the package
      boundary stated it, and every existing handler keeps working unchanged. }
    EInternalCheckFailed = MyExceptions.EInternalCheckFailed;

{ Fails unless ACondition holds.

  ADescription must say what was expected, in terms of the domain, not repeat the
  expression - "the fit must have a profile before curves are collected" is
  useful in a log; "FProfile <> nil" is what the next line of code already says. }
procedure CheckThat(ACondition: boolean; const ADescription: string);

{ Fails unless AObject is assigned. AName is what the thing IS, so the log reads
  "the R-factor bounds are missing" rather than naming a field. }
procedure CheckAssigned(AObject: TObject; const AName: string);

{ Fails unless AIndex is a valid position in a collection of ACount items.
  Reports the offending index and the size, because an off-by-one and a wildly
  wrong index are different defects and the numbers distinguish them. }
procedure CheckIndex(AIndex, ACount: longint; const AWhat: string);

{ Always fails. For a branch that must be unreachable - the `else` of a case over
  a closed set, a state the machine has no transition into. Reaching one means an
  assumption expired, and silently doing nothing there is how a new enum value
  ends up quietly ignored. }
procedure CheckUnreachable(const AWhat: string);

implementation

{ One place that logs and raises, so the two can never drift apart. }
procedure Fail(const AMessage: string);
begin
    //  Logged FIRST. A caller may catch and reword this, or a handler written
    //  for something else may swallow it; the log entry is written before either
    //  can happen and is what reaches a bug report.
    WriteLog('internal check failed: ' + AMessage, log.Fatal);
    raise EInternalCheckFailed.Create(AMessage);
end;

procedure CheckThat(ACondition: boolean; const ADescription: string);
begin
    if not ACondition then
        Fail(ADescription);
end;

procedure CheckAssigned(AObject: TObject; const AName: string);
begin
    if not Assigned(AObject) then
        Fail(AName + ' is missing when it is required');
end;

procedure CheckIndex(AIndex, ACount: longint; const AWhat: string);
begin
    if (AIndex < 0) or (AIndex >= ACount) then
        Fail(Format('%s: index %d is outside 0..%d', [AWhat, AIndex, ACount - 1]));
end;

procedure CheckUnreachable(const AWhat: string);
begin
    Fail(AWhat + ': this branch was believed unreachable');
end;

{ THE MINIMIZER PACKAGE'S CHECKS REACH THIS LOG TOO.

  `MyExceptions` carries the same three routines for the same reason - Assert is
  compiled out of a release build - but it is a separately licensed package that
  must build standalone, so it cannot name this unit and has no logger of its
  own. It offers a sink instead, and this is the one place that fills it.

  WIRED FROM AN INITIALIZATION SECTION, NOT FROM EACH PROGRAM. Four binaries link
  this - the client, the compute server, the test suites and dump_registries -
  and a line in each .lpr is a line the fifth one forgets. This unit is already
  linked into every one of them, because every one of them makes checks; so the
  wiring happens by the act of linking, and the failure mode of an injected sink
  - nobody assigned it - cannot arise.

  The severity is Fatal and the wording matches this unit's own, because a
  reader of the log should not have to know which side of a package boundary a
  broken invariant was stated on. }
procedure LogPackageCheckFailure(const AMessage: string);
begin
    WriteLog('internal check failed: ' + AMessage, log.Fatal);
end;

initialization
    MyExceptions.OnCheckFailed := @LogPackageCheckFailure;

end.
