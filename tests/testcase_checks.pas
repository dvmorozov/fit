// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The internal-check routines, which are now load-bearing in release.)

These replaced `Assert` throughout the engine (Stage 3E), and that raises the
stakes: an `Assert` that was wrong was compiled out of release and harmed
nothing, whereas a check that fires when it should not now stops a user's fit.
So both directions are asserted here - a holding invariant must pass SILENTLY,
and a violated one must raise - and the negative cases matter more than the
positive ones.

The distinguishing property is that these do NOT depend on a build flag. There is
no way to write a test that proves that from inside one build; what is written
instead is the behaviour that must be identical in both, so a future
IFOPT-conditional creeping in would break these in the release build that ships.
}
unit testcase_checks;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, checks, log, MyExceptions;

type
    TChecksTest = class(TTestCase)
    private
        { Runs AProc and reports whether it raised EInternalCheckFailed, and with
          what message. }
        function Fails(AProc: TProcedure; out AMessage: string): boolean;
    published
        procedure AHoldingConditionPassesSilently;
        procedure AViolatedConditionRaises;
        procedure TheDescriptionReachesTheMessage;
        procedure AnAssignedObjectPasses;
        procedure AMissingObjectRaisesNamingIt;
        procedure AValidIndexPasses;
        procedure AnIndexPastTheEndRaises;
        procedure ANegativeIndexRaises;
        procedure AnEmptyCollectionRejectsEveryIndex;
        procedure TheIndexMessageCarriesTheOffendingNumber;
        procedure AnUnreachableBranchAlwaysRaises;
        procedure TheFailureIsDistinguishableFromAnOrdinaryError;

        { The minimizer package makes the same checks through a sink this
          application fills. Nothing else would notice if that came undone. }
        procedure APackageCheckReachesThisApplicationsLog;
        procedure APackageCheckIsTheSameExceptionClass;
    end;

implementation

function TChecksTest.Fails(AProc: TProcedure; out AMessage: string): boolean;
begin
    AMessage := '';
    Result := False;
    try
        AProc();
    except
        on E: EInternalCheckFailed do
        begin
            AMessage := E.Message;
            Result := True;
        end;
    end;
end;

procedure ViolatedCondition;
begin
    CheckThat(1 = 2, 'the profile must have as many points as the model');
end;

procedure HoldingCondition;
begin
    CheckThat(1 = 1, 'this must not fire');
end;

procedure MissingObject;
begin
    CheckAssigned(nil, 'the R-factor bounds');
end;

procedure IndexPastEnd;
begin
    CheckIndex(3, 3, 'the curve list');
end;

procedure NegativeIndex;
begin
    CheckIndex(-1, 3, 'the curve list');
end;

procedure IndexIntoEmpty;
begin
    CheckIndex(0, 0, 'the curve list');
end;

procedure Unreachable;
begin
    CheckUnreachable('the loss kind switch');
end;

{ ------------------------------------------------------------------- tests }

procedure TChecksTest.AHoldingConditionPassesSilently;
var
    Msg: string;
begin
    //  The case that runs a million times a fit. If it were to raise, or to log,
    //  the checks would be unusable in the inner loop and would be removed again.
    AssertFalse('a true condition must not raise',
        Fails(@HoldingCondition, Msg));
end;

procedure TChecksTest.AViolatedConditionRaises;
var
    Msg: string;
begin
    AssertTrue('a false condition must raise', Fails(@ViolatedCondition, Msg));
end;

procedure TChecksTest.TheDescriptionReachesTheMessage;
var
    Msg: string;
begin
    Fails(@ViolatedCondition, Msg);
    //  The description is the whole value of the check. A failure that says only
    //  "assertion failed" costs the reader the diagnosis these exist to give.
    AssertTrue('the description must survive into the message',
        Pos('as many points as the model', Msg) > 0);
end;

procedure TChecksTest.AnAssignedObjectPasses;
var
    Obj: TObject;
begin
    Obj := TObject.Create;
    try
        //  No assertion needed: this raising IS the failure. The point is that
        //  the overwhelmingly common case - the object is there - costs nothing
        //  and says nothing.
        CheckAssigned(Obj, 'the object');
    finally
        Obj.Free;
    end;
end;

procedure TChecksTest.AMissingObjectRaisesNamingIt;
var
    Msg: string;
begin
    AssertTrue('nil must raise', Fails(@MissingObject, Msg));
    //  Named in the terms of the domain, so the log says what is missing rather
    //  than which field happened to hold it.
    AssertTrue('and the message must name what is missing',
        Pos('R-factor bounds', Msg) > 0);
end;

procedure TChecksTest.AValidIndexPasses;
begin
    //  Both ends of the valid range, since an off-by-one at either is the defect
    //  this check exists to catch.
    CheckIndex(0, 3, 'the curve list');
    CheckIndex(2, 3, 'the curve list');
end;

procedure TChecksTest.AnIndexPastTheEndRaises;
var
    Msg: string;
begin
    AssertTrue('count is not a valid index', Fails(@IndexPastEnd, Msg));
end;

procedure TChecksTest.ANegativeIndexRaises;
var
    Msg: string;
begin
    AssertTrue('a negative index must raise', Fails(@NegativeIndex, Msg));
end;

procedure TChecksTest.AnEmptyCollectionRejectsEveryIndex;
var
    Msg: string;
begin
    //  The boundary the naive form (`AIndex > ACount`) gets wrong, and the one a
    //  freshly cleared list hits first.
    AssertTrue('no index is valid in an empty collection',
        Fails(@IndexIntoEmpty, Msg));
end;

procedure TChecksTest.TheIndexMessageCarriesTheOffendingNumber;
var
    Msg: string;
begin
    Fails(@IndexPastEnd, Msg);
    //  An off-by-one and a wildly wrong index are different defects, and only
    //  the numbers tell them apart.
    AssertTrue('the index appears in the message', Pos('3', Msg) > 0);
    AssertTrue('and what was being indexed', Pos('curve list', Msg) > 0);
end;

procedure TChecksTest.AnUnreachableBranchAlwaysRaises;
var
    Msg: string;
begin
    AssertTrue('reaching it is by definition a defect',
        Fails(@Unreachable, Msg));
    AssertTrue('and it says which branch', Pos('loss kind switch', Msg) > 0);
end;

procedure TChecksTest.TheFailureIsDistinguishableFromAnOrdinaryError;
var
    Caught: boolean;
begin
    //  A handler written for user-facing errors must not silently absorb an
    //  internal defect - "the user did something unsupported" and "this program
    //  is wrong about itself" need different responses.
    Caught := False;
    try
        CheckThat(False, 'deliberate');
    except
        on E: EInternalCheckFailed do
            Caught := True;
    end;
    AssertTrue('it must be its own exception class', Caught);
end;

procedure TChecksTest.APackageCheckReachesThisApplicationsLog;
var
    LogFile, Contents: string;
    S: TStringList;
    F: TFileStream;
begin
    //  THE FAILURE MODE OF AN INJECTED SINK IS THAT NOBODY ASSIGNED IT, and it
    //  is silent: every check still raises, so every other test passes, and the
    //  only loss is the log line that would have reached a bug report. So this
    //  drives a check in the PACKAGE and reads this application's log file.
    //
    //  Wired from checks.pas's initialization rather than from each .lpr, which
    //  is why merely linking this unit is enough for the line to appear.
    LogFile := GetConfigDir + 'test_checks_sink.txt';
    DeleteFile(LogFile);
    SetLogFileName('test_checks_sink.txt');
    try
        try
            //  MyExceptions.CheckThat, not checks.CheckThat: the point is that
            //  the package's own routine reaches here.
            MyExceptions.CheckThat(False, 'a package invariant nobody upheld');
        except
            on E: MyExceptions.EInternalCheckFailed do ;
        end;
        Contents := '';
        if FileExists(LogFile) then
        begin
            //  Share-permissive: the logger holds this file open for append
            //  while the test reads it, and LoadFromFile asks for no sharing at
            //  all - which Windows refuses outright.
            F := TFileStream.Create(LogFile, fmOpenRead or fmShareDenyNone);
            try
                S := TStringList.Create;
                try
                    S.LoadFromStream(F);
                    Contents := S.Text;
                finally
                    S.Free;
                end;
            finally
                F.Free;
            end;
        end;
        AssertTrue('the package check was logged here, and said what it expected',
            Pos('a package invariant nobody upheld', Contents) > 0);
    finally
        SetLogFileName('log.txt');
        DeleteFile(LogFile);
    end;
end;

procedure TChecksTest.APackageCheckIsTheSameExceptionClass;
var
    Caught: boolean;
begin
    //  ONE CLASS, NOT TWO OF A NAME. The package declares
    //  EInternalCheckFailed because it cannot name this unit; `checks` aliases
    //  that class rather than declaring a second one, so a handler written
    //  against either catches both. Two same-named classes would make
    //  `on E: EInternalCheckFailed` catch whichever the uses clause resolved and
    //  silently miss the other - which is the bug this asserts away.
    Caught := False;
    try
        MyExceptions.CheckThat(False, 'a package invariant nobody upheld');
    except
        on E: checks.EInternalCheckFailed do
            Caught := True;
    end;
    AssertTrue('a package check is catchable as this unit''s own class', Caught);
end;

initialization
    RegisterTest('unit', TChecksTest);
end.
