// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The minimizer package's own unconditional checks, and which error they
raise.)

WHY THE PACKAGE HAS ITS OWN. `Assert` is compiled out of a release build, so a
check written with it is absent from exactly the build users run. The framework
answered that with `Common/checks.pas`; this package cannot use it, because it has
its own release cycle and its own licence and must build with nothing beside it -
so `MyExceptions` carries the same three routines.

WHAT THIS PINS, and it is not the arithmetic. It is the pair of directions every
check has: it must stay silent when the invariant holds, and it must raise
`EInternalCheckFailed` - not `EUserException` - when it does not. Both halves
matter, and only one of them is obvious. A check that always raised would be
caught by the first test written against any caller; a check that never raised
would be caught by nothing at all, which is the failure `Assert` had.

AND THE CLASS IS THE POINT. `EUserException` means the caller asked for something
this code does not support; `EInternalCheckFailed` means the code is wrong about
itself. They are kept distinct so a catch-all written for the first cannot
silently absorb the second - so a test that caught plain `Exception` would pass
while the distinction rotted away, which is how six handlers in the consuming
application came to catch an exception nothing raised.

THE CHECKS THESE REPLACED are the fifteen in `SimpMath`'s lineshape functions -
amplitude and width never negative, mixing fraction inside 0..1 - which
`Server/fit_task.pas` calls on every evaluated point. Those had been compiled out
of every release build the project has ever shipped.
}
unit testcase_my_exceptions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, MyExceptions;

type
    TMyExceptionsTest = class(TTestCase)
    private
        { Runs AProc and answers what it raised: 'nothing', 'defect' for
          EInternalCheckFailed, 'user error' for EUserException, or the class
          name of anything else. Naming the class rather than counting is what
          makes a wrong-class failure legible. }
        function KindRaisedBy(AProc: TProcedure): string;
    published
        { A condition that holds costs nothing and says nothing. }
        procedure AHoldingConditionIsSilent;
        procedure AnAssignedObjectIsSilent;
        procedure AnIndexInsideTheRangeIsSilent;

        { A condition that does not hold stops the program. }
        procedure AViolatedConditionRaises;
        procedure AMissingObjectRaises;
        procedure AnIndexPastTheEndRaises;
        procedure ANegativeIndexRaises;
        procedure AnEmptyCollectionRejectsEveryIndex;

        { What the failure says, and what class it arrives as. }
        procedure TheDescriptionReachesTheMessageUnchanged;
        procedure TheMissingObjectMessageNamesWhatIsMissing;
        procedure TheIndexMessageCarriesTheOffendingNumber;
        procedure AFailureIsADefectRatherThanUserError;
    end;

implementation

//  The bodies these drive. Free procedures rather than methods, because
//  TProcedure is a plain procedure pointer.
procedure ViolateACondition;
begin
    CheckThat(False, 'a wave count is never negative');
end;

procedure OmitAnObject;
begin
    CheckAssigned(nil, 'the point cloud being measured');
end;

procedure IndexPastTheEnd;
begin
    CheckIndex(3, 3, 'the values in the selected combination');
end;

procedure NegativeIndex;
begin
    CheckIndex(-1, 3, 'the values in the selected combination');
end;

procedure IndexIntoAnEmptyCollection;
begin
    CheckIndex(0, 0, 'the values in the selected combination');
end;

function TMyExceptionsTest.KindRaisedBy(AProc: TProcedure): string;
begin
    Result := 'nothing';
    try
        AProc;
    except
        on E: EInternalCheckFailed do
            Result := 'defect';
        on E: EUserException do
            Result := 'user error';
        on E: Exception do
            Result := E.ClassName;
    end;
end;

procedure TMyExceptionsTest.AHoldingConditionIsSilent;
begin
    CheckThat(True, 'this description is never used');
    AssertTrue('returned', True);
end;

procedure TMyExceptionsTest.AnAssignedObjectIsSilent;
var
    Obj: TObject;
begin
    Obj := TObject.Create;
    try
        CheckAssigned(Obj, 'the object under test');
        AssertTrue('returned', True);
    finally
        Obj.Free;
    end;
end;

procedure TMyExceptionsTest.AnIndexInsideTheRangeIsSilent;
begin
    //  Both ends, because an off-by-one at either is the defect this catches.
    CheckIndex(0, 3, 'a collection of three');
    CheckIndex(2, 3, 'a collection of three');
    AssertTrue('returned', True);
end;

procedure TMyExceptionsTest.AViolatedConditionRaises;
begin
    AssertEquals('refused', 'defect', KindRaisedBy(@ViolateACondition));
end;

procedure TMyExceptionsTest.AMissingObjectRaises;
begin
    AssertEquals('refused', 'defect', KindRaisedBy(@OmitAnObject));
end;

procedure TMyExceptionsTest.AnIndexPastTheEndRaises;
begin
    //  The first index OUTSIDE a collection of three is 3, and reading it is
    //  the ordinary off-by-one.
    AssertEquals('refused', 'defect', KindRaisedBy(@IndexPastTheEnd));
end;

procedure TMyExceptionsTest.ANegativeIndexRaises;
begin
    AssertEquals('refused', 'defect', KindRaisedBy(@NegativeIndex));
end;

procedure TMyExceptionsTest.AnEmptyCollectionRejectsEveryIndex;
begin
    //  ZERO IS OUT OF RANGE when there is nothing there, and the upper bound
    //  this prints is -1. Worth pinning: a guard written as `AIndex > ACount`
    //  would admit it.
    AssertEquals('refused', 'defect', KindRaisedBy(@IndexIntoAnEmptyCollection));
end;

procedure TMyExceptionsTest.TheDescriptionReachesTheMessageUnchanged;
var
    Message: string;
begin
    //  The description is the whole record of what went wrong here - this
    //  package has no logger - so it must arrive verbatim rather than wrapped.
    Message := '';
    try
        CheckThat(False, 'a wave count is never negative');
    except
        on E: EInternalCheckFailed do
            Message := E.Message;
    end;
    AssertEquals('a wave count is never negative', Message);
end;

procedure TMyExceptionsTest.TheMissingObjectMessageNamesWhatIsMissing;
var
    Message: string;
begin
    Message := '';
    try
        CheckAssigned(nil, 'the point cloud being measured');
    except
        on E: EInternalCheckFailed do
            Message := E.Message;
    end;
    AssertEquals('the point cloud being measured is missing when it is required',
        Message);
end;

procedure TMyExceptionsTest.TheIndexMessageCarriesTheOffendingNumber;
var
    Message: string;
begin
    //  THE NUMBERS ARE THE POINT: an off-by-one and a wildly wrong index are
    //  different defects, and the message is what tells them apart.
    Message := '';
    try
        CheckIndex(7, 3, 'the values in the selected combination');
    except
        on E: EInternalCheckFailed do
            Message := E.Message;
    end;
    AssertEquals('the values in the selected combination: index 7 is outside 0..2',
        Message);
end;

procedure TMyExceptionsTest.AFailureIsADefectRatherThanUserError;
begin
    //  Asserted as its own case, rather than left implicit in the tests above,
    //  because this is the distinction the two classes exist for: a handler
    //  written for user error must not be able to absorb a defect.
    AssertEquals('not user error', 'defect', KindRaisedBy(@ViolateACondition));
    AssertFalse('the two classes are unrelated',
        EInternalCheckFailed.InheritsFrom(EUserException));
end;

initialization
    //  A unit test: a condition in, an exception or silence out. No optimiser,
    //  no fit, no file.
    RegisterTest('unit', TMyExceptionsTest);
end.
