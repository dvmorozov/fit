// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which builder owns a marked point set, and what "nothing claims it"
means.)

The interesting thing here is not the lookup, it is the RETURN VALUE CONVENTION.
A builder returns True when it has dealt with the point set - including when it
deliberately built nothing - and False sends the engine down the position-based
path, which with nothing marked builds one curve per data point. A user once saw
that as a hang. So "handled and built nothing" and "not handled" must stay
distinguishable, and a builder that got the convention backwards would produce
exactly that hang.

PROCESS-GLOBAL AND APPEND-ONLY, like the module registry: no unregister, unique
names per test, relative counts.
}
unit testcase_curve_builder_registry;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, curve_builder_registry;

type
    TCurveBuilderRegistryTest = class(TTestCase)
    private
        function Refused(const AName: string; ABuilder: TCurveBuilder): boolean;
    published
        procedure ARegisteredBuilderIsFound;
        procedure ABuilderIsCountedOnce;
        procedure ADuplicateRegistrationIsRefused;
        procedure AnUnclaimedPointSetIsNotFound;
        procedure TheLookupIsByExactName;
        procedure HandledAndBuiltNothingIsDistinctFromNotHandled;
        procedure TheBuilderReceivesBothArguments;
        //  Registered with something missing.
        procedure ABuilderWithNoPointSetNameIsRefused;
        procedure ANamedPointSetWithNoBuilderIsRefused;
        procedure TheTwoRefusalsAreDistinguishable;
    end;

implementation

var
    { What the builders recorded. Module-level because a TCurveBuilder is a plain
      function pointer and cannot be a method. }
    LastTask, LastStored: TObject;
    Calls: longint;

function BuiltSomething(ATask, AStoredValues: TObject): boolean;
begin
    Inc(Calls);
    LastTask := ATask;
    LastStored := AStoredValues;
    Result := True;
end;

function HandledButBuiltNothing(ATask, AStoredValues: TObject): boolean;
begin
    //  The case the convention exists for: this point set is mine, and for this
    //  model there is nothing to build. True, so the engine does NOT fall back.
    Result := True;
end;

function DidNotHandle(ATask, AStoredValues: TObject): boolean;
begin
    Result := False;
end;

function TCurveBuilderRegistryTest.Refused(const AName: string;
    ABuilder: TCurveBuilder): boolean;
begin
    Result := False;
    try
        RegisterCurveBuilder(AName, ABuilder);
    except
        on ECurveBuilderRegistration do
            Result := True;
    end;
end;

procedure TCurveBuilderRegistryTest.ARegisteredBuilderIsFound;
var
    Found: TCurveBuilder;
begin
    AssertFalse('accepted', Refused('cb-found', BuiltSomething));
    AssertTrue('and findable', FindCurveBuilder('cb-found', Found));
    //  @Found, not Found: in Delphi mode a procedural variable in an expression
    //  is CALLED, so comparing without the address-of operator invokes the
    //  builder instead of identifying it.
    AssertTrue('and it is the one registered', @Found = @BuiltSomething);
end;

procedure TCurveBuilderRegistryTest.ABuilderIsCountedOnce;
var
    Before: longint;
begin
    Before := CurveBuilderCount;
    AssertFalse('accepted', Refused('cb-counted', BuiltSomething));
    AssertEquals('one more', Before + 1, CurveBuilderCount);
end;

procedure TCurveBuilderRegistryTest.ADuplicateRegistrationIsRefused;
var
    Before: longint;
    Found: TCurveBuilder;
begin
    //  Two builders for one point set would be resolved by registration order,
    //  and the loser would be dead code that still looks installed.
    AssertFalse('the first is accepted', Refused('cb-dup', BuiltSomething));
    Before := CurveBuilderCount;
    AssertTrue('the second is refused', Refused('cb-dup', DidNotHandle));
    AssertEquals('and nothing was stored', Before, CurveBuilderCount);
    AssertTrue('found', FindCurveBuilder('cb-dup', Found));
    AssertTrue('the first one still owns it', @Found = @BuiltSomething);
end;

procedure TCurveBuilderRegistryTest.AnUnclaimedPointSetIsNotFound;
var
    Found: TCurveBuilder;
begin
    //  The ORDINARY case in a build with no module, so False here must not read
    //  as a fault - a caller reporting it has to say "no module registered one".
    AssertFalse(FindCurveBuilder('cb-nothing-claims-this', Found));
end;

procedure TCurveBuilderRegistryTest.TheLookupIsByExactName;
var
    Found: TCurveBuilder;
begin
    AssertFalse('accepted', Refused('cb-exact', BuiltSomething));
    AssertFalse('a prefix is not a match', FindCurveBuilder('cb-exac', Found));
    AssertFalse('nor a longer name', FindCurveBuilder('cb-exactly', Found));
    AssertTrue('the name itself is', FindCurveBuilder('cb-exact', Found));
end;

procedure TCurveBuilderRegistryTest.HandledAndBuiltNothingIsDistinctFromNotHandled;
var
    Handled, NotHandled: TCurveBuilder;
begin
    //  THE DISTINCTION THAT COST A USER A HANG. Both build no curves; only one of
    //  them tells the engine to stop. If a future builder collapses these into
    //  one answer, the fallback path runs on an empty markup and builds one curve
    //  per data point.
    AssertFalse('accepted', Refused('cb-empty', HandledButBuiltNothing));
    AssertFalse('accepted', Refused('cb-declined', DidNotHandle));

    AssertTrue('found', FindCurveBuilder('cb-empty', Handled));
    AssertTrue('found', FindCurveBuilder('cb-declined', NotHandled));

    AssertTrue('handled, even though it built nothing', Handled(nil, nil));
    AssertFalse('not handled, so the engine falls back', NotHandled(nil, nil));
end;

procedure TCurveBuilderRegistryTest.TheBuilderReceivesBothArguments;
var
    Found: TCurveBuilder;
    Task, Stored: TObject;
begin
    //  Both are passed as TObject so this registry names no engine type. The
    //  second one is the values the LAST fit found, which the builder hands back
    //  per curve - drop it and every curve is rebuilt from defaults, losing the
    //  fit the user just ran.
    Task := TObject.Create;
    Stored := TObject.Create;
    try
        AssertFalse('accepted', Refused('cb-args', BuiltSomething));
        AssertTrue('found', FindCurveBuilder('cb-args', Found));
        LastTask := nil;
        LastStored := nil;
        AssertTrue('handled', Found(Task, Stored));
        AssertTrue('the task arrived', LastTask = Task);
        AssertTrue('and the stored values with it', LastStored = Stored);
    finally
        Task.Free;
        Stored.Free;
    end;
end;

{ ------------------- registered with something missing ---------------------- }

{ A CURVE BUILDER IS HOW A MODULE PLACES ITS OWN KIND OF CURVE, found by the name
  of the point set the user picks into. Both halves are required and neither can
  be inferred. }

procedure TCurveBuilderRegistryTest.ABuilderWithNoPointSetNameIsRefused;
begin
    //  NOTHING WOULD EVER REACH IT. The point-set name is the whole lookup key,
    //  so a builder registered without one sits in the registry unfound - and
    //  the module's curves silently never get built.
    AssertTrue('refused', Refused('', @BuiltSomething));
end;

procedure TCurveBuilderRegistryTest.ANamedPointSetWithNoBuilderIsRefused;
begin
    //  THE OPPOSITE OMISSION, and the dangerous one: the name IS found, so the
    //  framework believes the point set is placeable, routes the user's picks to
    //  it, and calls nil when it comes to building.
    AssertTrue('refused', Refused('cb-named-but-nil', nil));
end;

procedure TCurveBuilderRegistryTest.TheTwoRefusalsAreDistinguishable;
var
    NoName, NoBuilder: string;
begin
    //  The reader is whoever added the builder, looking at a two-argument call.
    //  One message for both would not say which argument.
    NoName := '';
    try
        RegisterCurveBuilder('', @BuiltSomething);
    except
        on E: ECurveBuilderRegistration do
            NoName := E.Message;
    end;
    NoBuilder := '';
    try
        RegisterCurveBuilder('cb-message-check', nil);
    except
        on E: ECurveBuilderRegistration do
            NoBuilder := E.Message;
    end;
    AssertTrue('the nameless one mentions the point set: ' + NoName,
        Pos('point set', NoName) > 0);
    AssertTrue('the other names the set it was given: ' + NoBuilder,
        Pos('cb-message-check', NoBuilder) > 0);
    AssertTrue('they are different messages', NoName <> NoBuilder);
end;

initialization
    //  A unit test: function pointers and a string key.
    RegisterTest('unit', TCurveBuilderRegistryTest);
end.
