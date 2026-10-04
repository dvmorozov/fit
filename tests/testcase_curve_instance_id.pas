// SPDX-License-Identifier: GPL-3.0-or-later
{ The curve instance handle: what it guarantees, and what it refuses.

  Small enough to look not worth testing, which is exactly why it is tested.
  Everything downstream - which curve keeps its fitted values across a model
  edit, which curve a REST caller addressed - rests on two properties only:
  a fresh handle is always distinct, and a handle survives a round trip through
  text unchanged. Neither is obvious from the code, because both are really
  claims about the platform's GUID generator and about StringToGUID. }
unit testcase_curve_instance_id;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, curve_instance_id;

type
    TCurveInstanceIdTest = class(TTestCase)
    published
        procedure AFreshHandleIsNeverTheEmptyOne;
        procedure EveryFreshHandleIsDistinct;
        procedure TheEmptyHandleIsNotAnIdentity;
        procedure AHandleEqualsItselfAndNoOther;
        procedure TextRoundTripsUnchanged;
        procedure TheBracelessFormIsReadToo;
        procedure TheWireFormCarriesNoBraces;
        procedure MalformedTextIsRefusedRatherThanGuessed;
        procedure RefusedTextLeavesNoUsableHandle;
    end;

implementation

{ The empty handle means "no instance". A generator that returned it would make
  every unissued curve look like the same curve. }
procedure TCurveInstanceIdTest.AFreshHandleIsNeverTheEmptyOne;
var
    Id: TCurveInstanceId;
begin
    Id := NewCurveInstanceId;
    AssertTrue('a fresh handle is an identity', IsCurveInstanceId(Id));
    AssertFalse('and is not the empty one',
        SameCurveInstanceId(Id, NoCurveInstanceId));
end;

{ THE PROPERTY EVERYTHING ELSE RESTS ON. Two instances that shared a handle
  would restore one curve's fitted values onto the other. A loop rather than a
  pair, because a generator that repeats does not usually repeat immediately. }
procedure TCurveInstanceIdTest.EveryFreshHandleIsDistinct;
const
    HOW_MANY = 200;
var
    Ids: array[0..HOW_MANY - 1] of TCurveInstanceId;
    i, j: longint;
begin
    for i := 0 to HOW_MANY - 1 do
        Ids[i] := NewCurveInstanceId;

    for i := 0 to HOW_MANY - 1 do
        for j := i + 1 to HOW_MANY - 1 do
            AssertFalse(Format('handles %d and %d collided', [i, j]),
                SameCurveInstanceId(Ids[i], Ids[j]));
end;

procedure TCurveInstanceIdTest.TheEmptyHandleIsNotAnIdentity;
begin
    AssertFalse('the empty handle is not an identity',
        IsCurveInstanceId(NoCurveInstanceId));
    AssertTrue('and is stable',
        SameCurveInstanceId(NoCurveInstanceId, NoCurveInstanceId));
end;

procedure TCurveInstanceIdTest.AHandleEqualsItselfAndNoOther;
var
    A, B, Copy: TCurveInstanceId;
begin
    A := NewCurveInstanceId;
    B := NewCurveInstanceId;
    Copy := A;

    AssertTrue('a handle equals itself', SameCurveInstanceId(A, A));
    AssertTrue('and equals a copy of itself', SameCurveInstanceId(A, Copy));
    AssertFalse('and not another', SameCurveInstanceId(A, B));
    //  Comparison is symmetric, which a hand-rolled byte compare can get wrong.
    AssertTrue('comparison is symmetric', SameCurveInstanceId(Copy, A));
end;

{ The handle crosses the wire as text and comes back. A round trip that lost a
  digit would silently address a different curve - or none. }
procedure TCurveInstanceIdTest.TextRoundTripsUnchanged;
var
    Id, Back: TCurveInstanceId;
    Text: string;
begin
    Id := NewCurveInstanceId;
    Text := CurveInstanceIdToStr(Id);

    AssertEquals('the text form is the registry form', 38, Length(Text));
    AssertEquals('opening brace', '{', Text[1]);
    AssertEquals('closing brace', '}', Text[Length(Text)]);

    AssertTrue('it reads back', TryStrToCurveInstanceId(Text, Back));
    AssertTrue('unchanged', SameCurveInstanceId(Id, Back));
end;

{ A URL path segment carries no braces, and the id travels in one. }
procedure TCurveInstanceIdTest.TheBracelessFormIsReadToo;
var
    Id, Back: TCurveInstanceId;
    Bare: string;
begin
    Id := NewCurveInstanceId;
    Bare := CurveInstanceIdToStr(Id);
    Bare := Copy(Bare, 2, Length(Bare) - 2);

    AssertEquals('the bare form is 36 characters', 36, Length(Bare));
    AssertTrue('it reads back', TryStrToCurveInstanceId(Bare, Back));
    AssertTrue('as the same handle', SameCurveInstanceId(Id, Back));
end;

{ Refused, not guessed. Anything that parsed loosely would turn a typo in a URL
  into a write to whichever curve happened to match. }
procedure TCurveInstanceIdTest.MalformedTextIsRefusedRatherThanGuessed;
var
    Back: TCurveInstanceId;
    i:    longint;
    Bad:  array[0..6] of string = (
        '',
        '   ',
        'not-a-guid',
        '0',
        '{}',
        '{ff4e399c-c33c-482e-84d7-952700bcd4ae',
        'ff4e399c-c33c-482e-84d7-952700bcd4aeXX');
begin
    for i := 0 to High(Bad) do
        AssertFalse(Format('"%s" must not read as a handle', [Bad[i]]),
            TryStrToCurveInstanceId(Bad[i], Back));
end;

{ A handle addresses a curve in a URL path, and braces are not valid there. This
  is the guard that stops the two forms drifting apart - it round-trips the wire
  form back through the reader, which is exactly what the REST routes do. }
procedure TCurveInstanceIdTest.TheWireFormCarriesNoBraces;
var
    Id, Back: TCurveInstanceId;
    Wire: string;
begin
    Id := NewCurveInstanceId;
    Wire := CurveInstanceIdToWire(Id);

    AssertEquals('the wire form is 36 characters', 36, Length(Wire));
    AssertTrue('and carries no braces',
        (Pos('{', Wire) = 0) and (Pos('}', Wire) = 0));
    //  Nothing else in a URL path segment needs encoding either.
    AssertEquals('and nothing needing encoding', 0, Pos('/', Wire));

    AssertTrue('it reads back', TryStrToCurveInstanceId(Wire, Back));
    AssertTrue('as the same handle', SameCurveInstanceId(Id, Back));
end;

{ A caller that ignores the False must not be handed something that looks
  usable: the out parameter is left as the empty handle, which matches nothing
  in the registry. }
procedure TCurveInstanceIdTest.RefusedTextLeavesNoUsableHandle;
var
    Back: TCurveInstanceId;
begin
    Back := NewCurveInstanceId;
    AssertFalse('refused', TryStrToCurveInstanceId('rubbish', Back));
    AssertFalse('and the handle is no longer an identity',
        IsCurveInstanceId(Back));
end;

initialization
    //  A UNIT test: no process, no filesystem, no optimiser.
    RegisterTest('unit', TCurveInstanceIdTest);
end.
