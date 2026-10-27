// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The curve-type registry - and an honest note about its limits.)

WHAT THESE TESTS CAN AND CANNOT DO, stated up front because getting this wrong is
what let the original defect ship. This binary LINKS every curve unit, so any
assertion here that "the types are registered" is true by construction and would
pass even in a build where `fit_server` had none of them. That is precisely the
false guard that was written once, passed forever, and caught nothing.

So what is tested here is the registry's OWN logic - that the expected list is
real, that every entry resolves, that the check is repeatable - and the guard
that actually bites lives in `testcase_http_fit_service`, which drives the real
spawned server binary. Anything stronger than this belongs there, not here.
}
unit testcase_curve_type_registration;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    coordinate_axis, axis_mode_registry, axis_mode_registration,
    curve_types_singleton, int_curve_type_iterator,
    named_points_set, int_curve_factory, points_set, curve_points_set,
    test_curve_types, curve_type_registration;

type
    { THE PACK THIS SUITE DECLARES. The framework declares no curve type, and a
      module's must not be named here - so without one of its own, the bare
      suite would hold an empty list of expected types and every test of that
      list would be about nothing. A type of its own, declared under a pack name
      of its own, so no other test's types change owner by it: a model holds
      one owner's curves only (fit_advice), and the test types stay Standard. }
    TPackDeclaredPointsSet = class(TTestGaussPointsSet)
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
    end;

const
    TEST_PACK = 'Framework tests';

type
    TCurveTypeRegistrationTest = class(TTestCase)
    published
        procedure EveryCurveTypeRefusesToGrowOnceBuilt;
        { A background shape takes no peak's role and seeds itself from the
          data's baseline - for every registered one, including a module's. }
        procedure EveryBackgroundTypeHasNoPositionAndSeedsFromItsBaseline;
        procedure TheExpectedListIsNotEmpty;
        procedure TheExpectedListHasNoDuplicateNames;
        procedure ItCoversEveryDeclaredPack;
        procedure APackMustDeclareSomethingToVerify;
        procedure APackHoldingANilTypeIsRefusedAndLeavesNothing;
        procedure EveryExpectedTypeResolvesToAClass;
        procedure CheckingTwiceIsHarmless;
        procedure EveryTypePrefersOnlyModesThatExist;
    procedure IsAnalyticAgreesWithTheExpression;
        procedure APeakTypeIsPlacedFromACurvePosition;
        { Which module a curve type belongs to (fit-performance.md, "one module per model"). }
        procedure ATypeIsOwnedByThePackThatDeclaredIt;
        procedure ATypeNoPackDeclaredIsStandard;
    end;

implementation

class function TPackDeclaredPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian of a declared pack (test)';
end;

class function TPackDeclaredPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{5C0E7A93-2F64-4B1D-8E39-A47D61B2C0F8}');
end;

procedure TCurveTypeRegistrationTest.TheExpectedListIsNotEmpty;
var
    Names: TStringList;
begin
    //  An empty list would make RegisterAllCurveTypes a no-op that reports
    //  success - the check would be gone and nothing would say so.
    Names := ExpectedCurveTypeNames;
    try
        AssertTrue('the registry must expect some types', Names.Count > 0);
    finally
        Names.Free;
    end;
end;

procedure TCurveTypeRegistrationTest.TheExpectedListHasNoDuplicateNames;
var
    Names: TStringList;
    i, j, Dups: longint;
begin
    Names := ExpectedCurveTypeNames;
    try
        Dups := 0;
        for i := 0 to Names.Count - 1 do
            for j := i + 1 to Names.Count - 1 do
                if Names[i] = Names[j] then
                    Inc(Dups);
        //  A duplicated line is the shape a copy-paste error takes here, and it
        //  hides the type that was MEANT to be added on that line.
        AssertEquals('a name listed twice means one was pasted over another',
            0, Dups);
    finally
        Names.Free;
    end;
end;

{ Every pack's declared types are in the list, by class name. The framework
  declares none of its own; a module's are asserted where the module is tested,
  and this file must not name one, because a build need not contain it. }
procedure TCurveTypeRegistrationTest.ItCoversEveryDeclaredPack;
var
    Names: TStringList;
begin
    Names := ExpectedCurveTypeNames;
    try
        AssertTrue('the pack this suite declared is covered',
            Names.IndexOf('TPackDeclaredPointsSet') >= 0);
        AssertTrue('and nothing no pack declared is expected',
            Names.IndexOf('TTestGaussPointsSet') < 0);
    finally
        Names.Free;
    end;
end;

{ The refusals that keep a pack declaration from being decorative. Both are
  checks that would otherwise pass forever while verifying nothing - the exact
  shape of guard this file exists to warn about. }
{ A MODEL IS ONE MODULE'S, and which module a curve type belongs to is the pack
  that declared it - recorded once, where the pack names its types, so nothing
  has to list "type X belongs to Y" anywhere else. Asked of a table rather than
  of the registry: a pack registered here would stay registered for the rest of
  the run, and the start-up check would then report its type missing. }
procedure TCurveTypeRegistrationTest.ATypeIsOwnedByThePackThatDeclaredIt;
var
    Packs: TPackTypes;
begin
    SetLength(Packs, 2);
    Packs[0].Name := 'TTestPseudoVoigtPointsSet';
    Packs[0].Cls := TTestPseudoVoigtPointsSet;
    Packs[0].Pack := 'Spectra';
    Packs[1].Name := 'TTestFormulaPointsSet';
    Packs[1].Cls := TTestFormulaPointsSet;
    Packs[1].Pack := 'Formulae';
    AssertEquals('the pack that declared it', 'Spectra',
        CurveTypeOwnerIn(Packs, TTestPseudoVoigtPointsSet.GetCurveTypeId));
    AssertEquals('each its own', 'Formulae',
        CurveTypeOwnerIn(Packs, TTestFormulaPointsSet.GetCurveTypeId));
    AssertEquals('a type no pack declared is the framework''s',
        STANDARD_CURVE_OWNER,
        CurveTypeOwnerIn(Packs, TTestGaussPointsSet.GetCurveTypeId));
end;

{ And in the registry this binary has: a type no pack declared is Standard,
  and a declared one its pack's - whatever packs the build links. }
procedure TCurveTypeRegistrationTest.ATypeNoPackDeclaredIsStandard;
begin
    AssertEquals('a peak type', STANDARD_CURVE_OWNER,
        CurveTypeOwner(TTestGaussPointsSet.GetCurveTypeId));
    AssertEquals('a formula type', STANDARD_CURVE_OWNER,
        CurveTypeOwner(TTestFormulaPointsSet.GetCurveTypeId));
    AssertEquals('a declared one is its pack''s', TEST_PACK,
        CurveTypeOwner(TPackDeclaredPointsSet.GetCurveTypeId));
end;

procedure TCurveTypeRegistrationTest.APackMustDeclareSomethingToVerify;
var
    Raised: boolean;
begin
    Raised := False;
    try
        ExpectCurveTypes('pack-with-nothing-in-it', []);
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('a pack that expects no types must be refused', Raised);

    Raised := False;
    try
        ExpectCurveTypes('', [TTestGaussPointsSet]);
    except
        on E: Exception do
            Raised := True;
    end;
    //  The name is what makes the registration idempotent across hosts; without
    //  one, a pack registered by both client and server would be listed twice.
    AssertTrue('a pack must be named', Raised);
end;

{ A pack naming a type that is nil - a unit not linked, a class variable not
  yet set - is refused before any of it is kept, so the list every later check
  walks holds no empty slot. }
procedure TCurveTypeRegistrationTest.APackHoldingANilTypeIsRefusedAndLeavesNothing;
var
    Before: TStringList;
    Count: longint;
    Raised: boolean;
begin
    Before := ExpectedCurveTypeNames;
    try
        Count := Before.Count;
    finally
        Before.Free;
    end;
    Raised := False;
    try
        ExpectCurveTypes('pack-with-a-nil-type', [TTestGaussPointsSet, nil]);
    except
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
    Before := ExpectedCurveTypeNames;
    try
        AssertEquals('nothing of it kept', Count, Before.Count);
    finally
        Before.Free;
    end;
end;

procedure TCurveTypeRegistrationTest.EveryExpectedTypeResolvesToAClass;
begin
    //  In THIS binary, which links everything - so this asserts the ids in the
    //  list are the real ones, not that any particular binary is complete. A
    //  typo'd or stale entry would fail here and nowhere else, and it would
    //  otherwise break start-up for every user instead.
    RegisterAllCurveTypes;
end;

procedure TCurveTypeRegistrationTest.CheckingTwiceIsHarmless;
begin
    //  It is called from the client, the server and the runner, and a binary
    //  that is both would call it twice. It must be a check, not a mutation.
    RegisterAllCurveTypes;
    RegisterAllCurveTypes;
    AssertTrue('the registry is still complete after a second check',
        FindCurveClassById(TPackDeclaredPointsSet.GetCurveTypeId) <> nil);
end;

{ The default has to stay empty, and this is what says so. PlacedByPointSet
  decides which path the engine takes to build a model, so a peak type that
  started answering anything else would be built from an extent it has no picks
  for - and would simply stop being creatable, with nothing failing here. }
procedure TCurveTypeRegistrationTest.APeakTypeIsPlacedFromACurvePosition;
begin
    AssertEquals('a Gaussian is placed from one curve position, not an extent',
        '', TTestGaussPointsSet.PlacedByPointSet);
end;

{ IsAnalytic must not contradict the expression.

  GetCurveExpression is the formula the out-of-process engines evaluate;
  IsAnalytic is asked of the CLASS - before any instance exists - to decide
  whether those engines are offered at all. A type that overrides one and forgets
  the other is offered an engine that cannot fit it, or denied one that can, and
  the fit then depends on which engine ran it - both answers looking plausible.

  ONE DIRECTION IS ASSERTABLE AND THE OTHER IS NOT, which is worth knowing before
  trying to strengthen this. "Has a formula, so it must be analytic" always holds.
  The converse does not: a user-defined curve is analytic BY NATURE - it is
  nothing but a formula - yet a freshly constructed one has no expression until
  the user types it. Asserting the converse would fail on the one type that is
  most obviously analytic, so this asserts what is true rather than what is
  symmetrical.

  Walked over the whole registry, so a type added later is covered without this
  test being edited. }
procedure TCurveTypeRegistrationTest.IsAnalyticAgreesWithTheExpression;
var
    Iter: ICurveTypeIterator;
    Cls:  TCurveClass;
    Inst: TNamedPointsSet;
    HasFormula: boolean;
begin
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        Cls := Iter.GetCurrentCurveClass;
        //  The expression is an instance method - it may depend on the
        //  parameters a curve was built with - so one instance is made per type.
        Inst := Cls.Create(nil);
        try
            HasFormula := Inst.GetCurveExpression <> '';
            if HasFormula then
                AssertTrue(Iter.GetCurveTypeName +
                    ' has an expression, so IsAnalytic must be True',
                    Cls.IsAnalytic);
            if not Cls.IsAnalytic then
                AssertEquals(Iter.GetCurveTypeName +
                    ' is not analytic, so it must have no expression',
                    '', Inst.GetCurveExpression);
        finally
            Inst.Free;
        end;
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

{ WHAT THE ENGINE ASSUMES OF EVERY BACKGROUND, walked over the registry so a
  module's own background shape is held to it without this test being edited.

  No position, amplitude or width: each of those is read as "a peak" -
  seeded from a pick, deleted by curve reduction when small, drawn with a
  marker. A formula, so every engine can fit it. And a seed from the data's own
  baseline that actually follows it: a flat baseline of 50 has to come out as a
  curve at 50, whatever the shape. }
procedure TCurveTypeRegistrationTest.
    EveryBackgroundTypeHasNoPositionAndSeedsFromItsBaseline;
var
    Iter: ICurveTypeIterator;
    Cls:  TCurveClass;
    Inst: TNamedPointsSet;
    Profile: TPointsSet;
    Found: boolean;
    i: longint;
begin
    Found := False;
    Profile := TPointsSet.Create(nil);
    try
        for i := 1 to 9 do
            Profile.AddNewPoint(i, 50);
        Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
        Iter.FirstCurveType;
        while True do
        begin
            Cls := Iter.GetCurrentCurveClass;
            if Cls.IsBackground then
            begin
                Found := True;
                Inst := Cls.Create(nil);
                try
                    AssertFalse(Iter.GetCurveTypeName + ' has no position', Inst.Hasx0);
                    AssertFalse(Iter.GetCurveTypeName + ' has no amplitude', Inst.HasA);
                    AssertFalse(Iter.GetCurveTypeName + ' has no width', Inst.HasSigma);
                    AssertTrue(Iter.GetCurveTypeName + ' has a formula',
                        Inst.GetCurveExpression <> '');
                    Inst.SeedFromBaseline([1, 3, 5, 7, 9], [50, 50, 50, 50, 50]);
                    Inst.SetWindow(Profile, 0, Profile.PointsCount - 1);
                    Inst.ReCalc;
                    for i := 0 to Inst.PointsCount - 1 do
                        AssertEquals(Iter.GetCurveTypeName +
                            ' seeded on a flat baseline follows it', 50,
                            Inst.PointYCoord[i], 1e-3);
                finally
                    Inst.Free;
                end;
            end;
            if Iter.EndCurveType then Break
            else Iter.NextCurveType;
        end;
    finally
        Profile.Free;
    end;
    AssertTrue('at least one background shape is registered', Found);
end;

procedure TCurveTypeRegistrationTest.EveryCurveTypeRefusesToGrowOnceBuilt;
var
    Iter: ICurveTypeIterator;
    Cls:  TCurveClass;
    Inst: TNamedPointsSet;
    Profile: TPointsSet;
    i: longint;
    Refused: boolean;
begin
    //  A curve's points ARE its extent, decided when it is built. One added
    //  afterwards would shift every later point relative to the profile, and the
    //  summation would put the curve in the wrong place - silently, because the
    //  fit still converges, to a slightly wrong answer.
    //
    //  Pascal cannot un-inherit AddNewPoint from TPointsSet, so the guarantee is
    //  a run-time refusal. WALKING THE REGISTRY is what makes that adequate: it
    //  holds for types nobody has written yet, which is the half a hand-written
    //  list of types cannot do. The compile-time form needs a curve to stop
    //  BEING a TPointsSet.
    Profile := TPointsSet.Create(nil);
    try
        for i := 0 to 9 do
            Profile.AddNewPoint(i, 0);

        Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
        Iter.FirstCurveType;
        while True do
        begin
            Cls := Iter.GetCurrentCurveClass;
            Inst := Cls.Create(nil);
            try
                //  Before a window there is nothing to protect, and the builders
                //  legitimately fill a curve then.
                Inst.AddNewPoint(0, 0);
                Inst.SetWindow(Profile, 2, 5);
                AssertEquals(Iter.GetCurveTypeName + ' takes its window''s samples',
                    4, Inst.PointsCount);

                Refused := False;
                try
                    Inst.AddNewPoint(99, 1);
                except
                    Refused := True;
                end;
                AssertTrue(Iter.GetCurveTypeName +
                    ' must refuse a point once its window is set', Refused);
            finally
                Inst.Free;
            end;
            if Iter.EndCurveType then Break
            else Iter.NextCurveType;
        end;
    finally
        Profile.Free;
    end;
end;

procedure TCurveTypeRegistrationTest.EveryTypePrefersOnlyModesThatExist;
var
    Iter: ICurveTypeIterator;
    D: TAxisDimension;
    Preferred: string;
begin
    //  Walks the whole registry, so a curve type added later cannot arrive
    //  naming an axis mode nobody registered - the automatic rule passes such a
    //  preference over, so it would say nothing and nobody would notice.
    RegisterAllAxisModes;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        for D := Low(TAxisDimension) to High(TAxisDimension) do
        begin
            Preferred := Iter.GetCurrentCurveClass.PreferredAxisMode(D);
            if Preferred <> '' then
                AssertTrue(Iter.GetCurveTypeName + ' prefers "' + Preferred +
                    '" for its ' + DimensionWord(D) + ', which no mode shows',
                    AxisModeShows(Preferred, D));
            //  AND ITS FALLBACK, which the rule passes over just as silently.
            Preferred := Iter.GetCurrentCurveClass.FallbackAxisMode(D);
            if Preferred <> '' then
                AssertTrue(Iter.GetCurveTypeName + ' falls back to "' +
                    Preferred + '" for its ' + DimensionWord(D) +
                    ', which no mode shows', AxisModeShows(Preferred, D));
        end;
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TPackDeclaredPointsSet);
    ExpectCurveTypes(TEST_PACK, [TPackDeclaredPointsSet]);
    RegisterTest('unit', TCurveTypeRegistrationTest);
end.
