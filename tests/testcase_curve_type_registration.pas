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
    argument_axis, curve_types_singleton, int_curve_type_iterator,
    named_points_set, int_curve_factory, points_set,
    gauss_points_set, voigt_points_set, user_points_set, curve_type_registration;

type
    TCurveTypeRegistrationTest = class(TTestCase)
    published
        procedure EveryCurveTypeRefusesToGrowOnceBuilt;
        procedure TheExpectedListIsNotEmpty;
        procedure TheExpectedListHasNoDuplicateNames;
        procedure ItCoversTheFrameworkTypes;
        procedure APackMustDeclareSomethingToVerify;
        procedure EveryExpectedTypeResolvesToAClass;
        procedure CheckingTwiceIsHarmless;
        procedure EveryTypeDefinesAUsableArgumentAxis;
    procedure IsAnalyticAgreesWithTheExpression;
        procedure APeakTypeIsPlacedFromACurvePosition;
    end;

implementation

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

{ The framework's own types - the ones every build has, whatever packs it ships.
  A pack's types are the pack's own business and are asserted where the pack is
  tested; this file must not name one, because a build need not contain it. }
procedure TCurveTypeRegistrationTest.ItCoversTheFrameworkTypes;
var
    Names: TStringList;
begin
    Names := ExpectedCurveTypeNames;
    try
        AssertTrue('the ordinary peaks are covered',
            Names.IndexOf('TGaussPointsSet') >= 0);
        AssertTrue('and the formula-defined user curve',
            Names.IndexOf('TUserPointsSet') >= 0);
        AssertTrue('and a line shape added later',
            Names.IndexOf('TVoigtPointsSet') >= 0);
    finally
        Names.Free;
    end;
end;

{ The refusals that keep a pack declaration from being decorative. Both are
  checks that would otherwise pass forever while verifying nothing - the exact
  shape of guard this file exists to warn about. }
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
        ExpectCurveTypes('', [TGaussPointsSet]);
    except
        on E: Exception do
            Raised := True;
    end;
    //  The name is what makes the registration idempotent across hosts; without
    //  one, a pack registered by both client and server would be listed twice.
    AssertTrue('a pack must be named', Raised);
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
        FindCurveClassById(TGaussPointsSet.GetCurveTypeId) <> nil);
end;

{ The default has to stay empty, and this is what says so. PlacedByPointSet
  decides which path the engine takes to build a model, so a peak type that
  started answering anything else would be built from an extent it has no picks
  for - and would simply stop being creatable, with nothing failing here. }
procedure TCurveTypeRegistrationTest.APeakTypeIsPlacedFromACurvePosition;
begin
    AssertEquals('a Gaussian is placed from one curve position, not an extent',
        '', TGaussPointsSet.PlacedByPointSet);
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
    //  BEING a TPointsSet - see the roadmap.
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

procedure TCurveTypeRegistrationTest.EveryTypeDefinesAUsableArgumentAxis;
const
    //  A representative wavelength (Cu K-alpha) and an abscissa value inside
    //  the range every axis in the app is defined on.
    CuKa = 1.54056;
    Raw  = 37.25;
var
    Iter: ICurveTypeIterator;
    Axis: TArgumentAxis;
begin
    //  Walks the whole registry, so a curve type added later cannot arrive with
    //  a broken or missing axis: its positions would then be captioned wrongly
    //  in the chart and mis-stored when edited in the parameters grid.
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        Axis := Iter.GetCurrentCurveClass.CreatePreferredAxis(CuKa);
        try
            AssertTrue(Iter.GetCurveTypeName + ' must define an axis',
                Assigned(Axis));
            AssertTrue(Iter.GetCurveTypeName + ' axis must have a caption',
                Axis.DisplayName <> '');
            //  The grid shows ToDisplay and stores back FromDisplay; if the two
            //  are not exact inverses, merely looking at a value moves it.
            AssertEquals(Iter.GetCurveTypeName + ' axis must round-trip',
                Raw, Axis.FromDisplay(Axis.ToDisplay(Raw)), 1e-9);
        finally
            Axis.Free;
        end;
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

initialization
    RegisterTest('unit', TCurveTypeRegistrationTest);
end.
