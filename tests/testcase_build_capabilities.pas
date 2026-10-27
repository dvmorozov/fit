// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a build can ever offer, decided from what registered.)

A COMMAND THE BUILD CAN NEVER USE IS HIDDEN; one the model does not use yet is
greyed (action_state). The two are different questions: a greyed Background menu
in a build with no background shape at all says "not now" about something that
will never be, and invites a user to look for how to enable it. Whether the
build has a background shape, a type placed from curve positions, or an axis
that reads a wavelength is a capability of what registered - asked once, here,
of the types and the modes, never of a list of products.
}
unit testcase_build_capabilities;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, action_state, int_curve_factory,
    named_points_set, axis_mode_registry, axis_mode_registration,
    test_axis_modes, test_curve_types, int_fit_service,
    curve_types_singleton, problem_value_store,
    build_capabilities;

type
    { A type a module places itself, from a point set of its own - as a wave
      pattern is placed from its markup. }
    TMarkedPointsSet = class(TTestGaussPointsSet)
    public
        class function PlacedByPointSet: string; override;
    end;

    { A type that is a formula, as the user's own is - declared here so the
      capability is asked of a class this test alone holds. }
    TFormulaTestPointsSet = class(TTestGaussPointsSet)
    public
        class function GetCurveTypeId: TCurveTypeId; override;
        class function IsDefinedByFormula: boolean; override;
    end;

    { Where a module keeps a value for the problem - here nowhere at all: the
      capability asks only whether a store is registered. }
    TNoValueStore = class(TObject, IProblemValueStore)
    public
        function Get(AService: IFitService): double;
        procedure Put(AService: IFitService; AValue: double);
    end;

    TBuildCapabilitiesTest = class(TTestCase)
    published
        procedure ABackgroundShapeOffersTheBackgroundCommands;
        procedure WithoutOneTheBuildOffersNoneOfThem;
        procedure CurvePositionsFollowATypePlacedFromThem;
        procedure TheWavelengthFollowsAModeThatReadsIt;
        procedure EveryOtherCommandIsOfferedByEveryBuild;
        procedure TheMenusThatHoldThemFollowTheSameAnswers;
        procedure TheBackgroundPointsTableFollowsTheBackground;
        procedure TheUserFormulaFollowsAFormulaType;
        procedure TheUserFormulaIsBuiltWithTheFirstFormulaType;
        procedure TheRegistrysFormulaTypeIsTheOneItsClassesName;
        procedure WhatRegisteredOffersAllThree;
        procedure EveryControlItAnswersForFollowsTheBuild;
        procedure TheBackgroundFractionFollowsAModuleThatKeepsOne;
    end;

implementation

var
    NoValueStore: TNoValueStore;

{$hints off}
function TNoValueStore.Get(AService: IFitService): double;
begin
    Result := 0;
end;

procedure TNoValueStore.Put(AService: IFitService; AValue: double);
begin
end;
{$hints on}

class function TFormulaTestPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{4E7A2C19-8B3D-4F60-A5E1-2D9C6B0F7A38}');
end;

class function TFormulaTestPointsSet.IsDefinedByFormula: boolean;
begin
    Result := True;
end;

class function TMarkedPointsSet.PlacedByPointSet: string;
begin
    Result := 'test.markup';
end;

function ModeInfos(const AModes: array of TAxisModeClass): TAxisModeInfos;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(AModes));
    for i := 0 to High(AModes) do
        Result[i] := AModes[i].Info;
end;

procedure TBuildCapabilitiesTest.ABackgroundShapeOffersTheBackgroundCommands;
var
    O: TBuildOffers;
begin
    O := BuildOffersOf([TTestGaussPointsSet, TTestLinearBackgroundPointsSet], ModeInfos([TPositionAxisMode]));
    AssertTrue(boBackground in O);
    AssertTrue(CommandOffered(ucSubtractBackground, O));
    AssertTrue(CommandOffered(ucSubtractBackgroundBySelectedPoints, O));
    AssertTrue(CommandOffered(ucBackground, O));
end;

procedure TBuildCapabilitiesTest.WithoutOneTheBuildOffersNoneOfThem;
var
    O: TBuildOffers;
begin
    O := BuildOffersOf([TTestGaussPointsSet], ModeInfos([TPositionAxisMode]));
    AssertFalse(boBackground in O);
    AssertFalse(CommandOffered(ucSubtractBackground, O));
    AssertFalse(CommandOffered(ucSubtractBackgroundBySelectedPoints, O));
    AssertFalse(CommandOffered(ucBackground, O));
end;

procedure TBuildCapabilitiesTest.CurvePositionsFollowATypePlacedFromThem;
begin
    AssertTrue('a peak is placed at a curve position',
        boCurvePositions in BuildOffersOf([TTestGaussPointsSet], nil));
    AssertFalse('a type placed from its own point set is not, nor is a background',
        boCurvePositions in BuildOffersOf([TMarkedPointsSet, TTestLinearBackgroundPointsSet], nil));
    AssertFalse(CommandOffered(ucCurvePositions, BuildOffersOf([TMarkedPointsSet], nil)));
end;

procedure TBuildCapabilitiesTest.TheWavelengthFollowsAModeThatReadsIt;
begin
    AssertTrue(boAxisValues in BuildOffersOf([], ModeInfos([TPositionAxisMode, TTestPlainAxisMode])));
    AssertFalse(boAxisValues in BuildOffersOf([], ModeInfos([TPositionAxisMode, TValueAxisMode])));
end;

procedure TBuildCapabilitiesTest.EveryOtherCommandIsOfferedByEveryBuild;
var
    C: TUiCommand;
begin
    for C := Low(TUiCommand) to High(TUiCommand) do
        if not (C in [ucSubtractBackground, ucSubtractBackgroundBySelectedPoints,
            ucBackground, ucCurvePositions]) then
            AssertTrue('offered by any build', CommandOffered(C, []));
end;

procedure TBuildCapabilitiesTest.TheMenusThatHoldThemFollowTheSameAnswers;
begin
    AssertFalse(ControlOffered('MenuBackground', []));
    AssertFalse(ControlOffered('MenuCurvePositions', []));
    AssertFalse(ControlOffered('MenuSetRuleParameters', []));
    AssertTrue(ControlOffered('MenuBackground', [boBackground]));
    AssertTrue(ControlOffered('MenuCurvePositions', [boCurvePositions]));
    AssertTrue(ControlOffered('MenuSetRuleParameters', [boAxisValues]));
    AssertTrue('any other menu is offered', ControlOffered('MenuFit', []));
end;

{ The table of the background points is the background's too: a build with
  no background shape has no points to list, and an empty tab beside the
  model's tables says there could be some. }
procedure TBuildCapabilitiesTest.TheBackgroundPointsTableFollowsTheBackground;
begin
    AssertFalse(ControlOffered('TabSheetBackground', []));
    AssertTrue(ControlOffered('TabSheetBackground', [boBackground]));
end;

{ The user's formulas - the User group, New User Curve, the formulas saved
  under it - are the formula type's: a build without one (the scientific
  module left out) has no shape to give a formula, and a menu that offered
  one would build nothing. }
procedure TBuildCapabilitiesTest.TheUserFormulaFollowsAFormulaType;
begin
    AssertTrue(boUserFormula in BuildOffersOf([TFormulaTestPointsSet], nil));
    AssertFalse(boUserFormula in BuildOffersOf([TTestGaussPointsSet, TMarkedPointsSet], nil));
end;

{ What Model > Curve Type > User builds a formula with: the formula type the
  build registered, found by its capability, so the window names no class. }
procedure TBuildCapabilitiesTest.TheUserFormulaIsBuiltWithTheFirstFormulaType;
begin
    AssertEquals('none without a formula type', GUIDToString(GUID_NULL),
        GUIDToString(UserFormulaIn([TTestGaussPointsSet, TTestLinearBackgroundPointsSet])));
    AssertEquals('the formula type where there is one',
        GUIDToString(TFormulaTestPointsSet.GetCurveTypeId),
        GUIDToString(UserFormulaIn([TTestGaussPointsSet, TFormulaTestPointsSet])));
end;

{ A build's window follows what registered: this suite registers a
  background and a peak (test_curve_types) and modes that read a number
  (test_axis_modes), as a module's front door does - so it offers all three. }
procedure TBuildCapabilitiesTest.WhatRegisteredOffersAllThree;
begin
    RegisterAllAxisModes;
    RegisterTestAxisModes;
    AssertTrue(boBackground in RegisteredBuildOffers);
    AssertTrue(boCurvePositions in RegisteredBuildOffers);
    AssertTrue(boAxisValues in RegisteredBuildOffers);
end;

{ The designed controls the window hides by capability: each is hidden by a
  build that offers nothing and shown by one that offers everything, so a
  name listed there that ControlOffered does not decide fails here. }
procedure TBuildCapabilitiesTest.EveryControlItAnswersForFollowsTheBuild;
var
    Name: string;
begin
    AssertEquals(5, Length(CapabilityControls));
    for Name in CapabilityControls do
    begin
        AssertFalse(Name + ' hidden', ControlOffered(Name, []));
        AssertTrue(Name + ' shown', ControlOffered(Name,
            [boBackground, boCurvePositions, boAxisValues, boUserFormula,
             boBackgroundFraction]));
    end;
end;

{ Set Background Fraction sets the fraction a module's search stops at, asked
  through the dialog that module's client registers: a build with a background
  shape and no module keeping a fraction would offer a command with nothing
  behind it. The menu around it still follows the background. }
procedure TBuildCapabilitiesTest.TheBackgroundFractionFollowsAModuleThatKeepsOne;
begin
    AssertFalse('a background alone', ControlOffered('MenuSetBackgroundFraction',
        [boBackground]));
    AssertTrue(ControlOffered('MenuSetBackgroundFraction',
        [boBackground, boBackgroundFraction]));
    //  A module that keeps one registers its store, as the scientific
    //  module's front door does. Only where none is registered yet: the last
    //  registration wins, and a suite linking that module must keep its own.
    if not Assigned(ProblemValueStore(BackgroundFractionValue)) then
        RegisterProblemValueStore(BackgroundFractionValue, NoValueStore);
    AssertTrue(boBackgroundFraction in RegisteredBuildOffers);
end;

{ What the window asks: the same answer over the registry as over the classes
  it holds, so the window and the build capability never disagree. }
procedure TBuildCapabilitiesTest.TheRegistrysFormulaTypeIsTheOneItsClassesName;
begin
    AssertEquals(GUIDToString(UserFormulaIn(RegisteredCurveClasses)),
        GUIDToString(UserFormulaCurveTypeId));
end;

initialization
    NoValueStore := TNoValueStore.Create;
    RegisterTest('unit', TBuildCapabilitiesTest);
finalization
    NoValueStore.Free;
end.
