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
    diffraction_axis_modes, gauss_points_set, polynomial_background_points_set,
    curve_types_singleton,
    build_capabilities;

type
    { A type a module places itself, from a point set of its own - as a wave
      pattern is placed from its markup. }
    TMarkedPointsSet = class(TGaussPointsSet)
    public
        class function PlacedByPointSet: string; override;
    end;

    { A type that is a formula, as the user's own is - declared here because
      the user's type links the LCL, and this suite builds without it. }
    TFormulaTestPointsSet = class(TGaussPointsSet)
    public
        class function GetCurveTypeId: TCurveTypeId; override;
        class function IsDefinedByFormula: boolean; override;
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
        procedure FreeFitsShapesAndAnglesOfferAllThree;
        procedure EveryControlItAnswersForFollowsTheBuild;
    end;

implementation

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
    O := BuildOffersOf([TGaussPointsSet, TQuadraticBackgroundPointsSet], ModeInfos([TPositionAxisMode]));
    AssertTrue(boBackground in O);
    AssertTrue(CommandOffered(ucSubtractBackground, O));
    AssertTrue(CommandOffered(ucSubtractBackgroundBySelectedPoints, O));
    AssertTrue(CommandOffered(ucBackground, O));
end;

procedure TBuildCapabilitiesTest.WithoutOneTheBuildOffersNoneOfThem;
var
    O: TBuildOffers;
begin
    O := BuildOffersOf([TGaussPointsSet], ModeInfos([TPositionAxisMode]));
    AssertFalse(boBackground in O);
    AssertFalse(CommandOffered(ucSubtractBackground, O));
    AssertFalse(CommandOffered(ucSubtractBackgroundBySelectedPoints, O));
    AssertFalse(CommandOffered(ucBackground, O));
end;

procedure TBuildCapabilitiesTest.CurvePositionsFollowATypePlacedFromThem;
begin
    AssertTrue('a peak is placed at a curve position',
        boCurvePositions in BuildOffersOf([TGaussPointsSet], nil));
    AssertFalse('a type placed from its own point set is not, nor is a background',
        boCurvePositions in BuildOffersOf([TMarkedPointsSet, TQuadraticBackgroundPointsSet], nil));
    AssertFalse(CommandOffered(ucCurvePositions, BuildOffersOf([TMarkedPointsSet], nil)));
end;

procedure TBuildCapabilitiesTest.TheWavelengthFollowsAModeThatReadsIt;
begin
    AssertTrue(boWaveLength in BuildOffersOf([], ModeInfos([TPositionAxisMode, TTwoThetaAxisMode])));
    AssertFalse(boWaveLength in BuildOffersOf([], ModeInfos([TPositionAxisMode, TValueAxisMode])));
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
    AssertTrue(ControlOffered('MenuSetRuleParameters', [boWaveLength]));
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
    AssertFalse(boUserFormula in BuildOffersOf([TGaussPointsSet, TMarkedPointsSet], nil));
end;

{ What Model > Curve Type > User builds a formula with: the formula type the
  build registered, found by its capability, so the window names no class. }
procedure TBuildCapabilitiesTest.TheUserFormulaIsBuiltWithTheFirstFormulaType;
begin
    AssertEquals('none without a formula type', GUIDToString(GUID_NULL),
        GUIDToString(UserFormulaIn([TGaussPointsSet, TQuadraticBackgroundPointsSet])));
    AssertEquals('the formula type where there is one',
        GUIDToString(TFormulaTestPointsSet.GetCurveTypeId),
        GUIDToString(UserFormulaIn([TGaussPointsSet, TFormulaTestPointsSet])));
end;

{ Free Fit's build has the scientific module's backgrounds and peaks (this
  suite links them) and the diffraction angles, which its front door
  registers - so its window is the one it always was. }
procedure TBuildCapabilitiesTest.FreeFitsShapesAndAnglesOfferAllThree;
begin
    RegisterAllAxisModes;
    RegisterDiffractionAxisModes;
    AssertTrue(boBackground in RegisteredBuildOffers);
    AssertTrue(boCurvePositions in RegisteredBuildOffers);
    AssertTrue(boWaveLength in RegisteredBuildOffers);
end;

{ The designed controls the window hides by capability: each is hidden by a
  build that offers nothing and shown by one that offers everything, so a
  name listed there that ControlOffered does not decide fails here. }
procedure TBuildCapabilitiesTest.EveryControlItAnswersForFollowsTheBuild;
var
    Name: string;
begin
    AssertEquals(4, Length(CapabilityControls));
    for Name in CapabilityControls do
    begin
        AssertFalse(Name + ' hidden', ControlOffered(Name, []));
        AssertTrue(Name + ' shown', ControlOffered(Name,
            [boBackground, boCurvePositions, boWaveLength, boUserFormula]));
    end;
end;

{ What the window asks: the same answer over the registry as over the classes
  it holds, so the window and the build capability never disagree. }
procedure TBuildCapabilitiesTest.TheRegistrysFormulaTypeIsTheOneItsClassesName;
begin
    AssertEquals(GUIDToString(UserFormulaIn(RegisteredCurveClasses)),
        GUIDToString(UserFormulaCurveTypeId));
end;

initialization
    RegisterTest('unit', TBuildCapabilitiesTest);
end.
