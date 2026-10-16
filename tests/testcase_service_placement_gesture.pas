// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a refusal names the gesture the user should have made instead.)

WHERE THIS IS USED. `SelectAllPointsAsCurvePositions` refuses outright for a
curve type that is placed by a module's own markup - one pattern per profile
sample is not a model, it is a hang - and the refusal has to say what to do
instead. What that IS depends on the pack, so the engine asks: the curve class
names the point set it is placed by, and the module owning that set says what it
calls the gesture.

WHY THE FALLBACKS MATTER, which is what this suite covers. The two guards are
reached in an application that has been rebuilt or reconfigured under the user:
a settings file naming a curve type this build no longer registers, and a type
whose module is absent. Neither can be allowed to produce an empty sentence -
"Mark it by its two ends instead, with the  command." - which is what a missing
answer would leave behind, and which reads as a bug in the program rather than
as a refusal the user can act on.

The gesture itself is asked of the module, so the ONE case where a real pack
answers belongs in that pack's own suite, which asserts that the refusal names
the gesture that pack understands. Here is the engine's half - what it says when nobody answers.
}
unit testcase_service_placement_gesture;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, gauss_points_set, curve_instance_id;

type
    { PlacementGestureName is protected: it is an implementation detail of one
      refusal, not part of the contract a client holds. Reaching it needs a
      descendant, which is cheaper and more honest than making it public for a
      test's sake. }
    TServiceForGestures = class(TFitService)
    public
        function CallPlacementGestureName: string;
        { Puts a curve type id in place WITHOUT the registry check SetCurveType
          makes - which is the whole point: this is the state an application
          reaches by reading a settings file written by a build that had a type
          this one does not. }
        procedure ForceCurveTypeId(const AId: TGUID);
    end;

    TServicePlacementGestureTest = class(TTestCase)
    private
        FService: TServiceForGestures;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnOrdinaryCurveTypeIsMarkedRatherThanNamedAfterAPack;
        procedure ATypeThisBuildDoesNotRegisterStillNamesAGesture;
        procedure TheAnswerIsNeverEmpty;
    end;

implementation

function TServiceForGestures.CallPlacementGestureName: string;
begin
    Result := PlacementGestureName;
end;

procedure TServiceForGestures.ForceCurveTypeId(const AId: TGUID);
begin
    FCurveTypeId := AId;
end;

procedure TServicePlacementGestureTest.SetUp;
begin
    FService := TServiceForGestures.Create;
end;

procedure TServicePlacementGestureTest.TearDown;
begin
    FreeAndNil(FService);
end;

procedure TServicePlacementGestureTest.AnOrdinaryCurveTypeIsMarkedRatherThanNamedAfterAPack;
var
    Svc: IFitService;
begin
    //  A GAUSSIAN IS PLACED BY PICKING, and no module owns that gesture, so the
    //  engine's own word for it is what comes back.
    Svc := FService;
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    AssertEquals('marking', FService.CallPlacementGestureName);
end;

procedure TServicePlacementGestureTest.ATypeThisBuildDoesNotRegisterStillNamesAGesture;
begin
    //  THE STATE A REBUILD LEAVES BEHIND: a settings file names a curve type
    //  that was registered by the build that wrote it and is not registered
    //  here. The refusal must still read as a sentence.
    FService.ForceCurveTypeId(
        StringToGUID('{9E1D4C7A-0000-0000-0000-00000000FFFF}'));
    AssertEquals('marking', FService.CallPlacementGestureName);
end;

procedure TServicePlacementGestureTest.TheAnswerIsNeverEmpty;
var
    Svc: IFitService;
begin
    //  THE INVARIANT BEHIND BOTH, and the one worth stating on its own: the
    //  sentence this goes into reads "...with the X command", so an empty
    //  answer is a defect the user sees as a broken message rather than as a
    //  refusal.
    AssertTrue('with nothing chosen at all',
        Trim(FService.CallPlacementGestureName) <> '');
    Svc := FService;
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    AssertTrue('and with a type chosen',
        Trim(FService.CallPlacementGestureName) <> '');
end;

initialization
    //  The engine in process, no server and no module: a unit test.
    RegisterTest('unit', TServicePlacementGestureTest);
end.
