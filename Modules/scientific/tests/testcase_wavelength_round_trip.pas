// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A wavelength set in the window survives saving and reopening the
project.)

IT DID NOT (found 2026-10-17, fixed with the move, at the user's direction):
the dialog set it in the client alone, a save took the server's - never set
by the dialog - and an open set the server's but not the client's, which the
axes read. The wavelength is now the scientific module's per-problem state:
the client writes and reads it through the module's resource, the module's
session keeps it in the module's own part of the project, and a restore's
re-read takes it back into the client.

THROUGH THE SURFACE THE APPLICATION USES: the client over the REST router to
a real engine, the project captured and applied by the code File > Save
Project and File > Open Project run, and the client's re-read
(FollowEngineAxisValues, the step ResyncFromService takes after a restore).

AND A FILE WRITTEN BEFORE THE MOVE STILL OPENS AS IT WAS SAVED: its wavelength
among the framework's settings and its argument axis as the display-mode
integer 2 (fixtures/project-before-2026-10-17, the three parts such a file
holds). Since the framework names neither the wavelength nor the angles, both
reach the window only through what this module registers - the problem value
the old member is handed to, and the map of the old integers - so the test
enters at the front door and reads the parts as File > Open Project does.
}
unit testcase_wavelength_round_trip;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_client, fit_rest_api,
    mock_loopback_fit_service, mock_fit_viewer, fit_project_document,
    fit_project_session, fit_project_archive, fit_project_json,
    project_ui_context, coordinate_axis, axis_mode_registry, axis_choice,
    chart_axes, axis_mode_registration, diffraction_axis_modes,
    scientific_module;

type
    { Two problems, each with a client talking to a real engine over the REST
      router. }
    TWavelengthProblemsTest = class(TTestCase)
    protected
        FApi, FApi2: TFitRestApi;
        FSvc, FSvc2: TLoopbackFitService;
        FView, FView2: TMockFitViewer;
        FClient, FClient2: TFitClient;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    end;

    TWavelengthRoundTripTest = class(TWavelengthProblemsTest)
    published
        procedure AWavelengthSetInTheWindowIsSavedAndReopened;
    end;

    { The same surface over a file on disk, so an integration test. }
    TFormerWavelengthProjectTest = class(TWavelengthProblemsTest)
    published
        procedure AFileWrittenBeforeTheMoveOpensWithItsWavelengthAndItsAxis;
    end;

implementation

procedure TWavelengthProblemsTest.SetUp;
begin
    //  As the window does: the framework's own axis modes first, then the
    //  module - before any problem is made, since each takes a session of
    //  every module registered by then.
    RegisterAllAxisModes;
    RegisterScientificModule;
    FApi := TFitRestApi.Create;
    FSvc := TLoopbackFitService.Create(FApi);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FApi2 := TFitRestApi.Create;
    FSvc2 := TLoopbackFitService.Create(FApi2);
    FView2 := TMockFitViewer.Create;
    FClient2 := TFitClient.Create;
    FClient2.FitService := FSvc2;
    FClient2.FFitViewer := FView2;
end;

procedure TWavelengthProblemsTest.TearDown;
begin
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FClient2.FFitViewer := nil;
    FClient2.FitService := nil;
    FreeAndNil(FClient2);
    FreeAndNil(FView);
    FreeAndNil(FView2);
    FreeAndNil(FSvc);
    FreeAndNil(FSvc2);
    FreeAndNil(FApi);
    FreeAndNil(FApi2);
end;

procedure TWavelengthRoundTripTest.AWavelengthSetInTheWindowIsSavedAndReopened;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    //  What the window's dialog does with the number it accepted.
    FClient.SetAxisValue(WavelengthParameter, 1.5406);
    Doc := CaptureProject(FSvc, EmptyProjectClientContext, EmptyProjectDocument);
    AssertTrue(Fault, ApplyProject(FSvc2, Doc, Fault));
    FClient2.FollowEngineAxisValues;
    AssertEquals('the axes read it again', 1.5406,
        AxisValueOf(FClient2.GetAxisValues, WavelengthParameter), 1e-9);
end;

function FixturePart(const AName: string): TProjectPart;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(ExpandFileName(ExtractFilePath(ParamStr(0)) +
            '../Modules/scientific/tests/fixtures/project-before-2026-10-17/' +
            AName));
        Result.Name := AName;
        Result.Content := Lines.Text;
    finally
        Lines.Free;
    end;
end;

procedure TFormerWavelengthProjectTest.AFileWrittenBeforeTheMoveOpensWithItsWavelengthAndItsAxis;
var
    Parts: TProjectParts;
    Doc: TProjectDocument;
    Plan: TProjectUiPlan;
    Axes: TChartAxes;
    Fault: string;
begin
    Parts := nil;
    SetLength(Parts, 3);
    Parts[0] := FixturePart(ProjectManifestPart);
    Parts[1] := FixturePart(ProjectProblemPart);
    Parts[2] := FixturePart(ProjectUiPart);
    AssertTrue(Fault, ProjectFromParts(Parts, Doc, Fault));

    //  The problem, as File > Open Project restores it, and the client's
    //  re-read after the restore.
    AssertTrue(Fault, ApplyProject(FSvc, Doc, Fault));
    FClient.FollowEngineAxisValues;
    AssertEquals('the wavelength the file states', 1.5406,
        AxisValueOf(FClient.GetAxisValues, WavelengthParameter), 1e-9);

    //  And the axis it was saved on, as the window plans and puts it back
    //  (TFormMain.PutBackAxes): after the wavelength, so the mode that cannot
    //  be drawn without one is not refused for want of it.
    Plan := PlanUiRestore(Doc, [True, True, True, True], True);
    AssertTrue('the user chose it', Plan.Axes[adArgument].Apply);
    AssertEquals('display mode 2 was Sin(Theta)/Lambda',
        SinThetaOverLambdaAxisModeId, Plan.Axes[adArgument].Mode);
    Axes := TChartAxes.Create;
    try
        Axes.SetAxisValues(FClient.GetAxisValues);
        Axes.Restore(adArgument, Plan.Axes[adArgument].Mode, True,
            Plan.Axes[adArgument].Definition);
        AssertEquals('and is shown', SinThetaOverLambdaAxisModeId,
            Axes.ResolvedModeId(adArgument));
        AssertEquals('dividing by that wavelength',
            Sin(21.5 * Pi / 360) / 1.5406,
            Axes.Axis(adArgument).ToDisplay(21.5), 1e-12);
    finally
        Axes.Free;
    end;
end;

initialization
    RegisterTest('unit', TWavelengthRoundTripTest);
    RegisterTest('integration', TFormerWavelengthProjectTest);
end.
