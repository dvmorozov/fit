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
(FollowEngineWaveLength, the step ResyncFromService takes after a restore).
}
unit testcase_wavelength_round_trip;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_client, fit_rest_api,
    mock_loopback_fit_service, mock_fit_viewer, fit_project_document,
    fit_project_session, scientific_module;

type
    TWavelengthRoundTripTest = class(TTestCase)
    private
        FApi, FApi2: TFitRestApi;
        FSvc, FSvc2: TLoopbackFitService;
        FView, FView2: TMockFitViewer;
        FClient, FClient2: TFitClient;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AWavelengthSetInTheWindowIsSavedAndReopened;
    end;

implementation

procedure TWavelengthRoundTripTest.SetUp;
begin
    //  Before any problem is made: each takes a session of every module
    //  registered by then.
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

procedure TWavelengthRoundTripTest.TearDown;
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
    FClient.SetWaveLength(1.5406);
    Doc := CaptureProject(FSvc, EmptyProjectClientContext, EmptyProjectDocument);
    AssertTrue(Fault, ApplyProject(FSvc2, Doc, Fault));
    FClient2.FollowEngineWaveLength;
    AssertEquals('the axes read it again', 1.5406, FClient2.GetWaveLength, 1e-9);
end;

initialization
    RegisterTest('unit', TWavelengthRoundTripTest);
end.
