// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The background fraction, read and written through the client, at
the module's own resource.)

The background fraction is the scientific module's since 2026-10-18: the client
reads and writes it through the problem value store the module registers
(scientific_value_store), not through the framework's settings document. These
were two of the client settings tests and moved here with the setting, so the
framework's suite names no unit of this module.
}
unit testcase_background_fraction_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport,
    problem_value_store, scientific_value_store;

type
    TBackgroundFractionClientTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TFitClient;
        { True when the last write named AField. }
        function Wrote(const AField: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheBackgroundFractionIsReadFromTheModulesResource;
        procedure TheBackgroundFractionIsWrittenToTheModulesResource;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TBackgroundFractionClientTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TBackgroundFractionClientTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

function TBackgroundFractionClientTest.Wrote(const AField: string): boolean;
begin
    Result := Pos('"' + AField + '"', FSvc.LastBody) > 0;
end;

procedure TBackgroundFractionClientTest.TheBackgroundFractionIsReadFromTheModulesResource;
begin
    //  The module's front door registers it; a suite that never calls it
    //  registers the same store here.
    RegisterProblemValueStore(BackgroundFractionValue, BackgroundFractionStore);
    FSvc.Reply('background-fraction', '{"waveLength":0,"backFactor":0.022}');
    AssertEquals('the module''s number', 0.022, FClient.BackFactor, 1E-9);
end;

procedure TBackgroundFractionClientTest.TheBackgroundFractionIsWrittenToTheModulesResource;
begin
    RegisterProblemValueStore(BackgroundFractionValue, BackgroundFractionStore);
    FClient.BackFactor := 0.06;
    AssertTrue('to the module: ' + FSvc.Log.AsText,
        Pos('background-fraction', FSvc.Log.AsText) > 0);
    AssertTrue('the number: ' + FSvc.LastBody, Wrote('backFactor'));
end;

initialization
    //  A unit test: the server is a canned reply.
    RegisterTest('unit', TBackgroundFractionClientTest);
end.
