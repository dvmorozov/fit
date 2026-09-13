// SPDX-License-Identifier: GPL-3.0-or-later
{ Opening a data file is the client's own job: it reads the file, fills the grid
  and plots it. Handing the profile to the compute server comes after, and the
  server is a separate process that may not be running at all - starting the
  client alone (or with the /INFILE start-up parameter) must still open the file
  and show it, reporting the missing server rather than losing the data. }
unit testcase_open_without_server;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry,
  fit_client, fit_client_app, http_fit_service, main_calc_thread,
  data_loader_registration, title_points_set;

type
  TOpenWithoutServerTest = class(TTestCase)
  private
    FApp: TFitClientApp;
    FSvc: THttpFitService;
    procedure RecordError(const AMessage: string);
    function DataFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure OpensTheFileWhenNoServerIsRunning;
    procedure AndReloadsItFromDisk;
  end;

var
  { What the client reported to the user, if anything. }
  GReportedError: string;

implementation

procedure TOpenWithoutServerTest.RecordError(const AMessage: string);
begin
  GReportedError := AMessage;
end;

function TOpenWithoutServerTest.DataFile: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../Data/2.dat');
end;

procedure TOpenWithoutServerTest.SetUp;
begin
  //  The loaders are registered by whatever opens files - deliberately NOT from
  //  an initialization section - so a test that opens one has to do what the
  //  application does at start-up. Idempotent.
  //
  //  This test used to pass without it, on registrations left behind by
  //  testcase_data_loader_registry. That is a unit test and this is an
  //  integration test, so once the two suites could be run separately the
  //  dependency became a failure: "no installed reader handles .dat files".
  RegisterAllDataLoaders;
  GReportedError := '';
  main_calc_thread.OnCalcError := @Self.RecordError;
  FApp := TFitClientApp.Create;
  //  A port nothing listens on: the compute server is not running.
  FSvc := THttpFitService.Create('http://127.0.0.1:9');
  FApp.FitClient.FitService := FSvc;
end;

procedure TOpenWithoutServerTest.TearDown;
begin
  main_calc_thread.OnCalcError := nil;
  FreeAndNil(FApp);
  FreeAndNil(FSvc);
end;

procedure TOpenWithoutServerTest.OpensTheFileWhenNoServerIsRunning;
var
  Profile: TTitlePointsSet;
begin
  AssertTrue('the sample data file is there: ' + DataFile, FileExists(DataFile));

  //  This must not raise: an unreachable server may not cost the user the file.
  FApp.FitClient.LoadDataSet(DataFile);

  AssertTrue('the file counts as opened',
    FApp.FitClient.OpenState = OpenSuccess);

  Profile := FApp.FitClient.GetProfilePoints;
  AssertTrue('the profile is loaded', Assigned(Profile));
  AssertEquals('all of 2.dat is there', 51, Profile.PointsCount);
  AssertEquals('first point', 116.0, Profile.PointXCoord[0], 1e-9);

  //  And the user is told why nothing can be fitted yet.
  AssertTrue('the unreachable server is reported (' + GReportedError + ')',
    Pos('could not be reached', GReportedError) > 0);
end;

{ RELOADING, which is what the user asks for after editing the file in something
  else. Never exercised until now, in either suite.

  WHAT IT KEEPS AND WHAT IT DROPS, since I guessed wrong once here and the answer
  is the interesting part. The LOADER keeps its own points set - its comment says
  external pointers may exist - but the client REPLACES the profile it holds and
  deliberately drops what the user was in the middle of: a selected interval does
  not identify anything in the new data, and the picks of an unfinished gesture
  point at the old x-values. So the object may or may not be the same one, and
  what a test may assert is the DATA and the state, not the identity. }
procedure TOpenWithoutServerTest.AndReloadsItFromDisk;
var
  After: TTitlePointsSet;
begin
  FApp.FitClient.LoadDataSet(DataFile);
  AssertTrue('loaded once', Assigned(FApp.FitClient.GetProfilePoints));

  //  Must not raise, for the same reason the first open must not: an unreachable
  //  server may not cost the user their file.
  FApp.FitClient.Reload;

  After := FApp.FitClient.GetProfilePoints;
  AssertTrue('still loaded', Assigned(After));
  AssertEquals('all of the file again', 51, After.PointsCount);
  AssertEquals('and the same first point', 116.0, After.PointXCoord[0], 1e-9);
  AssertTrue('the file still counts as opened',
    FApp.FitClient.OpenState = OpenSuccess);
end;

initialization
  RegisterTest('integration', TOpenWithoutServerTest);
end.
