// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Taking a new profile: what of the previous session's work has to go,
and what the server is asked to say about the rest.)

OPENING A FILE THROWS AWAY MORE THAN THE OLD DATA. Everything the user had built
on top of it - the interval they selected, the background points they picked,
the curves that were fitted - was built against x-values that are about to stop
existing. A pick at 37.25 means the peak the user clicked on; against the next
file it means whatever happens to sit at 37.25, which is usually nothing.

THE MARKUP IS SPLIT BETWEEN TWO OWNERS, and that is why this is not one clear
operation. The server owns the background, the positions, the bounds and every
computed result; the client owns the selected-area flag and the picks of a
gesture in progress. Taking a new profile resets the server's half by itself, so
the client reads its own copies back afterwards rather than clearing them by
hand - the server is the one place that decides what survives a reload. But the
client's own two are not in that answer, so they must be dropped here.

WHAT HAPPENS IF THEY ARE NOT. The chart and the tables keep showing the previous
file's markup over the new data - which looks like the new file having features
it does not have - and the next pick is made against it. The picks of an
unfinished gesture are worse: they point at x-values from the old file and the
gesture completes over two different data sets.

THE PICKS ARE CLEARED, NOT FREED. The set exists for as long as a selection mode
is active, and the user may still be in one - they can open a file mid-gesture.
Freeing it would take the set out from under a mode that is still running.
}
unit testcase_client_profile;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, int_data_loader, main_calc_thread,
    mock_fit_viewer, mock_http_transport,
    title_points_set, points_set;

type
    { A loader that hands out a profile made in memory. }
    TFakeLoader = class(TInterfacedObject, IDataLoader)
    private
        FFirst: double;
    public
        constructor Create(AFirstX: double);
        procedure LoadDataSet(AFileName: string);
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet;
    end;

    { The profile handling is protected - it is reached from LoadDataSet and
      Reload, not from outside - so a descendant is how a test drives it, the
      same way the rest of this suite reaches a protected seam. }
    TTestableClient = class(TFitClient)
    public
        procedure UseLoader(ALoader: IDataLoader);
        procedure TakeProfileFromLoader;
        procedure BeginAnAreaSelection;
        function HasSelectedPoints: boolean;
        function SelectedPointCount: longint;
        function ProfileFirstX: double;
    end;

    TClientProfileTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TTestableClient;
        FLoaderRef: IDataLoader;
        { The routes UpdateComputedData reads back after a profile is sent. }
        procedure StubTheMarkupRoutes;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The new data replaces the old.
        procedure TheProfileComesFromTheLoader;
        procedure TakingASecondProfileReplacesTheFirst;

        //  What the client drops.
        procedure TheSelectedAreaFlagIsCleared;
        procedure APickInProgressIsEmptied;
        procedure ButItsSetIsKeptSoTheModeSurvives;

        //  Handing it to the server.
        procedure TheProfileIsSentOn;
        procedure TheServersOwnMarkupIsReadBackRatherThanCleared;
        procedure AFailureToSendIsReportedNotRaised;
        procedure WithNoHandlerAFailureIsRaisedAfterAll;
    end;

implementation

const
    BASE = 'http://localhost:8080';

type
    { The engine's error hook is a method pointer, so something has to own the
      method. }
    TErrorSink = class(TObject)
        procedure Note(const AMessage: string);
    end;

var
    { What the hook was told, read by the test that installed it. }
    LastCalcError: string;
    Sink: TErrorSink;

procedure TErrorSink.Note(const AMessage: string);
begin
    LastCalcError := AMessage;
end;

{ ---- the doubles ----------------------------------------------------------- }

constructor TFakeLoader.Create(AFirstX: double);
begin
    inherited Create;
    FFirst := AFirstX;
end;

procedure TFakeLoader.LoadDataSet(AFileName: string);
begin
end;

procedure TFakeLoader.Reload;
begin
end;

function TFakeLoader.GetPointsSetCopy: TTitlePointsSet;
begin
    //  A NEW SET EVERY TIME, as the real loader does - the client takes
    //  ownership of what it is given.
    Result := TTitlePointsSet.Create(nil);
    Result.AddNewPoint(FFirst, 100);
    Result.AddNewPoint(FFirst + 1, 110);
end;

procedure TTestableClient.UseLoader(ALoader: IDataLoader);
begin
    FDataLoader := ALoader;
end;

procedure TTestableClient.TakeProfileFromLoader;
begin
    CopyProfileDataFromLoader;
end;

procedure TTestableClient.BeginAnAreaSelection;
begin
    FSelectedAreaMode := True;
    SelectionMode := ModeSelectIntervalBounds;
end;

function TTestableClient.HasSelectedPoints: boolean;
begin
    Result := Assigned(FSelectedPoints);
end;

function TTestableClient.SelectedPointCount: longint;
begin
    Result := 0;
    if Assigned(FSelectedPoints) then
        Result := FSelectedPoints.PointsCount;
end;

function TTestableClient.ProfileFirstX: double;
begin
    Result := -1;
    if Assigned(FExperimentalProfile) and
        (FExperimentalProfile.PointsCount > 0) then
        Result := FExperimentalProfile.PointXCoord[0];
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TClientProfileTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TTestableClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FLoaderRef := TFakeLoader.Create(10);
    FClient.UseLoader(FLoaderRef);
    LastCalcError := '';
end;

procedure TClientProfileTest.TearDown;
begin
    main_calc_thread.OnCalcError := nil;
    FreeAndNil(FClient);
    FLoaderRef := nil;
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TClientProfileTest.StubTheMarkupRoutes;
begin
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('delta-profile', '{"title":"d","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[1,2]}');
end;

{ ---- the new data replaces the old ----------------------------------------- }

procedure TClientProfileTest.TheProfileComesFromTheLoader;
begin
    FClient.TakeProfileFromLoader;
    AssertEquals('the loader''s first point', 10.0,
        FClient.ProfileFirstX, 1E-9);
end;

procedure TClientProfileTest.TakingASecondProfileReplacesTheFirst;
begin
    //  REPLACED, not added to. The client owns the profile it holds, and a
    //  second file has to leave one set of data behind rather than two.
    FClient.TakeProfileFromLoader;
    FLoaderRef := nil;
    FLoaderRef := TFakeLoader.Create(50);
    FClient.UseLoader(FLoaderRef);
    FClient.TakeProfileFromLoader;
    AssertEquals('the second file''s data', 50.0,
        FClient.ProfileFirstX, 1E-9);
end;

{ ---- what the client drops ------------------------------------------------- }

procedure TClientProfileTest.TheSelectedAreaFlagIsCleared;
begin
    //  AN INTERVAL SELECTED ON THE OLD DATA IDENTIFIES NOTHING IN THE NEW. The
    //  server drops its own when the profile is set, so a flag left standing
    //  here would leave the two disagreeing about what "the data" even means -
    //  the client fitting a window the server does not have.
    FClient.BeginAnAreaSelection;
    AssertTrue('an area is selected to begin with', FClient.SelectedAreaMode);
    FClient.TakeProfileFromLoader;
    AssertFalse('and not afterwards', FClient.SelectedAreaMode);
end;

procedure TClientProfileTest.APickInProgressIsEmptied;
begin
    //  THE PICKS POINT AT THE OLD X-VALUES. Kept, the gesture completes over two
    //  different data sets - one pick from the file that was open and one from
    //  the file that is.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    FClient.AddPointToActive(10, 100);
    AssertEquals('a pick was made', 1, FClient.SelectedPointCount);
    FClient.TakeProfileFromLoader;
    AssertEquals('and it is gone', 0, FClient.SelectedPointCount);
end;

procedure TClientProfileTest.ButItsSetIsKeptSoTheModeSurvives;
begin
    //  CLEARED, NOT FREED. The user can open a file in the middle of a gesture,
    //  and the set exists for as long as the mode is active - freeing it would
    //  take the set out from under a mode that is still running, so the next
    //  click has nowhere to go.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    FClient.TakeProfileFromLoader;
    AssertTrue('the set is still there', FClient.HasSelectedPoints);
end;

{ ---- handing it to the server ---------------------------------------------- }

procedure TClientProfileTest.TheProfileIsSentOn;
begin
    //  The server fits what it holds, not what the client drew. A profile that
    //  never arrived leaves the user fitting the previous file.
    //
    //  A WHOLE SET GOES TO /profile; the individual point edits go to
    //  /points/profile. Two routes one word apart, doing different things - one
    //  replaces the data, the other adds a point to it.
    StubTheMarkupRoutes;
    FClient.TakeProfileFromLoader;
    FSvc.Log.Clear;
    FClient.SendProfileToServer;
    AssertTrue('the profile went out: ' + FSvc.Log.AsText,
        Pos('PUT', FSvc.Log.AsText) > 0);
    AssertTrue('to the profile route: ' + FSvc.Log.AsText,
        Pos('/problems/1/profile', FSvc.Log.AsText) > 0);
end;

procedure TClientProfileTest.TheServersOwnMarkupIsReadBackRatherThanCleared;
begin
    //  READ BACK, not cleared by hand. Taking a new profile resets the problem
    //  server-side - background, positions, bounds and every computed result are
    //  picks on, or products of, the data being thrown away - and the server is
    //  the one place that decides what survives. Clearing them here instead
    //  would be a second opinion, and the two would drift.
    StubTheMarkupRoutes;
    FClient.TakeProfileFromLoader;
    FSvc.Log.Clear;
    FClient.SendProfileToServer;
    AssertTrue('the client asked what is left: ' + FSvc.Log.AsText,
        Pos('curves', FSvc.Log.AsText) > 0);
end;

procedure TClientProfileTest.AFailureToSendIsReportedNotRaised;
begin
    //  THROUGH THE ENGINE'S ERROR HOOK, which is how every other failure during
    //  a computation reaches the user. Raised instead, it would escape into the
    //  window's last-resort handler, which logs at Fatal and stops the state
    //  poll - so an unreachable server while opening a file would also
    //  disconnect the user from it.
    main_calc_thread.OnCalcError := @Sink.Note;
    FClient.TakeProfileFromLoader;
    FSvc.FailNextWith('connection refused');
    FClient.SendProfileToServer;
    AssertTrue('the user was told: ' + LastCalcError, LastCalcError <> '');
end;

procedure TClientProfileTest.WithNoHandlerAFailureIsRaisedAfterAll;
var
    Raised: boolean;
begin
    //  NO HANDLER MEANS NOBODY IS LISTENING, and swallowing the failure then
    //  would make opening a file against a dead server look like it worked.
    main_calc_thread.OnCalcError := nil;
    FClient.TakeProfileFromLoader;
    FSvc.FailNextWith('connection refused');
    Raised := False;
    try
        FClient.SendProfileToServer;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('it reached the caller', Raised);
end;

initialization
    Sink := TErrorSink.Create;
    //  A unit test: a loader that makes its points in memory, a mock transport
    //  and a mock viewer. No file is opened and no server is spoken to.
    RegisterTest('unit', TClientProfileTest);

finalization
    Sink.Free;
end.
