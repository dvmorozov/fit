// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the main window's layout is remembered on this machine, and where
it opens when the screen it was left on has changed.)

WHAT THE USER SEES WHEN THIS IS WRONG: a window that opens where no monitor is,
which looks like the program not starting at all; a pane dragged to nothing
that cannot be found again; a layout that halves or doubles when the display's
scale changes. Every rule that prevents those is arithmetic over plain numbers,
so each one is a test here. The form only reads and sets controls.

THE FILE IS BEHIND TMachineSettings, which mock_machine_settings keeps in
memory, so no test writes a user's settings.json.
}
unit testcase_window_layout;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    machine_settings, mock_machine_settings, window_layout;

type
    TWindowLayoutTest = class(TTestCase)
    private
        FSettings: TMemoryMachineSettings;
        { Settings on a machine whose file holds AStored, owned by the fixture. }
        function Machine(const AStored: string): TMemoryMachineSettings;
        { Every value set, none of them a default. }
        function FullLayout: TWindowLayout;
        { The whole file as it was last written; the caller frees it. }
        function StoredFile: TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What is kept.
        procedure AFirstRunRemembersNoLayout;
        procedure ALayoutWrittenIsTheLayoutRead;
        procedure WritingTheLayoutKeepsEveryOtherSection;
        procedure TheLayoutIsKeptUnderTheNamesTheGuideGives;
        procedure APaneNotRememberedIsNotWritten;
        procedure AMissingOrMistypedValueIsNotRemembered;
        procedure ASizeThatIsNotPositiveIsNotRemembered;
        procedure TheKeysAreReadInAnyCase;
        procedure ForgettingTheLayoutRemembersNothing;

        //  What is taken from the window when it closes.
        procedure AWindowInItsNormalStateIsRememberedAsItIs;
        procedure AMaximizedOrMinimizedWindowIsRememberedAtItsNormalBounds;
        procedure WithNoNormalBoundsKnownTheCurrentOnesAreRemembered;
        procedure ALayoutIsTheSameOnlyWhenNothingInItMoved;

        //  The display's scale.
        procedure PaneSizesAreStoredAtNinetySixPpi;
        procedure AScaleThatIsNotPositiveIsTakenAsNinetySix;
        procedure APaneIsClampedToItsLimits;
        procedure ASidePaneLeavesTheChartItsRoomBesideTheOtherSide;
        procedure TheTablesLeaveTheChartItsRoomBetweenTheBars;
        procedure AnInnerPaneLeavesItsNeighbourRoomInTheTabs;
        procedure InAWindowTooSmallEveryPaneKeepsItsLeast;

        //  Where the window opens.
        procedure AWindowWhoseTitleBarIsReachableStaysWhereItWas;
        procedure AWindowPartlyOffTheScreenStaysWhereTheUserPutIt;
        procedure AWindowOnAMonitorThatIsGoneOpensOnOneThatIsThere;
        procedure AWindowWhoseTitleBarIsAboveTheScreenIsBroughtDown;
        procedure AWindowLargerThanTheScreenIsShrunkToFit;
        procedure OfTwoMonitorsTheOneMostOfTheWindowIsOnIsChosen;
        procedure NoWorkAreaMeansTheDefaultPlacement;
        procedure AWindowWithNoSizeIsNotPlaced;
    end;

implementation

const
    Grip = 40;

function R(ALeft, ATop, AWidth, AHeight: longint): TLayoutRect;
begin
    Result.Left := ALeft;
    Result.Top := ATop;
    Result.Width := AWidth;
    Result.Height := AHeight;
end;

procedure AssertRect(const AMessage: string; const AExpected, AActual: TLayoutRect);
begin
    TAssert.AssertEquals(AMessage + ' left', AExpected.Left, AActual.Left);
    TAssert.AssertEquals(AMessage + ' top', AExpected.Top, AActual.Top);
    TAssert.AssertEquals(AMessage + ' width', AExpected.Width, AActual.Width);
    TAssert.AssertEquals(AMessage + ' height', AExpected.Height, AActual.Height);
end;

{ True when AInner lies wholly inside AOuter. }
function Inside(const AInner, AOuter: TLayoutRect): boolean;
begin
    Result := (AInner.Left >= AOuter.Left) and (AInner.Top >= AOuter.Top) and
        (AInner.Left + AInner.Width <= AOuter.Left + AOuter.Width) and
        (AInner.Top + AInner.Height <= AOuter.Top + AOuter.Height);
end;

procedure TWindowLayoutTest.SetUp;
begin
    FSettings := nil;
end;

procedure TWindowLayoutTest.TearDown;
begin
    FreeAndNil(FSettings);
end;

function TWindowLayoutTest.Machine(const AStored: string): TMemoryMachineSettings;
begin
    FreeAndNil(FSettings);
    FSettings := TMemoryMachineSettings.CreateWith(AStored <> '', AStored);
    Result := FSettings;
end;

function TWindowLayoutTest.FullLayout: TWindowLayout;
var
    Pane: TLayoutPane;
begin
    Result := EmptyWindowLayout;
    Result.HasWindow := True;
    Result.Window := R(120, 80, 1280, 800);
    Result.Maximized := True;
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        Result.Panes[Pane] := 200 + 10 * Ord(Pane);
end;

function TWindowLayoutTest.StoredFile: TJSONObject;
begin
    Result := GetJSON(FSettings.Stored) as TJSONObject;
end;

procedure TWindowLayoutTest.AFirstRunRemembersNoLayout;
var
    L: TWindowLayout;
    Pane: TLayoutPane;
begin
    L := ReadWindowLayout(Machine(''));
    AssertFalse('a window', L.HasWindow);
    AssertFalse('maximized', L.Maximized);
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        AssertEquals(PaneKey(Pane), NoSize, L.Panes[Pane]);
end;

procedure TWindowLayoutTest.ALayoutWrittenIsTheLayoutRead;
var
    Written, Read: TWindowLayout;
    Pane: TLayoutPane;
    Next: TMemoryMachineSettings;
begin
    Written := FullLayout;
    WriteWindowLayout(Machine(''), Written);
    //  A new session, on the file this one left.
    Next := TMemoryMachineSettings.CreateWith(True, FSettings.Stored);
    try
        Read := ReadWindowLayout(Next);
    finally
        Next.Free;
    end;
    AssertTrue('a window', Read.HasWindow);
    AssertRect('window', Written.Window, Read.Window);
    AssertTrue('maximized', Read.Maximized);
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        AssertEquals(PaneKey(Pane), Written.Panes[Pane], Read.Panes[Pane]);
end;

procedure TWindowLayoutTest.WritingTheLayoutKeepsEveryOtherSection;
var
    Doc: TJSONObject;
begin
    WriteWindowLayout(Machine('{"app": {"ServerUrl": "http://here"}, ' +
        '"recent": {"last": "/p.fit"}}'), FullLayout);
    Doc := StoredFile;
    try
        AssertEquals('http://here', Doc.Objects['app'].Strings['ServerUrl']);
        AssertEquals('/p.fit', Doc.Objects['recent'].Strings['last']);
        AssertNotNull(Doc.Find(LayoutSection));
    finally
        Doc.Free;
    end;
end;

procedure TWindowLayoutTest.TheLayoutIsKeptUnderTheNamesTheGuideGives;
var
    Doc, Layout: TJSONObject;
begin
    //  The guide's table of settings.json names this section and these keys,
    //  and a user editing the file by hand reads them.
    WriteWindowLayout(Machine(''), FullLayout);
    Doc := StoredFile;
    try
        Layout := Doc.Objects['layout'];
        AssertEquals(120, Layout.Objects['window'].Integers['left']);
        AssertEquals(80, Layout.Objects['window'].Integers['top']);
        AssertEquals(1280, Layout.Objects['window'].Integers['width']);
        AssertEquals(800, Layout.Objects['window'].Integers['height']);
        AssertTrue(Layout.Objects['window'].Booleans['maximized']);
        AssertEquals(200, Layout.Objects['panes'].Integers['left']);
        AssertEquals(210, Layout.Objects['panes'].Integers['right']);
        AssertEquals(220, Layout.Objects['panes'].Integers['bottom']);
        AssertEquals(230, Layout.Objects['panes'].Integers['curveTypes']);
        AssertEquals(240, Layout.Objects['panes'].Integers['explanation']);
        AssertEquals(250, Layout.Objects['panes'].Integers['historyDetails']);
    finally
        Doc.Free;
    end;
end;

procedure TWindowLayoutTest.APaneNotRememberedIsNotWritten;
var
    L: TWindowLayout;
    Doc: TJSONObject;
begin
    L := FullLayout;
    L.Panes[lpBottom] := NoSize;
    L.HasWindow := False;
    WriteWindowLayout(Machine(''), L);
    Doc := StoredFile;
    try
        AssertNull('bottom', Doc.Objects['layout'].Objects['panes'].Find('bottom'));
        AssertNull('window', Doc.Objects['layout'].Find('window'));
    finally
        Doc.Free;
    end;
end;

procedure TWindowLayoutTest.AMissingOrMistypedValueIsNotRemembered;
var
    L: TWindowLayout;
begin
    L := ReadWindowLayout(Machine('{"layout": {' +
        '"window": {"left": 10, "top": 10, "height": 500, "maximized": "yes"},' +
        '"panes": {"left": "wide", "right": 240, "bottom": null}}}'));
    AssertFalse('a window with no width is no window', L.HasWindow);
    AssertFalse('"yes" is not true', L.Maximized);
    AssertEquals('left', NoSize, L.Panes[lpLeft]);
    AssertEquals('right', 240, L.Panes[lpRight]);
    AssertEquals('bottom', NoSize, L.Panes[lpBottom]);
    AssertEquals('history', NoSize, L.Panes[lpHistoryDetails]);

    L := ReadWindowLayout(Machine('{"layout": {"window": 3, "panes": []}}'));
    AssertFalse('a window', L.HasWindow);
    AssertEquals('left', NoSize, L.Panes[lpLeft]);
end;

procedure TWindowLayoutTest.ASizeThatIsNotPositiveIsNotRemembered;
var
    L: TWindowLayout;
begin
    L := ReadWindowLayout(Machine('{"layout": {' +
        '"window": {"left": 10, "top": 10, "width": 0, "height": 500},' +
        '"panes": {"left": 0, "right": -5, "bottom": 180.6}}}'));
    AssertFalse('a window of no width', L.HasWindow);
    AssertEquals('left', NoSize, L.Panes[lpLeft]);
    AssertEquals('right', NoSize, L.Panes[lpRight]);
    //  A number written by hand with a fraction is still a size.
    AssertEquals('bottom', 181, L.Panes[lpBottom]);
end;

procedure TWindowLayoutTest.TheKeysAreReadInAnyCase;
var
    L: TWindowLayout;
begin
    L := ReadWindowLayout(Machine('{"Layout": {' +
        '"Window": {"LEFT": -1200, "Top": 5, "Width": 900, "Height": 700, ' +
        '"Maximized": true}, "Panes": {"CurveTypes": 99}}}'));
    AssertTrue('a window', L.HasWindow);
    //  A monitor left of the main one has negative coordinates.
    AssertRect('window', R(-1200, 5, 900, 700), L.Window);
    AssertTrue('maximized', L.Maximized);
    AssertEquals('curve types', 99, L.Panes[lpCurveTypes]);
end;

procedure TWindowLayoutTest.ForgettingTheLayoutRemembersNothing;
var
    L: TWindowLayout;
    Pane: TLayoutPane;
    Doc: TJSONObject;
begin
    WriteWindowLayout(Machine('{"app": {"ServerUrl": "x"}}'), FullLayout);
    ForgetWindowLayout(FSettings);
    L := ReadWindowLayout(FSettings);
    AssertFalse('a window', L.HasWindow);
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        AssertEquals(PaneKey(Pane), NoSize, L.Panes[Pane]);
    Doc := StoredFile;
    try
        AssertEquals('the other sections are kept', 'x',
            Doc.Objects['app'].Strings['ServerUrl']);
        AssertEquals('nothing is left in the section', 0,
            Doc.Objects['layout'].Count);
    finally
        Doc.Free;
    end;
end;

procedure TWindowLayoutTest.AWindowInItsNormalStateIsRememberedAsItIs;
begin
    AssertRect('normal', R(10, 20, 900, 700),
        NormalBounds(True, R(10, 20, 900, 700), R(0, 0, 640, 480)));
end;

procedure TWindowLayoutTest.AMaximizedOrMinimizedWindowIsRememberedAtItsNormalBounds;
begin
    //  The screen's size is not where the window goes back to when it is
    //  made smaller, and a minimized window has no size worth keeping.
    AssertRect('not normal', R(100, 50, 640, 480),
        NormalBounds(False, R(0, 0, 1920, 1080), R(100, 50, 640, 480)));
end;

procedure TWindowLayoutTest.WithNoNormalBoundsKnownTheCurrentOnesAreRemembered;
begin
    AssertRect('none known', R(0, 0, 1920, 1080),
        NormalBounds(False, R(0, 0, 1920, 1080), R(0, 0, 0, 0)));
end;

procedure TWindowLayoutTest.ALayoutIsTheSameOnlyWhenNothingInItMoved;
var
    A, B: TWindowLayout;
begin
    A := FullLayout;
    B := A;
    AssertTrue('the same', SameLayout(A, B));
    B.Panes[lpExplanation] := B.Panes[lpExplanation] + 1;
    AssertFalse('a border dragged', SameLayout(A, B));
    B := A;
    B.Window.Left := B.Window.Left + 1;
    AssertFalse('the window moved', SameLayout(A, B));
    B := A;
    B.Window.Height := B.Window.Height - 1;
    AssertFalse('the window resized', SameLayout(A, B));
    B := A;
    B.Maximized := not B.Maximized;
    AssertFalse('maximized', SameLayout(A, B));
end;

procedure TWindowLayoutTest.PaneSizesAreStoredAtNinetySixPpi;
begin
    AssertEquals('at 200 %', 150, ToLogical(300, 192));
    AssertEquals('back at 200 %', 300, FromLogical(150, 192));
    AssertEquals('at 100 %', 150, ToLogical(150, 96));
    AssertEquals('at 125 %', 160, ToLogical(200, 120));
    AssertEquals('back at 125 %', 200, FromLogical(160, 120));
    AssertEquals('the absent size stays absent', NoSize, ToLogical(NoSize, 192));
    AssertEquals('and back', NoSize, FromLogical(NoSize, 192));
end;

procedure TWindowLayoutTest.AScaleThatIsNotPositiveIsTakenAsNinetySix;
begin
    AssertEquals(150, ToLogical(150, 0));
    AssertEquals(150, FromLogical(150, -1));
end;

procedure TWindowLayoutTest.APaneIsClampedToItsLimits;
begin
    AssertEquals('within', 200, ClampPane(200, 50, 300));
    AssertEquals('too large', 300, ClampPane(500, 50, 300));
    AssertEquals('too small', 50, ClampPane(10, 50, 300));
    //  A window too small to give the pane its minimum: the minimum wins,
    //  since a pane dragged to nothing cannot be found again.
    AssertEquals('no room at all', 50, ClampPane(200, 50, 20));
end;

{ A window 1200 x 800 with a 40-high bar on top and a 20-high status bar,
  sides of 200 and 250, side tabs 500 high; at least 60 for any pane and 200
  for the chart. }
function Room: TPaneRoom;
begin
    Result.ClientWidth := 1200;
    Result.ClientHeight := 800;
    Result.TopBarHeight := 40;
    Result.StatusBarHeight := 20;
    Result.LeftWidth := 200;
    Result.RightWidth := 250;
    Result.LeftTabsHeight := 500;
    Result.RightTabsHeight := 500;
    Result.Least := 60;
    Result.ChartLeast := 200;
end;

procedure TWindowLayoutTest.ASidePaneLeavesTheChartItsRoomBesideTheOtherSide;
begin
    AssertEquals('left, as asked', 300, PaneSizeWithin(lpLeft, 300, Room));
    //  1200 - 250 on the right - 200 for the chart.
    AssertEquals('left, at most', 750, PaneSizeWithin(lpLeft, 900, Room));
    //  1200 - 200 on the left - 200 for the chart.
    AssertEquals('right, at most', 800, PaneSizeWithin(lpRight, 900, Room));
end;

procedure TWindowLayoutTest.TheTablesLeaveTheChartItsRoomBetweenTheBars;
begin
    AssertEquals('as asked', 300, PaneSizeWithin(lpBottom, 300, Room));
    //  800 - 40 - 20 - 200 for the chart.
    AssertEquals('at most', 540, PaneSizeWithin(lpBottom, 700, Room));
end;

procedure TWindowLayoutTest.AnInnerPaneLeavesItsNeighbourRoomInTheTabs;
var
    R: TPaneRoom;
begin
    R := Room;
    R.RightTabsHeight := 400;
    //  The tabs' height less twice the least: the neighbour keeps the least,
    //  and so does whatever else the tab holds.
    AssertEquals('curve types', 380, PaneSizeWithin(lpCurveTypes, 450, R));
    AssertEquals('explanation', 280, PaneSizeWithin(lpExplanation, 450, R));
    AssertEquals('history', 280, PaneSizeWithin(lpHistoryDetails, 450, R));
    AssertEquals('as asked', 150, PaneSizeWithin(lpHistoryDetails, 150, R));
end;

procedure TWindowLayoutTest.InAWindowTooSmallEveryPaneKeepsItsLeast;
var
    R: TPaneRoom;
    Pane: TLayoutPane;
begin
    R := Room;
    R.ClientWidth := 300;
    R.ClientHeight := 200;
    R.LeftTabsHeight := 50;
    R.RightTabsHeight := 50;
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        AssertEquals(PaneKey(Pane), 60, PaneSizeWithin(Pane, 500, R));
end;

procedure TWindowLayoutTest.AWindowWhoseTitleBarIsReachableStaysWhereItWas;
var
    Placed: TLayoutRect;
begin
    AssertTrue(PlaceOnScreen(R(100, 100, 800, 600), [R(0, 0, 1920, 1080)],
        Grip, Placed));
    AssertRect('placed', R(100, 100, 800, 600), Placed);
end;

procedure TWindowLayoutTest.AWindowPartlyOffTheScreenStaysWhereTheUserPutIt;
var
    Placed: TLayoutRect;
begin
    //  Pushed half off the right edge on purpose: the title bar can still be
    //  grabbed, so where the user left it is where it opens.
    AssertTrue(PlaceOnScreen(R(1500, 100, 800, 600), [R(0, 0, 1920, 1080)],
        Grip, Placed));
    AssertRect('placed', R(1500, 100, 800, 600), Placed);
end;

procedure TWindowLayoutTest.AWindowOnAMonitorThatIsGoneOpensOnOneThatIsThere;
var
    Placed: TLayoutRect;
    Screen: TLayoutRect;
begin
    Screen := R(0, 0, 1920, 1080);
    //  Left on a second monitor to the right, which is no longer connected.
    AssertTrue(PlaceOnScreen(R(2500, 100, 800, 600), [Screen], Grip, Placed));
    AssertTrue('on the screen that is there', Inside(Placed, Screen));
    AssertEquals('its size is kept', 800, Placed.Width);
    AssertEquals('its size is kept', 600, Placed.Height);
end;

procedure TWindowLayoutTest.AWindowWhoseTitleBarIsAboveTheScreenIsBroughtDown;
var
    Placed: TLayoutRect;
begin
    AssertTrue(PlaceOnScreen(R(100, -500, 800, 600), [R(0, 25, 1920, 1055)],
        Grip, Placed));
    AssertRect('placed', R(100, 25, 800, 600), Placed);
end;

procedure TWindowLayoutTest.AWindowLargerThanTheScreenIsShrunkToFit;
var
    Placed: TLayoutRect;
begin
    //  Saved on a larger display, opened on a laptop's.
    AssertTrue(PlaceOnScreen(R(0, 0, 3000, 2000), [R(0, 0, 1440, 900)],
        Grip, Placed));
    AssertRect('placed', R(0, 0, 1440, 900), Placed);
end;

procedure TWindowLayoutTest.OfTwoMonitorsTheOneMostOfTheWindowIsOnIsChosen;
var
    Placed: TLayoutRect;
    Right: TLayoutRect;
begin
    Right := R(1920, 0, 1280, 1024);
    //  Its title bar above both screens, most of it over the right one.
    AssertTrue(PlaceOnScreen(R(1900, -300, 800, 600),
        [R(0, 0, 1920, 1080), Right], Grip, Placed));
    AssertTrue('on the right-hand monitor', Inside(Placed, Right));
end;

procedure TWindowLayoutTest.NoWorkAreaMeansTheDefaultPlacement;
var
    Placed: TLayoutRect;
begin
    AssertFalse(PlaceOnScreen(R(100, 100, 800, 600), [], Grip, Placed));
end;

procedure TWindowLayoutTest.AWindowWithNoSizeIsNotPlaced;
var
    Placed: TLayoutRect;
begin
    AssertFalse(PlaceOnScreen(R(100, 100, 0, 600), [R(0, 0, 1920, 1080)],
        Grip, Placed));
end;

initialization
    //  Plain values and a settings file held in memory: nothing outside the
    //  process.
    RegisterTest('unit', TWindowLayoutTest);
end.
