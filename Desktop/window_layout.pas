// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How the main window was laid out on this machine - its size and
place, and the room the user gave each part - and where it may open again.)

WHY THIS MACHINE'S SETTINGS AND NOT THE PROJECT. A layout is about a screen: a
project opened on a laptop must not open at the size of the desktop monitor it
was saved on, and a project sent to someone else carries nothing about their
window. So the layout is the 'layout' section of settings.json
(machine_settings), beside the other things this machine remembers, and never
a file of its own - not LCL's TXMLPropStorage either, which would be exactly
that second file.

REMEMBERED SILENTLY. There is no "Save Layout" command and no setting that
turns this off: what the user arranged is how the next session opens, as in
every desktop application, and View > Reset Layout is the way back.

WHAT IS NOT REMEMBERED, ON PURPOSE.
- The table tab in front: the project keeps it (fit_project_document), and two
  owners of one choice would fight over it every time a project is opened.
- The tab in front on either side: the workflow chooses those - Tools at the
  start, Model after an automatic run - and a remembered one would only be
  overridden.
- Column widths: the tables size their columns again every time they refill.

PANE SIZES ARE KEPT AT 96 PPI, the LCL's unscaled unit. A pane dragged to 300
pixels at 200 % is 150 here, and comes back as 300 at 200 % and 150 at 100 %:
otherwise every change of scale - another monitor, /DPI - halves or doubles the
panes. The window's own place and size are kept as the screen reports them,
because the monitors' work areas they are checked against are reported in the
same units.

THE WINDOW ALWAYS OPENS WHERE IT CAN BE REACHED (PlaceOnScreen). A monitor
unplugged since, or a lower resolution, would otherwise open it where nothing
is shown - which looks like the program not starting at all.

NO LCL HERE, so every rule is a unit test (testcase_window_layout). The window
reads its controls and its monitors into these records and sets them back.
}
unit window_layout;

{$mode objfpc}{$H+}

interface

uses
    machine_settings;

const
    { The section of this machine's settings.json the layout is kept in. }
    LayoutSection = 'layout';
    { A size that is not remembered: the designed one is used. }
    NoSize = -1;

type
    { A rectangle as the window and the screen report one. Its own type rather
      than the LCL's TRect, so this unit names nothing from the widget set. }
    TLayoutRect = record
        Left, Top, Width, Height: longint;
    end;

    { Each border the user can drag, by the part it gives room to. Named for
      what the part IS rather than for a control, so a renamed control does
      not orphan what the file holds. }
    TLayoutPane = (
        lpLeft,             //  the Tools and Data tabs, its width
        lpRight,            //  the Graphs, Model and History tabs, its width
        lpBottom,           //  the tables, their height
        lpCurveTypes,       //  the curve-type list on Tools, its height
        lpExplanation,      //  the Explain pane on Model, its height
        lpHistoryDetails    //  the details under History, their height
    );

    { What the window has to share among its parts, in pixels now: what limits
      how large a remembered pane may be made in it. }
    TPaneRoom = record
        ClientWidth, ClientHeight: longint;
        { The toolbar above everything and the status bar below. }
        TopBarHeight, StatusBarHeight: longint;
        { The two side parts as they are now. }
        LeftWidth, RightWidth: longint;
        { The client height of the tabs on either side, which the inner panes
          share with what is beside them. }
        LeftTabsHeight, RightTabsHeight: longint;
        { The least any pane is given, and the least the chart keeps. }
        Least, ChartLeast: longint;
    end;

    TWindowLayout = record
        { False when no place for the window is remembered. }
        HasWindow: boolean;
        { The window's NORMAL bounds - where it goes back to when it is no
          longer maximized - so a maximized window is not remembered as the
          size of the screen. }
        Window: TLayoutRect;
        Maximized: boolean;
        { At 96 ppi; NoSize where none is remembered. }
        Panes: array[TLayoutPane] of longint;
    end;

{ Nothing remembered. }
function EmptyWindowLayout: TWindowLayout;
{ The key APane is kept under, inside the section's 'panes' object. }
function PaneKey(APane: TLayoutPane): string;

{ What ASettings remembers. A value that is missing, not a number or not a
  positive size is not remembered: a hand edit gets the designed size back
  rather than a pane of nothing. }
function ReadWindowLayout(ASettings: TMachineSettings): TWindowLayout;
{ Makes ALayout the whole of the section and writes the file. A pane at
  NoSize, and a window not had, are left out. }
procedure WriteWindowLayout(ASettings: TMachineSettings; const ALayout: TWindowLayout);
{ Remembers nothing from now on (View > Reset Layout). }
procedure ForgetWindowLayout(ASettings: TMachineSettings);

{ APixels at APpi, in 96-ppi units, and back. NoSize stays NoSize, and a
  scale that is not positive is taken as 96. }
function ToLogical(APixels, APpi: longint): longint;
function FromLogical(ALogical, APpi: longint): longint;

{ The bounds to remember of a window whose bounds are ABounds now: those
  themselves when it is in its normal state (AIsNormal), otherwise ARestored -
  where it goes back to - when the widget set has told those. }
function NormalBounds(AIsNormal: boolean; const ABounds, ARestored: TLayoutRect): TLayoutRect;
{ Whether nothing in A differs from B: the window's place, size and state, and
  every pane. }
function SameLayout(const A, B: TWindowLayout): boolean;

{ ASize within [AMin, AMax]. When there is not even AMin of room, AMin: a pane
  dragged to nothing could not be found again. }
function ClampPane(ASize, AMin, AMax: longint): longint;

{ ASize for APane, made to fit ARoom: no larger than leaves the chart its
  least, and the parts beside it theirs; never smaller than ARoom.Least. }
function PaneSizeWithin(APane: TLayoutPane; ASize: longint; const ARoom: TPaneRoom): longint;

{ Where a window saved at ASaved opens, given the work areas of the monitors
  there are now.

  KEPT AS IT WAS when a strip AGrip high across its top - its title bar - lies
  inside one work area for at least AGrip of its width: the user can reach it,
  and a window pushed partly off an edge was put there on purpose. It is only
  shrunk if it has become larger than that monitor.

  OTHERWISE MOVED onto the monitor most of it is over, or the first monitor
  when it is over none, and shrunk to fit that monitor.

  False when there is no monitor to place it on or it has no size: the window
  then opens where the system puts a new one. }
function PlaceOnScreen(const ASaved: TLayoutRect;
    const AWorkAreas: array of TLayoutRect; AGrip: longint;
    out APlaced: TLayoutRect): boolean;

implementation

uses
    Math, fpjson;

const
    WindowKey = 'window';
    PanesKey = 'panes';
    LeftKey = 'left';
    TopKey = 'top';
    WidthKey = 'width';
    HeightKey = 'height';
    MaximizedKey = 'maximized';
    { The unscaled unit of the LCL. }
    LogicalPpi = 96;
    { Larger than any screen there is; a size beyond it is a corrupt file. }
    LargestSize = 100000;

function EmptyWindowLayout: TWindowLayout;
var
    Pane: TLayoutPane;
begin
    Result.HasWindow := False;
    Result.Window.Left := 0;
    Result.Window.Top := 0;
    Result.Window.Width := 0;
    Result.Window.Height := 0;
    Result.Maximized := False;
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        Result.Panes[Pane] := NoSize;
end;

function PaneKey(APane: TLayoutPane): string;
const
    Keys: array[TLayoutPane] of string = ('left', 'right', 'bottom',
        'curveTypes', 'explanation', 'historyDetails');
begin
    Result := Keys[APane];
end;

{ The number under AKey in AObject, rounded, or False when there is none. }
function NumberIn(AObject: TJSONObject; const AKey: string; out AValue: longint): boolean;
var
    Data: TJSONData;
    F: double;
begin
    Result := False;
    AValue := 0;
    if not Assigned(AObject) then
        Exit;
    Data := ValueIn(AObject, AKey);
    if not (Data is TJSONNumber) then
        Exit;
    F := Data.AsFloat;
    if IsNan(F) or (Abs(F) > LargestSize) then
        Exit;
    AValue := Round(F);
    Result := True;
end;

{ A positive size under AKey, or NoSize. }
function SizeIn(AObject: TJSONObject; const AKey: string): longint;
begin
    if not NumberIn(AObject, AKey, Result) or (Result <= 0) then
        Result := NoSize;
end;

{ The object under AKey in AObject, or nil when it is not one. }
function ObjectIn(AObject: TJSONObject; const AKey: string): TJSONObject;
var
    Data: TJSONData;
begin
    Data := ValueIn(AObject, AKey);
    if Data is TJSONObject then
        Result := TJSONObject(Data)
    else
        Result := nil;
end;

function ReadWindowLayout(ASettings: TMachineSettings): TWindowLayout;
var
    Section, Window, Panes: TJSONObject;
    Data: TJSONData;
    Pane: TLayoutPane;
begin
    Result := EmptyWindowLayout;
    Section := ASettings.Section(LayoutSection);
    try
        Window := ObjectIn(Section, WindowKey);
        if Assigned(Window) then
        begin
            //  ALL FOUR OR NONE: a place without a size, or a size without a
            //  place, is not a window anyone left.
            Result.Window.Width := SizeIn(Window, WidthKey);
            Result.Window.Height := SizeIn(Window, HeightKey);
            Result.HasWindow := NumberIn(Window, LeftKey, Result.Window.Left) and
                NumberIn(Window, TopKey, Result.Window.Top) and
                (Result.Window.Width <> NoSize) and (Result.Window.Height <> NoSize);
            Data := ValueIn(Window, MaximizedKey);
            Result.Maximized := (Data is TJSONBoolean) and Data.AsBoolean;
        end;
        Panes := ObjectIn(Section, PanesKey);
        if Assigned(Panes) then
            for Pane := Low(TLayoutPane) to High(TLayoutPane) do
                Result.Panes[Pane] := SizeIn(Panes, PaneKey(Pane));
    finally
        Section.Free;
    end;
end;

procedure WriteWindowLayout(ASettings: TMachineSettings; const ALayout: TWindowLayout);
var
    Section, Panes: TJSONObject;
    Pane: TLayoutPane;
begin
    //  THE WHOLE SECTION, built afresh: this unit is its only writer, so there
    //  is nothing in it another part of the program would lose.
    Section := TJSONObject.Create;
    if ALayout.HasWindow then
        Section.Add(WindowKey, TJSONObject.Create([
            LeftKey, ALayout.Window.Left,
            TopKey, ALayout.Window.Top,
            WidthKey, ALayout.Window.Width,
            HeightKey, ALayout.Window.Height,
            MaximizedKey, ALayout.Maximized]));
    Panes := TJSONObject.Create;
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        if ALayout.Panes[Pane] > 0 then
            Panes.Add(PaneKey(Pane), ALayout.Panes[Pane]);
    Section.Add(PanesKey, Panes);
    ASettings.ReplaceSection(LayoutSection, Section);
end;

procedure ForgetWindowLayout(ASettings: TMachineSettings);
begin
    //  AN EMPTY SECTION RATHER THAN NONE: machine_settings replaces sections
    //  and has no way to remove one, and an empty one means the same thing.
    ASettings.ReplaceSection(LayoutSection, TJSONObject.Create);
end;

function UsablePpi(APpi: longint): longint;
begin
    if APpi > 0 then
        Result := APpi
    else
        Result := LogicalPpi;
end;

function ToLogical(APixels, APpi: longint): longint;
begin
    if APixels = NoSize then
        Exit(NoSize);
    Result := Round(APixels * LogicalPpi / UsablePpi(APpi));
end;

function FromLogical(ALogical, APpi: longint): longint;
begin
    if ALogical = NoSize then
        Exit(NoSize);
    Result := Round(ALogical * UsablePpi(APpi) / LogicalPpi);
end;

function NormalBounds(AIsNormal: boolean; const ABounds, ARestored: TLayoutRect): TLayoutRect;
begin
    if AIsNormal or (ARestored.Width <= 0) or (ARestored.Height <= 0) then
        Result := ABounds
    else
        Result := ARestored;
end;

function SameLayout(const A, B: TWindowLayout): boolean;
var
    Pane: TLayoutPane;
begin
    Result := (A.HasWindow = B.HasWindow) and (A.Maximized = B.Maximized) and
        (A.Window.Left = B.Window.Left) and (A.Window.Top = B.Window.Top) and
        (A.Window.Width = B.Window.Width) and (A.Window.Height = B.Window.Height);
    for Pane := Low(TLayoutPane) to High(TLayoutPane) do
        Result := Result and (A.Panes[Pane] = B.Panes[Pane]);
end;

function ClampPane(ASize, AMin, AMax: longint): longint;
begin
    Result := Max(AMin, Min(ASize, AMax));
end;

function PaneSizeWithin(APane: TLayoutPane; ASize: longint; const ARoom: TPaneRoom): longint;
var
    Most: longint;
begin
    //  THE ROOM IS THE WINDOW'S NOW, not the one the size was taken in: a
    //  window made smaller since, or opened on a smaller screen, must still
    //  show the chart and every part.
    case APane of
        lpLeft: Most := ARoom.ClientWidth - ARoom.RightWidth - ARoom.ChartLeast;
        lpRight: Most := ARoom.ClientWidth - ARoom.LeftWidth - ARoom.ChartLeast;
        lpBottom: Most := ARoom.ClientHeight - ARoom.TopBarHeight -
                ARoom.StatusBarHeight - ARoom.ChartLeast;
        //  Inside the side tabs, beside the buttons, the model list or the
        //  history list, which keep the least as well.
        lpCurveTypes: Most := ARoom.LeftTabsHeight - 2 * ARoom.Least;
    else
        Most := ARoom.RightTabsHeight - 2 * ARoom.Least;
    end;
    Result := ClampPane(ASize, ARoom.Least, Most);
end;

{ How far [AStart, AStart + ALength) and [BStart, BStart + BLength) overlap. }
function Overlap(AStart, ALength, BStart, BLength: longint): longint;
begin
    Result := Max(0, Min(AStart + ALength, BStart + BLength) - Max(AStart, BStart));
end;

{ ARect moved and, where it must be, shrunk to lie wholly inside AArea. }
function FitInto(const ARect, AArea: TLayoutRect): TLayoutRect;
begin
    Result.Width := Min(ARect.Width, AArea.Width);
    Result.Height := Min(ARect.Height, AArea.Height);
    Result.Left := EnsureRange(ARect.Left, AArea.Left,
        AArea.Left + AArea.Width - Result.Width);
    Result.Top := EnsureRange(ARect.Top, AArea.Top,
        AArea.Top + AArea.Height - Result.Height);
end;

function PlaceOnScreen(const ASaved: TLayoutRect;
    const AWorkAreas: array of TLayoutRect; AGrip: longint;
    out APlaced: TLayoutRect): boolean;
var
    i, Best: longint;
    Covered, BestCovered: int64;
    A: TLayoutRect;
begin
    APlaced := ASaved;
    if (Length(AWorkAreas) = 0) or (ASaved.Width <= 0) or (ASaved.Height <= 0) then
        Exit(False);
    Result := True;

    //  The title bar first: if the user can grab it, the window stays.
    for i := 0 to High(AWorkAreas) do
    begin
        A := AWorkAreas[i];
        if (Overlap(ASaved.Left, ASaved.Width, A.Left, A.Width) >= AGrip) and
            (ASaved.Top >= A.Top) and (ASaved.Top + AGrip <= A.Top + A.Height) then
        begin
            if (ASaved.Width > A.Width) or (ASaved.Height > A.Height) then
                APlaced := FitInto(ASaved, A);
            Exit;
        end;
    end;

    //  Otherwise the monitor most of it is over, or the first.
    Best := 0;
    BestCovered := 0;
    for i := 0 to High(AWorkAreas) do
    begin
        A := AWorkAreas[i];
        Covered := int64(Overlap(ASaved.Left, ASaved.Width, A.Left, A.Width)) *
            Overlap(ASaved.Top, ASaved.Height, A.Top, A.Height);
        if Covered > BestCovered then
        begin
            Best := i;
            BestCovered := Covered;
        end;
    end;
    APlaced := FitInto(ASaved, AWorkAreas[Best]);
end;

end.
