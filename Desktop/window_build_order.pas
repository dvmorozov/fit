// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The order the main window is built in, and why each step comes
after the ones it does.)

THE ORDER WAS IMPLICIT. TFormMain.FormCreate was four hundred lines run top to
bottom, and what had to come before what lived only in comments beside the
lines - "after the modules have registered", "before the settings have been
read". A line moved for an unrelated reason broke one of them in silence, and
the failures were the worst kind: inside FormCreate, before there is a window
to say anything (the name registers created after the axis entries that asked
them; the product title set after the caption made from it).

NOW IT IS DATA. Each step names the steps it needs; the window runs
WindowBuildOrder, one method per step; and WindowBuildFindings - which a test
calls over the order the window runs - reports a step that comes before
something it needs, so moving one is refused by name rather than discovered at
start-up. The steps are the window's (TFormMain.BuildWindowStep); this unit
names no widget and is tested without one.
}
unit window_build_order;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    TWindowBuildStep = (
        { The registers generated widgets take their names from. }
        wbNames,
        { The queue refusals and faults are reported through, the idle
          handler, and the phase observer. }
        wbDeferredWork,
        { The document commands, the exception handler, the parameter table's
          presenter, the title and the caption made from it. }
        wbDocument,
        { The axes and the chart's marks and popup. }
        wbAxes,
        { Every registry: the objectives' menu, the guide, the axis modes, the
          modules, the curve types, the data loaders. }
        wbRegistries,
        { Both axis menus and the chart's popup, from every registered mode. }
        wbAxisMenus,
        { Where data can be fetched from - after the loaders, which say what
          can read what a source produces. }
        wbDataSources,
        { The engines and their menu. }
        wbMinimizers,
        { Every module's menu, from what it declares. }
        wbModuleMenus,
        { The actions made in code, and Model > Clear Model - last in the
          Model menu, after any module's submenu. }
        wbCodedActions,
        { The command table, which resolves every name it holds. }
        wbCommandTable,
        { The panes the table fills, the report and Run Log tabs, the toolbar
          spacing. }
        wbPanels,
        { Every tickable entry declared so, over every menu as built. }
        wbCheckableMenus,
        { The status bar's advice panel and the legend's resizing. }
        wbStatusBar,
        { The viewer, the chart tabs and the progress view. }
        wbViewer,
        { What the client and the controls report to the window. }
        wbClientEvents,
        { The widgets' own settings: hint, colours, balloons, scroll bars. }
        wbWidgets,
        { The settings read, and the recent projects. }
        wbSettings,
        { What the user last chose: axes, objective, engine, curve type,
          animation. }
        wbRestoredChoices,
        { The palette. }
        wbTheme,
        { The zoom of the HTML panes, and the window's layout. }
        wbLayout,
        { Weighting, compute server, reduction rule, and the Curve Type menu. }
        wbEngineChoices,
        { The update entries of Help, and the platform's menu conventions -
          once every menu is built, before any has a handle. }
        wbHelpMenu,
        { The first state refresh, and the chart's timing hooks. }
        wbStart);
    TWindowBuildSteps = set of TWindowBuildStep;
    TWindowBuildOrder = array of TWindowBuildStep;

{ The order the window is built in. }
function WindowBuildOrder: TWindowBuildOrder;

{ What AStep needs built before it. }
function WindowBuildNeeds(AStep: TWindowBuildStep): TWindowBuildSteps;

{ What a step is called in a finding. }
function WindowBuildStepName(AStep: TWindowBuildStep): string;

{ Everything wrong with AOrder, one sentence each: a step that comes before one
  it needs, a step run twice, a step never run. Empty for a sound order. }
function WindowBuildFindings(const AOrder: TWindowBuildOrder): TStringArray;

implementation

uses
    TypInfo;

function WindowBuildOrder: TWindowBuildOrder;
var
    S: TWindowBuildStep;
begin
    //  The declaration order is the build order: the enumeration above is
    //  written in it, so the two cannot drift apart.
    Result := nil;
    for S := Low(TWindowBuildStep) to High(TWindowBuildStep) do
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := S;
    end;
end;

function WindowBuildNeeds(AStep: TWindowBuildStep): TWindowBuildSteps;
const
    EveryMenu = [wbAxisMenus, wbMinimizers, wbModuleMenus, wbCodedActions,
        wbPanels, wbEngineChoices];
begin
    case AStep of
        wbNames, wbDeferredWork, wbDocument, wbAxes, wbMinimizers,
        wbStatusBar:
            Result := [];
        //  The axis menus' entries take generated names, and list every mode
        //  registered - the framework's and every module's front door's.
        wbAxisMenus:
            Result := [wbNames, wbAxes, wbRegistries];
        //  A source says which kinds of file it produces; what reads one is
        //  the loader registry's answer.
        wbDataSources:
            Result := [wbRegistries];
        wbModuleMenus:
            Result := [wbNames, wbRegistries];
        //  Clear Model is the last entry of the Model menu, after any
        //  module's submenu.
        wbCodedActions:
            Result := [wbModuleMenus];
        //  It resolves every name it holds - module entries, coded actions,
        //  axis entries - and refuses one that answers to nothing.
        wbCommandTable:
            Result := [wbNames, wbAxisMenus, wbModuleMenus, wbCodedActions];
        //  Every button of the pane is a row of the table; the report tabs ask
        //  each module whether it reports.
        wbPanels:
            Result := [wbCommandTable, wbRegistries];
        //  Declared before any menu has a handle, over every menu as built.
        wbCheckableMenus:
            Result := EveryMenu - [wbEngineChoices];
        wbViewer:
            Result := [wbAxes];
        wbClientEvents:
            Result := [wbDocument, wbCodedActions];
        //  The Residual Spectrum's scroll bar is built with the chart tabs.
        wbWidgets:
            Result := [wbViewer];
        //  Reading them reports through the deferred queue.
        wbSettings:
            Result := [wbDeferredWork];
        wbRestoredChoices:
            Result := [wbSettings, wbViewer, wbAxisMenus, wbMinimizers,
                wbRegistries];
        wbTheme:
            Result := [wbSettings, wbViewer, wbPanels];
        //  What is noted as designed is what this build actually built.
        wbLayout:
            Result := [wbSettings, wbPanels];
        //  The Curve Type menu is built from the registered types.
        wbEngineChoices:
            Result := [wbSettings, wbRegistries];
        //  The platform reads the menus once, as they are then.
        wbHelpMenu:
            Result := EveryMenu + [wbCheckableMenus];
        wbStart:
            Result := [Low(TWindowBuildStep)..Pred(wbStart)];
    end;
end;

function WindowBuildStepName(AStep: TWindowBuildStep): string;
begin
    Result := GetEnumName(TypeInfo(TWindowBuildStep), Ord(AStep));
end;

procedure Add(var AList: TStringArray; const AText: string);
begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AText;
end;

function WindowBuildFindings(const AOrder: TWindowBuildOrder): TStringArray;
var
    Done, Needed: TWindowBuildSteps;
    S, N: TWindowBuildStep;
    i: longint;
begin
    Result := nil;
    Done := [];
    for i := 0 to High(AOrder) do
    begin
        S := AOrder[i];
        if S in Done then
            Add(Result, WindowBuildStepName(S) + ' is run twice');
        Needed := WindowBuildNeeds(S);
        for N := Low(TWindowBuildStep) to High(TWindowBuildStep) do
            if (N in Needed) and not (N in Done) then
                Add(Result, WindowBuildStepName(S) + ' runs before ' +
                    WindowBuildStepName(N) + ', which it needs');
        Include(Done, S);
    end;
    for S := Low(TWindowBuildStep) to High(TWindowBuildStep) do
        if not (S in Done) then
            Add(Result, WindowBuildStepName(S) + ' is never run');
end;

end.
