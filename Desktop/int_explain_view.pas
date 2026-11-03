// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What draws an explanation, without naming the component that does.)

WHY A SEAM. The desktop program draws explanations with TurboPowerIPro's HTML
panel. That component requires the Printer4Lazarus package, and Printer4Lazarus
does not compile against the nogui widget set - which is what the test suite is
built with, and the suite links the main form. Naming the component in the form
made the whole unit suite uncompilable. So the form names this interface, the
program links the component's unit (ipro_explain_view), and that unit registers
itself here.

A FORM WITH NO VIEW REGISTERED GETS NIL, not a fault, and checks for it at
start-up - so a build that forgot to link the view says so by name.
}
unit int_explain_view;

{$mode objfpc}{$H+}

interface

uses
    Classes, Controls, app_theme;

type
    TExplainLinkEvent = procedure(const AHref: string) of object;

    IExplainView = interface
        ['{C4A1E5D2-6B3F-4E87-9A20-7D5B1C8E3F46}']
        function Control: TWinControl;
        procedure ShowHtml(const AHtml: string);
        procedure SetOnLink(AHandler: TExplainLinkEvent);
        { Draws at this percentage of the component's own size - one of
          report_zoom's steps. A SIZE, NOT A PAGE: what the view is showing
          does not change, so the caller does not re-send it. }
        procedure SetZoom(APercent: longint);
        { Draws in APalette's colours - background, text and links. A
          COLOURING, NOT A PAGE, for the reason a size is not one: the view
          redraws what it holds, and the caller does not re-send it. }
        procedure SetPalette(const APalette: TThemePalette);
    end;

    TExplainViewFactory = function(AOwner: TComponent;
        AParent: TWinControl): IExplainView;

procedure RegisterExplainViewFactory(AFactory: TExplainViewFactory);
function ExplainViewFactoryRegistered: boolean;
{ The registered view, built for AOwner and AParent, or nil when none is
  registered.

  IT DRAWS ONLY WHAT CHANGED. The form asks for the pane's explanation every time
  the model panel is refilled - after each poll, each fit step, each rebuild -
  and nearly always the answer is the one already on screen. Handing that to the
  HTML component replaces its whole document, and doing so while the user drags
  the pane's scroll bar pulled the scroll bar's control out from under the drag:
  an access violation delivered by the widget set, which ended the application.
  So the view returned here forwards an explanation only when it differs from
  the one already shown. }
function CreateExplainView(AOwner: TComponent;
    AParent: TWinControl): IExplainView;

{ Paints every view CreateExplainView made, and that is still alive, in
  APalette - what View > Theme does to the HTML panes.

  ONE BROADCAST rather than a list each window keeps: a view is born in the
  palette in force and joins this, so the Explain Everything window, the report
  tabs and the Explain pane all follow a change without the window making it
  having to know any of them. }
procedure RepaintExplainViews(const APalette: TThemePalette);

implementation

uses
    SysUtils;

type
    { Forwards an explanation to the registered view only when it is not the
      one already shown - see CreateExplainView for why. Owned like any other
      component of the form, so it lives exactly as long as the view it wraps. }
    TChangedOnlyExplainView = class(TComponent, IExplainView)
    private
        FInner: IExplainView;
        FShown: string;
        FHasShown: boolean;
    public
        constructor CreateFor(AOwner: TComponent; const AInner: IExplainView);
        function Control: TWinControl;
        procedure ShowHtml(const AHtml: string);
        procedure SetOnLink(AHandler: TExplainLinkEvent);
        procedure SetZoom(APercent: longint);
        procedure SetPalette(const APalette: TThemePalette);
        destructor Destroy; override;
    end;

var
    { Every live wrapper; each leaves it as it is destroyed. }
    LiveViews: TList = nil;
    Factory: TExplainViewFactory = nil;

constructor TChangedOnlyExplainView.CreateFor(AOwner: TComponent;
    const AInner: IExplainView);
begin
    inherited Create(AOwner);
    FInner := AInner;
    if not Assigned(LiveViews) then
        LiveViews := TList.Create;
    LiveViews.Add(Self);
    FInner.SetPalette(CurrentPalette);
end;

destructor TChangedOnlyExplainView.Destroy;
begin
    if Assigned(LiveViews) then
        LiveViews.Remove(Self);
    inherited Destroy;
end;

function TChangedOnlyExplainView.Control: TWinControl;
begin
    Result := FInner.Control;
end;

procedure TChangedOnlyExplainView.ShowHtml(const AHtml: string);
begin
    //  The same page again is not a change, and redrawing it is what freed the
    //  scroll bar a drag was being delivered to.
    if FHasShown and (AHtml = FShown) then
        Exit;
    FShown := AHtml;
    FHasShown := True;
    FInner.ShowHtml(AHtml);
end;

procedure TChangedOnlyExplainView.SetOnLink(AHandler: TExplainLinkEvent);
begin
    FInner.SetOnLink(AHandler);
end;

procedure TChangedOnlyExplainView.SetZoom(APercent: longint);
begin
    //  FORWARDED WHOLE, and the remembered page is still the page: a size is
    //  not an explanation, so what this wrapper is here to skip is unaffected
    //  by it. The view below re-draws what it is holding.
    FInner.SetZoom(APercent);
end;

procedure TChangedOnlyExplainView.SetPalette(const APalette: TThemePalette);
begin
    //  Forwarded whole, and the remembered page stays the page - as SetZoom.
    FInner.SetPalette(APalette);
end;

procedure RepaintExplainViews(const APalette: TThemePalette);
var
    i: longint;
begin
    if not Assigned(LiveViews) then
        Exit;
    for i := 0 to LiveViews.Count - 1 do
        TChangedOnlyExplainView(LiveViews[i]).SetPalette(APalette);
end;

procedure RegisterExplainViewFactory(AFactory: TExplainViewFactory);
begin
    //  THE LATEST WINS, and nil clears: there is one view per program, and a
    //  program that links none must be able to say so.
    Factory := AFactory;
end;

function ExplainViewFactoryRegistered: boolean;
begin
    Result := Assigned(Factory);
end;

function CreateExplainView(AOwner: TComponent;
    AParent: TWinControl): IExplainView;
begin
    Result := nil;
    if not Assigned(Factory) then
        Exit;
    Result := Factory(AOwner, AParent);
    if Assigned(Result) then
        Result := TChangedOnlyExplainView.CreateFor(AOwner, Result);
end;

finalization
    FreeAndNil(LiveViews);
end.
