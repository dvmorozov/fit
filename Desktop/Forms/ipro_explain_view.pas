// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The Explain pane drawn with TurboPowerIPro's HTML panel.)

LINKED BY THE PROGRAM, NOT BY THE FORM. TIpHtmlPanel requires the Printer4Lazarus
package, which does not compile against the nogui widget set the test suite is
built with, and the suite links the main form. So this unit is named only in
Fit.lpr's uses clause, registers itself as the explain-view factory when it is
linked, and the form finds it through int_explain_view. A build that does not
link it gets no pane - and the form says so at start-up rather than showing a
blank space.

A UI WRAPPER: it reads a click and draws a string. What the string says is
explanation_html's.
}
unit ipro_explain_view;

{$mode objfpc}{$H+}

interface

implementation

uses
    Classes, Controls, IpHtml, int_explain_view,
    //  What a zoom does to the page and to the component's font size. The
    //  arithmetic is there so it can be tested without a widget set.
    report_zoom;

type
    TIproExplainView = class(TComponent, IExplainView)
    private
        FPanel: TIpHtmlPanel;
        FOnLink: TExplainLinkEvent;
        { The page as it was handed over, unzoomed, because a change of size
          has to redraw it and the component cannot be asked what it holds. }
        FHtml: string;
        FZoom: longint;
        procedure HotClick(Sender: TObject);
        procedure Draw;
    public
        constructor CreateIn(AOwner: TComponent; AParent: TWinControl);
        function Control: TWinControl;
        procedure ShowHtml(const AHtml: string);
        procedure SetOnLink(AHandler: TExplainLinkEvent);
        procedure SetZoom(APercent: longint);
    end;

constructor TIproExplainView.CreateIn(AOwner: TComponent; AParent: TWinControl);
begin
    inherited Create(AOwner);
    FPanel := TIpHtmlPanel.Create(Self);
    FPanel.Parent := AParent;
    FPanel.OnHotClick := @HotClick;
    FZoom := ReportZoomDefault;
end;

function TIproExplainView.Control: TWinControl;
begin
    Result := FPanel;
end;

{ Hands the component the page at the current size.

  THE PAGE IS RE-SENT FOR A CHANGE OF SIZE, and that is not an oversight.
  DefaultFontSize is read when a document is parsed (TIpHtmlCustomPanel.
  SetHtml), and its setter only invalidates - so a panel told a new size keeps
  the layout it already has. The document also carries the style block that is
  the only thing headings obey. Both arrive the same way: by handing the page
  over again. }
procedure TIproExplainView.Draw;
begin
    FPanel.DefaultFontSize := FontSizeAtZoom(FZoom);
    FPanel.SetHtmlFromStr(HtmlAtZoom(FHtml, FZoom));
end;

procedure TIproExplainView.ShowHtml(const AHtml: string);
begin
    FHtml := AHtml;
    Draw;
end;

procedure TIproExplainView.SetZoom(APercent: longint);
begin
    if UsableZoom(APercent) = FZoom then
        Exit;
    FZoom := UsableZoom(APercent);
    //  NOTHING SHOWN YET, NOTHING TO REDRAW - and handing the component an
    //  empty document before its first page would put one there.
    if FHtml <> '' then
        Draw;
end;

procedure TIproExplainView.SetOnLink(AHandler: TExplainLinkEvent);
begin
    FOnLink := AHandler;
end;

procedure TIproExplainView.HotClick(Sender: TObject);
begin
    if Assigned(FOnLink) then
        FOnLink(FPanel.HotURL);
end;

function CreateIproExplainView(AOwner: TComponent;
    AParent: TWinControl): IExplainView;
begin
    Result := TIproExplainView.CreateIn(AOwner, AParent);
end;

initialization
    RegisterExplainViewFactory(@CreateIproExplainView);
end.
