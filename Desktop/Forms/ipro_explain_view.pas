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
    Classes, Controls, IpHtml, int_explain_view;

type
    TIproExplainView = class(TComponent, IExplainView)
    private
        FPanel: TIpHtmlPanel;
        FOnLink: TExplainLinkEvent;
        procedure HotClick(Sender: TObject);
    public
        constructor CreateIn(AOwner: TComponent; AParent: TWinControl);
        function Control: TWinControl;
        procedure ShowHtml(const AHtml: string);
        procedure SetOnLink(AHandler: TExplainLinkEvent);
    end;

constructor TIproExplainView.CreateIn(AOwner: TComponent; AParent: TWinControl);
begin
    inherited Create(AOwner);
    FPanel := TIpHtmlPanel.Create(Self);
    FPanel.Parent := AParent;
    FPanel.OnHotClick := @HotClick;
end;

function TIproExplainView.Control: TWinControl;
begin
    Result := FPanel;
end;

procedure TIproExplainView.ShowHtml(const AHtml: string);
begin
    FPanel.SetHtmlFromStr(AHtml);
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
