// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Help > Explain Everything: every explanation this build can give.)

A UI WRAPPER AND NOTHING ELSE. What the list holds, in what order and under which
headings, is explanation_index's; how a page reads is explanation_html's; which
topic a link leads to is TopicFromLink's. This window reads a click and draws the
answer, and holds no decision that a headless test could not already reach.

Created in code rather than from a .lfm, for the reason the Tools pane and the
Model panel are: additive, and the designed form stays untouched.
}
unit explain_everything_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, LCLIntf,
    int_explain_view,
    explanation, explanation_registry, explanation_index, explanation_html,
    model_outline;

type
    TExplainEverythingWindow = class(TForm)
    private
        FList: TListBox;
        FPane: IExplainView;
        FRows: TExplanationIndexRows;
        procedure ListClick(Sender: TObject);
        procedure PaneLinkFollowed(const AHref: string);
        procedure ShowTopic(const ATopic: string);
    public
        constructor CreateIndex(AOwner: TComponent);
    end;

{ Opens the window over AOwner, non-modal, so a user can keep it beside the
  work it explains. }
procedure ShowExplainEverything(AOwner: TComponent);

implementation

constructor TExplainEverythingWindow.CreateIndex(AOwner: TComponent);
var
    Split: TSplitter;
    i: longint;
begin
    inherited CreateNew(AOwner);
    Caption := 'Explain Everything';
    Width := Scale96ToFont(900);
    Height := Scale96ToFont(600);
    Position := poOwnerFormCenter;

    FList := TListBox.Create(Self);
    FList.Parent := Self;
    FList.Align := alLeft;
    FList.Width := Scale96ToFont(280);
    FList.OnClick := @ListClick;

    Split := TSplitter.Create(Self);
    Split.Parent := Self;
    Split.Align := alLeft;
    Split.Left := FList.Width + 1;

    //  Through the seam, for the reason form_main gives: this unit is linked
    //  by the test suite too, which cannot build the HTML component.
    FPane := CreateExplainView(Self, Self);
    if Assigned(FPane) then
    begin
        FPane.Control.Align := alClient;
        FPane.SetOnLink(@PaneLinkFollowed);
    end;

    FRows := ExplanationIndexOf(RegisteredExplanationProviders);
    for i := 0 to High(FRows) do
        if FRows[i].IsHeader then
            FList.Items.Add(FRows[i].Caption)
        else
            FList.Items.Add('    ' + FRows[i].Caption);

    ShowTopic('');
end;

procedure TExplainEverythingWindow.ShowTopic(const ATopic: string);
var
    E: TExplanation;
begin
    if not Assigned(FPane) then
        Exit;
    if (ATopic <> '') and FindExplanation(ATopic, E) then
        FPane.ShowHtml(ExplanationHtml(E, @RegisteredTopicTitle))
    else
        FPane.ShowHtml(EmptyExplanationHtml(
            'Choose a topic on the left to read its explanation.'));
end;

procedure TExplainEverythingWindow.ListClick(Sender: TObject);
begin
    if (FList.ItemIndex < 0) or (FList.ItemIndex > High(FRows)) then
        Exit;
    ShowTopic(FRows[FList.ItemIndex].Topic);
end;

procedure TExplainEverythingWindow.PaneLinkFollowed(const AHref: string);
var
    Topic: string;
begin
    if TopicFromLink(AHref, Topic) then
        ShowTopic(Topic)
    else if AHref <> '' then
        OpenURL(AHref);
end;

procedure ShowExplainEverything(AOwner: TComponent);
begin
    TExplainEverythingWindow.CreateIndex(AOwner).Show;
end;

end.
