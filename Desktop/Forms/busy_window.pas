// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The small window that says what a long open or save is doing while
the main window waits for it.)

WHY A WINDOW OF ITS OWN. While a large project is opened or saved, the main
window is disabled - nothing the user clicks may start in the middle of it - and
a disabled window is drawn greyed by Qt and Cocoa, its status bar included. So
what is going on is said here, above the main window and centred on it: the
step, its stage, a bar that moves and the seconds gone by. Not modal: the work
that shows it is already running, and a modal loop of its own would run the
user's clicks in the middle of it.

Built in code, with no form file: three controls, laid out by their own sizes,
and nothing a designer would add. Every colour is the widget set's own.
}
unit busy_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls;

type
    TBusyWindow = class(TForm)
    private
        FStage: TLabel;
        FElapsed: TLabel;
        FBar: TProgressBar;
    public
        constructor CreateFor(AOwner: TCustomForm; const ATitle: string);
        { What the step is doing now. }
        procedure ShowStage(const AStage: string);
        { How long the step has been going, in words. }
        procedure ShowElapsed(const AText: string);
    end;

implementation

constructor TBusyWindow.CreateFor(AOwner: TCustomForm; const ATitle: string);
var
    Panel: TPanel;
begin
    CreateNew(AOwner);
    Caption := ATitle;
    BorderStyle := bsDialog;
    BorderIcons := [];
    //  ABOVE THE WINDOW IT SPEAKS FOR, and centred on it - not on the screen,
    //  which on two monitors is somewhere else entirely.
    PopupMode := pmExplicit;
    PopupParent := AOwner;
    Position := poOwnerFormCenter;
    AutoSize := True;

    Panel := TPanel.Create(Self);
    Panel.Parent := Self;
    Panel.BevelOuter := bvNone;
    Panel.AutoSize := True;
    Panel.ChildSizing.LeftRightSpacing := 16;
    Panel.ChildSizing.TopBottomSpacing := 12;
    Panel.ChildSizing.VerticalSpacing := 8;
    Panel.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    Panel.ChildSizing.ControlsPerLine := 1;

    FStage := TLabel.Create(Self);
    FStage.Parent := Panel;
    FStage.Caption := ' ';
    //  WIDE ENOUGH FOR THE LONGEST STAGE, so the window does not jump as the
    //  stages change.
    FStage.Constraints.MinWidth := Scale96ToForm(380);

    FBar := TProgressBar.Create(Self);
    FBar.Parent := Panel;
    //  MOVING, NOT MEASURING: nothing here knows how much of a step is left,
    //  and a bar that stands still reads as a window that has stopped.
    FBar.Style := pbstMarquee;
    FBar.Constraints.MinWidth := Scale96ToForm(380);

    FElapsed := TLabel.Create(Self);
    FElapsed.Parent := Panel;
    FElapsed.Caption := ' ';
end;

procedure TBusyWindow.ShowStage(const AStage: string);
begin
    if FStage.Caption <> AStage then
        FStage.Caption := AStage;
end;

procedure TBusyWindow.ShowElapsed(const AText: string);
begin
    if FElapsed.Caption <> AText then
        FElapsed.Caption := AText;
end;

end.
