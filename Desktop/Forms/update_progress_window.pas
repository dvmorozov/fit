// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The window that shows an update being checked for, downloaded and
handed over.)

A BOUNDARY AND NOTHING ELSE: every word, the bar's position and whether Cancel
can still act are update_progress's, where tests reach them. This reads the
box the checker's thread posts into, ten times a second, and writes controls.

NOT MODAL. The check runs on a thread of its own, and the window's thread
creates this through TThread.Synchronize - a ShowModal there would hold the
checker until the user closed it. The program stays usable while the
installer downloads; the window stays above it, beside the main window rather
than in the taskbar.

CLOSING IT IS CANCELLING, while Cancel can still act: a window closed with
the download still running underneath would be the hidden work this window
exists to end. Once the download is over it cannot be closed - it closes
itself as the installer starts.
}
unit update_progress_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ComCtrls, ExtCtrls,
    update_check, update_progress;

type
    TUpdateProgressWindow = class(TForm)
    private
        FBox: TUpdateProgressBox;
        FOnCancel: TThreadMethod;
        FTitle, FDetail: TLabel;
        FBar: TProgressBar;
        FCancel: TButton;
        FTimer: TTimer;
        FCancelled: boolean;
        procedure Tick(Sender: TObject);
        procedure CancelClick(Sender: TObject);
        procedure CloseAsked(Sender: TObject; var CanClose: boolean);
    public
        { ABOX is read, never owned; AONCANCEL is what Cancel calls - the
          checker's Cancel, which only sets flags. }
        constructor CreateFor(AOwner: TComponent; ABox: TUpdateProgressBox;
            AOnCancel: TThreadMethod);
    end;

implementation

constructor TUpdateProgressWindow.CreateFor(AOwner: TComponent;
    ABox: TUpdateProgressBox; AOnCancel: TThreadMethod);
var
    Buttons: TPanel;
begin
    inherited CreateNew(AOwner);
    FBox := ABox;
    FOnCancel := AOnCancel;
    Caption := 'Updating ' + Application.Title;
    BorderStyle := bsDialog;
    BorderIcons := [biSystemMenu];
    FormStyle := fsStayOnTop;
    Position := poMainFormCenter;
    ShowInTaskBar := stNever;
    AutoSize := True;
    Constraints.MinWidth := Scale96ToFont(420);
    ChildSizing.LeftRightSpacing := Scale96ToFont(12);
    ChildSizing.TopBottomSpacing := Scale96ToFont(12);
    OnCloseQuery := @CloseAsked;

    FTitle := TLabel.Create(Self);
    FTitle.Parent := Self;
    FTitle.Align := alTop;
    FTitle.Font.Style := [fsBold];
    FTitle.BorderSpacing.Bottom := Scale96ToFont(6);

    FBar := TProgressBar.Create(Self);
    FBar.Parent := Self;
    FBar.Align := alTop;
    FBar.Top := FTitle.Top + FTitle.Height;
    FBar.Height := Scale96ToFont(18);
    FBar.Min := 0;
    FBar.Max := 100;
    FBar.Smooth := True;
    FBar.BorderSpacing.Bottom := Scale96ToFont(6);

    FDetail := TLabel.Create(Self);
    FDetail.Parent := Self;
    FDetail.Align := alTop;
    FDetail.Top := FBar.Top + FBar.Height;
    FDetail.BorderSpacing.Bottom := Scale96ToFont(8);

    Buttons := TPanel.Create(Self);
    Buttons.Parent := Self;
    Buttons.Align := alTop;
    Buttons.Top := FDetail.Top + FDetail.Height;
    Buttons.BevelOuter := bvNone;
    Buttons.AutoSize := True;

    FCancel := TButton.Create(Self);
    FCancel.Parent := Buttons;
    FCancel.Caption := 'Cancel';
    FCancel.Cancel := True;
    FCancel.Align := alRight;
    FCancel.AutoSize := True;
    FCancel.OnClick := @CancelClick;

    FTimer := TTimer.Create(Self);
    FTimer.Interval := 100;
    FTimer.OnTimer := @Tick;
    Tick(nil);
end;

procedure TUpdateProgressWindow.Tick(Sender: TObject);
var
    P: TUpdateProgress;
    Elapsed: int64;
    Percent: integer;
begin
    if not FBox.Read(P, Elapsed) then
        Exit;
    FTitle.Caption := UpdateProgressTitle(P);
    FDetail.Caption := UpdateProgressDetail(P, Elapsed);
    Percent := UpdateProgressPercent(P);
    //  NOTHING TO MEASURE: a bar that moves by itself, rather than an empty
    //  one that reads as stuck.
    if Percent < 0 then
        FBar.Style := pbstMarquee
    else
    begin
        FBar.Style := pbstNormal;
        FBar.Position := Percent;
    end;
    FCancel.Enabled := (not FCancelled) and UpdateProgressCancellable(P);
end;

procedure TUpdateProgressWindow.CancelClick(Sender: TObject);
begin
    FCancelled := True;
    FCancel.Enabled := False;
    FDetail.Caption := 'Cancelling...';
    FOnCancel;
end;

procedure TUpdateProgressWindow.CloseAsked(Sender: TObject; var CanClose: boolean);
begin
    //  The window goes when the checker says it is done (ProgressDone);
    //  the close box only asks for what Cancel does.
    CanClose := False;
    if FCancel.Enabled then
        CancelClick(Sender);
end;

end.
