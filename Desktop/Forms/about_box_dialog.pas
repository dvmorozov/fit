// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TAboutBox.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}

unit about_box_dialog;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, ExtCtrls, Forms, LResources, StdCtrls;

type

    { TAboutBox }

    TAboutBox = class(TForm)
        ImageLogo:      TImage;
        PanelAbout:      TPanel;
        ButtonOK:    TButton;
        StaticTextDesignedBy: TStaticText;
        StaticTextAuthor: TStaticText;
        StaticTextTitle: TStaticText;
        StaticTextVersion: TStaticText;
        StaticTextGitHub: TStaticText;
        procedure FormShow(Sender: TObject);
        procedure StaticTextGitHubClick(Sender: TObject);
    private
        procedure ShowApplicationLogo;
        { Private declarations }
    public
        { Public declarations }
    end;

var
    AboutBox: TAboutBox;

implementation

uses
    form_main, LCLIntf, Graphics, GraphType, IntfGraphics, app_logo, app_version,
    checks;

{ TAboutBox }
procedure TAboutBox.FormShow(Sender: TObject);
var
    Version: string;
begin
    CheckAssigned(FormMain, 'the main window the about box reads its version from');
    CheckAssigned(FormMain.ApplicationProperties, 'the application properties carrying the version to show');

    //  'About Fit', not 'Fit': a dialog's title bar says what the dialog is,
    //  which is how the user tells it from the window it came from.
    Caption := 'About ' + FormMain.ApplicationProperties.Title;
    StaticTextTitle.Caption := FormMain.ApplicationProperties.Title;

    //  THE APPLICATION'S OWN ICON, read from the running binary on every show.
    //  The form file used to carry a picture of its own - the logo of an older
    //  release - so the dialog kept showing it after the icon on the window, the
    //  taskbar and the installer had changed. Application.Icon is the MAINICON
    //  resource Fit.lpr links (fit_icon.res), so the two cannot differ again.
    //  Scaled by app_logo rather than stretched by the image: a stretched TIcon
    //  was drawn with light stripes through it (see that unit).
    ShowApplicationLogo;

    //  Read from the binary on every show, never written into the .lfm: the
    //  caption in the form file said 'version 1.1' for as long as this dialog
    //  has existed, because a number that has to be edited by hand is a number
    //  nobody edits.
    Version := GetAppVersion;
    if Version = '' then
        //  Said rather than left blank. A build that cannot name itself is worth
        //  noticing in a bug report, and an empty line reads as a layout fault.
        StaticTextVersion.Caption := 'version unknown'
    else
        StaticTextVersion.Caption := 'version ' + Version;
end;

{ The largest frame of the application icon worth scaling from, scaled to the
  image's size on this screen and drawn unstretched. }
procedure TAboutBox.ShowApplicationLogo;
var
    Icon: TIcon;
    Sizes: array of longint;
    Format_: TPixelFormat;
    H, W: word;
    i, Target: longint;
    Src, Dst: TLazIntfImage;
    Logo: TBitmap;
begin
    Target := ImageLogo.Width;
    if ImageLogo.Height < Target then
        Target := ImageLogo.Height;
    Icon := TIcon.Create;
    try
        Icon.Assign(Application.Icon);
        SetLength(Sizes, Icon.Count);
        for i := 0 to Icon.Count - 1 do
        begin
            Icon.GetDescription(i, Format_, H, W);
            Sizes[i] := W;
        end;
        i := LogoFrameFor(Sizes, Target);
        if i < 0 then
            Exit;
        Icon.Current := i;
        Src := Icon.CreateIntfImage;
        Dst := nil;
        Logo := TBitmap.Create;
        try
            Dst := SmoothlyScaled(Src, Target, Target);
            Logo.LoadFromIntfImage(Dst);
            ImageLogo.Stretch := False;
            ImageLogo.Picture.Assign(Logo);
        finally
            Logo.Free;
            Dst.Free;
            Src.Free;
        end;
    finally
        Icon.Free;
    end;
end;

procedure TAboutBox.StaticTextGitHubClick(Sender: TObject);
begin
    OpenURL('https://github.com/dvmorozov/');
end;


initialization
  {$I about_box_dialog.lrs}
end.
 
