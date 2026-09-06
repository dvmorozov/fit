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
        { Private declarations }
    public
        { Public declarations }
    end;

var
    AboutBox: TAboutBox;

implementation

uses
    form_main, LCLIntf, app_version, checks;

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

procedure TAboutBox.StaticTextGitHubClick(Sender: TObject);
begin
    OpenURL('https://github.com/dvmorozov/');
end;


initialization
  {$I about_box_dialog.lrs}
end.
 
