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
        Image1:      TImage;
        Panel1:      TPanel;
        OKButton:    TButton;
        StaticText3: TStaticText;
        StaticText4: TStaticText;
        StaticTextTitle: TStaticText;
        StaticText11: TStaticText;
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
    form_main, LCLIntf;

{ TAboutBox }
procedure TAboutBox.FormShow(Sender: TObject);
begin
    Caption := FormMain.ApplicationProperties.Title;
    StaticTextTitle.Caption := FormMain.ApplicationProperties.Title;
end;

procedure TAboutBox.StaticTextGitHubClick(Sender: TObject);
begin
    OpenURL('https://github.com/dvmorozov/');
end;


initialization
  {$I about_box_dialog.lrs}
end.
 