{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TAboutBox.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit about_box_dialog;

{$MODE Delphi}

interface

uses Forms, StdCtrls,
  ExtCtrls, LResources, Classes;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    StaticText1: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticTextTitle: TStaticText;
    StaticText11: TStaticText;
    StaticTextGitHub: TStaticText;
    StaticText2: TStaticText;
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

uses main_form, LCLIntf;

{ TAboutBox }

procedure TAboutBox.FormShow(Sender: TObject);
begin
    Caption := FormMain.ApplicationProperties1.Title;
    StaticTextTitle.Caption := FormMain.ApplicationProperties1.Title;
end;

procedure TAboutBox.StaticTextGitHubClick(Sender: TObject);
begin
    OpenURL('https://github.com/dvmorozov/');
end;


initialization
  {$I about_box_dialog.lrs}
end.
 


