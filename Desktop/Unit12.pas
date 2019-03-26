{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TAboutBox.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit Unit12;

{$MODE Delphi}

interface

uses Forms, StdCtrls,
  ExtCtrls, LResources;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TButton;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText8: TStaticText;
    StaticTextTitle: TStaticText;
    StaticText11: TStaticText;
    StaticText1: TStaticText;
    StaticText9: TStaticText;
    StaticText2: TStaticText;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses Unit1;

{ TAboutBox }

procedure TAboutBox.FormShow(Sender: TObject);
begin
    Caption := FormMain.ApplicationProperties1.Title;
    StaticTextTitle.Caption := FormMain.ApplicationProperties1.Title;
end;

initialization
  {$I Unit12.lrs}
end.
 


