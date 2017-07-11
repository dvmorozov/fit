//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------
    This file is part of the MotifMASTER project. This software is
    distributed under GPL (see gpl.txt for details).

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}

unit Unit12;

{$MODE Delphi}

interface

uses LCLIntf, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LResources;

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
 


