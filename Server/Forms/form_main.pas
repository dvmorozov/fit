{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains hidden form used to render charts on the server.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit form_main;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, TAGraph,
  ExtCtrls;

type
  { TFormMain }
  TFormMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Chart: TTAChart;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

initialization
  {$I form_main.lrs}
end.

