unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Ipfilebroker, IpHtml;

type

  { TForm1 }

  TForm1 = class(TForm)
    IpFileDataProvider1: TIpFileDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var Path: string;
begin
    Path := ExtractFilePath(Application.ExeName);
    Path := Path + 'template.htm';
    IpHtmlPanel1.OpenURL(Path);
end;

initialization
  {$I Unit1.lrs}
end.

