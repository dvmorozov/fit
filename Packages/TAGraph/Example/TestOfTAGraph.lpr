program TestOfTAGraph;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Main;

begin
  Application.Title:='Test of TAChart';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

