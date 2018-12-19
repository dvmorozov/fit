program test_import;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, wstimportdlg, uform2;

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

