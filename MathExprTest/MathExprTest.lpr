program MathExprTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, Unit2;

begin
  Application.Initialize;
  Application.CreateForm(TCalcExpressionDlg, CalcExpressionDlg);
  Application.Run;
end.

