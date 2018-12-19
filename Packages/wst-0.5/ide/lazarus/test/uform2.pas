unit uform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2;

implementation
uses wstimportdlg;//, HeapTrc;

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var
  f : TformImport;
begin
  f := TformImport.Create(nil);
  try
    f.ShowModal();
  finally
    f.Release();
  end;
end;

initialization
  //SetHeapTraceOutput('heapTrace.txt');
  {$I uform2.lrs}

end.

