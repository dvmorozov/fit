{ Этот файл был автоматически создан Lazarus. Не редактировать!
Исходный код используется только для компиляции и установки пакета.
 }

unit ta; 

interface

uses
  TAGraph, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('TAGraph', @TAGraph.Register); 
end; 

initialization
  RegisterPackage('ta', @Register); 
end.
