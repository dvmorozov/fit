{ Этот файл был автоматически создан Lazarus. Не редактировать!
Исходный код используется только для компиляции и установки пакета.
 }

unit TA_LINUX; 

interface

uses
  TAGraph, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('TAGraph', @TAGraph.Register); 
end; 

initialization
  RegisterPackage('TA_LINUX', @Register); 
end.
