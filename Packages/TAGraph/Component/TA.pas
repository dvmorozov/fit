{ ���� ���� ��� ������������� ������ Lazarus. �� �������������!
�������� ��� ������������ ������ ��� ���������� � ��������� ������.
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
