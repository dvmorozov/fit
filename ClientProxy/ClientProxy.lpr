library ClientProxy;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this }, fit_client_proxy, ta;
  

function Test(): string; cdecl;
begin
     Result := 'test';
end;

function CreateFitServiceInstance: IFitService; cdecl;
begin
     Result := TFitClientProxy.Create(nil);
end;

exports
  Test, CreateFitServiceInstance;
end.

