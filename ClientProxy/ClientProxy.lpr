library ClientProxy;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this }, fit_client_proxy,
  ta, int_fit_service;
  
var
   FitService: IFitService;
  
function CreateFitServiceInstance: IFitService; cdecl;
begin
     FitService := TFitClientProxy.Create(nil);
     Result := FitService;
end;

procedure DestroyFitServiceInstance; cdecl;
begin
    FitService._Release;
end;

exports
    CreateFitServiceInstance,
    DestroyFitServiceInstance;
end.

