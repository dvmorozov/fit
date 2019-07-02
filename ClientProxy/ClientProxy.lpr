library ClientProxy;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this }, fit_client_proxy,
  ta, int_fit_service, fit_server, fit_server_proxy, Main;
  
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

var
    FitServer: IFitServer;

function CreateFitServerInstance: IFitServer; cdecl;
begin
    FitServer := TFitServer_Proxy.Create
    (
        'IFitServer',
        'binary:',
        'TCP:Address=' + InternalIP +
        ';Port=' + InternalPort + ';target=IFitServer'
    );
    Result := FitServer;
end;

procedure DestroyFitServerInstance; cdecl;
begin
    FitServer._Release;
end;

exports
    CreateFitServiceInstance,
    DestroyFitServiceInstance,
    CreateFitServerInstance,
    DestroyFitServerInstance;
end.

