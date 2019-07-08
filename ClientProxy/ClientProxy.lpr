library ClientProxy;

{$MODE Delphi}

uses Classes, fit_client_proxy, ta, int_fit_service, fit_server,
    fit_server_proxy, Main, SyncObjs, fit_problem;
  
var
    FitService: IFitService;
    FitProblem: IFitProblem;
    FitServer: IFitServer;
    CS: TCriticalSection;
  
function CreateFitServiceInstance: IFitService; cdecl;
begin
    CS.Acquire;
    
    if FitService = Nil then
        FitService := TFitProblem.Create(nil) as IFitService;
    Result := FitService;
    
    CS.Release;
end;

procedure DestroyFitServiceInstance; cdecl;
begin
    CS.Acquire;
    
    FitService._Release;
    FitService := Nil;
    
    CS.Release;
end;

function CreateFitProblemInstance: IFitProblem; cdecl;
begin
    CS.Acquire;
    
    if FitProblem = Nil then
        FitProblem := TFitProblem.Create(nil);
    Result := FitProblem;
    
    CS.Release;
end;

procedure DestroyFitProblemInstance; cdecl;
begin
    CS.Acquire;
    
    FitProblem._Release;
    FitProblem := Nil;
    
    CS.Release;
end;

function CreateFitServerInstance: IFitServer; cdecl;
begin
    CS.Acquire;
    
    if FitServer = Nil then
        { Uses configuration parameters from Main module. }
        FitServer := TFitServer_Proxy.Create
        (
            'IFitServer',
            'binary:',
            'TCP:Address=' + InternalIP +
            ';Port=' + InternalPort + ';target=IFitServer'
        );
    Result := FitServer;
    
    CS.Release;
end;

procedure DestroyFitServerInstance; cdecl;
begin
    CS.Acquire;
    
    FitServer._Release;
    FitServer := Nil;
    
    CS.Release;
end;

exports
    CreateFitServiceInstance,
    DestroyFitServiceInstance,
    CreateFitServerInstance,
    DestroyFitServerInstance,
    CreateFitProblemInstance,
    DestroyFitProblemInstance;
end.

