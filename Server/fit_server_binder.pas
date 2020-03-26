{
This unit has been produced by ws_helper.
  Input unit name : "fit_server".
  This unit name  : "fit_server_binder".
  Date            : "12.01.2009 11:43:17".
}
unit fit_server_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, int_fit_server,
  int_points_set;

type


  TFitServer_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure SmoothProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SubtractAllBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DoAllAutomaticallyHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure MinimizeDifferenceHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure MinimizeNumberOfSpecimensHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure FindSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure FindSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure FindBackPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure StopAsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SelectAreaHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReturnToTotalProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure CreateSpecimenListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetBackgroundPointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSelectedAreaHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetBackgroundPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCalcProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetDeltaProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetCurveThreshHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetMaxRFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetMaxRFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetBackFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetBackFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurveTypeHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetCurveTypeHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetWaveLengthHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetWaveLengthHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurveThreshHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetStateHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReplacePointInProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReplacePointInBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReplacePointInSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReplacePointInSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure CreateProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DiscardProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenParameterCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSpecimenParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToDataHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetGraphHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfileChunkHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfileChunkCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetSpecimenParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCalcTimeStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetAbsRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSqrRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

  TFitServer_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterFitServerService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ The module is generated, so all hints are suppressed. }
{$hints off}

{ TFitServer_ServiceBinder implementation }
procedure TFitServer_ServiceBinder.SmoothProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SmoothProfile(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SubtractAllBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  Auto : boolean;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'Auto';  AFormatter.Get(TypeInfo(boolean),strPrmName,Auto);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SubtractBackground(Auto,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.DoAllAutomaticallyHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.DoAllAutomatically(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.MinimizeDifferenceHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.MinimizeDifference(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.MinimizeNumberOfSpecimensHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.MinimizeNumberOfSpecimens(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.FindSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.FindSpecimenIntervals(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.FindSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.FindSpecimenPositions(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.FindBackPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.FindBackPoints(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.StopAsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.StopAsyncOper(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.AsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TBoolResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AsyncOper(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TBoolResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SelectAreaHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  StartPointIndex : integer;
  StopPointIndex : integer;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'StartPointIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,StartPointIndex);
  strPrmName := 'StopPointIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,StopPointIndex);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SelectArea(StartPointIndex,StopPointIndex,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.ReturnToTotalProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ReturnToTotalProfile(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.CreateSpecimenListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.CreateSpecimenList(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  PointsSet : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(PointsSet) := nil;
  
  strPrmName := 'PointsSet';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,PointsSet);
  if Assigned(Pointer(PointsSet)) then
    callCtx.AddObjectToFree(TObject(PointsSet));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetProfilePointsSet(PointsSet,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetBackgroundPointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  BackgroundPoints : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(BackgroundPoints) := nil;
  
  strPrmName := 'BackgroundPoints';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,BackgroundPoints);
  if Assigned(Pointer(BackgroundPoints)) then
    callCtx.AddObjectToFree(TObject(BackgroundPoints));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetBackgroundPointsSet(BackgroundPoints,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  SpecimenPositions : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(SpecimenPositions) := nil;
  
  strPrmName := 'SpecimenPositions';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,SpecimenPositions);
  if Assigned(Pointer(SpecimenPositions)) then
    callCtx.AddObjectToFree(TObject(SpecimenPositions));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetSpecimenPositions(SpecimenPositions,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  SpecimenIntervals : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(SpecimenIntervals) := nil;
  
  strPrmName := 'SpecimenIntervals';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,SpecimenIntervals);
  if Assigned(Pointer(SpecimenIntervals)) then
    callCtx.AddObjectToFree(TObject(SpecimenIntervals));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetSpecimenIntervals(SpecimenIntervals,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.AddPointToBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  XValue : Double;
  YValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'XValue';  AFormatter.Get(TypeInfo(Double),strPrmName,XValue);
  strPrmName := 'YValue';  AFormatter.Get(TypeInfo(Double),strPrmName,YValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddPointToBackground(XValue,YValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.AddPointToSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  XValue : Double;
  YValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'XValue';  AFormatter.Get(TypeInfo(Double),strPrmName,XValue);
  strPrmName := 'YValue';  AFormatter.Get(TypeInfo(Double),strPrmName,YValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddPointToSpecimenIntervals(XValue,YValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.AddPointToSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  XValue : Double;
  YValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'XValue';  AFormatter.Get(TypeInfo(Double),strPrmName,XValue);
  strPrmName := 'YValue';  AFormatter.Get(TypeInfo(Double),strPrmName,YValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddPointToSpecimenPositions(XValue,YValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetProfilePointsSet(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSelectedAreaHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSelectedArea(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetBackgroundPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetBackgroundPoints(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenPositions(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenIntervals(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetCalcProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCalcProfilePointsSet(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetDeltaProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetDeltaProfilePointsSet(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetCurveThreshHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  CurveThresh : Double;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'CurveThresh';  AFormatter.Get(TypeInfo(Double),strPrmName,CurveThresh);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetCurveThresh(CurveThresh,ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetMaxRFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : Double;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetMaxRFactor(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(Double),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetMaxRFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  MaxRFactor : Double;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'MaxRFactor';  AFormatter.Get(TypeInfo(Double),strPrmName,MaxRFactor);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetMaxRFactor(MaxRFactor,ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetBackFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : Double;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetBackFactor(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(Double),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetBackFactorHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  BackFactor : Double;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'BackFactor';  AFormatter.Get(TypeInfo(Double),strPrmName,BackFactor);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetBackFactor(BackFactor,ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetCurveTypeHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TCurveTypeId;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCurveType(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetCurveTypeHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  CurveTypeId : TCurveTypeId;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'CurveTypeId';  AFormatter.Get(TypeInfo(integer),strPrmName,CurveTypeId);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetCurveType(CurveTypeId,ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetWaveLengthHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : Double;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetWaveLength(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(Double),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetWaveLengthHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  WaveLength : Double;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'WaveLength';  AFormatter.Get(TypeInfo(Double),strPrmName,WaveLength);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetWaveLength(WaveLength,ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetCurveThreshHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : Double;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCurveThresh(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(Double),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetStateHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetState(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.ReplacePointInProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  PrevXValue : Double;
  PrevYValue : Double;
  NewXValue : Double;
  NewYValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'PrevXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevXValue);
  strPrmName := 'PrevYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevYValue);
  strPrmName := 'NewXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewXValue);
  strPrmName := 'NewYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewYValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ReplacePointInProfile(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.ReplacePointInBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  PrevXValue : Double;
  PrevYValue : Double;
  NewXValue : Double;
  NewYValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'PrevXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevXValue);
  strPrmName := 'PrevYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevYValue);
  strPrmName := 'NewXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewXValue);
  strPrmName := 'NewYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewYValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ReplacePointInBackground(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.ReplacePointInSpecimenIntervalsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  PrevXValue : Double;
  PrevYValue : Double;
  NewXValue : Double;
  NewYValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'PrevXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevXValue);
  strPrmName := 'PrevYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevYValue);
  strPrmName := 'NewXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewXValue);
  strPrmName := 'NewYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewYValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ReplacePointInSpecimenIntervals(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.ReplacePointInSpecimenPositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  PrevXValue : Double;
  PrevYValue : Double;
  NewXValue : Double;
  NewYValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'PrevXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevXValue);
  strPrmName := 'PrevYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,PrevYValue);
  strPrmName := 'NewXValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewXValue);
  strPrmName := 'NewYValue';  AFormatter.Get(TypeInfo(Double),strPrmName,NewYValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ReplacePointInSpecimenPositions(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.CreateProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  returnVal : integer;
begin
  callCtx := AContext;
  
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.CreateProblem();
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(integer),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.DiscardProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
begin
  callCtx := AContext;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.DiscardProblem(ProblemID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TIntResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenCount(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TIntResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  SpecIndex : integer;
  ProblemID : integer;
  returnVal : TNamedPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'SpecIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,SpecIndex);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenPoints(SpecIndex,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TNamedPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenParameterCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  SpecIndex : integer;
  returnVal : TIntResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  strPrmName := 'SpecIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,SpecIndex);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenParameterCount(ProblemID,SpecIndex);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TIntResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSpecimenParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  SpecIndex : integer;
  ParamIndex : integer;
  returnVal : TSpecParamResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  strPrmName := 'SpecIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,SpecIndex);
  strPrmName := 'ParamIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,ParamIndex);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSpecimenParameter(ProblemID,SpecIndex,ParamIndex);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TSpecParamResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.AddPointToDataHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  XValue : Double;
  YValue : Double;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'XValue';  AFormatter.Get(TypeInfo(Double),strPrmName,XValue);
  strPrmName := 'YValue';  AFormatter.Get(TypeInfo(Double),strPrmName,YValue);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.AddPointToData(XValue,YValue,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetGraphHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  Width : integer;
  Height : integer;
  ProblemID : integer;
  returnVal : TPictureResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'Width';  AFormatter.Get(TypeInfo(integer),strPrmName,Width);
  strPrmName := 'Height';  AFormatter.Get(TypeInfo(integer),strPrmName,Height);
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetGraph(Width,Height,ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPictureResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetProfileChunkHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  ChunkNum : integer;
  returnVal : TPointsResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  strPrmName := 'ChunkNum';  AFormatter.Get(TypeInfo(integer),strPrmName,ChunkNum);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetProfileChunk(ProblemID,ChunkNum);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TPointsResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetProfileChunkCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TIntResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetProfileChunkCount(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TIntResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.SetSpecimenParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  SpecIndex : integer;
  ParamIndex : integer;
  Value : Double;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  strPrmName := 'SpecIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,SpecIndex);
  strPrmName := 'ParamIndex';  AFormatter.Get(TypeInfo(integer),strPrmName,ParamIndex);
  strPrmName := 'Value';  AFormatter.Get(TypeInfo(Double),strPrmName,Value);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetSpecimenParameter(ProblemID,SpecIndex,ParamIndex,Value);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetCalcTimeStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TStringResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCalcTimeStr(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TStringResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TStringResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetRFactorStr(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TStringResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetAbsRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TStringResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetAbsRFactorStr(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TStringResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TFitServer_ServiceBinder.GetSqrRFactorStrHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  ProblemID : integer;
  returnVal : TStringResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSqrRFactorStr(ProblemID);
    if Assigned(TObject(returnVal)) then
      callCtx.AddObjectToFree(TObject(returnVal));
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('Result',TypeInfo(TStringResult),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TFitServer_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('IFitServer'));
  RegisterVerbHandler('SmoothProfile',{$IFDEF FPC}@{$ENDIF}SmoothProfileHandler);
  RegisterVerbHandler('SubtractBackground',{$IFDEF FPC}@{$ENDIF}SubtractAllBackgroundHandler);
  RegisterVerbHandler('DoAllAutomatically',{$IFDEF FPC}@{$ENDIF}DoAllAutomaticallyHandler);
  RegisterVerbHandler('MinimizeDifference',{$IFDEF FPC}@{$ENDIF}MinimizeDifferenceHandler);
  RegisterVerbHandler('MinimizeNumberOfSpecimens',{$IFDEF FPC}@{$ENDIF}MinimizeNumberOfSpecimensHandler);
  RegisterVerbHandler('FindSpecimenIntervals',{$IFDEF FPC}@{$ENDIF}FindSpecimenIntervalsHandler);
  RegisterVerbHandler('FindSpecimenPositions',{$IFDEF FPC}@{$ENDIF}FindSpecimenPositionsHandler);
  RegisterVerbHandler('FindBackPoints',{$IFDEF FPC}@{$ENDIF}FindBackPointsHandler);
  RegisterVerbHandler('StopAsyncOper',{$IFDEF FPC}@{$ENDIF}StopAsyncOperHandler);
  RegisterVerbHandler('AsyncOper',{$IFDEF FPC}@{$ENDIF}AsyncOperHandler);
  RegisterVerbHandler('SelectArea',{$IFDEF FPC}@{$ENDIF}SelectAreaHandler);
  RegisterVerbHandler('ReturnToTotalProfile',{$IFDEF FPC}@{$ENDIF}ReturnToTotalProfileHandler);
  RegisterVerbHandler('CreateSpecimenList',{$IFDEF FPC}@{$ENDIF}CreateSpecimenListHandler);
  RegisterVerbHandler('SetProfilePointsSet',{$IFDEF FPC}@{$ENDIF}SetProfilePointsSetHandler);
  RegisterVerbHandler('SetBackgroundPointsSet',{$IFDEF FPC}@{$ENDIF}SetBackgroundPointsSetHandler);
  RegisterVerbHandler('SetSpecimenPositions',{$IFDEF FPC}@{$ENDIF}SetSpecimenPositionsHandler);
  RegisterVerbHandler('SetSpecimenIntervals',{$IFDEF FPC}@{$ENDIF}SetSpecimenIntervalsHandler);
  RegisterVerbHandler('AddPointToBackground',{$IFDEF FPC}@{$ENDIF}AddPointToBackgroundHandler);
  RegisterVerbHandler('AddPointToSpecimenIntervals',{$IFDEF FPC}@{$ENDIF}AddPointToSpecimenIntervalsHandler);
  RegisterVerbHandler('AddPointToSpecimenPositions',{$IFDEF FPC}@{$ENDIF}AddPointToSpecimenPositionsHandler);
  RegisterVerbHandler('GetProfilePointsSet',{$IFDEF FPC}@{$ENDIF}GetProfilePointsSetHandler);
  RegisterVerbHandler('GetSelectedArea',{$IFDEF FPC}@{$ENDIF}GetSelectedAreaHandler);
  RegisterVerbHandler('GetBackgroundPoints',{$IFDEF FPC}@{$ENDIF}GetBackgroundPointsHandler);
  RegisterVerbHandler('GetSpecimenPositions',{$IFDEF FPC}@{$ENDIF}GetSpecimenPositionsHandler);
  RegisterVerbHandler('GetSpecimenIntervals',{$IFDEF FPC}@{$ENDIF}GetSpecimenIntervalsHandler);
  RegisterVerbHandler('GetCalcProfilePointsSet',{$IFDEF FPC}@{$ENDIF}GetCalcProfilePointsSetHandler);
  RegisterVerbHandler('GetDeltaProfilePointsSet',{$IFDEF FPC}@{$ENDIF}GetDeltaProfilePointsSetHandler);
  RegisterVerbHandler('SetCurveThresh',{$IFDEF FPC}@{$ENDIF}SetCurveThreshHandler);
  RegisterVerbHandler('GetMaxRFactor',{$IFDEF FPC}@{$ENDIF}GetMaxRFactorHandler);
  RegisterVerbHandler('SetMaxRFactor',{$IFDEF FPC}@{$ENDIF}SetMaxRFactorHandler);
  RegisterVerbHandler('GetBackFactor',{$IFDEF FPC}@{$ENDIF}GetBackFactorHandler);
  RegisterVerbHandler('SetBackFactor',{$IFDEF FPC}@{$ENDIF}SetBackFactorHandler);
  RegisterVerbHandler('GetCurveType',{$IFDEF FPC}@{$ENDIF}GetCurveTypeHandler);
  RegisterVerbHandler('SetCurveType',{$IFDEF FPC}@{$ENDIF}SetCurveTypeHandler);
  RegisterVerbHandler('GetWaveLength',{$IFDEF FPC}@{$ENDIF}GetWaveLengthHandler);
  RegisterVerbHandler('SetWaveLength',{$IFDEF FPC}@{$ENDIF}SetWaveLengthHandler);
  RegisterVerbHandler('GetCurveThresh',{$IFDEF FPC}@{$ENDIF}GetCurveThreshHandler);
  RegisterVerbHandler('GetState',{$IFDEF FPC}@{$ENDIF}GetStateHandler);
  RegisterVerbHandler('ReplacePointInProfile',{$IFDEF FPC}@{$ENDIF}ReplacePointInProfileHandler);
  RegisterVerbHandler('ReplacePointInBackground',{$IFDEF FPC}@{$ENDIF}ReplacePointInBackgroundHandler);
  RegisterVerbHandler('ReplacePointInSpecimenIntervals',{$IFDEF FPC}@{$ENDIF}ReplacePointInSpecimenIntervalsHandler);
  RegisterVerbHandler('ReplacePointInSpecimenPositions',{$IFDEF FPC}@{$ENDIF}ReplacePointInSpecimenPositionsHandler);
  RegisterVerbHandler('CreateProblem',{$IFDEF FPC}@{$ENDIF}CreateProblemHandler);
  RegisterVerbHandler('DiscardProblem',{$IFDEF FPC}@{$ENDIF}DiscardProblemHandler);
  RegisterVerbHandler('GetSpecimenCount',{$IFDEF FPC}@{$ENDIF}GetSpecimenCountHandler);
  RegisterVerbHandler('GetSpecimenPoints',{$IFDEF FPC}@{$ENDIF}GetSpecimenPointsHandler);
  RegisterVerbHandler('GetSpecimenParameterCount',{$IFDEF FPC}@{$ENDIF}GetSpecimenParameterCountHandler);
  RegisterVerbHandler('GetSpecimenParameter',{$IFDEF FPC}@{$ENDIF}GetSpecimenParameterHandler);
  RegisterVerbHandler('AddPointToData',{$IFDEF FPC}@{$ENDIF}AddPointToDataHandler);
  RegisterVerbHandler('GetGraph',{$IFDEF FPC}@{$ENDIF}GetGraphHandler);
  RegisterVerbHandler('GetProfileChunk',{$IFDEF FPC}@{$ENDIF}GetProfileChunkHandler);
  RegisterVerbHandler('GetProfileChunkCount',{$IFDEF FPC}@{$ENDIF}GetProfileChunkCountHandler);
  RegisterVerbHandler('SetSpecimenParameter',{$IFDEF FPC}@{$ENDIF}SetSpecimenParameterHandler);
  RegisterVerbHandler('GetCalcTimeStr',{$IFDEF FPC}@{$ENDIF}GetCalcTimeStrHandler);
  RegisterVerbHandler('GetRFactorStr',{$IFDEF FPC}@{$ENDIF}GetRFactorStrHandler);
  RegisterVerbHandler('GetAbsRFactorStr',{$IFDEF FPC}@{$ENDIF}GetAbsRFactorStrHandler);
  RegisterVerbHandler('GetSqrRFactorStr',{$IFDEF FPC}@{$ENDIF}GetSqrRFactorStrHandler);
end;


{ TFitServer_ServiceBinderFactory }

function TFitServer_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TFitServer_ServiceBinderFactory.Create();
begin
  FInstance := TFitServer_ServiceBinder.Create() as IInterface;
end;

destructor TFitServer_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterFitServerService();
Begin
  GetServerServiceRegistry().Register('IFitServer',TFitServer_ServiceBinderFactory.Create() as IItemFactory);
End;

{$hints on}

initialization
  {$i fit_server.wst}

  {$IF DECLARED(Register_fit_server_ServiceMetadata)}
  Register_fit_server_ServiceMetadata();
  {$IFEND}
End.
