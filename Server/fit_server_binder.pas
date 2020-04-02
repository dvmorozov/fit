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
    procedure SubtractBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DoAllAutomaticallyHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure MinimizeDifferenceHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure MinimizeNumberOfCurvesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ComputeCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ComputeCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ComputeBackgroundPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure StopAsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AsyncOperHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SelectProfileIntervalHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SelectEntireProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure CreateCurveListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetBackgroundPointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToRFactorBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfilePointsSetHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSelectedProfileIntervalHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetBackgroundPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetRFactorBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    procedure ReplacePointInCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ReplacePointInCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure CreateProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DiscardProblemHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurveCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurvePointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurveParameterCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCurveParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure AddPointToProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetGraphHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfileChunkHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProfileChunkCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetCurveParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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

procedure TFitServer_ServiceBinder.SubtractBackgroundHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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

procedure TFitServer_ServiceBinder.MinimizeNumberOfCurvesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.MinimizeNumberOfCurves(ProblemID);
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

procedure TFitServer_ServiceBinder.ComputeCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.ComputeCurveBounds(ProblemID);
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

procedure TFitServer_ServiceBinder.ComputeCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.ComputeCurvePositions(ProblemID);
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

procedure TFitServer_ServiceBinder.ComputeBackgroundPointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.ComputeBackgroundPoints(ProblemID);
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

procedure TFitServer_ServiceBinder.SelectProfileIntervalHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.SelectProfileInterval(StartPointIndex,StopPointIndex,ProblemID);
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

procedure TFitServer_ServiceBinder.SelectEntireProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.SelectEntireProfile(ProblemID);
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

procedure TFitServer_ServiceBinder.CreateCurveListHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.CreateCurveList(ProblemID);
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

procedure TFitServer_ServiceBinder.SetCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  CurvePositions : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(CurvePositions) := nil;
  
  strPrmName := 'CurvePositions';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,CurvePositions);
  if Assigned(Pointer(CurvePositions)) then
    callCtx.AddObjectToFree(TObject(CurvePositions));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetCurvePositions(CurvePositions,ProblemID);
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

procedure TFitServer_ServiceBinder.SetCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IFitServer;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  CurveBounds : TArrayOfFloatDoubleRemotable;
  ProblemID : integer;
  returnVal : TResult;
begin
  callCtx := AContext;
  TObject(returnVal) := nil;
  TObject(CurveBounds) := nil;
  
  strPrmName := 'CurveBounds';  AFormatter.Get(TypeInfo(TArrayOfFloatDoubleRemotable),strPrmName,CurveBounds);
  if Assigned(Pointer(CurveBounds)) then
    callCtx.AddObjectToFree(TObject(CurveBounds));
  strPrmName := 'ProblemID';  AFormatter.Get(TypeInfo(integer),strPrmName,ProblemID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IFitServer;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetCurveBounds(CurveBounds,ProblemID);
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

procedure TFitServer_ServiceBinder.AddPointToRFactorBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.AddPointToRFactorBounds(XValue,YValue,ProblemID);
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

procedure TFitServer_ServiceBinder.AddPointToCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.AddPointToCurvePositions(XValue,YValue,ProblemID);
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

procedure TFitServer_ServiceBinder.GetSelectedProfileIntervalHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetSelectedProfileInterval(ProblemID);
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

procedure TFitServer_ServiceBinder.GetCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetCurvePositions(ProblemID);
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

procedure TFitServer_ServiceBinder.SetRFactorBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.SetRFactorBounds(ProblemID);
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

procedure TFitServer_ServiceBinder.ReplacePointInCurveBoundsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.ReplacePointInCurveBounds(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
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

procedure TFitServer_ServiceBinder.ReplacePointInCurvePositionsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.ReplacePointInCurvePositions(PrevXValue,PrevYValue,NewXValue,NewYValue,ProblemID);
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

procedure TFitServer_ServiceBinder.GetCurveCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetCurveCount(ProblemID);
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

procedure TFitServer_ServiceBinder.GetCurvePointsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetCurvePoints(SpecIndex,ProblemID);
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

procedure TFitServer_ServiceBinder.GetCurveParameterCountHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetCurveParameterCount(ProblemID,SpecIndex);
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

procedure TFitServer_ServiceBinder.GetCurveParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.GetCurveParameter(ProblemID,SpecIndex,ParamIndex);
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

procedure TFitServer_ServiceBinder.AddPointToProfileHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.AddPointToProfile(XValue,YValue,ProblemID);
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

procedure TFitServer_ServiceBinder.SetCurveParameterHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
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
    returnVal := tmpObj.SetCurveParameter(ProblemID,SpecIndex,ParamIndex,Value);
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
  RegisterVerbHandler('SubtractBackground',{$IFDEF FPC}@{$ENDIF}SubtractBackgroundHandler);
  RegisterVerbHandler('DoAllAutomatically',{$IFDEF FPC}@{$ENDIF}DoAllAutomaticallyHandler);
  RegisterVerbHandler('MinimizeDifference',{$IFDEF FPC}@{$ENDIF}MinimizeDifferenceHandler);
  RegisterVerbHandler('MinimizeNumberOfCurves',{$IFDEF FPC}@{$ENDIF}MinimizeNumberOfCurvesHandler);
  RegisterVerbHandler('ComputeCurveBounds',{$IFDEF FPC}@{$ENDIF}ComputeCurveBoundsHandler);
  RegisterVerbHandler('ComputeCurvePositions',{$IFDEF FPC}@{$ENDIF}ComputeCurvePositionsHandler);
  RegisterVerbHandler('ComputeBackgroundPoints',{$IFDEF FPC}@{$ENDIF}ComputeBackgroundPointsHandler);
  RegisterVerbHandler('StopAsyncOper',{$IFDEF FPC}@{$ENDIF}StopAsyncOperHandler);
  RegisterVerbHandler('AsyncOper',{$IFDEF FPC}@{$ENDIF}AsyncOperHandler);
  RegisterVerbHandler('SelectProfileInterval',{$IFDEF FPC}@{$ENDIF}SelectProfileIntervalHandler);
  RegisterVerbHandler('SelectEntireProfile',{$IFDEF FPC}@{$ENDIF}SelectEntireProfileHandler);
  RegisterVerbHandler('CreateCurveList',{$IFDEF FPC}@{$ENDIF}CreateCurveListHandler);
  RegisterVerbHandler('SetProfilePointsSet',{$IFDEF FPC}@{$ENDIF}SetProfilePointsSetHandler);
  RegisterVerbHandler('SetBackgroundPointsSet',{$IFDEF FPC}@{$ENDIF}SetBackgroundPointsSetHandler);
  RegisterVerbHandler('SetCurvePositions',{$IFDEF FPC}@{$ENDIF}SetCurvePositionsHandler);
  RegisterVerbHandler('SetCurveBounds',{$IFDEF FPC}@{$ENDIF}SetCurveBoundsHandler);
  RegisterVerbHandler('AddPointToBackground',{$IFDEF FPC}@{$ENDIF}AddPointToBackgroundHandler);
  RegisterVerbHandler('AddPointToRFactorBounds',{$IFDEF FPC}@{$ENDIF}AddPointToRFactorBoundsHandler);
  RegisterVerbHandler('AddPointToCurvePositions',{$IFDEF FPC}@{$ENDIF}AddPointToCurvePositionsHandler);
  RegisterVerbHandler('GetProfilePointsSet',{$IFDEF FPC}@{$ENDIF}GetProfilePointsSetHandler);
  RegisterVerbHandler('GetSelectedProfileInterval',{$IFDEF FPC}@{$ENDIF}GetSelectedProfileIntervalHandler);
  RegisterVerbHandler('GetBackgroundPoints',{$IFDEF FPC}@{$ENDIF}GetBackgroundPointsHandler);
  RegisterVerbHandler('GetCurvePositions',{$IFDEF FPC}@{$ENDIF}GetCurvePositionsHandler);
  RegisterVerbHandler('SetRFactorBounds',{$IFDEF FPC}@{$ENDIF}SetRFactorBoundsHandler);
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
  RegisterVerbHandler('ReplacePointInCurveBounds',{$IFDEF FPC}@{$ENDIF}ReplacePointInCurveBoundsHandler);
  RegisterVerbHandler('ReplacePointInCurvePositions',{$IFDEF FPC}@{$ENDIF}ReplacePointInCurvePositionsHandler);
  RegisterVerbHandler('CreateProblem',{$IFDEF FPC}@{$ENDIF}CreateProblemHandler);
  RegisterVerbHandler('DiscardProblem',{$IFDEF FPC}@{$ENDIF}DiscardProblemHandler);
  RegisterVerbHandler('GetCurveCount',{$IFDEF FPC}@{$ENDIF}GetCurveCountHandler);
  RegisterVerbHandler('GetCurvePoints',{$IFDEF FPC}@{$ENDIF}GetCurvePointsHandler);
  RegisterVerbHandler('GetCurveParameterCount',{$IFDEF FPC}@{$ENDIF}GetCurveParameterCountHandler);
  RegisterVerbHandler('GetCurveParameter',{$IFDEF FPC}@{$ENDIF}GetCurveParameterHandler);
  RegisterVerbHandler('AddPointToProfile',{$IFDEF FPC}@{$ENDIF}AddPointToProfileHandler);
  RegisterVerbHandler('GetGraph',{$IFDEF FPC}@{$ENDIF}GetGraphHandler);
  RegisterVerbHandler('GetProfileChunk',{$IFDEF FPC}@{$ENDIF}GetProfileChunkHandler);
  RegisterVerbHandler('GetProfileChunkCount',{$IFDEF FPC}@{$ENDIF}GetProfileChunkCountHandler);
  RegisterVerbHandler('SetCurveParameter',{$IFDEF FPC}@{$ENDIF}SetCurveParameterHandler);
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
