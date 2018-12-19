{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit metadata_service;

interface

uses
  Classes, SysUtils,
  base_service_intf, metadata_repository;

{$INCLUDE wst.inc}

type

  TWSTMtdOperationParam = class(TBaseComplexRemotable)
  private
    FModifier: TOperationParamFlag;
    FName: string;
    FTypeName: string;
  published
    property Name : string read FName write FName;
    property TypeName : string read FTypeName write FTypeName;
    property Modifier : TOperationParamFlag read FModifier write FModifier;
  end;

  TWSTMtdOperationParamArray = class(TBaseObjectArrayRemotable)
  protected
    function GetParam(AIndex: Integer): TWSTMtdOperationParam;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TWSTMtdOperationParam read GetParam;default;
  end;

  TWSTMtdServiceOperation = class(TBaseComplexRemotable)
  private
    FName: string;
    FParams: TWSTMtdOperationParamArray;
    function GetParams: TWSTMtdOperationParamArray;
    procedure SetParams(const AValue: TWSTMtdOperationParamArray);
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName;
    property Params : TWSTMtdOperationParamArray read GetParams write SetParams;
  end;

  TWSTMtdServiceOperationArray = class(TBaseObjectArrayRemotable)
  private
    function GetOperation(AIndex: Integer): TWSTMtdServiceOperation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TWSTMtdServiceOperation read GetOperation;default;
  end;

  TWSTMtdService = class(TBaseComplexRemotable)
  private
    FName: string;
    FOperations: TWSTMtdServiceOperationArray;
    function GetOperations: TWSTMtdServiceOperationArray;
    procedure SetOperations(const AValue: TWSTMtdServiceOperationArray);
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName;
    property Operations : TWSTMtdServiceOperationArray read GetOperations write SetOperations;
  end;

  TWSTMtdServiceArray = class(TBaseObjectArrayRemotable)
  protected
    function GetService(AIndex: Integer): TWSTMtdService;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    Property Item[AIndex:Integer] : TWSTMtdService Read GetService;Default;
  end;
  
  TWSTMtdRepository = class(TBaseComplexRemotable)
  private
    FName: string;
    FNameSpace: string;
    FServices : TWSTMtdServiceArray;
    function GetServices: TWSTMtdServiceArray;
    procedure SetServices(const AValue: TWSTMtdServiceArray);
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName;
    property NameSpace : string read FNameSpace write FNameSpace;
    property Services : TWSTMtdServiceArray read GetServices write SetServices;
  end;
  
  {The unique metadata public service}
  IWSTMetadataService = interface
    ['{804A3825-ADA5-4499-87BF-CF5491BFD674}']
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(const AName : string):TWSTMtdRepository;
  end;
  
  procedure Register_metadata_service_NameSpace();
  
implementation

procedure Register_metadata_service_NameSpace();
begin
  GetModuleMetadataMngr().SetRepositoryNameSpace('metadata_service',sWST_BASE_NS);
end;

procedure Register_metadata_service_Types();
var
  r : TTypeRegistry;
begin
  r := GetTypeRegistry();

  r.Register(sWST_BASE_NS,TypeInfo(TOperationParamFlag),'TOperationParamFlag');
  r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdOperationParam),'TWSTMtdOperationParam');
    r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdOperationParamArray),'TWSTMtdOperationParamArray');

  r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdOperationParam),'TWSTMtdOperationParam');
    r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdOperationParamArray),'TWSTMtdOperationParamArray');

  r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdServiceOperation),'TWSTMtdServiceOperation');
    r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdServiceOperationArray),'TWSTMtdServiceOperationArray');

  r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdService),'TWSTMtdService');
    r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdServiceArray),'TWSTMtdServiceArray');
    
  r.Register(sWST_BASE_NS,TypeInfo(TWSTMtdRepository),'TWSTMtdRepository');
end;

{ TWSTMtdServiceArray }

function TWSTMtdServiceArray.GetService(AIndex: Integer): TWSTMtdService;
begin
  Result := inherited Item[AIndex] as TWSTMtdService;
end;

class function TWSTMtdServiceArray.GetItemClass(): TBaseRemotableClass;
begin
  Result := TWSTMtdService;
end;

{ TWSTMtdRepository }

function TWSTMtdRepository.GetServices: TWSTMtdServiceArray;
begin
  Result := FServices;
end;

procedure TWSTMtdRepository.SetServices(const AValue: TWSTMtdServiceArray);
begin
  FServices.Assign(AValue);
end;

constructor TWSTMtdRepository.Create();
begin
  inherited Create();
  FServices := TWSTMtdServiceArray.Create();
end;

destructor TWSTMtdRepository.Destroy();
begin
  FreeAndNil(FServices);
  inherited Destroy();
end;

{ TWSTMtdOperationParamArray }

function TWSTMtdOperationParamArray.GetParam(AIndex: Integer): TWSTMtdOperationParam;
begin
  Result := inherited Item[AIndex] as TWSTMtdOperationParam;
end;

class function TWSTMtdOperationParamArray.GetItemClass(): TBaseRemotableClass;
begin
  Result := TWSTMtdOperationParam;
end;

{ TWSTMtdServiceOperation }

function TWSTMtdServiceOperation.GetParams: TWSTMtdOperationParamArray;
begin
  Result := FParams;
end;

procedure TWSTMtdServiceOperation.SetParams(const AValue: TWSTMtdOperationParamArray);
begin
  FParams.Assign(AValue);
end;

constructor TWSTMtdServiceOperation.Create();
begin
  inherited Create();
  FParams := TWSTMtdOperationParamArray.Create();
end;

destructor TWSTMtdServiceOperation.Destroy();
begin
  FreeAndNil(FParams);
  inherited Destroy();
end;

{ TWSTMtdServiceOperationArray }

function TWSTMtdServiceOperationArray.GetOperation(AIndex: Integer): TWSTMtdServiceOperation;
begin
  Result := inherited Item[AIndex] as TWSTMtdServiceOperation;
end;

class function TWSTMtdServiceOperationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result := TWSTMtdServiceOperation;
end;

{ TWSTMtdService }

function TWSTMtdService.GetOperations: TWSTMtdServiceOperationArray;
begin
  Result := FOperations;
end;

procedure TWSTMtdService.SetOperations(const AValue: TWSTMtdServiceOperationArray);
begin
  FOperations.Assign(AValue);
end;

constructor TWSTMtdService.Create();
begin
  FOperations := TWSTMtdServiceOperationArray.Create();
  inherited Create();
end;

destructor TWSTMtdService.Destroy();
begin
  FreeAndNil(FOperations);
  inherited Destroy();
end;

initialization
  Register_metadata_service_Types();
  
end.
