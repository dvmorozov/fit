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
unit library_imp_utils;

interface

uses
  Classes, SysUtils;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  IwstModule = interface
    ['{A62A9A71-727E-47AD-9B84-0F7CA0AE51D5}']
    function GetFileName():string;
    function GetProc(const AProcName : string):Pointer;
  end;
  
  IwstModuleManager = interface
    ['{0A49D315-FF3E-40CD-BCA0-F958BCD5C57F}']
    function Get(const AFileName : string):IwstModule;
    procedure Clear();
  end;

var
  LibraryManager : IwstModuleManager = nil;

implementation
{$IFDEF FPC}
  uses DynLibs;
{$ELSE}
  uses Windows;

  type TLibHandle = THandle;
  const NilHandle = 0;
{$ENDIF}


type

  { TwstModule }

  TwstModule = class(TInterfacedObject,IwstModule)
  private
    FFileName : string;
    FHandle : TLibHandle;
  private
    procedure Load(const ADoLoad : Boolean);
  protected
    function GetFileName():string;
    function GetProc(const AProcName : string):Pointer;
  public
    constructor Create(const AFileName : string);
    destructor Destroy();override;
  end;

  { TwstModuleManager }

  TwstModuleManager = class(TInterfacedObject,IwstModuleManager)
  private
    FList : IInterfaceList;
  private
    function Load(const AFileName : string):IwstModule;
    function GetItem(const AIndex : Integer):IwstModule;
    function IndexOf(const AFileName : string):Integer;
  protected
    function Get(const AFileName : string):IwstModule;
    procedure Clear();
  public
    constructor Create();
    destructor Destroy();override;
  end;

procedure TwstModule.Load(const ADoLoad : Boolean);
begin
  if ADoLoad then begin
    if ( FHandle = NilHandle ) then begin
      {$IFDEF FPC}
      FHandle := LoadLibrary(FFileName);
      {$ELSE}
      FHandle := LoadLibrary(PCHAR(FFileName));
      {$ENDIF}
      if ( FHandle = NilHandle ) then
        raise Exception.CreateFmt('Error while loading : "%s".',[FFileName]);
    end;
  end else begin
    if ( FHandle <> NilHandle ) then begin
      FreeLibrary(FHandle);
      FHandle := NilHandle;
    end;
  end;
end;

function TwstModule.GetFileName(): string;
begin
  Result := FFileName;
end;

function TwstModule.GetProc(const AProcName: string): Pointer;
begin
  {$IFDEF FPC}
  Result := GetProcAddress(FHandle,AProcName);
  {$ELSE}
  Result := GetProcAddress(FHandle,PCHAR(AProcName));
  {$ENDIF}
  if not Assigned(Result) then
    raise Exception.CreateFmt('Procedure "%s" not found in this module( "%s" ).',[AProcName,FFileName]);
end;

constructor TwstModule.Create(const AFileName: string);
begin
  if not FileExists(AFileName) then
    raise Exception.CreateFmt('File not found : "%s".',[AFileName]);
  FHandle := NilHandle;
  FFileName := AFileName;
  Load(True);
end;

destructor TwstModule.Destroy();
begin
  Load(False);
  inherited Destroy();
end;

{ TwstModuleManager }

function TwstModuleManager.Get(const AFileName: string): IwstModule;
var
  i : Integer;
begin
  i := IndexOf(AFileName);
  if ( i < 0 ) then
    Result := Load(AFileName)
  else
    Result := GetItem(i);
end;

procedure TwstModuleManager.Clear();
begin
  FList.Clear();
end;

function TwstModuleManager.Load(const AFileName: string): IwstModule;
begin
  Result := TwstModule.Create(AFileName);
end;

function TwstModuleManager.GetItem(const AIndex: Integer): IwstModule;
begin
  Result := FList[AIndex] as IwstModule;
end;

function TwstModuleManager.IndexOf(const AFileName: string): Integer;
begin
  for Result := 0 to Pred(FList.Count) do begin
    if AnsiSameStr(AFileName,(FList[Result] as IwstModule).GetFileName()) then
      Exit;
  end;
  Result := -1;
end;

constructor TwstModuleManager.Create();
begin
  inherited;
  FList := TInterfaceList.Create();
end;

destructor TwstModuleManager.Destroy();
begin
  FList := nil;
  inherited Destroy();
end;

procedure InitLibraryManager();
begin
  LibraryManager := TwstModuleManager.Create();
end;

initialization
  InitLibraryManager();
  
finalization
  LibraryManager := nil;
  
end.

