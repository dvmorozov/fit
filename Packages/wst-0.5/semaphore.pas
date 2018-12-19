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
unit semaphore;

interface

uses
  Classes, SysUtils, syncobjs{$IFNDEF FPC},Windows{$ENDIF};

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  ESemaphoreException = class(Exception);

  { TSemaphoreObject }

  TSemaphoreObject = class
  private
    FHandle : {$IFNDEF FPC}THandle{$ELSE}PRTLEvent{$ENDIF};
    FLimit: Integer;
    {$IFDEF FPC}
    FCurrentState : Integer;
    FCriticalSection : TCriticalSection;
    {$ENDIF}
  public
    constructor Create(const ALimit : Integer);
    destructor Destroy(); override;
    function WaitFor(ATimeout : Cardinal) : TWaitResult;
    procedure Release();
    property Limit : Integer read FLimit;
  end;

implementation

{ TSemaphoreObject }

constructor TSemaphoreObject.Create(const ALimit: Integer);
begin
  Assert(ALimit>0);
  FLimit := ALimit;
{$IFNDEF FPC}
  FHandle := CreateSemaphore(nil,ALimit,ALimit,'');
{$ELSE}
  FHandle := RTLEventCreate();
  FCriticalSection := TCriticalSection.Create();
  FCurrentState := FLimit;
  RTLeventSetEvent(FHandle);  
{$ENDIF}
end;

destructor TSemaphoreObject.Destroy();
begin
{$IFNDEF FPC}
  CloseHandle(FHandle);
{$ELSE}
  RTLeventdestroy(FHandle);
  FreeAndNil(FCriticalSection);
{$ENDIF}
  inherited Destroy();
end;

function TSemaphoreObject.WaitFor(ATimeout: Cardinal): TWaitResult;
{$IFNDEF FPC}
var
  intRes : DWORD;
begin
  intRes := WaitForSingleObject(FHandle,ATimeout);
  case intRes of
    WAIT_OBJECT_0  : Result := wrSignaled;
    WAIT_TIMEOUT   : Result := wrTimeout;
    WAIT_ABANDONED : Result := wrAbandoned;
    else
                     Result := wrTimeout;
  end;
end;
{$ELSE}
var
  ok : Boolean;
begin
  Result := wrTimeout;
  ok := False;
  FCriticalSection.Acquire();
  try
    if ( FCurrentState > 0 ) then begin
      Dec(FCurrentState);
      ok := True;
      if ( FCurrentState = 0 ) then
        RTLeventResetEvent(FHandle);
    end;
  finally
    FCriticalSection.Release();
  end;
  if not ok then begin
    RTLeventWaitFor(FHandle,ATimeout);
    FCriticalSection.Acquire();
    try
      if ( FCurrentState > 0 ) then begin
        Dec(FCurrentState);
        ok := True;
      end;
    finally
      FCriticalSection.Release();
    end;
  end;
  if ok then
    Result := wrSignaled;
end;
{$ENDIF}

procedure TSemaphoreObject.Release();
begin
{$IFNDEF FPC}
  ReleaseSemaphore(FHandle,1,nil);
{$ELSE}
  FCriticalSection.Acquire();
  try
    if ( FCurrentState < Limit ) then begin
      Inc(FCurrentState);
    end else begin
      raise ESemaphoreException.Create('Invalid semaphore operation.');
    end;
  finally
    FCriticalSection.Release();
  end;
  RTLeventSetEvent(FHandle);
{$ENDIF}
end;

end.

