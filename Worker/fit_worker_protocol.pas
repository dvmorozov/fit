// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Line-delimited JSON protocol shared by the compute worker and the
client that supervises it.)

Each request and each response is a single JSON object on one line (D12: a
stdio / pipe transport, no TCP port). This unit is LCL-free and only depends on
fpjson, so both the worker executable and the desktop client link it.
}
unit fit_worker_protocol;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpjson, jsonparser;

const
    { Bumped when the request/response shape changes incompatibly. }
    WORKER_PROTOCOL_VERSION = 1;

{ Parses one line into a JSON object; returns nil on empty or malformed input.
  The caller owns the result. }
function ParseMessage(const Line: string): TJSONObject;
{ The 'op' field of a request message, or '' when absent / message is nil. }
function MessageOp(Msg: TJSONObject): string;
{ A one-line success response. Any fields in AData are merged into the response;
  AData is freed (pass nil for a bare {"ok":true}). }
function OkResponse(AData: TJSONObject = nil): string;
{ A one-line error response carrying a human-readable message. }
function ErrorResponse(const AMessage: string): string;

implementation

function ParseMessage(const Line: string): TJSONObject;
var
    D: TJSONData;
begin
    Result := nil;
    if Trim(Line) = '' then
        Exit;
    try
        D := GetJSON(Line);
        if D is TJSONObject then
            Result := TJSONObject(D)
        else
            D.Free;
    except
        Result := nil;
    end;
end;

function MessageOp(Msg: TJSONObject): string;
begin
    if Assigned(Msg) then
        Result := Msg.Get('op', '')
    else
        Result := '';
end;

function OkResponse(AData: TJSONObject): string;
var
    O: TJSONObject;
    Nm: string;
begin
    O := TJSONObject.Create;
    try
        O.Add('ok', True);
        if Assigned(AData) then
        begin
            //  Move every field from AData into the response, then free AData.
            //  Read the name before Extract removes the element (argument
            //  evaluation order is not guaranteed).
            while AData.Count > 0 do
            begin
                Nm := AData.Names[0];
                O.Add(Nm, AData.Extract(0));
            end;
            AData.Free;
        end;
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

function ErrorResponse(const AMessage: string): string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add('ok', False);
        O.Add('error', AMessage);
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

end.
