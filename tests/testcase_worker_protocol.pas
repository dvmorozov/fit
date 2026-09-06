// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the worker's line-delimited JSON protocol (fit_worker_protocol),
  the transport shared by the compute worker and its supervising client. }
unit testcase_worker_protocol;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, fpjson, fit_worker_protocol;
type
  TWorkerProtocolTest = class(TTestCase)
  published
    procedure ParsesOpFromRequest;
    procedure IgnoresMalformedLine;
    procedure OkResponseRoundTrips;
    procedure ErrorResponseCarriesMessage;
  end;

implementation

procedure TWorkerProtocolTest.ParsesOpFromRequest;
var m: TJSONObject;
begin
  m := ParseMessage('{"op":"ping"}');
  try
    AssertTrue('request parsed', Assigned(m));
    AssertEquals('op read back', 'ping', MessageOp(m));
  finally
    m.Free;
  end;
end;

procedure TWorkerProtocolTest.IgnoresMalformedLine;
var m: TJSONObject;
begin
  //  A malformed line yields nil (worker replies with an error, never crashes).
  m := ParseMessage('this is not json');
  AssertTrue('nil on malformed input', not Assigned(m));
  AssertEquals('empty op for nil message', '', MessageOp(m));
end;

procedure TWorkerProtocolTest.OkResponseRoundTrips;
var s: string; d, data: TJSONObject;
begin
  data := TJSONObject.Create;
  data.Add('version', 1);
  s := OkResponse(data);   //  takes ownership of data
  d := ParseMessage(s);
  try
    AssertTrue('response parsed', Assigned(d));
    AssertTrue('ok is true', d.Get('ok', False));
    AssertEquals('merged field survives', 1, d.Get('version', 0));
  finally
    d.Free;
  end;
end;

procedure TWorkerProtocolTest.ErrorResponseCarriesMessage;
var s: string; d: TJSONObject;
begin
  s := ErrorResponse('boom');
  d := ParseMessage(s);
  try
    AssertTrue('response parsed', Assigned(d));
    AssertFalse('ok is false', d.Get('ok', True));
    AssertEquals('error message carried', 'boom', d.Get('error', ''));
  finally
    d.Free;
  end;
end;

initialization
  RegisterTest('unit', TWorkerProtocolTest);
end.
