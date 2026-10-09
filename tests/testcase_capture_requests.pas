// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Capturing the model reads the curve list once, not once per value.)

FOUND BY check-ui, once its Stop probe actually pressed Stop: after an automatic
run was stopped, the window looked hung for five seconds. The engine had ended
six milliseconds after Stop; the window had then made 922 requests for the whole
curve list to capture a model of 33 curves - the model history records every
run, and the unsaved-work check captures too - because the capture asked for
each curve's handle, fitted flag, parameter count, and each parameter's value,
name and error through accessors that each fetch every curve. Quadratic in the
model, over the network.

So the capture reads the model in ONE request (GetCurveAttributes, the grid's
own read, which carries each curve's handle, fitted flag and parameters).
What it captures is unchanged, and that is asserted value by value.
}
unit testcase_capture_requests;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, mock_http_transport,
    fit_project_session, fit_project_document;

type
    TCaptureRequestsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        function CurvesJson(ACount: longint): string;
        function WholeListFetches: longint;
        function Reads(const ARoute: string): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheCurveListIsFetchedOnce;
        procedure EveryValueIsCapturedAsBefore;
        procedure EachResourceIsReadOnce;
        procedure RestoringTheModelFetchesTheListOnceNotOncePerCurve;
        procedure WalkingEveryCurveReadsTheListOnceBetweenWrites;
    end;

implementation

const
    CURVES = 20;

procedure TCaptureRequestsTest.SetUp;
begin
    FSvc := TMockHttpService.Create('http://localhost:8080');
    FSvc.Reply('curves', CurvesJson(CURVES));
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.5}');
    FSvc.Reply('stats', '{"ok":true,"rFactor":"0.1"}');
end;

procedure TCaptureRequestsTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

function TCaptureRequestsTest.CurvesJson(ACount: longint): string;
var
    i: longint;
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := '{"ok":true,"curves":[';
    for i := 0 to ACount - 1 do
    begin
        if i > 0 then
            Result := Result + ',';
        //  Three quantities and a label, which is not one and is left out.
        Result := Result + Format('{"id":"00000000-0000-0000-0000-%.12d",' +
            '"fitted":%s,"curveType":"",' +
            '"params":[{"name":"A","value":%s,"type":0,"error":%s},' +
            '{"name":"x0","value":%s,"type":0,"error":-1},' +
            '{"name":"sigma","value":%s,"type":0,"error":0.5},' +
            '{"name":"label","value":"W%d","kind":"text","type":0}]}',
            //  From 1: the all-zero GUID is no handle - it was never issued.
            [i + 1, BoolToStr(Odd(i), 'true', 'false'), FloatToStr(10 + i, FS),
            FloatToStr(0.25 * i, FS), FloatToStr(100 + i, FS),
            FloatToStr(1.5, FS), i]);
    end;
    Result := Result + ']}';
end;

function TCaptureRequestsTest.WholeListFetches: longint;
var
    Lines: TStringList;
    i: longint;
begin
    Result := 0;
    Lines := TStringList.Create;
    try
        Lines.Text := FSvc.Log.AsText;
        for i := 0 to Lines.Count - 1 do
            //  The list itself, not one curve's points or parameters.
            if Lines[i].EndsWith('/curves)') then
                Inc(Result);
    finally
        Lines.Free;
    end;
end;

{ ONCE: the handles, the fitted flags and the parameters all travel in the
  one reply. A flag asked per curve was a whole-list download per curve. }
procedure TCaptureRequestsTest.TheCurveListIsFetchedOnce;
var
    Doc: TProjectDocument;
begin
    Doc := CaptureProject(FSvc, EmptyProjectClientContext,
        EmptyProjectDocument);
    AssertEquals('every curve captured', CURVES, Length(Doc.Curves));
    AssertEquals(Format('fetches of the whole list for %d curves', [CURVES]),
        1, WholeListFetches);
end;

procedure TCaptureRequestsTest.EveryValueIsCapturedAsBefore;
var
    Doc: TProjectDocument;
    i: longint;
begin
    Doc := CaptureProject(FSvc, EmptyProjectClientContext,
        EmptyProjectDocument);
    AssertEquals(CURVES, Length(Doc.Curves));
    for i := 0 to CURVES - 1 do
    begin
        AssertEquals(Format('curve %d, its handle', [i]),
            Format('00000000-0000-0000-0000-%.12d', [i + 1]), Doc.Curves[i].Id);
        AssertEquals(Format('curve %d, fitted', [i]), Odd(i),
            Doc.Curves[i].Fitted);
        AssertEquals('the label is not a quantity', 3,
            Length(Doc.Curves[i].Params));
        AssertEquals('A', Doc.Curves[i].Params[0].Name);
        AssertEquals(10 + i, Doc.Curves[i].Params[0].Value, 0);
        AssertEquals(0.25 * i, Doc.Curves[i].Params[0].Error, 0);
        AssertEquals('x0', Doc.Curves[i].Params[1].Name);
        AssertEquals(100 + i, Doc.Curves[i].Params[1].Value, 0);
        AssertEquals(-1, Doc.Curves[i].Params[1].Error, 0);
        AssertEquals('sigma', Doc.Curves[i].Params[2].Name);
        AssertEquals(0.5, Doc.Curves[i].Params[2].Error, 0);
    end;
end;

function TCaptureRequestsTest.Reads(const ARoute: string): longint;
var
    Lines: TStringList;
    i: longint;
begin
    Result := 0;
    Lines := TStringList.Create;
    try
        Lines.Text := FSvc.Log.AsText;
        for i := 0 to Lines.Count - 1 do
            if Lines[i].StartsWith('GET(') and Lines[i].EndsWith(ARoute + ')') then
                Inc(Result);
    finally
        Lines.Free;
    end;
end;

{ ONE SNAPSHOT: the capture reads a dozen settings and several statistics, each
  through a getter that reads its whole resource. Inside a read batch each
  resource is read once - and every value comes from the same moment. }
procedure TCaptureRequestsTest.EachResourceIsReadOnce;
begin
    CaptureProject(FSvc, EmptyProjectClientContext, EmptyProjectDocument);
    AssertEquals('/settings', 1, Reads('/settings'));
    AssertEquals('/stats', 1, Reads('/stats'));
end;

{ FOUND IN USE: Fit Pro opened with no window for over a minute. It was
  reopening the last project, whose model holds 506 curves, and the restore
  found each saved curve's place by its handle (IndexOfCurveInstance) - a
  download of the whole curve list, half a megabyte, per curve. The restore
  reads inside one batch, so a resource is fetched once between writes. }
procedure TCaptureRequestsTest.RestoringTheModelFetchesTheListOnceNotOncePerCurve;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    Doc := CaptureProject(FSvc, EmptyProjectClientContext,
        EmptyProjectDocument);
    FSvc.Log.Clear;
    ApplyProject(FSvc, Doc, Fault);
    AssertTrue(Format('fetches of the whole list restoring %d curves: %d',
        [CURVES, WholeListFetches]), WholeListFetches <= 2);
end;

{ THE ROOT OF FOUR FREEZES. Every per-curve accessor reads the whole curve
  list, and restoring a project, making a history entry current, the refresh
  after it and Clear Model each walked a 506-curve model asking once per curve:
  half a megabyte fetched, or parsed, five hundred times - a minute each time,
  fixed one command at a time until the cause was seen. The service now keeps
  the list parsed until something is written, with no batch asked for. }
procedure TCaptureRequestsTest.WalkingEveryCurveReadsTheListOnceBetweenWrites;
var
    i, n: longint;
begin
    FSvc.Log.Clear;
    n := FSvc.GetCurveCount;
    for i := 0 to n - 1 do
    begin
        FSvc.IndexOfCurveInstance(FSvc.GetCurveInstanceId(i));
        FSvc.GetCurveParameterCount(i);
        FSvc.GetCurveParameterValue(i, 0);
    end;
    AssertEquals(Format('fetches of the whole list walking %d curves', [n]),
        1, WholeListFetches);
    //  And a write ends it: the server may have changed.
    FSvc.SetMaxRFactor(0.5);
    FSvc.GetCurveCount;
    AssertEquals('read again after a write', 2, WholeListFetches);
end;

initialization
    //  A UNIT test: the transport is a double, no server runs.
    RegisterTest('unit', TCaptureRequestsTest);
end.
