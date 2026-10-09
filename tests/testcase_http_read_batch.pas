// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A read batch: one snapshot of the problem, one request per resource.)

WHY. Each IFitService getter answers one field, and over HTTP each one reads
its whole resource: capturing a project read /settings thirteen times, and the
window's state refresh read it nineteen. Inside BeginReadBatch .. EndReadBatch
the client answers a repeated read of the same resource from the reply it
already has - which also makes what is read ONE snapshot, instead of reads
taken at different moments. A write clears it, because the server has changed;
outside a batch every read asks, as before.
}
unit testcase_http_read_batch;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, mock_http_transport, fit_client,
    mock_fit_viewer, self_copied_component, points_set;

type
    THttpReadBatchTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        function Reads(const ARoute: string): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure InABatchARepeatedReadAsksOnce;
        procedure OutsideABatchEveryReadAsks;
        procedure AWriteInABatchIsSeenByTheNextRead;
        procedure DifferentResourcesAreEachAsked;
        procedure ANestedBatchEndsWithTheOutermost;
        procedure AnotherThreadReadsAsEver;
        procedure ARefreshReadsTheCurvesOnce;
        procedure TheModelIsDrawnFromOneRequest;
        procedure ACurveSentWithoutPointsIsAskedForItsOwn;
    end;

    { Reads the settings twice on its own thread. }
    TReadTwice = class(TThread)
    public
        Svc: TMockHttpService;
    protected
        procedure Execute; override;
    end;

implementation

procedure THttpReadBatchTest.SetUp;
begin
    FSvc := TMockHttpService.Create('http://localhost:8080');
    FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.5,"curveThresh":2}');
    FSvc.Reply('stats', '{"ok":true,"rFactor":"0.1"}');
end;

procedure THttpReadBatchTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

function THttpReadBatchTest.Reads(const ARoute: string): longint;
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

procedure THttpReadBatchTest.InABatchARepeatedReadAsksOnce;
begin
    FSvc.BeginReadBatch;
    try
        AssertEquals(0.5, FSvc.GetMaxRFactor, 0);
        AssertEquals(2, FSvc.GetCurveThresh, 0);
        AssertEquals(0.5, FSvc.GetMaxRFactor, 0);
    finally
        FSvc.EndReadBatch;
    end;
    AssertEquals('one read of /settings for three answers', 1,
        Reads('/settings'));
end;

procedure THttpReadBatchTest.OutsideABatchEveryReadAsks;
begin
    FSvc.GetMaxRFactor;
    FSvc.GetCurveThresh;
    AssertEquals(2, Reads('/settings'));
    //  And a batch that ended keeps nothing.
    FSvc.BeginReadBatch;
    FSvc.GetMaxRFactor;
    FSvc.EndReadBatch;
    FSvc.GetMaxRFactor;
    AssertEquals(4, Reads('/settings'));
end;

procedure THttpReadBatchTest.AWriteInABatchIsSeenByTheNextRead;
begin
    FSvc.BeginReadBatch;
    try
        FSvc.GetMaxRFactor;
        FSvc.SetMaxRFactor(0.25);
        FSvc.Reply('settings', '{"ok":true,"maxRFactor":0.25,"curveThresh":2}');
        AssertEquals('the server as written', 0.25, FSvc.GetMaxRFactor, 0);
    finally
        FSvc.EndReadBatch;
    end;
    AssertEquals(2, Reads('/settings'));
end;

procedure THttpReadBatchTest.DifferentResourcesAreEachAsked;
begin
    FSvc.BeginReadBatch;
    try
        FSvc.GetMaxRFactor;
        FSvc.GetRFactorStr;
        FSvc.GetCalcTimeStr;
    finally
        FSvc.EndReadBatch;
    end;
    AssertEquals(1, Reads('/settings'));
    AssertEquals(1, Reads('/stats'));
end;

procedure THttpReadBatchTest.ANestedBatchEndsWithTheOutermost;
begin
    FSvc.BeginReadBatch;
    FSvc.BeginReadBatch;
    FSvc.GetMaxRFactor;
    FSvc.EndReadBatch;
    //  Still inside the outer one: the reply is still shared.
    FSvc.GetMaxRFactor;
    FSvc.EndReadBatch;
    AssertEquals(1, Reads('/settings'));
end;

procedure TReadTwice.Execute;
begin
    Svc.GetMaxRFactor;
    Svc.GetMaxRFactor;
end;

{ THE BATCH IS ITS THREAD'S. A fit runs its operation on a thread of its own
  while the window reads; a snapshot the window opened must not answer the
  fit's reads, which belong to the server as it is changing. }
procedure THttpReadBatchTest.AnotherThreadReadsAsEver;
var
    T: TReadTwice;
begin
    FSvc.BeginReadBatch;
    try
        T := TReadTwice.Create(True);
        try
            T.Svc := FSvc;
            T.Start;
            T.WaitFor;
        finally
            T.Free;
        end;
    finally
        FSvc.EndReadBatch;
    end;
    AssertEquals('both of its reads asked', 2, Reads('/settings'));
end;

{ THE CLIENT'S REFRESH after a model change reads the curves twice - their
  points for the chart, their parameters for the grid - from one resource. }
procedure THttpReadBatchTest.ARefreshReadsTheCurvesOnce;
var
    Client: TFitClient;
    View: TMockFitViewer;
begin
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    Client := TFitClient.Create;
    View := TMockFitViewer.Create;
    try
        Client.FitService := FSvc;
        Client.FFitViewer := View;
        Client.UpdateComputedData(True);
    finally
        Client.FFitViewer := nil;
        Client.Free;
        View.Free;
    end;
    //  The points for the chart and the parameters for the grid, in ONE reply:
    //  the list with points answers the plain list too.
    AssertEquals('requests for the curves, in either form', 1,
        Reads('/curves') + Reads('/curves?points=1'));
end;

const
    TWO_CURVES_WITH_POINTS =
        '{"ok":true,"curves":[' +
        '{"id":"00000000-0000-0000-0000-000000000001","fitted":true,' +
        '"curveType":"","params":[],' +
        '"points":{"title":"a","x":[1,2,3],"y":[10,20,30]}},' +
        '{"id":"00000000-0000-0000-0000-000000000002","fitted":true,' +
        '"curveType":"","params":[],' +
        '"points":{"title":"b","x":[4,5],"y":[40,50]}}]}';
    TWO_CURVES_ONE_WITHOUT =
        '{"ok":true,"curves":[' +
        '{"id":"00000000-0000-0000-0000-000000000001","fitted":true,' +
        '"curveType":"","params":[],' +
        '"points":{"title":"a","x":[1,2,3],"y":[10,20,30]}},' +
        '{"id":"00000000-0000-0000-0000-000000000002","fitted":true,' +
        '"curveType":"","params":[]}]}';

{ ONE REQUEST FOR THE CHART: the list carries each curve's points
  (GET /curves?points=1), so drawing a model of N curves is not N + 1 requests. }
procedure THttpReadBatchTest.TheModelIsDrawnFromOneRequest;
var
    Curves: TSelfCopiedCompList;
begin
    FSvc.Reply('curves', TWO_CURVES_WITH_POINTS);
    Curves := FSvc.GetCurves;
    try
        AssertEquals('both curves', 2, Curves.Count);
        AssertEquals('the first one''s points', 3,
            TPointsSet(Curves.Items[0]).PointsCount);
        AssertEquals(50, TPointsSet(Curves.Items[1]).PointYCoord[1], 0);
    finally
        Curves.Free;
    end;
    AssertEquals('one request', 1, FSvc.Log.CountOf('GET'));
    AssertTrue('and it asked for the points', Pos('points=1',
        FSvc.Log.AsText) > 0);
end;

{ A SERVER THAT DOES NOT KNOW THE PARAMETER sends the list as it always did:
  the curve it sent no points for is asked for on its own, as before. }
procedure THttpReadBatchTest.ACurveSentWithoutPointsIsAskedForItsOwn;
var
    Curves: TSelfCopiedCompList;
begin
    FSvc.Reply('curves', TWO_CURVES_ONE_WITHOUT);
    FSvc.Reply('points', '{"title":"b","x":[4,5],"y":[40,50]}');
    Curves := FSvc.GetCurves;
    try
        AssertEquals('both curves', 2, Curves.Count);
        AssertEquals(2, TPointsSet(Curves.Items[1]).PointsCount);
    finally
        Curves.Free;
    end;
    AssertEquals('the list and the one curve', 2, FSvc.Log.CountOf('GET'));
end;

initialization
    //  A UNIT test: the transport is a double.
    RegisterTest('unit', THttpReadBatchTest);
end.
