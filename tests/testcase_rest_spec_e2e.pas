// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The running server's account of itself, fetched over a socket.)

WHY THIS EXISTS BESIDE testcase_rest_spec. That one drives TFitRestApi as an
object and proves the document is TRUE. It cannot see the two things that only
exist on the far side of the socket:

  THE CONTENT TYPE. fit_server used to label every reply application/json before
  it knew what was being asked for, and a browser handed an HTML page under that
  label renders the markup as text. No in-process test can catch that: the API
  answers a string either way, and the label is applied above it.

  WHAT THE DEPLOYED BINARY CONTAINS. A registry entry linked into the test binary
  and not into the server produces a document that is right here and wrong there.
  Asking the server that is actually running is the only way to tell.

So this fetches both routes from the real fit_server, over HTTP, and then
compares what that server SENDS on ordinary routes against what the document IT
served says it sends - which is the whole promise, checked end to end.
}
unit testcase_rest_spec_e2e;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, fphttpclient,
    //  GaussPoint for the profile, and the server's OWN points encoder for the
    //  body - hand-formatting floats here would put the machine's decimal
    //  separator on the wire.
    SimpMath, fit_points_json,
    worker_process_harness, rest_spec;

type
    TRestSpecE2ETest = class(TWorkerProcessTest)
    private
        FContentType: string;
        { GETs APath from the running server, remembering its content type. }
        function Fetch(const APath: string): string;
        { The document that server served, parsed. Caller owns it. }
        function ServedDocument: TJSONObject;
        { The property names of one schema of a served document, sorted. }
        function PropertiesOf(ADoc: TJSONObject;
            const ASchema: string): TStringList;
        { The keys of a JSON object reply, sorted. }
        function KeysOf(const ABody: string): TStringList;
        { The keys of a JSON object, sorted. }
        function KeysOfObject(AObj: TJSONObject): TStringList;
        { A problem with a profile, one pick and one FITTED curve, over HTTP. }
        function FittedProblem(AClient: TFPHTTPClient;
            const AUrl: string): longint;
        { PUTs a body, which TFPHTTPClient needs a stream for. }
        procedure PutJson(AClient: TFPHTTPClient; const AUrl, ABody: string);
    published
        procedure TheRunningServerServesItsOwnDocument;
        procedure AndThePageThatRendersIt;
        procedure ThePageIsLabelledAsAPageAndNotAsJson;
        procedure TheDocumentCoversTheEndpointTheRouterNeverSees;
        procedure TheDeployedServerDescribesTheOperationsThisBuildHas;
        procedure WhatTheServerSendsIsWhatTheDocumentItServedDescribes;
        procedure AndSoDoesTheModelItProducesOnceItHasActuallyFitted;
    end;

implementation

function TRestSpecE2ETest.Fetch(const APath: string): string;
var
    C: TFPHTTPClient;
begin
    FContentType := '';
    C := TFPHTTPClient.Create(nil);
    try
        Result := C.Get(FSvc.GetServerUrl + APath);
        FContentType := LowerCase(C.ResponseHeaders.Values['Content-Type']);
        if FContentType = '' then
            FContentType := LowerCase(C.ResponseHeaders.Values['content-type']);
    finally
        C.Free;
    end;
end;

function TRestSpecE2ETest.ServedDocument: TJSONObject;
var
    D: TJSONData;
begin
    D := GetJSON(Fetch('/openapi.json'));
    AssertTrue('the server served a JSON document', D is TJSONObject);
    Result := TJSONObject(D);
end;

function TRestSpecE2ETest.PropertiesOf(ADoc: TJSONObject;
    const ASchema: string): TStringList;
var
    Props: TJSONObject;
    i: integer;
begin
    Result := TStringList.Create;
    Props := ADoc.Objects['components'].Objects['schemas'].Objects[ASchema].
        Objects['properties'];
    for i := 0 to Props.Count - 1 do
        Result.Add(Props.Names[i]);
    Result.Sort;
end;

function TRestSpecE2ETest.KeysOf(const ABody: string): TStringList;
var
    D: TJSONData;
    i: integer;
begin
    Result := TStringList.Create;
    D := GetJSON(ABody);
    try
        AssertTrue('the reply is an object', D is TJSONObject);
        for i := 0 to TJSONObject(D).Count - 1 do
            Result.Add(TJSONObject(D).Names[i]);
    finally
        D.Free;
    end;
    Result.Sort;
end;

function TRestSpecE2ETest.KeysOfObject(AObj: TJSONObject): TStringList;
var
    i: integer;
begin
    Result := TStringList.Create;
    for i := 0 to AObj.Count - 1 do
        Result.Add(AObj.Names[i]);
    Result.Sort;
end;

procedure TRestSpecE2ETest.PutJson(AClient: TFPHTTPClient;
    const AUrl, ABody: string);
var
    Stream: TStringStream;
begin
    Stream := TStringStream.Create(ABody);
    try
        AClient.RequestBody := Stream;
        AClient.Put(AUrl);
    finally
        //  Cleared before the stream goes, or the next request on this client
        //  reads freed memory.
        AClient.RequestBody := nil;
        Stream.Free;
    end;
end;

function TRestSpecE2ETest.FittedProblem(AClient: TFPHTTPClient;
    const AUrl: string): longint;
var
    D: TJSONData;
    Id, Tries, i, N: longint;
    P: TPointsData;
    x: double;
begin
    D := GetJSON(AClient.FormPost(AUrl + '/problems', ''));
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;
    AssertTrue('a problem was created', Id > 0);

    //  The same synthetic Gaussian the harness uses. This test is about the
    //  SHAPES that come back, so the data only has to be something the engine
    //  can fit and finish.
    P := Default(TPointsData);
    P.Title := 'profile';
    N := 101;
    SetLength(P.X, N);
    SetLength(P.Y, N);
    for i := 0 to N - 1 do
    begin
        x := i * 0.2;
        P.X[i] := x;
        P.Y[i] := GaussPoint(100, 1.5, 10, x);
    end;
    PutJson(AClient, Format('%s/problems/%d/profile', [AUrl, Id]),
        PointsToJsonString(P));

    P := Default(TPointsData);
    P.Title := 'positions';
    SetLength(P.X, 1);
    SetLength(P.Y, 1);
    P.X[0] := 10;
    P.Y[0] := 100;
    PutJson(AClient, Format('%s/problems/%d/positions', [AUrl, Id]),
        PointsToJsonString(P));

    AClient.FormPost(Format('%s/problems/%d/actions/minimize-difference',
        [AUrl, Id]), '');
    //  The verb returns before its work does, so the shapes below only exist
    //  once the fit has finished - which is what the async route is for, and
    //  polling it is what a client does here too.
    for Tries := 1 to 100 do
    begin
        D := GetJSON(AClient.Get(Format('%s/problems/%d/async', [AUrl, Id])));
        try
            if TJSONObject(D).Get('done', False) then
                Break;
        finally
            D.Free;
        end;
        Sleep(100);
    end;
    Result := Id;
end;

procedure TRestSpecE2ETest.AndSoDoesTheModelItProducesOnceItHasActuallyFitted;
var
    Doc: TJSONObject;
    C: TFPHTTPClient;
    Id: longint;
    D: TJSONData;
    Curve, Param: TJSONObject;
    Documented, Sent: TStringList;
    Url: string;
    i: integer;
begin
    //  THE SHAPES THAT ONLY EXIST AFTER A FIT. A curve, its parameters and the
    //  goodness-of-fit statistics cannot be checked on an empty problem - and
    //  they are the hand-written half of the document, so they are exactly the
    //  half that can drift away from what the server sends.
    Url := FSvc.GetServerUrl;
    Doc := ServedDocument;
    C := TFPHTTPClient.Create(nil);
    try
        Id := FittedProblem(C, Url);

        D := GetJSON(C.Get(Format('%s/problems/%d/curves', [Url, Id])));
        try
            AssertTrue('the fit produced a curve',
                TJSONObject(D).Arrays['curves'].Count > 0);
            Curve := TJSONObject(TJSONObject(D).Arrays['curves'].Items[0]);
            Documented := PropertiesOf(Doc, 'Curve');
            Sent := KeysOfObject(Curve);
            try
                AssertEquals('what a curve carries and what the document says '
                    + 'it carries', Documented.CommaText, Sent.CommaText);
            finally
                Documented.Free;
                Sent.Free;
            end;

            AssertTrue('the curve has parameters',
                Curve.Arrays['params'].Count > 0);
            Param := TJSONObject(Curve.Arrays['params'].Items[0]);
            Documented := PropertiesOf(Doc, 'CurveParameter');
            Sent := KeysOfObject(Param);
            try
                //  A SUBSET, deliberately: `kind` is emitted only when the
                //  value is not a number, so a numeric parameter sends one
                //  field fewer. What must hold is that nothing is sent which
                //  the document has not described, and that the four fields a
                //  reader needs are all there.
                for i := 0 to Sent.Count - 1 do
                    AssertTrue('a parameter carries ' + Sent[i] +
                        ', which the document does not describe',
                        Documented.IndexOf(Sent[i]) >= 0);
                AssertTrue('its name', Sent.IndexOf('name') >= 0);
                AssertTrue('its value', Sent.IndexOf('value') >= 0);
                AssertTrue('its uncertainty', Sent.IndexOf('error') >= 0);
                AssertTrue('and what it is to the model',
                    Sent.IndexOf('type') >= 0);
            finally
                Documented.Free;
                Sent.Free;
            end;
        finally
            D.Free;
        end;

        D := GetJSON(C.Get(Format('%s/problems/%d/stats', [Url, Id])));
        try
            Documented := PropertiesOf(Doc, 'Stats');
            Sent := KeysOfObject(TJSONObject(D));
            try
                AssertEquals('what /stats sends after a fit and what the '
                    + 'document describes', Documented.CommaText, Sent.CommaText);
            finally
                Documented.Free;
                Sent.Free;
            end;
            Documented := PropertiesOf(Doc, 'Statistics');
            Sent := KeysOfObject(TJSONObject(D).Objects['statistics']);
            try
                //  Real numbers, not the placeholder an unfitted problem
                //  carries - otherwise this would pass against a fit that
                //  never happened.
                AssertTrue('the statistics are of an actual fit',
                    TJSONObject(D).Objects['statistics'].Get('valid', False));
                AssertEquals('the goodness-of-fit statistics and what the '
                    + 'document describes', Documented.CommaText, Sent.CommaText);
            finally
                Documented.Free;
                Sent.Free;
            end;
        finally
            D.Free;
        end;

        //  And a point set with points in it, which an empty profile cannot
        //  show: the computed profile the fit just produced.
        D := GetJSON(C.Get(Format('%s/problems/%d/calc-profile', [Url, Id])));
        try
            Documented := PropertiesOf(Doc, 'PointsSet');
            Sent := KeysOfObject(TJSONObject(D));
            try
                AssertTrue('the computed profile has points',
                    TJSONObject(D).Arrays['y'].Count > 0);
                for i := 0 to Sent.Count - 1 do
                    AssertTrue('the computed profile carries ' + Sent[i] +
                        ', which the document does not describe',
                        Documented.IndexOf(Sent[i]) >= 0);
            finally
                Documented.Free;
                Sent.Free;
            end;
        finally
            D.Free;
        end;

        C.Delete(Format('%s/problems/%d', [Url, Id]));
    finally
        C.Free;
        Doc.Free;
    end;
end;

procedure TRestSpecE2ETest.TheRunningServerServesItsOwnDocument;
var
    Doc: TJSONObject;
begin
    Doc := ServedDocument;
    try
        AssertEquals('an OpenAPI document', '3.0.3', Doc.Get('openapi', ''));
        AssertTrue('describing paths', Doc.Objects['paths'].Count > 0);
        AssertTrue('labelled as JSON: ' + FContentType,
            Pos('application/json', FContentType) > 0);
    finally
        Doc.Free;
    end;
end;

procedure TRestSpecE2ETest.AndThePageThatRendersIt;
var
    Page: string;
begin
    Page := Fetch('/docs');
    AssertTrue('an HTML page', Pos('<html', LowerCase(Page)) > 0);
    AssertTrue('loading Swagger UI', Pos('swagger-ui-bundle', Page) > 0);
    //  It must read the document from the server that served it. A page
    //  pointing anywhere else would describe a different build.
    AssertTrue('reading this server''s own document',
        Pos('"/openapi.json"', Page) > 0);
end;

procedure TRestSpecE2ETest.ThePageIsLabelledAsAPageAndNotAsJson;
begin
    Fetch('/docs');
    //  The defect this exists for: a browser shown HTML under
    //  application/json prints the markup instead of rendering it, and every
    //  test on this side of the socket passes while it does.
    AssertTrue('the docs page is served as HTML, not as JSON: ' + FContentType,
        Pos('text/html', FContentType) > 0);
    Fetch('/health');
    AssertTrue('and everything else is still JSON: ' + FContentType,
        Pos('application/json', FContentType) > 0);
end;

procedure TRestSpecE2ETest.TheDocumentCoversTheEndpointTheRouterNeverSees;
var
    Doc: TJSONObject;
begin
    Doc := ServedDocument;
    try
        //  POST /fit is answered in fit_server.lpr, before the REST surface is
        //  reached, so nothing in the routing knows it exists. It is in the
        //  document by hand, and this is the only test that can confirm the
        //  server it describes really does answer there.
        AssertTrue('the document describes POST /fit',
            Doc.Objects['paths'].Find('/fit') <> nil);
    finally
        Doc.Free;
    end;
end;

procedure TRestSpecE2ETest.TheDeployedServerDescribesTheOperationsThisBuildHas;
var
    Doc: TJSONObject;
    Paths: TJSONObject;
    i, Served: integer;
begin
    Doc := ServedDocument;
    try
        Paths := Doc.Objects['paths'];
        Served := 0;
        for i := 0 to Paths.Count - 1 do
            Inc(Served, TJSONObject(Paths.Items[i]).Count);
        //  The same table this process links, answered by the process that was
        //  built and started. A mismatch means the binary under test is not the
        //  code under test - which is the failure this suite exists for.
        AssertEquals('operations the running server describes',
            Length(Operations), Served);
    finally
        Doc.Free;
    end;
end;

procedure TRestSpecE2ETest.WhatTheServerSendsIsWhatTheDocumentItServedDescribes;
var
    Doc: TJSONObject;
    Documented, Sent: TStringList;
    C: TFPHTTPClient;
    Id: longint;
    D: TJSONData;
    Url: string;
begin
    Url := FSvc.GetServerUrl;
    Doc := ServedDocument;
    C := TFPHTTPClient.Create(nil);
    try
        D := GetJSON(C.FormPost(Url + '/problems', ''));
        try
            Id := TJSONObject(D).Get('id', -1);
        finally
            D.Free;
        end;
        AssertTrue('a problem was created', Id > 0);

        //  The biggest hand-written schema, against the reply of the server
        //  that published it. Ten fields nothing in the program declares.
        Documented := PropertiesOf(Doc, 'Settings');
        Sent := KeysOf(C.Get(Format('%s/problems/%d/settings', [Url, Id])));
        try
            Documented.Add('ok');
            Documented.Sort;
            AssertEquals('the settings the running server sends and the ' +
                'settings its own document describes',
                Documented.CommaText, Sent.CommaText);
        finally
            Documented.Free;
            Sent.Free;
        end;

        Documented := PropertiesOf(Doc, 'Health');
        Sent := KeysOf(C.Get(Url + '/health'));
        try
            AssertEquals('what /health sends and what its document describes',
                Documented.CommaText, Sent.CommaText);
        finally
            Documented.Free;
            Sent.Free;
        end;

        C.Delete(Format('%s/problems/%d', [Url, Id]));
    finally
        C.Free;
        Doc.Free;
    end;
end;

initialization
    RegisterTest('integration', TRestSpecE2ETest);
end.
