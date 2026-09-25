// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(This build's own OpenAPI document, and the page that renders it.)

A SERVER THAT DESCRIBES ITSELF. The only description of this API used to be
prose - the route comments in rest_routes, the header of fit_rest_api, a table
in docs/contributing/client-server.md - and prose drifts from a router with
nothing catching it. Anything that is not the desktop client had to read Pascal
to learn the surface.

WHAT MAKES THE DOCUMENT TRUE, and it is three different mechanisms because the
three parts of an API drift for different reasons:

  PATHS AND METHODS. Every operation carries a concrete SamplePath, and the
  tests feed each one through rest_routes.RouteOf and assert it lands on the
  route the row declares. A route whose shape changes fails the tests rather
  than quietly producing a document that describes a server nobody is running.

  ENUMERATED VALUES. The action names, the module resources and the point-set
  names are not written here. They are read, at the moment the document is
  built, from action_registry, module_registry and rest_routes' own vocabulary.
  Registering an action makes it appear in the spec, and nothing here is edited.

  BODIES. Nothing in the program declares that a settings document has ten
  fields of these types, so those are hand-written, in rest_spec_schemas, and
  the tests compare them against what a running server actually emits.

NOT COMMITTED, AND THAT IS THE POINT. The document exists only while the server
runs, so there is no file to regenerate, diff or forget: what /openapi.json
answers is what THIS binary does. A committed spec would need a CI job to stay
honest, and would be wrong for anyone running a build that job never saw.

THE ONE ENDPOINT THAT IS NOT ROUTED. POST /fit is answered in fit_server.lpr,
before the REST surface is reached, so RouteOf has never heard of it. It is the
single row marked ServedOutsideRouter, and a test asserts there is exactly one -
if a second endpoint is ever added at the socket layer, that test is where it
gets noticed.
}
unit rest_spec;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, fpjson,
    rest_routes, rest_spec_schemas, action_registry, module_registry,
    int_app_module, fit_worker_protocol;

type
    { One operation of the API: a method and a path, and what they carry.

      A record rather than a case in a builder function because the tests read
      this as data - which is the same reason rest_routes made the route table
      a table. }
    TRestOperation = record
        { The route this path must classify as. rtUnknown only for the one
          operation the router never sees (see ServedOutsideRouter). }
        Route: TRestRoute;
        Method: string;
        { The OpenAPI path template, with braced placeholders - for instance
          the curves route, whose problem id is one. }
        Path: string;
        { A concrete path of that shape, for the test that checks this row
          against the real classifier. }
        SamplePath: string;
        { Which group of the document it belongs to. }
        Tag: string;
        Summary: string;
        Description: string;
        { Component schema names, or '' when the operation has no body of that
          kind. 'PointsSet' and friends live in rest_spec_schemas. }
        RequestSchema: string;
        ResponseSchema: string;
        { Failure statuses this operation can answer with, beyond the 500 and
          the 400 every route shares, as a comma-separated list. }
        Codes: string;
        { True for POST /fit, which fit_server answers itself. }
        ServedOutsideRouter: boolean;
    end;

    TRestOperationArray = array of TRestOperation;

{ Every operation this build serves, with the point-set routes expanded over the
  vocabularies rest_routes matches on, so each row has a concrete path. }
function Operations: TRestOperationArray;

{ The whole OpenAPI 3.0.3 document for this build.

  Enumerates whatever is REGISTERED WHEN IT IS CALLED. The caller registers -
  fit_rest_api does it before answering /openapi.json, as it does before running
  a verb - because the registration lives above this unit and calling upwards
  from here would tie the document to the router it describes. }
function OpenApiJson: string;

{ The Swagger UI page that reads it. }
function SwaggerUiHtml: string;

{ What a reply to this request is made of. The docs page is the only thing this
  server sends that is not JSON, and this is where that is decided - fit_server
  used to assert 'application/json' over every response before dispatching. }
function ContentTypeOf(const AMethod, APath: string): string;

{ The paths the two self-description routes answer on. Named so the router, the
  tests and the browser cannot disagree about them. }
const
    OPENAPI_PATH = '/openapi.json';
    DOCS_PATH = '/docs';
    { Pinned rather than floating: a major version of Swagger UI that changed
      its global would leave a blank page with nothing in any log. }
    SWAGGER_UI_VERSION = '5.17.14';

implementation

{ Appends one operation to the array being built. }
procedure Add(var AOps: TRestOperationArray; ARoute: TRestRoute;
    const AMethod, APath, ASample, ATag, ASummary, ADescription,
    ARequest, AResponse, ACodes: string; AOutside: boolean = False);
var
    Op: TRestOperation;
begin
    Op := Default(TRestOperation);
    Op.Route := ARoute;
    Op.Method := AMethod;
    Op.Path := APath;
    Op.SamplePath := ASample;
    Op.Tag := ATag;
    Op.Summary := ASummary;
    Op.Description := ADescription;
    Op.RequestSchema := ARequest;
    Op.ResponseSchema := AResponse;
    Op.Codes := ACodes;
    Op.ServedOutsideRouter := AOutside;
    SetLength(AOps, Length(AOps) + 1);
    AOps[High(AOps)] := Op;
end;

{ What one of the readable point sets is. The four writable ones are inputs; the
  rest are answers, which is why they can be read and not replaced. }
function PointsSetSummary(const AName: string): string;
begin
    if AName = 'profile' then
        Result := 'the measured profile'
    else if AName = 'background' then
        Result := 'the background points'
    else if AName = 'positions' then
        Result := 'the curve positions, as picked'
    else if AName = 'rfactor-bounds' then
        Result := 'the interval the R-factor is computed over'
    else if AName = 'calc-profile' then
        Result := 'the model profile, as computed'
    else if AName = 'delta-profile' then
        Result := 'the difference between the measured and computed profiles'
    else if AName = 'calc-positions' then
        Result := 'the curve positions, as built'
    else
        Result := AName;
end;

function Operations: TRestOperationArray;
var
    Ops: TRestOperationArray;
    i: integer;
    Name: string;
begin
    SetLength(Ops, 0);

    Add(Ops, rtHealth, 'GET', '/health', '/health', 'server',
        'Liveness and protocol version',
        'The cheapest call there is; a client uses it to find out whether a ' +
        'server is there at all, and whether it speaks a protocol it knows.',
        '', 'Health', '');

    Add(Ops, rtOpenApi, 'GET', OPENAPI_PATH, OPENAPI_PATH, 'server',
        'This document',
        'Assembled from this build''s own routing and registries when you ' +
        'ask for it, so it describes the server answering, not a release.',
        '', '', '');

    Add(Ops, rtDocs, 'GET', DOCS_PATH, DOCS_PATH, 'server',
        'The API browser',
        'An HTML page - the only non-JSON reply this server sends - that ' +
        'renders the document above.',
        '', '', '');

    Add(Ops, rtUnknown, 'POST', '/fit', '/fit', 'fitting',
        'Fit one whole problem, statelessly',
        'Everything the fit needs in one request, and nothing remembered ' +
        'afterwards. Used by clients that keep the model themselves; the ' +
        'routes under /problems are for the stateful conversation.',
        'FitProblem', 'FitOutcome', '400', True);

    Add(Ops, rtUnknown, 'GET', '/fit/progress', '/fit/progress', 'fitting',
        'How a stateless fit is going, while it runs',
        'Asked with `?id=<progressId>` - the id the POST /fit it belongs to ' +
        'carried - while that POST is still out. Answers the best outcome ' +
        'reached so far, or `found: false` with no curves before the first ' +
        'one; never 404 for an id it has not seen, because the first poll ' +
        'usually arrives before the first improvement.',
        '', 'FitOutcome', '', True);

    Add(Ops, rtCreateProblem, 'POST', '/problems', '/problems', 'problems',
        'Create a problem',
        'Answers with the handle every other route addresses it by. A ' +
        'problem holds a profile, a model and its settings, and several may ' +
        'be served at once.',
        '', 'ProblemCreated', '');

    Add(Ops, rtDiscardProblem, 'DELETE', '/problems/{id}', '/problems/1',
        'problems', 'Discard a problem',
        'Frees the problem and everything it holds. Takes no lock, so it ' +
        'succeeds even while an operation is running.',
        '', 'Ok', '404');

    Add(Ops, rtState, 'GET', '/problems/{id}/state', '/problems/1/state',
        'problems', 'Where the problem is in its lifecycle', '',
        '', 'State', '404');

    Add(Ops, rtAsync, 'GET', '/problems/{id}/async', '/problems/1/async',
        'fitting', 'Progress of a long operation',
        'The polled route: after starting an asynchronous action, read this ' +
        'until done is true. It never waits on the problem''s lock, so it ' +
        'answers while a fit is running.',
        '', 'AsyncState', '404');

    Add(Ops, rtProgress, 'GET', '/problems/{id}/progress',
        '/problems/1/progress', 'fitting',
        'The loss a running fit has reached, as it goes',
        'Polled while a fit runs, and answered without waiting on the ' +
        'problem''s lock. `since=<seq>` returns only the samples recorded from ' +
        'that seq on - pass back the `nextSeq` of the previous reply. ' +
        '`snapshot=1` also asks for the model as it stood at a recent sample; ' +
        'the engine takes one no more than twice a second, and only for as ' +
        'long as somebody keeps asking.',
        '', 'FitProgress', '404');

    Add(Ops, rtStats, 'GET', '/problems/{id}/stats', '/problems/1/stats',
        'fitting', 'How well the model fits, and what it cost', '',
        '', 'Stats', '404');

    Add(Ops, rtGetSettings, 'GET', '/problems/{id}/settings',
        '/problems/1/settings', 'settings', 'Read the problem''s settings', '',
        '', 'Settings', '404');

    Add(Ops, rtPutSettings, 'PUT', '/problems/{id}/settings',
        '/problems/1/settings', 'settings', 'Change some of the settings',
        'Absent fields are left alone, so a client may send only what it ' +
        'changed. Answers with the settings as they now stand.',
        'SettingsWrite', 'Settings', '400,404');

    Add(Ops, rtSelectedInterval, 'GET', '/problems/{id}/selected-interval',
        '/problems/1/selected-interval', 'points',
        'The part of the profile being fitted', '',
        '', 'PointsSet', '404');

    //  The point-set reads and writes, expanded over the vocabularies the
    //  router itself matches on. One concrete path each rather than a {set}
    //  template, so every row is a path RouteOf can be asked about - and so a
    //  reader sees the actual names instead of a placeholder.
    for i := Low(READABLE_POINTS_SETS) to High(READABLE_POINTS_SETS) do
    begin
        Name := READABLE_POINTS_SETS[i];
        //  'rfactor' rides in the readable vocabulary because the router
        //  classifies it there, but it answers a scalar rather than a set.
        if Name = 'rfactor' then
            Add(Ops, rtGetPointsSet, 'GET', '/problems/{id}/rfactor',
                '/problems/1/rfactor', 'fitting',
                'The current R-factor', '', '', 'RFactor', '404')
        else
            Add(Ops, rtGetPointsSet, 'GET', '/problems/{id}/' + Name,
                '/problems/1/' + Name, 'points',
                'Read ' + PointsSetSummary(Name),
                'Answers with the set itself rather than an ok-wrapped ' +
                'envelope: it is the resource.',
                '', 'PointsSet', '404');
    end;

    for i := Low(WRITABLE_POINTS_SETS) to High(WRITABLE_POINTS_SETS) do
    begin
        Name := WRITABLE_POINTS_SETS[i];
        Add(Ops, rtPutPointsSet, 'PUT', '/problems/{id}/' + Name,
            '/problems/1/' + Name, 'points',
            'Replace ' + PointsSetSummary(Name),
            'Replaces the whole set. Curve identities may be sent only with ' +
            'the positions - an identity is issued to the pick a curve is ' +
            'seeded from - and are refused by name on any other set. ' +
            'Replacing the profile resets the problem.',
            'PointsSet', 'OkMessage', '400,404');
    end;

    Add(Ops, rtAddPoint, 'POST', '/problems/{id}/points/{set}',
        '/problems/1/points/profile', 'points', 'Append one point to a set',
        'The set is one of the four the problem keeps, or one a module ' +
        'contributes - a module''s set is refused by name, in the module''s ' +
        'own terms, if no module claims it.',
        'PointCreate', 'Ok', '400,404');

    Add(Ops, rtMovePoint, 'PUT', '/problems/{id}/points/{set}',
        '/problems/1/points/profile', 'points', 'Move one point of a set',
        'Names the point by where it currently is.',
        'PointMove', 'Ok', '400,404');

    Add(Ops, rtDeletePoint, 'DELETE', '/problems/{id}/points/{set}/{pid}',
        '/problems/1/points/positions/c1', 'points',
        'Remove one member of a set, by the handle that names it',
        'Only the positions are addressable one at a time: a curve''s ' +
        'identity is issued to the pick it is seeded from, so only a pick ' +
        'carries a handle. Answers with the refreshed model.',
        '', 'Curves', '400,404');

    Add(Ops, rtCurves, 'GET', '/problems/{id}/curves', '/problems/1/curves',
        'curves', 'Every curve of the model, with its parameters', '',
        '', 'Curves', '404');

    Add(Ops, rtPutCurves, 'PUT', '/problems/{id}/curves', '/problems/1/curves',
        'curves', 'Restore the whole model''s values',
        'One request rather than one per parameter, because each write ' +
        'rebuilds the whole model - and because only a whole-model write can ' +
        'say that an optimiser produced the values, which cannot be derived ' +
        'from the values themselves. An unknown handle fails the whole ' +
        'request rather than being skipped.',
        'CurveValuesWrite', 'OkMessage', '400,404');

    Add(Ops, rtCurvePoints, 'GET', '/problems/{id}/curves/{cid}/points',
        '/problems/1/curves/c1/points', 'curves',
        'One curve''s plotted points', '',
        '', 'PointsSet', '404');

    Add(Ops, rtCurveParam, 'PUT', '/problems/{id}/curves/{cid}/params/{j}',
        '/problems/1/curves/c1/params/0', 'curves',
        'Set one parameter of one curve',
        'Rebuilds the model. Writing to an unknown handle is refused rather ' +
        'than applied to the first curve.',
        'CurveParamValue', 'Ok', '400,404');

    Add(Ops, rtGetSpecialParams, 'GET', '/problems/{id}/special-params',
        '/problems/1/special-params', 'curves',
        'The user-defined curve''s parameters', '',
        '', 'SpecialParams', '404');

    Add(Ops, rtPutSpecialParams, 'PUT', '/problems/{id}/special-params',
        '/problems/1/special-params', 'curves',
        'Set the user-defined curve''s formula and parameters', '',
        'SpecialParamsWrite', 'SpecialParams', '400,404');

    Add(Ops, rtDeleteSpecialParams, 'DELETE', '/problems/{id}/special-params',
        '/problems/1/special-params', 'curves',
        'Clear the user-defined curve',
        'The curve the formula described is gone, so the problem must stop ' +
        'fitting it.',
        '', 'SpecialParams', '404');

    Add(Ops, rtModuleStates, 'GET', '/problems/{id}/module-states',
        '/problems/1/module-states', 'modules',
        'Every module''s project-state document, in one answer',
        'A client cannot assemble this itself: the modules that matter are ' +
        'the server''s, and it may not have the same ones linked.',
        '', 'ModuleStates', '404');

    Add(Ops, rtModule, 'GET', '/problems/{id}/modules/{vendor}/{resource}',
        '/problems/1/modules/sample/detect', 'modules',
        'Read a resource a module contributes',
        'The reply is the resource itself rather than an ok-wrapped ' +
        'envelope, which is how these payloads already crossed the wire. A ' +
        'resource that declares it needs the Python component has it started ' +
        'first, and answers 503 when it cannot be.',
        '', '', '404,503');

    Add(Ops, rtModule, 'POST', '/problems/{id}/modules/{vendor}/{resource}',
        '/problems/1/modules/sample/detect', 'modules',
        'Ask a module to do something', 'As the GET above.',
        '', '', '404,503');

    Add(Ops, rtModule, 'PUT', '/problems/{id}/modules/{vendor}/{resource}',
        '/problems/1/modules/sample/detect', 'modules',
        'Replace a resource a module keeps',
        'Accepted alongside POST because replacing a resource wholesale is ' +
        'what PUT means.',
        '', '', '404,503');

    Add(Ops, rtAction, 'POST', '/problems/{id}/actions/{name}',
        '/problems/1/actions/minimize-difference', 'fitting',
        'Run one of the engine''s verbs',
        'The verbs are listed under the name parameter, with the ones that ' +
        'return before their work does marked: for those, poll ' +
        '/problems/{id}/async until done. Answers 503 when the problem''s ' +
        'minimizer needs the Python component and it cannot be started.',
        'ActionBody', 'OkMessage', '400,404,503');

    Result := Ops;
end;

{ ---------------------------------------------------------------------------
  Building the document.
  --------------------------------------------------------------------------- }

{ The verbs, as the enum of the action route's {name} - read from the registry
  rather than listed here, so a registered action documents itself. }
function ActionEnum: TJSONArray;
var
    Acts: TActionInfoArray;
    i: integer;
begin
    Result := TJSONArray.Create;
    Acts := RegisteredActions;
    for i := 0 to High(Acts) do
        Result.Add(Acts[i].Name);
end;

{ The same registry as prose, so each verb's own line is on the page. }
function ActionDescription: string;
var
    Acts: TActionInfoArray;
    i: integer;
begin
    Result := 'Which verb to run. This build offers:';
    Acts := RegisteredActions;
    for i := 0 to High(Acts) do
    begin
        Result := Result + LineEnding + '- `' + Acts[i].Name + '` - ' +
            Acts[i].Description;
        if Acts[i].IsAsynchronous then
            Result := Result + ' (returns at once; poll `async`)';
    end;
end;

{ The module names, or nil when this build has none. Both halves of the module
  route are read from the registry the same way the verbs are: a build that
  links a module says so in its own document, and one that links none must not
  claim a closed list of nothing - an empty enum reads as "this route accepts
  no value", which is false for every build that has one. }
function ModuleSegmentEnum(AVendor: boolean): TJSONArray;
var
    Mods: TAppModuleArray;
    Res: TModuleResourceArray;
    i, j, Slash: integer;
    Seen: TStringList;
    Part: string;
begin
    Result := nil;
    Seen := TStringList.Create;
    try
        Mods := RegisteredModules;
        for i := 0 to High(Mods) do
        begin
            Res := Mods[i].Resources;
            for j := 0 to High(Res) do
            begin
                //  Declared as '<module>/<resource>' - which is exactly the two
                //  path segments this route takes, so it splits at the first
                //  slash and anything after it is the resource, slashes and all.
                Slash := Pos('/', Res[j].Name);
                if Slash <= 0 then
                    Continue;
                if AVendor then
                    Part := Copy(Res[j].Name, 1, Slash - 1)
                else
                    Part := Copy(Res[j].Name, Slash + 1, Length(Res[j].Name));
                if Seen.IndexOf(Part) < 0 then
                    Seen.Add(Part);
            end;
        end;
        if Seen.Count = 0 then
            Exit;
        Result := TJSONArray.Create;
        for i := 0 to Seen.Count - 1 do
            Result.Add(Seen[i]);
    finally
        Seen.Free;
    end;
end;

{ The resources the linked modules answer, in the '<module>/<resource>' form the
  path carries as vendor/resource. }
function ModuleResourceDescription: string;
var
    Known: string;
begin
    //  KnownModuleResources answers in words when there are none, which is what
    //  the router puts in its refusal - so the same sentence appears here.
    Known := KnownModuleResources;
    Result := 'The resources this build offers, as `vendor/resource`: ' +
        Known + '.';
end;

{ Adds an enum, unless there is nothing to enumerate. }
procedure AddEnum(ASchema: TJSONObject; AValues: TJSONArray);
begin
    if Assigned(AValues) then
        ASchema.Add('enum', AValues);
end;

{ One path parameter, described by the name the template gives it. }
function PathParameter(const AName: string; ARoute: TRestRoute): TJSONObject;
var
    Schema: TJSONObject;
    Desc: string;
begin
    Schema := TJSONObject.Create;
    Schema.Add('type', 'string');
    Desc := AName;
    if AName = 'id' then
    begin
        Schema.Free;
        Schema := TJSONObject.Create;
        Schema.Add('type', 'integer');
        Desc := 'The problem, as POST /problems issued it.';
    end
    else if AName = 'set' then
    begin
        if ARoute = rtDeletePoint then
            Desc := 'Only `positions`: no other set carries handles.'
        else
            Desc := 'One of `profile`, `background`, `positions`, ' +
                '`rfactor-bounds`, or a set a module contributes.';
    end
    else if AName = 'pid' then
        Desc := 'The member''s handle, as the set''s `ids` reported it.'
    else if AName = 'cid' then
        Desc := 'The curve''s handle, as GET /problems/{id}/curves reported it.'
    else if AName = 'j' then
    begin
        Schema.Free;
        Schema := TJSONObject.Create;
        Schema.Add('type', 'integer');
        Desc := 'Which parameter of that curve, by position.';
    end
    else if AName = 'name' then
        Desc := ActionDescription
    else if (AName = 'vendor') or (AName = 'resource') then
        Desc := ModuleResourceDescription;

    Result := TJSONObject.Create;
    Result.Add('name', AName);
    Result.Add('in', 'path');
    Result.Add('required', True);
    Result.Add('description', Desc);
    //  What this segment may be, when the answer is a closed set this build
    //  knows. All three come from a registry rather than from a list here.
    if AName = 'name' then
        Schema.Add('enum', ActionEnum)
    else if AName = 'vendor' then
        AddEnum(Schema, ModuleSegmentEnum(True))
    else if AName = 'resource' then
        AddEnum(Schema, ModuleSegmentEnum(False));
    Result.Add('schema', Schema);
end;

{ The braced placeholders of a path template, in order, as parameters. }
function ParametersOf(const APath: string; ARoute: TRestRoute): TJSONArray;
var
    i, Start: integer;
begin
    Result := TJSONArray.Create;
    i := 1;
    while i <= Length(APath) do
    begin
        if APath[i] = '{' then
        begin
            Start := i + 1;
            while (i <= Length(APath)) and (APath[i] <> '}') do
                Inc(i);
            Result.Add(PathParameter(Copy(APath, Start, i - Start), ARoute));
        end;
        Inc(i);
    end;
end;

{ A response body of one component schema, or an unnamed object when the shape
  is the module's own rather than this server's. }
function JsonContent(const ASchema: string): TJSONObject;
var
    Media, Schema: TJSONObject;
begin
    Media := TJSONObject.Create;
    if ASchema = '' then
    begin
        //  A module's payload is the module's own shape; saying 'object' is
        //  the whole truth this framework has about it.
        Schema := TJSONObject.Create;
        Schema.Add('type', 'object');
        Media.Add('schema', Schema);
    end
    else
        Media.Add('schema', SchemaRef(ASchema));
    Result := TJSONObject.Create;
    Result.Add('application/json', Media);
end;

function ResponseOf(const ADescription, ASchema: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('description', ADescription);
    Result.Add('content', JsonContent(ASchema));
end;

{ What a status code means on this API. The messages are the router's own
  distinctions: a refusal is not a fault, and a handle the model no longer
  holds is not a body it cannot read. }
function MeaningOf(const ACode: string): string;
begin
    if ACode = '400' then
        Result := 'The request was inadmissible - malformed, or wrong for ' +
            'the problem''s state. It will fail again unchanged.'
    else if ACode = '404' then
        Result := 'No such problem, curve or endpoint.'
    else if ACode = '503' then
        Result := 'The Python component is needed here and could not be started.'
    else
        Result := 'Refused.';
end;

{ The success and failure replies of one operation. }
function ResponsesOf(const AOp: TRestOperation): TJSONObject;
var
    Codes: TStringList;
    i: integer;
    Ok, Media, Content, Schema: TJSONObject;
begin
    Result := TJSONObject.Create;
    if AOp.Route = rtDocs then
    begin
        Schema := TJSONObject.Create;
        Schema.Add('type', 'string');
        Media := TJSONObject.Create;
        Media.Add('schema', Schema);
        Content := TJSONObject.Create;
        Content.Add('text/html', Media);
        Ok := TJSONObject.Create;
        Ok.Add('description', 'The Swagger UI page.');
        Ok.Add('content', Content);
        Result.Add('200', Ok);
    end
    else
        Result.Add('200', ResponseOf('Success.', AOp.ResponseSchema));

    Codes := TStringList.Create;
    try
        Codes.Delimiter := ',';
        Codes.StrictDelimiter := True;
        Codes.DelimitedText := AOp.Codes;
        for i := 0 to Codes.Count - 1 do
            if Trim(Codes[i]) <> '' then
                Result.Add(Trim(Codes[i]),
                    ResponseOf(MeaningOf(Trim(Codes[i])), 'Error'));
    finally
        Codes.Free;
    end;
    //  Every route can fault, and every fault answers in the same envelope.
    Result.Add('500', ResponseOf(
        'The engine did something it did not intend.', 'Error'));
end;

function OperationObject(const AOp: TRestOperation): TJSONObject;
var
    Tags: TJSONArray;
    Params: TJSONArray;
    Body: TJSONObject;
begin
    Result := TJSONObject.Create;
    Tags := TJSONArray.Create;
    Tags.Add(AOp.Tag);
    Result.Add('tags', Tags);
    Result.Add('summary', AOp.Summary);
    if AOp.Description <> '' then
        Result.Add('description', AOp.Description);
    Result.Add('operationId', LowerCase(AOp.Method) + '-' +
        StringReplace(StringReplace(StringReplace(Copy(AOp.Path, 2,
        Length(AOp.Path)), '/', '-', [rfReplaceAll]), '{', '',
        [rfReplaceAll]), '}', '', [rfReplaceAll]));
    Params := ParametersOf(AOp.Path, AOp.Route);
    if Params.Count > 0 then
        Result.Add('parameters', Params)
    else
        Params.Free;
    if AOp.RequestSchema <> '' then
    begin
        Body := TJSONObject.Create;
        //  Only the actions take an optional body: most verbs need none, and
        //  the two that do say so in their own description.
        Body.Add('required', AOp.Route <> rtAction);
        Body.Add('content', JsonContent(AOp.RequestSchema));
        Result.Add('requestBody', Body);
    end;
    Result.Add('responses', ResponsesOf(AOp));
end;

function TagObject(const AName, ADescription: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('name', AName);
    Result.Add('description', ADescription);
end;

function TagList: TJSONArray;
begin
    Result := TJSONArray.Create;
    Result.Add(TagObject('server',
        'What this server is, and what it says about itself.'));
    Result.Add(TagObject('problems',
        'A problem is the unit of work: create one, address it by its id, ' +
        'discard it when done.'));
    Result.Add(TagObject('points',
        'The measured profile, the background, the picks, and the interval ' +
        'the R-factor is measured over.'));
    Result.Add(TagObject('curves', 'The model: its curves and their values.'));
    Result.Add(TagObject('settings',
        'The scalars that decide how a fit is run.'));
    Result.Add(TagObject('fitting',
        'Running the engine, and reading how it is doing.'));
    Result.Add(TagObject('modules',
        'What the modules linked into this build contribute.'));
end;

function InfoObject: TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('title', 'Fit compute server');
    Result.Add('version', IntToStr(WORKER_PROTOCOL_VERSION) + '.0.0');
    Result.Add('description',
        'The fitting engine over HTTP+JSON. This document is assembled by ' +
        'the server that served it, from its own routing and its own ' +
        'registries, at the moment you asked for it - so it describes this ' +
        'build and not a release. Nothing here is committed to the ' +
        'repository, because a stored copy is a copy that can be wrong.' +
        LineEnding + LineEnding +
        'A problem is a resource. Create one with POST /problems, put a ' +
        'profile on it, pick curve positions, run an action, and poll ' +
        '/async while it works. POST /fit is the stateless alternative for ' +
        'a client that keeps the model itself.');
end;

function OpenApiJson: string;
var
    Root, Paths, PathItem: TJSONObject;
    Server: TJSONObject;
    Servers: TJSONArray;
    Ops: TRestOperationArray;
    Components: TJSONObject;
    i: integer;
begin
    Root := TJSONObject.Create;
    try
        Root.Add('openapi', '3.0.3');
        Root.Add('info', InfoObject);
        Servers := TJSONArray.Create;
        Server := TJSONObject.Create;
        //  Relative, so "Try it out" reaches the server that served the page
        //  whatever host or port it is on.
        Server.Add('url', '/');
        Server.Add('description', 'This server.');
        Servers.Add(Server);
        Root.Add('servers', Servers);
        Root.Add('tags', TagList);

        Paths := TJSONObject.Create;
        Ops := Operations;
        for i := 0 to High(Ops) do
        begin
            PathItem := TJSONObject(Paths.Find(Ops[i].Path));
            if PathItem = nil then
            begin
                PathItem := TJSONObject.Create;
                Paths.Add(Ops[i].Path, PathItem);
            end;
            PathItem.Add(LowerCase(Ops[i].Method), OperationObject(Ops[i]));
        end;
        Root.Add('paths', Paths);

        Components := TJSONObject.Create;
        Components.Add('schemas', SchemaComponents);
        Root.Add('components', Components);

        Result := Root.FormatJSON;
    finally
        Root.Free;
    end;
end;

function SwaggerUiHtml: string;
begin
    //  The whole page. Swagger UI is loaded from a CDN rather than vendored:
    //  nothing is added to the repository, nothing has to be packaged, and the
    //  server keeps serving JSON and one string. The cost is that this page
    //  needs the browser to reach the network - the API itself does not.
    Result :=
        '<!DOCTYPE html>' + LineEnding +
        '<html lang="en">' + LineEnding +
        '<head>' + LineEnding +
        '<meta charset="utf-8">' + LineEnding +
        '<meta name="viewport" content="width=device-width, initial-scale=1">' +
        LineEnding +
        '<title>Fit compute server API</title>' + LineEnding +
        '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@'
        + SWAGGER_UI_VERSION + '/swagger-ui.css">' + LineEnding +
        '<style>body{margin:0}#swagger-ui{max-width:1400px;margin:0 auto}</style>'
        + LineEnding +
        '</head>' + LineEnding +
        '<body>' + LineEnding +
        '<div id="swagger-ui"></div>' + LineEnding +
        '<script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@' +
        SWAGGER_UI_VERSION + '/swagger-ui-bundle.js" crossorigin></script>' +
        LineEnding +
        '<script>' + LineEnding +
        'window.onload = function () {' + LineEnding +
        '  window.ui = SwaggerUIBundle({' + LineEnding +
        '    url: "' + OPENAPI_PATH + '",' + LineEnding +
        '    dom_id: "#swagger-ui",' + LineEnding +
        '    deepLinking: true,' + LineEnding +
        '    docExpansion: "list",' + LineEnding +
        '    tryItOutEnabled: true' + LineEnding +
        '  });' + LineEnding +
        '};' + LineEnding +
        '</script>' + LineEnding +
        '</body>' + LineEnding +
        '</html>' + LineEnding;
end;

function ContentTypeOf(const AMethod, APath: string): string;
begin
    if RouteOf(AMethod, APath) = rtDocs then
        Result := 'text/html; charset=utf-8'
    else
        Result := 'application/json';
end;

end.
