// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The server's account of itself, checked against the server.)

A SPEC IS A CLAIM, and an unchecked one is worse than none: a client that reads
a wrong document does not merely lack information, it acts on the wrong
information and blames its own code. So nothing here checks that the document is
well-formed and stops there. Each test picks one thing the document asserts and
asks the program itself whether it is so.

  The PATHS are checked against rest_routes.RouteOf - the classifier the running
  server uses - so a route whose shape changes cannot leave a document quietly
  describing the shape it used to have.

  The ENUMERATED VALUES are checked against the registries they were read from,
  which is also how the document notices a verb that was added without anyone
  thinking about documentation at all.

  The SCHEMAS are checked against the keys a live response actually carries.
  These are the hand-written part - nothing in the program declares that a
  settings document has ten fields - so they are the part that can drift, and
  the comparison is the only thing standing between "documented" and "true".

WHAT IS NOT HERE. Whether Swagger UI renders the page: that needs a browser, and
the end-to-end test checks the far side of the socket, which is as close as this
side can get.
}
unit testcase_rest_spec;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    rest_routes, rest_spec, rest_spec_schemas, action_registry,
    int_app_module, module_registry, fit_rest_api;

type
    TRestSpecTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        FDoc: TJSONObject;
        { The document, parsed once per test. }
        function Doc: TJSONObject;
        { The schema named in components/schemas. }
        function SchemaNamed(const AName: string): TJSONObject;
        { The property names of an object schema, sorted. }
        function PropertiesOf(ASchema: TJSONObject): TStringList;
        { The keys of one live reply, sorted. }
        function KeysOfReply(const AMethod, APath: string): TStringList;
        { Every $ref value anywhere in AData. }
        procedure CollectRefs(AData: TJSONData; AInto: TStringList);
        { The parameter object of AName on the operation at APath/AMethod. }
        function ParameterOf(const APath, AMethod, AName: string): TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The table against the router.
        procedure EveryRouteTheServerAnswersIsInTheDocument;
        procedure EverySamplePathClassifiesAsTheRouteItsRowDeclares;
        procedure ExactlyOneOperationIsServedOutsideTheRouter;

        //  The document itself.
        procedure TheDocumentIsAnOpenApiDocument;
        procedure EveryPathInTheDocumentComesFromTheTable;
        procedure EveryOperationAnswersWithAnErrorEnvelopeToo;
        procedure EveryReferenceResolves;
        procedure EveryPlaceholderIsADeclaredParameter;

        //  The registries.
        procedure EveryRegisteredVerbIsOfferedByTheActionRoute;
        procedure AndEachIsDescribedInItsOwnWords;
        procedure EveryModuleResourceThisBuildHasIsOfferedByTheModuleRoute;

        //  The schemas against live replies.
        procedure TheSettingsSchemaSaysWhatTheServerSends;
        procedure TheStatisticsSchemaSaysWhatTheServerSends;
        procedure ThePointsSetSchemaSaysWhatTheServerSends;
        procedure TheStatsSchemaSaysWhatTheServerSends;
        procedure TheCurvesSchemaSaysWhatTheServerSends;
        procedure TheHealthSchemaSaysWhatTheServerSends;

        //  The two routes that carry it.
        procedure TheServerAnswersTheDocumentRoute;
        procedure TheServerAnswersTheDocsPage;
        procedure TheDocsPageIsTheOnlyReplyThatIsNotJson;
    end;

implementation

procedure TRestSpecTest.SetUp;
begin
    FApi := TFitRestApi.Create;
    FDoc := nil;
end;

procedure TRestSpecTest.TearDown;
begin
    FreeAndNil(FDoc);
    FreeAndNil(FApi);
end;

function TRestSpecTest.Doc: TJSONObject;
var
    D: TJSONData;
begin
    if FDoc = nil then
    begin
        D := GetJSON(OpenApiJson);
        AssertTrue('the document is a JSON object', D is TJSONObject);
        FDoc := TJSONObject(D);
    end;
    Result := FDoc;
end;

function TRestSpecTest.SchemaNamed(const AName: string): TJSONObject;
var
    Schemas: TJSONObject;
begin
    Schemas := Doc.Objects['components'].Objects['schemas'];
    AssertTrue('the document defines ' + AName, Schemas.Find(AName) <> nil);
    Result := Schemas.Objects[AName];
end;

function TRestSpecTest.PropertiesOf(ASchema: TJSONObject): TStringList;
var
    Props: TJSONObject;
    i: integer;
begin
    Result := TStringList.Create;
    Props := ASchema.Objects['properties'];
    for i := 0 to Props.Count - 1 do
        Result.Add(Props.Names[i]);
    Result.Sort;
end;

function TRestSpecTest.KeysOfReply(const AMethod, APath: string): TStringList;
var
    Code: longint;
    Body: string;
    D: TJSONData;
    i: integer;
begin
    Result := TStringList.Create;
    FApi.Handle(AMethod, APath, '', Code, Body);
    AssertEquals(AMethod + ' ' + APath, 200, Code);
    D := GetJSON(Body);
    try
        AssertTrue('the reply is an object', D is TJSONObject);
        for i := 0 to TJSONObject(D).Count - 1 do
            Result.Add(TJSONObject(D).Names[i]);
    finally
        D.Free;
    end;
    Result.Sort;
end;

procedure TRestSpecTest.CollectRefs(AData: TJSONData; AInto: TStringList);
var
    i: integer;
    Obj: TJSONObject;
begin
    if AData is TJSONObject then
    begin
        Obj := TJSONObject(AData);
        for i := 0 to Obj.Count - 1 do
            if (Obj.Names[i] = '$ref') and (Obj.Items[i] is TJSONString) then
                AInto.Add(Obj.Items[i].AsString)
            else
                CollectRefs(Obj.Items[i], AInto);
    end
    else if AData is TJSONArray then
        for i := 0 to TJSONArray(AData).Count - 1 do
            CollectRefs(TJSONArray(AData).Items[i], AInto);
end;

function TRestSpecTest.ParameterOf(const APath, AMethod,
    AName: string): TJSONObject;
var
    Params: TJSONArray;
    i: integer;
begin
    Result := nil;
    Params := Doc.Objects['paths'].Objects[APath].Objects[AMethod].
        Arrays['parameters'];
    for i := 0 to Params.Count - 1 do
        if TJSONObject(Params.Items[i]).Get('name', '') = AName then
            Exit(TJSONObject(Params.Items[i]));
    AssertTrue(AMethod + ' ' + APath + ' declares a ' + AName + ' parameter',
        False);
end;

{ ---------------------- the table against the router ------------------------ }

procedure TRestSpecTest.EveryRouteTheServerAnswersIsInTheDocument;
var
    Ops: TRestOperationArray;
    R: TRestRoute;
    i: integer;
    Found: boolean;
    Missing: string;
begin
    Ops := Operations;
    Missing := '';
    //  Over the enum rather than over a list of names: a route added to
    //  rest_routes and forgotten here is exactly the drift this catches, and a
    //  list would have to be updated in the same breath as the omission.
    for R := Low(TRestRoute) to High(TRestRoute) do
    begin
        if R = rtUnknown then
            Continue;
        Found := False;
        for i := 0 to High(Ops) do
            if Ops[i].Route = R then
            begin
                Found := True;
                Break;
            end;
        if not Found then
            Missing := Missing + ' ' + IntToStr(Ord(R));
    end;
    AssertEquals('routes the server answers and the document does not ' +
        'describe (as TRestRoute ordinals):' + Missing, '', Missing);
end;

procedure TRestSpecTest.EverySamplePathClassifiesAsTheRouteItsRowDeclares;
var
    Ops: TRestOperationArray;
    i: integer;
begin
    Ops := Operations;
    for i := 0 to High(Ops) do
    begin
        if Ops[i].ServedOutsideRouter then
            Continue;
        //  THE WHOLE GUARANTEE, in one line: the document's own paths, put
        //  back through the classifier the server dispatches on.
        AssertEquals(Ops[i].Method + ' ' + Ops[i].SamplePath +
            ' (documented as ' + Ops[i].Path + ')',
            Ord(Ops[i].Route), Ord(RouteOf(Ops[i].Method, Ops[i].SamplePath)));
    end;
end;

procedure TRestSpecTest.ExactlyOneOperationIsServedOutsideTheRouter;
var
    Ops: TRestOperationArray;
    i, Count: integer;
begin
    Ops := Operations;
    Count := 0;
    for i := 0 to High(Ops) do
        if Ops[i].ServedOutsideRouter then
        begin
            Inc(Count);
            AssertEquals('the endpoint answered before the router is POST /fit',
                '/fit', Ops[i].Path);
        end;
    //  If this ever fails upwards, an endpoint was added at the socket layer
    //  where the router - and so every route test - cannot see it.
    AssertEquals('endpoints answered outside the route table', 1, Count);
end;

{ --------------------------- the document itself ---------------------------- }

procedure TRestSpecTest.TheDocumentIsAnOpenApiDocument;
begin
    AssertEquals('openapi version', '3.0.3', Doc.Get('openapi', ''));
    AssertTrue('it has an info block', Doc.Find('info') <> nil);
    AssertTrue('the info block names a version',
        Doc.Objects['info'].Get('version', '') <> '');
    AssertTrue('it describes some paths', Doc.Objects['paths'].Count > 0);
    AssertTrue('it defines some schemas',
        Doc.Objects['components'].Objects['schemas'].Count > 0);
    //  Relative, so "Try it out" reaches whichever host served the page.
    AssertEquals('the server it names is itself', '/',
        TJSONObject(Doc.Arrays['servers'].Items[0]).Get('url', ''));
end;

procedure TRestSpecTest.EveryPathInTheDocumentComesFromTheTable;
var
    Ops: TRestOperationArray;
    Paths: TJSONObject;
    Item: TJSONObject;
    i, j, k, Described: integer;
    Found: boolean;
begin
    Ops := Operations;
    Paths := Doc.Objects['paths'];
    Described := 0;
    for i := 0 to Paths.Count - 1 do
    begin
        Item := TJSONObject(Paths.Items[i]);
        for j := 0 to Item.Count - 1 do
        begin
            Inc(Described);
            Found := False;
            for k := 0 to High(Ops) do
                if (Ops[k].Path = Paths.Names[i]) and
                    (LowerCase(Ops[k].Method) = Item.Names[j]) then
                    Found := True;
            AssertTrue('the document describes ' + Item.Names[j] + ' ' +
                Paths.Names[i] + ', which no row of the table declares', Found);
        end;
    end;
    //  And nothing was dropped on the way in: every row reached the document.
    AssertEquals('operations described', Length(Ops), Described);
end;

procedure TRestSpecTest.EveryOperationAnswersWithAnErrorEnvelopeToo;
var
    Paths, Item, Op: TJSONObject;
    i, j: integer;
begin
    Paths := Doc.Objects['paths'];
    for i := 0 to Paths.Count - 1 do
    begin
        Item := TJSONObject(Paths.Items[i]);
        for j := 0 to Item.Count - 1 do
        begin
            Op := TJSONObject(Item.Items[j]);
            AssertTrue(Item.Names[j] + ' ' + Paths.Names[i] +
                ' says what a success looks like',
                Op.Objects['responses'].Find('200') <> nil);
            //  Every route can fault, and a document that mentions only the
            //  happy path leaves a client to guess what a 500 body holds.
            AssertTrue(Item.Names[j] + ' ' + Paths.Names[i] +
                ' says what a failure looks like',
                Op.Objects['responses'].Find('500') <> nil);
        end;
    end;
end;

procedure TRestSpecTest.EveryReferenceResolves;
var
    Refs: TStringList;
    Schemas: TJSONObject;
    i: integer;
    Name: string;
begin
    Refs := TStringList.Create;
    try
        CollectRefs(Doc, Refs);
        AssertTrue('the document refers to its schemas', Refs.Count > 0);
        Schemas := Doc.Objects['components'].Objects['schemas'];
        for i := 0 to Refs.Count - 1 do
        begin
            Name := Copy(Refs[i], Length('#/components/schemas/') + 1,
                Length(Refs[i]));
            //  A dangling $ref renders as an empty box in the UI and as
            //  nothing at all in a generated client.
            AssertTrue('the document refers to ' + Refs[i] +
                ', which it does not define', Schemas.Find(Name) <> nil);
        end;
    finally
        Refs.Free;
    end;
end;

procedure TRestSpecTest.EveryPlaceholderIsADeclaredParameter;
var
    Paths, Item, Op: TJSONObject;
    i, j, k, c, Start, Declared: integer;
    P, Name: string;
    Params: TJSONArray;
    Found: boolean;
begin
    Paths := Doc.Objects['paths'];
    for i := 0 to Paths.Count - 1 do
    begin
        P := Paths.Names[i];
        Item := TJSONObject(Paths.Items[i]);
        Declared := 0;
        c := 1;
        while c <= Length(P) do
        begin
            if P[c] = '{' then
            begin
                Start := c + 1;
                while (c <= Length(P)) and (P[c] <> '}') do
                    Inc(c);
                Name := Copy(P, Start, c - Start);
                Inc(Declared);
                for j := 0 to Item.Count - 1 do
                begin
                    Op := TJSONObject(Item.Items[j]);
                    Params := Op.Arrays['parameters'];
                    Found := False;
                    for k := 0 to Params.Count - 1 do
                        if TJSONObject(Params.Items[k]).Get('name', '') = Name then
                            Found := True;
                    //  An undeclared placeholder is a field the UI never asks
                    //  for, so "Try it out" sends the literal {id}.
                    AssertTrue(Item.Names[j] + ' ' + P + ' declares ' + Name,
                        Found);
                end;
            end;
            Inc(c);
        end;
        if Declared = 0 then
            for j := 0 to Item.Count - 1 do
                AssertTrue(Item.Names[j] + ' ' + P + ' needs no parameters',
                    TJSONObject(Item.Items[j]).Find('parameters') = nil);
    end;
end;

{ ------------------------------ the registries ------------------------------ }

procedure TRestSpecTest.EveryRegisteredVerbIsOfferedByTheActionRoute;
var
    Enum: TJSONArray;
    Acts: TActionInfoArray;
    i, j: integer;
    Found: boolean;
begin
    RegisterBuiltInActions;
    Enum := ParameterOf('/problems/{id}/actions/{name}', 'post', 'name').
        Objects['schema'].Arrays['enum'];
    Acts := RegisteredActions;
    AssertTrue('this build registers verbs at all', Length(Acts) > 0);
    for i := 0 to High(Acts) do
    begin
        Found := False;
        for j := 0 to Enum.Count - 1 do
            if Enum.Items[j].AsString = Acts[i].Name then
                Found := True;
        //  Registering a verb is what makes it appear here. If this fails, the
        //  document was built from a list somebody has to remember to update -
        //  which is the thing this design exists to avoid.
        AssertTrue('the document offers the registered verb ' + Acts[i].Name,
            Found);
    end;
    AssertEquals('verbs offered', Length(Acts), Enum.Count);
end;

procedure TRestSpecTest.AndEachIsDescribedInItsOwnWords;
var
    Desc: string;
    Acts: TActionInfoArray;
    i: integer;
begin
    RegisterBuiltInActions;
    Desc := ParameterOf('/problems/{id}/actions/{name}', 'post', 'name').
        Get('description', '');
    Acts := RegisteredActions;
    for i := 0 to High(Acts) do
        AssertTrue('the page carries what ' + Acts[i].Name + ' does',
            Pos(Acts[i].Description, Desc) > 0);
end;

procedure TRestSpecTest.EveryModuleResourceThisBuildHasIsOfferedByTheModuleRoute;
var
    Mods: TAppModuleArray;
    Res: TModuleResourceArray;
    i, j: integer;
    Vendors, Resources: TStringList;

    { The values of one path parameter's enum, sorted; nil when it has none. }
    function EnumOf(const AName: string): TStringList;
    var
        E: TJSONData;
        k: integer;
    begin
        Result := nil;
        E := ParameterOf('/problems/{id}/modules/{vendor}/{resource}',
            'get', AName).Objects['schema'].Find('enum');
        if E = nil then
            Exit;
        AssertTrue(AName + ' lists values', E is TJSONArray);
        Result := TStringList.Create;
        for k := 0 to TJSONArray(E).Count - 1 do
            Result.Add(TJSONArray(E).Items[k].AsString);
        Result.Sort;
    end;

    { A registry name of the form 'vendor/resource', split. }
    procedure AddSplit(const AFull: string);
    var
        Slash: integer;
    begin
        Slash := Pos('/', AFull);
        if Slash <= 0 then
            Exit;
        if Vendors.IndexOf(Copy(AFull, 1, Slash - 1)) < 0 then
            Vendors.Add(Copy(AFull, 1, Slash - 1));
        if Resources.IndexOf(Copy(AFull, Slash + 1, Length(AFull))) < 0 then
            Resources.Add(Copy(AFull, Slash + 1, Length(AFull)));
    end;

var
    Offered: TStringList;
begin
    Vendors := TStringList.Create;
    Resources := TStringList.Create;
    try
        Mods := RegisteredModules;
        for i := 0 to High(Mods) do
        begin
            Res := Mods[i].Resources;
            for j := 0 to High(Res) do
                //  A resource is declared as '<module>/<resource>', which is
                //  exactly the two path segments this route takes.
                AddSplit(Res[j].Name);
        end;
        Vendors.Sort;
        Resources.Sort;

        if Vendors.Count = 0 then
        begin
            //  THE FRAMEWORK SHIPS NO MODULE, and an empty enum would tell a
            //  reader that this route accepts nothing - which is false for the
            //  builds that do have one. No enum at all is the honest answer.
            Offered := EnumOf('vendor');
            AssertTrue('a build with no modules offers no closed list',
                Offered = nil);
            Offered := EnumOf('resource');
            AssertTrue('for either segment', Offered = nil);
            Exit;
        end;

        //  Read from the registry when the document is built, so a module
        //  linked into this binary documents itself.
        Offered := EnumOf('vendor');
        try
            AssertTrue('a build with modules names them', Offered <> nil);
            AssertEquals('the modules this build has and the modules it says ' +
                'it has', Vendors.CommaText, Offered.CommaText);
        finally
            Offered.Free;
        end;
        Offered := EnumOf('resource');
        try
            AssertTrue('and what they answer', Offered <> nil);
            AssertEquals('the resources this build answers and the resources ' +
                'it says it answers', Resources.CommaText, Offered.CommaText);
        finally
            Offered.Free;
        end;
    finally
        Vendors.Free;
        Resources.Free;
    end;
end;

{ --------------------- the schemas against live replies ---------------------- }

procedure TRestSpecTest.TheSettingsSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
    Code: longint;
    Body: string;
    Id: longint;
    D: TJSONData;
begin
    FApi.Handle('POST', '/problems', '', Code, Body);
    D := GetJSON(Body);
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;

    Documented := PropertiesOf(SchemaNamed('Settings'));
    Sent := KeysOfReply('GET', Format('/problems/%d/settings', [Id]));
    try
        //  'ok' rides on the envelope rather than being a setting, so it is
        //  the one key the schema adds; everything else must match exactly.
        Documented.Add('ok');
        Documented.Sort;
        AssertEquals('the settings the server sends and the settings the ' +
            'document describes', Documented.CommaText, Sent.CommaText);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

procedure TRestSpecTest.TheStatisticsSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
    Code: longint;
    Body: string;
    Id, i: longint;
    D, Stats: TJSONData;
begin
    FApi.Handle('POST', '/problems', '', Code, Body);
    D := GetJSON(Body);
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;

    Sent := TStringList.Create;
    Documented := PropertiesOf(SchemaNamed('Statistics'));
    try
        FApi.Handle('GET', Format('/problems/%d/stats', [Id]), '', Code, Body);
        AssertEquals('stats status', 200, Code);
        D := GetJSON(Body);
        try
            Stats := TJSONObject(D).Objects['statistics'];
            for i := 0 to TJSONObject(Stats).Count - 1 do
                Sent.Add(TJSONObject(Stats).Names[i]);
        finally
            D.Free;
        end;
        Sent.Sort;
        AssertEquals('the statistics the server sends and the statistics the ' +
            'document describes', Documented.CommaText, Sent.CommaText);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

procedure TRestSpecTest.ThePointsSetSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
    Code: longint;
    Body: string;
    Id, i: longint;
    D: TJSONData;
begin
    FApi.Handle('POST', '/problems', '', Code, Body);
    D := GetJSON(Body);
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;

    Documented := PropertiesOf(SchemaNamed('PointsSet'));
    Sent := TStringList.Create;
    try
        FApi.Handle('GET', Format('/problems/%d/profile', [Id]), '', Code, Body);
        AssertEquals('profile status', 200, Code);
        D := GetJSON(Body);
        try
            for i := 0 to TJSONObject(D).Count - 1 do
                Sent.Add(TJSONObject(D).Names[i]);
        finally
            D.Free;
        end;
        Sent.Sort;
        //  A point set sends 'ids' only where identities exist, so the reply
        //  is a subset of the schema rather than equal to it - what must hold
        //  is that it sends nothing the document has not described.
        for i := 0 to Sent.Count - 1 do
            AssertTrue('the profile carries ' + Sent[i] +
                ', which the document does not describe',
                Documented.IndexOf(Sent[i]) >= 0);
        AssertTrue('the profile carries its title', Sent.IndexOf('title') >= 0);
        AssertTrue('the profile carries its arguments', Sent.IndexOf('x') >= 0);
        AssertTrue('the profile carries its values', Sent.IndexOf('y') >= 0);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

procedure TRestSpecTest.TheStatsSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
    Code: longint;
    Body: string;
    Id: longint;
    D: TJSONData;
begin
    FApi.Handle('POST', '/problems', '', Code, Body);
    D := GetJSON(Body);
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;

    //  The envelope around the statistics, which has its own test: four
    //  display strings and the nested object, and every one of them is a field
    //  something reads by name.
    Documented := PropertiesOf(SchemaNamed('Stats'));
    Sent := KeysOfReply('GET', Format('/problems/%d/stats', [Id]));
    try
        AssertEquals('what /stats sends and what the document describes',
            Documented.CommaText, Sent.CommaText);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

procedure TRestSpecTest.TheCurvesSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
    Code: longint;
    Body: string;
    Id: longint;
    D: TJSONData;
begin
    FApi.Handle('POST', '/problems', '', Code, Body);
    D := GetJSON(Body);
    try
        Id := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;

    //  THE ENVELOPE ONLY, here. A curve exists once something has been fitted,
    //  which is not a unit test - so what one CONTAINS is compared against the
    //  Curve and CurveParameter schemas end to end, where a fit can be run.
    Documented := PropertiesOf(SchemaNamed('Curves'));
    Sent := KeysOfReply('GET', Format('/problems/%d/curves', [Id]));
    try
        AssertEquals('what /curves sends and what the document describes',
            Documented.CommaText, Sent.CommaText);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

procedure TRestSpecTest.TheHealthSchemaSaysWhatTheServerSends;
var
    Documented, Sent: TStringList;
begin
    Documented := PropertiesOf(SchemaNamed('Health'));
    Sent := KeysOfReply('GET', '/health');
    try
        AssertEquals('what /health sends and what the document describes',
            Documented.CommaText, Sent.CommaText);
    finally
        Documented.Free;
        Sent.Free;
    end;
end;

{ --------------------- the two routes that carry it ------------------------- }

procedure TRestSpecTest.TheServerAnswersTheDocumentRoute;
var
    Code: longint;
    Body: string;
    D: TJSONData;
begin
    //  Through the router, not through OpenApiJson: the point is that the
    //  route exists, needs no problem, and is reachable before anything else
    //  has happened to this server.
    FApi.Handle('GET', '/openapi.json', '', Code, Body);
    AssertEquals('status', 200, Code);
    D := GetJSON(Body);
    try
        AssertTrue('it answers a document', D is TJSONObject);
        AssertEquals('openapi version', '3.0.3',
            TJSONObject(D).Get('openapi', ''));
    finally
        D.Free;
    end;
end;

procedure TRestSpecTest.TheServerAnswersTheDocsPage;
var
    Code: longint;
    Body: string;
begin
    FApi.Handle('GET', '/docs', '', Code, Body);
    AssertEquals('status', 200, Code);
    AssertTrue('it answers an HTML page', Pos('<html', LowerCase(Body)) > 0);
    AssertTrue('the page loads Swagger UI',
        Pos('swagger-ui-bundle', Body) > 0);
    //  It must read the document from the server that served the page, not
    //  from anywhere else - that is what makes the two agree.
    AssertTrue('the page reads this server''s document',
        Pos('"/openapi.json"', Body) > 0);
end;

procedure TRestSpecTest.TheDocsPageIsTheOnlyReplyThatIsNotJson;
begin
    AssertEquals('the docs page', 'text/html; charset=utf-8',
        ContentTypeOf('GET', '/docs'));
    AssertEquals('the document itself', 'application/json',
        ContentTypeOf('GET', '/openapi.json'));
    AssertEquals('an ordinary route', 'application/json',
        ContentTypeOf('GET', '/problems/1/curves'));
    AssertEquals('a path that names nothing', 'application/json',
        ContentTypeOf('GET', '/nonesuch'));
end;

initialization
    RegisterTest('unit', TRestSpecTest);
end.
