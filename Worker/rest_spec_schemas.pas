// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The shapes the REST surface sends and accepts, as OpenAPI schemas.)

THE ONE HAND-AUTHORED PART OF THE SPEC. Paths and methods are read off the
router (rest_spec joins the route table to rest_routes' own classifier, and a
test feeds every path back through it), and the enumerated values - actions,
module resources, point-set names - are read off the live registries. Bodies
cannot be: nothing in the program declares that a settings document has ten
fields of these types. So they are written here, once, and the tests compare
them against what a running server actually emits.

WHY THE ENVELOPES ARE NOT ONE SCHEMA. Most replies are an ok flag merged with
the answer, but the point-set reads are the bare object and the module routes
are whatever the module says. An "everything is ok-wrapped" schema would be
wrong for a third of the surface and would read as a promise, so each shape is
written as it goes on the wire.

NUMBERS THAT ARE STRINGS. GetRFactorStr and GetCalcTimeStr are formatted for
display and cross the wire as strings; that is what the server sends, so that
is what the spec says. Typing them as numbers here would produce a document
that lies in the direction a client would trust.
}
unit rest_spec_schemas;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpjson;

{ A reference to a named component schema. }
function SchemaRef(const AName: string): TJSONObject;
{ A scalar node: type plus the one line that says what it is. }
function Prim(const AType, ADesc: string): TJSONObject;
{ An array node whose items are AItems (which it takes ownership of). }
function ArrayOf(AItems: TJSONData; const ADesc: string): TJSONObject;
{ An object node with an empty property bag, ready for AddProp. }
function ObjectSchema(const ADesc: string): TJSONObject;
{ Adds one named property to an object node made by ObjectSchema. }
procedure AddProp(ASchema: TJSONObject; const AName: string; AProp: TJSONData);
{ Marks properties as required. }
procedure Require(ASchema: TJSONObject; const ANames: array of string);

{ Every schema the document refers to, as components/schemas. Caller owns it. }
function SchemaComponents: TJSONObject;

implementation

function SchemaRef(const AName: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('$ref', '#/components/schemas/' + AName);
end;

function Prim(const AType, ADesc: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('type', AType);
    if ADesc <> '' then
        Result.Add('description', ADesc);
end;

function ArrayOf(AItems: TJSONData; const ADesc: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('type', 'array');
    if ADesc <> '' then
        Result.Add('description', ADesc);
    Result.Add('items', AItems);
end;

function ObjectSchema(const ADesc: string): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('type', 'object');
    if ADesc <> '' then
        Result.Add('description', ADesc);
    Result.Add('properties', TJSONObject.Create);
end;

procedure AddProp(ASchema: TJSONObject; const AName: string; AProp: TJSONData);
begin
    TJSONObject(ASchema.Find('properties')).Add(AName, AProp);
end;

procedure Require(ASchema: TJSONObject; const ANames: array of string);
var
    Arr: TJSONArray;
    i: integer;
begin
    Arr := TJSONArray.Create;
    for i := Low(ANames) to High(ANames) do
        Arr.Add(ANames[i]);
    ASchema.Add('required', Arr);
end;

{ The failure envelope. Every route can answer with this. }
function ErrorSchema: TJSONObject;
begin
    Result := ObjectSchema('What a refused or failed request answers.');
    AddProp(Result, 'ok', Prim('boolean', 'Always false here.'));
    AddProp(Result, 'error', Prim('string', 'What went wrong, for a person.'));
    Require(Result, ['ok', 'error']);
end;

function OkSchema: TJSONObject;
begin
    Result := ObjectSchema('The bare success envelope.');
    AddProp(Result, 'ok', Prim('boolean', 'Always true here.'));
    Require(Result, ['ok']);
end;

function OkMessageSchema: TJSONObject;
begin
    Result := ObjectSchema('Success, with whatever the engine said about it.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'message', Prim('string',
        'The engine''s own account of what it did; empty when it had none.'));
    Require(Result, ['ok']);
end;

function HealthSchema: TJSONObject;
begin
    Result := ObjectSchema('Liveness, and which protocol this build speaks.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'version', Prim('integer',
        'The worker protocol version. Bumped when the shapes change ' +
        'incompatibly, so a client can refuse a server it cannot talk to.'));
    Require(Result, ['ok', 'version']);
end;

function ProblemCreatedSchema: TJSONObject;
begin
    Result := ObjectSchema('The problem this call just created.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'id', Prim('integer',
        'The handle every other route addresses this problem by.'));
    Require(Result, ['ok', 'id']);
end;

function StateSchema: TJSONObject;
begin
    Result := ObjectSchema('Where the problem is in its lifecycle.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'state', Prim('integer',
        'The engine state as its ordinal (see TFitServerState).'));
    Require(Result, ['ok', 'state']);
end;

function AsyncStateSchema: TJSONObject;
begin
    Result := ObjectSchema(
        'Progress of a long operation. This is the polled route: a client ' +
        'that started an asynchronous action reads it until done is true.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'busy', Prim('boolean',
        'An asynchronous operation is running now.'));
    AddProp(Result, 'done', Prim('boolean',
        'The last asynchronous operation has finished.'));
    AddProp(Result, 'curMin', Prim('number',
        'The best value the running minimizer has reached.'));
    AddProp(Result, 'state', Prim('integer', 'As GET /problems/{id}/state.'));
    Require(Result, ['ok', 'busy', 'done', 'curMin', 'state']);
end;

function PointsSetSchema: TJSONObject;
begin
    Result := ObjectSchema(
        'A named set of points. Sent bare rather than ok-wrapped: it is the ' +
        'resource itself, and this is how it already crossed the wire.');
    AddProp(Result, 'title', Prim('string', 'What the set is called.'));
    AddProp(Result, 'x', ArrayOf(Prim('number', ''), 'The arguments.'));
    AddProp(Result, 'y', ArrayOf(Prim('number', ''), 'The values.'));
    AddProp(Result, 'ids', ArrayOf(Prim('string', ''),
        'Curve identities, one per point, and only on the curve positions: ' +
        'an identity is issued to the pick a curve is seeded from, so no ' +
        'other set has any. Absent or empty elsewhere.'));
    Require(Result, ['title', 'x', 'y']);
end;

{ The ten scalars of a problem. AOptional drops the required list, which is
  what makes the same ten fields serve as a partial write body. }
function SettingsSchema(AOptional: boolean): TJSONObject;
begin
    if AOptional then
        Result := ObjectSchema(
            'Any subset of the problem''s settings. Absent fields are left ' +
            'alone, so a client may send only what it changed.')
    else
        Result := ObjectSchema('The problem''s scalar settings.');
    AddProp(Result, 'maxRFactor', Prim('number',
        'The R-factor below which a fit is considered good enough.'));
    AddProp(Result, 'backFactor', Prim('number',
        'How far the background estimate is allowed to move.'));
    AddProp(Result, 'curveThresh', Prim('number',
        'The relative height below which a curve is dropped.'));
    AddProp(Result, 'waveLength', Prim('number',
        'The wavelength, for the argument axes that need one.'));
    AddProp(Result, 'backgroundVariation', Prim('boolean',
        'Let the background vary during the fit.'));
    AddProp(Result, 'curveScaling', Prim('boolean',
        'Fit an amplitude for each curve.'));
    AddProp(Result, 'minimizerKind', Prim('integer',
        'Which minimizer runs the fit, as its ordinal.'));
    AddProp(Result, 'lossKind', Prim('integer',
        'Which loss function the minimizer optimises, as its ordinal.'));
    AddProp(Result, 'weighting', Prim('integer',
        'How data points are weighted, as its ordinal.'));
    AddProp(Result, 'curveType', Prim('string',
        'The curve model, as the GUID string its type is registered under.'));
    if not AOptional then
        Require(Result, ['maxRFactor', 'backFactor', 'curveThresh',
            'waveLength', 'backgroundVariation', 'curveScaling',
            'minimizerKind', 'lossKind', 'weighting', 'curveType']);
end;

function StatisticsSchema: TJSONObject;
begin
    Result := ObjectSchema(
        'Goodness-of-fit statistics. Always present; valid says whether the ' +
        'numbers mean anything yet.');
    AddProp(Result, 'valid', Prim('boolean',
        'False before a fit has produced anything to measure.'));
    AddProp(Result, 'dataPoints', Prim('integer', ''));
    AddProp(Result, 'params', Prim('integer',
        'How many parameters the fit varied.'));
    AddProp(Result, 'degreesOfFreedom', Prim('integer', ''));
    AddProp(Result, 'chiSquare', Prim('number', ''));
    AddProp(Result, 'reducedChiSquare', Prim('number', ''));
    AddProp(Result, 'rSquared', Prim('number', ''));
    AddProp(Result, 'aic', Prim('number', 'Akaike information criterion.'));
    AddProp(Result, 'bic', Prim('number', 'Bayesian information criterion.'));
    Require(Result, ['valid', 'dataPoints', 'params', 'degreesOfFreedom',
        'chiSquare', 'reducedChiSquare', 'rSquared', 'aic', 'bic']);
end;

function StatsSchema: TJSONObject;
begin
    Result := ObjectSchema('How well the model fits, and what it cost.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'calcTime', Prim('string',
        'How long the last fit took, formatted for display.'));
    AddProp(Result, 'rFactor', Prim('string',
        'The R-factor, formatted for display.'));
    AddProp(Result, 'absRFactor', Prim('string', ''));
    AddProp(Result, 'sqrRFactor', Prim('string', ''));
    AddProp(Result, 'statistics', SchemaRef('Statistics'));
    Require(Result, ['ok', 'calcTime', 'rFactor', 'absRFactor', 'sqrRFactor',
        'statistics']);
end;

function RFactorSchema: TJSONObject;
begin
    Result := ObjectSchema('The current R-factor, and the running minimum.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'rFactor', Prim('string',
        'Formatted for display, as on the stats route.'));
    AddProp(Result, 'curMin', Prim('number',
        'The best value the minimizer has reached.'));
    Require(Result, ['ok', 'rFactor', 'curMin']);
end;

function CurveParameterSchema: TJSONObject;
begin
    Result := ObjectSchema('One quantity of one curve.');
    AddProp(Result, 'name', Prim('string', ''));
    AddProp(Result, 'value', Prim('number',
        'The value. A parameter that holds text sends a string here and says ' +
        'so in kind - JSON is self-describing, so nothing needs a second field.'));
    AddProp(Result, 'type', Prim('integer',
        'What the parameter is to the model (variable, fixed, computed...), ' +
        'as its ordinal.'));
    AddProp(Result, 'error', Prim('number',
        'The uncertainty the last fit estimated, or zero.'));
    AddProp(Result, 'kind', Prim('string',
        'Present only when value is not a number; then it is "text".'));
    Require(Result, ['name', 'value', 'type', 'error']);
end;

function CurveSchema: TJSONObject;
begin
    Result := ObjectSchema('One curve of the model.');
    AddProp(Result, 'id', Prim('string',
        'The handle the curve routes address this instance by. It is issued ' +
        'to the pick the curve was seeded from and survives a rebuild.'));
    AddProp(Result, 'fitted', Prim('boolean',
        'Whether an optimiser produced these values. Cannot be derived from ' +
        'the values themselves, so it is sent beside them.'));
    AddProp(Result, 'params', ArrayOf(SchemaRef('CurveParameter'), ''));
    Require(Result, ['id', 'fitted', 'params']);
end;

function CurvesSchema: TJSONObject;
begin
    Result := ObjectSchema('Every curve in the model, with its parameters.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'curves', ArrayOf(SchemaRef('Curve'), ''));
    Require(Result, ['ok', 'curves']);
end;

function CurveValuesWriteSchema: TJSONObject;
var
    Param, Curve: TJSONObject;
begin
    Param := ObjectSchema('');
    AddProp(Param, 'name', Prim('string', ''));
    AddProp(Param, 'value', Prim('number', ''));
    AddProp(Param, 'error', Prim('number', ''));
    Require(Param, ['name', 'value']);

    Curve := ObjectSchema('');
    AddProp(Curve, 'id', Prim('string',
        'Must name a curve the model still holds; an unknown handle fails ' +
        'the whole request rather than being skipped.'));
    AddProp(Curve, 'fitted', Prim('boolean', ''));
    AddProp(Curve, 'params', ArrayOf(Param, ''));
    Require(Curve, ['id', 'params']);

    Result := ObjectSchema(
        'The whole model''s values in one request. One request rather than ' +
        'one per parameter because each write rebuilds the whole model, and ' +
        'because only a whole-model write can say an optimiser produced them.');
    AddProp(Result, 'curves', ArrayOf(Curve, ''));
    Require(Result, ['curves']);
end;

function SpecialParameterSchema: TJSONObject;
begin
    Result := ObjectSchema('One parameter of the user-defined curve.');
    AddProp(Result, 'name', Prim('string', ''));
    AddProp(Result, 'value', Prim('number', ''));
    AddProp(Result, 'type', Prim('integer',
        'What the parameter is to the expression, as its ordinal.'));
    Require(Result, ['name', 'value', 'type']);
end;

function SpecialParamsSchema: TJSONObject;
begin
    Result := ObjectSchema('The user-defined curve''s parameters.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'params', ArrayOf(SchemaRef('SpecialParameter'), ''));
    Require(Result, ['ok', 'params']);
end;

function SpecialParamsWriteSchema: TJSONObject;
begin
    Result := ObjectSchema('The user-defined curve: its formula, and the ' +
        'parameters that formula names.');
    AddProp(Result, 'expression', Prim('string',
        'The formula, in the syntax the user-curve parser accepts.'));
    AddProp(Result, 'params', ArrayOf(SchemaRef('SpecialParameter'), ''));
end;

function PointCreateSchema: TJSONObject;
begin
    Result := ObjectSchema('A point to append to the set.');
    AddProp(Result, 'x', Prim('number', ''));
    AddProp(Result, 'y', Prim('number', ''));
    Require(Result, ['x', 'y']);
end;

function PointMoveSchema: TJSONObject;
begin
    Result := ObjectSchema('Where a point was, and where it goes.');
    AddProp(Result, 'prevX', Prim('number', 'The point to move.'));
    AddProp(Result, 'prevY', Prim('number', ''));
    AddProp(Result, 'x', Prim('number', 'Where it lands.'));
    AddProp(Result, 'y', Prim('number', ''));
    Require(Result, ['prevX', 'prevY', 'x', 'y']);
end;

function CurveParamValueSchema: TJSONObject;
begin
    Result := ObjectSchema('One parameter''s new value.');
    AddProp(Result, 'value', Prim('number', ''));
    Require(Result, ['value']);
end;

function ModuleStatesSchema: TJSONObject;
var
    State: TJSONObject;
begin
    State := ObjectSchema('');
    AddProp(State, 'module', Prim('string', 'Which module wrote it.'));
    AddProp(State, 'content', Prim('string',
        'The module''s document as text, not parsed and re-emitted: the ' +
        'framework does not read what a module keeps.'));
    Require(State, ['module', 'content']);

    Result := ObjectSchema(
        'Every module''s project-state document, collected in one answer. A ' +
        'client cannot assemble this itself: the modules that matter are the ' +
        'server''s, and it may not have the same ones linked.');
    AddProp(Result, 'ok', Prim('boolean', ''));
    AddProp(Result, 'states', ArrayOf(State, ''));
    Require(Result, ['ok', 'states']);
end;

function ActionBodySchema: TJSONObject;
begin
    Result := ObjectSchema(
        'What the action needs, if anything. Most take no body; ' +
        'select-profile-interval takes start and stop, subtract-background ' +
        'takes auto.');
    AddProp(Result, 'start', Prim('integer',
        'select-profile-interval: the first profile index to keep.'));
    AddProp(Result, 'stop', Prim('integer',
        'select-profile-interval: the last profile index to keep.'));
    AddProp(Result, 'auto', Prim('boolean',
        'subtract-background: estimate the background rather than using the ' +
        'points already set.'));
end;

{ The stateless whole-problem fit. Mirrors fit_problem_json. }
function FitProblemSchema: TJSONObject;
var
    Param, Curve: TJSONObject;
begin
    Param := ObjectSchema('');
    AddProp(Param, 'name', Prim('string', ''));
    AddProp(Param, 'value', Prim('number', ''));
    AddProp(Param, 'error', Prim('number', ''));
    AddProp(Param, 'vary', Prim('boolean', 'Let the fit move this one.'));
    AddProp(Param, 'shared', Prim('boolean',
        'One value across every curve rather than one per curve.'));
    AddProp(Param, 'min', Prim('number', 'Lower bound, when bounded.'));
    AddProp(Param, 'max', Prim('number', 'Upper bound, when bounded.'));
    Require(Param, ['name', 'value']);

    Curve := ObjectSchema('');
    AddProp(Curve, 'params', ArrayOf(Param, ''));

    Result := ObjectSchema(
        'A whole fitting problem, carrying everything the fit needs. No ' +
        'problem resource is created and nothing is remembered afterwards.');
    AddProp(Result, 'op', Prim('string', 'Always "fit".'));
    AddProp(Result, 'profileX', ArrayOf(Prim('number', ''), ''));
    AddProp(Result, 'profileY', ArrayOf(Prim('number', ''), ''));
    AddProp(Result, 'positionsX', ArrayOf(Prim('number', ''),
        'One entry per curve: where it sits.'));
    AddProp(Result, 'positionsY', ArrayOf(Prim('number', ''), ''));
    AddProp(Result, 'curveTypeId', Prim('string',
        'The curve model, as the GUID string its type is registered under.'));
    AddProp(Result, 'expression', Prim('string',
        'The formula, when the curve type is the user-defined one.'));
    AddProp(Result, 'curves', ArrayOf(Curve, ''));
    AddProp(Result, 'weighting', Prim('integer', ''));
    AddProp(Result, 'maxRFactor', Prim('number', ''));
    AddProp(Result, 'waveLength', Prim('number', ''));
    AddProp(Result, 'backgroundVariation', Prim('boolean', ''));
    AddProp(Result, 'curveScaling', Prim('boolean', ''));
    AddProp(Result, 'minimizerKind', Prim('integer', ''));
    AddProp(Result, 'lossKind', Prim('integer', ''));
    AddProp(Result, 'begIndex', Prim('integer',
        'The profile interval to fit over.'));
    AddProp(Result, 'endIndex', Prim('integer', ''));
    Require(Result, ['profileX', 'profileY']);
end;

function FitOutcomeSchema: TJSONObject;
var
    Param, Curve: TJSONObject;
begin
    Param := ObjectSchema('');
    AddProp(Param, 'name', Prim('string', ''));
    AddProp(Param, 'value', Prim('number', ''));
    AddProp(Param, 'error', Prim('number', ''));

    Curve := ObjectSchema('');
    AddProp(Curve, 'params', ArrayOf(Param, ''));

    Result := ObjectSchema('What the fit produced.');
    AddProp(Result, 'ok', Prim('boolean', 'True when errorCode is zero.'));
    AddProp(Result, 'errorCode', Prim('integer', ''));
    AddProp(Result, 'rFactor', Prim('number',
        'The R-factor as a number here, unlike the display strings the ' +
        'stateful routes send.'));
    AddProp(Result, 'curves', ArrayOf(Curve, ''));
    Require(Result, ['ok', 'errorCode', 'rFactor', 'curves']);
end;

function SchemaComponents: TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('Error', ErrorSchema);
    Result.Add('Ok', OkSchema);
    Result.Add('OkMessage', OkMessageSchema);
    Result.Add('Health', HealthSchema);
    Result.Add('ProblemCreated', ProblemCreatedSchema);
    Result.Add('State', StateSchema);
    Result.Add('AsyncState', AsyncStateSchema);
    Result.Add('PointsSet', PointsSetSchema);
    Result.Add('Settings', SettingsSchema(False));
    Result.Add('SettingsWrite', SettingsSchema(True));
    Result.Add('Statistics', StatisticsSchema);
    Result.Add('Stats', StatsSchema);
    Result.Add('RFactor', RFactorSchema);
    Result.Add('CurveParameter', CurveParameterSchema);
    Result.Add('Curve', CurveSchema);
    Result.Add('Curves', CurvesSchema);
    Result.Add('CurveValuesWrite', CurveValuesWriteSchema);
    Result.Add('SpecialParameter', SpecialParameterSchema);
    Result.Add('SpecialParams', SpecialParamsSchema);
    Result.Add('SpecialParamsWrite', SpecialParamsWriteSchema);
    Result.Add('PointCreate', PointCreateSchema);
    Result.Add('PointMove', PointMoveSchema);
    Result.Add('CurveParamValue', CurveParamValueSchema);
    Result.Add('ModuleStates', ModuleStatesSchema);
    Result.Add('ActionBody', ActionBodySchema);
    Result.Add('FitProblem', FitProblemSchema);
    Result.Add('FitOutcome', FitOutcomeSchema);
end;

end.
