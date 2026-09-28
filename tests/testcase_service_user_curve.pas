// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The user's formula and its parameters crossing the wire, and the one
route whose timeout depends on what is being asked for.)

A USER-DEFINED CURVE IS A STRING AND A LIST OF NAMES, and both have to reach the
server before it can fit anything: the expression, and every parameter with its
name, its value and what it stands for. The server parses the formula and answers
with the parameters it found, which is how the properties dialog learns what the
user's formula actually declares.

WHAT A DROPPED FIELD COSTS. A parameter that arrives without its TYPE is an
ordinary varied parameter as far as the engine is concerned - so the abscissa
becomes something the optimiser moves, and the curve is evaluated at an x that
has nothing to do with the point being computed. A parameter that arrives without
its NAME reaches no formula at all, because substitution is by name.

OWNERSHIP CROSSES WITH IT. The caller builds the parameter list and hands it
over; the service frees it. Written the other way round it is a leak per edit of
the formula, and the formula is edited a character at a time in a dialog.

AND ONE ROUTE CHOOSES ITS OWN PATIENCE. A module's resource is normally answered
promptly, but a module may declare one that searches - and a search over a long
profile takes as long as a fit. Given the ordinary timeout it would be reported
as a failure exactly on the data it is most useful for, so the declaration is
consulted before the request is made.
}
unit testcase_service_user_curve;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    http_fit_service, persistent_curve_parameters,
    persistent_curve_parameter_container, special_curve_parameter,
    amplitude_curve_parameter, mock_http_transport;

type
    TServiceUserCurveTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        { A list carrying an abscissa and a varied parameter. The caller hands
          ownership to the service, so this is never freed here. }
        function TwoParameters: Curve_parameters;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Sending the formula up.
        procedure TheExpressionIsSent;
        procedure ItGoesToTheSpecialParameterRoute;
        procedure EveryParameterIsSentWithIt;
        procedure AParameterCarriesItsName;
        procedure AParameterCarriesItsValue;
        procedure AParameterCarriesWhatItStandsFor;
        procedure AFormulaWithNoParametersIsStillSent;

        //  Reading the server's answer back.
        procedure TheParametersComeBack;
        procedure EachOneKeepsItsName;
        procedure EachOneKeepsItsValue;
        procedure EachOneKeepsWhatItStandsFor;
        procedure AnAnswerWithNoParametersIsEmptyNotNil;

        //  The module route that chooses its own patience.
        procedure AModuleResourceIsFetchedByItsName;
        procedure AnUnknownResourceIsStillFetched;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TServiceUserCurveTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
end;

procedure TServiceUserCurveTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

function TServiceUserCurveTest.TwoParameters: Curve_parameters;

    procedure Add(const AName: string; AType: TParameterType; AValue: double);
    var
        P: TSpecialCurveParameter;
    begin
        P := TAmplitudeCurveParameter.Create;
        P.Name := AName;
        P.Type_ := AType;
        P.Value := AValue;
        TPersistentCurveParameterContainer(Result.Params.Add).Parameter := P;
    end;

begin
    Result := Curve_parameters.Create(nil);
    Result.Params.Clear;
    Add('x', Argument, 0);
    Add('A', Variable, 2.5);
end;

{ ---- sending the formula up ------------------------------------------------ }

procedure TServiceUserCurveTest.TheExpressionIsSent;
begin
    //  The formula IS the curve. Without it the server has a list of names and
    //  nothing to evaluate.
    FSvc.SetSpecialCurveParameters('A*exp(-x*x)', TwoParameters);
    AssertTrue('the formula is in the body: ' + FSvc.LastBody,
        Pos('A*exp(-x*x)', FSvc.LastBody) > 0);
end;

procedure TServiceUserCurveTest.ItGoesToTheSpecialParameterRoute;
begin
    FSvc.SetSpecialCurveParameters('A*x', TwoParameters);
    AssertTrue('to /special-params: ' + FSvc.LastUrl,
        Pos('/special-params', FSvc.LastUrl) > 0);
end;

procedure TServiceUserCurveTest.EveryParameterIsSentWithIt;
begin
    //  Both of them, in one document. A parameter left behind is one the
    //  formula names and the server does not have.
    FSvc.SetSpecialCurveParameters('A*x', TwoParameters);
    AssertTrue('the abscissa: ' + FSvc.LastBody,
        Pos('"x"', FSvc.LastBody) > 0);
    AssertTrue('and the amplitude: ' + FSvc.LastBody,
        Pos('"A"', FSvc.LastBody) > 0);
end;

procedure TServiceUserCurveTest.AParameterCarriesItsName;
begin
    //  SUBSTITUTION IS BY NAME. A parameter that arrives without one reaches no
    //  formula at all, and the curve evaluates flat.
    FSvc.SetSpecialCurveParameters('A*x', TwoParameters);
    AssertTrue('named: ' + FSvc.LastBody, Pos('"name"', FSvc.LastBody) > 0);
end;

procedure TServiceUserCurveTest.AParameterCarriesItsValue;
begin
    FSvc.SetSpecialCurveParameters('A*x', TwoParameters);
    AssertTrue('valued: ' + FSvc.LastBody, Pos('2.5', FSvc.LastBody) > 0);
end;

procedure TServiceUserCurveTest.AParameterCarriesWhatItStandsFor;
begin
    //  THE TYPE IS THE PART THAT IS EASY TO DROP AND EXPENSIVE TO LOSE. Without
    //  it every parameter is an ordinary varied one, so the ABSCISSA becomes
    //  something the optimiser moves - and the curve is then evaluated at an x
    //  that has nothing to do with the point being computed.
    FSvc.SetSpecialCurveParameters('A*x', TwoParameters);
    AssertTrue('typed: ' + FSvc.LastBody, Pos('"type"', FSvc.LastBody) > 0);
end;

procedure TServiceUserCurveTest.AFormulaWithNoParametersIsStillSent;
begin
    //  The state the dialog is in between a formula being typed and being
    //  parsed: there is an expression and nothing else yet. Refusing it here
    //  would stop the server ever parsing one.
    FSvc.SetSpecialCurveParameters('42', nil);
    AssertTrue('the formula went: ' + FSvc.LastBody,
        Pos('42', FSvc.LastBody) > 0);
end;

{ ---- reading the server's answer back -------------------------------------- }

procedure TServiceUserCurveTest.TheParametersComeBack;
var
    P: Curve_parameters;
begin
    //  THE SERVER PARSES THE FORMULA, so this answer is how the properties
    //  dialog learns what the user's formula actually declares - not the
    //  client's own guess at it.
    FSvc.Reply('special-params',
        '{"params":[{"name":"x","value":0,"type":3},' +
        '{"name":"A","value":2.5,"type":1}]}');
    P := FSvc.GetSpecialCurveParameters;
    try
        AssertEquals('both of them', 2, P.Count);
    finally
        P.Free;
    end;
end;

procedure TServiceUserCurveTest.EachOneKeepsItsName;
var
    P: Curve_parameters;
begin
    FSvc.Reply('special-params',
        '{"params":[{"name":"x","value":0,"type":3},' +
        '{"name":"A","value":2.5,"type":1}]}');
    P := FSvc.GetSpecialCurveParameters;
    try
        AssertEquals('the first', 'x', P[0].Name);
        AssertEquals('and the second', 'A', P[1].Name);
    finally
        P.Free;
    end;
end;

procedure TServiceUserCurveTest.EachOneKeepsItsValue;
var
    P: Curve_parameters;
begin
    FSvc.Reply('special-params',
        '{"params":[{"name":"A","value":2.5,"type":1}]}');
    P := FSvc.GetSpecialCurveParameters;
    try
        AssertEquals(2.5, P[0].Value, 1E-9);
    finally
        P.Free;
    end;
end;

procedure TServiceUserCurveTest.EachOneKeepsWhatItStandsFor;
var
    P: Curve_parameters;
begin
    //  The other direction of the same rule as above: the abscissa has to come
    //  back AS the abscissa, or the dialog offers it as something the fit may
    //  vary.
    FSvc.Reply('special-params',
        '{"params":[{"name":"x","value":0,"type":3}]}');
    P := FSvc.GetSpecialCurveParameters;
    try
        AssertTrue('it is the argument', P[0].Type_ = Argument);
    finally
        P.Free;
    end;
end;

procedure TServiceUserCurveTest.AnAnswerWithNoParametersIsEmptyNotNil;
var
    P: Curve_parameters;
begin
    //  A LIST, NOT NIL. A formula that declares nothing is a formula the user is
    //  halfway through typing, and the dialog asks the list how long it is
    //  rather than whether it exists.
    FSvc.Reply('special-params', '{"params":[]}');
    P := FSvc.GetSpecialCurveParameters;
    try
        AssertTrue('there is a list', Assigned(P));
        AssertEquals('and it is empty', 0, P.Count);
    finally
        P.Free;
    end;
end;

{ ---- the module route that chooses its own patience ------------------------ }

procedure TServiceUserCurveTest.AModuleResourceIsFetchedByItsName;
begin
    //  The resource name is the route. A module's own data has no other way in.
    FSvc.Reply('overlay', '{"anything":1}');
    FSvc.Log.Clear;
    FSvc.ModuleGet('overlay');
    //  The LOG, because this is a GET and LastUrl records the last WRITE.
    AssertTrue('by name: ' + FSvc.Log.AsText,
        Pos('/modules/overlay', FSvc.Log.AsText) > 0);
end;

procedure TServiceUserCurveTest.AnUnknownResourceIsStillFetched;
begin
    //  A RESOURCE THIS BUILD DOES NOT DECLARE is not refused here: the registry
    //  is consulted only to decide how long to wait, and the server is the one
    //  that knows what it serves. Refusing locally would make a client built
    //  without a module unable to talk to a server that has it.
    FSvc.Reply('whatever', '{"anything":1}');
    FSvc.Log.Clear;
    FSvc.ModuleGet('whatever');
    AssertTrue('still asked: ' + FSvc.Log.AsText,
        Pos('/modules/whatever', FSvc.Log.AsText) > 0);
end;

initialization
    //  A unit test: the service over a mock transport. No socket and no server.
    RegisterTest('unit', TServiceUserCurveTest);
end.
