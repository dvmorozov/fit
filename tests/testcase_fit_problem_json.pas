// SPDX-License-Identifier: GPL-3.0-or-later
{ Round-trip tests for the client<->server fit wire contract (fit_problem_json).
  Pure record<->JSON, no engine or LCL, so it runs in both suites. }
unit testcase_fit_problem_json;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, fit_problem_json;
type
  TFitProblemJsonTest = class(TTestCase)
  published
    procedure ProblemRoundTrips;
    procedure ProblemExpressionAndSeedCurvesRoundTrip;
    procedure OutcomeRoundTrips;
    procedure MalformedProblemRejected;
  end;

implementation

procedure TFitProblemJsonTest.ProblemRoundTrips;
var
  P, Q: TFitProblem;
begin
  P.ProfileX := TDoubleArray.Create(0, 0.5, 1.0);
  P.ProfileY := TDoubleArray.Create(10, 20, 30);
  P.PositionsX := TDoubleArray.Create(0.5);
  P.PositionsY := TDoubleArray.Create(20);
  P.CurveTypeId := '{9C0EFC1A-0000-1111-2222-333344445555}';
  P.MaxRFactor := 0.02;
  P.WaveLength := 1.5406;
  P.BackgroundVariation := True;
  P.CurveScaling := False;
  P.MinimizerKind := 0;
  P.BegIndex := 0;
  P.EndIndex := 2;

  AssertTrue('parsed back', FitProblemFromJson(FitProblemToJson(P), Q));
  AssertEquals('profileX len', 3, Length(Q.ProfileX));
  AssertEquals('profileX[1]', 0.5, Q.ProfileX[1], 1e-12);
  AssertEquals('profileY[2]', 30.0, Q.ProfileY[2], 1e-12);
  AssertEquals('positionsX[0]', 0.5, Q.PositionsX[0], 1e-12);
  AssertEquals('positionsY[0]', 20.0, Q.PositionsY[0], 1e-12);
  AssertEquals('curveTypeId', P.CurveTypeId, Q.CurveTypeId);
  AssertEquals('maxRFactor', 0.02, Q.MaxRFactor, 1e-12);
  AssertEquals('waveLength', 1.5406, Q.WaveLength, 1e-12);
  AssertTrue('backgroundVariation', Q.BackgroundVariation);
  AssertFalse('curveScaling', Q.CurveScaling);
  AssertEquals('endIndex', 2, Q.EndIndex);
end;

procedure TFitProblemJsonTest.ProblemExpressionAndSeedCurvesRoundTrip;
var
  P, Q: TFitProblem;
begin
  P.ProfileX := TDoubleArray.Create(0, 1);
  P.ProfileY := TDoubleArray.Create(1, 2);
  P.Expression := 'A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))';
  P.Weighting := 'none';   //  Python-only residual weighting
  SetLength(P.Curves, 2);
  SetLength(P.Curves[0].Params, 2);
  P.Curves[0].Params[0].Name := 'A';
  P.Curves[0].Params[0].Value := 100.0;
  P.Curves[0].Params[0].Vary := True;
  P.Curves[0].Params[0].Shared := False;
  P.Curves[0].Params[0].Min := 0;             //  A >= 0
  P.Curves[0].Params[0].Max := Infinity;      //  unbounded above (omitted on wire)
  P.Curves[0].Params[1].Name := 'sigma';
  P.Curves[0].Params[1].Value := 1.5;
  P.Curves[0].Params[1].Vary := True;
  P.Curves[0].Params[1].Shared := True;   //  a shared parameter
  P.Curves[0].Params[1].Min := NegInfinity;
  P.Curves[0].Params[1].Max := Infinity;
  SetLength(P.Curves[1].Params, 1);
  P.Curves[1].Params[0].Name := 'x0';
  P.Curves[1].Params[0].Value := 7.25;
  P.Curves[1].Params[0].Vary := False;     //  a held parameter
  P.Curves[1].Params[0].Shared := False;
  P.Curves[1].Params[0].Min := NegInfinity;
  P.Curves[1].Params[0].Max := Infinity;

  AssertTrue('parsed back', FitProblemFromJson(FitProblemToJson(P), Q));
  AssertEquals('expression', P.Expression, Q.Expression);
  AssertEquals('weighting', 'none', Q.Weighting);
  AssertEquals('curve count', 2, Length(Q.Curves));
  AssertEquals('c0 param count', 2, Length(Q.Curves[0].Params));
  AssertEquals('c0 p0 name', 'A', Q.Curves[0].Params[0].Name);
  AssertEquals('c0 p1 value', 1.5, Q.Curves[0].Params[1].Value, 1e-12);
  AssertTrue('c0 p1 shared', Q.Curves[0].Params[1].Shared);
  AssertTrue('c0 p0 varied', Q.Curves[0].Params[0].Vary);
  //  A finite min round-trips; an infinite bound is omitted and reads back as Inf.
  AssertEquals('c0 p0 min', 0.0, Q.Curves[0].Params[0].Min, 1e-12);
  AssertTrue('c0 p0 max is +Inf', IsInfinite(Q.Curves[0].Params[0].Max)
    and (Q.Curves[0].Params[0].Max > 0));
  AssertTrue('c0 p1 min is -Inf', IsInfinite(Q.Curves[0].Params[1].Min)
    and (Q.Curves[0].Params[1].Min < 0));
  AssertEquals('c1 p0 name', 'x0', Q.Curves[1].Params[0].Name);
  AssertEquals('c1 p0 value', 7.25, Q.Curves[1].Params[0].Value, 1e-12);
  AssertFalse('c1 p0 held', Q.Curves[1].Params[0].Vary);
end;

procedure TFitProblemJsonTest.OutcomeRoundTrips;
var
  O, R: TFitOutcome;
begin
  O.ErrorCode := 0;
  O.RFactor := 0.0123;
  SetLength(O.Curves, 1);
  SetLength(O.Curves[0].Params, 2);
  O.Curves[0].Params[0].Name := 'A';
  O.Curves[0].Params[0].Value := 99.5;
  O.Curves[0].Params[1].Name := 'SIGMA';
  O.Curves[0].Params[1].Value := 1.42;

  AssertTrue('parsed back', FitOutcomeFromJson(FitOutcomeToJson(O), R));
  AssertEquals('errorCode', 0, R.ErrorCode);
  AssertEquals('rFactor', 0.0123, R.RFactor, 1e-12);
  AssertEquals('curve count', 1, Length(R.Curves));
  AssertEquals('param count', 2, Length(R.Curves[0].Params));
  AssertEquals('param0 name', 'A', R.Curves[0].Params[0].Name);
  AssertEquals('param0 value', 99.5, R.Curves[0].Params[0].Value, 1e-12);
  AssertEquals('param1 name', 'SIGMA', R.Curves[0].Params[1].Name);
end;

procedure TFitProblemJsonTest.MalformedProblemRejected;
var
  P: TFitProblem;
begin
  AssertFalse('garbage rejected', FitProblemFromJson('not json at all', P));
end;

initialization
  RegisterTest('unit', TFitProblemJsonTest);
end.
