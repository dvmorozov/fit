// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Levenberg-Marquardt, the package's minimiser for a fit with an exact
answer, through the same container the Downhill Simplex runs in.)

WHY A SECOND MINIMISER IN THE PACKAGE. The simplex is the right tool for a fit
to noisy data whose landscape has several valleys; it is the wrong one for a
smooth least-squares problem whose answer is exact - a pattern whose turns are
to go to given points. There the simplex stalls a hair short (two samples off on
the module's seed, found 2026-10-13), and Levenberg-Marquardt, which uses the
residuals themselves and not only their sum, converges. It used to be written
inside a module, beside the package - a parallel bypass; it lives here now, and
the container it runs in is the simplex's own, so parameters, limits, the best
point found and Stop behave the same for both.
}
unit testcase_levenberg_marquardt;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    DownhillSimplexServer, LevenbergMarquardtServer, CombEnumerator;

type
    { A problem as the container sees one: parameters, with limits where asked,
      and the residuals at the point last set. }
    TResidualProblem = class(TInterfacedObject, IDownhillRealParameters,
        IDiscretValue, IOptimizedResiduals, IUpdatingResults)
    public
        Values, Steps, MinLimit, MaxLimit: array of double;
        Limited: array of boolean;
        Kind: longint;
        Evaluations: longint;
        { Stop asked of this server after this many evaluations; 0 never. }
        StopServer: TLevenbergMarquardtServer;
        StopAfter: longint;
        constructor Create(const AStart: array of double; AKind: longint);
        //  IDownhillSimplexParameters
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);
        function GetVariationStep(index: LongInt): double;
        procedure SetVariationStep(index: LongInt; Value: double);
        //  IDownhillRealParameters
        procedure CreateParameters;
        procedure ParametersUpdated;
        //  IDiscretValue
        function GetNumberOfValues: LongInt;
        procedure SetValueIndex(const AValueIndex: LongInt);
        function GetValueIndex: LongInt;
        //  IOptimizedFunction, IOptimizedResiduals
        function GetOptimizedFunction: double;
        function GetResidualsNumber: LongInt;
        function GetResidual(index: LongInt): double;
        //  IUpdatingResults
        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdateResults(Sender: TComponent);
    end;

    TLevenbergMarquardtTest = class(TTestCase)
    private
        FServer: TLevenbergMarquardtServer;
        FProblem: TResidualProblem;
        FParams: IDownhillRealParameters;
        procedure Solve(const AStart: array of double; AKind: longint);
    protected
        procedure TearDown; override;
    published
        procedure RosenbrocksValleyIsFollowedToItsMinimum;
        procedure AnExactAnswerIsReachedToTheLastDigit;
        procedure ALimitedParameterNeverLeavesItsInterval;
        procedure TheCallerIsLeftAtTheBestPointFound;
        procedure StopEndsTheRunAtTheBestPointFoundSoFar;
    end;

implementation

const
    KIND_ROSENBROCK = 0;
    KIND_LINE = 1;
    KIND_TOWARDS_FIVE = 2;

constructor TResidualProblem.Create(const AStart: array of double; AKind: longint);
var
    i: longint;
begin
    inherited Create;
    Kind := AKind;
    SetLength(Values, Length(AStart));
    SetLength(Steps, Length(AStart));
    SetLength(MinLimit, Length(AStart));
    SetLength(MaxLimit, Length(AStart));
    SetLength(Limited, Length(AStart));
    for i := 0 to High(AStart) do
    begin
        Values[i] := AStart[i];
        Steps[i] := 0.1;
        Limited[i] := False;
    end;
end;

function TResidualProblem.GetParametersNumber: LongInt;
begin
    Result := Length(Values);
end;

function TResidualProblem.GetParameter(index: LongInt): TVariableParameter;
begin
    Result := Default(TVariableParameter);
    Result.Value := Values[index];
    Result.Limited := Limited[index];
    Result.MinLimit := MinLimit[index];
    Result.MaxLimit := MaxLimit[index];
end;

procedure TResidualProblem.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
begin
    Values[index] := AParameter.Value;
end;

function TResidualProblem.GetVariationStep(index: LongInt): double;
begin
    Result := Steps[index];
end;

procedure TResidualProblem.SetVariationStep(index: LongInt; Value: double);
begin
    Steps[index] := Value;
end;

procedure TResidualProblem.CreateParameters;
begin
end;

procedure TResidualProblem.ParametersUpdated;
begin
end;

function TResidualProblem.GetNumberOfValues: LongInt;
begin
    Result := 1;
end;

procedure TResidualProblem.SetValueIndex(const AValueIndex: LongInt);
begin
end;

function TResidualProblem.GetValueIndex: LongInt;
begin
    Result := 0;
end;

function TResidualProblem.GetResidualsNumber: LongInt;
begin
    case Kind of
        KIND_ROSENBROCK: Result := 2;
        KIND_LINE: Result := 5;
    else
        Result := 1;
    end;
end;

function TResidualProblem.GetResidual(index: LongInt): double;
begin
    case Kind of
        //  Rosenbrock's function as least squares: a curved valley floor.
        KIND_ROSENBROCK:
            if index = 0 then
                Result := 10 * (Values[1] - Sqr(Values[0]))
            else
                Result := 1 - Values[0];
        //  A line through five points it passes exactly: y = 2 x + 3.
        KIND_LINE:
            Result := Values[0] * index + Values[1] - (2 * index + 3);
    else
        Result := Values[0] - 5;
    end;
end;

function TResidualProblem.GetOptimizedFunction: double;
var
    i: longint;
begin
    Inc(Evaluations);
    if Assigned(StopServer) and (StopAfter > 0) and (Evaluations >= StopAfter) then
        StopServer.StopAlgorithm;
    Result := 0;
    for i := 0 to GetResidualsNumber - 1 do
        Result := Result + Sqr(GetResidual(i));
end;

procedure TResidualProblem.ShowCurJobProgress(Sender: TComponent;
    MinValue, MaxValue, CurValue: LongInt);
begin
end;

procedure TResidualProblem.ResetCurJobProgress(Sender: TComponent);
begin
end;

procedure TResidualProblem.ShowMessage(Sender: TComponent; Msg: string);
begin
end;

procedure TResidualProblem.UpdateResults(Sender: TComponent);
begin
end;

procedure TLevenbergMarquardtTest.Solve(const AStart: array of double;
    AKind: longint);
begin
    FProblem := TResidualProblem.Create(AStart, AKind);
    FParams := FProblem;
    FServer := TLevenbergMarquardtServer.Create(nil);
    FServer.OptimizedResiduals := FProblem;
    FServer.UpdatingResults := FProblem;
    FServer.AddIDSPToList(FParams);
end;

procedure TLevenbergMarquardtTest.TearDown;
begin
    FreeAndNil(FServer);
    FParams := nil;
    FProblem := nil;
end;

procedure TLevenbergMarquardtTest.RosenbrocksValleyIsFollowedToItsMinimum;
begin
    Solve([-1.2, 1], KIND_ROSENBROCK);
    FServer.Run;
    AssertEquals('x', 1, FProblem.Values[0], 1e-6);
    AssertEquals('y', 1, FProblem.Values[1], 1e-6);
end;

procedure TLevenbergMarquardtTest.AnExactAnswerIsReachedToTheLastDigit;
begin
    //  What the simplex does not do: a zero-residual problem solved to
    //  rounding, not to a tolerance on the spread of its values.
    Solve([0, 0], KIND_LINE);
    FServer.Run;
    AssertEquals('slope', 2, FProblem.Values[0], 1e-9);
    AssertEquals('intercept', 3, FProblem.Values[1], 1e-9);
end;

procedure TLevenbergMarquardtTest.ALimitedParameterNeverLeavesItsInterval;
begin
    //  The minimum is at 5; the parameter is limited to [-10, 2]. The
    //  container's limits wrap round, as an angle's do (SimpMath.
    //  PutValueIntoInterval), and write back what the parameter became, so the
    //  algorithm works with the truth: it ends inside the interval, as near
    //  the minimum as the interval lets it.
    Solve([0], KIND_TOWARDS_FIVE);
    FProblem.Limited[0] := True;
    FProblem.MinLimit[0] := -10;
    FProblem.MaxLimit[0] := 2;
    FServer.Run;
    AssertTrue('inside the interval: ' + FloatToStr(FProblem.Values[0]),
        (FProblem.Values[0] >= -10) and (FProblem.Values[0] <= 2));
    AssertEquals('as near the minimum as it allows', 2, FProblem.Values[0], 1e-9);
end;

procedure TLevenbergMarquardtTest.TheCallerIsLeftAtTheBestPointFound;
begin
    //  The last point evaluated is a Jacobian's trial step, not the answer:
    //  the caller must stand at the best point when the run ends.
    Solve([-1.2, 1], KIND_ROSENBROCK);
    FServer.Run;
    AssertEquals('the best value is where the caller stands',
        FServer.FTotalMinimum, FProblem.GetOptimizedFunction, 1e-15);
end;

procedure TLevenbergMarquardtTest.StopEndsTheRunAtTheBestPointFoundSoFar;
var
    Start: double;
begin
    Solve([-1.2, 1], KIND_ROSENBROCK);
    Start := FProblem.GetOptimizedFunction;
    FProblem.Evaluations := 0;
    FProblem.StopServer := FServer;
    FProblem.StopAfter := 12;
    FServer.Run;
    AssertTrue('ended soon after Stop: ' + IntToStr(FProblem.Evaluations),
        FProblem.Evaluations < 30);
    AssertTrue('no worse than where it started',
        FProblem.GetOptimizedFunction <= Start);
    AssertTrue('away from the minimum, so it really was stopped',
        Abs(FProblem.Values[0] - 1) > 1e-3);
end;

initialization
    RegisterTest('unit', TLevenbergMarquardtTest);
end.
