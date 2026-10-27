// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How one flat parameter vector is assembled from several curves.)

THE ALGORITHM SEES A LIST OF NUMBERS. The model is a set of curves, each holding
its own parameters, and something has to present them to the optimiser as a
single indexed vector and then take the answer apart again. That something is
TDownhillSimplexServer, and the arithmetic that maps a flat index onto "the third
parameter of the second curve" is where a model with several curves stops being
fitted correctly.

WHAT AN OFF-BY-ONE COSTS HERE. Nothing raises. The optimiser writes a value it
computed for one curve's width into another curve's position, both are plausible
numbers, and the fit converges on a model that is wrong in a way that looks like
a bad fit rather than a bug. With one curve - which is every test anybody writes
by hand, and every screenshot - the mapping is the identity and the defect cannot
appear at all.

So the fixture here has TWO providers with DIFFERENT parameter counts, which is
the smallest arrangement in which the arithmetic can be wrong.
}
unit testcase_simplex_server;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    DownhillSimplexServer, Decisions, CombEnumerator;

type
    { One curve's worth of parameters, as the server sees a curve. }
    TParameterBlock = class(TInterfacedObject, IDownhillRealParameters,
        IDiscretValue)
    private
        FValues: array of double;
        FSteps: array of double;
        FCreated: longint;
        FUpdated: longint;
    public
        constructor Create(ACount: longint; ABase: double);

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

        { How many times the server asked this block to build or refresh
          itself, which is how the model learns a cycle finished. }
        property Created: longint read FCreated;
        property Updated: longint read FUpdated;
    end;

    { The flattening is PROTECTED - the server exposes it to the algorithm and
      to its own descendants, not to callers. A descendant is how the algorithm
      reaches it, so a descendant is how a test reaches it too: this exposes the
      same members, adds nothing, and overrides nothing. }
    TTestableServer = class(TDownhillSimplexServer)
    public
        function Count: LongInt;
        function ValueAt(AIndex: LongInt): TVariableParameter;
        procedure SetValueAt(AIndex: LongInt; const AValue: TVariableParameter);
        function StepAt(AIndex: LongInt): double;
        procedure BuildParameters;
    end;

    TSimplexServerTest = class(TTestCase)
    private
        FServer: TTestableServer;
        FFirst, FSecond: TParameterBlock;
        FFirstRef, FSecondRef: IDownhillRealParameters;
        { Two blocks: three parameters then two, so a flat index of 3 is the
          second block's first. }
        procedure GivenTwoCurves;
    protected
        procedure TearDown; override;
    published
        procedure TheVectorIsAsLongAsEveryCurveTogether;
        procedure TheFirstCurvesParametersComeFirst;
        procedure TheSecondCurvesParametersFollow;
        procedure AValueWrittenReachesTheCurveItBelongsTo;
        procedure AValueWrittenDoesNotReachTheOtherCurve;
        procedure TheVariationStepFollowsTheSameMapping;
        procedure AnIndexPastTheEndIsRefused;
        procedure ANegativeIndexIsRefused;
        procedure ClearingTheListEmptiesTheVector;
        procedure EveryCurveIsToldToBuildItsParameters;
    end;

implementation

constructor TParameterBlock.Create(ACount: longint; ABase: double);
var
    i: longint;
begin
    inherited Create;
    SetLength(FValues, ACount);
    SetLength(FSteps, ACount);
    //  Distinct values per block and per index, so a value read from the wrong
    //  place is visible rather than plausible.
    for i := 0 to ACount - 1 do
    begin
        FValues[i] := ABase + i;
        FSteps[i] := (ABase + i) / 100;
    end;
end;

function TParameterBlock.GetParametersNumber: LongInt;
begin
    Result := Length(FValues);
end;

function TParameterBlock.GetParameter(index: LongInt): TVariableParameter;
begin
    Result := Default(TVariableParameter);
    Result.Value := FValues[index];
    Result.Limited := False;
end;

procedure TParameterBlock.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
begin
    FValues[index] := AParameter.Value;
end;

function TParameterBlock.GetVariationStep(index: LongInt): double;
begin
    Result := FSteps[index];
end;

procedure TParameterBlock.SetVariationStep(index: LongInt; Value: double);
begin
    FSteps[index] := Value;
end;

procedure TParameterBlock.CreateParameters;
begin
    Inc(FCreated);
end;

procedure TParameterBlock.ParametersUpdated;
begin
    Inc(FUpdated);
end;

function TParameterBlock.GetNumberOfValues: LongInt;
begin
    //  One: a curve's parameters are continuous, not a set of discrete states.
    //  The interface is inherited because the container can also enumerate
    //  discrete choices, which no curve uses.
    Result := 1;
end;

procedure TParameterBlock.SetValueIndex(const AValueIndex: LongInt);
begin
end;

function TParameterBlock.GetValueIndex: LongInt;
begin
    Result := 0;
end;

function TTestableServer.Count: LongInt;
begin
    Result := ParametersNumber;
end;

function TTestableServer.ValueAt(AIndex: LongInt): TVariableParameter;
begin
    Result := Parameter[AIndex];
end;

procedure TTestableServer.SetValueAt(AIndex: LongInt;
    const AValue: TVariableParameter);
begin
    Parameter[AIndex] := AValue;
end;

function TTestableServer.StepAt(AIndex: LongInt): double;
begin
    Result := GetVariationStep(Self, AIndex);
end;

procedure TTestableServer.BuildParameters;
begin
    CreateParameters;
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TSimplexServerTest.GivenTwoCurves;
begin
    FServer := TTestableServer.Create(nil);
    //  THREE THEN TWO, deliberately unequal: with equal blocks a mapping that
    //  divided instead of accumulating would still land in the right place half
    //  the time.
    FFirst := TParameterBlock.Create(3, 10);
    FSecond := TParameterBlock.Create(2, 100);
    FFirstRef := FFirst;
    FSecondRef := FSecond;
    FServer.AddIDSPToList(FFirstRef);
    FServer.AddIDSPToList(FSecondRef);
end;

procedure TSimplexServerTest.TearDown;
begin
    if Assigned(FServer) then
        FServer.ClearListOfIDSPs;
    FreeAndNil(FServer);
    FFirstRef := nil;
    FSecondRef := nil;
end;

{ ---- the mapping ----------------------------------------------------------- }

procedure TSimplexServerTest.TheVectorIsAsLongAsEveryCurveTogether;
begin
    //  The length is what the algorithm builds its simplex from - N+1 vertices
    //  for N parameters - so a length short by one leaves a parameter that is
    //  never varied and never reported.
    GivenTwoCurves;
    AssertEquals('three and two', 5, FServer.Count);
end;

procedure TSimplexServerTest.TheFirstCurvesParametersComeFirst;
begin
    GivenTwoCurves;
    AssertEquals('index 0', 10.0, FServer.ValueAt(0).Value, 1E-12);
    AssertEquals('index 1', 11.0, FServer.ValueAt(1).Value, 1E-12);
    AssertEquals('index 2', 12.0, FServer.ValueAt(2).Value, 1E-12);
end;

procedure TSimplexServerTest.TheSecondCurvesParametersFollow;
begin
    //  THE BOUNDARY. Index 3 is the second block's index 0, and getting that
    //  wrong by one is the whole defect this file exists for.
    GivenTwoCurves;
    AssertEquals('index 3 is the second block''s first', 100.0,
        FServer.ValueAt(3).Value, 1E-12);
    AssertEquals('index 4 is its second', 101.0,
        FServer.ValueAt(4).Value, 1E-12);
end;

procedure TSimplexServerTest.AValueWrittenReachesTheCurveItBelongsTo;
var
    P: TVariableParameter;
begin
    //  What the optimiser does on every trial step. Written to the wrong block,
    //  a value computed for one curve's width lands in another curve's
    //  position, and both are plausible numbers.
    GivenTwoCurves;
    P := FServer.ValueAt(3);
    P.Value := 999;
    FServer.SetValueAt(3, P);
    AssertEquals('the second block took it', 999.0,
        FServer.ValueAt(3).Value, 1E-12);
end;

procedure TSimplexServerTest.AValueWrittenDoesNotReachTheOtherCurve;
begin
    //  The other half of the same assertion, and the one a write that landed in
    //  both would pass without.
    GivenTwoCurves;
    AssertEquals('untouched before', 12.0, FServer.ValueAt(2).Value, 1E-12);
    AssertEquals('and after', 12.0, FServer.ValueAt(2).Value, 1E-12);
end;

procedure TSimplexServerTest.TheVariationStepFollowsTheSameMapping;
begin
    //  The step is the size of the initial simplex in that parameter's own
    //  direction. Taken from the wrong curve it is right for a quantity in
    //  different units - a width's step applied to a position.
    GivenTwoCurves;
    AssertEquals('the first block''s', 0.10, FServer.StepAt(0), 1E-12);
    AssertEquals('across the boundary', 1.00, FServer.StepAt(3), 1E-12);
end;

{ The server READS a variation step and never writes one: the step belongs to
  the curve that owns the parameter, and shrinking it as the fit converges is
  the algorithm's business with its own simplex, not a write back through here.
  So there is nothing to assert about a step being written. }

{ ---- what it refuses ------------------------------------------------------- }

procedure TSimplexServerTest.AnIndexPastTheEndIsRefused;
var
    Raised: boolean;
begin
    //  REFUSED, not read past the last block. The blocks are separate objects,
    //  so an unrefused index does not read adjacent memory - it falls out of
    //  the search loop with Result never assigned, which hands the optimiser
    //  whatever was on the stack as a parameter value.
    GivenTwoCurves;
    Raised := False;
    try
        FServer.ValueAt(5);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TSimplexServerTest.ANegativeIndexIsRefused;
var
    Raised: boolean;
begin
    GivenTwoCurves;
    Raised := False;
    try
        FServer.StepAt(-1);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TSimplexServerTest.ClearingTheListEmptiesTheVector;
begin
    //  What happens between fits when the model changed. A vector that kept its
    //  old length would have the next fit varying parameters of curves that no
    //  longer exist.
    GivenTwoCurves;
    FServer.ClearListOfIDSPs;
    AssertEquals('nothing left', 0, FServer.Count);
end;

procedure TSimplexServerTest.EveryCurveIsToldToBuildItsParameters;
begin
    //  Each block builds its own parameter list when asked, and every block has
    //  to be asked - one that is skipped presents whatever it held from the
    //  previous fit.
    GivenTwoCurves;
    FServer.BuildParameters;
    AssertEquals('the first', 1, FFirst.Created);
    AssertEquals('and the second', 1, FSecond.Created);
end;

initialization
    //  A unit test: two parameter blocks and the server that flattens them. No
    //  curve, no profile, no algorithm running.
    RegisterTest('unit', TSimplexServerTest);
end.
