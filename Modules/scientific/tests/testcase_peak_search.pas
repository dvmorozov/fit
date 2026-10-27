// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The peak search the engine asks the scientific module for: the
points of every peak, highest first, and the refusal of a build without it.)

The search moved from the engine to the module unchanged (2026-10-18); the
engine's own tests of Fit > Automatically and Compute Automatically still run
it end to end. These pin what the module's function returns on data written
out by hand, and that the engine's name for it refuses in words when nothing
registered it.
}
unit testcase_peak_search;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, points_set, title_points_set,
    named_points_set, peak_search, peak_search_registry, problem_value_store,
    MyExceptions, implementation_registry, machine_settings;

type
    TPeakSearchTest = class(TTestCase)
    private
        FData: TPointsSet;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheHighestPointIsFoundFirst;
        procedure BothPeaksAboveTheFractionAreFound;
        procedure TroughsAreFoundWhenAskedFor;
        procedure APositionIsPutAtEachPeaksTop;
        procedure EachPeakIsBounded;
        procedure ATroughsBottomIsAPositionForATroughType;
        procedure BothKindsAreFoundForATypeThatWantsBoth;
        procedure AnUnknownValueHasNoStore;
        procedure WithoutTheSearchTheEngineIsRefusedInWords;
    end;

    { THE SEARCH IS A CONTRACT WITH A CHOICE: the module's is the native one,
      and an alternative from another provider searches only while that
      provider is there. }
    TPeakSearchChoiceTest = class(TTestCase)
    private
        FData: TPointsSet;
        FProviderThere: boolean;
        function ProviderThere: boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheModulesSearchIsTheNativeImplementation;
        procedure AChosenAlternativeSearchesWhileItsProviderIsThere;
    end;

implementation

{ Two peaks on a flat floor of 1: 100 at x = 5, 50 at x = 15, and a bump of 2
  at x = 25. }
procedure TPeakSearchTest.SetUp;
var
    i: longint;
    y: double;
begin
    FData := TPointsSet.Create(nil);
    for i := 0 to 30 do
    begin
        y := 1;
        if Abs(i - 5) <= 1 then y := 100 - 40 * Abs(i - 5);
        if Abs(i - 15) <= 1 then y := 50 - 20 * Abs(i - 15);
        if i = 25 then y := 2;
        FData.AddNewPoint(i, y);
    end;
end;

procedure TPeakSearchTest.TearDown;
begin
    FreeAndNil(FData);
    RegisterPeakSearch(@peak_search.FindPeakPointsFor,
        @peak_search.FindCurvePositionsFor, @peak_search.FindPeakBoundsFor);
end;

procedure TPeakSearchTest.TheHighestPointIsFoundFirst;
var
    P: TTitlePointsSet;
begin
    P := peak_search.FindPeakPoints(FData, False, 30);
    try
        AssertEquals('x of the first point', 5, P.PointXCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TPeakSearchTest.BothPeaksAboveTheFractionAreFound;
var
    P: TTitlePointsSet;
begin
    P := peak_search.FindPeakPoints(FData, False, 30);
    try
        AssertTrue('the first peak', P.IndexOfValueX(5) >= 0);
        AssertTrue('the second', P.IndexOfValueX(15) >= 0);
    finally
        P.Free;
    end;
end;

procedure TPeakSearchTest.TroughsAreFoundWhenAskedFor;
var
    P: TTitlePointsSet;
    i: longint;
begin
    for i := 0 to FData.PointsCount - 1 do
        FData.PointYCoord[i] := -FData.PointYCoord[i];
    P := peak_search.FindPeakPoints(FData, True, 30);
    try
        AssertEquals('the deepest first', 5, P.PointXCoord[0], 1e-9);
    finally
        P.Free;
    end;
end;

procedure TPeakSearchTest.APositionIsPutAtEachPeaksTop;
var
    Positions: TPointsSet;
begin
    Positions := TPointsSet.Create(nil);
    try
        peak_search.FindCurvePositions(FData, OnlyMaximums, 30, Positions);
        AssertTrue('the first top', Positions.IndexOfValueX(5) >= 0);
        AssertTrue('the second', Positions.IndexOfValueX(15) >= 0);
    finally
        Positions.Free;
    end;
end;

procedure TPeakSearchTest.EachPeakIsBounded;
var
    Bounds: TPointsSet;
begin
    Bounds := TPointsSet.Create(nil);
    try
        peak_search.FindPeakBounds(FData, 30, Bounds);
        AssertTrue('at least one pair', Bounds.PointsCount >= 2);
        AssertTrue('the first bound is left of the first top',
            Bounds.PointXCoord[0] <= 5);
    finally
        Bounds.Free;
    end;
end;

procedure TPeakSearchTest.ATroughsBottomIsAPositionForATroughType;
var
    Positions: TPointsSet;
    i: longint;
begin
    for i := 0 to FData.PointsCount - 1 do
        FData.PointYCoord[i] := -FData.PointYCoord[i];
    Positions := TPointsSet.Create(nil);
    try
        peak_search.FindCurvePositions(FData, OnlyMinimums, 30, Positions);
        AssertTrue('the deepest trough', Positions.IndexOfValueX(5) >= 0);
    finally
        Positions.Free;
    end;
end;

procedure TPeakSearchTest.BothKindsAreFoundForATypeThatWantsBoth;
var
    Positions: TPointsSet;
begin
    Positions := TPointsSet.Create(nil);
    try
        peak_search.FindCurvePositions(FData, MaximumsAndMinimums, 30, Positions);
        AssertTrue('the top among them', Positions.IndexOfValueX(5) >= 0);
    finally
        Positions.Free;
    end;
end;

{ A value no module keeps has no store - a build without the module asks
  and gets nil, and the window does without. }
procedure TPeakSearchTest.AnUnknownValueHasNoStore;
begin
    AssertTrue(ProblemValueStore('no-such-value') = nil);
end;

procedure TPeakSearchTest.WithoutTheSearchTheEngineIsRefusedInWords;
var
    Refused: boolean;
begin
    RegisterPeakSearch(nil, nil, nil);
    AssertFalse('none available', PeakSearchAvailable);
    Refused := False;
    try
        peak_search_registry.FindPeakPoints(FData, False, nil).Free;
    except
        on EUserException do Refused := True;
    end;
    AssertTrue('the points', Refused);
    Refused := False;
    try
        peak_search_registry.FindCurvePositions(FData, OnlyMaximums, nil, FData);
    except
        on EUserException do Refused := True;
    end;
    AssertTrue('the positions', Refused);
    Refused := False;
    try
        peak_search_registry.FindPeakBounds(FData, nil, FData);
    except
        on EUserException do Refused := True;
    end;
    AssertTrue('the bounds', Refused);
end;

{ TPeakSearchChoiceTest }

{ The alternative: one point at (-1, -1), which no real search finds. }
function AlternativeFind(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.AddNewPoint(-1, -1);
end;

procedure AlternativePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
begin
    APositions.AddNewPoint(-1, -1);
end;

procedure AlternativeBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
begin
    ABounds.AddNewPoint(-1, 0);
    ABounds.AddNewPoint(-0.5, 0);
end;

function TPeakSearchChoiceTest.ProviderThere: boolean;
begin
    Result := FProviderThere;
end;

procedure TPeakSearchChoiceTest.SetUp;
var
    i: longint;
begin
    FData := TPointsSet.Create(nil);
    for i := 0 to 30 do
        if Abs(i - 5) <= 1 then
            FData.AddNewPoint(i, 100 - 40 * Abs(i - 5))
        else
            FData.AddNewPoint(i, 1);
end;

procedure TPeakSearchChoiceTest.TearDown;
begin
    FreeAndNil(FData);
    SetProviderProbe(PythonSidecarProvider, nil);
    MachineSettings.SetStr(ImplementationsSection, PeakSearchContract, '');
    //  The module's search alone again, as linking it left it.
    RegisterPeakSearch(@peak_search.FindPeakPointsFor,
        @peak_search.FindCurvePositionsFor, @peak_search.FindPeakBoundsFor);
end;

procedure TPeakSearchChoiceTest.TheModulesSearchIsTheNativeImplementation;
var
    All: TImplementationInfoArray;
begin
    All := ImplementationsOf(PeakSearchContract);
    AssertEquals('one, the module''s', 1, Length(All));
    AssertEquals(NativePeakSearchId, All[0].Id);
    AssertEquals(NativeProvider, All[0].Provider);
end;

procedure TPeakSearchChoiceTest.AChosenAlternativeSearchesWhileItsProviderIsThere;
var
    Info: TImplementationInfo;
    P: TTitlePointsSet;
    Positions, Bounds: TPointsSet;
begin
    Info := Default(TImplementationInfo);
    Info.Id := 'alternative';
    Info.Name := 'An alternative search';
    Info.Description := 'Finds one point no real search would.';
    Info.Provider := PythonSidecarProvider;
    RegisterPeakSearchImplementation(Info, @AlternativeFind,
        @AlternativePositions, @AlternativeBounds);
    ChooseImplementation(PeakSearchContract, 'alternative');
    SetProviderProbe(PythonSidecarProvider, @ProviderThere);

    FProviderThere := False;
    P := peak_search_registry.FindPeakPoints(FData, False, nil);
    try
        AssertEquals('absent, the native search finds the peak', 5,
            P.PointXCoord[0], 1e-9);
    finally
        P.Free;
    end;

    FProviderThere := True;
    P := peak_search_registry.FindPeakPoints(FData, False, nil);
    Positions := TPointsSet.Create(nil);
    Bounds := TPointsSet.Create(nil);
    try
        AssertEquals('there, the one chosen searches', -1, P.PointXCoord[0], 1e-9);
        FindCurvePositions(FData, OnlyMaximums, nil, Positions);
        AssertEquals('for positions too', -1, Positions.PointXCoord[0], 1e-9);
        FindPeakBounds(FData, nil, Bounds);
        AssertEquals('and bounds', -1, Bounds.PointXCoord[0], 1e-9);
        AssertTrue(PeakSearchAvailable);
    finally
        P.Free;
        Positions.Free;
        Bounds.Free;
    end;
end;

initialization
    RegisterTest('unit', TPeakSearchTest);
    RegisterTest('unit', TPeakSearchChoiceTest);
end.
