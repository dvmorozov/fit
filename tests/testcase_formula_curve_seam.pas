// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A curve type that IS a formula gets its formula from the engine
because it says so, not because it is the user's.)

THE ENGINE NAMED ONE CURVE CLASS, the user's formula (TUserPointsSet): the
fit task built it by its id and handed it the expression, and the service
refused it without one, by the same id. So the type could not leave the
framework for the scientific module, and no module could bring a formula
type of its own. The type now declares the fact
(TNamedPointsSet.IsDefinedByFormula) and receives the formula through
ApplyFormula; the engine asks the fact.

THE TEST TYPE IS A FORMULA TYPE OF ITS OWN, with its own id - what a
module's would be - built on the suite's formula type (test_curve_types),
since the user's own is the scientific module's and is not named here.
Driven through the service in process, the way a pick reaches the engine;
no fit is run.
}
unit testcase_formula_curve_seam;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_service, title_points_set, named_points_set, MyExceptions,
    persistent_curve_parameters, persistent_curve_parameter_container,
    special_curve_parameter, user_curve_parameter, test_curve_types;

type
    { A formula type a module might register: a formula's evaluation under an
      id and a name of its own. }
    TModuleFormulaPointsSet = class(TTestFormulaPointsSet)
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
    end;

    TFormulaCurveSeamTest = class(TTestCase)
    private
        FService: TFitService;
        procedure GivenAPeakedProfileWithBounds;
        function ParametersWithoutAPosition: Curve_parameters;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFormulaTypeOfAModuleIsGivenItsFormula;
        procedure AFormulaTypeOfAModuleWithNoFormulaIsRefused;
    end;

implementation

uses
    curve_types_singleton;

const
    ModuleFormulaId: TGuid = '{9B4E1D27-3C6A-4F85-B2D0-7A1E5C93F468}';

class function TModuleFormulaPointsSet.GetCurveTypeName: string;
begin
    Result := 'Module formula (test)';
end;

class function TModuleFormulaPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := ModuleFormulaId;
end;

procedure TFormulaCurveSeamTest.SetUp;
begin
    FService := TFitService.Create;
end;

procedure TFormulaCurveSeamTest.TearDown;
begin
    FreeAndNil(FService);
end;

procedure TFormulaCurveSeamTest.GivenAPeakedProfileWithBounds;
var
    P, B: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    FService.SetRFactorBounds(B);
end;

function TFormulaCurveSeamTest.ParametersWithoutAPosition: Curve_parameters;

    procedure Add(const AName: string; AType: TParameterType; AValue: double);
    var
        P: TSpecialCurveParameter;
    begin
        P := TUserCurveParameter.Create;
        P.Name := AName;
        P.Type_ := AType;
        P.Value := AValue;
        TPersistentCurveParameterContainer(Result.Params.Add).Parameter := P;
    end;

begin
    Result := Curve_parameters.Create(nil);
    Result.Params.Clear;
    Add('x', Argument, 0);
    Add('A', Variable, 1);
end;

procedure TFormulaCurveSeamTest.AFormulaTypeOfAModuleIsGivenItsFormula;
begin
    GivenAPeakedProfileWithBounds;
    FService.SetCurveType(TModuleFormulaPointsSet.GetCurveTypeId);
    FService.SetSpecialCurveParameters('A*exp(-x*x)', ParametersWithoutAPosition);
    FService.AddPointToCurvePositions(10, 110);
    AssertTrue('the formula was built into a curve',
        FService.CurrentCurves.Count > 0);
    AssertTrue('of the module''s type',
        FService.CurrentCurves.Items[0] is TModuleFormulaPointsSet);
    AssertEquals('carrying the formula', 'A*exp(-x*x)',
        TTestFormulaPointsSet(FService.CurrentCurves.Items[0]).Expression);
end;

procedure TFormulaCurveSeamTest.AFormulaTypeOfAModuleWithNoFormulaIsRefused;
var
    Refused: boolean;
begin
    GivenAPeakedProfileWithBounds;
    FService.SetCurveType(TModuleFormulaPointsSet.GetCurveTypeId);
    Refused := False;
    try
        FService.AddPointToCurvePositions(10, 110);
    except
        on EUserException do
            Refused := True;
    end;
    AssertTrue('refused in words, as the user''s own formula type is', Refused);
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(TModuleFormulaPointsSet);
    RegisterTest('unit', TFormulaCurveSeamTest);
end.
