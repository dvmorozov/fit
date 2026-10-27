// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The curve types the framework's own tests fit with.)

The framework ships no curve type, and its suite must build without any module:
a test that names a module's peak to have SOMETHING to fit makes the framework's
suite a module's suite, and leaves it unable to build the day that module moves
to a repository of its own. So the tests that need a peak, a second peak of
another shape, or a background register these instead.

They are ordinary types, registered from this unit's initialization exactly as a
module's are, with an explanation and a noise rule - so every registry walk in
the suite (explanations, formulas, the menu) covers them as it covers any type,
and a build that links a module as well holds both side by side. Each has its
own id and its own name: the registry is keyed by the id, and a curve's type is
read back from its title by the name (curve_types_singleton.
FindCurveClassByName), so borrowing either from a module's type would make the
two indistinguishable.

THE SHAPES ARE THE TEXTBOOK ONES, computed by fitminimizers' SimpMath - the
same arithmetic a module's peaks use - so a test that seeds a synthetic profile
with GaussPoint or PseudoVoigtPoint fits it exactly.
}
unit test_curve_types;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, named_points_set, points_set,
    special_curve_parameter, persistent_curve_parameters, noise_rule,
    configurable_points_set;

type
    { A Gaussian peak whose area is A, centred at x0, of standard deviation
      sigma: three parameters, placed on maxima only. }
    TTestGaussPointsSet = class(TNamedPointsSet)
    protected
        procedure DoCalc; override;
    public
        constructor Create(AOwner: TComponent; AX0: double); overload;
        { The one-argument constructor the registry builds a type with. }
        constructor Create(AOwner: TComponent); overload; override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        class function Explanation: TExplanation; override;
        class function NoiseRule: TNoiseRuleClass; override;
        function GetCurveExpression: string; override;
        function FullWidthPerUnit(const AParameterName: string): double; override;
    end;

    { A pseudo-Voigt peak: a fourth parameter, eta, mixing a Lorentzian into
      the Gaussian - so a test can hold two peak types whose parameter lists
      differ. sigma is shared between instances, as a pseudo-Voigt's is. }
    TTestPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        FEtaP: TSpecialCurveParameter;
        procedure DoCalc; override;
    public
        constructor Create(AOwner: TComponent; AX0: double); overload;
        constructor Create(AOwner: TComponent); overload; override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        class function Explanation: TExplanation; override;
        class function NoiseRule: TNoiseRuleClass; override;
        function GetCurveExpression: string; override;
    end;

    { A straight background b0 + b1 x under the peaks. The registry's default
      background (TNamedPointsSet.IsDefaultBackground) ONLY WHERE NO OTHER
      TYPE IS: an automatic run puts the default under a model, and the
      framework's own suite must see it do so - but a suite that links a
      module as well must take the module's, whichever registered first. }
    TTestLinearBackgroundPointsSet = class(TNamedPointsSet)
    protected
        FB0, FB1: TSpecialCurveParameter;
        procedure DoCalc; override;
    public
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        class function Explanation: TExplanation; override;
        class function NoiseRule: TNoiseRuleClass; override;
        class function IsBackground: boolean; override;
        class function IsDefaultBackground: boolean; override;
        { A background is a count, as a module's is: never below zero over
          data that are not (TNamedPointsSet.KeepsAboveZero). }
        class function ValuesAreNonNegative: boolean; override;
        { b0 is the level: adding to it raises every value alike. }
        function LevelParameterName: string; override;
        { The least-squares line through the baseline points: what every
          background must do with the data's own baseline, which the registry
          walk in testcase_curve_type_registration holds each one to. }
        procedure SeedFromBaseline(const AX, AY: array of double); override;
        function GetCurveExpression: string; override;
    end;

    { A curve whose formula the engine hands it (TNamedPointsSet.
      IsDefinedByFormula), as a user's own formula is: no pick places it
      unless the formula names a position, and its parameters are whatever the
      formula names. Evaluated by the framework's own parser (native_math_expr).

      NAMED TO SORT AFTER ANY MODULE'S FORMULA TYPE. The registry keeps its
      types alphabetically, and Model > Curve Type > User builds the first one
      that is a formula (curve_types_singleton.UserFormulaCurveTypeId) - so in
      a suite that links a module with a formula type of its own, this one must
      not be the one that answers. }
    { What a test formula type asks the user before a curve is added: that it
      has something to configure, as every formula type does (a shape does not
      exist until somebody types it) - and no dialog, which a test never opens:
      asked to show one, it answers as a dialog cancelled. }
    TTestFormulaConfiguration = class(TConfigurablePointsSet)
    public
        class function HasConfigurableParameters: boolean; override;
        class function ShowConfigurationDialog: boolean; override;
        class function HasDefaults: boolean; override;
        class procedure SetDefaults; override;
    end;

    TTestFormulaPointsSet = class(TNamedPointsSet)
    protected
        FExpression: string;
        procedure DoCalc; override;
        function CalcValue(AX: double): double;
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        class function Explanation: TExplanation; override;
        class function NoiseRule: TNoiseRuleClass; override;
        class function IsDefinedByFormula: boolean; override;
        { A formula type is configured before it is placed
          (TTestFormulaConfiguration). }
        class function GetConfigurablePointsSet: TConfigurablePointsSetClass;
            override;
        procedure ApplyFormula(const AExpression: string;
            AParams: Curve_parameters); override;
        procedure CopyParameters(Dest: TObject); override;
        function GetCurveExpression: string; override;
        property Expression: string read FExpression write FExpression;
    end;

implementation

uses
    SimpMath, checks, curve_types_singleton, int_curve_factory,
    amplitude_curve_parameter,
    position_curve_parameter, sigma_curve_parameter, eta_curve_parameter,
    user_curve_parameter, width_curve_parameter, native_math_expr,
    test_noise_rules;

{ What each test type says about itself: complete, as every registered type's
  explanation must be, and plain that it is a test's. }
function TestTypeExplanation(const ASummary, ABody: string): TExplanation;
begin
    Result := NewExplanation('', '', ASummary, esModelChoice);
    AddParagraph(Result, ABody);
    AddParagraph(Result, 'It is registered by the framework''s test suite, ' +
        'which ships no curve type of its own, so that the tests have ' +
        'something to fit without naming a module.');
    AddLimitation(Result, 'It is a test''s, not a field''s: no product ' +
        'offers it.');
end;

{============================ TTestGaussPointsSet =============================}

constructor TTestGaussPointsSet.Create(AOwner: TComponent);
begin
    Create(AOwner, 0);
end;

constructor TTestGaussPointsSet.Create(AOwner: TComponent; AX0: double);
begin
    inherited Create(AOwner);
    AddParameter(TAmplitudeCurveParameter.Create);
    AddParameter(TPositionCurveParameter.Create(AX0, Self));
    AddParameter(TSigmaCurveParameter.Create);
    InitListOfVariableParameters;
    CheckThat(FVariableParameters.Count = 3,
        'the test Gaussian must have built exactly its three variable parameters');
end;

class function TTestGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian (test)';
end;

class function TTestGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{6B1E2C47-0D3A-4F58-9C21-7E84A5D3B1F0}');
end;

class function TTestGaussPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

function TTestGaussPointsSet.GetCurveExpression: string;
begin
    //  GaussPoint in SimpMath, area-normalised by A.
    Result := 'A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))';
end;

procedure TTestGaussPointsSet.DoCalc;
begin
    Gauss(FPoints, A, Sigma, x0);
end;

function TTestGaussPointsSet.FullWidthPerUnit(
    const AParameterName: string): double;
begin
    if SameText(AParameterName, 'sigma') then
        Result := FULL_WIDTH_PER_STANDARD_DEVIATION
    else
        Result := inherited FullWidthPerUnit(AParameterName);
end;

class function TTestGaussPointsSet.Explanation: TExplanation;
begin
    Result := TestTypeExplanation(
        'A Gaussian peak of area A at x0 with standard deviation sigma, ' +
        'for the framework''s tests.',
        'A / (sigma sqrt(2 pi)) exp(-(x - x0)^2 / (2 sigma^2)).');
end;

class function TTestGaussPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestCountsNoiseRule;
end;

{========================= TTestPseudoVoigtPointsSet ==========================}

constructor TTestPseudoVoigtPointsSet.Create(AOwner: TComponent);
begin
    Create(AOwner, 0);
end;

constructor TTestPseudoVoigtPointsSet.Create(AOwner: TComponent; AX0: double);
var
    Parameter: TSpecialCurveParameter;
begin
    inherited Create(AOwner);
    AddParameter(TAmplitudeCurveParameter.Create);
    AddParameter(TPositionCurveParameter.Create(AX0, Self));
    Parameter := TSigmaCurveParameter.Create;
    Parameter.Type_ := Shared;
    AddParameter(Parameter);
    FEtaP := TEtaCurveParameter.Create;
    AddParameter(FEtaP);
    InitListOfVariableParameters;
    CheckThat(FVariableParameters.Count = 3,
        'the test pseudo-Voigt must have built exactly its three variable parameters');
end;

class function TTestPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Pseudo-Voigt (test)';
end;

class function TTestPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{A3C95D10-4B7E-4E26-8F13-2D6B90C4E57A}');
end;

class function TTestPseudoVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

function TTestPseudoVoigtPointsSet.GetCurveExpression: string;
begin
    //  PseudoVoigtPoint in SimpMath: (1-eta) Gaussian + eta Lorentzian, each
    //  normalised by its full width at half maximum.
    Result :=
        'A*((1-eta)*(2*sqrt(log(2))/(sigma*sqrt(pi))*exp(-4*log(2)*(x0-x)**2/sigma**2))'
        + '+eta*((2/(pi*sigma))*(1/(1+(2*(x-x0)/sigma)**2))))';
end;

procedure TTestPseudoVoigtPointsSet.DoCalc;
begin
    PseudoVoigt(FPoints, A, Sigma, FEtaP.Value, x0);
end;

class function TTestPseudoVoigtPointsSet.Explanation: TExplanation;
begin
    Result := TestTypeExplanation(
        'A pseudo-Voigt peak - a Gaussian and a Lorentzian mixed by eta - ' +
        'for the framework''s tests.',
        'A ((1 - eta) G(x) + eta L(x)), each normalised by the full width ' +
        'sigma.');
end;

class function TTestPseudoVoigtPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestCountsNoiseRule;
end;

{======================= TTestLinearBackgroundPointsSet =======================}

constructor TTestLinearBackgroundPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FB0 := TUserCurveParameter.Create;
    FB0.Name := 'b0';
    FB0.Value := 0;
    FB0.Type_ := Variable;
    AddParameter(FB0);
    FB1 := TUserCurveParameter.Create;
    FB1.Name := 'b1';
    FB1.Value := 0;
    FB1.Type_ := Variable;
    AddParameter(FB1);
    InitListOfVariableParameters;
end;

class function TTestLinearBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Linear background (test)';
end;

class function TTestLinearBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{E0F4A8B2-91C6-4D3B-A7E5-5C2F18D69B34}');
end;

class function TTestLinearBackgroundPointsSet.GetExtremumMode: TExtremumMode;
begin
    //  A background has no extremum to search for.
    Result := OnlyMaximums;
end;

class function TTestLinearBackgroundPointsSet.IsBackground: boolean;
begin
    Result := True;
end;

class function TTestLinearBackgroundPointsSet.IsDefaultBackground: boolean;
var
    C: TCurveClass;
begin
    for C in RegisteredCurveClasses do
        if (C <> TTestLinearBackgroundPointsSet) and C.IsBackground and
            C.IsDefaultBackground then
            Exit(False);
    Result := True;
end;

class function TTestLinearBackgroundPointsSet.ValuesAreNonNegative: boolean;
begin
    Result := True;
end;

function TTestLinearBackgroundPointsSet.LevelParameterName: string;
begin
    Result := 'b0';
end;

procedure TTestLinearBackgroundPointsSet.SeedFromBaseline(
    const AX, AY: array of double);
var
    i, N: longint;
    Sx, Sy, Sxx, Sxy, D: double;
begin
    N := Length(AX);
    if (N = 0) or (Length(AY) <> N) then
        Exit;
    Sx := 0; Sy := 0; Sxx := 0; Sxy := 0;
    for i := 0 to N - 1 do
    begin
        Sx := Sx + AX[i];
        Sy := Sy + AY[i];
        Sxx := Sxx + AX[i] * AX[i];
        Sxy := Sxy + AX[i] * AY[i];
    end;
    D := N * Sxx - Sx * Sx;
    if D = 0 then
    begin
        //  One abscissa, or one point: no slope to find, the level is the mean.
        FB0.Value := Sy / N;
        FB1.Value := 0;
    end
    else
    begin
        FB1.Value := (N * Sxy - Sx * Sy) / D;
        FB0.Value := (Sy - FB1.Value * Sx) / N;
    end;
end;

function TTestLinearBackgroundPointsSet.GetCurveExpression: string;
begin
    Result := 'b0+b1*x';
end;

procedure TTestLinearBackgroundPointsSet.DoCalc;
var
    j: longint;
begin
    for j := 0 to PointsCount - 1 do
        FPoints[j][2] := FB0.Value + FB1.Value * FPoints[j][1];
end;

class function TTestLinearBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := TestTypeExplanation(
        'A straight background b0 + b1 x, for the framework''s tests.',
        'b0 is the level at x = 0 and b1 the slope.');
end;

class function TTestLinearBackgroundPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestCountsNoiseRule;
end;

{=========================== TTestFormulaPointsSet ============================}

class function TTestFormulaPointsSet.GetCurveTypeName: string;
begin
    Result := 'User formula (test)';
end;

class function TTestFormulaPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{2D7C4F81-B5A9-4E03-96D2-8A1E3C5F7B60}');
end;

class function TTestFormulaPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := MaximumsAndMinimums;
end;

class function TTestFormulaPointsSet.IsDefinedByFormula: boolean;
begin
    Result := True;
end;

class function TTestFormulaPointsSet.GetConfigurablePointsSet:
    TConfigurablePointsSetClass;
begin
    Result := TTestFormulaConfiguration;
end;

class function TTestFormulaConfiguration.HasConfigurableParameters: boolean;
begin
    Result := True;
end;

class function TTestFormulaConfiguration.ShowConfigurationDialog: boolean;
begin
    Result := False;
end;

class function TTestFormulaConfiguration.HasDefaults: boolean;
begin
    Result := False;
end;

class procedure TTestFormulaConfiguration.SetDefaults;
begin
end;

procedure TTestFormulaPointsSet.ApplyFormula(const AExpression: string;
    AParams: Curve_parameters);
begin
    FExpression := AExpression;
    SetParameters(AParams);
end;

procedure TTestFormulaPointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TTestFormulaPointsSet(Dest).Expression := Expression;
end;

function TTestFormulaPointsSet.CalcValue(AX: double): double;
var
    Prs: string;
    i: longint;
begin
    CheckAssigned(FArgP, 'the parameter that carries the abscissa into the formula');
    FArgP.Value := AX;
    Prs := '';
    for i := 0 to Parameters.Count - 1 do
        Prs := Prs + Parameters[i].Name + '=' + FloatToStr(Parameters[i].Value) +
            Chr(0);
    Result := 0;
    //  No finite value (a zero denominator while the optimiser probes) leaves
    //  0, as a user's formula does, so the fit moves away rather than aborts.
    ParseAndCalcExpression(PChar(FExpression), PChar(Prs), @Result);
end;

procedure TTestFormulaPointsSet.DoCalc;
var
    j: longint;
begin
    for j := 0 to PointsCount - 1 do
        PointYCoord[j] := CalcValue(PointXCoord[j]);
end;

function TTestFormulaPointsSet.GetCurveExpression: string;
begin
    Result := ExpressionToNumpy(FExpression);
end;

class function TTestFormulaPointsSet.Explanation: TExplanation;
begin
    Result := TestTypeExplanation(
        'A curve given by the formula the engine hands it, for the ' +
        'framework''s tests.',
        'Every name in the formula other than x is a parameter.');
end;

class function TTestFormulaPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestCountsNoiseRule;
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(TTestGaussPointsSet);
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TTestPseudoVoigtPointsSet);
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TTestLinearBackgroundPointsSet);
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TTestFormulaPointsSet);
end.
