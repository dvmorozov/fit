// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The axis modes the framework's own tests choose between.)

The framework registers only coordinates that belong to no field (position,
value, logarithmic, custom...), and every field's coordinates are a module's.
But what the framework does WITH a module's mode - prefers it for a model,
remembers it, asks for the number it reads, refuses to draw it without that
number - can only be tested with one registered. Naming a module's modes for
that made the framework's suite a module's suite; these stand in for them.

THEY HAVE THE SHAPES A MODULE'S MODES HAVE: one shows the stored coordinate
under a name of its own, one transforms it, one cannot be drawn without a
number the module describes (RegisterAxisParameter), and one shows only the
value. Each has an id of its own, so a build that links a module too holds both
side by side; and nothing here claims a former display mode
(RegisterLegacyArgumentMode), which only one module may hold.

A peak type that prefers them is here too (TTestAxisPeakPointsSet): what a
model's curves say about its axes is a curve type's preference, so the tests of
that chain need a type that states one.
}
unit test_axis_modes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, coordinate_axis, axis_mode_registry, explanation,
    explanation_registry, named_points_set,
    test_curve_types;

const
    { Where the stand-ins name themselves, as a module names its modes in a
      chapter of its own: the guide must name every axis entry by its path
      (testcase_client_explanations), and these are entries like any other. }
    TestAxesNamespace = 'test-axes';
    TestAxesTopic = 'test-axes/the-test-modes';
    { The stored coordinate, under a name and unit of its own. }
    TestPlainAxisModeId = 'test.plain';
    { Half the stored coordinate. }
    TestHalfAxisModeId = 'test.half';
    { The stored coordinate divided by the scale; undrawable without one. }
    TestScaledAxisModeId = 'test.scaled';
    { The value, under a name of its own. }
    TestCountsAxisModeId = 'test.counts';
    { The number the modes read, as a module describes one. }
    TestScaleParameter = 'test.scale';

type
    { What the three argument modes draw on: the stored coordinate, half of it,
      or it divided by the scale. }
    TTestAxisKind = (takPlain, takHalf, takScaled);

    TTestArgumentAxis = class(TCoordinateAxis)
    private
        FKind: TTestAxisKind;
        FScale: double;
    public
        constructor Create(AKind: TTestAxisKind; AScale: double);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

    TTestPlainAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TTestHalfAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TTestScaledAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TTestCountsAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    { A peak that prefers the plain argument and the counts value, as a field's
      peak prefers its field's coordinates. }
    TTestAxisPeakPointsSet = class(TTestGaussPointsSet)
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
        class function PreferredAxisMode(ADimension: TAxisDimension): string;
            override;
    end;

    { The chapter that names the stand-ins. }
    TTestAxesChapter = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

{ Registers the scale, then the four modes and the chapter naming them - as a
  module's front door does, and as harmless to call twice. }
procedure RegisterTestAxisModes;

{ What the scale is, as the framework asks for it. }
function TestScaleParameterInfo: TAxisParameterInfo;

{ The numbers the modes may read: the scale, at AValue. }
function TestScale(AValue: double): TAxisValues;

implementation

uses
    checks, curve_types_singleton, guide_data;

constructor TTestArgumentAxis.Create(AKind: TTestAxisKind; AScale: double);
begin
    inherited Create;
    FKind := AKind;
    FScale := AScale;
end;

function TTestArgumentAxis.DisplayName: string;
begin
    case FKind of
        takPlain:  Result := 'Plain';
        takHalf:   Result := 'Half';
        takScaled: Result := 'Scaled';
    end;
end;

function TTestArgumentAxis.UnitName: string;
begin
    case FKind of
        takPlain, takHalf: Result := 'u';
        takScaled:         Result := 'u/s';
    end;
end;

function TTestArgumentAxis.ToDisplay(const RawValue: double): double;
begin
    case FKind of
        takPlain: Result := RawValue;
        takHalf:  Result := RawValue / 2;
        takScaled:
        begin
            CheckThat(FScale <> 0, 'a scaled test axis needs a nonzero scale');
            Result := RawValue / FScale;
        end;
    end;
end;

function TTestArgumentAxis.FromDisplay(const DisplayValue: double): double;
begin
    case FKind of
        takPlain: Result := DisplayValue;
        takHalf:  Result := DisplayValue * 2;
        takScaled:
        begin
            CheckThat(FScale <> 0, 'a scaled test axis needs a nonzero scale');
            Result := DisplayValue * FScale;
        end;
    end;
end;

function ArgumentInfo(const AId, ACaption: string): TAxisModeInfo;
begin
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Topic := ArgumentAxesTopic;
    Result.Dimensions := [adArgument];
    //  Every argument mode READS the scale, as every angle of a module reads
    //  its number; only the scaled one cannot be drawn without it.
    Result.Parameter := apValue;
    Result.ParameterRequired := False;
    Result.ParameterName := TestScaleParameter;
end;

class function TTestPlainAxisMode.Info: TAxisModeInfo;
begin
    Result := ArgumentInfo(TestPlainAxisModeId, 'Plain (test)');
end;

{$hints off}
class function TTestPlainAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TTestArgumentAxis.Create(takPlain,
        AxisValueOf(AContext.Values, TestScaleParameter));
end;
{$hints on}

class function TTestHalfAxisMode.Info: TAxisModeInfo;
begin
    Result := ArgumentInfo(TestHalfAxisModeId, 'Half (test)');
end;

{$hints off}
class function TTestHalfAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TTestArgumentAxis.Create(takHalf,
        AxisValueOf(AContext.Values, TestScaleParameter));
end;
{$hints on}

class function TTestScaledAxisMode.Info: TAxisModeInfo;
begin
    Result := ArgumentInfo(TestScaledAxisModeId, 'Scaled (test)');
    Result.ParameterRequired := True;
end;

{$hints off}
class function TTestScaledAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TTestArgumentAxis.Create(takScaled,
        AxisValueOf(AContext.Values, TestScaleParameter));
end;
{$hints on}

class function TTestCountsAxisMode.Info: TAxisModeInfo;
begin
    Result.Id := TestCountsAxisModeId;
    Result.Caption := 'Counts (test)';
    Result.Topic := ValueAxesTopic;
    Result.Dimensions := [adValue];
    Result.Parameter := apNone;
    Result.ParameterRequired := False;
    Result.ParameterName := '';
end;

{$hints off}
class function TTestCountsAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TNamedAxis.Create('Counts', '');
end;
{$hints on}

class function TTestAxisPeakPointsSet.GetCurveTypeName: string;
begin
    Result := 'Axis peak (test)';
end;

class function TTestAxisPeakPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{5F2A9C63-7E14-4B08-A1D6-3C8E07B42F95}');
end;

class function TTestAxisPeakPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '', 'A Gaussian peak that prefers the ' +
        'test axis modes, for the framework''s tests.', esModelChoice);
    AddParagraph(Result, 'Its argument is shown on the plain test axis and ' +
        'its value as counts, unless the user chooses otherwise.');
    AddParagraph(Result, 'It is registered by the framework''s test suite, ' +
        'which ships no curve type of its own, so that the tests of the ' +
        'axis preferences have a type that states one.');
    AddLimitation(Result, 'It is a test''s, not a field''s: no product ' +
        'offers it.');
end;

class function TTestAxisPeakPointsSet.PreferredAxisMode(
    ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := TestPlainAxisModeId;
        adValue:    Result := TestCountsAxisModeId;
    else
        Result := '';
    end;
end;

var
    Chapter: TTestAxesChapter;

function TTestAxesChapter.Namespace: string;
begin
    Result := TestAxesNamespace;
end;

function TTestAxesChapter.StaticTopics: TStringArray;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0] := TestAxesTopic;
end;

function TTestAxesChapter.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := ATopic = TestAxesTopic;
    if not Result then
        Exit;
    AExplanation := NewExplanation(TestAxesTopic, 'The test axis modes',
        'The axis modes the framework''s test suite registers in place of a ' +
        'module''s.', esModelChoice);
    AddParagraph(AExplanation, 'Data > Argument Transformation > Use Rule > ' +
        'Plain (test) shows the stored coordinate; Data > Argument ' +
        'Transformation > Use Rule > Half (test) shows half of it; Data > ' +
        'Argument Transformation > Use Rule > Scaled (test) divides it by the ' +
        'scale, and cannot be drawn without one. Data > Value Transformation ' +
        '> Use Rule > Counts (test) names the value.');
    AddLimitation(AExplanation, 'They are a test''s, not a field''s: no ' +
        'product offers them.');
end;

function TestScaleParameterInfo: TAxisParameterInfo;
begin
    Result.Name := TestScaleParameter;
    Result.Caption := 'Scale';
    Result.Example := '2.5';
end;

function TestScale(AValue: double): TAxisValues;
begin
    Result := nil;
    PutAxisValue(Result, TestScaleParameter, AValue);
end;

procedure RegisterTestAxisModes;
begin
    //  What the modes read, before the modes: a mode naming a number nobody
    //  describes is a finding (AxisModeFindings).
    RegisterAxisParameter(TestScaleParameterInfo);
    RegisterAxisMode(TTestHalfAxisMode);
    RegisterAxisMode(TTestPlainAxisMode);
    RegisterAxisMode(TTestScaledAxisMode);
    RegisterAxisMode(TTestCountsAxisMode);
    RegisterExplanationProvider(Chapter);
end;

initialization
    Chapter := TTestAxesChapter.Create;
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TTestAxisPeakPointsSet);
finalization
    Chapter.Free;
end.
