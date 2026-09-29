// SPDX-License-Identifier: GPL-3.0-or-later
{ The axis modes a build registers, and the rule that chooses among them.

  WHAT WENT WRONG. A price series analysed with the wave-count module was
  captioned "2*Theta [deg]": the argument's only default asked the curve type
  selected in the Tools list, which still named a diffraction peak, and the
  value was captioned "Intensity" whatever it was. Every test below that names a
  price or a bar is that report, one step at a time. }
unit testcase_axis_modes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    coordinate_axis, axis_mode_registry, axis_mode_registration, axis_choice,
    diffraction_axis_modes,
    explanation_registry, client_explanations, guide_data;

type
    TAxisModeRegistryTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        procedure AKnownIdResolvesToItsMode;
        procedure AnIdIsFoundWhateverItsCase;
        procedure AnUnknownIdResolvesToNothing;
        procedure TheSameModeRegisteredAgainIsANoOp;
        procedure TwoModesCannotClaimOneId;
        procedure AModeWithNoClassIsRefused;
        procedure AModeWithNoIdIsRefused;
        procedure AModeForNoCoordinateIsRefused;
        procedure EachCoordinateListsOnlyTheModesThatShowIt;
        procedure TheFrameworksOwnModesHeadEveryMenu;
        procedure ACustomAxisIsCaptionedForItsCoordinate;
        //  The walk.
        procedure EveryRegisteredModeIsComplete;
        procedure EveryRegisteredModeBuildsAnAxisThatRoundTrips;
        procedure AModeWithoutAnExplanationIsReported;
        procedure AModeNamingATopicNothingExplainsIsReported;
        procedure AModeRequiringAnUnnamedParameterIsReported;
        procedure AModeWithNoIdIsReportedByTheWalkToo;
        procedure AModeForNoCoordinateIsReportedByTheWalkToo;
        procedure AModeWithABlankCaptionIsReported;
    end;

    TAxisChoiceTest = class(TTestCase)
    private
        function Prefs(const AModel: array of string;
            const AData, ASelected: string): TAxisPreferences;
        function NoDefinition: TAxisDefinition;
        function TitleOf(ADimension: TAxisDimension; const AModeId: string;
            const APrefs: TAxisPreferences): string;
    protected
        procedure SetUp; override;
    published
        //  The reported defect.
        procedure ASelectedPeakDoesNotSpeakForAModelThatHoldsCurves;
        //  The order the automatic rule asks in.
        procedure TheModelOutranksTheData;
        procedure CurvesThatDisagreeSayNothing;
        procedure ACurveStatingNothingDoesNotVetoTheOthers;
        procedure TheDataOutranksTheSelectedType;
        procedure TheSelectedTypeSpeaksForAnEmptyModel;
        procedure WithNothingSaidTheCoordinateIsGeneral;
        procedure AModeFromAnAbsentModuleIsPassedOver;
        procedure AModeForTheOtherCoordinateIsPassedOver;
        procedure EachCoordinateIsDecidedOnItsOwn;
        //  The model's weaker preference.
        procedure AModelsFallbackAnswersWhenTheDataSaysNothing;
        procedure TheDataOutranksAModelsFallback;
        procedure AModelsStrongPreferenceOutranksItsFallback;
        //  A remembered choice.
        procedure AChoiceTheUserMadeIsHonoured;
        procedure AChoiceTheUserNeverMadeIsNot;
        procedure AChoiceOfAnUnregisteredModeFallsBack;
        procedure AChoiceForTheOtherCoordinateFallsBack;
        procedure SinThetaOverLambdaWithoutAWavelengthFallsBack;
        procedure TheOtherAnglesNeedNoWavelength;
        procedure ACustomAxisNeedsBothFormulas;
        procedure AutomaticRemembersAsAutomatic;
        //  Files written before the registry.
        procedure EveryFormerDisplayModeHasAnId;
        procedure AnUnknownFormerModeMeansAutomatic;
        procedure AnIdOutranksAFormerMode;
        procedure AFormerModeIsReadWhenThereIsNoId;
        procedure NothingRememberedIsAutomatic;
        //  What the axis is called.
        procedure ALogarithmicValueKeepsTheQuantitysName;
        procedure ALogarithmicAxisOverNothingIsTheGeneralName;
        procedure ACustomValueAxisIsTheUsersOwn;
        procedure ResolvingAutomaticNamesARegisteredMode;
        procedure DrawingAnUnregisteredModeIsABuildFaultNamedInWords;
        procedure AnUnregisteredModeCannotBeDrawn;
        procedure ALogOfAnUnnamedQuantityTakesTheGeneralName;
    end;

implementation

type
    { Modes that exist only to be walked wrongly. Never registered: the
      registry cannot take one back out, and a probe left in it would be a
      fault in every later test's walk. }
    TUnexplainedMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TMisexplainedMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    TUnnamedParameterMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    { Registered on purpose: a second claim on a framework id. }
    TImpostorPositionMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    TNamelessMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    TNowhereMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    TCaptionlessMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

class function TUnexplainedMode.Info: TAxisModeInfo;
begin
    Result.Id := 'test.unexplained';
    Result.Caption := 'Unexplained';
    Result.Topic := '';
    Result.Dimensions := [adArgument];
    Result.Parameter := apNone;
    Result.ParameterRequired := False;
end;

{$hints off}
class function TUnexplainedMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TNamedAxis.Create('Unexplained', '');
end;
{$hints on}

class function TMisexplainedMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.misexplained';
    Result.Topic := 'data/no-such-topic';
end;

class function TUnnamedParameterMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.unnamed-parameter';
    Result.Topic := ArgumentAxesTopic;
    Result.ParameterRequired := True;
end;

class function TImpostorPositionMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := PositionAxisModeId;
end;

class function TNamelessMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := '  ';
end;

class function TNowhereMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.nowhere';
    Result.Dimensions := [];
end;

class function TCaptionlessMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.captionless';
    Result.Topic := ArgumentAxesTopic;
    Result.Caption := '  ';
end;

function Resolves(const ATopic: string): boolean;
begin
    Result := RegisteredTopicResolves(ATopic);
end;

{ ---- the registry ---------------------------------------------------------- }

procedure TAxisModeRegistryTest.SetUp;
begin
    RegisterAllAxisModes;
    RegisterClientExplanations;
end;

procedure TAxisModeRegistryTest.AKnownIdResolvesToItsMode;
begin
    AssertTrue(FindAxisMode(PositionAxisModeId) = TPositionAxisMode);
    AssertTrue(FindAxisMode(TwoThetaAxisModeId) = TTwoThetaAxisMode);
end;

procedure TAxisModeRegistryTest.AnIdIsFoundWhateverItsCase;
begin
    //  A hand-edited settings file is not a reason to lose the choice.
    AssertTrue(FindAxisMode('LOGARITHMIC') = TLogarithmicAxisMode);
    AssertTrue(FindAxisMode('  logarithmic ') = TLogarithmicAxisMode);
end;

procedure TAxisModeRegistryTest.AnUnknownIdResolvesToNothing;
begin
    AssertNull(TObject(FindAxisMode('some.module.that-is-not-here')));
    AssertNull(TObject(FindAxisMode('')));
end;

procedure TAxisModeRegistryTest.TheSameModeRegisteredAgainIsANoOp;
var
    Before: longint;
begin
    Before := Length(RegisteredAxisModes);
    RegisterAllAxisModes;
    RegisterAxisMode(TLogarithmicAxisMode);
    AssertEquals(Before, Length(RegisteredAxisModes));
end;

procedure TAxisModeRegistryTest.TwoModesCannotClaimOneId;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterAxisMode(TImpostorPositionMode);
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue('names both claimants',
                (Pos('TPositionAxisMode', E.Message) > 0) and
                (Pos('TImpostorPositionMode', E.Message) > 0));
        end;
    end;
    AssertTrue(Raised);
end;

procedure TAxisModeRegistryTest.AModeWithNoClassIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterAxisMode(nil);
    except
        on EAxisModeRegistration do
            Raised := True;
    end;
    AssertTrue(Raised);
end;

procedure TAxisModeRegistryTest.AModeWithNoIdIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterAxisMode(TNamelessMode);
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue('says what could not happen', Pos('remember', E.Message) > 0);
        end;
    end;
    AssertTrue(Raised);
end;

procedure TAxisModeRegistryTest.AModeForNoCoordinateIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterAxisMode(TNowhereMode);
    except
        on EAxisModeRegistration do
            Raised := True;
    end;
    AssertTrue(Raised);
end;

procedure TAxisModeRegistryTest.EachCoordinateListsOnlyTheModesThatShowIt;
var
    Modes: TAxisModeClasses;
    i: longint;
    SawLog: boolean;
begin
    Modes := AxisModesFor(adValue);
    SawLog := False;
    for i := 0 to High(Modes) do
    begin
        AssertTrue(Modes[i].ClassName + ' shows the value',
            adValue in Modes[i].Info.Dimensions);
        if Modes[i] = TLogarithmicAxisMode then
            SawLog := True;
    end;
    AssertTrue('a logarithmic chart is a value mode', SawLog);
    AssertFalse('2*Theta is not', AxisModeShows(TwoThetaAxisModeId, adValue));
    AssertFalse('nor is Intensity an argument',
        AxisModeShows(IntensityAxisModeId, adArgument));
end;

procedure TAxisModeRegistryTest.TheFrameworksOwnModesHeadEveryMenu;
begin
    AssertTrue(AxisModesFor(adArgument)[0] = TPositionAxisMode);
    AssertTrue(AxisModesFor(adValue)[0] = TValueAxisMode);
end;

procedure TAxisModeRegistryTest.ACustomAxisIsCaptionedForItsCoordinate;
begin
    AssertEquals('the caption the argument''s entry always had',
        'Custom Position...', TCustomAxisMode.CaptionFor(adArgument));
    AssertEquals('Custom Value...', TCustomAxisMode.CaptionFor(adValue));
    AssertEquals('an ordinary mode has one caption', 'Logarithmic',
        TLogarithmicAxisMode.CaptionFor(adValue));
end;

procedure TAxisModeRegistryTest.EveryRegisteredModeIsComplete;
var
    Findings: TStringArray;
    i: longint;
begin
    Findings := AxisModeFindings(@Resolves);
    for i := 0 to High(Findings) do
        Fail(Findings[i]);
end;

procedure TAxisModeRegistryTest.EveryRegisteredModeBuildsAnAxisThatRoundTrips;
const
    //  Inside every registered axis's domain: a positive angle below 180
    //  degrees, a positive value for the logarithm.
    Raw = 37.25;
var
    Modes: TAxisModeClasses;
    Context: TAxisContext;
    Axis: TCoordinateAxis;
    D: TAxisDimension;
    i: longint;
begin
    Context.WaveLength := 1.54056;
    Context.Definition.Name := 'Custom';
    Context.Definition.UnitName := '';
    Context.Definition.Forward := 'x';
    Context.Definition.Inverse := 'x';
    Context.QuantityName := 'Quantity';
    Context.QuantityUnit := '';
    Modes := RegisteredAxisModes;
    for i := 0 to High(Modes) do
        for D := Low(TAxisDimension) to High(TAxisDimension) do
            if D in Modes[i].Info.Dimensions then
            begin
                Axis := Modes[i].CreateAxis(D, Context);
                try
                    AssertTrue(Modes[i].ClassName + ' names its axis',
                        Axis.DisplayName <> '');
                    //  The parameters grid shows ToDisplay and stores back
                    //  FromDisplay; if they are not inverses, looking at a
                    //  value moves it.
                    AssertEquals(Modes[i].ClassName + ' round-trips', Raw,
                        Axis.FromDisplay(Axis.ToDisplay(Raw)), 1e-9);
                finally
                    Axis.Free;
                end;
            end;
end;

procedure TAxisModeRegistryTest.AModeWithoutAnExplanationIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    SetLength(Modes, 1);
    Modes[0] := TUnexplainedMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Pos('no explanation topic', Findings[0]) > 0);
    AssertTrue('by name', Pos('TUnexplainedMode', Findings[0]) > 0);
end;

procedure TAxisModeRegistryTest.AModeNamingATopicNothingExplainsIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    SetLength(Modes, 1);
    Modes[0] := TMisexplainedMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Pos('data/no-such-topic', Findings[0]) > 0);
end;

procedure TAxisModeRegistryTest.AModeRequiringAnUnnamedParameterIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    SetLength(Modes, 1);
    Modes[0] := TUnnamedParameterMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Pos('without saying which', Findings[0]) > 0);
end;

function FindingsOf(AMode: TAxisModeClass): TStringArray;
var
    Modes: TAxisModeClasses;
begin
    SetLength(Modes, 1);
    Modes[0] := AMode;
    Result := AxisModeFindingsFor(Modes, @Resolves);
end;

procedure TAxisModeRegistryTest.AModeWithNoIdIsReportedByTheWalkToo;
var
    Findings: TStringArray;
begin
    //  Registration refuses it; a walk over a set that never went through
    //  registration must say so too.
    Findings := FindingsOf(TNamelessMode);
    AssertTrue(Length(Findings) > 0);
    AssertTrue(Pos('declares no id', Findings[0]) > 0);
end;

procedure TAxisModeRegistryTest.AModeForNoCoordinateIsReportedByTheWalkToo;
var
    Findings: TStringArray;
    i: longint;
    Found: boolean;
begin
    Findings := FindingsOf(TNowhereMode);
    Found := False;
    for i := 0 to High(Findings) do
        if Pos('shows no coordinate', Findings[i]) > 0 then
            Found := True;
    AssertTrue(Found);
end;

procedure TAxisModeRegistryTest.AModeWithABlankCaptionIsReported;
var
    Findings: TStringArray;
begin
    Findings := FindingsOf(TCaptionlessMode);
    AssertEquals(1, Length(Findings));
    AssertTrue('says what the user would see',
        Pos('menu entry would be blank', Findings[0]) > 0);
    AssertTrue('and for which axis', Pos('argument', Findings[0]) > 0);
end;

{ ---- the choice ------------------------------------------------------------ }

procedure TAxisChoiceTest.SetUp;
begin
    RegisterAllAxisModes;
end;

function TAxisChoiceTest.Prefs(const AModel: array of string;
    const AData, ASelected: string): TAxisPreferences;
var
    i: longint;
begin
    SetLength(Result.Model, Length(AModel));
    for i := 0 to High(AModel) do
        Result.Model[i] := AModel[i];
    Result.ModelFallback := nil;
    Result.Data := AData;
    Result.SelectedType := ASelected;
end;

function TAxisChoiceTest.NoDefinition: TAxisDefinition;
begin
    Result.Name := '';
    Result.UnitName := '';
    Result.Forward := '';
    Result.Inverse := '';
end;

function TAxisChoiceTest.TitleOf(ADimension: TAxisDimension;
    const AModeId: string; const APrefs: TAxisPreferences): string;
var
    Axis: TCoordinateAxis;
begin
    Axis := CreateAxisFor(ADimension, AModeId, APrefs, 0, NoDefinition);
    try
        Result := Axis.Title;
    finally
        Axis.Free;
    end;
end;

procedure TAxisChoiceTest.ASelectedPeakDoesNotSpeakForAModelThatHoldsCurves;
begin
    AssertEquals(PositionAxisModeId, AutomaticModeId(adArgument,
        Prefs([''], '', TwoThetaAxisModeId)));
end;

procedure TAxisChoiceTest.TheModelOutranksTheData;
begin
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs([TwoThetaAxisModeId], ThetaAxisModeId, '')));
end;

procedure TAxisChoiceTest.CurvesThatDisagreeSayNothing;
begin
    AssertEquals('the data speaks instead', ThetaAxisModeId,
        AutomaticModeId(adArgument,
            Prefs([TwoThetaAxisModeId, SinThetaOverLambdaAxisModeId],
                ThetaAxisModeId, '')));
end;

procedure TAxisChoiceTest.ACurveStatingNothingDoesNotVetoTheOthers;
begin
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs(['', TwoThetaAxisModeId, ''], ThetaAxisModeId, '')));
end;

procedure TAxisChoiceTest.TheDataOutranksTheSelectedType;
begin
    AssertEquals(ThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs([], ThetaAxisModeId, TwoThetaAxisModeId)));
end;

procedure TAxisChoiceTest.TheSelectedTypeSpeaksForAnEmptyModel;
begin
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs([], '', TwoThetaAxisModeId)));
end;

procedure TAxisChoiceTest.WithNothingSaidTheCoordinateIsGeneral;
begin
    AssertEquals(PositionAxisModeId, AutomaticModeId(adArgument, Prefs([], '', '')));
    AssertEquals(ValueAxisModeId, AutomaticModeId(adValue, Prefs([], '', '')));
end;

procedure TAxisChoiceTest.AModeFromAnAbsentModuleIsPassedOver;
begin
    //  By the model: the data answers instead.
    AssertEquals(ThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs(['vendor.absent'], ThetaAxisModeId, '')));
    //  By the data, over an empty model: the selected type answers instead.
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs([], 'vendor.absent-too', TwoThetaAxisModeId)));
end;

procedure TAxisChoiceTest.AModeForTheOtherCoordinateIsPassedOver;
begin
    //  A curve naming a value mode for its argument says nothing about it.
    AssertEquals(ThetaAxisModeId, AutomaticModeId(adArgument,
        Prefs([IntensityAxisModeId], ThetaAxisModeId, '')));
end;

procedure TAxisChoiceTest.EachCoordinateIsDecidedOnItsOwn;
var
    Argument, Value: TAxisPreferences;
begin
    //  A curve may know what its value is and not what its argument is - a
    //  wave pattern knows a price and not whether it stands on bars or dates -
    //  so the data answers one and the model the other.
    Argument := Prefs([''], ThetaAxisModeId, '');
    Value := Prefs([IntensityAxisModeId], ValueAxisModeId, '');
    AssertEquals(ThetaAxisModeId, AutomaticModeId(adArgument, Argument));
    AssertEquals(IntensityAxisModeId, AutomaticModeId(adValue, Value));
end;

procedure TAxisChoiceTest.AModelsFallbackAnswersWhenTheDataSaysNothing;
var
    P: TAxisPreferences;
begin
    //  A model whose curves assume an argument - a wave count assumes bars -
    //  over data that says nothing, with another type still selected: the
    //  assumption, not Position and not the selected type's.
    P := Prefs([''], '', TwoThetaAxisModeId);
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := ThetaAxisModeId;
    AssertEquals(ThetaAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.TheDataOutranksAModelsFallback;
var
    P: TAxisPreferences;
begin
    //  The same model over data that does say: the data.
    P := Prefs([''], TwoThetaAxisModeId, '');
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := ThetaAxisModeId;
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.AModelsStrongPreferenceOutranksItsFallback;
var
    P: TAxisPreferences;
begin
    P := Prefs([TwoThetaAxisModeId], '', '');
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := ThetaAxisModeId;
    AssertEquals(TwoThetaAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.AChoiceTheUserMadeIsHonoured;
begin
    AssertEquals(ThetaAxisModeId, UsableModeId(adArgument, ThetaAxisModeId,
        True, 0, NoDefinition));
    AssertEquals(LogarithmicAxisModeId, UsableModeId(adValue, 'Logarithmic',
        True, 0, NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceTheUserNeverMadeIsNot;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        TwoThetaAxisModeId, False, 1.54, NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceOfAnUnregisteredModeFallsBack;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        'vendor.absent', True, 0, NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceForTheOtherCoordinateFallsBack;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adValue,
        TwoThetaAxisModeId, True, 0, NoDefinition));
end;

procedure TAxisChoiceTest.SinThetaOverLambdaWithoutAWavelengthFallsBack;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        SinThetaOverLambdaAxisModeId, True, 0, NoDefinition));
    AssertEquals('kept with one', SinThetaOverLambdaAxisModeId,
        UsableModeId(adArgument, SinThetaOverLambdaAxisModeId, True, 1.54,
            NoDefinition));
end;

procedure TAxisChoiceTest.TheOtherAnglesNeedNoWavelength;
begin
    AssertEquals(ThetaAxisModeId, UsableModeId(adArgument, ThetaAxisModeId,
        True, 0, NoDefinition));
    AssertEquals(TwoThetaAxisModeId, UsableModeId(adArgument,
        TwoThetaAxisModeId, True, 0, NoDefinition));
end;

procedure TAxisChoiceTest.ACustomAxisNeedsBothFormulas;
var
    Definition: TAxisDefinition;
begin
    Definition := NoDefinition;
    Definition.Forward := 'ln(x)';
    AssertEquals('forward alone', AutomaticAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, 0, Definition));
    Definition.Forward := '';
    Definition.Inverse := 'exp(x)';
    AssertEquals('inverse alone', AutomaticAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, 0, Definition));
    Definition.Forward := 'ln(x)';
    AssertEquals('both', CustomAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, 0, Definition));
end;

procedure TAxisChoiceTest.AutomaticRemembersAsAutomatic;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        AutomaticAxisModeId, True, 0, NoDefinition));
end;

procedure TAxisChoiceTest.EveryFormerDisplayModeHasAnId;
begin
    //  The XCM_* integers a settings file or project written before the
    //  registry holds, each onto the mode it meant.
    AssertEquals(TwoThetaAxisModeId, LegacyArgumentModeId(0));
    AssertEquals(ThetaAxisModeId, LegacyArgumentModeId(1));
    AssertEquals(SinThetaOverLambdaAxisModeId, LegacyArgumentModeId(2));
    AssertEquals(PositionAxisModeId, LegacyArgumentModeId(3));
    AssertEquals(CustomAxisModeId, LegacyArgumentModeId(4));
    AssertEquals('"from the curve type" is the automatic entry now',
        AutomaticAxisModeId, LegacyArgumentModeId(5));
end;

procedure TAxisChoiceTest.AnUnknownFormerModeMeansAutomatic;
begin
    AssertEquals(AutomaticAxisModeId, LegacyArgumentModeId(99));
    AssertEquals(AutomaticAxisModeId, LegacyArgumentModeId(-1));
end;

procedure TAxisChoiceTest.AnIdOutranksAFormerMode;
begin
    AssertEquals(LogarithmicAxisModeId,
        StoredModeId(LogarithmicAxisModeId, True, 0));
end;

procedure TAxisChoiceTest.AFormerModeIsReadWhenThereIsNoId;
begin
    //  0 was 2*Theta - a real mode, not "nothing", which is why the flag says
    //  whether there is one.
    AssertEquals(TwoThetaAxisModeId, StoredModeId('', True, 0));
    AssertEquals(PositionAxisModeId, StoredModeId('  ', True, 3));
end;

procedure TAxisChoiceTest.NothingRememberedIsAutomatic;
begin
    AssertEquals(AutomaticAxisModeId, StoredModeId('', False, 0));
end;

procedure TAxisChoiceTest.ALogarithmicValueKeepsTheQuantitysName;
begin
    AssertEquals('Intensity, log scale', TitleOf(adValue, LogarithmicAxisModeId,
        Prefs([], IntensityAxisModeId, '')));
end;

procedure TAxisChoiceTest.ALogarithmicAxisOverNothingIsTheGeneralName;
begin
    AssertEquals('Value, log scale', TitleOf(adValue, LogarithmicAxisModeId,
        Prefs([], '', '')));
    AssertEquals('Position, log scale', TitleOf(adArgument,
        LogarithmicAxisModeId, Prefs([], '', '')));
end;

procedure TAxisChoiceTest.ACustomValueAxisIsTheUsersOwn;
var
    Definition: TAxisDefinition;
    Axis: TCoordinateAxis;
begin
    Definition.Name := 'Counts';
    Definition.UnitName := 'k';
    Definition.Forward := 'x/1000';
    Definition.Inverse := 'x*1000';
    Axis := CreateAxisFor(adValue, CustomAxisModeId, Prefs([], '', ''), 0,
        Definition);
    try
        AssertEquals('Counts [k]', Axis.Title);
        AssertEquals(2.5, Axis.ToDisplay(2500), 1e-12);
    finally
        Axis.Free;
    end;
end;

procedure TAxisChoiceTest.ResolvingAutomaticNamesARegisteredMode;
begin
    AssertEquals(IntensityAxisModeId, ResolvedModeId(adValue,
        AutomaticAxisModeId, Prefs([IntensityAxisModeId], '', '')));
    AssertEquals('an empty choice is automatic too', ValueAxisModeId,
        ResolvedModeId(adValue, '', Prefs([], '', '')));
    AssertEquals('a concrete choice is itself', ThetaAxisModeId,
        ResolvedModeId(adArgument, ThetaAxisModeId, Prefs([], '', '')));
end;

procedure TAxisChoiceTest.DrawingAnUnregisteredModeIsABuildFaultNamedInWords;
var
    Raised: boolean;
begin
    //  Resolution never yields an unregistered mode, so reaching one means a
    //  build that did not register its own - said, not drawn as something else.
    Raised := False;
    try
        CreateAxisFor(adArgument, 'vendor.absent', Prefs([], '', ''), 0,
            NoDefinition).Free;
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue(Pos('vendor.absent', E.Message) > 0);
            AssertTrue(Pos('argument', E.Message) > 0);
        end;
    end;
    AssertTrue(Raised);
end;

procedure TAxisChoiceTest.AnUnregisteredModeCannotBeDrawn;
begin
    AssertFalse(ModeCanBeDrawn('vendor.absent', 1.54, NoDefinition));
    AssertTrue(ModeCanBeDrawn(LogarithmicAxisModeId, 0, NoDefinition));
end;

procedure TAxisChoiceTest.ALogOfAnUnnamedQuantityTakesTheGeneralName;
var
    Context: TAxisContext;
    Axis: TCoordinateAxis;
begin
    //  Built directly with no quantity - the automatic rule always names one,
    //  but a mode must not caption an axis ", log scale" with nothing before it.
    Context := Default(TAxisContext);
    Axis := TLogarithmicAxisMode.CreateAxis(adValue, Context);
    try
        AssertEquals('Value, log scale', Axis.Title);
    finally
        Axis.Free;
    end;
end;

initialization
    RegisterTest('unit', TAxisModeRegistryTest);
    RegisterTest('unit', TAxisChoiceTest);
end.
