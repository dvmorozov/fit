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
    test_axis_modes,
    explanation_registry, client_explanations, guide_data,
    fit_progress_explanations;

type
    TAxisModeRegistryTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        { THE FRAMEWORK'S OWN MODES ARE NO FIELD'S: a field's coordinates are
          its module's, registered from its front door, so a product without
          it offers none of them and asks for no number they read. }
        procedure TheCoreRegistersNoFieldsModeAndReadsNoModulesNumber;
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
        procedure AModeUnexplainedForOneOfItsCoordinatesIsReported;
        //  A number a module's mode reads.
        procedure AModeReadingAnUnnamedNumberIsReported;
        procedure AModeReadingANumberNoModuleDescribesIsReported;
        procedure ANumberIsDescribedByOneModuleOnly;
        procedure ANumberWithNoCaptionIsRefused;

        //  What the axis menus list, in what order.
        procedure TheMenuOffersAutomaticFirst;
        procedure ThenEveryModeThatShowsTheCoordinate;
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
        procedure AModeThatNeedsANumberFallsBackWithoutIt;
        procedure ModesThatOnlyReadANumberNeedNone;
        procedure ACustomAxisNeedsBothFormulas;
        procedure AutomaticRemembersAsAutomatic;
        //  Files written before the registry.
        procedure EveryFormerDisplayModeHasAnId;
        procedure AnUnknownFormerModeMeansAutomatic;
        procedure AnIdOutranksAFormerMode;
        procedure AFormerModeIsReadWhenThereIsNoId;
        procedure NothingRememberedIsAutomatic;
        procedure AModulesFormerDisplayModeMeansWhatTheModuleSays;
        procedure TheFrameworksOwnFormerDisplayModesAreNoModulesToClaim;
        procedure AFormerDisplayModeIsClaimedForOneModeOnly;
        //  What the axis is called.
        procedure ALogarithmicValueKeepsTheQuantitysName;
        procedure ALogarithmicAxisOverNothingIsTheGeneralName;
        procedure ACustomValueAxisIsTheUsersOwn;
        procedure ResolvingAutomaticNamesARegisteredMode;
        procedure DrawingAnUnregisteredModeIsABuildFaultNamedInWords;
        procedure AnUnregisteredModeCannotBeDrawn;
        procedure ALogOfAnUnnamedQuantityTakesTheGeneralName;
    end;

    { THE R-FACTOR OF A RUNNING FIT IS A THIRD THING AN AXIS SHOWS. It was
      drawn as log10 with its label written into the progress model, and the
      line above it printed it one way only; now it is chosen like either
      coordinate of the data, from the same registry, so a module's own way of
      showing a figure of merit needs no framework edit. }
    TRFactorAxisTest = class(TTestCase)
    private
        function TitleOf(const AModeId: string): string;
    protected
        procedure SetUp; override;
    published
        procedure UnlessChosenTheRFactorIsOnALogarithmicScale;
        procedure ItCanBeDrawnLinearly;
        procedure OrOnTheUsersOwnFormula;
        procedure NoAxisOfTheDataIsOfferedForIt;
        procedure ItsMenuEntriesSayWhatTheyDoToTheRFactor;
        procedure ItsEntriesExplainThemselvesInItsOwnTerms;
        procedure AnUnregisteredModeNamesTheRFactorInItsComplaint;
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

    { Reads a number, and does not say which. }
    TUnnamedNumberMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

    { Reads a number no module describes. }
    TUndescribedNumberMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
    end;

const
    { A number described only by this file's tests. }
    TestNumber = 'test.axis-number';

{ The numbers a mode may read: this one, at AValue. }
function NumberAt(const AName: string; AValue: double): TAxisValues;
begin
    Result := nil;
    PutAxisValue(Result, AName, AValue);
end;

{ The scale the test modes read, at AValue. }
function WL(AValue: double): TAxisValues;
begin
    Result := TestScale(AValue);
end;

function TestNumberInfo: TAxisParameterInfo;
begin
    Result.Name := TestNumber;
    Result.Caption := 'Test number';
    Result.Example := '2.5';
end;

class function TUnnamedNumberMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.unnamed-number';
    Result.Topic := ArgumentAxesTopic;
    Result.Parameter := apValue;
    Result.ParameterName := '';
end;

class function TUndescribedNumberMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.undescribed-number';
    Result.Topic := ArgumentAxesTopic;
    Result.Parameter := apValue;
    Result.ParameterName := 'vendor.nobody-describes-this';
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

type
    { Explained for the argument, and for the R-factor under a topic nothing
      registers. }
    THalfExplainedMode = class(TUnexplainedMode)
    public
        class function Info: TAxisModeInfo; override;
        class function TopicFor(ADimension: TAxisDimension): string; override;
    end;

class function THalfExplainedMode.Info: TAxisModeInfo;
begin
    Result := inherited Info;
    Result.Id := 'test.half-explained';
    Result.Topic := ArgumentAxesTopic;
    Result.Dimensions := [adArgument, adLoss];
end;

class function THalfExplainedMode.TopicFor(ADimension: TAxisDimension): string;
begin
    if ADimension = adLoss then
        Result := 'fit-progress/no-such-topic'
    else
        Result := inherited TopicFor(ADimension);
end;

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

procedure TAxisModeRegistryTest.TheCoreRegistersNoFieldsModeAndReadsNoModulesNumber;
var
    Modes: TAxisModeClasses;
    i: longint;
begin
    Modes := CoreAxisModes;
    AssertTrue('the core has its own modes', Length(Modes) >= 4);
    for i := 0 to High(Modes) do
    begin
        AssertTrue(Modes[i].Info.Id + ' reads no number a module keeps',
            Modes[i].Info.Parameter <> apValue);
        AssertTrue(Modes[i].Info.Id + ' is no test stand-in for a field''s mode',
            Pos('test.', Modes[i].Info.Id) <> 1);
    end;
end;

procedure TAxisModeRegistryTest.SetUp;
begin
    RegisterAllAxisModes;
    //  A module's modes, as its front door registers them: the test
    //  stand-ins, explained by the framework's own chapters.
    RegisterTestAxisModes;
    RegisterClientExplanations;
end;

procedure TAxisModeRegistryTest.AKnownIdResolvesToItsMode;
begin
    AssertTrue(FindAxisMode(PositionAxisModeId) = TPositionAxisMode);
    AssertTrue(FindAxisMode(TestPlainAxisModeId) = TTestPlainAxisMode);
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
    //  A module's modes, as its front door registers them.
    RegisterTestAxisModes;
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
    AssertFalse('the plain test argument is not', AxisModeShows(TestPlainAxisModeId, adValue));
    AssertFalse('nor are the test counts an argument',
        AxisModeShows(TestCountsAxisModeId, adArgument));
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
    //  degrees (which a module's angle needs), a positive value for the
    //  logarithm.
    Raw = 37.25;
var
    Modes: TAxisModeClasses;
    Context: TAxisContext;
    Axis: TCoordinateAxis;
    D: TAxisDimension;
    Parameter: TAxisParameterInfo;
    i: longint;
begin
    //  Every number any registered mode reads, so a mode that cannot be
    //  drawn without one - whichever module registered it - is drawn.
    Context.Values := nil;
    for Parameter in RegisteredAxisParameters do
        PutAxisValue(Context.Values, Parameter.Name, 1.54056);
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

procedure TAxisModeRegistryTest.AModeReadingAnUnnamedNumberIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    SetLength(Modes, 1);
    Modes[0] := TUnnamedNumberMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Findings[0], Pos('without naming it', Findings[0]) > 0);
end;

procedure TAxisModeRegistryTest.AModeReadingANumberNoModuleDescribesIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    SetLength(Modes, 1);
    Modes[0] := TUndescribedNumberMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Findings[0],
        Pos('"vendor.nobody-describes-this"', Findings[0]) > 0);
    AssertTrue(Findings[0], Pos('no module describes', Findings[0]) > 0);
end;

procedure TAxisModeRegistryTest.ANumberIsDescribedByOneModuleOnly;
var
    Other: TAxisParameterInfo;
    Found: TAxisParameterInfo;
    Raised: boolean;
begin
    RegisterAxisParameter(TestNumberInfo);
    //  A front door called twice.
    RegisterAxisParameter(TestNumberInfo);
    AssertTrue(FindAxisParameter(TestNumber, Found));
    AssertEquals('Test number', Found.Caption);
    AssertTrue('found whatever its case',
        FindAxisParameter(UpperCase(TestNumber), Found));
    //  A second module's description of the same name would have each find
    //  the other's number.
    Other := TestNumberInfo;
    Other.Caption := 'Another number';
    Raised := False;
    try
        RegisterAxisParameter(Other);
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue(E.Message, Pos('described twice', E.Message) > 0);
        end;
    end;
    AssertTrue('a second description is refused', Raised);
    AssertFalse('an undescribed name is not found',
        FindAxisParameter('vendor.nobody-describes-this', Found));
end;

procedure TAxisModeRegistryTest.ANumberWithNoCaptionIsRefused;
var
    Info: TAxisParameterInfo;
    Raised: boolean;
begin
    Info := TestNumberInfo;
    Info.Name := 'test.captionless-number';
    Info.Caption := ' ';
    Raised := False;
    try
        RegisterAxisParameter(Info);
    except
        on E: EAxisModeRegistration do
            Raised := True;
    end;
    AssertTrue('the window could not say what it asks for', Raised);
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

procedure TAxisModeRegistryTest.AModeUnexplainedForOneOfItsCoordinatesIsReported;
var
    Modes: TAxisModeClasses;
    Findings: TStringArray;
begin
    //  A mode explains each coordinate it shows, since one entry may mean a
    //  different thing on each: Linear over the R-factor is not the data's
    //  value axis.
    SetLength(Modes, 1);
    Modes[0] := THalfExplainedMode;
    Findings := AxisModeFindingsFor(Modes, @Resolves);
    AssertEquals(1, Length(Findings));
    AssertTrue(Findings[0], Pos('no-such-topic', Findings[0]) > 0);
    AssertTrue('for which axis: ' + Findings[0], Pos('R-factor', Findings[0]) > 0);
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
    //  A module's modes, as its front door registers them.
    RegisterTestAxisModes;
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
    Axis := CreateAxisFor(ADimension, AModeId, APrefs, nil, NoDefinition);
    try
        Result := Axis.Title;
    finally
        Axis.Free;
    end;
end;

procedure TAxisChoiceTest.ASelectedPeakDoesNotSpeakForAModelThatHoldsCurves;
begin
    AssertEquals(PositionAxisModeId, AutomaticModeId(adArgument,
        Prefs([''], '', TestPlainAxisModeId)));
end;

procedure TAxisChoiceTest.TheModelOutranksTheData;
begin
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument,
        Prefs([TestPlainAxisModeId], TestHalfAxisModeId, '')));
end;

procedure TAxisChoiceTest.CurvesThatDisagreeSayNothing;
begin
    AssertEquals('the data speaks instead', TestHalfAxisModeId,
        AutomaticModeId(adArgument,
            Prefs([TestPlainAxisModeId, TestScaledAxisModeId],
                TestHalfAxisModeId, '')));
end;

procedure TAxisChoiceTest.ACurveStatingNothingDoesNotVetoTheOthers;
begin
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument,
        Prefs(['', TestPlainAxisModeId, ''], TestHalfAxisModeId, '')));
end;

procedure TAxisChoiceTest.TheDataOutranksTheSelectedType;
begin
    AssertEquals(TestHalfAxisModeId, AutomaticModeId(adArgument,
        Prefs([], TestHalfAxisModeId, TestPlainAxisModeId)));
end;

procedure TAxisChoiceTest.TheSelectedTypeSpeaksForAnEmptyModel;
begin
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument,
        Prefs([], '', TestPlainAxisModeId)));
end;

procedure TAxisChoiceTest.WithNothingSaidTheCoordinateIsGeneral;
begin
    AssertEquals(PositionAxisModeId, AutomaticModeId(adArgument, Prefs([], '', '')));
    AssertEquals(ValueAxisModeId, AutomaticModeId(adValue, Prefs([], '', '')));
end;

procedure TAxisChoiceTest.AModeFromAnAbsentModuleIsPassedOver;
begin
    //  By the model: the data answers instead.
    AssertEquals(TestHalfAxisModeId, AutomaticModeId(adArgument,
        Prefs(['vendor.absent'], TestHalfAxisModeId, '')));
    //  By the data, over an empty model: the selected type answers instead.
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument,
        Prefs([], 'vendor.absent-too', TestPlainAxisModeId)));
end;

procedure TAxisChoiceTest.AModeForTheOtherCoordinateIsPassedOver;
begin
    //  A curve naming a value mode for its argument says nothing about it.
    AssertEquals(TestHalfAxisModeId, AutomaticModeId(adArgument,
        Prefs([TestCountsAxisModeId], TestHalfAxisModeId, '')));
end;

procedure TAxisChoiceTest.EachCoordinateIsDecidedOnItsOwn;
var
    Argument, Value: TAxisPreferences;
begin
    //  A curve may know what its value is and not what its argument is - a
    //  wave pattern knows a price and not whether it stands on bars or dates -
    //  so the data answers one and the model the other.
    Argument := Prefs([''], TestHalfAxisModeId, '');
    Value := Prefs([TestCountsAxisModeId], ValueAxisModeId, '');
    AssertEquals(TestHalfAxisModeId, AutomaticModeId(adArgument, Argument));
    AssertEquals(TestCountsAxisModeId, AutomaticModeId(adValue, Value));
end;

procedure TAxisChoiceTest.AModelsFallbackAnswersWhenTheDataSaysNothing;
var
    P: TAxisPreferences;
begin
    //  A model whose curves assume an argument - a wave count assumes bars -
    //  over data that says nothing, with another type still selected: the
    //  assumption, not Position and not the selected type's.
    P := Prefs([''], '', TestPlainAxisModeId);
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := TestHalfAxisModeId;
    AssertEquals(TestHalfAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.TheDataOutranksAModelsFallback;
var
    P: TAxisPreferences;
begin
    //  The same model over data that does say: the data.
    P := Prefs([''], TestPlainAxisModeId, '');
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := TestHalfAxisModeId;
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.AModelsStrongPreferenceOutranksItsFallback;
var
    P: TAxisPreferences;
begin
    P := Prefs([TestPlainAxisModeId], '', '');
    SetLength(P.ModelFallback, 1);
    P.ModelFallback[0] := TestHalfAxisModeId;
    AssertEquals(TestPlainAxisModeId, AutomaticModeId(adArgument, P));
end;

procedure TAxisChoiceTest.AChoiceTheUserMadeIsHonoured;
begin
    AssertEquals(TestHalfAxisModeId, UsableModeId(adArgument, TestHalfAxisModeId,
        True, nil, NoDefinition));
    AssertEquals(LogarithmicAxisModeId, UsableModeId(adValue, 'Logarithmic',
        True, nil, NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceTheUserNeverMadeIsNot;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        TestPlainAxisModeId, False, WL(1.54), NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceOfAnUnregisteredModeFallsBack;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        'vendor.absent', True, nil, NoDefinition));
end;

procedure TAxisChoiceTest.AChoiceForTheOtherCoordinateFallsBack;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adValue,
        TestPlainAxisModeId, True, nil, NoDefinition));
end;

procedure TAxisChoiceTest.AModeThatNeedsANumberFallsBackWithoutIt;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        TestScaledAxisModeId, True, nil, NoDefinition));
    AssertEquals('kept with one', TestScaledAxisModeId,
        UsableModeId(adArgument, TestScaledAxisModeId, True, WL(1.54),
            NoDefinition));
end;

procedure TAxisChoiceTest.ModesThatOnlyReadANumberNeedNone;
begin
    AssertEquals(TestHalfAxisModeId, UsableModeId(adArgument, TestHalfAxisModeId,
        True, nil, NoDefinition));
    AssertEquals(TestPlainAxisModeId, UsableModeId(adArgument,
        TestPlainAxisModeId, True, nil, NoDefinition));
end;

procedure TAxisChoiceTest.ACustomAxisNeedsBothFormulas;
var
    Definition: TAxisDefinition;
begin
    Definition := NoDefinition;
    Definition.Forward := 'ln(x)';
    AssertEquals('forward alone', AutomaticAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, nil, Definition));
    Definition.Forward := '';
    Definition.Inverse := 'exp(x)';
    AssertEquals('inverse alone', AutomaticAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, nil, Definition));
    Definition.Forward := 'ln(x)';
    AssertEquals('both', CustomAxisModeId, UsableModeId(adValue,
        CustomAxisModeId, True, nil, Definition));
end;

procedure TAxisChoiceTest.AutomaticRemembersAsAutomatic;
begin
    AssertEquals(AutomaticAxisModeId, UsableModeId(adArgument,
        AutomaticAxisModeId, True, nil, NoDefinition));
end;

procedure TAxisChoiceTest.EveryFormerDisplayModeHasAnId;
begin
    //  The XCM_* integers a settings file or project written before the
    //  registry holds, each onto the mode it meant.
    //  0, 1 and 2 are a module's to claim (RegisterLegacyArgumentMode); the
    //  framework answers for its own.
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
    AssertEquals(PositionAxisModeId, StoredModeId('  ', True, 3));
end;

procedure TAxisChoiceTest.NothingRememberedIsAutomatic;
begin
    AssertEquals(AutomaticAxisModeId, StoredModeId('', False, 0));
end;

procedure TAxisChoiceTest.AModulesFormerDisplayModeMeansWhatTheModuleSays;
begin
    //  An integer no module has claimed means nothing yet.
    AssertEquals(AutomaticAxisModeId, LegacyArgumentModeId(41));
    RegisterLegacyArgumentMode(41, LogarithmicAxisModeId);
    //  A front door called twice.
    RegisterLegacyArgumentMode(41, LogarithmicAxisModeId);
    AssertEquals(LogarithmicAxisModeId, LegacyArgumentModeId(41));
    AssertEquals('and through what a project remembers',
        LogarithmicAxisModeId, StoredModeId('', True, 41));
end;

procedure TAxisChoiceTest.TheFrameworksOwnFormerDisplayModesAreNoModulesToClaim;
var
    ViewMode: longint;
    Raised: boolean;
begin
    for ViewMode := 3 to 5 do
    begin
        Raised := False;
        try
            RegisterLegacyArgumentMode(ViewMode, LogarithmicAxisModeId);
        except
            on E: EAxisModeRegistration do
                Raised := True;
        end;
        AssertTrue(IntToStr(ViewMode) + ' is refused', Raised);
    end;
    AssertEquals(PositionAxisModeId, LegacyArgumentModeId(3));
    AssertEquals(CustomAxisModeId, LegacyArgumentModeId(4));
    AssertEquals(AutomaticAxisModeId, LegacyArgumentModeId(5));
end;

procedure TAxisChoiceTest.AFormerDisplayModeIsClaimedForOneModeOnly;
var
    Raised: boolean;
begin
    RegisterLegacyArgumentMode(42, LogarithmicAxisModeId);
    Raised := False;
    try
        RegisterLegacyArgumentMode(42, PositionAxisModeId);
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue(E.Message, Pos('42', E.Message) > 0);
        end;
    end;
    AssertTrue('a second meaning is refused', Raised);
    AssertEquals('the first stands', LogarithmicAxisModeId,
        LegacyArgumentModeId(42));
end;

procedure TAxisChoiceTest.ALogarithmicValueKeepsTheQuantitysName;
begin
    AssertEquals('Counts, log scale', TitleOf(adValue, LogarithmicAxisModeId,
        Prefs([], TestCountsAxisModeId, '')));
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
    Axis := CreateAxisFor(adValue, CustomAxisModeId, Prefs([], '', ''), nil,
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
    AssertEquals(TestCountsAxisModeId, ResolvedModeId(adValue,
        AutomaticAxisModeId, Prefs([TestCountsAxisModeId], '', '')));
    AssertEquals('an empty choice is automatic too', ValueAxisModeId,
        ResolvedModeId(adValue, '', Prefs([], '', '')));
    AssertEquals('a concrete choice is itself', TestHalfAxisModeId,
        ResolvedModeId(adArgument, TestHalfAxisModeId, Prefs([], '', '')));
end;

procedure TAxisChoiceTest.DrawingAnUnregisteredModeIsABuildFaultNamedInWords;
var
    Raised: boolean;
begin
    //  Resolution never yields an unregistered mode, so reaching one means a
    //  build that did not register its own - said, not drawn as something else.
    Raised := False;
    try
        CreateAxisFor(adArgument, 'vendor.absent', Prefs([], '', ''), nil,
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
    AssertFalse(ModeCanBeDrawn('vendor.absent', WL(1.54), NoDefinition));
    AssertTrue(ModeCanBeDrawn(LogarithmicAxisModeId, nil, NoDefinition));
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

{ ---- what the axis menus list ------------------------------------------------ }

procedure TAxisModeRegistryTest.TheMenuOffersAutomaticFirst;
var
    E: TAxisMenuEntries;
begin
    //  The default, and a visible entry rather than hidden behaviour, so the
    //  user can see why a caption changed - and hand the axis back to it.
    E := AxisMenuEntries(adArgument, ArgumentAxesTopic);
    AssertEquals(AutomaticAxisModeId, E[0].ModeId);
    AssertEquals('Automatic', E[0].Caption);
    AssertEquals(ArgumentAxesTopic, E[0].Topic);
end;

procedure TAxisModeRegistryTest.ThenEveryModeThatShowsTheCoordinate;
var
    E: TAxisMenuEntries;
    Modes: TAxisModeClasses;
    i: longint;
begin
    E := AxisMenuEntries(adValue, ValueAxesTopic);
    Modes := AxisModesFor(adValue);
    AssertEquals(Length(Modes) + 1, Length(E));
    for i := 0 to High(Modes) do
    begin
        AssertEquals(Modes[i].Info.Id, E[i + 1].ModeId);
        AssertEquals(Modes[i].CaptionFor(adValue), E[i + 1].Caption);
        AssertEquals(Modes[i].Info.Topic, E[i + 1].Topic);
    end;
end;

{ ---- the R-factor ----------------------------------------------------------- }

procedure TRFactorAxisTest.SetUp;
begin
    RegisterAllAxisModes;
    //  A module's modes, as its front door registers them: the test
    //  stand-ins, explained by the framework's own chapters.
    RegisterTestAxisModes;
    RegisterClientExplanations;
end;

function TRFactorAxisTest.TitleOf(const AModeId: string): string;
var
    Axis: TCoordinateAxis;
begin
    Axis := CreateAxisFor(adLoss, AModeId, Default(TAxisPreferences), nil,
        Default(TAxisDefinition));
    try
        Result := Axis.Title;
    finally
        Axis.Free;
    end;
end;

procedure TRFactorAxisTest.UnlessChosenTheRFactorIsOnALogarithmicScale;
begin
    //  What the progress chart has always drawn: a fit improves by factors of
    //  ten, and on a linear scale everything after the first moments is flat.
    AssertEquals(LogarithmicAxisModeId, ResolvedModeId(adLoss,
        AutomaticAxisModeId, Default(TAxisPreferences)));
    AssertEquals('R-factor, log scale', TitleOf(AutomaticAxisModeId));
end;

procedure TRFactorAxisTest.ItCanBeDrawnLinearly;
begin
    AssertTrue(AxisModeShows(ValueAxisModeId, adLoss));
    AssertEquals('R-factor', TitleOf(ValueAxisModeId));
end;

procedure TRFactorAxisTest.OrOnTheUsersOwnFormula;
var
    Definition: TAxisDefinition;
    Axis: TCoordinateAxis;
begin
    Definition.Name := 'log10 R';
    Definition.UnitName := '';
    Definition.Forward := 'log(x)';
    Definition.Inverse := '10^x';
    Axis := CreateAxisFor(adLoss, CustomAxisModeId, Default(TAxisPreferences),
        nil, Definition);
    try
        AssertEquals('log10 R', Axis.Title);
        AssertEquals(-3.0, Axis.ToDisplay(0.001), 1e-12);
    finally
        Axis.Free;
    end;
end;

procedure TRFactorAxisTest.NoAxisOfTheDataIsOfferedForIt;
begin
    AssertFalse('a module''s argument is not', AxisModeShows(TestPlainAxisModeId, adLoss));
    AssertFalse('a module''s value is not', AxisModeShows(TestCountsAxisModeId, adLoss));
    AssertFalse('a position is not', AxisModeShows(PositionAxisModeId, adLoss));
    AssertTrue('the framework''s own lead its menu',
        AxisModesFor(adLoss)[0] = TValueAxisMode);
end;

procedure TRFactorAxisTest.ItsMenuEntriesSayWhatTheyDoToTheRFactor;
begin
    AssertEquals('Linear', TValueAxisMode.CaptionFor(adLoss));
    AssertEquals('Logarithmic', TLogarithmicAxisMode.CaptionFor(adLoss));
    AssertEquals('Custom R-factor...', TCustomAxisMode.CaptionFor(adLoss));
end;

procedure TRFactorAxisTest.ItsEntriesExplainThemselvesInItsOwnTerms;
var
    E: TAxisMenuEntries;
    i: longint;
begin
    //  "Linear" over the R-factor is not the value axes of the data, and its
    //  explanation must not be theirs.
    E := AxisMenuEntries(adLoss, RFactorScaleTopic);
    AssertEquals('Automatic heads it', RFactorScaleTopic, E[0].Topic);
    for i := 0 to High(E) do
    begin
        AssertTrue(E[i].Caption + ' explains itself',
            RegisteredTopicResolves(E[i].Topic));
        AssertTrue(E[i].Caption + ' is not explained as the data''s value',
            E[i].Topic <> ValueAxesTopic);
    end;
end;

procedure TRFactorAxisTest.AnUnregisteredModeNamesTheRFactorInItsComplaint;
var
    Raised: boolean;
begin
    Raised := False;
    try
        CreateAxisFor(adLoss, 'vendor.absent', Default(TAxisPreferences), nil,
            Default(TAxisDefinition)).Free;
    except
        on E: EAxisModeRegistration do
        begin
            Raised := True;
            AssertTrue(E.Message, Pos('R-factor', E.Message) > 0);
        end;
    end;
    AssertTrue(Raised);
end;

initialization
    RegisterTest('unit', TRFactorAxisTest);
    RegisterTest('unit', TAxisModeRegistryTest);
    RegisterTest('unit', TAxisChoiceTest);
end.
