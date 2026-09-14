// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The settings model and its XML serialization, with no file involved.)

WHY THIS EXISTS BESIDE testcase_settings_persistence. That suite writes a temp
file and is classified `integration` for exactly that reason - so none of
app_settings.pas was reachable from the measured half, and the unit sat at 0 of
105 lines while being the thing every user preference passes through.

NO FILE IS NEEDED. TXMLConfig keeps its document in memory and only touches the
disk in Flush, which returns immediately when Filename is empty (Laz2_XMLCfg
Flush: it writes only when Modified and the filename is not empty). Constructing
it with Create(nil) and calling Clear gives a live CONFIG document, and the writer
and the reader both work against that document rather than against a path. What
the file-based suite still adds is that the bytes survive a real write and read;
what is asserted here is what the components mean.

THE DEFAULTS ARE THE INTERESTING PART. Several of them exist specifically so that
a settings file written before a feature existed is distinguishable from a
deliberate choice - ViewModeChosenByUser and an empty SelectedCurveType both say
"the user never picked" - and a changed default silently moves every existing
user onto a different model. Those are asserted by value here.
}
unit testcase_settings_model;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, Laz_XMLCfg, Laz_XMLStreaming,
    app_settings, mscr_specimen_list, persistent_curve_parameters;

type
    TSettingsModelTest = class(TTestCase)
    private
        FCfg: TXMLConfig;
        procedure FindSettings(Reader: TReader; const AClassName: string;
            var ComponentClass: TComponentClass);
        procedure FindNothing(Reader: TReader; const AClassName: string;
            var ComponentClass: TComponentClass);
        { Writes ASaved into the in-memory document, reads it back into a fresh
          instance and returns it. The caller owns the result. }
        function RoundTrip(ASaved: Settings_v1): Settings_v1;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What a first run gets.
        procedure AFreshSettingsObjectHasNoCurveTypes;
        procedure TheAxisDefaultsToTheOneTheCurveDefines;
        procedure NothingHasBeenChosenByTheUserYet;
        procedure TheDefaultObjectiveIsTheCorrectedRFactor;
        procedure TheDefaultMinimizerIsTheOriginalSimplex;
        procedure FittingIsInProcessUntilAServerIsGiven;
        procedure CountingDataIsWeightedByDefault;
        procedure TheReservedFieldIsNotZero;
        procedure NoProjectIsRememberedOnAFirstRun;
        procedure AndNoRecentListEither;

        //  What survives being written and read again.
        procedure EveryPersistedFieldSurvivesTheRoundTrip;
        procedure AnExplicitAxisChoiceIsDistinguishableFromNone;
        procedure CurveTypesAreNotPersisted;

        //  How the serialization fails, which matters because a corrupt or
        //  foreign settings file reaches exactly these paths.
        procedure AnUnknownRootClassIsRefusedByName;
        procedure AnIncompatibleRootComponentIsRefused;
        procedure ReadingCanCreateTheRootComponentItself;

        //  The curve-type component the settings file carries.
        procedure ACurveTypeStartsWithAnEmptyParameterSet;
        procedure ACurveTypesParametersAreItsOwnCollection;
        procedure ReplacingTheParameterSetReplacesTheCollection;
    end;

implementation

const
    { Not XCM_CURVE: a value the constructor would never produce, so a field that
      was never written is visible as such. }
    SomeOtherAxis = 3;

procedure TSettingsModelTest.SetUp;
begin
    //  Create(nil) leaves the document nil - the filename setter is what builds
    //  it - so Clear is what makes this usable without a path.
    FCfg := TXMLConfig.Create(nil);
    FCfg.Clear;
end;

procedure TSettingsModelTest.TearDown;
begin
    FCfg.Free;
    FCfg := nil;
end;

procedure TSettingsModelTest.FindSettings(Reader: TReader;
    const AClassName: string; var ComponentClass: TComponentClass);
begin
    if AClassName = Settings_v1.ClassName then
        ComponentClass := Settings_v1
    else if AClassName = Curve_type.ClassName then
        ComponentClass := Curve_type;
end;

procedure TSettingsModelTest.FindNothing(Reader: TReader;
    const AClassName: string; var ComponentClass: TComponentClass);
begin
    //  Deliberately recognises nothing: this is a settings file naming a class
    //  this build does not have.
    ComponentClass := nil;
end;

function TSettingsModelTest.RoundTrip(ASaved: Settings_v1): Settings_v1;
var
    Loaded: TComponent;
begin
    WriteComponentToXMLConfig(FCfg, 'Component', ASaved);
    Loaded := Settings_v1.Create(nil);
    ReadComponentFromXMLConfig(FCfg, 'Component', Loaded, @FindSettings, nil);
    Result := Settings_v1(Loaded);
end;

{ ---- a first run ----------------------------------------------------------- }

procedure TSettingsModelTest.AFreshSettingsObjectHasNoCurveTypes;
var
    S: Settings_v1;
begin
    S := Settings_v1.Create(nil);
    try
        AssertTrue('the list exists', Assigned(S.Curve_types));
        AssertEquals('and is empty', 0, S.Curve_types.Count);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.TheAxisDefaultsToTheOneTheCurveDefines;
var
    S: Settings_v1;
begin
    //  XCM_CURVE, not the legacy hard-coded 2*Theta. A user who has never opened
    //  the axis menu sees the axis their model is written in.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('the curve-defined axis', XCM_CURVE, S.ViewMode);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.NothingHasBeenChosenByTheUserYet;
var
    S: Settings_v1;
begin
    //  BOTH of these mean "never chosen", and both are what an older settings
    //  file says. Defaulting either one to a real value would move every
    //  existing user onto a choice they never made.
    S := Settings_v1.Create(nil);
    try
        AssertFalse('the axis was not chosen', S.ViewModeChosenByUser);
        AssertEquals('and no curve type was', '', S.SelectedCurveType);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.TheDefaultObjectiveIsTheCorrectedRFactor;
var
    S: Settings_v1;
begin
    //  LOSS_KIND_RFACTOR = 0, so a settings file written before the objective
    //  was selectable loads onto the corrected R-factor rather than onto an
    //  absent objective.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('the corrected R-factor', 0, S.LossKind);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.TheDefaultMinimizerIsTheOriginalSimplex;
var
    S: Settings_v1;
begin
    S := Settings_v1.Create(nil);
    try
        AssertEquals('MIN_KIND_DHS', 0, S.MinimizerKind);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.FittingIsInProcessUntilAServerIsGiven;
var
    S: Settings_v1;
begin
    //  Empty, not localhost: the application must be usable with no server
    //  deployed, and a default URL would make a first run depend on one.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('no server', '', S.ServerUrl);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.CountingDataIsWeightedByDefault;
var
    S: Settings_v1;
begin
    //  Poisson, because the data are counts. Defaulting to no weighting would
    //  change the answer of every fit rather than only its speed.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('poisson', 'poisson', S.Weighting);
    finally
        S.Free;
    end;
end;

procedure TSettingsModelTest.TheReservedFieldIsNotZero;
var
    S: Settings_v1;
begin
    //  -1. The field exists only so that reading an old file does not raise, and
    //  its default is asserted here so that it is not quietly repurposed as a
    //  flag whose off state is zero.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('reserved', -1, S.Reserved);
    finally
        S.Free;
    end;
end;

{ ---- the round trip -------------------------------------------------------- }

procedure TSettingsModelTest.NoProjectIsRememberedOnAFirstRun;
var
    Fresh: Settings_v1;
begin
    //  EMPTY MEANS NONE, which is also what every settings file written before
    //  this field existed says - so an upgrade opens an empty window rather
    //  than reporting a project that is not there.
    Fresh := Settings_v1.Create(nil);
    try
        AssertEquals('', Fresh.LastProjectFile);
    finally
        Fresh.Free;
    end;
end;

procedure TSettingsModelTest.EveryPersistedFieldSurvivesTheRoundTrip;
var
    Saved, Loaded: Settings_v1;
begin
    //  ONE test over every field rather than one test each: what breaks a
    //  published property is a change to the property list, which takes all of
    //  them out together. Each value is distinct from every default, so a field
    //  that was not written is visible as a default rather than as a match.
    Saved := Settings_v1.Create(nil);
    Loaded := nil;
    try
        Saved.ViewMode := SomeOtherAxis;
        Saved.ViewModeChosenByUser := True;
        Saved.MinimizerKind := 2;
        Saved.LossKind := 1;
        Saved.SelectedCurveType := '{0B0E4B7C-0000-0000-0000-000000000001}';
        Saved.ServerUrl := 'http://compute.example:8080';
        Saved.Weighting := 'none';
        Saved.CustomAxisName := 'Energy';
        Saved.CustomAxisUnit := 'eV';
        Saved.CustomAxisForward := '1239.84/x';
        Saved.CustomAxisInverse := '1239.84/x';
        Saved.LastProjectFile := 'projects/run7.fitproj';
        Saved.RecentProjects := 'a.fitproj|b.fitproj';
        Saved.Reserved := 7;

        Loaded := RoundTrip(Saved);

        AssertEquals('the axis', SomeOtherAxis, Loaded.ViewMode);
        AssertTrue('the axis was chosen deliberately',
            Loaded.ViewModeChosenByUser);
        AssertEquals('the minimizer', 2, Loaded.MinimizerKind);
        AssertEquals('the objective', 1, Loaded.LossKind);
        AssertEquals('the curve type',
            '{0B0E4B7C-0000-0000-0000-000000000001}', Loaded.SelectedCurveType);
        AssertEquals('the server', 'http://compute.example:8080',
            Loaded.ServerUrl);
        AssertEquals('the weighting', 'none', Loaded.Weighting);
        AssertEquals('the axis name', 'Energy', Loaded.CustomAxisName);
        AssertEquals('the axis unit', 'eV', Loaded.CustomAxisUnit);
        AssertEquals('the forward formula', '1239.84/x',
            Loaded.CustomAxisForward);
        AssertEquals('the inverse formula', '1239.84/x',
            Loaded.CustomAxisInverse);
        AssertEquals('the project offered at start-up',
            'projects/run7.fitproj', Loaded.LastProjectFile);
        //  THE WHOLE LIST IN ONE FIELD, separator and all: what File > Open
        //  Recent offers has to survive a restart or the menu is empty every
        //  time the application is started.
        AssertEquals('the recent list', 'a.fitproj|b.fitproj',
            Loaded.RecentProjects);
        AssertEquals('the reserved field', 7, Loaded.Reserved);
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

procedure TSettingsModelTest.AnExplicitAxisChoiceIsDistinguishableFromNone;
var
    Saved, Loaded: Settings_v1;
begin
    //  The pair that carries the whole point of the flag: the SAME ViewMode,
    //  written once as a deliberate pick and once as an untouched default, has to
    //  come back different. Persisting the mode alone cannot express this, which
    //  is why the flag exists.
    Saved := Settings_v1.Create(nil);
    Loaded := nil;
    try
        Saved.ViewMode := 0;
        Saved.ViewModeChosenByUser := True;
        Loaded := RoundTrip(Saved);
        AssertEquals('the mode came back', 0, Loaded.ViewMode);
        AssertTrue('and it is known to be a choice', Loaded.ViewModeChosenByUser);
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

procedure TSettingsModelTest.CurveTypesAreNotPersisted;
var
    Saved, Loaded: Settings_v1;
begin
    //  DefineProperties is empty on purpose - the filer cannot carry a
    //  TComponentList through an XML stream - so the curve-type list is rebuilt
    //  by the application rather than read back. Asserted so that nobody reads
    //  the empty list on the far side as data loss. The two filer callbacks that
    //  a commented-out line in app_settings once named have been deleted; a
    //  commented-out line is not a caller, and they could never have run.
    Saved := Settings_v1.Create(nil);
    Loaded := nil;
    try
        Saved.Curve_types.Add(Curve_type.Create(nil));
        AssertEquals('one was added before writing', 1, Saved.Curve_types.Count);
        Loaded := RoundTrip(Saved);
        AssertEquals('and none came back', 0, Loaded.Curve_types.Count);
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

{ ---- how it refuses -------------------------------------------------------- }

procedure TSettingsModelTest.AnUnknownRootClassIsRefusedByName;
var
    Saved: Settings_v1;
    Loaded: TComponent;
    Raised: boolean;
begin
    //  A settings file naming a class this build does not have. It must raise
    //  something identifiable rather than return an empty object, which the
    //  caller would then write back over the user's real settings.
    Saved := Settings_v1.Create(nil);
    Loaded := nil;
    Raised := False;
    try
        WriteComponentToXMLConfig(FCfg, 'Component', Saved);
        try
            ReadComponentFromXMLConfig(FCfg, 'Component', Loaded,
                @FindNothing, nil);
        except
            on E: EClassNotFound do
            begin
                Raised := True;
                AssertTrue('the message names the class it could not find',
                    Pos(Settings_v1.ClassName, E.Message) > 0);
            end;
        end;
        AssertTrue('it refused', Raised);
        AssertFalse('and created nothing', Assigned(Loaded));
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

procedure TSettingsModelTest.AnIncompatibleRootComponentIsRefused;
var
    Saved: Settings_v1;
    Loaded: TComponent;
    Raised: boolean;
begin
    //  The document holds settings and the caller offered a curve type to read
    //  them into. Reading regardless would fill one class's fields from another
    //  class's stream.
    Saved := Settings_v1.Create(nil);
    Loaded := Curve_type.Create(nil);
    Raised := False;
    try
        WriteComponentToXMLConfig(FCfg, 'Component', Saved);
        try
            ReadComponentFromXMLConfig(FCfg, 'Component', Loaded,
                @FindSettings, nil);
        except
            on E: EComponentError do
                Raised := True;
        end;
        AssertTrue('it refused', Raised);
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

procedure TSettingsModelTest.ReadingCanCreateTheRootComponentItself;
var
    Saved: Settings_v1;
    Loaded: TComponent;
begin
    //  Passed nil, the reader constructs the class the document names. This is
    //  the branch the application does not use - it always passes an existing
    //  object - and it is the one that has to get the two-step NewInstance and
    //  Create right, so it is worth pinning.
    Saved := Settings_v1.Create(nil);
    Loaded := nil;
    try
        Saved.ServerUrl := 'http://made-by-the-reader';
        WriteComponentToXMLConfig(FCfg, 'Component', Saved);
        ReadComponentFromXMLConfig(FCfg, 'Component', Loaded,
            @FindSettings, nil);
        AssertTrue('something was created', Assigned(Loaded));
        AssertTrue('of the class the document named', Loaded is Settings_v1);
        AssertEquals('and it was read into', 'http://made-by-the-reader',
            Settings_v1(Loaded).ServerUrl);
    finally
        Loaded.Free;
        Saved.Free;
    end;
end;

{ ---- the curve-type component ---------------------------------------------- }

procedure TSettingsModelTest.ACurveTypeStartsWithAnEmptyParameterSet;
var
    C: Curve_type;
begin
    C := Curve_type.Create(nil);
    try
        AssertTrue('the parameter set exists', Assigned(C.Parameters));
        AssertTrue('and its collection does too', Assigned(C.Params));
        //  ONE, not none. Curve_parameters seeds a placeholder named '?' because
        //  the filer writes an empty collection incorrectly - a workaround its
        //  own comment records. Asserted here so that the placeholder is a known
        //  fact rather than something a reader mistakes for a real parameter.
        AssertEquals('the placeholder the filer needs', 1, C.Params.Count);
        AssertEquals('and no expression', '', C.Expression);
    finally
        C.Free;
    end;
end;

procedure TSettingsModelTest.ACurveTypesParametersAreItsOwnCollection;
var
    C: Curve_type;
begin
    //  Params is a view onto Parameters.Params, not a copy. A copy would make the
    //  settings file describe a parameter set the application then ignores.
    C := Curve_type.Create(nil);
    try
        C.Parameters.Params.Add;
        AssertEquals('the addition is visible through Params',
            2, C.Params.Count);
        AssertTrue('because it is the same collection',
            C.Params = C.Parameters.Params);
    finally
        C.Free;
    end;
end;

procedure TSettingsModelTest.ReplacingTheParameterSetReplacesTheCollection;
var
    C: Curve_type;
    Fresh: Curve_parameters;
begin
    //  The setter frees what it replaces, which is what makes assigning a loaded
    //  parameter set over a default one safe rather than a leak.
    C := Curve_type.Create(nil);
    try
        C.Parameters.Params.Add;
        Fresh := Curve_parameters.Create(nil);
        C.Parameters := Fresh;
        AssertTrue('the new set is in place', C.Parameters = Fresh);
        //  Back to one - the fresh set's own placeholder - rather than the two
        //  the replaced set had. Reading a different count here would mean the
        //  old collection is still the one being read.
        AssertEquals('and the old collection went with the old set',
            1, C.Params.Count);
    finally
        C.Free;
    end;
end;

procedure TSettingsModelTest.AndNoRecentListEither;
var
    S: Settings_v1;
begin
    //  A first run has nothing to offer, and every settings file written before
    //  this existed says the same thing.
    S := Settings_v1.Create(nil);
    try
        AssertEquals('', S.RecentProjects);
    finally
        S.Free;
    end;
end;

initialization
    //  A unit test: the XML document never leaves memory, so nothing here needs a
    //  file, a server or a widget set.
    RegisterTest('unit', TSettingsModelTest);
end.
