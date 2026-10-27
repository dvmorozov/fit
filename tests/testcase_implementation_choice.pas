// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Several implementations of one abstraction, from any provider: the
native one first, the user's choice kept, and the native one running - saying
why - when the chosen one's provider is not there.)

The rule is decisions.md's "Implementations from any provider". The registry
half is unit tests over contracts named here alone ('test-...'), so they meet
no registration of the program's; the REST half chooses the Python engine
while the Python provider says it is absent, through the surface a user's
choice reaches.
}
unit testcase_implementation_choice;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson,
    implementation_registry, minimizer_registry, minimizer_registration,
    int_fit_service, machine_settings, explanation, explanation_registry,
    testcase_rest_api,
    //  The choice made as the window makes it: the client, over its own
    //  transport answered by the server's own router.
    fit_client, fit_rest_api, mock_fit_viewer, mock_loopback_fit_service;

type
    { What a provider's host answers when asked whether it is there. }
    TProviderAnswer = class(TObject)
    public
        Available: boolean;
        function IsAvailable: boolean;
    end;

    TImplementationRegistryTest = class(TTestCase)
    private
        FAnswer: TProviderAnswer;
        { A contract named for this test alone, with a native implementation
          'own' and an alternative 'alt' on the Python sidecar. }
        procedure RegisterNativeAndAlternative(const AContract: string);
        function RaisesOn(const AInfo: TImplementationInfo): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheFirstImplementationOfAContractMustBeNative;
        procedure AnImplementationRefusedSaysWhatItLacks;
        procedure TwoImplementationsCannotShareAnId;
        procedure NoChoiceIsTheDefaultAndNoFallback;
        procedure AChoiceWhoseProviderIsThereRuns;
        procedure AChoiceWhoseProviderIsAbsentRunsTheNativeOneAndSaysWhy;
        procedure AProviderWhoseHostNeverSaidIsTakenToBeThere;
        procedure TheNativeProviderIsAlwaysThere;
        procedure AProviderRefusedSaysWhatItLacks;
        procedure AChoiceThisBuildDoesNotHaveRunsTheDefaultAndSaysWhy;
        procedure TheChoiceIsKeptInTheMachinesOwnSection;
        procedure WithOneImplementationNothingIsReadFromTheSettings;
        procedure TakingAContractOutLeavesItWithNone;
        procedure EveryEngineIsAnImplementationOfTheMinimizerContract;
        procedure AnEngineWhoseProviderIsAbsentResolvesToTheDefault;
        procedure EveryImplementationAndProviderExplainsItself;
        procedure TheExplanationsAreReachedThroughTheRegistry;
    end;

    { The choice made over REST, while the Python provider is absent. }
    TImplementationChoiceRestTest = class(TRestApiTestBase)
    private
        FAnswer: TProviderAnswer;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnEngineWhoseProviderIsAbsentFitsNativelyAndSaysWhy;
    end;

    { Fit > Minimizer, through the client the window assigns the choice to:
      what the server says about the choice reaches the window then, not only
      when a fit ends. }
    TImplementationChoiceClientTest = class(TTestCase)
    private
        FAnswer: TProviderAnswer;
        FApi: TFitRestApi;
        FSvc: TLoopbackFitService;
        FView: TMockFitViewer;
        FClient: TFitClient;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ChoosingAnEngineWhoseProviderIsAbsentTellsTheWindowWhy;
        procedure ChoosingAnEngineThatWillRunSaysNothing;
    end;

implementation

function TProviderAnswer.IsAvailable: boolean;
begin
    Result := Available;
end;

{ TImplementationRegistryTest }

procedure TImplementationRegistryTest.SetUp;
begin
    FAnswer := TProviderAnswer.Create;
end;

procedure TImplementationRegistryTest.TearDown;
begin
    //  THE PROVIDER IS THE PROGRAM'S: a probe left behind would make every
    //  later test's Python engine absent.
    SetProviderProbe(PythonSidecarProvider, nil);
    UnregisterContract('test-native-first');
    UnregisterContract('test-refusals');
    UnregisterContract('test-ids');
    UnregisterContract('test-choice');
    UnregisterContract('test-single');
    UnregisterContract('test-out');
    MachineSettings.SetStr(ImplementationsSection, 'test-choice', '');
    MachineSettings.SetStr(ImplementationsSection, 'test-single', '');
    FreeAndNil(FAnswer);
end;

function Implementation_(const AContract, AId, AProvider: string): TImplementationInfo;
begin
    Result := Default(TImplementationInfo);
    Result.Contract := AContract;
    Result.Id := AId;
    Result.Name := 'Test ' + AId;
    Result.Description := 'An implementation for this test alone.';
    Result.Provider := AProvider;
end;

procedure TImplementationRegistryTest.RegisterNativeAndAlternative(
    const AContract: string);
begin
    RegisterImplementation(Implementation_(AContract, 'own', NativeProvider));
    RegisterImplementation(Implementation_(AContract, 'alt',
        PythonSidecarProvider));
end;

function TImplementationRegistryTest.RaisesOn(
    const AInfo: TImplementationInfo): string;
begin
    Result := '';
    try
        RegisterImplementation(AInfo);
    except
        on E: EImplementationRegistration do
            Result := E.Message;
    end;
end;

procedure TImplementationRegistryTest.TheFirstImplementationOfAContractMustBeNative;
var
    Why: string;
begin
    //  NOTHING TO FALL BACK TO, otherwise: the default is the first, and a
    //  user without the optional provider would have no implementation at all.
    Why := RaisesOn(Implementation_('test-native-first', 'alt',
        PythonSidecarProvider));
    AssertTrue('refused: ' + Why, Pos('must be native', Why) > 0);
    AssertEquals('and nothing was registered', 0,
        Length(ImplementationsOf('test-native-first')));

    RegisterImplementation(Implementation_('test-native-first', 'own', ''));
    RegisterImplementation(Implementation_('test-native-first', 'alt',
        PythonSidecarProvider));
    AssertEquals('beside a native one it is welcome', 2,
        Length(ImplementationsOf('test-native-first')));
    AssertEquals('and the native one is the default', 'own',
        DefaultImplementationId('test-native-first'));
end;

procedure TImplementationRegistryTest.AnImplementationRefusedSaysWhatItLacks;
var
    Info: TImplementationInfo;
begin
    Info := Implementation_('test-refusals', 'x', '');
    Info.Description := '';
    AssertTrue('no description', Pos('description', RaisesOn(Info)) > 0);

    Info := Implementation_('test-refusals', 'x', '');
    Info.Name := '';
    AssertTrue('no name', Pos('no name', RaisesOn(Info)) > 0);

    Info := Implementation_('test-refusals', '', '');
    AssertTrue('no id', Pos('no id', RaisesOn(Info)) > 0);

    Info := Implementation_('', 'x', '');
    AssertTrue('no contract', Pos('no contract', RaisesOn(Info)) > 0);

    Info := Implementation_('test-refusals', 'x', 'nobody-declared-this');
    AssertTrue('a provider nobody declared',
        Pos('nobody-declared-this', RaisesOn(Info)) > 0);

    Info := Implementation_('provider', 'x', '');
    AssertTrue('the providers'' own segment is no contract',
        Pos('not a contract', RaisesOn(Info)) > 0);

    AssertEquals('none of them was registered', 0,
        Length(ImplementationsOf('test-refusals')));
end;

procedure TImplementationRegistryTest.TwoImplementationsCannotShareAnId;
begin
    //  One would be offered while the other ran.
    RegisterImplementation(Implementation_('test-ids', 'same', ''));
    AssertTrue(Pos('already taken',
        RaisesOn(Implementation_('test-ids', 'same', ''))) > 0);
end;

procedure TImplementationRegistryTest.NoChoiceIsTheDefaultAndNoFallback;
var
    Choice: TImplementationChoice;
begin
    RegisterNativeAndAlternative('test-choice');
    Choice := ResolveImplementation('test-choice', '');
    AssertEquals('own', Choice.Id);
    AssertFalse('nothing was chosen, so nothing fell back', Choice.FellBack);
    AssertEquals('', Choice.Why);
end;

procedure TImplementationRegistryTest.AChoiceWhoseProviderIsThereRuns;
var
    Choice: TImplementationChoice;
begin
    RegisterNativeAndAlternative('test-choice');
    FAnswer.Available := True;
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
    Choice := ResolveImplementation('test-choice', 'alt');
    AssertEquals('alt', Choice.Id);
    AssertFalse(Choice.FellBack);
end;

procedure TImplementationRegistryTest.AChoiceWhoseProviderIsAbsentRunsTheNativeOneAndSaysWhy;
var
    Choice: TImplementationChoice;
begin
    RegisterNativeAndAlternative('test-choice');
    FAnswer.Available := False;
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
    AssertFalse('the premise', ProviderIsAvailable(PythonSidecarProvider));
    Choice := ResolveImplementation('test-choice', 'alt');
    AssertEquals('the native one runs', 'own', Choice.Id);
    AssertTrue('in place of the one chosen', Choice.FellBack);
    AssertTrue('naming what runs: ' + Choice.Why, Pos('Test own', Choice.Why) > 0);
    AssertTrue('what was chosen: ' + Choice.Why, Pos('Test alt', Choice.Why) > 0);
    AssertTrue('and what is missing: ' + Choice.Why,
        Pos('Python sidecar', Choice.Why) > 0);
end;

procedure TImplementationRegistryTest.AProviderWhoseHostNeverSaidIsTakenToBeThere;
begin
    //  An in-process host hands the engine a URL itself and sets no probe: the
    //  one chosen is tried, and reports its own failure as it always did.
    RegisterNativeAndAlternative('test-choice');
    SetProviderProbe(PythonSidecarProvider, nil);
    AssertTrue(ProviderIsAvailable(PythonSidecarProvider));
    AssertEquals('alt', ResolveImplementation('test-choice', 'alt').Id);
end;

procedure TImplementationRegistryTest.TheNativeProviderIsAlwaysThere;
begin
    AssertTrue(ProviderIsAvailable(NativeProvider));
    AssertTrue('an unstated provider is the native one', ProviderIsAvailable(''));
    AssertFalse('one nobody declared is not', ProviderIsAvailable('no-such'));
end;

procedure TImplementationRegistryTest.AProviderRefusedSaysWhatItLacks;
var
    Info: TProviderInfo;
    Before: longint;
    Why: string;
begin
    Before := Length(RegisteredProviders);
    Info := Default(TProviderInfo);
    Info.Name := 'nameless';
    Why := '';
    try
        RegisterProvider(Info);
    except
        on E: EImplementationRegistration do
            Why := E.Message;
    end;
    AssertTrue('no id: ' + Why, Pos('no id', Why) > 0);

    Info.Id := 'test-provider-with-no-name';
    Info.Name := '';
    Why := '';
    try
        RegisterProvider(Info);
    except
        on E: EImplementationRegistration do
            Why := E.Message;
    end;
    AssertTrue('no name: ' + Why, Pos('no name', Why) > 0);
    AssertEquals('neither was declared', Before, Length(RegisteredProviders));

    //  EVERY HOST DECLARES THE FRAMEWORK'S OWN, so a second time is harmless.
    Info := Default(TProviderInfo);
    Info.Id := NativeProvider;
    Info.Name := 'again';
    RegisterProvider(Info);
    AssertEquals(Before, Length(RegisteredProviders));
    AssertTrue(FindProvider(NativeProvider, Info));
    AssertTrue('the first declaration stands', Info.Name <> 'again');

    Why := '';
    try
        SetProviderProbe('test-nobody-declared', @FAnswer.IsAvailable);
    except
        on E: EImplementationRegistration do
            Why := E.Message;
    end;
    AssertTrue('a probe for nothing: ' + Why,
        Pos('test-nobody-declared', Why) > 0);
end;

procedure TImplementationRegistryTest.AChoiceThisBuildDoesNotHaveRunsTheDefaultAndSaysWhy;
var
    Choice: TImplementationChoice;
begin
    //  Kept by a newer build, or one with a module this one lacks.
    RegisterNativeAndAlternative('test-choice');
    Choice := ResolveImplementation('test-choice', 'from-elsewhere');
    AssertEquals('own', Choice.Id);
    AssertTrue(Choice.FellBack);
    AssertTrue(Choice.Why, Pos('from-elsewhere', Choice.Why) > 0);
end;

procedure TImplementationRegistryTest.TheChoiceIsKeptInTheMachinesOwnSection;
begin
    RegisterNativeAndAlternative('test-choice');
    AssertEquals('none made yet', '', ChosenImplementation('test-choice'));
    ChooseImplementation('test-choice', 'alt');
    AssertEquals('kept as a key of its own section', 'alt',
        MachineSettings.Str('implementations', 'test-choice'));
    AssertEquals('alt', ChosenImplementation('test-choice'));
    FAnswer.Available := True;
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
    AssertEquals('and it is what runs', 'alt',
        ImplementationInForce('test-choice').Id);
    FAnswer.Available := False;
    AssertEquals('until its provider is gone', 'own',
        ImplementationInForce('test-choice').Id);
    AssertEquals('which leaves the choice as it was', 'alt',
        ChosenImplementation('test-choice'));
end;

procedure TImplementationRegistryTest.WithOneImplementationNothingIsReadFromTheSettings;
begin
    //  Asked from a fit's worker threads, so with nothing to choose between
    //  nothing is read - not even a choice naming something else.
    RegisterImplementation(Implementation_('test-single', 'own', ''));
    ChooseImplementation('test-single', 'from-elsewhere');
    AssertEquals('own', ImplementationInForce('test-single').Id);
    AssertFalse(ImplementationInForce('test-single').FellBack);
end;

procedure TImplementationRegistryTest.TakingAContractOutLeavesItWithNone;
begin
    RegisterNativeAndAlternative('test-out');
    UnregisterContract('test-out');
    AssertEquals(0, Length(ImplementationsOf('test-out')));
    AssertEquals('', DefaultImplementationId('test-out'));
    AssertEquals('nothing resolves', '', ResolveImplementation('test-out', 'alt').Id);
end;

procedure TImplementationRegistryTest.EveryEngineIsAnImplementationOfTheMinimizerContract;
var
    Engines: TMinimizerInfoArray;
    Info: TImplementationInfo;
    i: longint;
begin
    RegisterAllMinimizers;
    Engines := RegisteredMinimizers;
    AssertEquals('one each', Length(Engines),
        Length(ImplementationsOf(MinimizerContract)));
    for i := 0 to High(Engines) do
    begin
        AssertTrue(Engines[i].Name, FindImplementation(MinimizerContract,
            IntToStr(Engines[i].Kind), Info));
        AssertEquals('under its own name', Engines[i].Name, Info.Name);
    end;
    AssertEquals('and the default engine is the default implementation',
        IntToStr(DefaultMinimizerKind), DefaultImplementationId(MinimizerContract));
end;

procedure TImplementationRegistryTest.AnEngineWhoseProviderIsAbsentResolvesToTheDefault;
var
    R: TMinimizerResolution;
begin
    RegisterAllMinimizers;
    FAnswer.Available := False;
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
    R := ResolveMinimizer(MIN_KIND_PYTHON_LM);
    AssertEquals('the native engine runs', MIN_KIND_DHS, R.Kind);
    AssertTrue(R.FellBack);
    AssertTrue(R.Why, Pos('Python sidecar', R.Why) > 0);

    FAnswer.Available := True;
    R := ResolveMinimizer(MIN_KIND_PYTHON_LM);
    AssertEquals('with the sidecar there, the one chosen', MIN_KIND_PYTHON_LM,
        R.Kind);
    AssertFalse(R.FellBack);
    AssertEquals('a native engine is never replaced', MIN_KIND_DHS,
        ResolveMinimizer(MIN_KIND_DHS).Kind);
end;

procedure TImplementationRegistryTest.EveryImplementationAndProviderExplainsItself;
var
    Topics: TStringArray;
    E: TExplanation;
    Missing: string;
    i: longint;
begin
    //  SELF-ENFORCING: whatever registers next - an engine, a search, a
    //  provider - is walked here, and fails by name if its explanation is not
    //  worth showing.
    RegisterAllMinimizers;
    RegisterNativeAndAlternative('test-choice');
    Topics := ImplementationExplanationProvider.StaticTopics;
    AssertTrue('the framework''s providers are listed',
        Length(Topics) >= Length(RegisteredProviders));
    for i := 0 to High(Topics) do
    begin
        AssertTrue(Topics[i] + ' is answered',
            ImplementationExplanationProvider.Explain(Topics[i], E));
        AssertEquals('under its own topic', Topics[i], E.Topic);
        AssertTrue(Topics[i] + ': ' + Missing,
            ExplanationIsComplete(E, Missing));
    end;
end;

procedure TImplementationRegistryTest.TheExplanationsAreReachedThroughTheRegistry;
var
    E: TExplanation;
begin
    RegisterNativeAndAlternative('test-choice');
    AssertTrue(FindExplanation(ImplementationTopic('test-choice', 'alt'), E));
    AssertEquals('Test alt', E.Title);
    AssertEquals('it says what it needs', 1, Length(E.Limitations));
    AssertTrue(E.Limitations[0], Pos('Python sidecar', E.Limitations[0]) > 0);
    AssertTrue(FindExplanation(ProviderTopic(PythonSidecarProvider), E));
    AssertFalse('and nothing else', FindExplanation(
        ImplementationTopic('test-choice', 'nothing'), E));
end;

{ TImplementationChoiceRestTest }

procedure TImplementationChoiceRestTest.SetUp;
begin
    inherited SetUp;
    //  AS fit_server DOES at start: without the engines registered, nothing
    //  would know the Python engine runs on the sidecar.
    RegisterAllMinimizers;
    FAnswer := TProviderAnswer.Create;
    FAnswer.Available := False;
    //  AS fit_server SETS IT, asked of a sidecar that is not configured.
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
end;

procedure TImplementationChoiceRestTest.TearDown;
begin
    SetProviderProbe(PythonSidecarProvider, nil);
    FreeAndNil(FAnswer);
    inherited TearDown;
end;

procedure TImplementationChoiceRestTest.AnEngineWhoseProviderIsAbsentFitsNativelyAndSaysWhy;
var
    Id, Code: longint;
    R: TJSONObject;
    Native: TMinimizerInfo;
begin
    //  AN OPTIONAL PROVIDER ONLY ADDS ALTERNATIVES: chosen where it is absent,
    //  the native engine fits, and both the choice and the fit say why. The
    //  choice itself is the user's, and is kept.
    AssertTrue(FindMinimizer(MIN_KIND_DHS, Native));
    Id := FittableProblem;
    R := Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"minimizerKind":%d}', [MIN_KIND_PYTHON_LM]), Code);
    try
        AssertEquals('the choice is accepted', 200, Code);
        AssertEquals('and kept', MIN_KIND_PYTHON_LM, R.Get('minimizerKind', -1));
        AssertTrue('the reply says why it will not run: ' + R.AsJSON,
            Pos('Python sidecar', R.Get('note', '')) > 0);
    finally
        R.Free;
    end;

    R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
        '', Code);
    try
        AssertEquals('it fits rather than refusing: ' + R.AsJSON, 200, Code);
        AssertTrue('and says why on the native engine: ' + R.AsJSON,
            Pos(Native.Name, R.Get('message', '')) > 0);
    finally
        R.Free;
    end;

    R := Call('GET', Format('/problems/%d/progress', [Id]), '', Code);
    try
        AssertEquals('the run names the engine that ran', Native.Name,
            R.Get('engineName', ''));
    finally
        R.Free;
    end;
end;

{ TImplementationChoiceClientTest }

procedure TImplementationChoiceClientTest.SetUp;
begin
    RegisterAllMinimizers;
    FAnswer := TProviderAnswer.Create;
    FAnswer.Available := False;
    //  AS fit_server SETS IT, asked of a sidecar that is not configured.
    SetProviderProbe(PythonSidecarProvider, @FAnswer.IsAvailable);
    FApi := TFitRestApi.Create;
    FSvc := TLoopbackFitService.Create(FApi);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TImplementationChoiceClientTest.TearDown;
begin
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
    FreeAndNil(FApi);
    SetProviderProbe(PythonSidecarProvider, nil);
    FreeAndNil(FAnswer);
end;

procedure TImplementationChoiceClientTest.ChoosingAnEngineWhoseProviderIsAbsentTellsTheWindowWhy;
begin
    //  THE GESTURE: Fit > Minimizer > the Python engine, which the window
    //  hands to the client as it is (TFormMain.ApplyMinimizerKind). The
    //  server answers the choice with why the native engine will run instead;
    //  a client that drops that reply leaves a ticked entry the user has no
    //  reason to doubt until a fit ends.
    FClient.MinimizerKind := MIN_KIND_PYTHON_LM;
    AssertEquals('the choice is kept', MIN_KIND_PYTHON_LM, FClient.MinimizerKind);
    AssertTrue('the window is told why: ' + FView.Hints.Text,
        Pos('Python sidecar', FView.Hints.Text) > 0);
end;

procedure TImplementationChoiceClientTest.ChoosingAnEngineThatWillRunSaysNothing;
begin
    //  The note is about a fallback, and only a fallback: with the provider
    //  there - or a native engine chosen - nothing is said over the hint a
    //  user was reading.
    FAnswer.Available := True;
    FClient.MinimizerKind := MIN_KIND_PYTHON_LM;
    FClient.MinimizerKind := MIN_KIND_DHS;
    AssertEquals('nothing to say: ' + FView.Hints.Text, 0, FView.Hints.Count);
end;

initialization
    RegisterTest('unit', TImplementationRegistryTest);
    RegisterTest('unit', TImplementationChoiceClientTest);
    //  It fits.
    RegisterTest('integration', TImplementationChoiceRestTest);
end.
