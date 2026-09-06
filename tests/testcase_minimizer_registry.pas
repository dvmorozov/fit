// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What each fitting engine declares, and what the engine derives from it.)

These are the four questions that used to be four comparisons against an integer
constant, in four files that did not mention each other. Each is asserted here
against the DECLARATION rather than against the id, so an engine added later is
covered by the same rules with no edit.

What this cannot check, for the same reason testcase_curve_type_registration
says so: this binary links every engine, so it cannot tell whether another binary
registered them. The guard for that is the start-up call, in the process that is
actually running.
}
unit testcase_minimizer_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, int_fit_backend, minimizer_registry, minimizer_registration;

type
    TMinimizerRegistryTest = class(TTestCase)
    published
        procedure TheNativeEngineIsTheDefault;
        procedure TheNativeEngineNeedsNoFormulaAndNoSidecar;
        procedure TheFormulaEngineDeclaresWhatItNeeds;
        procedure OnlyAWeightableEngineOffersWeighting;
        procedure CurveScalingBelongsToTheNativeEngineAlone;
        procedure AnUnknownKindIsFittedNatively;
        procedure RegisteringTwiceIsHarmless;
        procedure TwoEnginesCannotShareAKind;
        procedure TheFormulaEngineWithoutASidecarBuildsNothing;
        procedure AServerUrlSendsTheNativeFitToTheServer;
        procedure TheGuardFallbackStaysInThisProcess;

        //  Restoring a persisted choice.
        procedure AKnownKindIsRestoredAsItself;
        procedure AnUnknownKindFallsBackToTheDefault;
        procedure TheFallbackIsTheFirstRegisteredEngine;
        procedure TheFallbackIsItselfAKnownKind;
        procedure RestoringIsIdempotent;

        //  Registered with something missing - see the group in the body.
        procedure AnEngineWithNoNameIsRefused;
        procedure AnEngineWithNoWayToBuildItsBackendIsRefused;
        procedure EachRefusalNamesWhichFieldIsMissing;
    end;

implementation

function DummyBackend(const AContext: TBackendContext): IFitBackend;
begin
    Result := nil;
end;

procedure TMinimizerRegistryTest.TheNativeEngineIsTheDefault;
begin
    RegisterAllMinimizers;
    //  Registration order is menu order and the fallback, so the engine that
    //  needs nothing installed must come first.
    AssertEquals('the native engine is what a fit falls back to',
        MIN_KIND_DHS, DefaultMinimizerKind);
end;

procedure TMinimizerRegistryTest.TheNativeEngineNeedsNoFormulaAndNoSidecar;
begin
    RegisterAllMinimizers;
    //  This is what makes the application usable with zero Python (D4), and what
    //  lets it fit curve types that have no closed form at all.
    AssertFalse('the native engine fits curves that have no formula',
        MinimizerNeedsFormula(MIN_KIND_DHS));
    AssertFalse('and needs no sidecar',
        MinimizerNeedsPythonSidecar(MIN_KIND_DHS));
end;

procedure TMinimizerRegistryTest.TheFormulaEngineDeclaresWhatItNeeds;
begin
    RegisterAllMinimizers;
    //  Both facts drive real behaviour: the first greys the choice out for a
    //  formula-less curve type and makes the server fall back; the second starts
    //  the sidecar before the fit.
    AssertTrue('lmfit fits by evaluating an expression',
        MinimizerNeedsFormula(MIN_KIND_PYTHON_LM));
    AssertTrue('and cannot run without the sidecar',
        MinimizerNeedsPythonSidecar(MIN_KIND_PYTHON_LM));
end;

procedure TMinimizerRegistryTest.OnlyAWeightableEngineOffersWeighting;
begin
    RegisterAllMinimizers;
    //  The native engine always fits unweighted, so showing the weighting menu
    //  under it would be a control that silently does nothing.
    AssertFalse('the native engine cannot be weighted',
        MinimizerSupportsWeighting(MIN_KIND_DHS));
    AssertTrue('lmfit can', MinimizerSupportsWeighting(MIN_KIND_PYTHON_LM));
end;

procedure TMinimizerRegistryTest.CurveScalingBelongsToTheNativeEngineAlone;
begin
    RegisterAllMinimizers;
    //  Scaling is "fit the shape, then scale the amplitude to the observed
    //  integral". An out-of-process engine fits the amplitude itself, so scaling
    //  afterwards would rescale an already-fitted value.
    AssertTrue('the native engine may scale curves',
        MinimizerSupportsCurveScaling(MIN_KIND_DHS));
    AssertFalse('lmfit must not',
        MinimizerSupportsCurveScaling(MIN_KIND_PYTHON_LM));
end;

procedure TMinimizerRegistryTest.AnUnknownKindIsFittedNatively;
const
    NotInstalled = 9999;
begin
    RegisterAllMinimizers;
    //  A settings file written by a newer build may name an engine this one does
    //  not have. It must still fit, natively - and every answer below is the one
    //  that describes what will actually run.
    AssertFalse('an unknown engine needs no formula here',
        MinimizerNeedsFormula(NotInstalled));
    AssertFalse('and no sidecar', MinimizerNeedsPythonSidecar(NotInstalled));
    AssertFalse('and offers no weighting',
        MinimizerSupportsWeighting(NotInstalled));
    AssertTrue('and DOES allow curve scaling, because the native engine is what ' +
        'will run it', MinimizerSupportsCurveScaling(NotInstalled));
    AssertFalse('and is not reported as installed',
        IsKnownMinimizer(NotInstalled));
end;

procedure TMinimizerRegistryTest.RegisteringTwiceIsHarmless;
var
    Before: longint;
begin
    RegisterAllMinimizers;
    Before := MinimizerCount;
    //  Called from the client, the compute server AND every fit, so it must be
    //  idempotent rather than tripping the duplicate-kind rule.
    RegisterAllMinimizers;
    AssertEquals('registering again must not add engines',
        Before, MinimizerCount);
end;

procedure TMinimizerRegistryTest.TwoEnginesCannotShareAKind;
var
    Info: TMinimizerInfo;
    Raised: boolean;
begin
    RegisterAllMinimizers;
    Info := Default(TMinimizerInfo);
    Info.Kind := MIN_KIND_DHS;
    Info.Name := 'Impostor';
    Info.CreateBackend := @DummyBackend;
    Raised := False;
    try
        RegisterMinimizer(Info);
    except
        on E: EMinimizerRegistration do
            Raised := True;
    end;
    //  Otherwise the menu would offer one engine while another actually ran.
    AssertTrue('a second claim on a kind must be refused', Raised);
end;

procedure TMinimizerRegistryTest.TheFormulaEngineWithoutASidecarBuildsNothing;
var
    Info: TMinimizerInfo;
    Context: TBackendContext;
begin
    RegisterAllMinimizers;
    AssertTrue('lmfit must be registered',
        FindMinimizer(MIN_KIND_PYTHON_LM, Info));
    Context := Default(TBackendContext);
    //  No sidecar URL. nil means "cannot run here", and the caller then takes
    //  the native fallback - which is why the application still fits with no
    //  Python installed.
    AssertTrue('no sidecar means no backend, not a failure',
        Info.CreateBackend(Context) = nil);
end;

procedure TMinimizerRegistryTest.AServerUrlSendsTheNativeFitToTheServer;
var
    Info: TMinimizerInfo;
    Context: TBackendContext;
    Backend: IFitBackend;
begin
    RegisterAllMinimizers;
    AssertTrue('the native engine must be registered',
        FindMinimizer(MIN_KIND_DHS, Info));

    Context := Default(TBackendContext);
    Backend := Info.CreateBackend(Context);
    AssertTrue('with no server it fits in this process', Assigned(Backend));
    AssertEquals('Native (Downhill Simplex)', Backend.Name);

    Context.ServerUrl := 'http://127.0.0.1:1';
    Backend := Info.CreateBackend(Context);
    //  The same ALGORITHM somewhere else: a server URL is a transport choice,
    //  not a different engine, which is why it lives in this factory rather than
    //  being registered as an engine of its own.
    AssertTrue('with a server URL it is sent there', Assigned(Backend));
    AssertTrue('and that is the compute-server backend, naming the URL',
        Pos('127.0.0.1:1', Backend.Name) > 0);
end;

{ The distinction that a fit depends on, and that a plausible simplification
  destroys: "the default engine, wherever it is configured" is NOT the same as
  "the native engine, here".

  When a model cannot go to a formula-based backend at all - a curve type with no
  closed form - the fit must stay in this process. A configured compute server is
  itself a formula-based backend, so falling back to it would send the model to
  exactly what the guard exists to avoid, and the symptom would be a connection
  error rather than a visibly wrong decision. }
procedure TMinimizerRegistryTest.TheGuardFallbackStaysInThisProcess;
var
    Context: TBackendContext;
    Backend: IFitBackend;
begin
    RegisterAllMinimizers;

    Context := Default(TBackendContext);
    Context.ServerUrl := 'http://127.0.0.1:1';

    //  The ordinary fallback honours the configured server...
    Backend := DefaultFitBackend(Context);
    AssertTrue('the default engine is used wherever it is configured',
        Pos('127.0.0.1:1', Backend.Name) > 0);

    //  ...the guard's does not, whatever is configured.
    Backend := NativeInProcessBackend;
    AssertEquals('the guard fallback runs here, not on a remote server',
        'Native (Downhill Simplex)', Backend.Name);
end;


{ ---- restoring a persisted choice ------------------------------------------ }

procedure TMinimizerRegistryTest.AKnownKindIsRestoredAsItself;
var
    All: TMinimizerInfoArray;
    i: longint;
begin
    //  Every engine this build has, not the two it happened to be written with:
    //  the window used to name its one alternative and send everything else to
    //  the native engine, so a third engine would have been unselectable across
    //  a restart - chosen, saved, and silently replaced on the next start.
    All := RegisteredMinimizers;
    AssertTrue('something is registered', Length(All) > 0);
    for i := 0 to High(All) do
        AssertEquals('kind ' + IntToStr(All[i].Kind) + ' survives a restart',
            All[i].Kind, MinimizerKindOrDefault(All[i].Kind));
end;

procedure TMinimizerRegistryTest.AnUnknownKindFallsBackToTheDefault;
begin
    //  A settings file from a newer build, or one with a plug-in this build
    //  lacks. The answer has to be a fit rather than a refusal.
    AssertFalse('the premise', IsKnownMinimizer(9999));
    AssertTrue('something usable came back',
        IsKnownMinimizer(MinimizerKindOrDefault(9999)));
    AssertTrue('and a negative id too',
        IsKnownMinimizer(MinimizerKindOrDefault(-1)));
end;

procedure TMinimizerRegistryTest.TheFallbackIsTheFirstRegisteredEngine;
var
    All: TMinimizerInfoArray;
begin
    //  Registration order is the order the menu shows and the default engine
    //  registers first, so "the first registered" is the always-available
    //  native one without this unit having to name it.
    All := RegisteredMinimizers;
    AssertEquals('the first registered engine', All[0].Kind,
        MinimizerKindOrDefault(9999));
end;

procedure TMinimizerRegistryTest.TheFallbackIsItselfAKnownKind;
begin
    //  Otherwise the fallback would need a fallback.
    AssertTrue('the default is registered',
        IsKnownMinimizer(MinimizerKindOrDefault(9999)));
end;

procedure TMinimizerRegistryTest.RestoringIsIdempotent;
begin
    //  The value goes through this on the way out of the settings file and is
    //  written back on the way in; a second pass that moved it would change the
    //  user's choice on every restart.
    AssertEquals('an unknown kind settles',
        MinimizerKindOrDefault(9999),
        MinimizerKindOrDefault(MinimizerKindOrDefault(9999)));
    AssertEquals('and a known one does not move',
        MIN_KIND_DHS, MinimizerKindOrDefault(MinimizerKindOrDefault(MIN_KIND_DHS)));
end;

{ ------------------- registered with something missing ---------------------- }

{ THE REFUSALS FOR AN INCOMPLETE REGISTRATION - the ones whoever adds an engine,
  a loader, a builder or a module meets on their first attempt, and their only
  feedback. The duplicate-claim refusals were already covered here; these were
  not.

  THEY FIRE AT LINK TIME, before any window exists, so the only reader is the
  developer who added the thing. That is why each has to name WHICH field is
  missing rather than say "invalid": the person reading is looking at a record
  with several fields and no idea which one is at fault. }

procedure TMinimizerRegistryTest.AnEngineWithNoNameIsRefused;
var
    Info: TMinimizerInfo;
    Raised: boolean;
begin
    //  NOTHING COULD OFFER IT. The name is what the menu shows, what the
    //  settings store and what a REST caller sends, so a nameless engine is
    //  registered and unreachable - in the count and in no list.
    Info := Default(TMinimizerInfo);
    Info.Kind := 9001;
    Info.CreateBackend := @DummyBackend;
    Raised := False;
    try
        RegisterMinimizer(Info);
    except
        on E: EMinimizerRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TMinimizerRegistryTest.AnEngineWithNoWayToBuildItsBackendIsRefused;
var
    Info: TMinimizerInfo;
    Raised: boolean;
begin
    //  WORSE THAN NO NAME, because it IS offered: the user selects it, the fit
    //  starts, and the call that would build its backend goes through nil -
    //  during a fit, on whatever thread the engine chose.
    Info := Default(TMinimizerInfo);
    Info.Kind := 9002;
    Info.Name := 'engine-with-no-backend';
    Info.CreateBackend := nil;
    Raised := False;
    try
        RegisterMinimizer(Info);
    except
        on E: EMinimizerRegistration do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
end;

procedure TMinimizerRegistryTest.EachRefusalNamesWhichFieldIsMissing;
var
    Info: TMinimizerInfo;
    NoName, NoBackend: string;
begin
    NoName := '';
    Info := Default(TMinimizerInfo);
    Info.Kind := 9003;
    Info.CreateBackend := @DummyBackend;
    try
        RegisterMinimizer(Info);
    except
        on E: EMinimizerRegistration do
            NoName := E.Message;
    end;

    NoBackend := '';
    Info := Default(TMinimizerInfo);
    Info.Kind := 9004;
    Info.Name := 'has-a-name';
    try
        RegisterMinimizer(Info);
    except
        on E: EMinimizerRegistration do
            NoBackend := E.Message;
    end;

    AssertTrue('the nameless one says so: ' + NoName,
        Pos('no name', NoName) > 0);
    AssertTrue('the other names the engine: ' + NoBackend,
        Pos('has-a-name', NoBackend) > 0);
    AssertTrue('and says what it lacks: ' + NoBackend,
        Pos('backend', NoBackend) > 0);
    AssertTrue('they are different messages', NoName <> NoBackend);
end;

initialization
    RegisterTest('unit', TMinimizerRegistryTest);
end.
