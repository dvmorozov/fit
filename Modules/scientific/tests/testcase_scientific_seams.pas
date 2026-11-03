// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's seams, each as the framework or the client
reaches it: where the formula type finds its windows, the server session that
keeps the wavelength, the background arithmetic's registry and the
saved-formula record.)
}
unit testcase_scientific_seams;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_user_curve_dialogs,
    int_expression_parser, user_curve_dialog_registry,
    configurable_user_points_set, mock_user_curve_dialogs, int_app_module,
    scientific_app_module, background_algorithms, background_search,
    points_set, saved_formula, persistent_curve_parameters, MyExceptions,
    implementation_registry, machine_settings, fit_service, title_points_set,
    int_fit_service;

type
    TScientificSeamsTest = class(TTestCase)
    private
        FSession: IModuleSession;
        FSavedFormula: IUserCurveFormulaDlg;
        FSavedRoles: IUserCurveRolesDlg;
        FSavedParser: IExpressionParser;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The windows that define a formula.
        procedure WithoutTheWindowsTheFormulaTypeDefinesNothing;
        procedure TheRegisteredWindowsAreTheOnesTheTypeAsks;
        //  The server session that keeps the wavelength.
        procedure TheModuleDeclaresItsResources;
        //  The scalogram is the server's, over the fit window: no session
        //  answers it.
        procedure TheScalogramIsAnsweredOverTheFitWindowNotByTheSession;
        procedure TheWavelengthIsKeptAndReadBack;
        procedure ItIsTheProjectsStateToo;
        procedure ANewProfileKeepsIt;
        procedure TheBackgroundFractionIsKeptForItsProblem;
        procedure ItAnswersNothingElse;
        //  The background arithmetic.
        procedure WithoutTheArithmeticTheEngineIsRefusedInWords;
        { A command refused for want of the arithmetic leaves the engine where
          it was - its state and the background the user picked - since the
          user was told nothing was done. }
        procedure ACommandRefusedForWantOfTheArithmeticLeavesTheEngineAsItWas;
        //  The record a saved formula is.
        procedure ASavedFormulaHoldsItsParameters;
        procedure OnlyItsOwnClassIsRead;
    end;

    { THE BACKGROUND ARITHMETIC IS A CONTRACT WITH A CHOICE: the module's is
      the native one, and an alternative from another provider runs only
      while that provider is there. }
    TBackgroundChoiceTest = class(TTestCase)
    private
        FProviderThere: boolean;
        function ProviderThere: boolean;
    protected
        procedure TearDown; override;
    published
        procedure TheModulesArithmeticIsTheNativeImplementation;
        procedure AChosenAlternativeRunsWhileItsProviderIsThere;
    end;

implementation

procedure TScientificSeamsTest.SetUp;
begin
    FSession := ScientificModule.CreateSession(nil);
    //  The registry is the program's: put back what was there.
    FSavedFormula := UserCurveFormulaDialog;
    FSavedRoles := UserCurveRolesDialog;
    FSavedParser := UserCurveExpressionParser;
end;

procedure TScientificSeamsTest.TearDown;
begin
    RegisterUserCurveDialogs(FSavedFormula, FSavedRoles, FSavedParser);
    //  Not freed, as the service frees none of the sessions it makes: an
    //  IModuleSession carries no way back to its object.
    FSession := nil;
    RegisterBackgroundAlgorithms(@background_search.ProposeBackgroundPoints,
        @background_search.SubtractPiecewiseLinear,
        @background_search.BackgroundRemovedCopy);
end;

procedure TScientificSeamsTest.WithoutTheWindowsTheFormulaTypeDefinesNothing;
begin
    RegisterUserCurveDialogs(nil, nil, nil);
    AssertFalse(UserCurveDialogsRegistered);
    AssertFalse('as cancelled', TConfigurableUserPointsSet.ShowConfigurationDialog);
end;

procedure TScientificSeamsTest.TheRegisteredWindowsAreTheOnesTheTypeAsks;
var
    Formula: TMockFormulaDlg;
    Roles: TMockRolesDlg;
    Parser: TMockExpressionParser;
begin
    Formula := TMockFormulaDlg.Create(Answers(daCancelled));
    Roles := TMockRolesDlg.Create(Answers(daAccepted));
    Parser := TMockExpressionParser.Create;
    try
        RegisterUserCurveDialogs(Formula, Roles, Parser);
        AssertTrue(UserCurveDialogsRegistered);
        AssertTrue(UserCurveFormulaDialog = IUserCurveFormulaDlg(Formula));
        AssertTrue(UserCurveRolesDialog = IUserCurveRolesDlg(Roles));
        AssertTrue(UserCurveExpressionParser = IExpressionParser(Parser));
        //  The user cancels the first window: nothing is defined.
        AssertFalse(TConfigurableUserPointsSet.ShowConfigurationDialog);
        AssertEquals('the formula window was asked', 1, Formula.Asked);
    finally
        RegisterUserCurveDialogs(nil, nil, nil);
        Formula.Free;
        Roles.Free;
        Parser.Free;
    end;
end;

procedure TScientificSeamsTest.TheModuleDeclaresItsResources;
var
    R: TModuleResourceArray;
begin
    AssertEquals(ScientificModuleName, ScientificModule.Name);
    R := ScientificModule.Resources;
    AssertEquals(4, Length(R));
    AssertEquals(WavelengthResource, R[0].Name);
    AssertEquals(ScientificProjectStateResource, R[1].Name);
    AssertEquals(BackgroundFractionResource, R[2].Name);
    AssertEquals(ScalogramResource, R[3].Name);
    AssertEquals(ScientificModuleName, FSession.Kind);
end;

procedure TScientificSeamsTest.TheScalogramIsAnsweredOverTheFitWindowNotByTheSession;
var
    R: TModuleResourceArray;
    i: longint;
    Json: string;
    Found: boolean;
begin
    R := ScientificModule.Resources;
    Found := False;
    for i := 0 to High(R) do
        if R[i].Name = ScalogramResource then
        begin
            Found := True;
            AssertTrue('the server computes it over the fit window',
                Assigned(R[i].OverFitWindow));
        end
        else
            AssertFalse(R[i].Name + ' is the session''s',
                Assigned(R[i].OverFitWindow));
    AssertTrue('declared', Found);
    AssertFalse('the session does not answer it',
        FSession.TryGet(ScalogramResource, Json));
end;

procedure TScientificSeamsTest.TheWavelengthIsKeptAndReadBack;
var
    Json: string;
begin
    AssertTrue(FSession.TryPost(WavelengthResource, '{"waveLength":1.5406}', Json));
    AssertTrue(Pos('1.5406', Json) > 0);
    AssertTrue(FSession.TryGet(WavelengthResource, Json));
    AssertTrue(Pos('1.5406', Json) > 0);
end;

procedure TScientificSeamsTest.ItIsTheProjectsStateToo;
var
    Json: string;
begin
    AssertTrue(FSession.TryPost(ScientificProjectStateResource,
        '{"waveLength":2.29}', Json));
    AssertTrue(FSession.TryGet(ScientificProjectStateResource, Json));
    //  Read back as a JSON float, "2.2900000000000000E+000".
    AssertTrue(Pos('2.29', Json) > 0);
    //  Not an object: left as it was.
    AssertTrue(FSession.TryPost(WavelengthResource, '[1]', Json));
    AssertTrue(Pos('2.29', Json) > 0);
end;

{ The background fraction: the module's since 2026-10-18, a thirtieth by
  default, kept per problem - the host the session was made for - and read
  by the peak search through that host. }
procedure TScientificSeamsTest.TheBackgroundFractionIsKeptForItsProblem;
var
    Json: string;
    Host: TObject;
    Session: IModuleSession;
begin
    Host := TObject.Create;
    try
        AssertEquals('a thirtieth where no session was made', DefaultBackFactor,
            BackFactorFor(Host), 1e-12);
        Session := ScientificModule.CreateSession(Host);
        AssertEquals('a thirtieth for a new problem', DefaultBackFactor,
            BackFactorFor(Host), 1e-12);
        AssertTrue(Session.TryPost(BackgroundFractionResource,
            '{"backFactor":12}', Json));
        AssertEquals('as posted', 12, BackFactorFor(Host), 1e-12);
        AssertTrue('in the project''s state too', Session.TryGet(
            ScientificProjectStateResource, Json) and (Pos('backFactor', Json) > 0));
        Session.Reset;
        AssertEquals('a new profile keeps it', 12, BackFactorFor(Host), 1e-12);
    finally
        Session := nil;
        Host.Free;
    end;
end;

procedure TScientificSeamsTest.ANewProfileKeepsIt;
var
    Json: string;
begin
    FSession.TryPost(WavelengthResource, '{"waveLength":1.54}', Json);
    FSession.Reset;
    FSession.TryGet(WavelengthResource, Json);
    AssertTrue('the radiation''s, not the data file''s', Pos('1.54', Json) > 0);
end;

procedure TScientificSeamsTest.ItAnswersNothingElse;
var
    Json, Note: string;
    Bounds: TIntervalBoundsX;
    Removed: TInstanceHandles;
begin
    AssertFalse(FSession.TryGet('other/thing', Json));
    AssertFalse(FSession.TryPost('other/thing', '{}', Json));
    AssertTrue(FSession.PointSink = nil);
    AssertFalse(FSession.ContributesFitReadiness);
    FSession.CheckFitAllowed;
    FSession.CheckIntervalAllowed(0, 1, 1);
    AssertTrue(FSession.SliceForInterval(0, 1, 1) = nil);
    AssertFalse(FSession.ProposeFitIntervals([0, 1], Bounds));
    AssertFalse(FSession.SearchFitIntervals([0, 1], [0, 1], Bounds, Note));
    AssertFalse(FSession.PlaceInstancesFromData([0, 1], [0, 1], nil, Note));
    AssertFalse(FSession.TryRemoveInstance('x', Removed));
end;

procedure TScientificSeamsTest.WithoutTheArithmeticTheEngineIsRefusedInWords;
var
    Data, Back: TPointsSet;
    Refused: longint;
begin
    RegisterBackgroundAlgorithms(nil, nil, nil);
    Data := TPointsSet.Create(nil);
    Back := TPointsSet.Create(nil);
    Refused := 0;
    try
        try
            //  Named by unit: background_search, later in the uses
            //  clause, declares the same names.
            background_algorithms.ProposeBackgroundPoints(Data).Free;
        except
            on EUserException do Inc(Refused);
        end;
        try
            background_algorithms.SubtractPiecewiseLinear(Data, Back);
        except
            on EUserException do Inc(Refused);
        end;
        try
            background_algorithms.BackgroundRemovedCopy(Data).Free;
        except
            on EUserException do Inc(Refused);
        end;
    finally
        Back.Free;
        Data.Free;
    end;
    AssertEquals('all three, in words', 3, Refused);
end;

procedure TScientificSeamsTest.ACommandRefusedForWantOfTheArithmeticLeavesTheEngineAsItWas;
var
    Moved: string;

    { Asks one command of a fresh engine holding a profile and two picked
      background points; adds to Moved what changed if it was refused. }
    procedure Ask(const AName: string; AOp: longint);
    var
        Service: TFitService;
        Profile, Kept: TTitlePointsSet;
        i: longint;
        Before: TFitServerState;
        Refused: boolean;
    begin
        Service := TFitService.Create;
        try
            Profile := TTitlePointsSet.Create(nil);
            for i := 0 to 9 do
                Profile.AddNewPoint(i, 10 + i);
            Service.SetProfilePointsSet(Profile);
            Service.AddPointToBackground(0, 10);
            Service.AddPointToBackground(9, 19);
            Before := Service.GetState;
            Refused := False;
            try
                case AOp of
                    0: Service.SubtractBackground(False);
                    1: Service.SubtractBackground(True);
                    2: Service.ComputeBackgroundPoints;
                end;
            except
                on EUserException do
                    Refused := True;
            end;
            if not Refused then
                Moved := Moved + Format('[%s: not refused] ', [AName]);
            if Service.GetState <> Before then
                Moved := Moved + Format('[%s: state %d -> %d] ',
                    [AName, Ord(Before), Ord(Service.GetState)]);
            Kept := Service.GetBackgroundPoints;
            try
                if Kept.PointsCount <> 2 then
                    Moved := Moved + Format('[%s: %d background points left] ',
                        [AName, Kept.PointsCount]);
            finally
                Kept.Free;
            end;
        finally
            Service.Free;
        end;
    end;

begin
    RegisterBackgroundAlgorithms(nil, nil, nil);
    Moved := '';
    Ask('subtract the picked background', 0);
    Ask('subtract a proposed background', 1);
    Ask('propose background points', 2);
    AssertEquals('a REFUSED command changed the engine: ' + Moved, '', Moved);
end;

procedure TScientificSeamsTest.ASavedFormulaHoldsItsParameters;
var
    F: Curve_type;
    C: TCollection;
begin
    F := Curve_type.Create(nil);
    try
        AssertTrue('a parameter set from the start', Assigned(F.Parameters));
        C := TCollection.Create(TCollectionItem);
        F.Params := C;
        AssertTrue('the collection it is given', F.Params = C);
    finally
        F.Free;
    end;
end;

procedure TScientificSeamsTest.OnlyItsOwnClassIsRead;
begin
    AssertTrue(SavedFormulaComponentClass('Curve_type') = Curve_type);
    AssertTrue(SavedFormulaComponentClass('Settings_v1') = nil);
end;

{ TBackgroundChoiceTest }

var
    AlternativeSubtracted: boolean = False;

{ The alternative: one point at (-1, -1), which no real proposal makes. }
function AlternativePropose(AData: TPointsSet): TPointsSet;
begin
    Result := TPointsSet.Create(nil);
    Result.AddNewPoint(-1, -1);
end;

procedure AlternativeSubtract(AData, ABackground: TPointsSet);
begin
    AlternativeSubtracted := True;
end;

function AlternativeRemovedCopy(AData: TPointsSet): TPointsSet;
begin
    Result := AlternativePropose(AData);
end;

function TBackgroundChoiceTest.ProviderThere: boolean;
begin
    Result := FProviderThere;
end;

procedure TBackgroundChoiceTest.TearDown;
begin
    SetProviderProbe(PythonSidecarProvider, nil);
    MachineSettings.SetStr(ImplementationsSection, BackgroundContract, '');
    //  The module's arithmetic alone again, as linking it left it.
    RegisterBackgroundAlgorithms(@background_search.ProposeBackgroundPoints,
        @background_search.SubtractPiecewiseLinear,
        @background_search.BackgroundRemovedCopy);
end;

procedure TBackgroundChoiceTest.TheModulesArithmeticIsTheNativeImplementation;
var
    All: TImplementationInfoArray;
begin
    All := ImplementationsOf(BackgroundContract);
    AssertEquals('one, the module''s', 1, Length(All));
    AssertEquals(NativeBackgroundId, All[0].Id);
    AssertEquals(NativeProvider, All[0].Provider);
end;

procedure TBackgroundChoiceTest.AChosenAlternativeRunsWhileItsProviderIsThere;
var
    Info: TImplementationInfo;
    Data, Back, Got: TPointsSet;
    i: longint;
begin
    Info := Default(TImplementationInfo);
    Info.Id := 'alternative';
    Info.Name := 'An alternative background';
    Info.Description := 'Proposes one point no real proposal would.';
    Info.Provider := PythonSidecarProvider;
    RegisterBackgroundImplementation(Info, @AlternativePropose,
        @AlternativeSubtract, @AlternativeRemovedCopy);
    ChooseImplementation(BackgroundContract, 'alternative');
    SetProviderProbe(PythonSidecarProvider, @ProviderThere);

    Data := TPointsSet.Create(nil);
    Back := TPointsSet.Create(nil);
    try
        for i := 0 to 20 do
            Data.AddNewPoint(i, 10 + i + 50 * Ord(i = 10));
        Back.AddNewPoint(0, 10);
        Back.AddNewPoint(20, 30);

        FProviderThere := False;
        Got := background_algorithms.ProposeBackgroundPoints(Data);
        try
            AssertTrue('absent, the native one proposes',
                Got.IndexOfValueX(-1) < 0);
        finally
            Got.Free;
        end;

        FProviderThere := True;
        Got := background_algorithms.ProposeBackgroundPoints(Data);
        try
            AssertEquals('there, the one chosen proposes', -1,
                Got.PointXCoord[0], 1e-9);
        finally
            Got.Free;
        end;
        Got := background_algorithms.BackgroundRemovedCopy(Data);
        try
            AssertEquals('and removes', -1, Got.PointXCoord[0], 1e-9);
        finally
            Got.Free;
        end;
        AlternativeSubtracted := False;
        background_algorithms.SubtractPiecewiseLinear(Data, Back);
        AssertTrue('and subtracts', AlternativeSubtracted);
    finally
        Data.Free;
        Back.Free;
    end;
end;

initialization
    RegisterTest('unit', TScientificSeamsTest);
    RegisterTest('unit', TBackgroundChoiceTest);
end.
