// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used in displaying results to user.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_client;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, int_client_callback, int_data_loader, int_data_loader_injector,
    curve_types_singleton, int_curve_type_selector,
    int_fit_service, int_fit_viewer, mscr_specimen_list, named_points_set,
    neutron_points_set, points_set, self_copied_component, SysUtils,
    title_points_set
    , persistent_curve_parameters, fit_statistics
    , client_log

    ;

{$IFNDEF SERVER}
// Switch on updating legend and grids.
{$DEFINE USE_LEGEND}
{$DEFINE USE_GRIDS}
{$ENDIF}

type
    { Modes of selectiion of active point set. }
    TSelMode    =
        (ModeSelectNothing, ModeSelectIntervalBounds, ModeSelectCharacteristicPoints,
        ModeSelectCurveBounds, ModeSelectBackground, ModeSelectCurvePositions,
        ModeSelectRFactorBounds,
        { Picks going into a MODULE's own point set - which one is
          ModulePickSet. Reuses the same collect-then-act shape as
          ModeSelectCurveBounds, so the gesture is one users already know from
          placing a curve.

          ONE member rather than an open registry: the four case statements over
          this enum have to stay exhaustive, and the set a pick belongs to is
          carried beside the mode instead of multiplying it. }
        ModeSelectModulePoints);
    { Results of data file opening. }
    TOpenState  = (OpenSuccess, OpenFailure);
    { States of processing long operations. }
    TAsyncState = (
        { Before start. }
        AsyncStart,
        { Fitting in progress. }
        AsyncWorks,
        { Fitting is done. }
        AsyncDone
        );

    TAsyncOperationFinished = procedure(Sender: TObject) of object;
    { A blocking server call, run off the UI thread by TFitClient.RunAsync. }
    TServerOp = function: string of object;
    TPlotProc = procedure of object;

    { Implements all client logic of the application. Must be completely independent from UI. }
    TFitClient = class(TInterfacedObject, IClientCallback)
    protected
        FFitService:     IFitService;
        FDataLoader:     IDataLoader;
        FDataLoaderInjector:  IDataLoaderInjector;
        { Enables animation mode in which UI is updated on every
          computation cycle not only on finishing. By default is false. }
        FAnimationMode: boolean;
        { All the data displayed on the chart. They are required to be able control of X-coordinate. }
        FExperimentalProfile: TTitlePointsSet;
        { Region of given profile data with which user is working at the given moment. }
        FSelectedArea:     TTitlePointsSet;
        { Sum of all model curces which is compared with experimental data. }
        FComputedProfile:  TTitlePointsSet;
        FDeltaProfile:     TTitlePointsSet;
        { Set of points selected by user. }
        FSelectedPoints:   TTitlePointsSet;
        { List of background points which is used for transmission between manual and automatic selection modes. }
        FBackgroundPoints: TTitlePointsSet;
        { List of point pairs which limit interval of R-factor calculation. 
          Always must be displayed in order to show user in which mode R-factor is calculated. }
        FRFactorBounds:    TTitlePointsSet;
        { Positions of curves. Only X-coordinates are used. }
        FCurvePositions:   TTitlePointsSet;
        { What the server reports the built model's curves as sitting at. Drawn,
          never edited: the picks above are the editable set. }
        FResultedCurvePositions: TTitlePointsSet;
        { Containers of calculated curves. Each object contains data of specimen curve. }
        FCurves:           TSelfCopiedCompList;
        { Containers of parameters of curves. }
        FCurveAttributes:  TMSCRCurveList;
        { TODO: remove this attribute. }
        FWaveLength:       double;
        { THE TWO SETTINGS THE WINDOW TICKS A MENU FROM, held here.

          They are server state and the server stays the owner of record - this
          class is its only writer - but they used to be read back over HTTP on
          every use, and their use is an action's Update handler. The LCL runs
          those on the IDLE LOOP and once during FormCreate, so a tick box cost a
          round trip continuously, and a client started before its server logged
          a fatal with a stack trace before the window was even shown. }
        FBackgroundVariationEnabled: boolean;
        FCurveScalingEnabled: boolean;
        { Whether the two above hold an answer from the server this client is
          currently pointed at. Cleared when it is pointed at another one, and
          only there. A SERVER THAT DID NOT ANSWER COUNTS AS ANSWERED: retrying
          would retry on every idle tick, and against a dead port each attempt
          costs a connect timeout that the window waits for. Pointing the client
          at a server is the action a user takes to fix that, and it is what
          re-reads them - see ReadServerFlags. }
        FServerFlagsRead: boolean;
        { Fills the two above from the server, once per server. }
        procedure ReadServerFlags;
        procedure SetCurvesListLambda;

    protected
        FCurMin:           double;
        { If True then in all operations only data belonging to selected ared are used
          otherwise all profile data are used. }
        FSelectedAreaMode: boolean;
        FSelectionMode: TSelMode;
        { Which module point set ModeSelectModulePoints is collecting into. }
        FModulePickSet: string;
        FOpenState:  TOpenState;
        FAsyncState: TAsyncState;
        { Adds new point to the given set. Second call removes point from the set. 
          In last case the set is recreated. }
        procedure AddPoint(var Points: TTitlePointsSet;
            XValue, YValue: double; Plot: TPlotProc);
        { Replaces point and updates chart. }
        procedure ReplacePoint(Points: TTitlePointsSet;
            PrevXValue, PrevYValue, NewXValue, NewYValue: double;
            Plot: TPlotProc);

    protected
        { Pointers to methods for curve displaying. }

        { Callback on asynchronous operation finishing. }
        FAsyncOperationFinished: TAsyncOperationFinished;

        procedure HideCurves;

        { Wrappers for calls to external displaying methods. 
          They are necessary to check that external interface methods are connected.
          Opposite means that there aren't corresponding GUI elements. }
        procedure PlotCurves;
        procedure PlotSelectedPoints;

        procedure PlotRFactorBounds;
        procedure HideRFactorBounds;

        procedure PlotCurvePositions;
        procedure HideCurvePositions;
        procedure PlotResultedCurvePositions;
        { Fills the positions table from whichever set belongs in it. }
        procedure TabulateCurvePositions;
        procedure HideResultedCurvePositions;

        procedure PlotExpProfile;
        procedure HideExpProfile;

        procedure PlotSelectedProfileInterval;

        procedure PlotBackground;
        procedure HideBackground;

        procedure PlotComputedProfile;
        procedure PlotDeltaProfile;
        procedure Refresh;
        procedure RefreshPointsSet(ToRefresh: TNeutronPointsSet);
        procedure Clear;
        procedure Hide(ToRefresh: TNeutronPointsSet);
{$IFDEF USE_GRIDS}
        procedure FillDatasheetTable;
{$ENDIF}

        { Runs a blocking server call in a worker thread and, when it returns,
          invokes ADone on the main thread. The compute server is reached over
          HTTP, so its operations are synchronous - without this the UI would
          freeze for the whole fit and the completion callback (which used to come
          back from the in-process server) would never arrive.

          VIRTUAL SO A TEST CAN RUN THE OPERATION IN PLACE. Everything above the
          thread - which command sends which verb, and which completion handler
          re-reads what - is ordinary logic that a background thread and a
          Synchronize made unreachable. A subclass that calls AOp and ADone
          directly reaches all of it and none of the threading, which is the one
          part that genuinely needs a running message loop.

          Overriding it changes nothing in production: this is the only
          declaration, and the application never subclasses TFitClient. }
        procedure RunAsync(AOp: TServerOp; ADone: TThreadMethod); virtual;

        procedure SetSelectionMode(ASelectionMode: TSelMode);

        function GetMaxRFactor: double;
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetBackFactor: double;
        procedure SetBackFactor(ABackFactor: double);
        function GetCurveThresh: double;
        procedure SetCurveThresh(ACurveThresh: double);
        function GetCurveType: TCurveTypeId;
        procedure SetCurveType(ACurveTypeId: TCurveTypeId);

        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);

        function GetMinimizerKind: longint;
        procedure SetMinimizerKind(AKind: longint);
        function GetLossKind: longint;
        procedure SetLossKind(AKind: longint);
        function GetWeighting: string;
        procedure SetWeighting(const AValue: string);

        function GetServerUrl: string;
        procedure SetServerUrl(const AUrl: string);

        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);

        { Creates list of selected points and inserts new item into chart legend (CheckListBox). }
        procedure RecreateAndShowSelectedPoints(Title: string);

        procedure SetExpProfile(AExpProfile: TTitlePointsSet);
        procedure ClearExpProfile;
        procedure RemoveSelectedPoints;
        procedure RemoveSelectedArea;
        procedure RemoveComputedProfile;
        procedure RemoveDeltaProfile;

        { Copies data from the given point set to the set of selected interval. }
        procedure SelectProfileIntervalActual(ANeutronPoints: TNeutronPointsSet;
            StartPointIndex, StopPointIndex: longint);
        procedure CopyProfileDataFromLoader;

    public
        FFitViewer: IFitViewer;

        { Updates all the data and refreshes the chart.

          Public because a module changes the model through its own resources,
          not through this class, and must then be able to say so - the same
          thing every built-in verb here says after a write.

          Virtual so a test can count the refreshes: the invariant worth guarding
          is that EVERY pick mode causes one, and asserting that through a mock of
          the 83-method service contract would cost more than it proves. }
        procedure UpdateComputedData(ShowExtraData: boolean); virtual;

        { Returns full profile or part of profile selected at the moment. }
        function GetProfilePoints: TTitlePointsSet;
        function GetBackgroundPoints: TNeutronPointsSet;
        function GetSelectedPoints: TNeutronPointsSet;
        function GetRFactorBounds: TNeutronPointsSet;
        function GetCurvePositions: TNeutronPointsSet;

        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means first initialization. }
            CP: Curve_parameters);
        { Tells the server that the user-defined formula it holds is gone. }
        procedure ClearSpecialCurve;

        { Do only cleaning of sets. }

        { Removes ONE curve from the model, named by the handle its instance
          carries. Not the same kind of thing as the Remove... methods below,
          which drop a local point set and its series: this reaches the service,
          which removes the curve and the pick it was seeded from together.

          Answers False when the model no longer holds that handle - which is
          not a fault: the user may have deleted the same curve twice, or a fit
          may have removed it in between. }
        function DeleteCurve(const AInstanceId: string): boolean;

        { The curves the last refresh read, and their attributes, for a caller
          that DISPLAYS them - the parameter table and the Model panel.

          READ-ONLY, and the caller must not free either: both are the client's
          own copies, replaced wholesale on the next refresh. Exposed rather
          than re-fetched because GetCurves costs one request per curve over
          HTTP, and a panel redrawn from a poll would make that a request storm.

          PAIRED BY INDEX, which is the pairing the wire itself uses:
          GetCurveCount indexes the points and GetCurveParameterCount the
          attributes of the same curve. }
        function CurvesForDisplay: TSelfCopiedCompList;
        function CurveAttributesForDisplay: TMSCRCurveList;

        procedure RemoveRFactorBounds;
        procedure RemoveCurvePositions;
        procedure RemoveResultedCurvePositions;
        procedure RemoveBackgroundPoints;

        { All call AddPoint method. }

        procedure AddPointToSelected(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);
        { A pick belonging to a module's own point set, named by
          BeginModuleSelection. A method rather than two lines in the dispatch
          below, so it cannot be written without the refresh that every other
          AddPointTo... ends with. }
        procedure AddPointToModuleSet(XValue, YValue: double);

        { All call ReplacePoint method. }

        procedure ReplacePointInProfile(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure AddPointToActive(XValue, YValue: double);
        { Returns a set with which user works at the moment. }
        function GetCurrentPointsSet: TTitlePointsSet;

        { Cleans chart and moves data from full profile to data of selected iterval. }
        procedure SelectProfileInterval(StartPointIndex, StopPointIndex: longint);
        procedure SelectEntireProfile;

        procedure SetWaveLength(AWaveLength: double);
        function GetWaveLength: double;

        constructor CreateWithInjector(
            ADataLoaderInjector: IDataLoaderInjector);
        destructor Destroy; override;

        procedure LoadDataSet(FileName: string);
        { Pushes the loaded profile to the server, surviving an unreachable one. }
        procedure SendProfileToServer;
        procedure Reload;
        { Re-reads everything from the engine into this client's own copies.

          WHAT RELOAD ABOVE CANNOT DO. Reload re-reads the DATA FILE, and a
          project has no data file to re-read - its profile is in the project.
          UpdateComputedData is no substitute either: it re-reads the derived
          sets and the picks and leaves FExperimentalProfile alone, because
          every other path that changes the profile has already put it there.
          Restoring a project is the one path that does not, so without this the
          model came back and the chart stayed empty. }
        procedure ResyncFromService;
        { Drops everything and leaves the engine with an empty problem.

          NOT Reload, which needs a data loader: a session that opened a PROJECT
          has never had one, so New Project raised there instead of clearing. }
        procedure StartEmpty;

        { Callbacks from the server. }

        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        { Wrappers for server methods. Mustn't create messages because this
          is responsibility of GUI. Instead of this must throw exceptions. }

        procedure SmoothProfile;
        procedure SubtractBackground(Auto: boolean);
        procedure DoAllAutomatically;
        procedure MinimizeDifference;
        procedure MinimizeNumberOfCurves;
        procedure ComputeCurveBounds;
        procedure ComputeBackgroundPoints;
        procedure ComputeCurvePositions;
        procedure SelectAllPointsAsCurvePositions;
        procedure StopAsyncOper;
        { Gets state of asynchronous operation from the server. }
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        { Goodness-of-fit statistics for the last fit. }
        function GetStatistics: TFitStatistics;
        procedure CreateCurveList;

        { Selects the curve type to fit with. There are two selections to keep
          in step: the client-side registry, which drives the curve-type menu,
          and the compute server's own, which decides what is actually fitted.
          Updating only the first left the menu showing one type while the
          server went on fitting with its default - see
          https://github.com/dvmorozov/fit/issues/126 - so the UI must never
          call the singleton directly, only this. }
        procedure SelectCurveType(ACurveTypeId: TCurveTypeId);

        { Server attributes. }

        property BackgroundVariationEnabled: boolean
            read GetBackgroundVariationEnabled write SetBackgroundVariationEnabled;

        property MinimizerKind: longint
            read GetMinimizerKind write SetMinimizerKind;
        { The objective a fit minimises (LOSS_KIND_* in fit_loss). }
        property LossKind: longint read GetLossKind write SetLossKind;
        { Residual weighting for the Python backend ('poisson'/'none'). }
        property Weighting: string read GetWeighting write SetWeighting;

        property ServerUrl: string read GetServerUrl write SetServerUrl;

        property CurveScalingEnabled: boolean
            read GetCurveScalingEnabled write SetCurveScalingEnabled;

        property MaxRFactor: double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: double read GetBackFactor write SetBackFactor;
        property CurveThresh: double read GetCurveThresh write SetCurveThresh;
        property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;

        { Plotting events are called from methods of the same name for providing
          synchronization with main application thread.
          Point to methods of TIIViewer. }

        { Callbacks for updating user interface. They are called from main thread
          of client application. Callbacks can throw exceptions.
          They can be not assigned (nil). }
        property OnAsyncOperationFinished: TAsyncOperationFinished
            read FAsyncOperationFinished write FAsyncOperationFinished;

        property SelectionMode: TSelMode read FSelectionMode write SetSelectionMode;
        { Starts collecting picks into a module's own point set. The set travels
          beside the mode, so the enum stays closed and its case statements stay
          exhaustive. }
        procedure BeginModuleSelection(const APointSet: string);
        property ModulePickSet: string read FModulePickSet;
        property OpenState: TOpenState read FOpenState;
        property AsyncState: TAsyncState read FAsyncState;
        property SelectedAreaMode: boolean read FSelectedAreaMode;
        property AnimationMode: boolean read FAnimationMode write FAnimationMode;

        property FitService: IFitService read FFitService write FFitService;
    end;

const
    //  The user-facing name of an R-factor interval: the stretch of the
    //  profile one sub-task fits. Named for what the user does with it.
    CurveIntervalsName: string = 'Fit intervals';
    BackgroundPointsName: string = 'Background points';
    CurvePositionsName: string = 'Curve positions';
    //  A SECOND series, not a renaming of the one above. "Curve positions" is
    //  what the user picked; this is where the fit put the curves. Both are
    //  drawn, always: before a fit this one is empty, and after one the gap
    //  between a pick and its curve is worth seeing.
    FittedPositionsName: string = 'Fitted positions';
    SelectedIntervalName: string = 'Selected interval';
    TotalAmplitudeName: string = 'Total Amplitude';
    PositionName: string = 'Position';
    ProfileName: string = 'Profile';
    NumberName: string = 'Number';
    AmplitudeName: string = 'Amplitude';
    DifferenceName: string = 'Difference';
    StartingPositionName: string = 'Starting Position';
    FinalPositionName: string = 'Final Position';

implementation

uses
    main_calc_thread, module_view_types, int_module_overlay, MyExceptions,
    //  When each computed series is named and when it is drawn.
    computed_series, checks, points_tables;

{================================ TFitClient ==================================}

destructor TFitClient.Destroy;
begin
    FCurveAttributes.Free;
    FBackgroundPoints.Free;
    FSelectedArea.Free;
    FCurves.Free;
    FDeltaProfile.Free;
    FComputedProfile.Free;
    FSelectedPoints.Free;
    FExperimentalProfile.Free;
    FRFactorBounds.Free;
    inherited;
end;

constructor TFitClient.CreateWithInjector(ADataLoaderInjector: IDataLoaderInjector);
begin
    inherited;
    FDataLoaderInjector := ADataLoaderInjector;
    FSelectionMode := ModeSelectNothing;
    //  Every degree visible until the user narrows it.
    FOpenState  := OpenFailure;
    FAsyncState := AsyncStart;

    { Empty lists are created to allow manual data input. }
    SetExpProfile(TTitlePointsSet.Create(nil));

    FCurvePositions := TTitlePointsSet.Create(nil);
    FCurvePositions.FTitle := CurvePositionsName;
    FCurvePositions.WaveLength := FWaveLength;

    FBackgroundPoints := TTitlePointsSet.Create(nil);
    FBackgroundPoints.FTitle := BackgroundPointsName;
    FBackgroundPoints.WaveLength := FWaveLength;

    FRFactorBounds := TTitlePointsSet.Create(nil);
    FRFactorBounds.FTitle := CurveIntervalsName;
    FRFactorBounds.WaveLength := FWaveLength;
end;

function TFitClient.GetBackgroundPoints: TNeutronPointsSet;
begin
    //  nil is a legitimate answer: the set may not exist yet.
    Result := FBackgroundPoints;
end;

function TFitClient.GetSelectedPoints: TNeutronPointsSet;
begin
    //  nil is a legitimate answer: the set may not exist yet.
    Result := FSelectedPoints;
end;

function TFitClient.GetRFactorBounds: TNeutronPointsSet;
begin
    //  nil is a legitimate answer: the set may not exist yet.
    Result := FRFactorBounds;
end;

function TFitClient.GetCurvePositions: TNeutronPointsSet;
begin
    //  nil is a legitimate answer: the set may not exist yet.
    Result := FCurvePositions;
end;

function TFitClient.GetSpecialCurveParameters: Curve_parameters;
begin
    Result := FitService.GetSpecialCurveParameters;
end;

procedure TFitClient.SelectProfileIntervalActual(ANeutronPoints: TNeutronPointsSet;
    StartPointIndex, StopPointIndex: longint);
var
    i: longint;
begin
    CheckAssigned(ANeutronPoints, 'the profile an interval is being selected from');
    CheckThat(ANeutronPoints.PointsCount <> 0, 'an interval cannot be selected from a profile with no points');
    if (StartPointIndex < 0) or (StopPointIndex >
        ANeutronPoints.PointsCount - 1) then
        CheckThat(False, 'the selected interval must lie inside the profile it is taken from');
    CheckThat(ANeutronPoints <> FSelectedArea, 'an interval must be selected from the profile, not from the interval already selected');

    RemoveSelectedArea;
    FSelectedArea := TTitlePointsSet.Create(nil);
    try
        FSelectedArea.WaveLength := FWaveLength;
        FSelectedArea.FTitle := SelectedIntervalName;
        for i := StartPointIndex to StopPointIndex do
            FSelectedArea.AddNewPoint(
                ANeutronPoints.PointXCoord[i], ANeutronPoints.PointYCoord[i]);
    except
        FSelectedArea.Free;
        FSelectedArea := nil;
        raise;
    end;
end;

procedure TFitClient.SelectProfileInterval(StartPointIndex, StopPointIndex: longint);
begin
    CheckAssigned(FExperimentalProfile, 'the experimental profile an interval is selected from');
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SelectProfileInterval(StartPointIndex, StopPointIndex);
    Clear;
    SelectProfileIntervalActual(FExperimentalProfile, StartPointIndex, StopPointIndex);
    PlotSelectedProfileInterval;

    FSelectedAreaMode := True;
end;

procedure TFitClient.SelectEntireProfile;
begin
    CheckAssigned(FExperimentalProfile, 'the experimental profile to select in full');
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SelectEntireProfile;
    Clear;
    PlotExpProfile;

    FSelectedAreaMode := False;
    FSelectedArea.Free;
    FSelectedArea := nil;
end;

procedure TFitClient.RecreateAndShowSelectedPoints(Title: string);
begin
    RemoveSelectedPoints;
    FSelectedPoints := TTitlePointsSet.Create(nil);
    FSelectedPoints.FTitle := Title;
    FSelectedPoints.WaveLength := FWaveLength;
    PlotSelectedPoints;
end;

procedure TFitClient.SetExpProfile(AExpProfile: TTitlePointsSet);
begin
    CheckAssigned(AExpProfile, 'the experimental profile being taken on');

    FExperimentalProfile.Free;
    FExperimentalProfile := AExpProfile;
    FExperimentalProfile.FTitle := ProfileName;
    FExperimentalProfile.WaveLength := FWaveLength;
end;

procedure TFitClient.RemoveSelectedPoints;
begin
    Hide(FSelectedPoints);
    FSelectedPoints.Free;
    FSelectedPoints := nil;
end;

procedure TFitClient.ClearExpProfile;
begin
    HideExpProfile;
    //  GUARDED, like HideExpProfile just above: the profile is nil until data has
    //  been loaded, and ShowProfile clears before re-reading. An unguarded Clear
    //  here crashed a client asked to refresh before it had ever held a profile.
    if Assigned(FExperimentalProfile) then
        FExperimentalProfile.Clear;
    { FExperimentalProfile shouldn't be destroyed here to allow manual adding. }
end;

procedure TFitClient.RemoveComputedProfile;
begin
    if Assigned(FComputedProfile) then
    begin
        Hide(FComputedProfile);
        FComputedProfile.Free;
        FComputedProfile := nil;
    end;
end;

procedure TFitClient.RemoveDeltaProfile;
begin
    if Assigned(FDeltaProfile) then
    begin
        Hide(FDeltaProfile);
        FDeltaProfile.Free;
        FDeltaProfile := nil;
    end;
end;

procedure TFitClient.RemoveSelectedArea;
begin
    if Assigned(FSelectedArea) then
    begin
        Hide(FSelectedArea);
        FSelectedArea.Free;
        FSelectedArea := nil;
    end;
end;

procedure TFitClient.RemoveRFactorBounds;
begin
    if Assigned(FRFactorBounds) then
    begin
        HideRFactorBounds;
        FRFactorBounds.Free;
        FRFactorBounds := nil;
    end;
end;

function TFitClient.CurvesForDisplay: TSelfCopiedCompList;
begin
    Result := FCurves;
end;

function TFitClient.CurveAttributesForDisplay: TMSCRCurveList;
begin
    Result := FCurveAttributes;
end;

function TFitClient.DeleteCurve(const AInstanceId: string): boolean;
var
    Index: longint;
begin
    Result := False;
    CheckThat(Assigned(FFitService), 'the fit service is missing');
    if AInstanceId = '' then
        //  Nothing was selected. Answering False rather than faulting: an empty
        //  handle is the ordinary state of a panel with no row chosen.
        Exit;

    Index := FFitService.IndexOfCurveInstance(AInstanceId);
    if Index < 0 then
        Exit;

    FFitService.DeleteCurve(Index);

    //  EVERY SERIES DRAWN FOR IT, OFF THE CHART, AS AN ACT.
    //
    //  The re-plot below would take the framework's own curve series off
    //  anyway, and a contributor that registers a redraw hook replots its own
    //  markers - so this is not what makes the chart correct today. It is what
    //  makes it correct for a contributor that draws per-curve series and
    //  registers no hook, whose series would otherwise stay on the chart with
    //  nothing left able to say which curve they belonged to.
    //
    //  BEFORE the re-read, while the handle still names something.
    if Assigned(FFitViewer) then
        FFitViewer.HideSeriesOwnedBy(AInstanceId);

    //  RE-READ EVERYTHING DERIVED. The deletion invalidates the curves, their
    //  attributes and the picks, and the service is the authority for all
    //  three - so the client asks rather than adjusting its own copies.
    UpdateComputedData(True);
    Result := True;
end;

procedure TFitClient.RemoveCurvePositions;
begin
    if Assigned(FCurvePositions) then
    begin
        HideCurvePositions;
        FCurvePositions.Free;
        FCurvePositions := nil;
    end;
end;

procedure TFitClient.RemoveResultedCurvePositions;
begin
    if Assigned(FResultedCurvePositions) then
    begin
        HideResultedCurvePositions;
        FResultedCurvePositions.Free;
        FResultedCurvePositions := nil;
    end;
end;

procedure TFitClient.RemoveBackgroundPoints;
begin
    if Assigned(FBackgroundPoints) then
    begin
        HideBackground;
        FBackgroundPoints.Free;
        FBackgroundPoints := nil;
    end;
end;

procedure TFitClient.ShowCurMin(Min: double);
begin
    //  Stores current minimum value.
    FCurMin := Min;
    //  Updates UI.
    if Assigned(FFitViewer) then
    begin
        FFitViewer.ShowTime;
        FFitViewer.ShowRFactor;
        if FAnimationMode then
            UpdateComputedData(False);
    end;
end;

procedure TFitClient.ShowProfile;
begin
    if FSelectedAreaMode then
    begin
        //  Dropped and re-read rather than reused, so that whatever changed
        //  server-side - the background having been subtracted, say - is
        //  reflected here.
        RemoveSelectedArea;
        FSelectedArea := FitService.GetSelectedProfileInterval;
        FSelectedArea.WaveLength := FWaveLength;
        FSelectedArea.FTitle := SelectedIntervalName;
        PlotSelectedProfileInterval;
    end
    else
    begin
        //  Dropped and re-read rather than reused, so that whatever changed
        //  server-side - the background having been subtracted, say - is
        //  reflected here.
        ClearExpProfile;
        SetExpProfile(FitService.GetProfilePointsSet);
        PlotExpProfile;
    end;
end;

function TFitClient.GetProfilePoints: TTitlePointsSet;
begin
    if FSelectedAreaMode then
        Result := FSelectedArea
    else
        Result := FExperimentalProfile;
end;

procedure TFitClient.UpdateComputedData(ShowExtraData: boolean);
begin
    LogClientState(Format('refreshing computed data (extra data: %s)',
        [BoolToStr(ShowExtraData, True)]));
    if Assigned(FFitViewer) then
    begin
{$IFDEF USE_GRIDS}
        FFitViewer.SetUpdateGrids(ShowExtraData);
{$ENDIF}
{$IFDEF USE_LEGEND}
        FFitViewer.SetUpdateLegends(ShowExtraData);
{$ENDIF}
    end;

    //  WHEN EACH OF THESE IS NAMED AND WHEN IT IS DRAWN is in computed_series,
    //  where the rule can be stated once and tested. It used to be five copies
    //  of two conditions written inline, and the difference between two of the
    //  copies was a defect: see that unit.
    RemoveComputedProfile;
    FComputedProfile := FitService.GetCalcProfilePointsSet;
    if Assigned(FComputedProfile) then
    begin
        if SeriesIsNamed(csComputedProfile, FComputedProfile.PointsCount) then
        begin
            FComputedProfile.FTitle := TotalAmplitudeName;
            FComputedProfile.WaveLength := FWaveLength;
        end;
        if SeriesIsPlotted(csComputedProfile, FComputedProfile.PointsCount) then
            PlotComputedProfile;
    end;

    RemoveDeltaProfile;
    FDeltaProfile := FitService.GetDeltaProfilePointsSet;
    if Assigned(FDeltaProfile) then
    begin
        if SeriesIsNamed(csDeltaProfile, FDeltaProfile.PointsCount) then
        begin
            FDeltaProfile.FTitle := DifferenceName;
            FDeltaProfile.WaveLength := FWaveLength;
        end;
        if SeriesIsPlotted(csDeltaProfile, FDeltaProfile.PointsCount) then
            PlotDeltaProfile;
    end;

    RemoveCurvePositions;
    RemoveResultedCurvePositions;
    RemoveRFactorBounds;

    if ShowExtraData then
    begin
        FCurvePositions := FitService.GetCurvePositions;
        if Assigned(FCurvePositions) then
        begin
            if SeriesIsNamed(csCurvePositions, FCurvePositions.PointsCount) then
            begin
                FCurvePositions.FTitle := CurvePositionsName;
                FCurvePositions.WaveLength := FWaveLength;
            end;
            if SeriesIsPlotted(csCurvePositions, FCurvePositions.PointsCount) then
                PlotCurvePositions;
        end;

        //  What the model was built into, beside what was picked for it. Fetched
        //  here rather than derived from FCurves: the server owns the model, and
        //  over HTTP the client rebuilds curves from points alone - it has no
        //  pattern instances to read an x0 off.
        FResultedCurvePositions := FitService.GetResultedCurvePositions;
        if Assigned(FResultedCurvePositions) then
        begin
            if SeriesIsNamed(csFittedPositions,
                FResultedCurvePositions.PointsCount) then
            begin
                FResultedCurvePositions.FTitle := FittedPositionsName;
                FResultedCurvePositions.WaveLength := FWaveLength;
            end;
            if SeriesIsPlotted(csFittedPositions,
                FResultedCurvePositions.PointsCount) then
                PlotResultedCurvePositions;
        end;

        //  WHICH SET THE TABLE SHOWS, decided here because this is the only
        //  side that holds both. The picks when there are any; the model's own
        //  positions when there are none - an analysis pack places its
        //  instances from its own markup and picks nothing, so the table used
        //  to stay empty while the chart drew every one of them.
        TabulateCurvePositions;

        FRFactorBounds := FitService.GetRFactorBounds;
        if Assigned(FRFactorBounds) then
        begin
            if SeriesIsNamed(csRFactorBounds, FRFactorBounds.PointsCount) then
            begin
                FRFactorBounds.FTitle := CurveIntervalsName;
                FRFactorBounds.WaveLength := FWaveLength;
            end;
            if SeriesIsPlotted(csRFactorBounds, FRFactorBounds.PointsCount) then
                PlotRFactorBounds;
        end;
    end;

    HideCurves;
    FCurves.Free;
    FCurves := FitService.GetCurves;
    if Assigned(FCurves) then
        SetCurvesListLambda;

    FCurveAttributes.Free;
    FCurveAttributes := FitService.GetCurveAttributes;
    if Assigned(FCurveAttributes) then
        FCurveAttributes.FWaveLength := FWaveLength;

    //  WHERE EACH CURVE EXISTS, stamped on the curve itself the moment both
    //  halves are in hand. The server computes it and sends it in the curve's
    //  own attributes; from here on every consumer reads it off the point set
    //  and none has to be passed it.

    PlotCurves;
    //  After the curves, so a module's markers sit on the profile that was just
    //  recomputed. What each draws is its own business; this says only when.
    DrawModuleOverlays(Self);
{$IFDEF USE_GRIDS}
    if ShowExtraData then
    begin
        FillDatasheetTable;
    end;
{$ENDIF}
end;

type
    { Runs one blocking server call, then reports completion on the main thread. }
    TServerCallThread = class(TThread)
    private
        FOp:    TServerOp;
        FDone:  TThreadMethod;
        FError: string;
        procedure Finished;
    protected
        procedure Execute; override;
    public
        constructor Create(AOp: TServerOp; ADone: TThreadMethod);
    end;

constructor TServerCallThread.Create(AOp: TServerOp; ADone: TThreadMethod);
begin
    inherited Create(True);
    FreeOnTerminate := True;
    FOp := AOp;
    FDone := ADone;
    Start;
end;

procedure TServerCallThread.Execute;
begin
    try
        FOp;
    except
        on E: Exception do
        begin
            FError := E.Message;
            LogClientWarning('server operation failed: ' + FError +
                ' (' + E.ClassName + ')');
        end;
    end;
    Synchronize(Finished);
end;

procedure TServerCallThread.Finished;
begin
    if FError <> '' then
    begin
        //  Surface the server's failure instead of silently leaving the UI busy.
        if Assigned(main_calc_thread.OnCalcError) then
            main_calc_thread.OnCalcError(FError);
    end;
    if Assigned(FDone) then
        //  REPORTED HERE, because this runs inside Synchronize. An exception
        //  that escapes the handler is re-raised in the WORKER thread - after
        //  the except block above, which has already run - and dies there with
        //  the thread: the window is left half refreshed and the log says
        //  nothing at all. That is how curves that had stopped being drawn
        //  after every fit looked like a chart bug with no error behind it.
        try
            FDone;
        except
            on E: Exception do
            begin
                LogClientWarning('refreshing the window after a server ' +
                    'operation failed: ' + E.Message + ' (' + E.ClassName + ')');
                if Assigned(main_calc_thread.OnCalcError) then
                    main_calc_thread.OnCalcError(E.Message);
            end;
        end;
end;

procedure TFitClient.RunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    LogUiAction('long server operation started');
    FAsyncState := AsyncWorks;
    TServerCallThread.Create(AOp, ADone);
end;

procedure TFitClient.Done;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    LogUiAction('long server operation finished');
    ShowProfile;
    UpdateComputedData(True);
    FAsyncState := AsyncDone;

    //  Updates UI.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeCurveBoundsDone;
begin
    //  Reaching this without a service is a programming error, not a
    //  situation to recover from.
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');
    //  Hidden first, because the pointer is about to be replaced and the
    //  chart would otherwise keep drawing the old set.
    RemoveRFactorBounds;
    FRFactorBounds := FitService.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := CurveIntervalsName;
        FRFactorBounds.WaveLength := FWaveLength;

        PlotRFactorBounds;
    end;
    FAsyncState := AsyncDone;
    //  Hands back to the main form.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeBackgroundPointsDone;
begin
    //  Reaching this without a service is a programming error, not a
    //  situation to recover from.
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    RemoveBackgroundPoints;
    FBackgroundPoints := FitService.GetBackgroundPoints;
    if Assigned(FBackgroundPoints) and (FBackgroundPoints.PointsCount <> 0) then
    begin
        FBackgroundPoints.FTitle := BackgroundPointsName;
        FBackgroundPoints.WaveLength := FWaveLength;

        PlotBackground;
    end;
    FAsyncState := AsyncDone;
    //  Hands back to the main form.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeCurvePositionsDone;
begin
    //  Reaching this without a service is a programming error, not a
    //  situation to recover from.
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');
    //  Hidden first, because the pointer is about to be replaced and the
    //  chart would otherwise keep drawing the old set.
    RemoveRFactorBounds;
    FRFactorBounds := FitService.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := CurveIntervalsName;
        FRFactorBounds.WaveLength := FWaveLength;
        PlotRFactorBounds;
    end;

    RemoveCurvePositions;
    FCurvePositions := FitService.GetCurvePositions;
    if Assigned(FCurvePositions) and (FCurvePositions.PointsCount <> 0) then
    begin
        FCurvePositions.FTitle := CurvePositionsName;
        FCurvePositions.WaveLength := FWaveLength;
        PlotCurvePositions;
    end;

    FAsyncState := AsyncDone;
    //  Hands back to the main form.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

{ WHERE EACH CURVE EXISTS, copied from the attributes onto the curve.

  The two arrive separately - the points from one request, the attributes from
  another - and are paired by position, which is the pairing the wire itself
  uses: GetCurveCount indexes the points and GetCurveParameterCount the
  attributes of the same curve.

  A curve the attributes say nothing about keeps the whole of itself, which is
  what every curve had before an extent was stated - so an older server, or a
  curve type that reports no extent, behaves exactly as it used to. }

procedure TFitClient.SetCurvesListLambda;
var
    i:  longint;
    PointsSet: TNeutronPointsSet;
begin
    CheckAssigned(FCurves, 'the curves whose wavelength is being set');

    with FCurves do
        for i := 0 to FCurves.Count - 1 do
        begin
            PointsSet := TNeutronPointsSet(FCurves.Items[i]);
            PointsSet.WaveLength := FWaveLength;
        end;
end;

procedure TFitClient.SetWaveLength(AWaveLength: double);
begin
    FWaveLength := AWaveLength;
    if Assigned(FExperimentalProfile) then
        FExperimentalProfile.WaveLength := AWaveLength;
    if Assigned(FBackgroundPoints) then
        FBackgroundPoints.WaveLength := AWaveLength;
    if Assigned(FSelectedArea) then
        FSelectedArea.WaveLength := AWaveLength;
    if Assigned(FSelectedPoints) then
        FSelectedPoints.WaveLength := AWaveLength;
    if Assigned(FComputedProfile) then
        FComputedProfile.WaveLength := AWaveLength;
    if Assigned(FDeltaProfile) then
        FDeltaProfile.WaveLength := AWaveLength;
    if Assigned(FCurves) then
        SetCurvesListLambda;
    if Assigned(FCurveAttributes) then
        FCurveAttributes.FWaveLength := FWaveLength;
end;

function TFitClient.GetWaveLength: double;
begin
    Result := FWaveLength;
end;

procedure TFitClient.ReplacePoint(Points: TTitlePointsSet;
    PrevXValue, PrevYValue, NewXValue, NewYValue: double; Plot: TPlotProc);
begin
    CheckAssigned(Points, 'the points set a point is being replaced in');

    Points.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    //  Redrawn unconditionally: it is what clears the fields after an invalid
    //  entry.
    Plot;
end;

//  CALLING THIS AGAIN WITH THE SAME COORDINATES DELETES THE POINT. The two
//  picks of a range gesture are one add and one delete, which is why a module
//  that collects PAIRS of picks must not route them through here.
procedure TFitClient.AddPoint(var Points: TTitlePointsSet;
    XValue, YValue: double; Plot: TPlotProc);
var
    i: longint;
begin
    CheckAssigned(Points, 'the points set a point is being added to');

    //  Look for the given point in the selected set.
    for i := 0 to Points.PointsCount - 1 do
        if XValue = Points.PointXCoord[i] then
        begin
            //  DeletePoint changes what Points refers to, so the set has to be
            //  removed from the list of all curves properly...

            Points.DeletePoint(XValue);
            //  ...and drawn again afterwards.
            Plot;
            Exit;
        end;
    //  Two points cannot share an X and differ in Y, so comparing X alone
    //  identifies a point.

    //  Not found, so it is a new one.
    Points.AddNewPoint(XValue, YValue);
    Plot;
end;

{ The cumulative model's value at AX - the profile every component sums into.

  Pivots are drawn here rather than on the component that owns them, because a
  nested component is a DEVIATION from its parent's trend: on its own it is a
  wiggle about a line, not the staircase an analyst draws. The count only becomes
  recognisable against the sum, so that is where its pivots belong. }
{ CumulativeValueAt REMOVED, not extracted. It answered what the model says at
  an abscissa - which is what a chart would show under the pointer - and nothing
  in either repository ever called it. The same shape as the chart extents
  removed from fit_viewer: a method written for a reader that never arrived. See
  docs/contributing/findings.md. }

procedure TFitClient.PlotCurves;
begin
    if Assigned(FFitViewer) and Assigned(FCurveAttributes) then
        FFitViewer.PlotCurves(Self, FCurves, FCurveAttributes);
end;

procedure TFitClient.HideCurves;
var
    i: longint;
begin
    if Assigned(FCurves) then
        for i := 0 to FCurves.Count - 1 do
        begin
            Hide(TNeutronPointsSet(FCurves.Items[i]));
        end;
end;

{ How a module gesture's picks are drawn while the gesture is in progress.

  A DIAGONAL CROSS ON THE POINT ITSELF, because a module's pick IS a point of the
  data - a wave bound is a pivot, and both of its coordinates carry meaning. The
  built-in range gestures draw vertical lines instead, and rightly: what they
  pick is an x. Not joined by a line, since the two picks of a pair are the ends
  of something, not a path through it.

  The same style for every module, deliberately: a per-module style is a seam to
  add when a second module wants a different one, not in anticipation of it. }
function ModulePickStyle: TModuleSeriesStyle;
begin
    Result := DefaultModuleSeriesStyle;
    Result.Shape := msDiagCross;
    Result.Size := 7;
    Result.Color := mcBlack;
    Result.ShowLines := False;
    Result.ShowPoints := True;
    Result.Sorted := False;
end;

procedure TFitClient.PlotSelectedPoints;
begin
    if not (Assigned(FFitViewer) and Assigned(FSelectedPoints)) then
        Exit;
    if FSelectionMode = ModeSelectModulePoints then
        //  Through the module series contract, which is where a styled marker
        //  series already lives - rather than a second way of saying the same
        //  thing, or a style switch inside the view.
        FFitViewer.PlotModuleSeries(Self, FSelectedPoints.FTitle,
            FSelectedPoints, nil, ModulePickStyle)
    else
        FFitViewer.PlotSelectedPoints(Self, FSelectedPoints);
end;

procedure TFitClient.PlotRFactorBounds;
begin
    if Assigned(FFitViewer) and Assigned(FRFactorBounds) then
        FFitViewer.PlotRFactorBounds(Self, FRFactorBounds);
end;

procedure TFitClient.HideRFactorBounds;
begin
    if Assigned(FFitViewer) and Assigned(FRFactorBounds) then
        FFitViewer.HideRFactorBounds(Self, FRFactorBounds);
end;

procedure TFitClient.PlotCurvePositions;
begin
    if Assigned(FFitViewer) and Assigned(FCurvePositions) then
        FFitViewer.PlotCurvePositions(Self, FCurvePositions);
end;

procedure TFitClient.HideCurvePositions;
begin
    if Assigned(FFitViewer) and Assigned(FCurvePositions) then
        FFitViewer.HideCurvePositions(Self, FCurvePositions);
end;

procedure TFitClient.PlotResultedCurvePositions;
begin
    if Assigned(FFitViewer) and Assigned(FResultedCurvePositions) then
        FFitViewer.PlotResultedCurvePositions(Self, FResultedCurvePositions);
end;

function CountIn(APoints: TTitlePointsSet): longint;
begin
    Result := 0;
    if Assigned(APoints) then
        Result := APoints.PointsCount;
end;

procedure TFitClient.TabulateCurvePositions;
begin
    if not Assigned(FFitViewer) then
        Exit;
    //  The rule is points_tables', where it is tested against every
    //  combination of the two counts.
    case PositionsForTable(CountIn(FCurvePositions),
        CountIn(FResultedCurvePositions)) of
        psPicked:
            FFitViewer.TabulateCurvePositions(Self, FCurvePositions);
        psAchieved:
            //  NOT SORTED, and it must not be: this set is derived from the
            //  built curves and may hold two instances that converged on one
            //  x0, so sorting it would be reordering the model's own answer.
            //  The rows come out in the order the model holds them, which for a
            //  pack's markup is the order the user placed it in.
            FFitViewer.TabulateCurvePositions(Self, FResultedCurvePositions);
        //  psNone: nothing to put there, and the table is left as it is - it is
        //  never cleared from here, because that takes the input focus away.
    end;
end;

procedure TFitClient.HideResultedCurvePositions;
begin
    if Assigned(FFitViewer) and Assigned(FResultedCurvePositions) then
        FFitViewer.HideResultedCurvePositions(Self, FResultedCurvePositions);
end;

{$IFDEF USE_GRIDS}
procedure TFitClient.FillDatasheetTable;
begin
    if Assigned(FFitViewer) then
        FFitViewer.FillSummaryTable(FExperimentalProfile, FCurves,
            FComputedProfile, FDeltaProfile, FRFactorBounds);
end;
{$ENDIF}

procedure TFitClient.PlotExpProfile;
begin
    if Assigned(FFitViewer) and Assigned(FExperimentalProfile) then
       FFitViewer.PlotExpProfile(Self, FExperimentalProfile);
end;

procedure TFitClient.HideExpProfile;
begin
    if Assigned(FFitViewer) and Assigned(FExperimentalProfile) then
        FFitViewer.HideExpProfile(Self, FExperimentalProfile);
end;

procedure TFitClient.PlotSelectedProfileInterval;
begin
    if Assigned(FFitViewer) and Assigned(FSelectedArea) then
        FFitViewer.PlotSelectedProfileInterval(Self, FSelectedArea);
end;

procedure TFitClient.PlotBackground;
begin
    if Assigned(FFitViewer) and Assigned(FBackgroundPoints) then
        FFitViewer.PlotBackground(Self, FBackgroundPoints);
end;

procedure TFitClient.HideBackground;
begin
    if Assigned(FFitViewer) and Assigned(FBackgroundPoints) then
        FFitViewer.HideBackground(Self, FBackgroundPoints);
end;

procedure TFitClient.PlotComputedProfile;
begin
    if Assigned(FFitViewer) and Assigned(FComputedProfile) then
        FFitViewer.PlotComputedProfile(Self, FComputedProfile);
end;

procedure TFitClient.PlotDeltaProfile;
begin
    if Assigned(FFitViewer) and Assigned(FDeltaProfile) then
        FFitViewer.PlotDeltaProfile(Self, FDeltaProfile);
end;

procedure TFitClient.Refresh;
begin
    if Assigned(FFitViewer) then
        FFitViewer.Refresh(Self);
end;

procedure TFitClient.RefreshPointsSet(ToRefresh: TNeutronPointsSet);
begin
    if Assigned(FFitViewer) and Assigned(ToRefresh) then
        FFitViewer.RefreshPointsSet(Self, ToRefresh);
end;

procedure TFitClient.Clear;
begin
    if Assigned(FFitViewer) then
        FFitViewer.Clear(Self);
end;

procedure TFitClient.Hide(ToRefresh: TNeutronPointsSet);
begin
    if Assigned(ToRefresh) and Assigned(FFitViewer) then
        FFitViewer.Hide(Self, ToRefresh);
end;

procedure TFitClient.ReplacePointInProfile(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    if FSelectedAreaMode then
    begin
        CheckAssigned(FSelectedArea, 'the selected profile interval being edited');
        ReplacePoint(FSelectedArea,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedProfileInterval);
    end
    else
    begin
        CheckAssigned(FExperimentalProfile, 'the experimental profile being edited');
        ReplacePoint(FExperimentalProfile,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotExpProfile);
    end;
    FitService.ReplacePointInProfile(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitClient.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    CheckAssigned(FBackgroundPoints, 'the background points being edited');
    ReplacePoint(FBackgroundPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotBackground);
    FitService.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

{ AND THE SERVICE IS TOLD, which these two did not do.

  A PICK SET IS MODEL INPUT, and the service is authoritative for it: the local
  edit and the redraw show the user their own gesture at once, but the picks the
  next fit reads are the service's. Both of these edited the local set, redrew
  it, and told nobody - so a moved bound or a moved position was discarded by the
  next refresh, silently and with the chart briefly showing otherwise.

  Their two siblings above have always sent it. These are the pick sets, which
  is what makes the omission matter more here rather than less: moving a pick
  moves the curve that pick seeds.

  AND THE MODEL IS RE-READ, unlike the siblings'. A moved pick rebuilds the
  curves - AddPointToCurvePositions re-reads for the same reason - so the
  attributes, the curves and the fitted positions are all stale afterwards. }
procedure TFitClient.ReplacePointInRFactorBounds(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    CheckAssigned(FRFactorBounds, 'the interval bounds being edited');
    ReplacePoint(FRFactorBounds,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotRFactorBounds);
    FitService.ReplacePointInRFactorBounds(
        PrevXValue, PrevYValue, NewXValue, NewYValue);
    UpdateComputedData(True);
end;

procedure TFitClient.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    CheckAssigned(FCurvePositions, 'the curve positions being edited');
    ReplacePoint(FCurvePositions,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotCurvePositions);
    FitService.ReplacePointInCurvePositions(
        PrevXValue, PrevYValue, NewXValue, NewYValue);
    UpdateComputedData(True);
end;

procedure TFitClient.AddPointToSelected(XValue, YValue: double);
begin
    CheckAssigned(FSelectedPoints, 'the picked points being added to');
    AddPoint(FSelectedPoints, XValue, YValue, PlotSelectedPoints);
end;

procedure TFitClient.AddPointToBackground(XValue, YValue: double);
begin
    CheckAssigned(FBackgroundPoints, 'the background points being added to');
    AddPoint(FBackgroundPoints, XValue, YValue, PlotBackground);
end;

procedure TFitClient.AddPointToRFactorBounds(XValue, YValue: double);
begin
    FitService.AddPointToRFactorBounds(XValue, YValue);
    UpdateComputedData(True);
end;

procedure TFitClient.AddPointToCurvePositions(XValue, YValue: double);
begin
    FitService.AddPointToCurvePositions(XValue, YValue);
    UpdateComputedData(True);
end;

procedure TFitClient.AddPointToModuleSet(XValue, YValue: double);
begin
    //  SHOWN AS IT IS MADE. A pair of picks bounds one pattern, and between the
    //  two the user has nothing to go on: the first pick changes nothing on the
    //  server that can be drawn, so without a marker the gesture is invisible
    //  until it is over and a mis-aimed first pick cannot be seen at all.
    //
    //  Added directly rather than through AddPoint: there, a repeated x is an
    //  EDIT and a matching y a DELETE, which is the built-in range gesture's
    //  rule and not this one (see AddPoint).
    //  A REPEATED X IS KEPT, and must be. A nested pattern shares a bound with
    //  its parent, so two picks at one x are an ordinary part of the gesture -
    //  ModulePicksAccumulateRatherThanCancel holds exactly that. Neither
    //  toggling (which would annihilate the shared bound) nor skipping (which
    //  would lose the second pattern's end) is right here.
    //
    //  This set is sorted by PlotSelectedPoints, which used to make a repeated x
    //  fatal - TPointsSet.Sort read index -1. That is fixed in Sort itself,
    //  where it belongs, rather than by thinning the data on the way in.
    if Assigned(FSelectedPoints) then
    begin
        FSelectedPoints.AddNewPoint(XValue, YValue);
        PlotSelectedPoints;
    end;

    FitService.AddPointToSet(FModulePickSet, XValue, YValue);
    LogClientState(Format('%s: pick (%g, %g) sent to the server',
        [FModulePickSet, XValue, YValue]));
    //  THE REFRESH IS THE POINT, not the send. The server records the pick and
    //  builds whatever it now can, but the client shows nothing until it reads
    //  the model back - so a branch here that only sends leaves the user
    //  clicking on a chart that never changes, with no error anywhere.
    UpdateComputedData(True);
end;

function SelModeName(AMode: TSelMode): string;
begin
    case AMode of
        ModeSelectNothing: Result := 'nothing';
        ModeSelectIntervalBounds: Result := 'interval bounds';
        ModeSelectCharacteristicPoints: Result := 'characteristic points';
        ModeSelectCurveBounds: Result := 'curve bounds';
        ModeSelectBackground: Result := 'background';
        ModeSelectCurvePositions: Result := 'curve positions';
        ModeSelectRFactorBounds: Result := 'fit interval bounds';
        ModeSelectModulePoints: Result := 'module points';
        else Result := 'unknown';
    end;
end;

procedure TFitClient.BeginModuleSelection(const APointSet: string);
begin
    FModulePickSet := APointSet;
    SetSelectionMode(ModeSelectModulePoints);
end;

procedure TFitClient.SetSelectionMode(ASelectionMode: TSelMode);
var
    Previous: TSelMode;
begin
    Previous := FSelectionMode;
    LogUiAction(Format('selection mode: %s -> %s',
        [SelModeName(Previous), SelModeName(ASelectionMode)]));
    //  THE NEW MODE IS CURRENT BEFORE ANYTHING IS DRAWN. What a gesture's picks
    //  look like depends on which gesture it is, and the series is created by
    //  the drawing below - so a mode assigned afterwards would style the first
    //  gesture as the previous one and never correct it. The branch that needs
    //  the mode being LEFT takes it from Previous.
    FSelectionMode := ASelectionMode;
    case ASelectionMode of
        ModeSelectNothing:
            case Previous of
                ModeSelectIntervalBounds: RemoveSelectedPoints;
                ModeSelectCharacteristicPoints: RemoveSelectedPoints;
                ModeSelectCurveBounds: RemoveSelectedPoints;
                ModeSelectModulePoints: RemoveSelectedPoints;
            end;
        ModeSelectIntervalBounds:
            RecreateAndShowSelectedPoints('Area Limits');
        ModeSelectCharacteristicPoints:
            RecreateAndShowSelectedPoints('Characteristic Points');
        ModeSelectCurveBounds:
            RecreateAndShowSelectedPoints('Curve Bounds');
        ModeSelectModulePoints:
            RecreateAndShowSelectedPoints('Module Points');
        ModeSelectBackground:
        begin
            CheckAssigned(FBackgroundPoints, 'the background points the background picking mode shows');
            PlotBackground;
        end;
        ModeSelectCurvePositions:
        begin
            CheckAssigned(FCurvePositions, 'the curve positions the position picking mode shows');
            PlotCurvePositions;
        end;
        ModeSelectRFactorBounds:
        begin
            CheckAssigned(FRFactorBounds, 'the interval bounds the bounds picking mode shows');
            PlotRFactorBounds;
        end;
    end;
end;

procedure TFitClient.AddPointToActive(XValue, YValue: double);
begin
    LogUiAction(Format('point picked in mode %s: (%g, %g)',
        [SelModeName(FSelectionMode), XValue, YValue]));
    case FSelectionMode of
        ModeSelectIntervalBounds: AddPointToSelected(XValue, YValue);
        ModeSelectCharacteristicPoints: AddPointToSelected(XValue, YValue);
        ModeSelectCurveBounds: AddPointToSelected(XValue, YValue);
        ModeSelectBackground: AddPointToBackground(XValue, YValue);
        ModeSelectCurvePositions: AddPointToCurvePositions(XValue, YValue);
        ModeSelectRFactorBounds: AddPointToRFactorBounds(XValue, YValue);
        ModeSelectModulePoints: AddPointToModuleSet(XValue, YValue);
    end;
end;

function TFitClient.GetCurrentPointsSet: TTitlePointsSet;
begin
    case FSelectionMode of
        ModeSelectNothing:        Result := GetProfilePoints;
        ModeSelectIntervalBounds: Result := FSelectedPoints;
        ModeSelectCharacteristicPoints: Result := FSelectedPoints;
        ModeSelectCurveBounds: Result    := FSelectedPoints;
        ModeSelectBackground: Result     := FBackgroundPoints;
        ModeSelectCurvePositions: Result := FCurvePositions;
        ModeSelectRFactorBounds: Result  := FRFactorBounds;
        //  The picks are collected in the shared selected-points set, like the
        //  other two-pick modes; the patterns themselves live server-side.
        ModeSelectModulePoints: Result     := FSelectedPoints;
    end;
end;

procedure TFitClient.CopyProfileDataFromLoader;
begin
    CheckThat(Assigned(FDataLoader), 'the loader the profile data is copied from is missing');

    //  What the user was in the middle of doing was being done to the profile
    //  that is being replaced, and only the sets the server owns are read back
    //  by SendProfileToServer - these three are the client's own and have to be
    //  dropped here:
    //
    //  * an interval selected on the old data does not identify anything in the
    //    new data, and the server drops its own when the profile is set, so
    //    keeping the flag would leave the two disagreeing about what "the data"
    //    even means;
    //  * the picks of an unfinished two-pick gesture point at the old x-values.
    //
    //  The picks are CLEARED rather than freed: the set exists for as long as a
    //  selection mode is active, and the user may well still be in one.
    FSelectedAreaMode := False;
    RemoveSelectedArea;
    if Assigned(FSelectedPoints) then
        FSelectedPoints.Clear;

    ClearExpProfile;
    SetExpProfile(FDataLoader.GetPointsSetCopy);
end;

{ Hands the loaded profile to the compute server. The file itself is read and
  displayed by the client, so a server that is down must not cost the user the
  data: it is reported, and the profile is pushed again when a server is set.
  (This is what the original "called last, so a server exception does not
  interrupt the client" comment intended.) }
procedure TFitClient.SendProfileToServer;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');
    CheckAssigned(FExperimentalProfile, 'the experimental profile being sent to the engine');
    try
        FitService.SetProfilePointsSet(FExperimentalProfile);
        //  THE MARKUP BELONGS TO THE PROFILE THAT WAS JUST REPLACED. Taking a
        //  new profile resets the problem server-side - background points,
        //  curve positions, data intervals, pattern bounds and every computed
        //  result are picks on, or products of, the data being thrown away. The
        //  copies held here are only a view of that state, so they are read
        //  back rather than cleared by hand: the sets shown come from the
        //  server, which is the one place that decides what survives a reload.
        //  Without this the chart and the tables kept showing the previous
        //  file's markup over the new data, and the next pick was made against
        //  it.
        UpdateComputedData(True);
    except
        on E: Exception do
            if Assigned(main_calc_thread.OnCalcError) then
                main_calc_thread.OnCalcError(E.Message)
            else
                raise;
    end;
end;

procedure TFitClient.LoadDataSet(FileName: string);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    LogUiAction('loading data file ' + FileName);
    FDataLoader := FDataLoaderInjector.CreateDataLoader(FileName);
    FDataLoader.LoadDataSet(FileName);
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  An exception on the server must not interrupt the sequence running in
    //  the client, so this is called last.
    SendProfileToServer;
end;

procedure TFitClient.ResyncFromService;
begin
    LogClientState('re-reading everything from the engine');
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    //  WHETHER A SUB-INTERVAL IS IN FORCE IS THE ENGINE'S ANSWER, not this
    //  client's memory of it: a restored problem was windowed by the restore,
    //  which never went through this object. Asked before ShowProfile, which
    //  reads the flag to decide which of the two profiles to draw.
    FSelectedAreaMode := False;
    if Assigned(FitService.GetSelectedProfileInterval) then
        FSelectedAreaMode := FitService.GetSelectedProfileInterval.PointsCount > 0;

    //  Drops and re-reads, which is what ShowProfile is for.
    ShowProfile;

    RemoveBackgroundPoints;
    FBackgroundPoints := FitService.GetBackgroundPoints;
    if Assigned(FBackgroundPoints) then
    begin
        FBackgroundPoints.FTitle := BackgroundPointsName;
        FBackgroundPoints.WaveLength := FWaveLength;
        PlotBackground;
    end;

    FOpenState := OpenSuccess;
    //  The derived sets, the picks and the tables. Last, because it reads the
    //  profile this has just re-read.
    UpdateComputedData(True);
end;

procedure TFitClient.StartEmpty;
begin
    LogClientState('starting an empty problem');
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    ClearExpProfile;
    RemoveBackgroundPoints;
    RemoveSelectedPoints;
    RemoveRFactorBounds;
    RemoveCurvePositions;
    RemoveResultedCurvePositions;
    RemoveComputedProfile;
    RemoveDeltaProfile;
    FSelectedAreaMode := False;
    FSelectionMode := ModeSelectNothing;
    //  NOTHING IS OPEN NOW, which is what turns the commands off - the window
    //  derives every one of them from this.
    FOpenState := OpenFailure;
    Clear;

    //  THE ENGINE IS DELIBERATELY LEFT ALONE, and this is worth stating because
    //  the obvious alternative was tried and is wrong. Setting a profile is what
    //  resets a problem, but an EMPTY profile is refused - correctly, because an
    //  empty profile is not data - so there is no "empty the problem" call to
    //  make here.
    //
    //  Nothing needs one. Whatever comes next begins by setting a profile: a
    //  data file through SendProfileToServer, a project through the restore's
    //  first step. Both reset the problem, so the leftovers cannot survive into
    //  what follows, and until then nothing can reach them - every command is
    //  off while nothing is open.
end;

procedure TFitClient.Reload;
begin
    //  Reaching this without a service is a programming error, not a
    //  situation to recover from.
    CheckThat(Assigned(FDataLoader), 'the loader that read the data set being reloaded is missing');
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FDataLoader.Reload;
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  An exception on the server must not interrupt the sequence running in
    //  the client, so this is called last.
    SendProfileToServer;
end;

procedure TFitClient.SmoothProfile;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SmoothProfile;
    ShowProfile;
end;

procedure TFitClient.SubtractBackground(Auto: boolean);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    if not Auto then
    begin
        CheckAssigned(FBackgroundPoints, 'the background points to subtract');
        FitService.SetBackgroundPointsSet(FBackgroundPoints);
    end;
    FitService.SubtractBackground(Auto);
    //  ochistka spiska i skrytie grafika
    RemoveBackgroundPoints;
    //  perezagruzka dannyh
    ClearExpProfile;
    SetExpProfile(FitService.GetProfilePointsSet);
    PlotExpProfile;
end;

procedure TFitClient.DoAllAutomatically;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Clear;
    RunAsync(FitService.DoAllAutomatically, Done);
end;

procedure TFitClient.MinimizeDifference;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Clear;
    { Curve positions and R-factor bounds are set by AddPointToCurvePositions,
      AddPointToRFactorBounds. }
    RunAsync(FitService.MinimizeDifference, Done);
end;

procedure TFitClient.MinimizeNumberOfCurves;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Clear;
    { Curve positions and R-factor bounds are set by AddPointToCurvePositions,
      AddPointToRFactorBounds. }
    RunAsync(FitService.MinimizeNumberOfCurves, Done);
end;

procedure TFitClient.ComputeCurveBounds;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    RunAsync(FitService.ComputeCurveBounds, ComputeCurveBoundsDone);
end;

procedure TFitClient.ComputeBackgroundPoints;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    RunAsync(FitService.ComputeBackgroundPoints, ComputeBackgroundPointsDone);
end;

procedure TFitClient.ComputeCurvePositions;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    RunAsync(FitService.ComputeCurvePositions, ComputeCurvePositionsDone);
end;

procedure TFitClient.SelectAllPointsAsCurvePositions;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    RunAsync(FitService.SelectAllPointsAsCurvePositions, ComputeCurvePositionsDone);
end;

procedure TFitClient.CreateCurveList;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.CreateCurveList;
end;

procedure TFitClient.StopAsyncOper;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    LogUiAction('stop requested for the running server operation');
    FitService.StopAsyncOper;
    //  Nothing is read back here: Done is called the usual way.
end;

function TFitClient.AsyncOper: boolean;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.AsyncOper;
end;

function TFitClient.GetCalcTimeStr: string;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetCalcTimeStr;
end;

function TFitClient.GetRFactorStr: string;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetRFactorStr;
end;

function TFitClient.GetStatistics: TFitStatistics;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetStatistics;
end;

function TFitClient.GetMaxRFactor: double;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetMaxRFactor;
end;

procedure TFitClient.SetMaxRFactor(AMaxRFactor: double);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetMaxRFactor(AMaxRFactor);
end;

function TFitClient.GetBackFactor: double;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetBackFactor;
end;

procedure TFitClient.SetBackFactor(ABackFactor: double);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetBackFactor(ABackFactor);
end;

function TFitClient.GetCurveThresh: double;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetCurveThresh;
end;

procedure TFitClient.SetCurveThresh(ACurveThresh: double);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetCurveThresh(ACurveThresh);
end;

function TFitClient.GetCurveType: TCurveTypeId;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetCurveType;
end;

procedure TFitClient.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetCurveType(ACurveTypeId);
end;

procedure TFitClient.SelectCurveType(ACurveTypeId: TCurveTypeId);
var
    CurveTypeSelector: ICurveTypeSelector;
begin
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;
    CheckThat(Assigned(CurveTypeSelector), 'the curve type selector the client offers types through is missing');

    CurveTypeSelector.SelectCurveType(ACurveTypeId);
    SetCurveType(ACurveTypeId);
end;

{ Reads both flags off the server, once per server. See the fields for why they
  are not read per use, and why a failure is not retried until the client is
  pointed somewhere else.

  BOTH TOGETHER, because they are read together or not at all: one flag answered
  by this server beside one left over from the last is a state no reply describes.
  It is two requests - the service exposes the two as two getters over one route -
  and that is two per SERVER, not two per use, which is the whole point. }
procedure TFitClient.ReadServerFlags;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    if FServerFlagsRead then
        Exit;

    //  WHAT THE ENGINE STARTS A PROBLEM AT, and the two are NOT the same value
    //  (see TFitService's constructor) - which is why they are read rather than
    //  assumed. Seeded here rather than in a constructor because this class has
    //  two constructors and neither chains to the other, so one place that every
    //  path reaches is the only place that cannot be missed. Only ever visible
    //  before a server has answered once.
    FBackgroundVariationEnabled := False;
    FCurveScalingEnabled := True;

    try
        FBackgroundVariationEnabled :=
            FitService.GetBackgroundVariationEnabled;
        FCurveScalingEnabled := FitService.GetCurveScalingEnabled;
        FServerFlagsRead := True;
    except
        //  REPORTED AND SURVIVED, not propagated. The caller is a menu tick, and
        //  the transport has already logged what went wrong - this says what it
        //  cost. Raising instead is what put a fatal and a stack trace in the log
        //  of every client started before its server, out of an action's Update
        //  handler during FormCreate.
        on E: EUserException do
        begin
            LogClientWarning('the compute server did not answer for the ' +
                'background-variation and curve-scaling settings, so the menu ' +
                'shows what the engine starts a problem at: ' + E.Message);
            //  COUNTED AS ANSWERED, deliberately. Leaving it unread retries on
            //  the next use, and the next use is the idle loop: against a dead
            //  port that is a connect timeout per tick, with the window waiting
            //  for each one and a warning per tick in the log.
            //
            //  And the values are not wrong meanwhile: nothing but this class
            //  writes these two, so a problem nobody has touched still holds
            //  what the engine started it at - which is what was just seeded.
            //  Pointing the client at a server re-reads them, and toggling one
            //  writes through, so both routes out of here are correct.
            FServerFlagsRead := True;
        end;
    end;
end;

function TFitClient.GetBackgroundVariationEnabled: boolean;
begin
    ReadServerFlags;
    Result := FBackgroundVariationEnabled;
end;

procedure TFitClient.SetBackgroundVariationEnabled(AEnable: boolean);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetBackgroundVariationEnabled(AEnable);
    //  WRITTEN THROUGH, so what is held is what the server was just told. This
    //  also settles the cache: a value we set is a value we know, whether or not
    //  anything had been read before.
    FBackgroundVariationEnabled := AEnable;
    FServerFlagsRead := True;
end;

function TFitClient.GetMinimizerKind: longint;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetMinimizerKind;
end;

procedure TFitClient.SetMinimizerKind(AKind: longint);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetMinimizerKind(AKind);
end;

function TFitClient.GetLossKind: longint;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetLossKind;
end;

procedure TFitClient.SetLossKind(AKind: longint);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetLossKind(AKind);
end;

function TFitClient.GetWeighting: string;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetWeighting;
end;

procedure TFitClient.SetWeighting(const AValue: string);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetWeighting(AValue);
end;

function TFitClient.GetServerUrl: string;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    Result := FitService.GetServerUrl;
end;

procedure TFitClient.SetServerUrl(const AUrl: string);
var
    Changed: boolean;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    //  ASKED BEFORE THE CHANGE, because afterwards there is nothing left to
    //  compare with. A DIFFERENT server means a different problem, so whatever
    //  is cached describes a problem this client no longer has. The SAME server
    //  again must change nothing, which is what start-up does when it applies
    //  the configured URL - and is why this compares rather than always clearing.
    Changed := AUrl <> FitService.GetServerUrl;
    FitService.SetServerUrl(AUrl);
    if Changed then
        FServerFlagsRead := False;
end;

function TFitClient.GetCurveScalingEnabled: boolean;
begin
    ReadServerFlags;
    Result := FCurveScalingEnabled;
end;

procedure TFitClient.SetCurveScalingEnabled(AEnabled: boolean);
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetCurveScalingEnabled(AEnabled);
    FCurveScalingEnabled := AEnabled;
    FServerFlagsRead := True;
end;

procedure TFitClient.ClearSpecialCurve;
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.ClearSpecialCurve;
end;

procedure TFitClient.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
    //  pervonachal'nuyu initsializatsiyu
    );
begin
    CheckThat(Assigned(FitService), 'the fitting engine this client talks to is missing');

    FitService.SetSpecialCurveParameters(ACurveExpr, CP);
end;

end.
