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
    int_fit_service, int_fit_viewer, mscr_specimen_list, named_points_set,
    neutron_points_set, self_copied_component, SysUtils, title_points_set
{$IFDEF FIT}
    , fit_service
{$ENDIF}
{$IFDEF _WINDOWS}
    , persistent_curve_parameters
{$ENDIF}
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
        ModeSelectRFactorBounds);
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
        { Containers of calculated curves. Each object contains data of specimen curve. }
        FCurvesList:       TSelfCopiedCompList;
        { Containers of parameters of curves. }
        FCurveList:        TMSCRCurveList;
        { TODO: remove this attribute. }
        FWaveLength:       double;
        procedure SetCurvesListLambda;

    protected
        FCurMin:         double;
        { If True then in all operations only data belonging to selected ared are used
          otherwise all profile data are used. }
        FSelectedAreaMode: boolean;
        FSelectionMode: TSelMode;
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

        { Updates all the data and refreshes chart. }
        procedure UpdateComputedData(ShowExtraData: boolean);
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

        procedure SetSelectionMode(ASelectionMode: TSelMode);
        function GetSelectionMode: TSelMode;

        function GetMaxRFactor: double;
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetBackFactor: double;
        procedure SetBackFactor(ABackFactor: double);
        function GetCurveThresh: double;
        procedure SetCurveThresh(ACurveThresh: double);
        function GetCurveType: TCurveTypeId;
{$IFNDEF FIT}
        procedure SetCurveType(ACurveType: TCurveTypeId);
{$ENDIF}

        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);

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

        function GetCurveList: TMSCRCurveList;

        { Returns full profile or part of profile selected at the moment. }
        function GetProfilePoints: TTitlePointsSet;
        function GetBackgroundPoints: TNeutronPointsSet;
        function GetSelectedPoints: TNeutronPointsSet;
        function GetRFactorBounds: TNeutronPointsSet;
        function GetCurvePositions: TNeutronPointsSet;
{$IFDEF _WINDOWS}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means first initialization. }
            CP: Curve_parameters);
{$ENDIF}
        { Do only cleaning of sets. }

        procedure RemoveRFactorBounds;
        procedure RemoveCurvePositions;
        procedure RemoveBackgroundPoints;

        { All call AddPoint method. }

        procedure AddPointToSelected(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        { All call ReplacePoint method. }

        procedure ReplacePointInProfile(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInSelected(
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
        procedure Reload;

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
        procedure CreateCurveList;
{$IFDEF FIT}
        function GetFitProxy: TFitService;
{$ENDIF}

        { Server attributes. }

        property BackgroundVariationEnabled: boolean
            read GetBackgroundVariationEnabled write SetBackgroundVariationEnabled;

        property CurveScalingEnabled: boolean
            read GetCurveScalingEnabled write SetCurveScalingEnabled;

        property MaxRFactor: double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: double read GetBackFactor write SetBackFactor;
        property CurveThresh: double read GetCurveThresh write SetCurveThresh;
        property CurveTypeId: TCurveTypeId read GetCurveType
{$IFNDEF FIT} write SetCurveType
{$ENDIF}            ;

        { Plotting events are called from methods of the same name for providing
          synchronization with main application thread.
          Point to methods of TIIViewer. }

        { Callbacks for updating user interface. They are called from main thread
          of client application. Callbacks can throw exceptions.
          They can be not assigned (nil). }
        property OnAsyncOperationFinished: TAsyncOperationFinished
            read FAsyncOperationFinished write FAsyncOperationFinished;

        property SelectionMode: TSelMode read FSelectionMode write SetSelectionMode;
        property OpenState: TOpenState read FOpenState;
        property AsyncState: TAsyncState read FAsyncState;
        property SelectedAreaMode: boolean read FSelectedAreaMode;
        property AnimationMode: boolean read FAnimationMode write FAnimationMode;

        property FitService: IFitService read FFitService write FFitService;
    end;

const
    RFactorBoundsName: string = 'Spec.app.intervals';
    BackgroundPointsName: string = 'Background points';
    CurvePositionsName: string = 'Spec.positions';
    SelectedAreaName: string = 'Selected interval';
    SummarizedName: string = 'Summarized';
    ArgumentName: string = 'Position';
    ProfileName: string = 'Data';
    NumberName: string = 'Number';
    ValueName: string = 'Amplitude';
    DeltaName: string = 'Difference';
    StartName: string = 'Starting Position';
    StopName: string = 'Final Position';

implementation

{$IFDEF FIT}
uses
    app;

{$ENDIF}

{================================ TFitClient ==================================}

{$IFDEF FIT}
function TFitClient.GetFitProxy: TFitService;
begin
    Result := FitServerApp_.FitStub;
end;

{$ENDIF}

destructor TFitClient.Destroy;
begin
    FCurveList.Free;
    FBackgroundPoints.Free;
    FSelectedArea.Free;
    FCurvesList.Free;
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
    FRFactorBounds.FTitle := RFactorBoundsName;
    FRFactorBounds.WaveLength := FWaveLength;
end;

function TFitClient.GetBackgroundPoints: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FBackgroundPoints;
end;

function TFitClient.GetSelectedPoints: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FSelectedPoints;
end;

function TFitClient.GetRFactorBounds: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FRFactorBounds;
end;

function TFitClient.GetCurvePositions: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FCurvePositions;
end;

{$IFDEF _WINDOWS}
function TFitClient.GetSpecialCurveParameters: Curve_parameters;
begin
    Result := FitService.GetSpecialCurveParameters;
end;

{$ENDIF}

procedure TFitClient.SelectProfileIntervalActual(ANeutronPoints: TNeutronPointsSet;
    StartPointIndex, StopPointIndex: longint);
var
    i: longint;
begin
    Assert(Assigned(ANeutronPoints));
    Assert(ANeutronPoints.PointsCount <> 0);
    if (StartPointIndex < 0) or (StopPointIndex >
        ANeutronPoints.PointsCount - 1) then
        Assert(False);
    Assert(ANeutronPoints <> FSelectedArea);

    RemoveSelectedArea;
    FSelectedArea := TTitlePointsSet.Create(nil);
    try
        FSelectedArea.WaveLength := FWaveLength;
        FSelectedArea.FTitle := SelectedAreaName;
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
    Assert(Assigned(FExperimentalProfile));
    Assert(Assigned(FitService));

    FitService.SelectProfileInterval(StartPointIndex, StopPointIndex);
    Clear;
    SelectProfileIntervalActual(FExperimentalProfile, StartPointIndex, StopPointIndex);
    PlotSelectedProfileInterval;

    FSelectedAreaMode := True;
end;

procedure TFitClient.SelectEntireProfile;
begin
    Assert(Assigned(FExperimentalProfile));
    Assert(Assigned(FitService));

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
    Assert(Assigned(AExpProfile));

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

procedure TFitClient.RemoveCurvePositions;
begin
    if Assigned(FCurvePositions) then
    begin
        HideCurvePositions;
        FCurvePositions.Free;
        FCurvePositions := nil;
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
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
        RemoveSelectedArea;
        FSelectedArea := FitService.GetSelectedProfileInterval;
        FSelectedArea.WaveLength := FWaveLength;
        FSelectedArea.FTitle := SelectedAreaName;
        PlotSelectedProfileInterval;
    end
    else
    begin
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
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
    if Assigned(FFitViewer) then
    begin
{$IFDEF USE_GRIDS}
        FFitViewer.SetUpdateGrids(ShowExtraData);
{$ENDIF}
{$IFDEF USE_LEGEND}
        FFitViewer.SetUpdateLegends(ShowExtraData);
{$ENDIF}
    end;

    RemoveComputedProfile;
    FComputedProfile := FitService.GetCalcProfilePointsSet;
    if Assigned(FComputedProfile) and (FComputedProfile.PointsCount <> 0) then
    begin
        FComputedProfile.FTitle := SummarizedName;
        FComputedProfile.WaveLength := FWaveLength;
        PlotComputedProfile;
    end;

    RemoveDeltaProfile;
    FDeltaProfile := FitService.GetDeltaProfilePointsSet;
    if Assigned(FDeltaProfile) and (FDeltaProfile.PointsCount <> 0) then
    begin
        FDeltaProfile.FTitle := DeltaName;
        FDeltaProfile.WaveLength := FWaveLength;
        PlotDeltaProfile;
    end;

    RemoveCurvePositions;
    RemoveRFactorBounds;

    if ShowExtraData then
    begin
        FCurvePositions := FitService.GetCurvePositions;
        if Assigned(FCurvePositions) and (FCurvePositions.PointsCount <> 0) then
        begin
            FCurvePositions.FTitle := CurvePositionsName;
            FCurvePositions.WaveLength := FWaveLength;
            PlotCurvePositions;
        end;

        FRFactorBounds := FitService.GetRFactorBounds;
        if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
        begin
            FRFactorBounds.FTitle := RFactorBoundsName;
            FRFactorBounds.WaveLength := FWaveLength;
            PlotRFactorBounds;
        end;
    end;

    HideCurves;
    FCurvesList.Free;
    FCurvesList := FitService.GetCurvesList;
    if Assigned(FCurvesList) then
        SetCurvesListLambda;

    FCurveList.Free;
    FCurveList := FitService.GetCurveList;
    if Assigned(FCurveList) then
        FCurveList.FWaveLength := FWaveLength;

    PlotCurves;
{$IFDEF USE_GRIDS}
    if ShowExtraData then
    begin
        FillDatasheetTable;
    end;
{$ENDIF}
end;

procedure TFitClient.Done;
begin
    Assert(Assigned(FitService));

    ShowProfile;
    UpdateComputedData(True);
    FAsyncState := AsyncDone;

    //  Updates UI.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeCurveBoundsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitService));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorBounds;
    FRFactorBounds := FitService.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := RFactorBoundsName;
        FRFactorBounds.WaveLength := FWaveLength;

        PlotRFactorBounds;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeBackgroundPointsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitService));

    RemoveBackgroundPoints;
    FBackgroundPoints := FitService.GetBackgroundPoints;
    if Assigned(FBackgroundPoints) and (FBackgroundPoints.PointsCount <> 0) then
    begin
        FBackgroundPoints.FTitle := BackgroundPointsName;
        FBackgroundPoints.WaveLength := FWaveLength;

        PlotBackground;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.ComputeCurvePositionsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitService));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorBounds;
    FRFactorBounds := FitService.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := RFactorBoundsName;
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
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

function TFitClient.GetCurveList: TMSCRCurveList;
begin
    Assert(Assigned(FCurveList));
    Result := FCurveList;
end;

procedure TFitClient.SetCurvesListLambda;
var
    i:  longint;
    PointsSet: TNeutronPointsSet;
begin
    Assert(Assigned(FCurvesList));

    with FCurvesList do
        for i := 0 to FCurvesList.Count - 1 do
        begin
            PointsSet := TNeutronPointsSet(FCurvesList.Items[i]);
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
    if Assigned(FCurvesList) then
        SetCurvesListLambda;
    if Assigned(FCurveList) then
        FCurveList.FWaveLength := FWaveLength;
end;

function TFitClient.GetWaveLength: double;
begin
    Result := FWaveLength;
end;

procedure TFitClient.ReplacePoint(Points: TTitlePointsSet;
    PrevXValue, PrevYValue, NewXValue, NewYValue: double; Plot: TPlotProc);
begin
    Assert(Assigned(Points));

    Points.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    //  vyvodit' nuzhno v lyubom sluchae dlya
    //  ochistki poley posle nepravil'nogo vvoda
    Plot;
end;

// !!! povtornyy vyzov dlya dannyh koordinat udalyaet tochku iz spiska !!!
procedure TFitClient.AddPoint(var Points: TTitlePointsSet;
    XValue, YValue: double; Plot: TPlotProc);
var
    i: longint;
begin
    Assert(Assigned(Points));

    // ischem zadannuyu tochku v vybrannom spiske tochek
    for i := 0 to Points.PointsCount - 1 do
        if XValue = Points.PointXCoord[i] then
        begin
            // nuzhno korrektno udalit' spisok tochek iz spiska vseh krivyh,
            // potomu chto DeletePoints izmenyaet znachenie uk-lya Points...
            //FToRefresh := Points;
            //Hide;

            Points.DeletePoint(XValue);
            // ...i posle etogo otobrazit' ego zanovo
            Plot;
            Exit;
        end;
    // !!! dvuh tochek s odinakovymi X i raznymi Y vse ravno byt'
    // ne mozhet, poetomu proveryaetsya tol'ko koordinata X !!!

    // tochka ne naydena - dobavlyaem novuyu
    Points.AddNewPoint(XValue, YValue);
    Plot;
end;

procedure TFitClient.PlotCurves;
begin
    if Assigned(FFitViewer) and Assigned(FCurveList) then
        FFitViewer.PlotCurves(Self, FCurvesList, FCurveList);
end;

procedure TFitClient.HideCurves;
var
    i: longint;
begin
    if Assigned(FCurvesList) then
        for i := 0 to FCurvesList.Count - 1 do
        begin
            Hide(TNeutronPointsSet(FCurvesList.Items[i]));
        end;
end;

procedure TFitClient.PlotSelectedPoints;
begin
    if Assigned(FFitViewer) and Assigned(FSelectedPoints) then
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

{$IFDEF USE_GRIDS}
procedure TFitClient.FillDatasheetTable;
begin
    if Assigned(FFitViewer) then
        FFitViewer.FillDatasheetTable(FExperimentalProfile, FCurvesList,
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
        Assert(Assigned(FSelectedArea));
        ReplacePoint(FSelectedArea,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedProfileInterval);
    end
    else
    begin
        Assert(Assigned(FExperimentalProfile));
        ReplacePoint(FExperimentalProfile,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotExpProfile);
    end;
    FitService.ReplacePointInProfile(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitClient.ReplacePointInSelected(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FSelectedPoints));
    ReplacePoint(FSelectedPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedPoints);
end;

procedure TFitClient.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FBackgroundPoints));
    ReplacePoint(FBackgroundPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotBackground);
    FitService.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitClient.ReplacePointInRFactorBounds(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FRFactorBounds));
    ReplacePoint(FRFactorBounds,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotRFactorBounds);
end;

procedure TFitClient.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FCurvePositions));
    ReplacePoint(FCurvePositions,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotCurvePositions);
end;

procedure TFitClient.AddPointToSelected(XValue, YValue: double);
begin
    Assert(Assigned(FSelectedPoints));
    AddPoint(FSelectedPoints, XValue, YValue, PlotSelectedPoints);
end;

procedure TFitClient.AddPointToBackground(XValue, YValue: double);
begin
    Assert(Assigned(FBackgroundPoints));
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

procedure TFitClient.SetSelectionMode(ASelectionMode: TSelMode);
begin
    case ASelectionMode of
        ModeSelectNothing:
            case FSelectionMode of
                ModeSelectIntervalBounds: RemoveSelectedPoints;
                ModeSelectCharacteristicPoints: RemoveSelectedPoints;
                ModeSelectCurveBounds: RemoveSelectedPoints;
            end;
        ModeSelectIntervalBounds:
            RecreateAndShowSelectedPoints('Area Limits');
        ModeSelectCharacteristicPoints:
            RecreateAndShowSelectedPoints('Characteristic Points');
        ModeSelectCurveBounds:
            RecreateAndShowSelectedPoints('Curve Bounds');
        ModeSelectBackground:
        begin
            Assert(Assigned(FBackgroundPoints));
            PlotBackground;
        end;
        ModeSelectCurvePositions:
        begin
            Assert(Assigned(FCurvePositions));
            PlotCurvePositions;
        end;
        ModeSelectRFactorBounds:
        begin
            Assert(Assigned(FRFactorBounds));
            PlotRFactorBounds;
        end;
    end;
    FSelectionMode := ASelectionMode;
end;

function TFitClient.GetSelectionMode: TSelMode;
begin
    Result := FSelectionMode;
end;

procedure TFitClient.AddPointToActive(XValue, YValue: double);
begin
    case FSelectionMode of
        ModeSelectIntervalBounds: AddPointToSelected(XValue, YValue);
        ModeSelectCharacteristicPoints: AddPointToSelected(XValue, YValue);
        ModeSelectCurveBounds: AddPointToSelected(XValue, YValue);
        ModeSelectBackground: AddPointToBackground(XValue, YValue);
        ModeSelectCurvePositions: AddPointToCurvePositions(XValue, YValue);
        ModeSelectRFactorBounds: AddPointToRFactorBounds(XValue, YValue);
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
    end;
end;

procedure TFitClient.CopyProfileDataFromLoader;
begin
    Assert(Assigned(FDataLoader));

    ClearExpProfile;
    SetExpProfile(FDataLoader.GetPointsSetCopy);
end;

procedure TFitClient.LoadDataSet(FileName: string);
begin
    Assert(Assigned(FitService));

    FDataLoader := FDataLoaderInjector.CreateDataLoader(FileName);
    FDataLoader.LoadDataSet(FileName);
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitService.SetProfilePointsSet(FExperimentalProfile);
end;

procedure TFitClient.Reload;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FDataLoader));
    Assert(Assigned(FitService));

    FDataLoader.Reload;
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitService.SetProfilePointsSet(FExperimentalProfile);
end;

procedure TFitClient.SmoothProfile;
begin
    Assert(Assigned(FitService));

    FitService.SmoothProfile;
    ShowProfile;
end;

procedure TFitClient.SubtractBackground(Auto: boolean);
begin
    Assert(Assigned(FitService));

    if not Auto then
    begin
        Assert(Assigned(FBackgroundPoints));
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
    Assert(Assigned(FitService));

    Clear;
    FitService.DoAllAutomatically;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.MinimizeDifference;
begin
    Assert(Assigned(FitService));

    Clear;
    { Curve positions and R-factor bounds are set by AddPointToCurvePositions,
      AddPointToRFactorBounds. }
    FitService.MinimizeDifference;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.MinimizeNumberOfCurves;
begin
    Assert(Assigned(FitService));

    Clear;
    { Curve positions and R-factor bounds are set by AddPointToCurvePositions,
      AddPointToRFactorBounds. }
    FitService.MinimizeNumberOfCurves;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.ComputeCurveBounds;
begin
    Assert(Assigned(FitService));

    FitService.ComputeCurveBounds;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.ComputeBackgroundPoints;
begin
    Assert(Assigned(FitService));

    FitService.ComputeBackgroundPoints;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.ComputeCurvePositions;
begin
    Assert(Assigned(FitService));

    FitService.ComputeCurvePositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.SelectAllPointsAsCurvePositions;
begin
    Assert(Assigned(FitService));

    FitService.SelectAllPointsAsCurvePositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.CreateCurveList;
begin
    Assert(Assigned(FitService));

    FitService.CreateCurveList;
end;

procedure TFitClient.StopAsyncOper;
begin
    Assert(Assigned(FitService));

    FitService.StopAsyncOper;
    //  izvlechenie dannyh zdes' delat' ne nuzhno,
    //  poskol'ku Done vyzyvatsya standartnym obrazom
end;

function TFitClient.AsyncOper: boolean;
begin
    Assert(Assigned(FitService));

    Result := FitService.AsyncOper;
end;

function TFitClient.GetCalcTimeStr: string;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetCalcTimeStr;
end;

function TFitClient.GetRFactorStr: string;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetRFactorStr;
end;

function TFitClient.GetMaxRFactor: double;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetMaxRFactor;
end;

procedure TFitClient.SetMaxRFactor(AMaxRFactor: double);
begin
    Assert(Assigned(FitService));

    FitService.SetMaxRFactor(AMaxRFactor);
end;

function TFitClient.GetBackFactor: double;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetBackFactor;
end;

procedure TFitClient.SetBackFactor(ABackFactor: double);
begin
    Assert(Assigned(FitService));

    FitService.SetBackFactor(ABackFactor);
end;

function TFitClient.GetCurveThresh: double;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetCurveThresh;
end;

procedure TFitClient.SetCurveThresh(ACurveThresh: double);
begin
    Assert(Assigned(FitService));

    FitService.SetCurveThresh(ACurveThresh);
end;

function TFitClient.GetCurveType: TCurveTypeId;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetCurveType;
end;

{$IFNDEF FIT}
procedure TFitClient.SetCurveType(ACurveType: TCurveTypeId);
begin
    Assert(Assigned(FitProxy));

    FitProxy.SetCurveType(ACurveType);
end;

{$ENDIF}

function TFitClient.GetBackgroundVariationEnabled: boolean;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetBackgroundVariationEnabled;
end;

procedure TFitClient.SetBackgroundVariationEnabled(AEnable: boolean);
begin
    Assert(Assigned(FitService));

    FitService.SetBackgroundVariationEnabled(AEnable);
end;

function TFitClient.GetCurveScalingEnabled: boolean;
begin
    Assert(Assigned(FitService));

    Result := FitService.GetCurveScalingEnabled;
end;

procedure TFitClient.SetCurveScalingEnabled(AEnabled: boolean);
begin
    Assert(Assigned(FitService));

    FitService.SetCurveScalingEnabled(AEnabled);
end;

{$IFDEF _WINDOWS}
procedure TFitClient.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
    //  pervonachal'nuyu initsializatsiyu
    );
begin
    Assert(Assigned(FitService));

    FitService.SetSpecialCurveParameters(ACurveExpr, CP);
end;

{$ENDIF}
end.
