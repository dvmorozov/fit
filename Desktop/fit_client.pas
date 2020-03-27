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
    , fit_server
{$ENDIF}
{$IFDEF _WINDOWS}
    , persistent_curve_parameters
{$ENDIF}
    ;

type
    { Modes of selectiion of active point set. }
    TSelMode    = (ModeSelNone, ModeSelAreaLimits, ModeSelCharacteristicPoints,
        ModeSelGaussianBounds, ModeSelBackground, ModeSelPeakPos,
        ModeSelPeakBounds);
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

    { Handler to fill data table. }
    TFillDatasheetTable = procedure(Profile: TTitlePointsSet;
        CurvesList: TSelfCopiedCompList; GaussProfile: TTitlePointsSet;
        DeltaProfile: TTitlePointsSet; RFactorIntervals: TTitlePointsSet) of
        object;
    { Handler drawing points selected by user. }
    TPlotSelectedPoints = procedure(Sender: TObject;
        SelectedPoints: TTitlePointsSet) of object;
    { Handler displaying data intervals. }
    TPlotRFactorIntervals = procedure(Sender: TObject;
        RFactorIntervals: TTitlePointsSet) of object;
    { Handler hiding data intervals. }
    THideRFactorIntervals = procedure(Sender: TObject;
        RFactorIntervals: TTitlePointsSet) of object;
    { Handler displaying curve positions. }
    TPlotCurvePositions = procedure(Sender: TObject;
        CurvePositions: TTitlePointsSet) of object;
    { Handler hiding curve positions. }
    THideCurvePositions = procedure(Sender: TObject;
        CurvePositions: TTitlePointsSet) of object;
    { Handler displaying background curve. }
    TPlotBackground = procedure(Sender: TObject;
        BackgroundPoints: TTitlePointsSet) of object;
    { Handler hiding background curve. }
    THideBackground = procedure(Sender: TObject;
        BackgroundPoints: TTitlePointsSet) of object;
    { Handler displaying data curve. }
    TPlotDataPoints = procedure(Sender: TObject;
        DataPoints: TTitlePointsSet) of object;
    { Handler hiding data curve. }
    THideDataPoints = procedure(Sender: TObject;
        DataPoints: TTitlePointsSet) of object;

    TPlotSelectedArea = procedure(Sender: TObject;
        SelectedArea: TTitlePointsSet) of object;
    TPlotGaussProfile = procedure(Sender: TObject;
        GaussProfile: TTitlePointsSet) of object;
    TPlotDeltaProfile = procedure(Sender: TObject;
        DeltaProfile: TTitlePointsSet) of object;
    TRefresh = procedure(Sender: TObject) of object;
    TRefreshPointsSet = procedure(Sender: TObject;
        points_set: TNeutronPointsSet) of object;
    TClear = procedure(Sender: TObject) of object;
    THide  = procedure(Sender: TObject; points_set: TNeutronPointsSet) of object;
    TAsyncOperationFinished = procedure(Sender: TObject) of object;
    TPlotProc = procedure of object;

    { Implements all client logic of the application. Must be completely independent from UI. }
    TFitClient = class(TInterfacedObject, IClientCallback)
    protected
        FFitProxy: IFitService;

    protected
        FDataLoader:     IDataLoader;
        FDataLoaderInjector: IDataLoaderInjector;
        { All the data displayed on the chart. They are required to be able control of X-coordinate. }
        FNeutronPointsSet: TTitlePointsSet;
        { Region of given profile data with which user is working at the given moment. }
        FSelectedArea:   TTitlePointsSet;
        { Sum of all model curces which is compared with experimental data. }
        FCurveProfile:   TTitlePointsSet;
        FDeltaProfile:   TTitlePointsSet;
        { Set of points selected by user. }
        FSelectedPoints: TTitlePointsSet;
        { List of background points which is used for transmission between manual and automatic selection modes. }
        FBackgroundPoints: TTitlePointsSet;
        { List of point pairs which limit interval of R-factor calculation. 
          Always must be displayed in order to show user in which mode R-factor is calculated. }
        FRFactorIntervals: TTitlePointsSet;
        { Positions of curves. Only X-coordinates are used. }
        FCurvePositions: TTitlePointsSet;
        { Containers of calculated curves. Each object contains data of specimen curve. }
        FCurvesList:     TSelfCopiedCompList;
        { Containers of parameters of curves. }
        FSpecimenList:   TMSCRSpecimenList;
        { TODO: remove this attribute. }
        FWaveLength:     double;
        procedure SetCurvesListLambda;

    protected
        FCurMin:     double;
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

        { Is used in RefreshPointsSet, Hide. }
        FToRefresh: TNeutronPointsSet;

        { Updates all the data and refreshes chart. }
        procedure UpdateAll;
        procedure HideSpecimens;

        { Wrappers for calls to external displaying methods. 
          They are necessary to check that external interface methods are connected.
          Opposite means that there aren't corresponding GUI elements. }
        procedure PlotSpecimens;
        procedure PlotSelectedPoints;

        procedure PlotRFactorIntervals;
        procedure HideRFactorIntervals;

        procedure PlotCurvePositions;
        procedure HideCurvePositions;

        procedure PlotDataPoints;
        procedure HideDataPoints;

        procedure PlotSelectedArea;

        procedure PlotBackground;
        procedure HideBackground;

        procedure PlotGaussProfile;
        procedure PlotDeltaProfile;
        procedure Refresh;
        procedure RefreshPointsSet;
        procedure Clear;
        procedure Hide;
{$IFDEF USE_GRIDS}
        procedure FillDatasheetTable;
{$ENDIF}

        { Returns full profile or part of profile selected at the moment. }
        function GetProfilePointsSet: TTitlePointsSet;

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

        procedure InitDataPoints;
        procedure RemoveDataPoints;
        procedure RemoveSelectedPoints;
        procedure RemoveSelectedArea;
        procedure RemoveGaussProfile;
        procedure RemoveDeltaProfile;

        { Copies data from the given point set to the set of selected interval. }
        procedure SelectAreaActual(ANeutronPoints: TNeutronPointsSet;
            StartPointIndex, StopPointIndex: longint);
        procedure CopyProfileDataFromLoader;

    public
        FFitViewer: IFitViewer;

        function GetSpecimenList: TMSCRSpecimenList;

        function GetBackgroundPoints: TNeutronPointsSet;
        function GetSelectedPoints: TNeutronPointsSet;
        function GetRFactorIntervals: TNeutronPointsSet;
        function GetCurvePositions: TNeutronPointsSet;
{$IFDEF _WINDOWS}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means first initialization. }
            CP: Curve_parameters);
{$ENDIF}
        { Do only cleaning of sets. }

        procedure RemoveRFactorIntervals;
        procedure RemoveCurvePositions;
        procedure RemoveBackgroundPoints;

        { All call AddPoint method. }

        procedure AddPointToSelected(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorIntervals(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        { All call ReplacePoint method. }

        procedure ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInSelected(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorIntervals(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure AddPointToActive(XValue, YValue: double);
        { Returns a set with which user works at the moment. }
        function GetCurrentPointsSet: TTitlePointsSet;

        { Cleans chart and moves data from full profile to data of selected iterval. }
        procedure SelectArea(StartPointIndex, StopPointIndex: longint);
        procedure ReturnToTotalProfile;

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
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        { Wrappers for server methods. Mustn't create messages because this
          is responsibility of GUI. Instead of this must throw exceptions. }

        procedure SmoothProfile;
        procedure SubtractBackground(Auto: boolean);
        procedure DoAllAutomatically;
        procedure MinimizeDifference;
        procedure MinimizeNumberOfCurves;
        procedure ComputeCurveBounds;
        procedure ComputeBackgroundPoints;
        procedure FindPeakPositions;
        procedure AllPointsAsPeakPositions;
        procedure StopAsyncOper;
        { Gets state of asynchronous operation from the server. }
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        procedure CreateSpecimenList;
{$IFDEF FIT}
        function GetFitProxy: TFitServer;
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

        property FitProxy: IFitService read FFitProxy write FFitProxy;

        property NeutronPointsSet: TTitlePointsSet
            read FNeutronPointsSet write FNeutronPointsSet;
    end;

const
    RFactorIntervalsName: string = 'Spec.app.intervals';
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
    HintDone: string = 'Calculation done';

implementation

{$IFDEF FIT}
uses
    app;
{$ENDIF}

{================================ TFitClient ==================================}

{$IFDEF FIT}
function TFitClient.GetFitProxy: TFitServer;
begin
    Result := FitServerApp_.FitStub;
end;

{$ENDIF}

destructor TFitClient.Destroy;
begin
    FSpecimenList.Free;
    FBackgroundPoints.Free;
    FSelectedArea.Free;
    FCurvesList.Free;
    FDeltaProfile.Free;
    FCurveProfile.Free;
    FSelectedPoints.Free;
    NeutronPointsSet.Free;
    FRFactorIntervals.Free;
    inherited;
end;

constructor TFitClient.CreateWithInjector(ADataLoaderInjector: IDataLoaderInjector);
begin
    inherited;
    FDataLoaderInjector := ADataLoaderInjector;
    FSelectionMode := ModeSelNone;
    FOpenState  := OpenFailure;
    FAsyncState := AsyncStart;

    //  sozdayutsya pustye spiski, chtoby mozhno bylo
    //  vvodit' dannye vruchnuyu
    NeutronPointsSet := TTitlePointsSet.Create(nil);
    InitDataPoints;

    FCurvePositions := TTitlePointsSet.Create(nil);
    FCurvePositions.FTitle := CurvePositionsName;
    FCurvePositions.Lambda := FWaveLength;

    FBackgroundPoints := TTitlePointsSet.Create(nil);
    FBackgroundPoints.FTitle := BackgroundPointsName;
    FBackgroundPoints.Lambda := FWaveLength;

    FRFactorIntervals := TTitlePointsSet.Create(nil);
    FRFactorIntervals.FTitle := RFactorIntervalsName;
    FRFactorIntervals.Lambda := FWaveLength;
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

function TFitClient.GetRFactorIntervals: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FRFactorIntervals;
end;

function TFitClient.GetCurvePositions: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := FCurvePositions;
end;

{$IFDEF _WINDOWS}
function TFitClient.GetSpecialCurveParameters: Curve_parameters;
begin
    Result := FitProxy.GetSpecialCurveParameters;
end;

{$ENDIF}

procedure TFitClient.SelectAreaActual(ANeutronPoints: TNeutronPointsSet;
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
        FSelectedArea.Lambda := FWaveLength;
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

procedure TFitClient.SelectArea(StartPointIndex, StopPointIndex: longint);
begin
    Assert(Assigned(NeutronPointsSet));
    Assert(Assigned(FitProxy));

    FitProxy.SelectArea(StartPointIndex, StopPointIndex);
    Clear;
    SelectAreaActual(NeutronPointsSet, StartPointIndex, StopPointIndex);
    PlotSelectedArea;

    FSelectedAreaMode := True;
end;

procedure TFitClient.ReturnToTotalProfile;
begin
    Assert(Assigned(NeutronPointsSet));
    Assert(Assigned(FitProxy));

    FitProxy.ReturnToTotalProfile;
    Clear;
    PlotDataPoints;

    FSelectedAreaMode := False;
    FSelectedArea.Free;
    FSelectedArea := nil;
end;

procedure TFitClient.RecreateAndShowSelectedPoints(Title: string);
begin
    RemoveSelectedPoints;
    FSelectedPoints := TTitlePointsSet.Create(nil);
    FSelectedPoints.FTitle := Title;
    FSelectedPoints.Lambda := FWaveLength;
    PlotSelectedPoints;
end;

procedure TFitClient.InitDataPoints;
begin
    Assert(Assigned(NeutronPointsSet));
    NeutronPointsSet.FTitle := ProfileName;
    NeutronPointsSet.Lambda := FWaveLength;
end;

procedure TFitClient.RemoveSelectedPoints;
begin
    //  dopuskaetsya ravenstvo nil
    FToRefresh := FSelectedPoints;
    Hide;
    FSelectedPoints.Free;
    FSelectedPoints := nil;
end;

procedure TFitClient.RemoveDataPoints;
begin
    HideDataPoints;
    NeutronPointsSet.Clear; //  chtoby mozhno bylo dobavlyat' tochki vruchnuyu
end;

procedure TFitClient.RemoveGaussProfile;
begin
    //  dopuskaetsya ravenstvo nil
    FToRefresh := FCurveProfile;
    Hide;
    FCurveProfile.Free;
    FCurveProfile := nil;
end;

procedure TFitClient.RemoveDeltaProfile;
begin
    //  dopuskaetsya ravenstvo nil
    FToRefresh := FDeltaProfile;
    Hide;
    FDeltaProfile.Free;
    FDeltaProfile := nil;
end;

procedure TFitClient.RemoveSelectedArea;
begin
    //  dopuskaetsya ravenstvo nil
    FToRefresh := FSelectedArea;
    Hide;
    FSelectedArea.Free;
    FSelectedArea := nil;
end;

procedure TFitClient.RemoveRFactorIntervals;
begin
    HideRFactorIntervals;
    FRFactorIntervals.Clear; //  dlya posleduyuschego vvoda
end;

procedure TFitClient.RemoveCurvePositions;
begin
    HideCurvePositions;
    FCurvePositions.Clear;   //  dlya posleduyuschego vvoda
end;

procedure TFitClient.RemoveBackgroundPoints;
begin
    HideBackground;
    FBackgroundPoints.Clear; //  dlya posleduyuschego vvoda
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
        if FFitViewer.GetAnimationMode then
            UpdateAll;
    end;
end;

procedure TFitClient.ShowProfile;
begin
    if FSelectedAreaMode then
    begin
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
        RemoveSelectedArea;
        FSelectedArea := FitProxy.GetSelectedArea;
        //??? kak zdes' obrabatyvat' isklyucheniya
        FSelectedArea.Lambda := FWaveLength;
        FSelectedArea.FTitle := SelectedAreaName;
        PlotSelectedArea;
    end
    else
    begin
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
        RemoveDataPoints;
        NeutronPointsSet.Free;
        NeutronPointsSet := nil;
        NeutronPointsSet := FitProxy.GetProfilePointsSet;
        InitDataPoints;
        PlotDataPoints;
    end;
end;

function TFitClient.GetProfilePointsSet: TTitlePointsSet;
begin
    if FSelectedAreaMode then
        Result := FSelectedArea
    else
        Result := NeutronPointsSet;
end;

procedure TFitClient.UpdateAll;
begin
    RemoveGaussProfile;
    FCurveProfile := FitProxy.GetCalcProfilePointsSet;
    if Assigned(FCurveProfile) and (FCurveProfile.PointsCount <> 0) then
    begin
        FCurveProfile.FTitle := SummarizedName;
        FCurveProfile.Lambda := FWaveLength;
        PlotGaussProfile;
    end;

    RemoveDeltaProfile;
    FDeltaProfile := FitProxy.GetDeltaProfilePointsSet;
    if Assigned(FDeltaProfile) and (FDeltaProfile.PointsCount <> 0) then
    begin
        FDeltaProfile.FTitle := DeltaName;
        FDeltaProfile.Lambda := FWaveLength;
        PlotDeltaProfile;
    end;

    RemoveCurvePositions;
    FCurvePositions.Free;
    FCurvePositions := nil;
    FCurvePositions := FitProxy.GetCurvePositions;
    if Assigned(FCurvePositions) and (FCurvePositions.PointsCount <> 0) then
    begin
        FCurvePositions.FTitle := CurvePositionsName;
        FCurvePositions.Lambda := FWaveLength;
        PlotCurvePositions;
    end;

    RemoveRFactorIntervals; //  nuzhno skryvat', t.k. menyaetsya uk-l'
    FRFactorIntervals.Free;
    FRFactorIntervals := nil;
    FRFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(FRFactorIntervals) and (FRFactorIntervals.PointsCount <> 0) then
    begin
        FRFactorIntervals.FTitle := RFactorIntervalsName;
        FRFactorIntervals.Lambda := FWaveLength;
        PlotRFactorIntervals;
    end;

    HideSpecimens;
    FCurvesList.Free;
    FCurvesList := nil;
    FCurvesList := FitProxy.GetCurvesList;
    if Assigned(FCurvesList) then
        SetCurvesListLambda;

    FSpecimenList.Free;
    FSpecimenList := nil;
    FSpecimenList := FitProxy.GetSpecimenList;
    if Assigned(FSpecimenList) then
        FSpecimenList.Lambda := FWaveLength;

    PlotSpecimens;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillDatasheetTable;
{$ENDIF}
end;

procedure TFitClient.Done;
begin
    Assert(Assigned(FitProxy));

    ShowProfile;
    UpdateAll;
    FAsyncState := AsyncDone;

    //  Updates UI.
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
    if Assigned(FFitViewer) then
        FFitViewer.ShowHint(HintDone);
end;

procedure TFitClient.ComputeCurveBoundsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorIntervals;
    FRFactorIntervals.Free;
    FRFactorIntervals := nil;
    FRFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(FRFactorIntervals) and (FRFactorIntervals.PointsCount <> 0) then
    begin
        FRFactorIntervals.FTitle := RFactorIntervalsName;
        FRFactorIntervals.Lambda := FWaveLength;

        PlotRFactorIntervals;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.FindBackPointsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));

    RemoveBackgroundPoints;
    FBackgroundPoints.Free;
    FBackgroundPoints := nil;
    FBackgroundPoints := FitProxy.GetBackgroundPoints;
    if Assigned(FBackgroundPoints) and (FBackgroundPoints.PointsCount <> 0) then
    begin
        FBackgroundPoints.FTitle := BackgroundPointsName;
        FBackgroundPoints.Lambda := FWaveLength;

        PlotBackground;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

procedure TFitClient.FindPeakPositionsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorIntervals;
    FRFactorIntervals.Free;
    FRFactorIntervals := nil;
    FRFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(FRFactorIntervals) and (FRFactorIntervals.PointsCount <> 0) then
    begin
        FRFactorIntervals.FTitle := RFactorIntervalsName;
        FRFactorIntervals.Lambda := FWaveLength;
        PlotRFactorIntervals;
    end;

    RemoveCurvePositions;
    FCurvePositions.Free;
    FCurvePositions := nil;
    FCurvePositions := FitProxy.GetCurvePositions;
    if Assigned(FCurvePositions) and (FCurvePositions.PointsCount <> 0) then
    begin
        FCurvePositions.FTitle := CurvePositionsName;
        FCurvePositions.Lambda := FWaveLength;
        PlotCurvePositions;
    end;

    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then
        OnAsyncOperationFinished(Self);
end;

function TFitClient.GetSpecimenList: TMSCRSpecimenList;
begin
    Assert(Assigned(FSpecimenList));
    Result := FSpecimenList;
end;

procedure TFitClient.SetCurvesListLambda;
var
    i:  longint;
    NS: TNeutronPointsSet;
begin
    Assert(Assigned(FCurvesList));

    with FCurvesList do
        for i := 0 to FCurvesList.Count - 1 do
        begin
            NS := TNeutronPointsSet(FCurvesList.Items[i]);
            NS.Lambda := FWaveLength;
        end;
end;

procedure TFitClient.SetWaveLength(AWaveLength: double);
begin
    FWaveLength := AWaveLength;
    if Assigned(NeutronPointsSet) then
        NeutronPointsSet.Lambda := AWaveLength;
    if Assigned(FBackgroundPoints) then
        FBackgroundPoints.Lambda := AWaveLength;
    if Assigned(FSelectedArea) then
        FSelectedArea.Lambda := AWaveLength;
    if Assigned(FSelectedPoints) then
        FSelectedPoints.Lambda := AWaveLength;
    if Assigned(FCurveProfile) then
        FCurveProfile.Lambda := AWaveLength;
    if Assigned(FDeltaProfile) then
        FDeltaProfile.Lambda := AWaveLength;
    if Assigned(FCurvesList) then
        SetCurvesListLambda;
    if Assigned(FSpecimenList) then
        FSpecimenList.Lambda := FWaveLength;
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
        end// !!! dvuh tochek s odinakovymi X i raznymi Y vse ravno byt'
    // ne mozhet, poetomu proveryaetsya tol'ko koordinata X !!!
    ;
    // tochka ne naydena - dobavlyaem novuyu
    Points.AddNewPoint(XValue, YValue);
    Plot;
end;

procedure TFitClient.PlotSpecimens;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotSpecimens(Self, FCurvesList, FSpecimenList);
end;

procedure TFitClient.HideSpecimens;
var
    i: longint;
begin
    if Assigned(FCurvesList) then
        for i := 0 to FCurvesList.Count - 1 do
        begin
            FToRefresh := TNeutronPointsSet(FCurvesList.Items[i]);
            Hide;
        end;
end;

procedure TFitClient.PlotSelectedPoints;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotSelectedPoints(Self, FSelectedPoints);
end;

procedure TFitClient.PlotRFactorIntervals;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotRFactorIntervals(Self, FRFactorIntervals);
end;

procedure TFitClient.HideRFactorIntervals;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideRFactorIntervals(Self, FRFactorIntervals);
end;

procedure TFitClient.PlotCurvePositions;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotCurvePositions(Self, FCurvePositions);
end;

procedure TFitClient.HideCurvePositions;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideCurvePositions(Self, FCurvePositions);
end;

{$IFDEF USE_GRIDS}
procedure TFitClient.FillDatasheetTable;
begin
    if Assigned(FitViewer) then
        FitViewer.FillDatasheetTable(GetProfilePointsSet, CurvesList,
            GaussProfile, DeltaProfile, RFactorIntervals);
end;

{$ENDIF}

procedure TFitClient.PlotDataPoints;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.HideDataPoints;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.PlotSelectedArea;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotSelectedArea(Self, FSelectedArea);
end;

procedure TFitClient.PlotBackground;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotBackground(Self, FBackgroundPoints);
end;

procedure TFitClient.HideBackground;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideBackground(Self, FBackgroundPoints);
end;

procedure TFitClient.PlotGaussProfile;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotGaussProfile(Self, FCurveProfile);
end;

procedure TFitClient.PlotDeltaProfile;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotDeltaProfile(Self, FDeltaProfile);
end;

procedure TFitClient.Refresh;
begin
    if Assigned(FFitViewer) then
        FFitViewer.Refresh(Self);
end;

procedure TFitClient.RefreshPointsSet;
begin
    Assert(Assigned(FToRefresh));

    if Assigned(FFitViewer) then
        FFitViewer.RefreshPointsSet(Self, FToRefresh);
    FToRefresh := nil;
end;

procedure TFitClient.Clear;
begin
    if Assigned(FFitViewer) then
        FFitViewer.Clear(Self);
end;

procedure TFitClient.Hide;
begin
    if Assigned(FFitViewer) then
        FFitViewer.Hide(Self, FToRefresh);
    FToRefresh := nil;
end;

procedure TFitClient.ReplacePointInData(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    if FSelectedAreaMode then
    begin
        Assert(Assigned(FSelectedArea));
        ReplacePoint(FSelectedArea,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedArea);
    end
    else
    begin
        Assert(Assigned(NeutronPointsSet));
        ReplacePoint(NeutronPointsSet,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotDataPoints);
    end;
    FitProxy.ReplacePointInData(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitClient.ReplacePointInSelected(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FSelectedPoints));
    ReplacePoint(FSelectedPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedPoints);
    //???  vyzov servera
end;

procedure TFitClient.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FBackgroundPoints));
    ReplacePoint(FBackgroundPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotBackground);
    FitProxy.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitClient.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
begin
    Assert(Assigned(FRFactorIntervals));
    ReplacePoint(FRFactorIntervals,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotRFactorIntervals);
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

procedure TFitClient.AddPointToRFactorIntervals(XValue, YValue: double);
begin
    //Assert(Assigned(FRFactorIntervals));
    //AddPoint(FRFactorIntervals, XValue, YValue, PlotRFactorIntervals);

    FitProxy.AddPointToRFactorIntervals(XValue, YValue);
    UpdateAll;
end;

procedure TFitClient.AddPointToCurvePositions(XValue, YValue: double);
begin
    //Assert(Assigned(FCurvePositions));
    //Assert(Assigned(FRFactorIntervals));
    //AddPoint(FCurvePositions, XValue, YValue, PlotCurvePositions);

    FitProxy.AddPointToCurvePositions(XValue, YValue);
    UpdateAll;
end;

procedure TFitClient.SetSelectionMode(ASelectionMode: TSelMode);
begin
    case ASelectionMode of
        ModeSelNone:
            case FSelectionMode of
                ModeSelAreaLimits: RemoveSelectedPoints;
                ModeSelCharacteristicPoints: RemoveSelectedPoints;
                ModeSelGaussianBounds: RemoveSelectedPoints;
            end;
        ModeSelAreaLimits:
            RecreateAndShowSelectedPoints('Area Limits');
        ModeSelCharacteristicPoints:
            RecreateAndShowSelectedPoints('Characteristic Points');
        ModeSelGaussianBounds:
            RecreateAndShowSelectedPoints('Curve Bounds');
        ModeSelBackground:
        begin
            Assert(Assigned(FBackgroundPoints));
            PlotBackground;
        end;
        ModeSelPeakPos:
        begin
            Assert(Assigned(FCurvePositions));
            PlotCurvePositions;
        end;
        ModeSelPeakBounds:
        begin
            Assert(Assigned(FRFactorIntervals));
            PlotRFactorIntervals;
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
        ModeSelAreaLimits: AddPointToSelected(XValue, YValue);
        ModeSelCharacteristicPoints: AddPointToSelected(XValue, YValue);
        ModeSelGaussianBounds: AddPointToSelected(XValue, YValue);
        ModeSelBackground: AddPointToBackground(XValue, YValue);
        ModeSelPeakPos: AddPointToCurvePositions(XValue, YValue);
        ModeSelPeakBounds: AddPointToRFactorIntervals(XValue, YValue);
    end;
end;

function TFitClient.GetCurrentPointsSet: TTitlePointsSet;
begin
    case FSelectionMode of
        ModeSelNone:
            if FSelectedAreaMode then
                Result := FSelectedArea
            else
                Result := NeutronPointsSet;

        ModeSelAreaLimits: Result := FSelectedPoints;
        ModeSelCharacteristicPoints: Result := FSelectedPoints;
        ModeSelGaussianBounds: Result := FSelectedPoints;
        ModeSelBackground: Result := FBackgroundPoints;
        ModeSelPeakPos: Result    := FCurvePositions;
        ModeSelPeakBounds: Result := FRFactorIntervals;
    end;
end;

procedure TFitClient.CopyProfileDataFromLoader;
begin
    Assert(Assigned(FDataLoader));

    RemoveDataPoints;
    NeutronPointsSet.Free;
    NeutronPointsSet := nil;
    NeutronPointsSet := FDataLoader.GetPointsSetCopy;
    InitDataPoints;
end;

procedure TFitClient.LoadDataSet(FileName: string);
begin
    Assert(Assigned(FitProxy));

    FDataLoader := FDataLoaderInjector.CreateDataLoader(FileName);
    FDataLoader.LoadDataSet(FileName);
    CopyProfileDataFromLoader;
    Clear;
    PlotDataPoints;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitProxy.SetProfilePointsSet(NeutronPointsSet);
end;

procedure TFitClient.Reload;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FDataLoader));
    Assert(Assigned(FitProxy));

    FDataLoader.Reload;
    CopyProfileDataFromLoader;
    Clear;
    PlotDataPoints;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitProxy.SetProfilePointsSet(NeutronPointsSet);
end;

procedure TFitClient.SmoothProfile;
var
    NPS: TNeutronPointsSet;
    i:   longint;
begin
    Assert(Assigned(FitProxy));

    FitProxy.SmoothProfile;
    //  nel'zya udalyat' spisok i sozdavat' zanovo - menyaetsya
    //  poryadok v legende; nuzhno obnovlyat' dannye
    NPS := FitProxy.GetProfilePointsSet;
    try
        Assert(NPS.PointsCount = NeutronPointsSet.PointsCount);

        for i := 0 to NPS.PointsCount - 1 do
            NeutronPointsSet.PointYCoord[i] := NPS.PointYCoord[i];
    except
        NPS.Free;
        raise;
    end;
    FToRefresh := NeutronPointsSet;
    RefreshPointsSet;
end;

procedure TFitClient.SubtractBackground(Auto: boolean);
begin
    Assert(Assigned(FitProxy));
    if not Auto then
    begin
        Assert(Assigned(FBackgroundPoints));
        FitProxy.SetBackgroundPointsSet(FBackgroundPoints);
    end;
    FitProxy.SubtractBackground(Auto);
    //  ochistka spiska i skrytie grafika
    RemoveBackgroundPoints;
    //  perezagruzka dannyh
    RemoveDataPoints;
    NeutronPointsSet.Free;
    NeutronPointsSet := nil;
    NeutronPointsSet := FitProxy.GetProfilePointsSet;
    InitDataPoints;
    PlotDataPoints;
end;

procedure TFitClient.DoAllAutomatically;
begin
    Assert(Assigned(FitProxy));
    FitProxy.DoAllAutomatically;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.MinimizeDifference;
begin
    Assert(Assigned(FitProxy));
    //???    FitProxy.SetCurvePositions(FCurvePositions);
    //???    FitProxy.SetRFactorIntervals(FRFactorIntervals);
    FitProxy.MinimizeDifference;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.MinimizeNumberOfCurves;
begin
    Assert(Assigned(FitProxy));
    //???    FitProxy.SetCurvePositions(FCurvePositions);
    //???    FitProxy.SetRFactorIntervals(FRFactorIntervals);
    FitProxy.MinimizeNumberOfCurves;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.ComputeCurveBounds;
begin
    Assert(Assigned(FitProxy));
    FitProxy.ComputeCurveBounds;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.ComputeBackgroundPoints;
begin
    Assert(Assigned(FitProxy));
    FitProxy.ComputeBackgroundPoints;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.FindPeakPositions;
begin
    Assert(Assigned(FitProxy));
    FitProxy.FindPeakPositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.AllPointsAsPeakPositions;
begin
    Assert(Assigned(FitProxy));
    FitProxy.AllPointsAsPeakPositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.CreateSpecimenList;
begin
    Assert(Assigned(FitProxy));
    FitProxy.CreateSpecimenList;
end;

procedure TFitClient.StopAsyncOper;
begin
    Assert(Assigned(FitProxy));
    FitProxy.StopAsyncOper;
    //  izvlechenie dannyh zdes' delat' ne nuzhno,
    //  poskol'ku Done vyzyvatsya standartnym obrazom
end;

function TFitClient.AsyncOper: boolean;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.AsyncOper;
end;

function TFitClient.GetCalcTimeStr: string;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetCalcTimeStr;
end;

function TFitClient.GetRFactorStr: string;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetRFactorStr;
end;

function TFitClient.GetMaxRFactor: double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetMaxRFactor;
end;

procedure TFitClient.SetMaxRFactor(AMaxRFactor: double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetMaxRFactor(AMaxRFactor);
end;

function TFitClient.GetBackFactor: double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetBackFactor;
end;

procedure TFitClient.SetBackFactor(ABackFactor: double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetBackFactor(ABackFactor);
end;

function TFitClient.GetCurveThresh: double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetCurveThresh;
end;

procedure TFitClient.SetCurveThresh(ACurveThresh: double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetCurveThresh(ACurveThresh);
end;

function TFitClient.GetCurveType: TCurveTypeId;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetCurveType;
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
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetBackgroundVariationEnabled;
end;

procedure TFitClient.SetBackgroundVariationEnabled(AEnable: boolean);
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetBackgroundVariationEnabled(AEnable);
end;

function TFitClient.GetCurveScalingEnabled: boolean;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.GetCurveScalingEnabled;
end;

procedure TFitClient.SetCurveScalingEnabled(AEnabled: boolean);
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetCurveScalingEnabled(AEnabled);
end;

{$IFDEF _WINDOWS}
procedure TFitClient.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
    //  pervonachal'nuyu initsializatsiyu
    );
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetSpecialCurveParameters(ACurveExpr, CP);
end;

{$ENDIF}
end.
