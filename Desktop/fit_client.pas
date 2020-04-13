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

    { Handler to fill data table. }
    TFillDatasheetTable = procedure(Profile: TTitlePointsSet;
        CurvesList: TSelfCopiedCompList; GaussProfile: TTitlePointsSet;
        DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet) of object;
    { Handler drawing points selected by user. }
    TPlotSelectedPoints = procedure(Sender: TObject;
        SelectedPoints: TTitlePointsSet) of object;
    { Handler displaying data intervals. }
    TPlotRFactorBounds = procedure(Sender: TObject;
        RFactorBounds: TTitlePointsSet) of object;
    { Handler hiding data intervals. }
    THideRFactorBounds = procedure(Sender: TObject;
        RFactorBounds: TTitlePointsSet) of object;
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

    TPlotSelectedProfileInterval = procedure(Sender: TObject;
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
        FDataLoader:     IDataLoader;
        FDataLoaderInjector: IDataLoaderInjector;
        { All the data displayed on the chart. They are required to be able control of X-coordinate. }
        FExpProfile:     TTitlePointsSet;
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
        FRFactorBounds:  TTitlePointsSet;
        { Positions of curves. Only X-coordinates are used. }
        FCurvePositions: TTitlePointsSet;
        { Containers of calculated curves. Each object contains data of specimen curve. }
        FCurvesList:     TSelfCopiedCompList;
        { Containers of parameters of curves. }
        FCurveList:      TMSCRCurveList;
        { TODO: remove this attribute. }
        FWaveLength:     double;
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

        { Is used in RefreshPointsSet, Hide. }
        FToRefresh: TNeutronPointsSet;

        { Updates all the data and refreshes chart. }
        procedure UpdateAll;
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

        procedure PlotGaussProfile;
        procedure PlotDeltaProfile;
        procedure Refresh;
        procedure RefreshPointsSet;
        procedure Clear;
        procedure Hide;
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
        procedure RemoveGaussProfile;
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

        property FitProxy: IFitService read FFitProxy write FFitProxy;
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
    HintDone: string = 'Calculation done';

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
    FCurveProfile.Free;
    FSelectedPoints.Free;
    FExpProfile.Free;
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
    Result := FitProxy.GetSpecialCurveParameters;
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
    Assert(Assigned(FExpProfile));
    Assert(Assigned(FitProxy));

    FitProxy.SelectProfileInterval(StartPointIndex, StopPointIndex);
    Clear;
    SelectProfileIntervalActual(FExpProfile, StartPointIndex, StopPointIndex);
    PlotSelectedProfileInterval;

    FSelectedAreaMode := True;
end;

procedure TFitClient.SelectEntireProfile;
begin
    Assert(Assigned(FExpProfile));
    Assert(Assigned(FitProxy));

    FitProxy.SelectEntireProfile;
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

    FExpProfile.Free;
    FExpProfile := AExpProfile;
    FExpProfile.FTitle := ProfileName;
    FExpProfile.WaveLength := FWaveLength;
end;

procedure TFitClient.RemoveSelectedPoints;
begin
    FToRefresh := FSelectedPoints;
    Hide;
    FSelectedPoints.Free;
    FSelectedPoints := nil;
end;

procedure TFitClient.ClearExpProfile;
begin
    HideExpProfile;
    FExpProfile.Clear;
    { FExpProfile shouldn't be destroyed here to allow manual adding. }
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

procedure TFitClient.RemoveRFactorBounds;
begin
    HideRFactorBounds;
    FRFactorBounds.Clear; //  dlya posleduyuschego vvoda
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
        FSelectedArea := FitProxy.GetSelectedProfileInterval;
        //??? kak zdes' obrabatyvat' isklyucheniya
        FSelectedArea.WaveLength := FWaveLength;
        FSelectedArea.FTitle := SelectedAreaName;
        PlotSelectedProfileInterval;
    end
    else
    begin
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
        ClearExpProfile;
        SetExpProfile(FitProxy.GetProfilePointsSet);
        PlotExpProfile;
    end;
end;

function TFitClient.GetProfilePoints: TTitlePointsSet;
begin
    if FSelectedAreaMode then
        Result := FSelectedArea
    else
        Result := FExpProfile;
end;

procedure TFitClient.UpdateAll;
begin
    RemoveGaussProfile;
    FCurveProfile := FitProxy.GetCalcProfilePointsSet;
    if Assigned(FCurveProfile) and (FCurveProfile.PointsCount <> 0) then
    begin
        FCurveProfile.FTitle := SummarizedName;
        FCurveProfile.WaveLength := FWaveLength;
        PlotGaussProfile;
    end;

    RemoveDeltaProfile;
    FDeltaProfile := FitProxy.GetDeltaProfilePointsSet;
    if Assigned(FDeltaProfile) and (FDeltaProfile.PointsCount <> 0) then
    begin
        FDeltaProfile.FTitle := DeltaName;
        FDeltaProfile.WaveLength := FWaveLength;
        PlotDeltaProfile;
    end;

    RemoveCurvePositions;
    FCurvePositions.Free;
    FCurvePositions := nil;
    FCurvePositions := FitProxy.GetCurvePositions;
    if Assigned(FCurvePositions) and (FCurvePositions.PointsCount <> 0) then
    begin
        FCurvePositions.FTitle := CurvePositionsName;
        FCurvePositions.WaveLength := FWaveLength;
        PlotCurvePositions;
    end;

    RemoveRFactorBounds; //  nuzhno skryvat', t.k. menyaetsya uk-l'
    FRFactorBounds.Free;
    FRFactorBounds := nil;
    FRFactorBounds := FitProxy.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := RFactorBoundsName;
        FRFactorBounds.WaveLength := FWaveLength;
        PlotRFactorBounds;
    end;

    HideCurves;
    FCurvesList.Free;
    FCurvesList := nil;
    FCurvesList := FitProxy.GetCurvesList;
    if Assigned(FCurvesList) then
        SetCurvesListLambda;

    FCurveList.Free;
    FCurveList := nil;
    FCurveList := FitProxy.GetCurveList;
    if Assigned(FCurveList) then
        FCurveList.FWaveLength := FWaveLength;

    PlotCurves;
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
    RemoveRFactorBounds;
    FRFactorBounds.Free;
    FRFactorBounds := nil;
    FRFactorBounds := FitProxy.GetRFactorBounds;
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
    Assert(Assigned(FitProxy));

    RemoveBackgroundPoints;
    FBackgroundPoints.Free;
    FBackgroundPoints := nil;
    FBackgroundPoints := FitProxy.GetBackgroundPoints;
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
    Assert(Assigned(FitProxy));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorBounds;
    FRFactorBounds.Free;
    FRFactorBounds := nil;
    FRFactorBounds := FitProxy.GetRFactorBounds;
    if Assigned(FRFactorBounds) and (FRFactorBounds.PointsCount <> 0) then
    begin
        FRFactorBounds.FTitle := RFactorBoundsName;
        FRFactorBounds.WaveLength := FWaveLength;
        PlotRFactorBounds;
    end;

    RemoveCurvePositions;
    FCurvePositions.Free;
    FCurvePositions := nil;
    FCurvePositions := FitProxy.GetCurvePositions;
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
    NS: TNeutronPointsSet;
begin
    Assert(Assigned(FCurvesList));

    with FCurvesList do
        for i := 0 to FCurvesList.Count - 1 do
        begin
            NS := TNeutronPointsSet(FCurvesList.Items[i]);
            NS.WaveLength := FWaveLength;
        end;
end;

procedure TFitClient.SetWaveLength(AWaveLength: double);
begin
    FWaveLength := AWaveLength;
    if Assigned(FExpProfile) then
        FExpProfile.WaveLength := AWaveLength;
    if Assigned(FBackgroundPoints) then
        FBackgroundPoints.WaveLength := AWaveLength;
    if Assigned(FSelectedArea) then
        FSelectedArea.WaveLength := AWaveLength;
    if Assigned(FSelectedPoints) then
        FSelectedPoints.WaveLength := AWaveLength;
    if Assigned(FCurveProfile) then
        FCurveProfile.WaveLength := AWaveLength;
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
        end// !!! dvuh tochek s odinakovymi X i raznymi Y vse ravno byt'
    // ne mozhet, poetomu proveryaetsya tol'ko koordinata X !!!
    ;
    // tochka ne naydena - dobavlyaem novuyu
    Points.AddNewPoint(XValue, YValue);
    Plot;
end;

procedure TFitClient.PlotCurves;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotCurves(Self, FCurvesList, FCurveList);
end;

procedure TFitClient.HideCurves;
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

procedure TFitClient.PlotRFactorBounds;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotRFactorBounds(Self, FRFactorBounds);
end;

procedure TFitClient.HideRFactorBounds;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideRFactorBounds(Self, FRFactorBounds);
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
            GaussProfile, DeltaProfile, RFactorBounds);
end;

{$ENDIF}

procedure TFitClient.PlotExpProfile;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotDataPoints(Self, FExpProfile);
end;

procedure TFitClient.HideExpProfile;
begin
    if Assigned(FFitViewer) then
        FFitViewer.HideDataPoints(Self, FExpProfile);
end;

procedure TFitClient.PlotSelectedProfileInterval;
begin
    if Assigned(FFitViewer) then
        FFitViewer.PlotSelectedProfileInterval(Self, FSelectedArea);
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
        Assert(Assigned(FExpProfile));
        ReplacePoint(FExpProfile,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotExpProfile);
    end;
    FitProxy.ReplacePointInProfile(PrevXValue, PrevYValue, NewXValue, NewYValue);
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
    //Assert(Assigned(FRFactorBounds));
    //AddPoint(FRFactorBounds, XValue, YValue, PlotRFactorBounds);

    FitProxy.AddPointToRFactorBounds(XValue, YValue);
    UpdateAll;
end;

procedure TFitClient.AddPointToCurvePositions(XValue, YValue: double);
begin
    //Assert(Assigned(FCurvePositions));
    //Assert(Assigned(FRFactorBounds));
    //AddPoint(FCurvePositions, XValue, YValue, PlotCurvePositions);

    FitProxy.AddPointToCurvePositions(XValue, YValue);
    UpdateAll;
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
    Assert(Assigned(FitProxy));

    FDataLoader := FDataLoaderInjector.CreateDataLoader(FileName);
    FDataLoader.LoadDataSet(FileName);
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitProxy.SetProfilePointsSet(FExpProfile);
end;

procedure TFitClient.Reload;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FDataLoader));
    Assert(Assigned(FitProxy));

    FDataLoader.Reload;
    CopyProfileDataFromLoader;
    Clear;
    PlotExpProfile;
    FOpenState := OpenSuccess;
    //  isklyuchenie v servere ne dolzhno preryvat'
    //  posl-t' vypolneniya v kliente, poetomu
    //  vyzyvaetsya v poslednyuyu ochered'
    FitProxy.SetProfilePointsSet(FExpProfile);
end;

procedure TFitClient.SmoothProfile;
begin
    Assert(Assigned(FitProxy));

    FitProxy.SmoothProfile;
    FExpProfile.Free;
    FExpProfile := FitProxy.GetProfilePointsSet;
    FToRefresh := FExpProfile;
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
    ClearExpProfile;
    SetExpProfile(FitProxy.GetProfilePointsSet);
    PlotExpProfile;
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
    //???    FitProxy.SetRFactorBounds(FRFactorBounds);
    FitProxy.MinimizeDifference;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.MinimizeNumberOfCurves;
begin
    Assert(Assigned(FitProxy));
    //???    FitProxy.SetCurvePositions(FCurvePositions);
    //???    FitProxy.SetRFactorBounds(FRFactorBounds);
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

procedure TFitClient.ComputeCurvePositions;
begin
    Assert(Assigned(FitProxy));
    FitProxy.ComputeCurvePositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.SelectAllPointsAsCurvePositions;
begin
    Assert(Assigned(FitProxy));
    FitProxy.SelectAllPointsAsCurvePositions;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.CreateCurveList;
begin
    Assert(Assigned(FitProxy));
    FitProxy.CreateCurveList;
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
