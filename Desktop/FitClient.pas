{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes used in displaying results to user.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit FitClient;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, PointsSet, TitlePointsSet, SelfCopied, SysUtils, MSCRDataClasses,
    Dialogs, FitClientProxy, CommonTypes, CBRCComponent, NeutronPointsSet,
    CurvePointsSet, IntClientCallback, IntFitViewer, IntDataLoader,
    IntDataLoaderInjector;
    
type
    { Modes of selectiion of active point set. }
    TSelMode = (ModeSelNone, ModeSelAreaLimits, ModeSelCharacteristicPoints,
                ModeSelGaussianBounds, ModeSelBackground, ModeSelPeakPos,
                ModeSelPeakBounds);
    { Results of data file opening. }
    TOpenState = (OpenSuccess, OpenFailure);
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
    TFillDatasheetTable = procedure(
            Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList;
            GaussProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet;
            RFactorIntervals: TTitlePointsSet
            ) of object;
    { Handler drawing points selected by user. }
    TPlotSelectedPoints = procedure(
        Sender: TObject; SelectedPoints: TTitlePointsSet) of object;
    { Handler displaying data intervals. }
    TPlotRFactorIntervals = procedure(
        Sender: TObject; RFactorIntervals: TTitlePointsSet) of object;
    { Handler hiding data intervals. }
    THideRFactorIntervals = procedure(
        Sender: TObject; RFactorIntervals: TTitlePointsSet) of object;
    { Handler displaying curve positions. }
    TPlotCurvePositions = procedure(
        Sender: TObject; CurvePositions: TTitlePointsSet) of object;
    { Handler hiding curve positions. }
    THideCurvePositions = procedure(
        Sender: TObject; CurvePositions: TTitlePointsSet) of object;
    { Handler displaying background curve. }
    TPlotBackground = procedure(
        Sender: TObject; BackgroundPoints: TTitlePointsSet) of object;
    { Handler hiding background curve. }
    THideBackground = procedure(
        Sender: TObject; BackgroundPoints: TTitlePointsSet) of object;
    { Handler displaying data curve. }
    TPlotDataPoints = procedure(
        Sender: TObject; DataPoints: TTitlePointsSet) of object;
    { Handler hiding data curve. }
    THideDataPoints = procedure(
        Sender: TObject; DataPoints: TTitlePointsSet) of object;
    
    TPlotSelectedArea = procedure(
        Sender: TObject; SelectedArea: TTitlePointsSet) of object;
    TPlotGaussProfile = procedure(
        Sender: TObject; GaussProfile: TTitlePointsSet) of object;
    TPlotDeltaProfile = procedure(
        Sender: TObject; DeltaProfile: TTitlePointsSet) of object;
    TRefresh = procedure(Sender: TObject) of object;
    TRefreshPointsSet = procedure(
        Sender: TObject; PointsSet: TNeutronPointsSet) of object;
    TClear = procedure(Sender: TObject) of object;
    THide = procedure(Sender: TObject; PointsSet: TNeutronPointsSet) of object;
    TAsyncOperationFinished = procedure(Sender: TObject) of object;
    TPlotProc = procedure of object;

    { Implements all client logic of the application. Must be completely independent from UI. }
    TFitClient = class(TCBRCComponent, IClientCallback)
    protected
        FFitProxy: TFitClientProxy;
        FDataLoader: IDataLoader;
        FDataLoaderInjector: IDataLoaderInjector;
        { All the data displayed on the chart. They are required to be able control of X-coordinate. }
        FNeutronPointsSet: TTitlePointsSet;
        { Region of given profile data with which user is working at the given moment. }
        SelectedArea: TTitlePointsSet;
        { Sum of all model specimens which is compared with experimental data. }
        GaussProfile: TTitlePointsSet;
        DeltaProfile: TTitlePointsSet;
        { Set of points selected by user. }
        SelectedPoints: TTitlePointsSet;
        { List of background points which is used for transmission between manual and automatic selection modes. }
        BackgroundPoints: TTitlePointsSet;
        { List of point pairs which limit interval of R-factor calculation. 
          Always must be displayed in order to show user in which mode R-factor is calculated. }
        RFactorIntervals: TTitlePointsSet;
        { Positions of pattern specimens. Only X-coordinates are used. }
        CurvePositions: TTitlePointsSet;
        { Containers of calculated pattern specimens. Each object contains data of specimen curve. }
        CurvesList: TSelfCopiedCompList;
        { Containers of parameters of pattern specimens. }
        SpecimenList: TMSCRSpecimenList;

        WaveLength: Double;
        procedure SetCurvesListLambda;

    protected
        CurMin: Double;
        { If True then in all operations only data belonging to selected ared are used
          otherwise all profile data are used. }
        FSelectedAreaMode: Boolean;
        FSelectionMode: TSelMode;
        FOpenState: TOpenState;
        FAsyncState: TAsyncState;
        { Adds new point to the given set. Second call removes point from the set. 
          In last case the set is recreated. }
        procedure AddPoint(var Points: TTitlePointsSet;
            XValue, YValue: Double; Plot: TPlotProc);
        { Replaces point and updates chart. }
        procedure ReplacePoint(Points: TTitlePointsSet;
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
            Plot: TPlotProc
            );

    protected
        { Pointers to methods for curve displaying. }

        { Callback on asynchronous operation finishing. }
        FAsyncOperationFinished: TAsyncOperationFinished;

        { Is used in RefreshPointsSet, Hide. }
        ToRefresh: TNeutronPointsSet;

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

        function GetMaxRFactor: Double;
        procedure SetMaxRFactor(AMaxRFactor: Double);
        function GetBackFactor: Double;
        procedure SetBackFactor(ABackFactor: Double);
        function GetCurveThresh: Double;
        procedure SetCurveThresh(ACurveThresh: Double);
        function GetCurveType: TCurveType;
        procedure SetCurveType(ACurveType: TCurveType);

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
            StartPointIndex, StopPointIndex: LongInt);
        procedure CopyProfileDataFromLoader;

    public
        FitViewer: IFitViewer;

        function GetSpecimenList: TMSCRSpecimenList;

        function GetBackgroundPoints: TNeutronPointsSet;
        function GetSelectedPoints: TNeutronPointsSet;
        function GetRFactorIntervals: TNeutronPointsSet;
        function GetCurvePositions: TNeutronPointsSet;
{$IFNDEF EXCLUDE_SOMETHING}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(
            ACurveExpr: string;
            { Nil means first initialization. }
            CP: Curve_parameters
            );
{$ENDIF}
        { Do only cleaning of sets. }
        
        procedure RemoveRFactorIntervals;
        procedure RemoveCurvePositions;
        procedure RemoveBackgroundPoints;

        { All call AddPoint method. }
        
        procedure AddPointToSelected(XValue, YValue: Double);
        procedure AddPointToBackground(XValue, YValue: Double);
        procedure AddPointToRFactorIntervals(XValue, YValue: Double);
        procedure AddPointToCurvePositions(XValue, YValue: Double);
        
        { All call ReplacePoint method. }
        
        procedure ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInSelected(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInRFactorIntervals(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure AddPointToActive(XValue, YValue: Double);
        { Returns a set with which user works at the moment. }
        function GetCurrentPointsSet: TTitlePointsSet;

        { Cleans chart and moves data from full profile to data of selected iterval. }
        procedure SelectArea(StartPointIndex, StopPointIndex: LongInt);
        procedure ReturnToTotalProfile;

        procedure SetWaveLength(AWaveLength: Double);
        function GetWaveLength: Double;

        constructor Create(AOwner: TComponent;
            ADataLoaderInjector: IDataLoaderInjector);
        destructor Destroy; override;

        procedure LoadDataSet(FileName: string);
        procedure Reload;
        
        { Callbacks from the server. }
        
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        { Wrappers for server methods. Mustn't create messages because this
          is responsibility of GUI. Instead of this must throw exceptions. }
        
        procedure SmoothProfile;
        procedure SubtractAllBackground(Auto: Boolean);
        procedure DoAllAutomatically;
        procedure FindGausses;
        procedure FindGaussesSequentially;
        procedure FindPeakBounds;
        procedure FindBackPoints;
        procedure FindPeakPositions;
        procedure AllPointsAsPeakPositions;
        procedure StopAsyncOper;
        { Gets state of asynchronous operation from the server. }
        function AsyncOper: Boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        procedure CreateSpecimenList;

        { Getters of server attributes. }
        
        property MaxRFactor: Double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: Double read GetBackFactor write SetBackFactor;
        property CurveThresh: Double read GetCurveThresh write SetCurveThresh;
        property CurveType: TCurveType read GetCurveType write SetCurveType;

        { Plotting events are called from methods of the same name for providing
          synchronization with main application thread. Point to methods of TIIViewer. }

        { Callbacks for updating user interface. They are called from main thread of client application.
          Callbacks can throw exceptions. They can be not assigned (nil). }
        property OnAsyncOperationFinished: TAsyncOperationFinished
            read FAsyncOperationFinished write FAsyncOperationFinished;

        property SelectionMode: TSelMode
            read FSelectionMode write SetSelectionMode;
        property OpenState: TOpenState read FOpenState;
        property AsyncState: TAsyncState read FAsyncState;
        property SelectedAreaMode: Boolean read FSelectedAreaMode;
        
        property FitProxy: TFitClientProxy read FFitProxy write FFitProxy;
        property NeutronPointsSet: TTitlePointsSet
            read FNeutronPointsSet write FNeutronPointsSet;
    end;
    
const
    RFactorIntervalsName:       string = 'Spec.app.intervals';
    BackgroundPointsName:       string = 'Background points';
    CurvePositionsName:         string = 'Spec.positions';
    SelectedAreaName:           string = 'Selected interval';
    SummarizedName:             string = 'Summarized';
    ArgumentName:               string = 'Position';
    ProfileName:                string = 'Data';
    NumberName:                 string = 'Number';
    ValueName:                  string = 'Amplitude';
    DeltaName:                  string = 'Difference';
    StartName:                  string = 'Starting Position';
    StopName:                   string = 'Final Position';
    HintDone:                   string = 'Calculation done';

implementation

{================================ TFitClient ==================================}

destructor TFitClient.Destroy;
begin
    SpecimenList.Free;
    BackgroundPoints.Free;
    SelectedArea.Free;
    CurvesList.Free;
    DeltaProfile.Free;
    GaussProfile.Free;
    SelectedPoints.Free;
    NeutronPointsSet.Free;
    RFactorIntervals.Free;
    inherited Destroy;
end;

constructor TFitClient.Create(AOwner: TComponent;
    ADataLoaderInjector: IDataLoaderInjector);
begin
    inherited Create(AOwner);
    FDataLoaderInjector := ADataLoaderInjector;
    FSelectionMode := ModeSelNone;
    FOpenState := OpenFailure;
    FAsyncState := AsyncStart;
    
    //  sozdayutsya pustye spiski, chtoby mozhno bylo
    //  vvodit' dannye vruchnuyu
    NeutronPointsSet := TTitlePointsSet.Create(nil);
    InitDataPoints;
    
    CurvePositions := TTitlePointsSet.Create(nil);
    CurvePositions.Title := CurvePositionsName;
    CurvePositions.Lambda := WaveLength;

    BackgroundPoints := TTitlePointsSet.Create(nil);
    BackgroundPoints.Title := BackgroundPointsName;
    BackgroundPoints.Lambda := WaveLength;
    
    RFactorIntervals := TTitlePointsSet.Create(nil);
    RFactorIntervals.Title := RFactorIntervalsName;
    RFactorIntervals.Lambda := WaveLength;
end;

function TFitClient.GetBackgroundPoints: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := BackgroundPoints;
end;

function TFitClient.GetSelectedPoints: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := SelectedPoints;
end;

function TFitClient.GetRFactorIntervals: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := RFactorIntervals;
end;

function TFitClient.GetCurvePositions: TNeutronPointsSet;
begin
    //  dopuskaetsya vydavat' nil
    Result := CurvePositions;
end;

{$IFNDEF EXCLUDE_SOMETHING}
function TFitClient.GetSpecialCurveParameters: Curve_parameters;
begin
    Result := FitProxy.GetSpecialCurveParameters;
end;
{$ENDIF}

procedure TFitClient.SelectAreaActual(
    ANeutronPoints: TNeutronPointsSet; StartPointIndex, StopPointIndex: LongInt
    );
var i: LongInt;
begin
    Assert(Assigned(ANeutronPoints));
    Assert(ANeutronPoints.PointsCount <> 0);
    if (StartPointIndex < 0) or
       (StopPointIndex > ANeutronPoints.PointsCount - 1) then Assert(False);
    Assert(ANeutronPoints <> SelectedArea);

    RemoveSelectedArea;
    SelectedArea := TTitlePointsSet.Create(nil);
    try
        SelectedArea.Lambda := WaveLength;
        SelectedArea.Title := SelectedAreaName;
        for i := StartPointIndex to StopPointIndex do
            SelectedArea.AddNewPoint(
                ANeutronPoints.PointXCoord[i], ANeutronPoints.PointYCoord[i]);
    except
        SelectedArea.Free; SelectedArea := nil;
        raise;
    end;
end;

procedure TFitClient.SelectArea(StartPointIndex, StopPointIndex: LongInt);
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
    SelectedArea.Free; SelectedArea := nil;
end;

procedure TFitClient.RecreateAndShowSelectedPoints(Title: string);
begin
    RemoveSelectedPoints;
    SelectedPoints := TTitlePointsSet.Create(nil);
    SelectedPoints.Title := Title;
    SelectedPoints.Lambda := WaveLength;
    PlotSelectedPoints;
end;

procedure TFitClient.InitDataPoints;
begin
    Assert(Assigned(NeutronPointsSet));
    NeutronPointsSet.Title := ProfileName;
    NeutronPointsSet.Lambda := WaveLength;
end;

procedure TFitClient.RemoveSelectedPoints;
begin
    //  dopuskaetsya ravenstvo nil
    ToRefresh := SelectedPoints;
    Hide;
    SelectedPoints.Free; SelectedPoints := nil;
end;

procedure TFitClient.RemoveDataPoints;
begin
    HideDataPoints;
    NeutronPointsSet.Clear; //  chtoby mozhno bylo dobavlyat' tochki vruchnuyu
end;

procedure TFitClient.RemoveGaussProfile;
begin
    //  dopuskaetsya ravenstvo nil
    ToRefresh := GaussProfile;
    Hide;
    GaussProfile.Free; GaussProfile := nil;
end;

procedure TFitClient.RemoveDeltaProfile;
begin
    //  dopuskaetsya ravenstvo nil
    ToRefresh := DeltaProfile;
    Hide;
    DeltaProfile.Free; DeltaProfile := nil;
end;

procedure TFitClient.RemoveSelectedArea;
begin
    //  dopuskaetsya ravenstvo nil
    ToRefresh := SelectedArea;
    Hide;
    SelectedArea.Free; SelectedArea := nil;
end;

procedure TFitClient.RemoveRFactorIntervals;
begin
    HideRFactorIntervals;
    RFactorIntervals.Clear; //  dlya posleduyuschego vvoda
end;

procedure TFitClient.RemoveCurvePositions;
begin
    HideCurvePositions;
    CurvePositions.Clear;   //  dlya posleduyuschego vvoda
end;

procedure TFitClient.RemoveBackgroundPoints;
begin
    HideBackground;
    BackgroundPoints.Clear; //  dlya posleduyuschego vvoda
end;

procedure TFitClient.ShowCurMin(Min: Double);
begin
    //  Stores current minimum value.
    CurMin := Min;
    //  Updates UI.
    if Assigned(FitViewer) then
    begin
        FitViewer.ShowTime;
        FitViewer.ShowRFactor;
        if FitViewer.GetAnimationMode then
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
        SelectedArea := FitProxy.GetSelectedArea;
        //??? kak zdes' obrabatyvat' isklyucheniya
        SelectedArea.Lambda := WaveLength;
        SelectedArea.Title := SelectedAreaName;
        PlotSelectedArea;
    end
    else
    begin
        //  dannye udalyayutsya dlya ucheta vozmozhnyh izmeneniy,
        //  naprimer udaleniya fona
        RemoveDataPoints;
        NeutronPointsSet.Free; NeutronPointsSet := nil;
        NeutronPointsSet := FitProxy.GetProfilePointsSet;
        InitDataPoints;
        PlotDataPoints;
    end;
end;

function TFitClient.GetProfilePointsSet: TTitlePointsSet;
begin
    if FSelectedAreaMode then
        Result := SelectedArea
    else Result := NeutronPointsSet;
end;

procedure TFitClient.UpdateAll;
begin
    RemoveGaussProfile;
    GaussProfile := FitProxy.GetCalcProfilePointsSet;
    if Assigned(GaussProfile) and (GaussProfile.PointsCount <> 0) then
    begin
        GaussProfile.Title := SummarizedName;
        GaussProfile.Lambda := WaveLength;
        PlotGaussProfile;
    end;

    RemoveDeltaProfile;
    DeltaProfile := FitProxy.GetDeltaProfilePointsSet;
    if Assigned(DeltaProfile) and (DeltaProfile.PointsCount <> 0) then
    begin
        DeltaProfile.Title := DeltaName;
        DeltaProfile.Lambda := WaveLength;
        PlotDeltaProfile;
    end;

    RemoveCurvePositions;
    CurvePositions.Free; CurvePositions := nil;
    CurvePositions := FitProxy.GetCurvePositions;
    if Assigned(CurvePositions) and (CurvePositions.PointsCount <> 0) then
    begin
        CurvePositions.Title := CurvePositionsName;
        CurvePositions.Lambda := WaveLength;
        PlotCurvePositions;
    end;

    RemoveRFactorIntervals; //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RFactorIntervals.Free; RFactorIntervals := nil;
    RFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(RFactorIntervals) and (RFactorIntervals.PointsCount <> 0) then
    begin
        RFactorIntervals.Title := RFactorIntervalsName;
        RFactorIntervals.Lambda := WaveLength;
        PlotRFactorIntervals;
    end;

    HideSpecimens;
    CurvesList.Free; CurvesList := nil;
    CurvesList := FitProxy.GetCurvesList;
    if Assigned(CurvesList) then SetCurvesListLambda;

    SpecimenList.Free; SpecimenList := nil;
    SpecimenList := FitProxy.GetSpecimenList;
    if Assigned(SpecimenList) then
        SpecimenList.Lambda := WaveLength;

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
    if Assigned(OnAsyncOperationFinished) then OnAsyncOperationFinished(Self);
    if Assigned(FitViewer) then
        FitViewer.ShowHint(HintDone);
end;

procedure TFitClient.FindPeakBoundsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorIntervals;
    RFactorIntervals.Free; RFactorIntervals := nil;
    RFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(RFactorIntervals) and (RFactorIntervals.PointsCount <> 0) then
    begin
        RFactorIntervals.Title := RFactorIntervalsName;
        RFactorIntervals.Lambda := WaveLength;

        PlotRFactorIntervals;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then OnAsyncOperationFinished(Self);
end;

procedure TFitClient.FindBackPointsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));

    RemoveBackgroundPoints;
    BackgroundPoints.Free; BackgroundPoints := nil;
    BackgroundPoints := FitProxy.GetBackgroundPoints;
    if Assigned(BackgroundPoints) and (BackgroundPoints.PointsCount <> 0) then
    begin
        BackgroundPoints.Title := BackgroundPointsName;
        BackgroundPoints.Lambda := WaveLength;

        PlotBackground;
    end;
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then OnAsyncOperationFinished(Self);
end;

procedure TFitClient.FindPeakPositionsDone;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));
    //  nuzhno skryvat', t.k. menyaetsya uk-l'
    RemoveRFactorIntervals;
    RFactorIntervals.Free; RFactorIntervals := nil;
    RFactorIntervals := FitProxy.GetRFactorIntervals;
    if Assigned(RFactorIntervals) and (RFactorIntervals.PointsCount <> 0) then
    begin
        RFactorIntervals.Title := RFactorIntervalsName;
        RFactorIntervals.Lambda := WaveLength;
        PlotRFactorIntervals;
    end;

    RemoveCurvePositions;
    CurvePositions.Free; CurvePositions := nil;
    CurvePositions := FitProxy.GetCurvePositions;
    if Assigned(CurvePositions) and (CurvePositions.PointsCount <> 0) then
    begin
        CurvePositions.Title := CurvePositionsName;
        CurvePositions.Lambda := WaveLength;
        PlotCurvePositions;
    end;
    
    FAsyncState := AsyncDone;
    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then OnAsyncOperationFinished(Self);
end;

function TFitClient.GetSpecimenList: TMSCRSpecimenList;
begin
    Assert(Assigned(SpecimenList));
    Result := SpecimenList;
end;

procedure TFitClient.SetCurvesListLambda;
var i: LongInt;
    NS: TNeutronPointsSet;
begin
    Assert(Assigned(CurvesList));
    
    with CurvesList do
        for i := 0 to CurvesList.Count - 1 do
        begin
            NS := TNeutronPointsSet(CurvesList.Items[i]);
            NS.Lambda := WaveLength;
        end;
end;

procedure TFitClient.SetWaveLength(AWaveLength: Double);
begin
    WaveLength := AWaveLength;
    if Assigned(NeutronPointsSet) then NeutronPointsSet.Lambda := AWaveLength;
    if Assigned(BackgroundPoints) then BackgroundPoints.Lambda := AWaveLength;
    if Assigned(SelectedArea) then SelectedArea.Lambda := AWaveLength;
    if Assigned(SelectedPoints) then SelectedPoints.Lambda := AWaveLength;
    if Assigned(GaussProfile) then GaussProfile.Lambda := AWaveLength;
    if Assigned(DeltaProfile) then DeltaProfile.Lambda := AWaveLength;
    if Assigned(CurvesList) then SetCurvesListLambda;
    if Assigned(SpecimenList) then SpecimenList.Lambda := WaveLength;
end;

function TFitClient.GetWaveLength: Double;
begin
    Result := WaveLength;
end;

procedure TFitClient.ReplacePoint(Points: TTitlePointsSet;
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    Plot: TPlotProc
    );
begin
    Assert(Assigned(Points));
    Points.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    //  vyvodit' nuzhno v lyubom sluchae dlya
    //  ochistki poley posle nepravil'nogo vvoda
    Plot;
end;

// !!! povtornyy vyzov dlya dannyh koordinat udalyaet tochku iz spiska !!!
procedure TFitClient.AddPoint(
    var Points: TTitlePointsSet; XValue, YValue: Double; Plot: TPlotProc);
var i: LongInt;
begin
    Assert(Assigned(Points));

    // ischem zadannuyu tochku v vybrannom spiske tochek
    for i := 0 to Points.PointsCount - 1 do
    begin
        // !!! dvuh tochek s odinakovymi X i raznymi Y vse ravno byt'
        // ne mozhet, poetomu proveryaetsya tol'ko koordinata X !!!
        if XValue = Points.PointXCoord[i] then
        begin
            // nuzhno korrektno udalit' spisok tochek iz spiska vseh krivyh,
            // potomu chto DeletePoints izmenyaet znachenie uk-lya Points...
            //ToRefresh := Points;
            //Hide;
            
            Points.DeletePoint(XValue);
            // ...i posle etogo otobrazit' ego zanovo
            Plot;
            Exit;
        end;
    end;
    // tochka ne naydena - dobavlyaem novuyu
    Points.AddNewPoint(XValue, YValue);
    Plot;
end;

procedure TFitClient.PlotSpecimens;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotSpecimens(Self, CurvesList, SpecimenList);
end;

procedure TFitClient.HideSpecimens;
var i: LongInt;
begin
    if Assigned(CurvesList) then
        for i := 0 to CurvesList.Count - 1 do
        begin
            ToRefresh := TNeutronPointsSet(CurvesList.Items[i]);
            Hide;
        end;
end;

procedure TFitClient.PlotSelectedPoints;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotSelectedPoints(Self, SelectedPoints);
end;

procedure TFitClient.PlotRFactorIntervals;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotRFactorIntervals(Self, RFactorIntervals);
end;

procedure TFitClient.HideRFactorIntervals;
begin
    if Assigned(FitViewer) then
        FitViewer.HideRFactorIntervals(Self, RFactorIntervals);
end;

procedure TFitClient.PlotCurvePositions;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotCurvePositions(Self, CurvePositions);
end;

procedure TFitClient.HideCurvePositions;
begin
    if Assigned(FitViewer) then
        FitViewer.HideCurvePositions(Self, CurvePositions);
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
    if Assigned(FitViewer) then
        FitViewer.PlotDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.HideDataPoints;
begin
    if Assigned(FitViewer) then
        FitViewer.HideDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.PlotSelectedArea;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotSelectedArea(Self, SelectedArea);
end;

procedure TFitClient.PlotBackground;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotBackground(Self, BackgroundPoints);
end;

procedure TFitClient.HideBackground;
begin
    if Assigned(FitViewer) then
        FitViewer.HideBackground(Self, BackgroundPoints);
end;

procedure TFitClient.PlotGaussProfile;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotGaussProfile(Self, GaussProfile);
end;

procedure TFitClient.PlotDeltaProfile;
begin
    if Assigned(FitViewer) then
        FitViewer.PlotDeltaProfile(Self, DeltaProfile);
end;

procedure TFitClient.Refresh;
begin
    if Assigned(FitViewer) then FitViewer.Refresh(Self);
end;

procedure TFitClient.RefreshPointsSet;
begin
    Assert(Assigned(ToRefresh));

    if Assigned(FitViewer) then
        FitViewer.RefreshPointsSet(Self, ToRefresh);
    ToRefresh := nil;
end;

procedure TFitClient.Clear;
begin
    if Assigned(FitViewer) then FitViewer.Clear(Self);
end;

procedure TFitClient.Hide;
begin
    if Assigned(FitViewer) then FitViewer.Hide(Self, ToRefresh);
    ToRefresh := nil;
end;

procedure TFitClient.ReplacePointInData(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
begin
    if FSelectedAreaMode then
    begin
        Assert(Assigned(SelectedArea));
        ReplacePoint(SelectedArea,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedArea);
    end
    else
    begin
        Assert(Assigned(NeutronPointsSet));
        ReplacePoint(NeutronPointsSet,
            PrevXValue, PrevYValue, NewXValue, NewYValue, PlotDataPoints);
    end;
    FitProxy.ReplacePointInData(PrevXValue, PrevYValue, NewXValue, NewYValue) ;
end;

procedure TFitClient.ReplacePointInSelected(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
begin
    Assert(Assigned(SelectedPoints));
    ReplacePoint(SelectedPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotSelectedPoints);
    //???  vyzov servera
end;

procedure TFitClient.ReplacePointInBackground(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
begin
    Assert(Assigned(BackgroundPoints));
    ReplacePoint(BackgroundPoints,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotBackground);
    FitProxy.ReplacePointInBackground(
        PrevXValue, PrevYValue, NewXValue, NewYValue) ;
end;

procedure TFitClient.ReplacePointInRFactorIntervals(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
begin
    Assert(Assigned(RFactorIntervals));
    ReplacePoint(RFactorIntervals,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotRFactorIntervals);
end;

procedure TFitClient.ReplacePointInCurvePositions(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
begin
    Assert(Assigned(CurvePositions));
    ReplacePoint(CurvePositions,
        PrevXValue, PrevYValue, NewXValue, NewYValue, PlotCurvePositions);
end;

procedure TFitClient.AddPointToSelected(XValue, YValue: Double);
begin
    Assert(Assigned(SelectedPoints));
    AddPoint(SelectedPoints, XValue, YValue, PlotSelectedPoints);
end;

procedure TFitClient.AddPointToBackground(XValue, YValue: Double);
begin
    Assert(Assigned(BackgroundPoints));
    AddPoint(BackgroundPoints, XValue, YValue, PlotBackground);
end;

procedure TFitClient.AddPointToRFactorIntervals(XValue, YValue: Double);
begin
    //Assert(Assigned(RFactorIntervals));
    //AddPoint(RFactorIntervals, XValue, YValue, PlotRFactorIntervals);
    
    FitProxy.AddPointToRFactorIntervals(XValue, YValue);
    UpdateAll;
end;

procedure TFitClient.AddPointToCurvePositions(XValue, YValue: Double);
begin
    //Assert(Assigned(CurvePositions));
    //Assert(Assigned(RFactorIntervals));
    //AddPoint(CurvePositions, XValue, YValue, PlotCurvePositions);
    
    FitProxy.AddPointToCurvePositions(XValue, YValue);
    UpdateAll;
end;

procedure TFitClient.SetSelectionMode(ASelectionMode: TSelMode);
begin
    case ASelectionMode of
        ModeSelNone:
            begin
                case FSelectionMode of
                    ModeSelAreaLimits: RemoveSelectedPoints;
                    ModeSelCharacteristicPoints: RemoveSelectedPoints;
                    ModeSelGaussianBounds: RemoveSelectedPoints;
                end;
            end;
        ModeSelAreaLimits:
            RecreateAndShowSelectedPoints('Area Limits');
        ModeSelCharacteristicPoints:
            RecreateAndShowSelectedPoints('Characteristic Points');
        ModeSelGaussianBounds:
            RecreateAndShowSelectedPoints('Curve Bounds');
        ModeSelBackground:
            begin
                Assert(Assigned(BackgroundPoints));
                PlotBackground;
            end;
        ModeSelPeakPos:
            begin
                Assert(Assigned(CurvePositions));
                PlotCurvePositions;
            end;
        ModeSelPeakBounds:
            begin
                Assert(Assigned(RFactorIntervals));
                PlotRFactorIntervals;
            end;
    end;
    FSelectionMode := ASelectionMode;
end;

function TFitClient.GetSelectionMode: TSelMode;
begin
    Result := FSelectionMode;
end;

procedure TFitClient.AddPointToActive(XValue, YValue: Double);
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
                Result := SelectedArea
            else Result := NeutronPointsSet;

        ModeSelAreaLimits: Result := SelectedPoints;
        ModeSelCharacteristicPoints: Result := SelectedPoints;
        ModeSelGaussianBounds: Result := SelectedPoints;
        ModeSelBackground: Result := BackgroundPoints;
        ModeSelPeakPos: Result := CurvePositions;
        ModeSelPeakBounds: Result := RFactorIntervals;
    end;
end;

procedure TFitClient.CopyProfileDataFromLoader;
begin
    Assert(Assigned(FDataLoader));

    RemoveDataPoints;
    NeutronPointsSet.Free; NeutronPointsSet := nil;
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
var NPS: TNeutronPointsSet;
    i: LongInt;
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
        NPS.Free; raise;
    end;
    ToRefresh := NeutronPointsSet;
    RefreshPointsSet;
end;

procedure TFitClient.SubtractAllBackground(Auto: Boolean);
begin
    Assert(Assigned(FitProxy));
    if not Auto then
    begin
        Assert(Assigned(BackgroundPoints));
        FitProxy.SetBackgroundPointsSet(BackgroundPoints);
    end;
    FitProxy.SubtractAllBackground(Auto);
    //  ochistka spiska i skrytie grafika
    RemoveBackgroundPoints;
    //  perezagruzka dannyh
    RemoveDataPoints;
    NeutronPointsSet.Free; NeutronPointsSet := nil;
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

procedure TFitClient.FindGausses;
begin
    Assert(Assigned(FitProxy));
//???    FitProxy.SetCurvePositions(CurvePositions);
//???    FitProxy.SetRFactorIntervals(RFactorIntervals);
    FitProxy.FindGausses;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.FindGaussesSequentially;
begin
    Assert(Assigned(FitProxy));
//???    FitProxy.SetCurvePositions(CurvePositions);
//???    FitProxy.SetRFactorIntervals(RFactorIntervals);
    FitProxy.FindGaussesSequentially;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.FindPeakBounds;
begin
    Assert(Assigned(FitProxy));
    FitProxy.FindPeakBounds;
    FAsyncState := AsyncWorks;
end;

procedure TFitClient.FindBackPoints;
begin
    Assert(Assigned(FitProxy));
    FitProxy.FindBackPoints;
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

function TFitClient.AsyncOper: Boolean;
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

function TFitClient.GetMaxRFactor: Double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.MaxRFactor;
end;

procedure TFitClient.SetMaxRFactor(AMaxRFactor: Double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.MaxRFactor := AMaxRFactor;
end;

function TFitClient.GetBackFactor: Double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.BackFactor;
end;

procedure TFitClient.SetBackFactor(ABackFactor: Double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.BackFactor := ABackFactor;
end;

function TFitClient.GetCurveThresh: Double;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.CurveThresh;
end;

procedure TFitClient.SetCurveThresh(ACurveThresh: Double);
begin
    Assert(Assigned(FitProxy));
    FitProxy.CurveThresh := ACurveThresh;
end;

function TFitClient.GetCurveType: TCurveType;
begin
    Assert(Assigned(FitProxy));
    Result := FitProxy.CurveType;
end;

procedure TFitClient.SetCurveType(ACurveType: TCurveType);
begin
    Assert(Assigned(FitProxy));
    FitProxy.CurveType := ACurveType;
end;

{$IFNDEF EXCLUDE_SOMETHING}
procedure TFitClient.SetSpecialCurveParameters(
    ACurveExpr: string;
    CP: Curve_parameters    //  ravenstvo nil oznachaet
                            //  pervonachal'nuyu initsializatsiyu
    );
begin
    Assert(Assigned(FitProxy));
    FitProxy.SetSpecialCurveParameters(ACurveExpr, CP);
end;
{$ENDIF}
end.



