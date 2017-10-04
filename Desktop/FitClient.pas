//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
//      modul' soderzhit klass, realizuyuschiy logiku klienta
//      zdes' net nichego, chto svyazano s raschetami, i nichego,
//      chto svyazano s otobrazheniem interfeysa pol'zovatelya
unit FitClient;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, DataLoader, SelfCopied, SysUtils, MSCRDataClasses,
    Dialogs, FitClientProxy, SimpMath, CommonTypes;
    
type
    //  rezhimy vybora aktivnogo nabora tochek
    TSelMode = (ModeSelNone, ModeSelAreaLimits, ModeSelCharacteristicPoints,
                ModeSelGaussianBounds, ModeSelBackground, ModeSelPeakPos,
                ModeSelPeakBounds);
    //  sostoyaniya, sootvet. rezul'tatu otkrytiya fayla dannyh
    TOpenState = (OpenSuccess, OpenFailure);
    //  sostoyaniya vypolneniya dlitel'noy operatsii
    TAsyncState = (
        AsyncStart,         //  podgonki esche ne bylo
        AsyncWorks,         //  podgonka delaetsya
        AsyncDone           //  podgonka zavershena
        );

    //  sobytiya otobrazhenie dolzhny byt' raznymi, chtoby obespechit'
    //  vozmozhnost' otobrazheniya raznymi sposobami
    //  komponent, kotoryy budet real'no otobrazhat' krivye
    //  dolzhen hranit' u sebya vse ukazateli na
    //  otbrazhaemye komponenty, chtoby imet' vozmozhnost' skryt' ih...
    TPlotSpecimens = procedure(Sender: TObject;
        CurvesList: TSelfCopiedCompList;
        SpecimenList: TMSCRSpecimenList) of object;
    TFillDatasheetTable = procedure(
            Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList;
            GaussProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet;
            RFactorIntervals: TTitlePointsSet
            ) of object;
    TPlotSelectedPoints = procedure(
        Sender: TObject; SelectedPoints: TTitlePointsSet) of object;

    TPlotRFactorIntervals = procedure(
        Sender: TObject; RFactorIntervals: TTitlePointsSet) of object;
    THideRFactorIntervals = procedure(
        Sender: TObject; RFactorIntervals: TTitlePointsSet) of object;

    TPlotCurvePositions = procedure(
        Sender: TObject; CurvePositions: TTitlePointsSet) of object;
    THideCurvePositions = procedure(
        Sender: TObject; CurvePositions: TTitlePointsSet) of object;

    TPlotBackground = procedure(
        Sender: TObject; BackgroundPoints: TTitlePointsSet) of object;
    THideBackground = procedure(
        Sender: TObject; BackgroundPoints: TTitlePointsSet) of object;
        
    TPlotDataPoints = procedure(
        Sender: TObject; DataPoints: TTitlePointsSet) of object;
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

    //  komponent, kot. realizuet vsyu logiku klientskoy chasti programmy;
    //  ??? d.b. polnost'yu otdelen ot pol'zovatel'skogo interfeysa
    TFitClient = class(TComponent)
    protected
        FFitProxy: TFitClientProxy;
        DataLoader: TDataLoader;
        //  =========== kopii dannyh, vyvedennyh na grafik =====================
        //  !!! kopii dannyh nel'zya ne hranit', potomu chto inache
        //  ne obespechit' vozmozhnost' izmeneniya koordinaty X !!!
        //  dannye profilya
        FNeutronPointsSet: TTitlePointsSet;
        //  uchastok profilya, s kotorym pol'zovatel' rabotaet v dannyy moment
        SelectedArea: TTitlePointsSet;
        //  krivaya, poluchaemaya summoy vseh podgonyaemyh
        //  krivyh i sravnivaemaya s eksperimental'noy
        GaussProfile: TTitlePointsSet;
        DeltaProfile: TTitlePointsSet;
        //  tochki, vybrannye pol'zovatelem
        SelectedPoints: TTitlePointsSet;
        //  spisok tochek fona, ispol'zuemyy dlya perehoda
        //  m-u rezhimami ruchnogo i avtomaticheskogo vybora
        BackgroundPoints: TTitlePointsSet;
        //  pary tochek ogranichivayuschie diapazony rascheta R-Factor'a;
        //  !!! nikogda ne dolzhny byt' skryty, chtoby pol'zovatel'
        //  videl, v kakom rezhime schitaetsya R-Factor !!!
        RFactorIntervals: TTitlePointsSet;
        
        //  tochki privyazki ekzemplyarov patterna;
        //  ispol'zuyutsya tol'ko x-koordinaty tochek
        CurvePositions: TTitlePointsSet;
        //  nabor ob'ektov dlya rascheta krivyh ekzemplyarov patterna;
        //  kazhdyy iz ob'ektov soderzhit krivuyu - ekzemplyar patterna
        CurvesList: TSelfCopiedCompList;
        //  nabor ob'ektov-parametrov ekzemplyarov patterna
        SpecimenList: TMSCRSpecimenList;

        WaveLength: Double;
        procedure SetCurvesListLambda;

    protected
        CurMin: Double;
        //  v zavisimosti ot etogo priznaka v dal'neyshih operatsiyah
        //  ispol'zuyutsya libo dannye vybrannoy oblasti, libo dannye
        //  polnogo profilya
        FSelectedAreaMode: Boolean;
        FSelectionMode: TSelMode;
        FOpenState: TOpenState;
        FAsyncState: TAsyncState;
        // vyzyvaetsya dlya dobavleniya novoy tochki k zadannomu naboru tochek
        // !!! povtornyy vyzov dlya dannyh koordinat udalyaet tochku iz spiska;
        // pri etom spisok zamenyaetsya na novyy !!!
        procedure AddPoint(var Points: TTitlePointsSet;
            XValue, YValue: Double; Plot: TPlotProc);
        //  zamenyaet tochku i obnovlyaet grafik
        procedure ReplacePoint(Points: TTitlePointsSet;
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
            Plot: TPlotProc
            );

    protected
        //  =============== uk-li na metody otobrazheniya krivyh =================
        FOnPlotSpecimens: TPlotSpecimens;
        FOnFillDatasheetTable: TFillDatasheetTable;
        FOnPlotSelectedPoints: TPlotSelectedPoints;

        FOnPlotRFactorIntervals: TPlotRFactorIntervals;
        FOnHideRFactorIntervals: THideRFactorIntervals;

        FOnPlotCurvePositions: TPlotCurvePositions;
        FOnHideCurvePositions: THideCurvePositions;

        FOnPlotDataPoints: TPlotDataPoints;
        FOnHideDataPoints: THideDataPoints;

        FOnPlotSelectedArea: TPlotSelectedArea;

        FOnPlotBackground: TPlotBackground;
        FOnHideBackground: THideBackground;

        FOnPlotGaussProfile: TPlotGaussProfile;
        FOnPlotDeltaProfile: TPlotDeltaProfile;
        FOnRefresh: TRefresh;
        FOnRefreshPointsSet: TRefreshPointsSet;
        FOnClear: TClear;
        FOnHide: THide;
        //  funktsiya obratnogo vyzova zaversheniya asinhr. operatsii
        FAsyncOperationFinished: TAsyncOperationFinished;

        //  global'naya peremennaya dlya vyzova RefreshPointsSet, Hide
        ToRefresh: TNeutronPointsSet;
        //  obnovlyaet vse, chto vozmozhno otobrazhaya sostoyanie dannyh servera
        procedure UpdateAll;
        procedure HideSpecimens;
        //  ========= obolochki dlya vyzova vneshnih metodov otobrazheniya ==========
        //  !!! obolochki nuzhny dlya proverki prisoedineniya interfeysnyh
        //  metodov; esli takovye ne prisoedineny, to eto oznachaet, chto
        //  net sootvetstvuyuschih el-tov GUI !!!
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
        procedure FillDatasheetTable;

        //  vozvraschaet polnyy profil' ili vybrannyy v dannyy moment uchastok
        function GetProfilePointsSet: TTitlePointsSet;

        procedure SetSelectionMode(ASelectionMode: TSelMode);
        function GetSelectionMode: TSelMode;

        //  ================= izvlechenie/ustanovka poley servera ===============
        function GetMaxRFactor: Double;
        procedure SetMaxRFactor(AMaxRFactor: Double);
        function GetBackFactor: Double;
        procedure SetBackFactor(ABackFactor: Double);
        function GetCurveThresh: Double;
        procedure SetCurveThresh(ACurveThresh: Double);
        function GetCurveType: TCurveType;
        procedure SetCurveType(ACurveType: TCurveType);

        //  sozdaet spisok vybrannyh tochek i vyzyvaet vstavku
        //  sootvetstvuyuschego elementa v CheckListBox
        procedure RecreateAndShowSelectedPoints(Title: string);

        procedure InitDataPoints;
        //  !!! tol'ko ochischaet spisok !!!
        procedure RemoveDataPoints;
        //  !!! osvobozhdayut spiski !!!
        procedure RemoveSelectedPoints;
        procedure RemoveSelectedArea;
        procedure RemoveGaussProfile;
        procedure RemoveDeltaProfile;

        //  perenosit dannye iz zadannogo spiska v spisok vybrannoy oblasti
        procedure SelectAreaActual(ANeutronPoints: TNeutronPointsSet;
            StartPointIndex, StopPointIndex: LongInt);
        procedure CopyProfileDataFromLoader;
    public
        function GetSpecimenList: TMSCRSpecimenList;

        function GetBackgroundPoints: TNeutronPointsSet;
        function GetSelectedPoints: TNeutronPointsSet;
        function GetRFactorIntervals: TNeutronPointsSet;
        function GetCurvePositions: TNeutronPointsSet;
{$IFNDEF EXCLUDE_SOMETHING}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(
            ACurveExpr: string;
            CP: Curve_parameters    //  ravenstvo nil oznachaet
                                    //  pervonachal'nuyu initsializatsiyu
            );
{$ENDIF}
        //  !!!  vypolnyayut tol'ko ochistku spiskov !!!
        procedure RemoveRFactorIntervals;
        procedure RemoveCurvePositions;
        procedure RemoveBackgroundPoints;
        //  !!! vse vyzyvayut AddPoint !!!
        procedure AddPointToSelected(XValue, YValue: Double);
        procedure AddPointToBackground(XValue, YValue: Double);
        procedure AddPointToRFactorIntervals(XValue, YValue: Double);
        procedure AddPointToCurvePositions(XValue, YValue: Double);
        //  !!! vse vyzyvayut ReplacePoint !!!
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
        //  !!! ne ispol'zuetsya pri vvode tochek vruchnuyu, potomu chto
        //  trebuet prezhde vhoda v spets. sostoyanie, kot. imeet smysl
        //  tol'ko pri vizual'nom vybore tochek !!!
        procedure AddPointToActive(XValue, YValue: Double);
        //  vozvraschaet nabor, s kotorym pol'zovatel' rabotaet v dannyy moment,
        //  t.e. nabor, v kotoryy dobavlyayutsya ili iz kotorogo udalyayutsya tochki
        function GetCurrentPointsSet: TTitlePointsSet;

        //  vypolnyaet ochistku oblasti grafika i perenosit chast' dannyh
        //  iz dannyh profilya v spisok dannyh vybrannoy oblasti
        procedure SelectArea(StartPointIndex, StopPointIndex: LongInt);
        procedure ReturnToTotalProfile;

        procedure SetWaveLength(AWaveLength: Double);
        function GetWaveLength: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure LoadDataSet(FileName: string);
        procedure Reload;
        
        //  ========== funktsii obratnogo vyzova so storony servera =============
        procedure ShowCurMin(Min: Double);
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        //  ================ perehodniki k metodam servera =====================
        //  ne dolzhny vydavat' soobscheniy, poskol'ku eto
        //  delo modulya gui, poetomu vybrasyvayut isklyucheniya
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
        //  poluchaet sostoyanie asinhr. operatsii neposredstvenno ot servera
        function AsyncOper: Boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        procedure CreateSpecimenList;

        //  =============== perehodniki k polyam servera ========================
        property MaxRFactor: Double read GetMaxRFactor write SetMaxRFactor;
        property BackFactor: Double read GetBackFactor write SetBackFactor;
        property CurveThresh: Double read GetCurveThresh write SetCurveThresh;
        property CurveType: TCurveType read GetCurveType write SetCurveType;

        //  sobytiya otobrazheniya vyzyvayutsya iz odnoimennyh metodov dlya
        //  obespecheniya vozmozhnosti sinhronizatsii s glavnym potokom programmy
        //  !!! ukazyvayut na metody TIIViewer !!!
        property OnPlotSpecimens: TPlotSpecimens
            read FOnPlotSpecimens write FOnPlotSpecimens;
        property OnFillDatasheetTable: TFillDatasheetTable
            read FOnFillDatasheetTable write FOnFillDatasheetTable;
        property OnPlotSelectedPoints: TPlotSelectedPoints
            read FOnPlotSelectedPoints write FOnPlotSelectedPoints;

        property OnPlotRFactorIntervals: TPlotRFactorIntervals
            read FOnPlotRFactorIntervals write FOnPlotRFactorIntervals;
        property OnHideRFactorIntervals: THideRFactorIntervals
            read FOnHideRFactorIntervals write FOnHideRFactorIntervals;

        property OnPlotCurvePositions: TPlotCurvePositions
            read FOnPlotCurvePositions write FOnPlotCurvePositions;
        property OnHideCurvePositions: THideCurvePositions
            read FOnHideCurvePositions write FOnHideCurvePositions;

        //  sobytie, vyzyvaemoe dlya risovaniya dannyh neytronogrammy
        property OnPlotDataPoints: TPlotDataPoints
            read FOnPlotDataPoints write FOnPlotDataPoints;
        property OnHideDataPoints: THideDataPoints
            read FOnHideDataPoints write FOnHideDataPoints;

        property OnPlotSelectedArea: TPlotSelectedArea
            read FOnPlotSelectedArea write FOnPlotSelectedArea;

        property OnPlotBackground: TPlotBackground
            read FOnPlotBackground write FOnPlotBackground;
        property OnHideBackground: THideBackground
            read FOnHideBackground write FOnHideBackground;

        property OnPlotGaussProfile: TPlotGaussProfile
            read FOnPlotGaussProfile write FOnPlotGaussProfile;
        property OnPlotDeltaProfile: TPlotDeltaProfile
            read FOnPlotDeltaProfile write FOnPlotDeltaProfile;
        //  sobytie, vyzyvaemoe dlya obnovleniya vseh profiley
        property OnRefresh: TRefresh read FOnRefresh write FOnRefresh;
        //  sobytie, vyzyvaemoe pri dobavlenii novoy tochki v nabor
        //  ili pri izmenenii tochki v nabore
        property OnRefreshPointsSet: TRefreshPointsSet
            read FOnRefreshPointsSet write FOnRefreshPointsSet;
        //  sobytie, vyzyvaemoe pered ochistkoy dannyh neytronogrammy
        property OnClear: TClear read FOnClear write FOnClear;
        property OnHide: THide read FOnHide write FOnHide;
        //  metody obratnogo vyzova pol'zovatel'skogo interfeysa;
        //  vyzyvayutsya iz osnovnogo potoka klientskoy programmy,
        //  mogut vybrasyvat' isklyucheniya;
        //  dopuskaetsya ravenstvo nil
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
    RFactorIntervalsName: string = 'Spec.app.intervals';
    BackgroundPointsName: string = 'Background points';
    CurvePositionsName: string = 'Spec.positions';
    SelectedAreaName: string = 'Selected area';
    SummarizedName: string = 'Summarized';
    ArgumentName: string = 'Position';
    ProfileName: string = 'Data';
    NumberName: string = 'Number';
    ValueName: string = 'Amplitude';
    DeltaName: string = 'Difference';
    StartName: string = 'Starting Position';
    StopName: string = 'Final Position';

implementation

uses Unit1; // ??? ubrat' svyazi s Form1

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
    DataLoader.Free;
    inherited Destroy;
end;

constructor TFitClient.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    DataLoader := TDATFileLoader.Create(nil);
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
    CurMin := Min;  //  prosto zapominaem zdes' znachenie
    FormMain.ShowTime;
    //???
    UpdateAll;
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
    FillDatasheetTable;
end;

procedure TFitClient.Done;
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(FitProxy));

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

    UpdateAll;
    FAsyncState := AsyncDone;

    //  vyzyvaetsya metod glavnoy formy
    if Assigned(OnAsyncOperationFinished) then OnAsyncOperationFinished(Self);
    FormMain.ShowHint(HintDone);
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
    if Assigned(OnPlotSpecimens) then
        OnPlotSpecimens(Self, CurvesList, SpecimenList);
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
    if Assigned(OnPlotSelectedPoints) then
        OnPlotSelectedPoints(Self, SelectedPoints);
end;

procedure TFitClient.PlotRFactorIntervals;
begin
    if Assigned(OnPlotRFactorIntervals) then
        OnPlotRFactorIntervals(Self, RFactorIntervals);
end;

procedure TFitClient.HideRFactorIntervals;
begin
    if Assigned(OnHideRFactorIntervals) then
        OnHideRFactorIntervals(Self, RFactorIntervals);
end;

procedure TFitClient.PlotCurvePositions;
begin
    if Assigned(OnPlotCurvePositions) then
        OnPlotCurvePositions(Self, CurvePositions);
end;

procedure TFitClient.HideCurvePositions;
begin
    if Assigned(OnHideCurvePositions) then
        OnHideCurvePositions(Self, CurvePositions);
end;

procedure TFitClient.FillDatasheetTable;
begin
    if Assigned(OnFillDatasheetTable) then
        OnFillDatasheetTable(GetProfilePointsSet, CurvesList,
            GaussProfile, DeltaProfile, RFactorIntervals);
end;

procedure TFitClient.PlotDataPoints;
begin
    if Assigned(OnPlotDataPoints) then
        OnPlotDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.HideDataPoints;
begin
    if Assigned(OnHideDataPoints) then
        OnHideDataPoints(Self, NeutronPointsSet);
end;

procedure TFitClient.PlotSelectedArea;
begin
    if Assigned(OnPlotSelectedArea) then
        OnPlotSelectedArea(Self, SelectedArea);
end;

procedure TFitClient.PlotBackground;
begin
    if Assigned(OnPlotBackground) then
        OnPlotBackground(Self, BackgroundPoints);
end;

procedure TFitClient.HideBackground;
begin
    if Assigned(OnHideBackground) then
        OnHideBackground(Self, BackgroundPoints);
end;

procedure TFitClient.PlotGaussProfile;
begin
    if Assigned(OnPlotGaussProfile) then
        OnPlotGaussProfile(Self, GaussProfile);
end;

procedure TFitClient.PlotDeltaProfile;
begin
    if Assigned(OnPlotDeltaProfile) then
        OnPlotDeltaProfile(Self, DeltaProfile);
end;

procedure TFitClient.Refresh;
begin
    if Assigned(OnRefresh) then OnRefresh(Self);
end;

procedure TFitClient.RefreshPointsSet;
begin
    Assert(Assigned(ToRefresh));

    if Assigned(OnRefreshPointsSet) then
        OnRefreshPointsSet(Self, ToRefresh);
    ToRefresh := nil;
end;

procedure TFitClient.Clear;
begin
    if Assigned(OnClear) then OnClear(Self);
end;

procedure TFitClient.Hide;
begin
    if Assigned(OnHide) then OnHide(Self, ToRefresh);
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
    Assert(Assigned(DataLoader));

    RemoveDataPoints;
    NeutronPointsSet.Free; NeutronPointsSet := nil;
    NeutronPointsSet := DataLoader.GetPointsSetCopy;
    InitDataPoints;
end;

procedure TFitClient.LoadDataSet(FileName: string);
begin
    //  zdes' eto nedopustimye sostoyaniya
    Assert(Assigned(DataLoader));
    Assert(Assigned(FitProxy));

    DataLoader.LoadDataSet(FileName);
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
    Assert(Assigned(DataLoader));
    Assert(Assigned(FitProxy));

    DataLoader.Reload;
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



