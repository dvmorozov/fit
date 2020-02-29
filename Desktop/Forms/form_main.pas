{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TFormMain.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit form_main;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ExtCtrls, StdCtrls, Menus, points_set, fit_viewer, ComCtrls,
    fit_client, NumericGrid, CheckLst, mscr_specimen_list, LResources, TAGraph,
    ActnList, app_settings, Laz_XMLCfg, common_types, neutron_points_set,
    int_points_set, curve_points_set, user_points_set, gauss_points_set,
    pseudo_voigt_points_set, asym_pseudo_voigt_points_set, lorentz_points_set,
    two_branches_pseudo_voigt_points_set, named_points_set,
    log
{$IFDEF _WINDOWS}
    , MyExceptions, Windows
{$ENDIF}
    ;

type
    { States of results grid window. }
    TResState = (
        { Window is invisible. }
        GridInvisible,
        { Window is visible but selection area is empty. }
        GridSelEmpty,
        { A few rows are selected. }
        GridSelNonEmpty,
        { Whole table is selected. }
        GridSelAll
        );
    { States of graph output. }
    TViewState = (GraphEmpty, GraphNotEmpty);

  { TFormMain }
  TFormMain = class(TForm)
    ActionEnableCurveScaling: TAction;
    ActionEnBackVariation: TAction;
    ActionPatternType: TAction;
    ActionAnimationMode: TAction;
    ActionSelSpecPosAtEveryPoint: TAction;
    ActionAbout: TAction;
    ActionGlossary: TAction;
    ActionViewMarkers: TAction;
    ActionZoomOut: TAction;
    ActionZoomIn: TAction;
    ActionSelectAll: TAction;
    ActionDelete: TAction;
    ActionCopy: TAction;
    ActionSetMaxRFactor: TAction;
    ActionStopFitting: TAction;
    ActionFitMinDifference: TAction;
    ActionFitMinNumberOfSpec: TAction;
    ActionDoAllAuto: TAction;
    ActionSaveAsText: TAction;
    ActionSelCurveBounds: TAction;
    ActionRemoveSpecPos: TAction;
    ActionSelSpecPosVis: TAction;
    ActionSelSpecPosAuto: TAction;
    ActionRemoveRFactorIntervals: TAction;
    ActionSelRFactorIntervalsVis: TAction;
    ActionSelRFactorIntervalsAuto: TAction;
    ActionImport: TAction;
    ActionReload: TAction;
    ActionSelEntireProf: TAction;
    ActionSelArea: TAction;
    ActionSelAreaLimits: TAction;
    ActionRmBackSelected: TAction;
    ActionRmBackAuto: TAction;
    ActionRemoveBack: TAction;
    ActionSelBackVis: TAction;
    ActionSelBackAuto: TAction;
    ActionSetPortion: TAction;
    ActionSmooth: TAction;
    ActionSelCharacteristicPoints: TAction;
    ActionQuit: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    ButCopy4: TButton;
    ButSaveAsText4: TButton;
    CheckListBoxLegend: TCheckListBox;
    EditBalloonChart: TEdit;
    EditBalloonGridSpecPositions: TEdit;
    EditBalloonGridParameters: TEdit;
    EditBalloonGridDatasheet: TEdit;
    EditBalloonGridBackground: TEdit;
    EditBalloonGridData: TEdit;
    EditBalloonGridIntervals: TEdit;
    GridBackground: TNumericGrid;
    GridSpecPositions: TNumericGrid;
    GridData: TNumericGrid;
    GridDatasheet: TNumericGrid;
    GridParameters: TNumericGrid;
    GridIntervals: TNumericGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelAngle: TLabel;
    LabelIntensity: TLabel;
    LabelMin: TLabel;
    MainMenu1: TMainMenu;
    Data: TMenuItem;
    DoAllAuto: TMenuItem;
    MenuItem1: TMenuItem;
    FitMinNumberOfSpec: TMenuItem;
    FitMinDifference: TMenuItem;
    ArgumentTransformation: TMenuItem;
    CreateRule: TMenuItem;
    BackPoints: TMenuItem;
    CurveScalingEnabled: TMenuItem;
    SpecAtEveryPoint: TMenuItem;
    MenuItem5: TMenuItem;
    BackEnableVariation: TMenuItem;
    PanelParameters: TPanel;
    PanelIntervals: TPanel;
    PanelDatasheet: TPanel;
    PanelSpecPositions: TPanel;
    PanelBackground: TPanel;
    SelArea: TMenuItem;
    MenuItem18: TMenuItem;
    SelEntireProf: TMenuItem;
    Range: TMenuItem;
    SelAreaLimits: TMenuItem;
    SelBackAuto: TMenuItem;
    RmBack: TMenuItem;
    RmBackAuto: TMenuItem;
    SelBackVis: TMenuItem;
    RemoveBack: TMenuItem;
    RmBackSelected: TMenuItem;
    Back: TMenuItem;
    SetPortion: TMenuItem;
    PageControl1: TPageControl;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    PanelChart: TPanel;
    Panel2: TPanel;
    PanelRight: TPanel;
    PeakPos: TMenuItem;
    SetWavelength: TMenuItem;
    ScrollBarX: TScrollBar;
    ScrollBarY: TScrollBar;
    SplitterChartRight: TSplitter;
    SplitterLeftChart: TSplitter;
    SplitterBottom: TSplitter;
    TabSheetSpecPositions: TTabSheet;
    TabSheetBackground: TTabSheet;
    TabSheetParameters: TTabSheet;
    TabSheetIntervals: TTabSheet;
    TabSheetDatasheet: TTabSheet;
    Chart: TTAChart;
    TimerAsync: TTimer;
    TimerBalloonHide: TTimer;
    TimerBalloonShow: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UseRule: TMenuItem;
    SinThetaLambda: TMenuItem;
    N2Theta: TMenuItem;
    Theta: TMenuItem;
    SetRuleParameters: TMenuItem;
    SelRFactorIntervalsAuto: TMenuItem;
    SelRFactorIntervalsVis: TMenuItem;
    MenuItem20: TMenuItem;
    RemoveRFactorIntervals: TMenuItem;
    RFactorIntervals: TMenuItem;
    SelSpecPosAuto: TMenuItem;
    SelSpecPosVis: TMenuItem;
    MenuItem19: TMenuItem;
    RemoveSpecPos: TMenuItem;
    StopFitting: TMenuItem;
    MenuItem4: TMenuItem;
    Smooth: TMenuItem;
    SelCharacteristicPoints: TMenuItem;
    SelCurveBounds: TMenuItem;
    MenuItem2: TMenuItem;
    SelCurveLorentzian: TMenuItem;
    SaveAsText: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem8: TMenuItem;
    SelCurveType: TMenuItem;
    Fitting: TMenuItem;
    SetMaxRFactor: TMenuItem;
    Glossary: TMenuItem;
    MenuItem12: TMenuItem;
    Quit: TMenuItem;
    MenuItem9: TMenuItem;
    Reload: TMenuItem;
    Model: TMenuItem;
    Import: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar: TStatusBar;
    CheckStateTimer: TTimer;
    View: TMenuItem;
    ZoomIn: TMenuItem;
    ZoomOut: TMenuItem;
    ImageList1: TImageList;
    Help1: TMenuItem;
    About: TMenuItem;
    ViewModeMenu: TPopupMenu;
    Theta2: TMenuItem;
    N2Theta2: TMenuItem;
    SinThetaLambda2: TMenuItem;
    ImageList2: TImageList;
    N4: TMenuItem;
    ViewMarkers: TMenuItem;
    SaveDialog1: TSaveDialog;
    Edit1: TMenuItem;
    Copy: TMenuItem;
    Delete: TMenuItem;
    Separator1: TMenuItem;
    SelectAll: TMenuItem;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAnimationModeExecute(Sender: TObject);
    procedure ActionAnimationModeUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionDoAllAutoExecute(Sender: TObject);
    procedure ActionEnBackVariationExecute(Sender: TObject);
    procedure ActionEnBackVariationUpdate(Sender: TObject);
    procedure ActionEnableCurveScalingExecute(Sender: TObject);
    procedure ActionEnableCurveScalingUpdate(Sender: TObject);
    procedure ActionFitMinDifferenceExecute(Sender: TObject);
    procedure ActionFitMinNumberOfSpecExecute(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure ActionPatternTypeUpdate(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ActionRemoveBackExecute(Sender: TObject);
    procedure ActionRemoveRFactorIntervalsExecute(Sender: TObject);
    procedure ActionRemoveSpecPosExecute(Sender: TObject);
    procedure ActionRmBackAutoExecute(Sender: TObject);
    procedure ActionRmBackSelectedExecute(Sender: TObject);
    procedure ActionSaveAsTextExecute(Sender: TObject);
    procedure ActionSelAreaExecute(Sender: TObject);
    procedure ActionSelAreaLimitsExecute(Sender: TObject);
    procedure ActionSelBackAutoExecute(Sender: TObject);
    procedure ActionSelBackVisExecute(Sender: TObject);
    procedure ActionSelCharacteristicPointsExecute(Sender: TObject);
    procedure ActionSelCurveBoundsExecute(Sender: TObject);
    procedure ActionSelCurveExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionSelEntireProfExecute(Sender: TObject);
    procedure ActionSelSpecPosAtEveryPointExecute(Sender: TObject);
    procedure ActionSelSpecPosAutoExecute(Sender: TObject);
    procedure ActionSelRFactorIntervalsAutoExecute(Sender: TObject);
    procedure ActionSelRFactorIntervalsVisExecute(Sender: TObject);
    procedure ActionSelSpecPosVisExecute(Sender: TObject);
    procedure ActionSetMaxRFactorExecute(Sender: TObject);
    procedure ActionSetPortionExecute(Sender: TObject);
    procedure ActionSmoothExecute(Sender: TObject);
    procedure ActionStopFittingExecute(Sender: TObject);
    procedure ActionViewMarkersExecute(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ApplicationProperties1Hint(Sender: TObject);
    procedure ButAddSelectedDataPointToPositionsClick(Sender: TObject);
    procedure ButAddSelectedPointToIntervalsClick(Sender: TObject);
    procedure ButAddSelectedDataPointClick(Sender: TObject);
    procedure CheckListBoxLegendDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CheckStateTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDataEditingDone(Sender: TObject);
    procedure GridDataSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure FormCreate(Sender: TObject);
    procedure ModelClick(Sender: TObject);
    procedure PanelTopClick(Sender: TObject);
    procedure PeakPosClick(Sender: TObject);
    procedure ScrollBarXChange(Sender: TObject);
    procedure ScrollBarYChange(Sender: TObject);
    procedure TabSheetBackgroundResize(Sender: TObject);
    procedure TabSheetBackgroundShow(Sender: TObject);
    procedure ChartDrawReticule(Sender: TComponent; IndexSerie, Index, Xi,
      Yi: Integer; Xg, Yg: Double);
    procedure ChartMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartMouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabSheetDatasheetShow(Sender: TObject);
    procedure TabSheetIntervalsShow(Sender: TObject);
    procedure TabSheetParametersShow(Sender: TObject);
    procedure TabSheetSpecPositionsShow(Sender: TObject);
    procedure TAChart1Zoom(Sender: TComponent);
    procedure TimerAsyncTimer(Sender: TObject);
    procedure TimerBalloonHideTimer(Sender: TObject);
    procedure TimerBalloonShowTimer(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckListBoxLegendKeyPress(Sender: TObject; var Key: Char);
    procedure CheckListBoxLegendClick(Sender: TObject);
    procedure SinThetaLambdaClick(Sender: TObject);
    procedure ThetaClick(Sender: TObject);
    procedure N2ThetaClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetWavelengthClick(Sender: TObject);

  protected
    { Initial values set up just after file loading. }
    InitXGraphMax, InitXGraphMin, InitYGraphMax, InitYGraphMin: Double;

    { These variables are used for separating clicks from area selection. }
    DownX, DownY, UpX, UpY: Integer;

    { Saved content of edited cells. }
    SavedPos, SavedAmp: string;
    { Protects from reentrance into editing finalization. }
    EditDone: Boolean;
    { Indicates that hint message should be displayed. }
    FHandleEditHint: Boolean;
    procedure SetHandleEditHint(EditHint: Boolean);

  protected
    { The object created event EditDone. }
    SenderEditHint: TNumericGrid;

    HintMessage: string;
    DrawReticule: Boolean;

    { Callback for calculating object. }
    procedure AsyncOperationFinished(Sender: TObject);
    { Wrapper. }
    procedure SubtractAllBackground(Auto: Boolean);

    procedure SetSelectionMode(ASelectionMode: TSelMode);
    procedure SetResState(State: TResState);
    procedure SetAsyncState(State: TAsyncState);
    { Sets states of controls according to state of file. }
    procedure SetOpenState(State: TOpenState);
    { Sets states of controls responsible for changing chart display mode. }
    procedure SetViewState(State: TViewState);
    procedure UpdateBarsPos;
    { The event OnClick of Chart arises between MouseUp and MouseDown.
      That is why OnClick is not used. }
    procedure OnChartClick;
    function GetConfigFileName: string;
    procedure OnFindComponentClass(Reader: TReader;
        const ClassName: string; var ComponentClass: TComponentClass);
    procedure OnException(Sender: TObject; E: Exception);
    procedure DoEditHint;

{$IFDEF _WINDOWS}
    procedure OnDeleteUserCurveClick(Sender: TObject);
    procedure OnUserCurveClick(Sender: TObject);
{$ENDIF}

  public
    { Application settings. Type should be checked. }
    Settings: Settings_v1;

    FitViewer: TFitViewer;
    { Index of curve on which the first click was. It is used in the cases when points of only one curve can be selected. }
    ActiveNumber: LongInt;
    { Collection should be passive. Object is set from TFitViewer and is checked on Nil. }
    SpecimenList: TMSCRSpecimenList;
    { Indicates that data in tables were changed. }
    ModifiedParameters: Boolean;
    ModifiedDatasheet: Boolean;
    { Index of a serie point of which is selected at the moment. }
    CurSerieIndex: LongInt;
    { Index of selected value. }
    ValueIndex: LongInt;

{$IFDEF _WINDOWS}
    //procedure AddDummyCurve;
    //procedure DeleteDummyCurve;
    procedure ReadUserCurves;
    procedure WriteUserCurve(ct: Curve_type);
    { Creates all menu items corresponding to user defined curves. }
    procedure CreateUserCurveMenus;
    { Adds menu item corresponding to user defined curve. }
    procedure AddUserCurveMenu(ct: Curve_type);
    procedure DeleteUserCurve(ct: Curve_type);
{$ENDIF}

    { Tries read the user settings object. In the case of failure creates new object. }
    procedure ReadSettings;
    procedure WriteSettings;
    { Searches files containing parameters of user defined curves and loads them. }

    { Creates single menu item. }
    procedure CreateMenuItem(Pos: LongInt; ct: Curve_type;
        ParentMenu: TMenuItem; OnClick: TNotifyEvent);

    procedure CheckListBoxChanged;
    { Saving curve parameters into text file. }
    function SaveTableAsText(GridData: TNumericGrid): Boolean;
    { Saving curve parameters into XML file. }
    procedure SaveTable;
    { Loading curve parameters from XML file. }
    procedure LoadTable;

    procedure ShowHint(const Hint: string);
    procedure ShowTime;
    procedure ShowRFactor;

    procedure LoadDataFile(FileName: string);

    property HandleEditHint: Boolean read FHandleEditHint
        write SetHandleEditHint;
  end;

var
    FormMain: TFormMain;

const
    crCursorDrag:       TCursor = 6;
    crCursorSelect:     TCursor = 7;

const
    StopVisualSel:      string = 'Stop Visual Selection';
    StartVisualSel:     string = 'Start Visual Selection';
    StopVisPosSel:      string = 'Stop Visual Position Selection';
    StartVisPosSel:     string = 'Start Visual Position Selection';

    HintSecondFinish:   string = 'Now you can pick a second point - "FINISH"';
    HintSecondPeak:     string = 'Now you can pick a second point - "PEAK"';
    HintSelectArea:     string = 'Now you can pick the menu item "Select Area"';
    HintThirdFinish:    string = 'Now you can pick a third point - "FINISH"';
    HintMovePeak:       string =
        'Now you can pick the menu item "Move Peak to Results"';
    HintNextPoint:      string =
        'Now you can pick a next point or the menu item "Find Gaussians"';
    HintNextBackPoint:  string =
        'Now you can pick a next point or the menu item "Remove Background"';
    HintNextPointOdd:   string = 'Now you can pick a left point of peak';
    HintNextPointEven:  string = 'Now you can pick a right point of peak';
    HintFirstStart:     string = 'Now you can pick a first point - "START"';
    HintFirst:          string = 'Now you can pick a first point';
    HintMain:           string =
        'Drag mouse from top-left to bottom-right to zoom';
    HintWait:           string = 'Calculation started. Please wait';
{$IFDEF _WINDOWS}
    MenuDelUserCapt:    string = 'Delete User';
{$ENDIF}

implementation

uses input_wavelength_dialog, input_max_rfactor_dialog,
    input_back_factor_dialog, about_box_dialog, app, int_curve_type_iterator,
    int_curve_type_selector, curve_types_singleton;

(*
function OFNHookProc(
    Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
end;

function OFNHookProcOldStyle(
    hdlg: HWnd;      // handle to dialog box
    uiMsg: UINT;     // message identifier
    wParam: WPARAM;  // message parameter
    lParam: LPARAM   // message parameter
    ): UINT_PTR; stdcall;
begin
    Result := 0;
end;
*)

{$hints off}
procedure TFormMain.CheckListBoxLegendDrawItem(
    Control: TWinControl; Index: Integer;
    ARect: TRect; State: TOwnerDrawState);
var LB: TCheckListBox;
    TS: TTASerie;
    Size: Longint;
    Color: TColor;
    Square: TRect;
begin
    TS := TTASerie(Chart.GetSerie(Index)); //  otsutstvie serii s dannym
                                              //  indeksom est' nedopustimoe
                                              //  sostoyanie => d.b. isklyuchenie
    LB := TCheckListBox(Control);
    Size := ARect.Bottom - ARect.Top;
    Color := LB.Canvas.Brush.Color;

    //if (LB.ItemIndex <> Index) or (LB.ItemIndex = -1) then
        LB.Canvas.Brush.Color := LB.Color;

    Inc(ARect.Bottom);  //  !!! nuzhno !!!
    Inc(ARect.Bottom);  //  !!! nuzhno !!!
    //  ochistka
    LB.Canvas.FillRect(ARect);
    LB.Canvas.Brush.Color := LB.Color;

    //  mesto pod galku
    Square.Top := ARect.Top + 1;
    Square.Bottom := Square.Top + Size;
    Square.Left := 1;
    Square.Right := Square.Left + Size;
    LB.Canvas.Rectangle(Square);

    //  zapolnenie
    Inc(Square.Left);
    Inc(Square.Top);
    LB.Canvas.FillRect(Square);

    //  marker vybrannogo elementa
    if LB.Checked[Index] then
    begin
        //  zapolnenie
        Square.Left := Square.Left + 2;
        Square.Top := Square.Top + 2;
        Square.Right := Square.Right - 2;
        Square.Bottom := Square.Bottom - 2;
        LB.Canvas.Brush.Color := LB.Canvas.Pen.Color;
        LB.Canvas.FillRect(Square);
    end;

    //  ramka
    Square.Top := ARect.Top + 1;
    Square.Bottom := Square.Top + Size;
    Square.Left := ARect.Right - 1 - Size;
    Square.Right := Square.Left + Size;
    LB.Canvas.Rectangle(Square);

    //  zapolnenie
    Inc(Square.Left);
    Inc(Square.Top);
    LB.Canvas.Brush.Color := TS.SeriesColor;
    LB.Canvas.FillRect(Square);

    LB.Canvas.Brush.Color := Color;           //  vosstanovlenie tsveta
    LB.Canvas.TextOut(
        ARect.Left + Size + 6, ARect.Top, LB.Items.Strings[Index]);
end;
{$hints on}

procedure TFormMain.CheckStateTimerTimer(Sender: TObject);
begin
    //  eta proverka pochemu-to ne srabatyvaet
    //if csDestroying in ComponentState then Exit;

    //  zdes' proveryayutsya sostoyaniya ob'ektov programmy i
    //  v sootvetstvuyuschee sostoyanie ustanavlivayutsya el-ty
    //  upravleniya
    if Chart.SeriesCount <> 0 then SetViewState(GraphNotEmpty)
    else SetViewState(GraphEmpty);

    if not (ActiveControl is TNumericGrid) then
        SetResState(GridInvisible)
    else
    begin
        with ActiveControl as TNumericGrid do
        begin
            if (Selection.Top = Selection.Bottom) and
               (Selection.Right = Selection.Left) then
                 SetResState(GridSelEmpty)
            else
            begin
                if (Selection.Left = FixedCols) and
                   (Selection.Right = ColCount - 1) and
                   (Selection.Top = FixedRows) and
                   (Selection.Bottom = RowCount - 1) then
                        SetResState(GridSelAll)
                else SetResState(GridSelNonEmpty);
            end;
        end;
    end;

    Assert(Assigned(FitClientApp_));
    SetOpenState(FitClientApp_.FitClient.OpenState);
end;

procedure TFormMain.ApplicationProperties1Hint(Sender: TObject);
begin
    ShowHint(Application.Hint);
end;

procedure TFormMain.ActionQuitExecute(Sender: TObject);
begin
    Close;
end;

procedure TFormMain.LoadDataFile(FileName: string);
begin
    //  v sluchae oshibki zagruzki d. vybrasyvat'sya isklyuchenie
    FitClientApp_.FitClient.LoadDataSet(FileName);
    Caption := ApplicationProperties1.Title +
        ' - ' + ExtractFileName(FileName);
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    InitXGraphMax := Chart.XGraphMax;
    InitXGraphMin := Chart.XGraphMin;
    InitYGraphMax := Chart.YGraphMax;
    InitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionImportExecute(Sender: TObject);
(*
var ofn: OPENFILENAME;
    Res: WINBOOL;
    FileName: array[0..MAX_PATH] of Char;

    dummy: DWORD;
*)
begin
    (*
    FillChar(ofn, SizeOf(ofn), 0);
    FillChar(FileName, SizeOf(FileName), 0);
    ofn.lStructSize := SizeOf(ofn);
    ofn.lpstrFile := @FileName;
    ofn.nMaxFile := SizeOf(FileName);
    //ofn.Flags :=;
    ofn.lpfnHook := OFNHookProcOldStyle;
    Res := GetOpenFileName(@ofn);
    if not Res then
    begin
         dummy := CommDlgExtendedError;
    end;
    *)
    //  !!! pri rabote pod otladchikom nablyudayutsya sboi pri
    //  vyvode podskazki v okne vybora fayla, ne isklyucheno,
    //  chto pri ruchnoy obrabotke sootvetstvuyuschih sobscheniy v
    //  hook'e mozhno ih izbezhat'; v normal'nom rezhime raboty
    //  sboi ne zamecheny, poetomu poka ostavleno tak kak est' !!!
    with OpenDialog1 do
    begin
        InitialDir := ExtractFilePath(Application.ExeName);
        if Execute then
        begin
            if FileExists(FileName) then
            begin
                LoadDataFile(FileName);
            end;
        end;
    end;{with OpenDialog do...}
end;

procedure TFormMain.ActionPatternTypeUpdate(Sender: TObject);
var CurveTypeIterator: ICurveTypeIterator;
    CurveTypeSelector: ICurveTypeSelector;
    MenuItem: TMenuItem;
    Index: Integer;
    SelectedCurveTypeId: TCurveTypeId;
begin
    CurveTypeIterator := TCurveTypesSingleton.CreateCurveTypeIterator;
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    SelectedCurveTypeId := CurveTypeSelector.GetSelectedCurveType;
    //  Clears menu.
    SelCurveType.Clear;
    //  Creates menu items for curve types.
    //  The list must contain at least one item.
    CurveTypeIterator.FirstCurveType;
    Index := 0;
    while True do
    begin
        MenuItem := TMenuItem.Create(SelCurveType);
        MenuItem.Name := 'CurveType' + IntToStr(Index);
        MenuItem.Tag := CurveTypeIterator.GetCurveTypeTag(CurveTypeIterator.GetCurveTypeId);
        Inc(Index);
        MenuItem.OnClick := ActionSelCurveExecute;
        //  Caption must be set after action to overwrite action attribute.
        MenuItem.Caption := CurveTypeIterator.GetCurveTypeName;
        //  Sets checked state.
        if IsEqualGUID(SelectedCurveTypeId, CurveTypeIterator.GetCurveTypeId) then
            MenuItem.Checked := True;

        SelCurveType.Add(MenuItem);
        //  The last item should be processed as well.
        if CurveTypeIterator.EndCurveType then Break
        else
            CurveTypeIterator.NextCurveType;
    end;
end;

procedure TFormMain.ActionDeleteExecute(Sender: TObject);
var i, RowsToDelete, Index: LongInt;
begin
    //  TODO: obobschit' na vse gridy
    if Assigned(SpecimenList) then
    begin
        with GridParameters do
        begin
            if (Selection.Left = FixedCols) and
               (Selection.Right = ColCount - 1) then
            begin
                //  udalit' mozhno tol'ko tselye stroki
                RowsToDelete := Selection.Bottom - Selection.Top + 1;
                Index := Selection.Top - FixedRows;
                for i := 1 to RowsToDelete do SpecimenList.Delete(Index);
            end;
        end;
        SpecimenList.GridAssign(GridParameters);
        ModifiedParameters := True;
    end;
end;

procedure TFormMain.ActionCopyExecute(Sender: TObject);
begin
    if ActiveControl is TNumericGrid then
        with ActiveControl as TNumericGrid do CopyToClipBoard;
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
    AboutBox.ShowModal;
end;

procedure TFormMain.ActionAnimationModeExecute(Sender: TObject);
begin
    FitViewer.SetAnimationMode(not FitViewer.GetAnimationMode);
end;

procedure TFormMain.ActionAnimationModeUpdate(Sender: TObject);
begin
    ActionAnimationMode.Checked := FitViewer.GetAnimationMode;
end;

procedure TFormMain.ActionDoAllAutoExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    //  polnyy perezapusk rascheta - flag proveryat' ne nuzhno
    FitClientApp_.FitClient.DoAllAutomatically;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionEnBackVariationExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.BackgroundVariationEnabled :=
        not FitClientApp_.FitClient.BackgroundVariationEnabled;
    ActionEnBackVariationUpdate(Sender);
end;

procedure TFormMain.ActionEnBackVariationUpdate(Sender: TObject);
begin
    ActionEnBackVariation.Checked := FitClientApp_.FitClient.BackgroundVariationEnabled;
end;

procedure TFormMain.ActionEnableCurveScalingExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.CurveScalingEnabled :=
        not FitClientApp_.FitClient.CurveScalingEnabled;
    ActionEnableCurveScalingUpdate(Sender);
end;

procedure TFormMain.ActionEnableCurveScalingUpdate(Sender: TObject);
begin
    ActionEnableCurveScaling.Checked := FitClientApp_.FitClient.CurveScalingEnabled;
end;

procedure TFormMain.ActionFitMinDifferenceExecute(Sender: TObject);
begin
    ShowHint(HintMain);
    SelSpecPosVis.Checked := False;
    FitClientApp_.FitClient.FindGausses;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionFitMinNumberOfSpecExecute(Sender: TObject);
begin
    ShowHint(HintMain);
    SelSpecPosVis.Checked := False;
    FitClientApp_.FitClient.FindGaussesSequentially;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionReloadExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.Reload;
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    InitXGraphMax := Chart.XGraphMax;
    InitXGraphMin := Chart.XGraphMin;
    InitYGraphMax := Chart.YGraphMax;
    InitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionRemoveBackExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.RemoveBackgroundPoints;
    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionRemoveRFactorIntervalsExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.RemoveRFactorIntervals;
    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionRemoveSpecPosExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.RemoveCurvePositions;
    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionRmBackAutoExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
    SubtractAllBackground(True);
end;

procedure TFormMain.ActionRmBackSelectedExecute(Sender: TObject);
begin
    if (FitClientApp_.FitClient.GetBackgroundPoints = nil) or
       (FitClientApp_.FitClient.GetBackgroundPoints.PointsCount < 2) then
       // zdes' eto dopustimaya oshibka
    begin
         MessageDlg('Background points must be selected...',
             mtWarning,[mbOk], 0);
         Exit;
    end;

    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
    SubtractAllBackground(False);
end;

procedure TFormMain.ActionSaveAsTextExecute(Sender: TObject);
begin
    if PageControl1.ActivePage = TabSheetParameters then
    begin
        if SaveTableAsText(GridParameters) then
            ModifiedParameters := False;
    end
    else
    if PageControl1.ActivePage = TabSheetDatasheet then
    begin
        if SaveTableAsText(GridDatasheet) then
            ModifiedDatasheet := False;
    end;
end;

procedure TFormMain.ActionSelAreaExecute(Sender: TObject);
var SP: TNeutronPointsSet;
    NP: TNeutronPointsSet;
begin
    SP := FitClientApp_.FitClient.GetSelectedPoints;
    if (SP = nil) or (SP.PointsCount <> 2) then
    begin
        //  zdes' SP=nil - dopustimaya oshibka pol'zovatelya
        MessageDlg('Two limiting points must be selected...',
            mtWarning, [mbOK], 0);
        Exit;
    end;

    NP := FitClientApp_.FitClient.NeutronPointsSet;
        //FitViewer.GetPointsSet(ActiveNumber);
    SP.Sort;

    ShowHint(HintMain);
    FitClientApp_.FitClient.SelectArea(
        NP.IndexOfValueX(SP.PointXCoord[0]),
        NP.IndexOfValueX(SP.PointXCoord[1])
        );
    FitClientApp_.FitClient.SelectionMode := ModeSelNone;
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    InitXGraphMax := Chart.XGraphMax;
    InitXGraphMin := Chart.XGraphMin;
    InitYGraphMax := Chart.YGraphMax;
    InitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelAreaLimitsExecute(Sender: TObject);
begin
    if not SelAreaLimits.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelAreaLimits;
        ShowHint(HintFirstStart);
    end
    else
    begin
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
    end;
end;

procedure TFormMain.ActionSelBackAutoExecute(Sender: TObject);
begin
    //  perehodim v rezhim vvoda tochek fona
    if not Back.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelBackground;
        ShowHint(HintFirst);
    end;
    //??? el-t menyu d.b. zapreschen do okonchaniya rascheta;
    // proverit' vse analogichnye sluchai
    FitClientApp_.FitClient.FindBackPoints;
end;

procedure TFormMain.ActionSelBackVisExecute(Sender: TObject);
begin
    if not Back.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelBackground;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionSelCharacteristicPointsExecute(Sender: TObject);
begin
    if not SelCharacteristicPoints.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelCharacteristicPoints;
        ShowHint(HintFirstStart);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionSelCurveBoundsExecute(Sender: TObject);
var PS: TNeutronPointsSet;
begin
    if not SelCurveBounds.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        PS := FitViewer.GetActivePointsSet;
        if not (PS is TCurvePointsSet) then
        begin
            MessageDlg('This operation allowed only with pattern specimens...',
                mtWarning, [mbOk], 0);
            Exit;
        end;
        FitClientApp_.FitClient.SelectionMode := ModeSelGaussianBounds;
        ShowHint(HintFirstStart);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionSelCurveExecute(Sender: TObject);
var
    CurveTypeIterator: ICurveTypeIterator;
    CurveTypeSelector: ICurveTypeSelector;

    NamedPointsSetClasses: array[1..6] of TNamedPointsSetClass = (
        TGaussPointsSet, TLorentzPointsSet,
        TPseudoVoigtPointsSet, TAsymPseudoVoigtPointsSet,
        T2BranchesPseudoVoigtPointsSet, TUserPointsSet);
    i: Integer;
    NamedPointsSetClass: TNamedPointsSetClass;
begin
    CurveTypeIterator := TCurveTypesSingleton.CreateCurveTypeIterator;
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    for i := 1 to 6 do
    begin
        NamedPointsSetClass := NamedPointsSetClasses[i];
        if TMenuItem(Sender).Tag =
            CurveTypeIterator.GetCurveTypeTag(NamedPointsSetClass.GetCurveTypeId_) then
        begin
            if NamedPointsSetClass.GetConfigurablePointsSet.HasConfigurableParameters then
                if not NamedPointsSetClass.GetConfigurablePointsSet.ShowConfigurationDialog then
                begin
                    //  Tries to apply default values of parameters.
                    if NamedPointsSetClass.GetConfigurablePointsSet.HasDefaults then
                        NamedPointsSetClass.GetConfigurablePointsSet.SetDefaults
                    else
                    begin
                        //  Configuration failed.
                        MessageDlg('Warning', 'Parameters must be configured',
                            mtWarning, [mbOK], '');
                        Break;
                    end;
                end
                else
                begin
                    //  Save configuration data.
                end;

            //  Curve type can be selected only after successful configuration.
            FitClientApp_.FitClient.CurveTypeId := NamedPointsSetClass.GetCurveTypeId_;
            CurveTypeSelector.SelectCurveType(NamedPointsSetClass.GetCurveTypeId_);
            Break;
        end
    end;
end;

procedure TFormMain.ActionSelectAllExecute(Sender: TObject);
begin
    if ActiveControl is TNumericGrid then
        with ActiveControl as TNumericGrid do SelectAll;
end;

procedure TFormMain.ActionSelEntireProfExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.ReturnToTotalProfile;
    ShowHint(HintMain);
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    InitXGraphMax := Chart.XGraphMax;
    InitXGraphMin := Chart.XGraphMin;
    InitYGraphMax := Chart.YGraphMax;
    InitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelSpecPosAtEveryPointExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.AllPointsAsPeakPositions;
end;

procedure TFormMain.ActionSelSpecPosAutoExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.FindPeakPositions;
end;

procedure TFormMain.ActionSelRFactorIntervalsAutoExecute(Sender: TObject);
begin
    //  perehodim v rezhim vybora intervalov rascheta R-faktora
    if not RFactorIntervals.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelPeakBounds;
        ShowHint(HintFirst);
    end;
    //??? el-t menyu d.b. zapreschen do okonchaniya rascheta;
    // proverit' vse analogichnye sluchai
    FitClientApp_.FitClient.FindPeakBounds;
end;

procedure TFormMain.ActionSelRFactorIntervalsVisExecute(Sender: TObject);
begin
    if not RFactorIntervals.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelPeakBounds;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionSelSpecPosVisExecute(Sender: TObject);
begin
    if not PeakPos.Checked then
    begin
        ActiveNumber := FitViewer.GetActiveCurve;
        FitClientApp_.FitClient.SelectionMode := ModeSelPeakPos;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelNone;
end;

procedure TFormMain.ActionSetMaxRFactorExecute(Sender: TObject);
begin
    InputMaxRFactorDlg.Value := FitClientApp_.FitClient.MaxRFactor;
    if InputMaxRFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.MaxRFactor := InputMaxRFactorDlg.Value;
end;

procedure TFormMain.ActionSetPortionExecute(Sender: TObject);
begin
    InputBackFactorDlg.Value := FitClientApp_.FitClient.BackFactor;
    if InputBackFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.BackFactor := InputBackFactorDlg.Value;
end;

procedure TFormMain.ActionSmoothExecute(Sender: TObject);
begin
    //  sglazhivanie mozhno primenyat' posledovatel'no neskol'ko raz
    FitClientApp_.FitClient.SmoothProfile;
end;

procedure TFormMain.ActionStopFittingExecute(Sender: TObject);
begin
    FitClientApp_.FitClient.StopAsyncOper;
end;

procedure TFormMain.ActionViewMarkersExecute(Sender: TObject);
begin
    ViewMarkers.Checked := not ViewMarkers.Checked;
    FitViewer.SetViewMarkers(ViewMarkers.Checked);
end;

procedure TFormMain.ActionZoomInExecute(Sender: TObject);
begin
    Chart.ZoomIn;
end;

procedure TFormMain.ActionZoomOutExecute(Sender: TObject);
begin
    Chart.ZoomOut;
end;

procedure TFormMain.ButAddSelectedDataPointToPositionsClick(Sender: TObject);
var XValue, YValue: Double;
begin
    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
    //  nekorrektnyy vvod !!!
    with GridData do
    begin
        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;
    FitClientApp_.FitClient.AddPointToCurvePositions(XValue, YValue);
end;

procedure TFormMain.ButAddSelectedPointToIntervalsClick(Sender: TObject);
var XValue, YValue: Double;
begin
    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
    //  nekorrektnyy vvod !!!
    with GridData do
    begin
        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;
    FitClientApp_.FitClient.AddPointToRFactorIntervals(XValue, YValue);
end;

procedure TFormMain.ButAddSelectedDataPointClick(Sender: TObject);
var XValue, YValue: Double;
begin
    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
    //  nekorrektnyy vvod !!!
    with GridData do
    begin
        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;
    FitClientApp_.FitClient.AddPointToBackground(XValue, YValue);
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
    //AddDummyCurve;
    WriteSettings;
    Settings.Free;
    FitViewer.Free;
end;

procedure TFormMain.GridDataEditingDone(Sender: TObject);
var PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    i: LongInt;
    AllData: Boolean;
begin
    //  vvod vruchnuyu tochek fona poka nevozmozhen
    if Sender = GridBackground then Exit;

    try
        //  !!! pochemu-to vyzyvaetsya po tri raza,
        //  poetomu nuzhno ispol'zovat' flag !!!
        if not EditDone then
            with Sender as TNumericGrid do
            begin
                EditDone := True;
                if Col = 0 then
                begin
                    //  redaktiruetsya polozhenie
                    //  !!! esli nichego ne vvedeno, to i obnovlyat' ne nuzhno !!!
                    if SavedPos <> Cells[0, Row] then
                        Objects[0, Row] := TObject(1);
                end
                else
                begin
                    //  redaktiruetsya amplituda
                    if SavedAmp <> Cells[1, Row] then
                        Objects[1, Row] := TObject(1);
                end;
                //  proveryayutsya vse priznaki zapolneniya yacheek...
                AllData := True;
                for i := FixedCols to ColCount - 1 do
                    if LongInt(Objects[i, Row]) = 0 then
                    begin
                        AllData := False;
                        Break;
                    end;
                //  ...i tol'ko pri vseh zapoln. yacheykah vyzyvaetsya
                //  obnovlenie, chtoby dobavlenie novoy stroki
                //  proishodilo v nuzhnyy moment
                if AllData then
                begin
                    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
                    //  sluchai, kogda stroka byla pustoy !!!
                    PrevXValue := StrToFloatDef(SavedPos, 0);
                    NewXValue := StrToFloatDef(Cells[0, Row], 0);
                    PrevYValue := StrToFloatDef(SavedAmp, 0);
                    NewYValue := StrToFloatDef(Cells[1, Row], 0);

                    if Sender = GridData then
                        FitClientApp_.FitClient.ReplacePointInData(
                            PrevXValue, PrevYValue, NewXValue, NewYValue
                            )
                    else
                    if Sender = GridBackground then
                        FitClientApp_.FitClient.ReplacePointInBackground(
                            PrevXValue, PrevYValue, NewXValue, NewYValue
                            );
                end;
            end;
    except
{$IFDEF _WINDOWS}
        on E: EUserException do
        //  !!! takie isklyucheniya ne popadut v log !!!
        begin
            HandleEditHint := True; //  vklyuchaetsya taymer vyvoda soobscheniya
            SenderEditHint := TNumericGrid(Sender);
            HintMessage := E.Message;
        end;
{$ELSE}
        raise;
{$ENDIF}
    end;
end;

procedure TFormMain.DoEditHint;
{$ifdef windows}
var //BE: BalloonException;
    Handle: HWND;
    CellRect: TRect;
    EditBalloon: TEdit;
{$endif}
begin
{$ifdef windows}
    if SenderEditHint = GridBackground then
        EditBalloon := EditBalloonGridBackground
    else
    if SenderEditHint = GridData then
        EditBalloon := EditBalloonGridData;
    //BE := BalloonException.Create(E.Message);
    //if TNumericGrid(Sender).EditorMode then
    //    BE.Handle
    //    Handle := TNumericGrid(Sender).Editor.Handle
    //else
    //begin
        CellRect := SenderEditHint.CellRect(
            SenderEditHint.Col, SenderEditHint.Row);
        EditBalloon.Left := CellRect.Left + SenderEditHint.Left;
        EditBalloon.Top := CellRect.Top + SenderEditHint.Top;
        //BE.Handle
            Handle := EditBalloon.Handle;
    //end;
    //raise BE;
    //  !!! bez aktivatsii okna ne rabotaet !!!
    ActiveControl := EditBalloon;
    //  !!! pri isp. ShowBalloon nel'zya dopuskat' vyhod
    //  isklyucheniya za granitsy obrabotchika sobytiya !!!
    ShowBalloon(Handle, WideString(HintMessage), WideString(''));
{$else}
    MessageDlg(HintMessage, mtError, [mbOk], 0);
{$endif}
end;

{$hints off}
procedure TFormMain.GridDataSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
    SavedPos := GridData.Cells[0, GridData.Row];
    SavedAmp := GridData.Cells[1, GridData.Row];
    EditDone := False;
end;
{$hints on}

procedure TFormMain.FormCreate(Sender: TObject);
begin
    Application.OnException := OnException;
    Caption := ApplicationProperties1.Title;

    FitViewer := TFitViewer.Create(nil);
    FitViewer.Form := Self;
    FitViewer.SetFitClient(FitClientApp_.FitClient);
    FitViewer.SetViewMarkers(ViewMarkers.Checked);
    FitViewer.Clear(Self);

    ActiveNumber := -1;

    FitClientApp_.FitClient.OnAsyncOperationFinished := AsyncOperationFinished;

    ShowHint(HintMain);
    ModifiedParameters := False;
    ModifiedDatasheet := False;

    //PanelLeft.Color := clWindow;
    //PanelChart.Color := clWindow;
    //PanelRight.Color := clWindow;
    //PageControl1.Color := clWindow;
    CheckListBoxLegend.Color := clBtnFace;
    //  pochemu-to pri pervonachal'nom otkrytii
    //  formy v IDE sbrasyvaetsya v False
    Chart.ParentColor := True;
    //  nel'zya peredavat' v kachestve tsveta indeks v palitre (tipa clBtnFace),
    //  potomu chto on nepravil'no preobrazuetsya v tsvet liniy v DrawReticule;
    //  libo nuzhno delat' preobrazovanie v RGB-tsvet
    Chart.AxisColor := clGray;   //clBtnFace;    //$00b99d7f;

    //  chtoby sdelat' nevidimymi, sohranyaya Visible = True
    EditBalloonGridBackground.Width := 0;
    EditBalloonGridBackground.Height := 0;
    EditBalloonGridData.Width := 0;
    EditBalloonGridData.Height := 0;
    EditBalloonGridIntervals.Width := 0;
    EditBalloonGridIntervals.Height := 0;
    EditBalloonChart.Width := 0;
    EditBalloonChart.Height := 0;
    EditBalloonGridSpecPositions.Width := 0;
    EditBalloonGridSpecPositions.Height := 0;
    EditBalloonGridParameters.Width := 0;
    EditBalloonGridParameters.Height := 0;
    EditBalloonGridDatasheet.Width := 0;
    EditBalloonGridDatasheet.Height := 0;

    //  u vertikal'nyh ScrollBar'ov a rantayme
    //  koordinaty ustanavlivayutsya nepravil'no
    ScrollBarY.Top := 7;
    ScrollBarY.Left := PanelChart.ClientWidth - 24;
    ScrollBarY.Width := 17;
    ScrollBarY.Height := PanelChart.ClientHeight - 24 - 7;
    //  chtoby uvesti pervonachal'nyy fokus vvoda s polosy prokrutki
    ActiveControl := CheckListBoxLegend;

    //??? eto zdes' ne rabotaet
    //Screen.Cursors[crCursorDrag] := LoadCursor(HInstance, 'CURSORDRAG');
    //Screen.Cursors[crCursorSelect] := LoadCursor(HInstance, 'CURSORSELECT');

    //Chart.Cursor := crCross;//crCursorDrag;
    //Windows.SetCursor(crCursorDrag);
    //Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
    Settings := Settings_v1.Create(nil);
    ReadSettings;
{$IFDEF _WINDOWS}
    ReadUserCurves;
    CreateUserCurveMenus;
{$ENDIF}
end;

procedure TFormMain.ModelClick(Sender: TObject);
begin

end;

procedure TFormMain.PanelTopClick(Sender: TObject);
begin

end;

procedure TFormMain.PeakPosClick(Sender: TObject);
begin

end;

procedure TFormMain.SubtractAllBackground(Auto: Boolean);
begin
    ShowHint(HintMain);
    Back.Checked := False;
    FitClientApp_.FitClient.SubtractAllBackground(Auto);
end;

procedure TFormMain.ScrollBarXChange(Sender: TObject);
var D, DeltaX: Double;
begin
    if Chart.SeriesCount <> 0 then
    begin
        (*  vraschenie v druguyu storonu
        DeltaX := (InitXGraphMax - InitXGraphMin) -
                  (Chart.XGraphMax - Chart.XGraphMin);
        D := Chart.XGraphMax - Chart.XGraphMin;
        Chart.XGraphMax := InitXGraphMax -
            (ScrollBarX.Position - ScrollBarX.Min) * DeltaX /
            (ScrollBarX.Max - ScrollBarX.Min);
        Chart.XGraphMin := Chart.XGraphMax - D;
        *)

        DeltaX := (InitXGraphMax - InitXGraphMin) -
                  (Chart.XGraphMax - Chart.XGraphMin);
        D := Chart.XGraphMax - Chart.XGraphMin;
        Chart.XGraphMax := InitXGraphMax -
            (ScrollBarX.Max - ScrollBarX.Position) * DeltaX /
            (ScrollBarX.Max - ScrollBarX.Min);
        Chart.XGraphMin := Chart.XGraphMax - D;
        Chart.Invalidate;
    end;
end;

procedure TFormMain.ScrollBarYChange(Sender: TObject);
var D, DeltaY: Double;
begin
    if Chart.SeriesCount <> 0 then
    begin
        (*  vraschenie v druguyu storonu
        DeltaY := (InitYGraphMax - InitYGraphMin) -
                  (Chart.YGraphMax - Chart.YGraphMin);
        D := Chart.YGraphMax - Chart.YGraphMin;
        Chart.YGraphMin := InitYGraphMin +
            (ScrollBarY.Position - ScrollBarY.Min) * DeltaY /
            (ScrollBarY.Max - ScrollBarY.Min);
        Chart.YGraphMax := Chart.YGraphMin + D;
        Chart.Invalidate;
        *)

        DeltaY := (InitYGraphMax - InitYGraphMin) -
                  (Chart.YGraphMax - Chart.YGraphMin);
        D := Chart.YGraphMax - Chart.YGraphMin;
        Chart.YGraphMin := InitYGraphMin +
            (ScrollBarY.Max - ScrollBarY.Position) * DeltaY /
            (ScrollBarY.Max - ScrollBarY.Min);
        Chart.YGraphMax := Chart.YGraphMin + D;
        Chart.Invalidate;
    end;
end;

procedure TFormMain.TabSheetBackgroundResize(Sender: TObject);
begin
    //  !!! esli pomestit' ust. razmera syuda, to zavisaet !!!!
    //GridBackground.Top := 8;
    //GridBackground.Left := 8;
    //GridBackground.Height := TabSheetBackground.ClientHeight - 16;
end;

procedure TFormMain.TabSheetBackgroundShow(Sender: TObject);
begin
{$ifdef windows}
    LockWindowUpdate(Handle);       //  zdorovo pomogaet umen'sheniyu
                                    //  mertsaniya pod Windows
{$endif}
    with GridBackground do
    begin
        Top := 8; Left := 4;
        Height := PanelBackground.ClientHeight - 16;
        Width := PanelBackground.ClientWidth - 12;
    end;
{$ifdef windows}
    LockWindowUpdate(0);
{$endif}
end;

procedure TFormMain.OnChartClick;
var XValue, YValue: Double;
    NS: TPointsSet;
{$ifdef windows}
//    BE: BalloonException;
    Handle: HWND;
{$endif}
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitViewer));
    try
        //  !!! esli m-u dvumya klikami ne bylo dvizheniya myshi i
        //  ChartDrawReticule ne vyzyvalas', to CurSerieIndex i ValueIndex
        //  budut imet' nepravil'nye znacheniya, poetomu ispol'zuetsya flag !!!
        //  mozhno bylo by perepisat' TTAChart.Click,
        //  odnako eto mozhet vyzvat' problemy so sleduyuschimi
        //  versiyami biblioteki, v kot. mozhet izmenit'sya
        //  TControl.Click; prostym perekrytiem metoda
        //  dobit'sya zhelaemogo effekta nel'zya; poetomu
        //  sdelana dopolnitel'naya prostaya proverka
        if DrawReticule and
            (DownX = UpX) and (DownY = UpY) and (ActiveNumber <> -1) then
        begin
            DrawReticule := False;
            //  seriya, na kotoroy byl sdelan klik sravnivaetsya s aktivnoy;
            //  dopuskaetsya klik tol'ko na aktivnoy serii, tochki kotoroy
            //  dobavlyayutsya v vybrannuyu seriyu, ili na vybrannoy serii, iz
            //  kotoroy pri etom tochka udalyaetsya
            if (CurSerieIndex = ActiveNumber) or
               (FitViewer.GetPointsSet(CurSerieIndex) =
                FitClientApp_.FitClient.GetCurrentPointsSet) then
            begin
                case FitClientApp_.FitClient.SelectionMode of
                    ModeSelNone: Exit;

                    ModeSelAreaLimits: begin
                        // vybor tochek, ogranichivayuschih oblast'
                        NS := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(NS));

                        case NS.PointsCount of
                            0: ShowHint(HintSecondFinish);
                            1: ShowHint(HintSelectArea);
                            2: Exit;    //  bol'she tochek ne dobavlyaem
                        end;
                    end;

                    ModeSelCharacteristicPoints: begin
                        // vybor tochek harakterizuyuschih pik
                        NS := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(NS));

                        case NS.PointsCount of
                            0: ShowHint(HintSecondPeak);
                            1: ShowHint(HintThirdFinish);
                            2: ShowHint(HintMovePeak);
                            3: Exit;
                        end;
                    end;

                    ModeSelGaussianBounds: begin
                        // vybor tochek, ogranichivayuschih gaussian
                        NS := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(NS));

                        case NS.PointsCount of
                            0: ShowHint(HintSecondFinish);
                            1: ShowHint(HintMovePeak);
                            2: Exit;
                        end;
                    end;

                    ModeSelBackground: begin
                        // vybor tochek fona
                        NS := FitClientApp_.FitClient.GetBackgroundPoints;
                        Assert(Assigned(NS));

                        if NS.PointsCount > 0 then ShowHint(HintNextBackPoint)
                        else ShowHint(HintFirst);
                    end;

                    ModeSelPeakPos: begin
                        // vybor tochek nachal'nogo polozheniya gaussianov
                        NS := FitClientApp_.FitClient.GetCurvePositions;
                        Assert(Assigned(NS));

                        if NS.PointsCount > 0 then ShowHint(HintNextPoint)
                        else ShowHint(HintFirst);
                    end;

                    ModeSelPeakBounds: begin
                        //  vybor granits pikov
                        NS := FitClientApp_.FitClient.GetRFactorIntervals;
                        Assert(Assigned(NS));

                        if Odd(NS.PointsCount) then ShowHint(HintNextPointOdd)
                        else ShowHint(HintNextPointEven);
                    end;
                end;

                NS := FitViewer.GetPointsSet(CurSerieIndex);

                XValue := NS.PointXCoord[ValueIndex];
                YValue := NS.PointYCoord[ValueIndex];
                FitClientApp_.FitClient.AddPointToActive(XValue, YValue);
            end;
        end;
    except
{$ifdef windows}
        on E: EUserException do
        //  !!! takie isklyucheniya ne popadut v log !!!
        begin
            EditBalloonChart.Left := UpX;
            EditBalloonChart.Top := UpY;
            //BE := BalloonException.Create(E.Message);
            //BE.Handle
                Handle := EditBalloonChart.Handle;
            //  !!! pri isp. ShowBalloon nel'zya dopuskat' vyhod
            //  isklyucheniya za granitsy obrabotchika sobytiya !!!
            ShowBalloon(Handle, WideString(E.Message), WideString(''));
            //raise BE;
        end;
{$else}
        raise;
{$endif}
    end;
end;

{$hints off}
procedure TFormMain.ChartDrawReticule(Sender: TComponent; IndexSerie, Index,
    Xi, Yi: Integer; Xg, Yg: Double);
begin
    CurSerieIndex := IndexSerie;
    ValueIndex := Index;
    DrawReticule := True;
    LabelAngle.Caption := Format('%6.2f', [Xg]);
    LabelIntensity.Caption := Format('%6.2f', [Yg]);
end;

procedure TFormMain.ChartMouseDown(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    //  Screen.Cursor := crCursorDrag;
    //  eto budet rabotat' tol'ko so standartnymi kursorami Windows
    //Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
    DownX := X; DownY := Y;
end;
{$hints on}

procedure TFormMain.UpdateBarsPos;
var DeltaX, DeltaY: Double;
begin
    Application.ProcessMessages;
    if Chart.SeriesCount <> 0 then
    begin
        // !!! ne budet pravil'no rabotat' pri ispol'zovanii MirrorX !!!
        DeltaX := (InitXGraphMax - InitXGraphMin) -
            (Chart.XGraphMax - Chart.XGraphMin);
        // DeltaX m.b. = 0
        if DeltaX <> 0 then
        begin
            //  ustanavlivaetsya priblizhennoe znachenie polozheniya ScrollBar'a
            (*  dlya prokrutki v druguyu storonu
            ScrollBarX.Position := ScrollBarX.Max - Round(
                (Chart.XGraphMin - InitXGraphMin) *
                (ScrollBarX.Max - ScrollBarX.Min) / DeltaX);
            *)
            ScrollBarX.Position := ScrollBarX.Min + Round(
                (Chart.XGraphMin - InitXGraphMin) *
                (ScrollBarX.Max - ScrollBarX.Min) / DeltaX);
        end;

        DeltaY := (InitYGraphMax - InitYGraphMin) -
            (Chart.YGraphMax - Chart.YGraphMin);
        // DeltaY m.b. = 0
        if DeltaY <> 0 then
        begin
            //  ustanavlivaetsya priblizhennoe znachenie polozheniya SrollBar'a
            (*  dlya prokrutki v druguyu storonu
            ScrollBarY.Position := ScrollBarY.Min + Round(
                (Chart.YGraphMin - InitYGraphMin) *
                (ScrollBarY.Max - ScrollBarY.Min) / DeltaY);
            *)
            //  polozhenie dvizhka bara na min. sootvetstvuet
            //  polozheniyu okna na maks. grafika
            ScrollBarY.Position := ScrollBarY.Max - Round(
                (Chart.YGraphMin - InitYGraphMin) *
                (ScrollBarY.Max - ScrollBarY.Min) / DeltaY);
        end;
    end;
end;

{$hints off}
procedure TFormMain.ChartMouseUp(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    UpdateBarsPos;
    UpX := X; UpY := Y;
    OnChartClick;
end;
{$hints on}

procedure TFormMain.TabSheetDatasheetShow(Sender: TObject);
begin
{$IFDEF _WINDOWS}
    LockWindowUpdate(Handle);       //  zdorovo pomogaet umen'sheniyu
                                    //  mertsaniya pod Windows
{$ENDIF}
    with GridDatasheet do
    begin
        Top := 8; Left := 4;
        Height := PanelDatasheet.ClientHeight - 16;
        Width := PanelDatasheet.ClientWidth - 12;
    end;
{$IFDEF _WINDOWS}
    LockWindowUpdate(0);
{$ENDIF}
end;

procedure TFormMain.TabSheetIntervalsShow(Sender: TObject);
begin
{$ifdef windows}
    LockWindowUpdate(Handle);       //  zdorovo pomogaet umen'sheniyu
                                    //  mertsaniya pod Windows
{$endif}
    with GridIntervals do
    begin
        Top := 8; Left := 4;
        Height := PanelIntervals.ClientHeight - 16;
        Width := PanelIntervals.ClientWidth - 12;
    end;
{$ifdef windows}
    LockWindowUpdate(0);
{$endif}
end;

procedure TFormMain.TabSheetParametersShow(Sender: TObject);
begin
{$ifdef windows}
    LockWindowUpdate(Handle);       //  zdorovo pomogaet umen'sheniyu
                                    //  mertsaniya pod Windows
{$endif}
    with GridParameters do
    begin
        Top := 8; Left := 4;
        Height := PanelParameters.ClientHeight - 16;
        Width := PanelParameters.ClientWidth - 12;
    end;
{$ifdef windows}
    LockWindowUpdate(0);
{$endif}
end;

procedure TFormMain.TabSheetSpecPositionsShow(Sender: TObject);
begin
{$ifdef windows}
    LockWindowUpdate(Handle);       //  zdorovo pomogaet umen'sheniyu
                                    //  mertsaniya pod Windows
{$endif}
    with GridSpecPositions do
    begin
        Top := 8; Left := 4;
        Height := PanelSpecPositions.ClientHeight - 16;
        Width := PanelSpecPositions.ClientWidth - 12;
    end;
{$ifdef windows}
    LockWindowUpdate(0);
{$endif}
end;

procedure TFormMain.TAChart1Zoom(Sender: TComponent);
begin
    UpdateBarsPos;
end;

procedure TFormMain.TimerAsyncTimer(Sender: TObject);
begin
{$IFDEF FITPRO}
    if not FitClientApp_.FitClient.AsyncOper then
    begin
        //  pervym delom nuzhno ostanovit' taimer,
        //  choby pri vozniknovenii islyucheniya nizhe
        //  ne bylo povtornogo vhoda v etot kod
        TimerAsync.Enabled := False;
        FitClientApp_.FitClient.Done;
    end;
{$ENDIF}
end;

procedure TFormMain.TimerBalloonHideTimer(Sender: TObject);
begin
    //  taymer avtomaticheskogo vyklyucheniya podskazki
    //  interval taymera d.b. garantirovanno men'she
    //  vremeni gasheniya balloon'a windows, inache
    //  budet ust. fokus vvoda nevpopad - ne yasno,
    //  kak eto garantirovat'
    (*
    if SenderEditHint = GridBackground then
        Edit_HideBalloonTip(EditBalloonGridBackground.Handle)
    else
    if SenderEditHint = GridData then
        Edit_HideBalloonTip(EditBalloonGridData.Handle);
    //  vzvraschaetsya fokus vvoda
    ActiveControl := SenderEditHint;
    *)
    TimerBalloonHide.Enabled := False;
end;

procedure TFormMain.TimerBalloonShowTimer(Sender: TObject);
begin
    //  taymer vyvoda soobscheniya
    if HandleEditHint then
    begin
        HandleEditHint := False;
        DoEditHint;
    end;
    TimerBalloonShow.Enabled := False;
    //TimerBalloonHide.Enabled := True;
end;

{$hints off}
procedure TFormMain.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var i: LongInt;
begin
    //with Chart1 do
    // for i := 0 to SeriesCount - 1 do
    //  if Series[i].GetCursorValueIndex <> -1 then
    //   begin Screen.Cursor := crCross; Quit end;
    Screen.Cursor := crArrow;
end;

procedure TFormMain.CheckListBoxLegendKeyPress(Sender: TObject; var Key: Char);
begin
    CheckListBoxChanged;
end;
{$hints on}

procedure TFormMain.CheckListBoxChanged;
var i: LongInt;
    TS: TTASerie;
begin
    with CheckListBoxLegend do
        for i := 0 to Items.Count - 1 do
        begin
            if Assigned(Items.Objects[i]) then
            begin
                if Items.Objects[i] is TTASerie then
                begin
                    TS := TTASerie(Items.Objects[i]);

                    if Checked[i] then
                    begin
                        TS.ShowLines := TS.InitShowLines;
                        TS.ShowPoints := TS.InitShowPoints;
                    end
                    else
                    begin
                        TS.ShowLines := False;
                        TS.ShowPoints := False;
                    end;
                end;
            end;
        end;
end;

procedure TFormMain.CheckListBoxLegendClick(Sender: TObject);
begin
    CheckListBoxChanged;
end;

{$warnings off}
procedure TFormMain.SinThetaLambdaClick(Sender: TObject);
var SaveDecimalSeparator: Char;
begin
    if FitClientApp_.FitClient.GetWaveLength = 0 then
    begin
        if InputWavelengthDlg.ShowModal = mrOk then
        begin
            SaveDecimalSeparator := DecimalSeparator;
            DecimalSeparator := '.';
            Screen.Cursor := crHourGlass;
            FitClientApp_.FitClient.SetWaveLength(StrToFloat(
                InputWavelengthDlg.WavelengthValueEdit.Text));
            Screen.Cursor := crDefault;
            DecimalSeparator := SaveDecimalSeparator;
            FitViewer.XCoordMode := XCM_SINTL;
            if Assigned(SpecimenList) then
            begin
                SpecimenList.ViewMode := XCM_SINTL;
                SpecimenList.GridAssign(GridParameters);
            end;
            Theta.Checked := False;
            Theta2.Checked := False;
            N2Theta.Checked := False;
            N2Theta2.Checked := False;
            SinThetaLambda.Checked := True;
            SinThetaLambda2.Checked := True;
        end;
    end{if FitClientApp_.FitClient.GetWaveLength = 0 then...}
    else
    begin
        FitViewer.XCoordMode := XCM_SINTL;
        if Assigned(SpecimenList) then
        begin
            SpecimenList.ViewMode := XCM_SINTL;
            SpecimenList.GridAssign(GridParameters);
        end;
        Theta.Checked := False;
        Theta2.Checked := False;
        N2Theta.Checked := False;
        N2Theta2.Checked := False;
        SinThetaLambda.Checked := True;
        SinThetaLambda2.Checked := True;
    end;
end;
{$warnings on}

procedure TFormMain.ThetaClick(Sender: TObject);
begin
    FitViewer.XCoordMode := XCM_T;
    if Assigned(SpecimenList) then
    begin
        SpecimenList.ViewMode := XCM_T;
        SpecimenList.GridAssign(GridParameters);
    end;
    Theta.Checked := True;
    Theta2.Checked := True;
    N2Theta.Checked := False;
    N2Theta2.Checked := False;
    SinThetaLambda.Checked := False;
    SinThetaLambda2.Checked := False;
end;

procedure TFormMain.N2ThetaClick(Sender: TObject);
begin
    FitViewer.XCoordMode := XCM_2T;
    if Assigned(SpecimenList) then
    begin
        SpecimenList.ViewMode := XCM_2T;
        SpecimenList.GridAssign(GridParameters);
    end;
    Theta.Checked := False;
    Theta2.Checked := False;
    N2Theta.Checked := True;
    N2Theta2.Checked := True;
    SinThetaLambda.Checked := False;
    SinThetaLambda2.Checked := False;
end;

function TFormMain.SaveTableAsText(GridData: TNumericGrid): Boolean;
var i, j, DlgResult: LongInt;
    F: TextFile;
    St: string;
    FileName: string;
label DoItAgain;
begin
    Result := True;

DoItAgain:
    if SaveDialog1.Execute then
    begin
        FileName := SaveDialog1.FileName;
        if FileName <> '' then
        begin
            SaveDialog1.InitialDir := ExtractFilePath(FileName);
            if ExtractFileExt(FileName) = '' then
                FileName := FileName + '.txt';

            if FileExists(FileName) then
            begin
                DlgResult := MessageDlg('File ' +
                    ExtractFileName(FileName) + ' exists.' +
                    #13#10 + 'Overwrite?', mtConfirmation,
                    mbYesNoCancel, 0
                    );
                if DlgResult = mrCancel then
                begin
                    Result := False;
                    Exit;
                end;
                if DlgResult = mrNo then goto DoItAgain;
            end;

            AssignFile(F, FileName);
            Rewrite(F);

            try
                with GridData do
                begin
                    //  zagolovki pishutsya pryamo iz tablitsy
                    for i := 0 to RowCount - 1 do
                    begin
                        St := '';
                        for j := 0 to ColCount - 1 do
                            if j <> ColCount - 1 then
                                St := St + Cells[j, i] + #9
                            else St := St + Cells[j, i];
                        WriteLn(F, St);
                    end;{for i := FixedRows to RowCount - 1 do...}
                end;
            finally
                CloseFile(F);
            end;
        end
        else
        begin
            DlgResult := MessageDlg(
                'File name must not be empty.' + #13#10 +
                'Select file again?', mtError, mbYesNo, 0);
            if DlgResult = mrYes then goto DoItAgain
            else Result := False;
        end;
    end else Result := False;
end;

//  sohranenie tabl. parametrov krivyh kak XML fayla
procedure TFormMain.SaveTable;
begin
end;

//  zagruzka parametrov krivyh iz XML fayla
procedure TFormMain.LoadTable;
begin
end;

procedure TFormMain.AsyncOperationFinished(Sender: TObject);
begin
    //!!! nel'zya vyzyvat' dialogi vnutri metoda Synchronize bez proverki,
    //potomu chto v dannoy biblioteke eto vyzyvaet tsikl vyborki soobscheniy,
    //kotoryy snova vhodit v Synchronize !!!
    //MessageDlg('Gaussians calculation done...', mtInformation, [mbOk], 0);
    ShowTime;
end;

procedure TFormMain.ShowTime;
var TimeStr: string;
begin
    TimeStr := FitClientApp_.FitClient.GetCalcTimeStr;
    StatusBar.Panels[0].Text := 'Elapsed time: ' + TimeStr;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Result: LongInt;
begin
    CanClose := True;
    if ModifiedParameters then
    begin
        Result := MessageDlg(
            'Model parameters has been modified.' + #13#10 + 'Save?',
            mtConfirmation, mbYesNoCancel, 0);

        if Result = mrYes then
        begin
            if SaveTableAsText(GridParameters) then ModifiedParameters := False
            else
            begin
                PageControl1.ActivePage := TabSheetParameters;
                CanClose := False;
                Exit;
            end;
        end;

        if Result = mrCancel then
        begin
            PageControl1.ActivePage := TabSheetParameters;
            CanClose := False;
            Exit;
        end;
    end;

    if ModifiedDatasheet then
    begin
        Result := MessageDlg(
            'Datasheet has been modified.' + #13#10 + 'Save?',
            mtConfirmation, mbYesNoCancel, 0);
        if Result = mrYes then
        begin
            if SaveTableAsText(GridDatasheet) then ModifiedDatasheet := False
            else
            begin
                PageControl1.ActivePage := TabSheetDatasheet;
                CanClose := False;
                Exit;
            end;
        end;

        if Result = mrCancel then
        begin
            PageControl1.ActivePage := TabSheetDatasheet;
            CanClose := False;
        end;
    end;
end;

procedure TFormMain.SetWavelengthClick(Sender: TObject);
begin
    if InputWavelengthDlg.ShowModal = mrOk then
    begin
        FitClientApp_.FitClient.SetWaveLength(InputWavelengthDlg.Value);
        FitViewer.XCoordMode := XCM_SINTL;
        if Assigned(SpecimenList) then
        begin
            SpecimenList.ViewMode := XCM_SINTL;
            SpecimenList.GridAssign(GridParameters);
        end;
        SinThetaLambda.Checked := True;
        SinThetaLambda2.Checked := True;
    end;
end;

procedure TFormMain.SetSelectionMode(ASelectionMode: TSelMode);
//  dlya predotvrascheniya mertsaniya menyu
var SelBackVisCaption, SelSpecPosVisCaption,
    SelRFactorIntervalsVisCaption: string;
    NS: TPointsSet;
begin
    SelArea.Tag := SelArea.Tag and $FFFFFFFE;
    SelAreaLimits.Tag := SelAreaLimits.Tag and $FFFFFFFD;
    SelCharacteristicPoints.Tag := SelCharacteristicPoints.Tag and $FFFFFFFD;
    SelCurveBounds.Tag := SelCurveBounds.Tag and $FFFFFFFD;
    Back.Tag := Back.Tag and $FFFFFFFD;
    RFactorIntervals.Tag := RFactorIntervals.Tag and $FFFFFFFD;
    PeakPos.Tag := PeakPos.Tag and $FFFFFFFD;
    //  initsializatsiya vosstanavlivaet ishodnyy tekst,
    //  kogda sootvetstvuyuschie flagi ne ustanovleny
    SelBackVisCaption := StartVisualSel;
    SelSpecPosVisCaption := StartVisPosSel;
    SelRFactorIntervalsVisCaption := StartVisualSel;

    RemoveBack.Tag := RemoveBack.Tag and $FFFFFFFE;
    RemoveRFactorIntervals.Tag := RemoveRFactorIntervals.Tag and $FFFFFFFE;
    RemoveSpecPos.Tag := RemoveSpecPos.Tag and $FFFFFFFE;

    case ASelectionMode of
        ModeSelNone:
            begin
                if FitClientApp_.FitClient.SelectedAreaMode then
                    SelEntireProf.Tag := SelEntireProf.Tag or 1;
                ShowHint(HintMain);
            end;
        ModeSelAreaLimits:
            begin
                SelAreaLimits.Tag := SelAreaLimits.Tag or 2;
                NS := FitClientApp_.FitClient.GetSelectedPoints;
                Assert(Assigned(NS));
                if NS.PointsCount = 2 then
                    SelArea.Tag := SelArea.Tag or 1;
            end;
        ModeSelCharacteristicPoints:
            SelCharacteristicPoints.Tag := SelCharacteristicPoints.Tag or 2;
        ModeSelGaussianBounds: SelCurveBounds.Tag := SelCurveBounds.Tag or 2;
        ModeSelBackground:
            begin
                RemoveBack.Tag := RemoveBack.Tag or 1;
                Back.Tag := Back.Tag or 2;
                NS := FitClientApp_.FitClient.GetBackgroundPoints;
                Assert(Assigned(NS));
                if NS.PointsCount > 0 then
                    ActionRmBackSelected.Tag := ActionRmBackSelected.Tag or 1
                else
                    ActionRmBackSelected.Tag := ActionRmBackSelected.Tag and $FFFFFFFE;
                SelBackVisCaption := StopVisualSel;
            end;

        ModeSelPeakPos:
            begin
                RemoveSpecPos.Tag := RemoveSpecPos.Tag or 1;
                PeakPos.Tag := PeakPos.Tag or 2;
                SelSpecPosVisCaption := StopVisPosSel;
            end;

        ModeSelPeakBounds:
            begin
                RemoveRFactorIntervals.Tag := RemoveRFactorIntervals.Tag or 1;
                RFactorIntervals.Tag := RFactorIntervals.Tag or 2;
                SelRFactorIntervalsVisCaption := StopVisualSel;
            end;
    end;
    SelBackVis.Caption := SelBackVisCaption;
    SelSpecPosVis.Caption := SelSpecPosVisCaption;
    SelRFactorIntervalsVis.Caption := SelRFactorIntervalsVisCaption;
end;

procedure TFormMain.SetAsyncState(State: TAsyncState);
begin
    case State of
        AsyncWorks:
        begin
            //  Operation
            ActionDoAllAuto.Tag := ActionDoAllAuto.Tag and $FFFFFFFE;
            Smooth.Tag := Smooth.Tag and $FFFFFFFE;
            RmBack.Tag := RmBack.Tag and $FFFFFFFE;
            ActionStopFitting.Tag := ActionStopFitting.Tag or 1;

            //  Dataset
            SelAreaLimits.Tag := SelAreaLimits.Tag and $FFFFFFFE;
            SelArea.Tag := SelArea.Tag and $FFFFFFFE;
            SelEntireProf.Tag := SelEntireProf.Tag and $FFFFFFFE;
            PeakPos.Tag := PeakPos.Tag and $FFFFFFFE;
            Back.Tag := Back.Tag and $FFFFFFFE;
            RFactorIntervals.Tag := RFactorIntervals.Tag and $FFFFFFFE;
            SelCharacteristicPoints.Tag := SelCharacteristicPoints.Tag and $FFFFFFFE;
            SelCurveBounds.Tag := SelCurveBounds.Tag and $FFFFFFFE;
        end;

        AsyncStart:
        begin
            //  Operation
            ActionStopFitting.Tag := ActionStopFitting.Tag and $FFFFFFFE;

            SetSelectionMode(FitClientApp_.FitClient.SelectionMode);
        end;

        AsyncDone:
        begin
            //  Operation
            ActionStopFitting.Tag := ActionStopFitting.Tag and $FFFFFFFE;

            SetSelectionMode(FitClientApp_.FitClient.SelectionMode);
        end;
    end;
end;

procedure TFormMain.SetResState(State: TResState);
begin
    case State of
        GridInvisible:      //  okno tablitsy ne vidno
        begin
            ActionCopy.Enabled := False;
            ActionDelete.Enabled := False;
            ActionSelectAll.Enabled := False;
        end;

        GridSelEmpty:       //  okno vidno, no oblast' vydeleniya pusta
        begin
            ActionCopy.Enabled := False;
            ActionDelete.Enabled := False;
            ActionSelectAll.Enabled := True;
        end;

        GridSelNonEmpty:    //  vydeleno neskol'ko strok tablitsy
        begin
            ActionCopy.Enabled := True;
            ActionDelete.Enabled := True;
            ActionSelectAll.Enabled := True;
        end;

        GridSelAll:         //  vydelena vsya tablitsa
        begin
            ActionCopy.Enabled := True;
            ActionDelete.Enabled := True;
            ActionSelectAll.Enabled := False;
        end;
    end;
end;

procedure TFormMain.SetOpenState(State: TOpenState);
var
    FitServerState: TFitServerState;
begin
    //  File
    ActionReload.Tag := ActionReload.Tag and $FFFFFFFE;
    ActionSaveAsText.Tag := ActionSaveAsText.Tag and $FFFFFFFE;

    //  Operation
    ActionDoAllAuto.Tag := ActionDoAllAuto.Tag and $FFFFFFFE;
    ActionSmooth.Tag := ActionSmooth.Tag and $FFFFFFFE;
    RmBack.Tag := RmBack.Tag and $FFFFFFFE;
    ActionRmBackSelected.Tag := ActionRmBackSelected.Tag and $FFFFFFFE;
    ActionFitMinNumberOfSpec.Tag := ActionFitMinNumberOfSpec.Tag and $FFFFFFFE;
    ActionFitMinDifference.Tag := ActionFitMinDifference.Tag and $FFFFFFFE;
    ActionStopFitting.Tag := ActionStopFitting.Tag and $FFFFFFFE;

    //  Dataset
    ActionSelAreaLimits.Tag := ActionSelAreaLimits.Tag and $FFFFFFFE;
    ActionSelArea.Tag := ActionSelArea.Tag and $FFFFFFFE;
    ActionSelEntireProf.Tag := ActionSelEntireProf.Tag and $FFFFFFFE;
    Back.Tag := Back.Tag and $FFFFFFFE;
    RFactorIntervals.Tag := RFactorIntervals.Tag and $FFFFFFFE;
    ActionSelCharacteristicPoints.Tag :=
        ActionSelCharacteristicPoints.Tag and $FFFFFFFE;
    ActionSelCurveBounds.Tag := ActionSelCurveBounds.Tag and $FFFFFFFE;

    case State of
        OpenSuccess:
        begin
            //  !!! rabota s Tag sdelana dlya zaschity ot mertsaniya !!!
            ActionReload.Tag := ActionReload.Tag or 1;

            ActionDoAllAuto.Tag := ActionDoAllAuto.Tag or 1;
            ActionSmooth.Tag := ActionSmooth.Tag or 1;
            RmBack.Tag := RmBack.Tag or 1;

            //  mozhno voobsche ubrat' proverku i deystvovat' kak pri
            //  polnost'yu avtomaticheskom raschete; eto pozvolit
            //  oboyti udalenie fona
            FitServerState := FitClientApp_.FitClient.FitProxy.GetState;
            if (FitServerState = ReadyForFit) or
               //   dopuskaetsya zapuskat' raschet v dannom sostoyanii,
               //   t.k. neobhodimye dannye budut dopolneny avtomaticheski
               (FitServerState = ReadyForAutoFit) or
               (FitServerState = Finished)
               then
            begin
                ActionFitMinNumberOfSpec.Tag := ActionFitMinNumberOfSpec.Tag or 1;
                ActionFitMinDifference.Tag := ActionFitMinDifference.Tag or 1;
            end;

            ActionSelAreaLimits.Tag := ActionSelAreaLimits.Tag or 1;
            PeakPos.Tag := PeakPos.Tag or 1;
            Back.Tag := Back.Tag or 1;
            RFactorIntervals.Tag := RFactorIntervals.Tag or 1;
            ActionSelCharacteristicPoints.Tag :=
                ActionSelCharacteristicPoints.Tag or 1;
            ActionSelCurveBounds.Tag := ActionSelCurveBounds.Tag or 1;
            //  real'no razresheniya primenyayutsya nizhe
            SetAsyncState(FitClientApp_.FitClient.AsyncState);
        end;

        OpenFailure:
        begin
            Caption := ApplicationProperties1.Title;
        end;
    end;

    if (ActionReload.Tag and 1) = 0 then ActionReload.Enabled := False;
    if (ActionReload.Tag and 1) = 1 then ActionReload.Enabled := True;
    if (ActionSaveAsText.Tag and 1) = 0 then ActionSaveAsText.Enabled := False;
    if (ActionSaveAsText.Tag and 1) = 1 then ActionSaveAsText.Enabled := True;

    if (ActionDoAllAuto.Tag and 1) = 0 then ActionDoAllAuto.Enabled := False;
    if (ActionDoAllAuto.Tag and 1) = 1 then ActionDoAllAuto.Enabled := True;
    if (ActionSmooth.Tag and 1) = 0 then ActionSmooth.Enabled := False;
    if (ActionSmooth.Tag and 1) = 1 then ActionSmooth.Enabled := True;
    if (RmBack.Tag and 1) = 0 then
    begin
        RmBack.Enabled := False;
        //  управление действиями подменю
        ActionSelBackAuto.Enabled := False;
        ActionSelBackVis.Enabled := False;
        ActionRemoveBack.Enabled := False;
        ActionRmBackAuto.Enabled := False;
        ActionRmBackSelected.Enabled := False;
    end;
    if (RmBack.Tag and 1) = 1 then
    begin
        RmBack.Enabled := True;
        ActionSelBackAuto.Enabled := True;
        ActionSelBackVis.Enabled := True;
        ActionRemoveBack.Enabled := True;
        ActionRmBackAuto.Enabled := True;
        ActionRmBackSelected.Enabled := True;
    end;

    if (ActionFitMinNumberOfSpec.Tag and 1) = 0 then
        ActionFitMinNumberOfSpec.Enabled := False;
    if (ActionFitMinNumberOfSpec.Tag and 1) = 1 then
        ActionFitMinNumberOfSpec.Enabled := True;

    if (ActionFitMinDifference.Tag and 1) = 0 then
        ActionFitMinDifference.Enabled := False;
    if (ActionFitMinDifference.Tag and 1) = 1 then
        ActionFitMinDifference.Enabled := True;

    if (ActionStopFitting.Tag and 1) = 0 then
        ActionStopFitting.Enabled := False;
    if (ActionStopFitting.Tag and 1) = 1 then
        ActionStopFitting.Enabled := True;

    if (ActionSelAreaLimits.Tag and 1) = 0 then
        ActionSelAreaLimits.Enabled := False;
    if (ActionSelAreaLimits.Tag and 1) = 1 then
        ActionSelAreaLimits.Enabled := True;

    if (ActionSelAreaLimits.Tag and 2) = 0 then
        ActionSelAreaLimits.Checked := False;
    if (ActionSelAreaLimits.Tag and 2) = 2 then
        ActionSelAreaLimits.Checked := True;

    if (ActionSelArea.Tag and 1) = 0 then
        ActionSelArea.Enabled := False;
    if (ActionSelArea.Tag and 1) = 1 then
        ActionSelArea.Enabled := True;

    if (ActionSelEntireProf.Tag and 1) = 0 then
        ActionSelEntireProf.Enabled := False;
    if (ActionSelEntireProf.Tag and 1) = 1 then
        ActionSelEntireProf.Enabled := True;

    if (PeakPos.Tag and 1) = 0 then PeakPos.Enabled := False;
    if (PeakPos.Tag and 1) = 1 then PeakPos.Enabled := True;
    if (PeakPos.Tag and 2) = 0 then PeakPos.Checked := False;
    if (PeakPos.Tag and 2) = 2 then PeakPos.Checked := True;

    if (Back.Tag and 1) = 0 then Back.Enabled := False;
    if (Back.Tag and 1) = 1 then Back.Enabled := True;
    if (Back.Tag and 2) = 0 then Back.Checked := False;
    if (Back.Tag and 2) = 2 then Back.Checked := True;

    if (RFactorIntervals.Tag and 1) = 0 then RFactorIntervals.Enabled := False;
    if (RFactorIntervals.Tag and 1) = 1 then RFactorIntervals.Enabled := True;
    if (RFactorIntervals.Tag and 2) = 0 then RFactorIntervals.Checked := False;
    if (RFactorIntervals.Tag and 2) = 2 then RFactorIntervals.Checked := True;

    if (ActionSelCharacteristicPoints.Tag and 1) = 0 then
        ActionSelCharacteristicPoints.Enabled := False;
    if (ActionSelCharacteristicPoints.Tag and 1) = 1 then
        ActionSelCharacteristicPoints.Enabled := True;

    if (ActionSelCharacteristicPoints.Tag and 2) = 0 then
        ActionSelCharacteristicPoints.Checked := False;
    if (ActionSelCharacteristicPoints.Tag and 2) = 2 then
        ActionSelCharacteristicPoints.Checked := True;

    if (ActionSelCurveBounds.Tag and 1) = 0 then
        ActionSelCurveBounds.Enabled := False;
    if (ActionSelCurveBounds.Tag and 1) = 1 then
        ActionSelCurveBounds.Enabled := True;
end;

procedure TFormMain.SetViewState(State: TViewState);
begin
    case State of
        GraphEmpty:
        begin
            ActionZoomIn.Enabled := False;
            ActionZoomOut.Enabled := False;
            ActionViewMarkers.Enabled := False;
            UseRule.Enabled := False;
        end;

        GraphNotEmpty:
        begin
            ActionZoomIn.Enabled := True;
            ActionZoomOut.Enabled := True;
            ActionViewMarkers.Enabled := True;
            UseRule.Enabled := True;
        end;
    end;
end;

procedure TFormMain.ShowRFactor;
begin
    LabelMin.Caption := FitClientApp_.FitClient.GetRFactorStr;
end;

procedure TFormMain.ShowHint(const Hint: string);
begin
    if csDestroying in ComponentState then Exit;    //  Otherwise sometimes
                                                    //  exception is thrown.
    StatusBar.Panels[1].Text := Hint;
    if Hint = HintDone then
        // Displays latest R-factor value.
        ShowRFactor;
end;

{$hints off}
procedure TFormMain.OnFindComponentClass(Reader: TReader;
    const ClassName: string; var ComponentClass: TComponentClass);
begin
    if CompareText(ClassName, 'Settings_v1') = 0 then
        ComponentClass := Settings_v1
    else
    if CompareText(ClassName, 'Curve_type') = 0 then
        ComponentClass := Curve_type
    else ComponentClass := nil;
end;
{$hints on}

//  poluchenie imeni konfiguratsionnogo fayla
function TFormMain.GetConfigFileName: string;
begin
    Result := GetConfigDir + 'config.xml';
end;

procedure TFormMain.CreateMenuItem(Pos: LongInt; ct: Curve_type;
    ParentMenu: TMenuItem; OnClick: TNotifyEvent);
var mi: TMenuItem;
begin
    mi := TMenuItem.Create(ParentMenu);
    mi.Caption := ct.Name;
    mi.OnClick := OnClick;
    //  obratnaya svyaz'
    mi.Tag := LongInt(ct);  (* 32 *)
    //  novye elementy dobavlyayutsya v nachalo
    //  spiska, no v poryadke sozdaniya
    ParentMenu.Insert(Pos, mi);
end;

{$IFDEF _WINDOWS}
(*
procedure TFormMain.DeleteDummyCurve;
var i: LongInt;
    ct: Curve_type;
    Flag: Boolean;
begin
    while Settings.Curve_types.Count > 1 do
    begin
        Flag := False;
        for i := 0 to Settings.Curve_types.Count - 1 do
        begin
            ct := Curve_type(Settings.Curve_types.Items[i]);
            if ct.Name = 'Dummy' then
            begin
                Settings.Curve_types.Delete(i);
                Flag := True;
                Break;
            end;
        end;
        if not Flag then Break;
    end;
end;
*)
(*
procedure TFormMain.AddDummyCurve;
var ct: Curve_type;
begin
    //  esli ne byl vnesen hotya by odin element,
    //  to chtenie spiska vposledstvii vyzyvaet
    //  isklyuchenie SIGSEGV i padenie programmy;
    //  obschaya zaschita ot nepravil'nogo xml-fayla
    //  ne pomogaet - krivizna biblioteki lazarus'a
    if Settings.Curve_types.Count = 0 then
    begin
        ct := Curve_type.Create(nil);
        ct.Name := 'Dummy';
        ct.Expression := '1.0+1.0';
        Settings.Curve_types.Add(ct);
    end;
end;
*)

procedure TFormMain.DeleteUserCurve(ct: Curve_type);
    //  udalenie elementa podmenyu iz dannogo menyu,
    //  svyazannogo s dannoy zapis'yu tipa krivoy
    procedure DeleteItem(ParentMenu: TMenuItem; Tag: LongInt);
    var i: LongInt;
        mi: TMenuItem;
    begin
        for i := 0 to ParentMenu.Count - 1 do
        begin
            mi := ParentMenu.Items[i];
            if mi.Tag = Tag then
            begin
                mi.Free;
                Break;
            end;
        end;
    end;

var mi: TMenuItem;
begin
    DeleteFile(PChar(ct.FileName));
    Settings.Curve_types.Delete(Settings.Curve_types.IndexOf(ct));
    //AddDummyCurve;
    //  udalenie menyu
    DeleteItem(SelCurveType, LongInt(ct));
    //  poisk menyu MenuDelUserCapt
    mi := SelCurveType.Find(MenuDelUserCapt);
    DeleteItem(mi, LongInt(ct));
    //  udalenie menyu MenuDelUserCapt
    if mi.Count = 0 then mi.Free;
end;

procedure TFormMain.OnDeleteUserCurveClick(Sender: TObject);
var i: LongInt;
    ct: Curve_type;
    mi: TMenuItem;
    Tag: LongInt;
begin
    mi := TMenuItem(Sender);
    Tag := mi.Tag;
    //  udalenie pol'zovatel'skogo tipa krivoy
    for i := 0 to Settings.Curve_types.Count - 1 do
    begin
        ct := Curve_type(Settings.Curve_types.Items[i]);
        //  el-ty sravnivayutsya po ukazatelyu (* 32 *)
        if LongInt(ct) = Tag then
        begin
            DeleteUserCurve(ct);
            Break;
        end;
    end;
end;

procedure TFormMain.OnUserCurveClick(Sender: TObject);
var
    i: LongInt;
    ct: Curve_type;
    mi: TMenuItem;
    Tag: LongInt;
begin
    mi := TMenuItem(Sender);
    Tag := mi.Tag;
    //  poisk pol'zovatel'skogo tipa krivoy
    for i := 0 to Settings.Curve_types.Count - 1 do
    begin
        ct := Curve_type(Settings.Curve_types.Items[i]);
        //  el-ty sravnivayutsya po ukazatelyu (* 32 *)
        if LongInt(ct) = Tag then
        begin
            FitClientApp_.FitClient.SetSpecialCurveParameters(
                ct.Expression, ct.Parameters);
            FitClientApp_.FitClient.CurveTypeId :=
                TUserPointsSet.GetCurveTypeId_;
            Break;
        end;
    end;
end;

procedure TFormMain.CreateUserCurveMenus;
    { True is returned if menu items were added. }
    function AddItem(ParentMenu: TMenuItem;
        var ItemCount: LongInt; OnClick: TNotifyEvent): Boolean;
    var i: LongInt;
        ct: Curve_type;
    begin
        Result := False;
        for i := 0 to Settings.Curve_types.Count - 1 do
        begin
            ct := Curve_type(Settings.Curve_types.Items[i]);
            if ct.Name <> 'Dummy' then
            begin
                CreateMenuItem(i, ct, ParentMenu, OnClick);
                Result := True;
            end;
        end;
        ItemCount := i;
    end;

var ItemCount: LongInt;
    mi: TMenuItem;
begin
    ItemCount := 0;
    if AddItem(SelCurveType, ItemCount, OnUserCurveClick) then
    begin
        { Separator is created. }
        mi := TMenuItem.Create(SelCurveType);
        mi.Caption := ' ';
        mi.Enabled := False;
        SelCurveType.Insert(ItemCount + 1, mi);
        { Menu is created for deleting user curve types. }
        mi := TMenuItem.Create(SelCurveType);
        mi.Caption := MenuDelUserCapt;
        SelCurveType.Add(mi);
        { Submenu for deleting item is created. }
        AddItem(mi, ItemCount, OnDeleteUserCurveClick);
    end;
end;

procedure TFormMain.AddUserCurveMenu(ct: Curve_type);
var i, LastIndex: LongInt;
    mi, DelMenu: TMenuItem;
begin
    //  poisk poslednego elementa menyu,
    //  sootvetstvuyuschego pol'zovatel'skoy krivoy;
    //  odnoznachno otdelit' takie elementy mozhno
    //  po ukazatelyu na obrabotchik sobytiy OnClick
    LastIndex := -1;
    for i := 0 to SelCurveType.Count - 1 do
    begin
        mi := SelCurveType.Items[i];
        if PtrUInt(@mi.OnClick) = PtrUInt(OnUserCurveClick) then
            LastIndex := i;
    end;

    Inc(LastIndex);     //  indeks sleduyuschego elementa
    CreateMenuItem(LastIndex, ct, SelCurveType, OnUserCurveClick);

    DelMenu := SelCurveType.Find(MenuDelUserCapt);
    //  sozdaetsya menyu dlya udaleniya pol'zovatel'skih
    //  krivyh, esli ono ne bylo sozdano ranee
    if DelMenu = nil then
    begin
        DelMenu := TMenuItem.Create(SelCurveType);
        DelMenu.Caption := MenuDelUserCapt;
        SelCurveType.Add(DelMenu);
    end;

    if SelCurveType.Find('-') = nil then
    begin
        //  vstavlyaetsya razdelitel'
        mi := TMenuItem.Create(SelCurveType);
        mi.Caption := '-';
        SelCurveType.Insert(LastIndex + 1, mi);
    end;
    //  sozdaetsya element podmenyu udaleniya
    CreateMenuItem(
        DelMenu.Count, ct, DelMenu, OnDeleteUserCurveClick);
end;

procedure TFormMain.ReadUserCurves;
var F: TSearchRec;
    Path, FileName: string;
    XMLConfig: TXMLConfig;
    C: Curve_type;
begin
    Path := GetConfigDir;
    if FindFirst(Path + '*.cpr', faAnyFile, F) = 0 then
    begin
        repeat
            FileName := GetConfigDir + F.Name;
            XMLConfig := TXMLConfig.Create(FileName);
            try
                C := nil;   //  !!! obyazat. d.b. proinitsializirovano nil !!!
                ReadComponentFromXMLConfig(XMLConfig, 'Component',
                    TComponent(C), OnFindComponentClass, nil);
                C.FileName := FileName;
                Settings.Curve_types.Add(C);
            except
                C.Free;
            end;
            XMLConfig.Free;
        until FindNext(F) <> 0;
{$ifdef windows}
        FindClose(F.FindHandle);
{$else}
        FindClose(F);
{$endif}
    end;
end;

procedure TFormMain.WriteUserCurve(ct: Curve_type);
var XMLConfig: TXMLConfig;
begin
    ct.FileName := GetConfigDir +
        IntToStr(QWord(TimeStampToMSecs(DateTimeToTimeStamp(Now)))) + '.cpr';
    XMLConfig := TXMLConfig.Create(ct.FileName);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', ct);
        XMLConfig.Flush;
    except end;
    XMLConfig.Free;
end;
{$ENDIF}

procedure TFormMain.ReadSettings;
var XMLConfig: TXMLConfig;
    FileName: string;
begin
    FileName := GetConfigFileName;
    if FileExists(FileName) then
    begin
        XMLConfig := TXMLConfig.Create(FileName);
        try
            ReadComponentFromXMLConfig(XMLConfig, 'Component',
                TComponent(Settings), OnFindComponentClass, nil);
        except
            Settings.Free; Settings := nil;
            Settings := Settings_v1.Create(nil);
        end;
        XMLConfig.Free;
    end;
    //  inache ostayutsya nastroyki po-umolchaniyu
end;

procedure TFormMain.WriteSettings;
var XMLConfig: TXMLConfig;
    FileName: string;
begin
    FileName := GetConfigFileName;
    XMLConfig := TXMLConfig.Create(Filename);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', Settings);
        XMLConfig.Flush;
    except end;
    XMLConfig.Free;
end;

procedure TFormMain.OnException(Sender: TObject; E: Exception);
begin
    MessageDlg(E.Message, mtError, [mbOk], 0);

    WriteLog(E.Message, Fatal);
end;

procedure TFormMain.SetHandleEditHint(EditHint: Boolean);
begin
    FHandleEditHint := EditHint;
    if EditHint then TimerBalloonShow.Enabled := True;
end;

initialization
  //{$i cursors.lrs}
  {$i form_main.lrs}
end.



