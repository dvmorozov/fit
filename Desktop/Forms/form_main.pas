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
    LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
    StdCtrls, Menus, points_set, fit_viewer, ComCtrls, fit_client, NumericGrid,
    CheckLst, mscr_specimen_list, LResources, TAGraph, ActnList, app_settings,
    Laz_XMLCfg, neutron_points_set, curve_points_set, gauss_points_set,
    asym_pseudo_voigt_points_set, lorentz_points_set, pseudo_voigt_points_set,
    int_fit_service, two_branches_pseudo_voigt_points_set, named_points_set, log
{$IFDEF _WINDOWS}
{$IFDEF WINDOWS_SPECIFIC}
    , user_points_set
{$ENDIF}
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
    ActionEnableBackgroundVariation: TAction;
    ActionAnimationMode: TAction;
    ActionSelectAllPointsAsCurvePositions: TAction;
    ActionAbout: TAction;
    ActionGlossary: TAction;
    ActionViewMarkers: TAction;
    ActionZoomOut: TAction;
    ActionZoomIn: TAction;
    ActionSelectAll: TAction;
    ActionDelete: TAction;
    ActionCopy: TAction;
    ActionSetMaximumRFactor: TAction;
    ActionStopFit: TAction;
    ActionMinimizeDifference: TAction;
    ActionMinimizeNumberOfCurves: TAction;
    ActionDoAllAutomatically: TAction;
    ActionSaveModelAsText: TAction;
    ActionSelectCurveBounds: TAction;
    ActionRemoveCurvePositions: TAction;
    ActionSelectCurvePositionsManually: TAction;
    ActionComputCurvePositions: TAction;
    ActionRemoveRFactorBounds: TAction;
    ActionSelectRFactorBoundsManually: TAction;
    ActionComputeRFactorBounds: TAction;
    ActionLoadProfile: TAction;
    ActionReloadData: TAction;
    ActionSelectEntireProfile: TAction;
    ActionSelectDataInterval: TAction;
    ActionSelectIntervalBounds: TAction;
    ActionSubtractBackgroundBySelectedPoints: TAction;
    ActionSubtractBackgroundAutomatically: TAction;
    ActionRemoveBackgroundPoints: TAction;
    ActionSelectBackgroundManually: TAction;
    ActionComputeBackgroundPoints: TAction;
    ActionSetBackgroundFraction: TAction;
    ActionSmoothProfile: TAction;
    ActionSelectCharacteristicPoints: TAction;
    ActionQuit: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
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
    LabelAngle: TLabel;
    LabelIntensity: TLabel;
    LabelRFactor: TLabel;
    MainMenu: TMainMenu;
    MenuData: TMenuItem;
    MenuDoAllAutomatically: TMenuItem;
    MenuFile: TMenuItem;
    MenuSeparator4: TMenuItem;
    MenuLoadProfile: TMenuItem;
    MenuSeparator12: TMenuItem;
    MenuSeparator8: TMenuItem;
    MenuSeparator9: TMenuItem;
    MenuSeparator10: TMenuItem;
    MenuSeparator11: TMenuItem;
    MenuReload: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSeparator1: TMenuItem;
    MenuSeparator3: TMenuItem;
    MenuSeparator2: TMenuItem;
    MenuSeparator5: TMenuItem;
    MenuSeparator6: TMenuItem;
    MenuSeparator7: TMenuItem;
    MenuMinimizeNumberOfCurves: TMenuItem;
    MenuMinimizeDifference: TMenuItem;
    MenuArgumentTransformation: TMenuItem;
    MenuCreateRule: TMenuItem;
    MenuBackgroundPoints: TMenuItem;
    MenuEnableCurveScaling: TMenuItem;
    MenuSelectAllPointsAsCurvePositions: TMenuItem;
    MenuAnimationMode: TMenuItem;
    MenuEnableBackgroundVariation: TMenuItem;
    PanelParameters: TPanel;
    PanelIntervals: TPanel;
    PanelDatasheet: TPanel;
    PanelSpecPositions: TPanel;
    PanelBackground: TPanel;
    MenuSelectDataInterval: TMenuItem;
    MenuSelectEntireProfile: TMenuItem;
    MenuRange: TMenuItem;
    MenuSelectIntervalBounds: TMenuItem;
    MenuComputeBackgroundPoints: TMenuItem;
    MenuSubtractBackground: TMenuItem;
    MenuSubtractBackgroundAutomatically: TMenuItem;
    MenuSelectBackgroundManually: TMenuItem;
    MenuRemoveBackgroundPoints: TMenuItem;
    MenuSubtractBackgroundBySelectedPoints: TMenuItem;
    MenuBackground: TMenuItem;
    MenuSetBackgroundFraction: TMenuItem;
    PageControl: TPageControl;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    PanelChart: TPanel;
    Panel2: TPanel;
    PanelRight: TPanel;
    MenuCurvePositions: TMenuItem;
    MenuSetWavelength: TMenuItem;
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
    MenuUseRule: TMenuItem;
    MenuSinThetaLambda: TMenuItem;
    MenuN2Theta: TMenuItem;
    MenuTheta: TMenuItem;
    MenuSetRuleParameters: TMenuItem;
    MenuComputeRFactorBounds: TMenuItem;
    MenuSelectRFactorBoundsManually: TMenuItem;
    MenuRemoveRFactorBounds: TMenuItem;
    MenuRFactorIntervals: TMenuItem;
    MenuComputCurvePositions: TMenuItem;
    MenuSelectCurvePositionsManually: TMenuItem;
    MenuRemoveCurvePositions: TMenuItem;
    MenuStopFit: TMenuItem;
    MenuSmoothProfile: TMenuItem;
    MenuSelectCharacteristicPoints: TMenuItem;
    MenuSelectCurveBounds: TMenuItem;
    SelCurveLorentzian: TMenuItem;
    MenuSaveModelAsText: TMenuItem;
    MenuSelectCurveType: TMenuItem;
    MenuFit: TMenuItem;
    MenuSetMaximumRFactor: TMenuItem;
    MenuGlossary: TMenuItem;
    MenuModel: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    TimerCheckState: TTimer;
    MenuView: TMenuItem;
    MenuZoomIn: TMenuItem;
    MenuZoomOut: TMenuItem;
    ImageList1: TImageList;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    PopupViewMode: TPopupMenu;
    MenuTheta2: TMenuItem;
    MenuN2Theta2: TMenuItem;
    MenuSinThetaLambda2: TMenuItem;
    ImageList2: TImageList;
    MenuViewMarkers: TMenuItem;
    SaveDialog: TSaveDialog;
    MenuEdit: TMenuItem;
    MenuCopy: TMenuItem;
    MenuDelete: TMenuItem;
    MenuSelectAll: TMenuItem;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAnimationModeExecute(Sender: TObject);
    procedure ActionAnimationModeUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionDoAllAutomaticallyExecute(Sender: TObject);
    procedure ActionEnableBackgroundVariationExecute(Sender: TObject);
    procedure ActionEnableBackgroundVariationUpdate(Sender: TObject);
    procedure ActionEnableCurveScalingExecute(Sender: TObject);
    procedure ActionEnableCurveScalingUpdate(Sender: TObject);
    procedure ActionMinimizeDifferenceExecute(Sender: TObject);
    procedure ActionMinimizeNumberOfCurvesExecute(Sender: TObject);
    procedure ActionLoadProfileExecute(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionReloadDataExecute(Sender: TObject);
    procedure ActionRemoveBackgroundPointsExecute(Sender: TObject);
    procedure ActionRemoveRFactorBoundsExecute(Sender: TObject);
    procedure ActionRemoveCurvePositionsExecute(Sender: TObject);
    procedure ActionSubtractBackgroundAutomaticallyExecute(Sender: TObject);
    procedure ActionSubtractBackgroundBySelectedPointsExecute(Sender: TObject);
    procedure ActionSaveModelAsTextExecute(Sender: TObject);
    procedure ActionSelectDataIntervalExecute(Sender: TObject);
    procedure ActionSelectIntervalBoundsExecute(Sender: TObject);
    procedure ActionComputeBackgroundPointsExecute(Sender: TObject);
    procedure ActionSelectBackgroundManuallyExecute(Sender: TObject);
    procedure ActionSelectCharacteristicPointsExecute(Sender: TObject);
    procedure ActionSelectCurveBoundsExecute(Sender: TObject);
    procedure ActionSelCurveExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionSelectEntireProfileExecute(Sender: TObject);
    procedure ActionSelectAllPointsAsCurvePositionsExecute(Sender: TObject);
    procedure ActionComputCurvePositionsExecute(Sender: TObject);
    procedure ActionComputeRFactorBoundsExecute(Sender: TObject);
    procedure ActionSelectRFactorBoundsManuallyExecute(Sender: TObject);
    procedure ActionSelectCurvePositionsManuallyExecute(Sender: TObject);
    procedure ActionSetMaximumRFactorExecute(Sender: TObject);
    procedure ActionSetBackgroundFractionExecute(Sender: TObject);
    procedure ActionSmoothProfileExecute(Sender: TObject);
    procedure ActionStopFitExecute(Sender: TObject);
    procedure ActionViewMarkersExecute(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ApplicationPropertiesHint(Sender: TObject);
    procedure ButAddSelectedDataPointToPositionsClick(Sender: TObject);
    procedure ButAddSelectedPointToIntervalsClick(Sender: TObject);
    procedure ButAddSelectedDataPointClick(Sender: TObject);
    procedure CheckListBoxLegendDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure TimerCheckStateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDataEditingDone(Sender: TObject);
    procedure GridDataSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure FormCreate(Sender: TObject);
    procedure MenuModelClick(Sender: TObject);
    procedure PanelTopClick(Sender: TObject);
    procedure CurvePositionsClick(Sender: TObject);
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
    procedure MenuSinThetaLambdaClick(Sender: TObject);
    procedure MenuThetaClick(Sender: TObject);
    procedure MenuN2ThetaClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuSetWavelengthClick(Sender: TObject);

  protected
    { Initial values set up just after file loading. }
    FInitXGraphMax, FInitXGraphMin, FInitYGraphMax, FInitYGraphMin: Double;

    { These variables are used for separating clicks from area selection. }
    FDownX, FDownY, FUpX, FUpY: Integer;

    { Saved content of edited cells. }
    FSavedPos, FSavedAmp: string;
    { Protects from reentrance into editing finalization. }
    FEditDone: Boolean;
    { Indicates that hint message should be displayed. }
    FHandleEditHint: Boolean;
    procedure SetHandleEditHint(EditHint: Boolean);

  protected
    { The object created event FEditDone. }
    FSenderEditHint: TNumericGrid;

    FHintMessage: string;
    FDrawReticule: Boolean;

    { Callback for calculating object. }
    procedure AsyncOperationFinished(Sender: TObject);
    { Wrapper. }
    procedure SubtractBackground(Auto: Boolean);

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
    procedure CreateCurveTypeMenus;

{$IFDEF _WINDOWS}
    procedure OnDeleteUserCurveClick(Sender: TObject);
    procedure OnUserCurveClick(Sender: TObject);
{$ENDIF}

  public
    { Application settings. Type should be checked. }
    FSettings: Settings_v1;

    FFitViewer: TFitViewer;
    { Index of curve on which the first click was. It is used in the cases when points of only one curve can be selected. }
    FActiveNumber: LongInt;
    { Collection should be passive. Object is set from TFitViewer and is checked on Nil. }
    FCurveList: TMSCRCurveList;
    { Indicates that MenuData in tables were changed. }
    FModifiedParameters: Boolean;
    FModifiedDatasheet: Boolean;
    { Index of a serie point of which is selected at the moment. }
    FCurSerieIndex: LongInt;
    { Index of selected value. }
    FValueIndex: LongInt;

{$IFDEF _WINDOWS}
    //procedure AddDummyCurve;
    //procedure DeleteDummyCurve;
    procedure ReadUserCurves;
    procedure WriteUserCurve(CurveType: Curve_type);
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
    HintSelectProfileInterval:     string = 'Now you can pick the menu item "Select Area"';
    HintThirdFinish:    string = 'Now you can pick a third point - "FINISH"';
    HintMovePeak:       string =
        'Now you can pick the menu item "Move Peak to Results"';
    HintNextPoint:      string =
        'Now you can pick a next point or the menu item "Minimize Difference"';
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

uses
    input_wavelength_dialog, set_maximum_rfactor_dialog, input_back_factor_dialog,
    about_box_dialog, app, int_curve_type_iterator, int_curve_type_selector,
    curve_types_singleton;

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
    Assert(Assigned(Control));
    Assert((Index >= 0) and (Index < Chart.SeriesCount));

    TS := TTASerie(Chart.GetSerie(Index));    //  otsutstvie serii s dannym
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

procedure TFormMain.TimerCheckStateTimer(Sender: TObject);
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
    Assert(Assigned(FitClientApp_.FitClient));
    SetOpenState(FitClientApp_.FitClient.OpenState);
end;

procedure TFormMain.ApplicationPropertiesHint(Sender: TObject);
begin
    ShowHint(Application.Hint);
end;

procedure TFormMain.ActionQuitExecute(Sender: TObject);
begin
    Close;
end;

procedure TFormMain.LoadDataFile(FileName: string);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    //  v sluchae oshibki zagruzki d. vybrasyvat'sya isklyuchenie
    FitClientApp_.FitClient.LoadDataSet(FileName);
    Caption := ApplicationProperties.Title +
        ' - ' + ExtractFileName(FileName);
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionLoadProfileExecute(Sender: TObject);
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
    with OpenDialog do
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

procedure TFormMain.CreateCurveTypeMenus;
var CurveTypeIterator: ICurveTypeIterator;
    CurveTypeSelector: ICurveTypeSelector;
    MenuItem: TMenuItem;
    Index: Integer;
    SelectedCurveTypeId: TCurveTypeId;
begin
    CurveTypeIterator := TCurveTypesSingleton.CreateCurveTypeIterator;
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    Assert(Assigned(CurveTypeIterator));
    Assert(Assigned(CurveTypeSelector));

    SelectedCurveTypeId := CurveTypeSelector.GetSelectedCurveType;
    //  Clears menu.
    MenuSelectCurveType.Clear;
    //  Creates menu items for curve types.
    //  The list must contain at least one item.
    CurveTypeIterator.FirstCurveType;
    Index := 0;
    while True do
    begin
        MenuItem := TMenuItem.Create(MenuSelectCurveType);
        MenuItem.Name := 'CurveType' + IntToStr(Index);
        MenuItem.Tag := CurveTypeIterator.GetCurveTypeTag(CurveTypeIterator.GetCurveTypeId);
        Inc(Index);
        MenuItem.OnClick := ActionSelCurveExecute;
        //  Caption must be set after action to overwrite action attribute.
        MenuItem.Caption := CurveTypeIterator.GetCurveTypeName;
        //  Sets checked state.
        if IsEqualGUID(SelectedCurveTypeId, CurveTypeIterator.GetCurveTypeId) then
            MenuItem.Checked := True;

        MenuSelectCurveType.Add(MenuItem);
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
    if Assigned(FCurveList) then
    begin
        with GridParameters do
        begin
            if (Selection.Left = FixedCols) and
               (Selection.Right = ColCount - 1) then
            begin
                //  udalit' mozhno tol'ko tselye stroki
                RowsToDelete := Selection.Bottom - Selection.Top + 1;
                Index := Selection.Top - FixedRows;
                for i := 1 to RowsToDelete do FCurveList.Delete(Index);
            end;
        end;
        FCurveList.GridAssign(GridParameters);
        FModifiedParameters := True;
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
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.AnimationMode :=
        not FitClientApp_.FitClient.AnimationMode;
end;

procedure TFormMain.ActionAnimationModeUpdate(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    ActionAnimationMode.Checked := FitClientApp_.FitClient.AnimationMode;
end;

procedure TFormMain.ActionDoAllAutomaticallyExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    //  polnyy perezapusk rascheta - flag proveryat' ne nuzhno
    FitClientApp_.FitClient.DoAllAutomatically;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionEnableBackgroundVariationExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.BackgroundVariationEnabled :=
        not FitClientApp_.FitClient.BackgroundVariationEnabled;
    ActionEnableBackgroundVariationUpdate(Sender);
end;

procedure TFormMain.ActionEnableBackgroundVariationUpdate(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    ActionEnableBackgroundVariation.Checked :=
        FitClientApp_.FitClient.BackgroundVariationEnabled;
end;

procedure TFormMain.ActionEnableCurveScalingExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.CurveScalingEnabled :=
        not FitClientApp_.FitClient.CurveScalingEnabled;
    ActionEnableCurveScalingUpdate(Sender);
end;

procedure TFormMain.ActionEnableCurveScalingUpdate(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    ActionEnableCurveScaling.Checked := FitClientApp_.FitClient.CurveScalingEnabled;
end;

procedure TFormMain.ActionMinimizeDifferenceExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    MenuSelectCurvePositionsManually.Checked := False;
    FitClientApp_.FitClient.MinimizeDifference;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionMinimizeNumberOfCurvesExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    MenuSelectCurvePositionsManually.Checked := False;
    FitClientApp_.FitClient.MinimizeNumberOfCurves;
    ShowHint(HintWait);
{$IFDEF FITPRO}
    TimerAsync.Enabled := True;
{$ENDIF}
end;

procedure TFormMain.ActionReloadDataExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.Reload;
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionRemoveBackgroundPointsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.RemoveBackgroundPoints;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionRemoveRFactorBoundsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.RemoveRFactorBounds;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionRemoveCurvePositionsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.RemoveCurvePositions;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSubtractBackgroundAutomaticallyExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    SubtractBackground(True);
end;

procedure TFormMain.ActionSubtractBackgroundBySelectedPointsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    if (FitClientApp_.FitClient.GetBackgroundPoints = nil) or
       (FitClientApp_.FitClient.GetBackgroundPoints.PointsCount < 2) then
       // zdes' eto dopustimaya oshibka
    begin
         MessageDlg('Background points must be selected...',
             mtWarning,[mbOk], 0);
         Exit;
    end;

    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    SubtractBackground(False);
end;

procedure TFormMain.ActionSaveModelAsTextExecute(Sender: TObject);
begin
    if PageControl.ActivePage = TabSheetParameters then
    begin
        if SaveTableAsText(GridParameters) then
            FModifiedParameters := False;
    end
    else
    if PageControl.ActivePage = TabSheetDatasheet then
    begin
        if SaveTableAsText(GridDatasheet) then
            FModifiedDatasheet := False;
    end;
end;

procedure TFormMain.ActionSelectDataIntervalExecute(Sender: TObject);
var SP: TNeutronPointsSet;
    NP: TNeutronPointsSet;
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    SP := FitClientApp_.FitClient.GetSelectedPoints;
    if (SP = nil) or (SP.PointsCount <> 2) then
    begin
        //  zdes' SP=nil - dopustimaya oshibka pol'zovatelya
        MessageDlg('Two limiting points must be selected...',
            mtWarning, [mbOK], 0);
        Exit;
    end;

    NP := FitClientApp_.FitClient.GetProfilePoints;
        //FFitViewer.GetPointsSet(FActiveNumber);
    SP.Sort;

    FitClientApp_.FitClient.SelectProfileInterval(
        NP.IndexOfValueX(SP.PointXCoord[0]),
        NP.IndexOfValueX(SP.PointXCoord[1])
        );
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelectIntervalBoundsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuSelectIntervalBounds.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectIntervalBounds;
        ShowHint(HintFirstStart);
    end
    else
    begin
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    end;
end;

procedure TFormMain.ActionComputeBackgroundPointsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));
    //  perehodim v rezhim vvoda tochek fona
    if not MenuBackground.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectBackground;
        ShowHint(HintFirst);
    end;
    //??? el-t menyu d.b. zapreschen do okonchaniya rascheta;
    // proverit' vse analogichnye sluchai
    FitClientApp_.FitClient.ComputeBackgroundPoints;
end;

procedure TFormMain.ActionSelectBackgroundManuallyExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuBackground.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectBackground;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSelectCharacteristicPointsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuSelectCharacteristicPoints.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectCharacteristicPoints;
        ShowHint(HintFirstStart);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSelectCurveBoundsExecute(Sender: TObject);
var PS: TNeutronPointsSet;
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuSelectCurveBounds.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        PS := FFitViewer.GetActivePointsSet;
        if not (PS is TCurvePointsSet) then
        begin
            MessageDlg('This operation allowed only with curves...',
                mtWarning, [mbOk], 0);
            Exit;
        end;
        FitClientApp_.FitClient.SelectionMode := ModeSelectCurveBounds;
        ShowHint(HintFirstStart);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSelCurveExecute(Sender: TObject);
var
    CurveTypeIterator: ICurveTypeIterator;
    CurveTypeSelector: ICurveTypeSelector;

    NamedPointsSetClasses: array[1..
{$IFDEF WINDOWS_SPECIFIC}
    6
{$ELSE}
    5
{$ENDIF}
    ] of TNamedPointsSetClass = (
        TGaussPointsSet, TLorentzPointsSet,
        TPseudoVoigtPointsSet, TAsymPseudoVoigtPointsSet,
        T2BranchesPseudoVoigtPointsSet
{$IFDEF WINDOWS_SPECIFIC}
        , TUserPointsSet
{$ENDIF}
        );
    i: Integer;
    NamedPointsSetClass: TNamedPointsSetClass;
begin
    Assert(Assigned(Sender));

    CurveTypeIterator := TCurveTypesSingleton.CreateCurveTypeIterator;
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    Assert(Assigned(CurveTypeIterator));
    Assert(Assigned(CurveTypeSelector));

    for i := 1 to Length(NamedPointsSetClasses) do
    begin
        NamedPointsSetClass := NamedPointsSetClasses[i];
        Assert(Assigned(NamedPointsSetClass));

        if TMenuItem(Sender).Tag =
            CurveTypeIterator.GetCurveTypeTag(NamedPointsSetClass.GetCurveTypeId) then
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
                    //  Save configuration MenuData.
                end;

            //  Curve type can be selected only after successful configuration.
{$IFNDEF FIT}
            Assert(Assigned(FitClientApp_));
            Assert(Assigned(FitClientApp_.FitClient));

            FitClientApp_.FitClient.CurveTypeId := NamedPointsSetClass.GetCurveTypeId_;
{$ENDIF}
            CurveTypeSelector.SelectCurveType(NamedPointsSetClass.GetCurveTypeId);
            Break;
        end
    end;
    CreateCurveTypeMenus;
end;

procedure TFormMain.ActionSelectAllExecute(Sender: TObject);
begin
    if ActiveControl is TNumericGrid then
        with ActiveControl as TNumericGrid do SelectAll;
end;

procedure TFormMain.ActionSelectEntireProfileExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.SelectEntireProfile;
    // chtoby proshlo obnovlenie grafika i Chart.XGraphMax
    // i dr. imeli pravil'nye znacheniya
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelectAllPointsAsCurvePositionsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.SelectAllPointsAsCurvePositions;
end;

procedure TFormMain.ActionComputCurvePositionsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.ComputeCurvePositions;
end;

procedure TFormMain.ActionComputeRFactorBoundsExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));
    //  perehodim v rezhim vybora intervalov rascheta R-faktora
    if not MenuRFactorIntervals.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectRFactorBounds;
        ShowHint(HintFirst);
    end;
    //??? el-t menyu d.b. zapreschen do okonchaniya rascheta;
    // proverit' vse analogichnye sluchai
    FitClientApp_.FitClient.ComputeCurveBounds;
end;

procedure TFormMain.ActionSelectRFactorBoundsManuallyExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuRFactorIntervals.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectRFactorBounds;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSelectCurvePositionsManuallyExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if not MenuCurvePositions.Checked then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectCurvePositions;
        ShowHint(HintFirst);
    end
    else
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSetMaximumRFactorExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(SetMaximumRFactorDlg));

    SetMaximumRFactorDlg.FValue := FitClientApp_.FitClient.MaxRFactor;
    if SetMaximumRFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.MaxRFactor := SetMaximumRFactorDlg.FValue;
end;

procedure TFormMain.ActionSetBackgroundFractionExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(InputBackFactorDlg));

    InputBackFactorDlg.FValue := FitClientApp_.FitClient.BackFactor;
    if InputBackFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.BackFactor := InputBackFactorDlg.FValue;
end;

procedure TFormMain.ActionSmoothProfileExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    //  sglazhivanie mozhno primenyat' posledovatel'no neskol'ko raz
    FitClientApp_.FitClient.SmoothProfile;
end;

procedure TFormMain.ActionStopFitExecute(Sender: TObject);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.StopAsyncOper;
end;

procedure TFormMain.ActionViewMarkersExecute(Sender: TObject);
begin
    Assert(Assigned(FFitViewer));

    MenuViewMarkers.Checked := not MenuViewMarkers.Checked;
    FFitViewer.SetViewMarkers(MenuViewMarkers.Checked);
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
        Assert(ColCount >= 2);
        Assert(RowCount >= 1);

        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;

    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.AddPointToCurvePositions(XValue, YValue);
end;

procedure TFormMain.ButAddSelectedPointToIntervalsClick(Sender: TObject);
var XValue, YValue: Double;
begin
    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
    //  nekorrektnyy vvod !!!
    with GridData do
    begin
        Assert(ColCount >= 2);
        Assert(RowCount >= 1);

        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;

    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.AddPointToRFactorBounds(XValue, YValue);
end;

procedure TFormMain.ButAddSelectedDataPointClick(Sender: TObject);
var XValue, YValue: Double;
begin
    //  !!! ispol'zuetsya StrToFloatDef, chtoby obrabatyvat'
    //  nekorrektnyy vvod !!!
    with GridData do
    begin
        Assert(ColCount >= 2);
        Assert(RowCount >= 1);

        XValue := StrToFloatDef(Cells[0, Row], 0);
        YValue := StrToFloatDef(Cells[1, Row], 0);
    end;

    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.AddPointToBackground(XValue, YValue);
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
    //AddDummyCurve;
    WriteSettings;
    FSettings.Free; FSettings := nil;
    FFitViewer.Free; FFitViewer := nil;
end;

procedure TFormMain.GridDataEditingDone(Sender: TObject);
var PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    i: LongInt;
    AllData: Boolean;
begin
    //  vvod vruchnuyu tochek fona poka nevozmozhen
    Assert(Assigned(Sender));

    if Sender = GridBackground then Exit;
    try
        //  !!! pochemu-to vyzyvaetsya po tri raza,
        //  poetomu nuzhno ispol'zovat' flag !!!
        if not FEditDone then
            with Sender as TNumericGrid do
            begin
                Assert(ColCount >= 2);
                Assert(RowCount >= 1);

                FEditDone := True;
                if Col = 0 then
                begin
                    //  redaktiruetsya polozhenie
                    //  !!! esli nichego ne vvedeno, to i obnovlyat' ne nuzhno !!!
                    if FSavedPos <> Cells[0, Row] then
                        Objects[0, Row] := TObject(1);
                end
                else
                begin
                    //  redaktiruetsya amplituda
                    if FSavedAmp <> Cells[1, Row] then
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
                    PrevXValue := StrToFloatDef(FSavedPos, 0);
                    NewXValue := StrToFloatDef(Cells[0, Row], 0);
                    PrevYValue := StrToFloatDef(FSavedAmp, 0);
                    NewYValue := StrToFloatDef(Cells[1, Row], 0);

                    Assert(Assigned(FitClientApp_));
                    Assert(Assigned(FitClientApp_.FitClient));

                    if Sender = GridData then
                        FitClientApp_.FitClient.ReplacePointInProfile(
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
            FSenderEditHint := TNumericGrid(Sender);
            FHintMessage := E.Message;
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
    if FSenderEditHint = GridBackground then
        EditBalloon := EditBalloonGridBackground
    else
    if FSenderEditHint = GridData then
        EditBalloon := EditBalloonGridData;
    //BE := BalloonException.Create(E.Message);
    //if TNumericGrid(Sender).EditorMode then
    //    BE.Handle
    //    Handle := TNumericGrid(Sender).Editor.Handle
    //else
    //begin
        CellRect := FSenderEditHint.CellRect(
            FSenderEditHint.Col, FSenderEditHint.Row);
        EditBalloon.Left := CellRect.Left + FSenderEditHint.Left;
        EditBalloon.Top := CellRect.Top + FSenderEditHint.Top;
        //BE.Handle
            Handle := EditBalloon.Handle;
    //end;
    //raise BE;
    //  !!! bez aktivatsii okna ne rabotaet !!!
    ActiveControl := EditBalloon;
    //  !!! pri isp. ShowBalloon nel'zya dopuskat' vyhod
    //  isklyucheniya za granitsy obrabotchika sobytiya !!!
    ShowBalloon(Handle, WideString(FHintMessage), WideString(''));
{$else}
    MessageDlg(FHintMessage, mtError, [mbOk], 0);
{$endif}
end;

{$hints off}
procedure TFormMain.GridDataSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
    with GridData do
    begin
        Assert(ColCount >= 2);
        Assert(RowCount >= 1);

        FSavedPos := Cells[0, Row];
        FSavedAmp := Cells[1, Row];
    end;
    FEditDone := False;
end;
{$hints on}

procedure TFormMain.FormCreate(Sender: TObject);
begin
    Application.OnException := OnException;
    Caption := ApplicationProperties.Title;

    FFitViewer := TFitViewer.Create(nil);
    FFitViewer.Form := Self;
    FFitViewer.SetFitClient(FitClientApp_.FitClient);
    FFitViewer.SetViewMarkers(MenuViewMarkers.Checked);
    FFitViewer.Clear(Self);

    FActiveNumber := -1;

    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.OnAsyncOperationFinished := AsyncOperationFinished;

    ShowHint(HintMain);
    FModifiedParameters := False;
    FModifiedDatasheet := False;

    //PanelLeft.Color := clWindow;
    //PanelChart.Color := clWindow;
    //PanelRight.Color := clWindow;
    //PageControl.Color := clWindow;
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
    FSettings := Settings_v1.Create(nil);
    ReadSettings;
    CreateCurveTypeMenus;
{$IFDEF _WINDOWS}
    ReadUserCurves;
    CreateUserCurveMenus;
{$ENDIF}
end;

procedure TFormMain.MenuModelClick(Sender: TObject);
begin

end;

procedure TFormMain.PanelTopClick(Sender: TObject);
begin

end;

procedure TFormMain.CurvePositionsClick(Sender: TObject);
begin

end;

procedure TFormMain.SubtractBackground(Auto: Boolean);
begin
    MenuBackground.Checked := False;

    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    FitClientApp_.FitClient.SubtractBackground(Auto);
end;

procedure TFormMain.ScrollBarXChange(Sender: TObject);
var D, DeltaX: Double;
begin
    if Chart.SeriesCount <> 0 then
    begin
        (*  vraschenie v druguyu storonu
        DeltaX := (FInitXGraphMax - FInitXGraphMin) -
                  (Chart.XGraphMax - Chart.XGraphMin);
        D := Chart.XGraphMax - Chart.XGraphMin;
        Chart.XGraphMax := FInitXGraphMax -
            (ScrollBarX.Position - ScrollBarX.Min) * DeltaX /
            (ScrollBarX.Max - ScrollBarX.Min);
        Chart.XGraphMin := Chart.XGraphMax - D;
        *)

        DeltaX := (FInitXGraphMax - FInitXGraphMin) -
                  (Chart.XGraphMax - Chart.XGraphMin);
        D := Chart.XGraphMax - Chart.XGraphMin;
        Chart.XGraphMax := FInitXGraphMax -
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
        DeltaY := (FInitYGraphMax - FInitYGraphMin) -
                  (Chart.YGraphMax - Chart.YGraphMin);
        D := Chart.YGraphMax - Chart.YGraphMin;
        Chart.YGraphMin := FInitYGraphMin +
            (ScrollBarY.Position - ScrollBarY.Min) * DeltaY /
            (ScrollBarY.Max - ScrollBarY.Min);
        Chart.YGraphMax := Chart.YGraphMin + D;
        Chart.Invalidate;
        *)

        DeltaY := (FInitYGraphMax - FInitYGraphMin) -
                  (Chart.YGraphMax - Chart.YGraphMin);
        D := Chart.YGraphMax - Chart.YGraphMin;
        Chart.YGraphMin := FInitYGraphMin +
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
    PointsSet: TPointsSet;
{$ifdef windows}
//    BE: BalloonException;
    Handle: HWND;
{$endif}
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

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
        if FDrawReticule and
            (FDownX = FUpX) and (FDownY = FUpY) and (FActiveNumber <> -1) then
        begin
            FDrawReticule := False;
            //  seriya, na kotoroy byl sdelan klik sravnivaetsya s aktivnoy;
            //  dopuskaetsya klik tol'ko na aktivnoy serii, tochki kotoroy
            //  dobavlyayutsya v vybrannuyu seriyu, ili na vybrannoy serii, iz
            //  kotoroy pri etom tochka udalyaetsya
            if (FCurSerieIndex = FActiveNumber) or
               (FFitViewer.GetPointsSet(FCurSerieIndex) =
                FitClientApp_.FitClient.GetCurrentPointsSet) then
            begin
                case FitClientApp_.FitClient.SelectionMode of
                    ModeSelectNothing: Exit;

                    ModeSelectIntervalBounds: begin
                        // vybor tochek, ogranichivayuschih oblast'
                        PointsSet := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(PointsSet));

                        case PointsSet.PointsCount of
                            0: ShowHint(HintSecondFinish);
                            1: ShowHint(HintSelectProfileInterval);
                            2: Exit;    //  bol'she tochek ne dobavlyaem
                        end;
                    end;

                    ModeSelectCharacteristicPoints: begin
                        // vybor tochek harakterizuyuschih pik
                        PointsSet := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(PointsSet));

                        case PointsSet.PointsCount of
                            0: ShowHint(HintSecondPeak);
                            1: ShowHint(HintThirdFinish);
                            2: ShowHint(HintMovePeak);
                            3: Exit;
                        end;
                    end;

                    ModeSelectCurveBounds: begin
                        // vybor tochek, ogranichivayuschih gaussian
                        PointsSet := FitClientApp_.FitClient.GetSelectedPoints;
                        Assert(Assigned(PointsSet));

                        case PointsSet.PointsCount of
                            0: ShowHint(HintSecondFinish);
                            1: ShowHint(HintMovePeak);
                            2: Exit;
                        end;
                    end;

                    ModeSelectBackground: begin
                        // vybor tochek fona
                        PointsSet := FitClientApp_.FitClient.GetBackgroundPoints;
                        Assert(Assigned(PointsSet));

                        if PointsSet.PointsCount > 0 then ShowHint(HintNextBackPoint)
                        else ShowHint(HintFirst);
                    end;

                    ModeSelectCurvePositions: begin
                        // vybor tochek nachal'nogo polozheniya gaussianov
                        PointsSet := FitClientApp_.FitClient.GetCurvePositions;
                        Assert(Assigned(PointsSet));

                        if PointsSet.PointsCount > 0 then ShowHint(HintNextPoint)
                        else ShowHint(HintFirst);
                    end;

                    ModeSelectRFactorBounds: begin
                        //  vybor granits pikov
                        PointsSet := FitClientApp_.FitClient.GetRFactorBounds;
                        Assert(Assigned(PointsSet));

                        if Odd(PointsSet.PointsCount) then ShowHint(HintNextPointOdd)
                        else ShowHint(HintNextPointEven);
                    end;
                end;

                PointsSet := FFitViewer.GetPointsSet(FCurSerieIndex);

                XValue := PointsSet.PointXCoord[FValueIndex];
                YValue := PointsSet.PointYCoord[FValueIndex];
                FitClientApp_.FitClient.AddPointToActive(XValue, YValue);
            end;
        end;
    except
{$ifdef windows}
        on E: EUserException do
        //  !!! takie isklyucheniya ne popadut v log !!!
        begin
            EditBalloonChart.Left := FUpX;
            EditBalloonChart.Top := FUpY;
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
    FCurSerieIndex := IndexSerie;
    FValueIndex := Index;
    FDrawReticule := True;
    LabelAngle.Caption := Format('%6.2f', [Xg]);
    LabelIntensity.Caption := Format('%6.2f', [Yg]);
end;

procedure TFormMain.ChartMouseDown(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    //  Screen.Cursor := crCursorDrag;
    //  eto budet rabotat' tol'ko so standartnymi kursorami Windows
    //Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
    FDownX := X; FDownY := Y;
end;
{$hints on}

procedure TFormMain.UpdateBarsPos;
var DeltaX, DeltaY: Double;
begin
    Application.ProcessMessages;
    if Chart.SeriesCount <> 0 then
    begin
        // !!! ne budet pravil'no rabotat' pri ispol'zovanii MirrorX !!!
        DeltaX := (FInitXGraphMax - FInitXGraphMin) -
            (Chart.XGraphMax - Chart.XGraphMin);
        // DeltaX m.b. = 0
        if DeltaX <> 0 then
        begin
            //  ustanavlivaetsya priblizhennoe znachenie polozheniya ScrollBar'a
            (*  dlya prokrutki v druguyu storonu
            ScrollBarX.Position := ScrollBarX.Max - Round(
                (Chart.XGraphMin - FInitXGraphMin) *
                (ScrollBarX.Max - ScrollBarX.Min) / DeltaX);
            *)
            ScrollBarX.Position := ScrollBarX.Min + Round(
                (Chart.XGraphMin - FInitXGraphMin) *
                (ScrollBarX.Max - ScrollBarX.Min) / DeltaX);
        end;

        DeltaY := (FInitYGraphMax - FInitYGraphMin) -
            (Chart.YGraphMax - Chart.YGraphMin);
        // DeltaY m.b. = 0
        if DeltaY <> 0 then
        begin
            //  ustanavlivaetsya priblizhennoe znachenie polozheniya SrollBar'a
            (*  dlya prokrutki v druguyu storonu
            ScrollBarY.Position := ScrollBarY.Min + Round(
                (Chart.YGraphMin - FInitYGraphMin) *
                (ScrollBarY.Max - ScrollBarY.Min) / DeltaY);
            *)
            //  polozhenie dvizhka bara na min. sootvetstvuet
            //  polozheniyu okna na maks. grafika
            ScrollBarY.Position := ScrollBarY.Max - Round(
                (Chart.YGraphMin - FInitYGraphMin) *
                (ScrollBarY.Max - ScrollBarY.Min) / DeltaY);
        end;
    end;
end;

{$hints off}
procedure TFormMain.ChartMouseUp(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    UpdateBarsPos;
    FUpX := X; FUpY := Y;
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
    if FSenderEditHint = GridBackground then
        Edit_HideBalloonTip(EditBalloonGridBackground.Handle)
    else
    if FSenderEditHint = GridData then
        Edit_HideBalloonTip(EditBalloonGridData.Handle);
    //  vzvraschaetsya fokus vvoda
    ActiveControl := FSenderEditHint;
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
end;

{$hints off}
procedure TFormMain.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var i: LongInt;
begin
    //with Chart1 do
    // for i := 0 to SeriesCount - 1 do
    //  if Series[i].GetCursorValueIndex <> -1 then
    //   begin Screen.Cursor := crCross; MenuQuit end;
    Screen.Cursor := crArrow;
end;

procedure TFormMain.CheckListBoxLegendKeyPress(Sender: TObject; var Key: Char);
begin
    CheckListBoxChanged;
end;
{$hints on}

procedure TFormMain.CheckListBoxChanged;
var i: LongInt;
    Serie: TTASerie;
begin
    with CheckListBoxLegend do
        for i := 0 to Items.Count - 1 do
        begin
            if Assigned(Items.Objects[i]) then
            begin
                if Items.Objects[i] is TTASerie then
                begin
                    Serie := TTASerie(Items.Objects[i]);

                    if Checked[i] then
                    begin
                        Serie.ShowLines := Serie.InitShowLines;
                        Serie.ShowPoints := Serie.InitShowPoints;
                    end
                    else
                    begin
                        Serie.ShowLines := False;
                        Serie.ShowPoints := False;
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
procedure TFormMain.MenuSinThetaLambdaClick(Sender: TObject);
var SaveDecimalSeparator: Char;
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    Assert(Assigned(FFitViewer));

    if FitClientApp_.FitClient.GetWaveLength = 0 then
    begin
        Assert(Assigned(InputWavelengthDlg));

        if InputWavelengthDlg.ShowModal = mrOk then
        begin
            SaveDecimalSeparator := DecimalSeparator;
            DecimalSeparator := '.';
            Screen.Cursor := crHourGlass;
            FitClientApp_.FitClient.SetWaveLength(StrToFloat(
                InputWavelengthDlg.WavelengthValueEdit.Text));
            Screen.Cursor := crDefault;
            DecimalSeparator := SaveDecimalSeparator;
            FFitViewer.XCoordMode := XCM_SINTL;
            if Assigned(FCurveList) then
            begin
                FCurveList.FViewMode := XCM_SINTL;
                FCurveList.GridAssign(GridParameters);
            end;
            MenuTheta.Checked := False;
            MenuTheta2.Checked := False;
            MenuN2Theta.Checked := False;
            MenuN2Theta2.Checked := False;
            MenuSinThetaLambda.Checked := True;
            MenuSinThetaLambda2.Checked := True;
        end;
    end {if FitClientApp_.FitClient.GetWaveLength = 0 then...}
    else
    begin
        FFitViewer.XCoordMode := XCM_SINTL;
        if Assigned(FCurveList) then
        begin
            FCurveList.FViewMode := XCM_SINTL;
            FCurveList.GridAssign(GridParameters);
        end;
        MenuTheta.Checked := False;
        MenuTheta2.Checked := False;
        MenuN2Theta.Checked := False;
        MenuN2Theta2.Checked := False;
        MenuSinThetaLambda.Checked := True;
        MenuSinThetaLambda2.Checked := True;
    end;
end;
{$warnings on}

procedure TFormMain.MenuThetaClick(Sender: TObject);
begin
    Assert(Assigned(FFitViewer));

    FFitViewer.XCoordMode := XCM_T;
    if Assigned(FCurveList) then
    begin
        FCurveList.FViewMode := XCM_T;
        FCurveList.GridAssign(GridParameters);
    end;
    MenuTheta.Checked := True;
    MenuTheta2.Checked := True;
    MenuN2Theta.Checked := False;
    MenuN2Theta2.Checked := False;
    MenuSinThetaLambda.Checked := False;
    MenuSinThetaLambda2.Checked := False;
end;

procedure TFormMain.MenuN2ThetaClick(Sender: TObject);
begin
    Assert(Assigned(FFitViewer));

    FFitViewer.XCoordMode := XCM_2T;
    if Assigned(FCurveList) then
    begin
        FCurveList.FViewMode := XCM_2T;
        FCurveList.GridAssign(GridParameters);
    end;
    MenuTheta.Checked := False;
    MenuTheta2.Checked := False;
    MenuN2Theta.Checked := True;
    MenuN2Theta2.Checked := True;
    MenuSinThetaLambda.Checked := False;
    MenuSinThetaLambda2.Checked := False;
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
    if SaveDialog.Execute then
    begin
        FileName := SaveDialog.FileName;
        if FileName <> '' then
        begin
            SaveDialog.InitialDir := ExtractFilePath(FileName);
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
    //MessageDlg('Computation done...', mtInformation, [mbOk], 0);
    ShowTime;
    ShowHint(HintMain);
end;

procedure TFormMain.ShowTime;
var TimeStr: string;
begin
    TimeStr := FitClientApp_.FitClient.GetCalcTimeStr;
    StatusBar.Panels[0].Text := 'Elapsed time: ' + TimeStr;
    Application.ProcessMessages;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Result: LongInt;
begin
    CanClose := True;
    if FModifiedParameters then
    begin
        Result := MessageDlg(
            'Model parameters has been modified.' + #13#10 + 'Save?',
            mtConfirmation, mbYesNoCancel, 0);

        if Result = mrYes then
        begin
            if SaveTableAsText(GridParameters) then FModifiedParameters := False
            else
            begin
                PageControl.ActivePage := TabSheetParameters;
                CanClose := False;
                Exit;
            end;
        end;

        if Result = mrCancel then
        begin
            PageControl.ActivePage := TabSheetParameters;
            CanClose := False;
            Exit;
        end;
    end;

    if FModifiedDatasheet then
    begin
        Result := MessageDlg(
            'Datasheet has been modified.' + #13#10 + 'Save?',
            mtConfirmation, mbYesNoCancel, 0);
        if Result = mrYes then
        begin
            if SaveTableAsText(GridDatasheet) then FModifiedDatasheet := False
            else
            begin
                PageControl.ActivePage := TabSheetDatasheet;
                CanClose := False;
                Exit;
            end;
        end;

        if Result = mrCancel then
        begin
            PageControl.ActivePage := TabSheetDatasheet;
            CanClose := False;
        end;
    end;
end;

procedure TFormMain.MenuSetWavelengthClick(Sender: TObject);
begin
    if InputWavelengthDlg.ShowModal = mrOk then
    begin
        Assert(Assigned(FitClientApp_));
        Assert(Assigned(FitClientApp_.FitClient));

        FitClientApp_.FitClient.SetWaveLength(InputWavelengthDlg.FValue);
        FFitViewer.XCoordMode := XCM_SINTL;
        if Assigned(FCurveList) then
        begin
            FCurveList.FViewMode := XCM_SINTL;
            FCurveList.GridAssign(GridParameters);
        end;
        MenuSinThetaLambda.Checked := True;
        MenuSinThetaLambda2.Checked := True;
    end;
end;

procedure TFormMain.SetSelectionMode(ASelectionMode: TSelMode);
//  dlya predotvrascheniya mertsaniya menyu
var SelBackVisCaption, SelSpecPosVisCaption,
    SelRFactorIntervalsVisCaption: string;
    PointsSet: TPointsSet;
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    MenuSelectDataInterval.Tag := MenuSelectDataInterval.Tag and $FFFFFFFE;
    MenuSelectIntervalBounds.Tag := MenuSelectIntervalBounds.Tag and $FFFFFFFD;
    MenuSelectCharacteristicPoints.Tag := MenuSelectCharacteristicPoints.Tag and $FFFFFFFD;
    MenuSelectCurveBounds.Tag := MenuSelectCurveBounds.Tag and $FFFFFFFD;
    MenuBackground.Tag := MenuBackground.Tag and $FFFFFFFD;
    MenuRFactorIntervals.Tag := MenuRFactorIntervals.Tag and $FFFFFFFD;
    MenuCurvePositions.Tag := MenuCurvePositions.Tag and $FFFFFFFD;
    //  initsializatsiya vosstanavlivaet ishodnyy tekst,
    //  kogda sootvetstvuyuschie flagi ne ustanovleny
    SelBackVisCaption := StartVisualSel;
    SelSpecPosVisCaption := StartVisPosSel;
    SelRFactorIntervalsVisCaption := StartVisualSel;

    MenuRemoveBackgroundPoints.Tag := MenuRemoveBackgroundPoints.Tag and $FFFFFFFE;
    MenuRemoveRFactorBounds.Tag := MenuRemoveRFactorBounds.Tag and $FFFFFFFE;
    MenuRemoveCurvePositions.Tag := MenuRemoveCurvePositions.Tag and $FFFFFFFE;

    case ASelectionMode of
        ModeSelectNothing:
            begin
                if FitClientApp_.FitClient.SelectedAreaMode then
                    MenuSelectEntireProfile.Tag := MenuSelectEntireProfile.Tag or 1;
            end;
        ModeSelectIntervalBounds:
            begin
                MenuSelectIntervalBounds.Tag := MenuSelectIntervalBounds.Tag or 2;
                PointsSet := FitClientApp_.FitClient.GetSelectedPoints;
                Assert(Assigned(PointsSet));

                if PointsSet.PointsCount = 2 then
                    MenuSelectDataInterval.Tag := MenuSelectDataInterval.Tag or 1;
            end;
        ModeSelectCharacteristicPoints:
            MenuSelectCharacteristicPoints.Tag := MenuSelectCharacteristicPoints.Tag or 2;
        ModeSelectCurveBounds: MenuSelectCurveBounds.Tag := MenuSelectCurveBounds.Tag or 2;
        ModeSelectBackground:
            begin
                MenuRemoveBackgroundPoints.Tag := MenuRemoveBackgroundPoints.Tag or 1;
                MenuBackground.Tag := MenuBackground.Tag or 2;
                PointsSet := FitClientApp_.FitClient.GetBackgroundPoints;
                Assert(Assigned(PointsSet));

                if PointsSet.PointsCount > 0 then
                    ActionSubtractBackgroundBySelectedPoints.Tag := ActionSubtractBackgroundBySelectedPoints.Tag or 1
                else
                    ActionSubtractBackgroundBySelectedPoints.Tag := ActionSubtractBackgroundBySelectedPoints.Tag and $FFFFFFFE;
                SelBackVisCaption := StopVisualSel;
            end;

        ModeSelectCurvePositions:
            begin
                MenuRemoveCurvePositions.Tag := MenuRemoveCurvePositions.Tag or 1;
                MenuCurvePositions.Tag := MenuCurvePositions.Tag or 2;
                SelSpecPosVisCaption := StopVisPosSel;
            end;

        ModeSelectRFactorBounds:
            begin
                MenuRemoveRFactorBounds.Tag := MenuRemoveRFactorBounds.Tag or 1;
                MenuRFactorIntervals.Tag := MenuRFactorIntervals.Tag or 2;
                SelRFactorIntervalsVisCaption := StopVisualSel;
            end;
    end;
    MenuSelectBackgroundManually.Caption := SelBackVisCaption;
    MenuSelectCurvePositionsManually.Caption := SelSpecPosVisCaption;
    MenuSelectRFactorBoundsManually.Caption := SelRFactorIntervalsVisCaption;
end;

procedure TFormMain.SetAsyncState(State: TAsyncState);
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    case State of
        AsyncWorks:
        begin
            //  Operation
            ActionDoAllAutomatically.Tag := ActionDoAllAutomatically.Tag and $FFFFFFFE;
            MenuSmoothProfile.Tag := MenuSmoothProfile.Tag and $FFFFFFFE;
            MenuSubtractBackground.Tag := MenuSubtractBackground.Tag and $FFFFFFFE;
            ActionStopFit.Tag := ActionStopFit.Tag or 1;

            //  Dataset
            MenuSelectIntervalBounds.Tag := MenuSelectIntervalBounds.Tag and $FFFFFFFE;
            MenuSelectDataInterval.Tag := MenuSelectDataInterval.Tag and $FFFFFFFE;
            MenuSelectEntireProfile.Tag := MenuSelectEntireProfile.Tag and $FFFFFFFE;
            MenuCurvePositions.Tag := MenuCurvePositions.Tag and $FFFFFFFE;
            MenuBackground.Tag := MenuBackground.Tag and $FFFFFFFE;
            MenuRFactorIntervals.Tag := MenuRFactorIntervals.Tag and $FFFFFFFE;
            MenuSelectCharacteristicPoints.Tag := MenuSelectCharacteristicPoints.Tag and $FFFFFFFE;
            MenuSelectCurveBounds.Tag := MenuSelectCurveBounds.Tag and $FFFFFFFE;
        end;

        AsyncStart:
        begin
            //  Operation
            ActionStopFit.Tag := ActionStopFit.Tag and $FFFFFFFE;

            SetSelectionMode(FitClientApp_.FitClient.SelectionMode);
        end;

        AsyncDone:
        begin
            //  Operation
            ActionStopFit.Tag := ActionStopFit.Tag and $FFFFFFFE;

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
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));
    //  File
    ActionReloadData.Tag := ActionReloadData.Tag and $FFFFFFFE;
    ActionSaveModelAsText.Tag := ActionSaveModelAsText.Tag and $FFFFFFFE;

    //  Operation
    ActionDoAllAutomatically.Tag := ActionDoAllAutomatically.Tag and $FFFFFFFE;
    ActionSmoothProfile.Tag := ActionSmoothProfile.Tag and $FFFFFFFE;
    MenuSubtractBackground.Tag := MenuSubtractBackground.Tag and $FFFFFFFE;
    ActionSubtractBackgroundBySelectedPoints.Tag := ActionSubtractBackgroundBySelectedPoints.Tag and $FFFFFFFE;
    ActionMinimizeNumberOfCurves.Tag := ActionMinimizeNumberOfCurves.Tag and $FFFFFFFE;
    ActionMinimizeDifference.Tag := ActionMinimizeDifference.Tag and $FFFFFFFE;
    ActionStopFit.Tag := ActionStopFit.Tag and $FFFFFFFE;

    //  Dataset
    ActionSelectIntervalBounds.Tag := ActionSelectIntervalBounds.Tag and $FFFFFFFE;
    ActionSelectDataInterval.Tag := ActionSelectDataInterval.Tag and $FFFFFFFE;
    ActionSelectEntireProfile.Tag := ActionSelectEntireProfile.Tag and $FFFFFFFE;
    MenuBackground.Tag := MenuBackground.Tag and $FFFFFFFE;
    MenuRFactorIntervals.Tag := MenuRFactorIntervals.Tag and $FFFFFFFE;
    ActionSelectCharacteristicPoints.Tag :=
        ActionSelectCharacteristicPoints.Tag and $FFFFFFFE;
    ActionSelectCurveBounds.Tag := ActionSelectCurveBounds.Tag and $FFFFFFFE;

    case State of
        OpenSuccess:
        begin
            //  !!! rabota s Tag sdelana dlya zaschity ot mertsaniya !!!
            ActionReloadData.Tag := ActionReloadData.Tag or 1;

            ActionDoAllAutomatically.Tag := ActionDoAllAutomatically.Tag or 1;
            ActionSmoothProfile.Tag := ActionSmoothProfile.Tag or 1;
            MenuSubtractBackground.Tag := MenuSubtractBackground.Tag or 1;

            //  mozhno voobsche ubrat' proverku i deystvovat' kak pri
            //  polnost'yu avtomaticheskom raschete; eto pozvolit
            //  oboyti udalenie fona
            FitServerState := FitClientApp_.FitClient.FitService.GetState;
            if (FitServerState = ReadyForFit) or
               //   dopuskaetsya zapuskat' raschet v dannom sostoyanii,
               //   t.k. neobhodimye dannye budut dopolneny avtomaticheski
               (FitServerState = ReadyForAutoFit) or
               (FitServerState = Finished)
               then
            begin
                ActionMinimizeNumberOfCurves.Tag := ActionMinimizeNumberOfCurves.Tag or 1;
                ActionMinimizeDifference.Tag := ActionMinimizeDifference.Tag or 1;
            end;

            ActionSelectIntervalBounds.Tag := ActionSelectIntervalBounds.Tag or 1;
            MenuCurvePositions.Tag := MenuCurvePositions.Tag or 1;
            MenuBackground.Tag := MenuBackground.Tag or 1;
            MenuRFactorIntervals.Tag := MenuRFactorIntervals.Tag or 1;
            ActionSelectCharacteristicPoints.Tag :=
                ActionSelectCharacteristicPoints.Tag or 1;
            ActionSelectCurveBounds.Tag := ActionSelectCurveBounds.Tag or 1;
            //  real'no razresheniya primenyayutsya nizhe
            SetAsyncState(FitClientApp_.FitClient.AsyncState);
        end;

        OpenFailure:
        begin
            Caption := ApplicationProperties.Title;
        end;
    end;

    if (ActionReloadData.Tag and 1) = 0 then ActionReloadData.Enabled := False;
    if (ActionReloadData.Tag and 1) = 1 then ActionReloadData.Enabled := True;
    if (ActionSaveModelAsText.Tag and 1) = 0 then ActionSaveModelAsText.Enabled := False;
    if (ActionSaveModelAsText.Tag and 1) = 1 then ActionSaveModelAsText.Enabled := True;

    if (ActionDoAllAutomatically.Tag and 1) = 0 then ActionDoAllAutomatically.Enabled := False;
    if (ActionDoAllAutomatically.Tag and 1) = 1 then ActionDoAllAutomatically.Enabled := True;
    if (ActionSmoothProfile.Tag and 1) = 0 then ActionSmoothProfile.Enabled := False;
    if (ActionSmoothProfile.Tag and 1) = 1 then ActionSmoothProfile.Enabled := True;
    if (MenuSubtractBackground.Tag and 1) = 0 then
    begin
        MenuSubtractBackground.Enabled := False;
        //  управление действиями подменю
        ActionComputeBackgroundPoints.Enabled := False;
        ActionSelectBackgroundManually.Enabled := False;
        ActionRemoveBackgroundPoints.Enabled := False;
        ActionSubtractBackgroundAutomatically.Enabled := False;
        ActionSubtractBackgroundBySelectedPoints.Enabled := False;
    end;
    if (MenuSubtractBackground.Tag and 1) = 1 then
    begin
        MenuSubtractBackground.Enabled := True;
        ActionComputeBackgroundPoints.Enabled := True;
        ActionSelectBackgroundManually.Enabled := True;
        ActionRemoveBackgroundPoints.Enabled := True;
        ActionSubtractBackgroundAutomatically.Enabled := True;
        ActionSubtractBackgroundBySelectedPoints.Enabled := True;
    end;

    if (ActionMinimizeNumberOfCurves.Tag and 1) = 0 then
        ActionMinimizeNumberOfCurves.Enabled := False;
    if (ActionMinimizeNumberOfCurves.Tag and 1) = 1 then
        ActionMinimizeNumberOfCurves.Enabled := True;

    if (ActionMinimizeDifference.Tag and 1) = 0 then
        ActionMinimizeDifference.Enabled := False;
    if (ActionMinimizeDifference.Tag and 1) = 1 then
        ActionMinimizeDifference.Enabled := True;

    if (ActionStopFit.Tag and 1) = 0 then
        ActionStopFit.Enabled := False;
    if (ActionStopFit.Tag and 1) = 1 then
        ActionStopFit.Enabled := True;

    if (ActionSelectIntervalBounds.Tag and 1) = 0 then
        ActionSelectIntervalBounds.Enabled := False;
    if (ActionSelectIntervalBounds.Tag and 1) = 1 then
        ActionSelectIntervalBounds.Enabled := True;

    if (ActionSelectIntervalBounds.Tag and 2) = 0 then
        ActionSelectIntervalBounds.Checked := False;
    if (ActionSelectIntervalBounds.Tag and 2) = 2 then
        ActionSelectIntervalBounds.Checked := True;

    if (ActionSelectDataInterval.Tag and 1) = 0 then
        ActionSelectDataInterval.Enabled := False;
    if (ActionSelectDataInterval.Tag and 1) = 1 then
        ActionSelectDataInterval.Enabled := True;

    if (ActionSelectEntireProfile.Tag and 1) = 0 then
        ActionSelectEntireProfile.Enabled := False;
    if (ActionSelectEntireProfile.Tag and 1) = 1 then
        ActionSelectEntireProfile.Enabled := True;

    if (MenuCurvePositions.Tag and 1) = 0 then MenuCurvePositions.Enabled := False;
    if (MenuCurvePositions.Tag and 1) = 1 then MenuCurvePositions.Enabled := True;
    if (MenuCurvePositions.Tag and 2) = 0 then MenuCurvePositions.Checked := False;
    if (MenuCurvePositions.Tag and 2) = 2 then MenuCurvePositions.Checked := True;

    if (MenuBackground.Tag and 1) = 0 then MenuBackground.Enabled := False;
    if (MenuBackground.Tag and 1) = 1 then MenuBackground.Enabled := True;
    if (MenuBackground.Tag and 2) = 0 then MenuBackground.Checked := False;
    if (MenuBackground.Tag and 2) = 2 then MenuBackground.Checked := True;

    if (MenuRFactorIntervals.Tag and 1) = 0 then MenuRFactorIntervals.Enabled := False;
    if (MenuRFactorIntervals.Tag and 1) = 1 then MenuRFactorIntervals.Enabled := True;
    if (MenuRFactorIntervals.Tag and 2) = 0 then MenuRFactorIntervals.Checked := False;
    if (MenuRFactorIntervals.Tag and 2) = 2 then MenuRFactorIntervals.Checked := True;

    if (ActionSelectCharacteristicPoints.Tag and 1) = 0 then
        ActionSelectCharacteristicPoints.Enabled := False;
    if (ActionSelectCharacteristicPoints.Tag and 1) = 1 then
        ActionSelectCharacteristicPoints.Enabled := True;

    if (ActionSelectCharacteristicPoints.Tag and 2) = 0 then
        ActionSelectCharacteristicPoints.Checked := False;
    if (ActionSelectCharacteristicPoints.Tag and 2) = 2 then
        ActionSelectCharacteristicPoints.Checked := True;

    if (ActionSelectCurveBounds.Tag and 1) = 0 then
        ActionSelectCurveBounds.Enabled := False;
    if (ActionSelectCurveBounds.Tag and 1) = 1 then
        ActionSelectCurveBounds.Enabled := True;
end;

procedure TFormMain.SetViewState(State: TViewState);
begin
    case State of
        GraphEmpty:
        begin
            ActionZoomIn.Enabled := False;
            ActionZoomOut.Enabled := False;
            ActionViewMarkers.Enabled := False;
            MenuUseRule.Enabled := False;
        end;

        GraphNotEmpty:
        begin
            ActionZoomIn.Enabled := True;
            ActionZoomOut.Enabled := True;
            ActionViewMarkers.Enabled := True;
            MenuUseRule.Enabled := True;
        end;
    end;
end;

procedure TFormMain.ShowRFactor;
begin
    Assert(Assigned(FitClientApp_));
    Assert(Assigned(FitClientApp_.FitClient));

    LabelRFactor.Caption := FitClientApp_.FitClient.GetRFactorStr;
    Application.ProcessMessages;
end;

procedure TFormMain.ShowHint(const Hint: string);
begin
    if csDestroying in ComponentState then Exit;    //  Otherwise sometimes
                                                    //  exception is thrown.

    Assert(StatusBar.Panels.Count <> 0);

    StatusBar.Panels[1].Text := Hint;
    Application.ProcessMessages;
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
    Assert(Assigned(ParentMenu));

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
    Assert(Assigned(ct));
    Assert(Assigned(FSettings));
    Assert(Assigned(FSettings.Curve_types));

    DeleteFile(PChar(ct.FFileName));
    FSettings.Curve_types.Delete(FSettings.Curve_types.IndexOf(ct));
    //AddDummyCurve;
    //  udalenie menyu
    DeleteItem(MenuSelectCurveType, LongInt(ct));
    //  poisk menyu MenuDelUserCapt
    mi := MenuSelectCurveType.Find(MenuDelUserCapt);
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
    Assert(Assigned(FSettings));
    Assert(Assigned(FSettings.Curve_types));

    mi := TMenuItem(Sender);
    Tag := mi.Tag;
    //  udalenie pol'zovatel'skogo tipa krivoy
    for i := 0 to FSettings.Curve_types.Count - 1 do
    begin
        ct := Curve_type(FSettings.Curve_types.Items[i]);
        //  el-ty sravnivayutsya po ukazatelyu (* 32 *)
        if LongInt(ct) = Tag then
        begin
            DeleteUserCurve(ct);
            Break;
        end;
    end;
end;

procedure TFormMain.OnUserCurveClick(Sender: TObject);
{$IFDEF WINDOWS_SPECIFIC}
var
    i: LongInt;
    ct: Curve_type;
    mi: TMenuItem;
    Tag: LongInt;
    CurveTypeSelector: ICurveTypeSelector;
{$ENDIF}
begin
{$IFDEF WINDOWS_SPECIFIC}
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    Assert(Assigned(CurveTypeSelector));
    Assert(Assigned(Sender));
    Assert(Assigned(FSettings);
    Assert(Assigned(FSettings.Curve_types);
    Assert(Assigned(FitClientApp_);
    Assert(Assigned(FitClientApp_.FitClient);

    mi := TMenuItem(Sender);
    Tag := mi.Tag;
    //  poisk pol'zovatel'skogo tipa krivoy
    for i := 0 to FSettings.Curve_types.Count - 1 do
    begin
        ct := Curve_type(FSettings.Curve_types.Items[i]);
        //  el-ty sravnivayutsya po ukazatelyu (* 32 *)
        if LongInt(ct) = Tag then
        begin
            FitClientApp_.FitClient.SetSpecialCurveParameters(
                ct.Expression, ct.Parameters);
{$IFNDEF FIT}
            FitClientApp_.FitClient.CurveTypeId :=
                TUserPointsSet.GetCurveTypeId;
{$ENDIF}
            CurveTypeSelector.SelectCurveType(TUserPointsSet.GetCurveTypeId);
            Break;
        end;
    end;
{$ENDIF}
end;

procedure TFormMain.CreateUserCurveMenus;
    { True is returned if menu items were added. }
    function AddItem(ParentMenu: TMenuItem;
        var ItemCount: LongInt; OnClick: TNotifyEvent): Boolean;

    var i: LongInt;
        ct: Curve_type;
    begin
        Assert(Assigned(FSettings));
        Assert(Assigned(FSettings.Curve_types));

        Result := False;
        for i := 0 to FSettings.Curve_types.Count - 1 do
        begin
            ct := Curve_type(FSettings.Curve_types.Items[i]);
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
    if AddItem(MenuSelectCurveType, ItemCount, OnUserCurveClick) then
    begin
        { Separator is created. }
        mi := TMenuItem.Create(MenuSelectCurveType);
        mi.Caption := ' ';
        mi.Enabled := False;
        MenuSelectCurveType.Insert(ItemCount + 1, mi);
        { Menu is created for deleting user curve types. }
        mi := TMenuItem.Create(MenuSelectCurveType);
        mi.Caption := MenuDelUserCapt;
        MenuSelectCurveType.Add(mi);
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
    for i := 0 to MenuSelectCurveType.Count - 1 do
    begin
        mi := MenuSelectCurveType.Items[i];
        if PtrUInt(@mi.OnClick) = PtrUInt(OnUserCurveClick) then
            LastIndex := i;
    end;

    Inc(LastIndex);     //  indeks sleduyuschego elementa
    CreateMenuItem(LastIndex, ct, MenuSelectCurveType, OnUserCurveClick);

    DelMenu := MenuSelectCurveType.Find(MenuDelUserCapt);
    //  sozdaetsya menyu dlya udaleniya pol'zovatel'skih
    //  krivyh, esli ono ne bylo sozdano ranee
    if DelMenu = nil then
    begin
        DelMenu := TMenuItem.Create(MenuSelectCurveType);
        DelMenu.Caption := MenuDelUserCapt;
        MenuSelectCurveType.Add(DelMenu);
    end;

    if MenuSelectCurveType.Find('-') = nil then
    begin
        //  vstavlyaetsya razdelitel'
        mi := TMenuItem.Create(MenuSelectCurveType);
        mi.Caption := '-';
        MenuSelectCurveType.Insert(LastIndex + 1, mi);
    end;
    //  sozdaetsya element podmenyu udaleniya
    CreateMenuItem(
        DelMenu.Count, ct, DelMenu, OnDeleteUserCurveClick);
end;

procedure TFormMain.ReadUserCurves;
var SearchRec: TSearchRec;
    Path, FileName: string;
    XMLConfig: TXMLConfig;
    CurveType: Curve_type;
begin
    Assert(Assigned(FSettings));
    Assert(Assigned(FSettings.Curve_types));

    Path := GetConfigDir;
    if FindFirst(Path + '*.cpr', faAnyFile, SearchRec) = 0 then
    begin
        repeat
            FileName := GetConfigDir + SearchRec.Name;
            XMLConfig := TXMLConfig.Create(FileName);
            try
                //  !!! obyazat. d.b. proinitsializirovano nil !!!
                CurveType := nil;
                ReadComponentFromXMLConfig(XMLConfig, 'Component',
                    TComponent(CurveType), OnFindComponentClass, nil);
                try
                    Assert(Assigned(CurveType));

                    CurveType.FFileName := FileName;
                    FSettings.Curve_types.Add(CurveType);
                except
                    CurveType.Free;
                end;
            finally
                XMLConfig.Free;
            end;
        until FindNext(SearchRec) <> 0;
{$ifdef windows}
        FindClose(SearchRec.FindHandle);
{$else}
        FindClose(F);
{$endif}
    end;
end;

procedure TFormMain.WriteUserCurve(CurveType: Curve_type);
var XMLConfig: TXMLConfig;
begin
    Assert(Assigned(CurveType));

    CurveType.FFileName := GetConfigDir +
        IntToStr(QWord(TimeStampToMSecs(DateTimeToTimeStamp(Now)))) + '.cpr';
    XMLConfig := TXMLConfig.Create(CurveType.FFileName);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', CurveType);
        XMLConfig.Flush;
    finally
        XMLConfig.Free;
    end;
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
            try
                ReadComponentFromXMLConfig(XMLConfig, 'Component',
                    TComponent(FSettings), OnFindComponentClass, nil);
            except
                FSettings.Free; FSettings := nil;
                FSettings := Settings_v1.Create(nil);
            end;
        finally
            XMLConfig.Free;
        end;
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
        WriteComponentToXMLConfig(XMLConfig, 'Component', FSettings);
        XMLConfig.Flush;
    finally
        XMLConfig.Free;
    end;
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



