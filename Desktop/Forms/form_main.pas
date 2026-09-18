// SPDX-License-Identifier: GPL-3.0-or-later
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
    LCLIntf, SysUtils, DateUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
    StdCtrls, Menus, points_set, fit_viewer, ComCtrls, fit_client, NumericGrid,
    Buttons,
    CheckLst, mscr_specimen_list, LResources, TAGraph, ActnList, app_settings,
    series_style, series_register,
    Laz_XMLCfg, neutron_points_set, curve_points_set, gauss_points_set,
    asym_pseudo_voigt_points_set, doniach_sunjic_points_set, emg_points_set, lorentz_points_set, skewed_gaussian_points_set, step_points_set, voigt_points_set, moffat_points_set, pearson7_points_set,
    pseudo_voigt_points_set,
    int_fit_service, two_branches_pseudo_voigt_points_set, named_points_set,
    persistent_curve_parameters, main_calc_thread, log, pick_target

    , user_points_set, MyExceptions, http_fit_service, fit_statistics
    , fit_loss, loss_compatibility, fit_advice
    //  The class of FDeferred below, so it belongs in this half of the clause.
    , deferred_ui
    //  The two weighting names, and what an unrecognised one means.
    , fit_weighting
    , module_view_types, curve_type_registration
    //  A module's report: its verdicts as data, how they are drawn, and which
    //  modules get a tab for them.
    , module_report_types, report_html, report_tabs, report_explanations
    //  What the chart area shows while a fit runs, decided in fit_progress.
    , fit_progress, client_explanations
    , data_loader_registration, minimizer_registry, minimizer_registration
    //  How a price-data file is read: the choice Data > Price Data makes.
    , ohlc_csv_loader
    , int_ui_host
    //  The self-explaining layer: what an explanation is, where one is found,
    //  which one the Explain pane shows, how it is drawn, and the entry that
    //  reports which one the pointer rests on.
    , explanation, explanation_registry, explanation_focus, explanation_html
    , curve_type_explanations, explained_menu_item, explain_everything_window
    //  The Explain pane's view, by its seam only - see int_explain_view.
    , int_explain_view
    , app_modules
    , app_version
    //  The project file: the document, what a command decides, where it is
    //  written and which one to offer at start-up.
    , fit_project_document, fit_project_session, fit_project_file
    , fit_project_provenance
    , project_commands, recent_project, project_workflow
    //  For TStringDynArray, which recent_project answers the list in.
    , Types
    , project_ui_context
    , int_project_host
    //  TSaveAnswer is named in the class declaration - the window is asked
    //  the question and reports the answer, it does not decide what the
    //  answer means.
    , close_query
    , checks
    , argument_axis
    , ui_menus
    //  The two menu conventions the LCL leaves to the application on macOS;
    //  an empty body everywhere else.
    , mac_menu
    , curve_list_grid
    //  What the Model panel shows when the framework fills it, and which of the
    //  two contributors that is.
    , model_outline
    , tool_pane_layout
    //  The list of curves the client holds, and the handle on an attributes
    //  row - the two halves the Model panel pairs by index.
    , self_copied_component
    , curve_instance_id
    //  Which commands the window offers and which are ticked. The decision;
    //  this unit reads the inputs and writes the answer onto the widgets.
    , action_state
    //  WHICH WIDGET SAYS SO, and what each command is captioned in a panel too
    //  narrow for the menu's wording. The mapping used to be forty hand-written
    //  lines here; it is a table now, and this unit loops over it.
    , ui_commands
    //  Which pairs of widget readings disagree, in the words the log carries.
    , ui_selfcheck
    //  A component name made from data that the widget set will accept: the
    //  generated buttons and menu entries take their names from curve types and
    //  command ids, and one of the shipped types begins with a digit.
    , ui_names
    //  What the user is told next while picking, and when a gesture is over.
    , pick_guidance
    , chart_panning
    //  How a parameter is treated, in the terms the user reads.
    , parameter_kinds
    //  The tree a module's flattened outline describes.
    , outline_layout
    //  What editing a cell of the profile table means.
    , grid_edit
    //  How a table leaves this program as text.
    , table_export, table_text, points_tables
    //  How the curve-type menu is laid out.
    , curve_type_menu
    //  The menu a module's declarations describe.
    , module_menu
    //  The user-defined argument axis: what it starts as, and when it is usable.
    , custom_axis
    //  Where the pieces of a legend row sit.
    , legend_layout
    //  The numbers along the bottom of the window.
    , status_readout
    //  A number as a user typed it.
    , typed_number

{$IFDEF windows}
    , Windows
{$ENDIF}
    ;

{ TResState moved to action_state, with the decision that reads it. TViewState
  is gone: "the chart is drawing something" is a boolean, and naming its two
  values added a type without adding a distinction. }

type
  { TFormMain }
  TFormMain = class(TForm, IUiHost, IProjectHost)
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
    ActionExportCurveParameters: TAction;
    ActionExportSummaryTable: TAction;
    ActionNewProject: TAction;
    ActionOpenProject: TAction;
    ActionSaveProject: TAction;
    ActionSaveProjectAs: TAction;
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
    LabelPositionCaption: TLabel;
    LabelIntensityCaption: TLabel;
    LabelData: TLabel;
    LabelGraphs: TLabel;
    LabelPositionValue: TLabel;
    LabelIntensityValue: TLabel;
    MainMenu: TMainMenu;
    MenuData: TMenuItem;
    MenuDoAllAutomatically: TMenuItem;
    MenuFile: TMenuItem;
    MenuSeparatorBeforeEntireProfile: TMenuItem;
    MenuLoadProfile: TMenuItem;
    MenuSeparatorBeforeRemovePositions: TMenuItem;
    MenuSeparatorBeforeMinimizers: TMenuItem;
    MenuSeparatorBeforeMaxRFactor: TMenuItem;
    MenuReload: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSeparatorBeforeQuit: TMenuItem;
    MenuSeparatorBeforeRange: TMenuItem;
    MenuSeparatorBeforeSelectAll: TMenuItem;
    MenuSeparatorBeforeViewMarkers: TMenuItem;
    MenuSeparatorBeforeRemoveBounds: TMenuItem;
    MenuMinimizeNumberOfCurves: TMenuItem;
    MenuMinimizeDifference: TMenuItem;
    MenuArgumentTransformation: TMenuItem;
    MenuCreateRule: TMenuItem;
    MenuBackgroundPoints: TMenuItem;
    MenuEnableCurveScaling: TMenuItem;
    MenuSeparatorMinimizer: TMenuItem;
    MenuComputeServer: TMenuItem;
    MenuMinimizer: TMenuItem;
    MenuWeighting: TMenuItem;
    MenuWeightingPoisson: TMenuItem;
    MenuWeightingNone: TMenuItem;
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
    PanelRightContent: TPanel;
    PanelRight: TPanel;
    MenuCurvePositions: TMenuItem;
    MenuSetWavelength: TMenuItem;
    ScrollBarX: TScrollBar;
    ScrollBarY: TScrollBar;
    SplitterChartRight: TSplitter;
    SplitterLeftChart: TSplitter;
    SplitterBottom: TSplitter;
    TabSheetCurvePositions: TTabSheet;
    TabSheetBackground: TTabSheet;
    TabSheetCurveAttributes: TTabSheet;
    TabSheetCurveIntervals: TTabSheet;
    TabSheetSummary: TTabSheet;
    Chart: TTAChart;
    ToolBarBackground: TToolBar;
    ToolBarCurveIntervals: TToolBar;
    ToolBarCurvePositions: TToolBar;
    ToolBarCurveAttributes: TToolBar;
    ToolBarSummary: TToolBar;
    ToolBarMain: TToolBar;
    ToolButtonComputeBackgroundPoints: TToolButton;
    ToolButtonCopyAttributes: TToolButton;
    ToolButtonExportSummaryTable: TToolButton;
    ToolButtonCopySummary: TToolButton;
    ToolButtonLoadProfile: TToolButton;
    ToolButtonZoomIn: TToolButton;
    ToolButtonZoomOut: TToolButton;
    ToolButtonDoAllAutomatically: TToolButton;
    ToolButtonSubtractBackgroundAutomatically: TToolButton;
    ToolButtonSubtractBackgroundBySelectedPoints: TToolButton;
    ToolButtonRemoveBackgroundPoints: TToolButton;
    ToolButtonComputeRFactorBounds: TToolButton;
    ToolButtonComputeCurvePositions: TToolButton;
    ToolButtonRemoveRFactorBounds: TToolButton;
    ToolButtonRemoveCurvePositions: TToolButton;
    ToolButtonExportCurveParameters: TToolButton;
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
    SelCurveLorentzian: TMenuItem;
    MenuExport: TMenuItem;
    MenuExportCurveParameters: TMenuItem;
    MenuExportSummaryTable: TMenuItem;
    MenuNewProject: TMenuItem;
    MenuOpenProject: TMenuItem;
    MenuSaveProject: TMenuItem;
    MenuSaveProjectAs: TMenuItem;
    MenuSeparatorAfterOpenProject: TMenuItem;
    MenuSeparatorAfterSaveProject: TMenuItem;
    MenuSeparatorBeforeExport: TMenuItem;
    ProjectOpenDialog: TOpenDialog;
    ProjectSaveDialog: TSaveDialog;
    MenuSelectCurveType: TMenuItem;
    MenuFit: TMenuItem;
    MenuSetMaximumRFactor: TMenuItem;
    MenuGlossary: TMenuItem;
    MenuModel: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    MenuView: TMenuItem;
    MenuOpenRecent: TMenuItem;
    MenuZoomIn: TMenuItem;
    MenuZoomOut: TMenuItem;
    ImageListMenuIcons: TImageList;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    PopupViewMode: TPopupMenu;
    PopupMenuTheta: TMenuItem;
    PopupMenuN2Theta: TMenuItem;
    PopupMenuSinThetaLambda: TMenuItem;
    ImageListToolbars: TImageList;
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
    procedure MenuMinimizerClick(Sender: TObject);
    procedure BuildMinimizerMenu;
    { Ticks the selected engine and unticks the rest. }
    procedure MarkSelectedMinimizer(AKind: longint);
    procedure MenuWeightingPoissonClick(Sender: TObject);
    procedure MenuWeightingNoneClick(Sender: TObject);
    procedure MenuComputeServerClick(Sender: TObject);
    procedure MenuComputeBackendsClick(Sender: TObject);
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
    procedure ActionExportCurveParametersExecute(Sender: TObject);
    procedure ActionExportSummaryTableExecute(Sender: TObject);
    procedure ActionNewProjectExecute(Sender: TObject);
    procedure ActionOpenProjectExecute(Sender: TObject);
    procedure RecentProjectClick(Sender: TObject);
    procedure ActionSaveProjectExecute(Sender: TObject);
    procedure ActionSaveProjectAsExecute(Sender: TObject);
    procedure ActionSelectDataIntervalExecute(Sender: TObject);
    procedure ActionSelectIntervalBoundsExecute(Sender: TObject);
    procedure ActionComputeBackgroundPointsExecute(Sender: TObject);
    procedure ActionSelectBackgroundManuallyExecute(Sender: TObject);
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
    procedure CheckListBoxLegendDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure PageControlChange(Sender: TObject);
    { The polling itself; the timer handler wraps it in error reporting. }
    procedure CheckState;
    { Asks for CheckState to run once the current event has unwound - see
      deferred_ui for why events, not a timer, ask. }
    procedure RequestStateRefresh;
    procedure RunStateRefresh(Data: PtrInt);
    { Takes up deferred work that had to wait for an open menu. }
    procedure DeferredUiIdle(Sender: TObject; var Done: Boolean);
    procedure StateChangedByViewer(Sender: TObject);
    procedure ActionListExecuteAny(AAction: TBasicAction; var Handled: Boolean);
    procedure ActiveControlChanged(Sender: TObject);
    procedure GridSelectionChanged(Sender: TObject; aCol, aRow: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure GridDataEditingDone(Sender: TObject);
    procedure GridPicksEditingDone(Sender: TObject);
    procedure GridParametersEditingDone(Sender: TObject);
    procedure GridPicksSelectEditor(Sender: TObject; aCol, aRow: Integer;
        var Editor: TWinControl);
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
    procedure TabSheetSummaryShow(Sender: TObject);
    procedure TabSheetCurveIntervalsShow(Sender: TObject);
    procedure TabSheetCurveAttributesShow(Sender: TObject);
    procedure TabSheetCurvePositionsShow(Sender: TObject);
    procedure CheckListBoxLegendClickCheck(Sender: TObject);
    procedure MenuSinThetaLambdaClick(Sender: TObject);
    procedure MenuThetaClick(Sender: TObject);
    procedure MenuN2ThetaClick(Sender: TObject);
    procedure MenuIdentityClick(Sender: TObject);
    procedure MenuCurveAxisClick(Sender: TObject);
    procedure MenuCustomAxisClick(Sender: TObject);
    { Shown (on the UI thread) when a calculation fails; see main_calc_thread. }
    procedure ShowCalcError(const AMessage: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuSetWavelengthClick(Sender: TObject);
    { Modal dialog to enter a user-defined axis (name, unit, forward + inverse
      formulas of x). Returns True when confirmed. Built in code (no resource). }
    function ShowCustomAxisDialog(var AName, AUnit, AForward,
        AInverse: string): boolean;

  protected
    { What the window owes the user and has not been able to do yet: a message
      raised where a dialog could not be opened, a menu rebuild asked for by the
      menu item being clicked.

      FOUR FLAGS AND THE RULES THAT READ THEM used to live here and in five
      methods. deferred_ui holds both now - one message at a time whatever the
      fault does, one rebuild however many clicks ask, nothing at all while a
      menu is open, and the rebuild before the message - and being an ordinary
      object it can be asked those questions by a test. }
    FDeferred: TDeferredUi;
    { 'From Curve Type' axis menu items, created at run time (one per host menu). }
    FMenuCurveAxis, FMenuCurveAxisPopup: TMenuItem;
    { 'General Position' axis menu items, created at run time (one per host menu). }
    FMenuIdentity, FMenuIdentityPopup: TMenuItem;
    { 'Custom Position...' axis menu items, created at run time. }
    FMenuCustom, FMenuCustomPopup: TMenuItem;
    { Last user-defined axis definition (remembered between invocations). }
    FCustomAxisName, FCustomAxisUnit, FCustomAxisForward, FCustomAxisInverse: string;
    { True once the user has picked an axis from the menu. Until then the axis is
      the one the selected curve type defines (XCM_CURVE), so a persisted
      ViewMode written before that mode existed does not pin the user to 2*Theta. }
    FAxisModeChosenByUser: boolean;
    { Selected minimizer algorithm (MIN_KIND_* constant), persisted in settings. }
    { Created in FormCreate rather than in the .lfm, like the argument-axis
      items: additive, and it keeps the designed menu untouched (D1). }
    { The engine items built from the registry, so the checked state and the
      greying can be updated without searching the menu by caption. }
    FMinimizerItems: TList;
    { 'Undo Wave Detection'. Held so it can be enabled only when there is
      something to undo - an item that is always enabled and sometimes says "no"
      teaches the user to distrust the menu. }
    FMenuUndoWaveDetection: TMenuItem;
    { The pattern selected in the outline, by GUID, or empty for none. Held as
      an IDENTITY rather than a row index: rows are rebuilt on every overlay
      refresh, and an index would silently come to mean a different pattern. }
    { The panel row the user last selected, remembered only so a rebuild can
      restore it. What the row MEANS is the module's business. }
    FSelectedRowId: string;
    { GUIDs of the outline rows, in row order. Rebuilt in the SAME pass that
      fills the tree, so the two cannot drift; a node carries its position in
      this list rather than an allocated string, which would have to be freed by
      hand on every refresh. }
    FOutlineGuids: TStringList;
    { The right panel's tabs, created at run time: the graph legend on one sheet,
      the wave-count outline on the other.

      A chart can only IMPLY nesting - a nested count draws as more pivots, and
      which pattern refines which leg is not readable from it at all. That
      hierarchy is what this pack adds, so it needs somewhere to be seen.

      Beside the legend rather than beside the CHART, which is where it started:
      docked on PanelChart it cost a fixed slice of the plot for the whole
      session, and read as a second legend saying the same words - for a single
      unlabelled pattern the outline caption and the legend title are literally
      the same string, both being GetCurveTypeName. As a tab it costs nothing
      when it is not the one in front. }
    FRightTabs: TPageControl;
    FTabGraphs: TTabSheet;
    { The tab and tree a module's panel is drawn in, and which module's panel
      it is. One today; named for the mechanism rather than for the pack, so a
      second needs no new vocabulary. }
    FModuleTree: TTreeView;
    FModulePanelId: string;
    { The point set a module's pick mode is currently collecting into, or empty
      when no module gesture is active. }
    FModulePickSet: string;
    { The module's own toggle entry for that mode, so its check mark can be put
      back when the mode ends - which often happens without the module being
      told: another selection mode starts, a profile is loaded. Kept beside the
      point set because the two are one fact about one gesture. }
    FModulePickMenuId: string;
    { How many picks make one of whatever the module is collecting; 0 when the
      gesture has no natural end. Declared by the module when it asks for the
      picks - the window cannot know what a pair of them means. }
    FModulePicksPerGesture: longint;
    { Menu entry ids a module declared, mapped to the items built for them. }
    FModuleMenuItems: TStringList;
    { EVERY COMMAND THIS WINDOW OFFERS, declared once in ui_commands and
      rendered by every surface that shows one. The menus, the Tools pane and
      the Model panel's context menu all read this. }
    FCommands: TCommandTable;
    { The component each row drives, resolved once from the name the table
      gives. Resolved once rather than per poll: the state is applied twice a
      second, and FindComponent is a linear scan over two hundred
      components. }
    FCommandTargets: array of TComponent;
    { Deleting one curve. Created here rather than in the .lfm for the same
      reason the module menus and the axis entries are: additive, and it keeps
      the designed form untouched. }
    FActionDeleteCurve: TAction;
    { Removing every curve at once, from the Model menu. Runtime for the same
      reason as the action above. }
    FActionClearModel: TAction;
    { The handle of the curve the Model panel has selected, or empty. THE
      HANDLE, not an index: the model's order is derived from the intervals and
      the picks inside them, so an index held across an edit names a different
      curve. }
    FSelectedCurveId: string;

    //  ---- the Tools pane, on the left. Built at runtime for the reason the
    //  module menus and the axis entries are: additive, and the designed form
    //  stays untouched.
    { The Model panel: the framework's, and there is one of it. It used to be a
      module's - existing only when one declared it, captioned in that module's
      vocabulary, and never more than one. }
    FTabModel: TTabSheet;
    { What can be done to the selected row, built from the same declarations
      the menus and the Tools pane are drawn from. }
    FModelPopup: TPopupMenu;
    { The context entries that are command-table rows, as distinct from the
      ones a module builds each time the menu opens. HELD APART because the
      table's rows are enabled by their Tag, which is a table index, and a
      module's entries carry a tag of their own meaning - applying the table's
      states to every item in the menu would overwrite what the module said. }
    FRowCommandItems: array of TMenuItem;
    { What a module added as the menu last opened: its top-level entries and
      the separator before them, freed and rebuilt on every opening. }
    FRowMenuItems: array of TMenuItem;
    FRowMenuNodes: TModuleMenuNodes;
    { Which module built them, and THE ROW THE MENU OPENED OVER - kept, because
      the selection can change while the menu is open (a poll rebuilds the
      tree), and a click must act on the row the user was looking at. }
    FRowMenuModule: longint;
    FRowMenuRowId: string;
    { The panel id the rows now shown were pushed under: the framework's own,
      or a module's. Decides whose context entries the menu asks for. }
    FShownPanelId: string;
    { A module's rows that arrived while another type was selected, by panel
      id - shown the moment one of that module's types is (see
      model_outline.ModulePanelRowsShown). Parallel arrays. }
    FHeldPanels: THeldPanelRows;
    { The Explain pane under the Model tree, and what is in focus for it. Which
      topic that makes it show is explanation_focus's decision. }
    FExplainPane: IExplainView;
    FExplainFocus: TExplanationFocus;
    { THE REPORT TABS, one per module that declared one, in the bottom strip
      after the designed tabs. Parallel arrays: FReportTabs[i] says whose tab
      FReportSheets[i] is, and FReportViews[i] draws it. }
    FReportTabs: TReportTabs;
    FReportSheets: array of TTabSheet;
    FReportViews: array of IExplainView;
    { WHAT A RUNNING FIT SHOWS IN THE CHART AREA, made at run time like the report
      tabs so the designed form is untouched: a line saying how the fit is going
      over the chart, a chart of the loss it has reached, and the timer that asks.
      Every decision - which of the two charts is shown, what the line says, what
      is plotted - is fit_progress's; these only put its answer on screen. }
    FProgressHeader: TLabel;
    FProgressChart: TTAChart;
    FProgressSerie: TTASerie;
    { Marks the loss chart's elapsed time in hours, minutes and seconds. }
    FProgressAxis: TDurationAxis;
    FProgressTimer: TTimer;
    { What this window drew while the last fit ran. Read by the self-check
      below, which is the only thing that asks. }
    FProgressFramesDrawn: longint;
    FMostLossPointsDrawn: longint;
    FProgressTimerTicks: longint;
    procedure BuildFitProgressView;
    procedure ProgressTimerTick(Sender: TObject);
  private
    { The rows now in the Model panel, as whoever filled it sent them.

      HELD BECAUSE THE ROW IS WHAT ANSWERS "which curve is selected?". This was
      answered by asking whether the framework or a contributor last filled the
      panel, and a contributor's row was taken to name no curve at all - so
      Delete curve was permanently greyed over every pattern an analysis pack
      had placed. A row says which curve it stands for; the tree control holds
      only ids, so the rows are kept beside it. }
    FShownRows: TOutline;
    FLeftTabs: TPageControl;
    FTabTools: TTabSheet;
    FTabData: TTabSheet;
    { The curve types as a flat list. A list rather than icons because thirteen
      peak shapes have no distinguishable glyphs, and a choice among named
      shapes is a list. }
    FCurveTypeList: TListBox;
    { What the list currently shows, so a click can be mapped back to the
      registry handle without asking the registry again. }
    FCurveRows: TCurveListRows;
    { Where the generated command buttons live. A scroll box, because how many
      rows there are depends on what the modules added. }
    FToolBox: TScrollBox;
    { One button per pane row, and the table row each one stands for. }
    FToolButtons: array of TSpeedButton;
    FToolRows: array of longint;
    { One heading per group, and the group each one names, so the counts can be
      written back into it. }
    FToolHeadings: array of TLabel;
    FToolGroups: array of string;
    { Issues the names of widgets generated ONCE - the pane's buttons, the
      module menus, the axis entries. }
    FWidgetNames: TWidgetNames;
    { And a register of its own for the menus that are REBUILT.

      The curve-type menu is cleared and recreated on every selection change,
      which frees the old entries and so frees their names - but a register does
      not know that, and would suffix the same name one higher on every rebuild:
      MenuCurveTypeGaussian, then ...2, then ...3, for as long as the session
      lasts. Cleared where the menu is cleared, which is the only place that
      knows the old names have gone. }
    FMenuNames: TWidgetNames;
    FMinimizerKind: longint;
    FLossKind: longint;
    { The 'Loss function' submenu and its items, built at runtime from the
      fit_loss vocabulary rather than declared in the .lfm - so adding an
      objective adds its menu entry with no UI edit at all, and the caption and
      tooltip come from the one place that defines them. }
    FMenuLoss: TMenuItem;
    { Data > Price Data > Value and > Argument: how a price-data (.csv) file is
      read. The choice, as ordinals of ohlc_csv_loader's two enumerations. }
    FMenuPriceValue, FMenuPriceArgument: TMenuItem;
    { What a cell of a pick or parameter table held when typing into it began. }
    FSavedCellText: string;
    FPriceValueColumn, FPriceArgument: longint;
    { Status-bar panel that always states what the fit will ACTUALLY do. Added at
      runtime beside the existing panels, so no .lfm edit is needed. }
    FAdvicePanel: integer;
    { The last explanation shown in a dialog. The dialog appears when the reason
      CHANGES, not on every click - a message box that reappears on every menu
      selection is one people learn to dismiss without reading, which would cost
      us the times it matters. }
    FLastAdviceShown: string;
    { Residual weighting for the Python backend ('poisson'/'none'), persisted. }
    FWeighting: string;
    { Compute-server URL; empty = fit in-process. Persisted in settings. }
    FServerUrl: string;
    { The user-defined curve type currently selected (nil when a built-in type is
      selected). Needed for the menu check state because all user curves share
      one curve-type id. }
    FSelectedUserCurve: Curve_type;
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
    procedure ShowEditHintQueued(Data: PtrInt);

  protected
    { The object created event FEditDone. }
    FSenderEditHint: TNumericGrid;

    FHintMessage: string;
    FDrawReticule: Boolean;

    { Callback for calculating object. }
    procedure AsyncOperationFinished(Sender: TObject);
    { Wrapper. }
    procedure SubtractBackground(Auto: Boolean);

    { The captions and ticks that follow the picking mode. Everything else the
      mode decides is in action_state. }
    procedure ApplyPickingCaptions(ASelectionMode: TSelMode);
    procedure AimPickAtActiveSerie;
    { The module gesture is over: put its picks and its tick away. }
    procedure EndModulePicking;
    { True when the picks just made complete the module's gesture. }
    function ModuleGestureIsComplete: boolean;
    { The set the given picking mode collects into. }
    function PicksOfCurrentMode(AMode: TSelMode): TPointsSet;
    procedure UpdateBarsPos;
    { Sizes one of the tabbed tables to fill its panel, less a margin. The five
      tabs each used to do this inline with the same four literals; the margin
      is quoted once here, at 96 dpi, and converted for the display. }
    procedure InsetGridInPanel(AGrid: TControl; APanel: TWinControl);
    { Divides the status bar between its panels by what they have to hold.
      Bound to the bar's own OnResize, so it re-runs after the LCL has scaled
      the form and again whenever the window changes width. }
    procedure StatusBarResize(Sender: TObject);
    { Sizes the legend's rows to the font they are drawn in. Bound to the
      list's own OnResize, which is the first moment after the LCL has scaled
      both its bounds and its font. }
    procedure CheckListBoxLegendResize(Sender: TObject);
    { Sizes the parameters-table colour key to the font it is written in. }
    procedure SizeParameterLegend;
    { The event OnClick of Chart arises between MouseUp and MouseDown.
      That is why OnClick is not used. }
    procedure OnChartClick;
    function GetConfigFileName: string;
    procedure OnFindComponentClass(Reader: TReader;
        const ClassName: string; var ComponentClass: TComponentClass);
    procedure OnException(Sender: TObject; E: Exception);
    { Reports a failure without opening a dialog here and now: at most one dialog
      is shown, and it is shown from the main loop. See OnException for why that
      matters. }
    procedure QueueError(const AMessage: string);
    { The same treatment for a message that is not a failure - something the user
      has just done and needs explained. Queued for the second reason given in
      QueueCurveTypeMenuRebuild as well: a modal dialog runs its own message loop,
      which would run the queued menu rebuild while the click that caused it is
      still being dispatched. }
    procedure QueueNotice(const AMessage: string);
    { What QueueError and QueueNotice have in common. }
    { The widget set's dialog type for a framework notice kind. }
    function DialogTypeOf(AKind: TNoticeKind): TMsgDlgType;
    procedure QueueDialog(const AMessage: string; AKind: TMsgDlgType);
    { Shows the message left by QueueError. Runs from the main loop, never from
      the call that failed. }
    procedure ShowPendingError(Data: PtrInt);
    procedure DoEditHint;
    { Everything the registry says about the curve types, as plain records. }
    function GatherCurveTypes: TCurveTypeInfos;
    procedure CreateCurveTypeMenus;
    { Asks for the curve-type menus to be rebuilt from the main loop instead of
      here and now. The only form a rebuild may take when the caller is a menu
      item's own event handler - see RebuildCurveTypeMenus. }
    procedure QueueCurveTypeMenuRebuild;
    { Rebuilds the curve-type menus. Runs from the main loop, never from inside
      the click being handled. }
    procedure RebuildCurveTypeMenus(Data: PtrInt);
    { Records how long a chart repaint took. Assigned to Chart.OnPaintTiming. }
    procedure ChartPaintTiming(ADurationMs: Int64; const ADetail: string);


    procedure OnDeleteUserCurveClick(Sender: TObject);
    procedure OnUserCurveClick(Sender: TObject);


  protected
    { THE TOOLS PANE IS LAID OUT AGAIN WHENEVER THE WINDOW IS RESCALED - to the
      display at start-up (a form designed at one ppi is laid out at another)
      and again when it moves to a monitor of another density. The pane is sized
      from measured captions, and a width measured in one font is wrong in the
      next: at 107 ppi designed and 96 laid out, "Mark Bounds" was left on an
      83 px button its 80 px caption no longer fitted with padding. }
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
        const AXProportion, AYProportion: double); override;
  public
    { Application settings. Type should be checked. }
    FSettings: Settings_v1;
    { Where the open project lives, or empty when it has never been
      saved - which is what makes Save behave as Save As the first time. }
    { The document commands, and where the open one lives. }
    FProjectFlow: TProjectWorkflow;
    { Where the data came from, described when it was loaded. Recorded at load
      rather than at save because by then the file may be gone - and "we came
      from there and it is not there now" is exactly what a user needs told. }

    FFitViewer: TFitViewer;
    { Index of curve on which the first click was. It is used in the cases when points of only one curve can be selected. }
    FActiveNumber: LongInt;
    { Collection should be passive. Object is set from TFitViewer and is checked on Nil. }
    FCurveList: TMSCRCurveList;
    {  Puts FCurveList into GridParameters. A wrapper rather than something
       FCurveList inherits: the list is engine-side data and must not carry a
       widget-set dependency into the compute server. See curve_list_grid. }
    FCurveGrid: TCurveListGrid;
    { Names the colours used in the parameters table. Built in code rather than
      on the form, because it is generated from ONE list of kinds - the same
      list the cell colouring reads - so a colour can never appear in the table
      without appearing in the key. }
    FParameterLegend: TPanel;
    { Indicates that MenuData in tables were changed. }
    FModifiedParameters: Boolean;
    FModifiedDatasheet: Boolean;
    { Index of a serie point of which is selected at the moment. }
    FCurSerieIndex: LongInt;
    { Index of selected value. }
    FValueIndex: LongInt;


    { Explains what a parameter's colour means, and creates FParameterLegend. }
    procedure BuildParameterLegend;
    { Colours a cell of the parameters table by what KIND of parameter it
      holds. }
    procedure GridParametersGetCellColor(Sender: TObject;
        ColNum, RowNum: longint; var CellColor: TColor);

    procedure ReadUserCurves;
    procedure WriteUserCurve(CurveType: Curve_type);
    { The user curve created last - the one TConfigurableUserPointsSet has just
      added to the settings when its dialog is confirmed. nil, with a warning in
      the log, if there is none: the caller has just been told one was created. }
    function LastUserCurve: Curve_type;
    { The "User" submenu of the Curve Type menu - the one group that is filled
      from the settings and not from the curve-type registry. Only valid after
      CreateCurveTypeMenus has run, which is what creates it. }
    function UserCurveMenu: TMenuItem;
    { Creates all menu items corresponding to user defined curves. }
    procedure CreateUserCurveMenus;
    { Adds menu item corresponding to user defined curve. }
    procedure AddUserCurveMenu(ct: Curve_type);
    procedure DeleteUserCurve(ct: Curve_type);


    { Tries read the user settings object. In the case of failure creates new object. }
    procedure ReadSettings;
    procedure WriteSettings;
    { Applies the persisted argument axis (FSettings.ViewMode) to the viewer + menus. }
    procedure RestoreViewMode;
    { The wavelength the compute server knows about, or 0 before there is a
      client to ask - which is what UsableViewMode reads as "none known". }
    function FitClientWaveLength: double;
    { Central axis switch: sets the viewer/grid mode, menu check state, and gates
      the wavelength control (meaningless on the general 'Position' axis). }
    procedure ApplyViewMode(Mode: longint);
    { Re-applies the current mode after the selected curve type changed, so the
      caption and the grid follow the model while the axis comes from it. }
    procedure RefreshAxisForSelectedCurveType;
    { True when the axis in force for Mode belongs to the diffraction family. }
    function CurrentAxisIsDiffraction(Mode: longint): boolean;
    { The wavelength known to the client, or 0 when there is no client yet. }
    function CurrentWaveLength: double;
    { Labels the chart x-axis from the current IArgumentAxis (name + unit). }
    procedure UpdateAxisLabel(Mode: longint);
    { Central minimizer switch: sets the server-side algorithm, menu check state
      and the persisted field. AKind is a MIN_KIND_* constant. }
    procedure ApplyMinimizerKind(AKind: longint);
    procedure ApplyLossKind(AKind: longint);
    procedure BuildLossMenu;
    procedure BuildPriceDataMenu;
    procedure ApplyPriceData(AValueColumn, AArgument: longint);
    { Builds every registered module's menu under Model, from what each module
      DECLARES. The window owns where they hang; a module owns its captions,
      its hints and what its entries do. }
    procedure BuildModuleMenus;
    { Declares up front every menu entry that can be ticked later, so ticking
      one never rebuilds the widget under an open menu. See the body. }
    procedure DeclareCheckableMenuEntries;
    procedure ModuleMenuClick(Sender: TObject);
    { The one path a module command runs by, from either surface. }
    procedure RunModuleCommand(ARow: longint);
    { The module entry a menu item stands for, by the id it was built with. }
    function ModuleItemById(const AId: string): TMenuItem;
    { Splits the right panel into the legend tab and a tab per module. }
    { Builds the command table and resolves every row's target.

      A NAME THAT RESOLVES TO NOTHING IS REFUSED HERE, at start-up, rather than
      producing a widget that never enables. That is the whole reason the table
      names components instead of holding references: a typo is then a failing
      test and a loud start-up, not a dead button. }
    { Splits the left panel into Tools and Data, and fills the Tools tab from
      the command table. }
    procedure ApplyRowCommandStates;
    { The table refreshed and the entries brought into line with it - what the
      context menu does as it opens, and what the self-check has to do to read
      the same answer. }
    procedure RefreshRowCommands;
    { The curve the Model panel has selected, and the chart told of it.
      THE ONE WAY FSelectedCurveId CHANGES, so a selection cannot move - a
      row clicked, a project restored, a curve deleted - without the
      highlight on the chart moving with it. }
    procedure SetSelectedCurveId(const AId: string);
    { The chart's highlight as it is drawn right now, read off every series
      the viewer registered, for the self-check. }
    function ReadHighlight(AExpectSelection: boolean): THighlightReading;
    { Logs a reading's findings and its verdict. }
    procedure ReportHighlight(const AReading: THighlightReading;
        const AWhen: string);
    { The curve a row of the Curve Attributes table stands for becomes the
      selected curve, and the Model panel selects the row standing for it.
      ARow is zero-based within the data. }
    procedure SelectCurveOfTableRow(ARow: longint);
    { Whether the context menu currently offers anything at all. }
    function AnyContextEntryEnabled: boolean;
    procedure ModelRowCommandClick(Sender: TObject);
    { Fills the Model panel from the framework's own model. }
    procedure RefreshModelStructure;
    { PlacedByPointSet of the selected curve type - empty for a type placed by
      one pick per curve. What decides which contributor fills the Model
      panel. }
    function SelectedPlacedByPointSet: string;
    procedure BuildLeftPanelTabs;
    { Sizes the Tools pane to its captions and puts each heading and button
      where tool_pane_layout says, in the fonts they are drawn in now. }
    procedure LayoutToolPane;
    { Writes the polled state into the generated buttons and headings. }
    procedure RefreshToolPane;
        { Logs every way the pane and the menus disagree about the commands
          they were both generated from. Called by the self-check switch. }
    procedure ReportSurfaceDisagreements;
        { Whether the Model panel's context menu can ever be used: builds the
          smallest model with curves in it, selects a row and asks. }
    procedure ReportRowCommandReachability;
        { Whether every legend row still names the series it carries. }
    procedure ReportLegendPairing;
    procedure ReportLiveFitProgress;
        { Every pane row as the widgets currently show it. }
    function SurfaceRowsNow: TSurfaceRows;
        { Everything the command decision is made from, gathered whole. }
    function GatherUiInputs: TUiInputs;
        { How many positions, intervals and background points the model holds. }
    function CurrentModelCounts: TModelCounts;
        { Brings the Model panel's context entries up to date as it opens. }
    procedure ModelPopupPopup(Sender: TObject);
    { The Model panel's context menu closed: the pane stops following it. }
    procedure ModelPopupClose(Sender: TObject);
    { Replaces the entries a module contributes over the selected row. }
    procedure RebuildRowMenu;
    procedure RowMenuItemClick(Sender: TObject);
    procedure RowMenuTopicHovered(const ATopic: string);
    { Draws whatever explanation_focus says is in focus. }
    procedure RefreshExplainPane;
    procedure ExplainPaneLinkFollowed(const AHref: string);
    { The topic of the curve type the model is using, or ''. }
    function SelectedCurveTypeTopic: string;
    procedure ExplainEverythingExecute(Sender: TObject);
    { Refills the curve-type list from the same entries the menu is built
      from. }
    procedure RefreshCurveTypeList(const AEntries: TCurveMenuEntries);
    procedure ToolButtonClick(Sender: TObject);
    procedure CurveTypeListClick(Sender: TObject);
    procedure CurveTypeListShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure SelectCurveTypeByTag(ATag: longint);
    procedure ActionDeleteCurveExecute(Sender: TObject);
    procedure ActionClearModelExecute(Sender: TObject);
    procedure BuildCommandTable;
    { Applies the polled state to every row's widget. }
    procedure ApplyCommandStates;
    procedure BuildRightPanelTabs;
    { One hidden tab per module that declares a report, appended after the
      designed bottom tabs - see report_tabs. }
    procedure BuildReportTabs;
    { A link in a report: a rule opens in the Explain pane, a row is selected
      in the Model panel, anything else opens outside. }
    procedure ReportLinkFollowed(const AHref: string);
    { Selects the Model-panel row with this id and brings the panel forward;
      False when no row of the panel has it. }
    function SelectModelRowById(const ARowId: string): boolean;
    { Restores the persisted curve type, if this build still has it. }
    procedure RestoreCurveType;
    procedure ModulePanelSelectionChanged(Sender: TObject);
    procedure MenuLossClick(Sender: TObject);
    procedure MenuPriceValueClick(Sender: TObject);
    procedure MenuPriceArgumentClick(Sender: TObject);
    procedure UpdateLossAvailability;
    procedure UpdateFitAdvice(AAnnounce: boolean);
    { Enables/disables the Python engine for the selected curve type. }
    procedure UpdateMinimizerAvailability;
    { Central weighting switch: sets the server-side weighting, menu check state
      and the persisted field. AValue is 'poisson' or 'none'. }
    procedure ApplyWeighting(const AValue: string);
    { Points the client at a compute server (empty = fit in-process). }
    procedure ApplyServerUrl(const AUrl: string);
    { Searches files containing parameters of user defined curves and loads them. }

    { Creates single menu item. }
    { ATickable declares the entry checkable before it has a handle, which is
      what an entry whose tick moves with the selection must be - see
      DeclareCheckableMenuEntries. The entries under 'Delete' are not ticked and
      must not reserve a box. }
    procedure CreateMenuItem(Pos: LongInt; ct: Curve_type;
        ParentMenu: TMenuItem; OnClick: TNotifyEvent; ATickable: boolean);

    procedure CheckListBoxChanged;
    { Saving curve parameters into text file. }
    function SaveTableAsText(const ARows: TTextRows): Boolean;
    { The three things the export conversation needs of this window. The
      conversation itself is in table_export. }
    function AskExportPath(out APath: string): boolean;
    function AskExportQuestion(AQuestion: TExportQuestion;
        const APath: string): TExportAnswer;
    function ExportPathExists(const APath: string): boolean;
    { WHAT THE DOCUMENT COMMANDS NEED OF THIS WINDOW, and nothing more -
      see int_project_host. Each of these opens a dialog, shows a message
      or writes a caption; the sequences they belong to are in
      project_workflow, where a test can reach them. They were methods of
      this form until the coverage gate reported what that cost: eighty-one
      lines of decision inside the excluded wrapper, which is eighty-one
      lines that stopped being counted. }
    function AskProjectToOpen(out APath: string): boolean;
    function AskProjectToSaveAs(const ASuggested: string;
        out APath: string): boolean;
    function AskSaveBeforeClosing(const AWhat: string): TSaveAnswer;
    function Confirm(const AQuestion: string): boolean;
    procedure ReportProblem(const AMessage: string);
    procedure ShowDocument(const APath: string);
    procedure RefreshFromEngine;
    procedure ClearEverything;
    procedure LoadProfile(const APath: string);
    function CurrentContext: TProjectClientContext;
    procedure ApplyWorkingContext(const APlan: TProjectUiPlan);
    function TabCount: longint;
    function ModelHoldsCurve(const AHandle: string): boolean;
    function HasUnsavedWork: boolean;
    procedure MarkSaved;

    procedure ShowHint(const Hint: string);
    { Fills the outline panel, showing it when there is a count and hiding it
      when there is not. Rows arrive flattened and captioned. }
    procedure ShowModulePanel(const APanelId: string; const ARows: TOutline);
    procedure ShowModuleReport(const AReport: TModuleReport;
        ABringForward: boolean);
        virtual;
    { IUiHost - what the window can do for a module. }
    procedure ShowMessage(const ATitle, AText: string; AKind: TUiMessageKind);
    function Confirm(const ATitle, AText: string): boolean;
    function AskText(const ATitle, APrompt: string; var AValue: string): boolean;
    procedure BeginPointPicking(const APointSet, AMenuId: string;
        APicksPerGesture: longint; const AHint: string);
    procedure SetMenuEnabled(const AId: string; AEnabled: boolean);
    procedure SetMenuChecked(const AId: string; AChecked: boolean);
    procedure ShowExplanation(const ATopic: string);
    procedure ShowTime;
    { Displays reduced chi-squared and R^2 for the finished fit. }
    procedure ShowStatistics;
    procedure ShowRFactor;
    { The chart area while a fit runs, and back to the model when it is over. }
    procedure ShowFitProgress(const AView: TFitProgressView);
    procedure HideFitProgress;
    procedure ProgressXMarks(AMin, AMax: double; var AStart, AStep: double;
        var AHandled: boolean);
    function ProgressXMarkText(AValue, AStep: double): string;

    procedure LoadDataFile(FileName: string);
    { Fills File > Open Recent from the settings. }
    procedure RefreshRecentMenu;
    { The project offered at start-up, as the settings remember it. }
    function LastProjectFile: string;
    { Stop offering the project last open - it is gone, or it would not open.
      startup_sequence decides when; this only forgets. }
    procedure ForgetLastProject;
    { The document commands. Public because the program file opens a project
      before the window has been shown, exactly as it loads a data file - and a
      property rather than a forwarding method, because a forwarder here is a
      line in the group whose total may only shrink. }
    property ProjectFlow: TProjectWorkflow read FProjectFlow;

    property HandleEditHint: Boolean read FHandleEditHint
        write SetHandleEditHint;
  end;

var
    FormMain: TFormMain;

const
    crCursorDrag:       TCursor = 6;
    crCursorSelect:     TCursor = 7;

const

    { The prompts that guide a picking gesture moved to pick_guidance, with the
      rule that chooses them: a prompt and the pick count it belongs to are one
      fact, and they were three hundred lines apart. HintFirst is still named
      here because five menu handlers open a gesture with it. }
    HintFirstStart:     string = 'Now you can pick a first point - "START"';
    HintMain:           string =
        'Drag mouse from top-left to bottom-right to zoom';
    HintWait:           string = 'Calculation started. Please wait';

    MenuDelUserCapt:    string = 'Delete User Curve';
    { The generic user-defined type is a FAMILY of curves, not one curve: its
      menu item asks for a name and a formula and creates a curve. It is named
      after that action, and never carries a check mark - what gets selected is
      the curve it creates, which has an item of its own. }
    MenuNewUserCapt:    string = 'New User Curve...';

    { The Curve Type menu is split into groups. A module's group name comes from
      the model (TNamedPointsSet.GetCurveTypeGroup); these two are the UI's own
      names for "everything ungrouped" and "the user's own curves", so that the
      menu has a few entries instead of one flat list of twenty. }
    { The group names moved to curve_type_menu, with the rule that assigns
      them; four other places in this unit still name UserCurveGroup, and get it
      from there. }


implementation

uses
    input_wavelength_dialog, set_maximum_rfactor_dialog, input_back_factor_dialog,
    about_box_dialog, app, int_curve_type_iterator, int_curve_type_selector,
    curve_types_singleton, curve_type_choice, configurable_points_set,
    user_curve_library, server_connection,
    int_curve_factory, client_log,
    special_curve_parameter,
    //  WHICH PARAMETER PLACES A CURVE, asked in the one place that already
    //  answers it - both position types, one role wearing two hats.
    parameter_roles,
    //  Model > Clear Model: the confirmation and the removal, where a test can
    //  reach them.
    model_clearing,
    Themes, InterfaceBase, LCLPlatformDef;


{$hints off}
{ THE TICK BOX IS AS WIDE AS THE ONE THE WIDGET SET LISTENS ON.

  The legend draws its own tick box over the row's check column, and only the
  widget set's own check area toggles the item: gtk2 builds the list as a
  GtkTreeView whose first column holds a GtkCellRendererToggle - the toggle
  fires for a click inside THAT column, which is the theme's indicator size plus
  the cell padding, and nothing else in the row toggles anything. Qt tests the
  same thing by hand, against QStylePM_IndicatorWidth.

  The box used to be the row's height, and the row was 13 pixels, which is the
  gtk2 indicator size - so the box happened to sit inside the column and every
  click on it landed. Then the rows started being measured against the desktop's
  font and grew to about 35: the box grew with them, the toggle column did not
  (a gtk2 indicator is a theme metric, not a font one), and a click on the
  right-hand two thirds of a box the user can plainly see did nothing at all.

  So the size is asked of the theme rather than taken from the row. That is the
  same metric both widget sets derive their hit area from, so what is drawn is
  what can be clicked. Clamped to the row, and falling back to the quoted 13 at
  96 dpi if the theme answers nothing. }
{ True where an owner-drawn row is left without a check mark, so the legend has
  to draw its own. Which is nearly everywhere: OWNER-DRAWN MEANS THE APPLICATION
  PAINTS THE ROW, and two of the three widget sets take that literally.

  win32 says it in as many words: with an owner-draw style the draw message is
  translated to LM_DRAWLISTITEM, and then "we don't get to draw the checkmark
  and the CLB looks like a regular list" (win32wschecklst.pp).

  Qt does the same thing without saying so. TQtListWidget.ItemDelegatePaint hands
  the whole item to the LCL as LM_DRAWLISTITEM and paints nothing of its own -
  only the check STATE survives, as odChecked in the draw struct. A build that
  drew no box on Qt showed rows with a title and a colour swatch and no check box
  at all.

  gtk2 is the exception, and the reason is structural rather than a decision: its
  list is a GtkTreeView whose first column is a GtkCellRendererToggle, a widget of
  its own that paints whatever the rest of the row does. A box drawn here lands
  beside that one and the row wears two.

  So the test is for the gtk family, not for win32. }
function LegendMustDrawCheck: boolean;
begin
    Result := not (WidgetSet.LCLPlatform in [lpGtk, lpGtk2, lpGtk3]);
end;

function LegendCheckSize(LB: TCheckListBox; ARowHeight: integer): integer;
begin
    //  Asking the theme and scaling for the display are this unit's business;
    //  the fallback and the clamp are in legend_layout, where they are tested.
    Result := LegendCheckSizeFor(
        ThemeServices.GetDetailSize(
            ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)).cx,
        LB.Scale96ToFont(DefaultCheckSize96),
        ARowHeight);
end;

procedure TFormMain.CheckListBoxLegendDrawItem(
    Control: TWinControl; Index: Integer;
    ARect: TRect; State: TOwnerDrawState);
var LB: TCheckListBox;
    TS: TTASerie;
    Size: Longint;
    Color: TColor;
    Square: TRect;
    Inset, TextGap, CheckSize: integer;
    DrawnCheck: boolean;
begin
    CheckAssigned(Control, 'the legend list box being drawn');
    LB := TCheckListBox(Control);
    CheckIndex(Index, LB.Items.Count, 'the legend rows being drawn');

    //  THE SERIES THE ROW IS FOR, not the series at the row's position.
    //
    //  This read Chart.GetSerie(Index), which assumes the legend and the chart
    //  are index-parallel. They are not, and cannot be: AddSerieToChart appends
    //  the chart series ALWAYS and the legend row only `if FUpdateLegends`, and
    //  UpdateComputedData clears that flag for the redraws during a running fit
    //  (ShowCurMin). So after the first fit every row below the first curve
    //  drew another series' colour and its tick, and the CheckIndex against
    //  SeriesCount could not see it - it raises only when the legend is LONGER
    //  than the chart, never when the mapping is merely wrong.
    //
    //  AddSerieToChart stores the series as the row's object, so the pairing was
    //  always available; nothing was using it. CheckListBoxChanged, which
    //  applies the ticks, has read it correctly all along - which is why
    //  clicking a row worked while the row it drew was wrong.
    TS := nil;
    if LB.Items.Objects[Index] is TTASerie then
        TS := TTASerie(LB.Items.Objects[Index]);
    if not Assigned(TS) then
        //  A row whose object is not a series is the empty-panel placeholder or
        //  a row left behind by a removal. Drawn as its text alone rather than
        //  faulting: the legend is repainted from a poll, so a fault here is a
        //  dialog the user cannot dismiss.
        Exit;
    //  Stated rather than assumed: the row is measured against this font in
    //  CheckListBoxLegendResize, so it has to be the one drawn with here.
    LB.Canvas.Font.Assign(LB.Font);
    Size := ARect.Bottom - ARect.Top;
    Color := LB.Canvas.Brush.Color;
    //  The two swatches take their size from the row, which the LCL has already
    //  scaled. These two do not come from anywhere - they were written as device
    //  pixels, so on a scaled display the tick shrank inside its box and the
    //  title crowded the colour swatch. Quoted at 96 dpi, converted here.
    Inset   := LB.Scale96ToFont(2);
    TextGap := LB.Scale96ToFont(6);
    CheckSize := LegendCheckSize(LB, Size);

    //if (LB.ItemIndex <> Index) or (LB.ItemIndex = -1) then
        LB.Canvas.Brush.Color := LB.Color;

    Inc(ARect.Bottom);  //  !!! nuzhno !!!
    Inc(ARect.Bottom);  //  !!! nuzhno !!!
    //  ochistka
    LB.Canvas.FillRect(ARect);
    LB.Canvas.Brush.Color := LB.Color;

    //  mesto pod galku - only where the widget set draws none; see
    //  LegendMustDrawCheck. CheckSize, not the row height: the box has to sit
    //  inside the area the widget set toggles on - see LegendCheckSize - and it
    //  is centred in a row that is now taller than a check box.
    DrawnCheck := LegendMustDrawCheck;
    if DrawnCheck then
    begin
        Square.Top := CenteredBoxTop(ARect.Top, Size, CheckSize);
        Square.Bottom := Square.Top + CheckSize;
        Square.Left := 1;
        Square.Right := Square.Left + CheckSize;
        LB.Canvas.Rectangle(Square);

        //  zapolnenie
        Inc(Square.Left);
        Inc(Square.Top);
        LB.Canvas.FillRect(Square);

        //  marker vybrannogo elementa
        if LB.Checked[Index] then
        begin
            //  zapolnenie
            Square.Left := Square.Left + Inset;
            Square.Top := Square.Top + Inset;
            Square.Right := Square.Right - Inset;
            Square.Bottom := Square.Bottom - Inset;
            LB.Canvas.Brush.Color := LB.Canvas.Pen.Color;
            LB.Canvas.FillRect(Square);
        end;
        LB.Canvas.Brush.Color := LB.Color;
    end;

    //  ramka
    Square.Top := ARect.Top + 1;
    Square.Bottom := Square.Top + Size;
    Square.Left := LegendSwatchLeft(ARect.Right, Size);
    Square.Right := Square.Left + Size;
    LB.Canvas.Rectangle(Square);

    //  zapolnenie
    Inc(Square.Left);
    Inc(Square.Top);
    LB.Canvas.Brush.Color := TS.SeriesColor;
    LB.Canvas.FillRect(Square);

    LB.Canvas.Brush.Color := Color;           //  vosstanovlenie tsveta
    //  THE SELECTED CURVE'S ROW READS BOLD, so the legend names the curve the
    //  chart is showing wide. Asked of how the series IS drawn, by the same
    //  rule the self-check reads the chart with.
    if SeriesDrawnHighlighted(TS.LineWidth, TS.DrawOnTop) then
        LB.Canvas.Font.Style := LB.Canvas.Font.Style + [fsBold];
    //  Room for the box only where the box was drawn. Where the widget set drew
    //  it, ARect already begins after its own check column, so reserving a
    //  second box's width here would only be white space.
    LB.Canvas.TextOut(
        LegendTextLeft(ARect.Left, CheckSize, TextGap, DrawnCheck),
        ARect.Top, LB.Items.Strings[Index]);
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin

end;

{$hints on}


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
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    //  A load failure must raise.
    FitClientApp_.FitClient.LoadDataSet(FileName);
    FProjectFlow.RememberSource(FileName, ExtractFileExt(FileName));
    //  Document first, then the application and the build it is: a task bar
    //  truncates a title from the right, and what has to survive that is the
    //  name of the file this window is showing.
    Caption := GetWindowTitle(ApplicationProperties.Title, ExtractFileName(FileName));
    //  So that the chart is refreshed and Chart.XGraphMax and the rest hold
    //  correct values.
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionLoadProfileExecute(Sender: TObject);
begin
    //  A commented-out block calling the Win32 GetOpenFileName by hand used to
    //  stand here, from before this program used the widget set's own dialog. It
    //  named types no other platform has and could not have been compiled since;
    //  removed rather than kept as an alternative nobody would choose.
    //
    //  Known quirk: under a debugger, showing the hint inside the file-selection
    //  dialog misbehaves. Handling the corresponding messages in a hook by hand
    //  would probably avoid it. Not reproducible in normal operation, so it is
    //  left as it is.
    with OpenDialog do
    begin
        //  The filter is derived from the loader registry rather than stored in
        //  the .lfm, so the dialog cannot offer a format this build has no
        //  reader for - which is precisely what it did while the two lists were
        //  maintained by hand in different files.
        Filter := DataLoaderDialogFilter;
        InitialDir := ExtractFilePath(Application.ExeName);
        if Execute then
        begin
            if FileExists(FileName) then
            begin
                //  ASKED BEFORE ANYTHING IS DISCARDED, and only when there is
                //  something to lose - project_workflow decides which.
                if FProjectFlow.MayImportProfile(FileName) then
                    LoadDataFile(FileName);
            end;
        end;
    end;{with OpenDialog do...}
end;

{ Everything the registry has to say about the curve types, as plain records.
  Gathered in one pass so that the layout can be decided without iterating an
  interface. }
function TFormMain.GatherCurveTypes: TCurveTypeInfos;
var
    Iterator: ICurveTypeIterator;
    Info: TCurveTypeInfo;
begin
    SetLength(Result, 0);
    Iterator := TCurveTypesSingleton.CreateCurveTypeIterator;
    CheckThat(Assigned(Iterator), 'the walk over the registered curve types could not be started');
    Iterator.FirstCurveType;
    while True do
    begin
        Info := Default(TCurveTypeInfo);
        Info.Id := Iterator.GetCurveTypeId;
        Info.Name := Iterator.GetCurveTypeName;
        Info.Group := Iterator.GetCurrentCurveClass.GetCurveTypeGroup;
        //  What the type says it is, in one sentence - shown on hover in the
        //  menu, before the user has committed to anything.
        Info.Hint := Iterator.GetCurrentCurveClass.Explanation.Summary;
        Info.Tag := Iterator.GetCurveTypeTag(Info.Id);
        Info.IsUserCurveFactory :=
            IsEqualGUID(Info.Id, TUserPointsSet.GetCurveTypeId);
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Info;

        if Iterator.EndCurveType then
            Break;
        Iterator.NextCurveType;
    end;
end;

procedure TFormMain.CreateCurveTypeMenus;
var CurveTypeSelector: ICurveTypeSelector;
    MenuItem: TMenuItem;
    SelectedCurveTypeId: TCurveTypeId;
    i, j: Integer;
    Types: TCurveTypeInfos;
    Entries: TCurveMenuEntries;
    Order: TStringList;
    GroupItem: TMenuItem;
begin
    CurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;
    CheckThat(Assigned(CurveTypeSelector), 'the curve type selector the menus are built from is missing');
    SelectedCurveTypeId := CurveTypeSelector.GetSelectedCurveType;

    //  WHICH GROUP EACH TYPE GOES IN, what each entry is captioned and ticked
    //  as, and the order the groups appear in are all decided in
    //  curve_type_menu - where a curve pack's own group can be tried without
    //  one existing. What is left here is making menu items say it.
    Types := GatherCurveTypes;
    Entries := CurveMenuEntries(Types, SelectedCurveTypeId, MenuNewUserCapt);

    MenuSelectCurveType.Clear;
    //  The names of everything that menu held went with it.
    FMenuNames.Clear;
    Order := TStringList.Create;
    try
        CurveMenuGroupOrder(Entries, Order);
        for i := 0 to Order.Count - 1 do
        begin
            GroupItem := TMenuItem.Create(MenuSelectCurveType);
            GroupItem.Name := FMenuNames.NameFor('MenuCurveGroup', Order[i]);
            GroupItem.Caption := Order[i];
            MenuSelectCurveType.Add(GroupItem);

            for j := 0 to High(Entries) do
            begin
                if Entries[j].Group <> Order[i] then
                    Continue;
                MenuItem := TMenuItem.Create(MenuSelectCurveType);
                //  NAMED FOR THE TYPE IT SELECTS, not for its position.
                //  Nothing looks it up - there is no FindComponent by name
                //  anywhere here - so the name exists only to be read, in the
                //  object inspector, a debugger and ui_menus' own log line, and
                //  "CurveType7" says nothing. Through ui_names because the
                //  source is user data: one shipped type begins with a digit
                //  and two user curves may share a name, and a name the widget
                //  set refuses RAISES.
                MenuItem.Name := FMenuNames.NameFor('MenuCurveType',
                    Entries[j].Caption);
                MenuItem.Tag := Entries[j].Tag;
                MenuItem.OnClick := ActionSelCurveExecute;
                //  The caption is set AFTER the click handler, which would
                //  otherwise overwrite it with the action's own.
                MenuItem.Caption := Entries[j].Caption;
                MenuItem.Hint := Entries[j].Hint;
                //  Before the item is added, and for every entry that can ever
                //  carry a tick - see DeclareCheckableMenuEntries.
                MenuItem.ShowAlwaysCheckable := Entries[j].Checkable;
                MenuItem.Checked := Entries[j].Checked;
                GroupItem.Add(MenuItem);
            end;
        end;
    finally
        Order.Free;
    end;

    //  THE LIST, FROM THE SAME ENTRIES. Here rather than anywhere else because
    //  this is the one place that always sees the current curve type, and the
    //  list and the menu must not be able to disagree about it.
    RefreshCurveTypeList(Entries);

    //  Re-append the user-defined curve types (and their delete submenu) after
    //  every rebuild; otherwise clearing the menu here would drop them whenever a
    //  curve type is selected.
    CreateUserCurveMenus;

    //  The menu is rebuilt on every selection change, so this is the one place
    //  that always sees the current curve type.
    UpdateMinimizerAvailability;
    UpdateLossAvailability;
    UpdateFitAdvice(True);

    //  Check the active user-defined curve's own item (they share one curve-type
    //  id, so this can't be done by id). The delete-submenu items are nested, so
    //  iterating the direct children of the User group only affects the
    //  selection items.
    if Assigned(FSelectedUserCurve) then
    begin
        GroupItem := UserCurveMenu;
        for i := 0 to GroupItem.Count - 1 do
            if GroupItem.Items[i].Tag = LongInt(FSelectedUserCurve) then
                SetMenuEntryChecked(GroupItem.Items[i], True);
    end;
end;

{ Deletes the curves the parameter table has selected.

  WHAT THIS USED TO DO, and why it had to change. It removed rows from
  FCurveList - which is a COPY: GetCurveAttributes answers
  FCurveAttributes.GetCopy, so the deletion never reached the engine and the
  next refresh put every row back. It also ran against GridParameters whatever
  grid had focus, while ucDelete is enabled from the focused grid's selection -
  so Delete lit up on the Data grid and silently rewrote the Curve Attributes
  one. Nothing said so; the rows simply returned.

  Now it is the same verb the Model panel's context entry runs, because it is
  the same act: each selected row names a curve by the handle its attributes
  carry, and the service removes that curve and the pick it was seeded from.

  DELETED FROM THE BOTTOM UP. Every removal rebuilds the model and renumbers
  what is left, so handles are collected FIRST and then spent - an index taken
  before a deletion names a different curve after it. }
procedure TFormMain.ActionDeleteExecute(Sender: TObject);
var
    i, Row: longint;
    Id: string;
    Handles: TStringList;
    Removed: longint;
begin
    if not Assigned(FCurveList) then
        Exit;
    //  Only the parameter table names curves. On any other grid this command
    //  is not offered - see CommandStates, where ucDelete follows the focused
    //  grid - and reaching here from one would be that defect returning.
    if ActiveControl <> GridParameters then
        Exit;

    with GridParameters do
        if (Selection.Left <> FixedCols) or (Selection.Right <> ColCount - 1) then
            //  Whole rows only: half a row is half a curve, which is not a
            //  thing the model has.
            Exit;

    Handles := TStringList.Create;
    try
        for Row := GridParameters.Selection.Top to
            GridParameters.Selection.Bottom do
        begin
            Id := FCurveList.CurveHandleOfRow(Row - GridParameters.FixedRows);
            if Id <> '' then
                Handles.Add(Id);
        end;

        Removed := 0;
        for i := 0 to Handles.Count - 1 do
            if FitClientApp_.FitClient.DeleteCurve(Handles[i]) then
                Inc(Removed);

        if Removed < Handles.Count then
            //  A handle the model no longer holds is not a fault: an earlier
            //  removal in this same selection may have taken it, or a fit may
            //  have. Said once rather than raised.
            ShowHint(Format('%d of %d curves were already gone.',
                [Handles.Count - Removed, Handles.Count]));
    finally
        Handles.Free;
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
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.AnimationMode :=
        not FitClientApp_.FitClient.AnimationMode;
end;

procedure TFormMain.ActionAnimationModeUpdate(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    ActionAnimationMode.Checked := FitClientApp_.FitClient.AnimationMode;
end;

procedure TFormMain.ActionDoAllAutomaticallyExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    //  A full restart of the calculation - the flag need not be checked.
    FitClientApp_.FitClient.DoAllAutomatically;
    ShowHint(HintWait);
end;

procedure TFormMain.ActionEnableBackgroundVariationExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.BackgroundVariationEnabled :=
        not FitClientApp_.FitClient.BackgroundVariationEnabled;
    ActionEnableBackgroundVariationUpdate(Sender);
end;

procedure TFormMain.ActionEnableBackgroundVariationUpdate(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    ActionEnableBackgroundVariation.Checked :=
        FitClientApp_.FitClient.BackgroundVariationEnabled;
end;

procedure TFormMain.ActionEnableCurveScalingExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.CurveScalingEnabled :=
        not FitClientApp_.FitClient.CurveScalingEnabled;
    ActionEnableCurveScalingUpdate(Sender);
end;

{ The formula-based backends (Python sidecar, remote compute server) fit by
  evaluating a curve's expression, so they cannot fit a curve type that has
  none - a pattern built from picked pivots is one such. Offer the option
  only when it can actually work, and step back off it if it was already
  selected when such a type was chosen.

  The compute server enforces this independently (TFitTask.Optimization falls
  back to the native engine): the server must not trust a client to have done
  it. This is the courtesy half - not offering something that would silently be
  overridden. }
{ The engine's own description, or empty when it has none - used as the tooltip
  whenever the item is selectable. }
function MinimizerHint(AKind: longint): string;
var
    Info: TMinimizerInfo;
begin
    Result := '';
    if FindMinimizer(AKind, Info) then
        Result := Info.Description;
end;

procedure TFormMain.UpdateMinimizerAvailability;
var
    Selected: TCurveClass;
    Analytic, NeedsFormula: boolean;
    Item: TMenuItem;
    i: longint;
begin
    Selected := FindCurveClassById(
        TCurveTypesSingleton.CreateCurveTypeSelector.GetSelectedCurveType);
    Analytic := (not Assigned(Selected)) or Selected.IsAnalytic;

    //  An engine that fits by evaluating a formula cannot fit a curve type that
    //  has none. Derived from what each engine declares, so a third engine is
    //  greyed out correctly with no edit here.
    if not Assigned(FMinimizerItems) then
        Exit;
    for i := 0 to FMinimizerItems.Count - 1 do
    begin
        Item := TMenuItem(FMinimizerItems[i]);
        NeedsFormula := MinimizerNeedsFormula(Item.Tag);
        Item.Enabled := Analytic or (not NeedsFormula);
        if Item.Enabled then
            Item.Hint := MinimizerHint(Item.Tag)
        else
            Item.Hint :=
                'This curve type has no formula, so it is fitted by the native ' +
                'engine.';
    end;

    //  Silently leaving an unusable engine selected would misreport which one
    //  ran. Falls back to the default, which is the first registered.
    if (not Analytic) and MinimizerNeedsFormula(FMinimizerKind) then
        ApplyMinimizerKind(DefaultMinimizerKind);
end;

procedure TFormMain.ApplyMinimizerKind(AKind: longint);
begin
    FMinimizerKind := AKind;
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        FitClientApp_.FitClient.MinimizerKind := AKind;
    MarkSelectedMinimizer(AKind);
    //  Shown only for an engine that can actually be weighted - the native one
    //  always fits unweighted, so the menu under it would be a control that does
    //  nothing. Asked of the engine, not of one engine's id.
    MenuWeighting.Visible := MinimizerSupportsWeighting(AKind);
end;

{ Builds the objective submenu from fit_loss itself.

  Deliberately not declared in the .lfm: the set of objectives is defined in one
  place, and a menu transcribed by hand would be a second place to forget. A new
  LOSS_KIND_* appears here automatically, with its own name and its description
  as the tooltip. }
procedure TFormMain.BuildLossMenu;
var
    Losses: TLossInfoArray;
    K: longint;
    Item: TMenuItem;
begin
    FMenuLoss := TMenuItem.Create(Self);
    FMenuLoss.Name := 'MenuLossRoot';
    FMenuLoss.Caption := 'Loss Function';
    //  Over what registered, not over a numeric range: a range assumes the
    //  objectives are contiguous integers, which stops being true the moment one
    //  is contributed from outside this unit.
    Losses := RegisteredLosses;
    for K := 0 to High(Losses) do
    begin
        Item := TMenuItem.Create(FMenuLoss);
        Item.Name := FWidgetNames.NameFor('MenuLoss', Losses[K].Name);
        Item.Caption := Losses[K].Name;
        Item.Hint := Losses[K].Description;
        Item.RadioItem := True;
        //  A group index distinct from the minimizer's (7) and weighting's (8).
        Item.GroupIndex := 9;
        //  The kind travels on the item, so the handler needs no lookup table
        //  that could fall out of step with the menu order.
        Item.Tag := Losses[K].Kind;
        Item.OnClick := MenuLossClick;
        FMenuLoss.Add(Item);
    end;
    //  Beside the minimizer, since the two together define what a fit does.
    MenuMinimizer.Parent.Add(FMenuLoss);
end;

{ Data > Price Data: which column of a price-data (.csv) file is the value, and
  what its argument is.

  THE LOADER COULD ALWAYS DO BOTH; nothing in the window chose. The choice sets
  the defaults every price-data loader is created with (ApplyPriceDataChoice),
  so it applies to the next File > Import Profile or File > Reload Profile - the
  profile already loaded is not re-read behind the user's back. Kept between
  sessions. }
procedure TFormMain.BuildPriceDataMenu;

    function Entry(AParent: TMenuItem; const ACaption, AHint: string;
        ATag, AGroup: longint; AClick: TNotifyEvent): TMenuItem;
    begin
        Result := TMenuItem.Create(AParent);
        Result.Name := FWidgetNames.NameFor('MenuPrice', ACaption);
        Result.Caption := ACaption;
        Result.Hint := AHint;
        Result.RadioItem := True;
        //  Declared checkable at creation - see DeclareCheckableMenuEntries.
        Result.ShowAlwaysCheckable := True;
        //  Groups of their own: 7 to 9 are the minimizer, weighting and loss.
        Result.GroupIndex := AGroup;
        Result.Tag := ATag;
        Result.OnClick := AClick;
        AParent.Add(Result);
    end;

var
    Root: TMenuItem;
begin
    Root := TMenuItem.Create(Self);
    Root.Name := 'MenuPriceData';
    Root.Caption := 'Price Data';
    Root.Hint := 'How a price-data (.csv) file is read: which column is the ' +
        'value, and what the argument is';

    FMenuPriceValue := TMenuItem.Create(Root);
    FMenuPriceValue.Name := 'MenuPriceValue';
    FMenuPriceValue.Caption := 'Value';
    Root.Add(FMenuPriceValue);
    Entry(FMenuPriceValue, 'Close', 'Read the closing price (Adj Close when ' +
        'the file has it) as the value', Ord(vcClose), 13, MenuPriceValueClick);
    Entry(FMenuPriceValue, 'Open', 'Read the opening price as the value',
        Ord(vcOpen), 13, MenuPriceValueClick);
    Entry(FMenuPriceValue, 'High', 'Read the high as the value',
        Ord(vcHigh), 13, MenuPriceValueClick);
    Entry(FMenuPriceValue, 'Low', 'Read the low as the value',
        Ord(vcLow), 13, MenuPriceValueClick);

    FMenuPriceArgument := TMenuItem.Create(Root);
    FMenuPriceArgument.Name := 'MenuPriceArgument';
    FMenuPriceArgument.Caption := 'Argument';
    Root.Add(FMenuPriceArgument);
    Entry(FMenuPriceArgument, 'Bar Number', 'Number the rows 0, 1, 2 ... so ' +
        'that weekends and holidays leave no gaps', Ord(xmBarIndex), 14,
        MenuPriceArgumentClick);
    Entry(FMenuPriceArgument, 'Date', 'Place each row at its date, keeping ' +
        'the calendar gaps', Ord(xmDateSerial), 14, MenuPriceArgumentClick);

    MenuData.Add(Root);
end;

procedure TFormMain.ApplyPriceData(AValueColumn, AArgument: longint);
var
    i: longint;
begin
    ApplyPriceDataChoice(AValueColumn, AArgument);
    //  READ BACK, so an out-of-range stored value shows the default it became.
    FPriceValueColumn := Ord(DefaultOhlcValueColumn);
    FPriceArgument := Ord(DefaultOhlcXMode);
    if Assigned(FMenuPriceValue) then
        for i := 0 to FMenuPriceValue.Count - 1 do
            SetMenuEntryChecked(FMenuPriceValue.Items[i],
                FMenuPriceValue.Items[i].Tag = FPriceValueColumn);
    if Assigned(FMenuPriceArgument) then
        for i := 0 to FMenuPriceArgument.Count - 1 do
            SetMenuEntryChecked(FMenuPriceArgument.Items[i],
                FMenuPriceArgument.Items[i].Tag = FPriceArgument);
end;

procedure TFormMain.MenuPriceValueClick(Sender: TObject);
begin
    ApplyPriceData(TMenuItem(Sender).Tag, FPriceArgument);
    ShowHint('Takes effect at the next File > Import Profile or File > ' +
        'Reload Profile of a price-data file.');
end;

procedure TFormMain.MenuPriceArgumentClick(Sender: TObject);
begin
    ApplyPriceData(FPriceValueColumn, TMenuItem(Sender).Tag);
    ShowHint('Takes effect at the next File > Import Profile or File > ' +
        'Reload Profile of a price-data file.');
end;

{ Builds every registered module's menu from what it DECLARES.

  The window decides only where module menus hang - under Model, beside Curve
  Type, because a model family is what they are and one top-level entry per pack
  does not scale. Everything else, including the wording, belongs to the module.

  Ids travel on the items so a handler needs no lookup table that could fall out
  of step with the menu - the same shape the loss and curve-type menus use. }
procedure TFormMain.BuildModuleMenus;
var
    Mods: TUiModuleArray;
    Decls: TUiMenuDeclArray;
    Nodes: TModuleMenuNodes;
    Created: array of TMenuItem;
    Root, Item: TMenuItem;
    m, i: longint;
begin
    if not Assigned(FModuleMenuItems) then
        FModuleMenuItems := TStringList.Create;
    FModuleMenuItems.Clear;

    Mods := RegisteredUiModules;
    for m := 0 to High(Mods) do
    begin
        Decls := Mods[m].MenuItems;
        if Length(Decls) = 0 then
            Continue;

        Root := TMenuItem.Create(Self);
        Root.Name := FWidgetNames.NameFor('MenuModule', Mods[m].Name);
        Root.Caption := ModuleRootCaption(Mods[m].Name);
        MenuModel.Add(Root);

        //  WHAT HANGS FROM WHAT, and what kind of widget each entry is, are
        //  decided in module_menu - where a declaration naming a parent that
        //  does not exist can be tried, and the framework ships no module to try
        //  it with. This loop makes widgets say the answer.
        Nodes := ModuleMenuNodes(Decls);
        SetLength(Created, Length(Nodes));
        begin
            for i := 0 to High(Nodes) do
            begin
                Item := TMenuItem.Create(Root);
                Item.Name := FWidgetNames.NameFor('MenuModuleItem',
                    Mods[m].Name + Nodes[i].Id);
                Item.Caption := Nodes[i].Caption;
                Item.Hint := Nodes[i].Hint;
                if Nodes[i].IsRadio then
                begin
                    Item.RadioItem := True;
                    Item.GroupIndex := Nodes[i].RadioGroup;
                end;
                if Nodes[i].Checkable then
                    Item.ShowAlwaysCheckable := True;
                Item.Checked := Nodes[i].Checked;
                if Nodes[i].Clickable then
                    Item.OnClick := ModuleMenuClick;

                if Nodes[i].ParentIndex >= 0 then
                    Created[Nodes[i].ParentIndex].Add(Item)
                else
                    Root.Add(Item);
                Created[i] := Item;

                //  Id -> item, and id -> module, so a click can be routed back
                //  to whoever declared it.
                FModuleMenuItems.AddObject(Nodes[i].Id, Item);
                Item.Tag := m;
            end;
        end;
    end;
end;

{ Declares every menu entry that can be TICKED to be a checkable one - even the
  ones that start unticked - once, before any menu has a handle.

  WHY. A menu entry is one of two different widgets: a plain one, or a checkable
  one. Which it is is decided when its handle is created, from whether the entry
  is checkable AT THAT MOMENT (TMenuItem.IsCheckItem). Ticking a plain entry
  afterwards cannot be done to the widget it already is, so the widgetset
  DESTROYS it and builds a checkable one in its place (on gtk2,
  TGtk2WSMenuItem.SetCheck -> TMenuItem.RecreateHandle).

  Which would be harmless if ticks only ever arrived between menus. They do not:
  the picking modes are applied whenever the window refreshes its state, which an
  event can ask for at any moment - and the tick that says a mode is on can land
  while the user is standing in the menu. The
  entry destroyed is then the parent of a submenu that is dropped down, and that
  submenu is left on screen attached to nothing: moving the pointer away does not
  hide it, Escape does not reach it, and only clicking one of its entries -
  running a command the user did not want - makes it go away. That is the
  dangling menu, and it is why it could be any submenu and only sometimes.

  Declared checkable from the start, the entry is the right widget already and a
  tick is what it should always have been: a property change on a live widget.
  The check box it reserves is what a togglable entry looks like anyway - which
  is the second half of the rule: an entry that reserves one must be one. The
  submenu parents that used to carry the picking mode are no longer ticked at
  all, so they need no box and cannot be recreated either.

  Radio entries (the engines, the objectives, the weighting) are already
  declared - RadioItem does the same job - and so are the curve types and the
  user's own curves, which declare it as each item is created.

  AN ENTRY THAT CAN BE TICKED BELONGS IN THIS LIST.

  And because a list is a thing to forget: what this procedure declares is
  CHECKED at startup, over the menus as actually built, by
  MenuEntriesAtRiskOfDangling - and a tick that reaches an entry this list
  missed is refused, and named in the log, while a menu is open. Both are in
  ui_menus, which also says what else must not be done to an open menu. }
procedure TFormMain.DeclareCheckableMenuEntries;

    procedure Checkable(const AItems: array of TMenuItem);
    var
        i: longint;
    begin
        for i := Low(AItems) to High(AItems) do
            if Assigned(AItems[i]) then
                AItems[i].ShowAlwaysCheckable := True;
    end;

begin
    //  The picking modes that are an entry of their own. The three whose mode
    //  lives behind a SUBMENU - Fit Intervals, Curve Positions, Background -
    //  are NOT here: a submenu parent is not a togglable thing, and a check box
    //  on one only asks the user what ticking it would mean. Their mode is said
    //  where it is entered and left, by the caption of the entry inside that
    //  reads Start Selection or Stop Selection.
    Checkable([MenuSelectIntervalBounds]);

    //  The view toggles.
    Checkable([MenuViewMarkers, MenuAnimationMode, MenuEnableCurveScaling,
        MenuEnableBackgroundVariation]);

    //  The argument axis, in the main menu and in the chart's popup: one of
    //  them is ticked at a time, and which one changes with the model.
    Checkable([MenuTheta, MenuN2Theta, MenuSinThetaLambda,
        PopupMenuTheta, PopupMenuN2Theta, PopupMenuSinThetaLambda,
        FMenuCurveAxis, FMenuCurveAxisPopup, FMenuIdentity, FMenuIdentityPopup,
        FMenuCustom, FMenuCustomPopup]);
end;

function TFormMain.ModuleItemById(const AId: string): TMenuItem;
var
    i: longint;
begin
    Result := nil;
    if not Assigned(FModuleMenuItems) then
        Exit;
    i := FModuleMenuItems.IndexOf(AId);
    if i >= 0 then
        Result := TMenuItem(FModuleMenuItems.Objects[i]);
end;

{ Runs the module command a table row names, whichever surface was clicked.

  ONE PLACE, because a command chosen from the menu and the same command chosen
  from the Tools pane must mean the same thing - including the part that is not
  the command at all: a radio entry names one of a set, and clicking it is
  choosing it. The menu's own radio group does that to the entries by itself and
  the pane hears nothing of it, so the choice is recorded here for both, before
  the command runs - a module that states the choice itself then has the last
  word. }
procedure TFormMain.RunModuleCommand(ARow: longint);
var
    Mods: TUiModuleArray;
    Target: TCommandTarget;
begin
    Target := FCommands.TargetOf(ARow);
    if Target.Kind <> ctModuleCommand then
        Exit;
    FCommands.ChooseModuleRow(ARow);
    Mods := RegisteredUiModules;
    if (Target.ModuleIndex >= 0) and (Target.ModuleIndex <= High(Mods)) then
        Mods[Target.ModuleIndex].Command(Target.CommandId, '',
            Self as IUiHost);
end;

procedure TFormMain.ModuleMenuClick(Sender: TObject);
var
    Item: TMenuItem;
    i: longint;
begin
    RequestStateRefresh;
    Item := TMenuItem(Sender);
    i := FModuleMenuItems.IndexOfObject(Item);
    if i < 0 then
        Exit;
    RunModuleCommand(FCommands.IndexOfModuleRow(Item.Tag,
        FModuleMenuItems[i]));
end;

procedure TFormMain.MenuLossClick(Sender: TObject);
begin
    ApplyLossKind(TMenuItem(Sender).Tag);
    //  Announce: the user just chose this, so if it cannot be honoured they
    //  should find out now, not from a puzzling result later.
    UpdateFitAdvice(True);
end;

{ Keeps the user aware of what the fit will really do.

  The engine corrects choices that cannot be honoured - a formula engine cannot
  fit a formula-less curve, or minimise an objective that is not a sum of
  squares - and those corrections are all sound. They are also invisible, and an
  invisible correction is indistinguishable from a bug when the user notices the
  result does not match what they selected.

  Two levels, deliberately:
    * the status bar ALWAYS states what will happen, so the current state is
      readable at a glance without anyone having to ask;
    * a dialog appears only when the reason CHANGES, so the explanation arrives
      once, attached to the action that caused it.

  Nothing here decides anything - AdviseFit does, and the engine calls the same
  function. This only reports. }
procedure TFormMain.UpdateFitAdvice(AAnnounce: boolean);
var
    Selected: TCurveClass;
    Advice: TFitAdvice;
    Scaling: boolean;
begin
    if csDestroying in ComponentState then Exit;
    if not (Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient)) then
        Exit;

    Selected := FindCurveClassById(
        TCurveTypesSingleton.CreateCurveTypeSelector.GetSelectedCurveType);
    Scaling := FitClientApp_.FitClient.CurveScalingEnabled;

    Advice := AdviseFit(
        FLossKind,
        FMinimizerKind = MIN_KIND_PYTHON_LM,
        (not Assigned(Selected)) or Selected.IsAnalytic,
        Assigned(Selected) and Selected.AmplitudeIsUnbounded,
        Scaling);

    if (FAdvicePanel >= 0) and (FAdvicePanel < StatusBar.Panels.Count) then
        StatusBar.Panels[FAdvicePanel].Text := Advice.Summary;

    //  The full reasoning is always reachable by hovering, whether or not the
    //  dialog has been shown.
    StatusBar.Hint := Advice.Detail;
    StatusBar.ShowHint := Advice.Detail <> '';

    //  WHETHER TO SAY IT OUT LOUD, and what to remember having said, is in
    //  fit_advice - including the part that is easy to get wrong: the memory is
    //  cleared when the advice stops needing attention, so returning to a
    //  problematic selection explains itself afresh.
    if AdviceShouldBeAnnounced(AAnnounce, Advice, FLastAdviceShown,
            FLastAdviceShown) then
        MessageDlg('About this fit', Advice.Detail, mtInformation, [mbOK], 0);
end;

procedure TFormMain.ApplyLossKind(AKind: longint);
var
    i: integer;
begin
    if not IsKnownLoss(AKind) then
        AKind := LOSS_KIND_RFACTOR;
    FLossKind := AKind;
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        FitClientApp_.FitClient.LossKind := AKind;
    if Assigned(FMenuLoss) then
        for i := 0 to FMenuLoss.Count - 1 do
            SetMenuEntryChecked(FMenuLoss.Items[i],
                FMenuLoss.Items[i].Tag = AKind);
end;

{ Offers only the objectives that are legitimate for the selected curve type.

  The rule is not restated here - it is LossAllowedForCapability, the same one
  the engine enforces (D18). This is the courtesy half: the engine substitutes a
  compatible objective anyway, so without this the user could select something
  that would be silently overridden, which is worse than not offering it. }
procedure TFormMain.UpdateLossAvailability;
var
    Selected: TCurveClass;
    FreeAmplitude: boolean;
    i: integer;
    K: longint;
begin
    if not Assigned(FMenuLoss) then
        Exit;

    Selected := FindCurveClassById(
        TCurveTypesSingleton.CreateCurveTypeSelector.GetSelectedCurveType);
    FreeAmplitude := Assigned(Selected) and Selected.AmplitudeIsUnbounded;

    for i := 0 to FMenuLoss.Count - 1 do
    begin
        K := FMenuLoss.Items[i].Tag;
        FMenuLoss.Items[i].Enabled := LossAllowedForCapability(K, FreeAmplitude);
        if FMenuLoss.Items[i].Enabled then
            FMenuLoss.Items[i].Hint := LossDescription(K)
        else
            FMenuLoss.Items[i].Hint := LossRefusalReason(K);
    end;

    //  Leaving an unusable objective selected would misreport what was minimised.
    if not LossAllowedForCapability(FLossKind, FreeAmplitude) then
        ApplyLossKind(DefaultLossFor(FreeAmplitude));
end;

{ Residual weighting for the Python backend: 'poisson' (counting statistics) or
  'none' (unweighted, like the native engine). }
procedure TFormMain.ApplyWeighting(const AValue: string);
begin
    FWeighting := AValue;
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        FitClientApp_.FitClient.Weighting := AValue;
    //  The tick follows the same rule the backend applies, so it cannot
    //  say one thing while the fit does another.
    SetMenuEntryChecked(MenuWeightingPoisson,
        not WeightingIsUnweighted(AValue));
    SetMenuEntryChecked(MenuWeightingNone, WeightingIsUnweighted(AValue));
end;

procedure TFormMain.MenuWeightingPoissonClick(Sender: TObject);
begin
    ApplyWeighting(WEIGHTING_POISSON);
end;

procedure TFormMain.MenuWeightingNoneClick(Sender: TObject);
begin
    ApplyWeighting(WEIGHTING_NONE);
end;

{ Selects an engine. Which one travels on the menu item's Tag, so this needs no
  lookup table that could fall out of step with the menu order - the same shape
  the loss and curve-type menus use. }
procedure TFormMain.MarkSelectedMinimizer(AKind: longint);
var
    i: longint;
begin
    if not Assigned(FMinimizerItems) then
        Exit;
    for i := 0 to FMinimizerItems.Count - 1 do
        SetMenuEntryChecked(TMenuItem(FMinimizerItems[i]),
            TMenuItem(FMinimizerItems[i]).Tag = AKind);
end;

procedure TFormMain.MenuMinimizerClick(Sender: TObject);
begin
    ApplyMinimizerKind(TMenuItem(Sender).Tag);
    UpdateFitAdvice(True);
end;

{ Builds the engine list from what registered, rather than from items placed in
  the .lfm by hand.

  The transcription was the defect: adding an engine meant editing the form, four
  decision sites and the settings default, and the form was the one nothing would
  fail without - the engine would simply never appear. }
procedure TFormMain.BuildMinimizerMenu;
var
    Engines: TMinimizerInfoArray;
    Item: TMenuItem;
    i: longint;
begin
    MenuMinimizer.Clear;
    FMinimizerItems := TList.Create;
    Engines := RegisteredMinimizers;
    for i := 0 to High(Engines) do
    begin
        Item := TMenuItem.Create(MenuMinimizer);
        Item.Name := FWidgetNames.NameFor('MenuMinimizer', Engines[i].Name);
        Item.Caption := Engines[i].Name;
        Item.Hint := Engines[i].Description;
        Item.RadioItem := True;
        //  Group 7, as the transcribed items used.
        Item.GroupIndex := 7;
        Item.Tag := Engines[i].Kind;
        Item.OnClick := MenuMinimizerClick;
        MenuMinimizer.Add(Item);
        FMinimizerItems.Add(Item);
    end;
end;

procedure TFormMain.ApplyServerUrl(const AUrl: string);
begin
    //  There is no in-process engine: an empty setting just means the default.
    //  The rule is in server_connection, beside the one that decides what is
    //  OFFERED - they were two copies of it, and a difference between them shows
    //  the user one server and talks to another.
    FServerUrl := ServerUrlToUse(AUrl, DefaultServerUrl);
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        FitClientApp_.FitClient.ServerUrl := FServerUrl;
end;

procedure TFormMain.MenuComputeServerClick(Sender: TObject);
var
    Url: string;
    Probe: THttpFitService;
begin
    //  The compute server is an independent process (possibly on another machine);
    //  it must already be running. An empty URL means the default address.
    Url := ServerUrlToOffer(FServerUrl, DefaultServerUrl);
    if not InputQuery('Compute Server',
        'Server URL (fit_server must be running there):', Url) then
        Exit;

    ApplyServerUrl(Url);
    //  A new server: what the window offers depends on it.
    RequestStateRefresh;

    //  Tell the user straight away if nothing is listening there - without a
    //  server this client cannot compute anything.
    Probe := THttpFitService.Create(FServerUrl);
    try
        //  WHAT FOLLOWS FROM THE PROBE is in server_connection: nothing
        //  answering is said at once, and a profile loaded while no server was
        //  reachable lives only in the client, so this is the moment it can be
        //  handed over.
        case StepAfterProbing(Probe.IsAvailable,
            FitClientApp_.FitClient.OpenState = OpenSuccess) of
            csTellTheUserNothingAnswered:
                MessageDlg('Compute Server',
                    NoServerAnsweredNotice(FServerUrl), mtWarning, [mbOK], 0);
            csSendTheProfile:
                FitClientApp_.FitClient.SendProfileToServer;
        end;
    finally
        Probe.Free;
    end;
end;

{ Explains the two compute backends and how to enable the optional Python one,
  so the setup is discoverable without hunting through the docs. }
procedure TFormMain.MenuComputeBackendsClick(Sender: TObject);
begin
    MessageDlg('Compute Backends',
        'Fitting runs in a compute server the app connects to.' + LineEnding +
        LineEnding +
        'Native engine (default): built in, needs nothing extra. Reports the ' +
        'R-factor, then reduced Chi-squared and R-squared in the status bar; AIC ' +
        'and BIC are kept in the saved project.' + LineEnding +
        LineEnding +
        'Python engine (optional): adds per-parameter uncertainties via lmfit. ' +
        'It needs Python 3 with numpy, scipy and lmfit, at pinned versions, in a ' +
        'self-contained environment (your system Python is untouched). One-time ' +
        'setup is described under Help -> Explain Everything, in the Fitting ' +
        'chapter, Setting up the Python engine.' + LineEnding +
        LineEnding +
        'After that, choose Fit -> Minimizer -> Levenberg-Marquardt ' +
        '(Python/lmfit) and ' +
        'run Minimize as usual. The compute server starts and stops the Python ' +
        'worker for you; there is no separate process to run.',
        mtInformation, [mbOK], 0);
end;

{ Shows a read-only, scrollable report (the Python fit's parameters with their
  uncertainties, and the statistics). }

procedure TFormMain.ActionEnableCurveScalingUpdate(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    ActionEnableCurveScaling.Checked := FitClientApp_.FitClient.CurveScalingEnabled;
end;

procedure TFormMain.ActionMinimizeDifferenceExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    SetMenuEntryChecked(MenuSelectCurvePositionsManually, False);
    FitClientApp_.FitClient.MinimizeDifference;
    ShowHint(HintWait);
end;

procedure TFormMain.ActionMinimizeNumberOfCurvesExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    SetMenuEntryChecked(MenuSelectCurvePositionsManually, False);
    FitClientApp_.FitClient.MinimizeNumberOfCurves;
    ShowHint(HintWait);
end;

procedure TFormMain.ActionReloadDataExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    //  WHICH FILE, whether it is still there and whether to ask are all the
    //  workflow's: it re-reads the file the open document came from, and comes
    //  back through LoadProfile below.
    FProjectFlow.ReloadProfile;
end;

procedure TFormMain.LoadProfile(const APath: string);
begin
    LoadDataFile(APath);
end;

procedure TFormMain.ActionRemoveBackgroundPointsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.RemoveAllBackgroundPoints;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionRemoveRFactorBoundsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.RemoveAllRFactorBounds;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionRemoveCurvePositionsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.RemoveAllCurvePositions;
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
end;

procedure TFormMain.ActionSubtractBackgroundAutomaticallyExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    SubtractBackground(True);
end;

procedure TFormMain.ActionSubtractBackgroundBySelectedPointsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    if (FitClientApp_.FitClient.GetBackgroundPoints = nil) or
       (FitClientApp_.FitClient.GetBackgroundPoints.PointsCount < 2) then
       //  An admissible error here.
    begin
         MessageDlg('No background points are selected. Select the background ' +
             'points on the chart first, then run this operation again.',
             mtWarning,[mbOk], 0);
         Exit;
    end;

    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    SubtractBackground(False);
end;

{ EACH EXPORT NAMES ITS OWN TABLE. What stood here decided which grid to write
  from the tab in front, with no else branch - so on any other tab the command
  silently did nothing - and it cleared that table's modified flag, claiming the
  work had been kept when what it had written cannot be opened again by anything.

  Exporting no longer clears the flag. It is not a save: the document is the
  project, and Save is what keeps it. }
procedure TFormMain.ActionExportCurveParametersExecute(Sender: TObject);
begin
    //  FROM THE MODEL, not the grid: the grid shows four decimals and puts an
    //  uncertainty in its value's cell (see TCurveListBase.ExportRows).
    SaveTableAsText(
        FitClientApp_.FitClient.CurveAttributesForDisplay.ExportRows);
end;

procedure TFormMain.ActionExportSummaryTableExecute(Sender: TObject);
begin
    SaveTableAsText(FitClientApp_.FitClient.SummaryExportRows);
end;

procedure TFormMain.ActionNewProjectExecute(Sender: TObject);
begin
    FProjectFlow.NewProject;
end;

procedure TFormMain.ActionOpenProjectExecute(Sender: TObject);
begin
    FProjectFlow.OpenProject;
end;

procedure TFormMain.ActionSaveProjectExecute(Sender: TObject);
begin
    FProjectFlow.SaveProject;
end;

procedure TFormMain.ActionSaveProjectAsExecute(Sender: TObject);
begin
    FProjectFlow.SaveProjectAs;
end;

procedure TFormMain.ActionSelectDataIntervalExecute(Sender: TObject);
var SP: TNeutronPointsSet;
    NP: TNeutronPointsSet;
    From_, To_: longint;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    SP := FitClientApp_.FitClient.GetSelectedPoints;
    if (SP = nil) or (SP.PointsCount <> 2) then
    begin
        //  SP=nil here is an admissible user error.
        MessageDlg('Select two limiting points on the chart to define the ' +
            'interval, then run this operation again.',
            mtWarning, [mbOK], 0);
        Exit;
    end;

    NP := FitClientApp_.FitClient.GetProfilePoints;
        //FFitViewer.GetPointsSet(FActiveNumber);
    SP.Sort;

    From_ := NP.IndexOfValueX(SP.PointXCoord[0]);
    To_ := NP.IndexOfValueX(SP.PointXCoord[1]);
    //  TOLD TO THE DOCUMENT, because nothing else knows it afterwards: the
    //  engine holds the windowed data, not the indices it was windowed by.
    FProjectFlow.RememberInterval(From_, To_);
    FitClientApp_.FitClient.SelectProfileInterval(From_, To_);
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    //  So that the chart is refreshed and Chart.XGraphMax and the rest hold
    //  correct values.
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelectIntervalBoundsExecute(Sender: TObject);
var
    NewMode: TSelMode;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    //  WHETHER THIS ENTERS OR LEAVES is action_state's answer. This handler used
    //  to write the rule out itself, as three others did.
    NewMode := ModeAfterPicking(FitClientApp_.FitClient.SelectionMode,
        ModeSelectIntervalBounds);
    if NewMode = ModeSelectNothing then
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing
    else
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := NewMode;
        ShowHint(HintFirstStart);
    end;
end;

procedure TFormMain.ActionComputeBackgroundPointsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');
    //  Into the mode for picking background points.
    if FitClientApp_.FitClient.SelectionMode <> ModeSelectBackground then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectBackground;
        ShowHint(HintFirst);
    end;
    //??? The menu item should be disabled until the calculation finishes;
    //  check every similar case.
    FitClientApp_.FitClient.ComputeBackgroundPoints;
end;

procedure TFormMain.ActionSelectBackgroundManuallyExecute(Sender: TObject);
var
    NewMode: TSelMode;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    //  WHETHER THIS ENTERS OR LEAVES is action_state's answer, not this
    //  window's: the same entry both starts and stops the mode, and the rule
    //  was written out once per entry until it was asked for instead.
    NewMode := ModeAfterPicking(FitClientApp_.FitClient.SelectionMode, ModeSelectBackground);
    if NewMode = ModeSelectNothing then
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing
    else
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := NewMode;
        ShowHint(HintFirst);
    end;
end;

{ Choosing a curve type from the Curve menu.

  WHAT THE RULES ARE is in curve_type_choice: which type the clicked item stands
  for, and what to do about a type whose setup the user backed out of. They were
  a nested if/else with an empty branch and a Break inside a while-true loop
  here, where the only way to reach them was to click.

  WHAT IS LEFT IS THE WINDOW'S OWN WORK: opening the dialog, showing the message,
  and telling the client - which selects on both sides, menu and compute server. }
{ A menu entry naming a curve type was chosen. The entry carries the registry's
  handle in its Tag, and everything past reading it is shared with the Tools
  pane's list - see SelectCurveTypeByTag. }
procedure TFormMain.ActionSelCurveExecute(Sender: TObject);
begin
    CheckAssigned(Sender, 'the control that raised this event');
    SelectCurveTypeByTag(TMenuItem(Sender).Tag);
end;

{ Selects the curve type the registry's handle names.

  TAKES THE TAG, not the widget that carried it. The Tools pane's list holds the
  same handles and had to reach this: it did so by creating a throwaway TMenuItem
  with the Tag set, purely to satisfy a handler that wanted a Sender - an object
  that was never parented, never shown and never named, standing in for a menu
  entry that does not exist. Two callers with one handle between them want a
  method, not a decoy. }
procedure TFormMain.SelectCurveTypeByTag(ATag: longint);
var
    CurveClass: TCurveClass;
    Configurable: TConfigurablePointsSetClass;
    Outcome: TCurveSetupOutcome;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    CurveClass := CurveClassForMenuTag(
        TCurveTypesSingleton.CreateCurveTypeIterator, ATag);
    //  NOT AN ASSERTION. A click can be delivered against the strip as it was
    //  before the rebuild below replaced it, and the old code answered that
    //  case by running off the end of the registry and doing nothing.
    if Assigned(CurveClass) then
    begin
        Configurable := CurveClass.GetConfigurablePointsSet;
        if Configurable.HasConfigurableParameters then
            //  The dialog is opened here and nowhere else, and only for a type
            //  that has something to ask about - which is why the answer is
            //  passed in rather than the dialog being reachable from the rule.
            Outcome := CurveSetupOutcome(True,
                Configurable.ShowConfigurationDialog, Configurable.HasDefaults)
        else
            Outcome := CurveSetupOutcome(False, False, False);

        if Outcome = csoRefuse then
            //  Told, rather than left as a menu that appears not to work.
            MessageDlg('User-defined curve not created',
                CurveSetupWasCancelled(MenuNewUserCapt), mtInformation,
                [mbOK], '')
        else
        begin
            //  BEFORE SELECTING, always: selecting is what builds a curve, and
            //  a curve built from unset parameters is what this ordering is for.
            if Outcome = csoApplyDefaultsThenSelect then
                Configurable.SetDefaults;

            //  Selects on both sides - menu and compute server; see
            //  TFitClient.SelectCurveType.
            FitClientApp_.FitClient.SelectCurveType(
                CurveClass.GetCurveTypeId);
            //  Configuring the user-defined type CREATED a curve, and that
            //  curve is what is now active - so it, not the item that was
            //  clicked, is what the menu has to show as selected. Every other
            //  type is a curve in itself and leaves no user curve selected.
            if SelectionLeavesUserCurve(CurveClass.GetCurveTypeId,
                    TUserPointsSet.GetCurveTypeId) then
                FSelectedUserCurve := LastUserCurve
            else
                FSelectedUserCurve := nil;
            //  The new type may define a different abscissa (a peak is fitted
            //  against a scattering angle, a wave pattern against a plain
            //  position), so the caption and the grid have to follow it.
            RefreshAxisForSelectedCurveType;
            //  AND THE EXPLAIN PANE FOLLOWS THE CHOICE. Only a click on the type
            //  already selected used to tell it, so choosing a different type -
            //  from the Tools list or the menu - left the pane explaining the
            //  previous one.
            FExplainFocus := FocusCurveTypeSelected(FExplainFocus,
                SelectedCurveTypeTopic);
            RefreshExplainPane;
        end;
    end;
    //  Sender is one of the items this rebuild destroys, and the widgetset has
    //  not finished with it yet. Queued, never direct - see
    //  QueueCurveTypeMenuRebuild.
    QueueCurveTypeMenuRebuild;
end;

procedure TFormMain.ActionSelectAllExecute(Sender: TObject);
begin
    if ActiveControl is TNumericGrid then
        with ActiveControl as TNumericGrid do SelectAll;
end;

procedure TFormMain.ActionSelectEntireProfileExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    //  The counterpart of RememberInterval: going back to the whole profile is
    //  what "no selected interval" means, and a project saved afterwards must
    //  not ask for a window nobody is in any more.
    FProjectFlow.ForgetInterval;
    FitClientApp_.FitClient.SelectEntireProfile;
    //  So that the chart is refreshed and Chart.XGraphMax and the rest hold
    //  correct values.
    Application.ProcessMessages;
    FInitXGraphMax := Chart.XGraphMax;
    FInitXGraphMin := Chart.XGraphMin;
    FInitYGraphMax := Chart.YGraphMax;
    FInitYGraphMin := Chart.YGraphMin;
end;

procedure TFormMain.ActionSelectAllPointsAsCurvePositionsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.SelectAllPointsAsCurvePositions;
end;

procedure TFormMain.ActionComputCurvePositionsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.ComputeCurvePositions;
end;

procedure TFormMain.ActionComputeRFactorBoundsExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');
    //  Into the mode for picking the R-factor intervals.
    if FitClientApp_.FitClient.SelectionMode <> ModeSelectRFactorBounds then
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := ModeSelectRFactorBounds;
        ShowHint(HintFirst);
    end;
    //??? The menu item should be disabled until the calculation finishes;
    //  check every similar case.
    FitClientApp_.FitClient.ComputeCurveBounds;
end;

procedure TFormMain.ActionSelectRFactorBoundsManuallyExecute(Sender: TObject);
var
    NewMode: TSelMode;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    //  WHETHER THIS ENTERS OR LEAVES is action_state's answer, not this
    //  window's: the same entry both starts and stops the mode, and the rule
    //  was written out once per entry until it was asked for instead.
    NewMode := ModeAfterPicking(FitClientApp_.FitClient.SelectionMode, ModeSelectRFactorBounds);
    if NewMode = ModeSelectNothing then
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing
    else
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := NewMode;
        ShowHint(HintFirst);
    end;
end;

{ ------------------------------- IUiHost ---------------------------------- }

procedure TFormMain.ShowMessage(const ATitle, AText: string;
    AKind: TUiMessageKind);
const
    Kinds: array[TUiMessageKind] of TMsgDlgType =
        (mtInformation, mtWarning, mtError);
begin
    MessageDlg(ATitle, AText, Kinds[AKind], [mbOK], 0);
end;

function TFormMain.Confirm(const ATitle, AText: string): boolean;
begin
    Result := MessageDlg(ATitle, AText, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TFormMain.AskText(const ATitle, APrompt: string;
    var AValue: string): boolean;
begin
    Result := InputQuery(ATitle, APrompt, AValue);
end;

{ Starts the two-pick gesture for a module's own point set - the same shape the
  fitting intervals use, so it is one users already know.

  FActiveNumber is captured here because it is the WINDOW's state: which curve
  the chart considers active. A module cannot know about it and should not. }
procedure TFormMain.BeginPointPicking(const APointSet, AMenuId: string;
    APicksPerGesture: longint; const AHint: string);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    if FModulePickSet = APointSet then
    begin
        //  The rest of leaving - the tick, and forgetting the set - is done by
        //  ApplyPickingCaptions, which sees every way the mode can end and not only
        //  this one.
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
        LogUiAction('module markup mode left');
        Exit;
    end;

    FActiveNumber := FFitViewer.GetActiveCurveIndex;
    //  Before the mode is entered: the state poll that ticks the entry can run
    //  as soon as the client's mode changes, and it ticks THIS entry.
    FModulePickSet := APointSet;
    FModulePickMenuId := AMenuId;
    FModulePicksPerGesture := APicksPerGesture;
    FitClientApp_.FitClient.BeginModuleSelection(APointSet);
    //  Ticked here as well as by the poll, so the entry answers the click that
    //  opened the mode rather than a timer some milliseconds later.
    SetMenuChecked(AMenuId, True);
    LogUiAction('module markup mode entered: ' + APointSet);
    ShowHint(AHint);
end;

{ THE ENTRY AND THE BUTTON, because a module's command is drawn twice.

  The name is the menu's, and it was the truth while the menu was the only place
  a module could put anything. It is not any more: the same declaration can ask
  for a button in the Tools pane, that button follows the command table, and a
  module speaking only to the menu item left the pane showing the state the
  window was built with. So both are written here - the table rather than the
  button, because a module may speak while a menu is open and the window applies
  the table on the next poll. }
procedure TFormMain.SetMenuEnabled(const AId: string; AEnabled: boolean);
var
    Item: TMenuItem;
begin
    Item := ModuleItemById(AId);
    if Assigned(Item) then
        Item.Enabled := AEnabled;
    if Assigned(FCommands) then
        FCommands.SetModuleEnabled(AId, AEnabled);
end;

procedure TFormMain.SetMenuChecked(const AId: string; AChecked: boolean);
var
    Item: TMenuItem;
begin
    Item := ModuleItemById(AId);
    if Assigned(Item) then
        //  A module declares what its entries are, and a module can ask for an
        //  entry that is not a toggle to be ticked. Through the one place that
        //  says what a tick may cost.
        SetMenuEntryChecked(Item, AChecked);
    if Assigned(FCommands) then
        FCommands.SetModuleChecked(AId, AChecked);
end;

procedure TFormMain.ActionSelectCurvePositionsManuallyExecute(Sender: TObject);
var
    NewMode: TSelMode;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    //  WHETHER THIS ENTERS OR LEAVES is action_state's answer, not this
    //  window's: the same entry both starts and stops the mode, and the rule
    //  was written out once per entry until it was asked for instead.
    NewMode := ModeAfterPicking(FitClientApp_.FitClient.SelectionMode, ModeSelectCurvePositions);
    if NewMode = ModeSelectNothing then
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing
    else
    begin
        FActiveNumber := FFitViewer.GetActiveCurveIndex;
        FitClientApp_.FitClient.SelectionMode := NewMode;
        ShowHint(HintFirst);
    end;
end;

procedure TFormMain.ActionSetMaximumRFactorExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(SetMaximumRFactorDlg, 'the dialog that asks for the maximum R-factor');

    SetMaximumRFactorDlg.FValue := FitClientApp_.FitClient.MaxRFactor;
    if SetMaximumRFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.MaxRFactor := SetMaximumRFactorDlg.FValue;
end;

procedure TFormMain.ActionSetBackgroundFractionExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(InputBackFactorDlg, 'the dialog that asks for the background factor');

    InputBackFactorDlg.FValue := FitClientApp_.FitClient.BackFactor;
    if InputBackFactorDlg.ShowModal = mrOk then
        FitClientApp_.FitClient.BackFactor := InputBackFactorDlg.FValue;
end;

procedure TFormMain.ActionSmoothProfileExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    //  sglazhivanie mozhno primenyat' posledovatel'no neskol'ko raz
    FitClientApp_.FitClient.SmoothProfile;
end;

procedure TFormMain.ActionStopFitExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.StopAsyncOper;
end;

procedure TFormMain.ActionViewMarkersExecute(Sender: TObject);
begin
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    SetMenuEntryChecked(MenuViewMarkers, not MenuViewMarkers.Checked);
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


procedure TFormMain.FormDestroy(Sender: TObject);
begin
    //  Anything this form queued for the main loop - a menu rebuild, an error
    //  dialog - must not reach a form that is being torn down. The queue holds
    //  bare method pointers, so a call left in it would run against freed fields.
    Application.RemoveAsyncCalls(Self);
    Application.RemoveOnIdleHandler(DeferredUiIdle);
    //  Only if it is still this form's: compared as a method record, because a
    //  bare comparison of an event with a method would call the event.
    if TMethod(Screen.OnActiveControlChange).Data = Pointer(Self) then
        Screen.OnActiveControlChange := nil;

    WriteSettings;
    //  Before the settings above would be wrong: ShowDocument writes the
    //  remembered project into them, and the workflow is what calls it.
    FProjectFlow.Free; FProjectFlow := nil;
    //  After RemoveAsyncCalls above: nothing can ask it anything now.
    FDeferred.Free; FDeferred := nil;
    //  The command table owns nothing the widgets own: it holds declarations
    //  and the state last polled, and the widgets it names are the form's.
    FCommands.Free; FCommands := nil;
    FWidgetNames.Free; FWidgetNames := nil;
    FMenuNames.Free; FMenuNames := nil;
    FSettings.Free; FSettings := nil;
    FFitViewer.Free; FFitViewer := nil;
    //  Created lazily on the first outline refresh, so it is freed here rather
    //  than being assumed to exist.
    FOutlineGuids.Free; FOutlineGuids := nil;
    FCurveGrid.Free; FCurveGrid := nil;
    //  The loss chart is the form's and holds only method pointers to it.
    FProgressAxis.Free; FProgressAxis := nil;
end;

procedure TFormMain.GridDataEditingDone(Sender: TObject);
var PrevXValue, PrevYValue, NewXValue, NewYValue: Double;
    AllData: Boolean;
begin
    CheckAssigned(Sender, 'the control that raised this event');

    //  THE BACKGROUND TABLE IS APPLIED TOO. It returned here, under a comment
    //  saying typed background points were not possible yet - while the table
    //  accepted typing, so a value was shown and silently thrown away. The
    //  client's ReplacePointInBackground below edits the set and tells the
    //  engine, and is tested (testcase_fit_client_view).
    try
        //  Called three times over for some reason, hence the flag.
        if not FEditDone then
            with Sender as TNumericGrid do
            begin
                CheckThat(ColCount >= 2, 'the parameter grid must have a name column and a value column');
                CheckThat(RowCount >= 1, 'the parameter grid must have its heading row');

                FEditDone := True;
                //  WHAT AN EDIT MEANS is in grid_edit: a point whose either
                //  cell changed moves; a row typed from nothing waits for both.
                AllData := RowEditApplies(FSavedPos, FSavedAmp,
                    Cells[0, Row], Cells[1, Row]);

                if AllData then
                begin
                    //  REFUSED IN WORDS, not read as zero: a typo used to move
                    //  the point to the origin. Raised as a user exception so
                    //  the balloon below says it beside the cell.
                    FHintMessage := RowEditRefusal(Cells[0, Row], Cells[1, Row]);
                    if FHintMessage <> '' then
                    begin
                        //  And the row shows the point where it still is.
                        Cells[0, Row] := FSavedPos;
                        Cells[1, Row] := FSavedAmp;
                        raise EUserException.Create(FHintMessage);
                    end;
                    PrevXValue := EditedValue(FSavedPos);
                    NewXValue := EditedValue(Cells[0, Row]);
                    PrevYValue := EditedValue(FSavedAmp);
                    NewYValue := EditedValue(Cells[1, Row]);

                    CheckAssigned(FitClientApp_, 'the client application object');
                    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

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

        on E: EUserException do
        //  SUCH EXCEPTIONS DO NOT REACH THE LOG.
        begin
            HandleEditHint := True; //  vklyuchaetsya taymer vyvoda soobscheniya
            FSenderEditHint := TNumericGrid(Sender);
            FHintMessage := E.Message;
        end;

    end;
end;

{ A cell of the Curve Positions or the Fit Intervals table was typed over: the
  pick it shows moves to the nearest sample of the typed position. What may
  move where, and what is refused, is TFitClient.MovePick's - points_tables
  decides it. This reads the cell, reports a refusal, and puts the cell back
  when nothing moved. }
procedure TFormMain.GridPicksSelectEditor(Sender: TObject; aCol, aRow: Integer;
    var Editor: TWinControl);
begin
    CheckThat(Sender is TNumericGrid, 'the editor was selected in a table');
    FSavedCellText := TNumericGrid(Sender).Cells[aCol, aRow];
    FEditDone := False;
end;

procedure TFormMain.GridPicksEditingDone(Sender: TObject);
var
    Grid: TNumericGrid;
    Kind: TPickKind;
    Index: longint;
    Typed, Why: string;
    Moved: boolean;
begin
    CheckThat(Sender is TNumericGrid, 'the edit was made in a table');
    Grid := TNumericGrid(Sender);
    //  Raised more than once per edit, as for the other tables.
    if FEditDone then
        Exit;
    FEditDone := True;
    Typed := Grid.Cells[Grid.Col, Grid.Row];
    if Typed = FSavedCellText then
        Exit;

    Why := '';
    Moved := False;
    if Grid = GridSpecPositions then
    begin
        Kind := pkCurvePositions;
        Index := PositionsCellPick(Grid.Row);
        //  THE POSITION IS WHAT MOVES: the amplitude beside it is the
        //  profile's value at that position, not something to type.
        if Grid.Col <> 0 then
        begin
            Why := 'Only the position can be typed; the amplitude is the ' +
                'profile''s value at it.';
            Index := -1;
        end;
    end
    else
    begin
        Kind := pkFitBounds;
        Index := IntervalCellPick(Grid.Col, Grid.Row);
    end;

    if Index >= 0 then
        try
            Moved := FitClientApp_.FitClient.MovePick(Kind, Index, Typed, Why);
        except
            //  The engine refuses a move while a fit runs - in words.
            on E: EUserException do
                Why := E.Message;
        end;

    if not Moved then
    begin
        Grid.Cells[Grid.Col, Grid.Row] := FSavedCellText;
        if Why <> '' then
            MessageDlg('The point was not moved', Why, mtInformation, [mbOK], 0);
    end;
end;

{ A value was typed into the Curve Attributes table: sent to the engine for that
  parameter of that curve by TFitClient.EditCurveParameter, which also says
  what is refused. The engine's rebuilt model refills the table; a refused cell
  is put back as it was. }
procedure TFormMain.GridParametersEditingDone(Sender: TObject);
var
    Grid: TNumericGrid;
    Typed, Why: string;
    Applied: boolean;
begin
    CheckThat(Sender is TNumericGrid, 'the edit was made in a table');
    Grid := TNumericGrid(Sender);
    if FEditDone then
        Exit;
    FEditDone := True;
    Typed := Grid.Cells[Grid.Col, Grid.Row];
    if Typed = FSavedCellText then
        Exit;
    Why := '';
    Applied := False;
    try
        Applied := FitClientApp_.FitClient.EditCurveParameter(
            Grid.Row - Grid.FixedRows, Grid.Col - Grid.FixedCols, Typed, Why);
    except
        //  The engine refuses a change while a fit runs - in words.
        on E: EUserException do
            Why := E.Message;
    end;
    if not Applied then
    begin
        Grid.Cells[Grid.Col, Grid.Row] := FSavedCellText;
        if Why <> '' then
            MessageDlg('The value was not changed', Why, mtInformation, [mbOK], 0);
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
    //  WITH ShowBalloon an exception must not be allowed to escape the event
    //  handler.
    ShowBalloon(Handle, WideString(FHintMessage), WideString(''));
{$else}
    MessageDlg(FHintMessage, mtError, [mbOk], 0);
{$endif}
end;

{$hints off}
procedure TFormMain.GridDataSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
    //  THE GRID BEING EDITED, not always the Data grid. Both editable tables
    //  share this handler, and it read GridData whichever raised it - so an
    //  edit in the Background Points table started from the Data table's row,
    //  and the point it asked to move was one the background did not have.
    CheckThat(Sender is TNumericGrid, 'the editor was selected in a table');
    with TNumericGrid(Sender) do
    begin
        CheckThat(ColCount >= 2, 'the table must have a position and an amplitude column');
        CheckThat(RowCount >= 1, 'the table must have its heading row');

        FSavedPos := Cells[0, aRow];
        FSavedAmp := Cells[1, aRow];
    end;
    FEditDone := False;
end;
{$hints on}

procedure TFormMain.FormCreate(Sender: TObject);

    function NewIdentityItem(AOwner: TComponent): TMenuItem;
    begin
        Result := TMenuItem.Create(AOwner);
        //  Created twice - the menu bar's and the chart popup's - so the second
        //  takes the register's collision suffix. That is the one place a digit
        //  in a generated name means something: "the other one of these".
        Result.Name := FWidgetNames.NameFor('MenuAxisIdentity', '');
        Result.Caption := 'General Position';
        Result.OnClick := MenuIdentityClick;
    end;

    function NewCustomItem(AOwner: TComponent): TMenuItem;
    begin
        Result := TMenuItem.Create(AOwner);
        //  Created twice - the menu bar's and the chart popup's - so the second
        //  takes the register's collision suffix. That is the one place a digit
        //  in a generated name means something: "the other one of these".
        Result.Name := FWidgetNames.NameFor('MenuAxisCustom', '');
        Result.Caption := 'Custom Position...';
        Result.OnClick := MenuCustomAxisClick;
    end;

    function NewCurveAxisItem(AOwner: TComponent): TMenuItem;
    begin
        Result := TMenuItem.Create(AOwner);
        //  Created twice - the menu bar's and the chart popup's - so the second
        //  takes the register's collision suffix. That is the one place a digit
        //  in a generated name means something: "the other one of these".
        Result.Name := FWidgetNames.NameFor('MenuAxisFromCurveType', '');
        Result.Caption := 'From Curve Type';
        Result.OnClick := MenuCurveAxisClick;
    end;

var
    MenuRisks: string;
    ClearModelEntry: TMenuItem;
    i: longint;
begin
    //  THE NAME REGISTERS FIRST, before anything generated takes a name from
    //  one. They used to be created with the command table, which is built
    //  after the axis entries - so those entries asked a register that did not
    //  exist yet. That is the same ordering trap as the panel's, and it fails
    //  the same way: inside FormCreate, before there is a window to say so.
    FWidgetNames := TWidgetNames.Create;
    FMenuNames := TWidgetNames.Create;

    //  FIRST, before anything below can want it. Everything that reports a
    //  refusal or a fault does so through this queue, including the settings
    //  read further down, so it has to exist before any of that runs.
    FDeferred := TDeferredUi.Create;
    //  WORK PUT OFF BEHIND AN OPEN MENU is taken up when the application goes
    //  idle - which it does once the menu's own loop has ended - not by asking
    //  a few times a second whether it has.
    Application.AddOnIdleHandler(DeferredUiIdle);
    //  The document commands. This window is the IProjectHost they reach it
    //  through - it supplies the dialogs and the captions, and decides none
    //  of the sequences.
    FProjectFlow := TProjectWorkflow.Create(
        FitClientApp_.FitClient.FitService, Self);
    Application.OnException := OnException;
    //  The parameter table's presenter. Owns no data - FCurveList is the
    //  model, and the service owns that - so it lives as long as the form.
    FCurveGrid := TCurveListGrid.Create;
    //  Fit or Fit Pro, decided by the build. Set HERE, before the caption below
    //  is made from it: this runs inside Application.CreateForm, so an
    //  assignment in Fit.lpr after CreateForm came too late and Fit Pro opened
    //  titled "Fit". The About box and the macOS menu read the same property.
    ApplicationProperties.Title := GetProductName;
    //  The build is in the title from the first frame the user sees, so a
    //  screenshot in a bug report always says which binary produced it.
    Caption := GetWindowTitle(ApplicationProperties.Title, '');
    //  Let the background calculation thread report fatal errors to the user.
    main_calc_thread.OnCalcError := ShowCalcError;

    //  'From Curve Type': the default axis option. It is a visible menu item, not
    //  hidden behaviour, so the user can see WHY the caption changed with the
    //  model - and can override it with any of the items below.
    FMenuCurveAxis := NewCurveAxisItem(Self);
    MenuUseRule.Add(FMenuCurveAxis);
    FMenuCurveAxisPopup := NewCurveAxisItem(Self);
    PopupViewMode.Items.Add(FMenuCurveAxisPopup);

    //  'General Position' axis option, added next to the diffraction modes in
    //  both the main menu and the chart popup (raw argument, no wavelength needed).
    FMenuIdentity := NewIdentityItem(Self);
    MenuUseRule.Add(FMenuIdentity);
    FMenuIdentityPopup := NewIdentityItem(Self);
    PopupViewMode.Items.Add(FMenuIdentityPopup);

    //  'Custom Position...' axis option: opens a dialog to define a display
    //  formula and its inverse; the axis is presentational only (D5).
    FMenuCustom := NewCustomItem(Self);
    MenuUseRule.Add(FMenuCustom);
    FMenuCustomPopup := NewCustomItem(Self);
    PopupViewMode.Items.Add(FMenuCustomPopup);

    //  AND ATTACHED TO THE CHART, which it never was. Its six entries were
    //  created here, declared checkable in DeclareCheckableMenuEntries and kept
    //  ticked by ApplyViewMode - and no control's PopupMenu ever named it, so
    //  none of it was reachable. It could not be attached before either: a
    //  right-click placed a point, so opening a menu would have edited the
    //  model. ChartMouseUp asks pick_target which buttons pick now, so the
    //  right button is free.
    Chart.PopupMenu := PopupViewMode;
    CheckThat(Assigned(Chart.PopupMenu),
        'the argument-axis menu is attached to the chart');

    BuildLossMenu;
    BuildPriceDataMenu;
    //  Before any menu is built from the registered types, and before anything
    //  can create a curve. Raises naming what is missing, rather than leaving
    //  the application running with a type it cannot build (Stage 3D).
    //  THE USER GUIDE, before anything else explains itself: Explain Everything
    //  lists namespaces in the order they registered, and a reader meets the
    //  guide's chapters first, then what the curve types and modules say. The
    //  fit-progress topics come with it - every build can fit.
    RegisterClientExplanations;
    //  The modules this build contains, before the check that they are linked.
    //  Which they are is decided by app_modules alone - see that unit.
    RegisterAppModules;
    RegisterAllCurveTypes;
    //  Same reason, for the other extension point that decides what this build
    //  can do: the Open dialog's filter is derived from what registered, so it
    //  cannot offer a format nothing here can read.
    RegisterAllDataLoaders;
    //  And the engines, before the menu that offers them is built.
    RegisterAllMinimizers;
    //  After registration, since it is built from what registered.
    BuildMinimizerMenu;

    //  After the modules have registered, since it is built from what they
    //  declare. A module is added or removed in exactly one place - the
    //  registration above - and its menu, panel and pick mode follow.
    BuildModuleMenus;

    //  DELETING ONE CURVE, as an action of its own. Created here rather than in
    //  the .lfm for the reason the module menus and the axis entries are:
    //  additive, and the designed form stays untouched.
    FActionDeleteCurve := TAction.Create(Self);
    FActionDeleteCurve.Name := 'ActionDeleteCurve';
    FActionDeleteCurve.Caption := 'Delete Curve';
    FActionDeleteCurve.Hint := 'Removes this curve from the model';
    FActionDeleteCurve.OnExecute := ActionDeleteCurveExecute;
    FActionDeleteCurve.ActionList := ActionList;

    //  CLEARING THE WHOLE MODEL. The LAST entry of the Model menu, after a
    //  separator and after any module's submenu: a destructive command at the
    //  end of a menu is where it is least often hit by accident, and the
    //  ellipsis says a question comes before anything happens.
    FActionClearModel := TAction.Create(Self);
    FActionClearModel.Name := 'ActionClearModel';
    FActionClearModel.Caption := 'Clear Model...';
    FActionClearModel.Hint := 'Removes every curve from the model, after asking';
    FActionClearModel.OnExecute := ActionClearModelExecute;
    FActionClearModel.ActionList := ActionList;
    ClearModelEntry := TMenuItem.Create(Self);
    ClearModelEntry.Caption := '-';
    MenuModel.Add(ClearModelEntry);
    ClearModelEntry := TMenuItem.Create(Self);
    ClearModelEntry.Name := 'MenuClearModel';
    ClearModelEntry.Action := FActionClearModel;
    MenuModel.Add(ClearModelEntry);

    //  AFTER the modules have declared their menus and after the action above
    //  exists: the table resolves every name it holds, and refuses one that
    //  answers to nothing.
    BuildCommandTable;

    //  AND THEN THE PANE THE TABLE FILLS. After BuildCommandTable, because
    //  every button it makes is a row of that table.

    //  AFTER the table, because the Model panel's context menu is built from
    //  it - and BEFORE the panels, because both are drawn from it.
    BuildRightPanelTabs;
    BuildLeftPanelTabs;
    //  AFTER the modules registered, since it asks each whether it reports.
    BuildReportTabs;

    //  After every menu exists and before the window is shown, so no entry has
    //  a handle yet and none has to be rebuilt to gain a check mark.
    DeclareCheckableMenuEntries;

    //  And then said, once, whether the rule that procedure exists for actually
    //  holds over the menus as built - including the ones a module contributed,
    //  which this window does not write and cannot check by reading itself. A
    //  breach is a dangling menu waiting to happen, and it is invisible until it
    //  happens, so it is named here rather than found again from a screenshot.
    MenuRisks := MenuEntriesAtRiskOfDangling(Self);
    if MenuRisks <> '' then
        WriteLog('menu entries that are both a submenu parent and tickable, ' +
            'which is how a submenu is left on screen with nothing under it: ' +
            MenuRisks, Warning);

    //  A panel of its own, so the fit-configuration summary is always visible
    //  and never competes with the transient mode hints in the panel beside it.
    //  Taken from the hint panel's width rather than widening the bar.
    FAdvicePanel := -1;
    if StatusBar.Panels.Count >= 3 then
    begin
        FAdvicePanel := StatusBar.Panels.Add.Index;
        //  The widths themselves are worked out from the text and the bar's
        //  actual width, every time either could have changed.
        StatusBar.OnResize := StatusBarResize;
        StatusBarResize(StatusBar);
    end;

    CheckListBoxLegend.OnResize := CheckListBoxLegendResize;

    FFitViewer := TFitViewer.Create(nil);
    //  Every model change the client reports reaches the window through the
    //  viewer, so that is where the window learns it must refresh its state.
    FFitViewer.OnStateChanged := StateChangedByViewer;
    FFitViewer.Form := Self;
    FFitViewer.SetFitClient(FitClientApp_.FitClient);
    FFitViewer.SetViewMarkers(MenuViewMarkers.Checked);
    FFitViewer.Clear(Self);
    BuildFitProgressView;

    FActiveNumber := -1;

    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.OnAsyncOperationFinished := AsyncOperationFinished;

    //  THE EVENTS THAT CHANGE WHAT THE WINDOW OFFERS, each asking for one
    //  refresh. This replaced a timer that re-derived everything twice a second
    //  whether anything had changed or not.
    ActionList.OnExecute := ActionListExecuteAny;
    Screen.OnActiveControlChange := ActiveControlChanged;
    for i := 0 to ComponentCount - 1 do
        if (Components[i] is TNumericGrid) and
            not Assigned(TNumericGrid(Components[i]).OnSelection) then
            TNumericGrid(Components[i]).OnSelection := GridSelectionChanged;

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
    //  A palette index (clBtnFace and the like) cannot be passed as a colour:
    //  DrawReticule converts it to a line colour incorrectly. It would have to be
    //  converted to an RGB colour first.
    Chart.AxisColor := clGray;   //clBtnFace;    //$00b99d7f;

    //  Makes them invisible while Visible stays True.
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

    //  The parameters table says what every number IS - fitted, fixed,
    //  computed - which until now the user could only infer from a cell
    //  refusing to be edited.
    GridParameters.OnGetCellColor := GridParametersGetCellColor;
    BuildParameterLegend;

    //  u vertikal'nyh ScrollBar'ov a rantayme
    //  koordinaty ustanavlivayutsya nepravil'no
    ScrollBarY.Top := 7;
    ScrollBarY.Left := PanelChart.ClientWidth - 24;
    ScrollBarY.Width := 17;
    ScrollBarY.Height := PanelChart.ClientHeight - 24 - 7;
    //  Moves the initial input focus off the scroll bar.
    ActiveControl := CheckListBoxLegend;

    //??? this does not work here
    //Screen.Cursors[crCursorDrag] := LoadCursor(HInstance, 'CURSORDRAG');
    //Screen.Cursors[crCursorSelect] := LoadCursor(HInstance, 'CURSORSELECT');

    //Chart.Cursor := crCross;//crCursorDrag;
    //Windows.SetCursor(crCursorDrag);
    //Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
    FSettings := Settings_v1.Create(nil);
    ReadSettings;
    //  The recent list is in those settings, and the menu has to carry it from
    //  the first frame: a user who opens File before opening anything is
    //  exactly who the list is for.
    RefreshRecentMenu;
    RestoreViewMode;
    //  Restore the chosen minimizer; an unknown persisted value falls back to the
    //  always-available Downhill Simplex.
    ApplyLossKind(FSettings.LossKind);
    //  How a price-data file is read, as last chosen.
    ApplyPriceData(FSettings.PriceValueColumn, FSettings.PriceArgument);
    //  Not announced: a dialog on every startup for a setting the user chose
    //  long ago is nagging, not information. The status bar still says it.
    UpdateFitAdvice(False);
    ApplyMinimizerKind(MinimizerKindOrDefault(FSettings.MinimizerKind));
    RestoreCurveType;
    //  Whether the user last chose to watch the model move during a fit.
    FitClientApp_.FitClient.AnimationMode := FSettings.AnimationMode;
    //  Restore the Python backend's residual weighting (ignored by the native
    //  one). Normalised rather than compared: a settings file older than the
    //  setting carries an empty string, and what that means is fit_weighting's
    //  answer - which is the sidecar's.
    ApplyWeighting(WeightingOrDefault(FSettings.Weighting));
    //  Restore the compute server (empty = fit in-process).
    ApplyServerUrl(FSettings.ServerUrl);
    //  Load the stored user-defined curve types first; CreateCurveTypeMenus now
    //  appends them itself, so the menu is complete after one call.
    ReadUserCurves;
    CreateCurveTypeMenus;

    //  macOS: Cmd rather than Ctrl, and About where a Mac user looks for it.
    //  Before the menu's handle exists, which is when the application menu is
    //  built - and the widget set reads what it is given exactly once.
    ApplyMacMenuConventions(Self, MainMenu, ActionList, MenuAbout);
    //  What the window offers once it is built.
    RequestStateRefresh;

    //  From here on every repaint reports what it cost. No switch turns this on:
    //  see client_log and log.DEFAULT_LOG_LEVEL.
    Chart.OnPaintTiming := ChartPaintTiming;
end;

{ A repaint that has become slow is invisible from any other vantage point: it
  costs no server call, so the server log stays clean, and it falls between two
  user actions, so the UI-action tier cannot show it either. That is exactly how
  a chart drawing itself one pixel at a time - hundreds of thousands of blocking
  X round trips per repaint - stayed unattributed while every operation in the
  application lagged for seconds.

  So the duration is logged, at the trace tier, on every repaint. ADetail names
  the individual series that took any measurable time, so the next time this
  happens the log says which one. }
procedure TFormMain.ChartPaintTiming(ADurationMs: Int64; const ADetail: string);
begin
    LogClientTrace(Format('chart repaint %d ms%s', [ADurationMs, ADetail]));
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
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    FitClientApp_.FitClient.SubtractBackground(Auto);
end;

procedure TFormMain.ScrollBarXChange(Sender: TObject);
var
    Window: TAxisWindow;
begin
    if Chart.SeriesCount = 0 then
        Exit;
    Window := WindowForBarPosition(
        AxisWindow(FInitXGraphMin, FInitXGraphMax,
            Chart.XGraphMin, Chart.XGraphMax),
        BarRange(ScrollBarX.Min, ScrollBarX.Max, ScrollBarX.Position),
        False);
    //  THE UPPER END FIRST, as it always was here: the chart holds one end
    //  against the other as each is written, so writing the lower end of a
    //  window that is moving right would be undone by the old upper end.
    Chart.XGraphMax := Window.ViewMax;
    Chart.XGraphMin := Window.ViewMin;
    Chart.Invalidate;
end;

procedure TFormMain.ScrollBarYChange(Sender: TObject);
var
    Window: TAxisWindow;
begin
    if Chart.SeriesCount = 0 then
        Exit;
    Window := WindowForBarPosition(
        AxisWindow(FInitYGraphMin, FInitYGraphMax,
            Chart.YGraphMin, Chart.YGraphMax),
        BarRange(ScrollBarY.Min, ScrollBarY.Max, ScrollBarY.Position),
        True);
    //  The lower end first here, for the same reason mirrored.
    Chart.YGraphMin := Window.ViewMin;
    Chart.YGraphMax := Window.ViewMax;
    Chart.Invalidate;
end;

procedure TFormMain.TabSheetBackgroundResize(Sender: TObject);
begin
    //  PUTTING THE SIZING HERE HANGS THE APPLICATION.
    //GridBackground.Top := 8;
    //GridBackground.Left := 8;
    //GridBackground.Height := TabSheetBackground.ClientHeight - 16;
end;

{ A ROW OF THE LEGEND IS AS TALL AS ITS TEXT, not 13 pixels.

  ItemHeight came from the .lfm as 13, and the LCL does scale it - to 23 on a
  192 dpi display. What it cannot know is that the font is no longer the 6.7 pt
  one the form was drawn with: following the desktop's font made the text 27
  pixels tall inside a 23 pixel row, so the legend stayed cramped and clipped
  while the tables around it grew. A fixed ItemHeight is a guess about a font,
  the same mistake as a fixed panel width.

  The row also sets the size of the two swatches - CheckListBoxLegendDrawItem
  takes them from ARect - so they follow the text for free. }
{ Height from the font, for the same reason as the legend list above: 22 was
  written for a 6.7 pt font and the LCL can only scale that number, not notice
  that the font it was chosen for is gone. Called from the tab's OnShow, which
  is the first point at which the font is final AND the panel matters. }
procedure TFormMain.SizeParameterLegend;
var
    Wanted: integer;
begin
    if not Assigned(FParameterLegend) then
        Exit;
    Wanted := FParameterLegend.Canvas.TextHeight('Wg') +
        FParameterLegend.Scale96ToFont(8);
    if FParameterLegend.Height <> Wanted then
        FParameterLegend.Height := Wanted;
end;

procedure TFormMain.CheckListBoxLegendResize(Sender: TObject);
var
    Wanted: integer;
begin
    //  'Wg' rather than the widest item: ascender and descender together are
    //  what decides the height, and they do not depend on the series names.
    Wanted := CheckListBoxLegend.Canvas.TextHeight('Wg') +
        CheckListBoxLegend.Scale96ToFont(4);
    if CheckListBoxLegend.ItemHeight <> Wanted then
        CheckListBoxLegend.ItemHeight := Wanted;
end;

{ THE PANELS ARE MEASURED, NOT COUNTED OUT IN PIXELS.

  The three widths in the .lfm (268, 201, 557) plus a fourth of 400 added in
  code came to more than the window is wide once the form was scaled, so the
  right-hand panels were squeezed and the mode hint arrived as
  "Drag mouse from top-left to bottom-r...". Fixed pixel widths are a guess
  about a font, and this application now follows the desktop's font, whose size
  is not ours to predict.

  So the two panels whose contents have a known longest form are sized to that
  text in the bar's own font, and the two that hold free prose - the transient
  mode hint and the fit-configuration summary - divide whatever is left. }
{ The samples the two measured panels are sized to moved to status_readout, with
  the readouts whose shape they stand for: a sample and the format string it
  stands for are one fact, and a format widened without its sample gives a panel
  too narrow for its own text. }
procedure TFormMain.StatusBarResize(Sender: TObject);
var
    Padding: integer;
    HintWidth, AdviceWidth: longint;
begin
    if StatusBar.Panels.Count < 3 then
        Exit;
    //  Breathing space either side of the text, so a panel never renders its
    //  own contents flush against the divider.
    Padding := StatusBar.Scale96ToFont(16);
    StatusBar.Panels[0].Width :=
        StatusBar.Canvas.TextWidth(ElapsedSample) + Padding;
    StatusBar.Panels[1].Width :=
        StatusBar.Canvas.TextWidth(StatsSample) + Padding;

    ProsePanelWidths(StatusBar.ClientWidth, StatusBar.Panels[0].Width,
        StatusBar.Panels[1].Width, FAdvicePanel >= 0, HintWidth, AdviceWidth);
    StatusBar.Panels[2].Width := HintWidth;
    if FAdvicePanel >= 0 then
        StatusBar.Panels[FAdvicePanel].Width := AdviceWidth;
end;

{ The tables are positioned by hand rather than by Align because they are only
  laid out when their tab is first shown - see the five OnShow handlers.

  THE MARGINS ARE SCALED. They were written as 8 and 4 device pixels, which is a
  margin at 96 dpi and a hairline at 192: on a scaled display every table sat
  hard against the edge of its panel. Worse, this runs on tab Show, which is
  AFTER TCustomForm.AfterConstruction has scaled the form - so these four
  literals did not merely fail to scale, they overwrote the scaled geometry with
  design-time pixels every time the user changed tabs. }
procedure TFormMain.InsetGridInPanel(AGrid: TControl; APanel: TWinControl);
var
    InsetLeft, InsetTop, InsetRight, InsetBottom: integer;
begin
    CheckAssigned(AGrid, 'the grid being inset into a panel');
    CheckAssigned(APanel, 'the panel the grid is inset into');

    InsetLeft   := Scale96ToForm(4);
    InsetTop    := Scale96ToForm(8);
    InsetRight  := Scale96ToForm(8);
    InsetBottom := Scale96ToForm(8);
{$ifdef windows}
    LockWindowUpdate(Handle);       //  much less flicker on Windows
{$endif}
    AGrid.SetBounds(InsetLeft, InsetTop,
        APanel.ClientWidth - InsetLeft - InsetRight,
        APanel.ClientHeight - InsetTop - InsetBottom);
{$ifdef windows}
    LockWindowUpdate(0);
{$endif}
end;

procedure TFormMain.TabSheetBackgroundShow(Sender: TObject);
begin
    InsetGridInPanel(GridBackground, PanelBackground);
end;

{ Re-aims a pick that the crosshair snapped to the wrong series.

  THE CROSSHAIR IS NOT THE PICK. TTAChart snaps it to the nearest point of ANY
  visible series, and the click was then accepted only if that series was one a
  pick may come from. That held while the chart showed the data alone. It stops
  holding the moment a model is drawn over the data - which is exactly what
  marking the first bounded pattern does - and from then on click after click
  landed on a model series and was dropped in silence: no pick, no message, no
  log line, and a mode that looked like it had stopped working. The second
  pattern could not be marked at all (VM logs, 2026-08-21).

  So a click that belongs to no pickable series is taken as what it plainly is -
  a click on the data underneath - and aimed at the nearest point of the active
  series. Clicks that already belong to a pickable series are left alone: on the
  collected set, a click TAKES A PICK BACK, and re-aiming it would turn removing
  a point into adding one. }
procedure TFormMain.AimPickAtActiveSerie;
var
    Serie: TComponent;
    Xs, Ys: array of longint;
    i, Count, Nearest: longint;
begin
    //  Both indices were taken when the chart held other series than it holds
    //  now - the crosshair's before this click, the active one when the mode
    //  was entered - and curves come and go between those moments.
    if (FCurSerieIndex < 0) or (FCurSerieIndex >= Chart.SeriesCount) then
        Exit;
    if (FActiveNumber < 0) or (FActiveNumber >= Chart.SeriesCount) then
        Exit;
    if IsPickableSerie(FCurSerieIndex, FActiveNumber,
        FFitViewer.GetPointsSet(FCurSerieIndex) =
        FitClientApp_.FitClient.GetCurrentPointsSet) then
        Exit;

    Serie := Chart.GetSerie(FActiveNumber);
    if not (Serie is TTASerie) then
        Exit;

    //  Image coordinates, because that is what the user aimed at: the pointer
    //  is over pixels, and the point nearest in data units can be a different
    //  one entirely wherever the axes are not equally scaled.
    Count := TTASerie(Serie).Count;
    SetLength(Xs, Count);
    SetLength(Ys, Count);
    for i := 0 to Count - 1 do
    begin
        Xs[i] := TTASerie(Serie).GetXImgValue(i);
        Ys[i] := TTASerie(Serie).GetYImgValue(i);
    end;

    Nearest := NearestPointIndex(Xs, Ys, FUpX, FUpY);
    if Nearest = NO_POINT then
        Exit;

    FCurSerieIndex := FActiveNumber;
    FValueIndex := Nearest;
end;

{ Ends a module's picking gesture and leaves nothing of it behind.

  ONE PLACE, because the gesture ends in more ways than one - the picks are all
  made, the user chooses the entry again, another selection mode starts, a
  profile is loaded - and each of them must put away the same three things: the
  mode, the markers that were drawn for it, and the tick that says it is on.
  Leaving the mode is what removes the markers: the client drops the set the
  picks were collected into.

  Safe to call when the mode has already gone, which is how the state poll uses
  it: then there is only the tick left to put back. }
procedure TFormMain.EndModulePicking;
begin
    if FitClientApp_.FitClient.SelectionMode = ModeSelectModulePoints then
        FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    if FModulePickMenuId <> '' then
        SetMenuChecked(FModulePickMenuId, False);
    FModulePickMenuId := '';
    FModulePickSet := '';
    FModulePicksPerGesture := 0;
end;

{ Whether the picks made so far are a whole one of whatever the module asked
  for.

  Counted from the set the picks were collected into rather than from a counter
  of the window's own: the set is what the markers are drawn from, so one fact
  decides both and there is no second one to fall out of step with it. }
{ The set the given picking mode is collecting into.

  It is not always the selected-points set: the modes that build a lasting set -
  the background, the curve positions, the fitting intervals - add straight to
  it, and how many it already holds is what says where the user is. }
function TFormMain.PicksOfCurrentMode(AMode: TSelMode): TPointsSet;
begin
    case AMode of
        ModeSelectBackground:
            Result := FitClientApp_.FitClient.GetBackgroundPoints;
        ModeSelectCurvePositions:
            Result := FitClientApp_.FitClient.GetCurvePositions;
        ModeSelectRFactorBounds:
            Result := FitClientApp_.FitClient.GetRFactorBounds;
        else
            Result := FitClientApp_.FitClient.GetSelectedPoints;
    end;
end;

function TFormMain.ModuleGestureIsComplete: boolean;
var
    Picks: TPointsSet;
begin
    Result := False;
    //  A gesture with no declared size has no natural end - it runs until the
    //  user leaves it, which is the older behaviour and still a valid one.
    if FModulePicksPerGesture <= 0 then
        Exit;
    Picks := FitClientApp_.FitClient.GetSelectedPoints;
    Result := Assigned(Picks) and (Picks.PointsCount >= FModulePicksPerGesture);
end;

procedure TFormMain.OnChartClick;
var XValue, YValue: Double;
    PointsSet: TPointsSet;
    Entry: TMenuItem;
    Mode: TSelMode;
    PicksSoFar: longint;
    Hint: string;
{$ifdef windows}
//    BE: BalloonException;
    Handle: HWND;
{$endif}
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    try
        //  IF THE MOUSE DID NOT MOVE between two clicks and ChartDrawReticule
        //  was not called, CurSerieIndex and ValueIndex hold wrong values -
        //  hence the flag.
        //
        //  TTAChart.Click could have been rewritten instead, but that risks
        //  trouble with later versions of the library, where TControl.Click may
        //  change; overriding the method alone does not achieve the desired
        //  effect. So a simple extra check was added.
        if FDrawReticule and
            (FDownX = FUpX) and (FDownY = FUpY) and (FActiveNumber <> -1) then
        begin
            FDrawReticule := False;
            //  A click during a picking gesture is a click on the DATA, so it
            //  is re-aimed at the active series when the chart's crosshair
            //  snapped it to some curve drawn over it. See AimPickAtActiveSerie.
            if FitClientApp_.FitClient.SelectionMode <> ModeSelectNothing then
                AimPickAtActiveSerie;
            //  The series that was clicked is compared with the active one.
            //  Only two clicks are accepted: on the active series, whose points
            //  are added to the selected one, or on the selected series, from
            //  which a point is then removed.
            if (FCurSerieIndex = FActiveNumber) or
               (FFitViewer.GetPointsSet(FCurSerieIndex) =
                FitClientApp_.FitClient.GetCurrentPointsSet) then
            begin
                Mode := FitClientApp_.FitClient.SelectionMode;
                if Mode = ModeSelectNothing then
                    Exit;

                //  HOW FAR THROUGH THE GESTURE the user is. Which set holds the
                //  picks depends on the mode - the bounded gestures collect into
                //  the selected-points set, the open-ended ones into the set they
                //  are building - so the count is read here and the rule that
                //  reads it is in pick_guidance.
                PointsSet := PicksOfCurrentMode(Mode);
                CheckAssigned(PointsSet, 'the set the current picking mode collects into');
                PicksSoFar := PointsSet.PointsCount;

                //  A COMPLETED GESTURE IGNORES FURTHER CLICKS. Adding a third
                //  end to a two-ended area is not something the rest of the
                //  program has a meaning for.
                if GestureIsComplete(Mode, PicksSoFar) then
                    Exit;

                Hint := PickHint(Mode, PicksSoFar);
                if Hint <> '' then
                    ShowHint(Hint);

                PointsSet := FFitViewer.GetPointsSet(FCurSerieIndex);

                XValue := PointsSet.PointXCoord[FValueIndex];
                YValue := PointsSet.PointYCoord[FValueIndex];
                FitClientApp_.FitClient.AddPointToActive(XValue, YValue);

                //  A MODULE'S GESTURE ENDS ITSELF once it has the picks the
                //  module said it takes. What the picks made is on the chart by
                //  now, and leaving the mode is what takes their markers off it,
                //  so the finished thing is shown instead of being buried under
                //  the crosses that built it. The mode is not silently kept open
                //  either: a mode that outlives what it was entered for is how
                //  the next stray click becomes a pick nobody meant to make.
                if (FitClientApp_.FitClient.SelectionMode = ModeSelectModulePoints)
                    and ModuleGestureIsComplete then
                begin
                    Entry := ModuleItemById(FModulePickMenuId);
                    EndModulePicking;
                    //  SAID, because a mode that switches itself off is
                    //  otherwise indistinguishable from one that broke - and the
                    //  way back is the same entry, which is what is named.
                    if Assigned(Entry) then
                        ShowHint(Format('Done. %s is off again - choose it to ' +
                            'mark another.', [StringReplace(Entry.Caption, '&',
                            '', [rfReplaceAll])]))
                    else
                        ShowHint('Done. The marking mode is off again.');
                end;
            end;
        end;
    except
        //  A REFUSED PICK IS A MESSAGE, NOT A FAULT - ON EVERY PLATFORM.
        //
        //  This used to catch EUserException under Windows and `raise` under
        //  everything else, which meant that on Linux a deliberate refusal - the
        //  server declining a pick and explaining why - escaped the click handler
        //  and reached TFormMain.OnException. That is the last-resort handler for
        //  faults: it logs at Fatal and STOPS THE STATE POLL, so the user got
        //  their explanation with "Server polling has been stopped" stapled to the
        //  end of it and had to reconnect from the Fit menu. The message was
        //  right and everything around it was wrong.
        //
        //  The balloon is kept where it exists, because it is anchored at the
        //  click and reads better than a dialog; elsewhere the message is queued
        //  and shown from the main loop, which is the same route every other
        //  non-fatal message in this form takes. Neither touches the timer.
        on E: EUserException do
        //  SUCH EXCEPTIONS DO NOT REACH THE LOG.
        begin
{$ifdef windows}
            EditBalloonChart.Left := FUpX;
            EditBalloonChart.Top := FUpY;
                Handle := EditBalloonChart.Handle;
            //  WITH ShowBalloon an exception must not be allowed to escape the
            //  event handler.
            ShowBalloon(Handle, WideString(E.Message), WideString(''));
{$else}
            QueueNotice(E.Message);
{$endif}
        end;
    end;
end;

{$hints off}
procedure TFormMain.ChartDrawReticule(Sender: TComponent; IndexSerie, Index,
    Xi, Yi: Integer; Xg, Yg: Double);
begin
    FCurSerieIndex := IndexSerie;
    FValueIndex := Index;
    FDrawReticule := True;
    LabelPositionValue.Caption := CoordinateReadout(Xg);
    LabelIntensityValue.Caption := CoordinateReadout(Yg);
end;

procedure TFormMain.ChartMouseDown(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    //  Screen.Cursor := crCursorDrag;
    //  This would only work with the standard Windows cursors.
    //Windows.SetCursor(Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]));
    FDownX := X; FDownY := Y;
end;
{$hints on}

{ WHERE THE THUMBS BELONG for the part of the chart on show. The arithmetic is
  in chart_panning, where the two directions can be asserted to be inverses of
  each other - a pair that disagrees makes the thumb jump out from under the
  pointer on every drag. These read the chart and write the bars. }
procedure TFormMain.UpdateBarsPos;
begin
    Application.ProcessMessages;
    if Chart.SeriesCount = 0 then
        Exit;
    ScrollBarX.Position := BarPositionForWindow(
        AxisWindow(FInitXGraphMin, FInitXGraphMax,
            Chart.XGraphMin, Chart.XGraphMax),
        BarRange(ScrollBarX.Min, ScrollBarX.Max, ScrollBarX.Position),
        False);
    //  INVERTED: a scroll bar's minimum is at the top of the screen and the
    //  chart's maximum is, so the thumb at the minimum means the window at the
    //  top of the data.
    ScrollBarY.Position := BarPositionForWindow(
        AxisWindow(FInitYGraphMin, FInitYGraphMax,
            Chart.YGraphMin, Chart.YGraphMax),
        BarRange(ScrollBarY.Min, ScrollBarY.Max, ScrollBarY.Position),
        True);
end;

{$hints off}
procedure TFormMain.ChartMouseUp(Sender: TOBject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    RequestStateRefresh;
    UpdateBarsPos;
    FUpX := X; FUpY := Y;
    //  WHICH BUTTON, at last. The rule is in pick_target, where it can be
    //  asserted in both directions; what is here is the one translation from
    //  the widget set's own enum. Without it a right-click reached OnChartClick
    //  like any other and placed a point, which is why the chart had no context
    //  menu it could offer.
    if ClickPlacesAPick(Button = mbLeft) then
        OnChartClick;
end;
{$hints on}

procedure TFormMain.TabSheetSummaryShow(Sender: TObject);
begin
    InsetGridInPanel(GridDatasheet, PanelDatasheet);
end;

procedure TFormMain.TabSheetCurveIntervalsShow(Sender: TObject);
begin
    InsetGridInPanel(GridIntervals, PanelIntervals);
end;

{ ----------------------- the parameters table's colours --------------------- }

{ ONE LIST, read twice: the cell colouring below and the key beside the table
  are generated from it, so a colour cannot reach the table without reaching the
  key as well. A legend that drifts from what is drawn is worse than no legend -
  it is a wrong explanation the user has no reason to doubt.

  Four kinds rather than eight parameter types: what a user needs to know is
  whether a number is theirs to change, whether the fit will move it, or whether
  it follows from the others. The types that differ only in what the ENGINE does
  with them are one kind here. }
{ TParameterKind, its captions, its hints and KindOfParameter moved to
  parameter_kinds. What a kind IS and what it is called is not a widget concern;
  the colours below are, so they stayed. }

const
    //  A TINT, not a colour: the value stays the thing you read, and the grid
    //  picks a contrasting text colour for whatever background it is given.
    //  Fitted - the ordinary case, and most of the table - is left on the row
    //  stripe, so the colours mark the EXCEPTIONS instead of painting
    //  everything. Different hues rather than shades of one, so the four do not
    //  become an ordering that means nothing.
    ParameterKindColor: array[TParameterKind] of TColor = (
        clNone,        //  pkFitted   - the row's own colour
        $00F4D6E2,     //  pkShared   - violet
        $00AADEFA,     //  pkFixed    - amber
        $00F8E8D6);    //  pkComputed - blue

procedure TFormMain.GridParametersGetCellColor(Sender: TObject;
    ColNum, RowNum: longint; var CellColor: TColor);
var
    T: TParameterType;
    Kind: TParameterKind;
begin
    //  The striping the grid would have drawn by itself. Assigning this event
    //  REPLACES that, so it has to be reproduced rather than assumed.
    if Odd(RowNum) then
        CellColor := GridParameters.OddRowColor
    else
        CellColor := GridParameters.EvenRowColor;

    if not Assigned(FCurveList) then
        Exit;
    if (ColNum < GridParameters.FixedCols) or
       (RowNum < GridParameters.FixedRows) then
        Exit;
    if not FCurveList.ColumnParameterType(RowNum - GridParameters.FixedRows,
        ColNum - GridParameters.FixedCols, T) then
        Exit;

    Kind := KindOfParameter(T);
    if ParameterKindColor[Kind] <> clNone then
        CellColor := ParameterKindColor[Kind];
end;

procedure TFormMain.BuildParameterLegend;
const
    SwatchSize = 11;
    Gap        = 6;
var
    Kind: TParameterKind;
    Swatch: TShape;
    Text: TLabel;
    Caption_: TLabel;
    Prev: TControl;
begin
    if Assigned(FParameterLegend) then
        Exit;

    FParameterLegend := TPanel.Create(Self);
    FParameterLegend.Name := 'PanelParameterLegend';
    //  EMPTIED, AFTER THE NAME. Naming a control whose caption is still its
    //  old name renames the caption too, and a panel DRAWS its caption - so
    //  "PanelParameterLegend" sat behind the legend's labels, and the layout
    //  check measured it as a caption that does not fit.
    FParameterLegend.Caption := '';
    FParameterLegend.Parent := PanelParameters;
    FParameterLegend.Align := alBottom;
    FParameterLegend.BevelOuter := bvNone;
    FParameterLegend.Height := 22;
    FParameterLegend.ShowHint := True;

    //  LAID OUT BY ANCHORS, not by adding up widths. The first attempt placed
    //  each item at a Left computed from the one before it, which needs every
    //  label's width BEFORE the labels have been measured - so under the user's
    //  font the row overlapped itself. Anchoring is resolved after autosizing,
    //  by which time the widths are real, and it stays right if the font,
    //  theme or wording changes.
    Caption_ := TLabel.Create(Self);
    Caption_.Name := 'LabelParameterLegendCaption';
    Caption_.Parent := FParameterLegend;
    Caption_.AutoSize := True;
    Caption_.Caption := 'Colour shows how each value is treated:';
    Caption_.AnchorParallel(akLeft, 4, FParameterLegend);
    Caption_.AnchorVerticalCenterTo(FParameterLegend);
    Prev := Caption_;

    for Kind := Low(TParameterKind) to High(TParameterKind) do
    begin
        Swatch := TShape.Create(Self);
        Swatch.Name := FWidgetNames.NameFor('ShapeLegendSwatch',
            ParameterKindCaption[Kind]);
        Swatch.Parent := FParameterLegend;
        Swatch.Shape := stRectangle;
        Swatch.Width := SwatchSize;
        Swatch.Height := SwatchSize;
        //  The fitted swatch shows the row colour itself, because that is what
        //  an untinted cell actually looks like - a white square beside a grey
        //  row would be a fourth colour the table never uses.
        if ParameterKindColor[Kind] = clNone then
            Swatch.Brush.Color := GridParameters.OddRowColor
        else
            Swatch.Brush.Color := ParameterKindColor[Kind];
        Swatch.Pen.Color := clGray;
        Swatch.Hint := ParameterKindHint[Kind];
        Swatch.ShowHint := True;
        Swatch.AnchorToNeighbour(akLeft, 2 * Gap, Prev);
        Swatch.AnchorVerticalCenterTo(FParameterLegend);

        Text := TLabel.Create(Self);
        Text.Name := FWidgetNames.NameFor('LabelLegendKind',
            ParameterKindCaption[Kind]);
        Text.Parent := FParameterLegend;
        Text.AutoSize := True;
        Text.Caption := ParameterKindCaption[Kind];
        //  The sentence lives on the hint, not in the row: four explanations
        //  side by side is a paragraph, and a paragraph under a table is read
        //  once and then never again.
        Text.Hint := ParameterKindHint[Kind];
        Text.ShowHint := True;
        Text.AnchorToNeighbour(akLeft, 4, Swatch);
        Text.AnchorVerticalCenterTo(FParameterLegend);

        Prev := Text;
    end;
end;

procedure TFormMain.TabSheetCurveAttributesShow(Sender: TObject);
begin
    //  Before the grid: the key is aligned to the bottom, so its height is what
    //  is left over for the table.
    SizeParameterLegend;
    InsetGridInPanel(GridParameters, PanelParameters);
end;

procedure TFormMain.TabSheetCurvePositionsShow(Sender: TObject);
begin
    InsetGridInPanel(GridSpecPositions, PanelSpecPositions);
end;



{$hints off}
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

{ A TICK IS READ AFTER IT IS MADE, WHICH IS WHAT OnClickCheck IS FOR.

  This ran on OnClick, and on gtk2 that happened to work: gtk2 flips the item's
  state in its own 'toggled' callback and only then moves the cursor, so the
  click arrived at a list whose tick was already the new one. Qt does it the
  other way round - TQtCheckListBox.itemViewViewportEventFilter delivers the
  mouse release to the LCL first and flips the state afterwards - so every read
  was one click stale: turning a curve off left it drawn, and the next click on
  any row applied the previous one's answer.

  OnClickCheck is delivered from the tick itself (LM_CHANGED, which all three
  widget sets send after calling Toggle) rather than from the mouse, so the
  order is not the widget set's to decide. It also covers the space bar, which
  is why the separate OnKeyPress handler is gone: TCustomCheckListBox.KeyDown
  toggles and raises ClickCheck for it. }
procedure TFormMain.CheckListBoxLegendClickCheck(Sender: TObject);
begin
    //  Logged because the log could not answer 'did the tick change?' when it
    //  was asked - see the UI-action tier in client_log.
    if CheckListBoxLegend.ItemIndex >= 0 then
        LogUiAction('legend: ' + CheckListBoxLegend.Items[CheckListBoxLegend.ItemIndex] +
            ' ' + BoolToStr(CheckListBoxLegend.Checked[CheckListBoxLegend.ItemIndex],
            'shown', 'hidden'))
    else
        LogUiAction('legend: tick changed');
    CheckListBoxChanged;
end;

{$warnings off}
procedure TFormMain.MenuSinThetaLambdaClick(Sender: TObject);
var
    WaveLength: double;
    Refusal: string;
begin
    FAxisModeChosenByUser := True;
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    if FitClientApp_.FitClient.GetWaveLength = 0 then
    begin
        CheckAssigned(InputWavelengthDlg, 'the dialog that asks for the wavelength');

        if InputWavelengthDlg.ShowModal = mrOk then
        begin
            //  REFUSED, not raised. This used to swap the process-wide decimal
            //  separator around a StrToFloat that raises on a typo - so the
            //  separator was never put back, and the exception reached the
            //  top-level handler, which logs at Fatal and STOPS THE SERVER
            //  POLL. A typo in a text box disconnected the user from the
            //  compute server. See findings.md.
            //  BOTH REFUSALS are in argument_axis, beside the axis that
            //  divides by this number: not a number, and not greater than zero -
            //  zero being what "not set" already means to the client, so
            //  accepting it would silently do nothing at all.
            if not WavelengthFromText(
                InputWavelengthDlg.EditWavelength.Text, WaveLength,
                Refusal) then
            begin
                MessageDlg('Wavelength', Refusal, mtInformation, [mbOK], 0);
                Exit;
            end;
            Screen.Cursor := crHourGlass;
            try
                FitClientApp_.FitClient.SetWaveLength(WaveLength);
            finally
                Screen.Cursor := crDefault;
            end;
            ApplyViewMode(XCM_SINTL);
        end;
    end {if FitClientApp_.FitClient.GetWaveLength = 0 then...}
    else
        ApplyViewMode(XCM_SINTL);
end;
{$warnings on}

procedure TFormMain.ApplyViewMode(Mode: longint);
begin
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    FFitViewer.XCoordMode := Mode;
    if Assigned(FCurveList) then
    begin
        FCurveList.FViewMode := Mode;
        //  Keep the grid's custom-axis definition in sync with the viewer's, so
        //  parameter positions display through the same transform (also covers
        //  restoring a persisted custom axis once data is loaded).
        if Mode = XCM_CUSTOM then
        begin
            FCurveList.FCustomName    := FCustomAxisName;
            FCurveList.FCustomUnit    := FCustomAxisUnit;
            FCurveList.FCustomForward := FCustomAxisForward;
            FCurveList.FCustomInverse := FCustomAxisInverse;
        end;
        FCurveGrid.Assign(GridParameters, FCurveList);
    end;

    SetMenuEntryChecked(MenuTheta, Mode = XCM_T);
    SetMenuEntryChecked(PopupMenuTheta, Mode = XCM_T);
    SetMenuEntryChecked(MenuN2Theta, Mode = XCM_2T);
    SetMenuEntryChecked(PopupMenuN2Theta, Mode = XCM_2T);
    SetMenuEntryChecked(MenuSinThetaLambda, Mode = XCM_SINTL);
    SetMenuEntryChecked(PopupMenuSinThetaLambda, Mode = XCM_SINTL);
    if Assigned(FMenuIdentity) then
        SetMenuEntryChecked(FMenuIdentity, Mode = XCM_IDENTITY);
    if Assigned(FMenuIdentityPopup) then
        SetMenuEntryChecked(FMenuIdentityPopup, Mode = XCM_IDENTITY);
    if Assigned(FMenuCustom) then
        SetMenuEntryChecked(FMenuCustom, Mode = XCM_CUSTOM);
    if Assigned(FMenuCustomPopup) then
        SetMenuEntryChecked(FMenuCustomPopup, Mode = XCM_CUSTOM);
    if Assigned(FMenuCurveAxis) then
        SetMenuEntryChecked(FMenuCurveAxis, Mode = XCM_CURVE);
    if Assigned(FMenuCurveAxisPopup) then
        SetMenuEntryChecked(FMenuCurveAxisPopup, Mode = XCM_CURVE);

    {  The wavelength governs only the diffraction-angle family; on the general
       'Position' and user-defined axes it is meaningless, so gate it off there
       (D5). Asked of the axis itself rather than of a list of modes, so it stays
       right for XCM_CURVE, where the axis depends on the selected curve type. }
    if Assigned(MenuSetRuleParameters) then
        MenuSetRuleParameters.Enabled := CurrentAxisIsDiffraction(Mode);

    UpdateAxisLabel(Mode);
end;

{ True when the axis currently in force is one of the diffraction-angle family,
  i.e. when the wavelength means anything at all. }
function TFormMain.CurrentAxisIsDiffraction(Mode: longint): boolean;
var
    Axis: TArgumentAxis;
begin
    Axis := CreateAxisForMode(Mode, CurrentWaveLength, FCustomAxisName,
        FCustomAxisUnit, FCustomAxisForward, FCustomAxisInverse);
    try
        Result := Axis is TDiffractionAngleAxis;
    finally
        Axis.Free;
    end;
end;

{ The wavelength the client knows about, or 0 when there is no client yet. }
function TFormMain.CurrentWaveLength: double;
begin
    Result := 0;
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        Result := FitClientApp_.FitClient.GetWaveLength;
end;

{ Labels the chart's x-axis with the current axis' display name and unit, so the
  user sees e.g. '2*Theta [deg]', 'Sin(Theta)/Lambda [1/A]', 'Position', or a
  custom name/unit - single-sourced from the IArgumentAxis. }
procedure TFormMain.UpdateAxisLabel(Mode: longint);
begin
    //  Single-sourced from mscr_specimen_list so the label matches the transform.
    Chart.XAxisLabel := AxisLabelForMode(Mode, CurrentWaveLength, FCustomAxisName,
        FCustomAxisUnit, FCustomAxisForward, FCustomAxisInverse);
    Chart.ShowAxisLabel := True;
    Chart.Invalidate;
end;

procedure TFormMain.MenuThetaClick(Sender: TObject);
begin
    FAxisModeChosenByUser := True;
    ApplyViewMode(XCM_T);
end;

procedure TFormMain.MenuN2ThetaClick(Sender: TObject);
begin
    FAxisModeChosenByUser := True;
    ApplyViewMode(XCM_2T);
end;

procedure TFormMain.MenuIdentityClick(Sender: TObject);
begin
    FAxisModeChosenByUser := True;
    ApplyViewMode(XCM_IDENTITY);
end;

{ Hands the axis back to the model: from here on the caption and the displayed
  positions follow whatever curve type is selected. }
procedure TFormMain.MenuCurveAxisClick(Sender: TObject);
begin
    FAxisModeChosenByUser := True;
    ApplyViewMode(XCM_CURVE);
end;

{ Called through Synchronize from the calculation thread: it runs at whatever
  point the main loop was interrupted, so the dialog is queued for the same
  reason the one in OnException is. }
procedure TFormMain.ShowCalcError(const AMessage: string);
begin
    QueueError('Calculation error: ' + AMessage);
end;

function TFormMain.ShowCustomAxisDialog(var AName, AUnit, AForward,
    AInverse: string): boolean;
const
    { Consistent 8px-grid spacing used across the app's dialogs, AT 96 DPI.
      Nothing below is a device pixel count - every one of them is passed
      through Scale96ToForm before it reaches a control. }
    DesMargin  = 12;    //  gap to the dialog border
    DesGap     = 8;     //  gap between a label and its control
    DesRowGap  = 10;    //  vertical gap between rows
    DesEditH   = 24;
    DesLabelW  = 168;
    DesBtnW    = 76;
    DesBtnH    = 25;
    DesDlgW    = 520;
    DesIntroH  = 92;    //  the two-line explanation at the top
var
    Dlg: TForm;
    EdName, EdUnit, EdFwd, EdInv: TEdit;
    RowTop: integer;
    { The same values in the pixels of the display the dialog opens on. This
      dialog is built by hand rather than streamed from a .lfm, and a hand-built
      form gets no free ride: TCustomForm.AfterConstruction scales the form
      BEFORE this code adds a single control to it, so anything placed here in
      design pixels stays design-sized. That is what made the dialog a postage
      stamp with clipped captions on a scaled display. }
    Margin, Gap, RowGap, EditH, LabelW, BtnW, BtnH, DlgW: integer;
    EditLeft, EditW: integer;

    procedure AddLabel(const ACaption: string; ATop, AWidth, AHeight: integer;
        AWrap: boolean);
    var
        Lab: TLabel;
    begin
        Lab := TLabel.Create(Dlg);
        Lab.Name := FWidgetNames.NameFor('LabelAxisDialog', ACaption);
        Lab.Parent := Dlg;
        Lab.Left := Margin; Lab.Top := ATop; Lab.Width := AWidth;
        Lab.Height := AHeight;
        Lab.AutoSize := False;
        Lab.WordWrap := AWrap;
        Lab.Caption := ACaption;
    end;

    function AddRow(const ACaption, AValue, AHint: string): TEdit;
    var
        Lab: TLabel;
    begin
        Lab := TLabel.Create(Dlg);
        Lab.Name := FWidgetNames.NameFor('LabelAxisRow', ACaption);
        Lab.Parent := Dlg;
        //  The nudge that sits the caption against the middle of its edit box;
        //  scaled with everything else in this dialog.
        Lab.Left := Margin; Lab.Top := RowTop + Dlg.Scale96ToForm(4);
        Lab.Width := LabelW;
        Lab.AutoSize := False;
        Lab.Caption := ACaption;
        Result := TEdit.Create(Dlg);
        Result.Name := FWidgetNames.NameFor('EditAxis', ACaption);
        Result.Parent := Dlg;
        Result.Left := EditLeft; Result.Top := RowTop; Result.Width := EditW;
        Result.Height := EditH;
        Result.Text := AValue;
        Result.Hint := AHint;
        Result.ShowHint := True;
        Inc(RowTop, EditH + RowGap);
    end;

var
    BtnOk, BtnCancel: TButton;
    IntroH: integer;
begin
    Dlg := TForm.CreateNew(nil);
    try
        Dlg.Caption := 'Custom Argument Axis';
        Dlg.BorderStyle := bsDialog;
        Dlg.Position := poMainFormCenter;
        Dlg.ShowHint := True;

        Margin := Dlg.Scale96ToForm(DesMargin);
        Gap    := Dlg.Scale96ToForm(DesGap);
        RowGap := Dlg.Scale96ToForm(DesRowGap);
        EditH  := Dlg.Scale96ToForm(DesEditH);
        LabelW := Dlg.Scale96ToForm(DesLabelW);
        BtnW   := Dlg.Scale96ToForm(DesBtnW);
        BtnH   := Dlg.Scale96ToForm(DesBtnH);
        DlgW   := Dlg.Scale96ToForm(DesDlgW);
        EditLeft := Margin + LabelW + Gap;
        EditW    := DlgW - EditLeft - Margin;

        Dlg.ClientWidth := DlgW;

        //  Always-visible explanation (more discoverable than tooltips alone):
        //  says what the axis does and why both a formula and its inverse.
        IntroH := Dlg.Scale96ToForm(DesIntroH);
        AddLabel(
            'A custom axis only changes how positions are shown — it never ' +
            'changes your data or the fit.' + LineEnding +
            'f(x) converts the stored value x to the value displayed. g(x) is ' +
            'its inverse (displayed value back to x), used when you read or ' +
            'edit positions.',
            Margin, DlgW - 2 * Margin, IntroH, True);

        RowTop := Margin + IntroH + RowGap;
        EdName := AddRow('Display name:', AName, 'A short label for the axis.');
        EdUnit := AddRow('Unit:', AUnit, 'Optional unit shown next to the name.');
        EdFwd  := AddRow('Displayed value  f(x):', AForward,
            'Value shown on the axis as a formula of the stored value x. ' +
            'Example: ln(x)');
        EdInv  := AddRow('Inverse  g(x):', AInverse,
            'Converts a displayed value back to the stored x — the inverse of ' +
            'f(x). Example: exp(x)');

        Dlg.ClientHeight := RowTop + RowGap + BtnH + Margin;

        BtnOk := TButton.Create(Dlg);
        BtnOk.Name := FWidgetNames.NameFor('ButtonAxisOK', '');
        BtnOk.Parent := Dlg; BtnOk.Caption := 'OK';
        BtnOk.Width := BtnW; BtnOk.Height := BtnH;
        BtnOk.Top := RowTop + RowGap;
        BtnOk.Left := DlgW - Margin - BtnW - Gap - BtnW;
        BtnOk.ModalResult := mrOk; BtnOk.Default := True;

        BtnCancel := TButton.Create(Dlg);
        BtnCancel.Name := FWidgetNames.NameFor('ButtonAxisCancel', '');
        BtnCancel.Parent := Dlg; BtnCancel.Caption := 'Cancel';
        BtnCancel.Width := BtnW; BtnCancel.Height := BtnH;
        BtnCancel.Top := RowTop + RowGap;
        BtnCancel.Left := DlgW - Margin - BtnW;
        BtnCancel.ModalResult := mrCancel; BtnCancel.Cancel := True;

        Result := Dlg.ShowModal = mrOk;
        if Result then
        begin
            AName    := Trim(EdName.Text);
            AUnit    := Trim(EdUnit.Text);
            AForward := Trim(EdFwd.Text);
            AInverse := Trim(EdInv.Text);
        end;
    finally
        Dlg.Free;
    end;
end;

procedure TFormMain.MenuCustomAxisClick(Sender: TObject);
var
    Axis: TCustomAxisDefinition;
    Problem: TCustomAxisProblem;
begin
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    Axis.Name := FCustomAxisName;
    Axis.Units := FCustomAxisUnit;
    Axis.Forward_ := FCustomAxisForward;
    Axis.Inverse := FCustomAxisInverse;

    //  SEEDED WITH THE IDENTITY on a first use. Two empty boxes give no clue
    //  that what belongs in them is a formula in x; f(x)=x is the instruction,
    //  and accepting it unchanged does nothing surprising.
    if CustomAxisIsUnset(Axis) then
        Axis := DefaultCustomAxis;

    FCustomAxisName := Axis.Name;
    FCustomAxisUnit := Axis.Units;
    FCustomAxisForward := Axis.Forward_;
    FCustomAxisInverse := Axis.Inverse;

    if not ShowCustomAxisDialog(FCustomAxisName, FCustomAxisUnit,
        FCustomAxisForward, FCustomAxisInverse) then
        Exit;

    Axis.Name := FCustomAxisName;
    Axis.Units := FCustomAxisUnit;
    Axis.Forward_ := FCustomAxisForward;
    Axis.Inverse := FCustomAxisInverse;
    Problem := CustomAxisProblem(Axis);
    if Problem <> capNone then
    begin
        MessageDlg('Custom axis', CustomAxisProblemMessage(Problem),
            mtInformation, [mbOK], 0);
        Exit;
    end;

    FFitViewer.SetCustomAxis(FCustomAxisName, FCustomAxisUnit,
        FCustomAxisForward, FCustomAxisInverse);
    if Assigned(FCurveList) then
    begin
        FCurveList.FCustomName    := FCustomAxisName;
        FCurveList.FCustomUnit    := FCustomAxisUnit;
        FCurveList.FCustomForward := FCustomAxisForward;
        FCurveList.FCustomInverse := FCustomAxisInverse;
    end;
    FAxisModeChosenByUser := True;
    ApplyViewMode(XCM_CUSTOM);
end;

procedure TFormMain.RestoreViewMode;
var
    Mode: longint;
begin
    CheckAssigned(FFitViewer, 'the viewer that draws into this window');

    FAxisModeChosenByUser := FSettings.ViewModeChosenByUser;
    FCustomAxisName    := FSettings.CustomAxisName;
    FCustomAxisUnit    := FSettings.CustomAxisUnit;
    FCustomAxisForward := FSettings.CustomAxisForward;
    FCustomAxisInverse := FSettings.CustomAxisInverse;

    //  WHICH AXIS A SAVED SETTING ACTUALLY RESOLVES TO is single-sourced in
    //  mscr_specimen_list, with both of its fall-backs - no wavelength for the
    //  diffraction axis, no formulas for the custom one. Getting either wrong
    //  blocks start-up on a setting the user cannot see to correct.
    Mode := UsableViewMode(FSettings.ViewMode, FAxisModeChosenByUser,
        FitClientWaveLength, FCustomAxisForward, FCustomAxisInverse);

    if Mode = XCM_CUSTOM then
        FFitViewer.SetCustomAxis(FCustomAxisName, FCustomAxisUnit,
            FCustomAxisForward, FCustomAxisInverse);

    ApplyViewMode(Mode);
end;

{ The wavelength the compute server knows about, or 0 before there is a client
  to ask - which is what UsableViewMode reads as "none known". }
function TFormMain.FitClientWaveLength: double;
begin
    Result := 0;
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        Result := FitClientApp_.FitClient.GetWaveLength;
end;

procedure TFormMain.RefreshAxisForSelectedCurveType;
begin
    //  Only XCM_CURVE depends on the selected type, but re-applying the current
    //  mode unconditionally keeps this a single unconditional call at each
    //  selection site instead of a rule duplicated across them.
    if Assigned(FFitViewer) then
        ApplyViewMode(FFitViewer.XCoordMode);
    //  Here for the same reason: this is already the one hook every curve-type
    //  selection site calls, so the Waves tab follows the selection without the
    //  rule being repeated at each of them.
    RefreshModelStructure;
end;

function TFormMain.CurrentContext: TProjectClientContext;
var
    Formula: string;
begin
    //  READS WIDGETS AND HANDS THEM OVER. The assembly is in
    //  project_ui_context, and it is there because three fields were silently
    //  missing from it for as long as it was here - the selected interval, the
    //  user-defined formula, and reading any of it back at all. Nothing failed;
    //  nothing tested it either, because this file is excluded from the
    //  coverage target.
    Formula := '';
    if Assigned(FSelectedUserCurve) then
        Formula := FSelectedUserCurve.Expression;

    Result := BuildProjectContext(
        FCurveList, FAxisModeChosenByUser,
        Ord(FitClientApp_.FitClient.SelectionMode), PageControl.ActivePageIndex,
        FSelectedCurveId,
        FCustomAxisName, FCustomAxisUnit,
        FCustomAxisForward, FCustomAxisInverse,
        //  The interval and the provenance are the DOCUMENT'S memory and are
        //  filled in by project_workflow, which outlives any one gesture here.
        False, 0, 0,
        Formula <> '', Formula,
        Default(TProjectProvenance), GetAppVersion);
end;

procedure TFormMain.ApplyWorkingContext(const APlan: TProjectUiPlan);
begin
    if APlan.ApplyAxis then
    begin
        FCustomAxisName := APlan.CustomAxisName;
        FCustomAxisUnit := APlan.CustomAxisUnit;
        FCustomAxisForward := APlan.CustomAxisForward;
        FCustomAxisInverse := APlan.CustomAxisInverse;
        ApplyViewMode(APlan.ViewMode);
    end;
    if APlan.ApplyTab then
        PageControl.ActivePageIndex := APlan.ActiveTab;
    if APlan.ApplySelectionMode then
        FitClientApp_.FitClient.SelectionMode := TSelMode(APlan.SelectionMode);
    //  BY HANDLE. It is what the Delete-curve command follows, so restoring it
    //  restores what the user could do to the curve they had chosen.
    if APlan.ApplySelectedCurve then
        SetSelectedCurveId(APlan.SelectedCurveId);
end;

function TFormMain.TabCount: longint;
var
    Visible: array of boolean;
    i: longint;
begin
    //  THE TABS A RESTORE MAY SELECT, not every page. A module's report tab is
    //  hidden until the module has reported, which on opening a project is
    //  after the tab would be restored - and a hidden page cannot be brought
    //  forward. report_tabs decides; the widget set only answers visibility.
    SetLength(Visible, PageControl.PageCount);
    for i := 0 to PageControl.PageCount - 1 do
        Visible[i] := PageControl.Pages[i].TabVisible;
    Result := RestorableTabCount(Visible);
end;

function TFormMain.ModelHoldsCurve(const AHandle: string): boolean;
begin
    Result := (AHandle <> '') and
        (FitClientApp_.FitClient.FitService.IndexOfCurveInstance(AHandle) >= 0);
end;

function TFormMain.AskProjectToOpen(out APath: string): boolean;
begin
    APath := '';
    Result := ProjectOpenDialog.Execute;
    if Result then
        APath := ProjectOpenDialog.FileName;
end;

function TFormMain.AskProjectToSaveAs(const ASuggested: string;
    out APath: string): boolean;
begin
    APath := '';
    ProjectSaveDialog.FileName := ASuggested;
    Result := ProjectSaveDialog.Execute;
    if Result then
        APath := ProjectSaveDialog.FileName;
end;

function TFormMain.AskSaveBeforeClosing(const AWhat: string): TSaveAnswer;
begin
    case MessageDlg(SaveQuestion(AWhat), mtConfirmation, mbYesNoCancel, 0) of
        mrYes: Result := saYes;
        mrNo:  Result := saNo;
    else
        //  Anything that is not an explicit yes or no - Cancel, or the dialog
        //  closed by the window manager - is a cancel.
        Result := saCancel;
    end;
end;

function TFormMain.Confirm(const AQuestion: string): boolean;
begin
    //  The wording is the caller's; this shows it and maps the answer.
    Result := MessageDlg(AQuestion, mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TFormMain.ReportProblem(const AMessage: string);
begin
    MessageDlg(AMessage, mtError, [mbOK], 0);
end;

procedure TFormMain.ShowDocument(const APath: string);
begin
    Caption := GetWindowTitle(ApplicationProperties.Title, ProjectTitle(APath));
    if Assigned(FSettings) then
    begin
        //  Into the settings this window already writes at shutdown, beside the
        //  server URL - a project path is a per-machine convenience exactly as
        //  that is, and there is no second store for it.
        FSettings.LastProjectFile := APath;
        //  AND INTO THE LIST behind File > Open Recent. What that list does
        //  with the path - promote, de-duplicate, trim - is recent_project's.
        FSettings.RecentProjects :=
            RecentAfterOpening(FSettings.RecentProjects, APath);
    end;
    RefreshRecentMenu;
end;

{ THE MENU IS REBUILT, NEVER EDITED IN PLACE. What it should show - which
  projects, in what order, how many, and the one disabled line an empty list
  gets - is recent_project's, so this makes one widget per line and nothing
  else.

  BUILT HERE RATHER THAN IN THE .lfm, and that is not a style choice: a
  design-time child would be freed by the Clear below on the first refresh,
  leaving the form holding a pointer to it. }
procedure TFormMain.RefreshRecentMenu;
var
    Lines: TRecentMenuLines;
    Item: TMenuItem;
    Stored: string;
    i: longint;
begin
    if not Assigned(MenuOpenRecent) then
        Exit;
    MenuOpenRecent.Clear;
    Stored := '';
    if Assigned(FSettings) then
        Stored := FSettings.RecentProjects;
    Lines := RecentMenu(Stored);
    for i := 0 to High(Lines) do
    begin
        Item := TMenuItem.Create(MenuOpenRecent);
        Item.Caption := Lines[i].Caption;
        Item.Enabled := Lines[i].Enabled;
        if Lines[i].OpensAProject then
            Item.OnClick := RecentProjectClick;
        MenuOpenRecent.Add(Item);
    end;
end;

procedure TFormMain.RecentProjectClick(Sender: TObject);
begin
    //  The caption IS the path - only lines that open a project were given this
    //  handler - so nothing has to be looked up.
    if not (Sender is TMenuItem) then
        Exit;
    FProjectFlow.OpenRecentProject(TMenuItem(Sender).Caption);
end;

procedure TFormMain.RefreshFromEngine;
begin
    //  ResyncFromService, not UpdateComputedData: the latter leaves the
    //  experimental profile alone, because every other path that changes it has
    //  already put it here. Restoring a project is the one that has not.
    FitClientApp_.FitClient.ResyncFromService;
end;

procedure TFormMain.ClearEverything;
begin
    //  StartEmpty, not Reload: Reload re-reads the DATA FILE, and a session that
    //  opened a project has never had one - so New Project raised there instead
    //  of clearing.
    FitClientApp_.FitClient.StartEmpty;
end;

function TFormMain.HasUnsavedWork: boolean;
begin
    //  READ FROM THE TABLE FLAGS, which is not a shortcut: both are set wherever
    //  the model is reported into a grid, and the model is regenerated on every
    //  edit - so between them they already mean "something changed since this
    //  window was last written out". A third flag maintained at every edit site
    //  would be a third thing to forget at one of them.
    Result := FModifiedParameters or FModifiedDatasheet;
end;

procedure TFormMain.MarkSaved;
begin
    FModifiedParameters := False;
    FModifiedDatasheet := False;
end;

function TFormMain.LastProjectFile: string;
begin
    Result := '';
    if Assigned(FSettings) then
        Result := FSettings.LastProjectFile;
end;

procedure TFormMain.ForgetLastProject;
begin
    if not Assigned(FSettings) then
        Exit;
    //  OUT OF THE RECENT LIST TOO, and for the same reason it is forgotten
    //  here: an entry that opens nothing is a line the user can only be
    //  disappointed by.
    FSettings.RecentProjects := RecentWithout(FSettings.RecentProjects,
        FSettings.LastProjectFile);
    FSettings.LastProjectFile := '';
    RefreshRecentMenu;
end;

{ Asks the user for a name and writes the table to it.

  THE CONVERSATION IS NOT HERE. Which question to put, what each answer means and
  whether to ask again are table_export's, and so - since this commit - is the
  loop around them: it was a repeat here, and before that a loop with a label and
  two gotos, so the one thing worth checking about it could not be. This method
  now asks what it is told to ask and writes the file. }
function TFormMain.AskExportPath(out APath: string): boolean;
begin
    APath := '';
    Result := SaveDialog.Execute;
    if not Result then
        Exit;
    APath := SaveDialog.FileName;
    if Trim(APath) <> '' then
        SaveDialog.InitialDir := ExtractFilePath(APath);
end;

function TFormMain.AskExportQuestion(AQuestion: TExportQuestion;
    const APath: string): TExportAnswer;
begin
    Result := eaCancel;
    case AQuestion of
        eqNameIsEmpty:
            if MessageDlg('File name must not be empty.' + LineEnding +
                'Select file again?', mtError, mbYesNo, 0) = mrYes then
                Result := eaYes
            else
                Result := eaNo;
        eqFileExists:
            case MessageDlg('File ' + ExtractFileName(APath) +
                ' exists.' + LineEnding + 'Overwrite?', mtConfirmation,
                mbYesNoCancel, 0) of
                mrYes: Result := eaYes;
                mrNo: Result := eaNo;
            end;
    end;
end;

function TFormMain.ExportPathExists(const APath: string): boolean;
begin
    Result := FileExists(APath);
end;

function TFormMain.SaveTableAsText(const ARows: TTextRows): Boolean;
var
    i: LongInt;
    F: TextFile;
    FileName: string;
begin
    //  No @: this unit compiles in Delphi mode, where a method named where an
    //  `of object` value is expected IS that value - the same way OnClick is
    //  assigned everywhere else in this form.
    FileName := ChooseExportPath(AskExportPath, AskExportQuestion,
        ExportPathExists);
    Result := FileName <> '';
    if not Result then
        Exit;

    AssignFile(F, FileName);
    Rewrite(F);
    try
        //  The rows come from the table's model with every digit; the headings
        //  are the table's own. The separation rule is in table_export: a
        //  trailing tab is an extra empty column in every spreadsheet that
        //  reads the file, and invisible in the file.
        for i := 0 to High(ARows) do
            WriteLn(F, TabSeparatedRow(ARows[i]));
    finally
        CloseFile(F);
    end;
end;

//  sohranenie tabl. parametrov krivyh kak XML fayla


procedure TFormMain.AsyncOperationFinished(Sender: TObject);
begin
    //  DIALOGS MUST NOT BE OPENED inside Synchronize without a check: in this
    //  library that starts a message loop which re-enters Synchronize.
    //MessageDlg('Computation done...', mtInformation, [mbOk], 0);
    ShowTime;
    ShowStatistics;
    ShowHint(HintMain);
    //  The server is idle again, and what can be done follows.
    RequestStateRefresh;
end;

{ Shows the goodness-of-fit statistics for the finished fit in the status bar.
  These are the publishable numbers beyond the R-factor: weighted reduced
  chi-squared and R^2 (the full set, AIC and BIC among them, is saved with the
  project; no dialog shows it). }
procedure TFormMain.ShowStatistics;
var
    S: TFitStatistics;
begin
    try
        S := FitClientApp_.FitClient.GetStatistics;
    except
        //  Never let fetching a status-bar number break the done-handler.
        S := EmptyFitStatistics;
    end;
    //  Empty when there is no fit: a reduced chi-squared shown for a model
    //  nobody fitted reads as a fit that went badly.
    StatusBar.Panels[1].Text := StatisticsSummary(S);
end;

procedure TFormMain.ShowTime;
var TimeStr: string;
begin
    TimeStr := FitClientApp_.FitClient.GetCalcTimeStr;
    StatusBar.Panels[0].Text := ElapsedTimeText(TimeStr);
    Application.ProcessMessages;
end;

{ Closing with work the user has not saved.

  THE RULES ARE IN close_query, where they can be tested - which of them applies
  is decided per document, and the one that matters is that a save the user asked
  for and which failed must stop the close rather than proceed.

  WALKED, NOT WRITTEN TWICE. Each editable table used to have its own copy of
  this thirty-line block, and the copies had already begun to differ; a third
  table is now an entry in the list below. }
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    //  The conversation itself - what is asked, what each answer means, and
    //  that a failed save blocks the close - is close_query's, driven by
    //  project_workflow. This window only supplies the dialog.
    CanClose := FProjectFlow.MayClose;
end;

procedure TFormMain.MenuSetWavelengthClick(Sender: TObject);
begin
    if InputWavelengthDlg.ShowModal = mrOk then
    begin
        CheckAssigned(FitClientApp_, 'the client application object');
        CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

        FitClientApp_.FitClient.SetWaveLength(InputWavelengthDlg.FValue);
        FAxisModeChosenByUser := True;
        ApplyViewMode(XCM_SINTL);
    end;
end;

{ WHAT THE WINDOW OFFERS, gathered and then decided.

  The decision is in action_state, where it can be asked what it would do; what
  is left here is reading the inputs off the widgets and the client, and writing
  the answer back onto the actions and menu entries.

  It used to be four methods packing bit flags into every widget's Tag and
  unpacking them again at the end, and they had to run in the right order. Two
  hundred lines that could only be exercised by opening the window. }
{ EVERYTHING THE DECISION IS MADE FROM, read off the widgets and the client and
  handed back complete.

  A FUNCTION SO THAT NOTHING CAN BE ADDED AFTER THE DECISION. This was inline,
  and one input - whether the selected Model panel row names a curve - was
  assigned AFTER CommandStates had already been called on the record. So the
  rule reading it never saw anything but False and Delete curve was permanently
  greyed, however the panel was clicked. Both halves were right on their own,
  the rule has its own tests, and nothing could catch the order. Gathering that
  returns its result cannot have that shape. }
function TFormMain.GatherUiInputs: TUiInputs;
var
    Grid: TNumericGrid;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    Result := EmptyUiInputs;
    Result.Open := FitClientApp_.FitClient.OpenState;
    Result.Server := FitClientApp_.FitClient.FitService.GetState;
    Result.Async := FitClientApp_.FitClient.AsyncState;
    Result.Selection := FitClientApp_.FitClient.SelectionMode;
    Result.SelectedAreaInForce := FitClientApp_.FitClient.SelectedAreaMode;
    Result.GraphHasSeries := Chart.SeriesCount <> 0;

    if Assigned(FitClientApp_.FitClient.GetSelectedPoints) then
        Result.SelectedPointCount :=
            FitClientApp_.FitClient.GetSelectedPoints.PointsCount;

    if ActiveControl is TNumericGrid then
    begin
        Grid := ActiveControl as TNumericGrid;
        //  Which grid it is, because Delete acts on curves and only one grid
        //  holds any.
        Result.GridIsCurveTable := Grid = GridParameters;
        Result.Grid := GridSelectionState(True,
            Grid.Selection.Left, Grid.Selection.Top,
            Grid.Selection.Right, Grid.Selection.Bottom,
            Grid.FixedCols, Grid.FixedRows, Grid.ColCount, Grid.RowCount);
    end
    else
        Result.Grid := GridSelectionState(False, 0, 0, 0, 0, 0, 0, 0, 0);

    //  Whether anything is selected that can be deleted. The panel holds the
    //  selection; whether that selection names a curve is what the rule needs.
    //
    //  BEFORE THE DECISION, and it was written AFTER it - so the rule reading it
    //  never saw anything but False, and Delete curve was permanently greyed
    //  however the panel was clicked. The whole point of gathering the inputs
    //  and then deciding is that the gathering finishes first; one line on the
    //  wrong side of that call disabled a command outright, and neither half is
    //  wrong on its own, which is why nothing caught it.
    Result.ModelRowNamesACurve := FSelectedCurveId <> '';
    //  From the copy the last refresh left, not a request: this is gathered on
    //  every poll, and the Model panel is drawn from the same copy.
    Result.ModelHasCurves :=
        Assigned(FitClientApp_.FitClient.CurvesForDisplay) and
        (FitClientApp_.FitClient.CurvesForDisplay.Count > 0);

end;

{ How many of each thing the model holds, for the panel headings. Nil-safe: a
  set the client has not built yet counts nothing rather than faulting. }
function TFormMain.CurrentModelCounts: TModelCounts;

    function CountOf(APoints: TNeutronPointsSet): longint;
    begin
        Result := 0;
        if Assigned(APoints) then
            Result := APoints.PointsCount;
    end;

begin
    Result := EmptyModelCounts;
    Result.Positions := CountOf(FitClientApp_.FitClient.GetCurvePositions);
    Result.Intervals := CountOf(FitClientApp_.FitClient.GetRFactorBounds);
    Result.BackgroundPoints :=
        CountOf(FitClientApp_.FitClient.GetBackgroundPoints);
end;

procedure TFormMain.CheckState;
var
    Inputs: TUiInputs;
begin
    //  GATHERED WHOLE, then decided from - see GatherUiInputs for what an
    //  input assigned after the decision cost.
    Inputs := GatherUiInputs;

    //  ONE LOOP OVER THE TABLE. Which widget each command drives, and whether
    //  it may carry a tick, are declared in ui_commands where a test can read
    //  them - including the rule that used to be written out five times here,
    //  that the background submenu and every entry under it move together.
    FCommands.Refresh(CommandStates(Inputs), Inputs.Selection,
        CurrentModelCounts);
    ApplyCommandStates;
    RefreshToolPane;

    //  NOT WHILE AN OPERATION RUNS. Rebuilding the panel re-establishes the
    //  selection, and that hands the row back to whichever module owns the
    //  panel - a call this window cannot see inside. A module that answers it
    //  by asking the engine about the model waits for the fit's lock, and the
    //  window waits with it: the whole window, on its own thread, for the
    //  length of the fit. That is the freeze the live progress view exists to
    //  end, arriving by the one road the client's own rule about polled routes
    //  does not cover, because the call is not the client's.
    //
    //  Nothing is lost by waiting: the model does not change during an
    //  operation - the frames an animated fit draws are pictures, not the
    //  model - and Done rebuilds the panel from the result.
    if ModelStructureMayBeRebuilt(Inputs.Async) then
        RefreshModelStructure;

    //  The context menu follows the same table as everything else. Its entries
    //  are row-scoped, so what they need is a row - and the popup is where the
    //  user learns whether they have one.
    ApplyRowCommandStates;

    //  What is left of the picking mode: a module's own entry, which the module
    //  named and this window must not guess at. The framework's own captions
    //  come from the table above.
    ApplyPickingCaptions(Inputs.Selection);
end;

{ Writes the table's answer onto the widgets, and nothing else. Every decision
  behind it is in ui_commands or action_state. }
procedure TFormMain.ApplyCommandStates;
var
    i: longint;
    Target: TComponent;
    Caption_: string;
begin
    for i := 0 to FCommands.Count - 1 do
    begin
        if i > High(FCommandTargets) then
            Break;
        Target := FCommandTargets[i];
        if not Assigned(Target) then
            //  A row that renders nowhere and drives nothing - or a module's
            //  row whose entry the menus never drew.
            Continue;

        if Target is TAction then
        begin
            TAction(Target).Enabled := FCommands.IsEnabled(i);
            if FCommands.Item(i).WithChecked then
                TAction(Target).Checked := FCommands.IsDown(i);
        end
        else if Target is TMenuItem then
        begin
            TMenuItem(Target).Enabled := FCommands.IsEnabled(i);
            if FCommands.Item(i).WithChecked then
                //  Through ui_menus, which refuses to tick an entry that was
                //  not declared checkable while a menu is open - the widget set
                //  would destroy the entry the user is standing in.
                SetMenuEntryChecked(TMenuItem(Target), FCommands.IsDown(i));
        end;

        //  A picking row's caption reads "stop" while its own mode runs. Empty
        //  means "leave it alone", which is every other row.
        Caption_ := FCommands.MenuCaption(i);
        if (Caption_ <> '') and (Target is TAction) then
            TAction(Target).Caption := Caption_;
    end;
end;

{ The three manual-picking entries say "stop" while their own mode is running
  and "start" otherwise.

  DRIVEN FROM THE MODE, not from the click that started it, because a mode ends
  in ways the entry never hears about - another mode starting, a profile being
  loaded. A caption left saying "stop" claims a mode that is off. }
procedure TFormMain.ApplyPickingCaptions(ASelectionMode: TSelMode);
begin
    //  THE FRAMEWORK'S THREE ENTRIES ARE NO LONGER WRITTEN OUT HERE. Each is
    //  a row of the command table carrying the picking entry it belongs to, and
    //  ApplyCommandStates writes the caption the table decided onto the action
    //  the menu entry is bound to. The rule itself is unchanged and still lives
    //  in action_state beside ModeAfterPicking - the caption describes the click
    //  that rule decides, and they must not be able to disagree.

    { A MODULE's picking mode gets the same treatment, from the same polled
      state, so its entry reads like the ones above.

      Which entry it is, the window does not know and must not - the module
      named it when it asked for the picks. }
    if ASelectionMode = ModeSelectModulePoints then
    begin
        if FModulePickMenuId <> '' then
            SetMenuChecked(FModulePickMenuId, True);
    end
    else
        EndModulePicking;
end;

procedure TFormMain.ShowRFactor;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    StatusBar.Panels[1].Text := 'R-factor: ' +
        FitClientApp_.FitClient.GetRFactorStr;
    Application.ProcessMessages;
end;

procedure TFormMain.BuildFitProgressView;
begin
    FProgressHeader := TLabel.Create(Self);
    FProgressHeader.Parent := PanelChart;
    FProgressHeader.Align := alTop;
    FProgressHeader.WordWrap := True;
    FProgressHeader.BorderSpacing.Around := 4;
    FProgressHeader.Visible := False;

    FProgressChart := TTAChart.Create(Self);
    FProgressChart.Parent := PanelChart;
    FProgressChart.Align := alClient;
    FProgressChart.ShowLegend := False;
    FProgressChart.ShowTitle := False;
    FProgressChart.ShowAxisLabel := True;
    FProgressChart.AutoUpdateXMin := True;
    FProgressChart.AutoUpdateXMax := True;
    FProgressChart.AutoUpdateYMin := True;
    FProgressChart.AutoUpdateYMax := True;
    FProgressChart.Visible := False;
    FProgressAxis := TDurationAxis.Create;
    FProgressChart.OnXMarks := ProgressXMarks;
    FProgressChart.OnXMarkText := ProgressXMarkText;

    //  OWNED BY NOTHING: the chart frees the series it holds (TTAChart.Destroy).
    FProgressSerie := TTASerie.Create(nil);
    FProgressSerie.ShowLines := True;
    FProgressSerie.ShowPoints := False;
    FProgressSerie.SeriesColor := clBlue;
    FProgressChart.AddSerie(FProgressSerie);

    //  ALWAYS TICKING: whether there is anything to ask is the client's decision
    //  (PollProgress asks nothing outside an operation), and a computation that
    //  shows no progress view still wants its clock.
    FProgressTimer := TTimer.Create(Self);
    FProgressTimer.Enabled := True;
    FProgressTimer.Interval := PROGRESS_POLL_INTERVAL_MS;
    FProgressTimer.OnTimer := ProgressTimerTick;

    //  ATTACHED, AND CHECKED TO BE: a control built and never parented is
    //  invisible, and nothing reports it.
    CheckThat((FProgressHeader.Parent = PanelChart) and
        (FProgressChart.Parent = PanelChart),
        'the fit progress view is attached to the chart panel');
end;

procedure TFormMain.ProgressTimerTick(Sender: TObject);
begin
    Inc(FProgressTimerTicks);
    //  Whether there is anything to ask is the client's decision; outside a
    //  fit this asks nothing.
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
        FitClientApp_.FitClient.PollProgress;
end;

procedure TFormMain.ProgressXMarks(AMin, AMax: double;
    var AStart, AStep: double; var AHandled: boolean);
begin
    AHandled := FProgressAxis.ChooseMarks(AMin, AMax, AStart, AStep);
end;

function TFormMain.ProgressXMarkText(AValue, AStep: double): string;
begin
    Result := FProgressAxis.MarkText(AValue, AStep);
end;

procedure TFormMain.ShowFitProgress(const AView: TFitProgressView);
var
    i: longint;
begin
    if csDestroying in ComponentState then
        Exit;
    FProgressHeader.Caption := AView.Header;
    FProgressHeader.Visible := True;

    FProgressChart.XAxisLabel := AView.XAxisLabel;
    FProgressChart.YAxisLabel := AView.YAxisLabel;
    FProgressSerie.Clear;
    for i := 0 to High(AView.X) do
        FProgressSerie.AddXY(AView.X[i], AView.Y[i], FProgressSerie.SeriesColor);
    Inc(FProgressFramesDrawn);
    if Length(AView.X) > FMostLossPointsDrawn then
        FMostLossPointsDrawn := Length(AView.X);

    //  ONE CHART OR THE OTHER, never both and never neither.
    FProgressChart.Visible := AView.LossChartVisible;
    Chart.Visible := not AView.LossChartVisible;
    ScrollBarX.Visible := not AView.LossChartVisible;
    ScrollBarY.Visible := not AView.LossChartVisible;
    FProgressChart.Invalidate;
end;

procedure TFormMain.HideFitProgress;
begin
    if csDestroying in ComponentState then
        Exit;
    FProgressHeader.Visible := False;
    FProgressChart.Visible := False;
    Chart.Visible := True;
    ScrollBarX.Visible := True;
    ScrollBarY.Visible := True;
end;

{ Splits the right panel into tabs and puts the legend on one, the wave outline
  on the other. Runtime rather than the .lfm, like the module menus and the axis
  items: additive, and it keeps the designed form untouched (D1).

  No new UI vocabulary is introduced - the bottom panel is already a PageControl,
  so a second one reads as the same idiom rather than as an invention. }
{ Builds the command table and resolves every row's target.

  THE TABLE NAMES COMPONENTS, and this is where a name becomes a widget. A name
  that resolves to nothing is refused here, at start-up, rather than producing a
  button that never enables and never says why - which is the whole reason the
  table holds names rather than references: a typo is then a failing test and a
  loud start-up instead of a dead widget nobody notices. }
{ Removes the curve the Model panel has selected.

  The verb is the service's; what this adds is the handle and the refusal. A
  handle the model no longer holds is not a fault - the same curve may have been
  deleted twice, or a fit may have removed it in between - so it is said once and
  the selection is cleared rather than raised over. }
procedure TFormMain.ActionDeleteCurveExecute(Sender: TObject);
var
    Id: string;
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    Id := FSelectedCurveId;
    if Id = '' then
        Exit;

    //  A REFUSAL IS A MESSAGE, HANDLED HERE. The server declines a curve it
    //  cannot remove on its own and says why; letting that escape would reach
    //  the last-resort handler, which is for faults. Same shape as the chart's
    //  click handler, and for the same reason.
    try
        if not FitClientApp_.FitClient.DeleteCurve(Id) then
        begin
            ShowHint('That curve is no longer in the model.');
            SetSelectedCurveId('');
            Exit;
        end;
    except
        on E: EUserException do
        begin
            //  Queued rather than shown: this runs from a menu click, and the
            //  same route every other non-fatal message in this form takes.
            QueueError(E.Message);
            SetSelectedCurveId('');
            Exit;
        end;
    end;

    //  The row it named is gone, so no curve is selected until the panel is
    //  refilled - which selects the model's first element again.
    SetSelectedCurveId('');
end;

{ Removes every curve, once the user has agreed.

  Everything that decides anything is in model_clearing, including what is said
  about a curve that stays; this passes the window and the client. The handler
  below is for a refusal raised before any curve is asked about. }
procedure TFormMain.ActionClearModelExecute(Sender: TObject);
begin
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    try
        ClearModelWithConsent(Self as IUiHost, FitClientApp_.FitClient);
    except
        on E: EUserException do
            QueueError(E.Message);
    end;
    //  Whatever was selected may be gone, and an empty model selects nothing.
    SetSelectedCurveId('');
end;

{ Splits the left panel into Tools and Data, and fills the Tools tab.

  RUNTIME RATHER THAN THE .lfm, like the module menus, the axis entries and the
  right-hand tabs: additive, and the designed form stays untouched (D1). No new
  UI vocabulary is introduced either - the bottom panel and the right panel are
  already page controls, so a third reads as the same idiom.

  WHY A PANE AT ALL. Building a model was reachable only through the menu bar,
  three levels down for the curve type, and the workflow is a LOOP - place some
  model, fit, look at the difference, place more - so every round paid the menu
  cost again. The menus are unchanged: they are the feature inventory, and that
  is what teaches the program. }
procedure TFormMain.BuildLeftPanelTabs;
var
    Rows: TIndexList;
    Groups: TGroupList;
    g, r, N: longint;
    Heading: TLabel;
    Btn: TSpeedButton;
    Split: TSplitter;
begin
    FLeftTabs := TPageControl.Create(Self);
    FLeftTabs.Name := 'PageControlLeft';
    FLeftTabs.Parent := PanelLeft;
    FLeftTabs.Align := alClient;

    FTabTools := TTabSheet.Create(Self);
    FTabTools.Name := 'TabSheetTools';
    FTabTools.PageControl := FLeftTabs;
    FTabTools.Caption := 'Tools';

    FTabData := TTabSheet.Create(Self);
    FTabData.Name := 'TabSheetData';
    FTabData.PageControl := FLeftTabs;
    FTabData.Caption := 'Data';

    //  The designed grid moves onto its own tab. Setting Align supersedes the
    //  geometry it was designed with, exactly as BuildRightPanelTabs does for
    //  the legend.
    GridData.Parent := FTabData;
    GridData.Align := alClient;
    //  The tab caption says 'Data' now; the label would repeat it.
    LabelData.Visible := False;

    //  ---- the curve types, on top and given the larger share: it is the
    //  longest list and the one the user reads rather than clicks blind.
    FCurveTypeList := TListBox.Create(Self);
    FCurveTypeList.Name := 'ListBoxCurveTypes';
    FCurveTypeList.Parent := FTabTools;
    FCurveTypeList.Align := alTop;
    FCurveTypeList.Height := Scale96ToFont(150);
    //  A GROUP HEADING IS TOLD FROM A CURVE TYPE BY INDENTATION, not by being
    //  drawn differently. Owner-drawing it would be the legend's idiom, but the
    //  legend's handler is assigned by the .lfm and this one would be assigned
    //  from code, where the event's calling convention has to match - and the
    //  distinction is worth no LCL subtlety at all. A header is unselectable
    //  either way: NextSelectableRow is what enforces that, not its looks.
    FCurveTypeList.OnClick := CurveTypeListClick;
    FCurveTypeList.Hint := 'The shape every curve of the model is made of';
    FCurveTypeList.ShowHint := True;
    //  EACH TYPE SAYS WHAT IT IS under the pointer - the same summary its menu
    //  entry carries; the list's own hint is what a heading or empty space shows.
    FCurveTypeList.OnShowHint := CurveTypeListShowHint;
    //  A LIST BOX DRAWS NO CAPTION, and TControl.Caption defaults to the
    //  component's name - which the layout check then measures against the
    //  control's width and reports as not fitting. Cleared because it is
    //  meaningless here, not to quiet the check: the same reasoning the check
    //  itself applies to an icon tool button.
    FCurveTypeList.Caption := '';

    //  So the user can give either half the room, which matters because how
    //  many command rows there are depends on what the modules added.
    Split := TSplitter.Create(Self);
    Split.Name := 'SplitterTools';
    Split.Parent := FTabTools;
    Split.Align := alTop;
    Split.ResizeAnchor := akTop;
    //  Nor does a splitter - see the list box above.
    Split.Caption := '';

    FToolBox := TScrollBox.Create(Self);
    FToolBox.Name := 'ScrollBoxToolCommands';
    FToolBox.Parent := FTabTools;
    FToolBox.Align := alClient;
    FToolBox.BorderStyle := bsNone;
    FToolBox.HorzScrollBar.Visible := False;

    //  ---- the commands, generated from the table. ONE LOOP: a module adding
    //  rows costs this window nothing, and the framework's own rows go through
    //  the same path, so it is exercised in every build.
    //
    //  WHERE EACH ONE GOES is LayoutToolPane's business, once they all exist.
    begin
        Groups := FCommands.PaneGroups;
        SetLength(FToolHeadings, Length(Groups));
        SetLength(FToolGroups, Length(Groups));
        N := 0;
        Rows := FCommands.IndicesFor(csPane, scGlobal);

        for g := 0 to High(Groups) do
        begin
            Heading := TLabel.Create(Self);
            Heading.Name := FWidgetNames.NameFor('LabelGroup', Groups[g]);
            Heading.Parent := FToolBox;
            Heading.Caption := Groups[g];
            Heading.Font.Style := [fsBold];
            FToolHeadings[g] := Heading;
            FToolGroups[g] := Groups[g];

            for r := 0 to High(Rows) do
            begin
                if FCommands.Item(Rows[r]).Group <> Groups[g] then
                    Continue;

                Btn := TSpeedButton.Create(Self);
                Btn.Name := FWidgetNames.NameFor('ButtonCommand',
                    FCommands.Item(Rows[r]).Id);
                Btn.Parent := FToolBox;
                Btn.Caption := FCommands.Item(Rows[r]).PaneCaption;
                Btn.Hint := FCommands.Item(Rows[r]).Hint;
                Btn.ShowHint := Btn.Hint <> '';

                //  A PICKING ROW IS A LATCH: it stays pressed while its own
                //  mode runs, which is the one thing a menu entry cannot show.
                //  Its caption never changes - the menu says start or stop, the
                //  button shows which.
                //
                //  AND SO IS A MODULE'S TOGGLE OR RADIO, for the same reason:
                //  it says something is on. WHICH BUTTONS RELEASE EACH OTHER is
                //  the table's answer - one number per set, so the radios of a
                //  group share it and everything else latches alone. A radio
                //  does NOT allow all up: releasing the pressed one would leave
                //  a setting with no value showing.
                if FCommands.Item(Rows[r]).HasPicking or
                    FCommands.Item(Rows[r]).Latching then
                begin
                    Btn.AllowAllUp := not FCommands.Item(Rows[r]).Radio;
                    Btn.GroupIndex := 100 + FCommands.LatchGroup(Rows[r]);
                end;

                Btn.Tag := N;
                Btn.OnClick := ToolButtonClick;
                SetLength(FToolButtons, N + 1);
                SetLength(FToolRows, N + 1);
                FToolButtons[N] := Btn;
                FToolRows[N] := Rows[r];
                Inc(N);
            end;

        end;
    end;
    LayoutToolPane;

    FLeftTabs.ActivePage := FTabTools;
end;

procedure TFormMain.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
    const AXProportion, AYProportion: double);
begin
    inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
    LayoutToolPane;
end;

procedure TFormMain.LayoutToolPane;
var
    Gap, PaneW, PaneChrome, i, g: longint;
    Widths: TCaptionWidths;
    Metrics: TPaneMetrics;
    Layout: TToolPaneLayout;
    Box: TPaneRect;
    //  QUALIFIED: unqualified TBitmap here is the Windows unit's BITMAP
    //  record, which a unit later in the uses clause re-exports over LCL's
    //  Graphics. The class is the one that has a Canvas.
    Measure: Graphics.TBitmap;
begin
    //  Asked by every rescale, including the one the designed form gets before
    //  the pane is built.
    if not Assigned(FToolBox) then
        Exit;

    Gap := Scale96ToFont(4);
    //  What the page control's borders take out of the panel, as the form was
    //  designed: PanelLeft is 189 and the pane it leaves is 178.
    PaneChrome := Scale96ToFont(189 - 178);

    //  ---- HOW WIDE THE PANE HAS TO BE, measured rather than assumed.
    //
    //  This was a constant - 178 scaled - and the button width was whatever two
    //  columns of it left. So the longest caption in the table decided whether
    //  the pane was wide enough and nothing asked it: "Subtract" sat at the very
    //  edge of its button, and a module contributing a longer word, or a font a
    //  little wider, would have clipped it. The layout check cannot see that
    //  coming either - it measures against the width a control HAS.
    //
    //  MEASURED HERE, DECIDED THERE. Text measuring needs a font and a canvas,
    //  which is this window's business; how wide the pane must be from those
    //  numbers is tool_pane_layout's, where a test can read it. A bitmap canvas
    //  exactly as the layout check measures with, because a button's own canvas
    //  may have no handle yet - and in EACH BUTTON'S OWN FONT, as that check
    //  measures it, not the window's, which is scaled separately from them.
    SetLength(Widths, Length(FToolButtons));
    Measure := Graphics.TBitmap.Create;
    try
        Measure.SetSize(1, 1);
        for i := 0 to High(FToolButtons) do
        begin
            Measure.Canvas.Font.Assign(FToolButtons[i].Font);
            Widths[i] := Measure.Canvas.TextWidth(FToolButtons[i].Caption);
        end;
    finally
        Measure.Free;
    end;
    PaneW := PaneWidthForCaptions(Widths, Gap, Scale96ToFont(178));
    //  AND THE PANEL FOLLOWS THE PANE. The tabs fill the panel and the pane is
    //  the tab's client area, so the panel has to be wider than the pane by
    //  whatever the page control's borders take. Laying the buttons out for a
    //  width the panel has not got is what puts a caption under the scroll bar -
    //  the one arrangement neither the layout check nor the eye catches until a
    //  module adds a longer row.
    if PanelLeft.Width < PaneW + PaneChrome then
        PanelLeft.Width := PaneW + PaneChrome;

    //  WHERE EACH ONE GOES is tool_pane_layout's answer, not this method's: it
    //  is arithmetic over a row count nothing knows until run time, and a
    //  button drawn over another looks exactly like the pane not offering that
    //  command. The buttons were made group by group, so they are walked in
    //  that order.
    Metrics.PaneWidth := PaneW;
    Metrics.Gap := Gap;
    Metrics.ButtonHeight := Scale96ToFont(23);
    Metrics.HeadingHeight := Scale96ToFont(17);
    Layout := TToolPaneLayout.Create(Metrics);
    try
        i := 0;
        for g := 0 to High(FToolHeadings) do
        begin
            Box := Layout.StartGroup;
            FToolHeadings[g].SetBounds(Box.Left, Box.Top, Box.Width, Box.Height);
            while (i <= High(FToolButtons)) and
                (FCommands.Item(FToolRows[i]).Group = FToolGroups[g]) do
            begin
                Box := Layout.NextButton;
                FToolButtons[i].SetBounds(Box.Left, Box.Top, Box.Width,
                    Box.Height);
                Inc(i);
            end;
            Layout.EndGroup;
        end;
    finally
        Layout.Free;
    end;
end;

{ Writes the polled state into the generated widgets, and nothing else. }
{ Reports every way the Tools pane and the menus disagree about the commands
  they were both generated from.

  READ OFF THE WIDGETS, not off the table that made them. Asserting that the
  table equals itself would pass in every build; what can be wrong is a widget
  that stopped following it - a button never refreshed, an action whose Enabled
  is written somewhere else as well, a hint the pane took before the action had
  one. So this reads the button and the action, and ui_selfcheck says which
  pairs disagree.

  AFTER A REFRESH, deliberately: with the model state written onto both surfaces
  first, so the enabled states compared are the ones the user would be looking
  at rather than whatever construction left behind. }
{ Every pane row as the widgets currently show it. A function because the check
  runs it TWICE: once in the state the window is in, and once with a picking mode
  running - a latch that disagrees with its mode can only be seen while one is. }
function TFormMain.SurfaceRowsNow: TSurfaceRows;
var
    Target: TComponent;
    i, Row: longint;
begin
    SetLength(Result, Length(FToolButtons));
    for i := 0 to High(FToolButtons) do
    begin
        Row := FToolRows[i];
        Result[i].Id := FCommands.Item(Row).Id;
        Result[i].PaneCaption := FToolButtons[i].Caption;
        Result[i].PaneHint := FToolButtons[i].Hint;
        Result[i].PaneEnabled := FToolButtons[i].Enabled;
        Result[i].PaneWidth := FToolButtons[i].Width;
        //  THE LATCH, and what the mode says it should be. A picking button is
        //  the only place "this mode is running" is stated, so the two must
        //  agree - the table's answer comes from the polled selection mode.
        Result[i].HasPicking := FCommands.Item(Row).HasPicking;
        Result[i].PaneDown := FToolButtons[i].Down;
        Result[i].ModeSaysDown := FCommands.IsDown(Row);

        Target := nil;
        if Row <= High(FCommandTargets) then
            Target := FCommandTargets[Row];
        //  A module's row drives no ACTION of this window's - but it has a menu
        //  entry, the one the module declared under Model, and that entry and
        //  this button were generated from the same declaration by two
        //  different loops. Which is exactly the pair worth watching: the
        //  module speaks to the entry through IUiHost and to the button through
        //  the command table, and nothing but this compares the two.
        Result[i].HasMenuSide := Assigned(Target);
        if Target is TAction then
        begin
            Result[i].MenuHint := TAction(Target).Hint;
            Result[i].MenuEnabled := TAction(Target).Enabled;
        end
        else if Target is TMenuItem then
        begin
            Result[i].MenuHint := TMenuItem(Target).Hint;
            Result[i].MenuEnabled := TMenuItem(Target).Enabled;
            //  THE TICK IS THE MENU'S WAY of saying what the latch says, and
            //  only a latching row has one: comparing a plain command's button,
            //  never pressed, with a tick nothing writes would report every
            //  ordinary row in every build.
            Result[i].HasMenuTick := FCommands.Item(Row).WithChecked;
            Result[i].MenuChecked := TMenuItem(Target).Checked;
        end;
    end;

end;

procedure TFormMain.ReportSurfaceDisagreements;
var
    Rows: TSurfaceRows;
    Findings: TUiFindings;
    i: longint;
begin
    if not Assigned(FCommands) then
        Exit;
    //  Both surfaces brought up to date first, so the states compared are the
    //  ones the user would be looking at rather than whatever construction left
    //  behind.
    CheckState;
    Rows := SurfaceRowsNow;
    Findings := SurfaceFindings(Rows);
    for i := 0 to High(Findings) do
        WriteLog('ui: ' + Findings[i], Warning);
    //  The verdict either way, because a check that logs only on a finding
    //  cannot be told from one that did not run.
    //  QUALIFIED, because TComponent has a Notification method and the plain
    //  name resolves to it inside this class - the same reason the settings log
    //  below spells it out.
    WriteLog(SurfaceSummary(Length(Rows), Length(Findings)), log.Notification);
end;

procedure TFormMain.RefreshToolPane;
var
    i: longint;
    Counts: TModelCounts;

    function CountOf(APoints: TNeutronPointsSet): longint;
    begin
        Result := 0;
        if Assigned(APoints) then
            Result := APoints.PointsCount;
    end;

begin
    if not Assigned(FToolBox) then
        Exit;
    //  Same reason as RefreshModelStructure: this is reachable before the
    //  client is there to be counted.
    if not Assigned(FitClientApp_) then
        Exit;
    if not Assigned(FitClientApp_.FitClient) then
        Exit;

    for i := 0 to High(FToolButtons) do
    begin
        FToolButtons[i].Enabled := FCommands.IsEnabled(FToolRows[i]);
        //  Down on a latch only - a picking mode of the framework's, or a
        //  module's toggle or radio. Writing it on a plain button would leave
        //  one looking held after a click.
        if FCommands.Item(FToolRows[i]).HasPicking or
            FCommands.Item(FToolRows[i]).Latching then
            FToolButtons[i].Down := FCommands.IsDown(FToolRows[i]);
    end;

    Counts := EmptyModelCounts;
    Counts.Positions := CountOf(FitClientApp_.FitClient.GetCurvePositions);
    Counts.Intervals := CountOf(FitClientApp_.FitClient.GetRFactorBounds);
    Counts.BackgroundPoints :=
        CountOf(FitClientApp_.FitClient.GetBackgroundPoints);

    //  A COUNT IS THE ONE THING THE MENUS CANNOT SHOW, which is why the
    //  headings carry it.
    for i := 0 to High(FToolHeadings) do
        FToolHeadings[i].Caption := GroupHeading(FToolGroups[i], Counts);
end;

{ A generated button was pressed. Which command that is, and what running it
  means, are both the table's answer. }
procedure TFormMain.ToolButtonClick(Sender: TObject);
var
    Row: longint;
    Target: TCommandTarget;
    Act: TBasicAction;
begin
    RequestStateRefresh;
    if not (Sender is TSpeedButton) then
        Exit;
    Row := TSpeedButton(Sender).Tag;
    if (Row < 0) or (Row > High(FToolRows)) then
        Exit;

    Target := FCommands.TargetOf(FToolRows[Row]);
    case Target.Kind of
        ctAction:
        begin
            //  THE SAME ACTION the menu entry runs, so one command cannot mean
            //  two things depending on where it was pressed.
            Act := ActionList.ActionByName(Target.ActionName);
            if Assigned(Act) then
                Act.Execute;
        end;
        ctModuleCommand:
            RunModuleCommand(FToolRows[Row]);
    end;
end;

{ Refills the curve-type list from the entries the menu is built from.

  THE SAME ENTRIES, so the list and the menu cannot disagree about what exists
  or which type is selected. A list box has no submenus, so the grouping the
  menu expresses by nesting is expressed here by header rows - a second
  projection of one decision, in curve_type_menu, not a second decision. }
procedure TFormMain.RefreshCurveTypeList(const AEntries: TCurveMenuEntries);
var
    i, Sel: longint;
begin
    if not Assigned(FCurveTypeList) then
        Exit;

    FCurveRows := CurveTypeListRows(AEntries);
    FCurveTypeList.Items.BeginUpdate;
    try
        FCurveTypeList.Items.Clear;
        for i := 0 to High(FCurveRows) do
            if FCurveRows[i].IsHeader then
                FCurveTypeList.Items.Add(FCurveRows[i].Caption)
            else
                //  Indented under the heading it belongs to. The caption itself
                //  stays clean in curve_type_menu, where it is what the menu
                //  shows too.
                FCurveTypeList.Items.Add('    ' + FCurveRows[i].Caption);
    finally
        FCurveTypeList.Items.EndUpdate;
    end;

    //  REFILLING DESTROYS NO WIDGET the user is standing on, which is the whole
    //  hazard the curve-type MENU has to be rebuilt around - see
    //  QueueCurveTypeMenuRebuild. A list box only changes its strings.
    Sel := SelectedCurveRow(FCurveRows);
    if Sel >= 0 then
        FCurveTypeList.ItemIndex := Sel;

    //  THE PANE IS LEFT ALONE. A refill is not the user choosing a type, and the
    //  type the model uses is not something to explain unless it is clicked -
    //  CurveTypeListClick is what shows it.
end;

{ A curve type was chosen from the list. }
procedure TFormMain.CurveTypeListClick(Sender: TObject);
var
    Row: longint;
begin
    RequestStateRefresh;
    if not Assigned(FCurveTypeList) then
        Exit;

    //  A CLICK LANDS ON WHATEVER ROW IS UNDER THE POINTER, and a header is a
    //  row. Without resolving it the user clicks a group name, the highlight
    //  moves and the model keeps the previous type - the list and the model
    //  then disagree about what is selected.
    Row := NextSelectableRow(FCurveRows, FCurveTypeList.ItemIndex);
    if Row < 0 then
        Exit;
    if Row <> FCurveTypeList.ItemIndex then
        FCurveTypeList.ItemIndex := Row;
    if FCurveRows[Row].Selected then
    begin
        //  Already the model's type. Selecting it again would rebuild the model
        //  from nothing for no change - but the user clicked it, which is
        //  asking about it, so the pane answers.
        FExplainFocus := FocusCurveTypeSelected(FExplainFocus,
            SelectedCurveTypeTopic);
        RefreshExplainPane;
        Exit;
    end;

    //  THE SAME PATH THE MENU TAKES, by the same registry handle. That is what
    //  makes the list run the identical configuration and refusal flow - a user
    //  curve with no formula is refused here for the same reason and in the
    //  same words.
    SelectCurveTypeByTag(FCurveRows[Row].Tag);
end;

procedure TFormMain.BuildCommandTable;
var
    i: longint;
    Mods: TUiModuleArray;
    m: longint;
    Name_: string;
    Target: TComponent;
begin
    FCommands := TCommandTable.Create;
    FCommands.AddFrameworkCommands;

    //  A module's rows come from the SAME declarations BuildModuleMenus reads,
    //  so one declaration feeds the menu, the Tools pane and the context menu.
    Mods := RegisteredUiModules;
    for m := 0 to High(Mods) do
        FCommands.AddModuleCommands(m, Mods[m].Name, Mods[m].MenuItems);

    SetLength(FCommandTargets, FCommands.Count);
    for i := 0 to FCommands.Count - 1 do
    begin
        FCommandTargets[i] := nil;
        Name_ := FCommands.Item(i).TargetName;
        if Name_ = '' then
        begin
            //  A module's row names no component of this window's - its widget
            //  is the entry the module declared, addressed by the module's own
            //  id. Bound here so that everything downstream treats it as any
            //  other menu-item row: one apply loop writes its Enabled and its
            //  tick, and one check compares it with the button beside it.
            FCommandTargets[i] := ModuleItemById(FCommands.Item(i).CommandId);
            Continue;
        end;

        Target := FindComponent(Name_);
        CheckAssigned(Target, 'the component "' + Name_ +
            '" that command ' + FCommands.Item(i).Id + ' drives');
        FCommandTargets[i] := Target;
        //  AND ITS HINT COMES WITH IT, so a tool button and the menu entry for
        //  the same command say the same thing. The actions carry the hints
        //  already, beside the captions the menus show; declaring them again in
        //  the table would be two texts for one command.
        if Target is TAction then
            FCommands.AdoptHint(i, TAction(Target).Hint);
    end;
end;

procedure TFormMain.BuildRightPanelTabs;
var
    Bottom: longint;
    ExplainSplit: TSplitter;
    Mods: TUiModuleArray;
    m: longint;
    Rows: TIndexList;
    r: longint;
    Entry: TMenuItem;
begin
    //  The legend's DESIGNED geometry, read before it is reparented and loses
    //  it. Its bottom edge is where the Position/Intensity readouts begin, and
    //  those are anchored to the panel's bottom, so the tabs must stop there
    //  rather than take the whole client area.
    Bottom := CheckListBoxLegend.Top + CheckListBoxLegend.Height;

    FRightTabs := TPageControl.Create(Self);
    FRightTabs.Name := 'PageControlRight';
    FRightTabs.Parent := PanelRightContent;
    //  Anchors, not alClient: aligning to the client area would cover the
    //  readouts below instead of leaving them their strip.
    FRightTabs.SetBounds(0, 0, PanelRightContent.ClientWidth, Bottom);
    FRightTabs.Anchors := [akTop, akLeft, akRight, akBottom];

    FTabGraphs := TTabSheet.Create(Self);
    FTabGraphs.Name := 'TabSheetGraphs';
    FTabGraphs.PageControl := FRightTabs;
    FTabGraphs.Caption := 'Graphs';
    //  Setting Align supersedes the four-way anchors it was designed with.
    CheckListBoxLegend.Parent := FTabGraphs;
    CheckListBoxLegend.Align := alClient;
    //  The tab caption says 'Graphs' now; the label would repeat it.
    LabelGraphs.Visible := False;

    //  ---- THE MODEL PANEL, and there is exactly one of it.
    //
    //  It used to be a MODULE's panel: it existed only when a module declared
    //  one, it was captioned in that module's vocabulary, and only one module
    //  could ever have one. So a build with no module had no structure view at
    //  all, and the model - which is one thing - had its description owned by
    //  whichever pack happened to be loaded.
    //
    //  Now the framework owns the panel and either contributor fills it: the
    //  framework for a model built from picks, whoever placed the markup for a
    //  model built from that. Which of the two is not arbitrated - it follows
    //  the selected type's PlacedByPointSet, for the reason model_outline
    //  states.
    FTabModel := TTabSheet.Create(Self);
    FTabModel.Name := 'TabSheetModel';
    FTabModel.PageControl := FRightTabs;
    FTabModel.Caption := ModelPanelCaption;

    FModuleTree := TTreeView.Create(Self);
    FModuleTree.Name := 'TreeViewModel';
    FModuleTree.Parent := FTabModel;
    FModuleTree.Align := alClient;
    FModuleTree.ReadOnly := True;
    FModuleTree.Hint := ModelPanelHint;
    FModuleTree.ShowHint := True;
    //  A panel that only displays is a picture. Selecting a row says which
    //  curve the user means, which is the difference between showing a model
    //  and working on one.
    FModuleTree.OnSelectionChanged := ModulePanelSelectionChanged;
    //  A RIGHT-CLICK SELECTS THE ROW IT LANDS ON, which is not the default and
    //  is why the context menu below was never live: every command on it is
    //  row-scoped, the selected row is the only thing that names a curve, and
    //  a right-click left the selection exactly as it was - so the user
    //  right-clicked a row, got a menu about nothing, and saw one greyed-out
    //  entry. The widget set has the behaviour; nobody asked for it.
    FModuleTree.RightClickSelect := True;

    //  ---- THE EXPLAIN PANE, under the tree and on the same tab.
    //
    //  WHY HERE AND NOT IN A HINT. An explanation is a paragraph or several,
    //  with a quotation, limitations and references, and it is read rather than
    //  glanced at; a tooltip vanishes when the pointer moves and the status bar
    //  holds one line. And the one kind of hint that could follow the pointer
    //  through a context menu does not exist on macOS: Cocoa never reports a
    //  menu entry under the pointer (see explained_menu_item). A pane beside the
    //  tree works identically on all three platforms and covers no chart.
    //
    //  Under the TREE because what it explains most is the row selected in it.
    //
    //  THE VIEW COMES FROM THE PROGRAM, not from this unit. The HTML component
    //  pulls in the printing package, which does not compile against the
    //  headless widget set the test suite is built with - and the suite links
    //  this form. So Fit.lpr links the component (ipro_explain_view) and this
    //  form names only the seam.
    FExplainPane := CreateExplainView(Self, FTabModel);
    CheckThat(Assigned(FExplainPane), 'the Explain pane has a view - the ' +
        'program registers one by linking ipro_explain_view');
    FExplainPane.Control.Name := 'ExplainPane';
    FExplainPane.Control.Align := alBottom;
    FExplainPane.Control.Height := Scale96ToFont(240);
    FExplainPane.SetOnLink(ExplainPaneLinkFollowed);
    ExplainSplit := TSplitter.Create(Self);
    ExplainSplit.Name := 'SplitterExplain';
    ExplainSplit.Parent := FTabModel;
    ExplainSplit.Align := alBottom;
    //  Above the pane: two bottom-aligned controls stack by position.
    ExplainSplit.Top := FExplainPane.Control.Top - 1;
    ExplainSplit.ResizeAnchor := akBottom;
    ExplainSplit.Caption := '';
    FExplainFocus := FocusStart;
    RefreshExplainPane;
    //  ATTACHED, AND CHECKED TO BE - the lesson of PopupViewMode below: a
    //  control built and never parented is invisible, and nothing reports it.
    CheckThat(FExplainPane.Control.Parent = FTabModel,
        'the Explain pane is attached under the Model panel');

    //  HELP > EXPLAIN EVERYTHING replaces the Glossary entry, which was a
    //  hidden action with no handler: an index of every explanation this build
    //  can give is what a glossary of this application is.
    ActionGlossary.Caption := 'Explain Everything...';
    ActionGlossary.Hint := 'The user guide: every command, pane and file of ' +
        'this build, every curve type and every rule a module enforces.';
    ActionGlossary.OnExecute := ExplainEverythingExecute;
    ActionGlossary.Visible := True;

    //  ---- what can be done to the selected row, from the SAME declarations
    //  the menus and the Tools pane are drawn from. Delete curve is one
    //  declared command; a module adding a row command declares it the same
    //  way and appears here beside it.
    FModelPopup := TPopupMenu.Create(Self);
    FModelPopup.Name := 'PopupMenuModel';
    Rows := FCommands.IndicesFor(csMenu, scRow);
    for r := 0 to High(Rows) do
    begin
        Entry := TMenuItem.Create(FModelPopup);
        Entry.Name := FWidgetNames.NameFor('MenuModelRow',
            FCommands.Item(Rows[r]).Id);
        Entry.Caption := FCommands.Item(Rows[r]).PaneCaption;
        Entry.Hint := FCommands.Item(Rows[r]).Hint;
        Entry.Tag := Rows[r];
        Entry.OnClick := ModelRowCommandClick;
        FModelPopup.Items.Add(Entry);
        SetLength(FRowCommandItems, Length(FRowCommandItems) + 1);
        FRowCommandItems[High(FRowCommandItems)] := Entry;
    end;
    FModuleTree.PopupMenu := FModelPopup;
    //  AND ITS ENTRIES ARE STATED AS IT OPENS, not as of the last timer tick.
    //  Enablement follows the selected row, the right-click above has just
    //  changed it, and the poll that writes the states runs twice a second -
    //  so without this the menu shows the state of the row that WAS selected,
    //  which is the wrong answer roughly half the time.
    FModelPopup.OnPopup := ModelPopupPopup;
    FModelPopup.OnClose := ModelPopupClose;
    FRowMenuModule := -1;
    //  ATTACHED, AND CHECKED TO BE. PopupViewMode in this same form is
    //  populated, declared checkable and kept ticked by ApplyViewMode - and
    //  assigned to no control, so six maintained entries are reachable by
    //  nobody. A menu built and never attached is invisible, and invisible is
    //  exactly what nothing reports.
    CheckThat(Assigned(FModuleTree.PopupMenu),
        'the Model panel context menu is attached to the panel');
    //  AND THAT IT CAN EVER BE LIVE. Attaching it was not enough: every command
    //  on it is row-scoped, so a menu over a panel that does not select on
    //  right-click is a menu about nothing - which is exactly how it shipped,
    //  showing one permanently greyed entry. Both halves asserted, because
    //  either alone leaves it useless and neither is visible in a build log.
    CheckThat(FModuleTree.RightClickSelect,
        'the Model panel selects the row a right-click lands on');
    CheckThat(Assigned(FModelPopup.OnPopup),
        'the Model panel context menu refreshes its entries as it opens');

    //  A module still says what its own panel is called and what an empty one
    //  reads as - PanelTextFor looks that up by the id a module pushes rows
    //  under. What it no longer gets is a TAB of its own.
    Mods := RegisteredUiModules;
    for m := 0 to High(Mods) do
        if Mods[m].PanelId <> '' then
        begin
            FModulePanelId := Mods[m].PanelId;
            Break;
        end;

    FRightTabs.ActivePage := FTabGraphs;
end;

{ A row command was chosen over the Model panel. }
{ Enables or disables the Model panel's context entries.

  A ROW-SCOPED COMMAND NEEDS A ROW, and the panel's selection is the only thing
  that names one. Offered DISABLED rather than hidden when nothing is selected:
  an entry that vanishes tells the user nothing about why. }
{ The context menu is about to open over whatever row was just right-clicked.

  CheckState rather than ApplyRowCommandStates alone: the entry's availability
  comes from the command table, and the table has to be refreshed from the
  window's current inputs - the selected row among them - before what it says is
  about this row rather than the previous one. }
{ Reports every legend row that does not match the series it names.

  READ OFF BOTH WIDGETS. The row's text comes from the legend and the title from
  the series the row carries as its object, and whether that series is still on
  the chart is asked of the chart. Nothing here trusts a position: the two lists
  were index-parallel only by luck of construction, and after the first fit row i
  named one series and controlled another - ticking a row then hid a curve the
  user was not pointing at.

  UNDER /CHECK_UI, after a model has been built, which is when the legend has
  more than the profile in it. }
procedure TFormMain.ReportLegendPairing;
var
    Rows: TLegendRows;
    Findings: TUiFindings;
    Serie: TObject;
    i, j: longint;
begin
    //  NOT GUARDED BY USE_LEGEND. That define is set per unit - fit_viewer,
    //  fit_client and int_fit_viewer each declare it at the top of themselves -
    //  and the project defines nothing of the kind, so an IFDEF here compiled
    //  the whole method away and the check silently did not run. The legend
    //  this window owns is unconditional.
    SetLength(Rows, CheckListBoxLegend.Items.Count);
    for i := 0 to CheckListBoxLegend.Items.Count - 1 do
    begin
        Rows[i].Text_ := CheckListBoxLegend.Items[i];
        Serie := CheckListBoxLegend.Items.Objects[i];
        Rows[i].HasSeries := Assigned(Serie);
        Rows[i].SeriesOnChart := False;
        Rows[i].SeriesTitle := '';
        if not Rows[i].HasSeries then
            Continue;
        //  ASKED OF THE CHART, not assumed: a series taken off it and freed
        //  leaves the row holding a pointer, and that pointer is what ticking
        //  the row would follow.
        for j := 0 to Chart.SeriesCount - 1 do
            if Chart.GetSerie(j) = Serie then
            begin
                Rows[i].SeriesOnChart := True;
                Rows[i].SeriesTitle := TTASerie(Serie).Title;
                Break;
            end;
    end;

    Findings := LegendFindings(Rows);
    for i := 0 to High(Findings) do
        WriteLog('ui: ' + Findings[i], Warning);
    WriteLog(LegendSummary(Length(Rows), Length(Findings)), log.Notification);
end;

{ Runs a real fit and reports what this window drew while it ran.

  THE ONLY PLACE THE FEATURE IS ACTUALLY EXERCISED. Every other test of live
  progress drives TFitClient with a mocked transport and a recording viewer: it
  proves the client asks the engine and hands frames on. What none of them can
  touch is the part the user watches - a TTimer firing in a real event loop
  while a fit occupies another thread and a real socket answers the polls. The
  feature was reported dead by its user twice while all of those were green.

  ANIMATION MODE, because it is the stricter of the two: it needs the poll, the
  snapshot the engine builds only while asked, and the redraw. A build in which
  the curves move is one in which the loss chart would have filled too.

  IT MUTATES THE MODEL, like the check above it and for the same reason: this
  runs under /CHECK_UI in a process that terminates immediately afterwards, on
  a file the checking task opened for the purpose, and nothing is saved. }
procedure TFormMain.ReportLiveFitProgress;
const
    { Long enough for a fit over a handful of curves, short enough that a
      wedged one fails the build rather than hanging it. }
    WatchedFitTimeoutMs = 90 * 1000;
var
    Reading: TLiveProgressReading;
    Findings: TUiFindings;
    Profile: TNeutronPointsSet;
    Blocked: string;
    Started: TDateTime;
    WasAnimating: boolean;
    i, Points: longint;
begin
    if not Assigned(FitClientApp_) then
        Exit;
    if not Assigned(FitClientApp_.FitClient) then
        Exit;

    Profile := nil;
    Points := 0;
    if FitClientApp_.FitClient.OpenState = OpenSuccess then
    begin
        Profile := FitClientApp_.FitClient.GetProfilePoints;
        if Assigned(Profile) then
            Points := Profile.PointsCount;
    end;
    Blocked := RowCommandBlocked(
        FitClientApp_.FitClient.OpenState = OpenSuccess, Points);
    if Blocked <> '' then
    begin
        WriteLog('ui: ' + Blocked, Warning);
        Exit;
    end;

    //  MORE CURVES THAN THE CHECK BEFORE THIS ONE PLACED. A fit of two curves
    //  over a clean profile converges in a few dozen milliseconds, which is
    //  less than one tick of the window's timer - and a check whose fit is
    //  over before the first frame could be drawn would report nothing either
    //  way. These are read off the profile, as picks must be.
    //  ONLY WHEN THE MODEL IS THE SMALL ONE THE CHECK BEFORE THIS BUILT. Run
    //  over a project the user opened, the curves already there are the ones
    //  worth watching a fit of, and adding more would turn a fit of seconds
    //  into one of minutes.
    if Assigned(FitClientApp_.FitClient.GetCurvePositions) and
        (FitClientApp_.FitClient.GetCurvePositions.PointsCount < 3) then
        for i := 1 to 11 do
            FitClientApp_.FitClient.AddPointToCurvePositions(
                Profile.PointXCoord[(i * Points) div 12],
                Profile.PointYCoord[(i * Points) div 12]);

    WasAnimating := FitClientApp_.FitClient.AnimationMode;
    FitClientApp_.FitClient.AnimationMode := True;
    FProgressFramesDrawn := 0;
    FMostLossPointsDrawn := 0;
    FProgressTimerTicks := 0;

    Reading := Default(TLiveProgressReading);
    Reading.Animating := True;
    Started := Now;
    try
        FitClientApp_.FitClient.MinimizeDifference;
        //  THE WINDOW'S OWN LOOP, which is the thing under test: the timer
        //  fires from here, and the fit's completion arrives here too.
        while (FitClientApp_.FitClient.AsyncState = AsyncWorks) and
            (MilliSecondsBetween(Now, Started) < WatchedFitTimeoutMs) do
        begin
            Application.ProcessMessages;
            Sleep(5);
        end;
        Reading.Ran := FitClientApp_.FitClient.AsyncState <> AsyncWorks;
        Reading.Seconds := MilliSecondsBetween(Now, Started) / 1000;
        Reading.Frames := FProgressFramesDrawn;
        Reading.LossPoints := FMostLossPointsDrawn;
        Reading.Curves := FitClientApp_.FitClient.ModelFramesDrawn;
        Reading.Ticks := FProgressTimerTicks;
    finally
        FitClientApp_.FitClient.AnimationMode := WasAnimating;
    end;

    Findings := LiveProgressFindings(Reading);
    for i := 0 to High(Findings) do
        WriteLog('ui: ' + Findings[i], Warning);
    WriteLog(LiveProgressSummary(Reading), log.Notification);
end;

{ Whether the Model panel's context menu can ever actually be used.

  WHY THIS DRIVES THE MODEL ITSELF. Every command on that menu is row-scoped, so
  the only state in which it means anything is "a row is selected and that row
  names a curve" - and nothing on the machine could reach that state. The panel
  is empty until picks are placed, placing picks needs the chart clicked, and so
  the one entry there is shipped permanently greyed: the input saying a row names
  a curve was assigned AFTER the decision that reads it, and both halves were
  right on their own.

  So this builds the smallest model that produces rows - two picks and an
  interval, through the same client calls the pane's buttons make - selects the
  first row that names a curve, and asks the menu whether it offers anything.
  No pointer, and it fails the build rather than waiting to be noticed.

  IT MUTATES THE MODEL, deliberately and only here: this runs under /CHECK_UI in
  a process that terminates immediately afterwards, on a file the checking task
  opened for the purpose. Nothing is saved. }
procedure TFormMain.ReportRowCommandReachability;
var
    //  TNeutronPointsSet rather than the TTitlePointsSet the client hands back:
    //  the base type is what this unit already names, and nothing here reads a
    //  title.
    Profile: TNeutronPointsSet;
    Was, Blocked: string;
    Node: TTreeNode;
    Any: boolean;
    Reach: TRowCommandReach;
    SerieIndex: longint;
    TableRow: longint;
    TableHandle: string;
    Reading: THighlightReading;
    Findings: TUiFindings;
    i, Points: longint;
    { One row of the shape an analysis pack sends: identified by the
      contributor's own markup, naming the curve it stands for separately. }
    ContributorRow: TOutline;
begin
    if not (Assigned(FModelPopup) and Assigned(FModuleTree)) then
        Exit;
    if not Assigned(FitClientApp_) then
        Exit;
    if not Assigned(FitClientApp_.FitClient) then
        Exit;

    //  WHETHER THIS CAN RUN, and the sentence to say when it cannot, are
    //  ui_selfcheck's - the same division the findings below follow. What is
    //  left here is reading the two things only the client can answer.
    Profile := nil;
    Points := 0;
    if FitClientApp_.FitClient.OpenState = OpenSuccess then
    begin
        Profile := FitClientApp_.FitClient.GetProfilePoints;
        if Assigned(Profile) then
            Points := Profile.PointsCount;
    end;
    Blocked := RowCommandBlocked(
        FitClientApp_.FitClient.OpenState = OpenSuccess, Points);
    if Blocked <> '' then
    begin
        //  SAID, not skipped silently: a check that quietly does nothing is
        //  worse than one that is absent - it reads as a pass.
        WriteLog('ui: ' + Blocked, Warning);
        Exit;
    end;

    Was := FSelectedRowId;

    //  THE SMALLEST MODEL THAT HAS CURVES IN IT: an interval spanning the data,
    //  and two picks inside it. A pick must name a real sample of the profile,
    //  which is why these are read off it rather than invented.
    FitClientApp_.FitClient.AddPointToRFactorBounds(
        Profile.PointXCoord[0], Profile.PointYCoord[0]);
    FitClientApp_.FitClient.AddPointToRFactorBounds(
        Profile.PointXCoord[Profile.PointsCount - 1],
        Profile.PointYCoord[Profile.PointsCount - 1]);
    FitClientApp_.FitClient.AddPointToCurvePositions(
        Profile.PointXCoord[Profile.PointsCount div 3],
        Profile.PointYCoord[Profile.PointsCount div 3]);
    FitClientApp_.FitClient.AddPointToCurvePositions(
        Profile.PointXCoord[(2 * Profile.PointsCount) div 3],
        Profile.PointYCoord[(2 * Profile.PointsCount) div 3]);

    RefreshModelStructure;

    //  The first row that names a curve. A row a contributor put there names
    //  whatever that contributor chose, and no framework command applies to it.
    Any := False;
    Node := FModuleTree.Items.GetFirstNode;
    while Assigned(Node) do
    begin
        if Assigned(FOutlineGuids) and (PtrInt(Node.Data) > 0) and
            (FOutlineGuids[PtrInt(Node.Data) - 1] <> '') then
        begin
            FModuleTree.Selected := Node;
            Any := True;
            Break;
        end;
        Node := Node.GetNext;
    end;

    if not Any then
    begin
        WriteLog('ui: ' + NoRowNamesACurve, Warning);
        Exit;
    end;

    //  Exactly what the menu does as it opens.
    RefreshRowCommands;
    Reach.EntryCount := FModelPopup.Items.Count;
    Reach.OfferedOnOwnRow := AnyContextEntryEnabled;
    ReportHighlight(ReadHighlight(True), 'over the framework''s own row');

    //  ---- AND FROM THE CURVE ATTRIBUTES TABLE, the other place a curve is
    //  chosen. Entered below the focus test GridSelectionChanged makes: this
    //  window is never focused, and that test is what keeps a refill of the
    //  table from moving the selection - not what is measured here. A row
    //  naming a curve OTHER than the one selected, so that a selection which
    //  did not move cannot pass for one that did.
    TableHandle := '';
    TableRow := -1;
    if Assigned(FCurveList) then
    begin
        TableRow := FCurveList.RowNamingAnotherCurve(FSelectedCurveId);
        TableHandle := FCurveList.CurveHandleOfRow(TableRow);
    end;
    if TableHandle = '' then
        WriteLog('ui: ' + NoTableRowToSelect, Warning)
    else
    begin
        SelectCurveOfTableRow(TableRow);
        Reading := ReadHighlight(True);
        Reading.ExpectedCurveId := TableHandle;
        ReportHighlight(Reading, 'after a Curve Attributes row was selected');
    end;

    //  ---- AND OVER A ROW A CONTRIBUTOR PUT THERE, which is the one this
    //  check could not see and the one the user reported. A pack fills this
    //  panel for its own curve types and identifies its rows by its own markup,
    //  so the row's id is NOT a curve handle - the handle travels beside it. The
    //  window used to answer "which curve is selected?" by asking whose rows
    //  these were, and every contributor row answered "no curve": Delete curve
    //  was greyed over every pattern in a wave count, however it was clicked,
    //  while this check went on passing over the framework's own rows.
    //
    //  A ROW OF THE SHAPE A PACK SENDS, pushed through the entry point a pack
    //  uses, over a curve the model really holds - so what is exercised is the
    //  window's wiring, not a stub of it. The framework ships no pack, and this
    //  is the only place in a framework build where that wiring runs at all.
    ContributorRow := nil;
    SetLength(ContributorRow, 1);
    ContributorRow[0].Indent := 0;
    ContributorRow[0].Caption := 'A row a contributor put here';
    //  Its own identity, which is deliberately not a handle.
    ContributorRow[0].Id := 'check-ui.contributor-row';
    ContributorRow[0].CurveId :=
        FitClientApp_.FitClient.FitService.GetCurveInstanceId(0);
    ContributorRow[0].IsDetached := False;

    Reach.ContributorCurveId := ContributorRow[0].CurveId;
    Reach.SelectedCurveId := '';
    Reach.OfferedOnContributorRow := False;
    if Reach.ContributorCurveId <> '' then
    begin
        ShowModulePanel('check-ui.contributor', ContributorRow);
        FModuleTree.Selected := FModuleTree.Items.GetFirstNode;
        Reach.SelectedCurveId := FSelectedCurveId;

        ReportHighlight(ReadHighlight(True), 'over a contributor row');

        RefreshRowCommands;
        Reach.OfferedOnContributorRow := AnyContextEntryEnabled;

        WriteLog(ContributorRowSummary(Reach.OfferedOnContributorRow,
            Reach.EntryCount), log.Notification);

        //  AFTER THE MENU WAS READ, deliberately: clearing the selection below
        //  disables every command on one curve, and read before it the menu
        //  over this row reported itself dead for a reason the check caused.
        //  AND AFTER A REPLOT, which re-creates every series. The series are
        //  first put back to the normal style BY HAND, past the viewer: a
        //  replot that re-applies nothing would otherwise leave the old
        //  series looking right, and comparing pointers would not say
        //  whether they were re-created, since a freed series' address is
        //  usually reused by the next. Only a re-applied highlight passes.
        for SerieIndex := 0 to FFitViewer.Series.Count - 1 do
        begin
            TTASerie(FFitViewer.Series.Item(SerieIndex).Serie).LineWidth :=
                NormalLineWidth;
            TTASerie(FFitViewer.Series.Item(SerieIndex).Serie).DrawOnTop := False;
        end;
        //  TRUE, as every refresh after an edit passes: False is the redraw
        //  during a running fit, which skips the curve positions - and left
        //  without them, the picking check below this one failed an internal
        //  check and took the rest of the checks with it.
        FitClientApp_.FitClient.UpdateComputedData(True);
        ReportHighlight(ReadHighlight(True), 'after the chart was replotted');

        //  AND CLEARED, as Delete curve clears it: nothing may stay wide.
        SetSelectedCurveId('');
        ReportHighlight(ReadHighlight(False), 'with no curve selected');

        //  Put the framework's own rows back, so nothing after this check reads
        //  a panel this check invented.
        RefreshModelStructure;
    end;

    //  EVERY JUDGEMENT AT ONCE, from what was gathered above. Which readings are
    //  wrong, and the words each finding is reported in, are ui_selfcheck's -
    //  the same division SurfaceFindings and LegendFindings already follow, and
    //  the reason those rules can be read and tested without a display.
    Findings := RowCommandFindings(Reach);
    for i := 0 to High(Findings) do
        WriteLog('ui: ' + Findings[i], Warning);

    //  ---- AND THE LATCH, WITH A MODE ACTUALLY RUNNING.
    //
    //  Without this the rule passes for nothing: no picking mode runs during a
    //  check, so every button is up, the table says up, and they agree
    //  trivially. A picking button is the ONLY place "this mode is running" is
    //  stated - the menu entry says start or stop instead - so it is worth
    //  entering one and looking.
    FitClientApp_.FitClient.SelectionMode := ModeSelectCurvePositions;
    CheckState;
    Findings := SurfaceFindings(SurfaceRowsNow);
    for i := 0 to High(Findings) do
        WriteLog('ui: with a picking mode running, ' + Findings[i], Warning);
    //  Left as it was found, so nothing after this check inherits a mode it did
    //  not ask for.
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
    CheckState;

    //  A VERDICT EITHER WAY, by the same rule the other two checks follow: one
    //  that speaks only when it finds something cannot be told from one that
    //  never ran, and this one has several reasons to bow out early.
    WriteLog(RowCommandSummary(Reach.OfferedOnOwnRow, Reach.EntryCount),
        log.Notification);

    FSelectedRowId := Was;
end;

{ The table refreshed from the current inputs, and the entries brought into line
  with it - exactly what the context menu does as it opens.

  ONE PLACE, because the self-check has to do the same thing to read the same
  answer: two copies of this sequence would let the check pass over a menu the
  user never sees. }
procedure TFormMain.RefreshRowCommands;
var
    Inputs: TUiInputs;
begin
    Inputs := GatherUiInputs;
    FCommands.Refresh(CommandStates(Inputs), Inputs.Selection,
        CurrentModelCounts);
    ApplyRowCommandStates;
end;

{ Whether the context menu currently offers anything at all. }
function TFormMain.AnyContextEntryEnabled: boolean;
var
    i: longint;
begin
    Result := False;
    if not Assigned(FModelPopup) then
        Exit;
    //  The command table's entries: what this answers is whether the
    //  FRAMEWORK's commands can be used over the row.
    for i := 0 to High(FRowCommandItems) do
        if FRowCommandItems[i].Enabled then
            Exit(True);
end;

procedure TFormMain.ModelPopupPopup(Sender: TObject);
begin
    //  THE TABLE, NOT THE WHOLE WINDOW. This called CheckState, which also
    //  refills the Model panel - rebuilding the tree view whose context menu is
    //  in the middle of opening, and dropping and restoring the selection the
    //  menu is about. The entries need the table refreshed from the current
    //  inputs and nothing else.
    RefreshRowCommands;
    //  AND WHAT THE MODULE OFFERS OVER THIS ROW, asked now: the row was just
    //  right-clicked, and what a module can do to it depends on what it is.
    RebuildRowMenu;
    FExplainFocus := FocusMenuOpened(FExplainFocus);
    RefreshExplainPane;
end;

procedure TFormMain.ModelPopupClose(Sender: TObject);
begin
    FExplainFocus := FocusMenuClosed(FExplainFocus);
    RefreshExplainPane;
end;

procedure TFormMain.RebuildRowMenu;
var
    Mods: TUiModuleArray;
    Decls: TUiMenuDeclArray;
    Created: array of TMenuItem;
    Item: TExplainedMenuItem;
    Separator: TMenuItem;
    Asked: TModuleIndexes;
    i, a: longint;
begin
    for i := 0 to High(FRowMenuItems) do
        FRowMenuItems[i].Free;
    FRowMenuItems := nil;
    FRowMenuNodes := nil;
    FRowMenuRowId := FSelectedRowId;
    FRowMenuModule := -1;
    if not Assigned(FModelPopup) then
        Exit;

    //  WHOSE ROWS THESE ARE decides who is asked - module_menu, where it can be
    //  tried without a window: the owner over a module's rows, every module in
    //  turn over the framework's, and the first that offers something owns
    //  the click.
    if FRowMenuRowId = '' then
        Exit;
    //  Nor here, and for the same reason: what a module offers over a row is
    //  the module's answer to give, and giving it may mean reading the model.
    //  The framework's own entries are in the table above and stay, disabled by
    //  the command rules as they already are during an operation.
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) and
        not ModuleMayBeCalledBack(FitClientApp_.FitClient.AsyncState) then
        Exit;
    Mods := RegisteredUiModules;
    Asked := RowMenuModulesToAsk(Mods, FShownPanelId);
    Decls := nil;
    for a := 0 to High(Asked) do
    begin
        try
            Decls := Mods[Asked[a]].RowMenuItems(FRowMenuRowId);
        except
            on E: Exception do
            begin
                //  AN EMPTY SECTION, NOT A FAULT. Raised into OnException this
                //  would log at Fatal and stop the state poll - for a menu.
                WriteLog('ui: module ' + Mods[Asked[a]].Name +
                    ' could not say what it offers over this row: ' + E.Message,
                    Warning);
                Decls := nil;
            end;
        end;
        if Length(Decls) > 0 then
        begin
            FRowMenuModule := Asked[a];
            Break;
        end;
    end;

    FRowMenuNodes := ModuleMenuNodes(Decls);
    if Length(FRowMenuNodes) = 0 then
        Exit;

    Separator := TMenuItem.Create(FModelPopup);
    Separator.Caption := SeparatorCaption;
    FModelPopup.Items.Add(Separator);
    SetLength(FRowMenuItems, 1);
    FRowMenuItems[0] := Separator;

    SetLength(Created, Length(FRowMenuNodes));
    for i := 0 to High(FRowMenuNodes) do
    begin
        Item := TExplainedMenuItem.Create(FModelPopup);
        Item.Caption := FRowMenuNodes[i].Caption;
        Item.Hint := FRowMenuNodes[i].Hint;
        //  GREYED, NOT OMITTED, when the module says so - and its topic still
        //  explains why, which is the point of showing it at all.
        Item.Enabled := FRowMenuNodes[i].Enabled;
        Item.Topic := FRowMenuNodes[i].Topic;
        Item.OnTopicHovered := RowMenuTopicHovered;
        //  The node's position, which is what comes back on a click.
        Item.Tag := i;
        if FRowMenuNodes[i].IsRadio then
        begin
            Item.RadioItem := True;
            Item.GroupIndex := FRowMenuNodes[i].RadioGroup;
        end;
        if FRowMenuNodes[i].Checkable then
            Item.ShowAlwaysCheckable := True;
        Item.Checked := FRowMenuNodes[i].Checked;
        if FRowMenuNodes[i].Clickable then
            Item.OnClick := RowMenuItemClick;

        if FRowMenuNodes[i].ParentIndex >= 0 then
            Created[FRowMenuNodes[i].ParentIndex].Add(Item)
        else
        begin
            FModelPopup.Items.Add(Item);
            SetLength(FRowMenuItems, Length(FRowMenuItems) + 1);
            FRowMenuItems[High(FRowMenuItems)] := Item;
        end;
        Created[i] := Item;
    end;
end;

procedure TFormMain.RowMenuItemClick(Sender: TObject);
var
    Mods: TUiModuleArray;
    i: longint;
begin
    RequestStateRefresh;
    if not (Sender is TMenuItem) then
        Exit;
    i := TMenuItem(Sender).Tag;
    if (i < 0) or (i > High(FRowMenuNodes)) then
        Exit;
    Mods := RegisteredUiModules;
    if (FRowMenuModule < 0) or (FRowMenuModule > High(Mods)) then
        Exit;
    //  THE ROW THE MENU OPENED OVER, not whatever is selected by the time the
    //  click arrives.
    Mods[FRowMenuModule].Command(FRowMenuNodes[i].Id, FRowMenuRowId,
        Self as IUiHost);
end;

procedure TFormMain.RowMenuTopicHovered(const ATopic: string);
begin
    FExplainFocus := FocusMenuHovered(FExplainFocus, ATopic);
    RefreshExplainPane;
end;

procedure TFormMain.RefreshExplainPane;
var
    Topic: string;
    E: TExplanation;
begin
    if not Assigned(FExplainPane) then
        Exit;
    Topic := FocusShownTopic(FExplainFocus, @RegisteredTopicResolves);
    if (Topic <> '') and FindExplanation(Topic, E) then
        FExplainPane.ShowHtml(ExplanationHtml(E, @RegisteredTopicTitle))
    else
        FExplainPane.ShowHtml(EmptyExplanationHtml(ExplainPaneEmptyText));
end;

procedure TFormMain.ExplainPaneLinkFollowed(const AHref: string);
var
    Topic: string;
begin
    if TopicFromLink(AHref, Topic) then
    begin
        FExplainFocus := FocusLinkFollowed(FExplainFocus, Topic);
        RefreshExplainPane;
    end
    else if AHref <> '' then
        //  A reference in the literature: read outside the application.
        OpenURL(AHref);
end;

function TFormMain.SelectedCurveTypeTopic: string;
var
    Selector: ICurveTypeSelector;
    Cls: TCurveClass;
begin
    Result := '';
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    if not Assigned(Selector) then
        Exit;
    Cls := FindCurveClassById(Selector.GetSelectedCurveType);
    if Assigned(Cls) then
        Result := CurveTypeTopic(Cls);
end;

procedure TFormMain.ExplainEverythingExecute(Sender: TObject);
begin
    ShowExplainEverything(Self);
end;

procedure TFormMain.ShowExplanation(const ATopic: string);
begin
    FExplainFocus := FocusLinkFollowed(FExplainFocus, ATopic);
    //  Brought to the front, or a module asking to show the rule that refused a
    //  click would change a tab the user cannot see.
    if Assigned(FRightTabs) and Assigned(FTabModel) then
        FRightTabs.ActivePage := FTabModel;
    RefreshExplainPane;
end;

{ The explanation for a topic a report links to. A NAMED ROUTINE with the
  signature written out, for the reason AGENTS gives: in Delphi mode an @Routine
  of the wrong shape is assignment-compatible with any procedural variable, and a
  seam wired wrongly compiles in silence. }
function ReportTopicExplanation(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    Result := FindExplanation(ATopic, AExplanation);
end;

procedure TFormMain.BuildReportTabs;
var
    i: longint;
    Sheet: TTabSheet;
    View: IExplainView;
begin
    FReportTabs := ReportTabsFor(RegisteredUiModules);
    //  A report links to how it is read; a build with no report explains none.
    if Length(FReportTabs) > 0 then
        RegisterReportExplanations;
    SetLength(FReportSheets, Length(FReportTabs));
    SetLength(FReportViews, Length(FReportTabs));
    for i := 0 to High(FReportTabs) do
    begin
        Sheet := TTabSheet.Create(Self);
        Sheet.Name := FWidgetNames.NameFor('TabSheetReport', FReportTabs[i].Module);
        //  LAST, so the index a saved project names for every designed tab is
        //  the same with or without a module that reports.
        Sheet.PageControl := PageControl;
        Sheet.Caption := FReportTabs[i].Caption;
        //  HIDDEN until the module has something to say: an empty tab is
        //  indistinguishable from a broken one (D26).
        Sheet.TabVisible := False;

        //  The same view the Explain pane draws with, so a report and the rule
        //  it links to read as one application.
        View := CreateExplainView(Self, Sheet);
        CheckThat(Assigned(View), 'a report tab has a view - the program ' +
            'registers one by linking ipro_explain_view');
        View.Control.Name := FWidgetNames.NameFor('ReportView', FReportTabs[i].Module);
        View.Control.Align := alClient;
        View.SetOnLink(ReportLinkFollowed);
        View.ShowHtml(EmptyExplanationHtml(ReportTabEmptyText));

        FReportSheets[i] := Sheet;
        FReportViews[i] := View;
        //  ATTACHED, AND CHECKED TO BE, like the Explain pane: a control built
        //  and never parented is invisible, and nothing reports it.
        CheckThat((Sheet.PageControl = PageControl) and
            (Sheet.PageIndex = PageControl.PageCount - 1) and
            (View.Control.Parent = Sheet),
            'a report tab is attached last in the bottom strip, with its view');
    end;
end;

procedure TFormMain.ShowModuleReport(const AReport: TModuleReport;
    ABringForward: boolean);
var
    i: longint;
    Visible: boolean;
begin
    if csDestroying in ComponentState then
        Exit;
    i := ReportTabIndex(FReportTabs, AReport.Module);
    if i < 0 then
    begin
        //  A module reporting without having declared a tab is a defect in the
        //  module, not something to show the user - but not something to lose
        //  in silence either.
        WriteLog(UndeclaredReportWarning(AReport.Module), Warning);
        Exit;
    end;
    FReportViews[i].ShowHtml(ModuleReportHtml(AReport, @ReportTopicExplanation,
        ReportTabEmptyText));
    Visible := ReportTabVisible(AReport);
    FReportSheets[i].TabVisible := Visible;
    if ABringForward and Visible then
        PageControl.ActivePage := FReportSheets[i];
end;

procedure TFormMain.ReportLinkFollowed(const AHref: string);
var
    Topic, RowId: string;
begin
    if TopicFromLink(AHref, Topic) then
        ShowExplanation(Topic)
    else if RowFromLink(AHref, RowId) then
    begin
        if not SelectModelRowById(RowId) then
            ShowHint(RowLinkMissHint(FHeldPanels, RowId));
    end
    else if AHref <> '' then
        OpenURL(AHref);
end;

function TFormMain.SelectModelRowById(const ARowId: string): boolean;
var
    Index: longint;
    Node: TTreeNode;
begin
    Result := False;
    if (not Assigned(FModuleTree)) or (not Assigned(FOutlineGuids)) then
        Exit;
    Index := FOutlineGuids.IndexOf(ARowId);
    if Index < 0 then
        Exit;
    Node := FModuleTree.Items.GetFirstNode;
    while Assigned(Node) do
    begin
        //  1-based in Data, for the reason ModulePanelSelectionChanged gives.
        if PtrInt(Node.Data) = Index + 1 then
        begin
            if Assigned(FRightTabs) and Assigned(FTabModel) then
                FRightTabs.ActivePage := FTabModel;
            FModuleTree.Selected := Node;
            Exit(True);
        end;
        Node := Node.GetNext;
    end;
end;

procedure TFormMain.ApplyRowCommandStates;
var
    i: longint;
begin
    if not Assigned(FModelPopup) then
        Exit;
    //  THE TABLE'S OWN ROWS ONLY - see FRowCommandItems.
    for i := 0 to High(FRowCommandItems) do
        FRowCommandItems[i].Enabled :=
            FCommands.IsEnabled(FRowCommandItems[i].Tag);
end;

procedure TFormMain.ModelRowCommandClick(Sender: TObject);
var
    Row: longint;
    Target: TCommandTarget;
    Act: TBasicAction;
    Mods: TUiModuleArray;
begin
    if not (Sender is TMenuItem) then
        Exit;
    Row := TMenuItem(Sender).Tag;

    Target := FCommands.TargetOf(Row);
    case Target.Kind of
        ctAction:
        begin
            Act := ActionList.ActionByName(Target.ActionName);
            if Assigned(Act) then
                Act.Execute;
        end;
        ctModuleCommand:
        begin
            Mods := RegisteredUiModules;
            if (Target.ModuleIndex >= 0) and
                (Target.ModuleIndex <= High(Mods)) then
                //  THE SELECTED ROW'S ID as the payload, because a row command
                //  is about the row: without it the module would be told that
                //  something was chosen and not what.
                Mods[Target.ModuleIndex].Command(Target.CommandId,
                    FSelectedRowId, Self as IUiHost);
        end;
    end;
end;

{ Fills the Model panel from the framework's own model.

  CALLED WHERE THE MODEL CHANGES, and it decides nothing about what the rows
  say: model_outline does that, where a test can read it. What is here is
  reading the client and putting strings into a tree. }
{ PlacedByPointSet of the selected curve type.

  ASKED OF THE CLASS, and that is the whole point: it is a property of the type,
  known before anything is built. Deriving which contributor fills the panel
  from whether one HAS rows - "did the module handle this?" - is what
  named_points_set records as having generated one curve per data point and
  presented as a hang. }
function TFormMain.SelectedPlacedByPointSet: string;
var
    Selector: ICurveTypeSelector;
    Cls: TCurveClass;
begin
    Result := '';
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    if not Assigned(Selector) then
        Exit;
    Cls := FindCurveClassById(Selector.GetSelectedCurveType);
    if not Assigned(Cls) then
        Exit;
    Result := Cls.PlacedByPointSet;
end;

procedure TFormMain.RefreshModelStructure;
var
    Curves: TSelfCopiedCompList;
    Attrs: TMSCRCurveList;
    Rows: TModelCurveRows;
    i, N: longint;
    Curve: TCurvePointsSet;
    Params: Curve_parameters;
    Position: TSpecialCurveParameter;
    HeldId: string;
    HeldRows: TOutline;
begin
    if not Assigned(FModuleTree) then
        Exit;
    if csDestroying in ComponentState then
        Exit;
    //  REACHED DURING START-UP, before the client exists: the hook this hangs
    //  from is the one every curve-type selection passes through, and the
    //  stored type is restored while the window is still being built.
    if not Assigned(FitClientApp_) then
        Exit;
    if not Assigned(FitClientApp_.FitClient) then
        Exit;
    if not FrameworkFillsStructure(SelectedPlacedByPointSet) then
    begin
        //  The selected type is placed from its own markup, so whoever owns
        //  that markup fills the panel through ShowModulePanel. Writing the
        //  framework's rows over it would be two descriptions of one model.
        //  WHAT IT LAST PUSHED is shown now, if it was held while another type
        //  was selected - otherwise the panel would go on describing the old
        //  model until that module next redrew.
        if HeldRowsToShow(FHeldPanels, SelectedPlacedByPointSet, FShownPanelId,
            HeldId, HeldRows) then
            ShowModulePanel(HeldId, HeldRows);
        Exit;
    end;

    Rows := nil;
    N := 0;
    Curves := FitClientApp_.FitClient.CurvesForDisplay;
    Attrs := FitClientApp_.FitClient.CurveAttributesForDisplay;
    if Assigned(Curves) then
        for i := 0 to Curves.Count - 1 do
        begin
            if not (Curves.Items[i] is TCurvePointsSet) then
                Continue;
            Curve := TCurvePointsSet(Curves.Items[i]);
            SetLength(Rows, N + 1);
            Rows[N].Title := Curve.FTitle;
            //  THE CURVE'S OWN TYPE, not the selected one: a model can hold
            //  several. Read back from the title the engine gives every curve,
            //  because the type itself is not on the wire (model_outline).
            Rows[N].Topic := CurveTypeTopicForName(
                CurveTypeNameOfTitle(Curve.FTitle));
            //  PAIRED BY INDEX with the attributes, which is the pairing the
            //  wire itself uses. The handle lives on the attributes row.
            Rows[N].InstanceId := '';
            Rows[N].HasPosition := False;
            Rows[N].Position := 0;
            if Assigned(Attrs) and (i < Attrs.Count) and
                (Attrs.Items[i] is Curve_parameters) then
            begin
                Params := Curve_parameters(Attrs.Items[i]);
                Rows[N].InstanceId := CurveInstanceIdToWire(Params.FInstanceId);
                //  THE POSITION COMES FROM THE ATTRIBUTES TOO, and asking the
                //  plotted curve was the defect: this asked Curve.Hasx0, and a
                //  curve that arrived over the wire as a point set carries no
                //  parameters at all - so Hasx0 was false for every one of them
                //  and ten curves of one type all read "Asym. Pseudo-Voigt"
                //  with nothing to tell them apart. The position is the ONLY
                //  thing that does, which is what model_outline says it is for.
                //
                //  Through ParameterWithRole, which is where "which parameter
                //  places this curve" already lives - both position types, one
                //  role wearing two hats.
                Position := ParameterWithRole(Params, prPosition);
                if Assigned(Position) then
                begin
                    Rows[N].HasPosition := True;
                    Rows[N].Position := Position.Value;
                end;
            end;
            Inc(N);
        end;

    //  THROUGH THE SAME ENTRY POINT a module's rows arrive by, so one renderer
    //  serves both and the framework's own path is exercised in every build.
    ShowModulePanel(FrameworkStructureId, ModelOutlineOf(Rows));
end;

{ Whether a module's panel is worth putting on the tab strip.

  ASKED of the module, not decided here: the answer combines what the module
  holds with what the user has selected, and only the module knows how those
  relate. The window contributes the one fact it owns - whether the panel
  currently has rows. }
{ Fills a module's panel from rows it has already flattened, indented and
  captioned. The tree mechanics - building nodes, preserving the selection by id
  across a rebuild - stay HERE rather than in the module: they are the same for
  any hierarchy, and every module reimplementing them would be the duplication
  this framework exists to avoid.

  APanelId names which panel. One module contributes one today; the parameter is
  what keeps that from being an assumption baked into the call. }
procedure TFormMain.ShowModulePanel(const APanelId: string;
    const ARows: TOutline);
var
    i: longint;
    Nodes: TOutlineNodes;
    Created: array of TTreeNode;
    Node, Parent, Restore: TTreeNode;
    WasSelected, EmptyText, DetachedSuffix: string;
    Text: TModulePanelText;
    RestoreAt: longint;
    Mods: TUiModuleArray;
    PanelIds: array of string;
begin
    if csDestroying in ComponentState then
        Exit;
    if not Assigned(FModuleTree) then
        Exit;

    //  KEPT, AND SHOWN ONLY while one of the module's types is selected: the
    //  panel describes the model the user is working on, and a module redrawing
    //  does not change which that is. model_outline.PushPanelRows decides.
    Mods := RegisteredUiModules;
    PanelIds := nil;
    SetLength(PanelIds, Length(Mods));
    for i := 0 to High(Mods) do
        PanelIds[i] := Mods[i].PanelId;
    if not PushPanelRows(FHeldPanels, APanelId, SelectedPlacedByPointSet,
        PanelIds, ARows) then
        Exit;

    //  THE MODULE OWNS THE WORDING FOR BOTH OF THESE, and which module that is
    //  is decided in module_menu, where "no module claims this id" can be told
    //  apart from "this module says nothing".
    Text := PanelTextFor(RegisteredUiModules, APanelId);
    EmptyText := Text.EmptyText;
    if APanelId = FrameworkStructureId then
        //  THE FRAMEWORK'S OWN ROWS. PanelTextFor finds nothing for this id -
        //  deliberately, because it must not lend a module's wording to them -
        //  so the framework supplies its own, and it says WHY the panel is
        //  empty: nothing open, nothing placed, or a type that places itself.
        EmptyText := EmptyStructureText(
            Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) and
                (FitClientApp_.FitClient.OpenState = OpenSuccess),
            FrameworkFillsStructure(SelectedPlacedByPointSet));
    DetachedSuffix := Text.DetachedSuffix;

    //  WHAT THE PANEL NOW HOLDS, recorded where the rows arrive - the one place
    //  that knows for certain, and BEFORE any node is made: putting the
    //  selection back below fires the selection handler, which asks these rows
    //  which curve the restored row names. Recorded afterwards, that answer
    //  would come from the rows the panel held a moment ago.
    FShownRows := ARows;
    FShownPanelId := APanelId;

    //  The selection cannot survive a rebuild by row position, so it is
    //  re-established below by GUID or dropped - never left pointing at
    //  whatever now occupies the old row.
    //  Held by the WINDOW only for the length of a rebuild: it is how a
    //  selection survives one, not a fact about the module's data.
    WasSelected := FSelectedRowId;
    Restore := nil;

    FModuleTree.Items.BeginUpdate;
    try
        FModuleTree.Items.Clear;
        if not Assigned(FOutlineGuids) then
            FOutlineGuids := TStringList.Create;
        FOutlineGuids.Clear;

        //  WHAT HANGS FROM WHAT is decided in outline_layout, where it can be
        //  tested; this loop only creates the nodes it names. The captions come
        //  back with the detached suffix already applied.
        Nodes := BuildOutlineNodes(ARows, DetachedSuffix);
        //  THE USER'S ROW, or the model's first element when there is none -
        //  so the Explain pane describes something the model holds rather than
        //  whatever curve type the Tools list happens to show.
        RestoreAt := InitialOutlineIndex(Nodes, WasSelected);
        SetLength(Created, Length(Nodes));
        for i := 0 to High(Nodes) do
        begin
            Parent := nil;
            if Nodes[i].ParentIndex >= 0 then
                Parent := Created[Nodes[i].ParentIndex];
            Node := FModuleTree.Items.AddChild(Parent, Nodes[i].Caption);
            Node.Data := Pointer(PtrInt(FOutlineGuids.Add(Nodes[i].Id)) + 1);
            Created[i] := Node;
            if i = RestoreAt then
                Restore := Node;
        end;

        //  The tab can be open before anything has been marked. Say what to do
        //  next rather than showing an empty box: a blank panel is
        //  indistinguishable from a broken one (D26). The wording is the
        //  module's - it knows what "nothing yet" means here. Data stays nil,
        //  which the selection handler's own guard treats as "no row".
        if Length(ARows) = 0 then
            FModuleTree.Items.AddChild(nil, EmptyText);

        FModuleTree.FullExpand;
    finally
        FModuleTree.Items.EndUpdate;
    end;

    //  After EndUpdate: assigning Selected fires ModulePanelSelectionChanged,
    //  which is what puts FSelectedRowId back. A row that is gone from the
    //  model gives way to the first element; only a panel with no row that
    //  stands for anything leaves Restore nil and the selection dropped.
    FModuleTree.Selected := Restore;
    if not Assigned(Restore) then
        FSelectedRowId := '';

    //  NOTHING IS CALLED BACK FROM HERE. This used to end with
    //  UpdateModuleTabVisibility, which asked a module whether its panel was
    //  worth a tab. The Model panel is always on the strip, so there is nothing
    //  left to ask - and asking would now recurse without end, because the
    //  answer's new job is to REFILL this panel: fill -> re-evaluate -> fill.
    //  A stack overflow inside FormCreate, before the window is up, which is
    //  reported as an abort with no message.
end;

{ Remembers which pattern the user picked, and says which it is.

  The GUID is stored on each node when the tree is filled, so this needs no
  parallel lookup that could fall out of step with what is displayed. }
function TFormMain.ReadHighlight(AExpectSelection: boolean): THighlightReading;
var
    i: longint;
    Plotted: TPlottedSeries;
begin
    Result := Default(THighlightReading);
    Result.ExpectSelection := AExpectSelection;
    Result.CurveId := FSelectedCurveId;
    Result.PanelCurveId := CurveHandleForRowId(FShownRows, FSelectedRowId);
    for i := 0 to FFitViewer.Series.Count - 1 do
    begin
        Plotted := FFitViewer.Series.Item(i);
        CountSeriesHighlight(Result, Plotted.OwnerCurveId,
            TTASerie(Plotted.Serie).LineWidth, TTASerie(Plotted.Serie).DrawOnTop);
    end;
end;

procedure TFormMain.ReportHighlight(const AReading: THighlightReading;
    const AWhen: string);
var
    Findings: TUiFindings;
    i: longint;
begin
    Findings := HighlightFindings(AReading, AWhen);
    for i := 0 to High(Findings) do
        WriteLog('ui: ' + Findings[i], Warning);
    WriteLog(HighlightSummary(AReading, AWhen), log.Notification);
end;

{ THE SAME SELECTION, WHEREVER THE CURVE IS CHOSEN. What the choice means -
  the curve, and the panel's row for it or none - is
  model_outline.CurveChoiceFromTable; this finds that row's node and selects
  it, which fires ModulePanelSelectionChanged and sets the handle, the
  highlight and the Explain pane exactly as a click would. The node's Data
  indexes FOutlineGuids, so the row is matched by its own id, never by
  position. }
procedure TFormMain.SelectCurveOfTableRow(ARow: longint);
var
    Choice: TTableCurveChoice;
    Node: TTreeNode;
begin
    if not Assigned(FCurveList) then
        Exit;
    Choice := CurveChoiceFromTable(FShownRows,
        FCurveList.CurveHandleOfRow(ARow), FSelectedCurveId);
    if not Choice.Changes then
        Exit;

    Node := nil;
    if (Choice.RowId <> '') and Assigned(FModuleTree) and
        Assigned(FOutlineGuids) then
    begin
        Node := FModuleTree.Items.GetFirstNode;
        while Assigned(Node) and not ((PtrInt(Node.Data) > 0) and
            (FOutlineGuids[PtrInt(Node.Data) - 1] = Choice.RowId)) do
            Node := Node.GetNext;
    end;

    if Assigned(Node) then
        FModuleTree.Selected := Node
    else
    begin
        if Assigned(FModuleTree) then
            FModuleTree.Selected := nil;
        SetSelectedCurveId(Choice.CurveId);
    end;
end;

procedure TFormMain.SetSelectedCurveId(const AId: string);
begin
    FSelectedCurveId := AId;
    //  Not yet during construction, and the id is kept either way: the
    //  viewer applies it to every series added after it is told.
    if Assigned(FFitViewer) then
        FFitViewer.HighlightSeriesOwnedBy(AId);
end;

{ A row was chosen. WHICH row is the window's business; what it MEANS is the
  module's, so the id and the text go straight back to it. }
procedure TFormMain.ModulePanelSelectionChanged(Sender: TObject);
var
    Node: TTreeNode;
    Mods: TUiModuleArray;
    RowId: string;
    i: longint;
begin
    RequestStateRefresh;
    if csDestroying in ComponentState then
        Exit;
    if not Assigned(FModuleTree) then
        Exit;

    RowId := '';
    Node := FModuleTree.Selected;
    //  Node.Data holds the row's position, stored as 1-based so that "no data"
    //  (nil) and "row 0" are distinguishable - a distinction Pointer(0) loses.
    if Assigned(Node) and Assigned(FOutlineGuids) and (PtrInt(Node.Data) > 0) then
    begin
        CheckIndex(PtrInt(Node.Data) - 1, FOutlineGuids.Count,
            'the module panel rows');
        RowId := FOutlineGuids[PtrInt(Node.Data) - 1];
    end;
    FSelectedRowId := RowId;

    //  WHICH CURVE THE ROW STANDS FOR, asked of the ROW - which is the only
    //  thing that knows. The framework's rows are identified by the handle
    //  itself; a pack's rows are identified by its own markup and carry the
    //  handle beside it. This used to ask whose rows these were instead and
    //  take a contributor's row to name no curve, so Delete curve was greyed
    //  over every pattern in a wave count however it was clicked. A row that
    //  stands for nothing addressable still yields '', which is what keeps the
    //  commands that need a handle disabled over it.
    SetSelectedCurveId(CurveHandleForRowId(FShownRows, RowId));

    //  AND WHAT IT STANDS FOR, from the same rows: a framework row explains its
    //  curve's type, a module's row whatever the module says it is.
    FExplainFocus := FocusRowSelected(FExplainFocus,
        TopicForRowId(FShownRows, RowId));
    RefreshExplainPane;

    //  THE ROW IS THE WINDOW'S OWN BUSINESS ABOVE; below, the module's. And
    //  not while an operation runs - see ModuleMayBeCalledBack. The selection,
    //  the highlight and the Explain pane are all applied either way, because
    //  those are the window's and cost nothing; only the call out is held back,
    //  and the rebuild at Done makes it with the model the fit produced.
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) and
        not ModuleMayBeCalledBack(FitClientApp_.FitClient.AsyncState) then
        Exit;

    Mods := RegisteredUiModules;
    for i := 0 to High(Mods) do
        if Mods[i].PanelId = FModulePanelId then
        begin
            if Assigned(Node) then
                Mods[i].PanelSelectionChanged(RowId, Node.Text, Self as IUiHost)
            else
                Mods[i].PanelSelectionChanged('', '', Self as IUiHost);
            Break;
        end;
end;

{ Puts back the curve type the last session ended on.

  Silently ignores an id this build does not have. That is deliberate and is the
  one place a quiet fallback is right: a settings file may name a curve type from
  a newer build, or one whose plug-in is no longer installed, and refusing to
  start over a remembered preference would be worse than starting on the default.
  It IS logged, so the reason a selection did not come back is discoverable. }
procedure TFormMain.RestoreCurveType;
var
    Id: TGuid;
begin
    if FSettings.SelectedCurveType = '' then
        Exit;   //  never chosen - leave the registry's default alone

    try
        Id := StringToGUID(FSettings.SelectedCurveType);
    except
        on E: Exception do
        begin
            WriteLog('settings: curve type "' + FSettings.SelectedCurveType +
                '" is not a valid id; starting on the default', Warning);
            Exit;
        end;
    end;

    if not Assigned(FindCurveClassById(Id)) then
    begin
        WriteLog('settings: curve type ' + FSettings.SelectedCurveType +
            ' is not registered in this build; starting on the default',
            Warning);
        Exit;
    end;

    //  The one entry point: it updates the client registry (so the menu shows
    //  the right checkmark) AND tells the server what to fit.
    FitClientApp_.FitClient.SelectCurveType(Id);
    RefreshAxisForSelectedCurveType;
    WriteLog('settings: curve type restored to ' + FSettings.SelectedCurveType,
        log.Notification);
end;

procedure TFormMain.ShowHint(const Hint: string);
begin
    if csDestroying in ComponentState then Exit;    //  Otherwise sometimes
                                                    //  exception is thrown.

    CheckThat(StatusBar.Panels.Count <> 0, 'the status bar must have its panels before a hint can be shown in one');

    StatusBar.Panels[2].Text := Hint;
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
    ParentMenu: TMenuItem; OnClick: TNotifyEvent; ATickable: boolean);
var mi: TMenuItem;
begin
    CheckAssigned(ParentMenu, 'the menu the new item is added under');

    mi := TMenuItem.Create(ParentMenu);
    mi.Name := FMenuNames.NameFor('MenuUserCurve', ct.Name);
    //  Before the item is inserted, i.e. before it has a handle: an entry
    //  becomes a check item or a plain one when its handle is made, and giving
    //  it the check box afterwards means the widget set rebuilding it under
    //  whatever menu is open. See DeclareCheckableMenuEntries.
    mi.ShowAlwaysCheckable := ATickable;
    mi.Caption := ct.Name;
    mi.OnClick := OnClick;
    //  obratnaya svyaz'
    //  THE WHOLE POINTER, not its low half. This was LongInt, which on a
    //  64-bit build truncates the address - the comparison below then works
    //  only because it truncates the same way, and two curve types whose
    //  addresses differ above bit 32 would select each other. Tag is PtrInt.
    mi.Tag := PtrInt(ct);
    //  novye elementy dobavlyayutsya v nachalo
    //  spiska, no v poryadke sozdaniya
    ParentMenu.Insert(Pos, mi);
end;



{ The caller is OnDeleteUserCurveClick, i.e. a menu item's own OnClick - and the
  item that has to disappear is that same Sender. This used to free it here,
  by hand, from inside the click: see QueueCurveTypeMenuRebuild for what the
  widgetset then does with the item it is still dispatching.

  The model is the single source of truth for the menu, so removing the curve
  type from FSettings.Curve_types is the whole deletion: the queued rebuild runs
  CreateUserCurveMenus over what is left, which drops the selection item, drops
  the matching entry under MenuDelUserCapt, and - when the last user curve goes -
  omits that submenu and its separator altogether. That is what the hand-rolled
  item surgery was reproducing, one item at a time and one bug at a time. }
procedure TFormMain.DeleteUserCurve(ct: Curve_type);
var WasBeingFitted: boolean;
    Name_: string;
begin
    CheckAssigned(ct, 'the user-defined curve type being worked on');
    CheckAssigned(FSettings, 'the saved application settings');
    CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');

    //  Deleting the curve that is CURRENTLY BEING FITTED leaves the model with a
    //  curve type that exists nowhere in the menu any more. The formula lives on
    //  the server, which knows nothing about this deletion, so unless it is told
    //  the next fit quietly produces more curves of the type just deleted - the
    //  user sees "User Defined" curves and no "User Defined" anything to explain
    //  them.
    WasBeingFitted := DeletingLeavesTheModelWithoutACurveType(
        FSelectedUserCurve, ct);
    Name_ := ct.Name;

    //  Drop the selection reference before the object goes.
    FSelectedUserCurve := SelectionAfterDeleting(FSelectedUserCurve, ct);

    SysUtils.DeleteFile(ct.FFileName);
    FSettings.Curve_types.Delete(FSettings.Curve_types.IndexOf(ct));
    QueueCurveTypeMenuRebuild;

    if WasBeingFitted then
    begin
        //  The server refuses to fit the user-defined type from here on (see
        //  TFitService.CreateTasks), so this message is what makes that refusal
        //  expected rather than puzzling.
        FitClientApp_.FitClient.ClearSpecialCurve;
        //  Queued, so it appears after the menu has been rebuilt - and so its
        //  message loop cannot run that rebuild from inside the click being
        //  dispatched. See QueueNotice.
        QueueNotice(DeletedFittedCurveNotice(Name_));
    end;
end;

procedure TFormMain.OnDeleteUserCurveClick(Sender: TObject);
var
    ct: Curve_type;
begin
    CheckAssigned(FSettings, 'the saved application settings');
    CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');

    //  WHICH CURVE THIS ITEM STANDS FOR is in user_curve_library, and it matches
    //  by the object's own address: a menu built against a list that has since
    //  changed matches nothing rather than the curve now in that position.
    ct := CurveWithTag(FSettings.Curve_types, TMenuItem(Sender).Tag);
    if Assigned(ct) then
        DeleteUserCurve(ct);
end;

procedure TFormMain.OnUserCurveClick(Sender: TObject);
var
    ct: Curve_type;
begin
    CheckAssigned(Sender, 'the control that raised this event');
    CheckAssigned(FSettings, 'the saved application settings');
    CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');
    CheckAssigned(FitClientApp_, 'the client application object');
    CheckAssigned(FitClientApp_.FitClient, 'the fit client behind this window');

    //  Same search as the delete handler, and now literally the same code.
    ct := CurveWithTag(FSettings.Curve_types, TMenuItem(Sender).Tag);
    if Assigned(ct) then
    begin
        //  WHETHER IT CAN BE SELECTED AT ALL is in curve_type_menu, where it is
        //  tested: a curve saved without its formula is an entry that cannot
        //  become a curve, and selecting it used to fail an assertion in the
        //  optimiser rather than saying so here.
        if not UserCurveIsUsable(ct.Expression) then
            MessageDlg('User-defined curve',
                UnusableCurveNotice(UserCurveGroup, MenuDelUserCapt),
                mtWarning, [mbOK], 0)
        else
        begin
            //  Pass a COPY: SetSpecialCurveParameters takes ownership of the
            //  parameters, but ct keeps owning ct.Parameters. Sharing the same
            //  object caused a later double-free / access violation.
            FitClientApp_.FitClient.SetSpecialCurveParameters(
                ct.Expression, Curve_parameters(ct.Parameters.GetCopy));
            FitClientApp_.FitClient.SelectCurveType(TUserPointsSet.GetCurveTypeId);
            RefreshAxisForSelectedCurveType;
            //  Remember which user curve is active so the menu can check its
            //  item.
            FSelectedUserCurve := ct;
        end;
    end;

    //  Rebuild the menu so the check moves to the selected user-curve item.
    //  Queued: Sender is one of the items the rebuild destroys, and freeing it
    //  from its own handler faults inside the widgetset. See
    //  QueueCurveTypeMenuRebuild.
    QueueCurveTypeMenuRebuild;
end;

function TFormMain.LastUserCurve: Curve_type;
begin
    CheckAssigned(FSettings, 'the saved application settings');
    CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');

    //  WHICH ONE IS "THE LAST" is in user_curve_library: the last entry that is
    //  not the placeholder, which is not the same as the last entry.
    Result := LastCreatedCurve(FSettings.Curve_types);

    if Result = nil then
        //  Not fatal - the menu simply shows no curve as selected - but it means
        //  the curve the dialog reported creating never reached the settings.
        WriteLog('a user curve was created but none is stored in the settings',
            Warning);
end;

function TFormMain.UserCurveMenu: TMenuItem;
begin
    Result := MenuSelectCurveType.Find(UserCurveGroup);
    //  The group is created from TUserPointsSet, which is always registered, so
    //  a missing group means the menu was not built - not an empty one.
    if Result = nil then
        raise Exception.Create('The "' + UserCurveGroup +
            '" curve type group is missing from the Curve Type menu.');
end;

{ The saved user curves are appended to the User group, below the generic "User
  Defined" item that CreateCurveTypeMenus put there: that item creates a curve,
  the ones added here select one already created. }
procedure TFormMain.CreateUserCurveMenus;
    { True is returned if menu items were added. }
    function AddItem(ParentMenu: TMenuItem; OnClick: TNotifyEvent;
        ATickable: boolean): Boolean;

    var i: LongInt;
        ct: Curve_type;
    begin
        CheckAssigned(FSettings, 'the saved application settings');
        CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');

        Result := False;
        for i := 0 to FSettings.Curve_types.Count - 1 do
        begin
            ct := Curve_type(FSettings.Curve_types.Items[i]);
            //  The placeholder an old version wrote into an empty settings file
            //  is not a curve the user made, so it gets no entry. Named in
            //  user_curve_library, where the same filter decides which curve is
            //  the last one created.
            if ct.Name <> DUMMY_CURVE_NAME then
            begin
                CreateMenuItem(ParentMenu.Count, ct, ParentMenu, OnClick,
                    ATickable);
                Result := True;
            end;
        end;
    end;

var UserMenu, mi: TMenuItem;
    FirstCurveIndex: LongInt;
begin
    UserMenu := UserCurveMenu;
    FirstCurveIndex := UserMenu.Count;
    if AddItem(UserMenu, OnUserCurveClick, True) then
    begin
        { Separator between the generic item and the saved curves. }
        mi := TMenuItem.Create(UserMenu);
        mi.Name := FMenuNames.NameFor('MenuSeparatorBeforeUserCurves', '');
        mi.Caption := '-';
        UserMenu.Insert(FirstCurveIndex, mi);
        { Menu is created for deleting user curve types. }
        mi := TMenuItem.Create(UserMenu);
        mi.Name := FMenuNames.NameFor('MenuDeleteUserCurve', '');
        mi.Caption := MenuDelUserCapt;
        UserMenu.Add(mi);
        { Submenu for deleting item is created. }
        AddItem(mi, OnDeleteUserCurveClick, False);
    end;
end;

{ The new curve is already in FSettings.Curve_types, which is the single source
  of truth for this part of the menu, so the whole menu is rebuilt from it - the
  same treatment DeleteUserCurve gives a removed one. Placing the item by hand
  instead meant tracking, in this one procedure, where the user block starts,
  where its separator goes and whether the delete submenu exists yet; the rebuild
  derives all of that in one place.

  Queued rather than immediate: the caller can be a dialog running over the menu
  the rebuild destroys. See QueueCurveTypeMenuRebuild. }
procedure TFormMain.AddUserCurveMenu(ct: Curve_type);
begin
    CheckAssigned(ct, 'the user-defined curve type being worked on');
    QueueCurveTypeMenuRebuild;
end;

procedure TFormMain.ReadUserCurves;
var SearchRec: TSearchRec;
    Path, FileName: string;
    XMLConfig: TXMLConfig;
    CurveType: Curve_type;
begin
    CheckAssigned(FSettings, 'the saved application settings');
    CheckAssigned(FSettings.Curve_types, 'the list of user-defined curve types');

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
                    CheckAssigned(CurveType, 'the user-defined curve type being worked on');

                    if Trim(CurveType.Expression) = '' then
                    begin
                        //  A curve type with no formula can never be selected or
                        //  used; drop it and remove its stale file so it does not
                        //  clutter the menu (such files came from older builds).
                        CurveType.Free;
                        SysUtils.DeleteFile(FileName);
                    end
                    else
                    begin
                        CurveType.FFileName := FileName;
                        FSettings.Curve_types.Add(CurveType);
                    end;
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
        SysUtils.FindClose(SearchRec);
{$endif}
    end;
end;

procedure TFormMain.WriteUserCurve(CurveType: Curve_type);
var XMLConfig: TXMLConfig;
begin
    CheckAssigned(CurveType, 'the user-defined curve type being worked on');

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
    //  A FORM THAT NEVER FINISHED BEING CREATED HAS NOTHING TO PERSIST, and this
    //  runs on that path: FormCreate asks the compute server for a setting while
    //  building the action states, so starting the client before the server
    //  raises out of FormCreate - and the widget set then calls OnDestroy on a
    //  form whose fields were never assigned. FSettings is nil there, and the
    //  first line below used to dereference it: a memory fault on the way out,
    //  reported through client_log.EndProcessAfterFault, so the user who started
    //  the app in the wrong order saw it die rather than open.
    //
    //  Verified on the VM against the revision before this phase, so it is not a
    //  regression - it is a crash nobody had run into with no server listening.
    //  See docs/contributing/findings.md.
    if not Assigned(FSettings) then
        Exit;

    FileName := GetConfigFileName;
    if Assigned(FFitViewer) then
        FSettings.ViewMode := FFitViewer.XCoordMode;   //  persist the chosen axis
    //  ... and whether it was chosen at all: only then may it override the axis
    //  the selected curve type defines on the next start.
    FSettings.ViewModeChosenByUser := FAxisModeChosenByUser;
    FSettings.MinimizerKind := FMinimizerKind;   //  persist the chosen minimizer
    FSettings.LossKind := FLossKind;             //  and the chosen objective
    FSettings.PriceValueColumn := FPriceValueColumn;
    FSettings.PriceArgument := FPriceArgument;
    //  The curve type, so a session does not begin on whatever the registry
    //  happens to list first. Read back from the CLIENT rather than remembered
    //  in a field: the menu, the client registry and the server all follow
    //  TFitClient.SelectCurveType, so the client is the one place that is
    //  certain to be current.
    if Assigned(FitClientApp_) and Assigned(FitClientApp_.FitClient) then
    begin
        FSettings.SelectedCurveType :=
            GUIDToString(FitClientApp_.FitClient.CurveTypeId);
        FSettings.AnimationMode := FitClientApp_.FitClient.AnimationMode;
    end;
    FSettings.Weighting := FWeighting;           //  persist the Python weighting
    FSettings.ServerUrl := FServerUrl;           //  persist the compute-server URL
    //  Persist the user-defined axis definition so a XCM_CUSTOM mode can be
    //  restored on next start (otherwise the mode would come up undefined).
    FSettings.CustomAxisName    := FCustomAxisName;
    FSettings.CustomAxisUnit    := FCustomAxisUnit;
    FSettings.CustomAxisForward := FCustomAxisForward;
    FSettings.CustomAxisInverse := FCustomAxisInverse;
    XMLConfig := TXMLConfig.Create(Filename);
    try
        WriteComponentToXMLConfig(XMLConfig, 'Component', FSettings);
        XMLConfig.Flush;
    finally
        XMLConfig.Free;
    end;
end;

{ The last stop for an exception nothing else caught.

  Three things must not happen here, all of them learned from freezes that took
  the whole desktop down with the client:

  The dialog must not be shown from this procedure. OnException runs inside the
  call that faulted - a paint handler, a widget signal - and under X11 that call
  may hold an implicit pointer and keyboard grab. A modal dialog opened there
  runs its own message loop inside the grab; if the wedged code never returns,
  the grab is never released and every click and keystroke in the session goes
  on being delivered to this process. The machine is fine, and nothing but the
  power button answers. So the message is queued and shown from the main loop,
  after the faulting call has unwound and any grab with it.

  A repeating fault must not open a dialog per occurrence: an event that keeps
  arriving would stack dialogs until nothing else can be reached. deferred_ui
  admits one, drops the rest and counts them.

  AND A MEMORY FAULT IS NOT REPORTED AT ALL - the process ends. Queueing the
  dialog is enough only while the thing being reported is a failed operation.
  It is not enough when the fault came from inside the widget set, because then
  the queue, the message loop and the dialog all run through code that has just
  been abandoned half-done: the client this was written for froze with an empty
  error dialog holding the X pointer grab, its main thread blocked forever in
  GLib's signal mutex which the aborted signal emission never released. A
  process that dies leaves a log and a restartable app; that one left neither,
  and took the desktop session with it. See client_log.EndProcessAfterFault. }
procedure TFormMain.OnException(Sender: TObject; E: Exception);
begin
    //  A REFUSAL IS NOT A FAULT, and reaching here does not make it one.
    //
    //  The server declines an operation and explains why - "this curve carries
    //  no handle, so it cannot be removed on its own" - and that arrives as
    //  EUserException. A caller that lets one reach this handler has a gap in
    //  it, and the gap is worth fixing where it is; but the CONSEQUENCE must not
    //  be a dead application. This logged at Fatal and stopped the state poll,
    //  so a refused delete left the window frozen with "Server polling has been
    //  stopped" stapled to a message that was perfectly correct - and the user
    //  reasonably read it as the compute server having crashed.
    //
    //  So: say what was refused, keep polling, and leave the log alone - the
    //  same treatment the chart's click handler already gives a refused pick.
    if IsRefusalRatherThanFault(E) then
    begin
        QueueError(E.Message);
        Exit;
    end;

    //  Logged BEFORE anything else: whatever happens next - a wedged dialog, a
    //  killed process - the record of what happened is already on disk. With the
    //  stack, so the next crash names its origin instead of only its type.
    LogClientFatalException(E);

    //  An operation interrupted by the fault may have left the busy cursor on;
    //  an hourglass over an idle app reads as a hang.
    Screen.Cursor := crDefault;

    if FaultLeavesProcessUnsound(E) then
    begin
        //  With the map, because the stack above is bare addresses and the
        //  libraries are only where this run's loader put them.
        LogClientModuleMap;
        EndProcessAfterFault;
    end;

    QueueError(E.Message + LineEnding + LineEnding +
        'Server polling has been stopped. Use Fit -> Compute Server... ' +
        'to resume it.');
end;

procedure TFormMain.QueueError(const AMessage: string);
begin
    QueueDialog(AMessage, mtError);
end;

procedure TFormMain.QueueNotice(const AMessage: string);
begin
    QueueDialog(AMessage, mtInformation);
end;

{ WHAT IS OUTSTANDING AND WHETHER IT MAY RUN is in deferred_ui, where it is
  tested. What is left here is the widget set's half: the async call, the retry
  timer, the dialog itself, and the mapping from a notice kind onto TMsgDlgType. }

function TFormMain.DialogTypeOf(AKind: TNoticeKind): TMsgDlgType;
begin
    case AKind of
        nkInformation: Result := mtInformation;
        nkWarning:     Result := mtWarning;
    else
        Result := mtError;
    end;
end;

procedure TFormMain.QueueDialog(const AMessage: string; AKind: TMsgDlgType);
var
    Kind: TNoticeKind;
begin
    //  Back into the framework's own vocabulary. The callers here speak the
    //  widget set's, because that is what MessageDlg takes.
    if AKind = mtInformation then
        Kind := nkInformation
    else if AKind = mtWarning then
        Kind := nkWarning
    else
        Kind := nkError;
    //  False means the request was dropped - one is already outstanding - and
    //  then nothing is queued, or the main loop wakes for no work.
    if FDeferred.RequestDialog(AMessage, Kind) then
        Application.QueueAsyncCall(ShowPendingError, 0);
end;

procedure TFormMain.ShowPendingError(Data: PtrInt);
var
    Message_: string;
    Kind: TNoticeKind;
begin
    //  A dialog opened over a dropped-down menu takes the grab from it and
    //  leaves it painted on screen belonging to nothing (ui_menus). The queue
    //  exists precisely because this message arrives from an event or from the
    //  calculation thread, so its turn can come at any instant - including while
    //  the user is reading a menu. It waits.
    //  Not now: the work keeps, and the application going idle - once the menu's
    //  own loop has ended - takes it up again (DeferredUiIdle).
    if FDeferred.WorkNow(AMenuIsOpen) <> dwDialog then
        Exit;

    //  TAKEN BEFORE IT IS SHOWN, which is what the try..finally did: a show that
    //  raises must not leave the request outstanding and every later message
    //  blocked behind it.
    if FDeferred.TakeDialog(Message_, Kind) then
        MessageDlg(Message_, DialogTypeOf(Kind), [mbOk], 0);
end;



{ CreateCurveTypeMenus starts with MenuSelectCurveType.Clear, which DESTROYS the
  menu items - and the curve-type items are the ones whose OnClick handlers ask
  for the rebuild. Calling it from such a handler frees the very TMenuItem that
  is Sender, while the widgetset is still holding it and has not finished
  dispatching the click. What follows is an access violation with no frame of
  ours on the stack: it happens inside the widgetset's menu machinery after the
  handler returned, which is exactly how it appeared in fit_client.log - two
  EAccessViolations 4 ms apart, every frame in the shared-library region, none in
  the Fit binary.

  So the rebuild is queued and runs from the main loop, once the click has fully
  unwound and the item is nobody's business but ours. This is the same treatment,
  and for the same class of reason, that QueueError gives an error dialog raised
  from inside a timer or a menu.

  Do not "simplify" this back into a direct call from a click handler. }
procedure TFormMain.QueueCurveTypeMenuRebuild;
begin
    //  Several clicks can queue before the main loop runs; one rebuild covers
    //  them all, and a second would work over items the first had just created.
    if FDeferred.RequestMenuRebuild then
        Application.QueueAsyncCall(RebuildCurveTypeMenus, 0);
end;

procedure TFormMain.RebuildCurveTypeMenus(Data: PtrInt);
begin
    //  The rebuild DESTROYS menu entries, submenu parents among them (the curve
    //  type groups). Doing that to a menu that is open is the other half of what
    //  ui_menus describes, and it need not be done now: the flag stays set and
    //  the application going idle brings the rebuild back.
    if AMenuIsOpen then
        Exit;

    try
        CreateCurveTypeMenus;
    finally
        //  Declared done even if it raised: left outstanding, it would be
        //  attempted again every time the application goes idle.
        FDeferred.RebuildDone;
    end;
end;

procedure TFormMain.SetHandleEditHint(EditHint: Boolean);
begin
    FHandleEditHint := EditHint;
    //  Not from inside the grid's own event: queued, and shown once it unwinds.
    if EditHint then
        Application.QueueAsyncCall(ShowEditHintQueued, 0);
end;


{ ---------------------------------------------------------- the window state }

procedure TFormMain.RequestStateRefresh;
begin
    //  One queued refresh covers every request made before it runs, and a
    //  request raised while the state is being applied is not a change.
    if FDeferred.RequestStateRefresh then
        Application.QueueAsyncCall(RunStateRefresh, 0);
end;

procedure TFormMain.RunStateRefresh(Data: PtrInt);
begin
    if csDestroying in ComponentState then
        Exit;
    //  NOT WHILE THE USER IS IN A MENU. Applying the state writes to menu
    //  entries, and an entry the widget set has to rebuild takes the open menu
    //  with it (ui_menus). The request keeps; going idle takes it up again.
    if AMenuIsOpen then
        Exit;
    //  The rebuild first, so the state is applied to the menus it produces.
    if FDeferred.WorkNow(False) = dwMenuRebuild then
        RebuildCurveTypeMenus(0);
    if FDeferred.WorkNow(False) <> dwStateRefresh then
        Exit;

    FDeferred.BeginStateRefresh;
    try
        try
            CheckState;
        except
            on E: Exception do
            begin
                //  The compute server went away or refused: say so once, and
                //  keep the window working - the next event asks again.
                LogClientWarning('refreshing the window state failed: ' + E.Message);
                QueueError('Compute Server: ' + E.Message);
            end;
        end;
    finally
        FDeferred.EndStateRefresh;
    end;
end;

procedure TFormMain.DeferredUiIdle(Sender: TObject; var Done: Boolean);
begin
    if csDestroying in ComponentState then
        Exit;
    if AMenuIsOpen or not FDeferred.AnythingOutstanding then
        Exit;
    //  IN THE ORDER deferred_ui gives them.
    case FDeferred.WorkNow(False) of
        dwMenuRebuild: RebuildCurveTypeMenus(0);
        dwStateRefresh: RunStateRefresh(0);
        dwDialog: ShowPendingError(0);
    end;
end;

procedure TFormMain.StateChangedByViewer(Sender: TObject);
begin
    RequestStateRefresh;
end;

procedure TFormMain.ActionListExecuteAny(AAction: TBasicAction;
    var Handled: Boolean);
begin
    //  Before the action runs, but the refresh is queued, so it applies what the
    //  action changed. Handled stays False: the action still executes.
    RequestStateRefresh;
end;

procedure TFormMain.ActiveControlChanged(Sender: TObject);
begin
    //  Which grid has the focus decides what Copy, Paste and Delete act on.
    RequestStateRefresh;
end;

procedure TFormMain.GridSelectionChanged(Sender: TObject; aCol, aRow: Integer);
begin
    RequestStateRefresh;
    //  A USER CHOOSING A ROW, not the table being refilled: GridParameters is
    //  rewritten on every recompute and moves its cursor as it goes, and
    //  following that would move the selection - and the highlight, and the
    //  target of Delete curve - out from under the user.
    if (Sender = GridParameters) and (ActiveControl = GridParameters) then
        SelectCurveOfTableRow(aRow - GridParameters.FixedRows);
end;

procedure TFormMain.ShowEditHintQueued(Data: PtrInt);
begin
    if not FHandleEditHint then
        Exit;
    FHandleEditHint := False;
    DoEditHint;
end;

procedure TFormMain.CurveTypeListShowHint(Sender: TObject; HintInfo: PHintInfo);
begin
    if not Assigned(FCurveTypeList) then
        Exit;
    HintInfo^.HintStr := CurveListHintAt(FCurveRows,
        FCurveTypeList.ItemAtPos(HintInfo^.CursorPos, True), FCurveTypeList.Hint);
end;

initialization
  //{$i cursors.lrs}
  {$i form_main.lrs}
end.



