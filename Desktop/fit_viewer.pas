// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of component which can draw client data in UI.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_viewer;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, Contnrs, curve_points_set, Graphics, Math, named_points_set,
    neutron_points_set, persistent_curve_parameters, points_set,
    self_copied_component, series_register, SysUtils, title_points_set,
{$IFNDEF SERVER}
    fit_client, int_fit_viewer,
{$ENDIF}
    Forms, mscr_specimen_list, argument_axis, TAGraph, module_view_types;

{$IFNDEF SERVER}
// Switch on updating legend and grids.
{$DEFINE USE_LEGEND}
{$DEFINE USE_GRIDS}
{$ENDIF}

//  The display-mode constants (XCM_*) are defined once in mscr_specimen_list and
//  reused here (that unit is in the uses clause) - they were duplicated before.

type
    { Component responsible for displaying client data by means of UI components.
      Supports correspondence between series of Chart and items of CheckListBox.
      On turning off CheckListBox item serie can be only hidden but not deleted
      because there is no direct connection between items of CheckListBox and
      items of PointsSetList. }
    TFitViewer = class(TComponent
{$IFNDEF SERVER}
        , IFitViewer
{$ENDIF}
        )
    private
{$IFNDEF SERVER}
        FFitClient: TFitClient;
{$ENDIF}
        FXCoordMode: longint;
        { The user-defined display axis used when XCoordMode = XCM_CUSTOM. Owned. }
        FCustomAxis: TArgumentAxis;
        FViewMarkers: boolean;
        FForm: TForm;
        { Enables updating grids. By default is true. }
        FUpdateGrids: boolean;
        { Enables updating legend. By default is true. }
        FUpdateLegends: boolean;

        procedure SetXCoordMode(AMode: longint);
        { Maps a raw stored argument (Point2T) to the value shown on the current
          axis, single-sourcing the 2*Theta / Theta / Sin/Lambda / custom transforms. }
        function DisplayX(ARawValue, AWaveLength: double): double;

    private
        { List of curves related with chart series.
          The list is passive, it contains pointers to external data. }
        FCurves: TComponentList;
        { WHAT EACH SERIES WAS DRAWN FOR. FCurves says which point sets are
          plotted; this says which CURVE owns each series, which the chart
          itself cannot say - a series carries a Title and nothing else, and a
          title is neither unique nor an identity. }
        FSeries: TSeriesRegister;

        { Puts a series on the chart and records what it was drawn for.

          APoints is the set it draws; AOwnerCurveId is the handle of the curve
          that owns it, or EMPTY for a series belonging to the model as a whole -
          the profile, the difference, the background, the bounds, the positions,
          the picked points. Those must survive a curve being deleted. }
        procedure AddSerieToChart(Serie: TTASerie; APoints: TComponent;
            const AOwnerCurveId: string);
        { Takes one series off the chart, out of the legend and out of the
          register, and frees it. The single place a series stops being drawn. }
        procedure DropSerie(ASerie: TTASerie);
{$IFDEF USE_GRIDS}
        procedure FillBoundsTable(RFactorBounds: TTitlePointsSet);
        procedure FillDataTable(Profile: TTitlePointsSet);
        procedure FillBackgroundTable(BackgroundPoints: TTitlePointsSet);
        procedure FillPositionsTable(CurvePositions: TTitlePointsSet);
        procedure FillCurveTable(CurveList: TMSCRCurveList);

        procedure ClearBoundsTable;
        procedure ClearDataTable;
        procedure ClearBackgroundTable;
        procedure ClearPositionsTable;
        procedure ClearCurveTable;
        procedure ClearSummaryTable;
{$ENDIF}
        { Clears serie set and fills it again. }
        { Fills a series from a point set, over the stretch that set says means
          something - see TPointsSet.SetExtent. }
        procedure PlotPointsSet(PointsSet: TNeutronPointsSet);

    public
{$IFNDEF SERVER}
        procedure SetFitClient(AFitClient: TFitClient);
    private
{$ELSE}
        procedure Paint;
{$ENDIF}
    public
        { Method of IFitViewer interface. }
        procedure PlotBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotExpProfile(Sender: TObject;
            ExpProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotSelectedProfileInterval(Sender: TObject;
            SelectedArea: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotCurves(Sender: TObject;
            Curves: TSelfCopiedCompList;
            CurveAttributes: TMSCRCurveList);
        { Method of IFitViewer interface. }
        procedure PlotRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure TabulateCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotComputedProfile(Sender: TObject;
            ComputedProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotDeltaProfile(Sender: TObject;
            DeltaProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotModuleSeries(Sender: TObject; const AName: string;
            APoints: TTitlePointsSet; ALabels: TStrings;
            const AStyle: TModuleSeriesStyle;
            const AOwnerCurveId: string = '');
        procedure HideSeriesOwnedBy(const ACurveId: string);
        procedure HideModuleSeries(Sender: TObject; APoints: TTitlePointsSet);
        procedure ShowModulePanel(const APanelId: string; const ARows: TOutline);
        procedure PlotSelectedPoints(Sender: TObject;
            SelectedPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideExpProfile(Sender: TObject;
            DataPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure Refresh(Sender: TObject);
        { Does not clear series but only refreshes intencities. }
        procedure RefreshPointsSet(Sender: TObject;
            PointsSet: TNeutronPointsSet);
        { Method of IFitViewer interface. }
        procedure Clear(Sender: TObject);
        { Method of IFitViewer interface. }
        procedure Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
{$IFDEF USE_GRIDS}
        { Method of IFitViewer interface. }
        procedure SetUpdateGrids(Update: boolean);
        { Method of IFitViewer interface. }
        procedure FillSummaryTable(ExperimentalProfile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
{$ENDIF}
{$IFDEF USE_LEGEND}
        { Method of IFitViewer interface. }
        procedure SetUpdateLegends(Update: boolean);
{$ENDIF}
{$IFNDEF SERVER}
        { Method of IFitViewer interface. }
        procedure ShowTime;
        { Method of IFitViewer interface. }
        procedure ShowRFactor;
        { Method of IFitViewer interface. }
        procedure ShowHint(Hint: string);
{$ENDIF}

        procedure SetViewMarkers(AViewMarkers: boolean);
        procedure ViewAllMarkers;
        { Clears all series and fills them again saving parameter values. }
        procedure Plot;
        { Returns number of the first visible curve from curve list.
          This function actually gives the number of active curve
          when only single curve is visible in the chart. This should
          be checked separately. }
        function GetActiveCurveIndex: longint;
        function GetActivePointsSet: TNeutronPointsSet;
        function GetPointsSet(ActiveCurveIndex: longint): TNeutronPointsSet;

        { Return boundary values among all curves. }

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        { Defines the user-defined display axis (used with XCM_CUSTOM): a display
          name, unit and forward/inverse formulas of x. }
        procedure SetCustomAxis(const AName, AUnit, AForward, AInverse: string);

        property XCoordMode: longint read FXCoordMode write SetXCoordMode;
        property Form: TForm read FForm write FForm;
    end;

implementation

uses form_main, client_log, checks, summary_table, series_palette,
    series_style,
    //  When a legend row appears and when it goes. The asymmetry between those
    //  two is what made the legend draw one series against another's name.
    legend_layout,
    //  The curve handle a series is owned by, in the form the model addresses
    //  it by. The view had no notion of curve identity at all.
    curve_instance_id,
    points_tables;

{========================== TFitViewer ==================================}
{$IFNDEF SERVER}
procedure TFitViewer.SetFitClient(AFitClient: TFitClient);
begin
    FFitClient := AFitClient;
    FFitClient.FFitViewer := Self;
end;

{$ENDIF}
const
    { Sixteen, and series_palette.SeriesColorCount has to agree - which is why
      the count is named there rather than written as a literal in the wrapping
      arithmetic, where it was. }
    ColorPalette: array[1..SeriesColorCount] of TColor =
        (clRed, clGreen, clYellow, clBlue, clBlack, clGray, clFuchsia, clTeal,
        clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua, clBlack);

{ THE ONLY PLACE A STYLE MEETS THE CHART. Everything above it - which series
  gets which shape, size and colour, and which of them the markers toggle may
  touch - is decided in series_style, where it is reachable by a test. What is
  left here is two translations into the plotting component's vocabulary, and
  they are the parts that have to change when that component is replaced. }

function ChartPointStyle(AMarker: TSeriesMarker): TPointStyle;
begin
    case AMarker of
        smCircle:     Result := psCircle;
        smDiagCross:  Result := psDiagCross;
        smRectangle:  Result := psRectangle;
        smVertLineTB: Result := psVertLineTB;
        smVertLineBT: Result := psVertLineBT;
    else
        //  A marker added to the enum and not mapped here would otherwise draw
        //  as whatever the first branch happens to be - a wrong picture with no
        //  error (D26).
        CheckUnreachable('the series marker mapping');
        Result := psCircle;
    end;
end;

{ Which colour a role is drawn in. The values are the widget set's, which is why
  they are here and not in series_style; ACurveIndex matters only for a model
  curve, where series_palette decides which of the sixteen it takes. }
function ChartSeriesColor(ARole: TSeriesColorRole;
    ACurveIndex: longint): TColor;
begin
    case ARole of
        crExperiment:    Result := clRed;
        crModelCurve:    Result := ColorPalette[SeriesColorIndex(ACurveIndex)];
        crComputed:      Result := clBlack;
        crResidual:      Result := clGreen;
        crBackground:    Result := clGray;
        crIntervalBound: Result := clBlue;
        crPosition:      Result := clBlack;
        crPickedPoint:   Result := clGreen;
    else
        CheckUnreachable('the series colour mapping');
        Result := clBlack;
    end;
end;

{ Applies a style to a freshly created series.

  THE INIT FLAGS ARE SET FROM THE SAME VALUES, ALWAYS, and that is the whole
  reason this is one procedure. The chart reads InitShowLines to decide whether
  the "View markers" toggle may reach a series (ViewAllMarkers), so a series that
  set ShowLines and forgot InitShowLines answered to a toggle that was not meant
  for it - and five of the ten call sites this replaced were relying on the
  component's defaults happening to agree. }
{ The handle the model addresses a curve by, as a series owner.

  ASKED OF THE OBJECT rather than cast to it: PlotCurves is handed a list of
  TNamedPointsSet, and only a curve carries an instance handle. A set that is
  not one - and a curve whose handle was never issued - owns nothing, which is
  the same answer a model-wide series gives. }
function OwnerOf(APoints: TNamedPointsSet): string;
begin
    Result := '';
    if APoints is TCurvePointsSet then
        Result := CurveInstanceIdToWire(TCurvePointsSet(APoints).FInstanceId);
end;

procedure ApplySeriesStyle(Serie: TTASerie; const AStyle: TSeriesStyle;
    const ATitle: string; ACurveIndex: longint = 0);
begin
    Serie.PointStyle := ChartPointStyle(AStyle.Marker);
    Serie.ImageSize := AStyle.MarkerSize;
    Serie.SeriesColor := ChartSeriesColor(AStyle.ColorRole, ACurveIndex);
    Serie.ShowLines := AStyle.ShowLines;
    Serie.ShowPoints := AStyle.ShowPoints;
    Serie.InitShowLines := Serie.ShowLines;
    Serie.InitShowPoints := Serie.ShowPoints;
    if AStyle.Hollow then
        Serie.PointBrushStyle := bsClear;
    Serie.Title := ATitle;
end;

procedure TFitViewer.Plot;
var
    PointsSet: TNeutronPointsSet;
    j:  longint;
begin
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    for j := 0 to FCurves.Count - 1 do
    begin
        PointsSet := TNeutronPointsSet(FCurves.Items[j]);
        PlotPointsSet(PointsSet);
    end;
end;

procedure TFitViewer.PlotPointsSet(PointsSet: TNeutronPointsSet);
var
    Serie: TTASerie;
    i:  longint;
begin
    CheckAssigned(PointsSet, 'the points set to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    Serie := TTASerie(TFormMain(Form).Chart.GetSerie(FCurves.IndexOf(PointsSet)));
    Serie.Clear;
    //  Point2T is the raw stored argument; the current axis maps it to the shown
    //  value (single source of the 2*Theta / Theta / Sin/Lambda / custom transforms).
    //
    //  ONLY WHERE THE SET MEANS SOMETHING, and the set is asked rather than
    //  told: EVERY path that fills a series comes through here - the curve
    //  plot, the whole-chart re-plot, a module's markers - and a stretch that
    //  was excluded by one of them and drawn by another is exactly the bug this
    //  prevents. Unbounded unless something stated otherwise, so nothing but a
    //  curve with a stated extent is affected at all.
    //
    //  The test is on the RAW argument, the domain the extent is stated in.
    //  Testing the displayed value would break as soon as an axis transform is
    //  not monotonic in the way that assumes.
    with PointsSet do
        for i := 0 to PointsCount - 1 do
            Serie.AddXY(DisplayX(Point2T[i], WaveLength), PointIntensity[i],
                Serie.SeriesColor);
end;

procedure TFitViewer.AddSerieToChart(Serie: TTASerie; APoints: TComponent;
    const AOwnerCurveId: string);
var
    GotRow: boolean;
begin
    TFormMain(Form).Chart.AddSerie(Serie);
    GotRow := False;
{$IFDEF USE_LEGEND}
    //  ASKED, not restated. Whether a row appears now and whether the legend is
    //  emptied later are two different answers, and writing the flag out at both
    //  ends is what made them look like one - see legend_layout.
    if LegendRowIsAdded(FUpdateLegends) then
    begin
        TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
        TFormMain(Form).CheckListBoxLegend.Checked[
            TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        GotRow := True;
    end;
{$ENDIF}

    //  RECORDED HERE, which is the one place every series passes through - so a
    //  series cannot reach the chart without its owner being known.
    FSeries.Add(Serie, APoints, AOwnerCurveId, GotRow);
    if Assigned(APoints) then
        FCurves.Add(APoints);
end;

procedure TFitViewer.PlotSelectedProfileInterval(Sender: TObject; SelectedArea: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(SelectedArea, 'the selected profile interval to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    if FCurves.IndexOf(SelectedArea) = -1 then
    begin
        //  A new series.
        Serie := TTASerie.Create(nil);
        try
            //  NOT SelectedArea.FTitle, which the old code assigned first
            //  and then overwrote with this: the interval is one thing to the
            //  user however many times it is re-selected.
            ApplySeriesStyle(Serie, FitSeriesStyle(fskSelectedInterval,
                FViewMarkers), 'Selected area');

            AddSerieToChart(Serie, SelectedArea, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    SelectedArea.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillDataTable(SelectedArea);
{$ENDIF}
    PlotPointsSet(SelectedArea);
end;

{$hints off}
procedure TFitViewer.PlotCurves(Sender: TObject;
    Curves: TSelfCopiedCompList; CurveAttributes: TMSCRCurveList);

    procedure AddCurveToChart(Curve: TNamedPointsSet; Index: longint);
    var
        Serie: TTASerie;
    begin
        CheckAssigned(Form, 'the main window this viewer draws into');

        if FCurves.IndexOf(Curve) = -1 then
        begin
            Serie := TTASerie.Create(nil);
            try
                //  WHICH colour is series_palette's arithmetic and the
                //  colours themselves are the widget set's; the conditional
                //  both replaced read ColorPalette[0] for every thirty-second
                //  curve, which is outside the array.
                ApplySeriesStyle(Serie, FitSeriesStyle(fskModelCurve,
                    FViewMarkers), Curve.FTitle, Index);

                //  THE CURVE'S OWN HANDLE, so deleting that curve can take
                //  this series with it.
                AddSerieToChart(Serie, Curve, OwnerOf(Curve));
            except
                Serie.Free;
                raise;
            end;
        end;
    end;

var
    Curve: TNamedPointsSet;
    j:  longint;
begin
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillCurveTable(CurveAttributes);
{$ENDIF}
    CheckAssigned(Curves, 'the curves the client asked to be drawn');

    for j := 0 to Curves.Count - 1 do
    begin
        Curve := TNamedPointsSet(Curves.Items[j]);
        AddCurveToChart(Curve, j + 1);
        //  The curve carries its own extent by now - the client stamps it from
        //  the attributes the server sends - so this needs no argument and,
        //  more to the point, neither does the re-plot in Plot.
        PlotPointsSet(Curve);
    end; {for j := 0 to GL.Count - 1 do...}
end;

{$hints on}

procedure TFitViewer.Clear(Sender: TObject);
begin
    CheckAssigned(Form, 'the main window this viewer draws into');

{$IFDEF USE_LEGEND}
    //  THE LEGEND FIRST, because its rows hold the series pointers that are
    //  about to be freed - and unconditionally, which is legend_layout's answer
    //  rather than this method's opinion: the flag says whether rows are being
    //  ADDED right now, and a row added while it was true must go whatever it
    //  says now, or it outlives the series it names.
    if LegendIsClearedWith(FUpdateLegends) then
        TFormMain(Form).CheckListBoxLegend.Items.Clear;
{$ENDIF}

    //  FREED, not merely unlinked. DeleteSerie takes a series off the chart and
    //  leaves it allocated, so clearing the chart used to leak every series on
    //  it. Freeing is enough by itself: TTASerie.Destroy calls DeleteSerie on
    //  its own chart, which is what makes SeriesCount fall and this loop
    //  terminate.
    while TFormMain(Form).Chart.SeriesCount <> 0 do
        TFormMain(Form).Chart.GetSerie(0).Free;
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    FCurves.Clear;
    FSeries.Clear;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
    begin
        ClearDataTable;
        ClearBackgroundTable;
        ClearPositionsTable;
        ClearBoundsTable;
        ClearCurveTable;
        ClearSummaryTable;
    end;
{$ENDIF}
end;

{ Takes ONE series off the chart, out of the legend and out of the register, and
  frees it. The single place a series stops being drawn, because every part of
  that used to be wrong in a different place.

  THE LEGEND ROW BY IDENTITY. This searched for a row whose TEXT equalled the
  series title and deleted the first match. Titles are not unique in practice -
  a contributor plots one series per pattern under one name - so it could delete
  another series' row; and when the row did not exist at all, because the legend
  was not being updated when the series was added, it deleted nothing and left
  the legend one row longer for good. The row's object IS the series, so
  identity answers exactly and -1 correctly means "this one never had a row".

  AND FREED. DeleteSerie only unlinks - it is TTASerie.Destroy that calls it,
  not the other way about - so every hidden series stayed allocated, and the
  chart hides and re-adds every model curve on every recompute. After the legend
  row, deliberately: the row holds this pointer. }
procedure TFitViewer.DropSerie(ASerie: TTASerie);
var
    LegendRow: longint;
begin
    if not Assigned(ASerie) then
        Exit;
{$IFDEF USE_LEGEND}
    LegendRow := TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(ASerie);
    if LegendRow >= 0 then
        TFormMain(Form).CheckListBoxLegend.Items.Delete(LegendRow);
{$ENDIF}
    //  Out of the register too, or it would keep naming a freed series as
    //  belonging to a curve - worse than not recording the relation at all,
    //  because the next removal would follow it.
    FSeries.Remove(FSeries.IndexOfSerie(ASerie));
    TFormMain(Form).Chart.DeleteSerie(ASerie);
    ASerie.Free;
end;

procedure TFitViewer.Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
var
    Index: longint;
begin
    CheckAssigned(PointsSet, 'the points set to stop drawing');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    //  ASKED OF THE REGISTER, not of a position. The chart index and FCurves
    //  are parallel only by luck of construction, and this is the operation
    //  that breaks that parallelism.
    Index := FSeries.IndexOfPoints(PointsSet);
    if Index <> -1 then
    begin
        DropSerie(TTASerie(FSeries.Item(Index).Serie));
        FCurves.Remove(PointsSet);
    end;
end;

procedure TFitViewer.Refresh(Sender: TObject);
var
    i:  longint;
    PointsSet: TNeutronPointsSet;
begin
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    for i := 0 to FCurves.Count - 1 do
    begin
        PointsSet := TNeutronPointsSet(FCurves.Items[i]);
        RefreshPointsSet(Sender, PointsSet);
    end;
end;

procedure TFitViewer.RefreshPointsSet(Sender: TObject; PointsSet: TNeutronPointsSet);
var
    Index, j, k: longint;
    Serie: TTASerie;
begin
    CheckAssigned(PointsSet, 'the points set to redraw');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    Index := FCurves.IndexOf(PointsSet);
    CheckThat(Index <> -1, 'a points set can only be redrawn while it is still plotted');

    Serie := TTASerie(TFormMain(Form).Chart.GetSerie(Index));
    //  The series holds the points PlotPointsSet let through, so this has to
    //  walk the same filter or it writes each y to the wrong point - silently,
    //  since the assertion that would have caught it is compiled out of a
    //  release build.
    CheckThat(Serie.Count = PointsSet.PointsCount, 'the chart series must hold exactly the points the set does, or intensities are written to the wrong points');

    with PointsSet do
        for j := 0 to PointsCount - 1 do
            Serie.SetYValue(j, PointIntensity[j]);
end;

procedure TFitViewer.HideRFactorBounds(Sender: TObject;
    RFactorBounds: TTitlePointsSet);
begin
    Hide(Sender, RFactorBounds);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearBoundsTable;
{$ENDIF}
end;

procedure TFitViewer.PlotRFactorBounds(Sender: TObject;
    RFactorBounds: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(RFactorBounds, 'the interval bounds to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(RFactorBounds) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskIntervalBounds,
                FViewMarkers), RFactorBounds.FTitle);

            AddSerieToChart(Serie, RFactorBounds, '');
        except
            Serie.Free;
            raise
        end;
    end;
    //  SORTED because the psVertLineXX styles need it.
    RFactorBounds.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillBoundsTable(RFactorBounds);
{$ENDIF}
    PlotPointsSet(RFactorBounds);
end;

procedure TFitViewer.HideCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Hide(Sender, CurvePositions);
{$IFDEF USE_GRIDS}
    ClearPositionsTable;
{$ENDIF}
end;

procedure TFitViewer.PlotCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(CurvePositions, 'the curve positions to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(CurvePositions) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskRequestedPositions,
                FViewMarkers), CurvePositions.FTitle);

            AddSerieToChart(Serie, CurvePositions, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    //  SORTED because the psVertLineXX styles need it. The TABLE is filled
    //  separately now - see TabulateCurvePositions - since which set it shows
    //  is a decision this method cannot make: it is handed one of the two.
    CurvePositions.Sort;
    PlotPointsSet(CurvePositions);
end;

{ The positions TABLE, filled from whichever set the client chose. }
procedure TFitViewer.TabulateCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
{$IFDEF USE_GRIDS}
    if FUpdateGrids and Assigned(CurvePositions) then
        FillPositionsTable(CurvePositions);
{$ENDIF}
end;

{ WHERE THE MODEL'S CURVES ACTUALLY SIT, beside where they were picked.

  A second series rather than a restyling of the first, because the two are
  different statements and the user needs both: "Curve positions" is what was
  asked for, this is what the fit made of it, and the distance between them is
  information. Before a fit it is empty and nothing is drawn.

  Hollow circles, not the picks' filled diagonal crosses: the two sets sit close
  together after a good fit, so they have to be separable at a glance rather than
  by colour alone.

  NOT SORTED, and it must not be. This set is derived from the built curves and
  may legitimately hold two instances that converged on one x0 - it is drawn as
  points only, so no drawing style needs an order, and there is no table under it
  to keep in step. }
procedure TFitViewer.PlotResultedCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(CurvePositions, 'the fitted curve positions to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(CurvePositions) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskAchievedPositions,
                FViewMarkers), CurvePositions.FTitle);

            AddSerieToChart(Serie, CurvePositions, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    PlotPointsSet(CurvePositions);
end;

procedure TFitViewer.HideResultedCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Hide(Sender, CurvePositions);
end;

{ Translates the framework's marker vocabulary into the charting component's.
  THE ONLY PLACE that mapping exists, which is what lets the component be
  replaced without touching a module or the presenter contract. }

{ A module's own series: markers, optionally joined. The presenter has already
  put the points where they belong, so nothing here needs to know what they
  mean. }
procedure TFitViewer.PlotModuleSeries(Sender: TObject; const AName: string;
    APoints: TTitlePointsSet; ALabels: TStrings;
    const AStyle: TModuleSeriesStyle;
    const AOwnerCurveId: string = '');
var
    Serie: TTASerie;
    Marker: TSeriesMarker;
begin
    CheckAssigned(APoints, 'the module series points');
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(Form, 'the form');

    if FCurves.IndexOf(APoints) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            //  A module states its style rather than naming a series kind,
            //  so only the shape needs translating - series_style does that,
            //  and refuses a shape it does not know rather than drawing the
            //  wrong one.
            if not ChartMarkerForModuleShape(AStyle.Shape, Marker) then
                CheckUnreachable('the module marker shape mapping');
            Serie.PointStyle := ChartPointStyle(Marker);
            Serie.ImageSize := AStyle.Size;
            Serie.SeriesColor := AStyle.Color;
            Serie.ShowLines := AStyle.ShowLines;
            Serie.ShowPoints := AStyle.ShowPoints;
            Serie.InitShowLines := Serie.ShowLines;
            Serie.InitShowPoints := Serie.ShowPoints;
            if AName <> '' then
                Serie.Title := AName
            else
                Serie.Title := APoints.FTitle;

            AddSerieToChart(Serie, APoints, AOwnerCurveId);
        except
            Serie.Free;
            raise;
        end;
    end;

    //  Sorting is the caller's declaration, not this method's choice: a series
    //  whose ORDER is its meaning - a sequence of wave pivots, say - is silently
    //  corrupted by sorting, while a scatter is unaffected either way.
    if AStyle.Sorted then
        APoints.Sort;
    PlotPointsSet(APoints);

    //  ALabels is accepted but not drawn: the forked TTASerie has no per-point
    //  caption API. The parameter stays in the contract because it is what a view
    //  SHOULD draw - a component that supports point labels can render it with no
    //  change to any module or to the presenter.
end;

procedure TFitViewer.HideModuleSeries(Sender: TObject; APoints: TTitlePointsSet);
begin
    if Assigned(APoints) then
        Hide(Sender, APoints);
end;

{ Handed straight to the form. The rows are already flattened, indented and
  captioned by whoever owns that vocabulary, so there is nothing to decide here -
  which is the point: no reasoning lives in a place a test cannot reach. }
{ EVERY SERIES DRAWN FOR ONE CURVE, framework's and contributor's alike.

  WHY THIS IS AN ACT AND NOT A CONSEQUENCE. A deleted curve's series did leave
  the chart before this existed - because every model change rebuilds the chart
  wholesale and each contributor replots its own markers from its own redraw
  hook. So the removal was a SIDE EFFECT of everybody redrawing, and a
  contributor that draws per-curve series without registering a redraw hook
  would leave its series behind with nothing to say so. }
procedure TFitViewer.HideSeriesOwnedBy(const ACurveId: string);
var
    Owned: TSeriesIndices;
    Points: TComponent;
    i: longint;
begin
    CheckAssigned(Form, 'the main window this viewer draws into');

    //  HIGHEST INDEX FIRST, which is what OwnedBy answers with - removing
    //  upwards would shift the indices this loop has left to visit.
    Owned := FSeries.OwnedBy(ACurveId);
    for i := 0 to High(Owned) do
    begin
        Points := TComponent(FSeries.Item(Owned[i]).Points);
        DropSerie(TTASerie(FSeries.Item(Owned[i]).Serie));
        //  AND OUT OF FCurves, which the re-plot still walks - a point set left
        //  behind would be redrawn into a series that is no longer there.
        if Assigned(Points) then
            FCurves.Remove(Points);
    end;
end;

procedure TFitViewer.ShowModulePanel(const APanelId: string;
    const ARows: TOutline);
begin
    TFormMain(Form).ShowModulePanel(APanelId, ARows);
end;

procedure TFitViewer.PlotSelectedPoints(Sender: TObject;
    SelectedPoints: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(SelectedPoints, 'the picked points to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(SelectedPoints) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskPickedPoints,
                FViewMarkers), SelectedPoints.FTitle);

            AddSerieToChart(Serie, SelectedPoints, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    //  SORTED because the psVertLineXX styles need it.
    SelectedPoints.Sort;
    PlotPointsSet(SelectedPoints);
end;

procedure TFitViewer.PlotComputedProfile(Sender: TObject; ComputedProfile: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(ComputedProfile, 'the computed profile to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(ComputedProfile) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskComputedProfile,
                FViewMarkers), ComputedProfile.FTitle);

            AddSerieToChart(Serie, ComputedProfile, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    Plot; // TODO: sdelat' optimal'no - bez polnogo perestroeniya
end;

procedure TFitViewer.PlotDeltaProfile(Sender: TObject; DeltaProfile: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(DeltaProfile, 'the residual profile to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(DeltaProfile) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskResidual,
                FViewMarkers), DeltaProfile.FTitle);

            AddSerieToChart(Serie, DeltaProfile, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    Plot; //TODO: sdelat' optimal'no - bez polnogo perestroeniya
end;

procedure TFitViewer.HideExpProfile(Sender: TObject; DataPoints: TTitlePointsSet);
begin
    Hide(Sender, DataPoints);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearDataTable;
{$ENDIF}
end;

procedure TFitViewer.HideBackground(Sender: TObject;
    BackgroundPoints: TTitlePointsSet);
begin
    Hide(Sender, BackgroundPoints);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearBackgroundTable;
{$ENDIF}
end;

procedure TFitViewer.PlotBackground(Sender: TObject;
    BackgroundPoints: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(BackgroundPoints, 'the background points to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(BackgroundPoints) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskBackground,
                FViewMarkers), BackgroundPoints.FTitle);

            AddSerieToChart(Serie, BackgroundPoints, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    BackgroundPoints.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillBackgroundTable(BackgroundPoints);
{$ENDIF}
    PlotPointsSet(BackgroundPoints);
end;

procedure TFitViewer.PlotExpProfile(Sender: TObject; ExpProfile: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    CheckAssigned(ExpProfile, 'the experimental profile to plot');
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');
    CheckAssigned(Form, 'the main window this viewer draws into');

    if FCurves.IndexOf(ExpProfile) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            ApplySeriesStyle(Serie, FitSeriesStyle(fskExperimentalProfile,
                FViewMarkers), ExpProfile.FTitle);

            AddSerieToChart(Serie, ExpProfile, '');
        except
            Serie.Free;
            raise;
        end;
    end;
    ExpProfile.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillDataTable(ExpProfile);
{$ENDIF}
    PlotPointsSet(ExpProfile);
end;

{$IFDEF USE_GRIDS}
procedure TFitViewer.SetUpdateGrids(Update: boolean);
begin
    FUpdateGrids := Update;
end;
{$ENDIF}

{$IFDEF USE_LEGEND}
procedure TFitViewer.SetUpdateLegends(Update: boolean);
begin
    FUpdateLegends := Update;
end;
{$ENDIF}

{$IFNDEF SERVER}
procedure TFitViewer.ShowTime;
begin
    TFormMain(Form).ShowTime;
end;

procedure TFitViewer.ShowRFactor;
begin
    TFormMain(Form).ShowRFactor;
end;

procedure TFitViewer.ShowHint(Hint: string);
begin
    TFormMain(Form).ShowHint(Hint);
end;
{$ENDIF}

constructor TFitViewer.Create(AOwner: TComponent);
begin
    inherited;
    { List shouldn't destroy components,
      they are destroyed by owners. }
    FCurves := TComponentList.Create(False);
    FSeries := TSeriesRegister.Create;
    FXCoordMode    := 0;
    FUpdateGrids   := True;
    FUpdateLegends := True;
end;

destructor TFitViewer.Destroy;
begin
    FCurves.Free;
    FSeries.Free;
    FCustomAxis.Free;
    inherited;
end;

procedure TFitViewer.SetCustomAxis(const AName, AUnit, AForward, AInverse: string);
begin
    FCustomAxis.Free;
    FCustomAxis := TExpressionAxis.Create(AName, AUnit, AForward, AInverse);
end;

function TFitViewer.GetActiveCurveIndex: longint;
var
    i:  longint;
    Serie: TTASerie;
begin
    Result := -1;

    CheckAssigned(Form, 'the main window this viewer draws into');
    CheckThat(TFormMain(Form).Chart.SeriesCount <> 0, 'there must be something on the chart before a curve can be the active one');

    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        Serie := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        if Serie.ShowPoints or Serie.ShowLines then
        begin
            Result := i;
            Break;
        end;
    end;{for i := 0 to SeriesCount - 1 do...}
    CheckThat(Result <> -1, 'exactly one chart series is drawn, so one of them is always the active curve');
end;

function TFitViewer.GetActivePointsSet: TNeutronPointsSet;
var
    ActiveCurveIndex: longint;
begin
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    ActiveCurveIndex := GetActiveCurveIndex;

    CheckIndex(ActiveCurveIndex, FCurves.Count, 'the list of curves this viewer has plotted');
    Result := TNeutronPointsSet(FCurves.Items[ActiveCurveIndex]);
end;

function TFitViewer.GetPointsSet(ActiveCurveIndex: longint): TNeutronPointsSet;
begin
    CheckAssigned(FCurves, 'the list of curves this viewer has plotted');

    CheckIndex(ActiveCurveIndex, FCurves.Count, 'the list of curves this viewer has plotted');
    Result := TNeutronPointsSet(FCurves.Items[ActiveCurveIndex]);
end;

procedure TFitViewer.SetXCoordMode(AMode: longint);
begin
    FXCoordMode := AMode;
    Plot;   //  TODO: this should be an event handler, and the mode should be
            //  set in TIntegralIntmaker.
end;

function TFitViewer.DisplayX(ARawValue, AWaveLength: double): double;
var
    Axis: TArgumentAxis;
begin
    //  The custom axis is a long-lived object owned by the viewer; the diffraction
    //  and identity axes are cheap value objects built per call for the transform.
    if FXCoordMode = XCM_CUSTOM then
    begin
        if Assigned(FCustomAxis) then
            Result := FCustomAxis.ToDisplay(ARawValue)
        else
            Result := ARawValue;
        Exit;
    end;

    //  Single-sourced in mscr_specimen_list, so the plotted transform, the axis
    //  caption and the parameters grid can never drift apart. The custom-axis
    //  arguments are empty because XCM_CUSTOM is handled above.
    Axis := CreateAxisForMode(FXCoordMode, AWaveLength, '', '', '', '');
    try
        Result := Axis.ToDisplay(ARawValue);
    finally
        Axis.Free;
    end;
end;

{ GetMinX/GetMaxX/GetMinY/GetMaxY/GetMinMax REMOVED, not extracted. Nothing in
  either repository called them - not this class, not IFitViewer, not the form -
  and the X half was wrong: it compared a RAW abscissa against an accumulator
  holding a DISPLAY one, so on any axis but the identity it answered with the
  extreme of a comparison between two different quantities. Extracting it would
  have moved thirty lines nobody wants into the counted half and given them
  tests. See docs/contributing/findings.md. }

procedure TFitViewer.SetViewMarkers(AViewMarkers: boolean);
begin
    FViewMarkers := AViewMarkers;
    ViewAllMarkers;
end;

procedure TFitViewer.ViewAllMarkers;
var
    i:  longint;
    Serie: TTASerie;
begin
    CheckAssigned(Form, 'the main window this viewer draws into');
    //  Turning markers on or off only means anything for series that are drawn
    //  as lines.
    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        Serie := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        //  WHICH SERIES THE TOGGLE MAY TOUCH is series_style's rule, asked here
        //  rather than restated: a series drawn without lines would be left
        //  blank by it, so it is exempt.
        if MarkersToggleApplies(Serie.ShowLines, Serie.InitShowLines) then
        begin
            Serie.ShowPoints     := FViewMarkers;
            Serie.InitShowPoints := FViewMarkers;
        end;
    end;
end;

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearBoundsTable;
begin
    with TFormMain(Form).GridIntervals do
    begin
        ColCount  := 2;
        //  Manual entry is not supported yet.
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := StartingPositionName;
        Cells[1, 0] := FinalPositionName;

        ResetColWidths;
    end;
end;

{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.FillBoundsTable(RFactorBounds: TTitlePointsSet);
var
    i, RowIndex: longint;
begin
    CheckAssigned(RFactorBounds, 'the interval bounds to tabulate');
    //  NOT cleared first: that would take the input focus away.
    //ClearBoundsTable;

    with TFormMain(Form).GridIntervals do
    begin
        ColCount  := 2;
        //  HOW MANY ROWS and WHAT IS IN THEM are in points_tables, where the
        //  half-picked interval - which is the state the user is in every time
        //  they mark one - can be asserted.
        RowCount  := IntervalTableRowCount(RFactorBounds.PointsCount);
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := StartingPositionName;
        Cells[1, 0] := FinalPositionName;

        for RowIndex := FixedRows to RowCount - 1 do
            for i := 0 to 1 do
                Cells[i, RowIndex] :=
                    IntervalCellText(RFactorBounds, i, RowIndex);

        ResetColWidths;
    end;
end;

{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearCurveTable;
begin
    with TFormMain(Form).GridParameters do
    begin
        //  Manual entry is not supported yet, so no spare row is added.
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := '         ';
        Cells[1, 0] := '         ';

        ResetColWidths;
    end;
    TFormMain(Form).TabSheetCurveAttributes.TabVisible := False;
end;

{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearPositionsTable;
begin
    with TFormMain(Form).GridSpecPositions do
    begin
        //  Manual entry is not supported yet, so no spare row is added.
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := NumberName;
        Cells[1, 0] := PositionName;

        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearBackgroundTable;
begin
    with TFormMain(Form).GridBackground do
    begin
        //  Manual entry is not supported yet, so no spare row is added.
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := PositionName;
        Cells[1, 0] := AmplitudeName;

        ResetColWidths;
    end;
end;

procedure TFitViewer.FillPositionsTable(CurvePositions: TTitlePointsSet);
var
    j: longint;
begin
    CheckAssigned(CurvePositions, 'the curve positions to tabulate');
    //  NOT cleared first: that would take the input focus away.
    //ClearPositionsTable;

    with TFormMain(Form).GridSpecPositions do
    begin
        ColCount  := 2;
        //  How many rows, and what is in each cell, are in points_tables.
        RowCount  := PointsTableRowCount(CurvePositions.PointsCount);
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := PositionName;
        Cells[1, 0] := AmplitudeName;

        for j := FixedRows to RowCount - 1 do
        begin
            Cells[0, j] := PointsCellText(CurvePositions, 0, j);
            Cells[1, j] := PointsCellText(CurvePositions, 1, j);
            //  Marks the cells as filled in.
            Objects[0, j] := TObject(1);
            Objects[1, j] := TObject(1);
        end;

        ResetColWidths;
    end;
end;

procedure TFitViewer.FillBackgroundTable(BackgroundPoints: TTitlePointsSet);
var
    j: longint;
begin
    CheckAssigned(BackgroundPoints, 'the background points to tabulate');
    //  NOT cleared first: that would take the input focus away.
    //ClearBackgroundTable;

    with TFormMain(Form).GridBackground do
    begin
        ColCount  := 2;
        //  How many rows, and what is in each cell, are in points_tables.
        RowCount  := PointsTableRowCount(BackgroundPoints.PointsCount);
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := PositionName;
        Cells[1, 0] := AmplitudeName;

        for j := FixedRows to RowCount - 1 do
        begin
            Cells[0, j] := PointsCellText(BackgroundPoints, 0, j);
            Cells[1, j] := PointsCellText(BackgroundPoints, 1, j);
            //  Marks the cells as filled in.
            Objects[0, j] := TObject(1);
            Objects[1, j] := TObject(1);
        end;

        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearDataTable;
begin
    with TFormMain(Form).GridData do
    begin
        ColCount  := 2;
        RowCount  := 2;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0]   := PositionName;
        Cells[1, 0]   := AmplitudeName;
        //  Clears the spare row.
        Cells[0, 1]   := '';
        Cells[1, 1]   := '';
        //  Marks the cells as NOT filled in.
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        ResetColWidths;
    end;
end;

procedure TFitViewer.FillDataTable(Profile: TTitlePointsSet);
var
    j: longint;
begin
    //  The whole profile, or the interval currently selected.
    CheckAssigned(Profile, 'the profile to tabulate');
    //  NOT cleared first: that would take the input focus away.
    //ClearDataTable;

    with TFormMain(Form).GridData do
    begin
        ColCount  := 2;
        //  ONE SPARE ROW, because this is the one table a value can be typed
        //  into. See points_tables for why the other three have none.
        RowCount  := EditablePointsTableRowCount(Profile.PointsCount);
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := PositionName;
        Cells[1, 0] := AmplitudeName;

        for j := FixedRows to RowCount - 2 do
        begin
            Cells[0, j] := PointsCellText(Profile, 0, j);
            Cells[1, j] := PointsCellText(Profile, 1, j);
            //  Marks the cells as filled in.
            Objects[0, j] := TObject(1);
            Objects[1, j] := TObject(1);
        end;
        //  Clears the spare row.
        Cells[0, RowCount - 1]   := '';
        Cells[1, RowCount - 1]   := '';
        //  Marks the last cells as NOT filled in.
        Objects[0, RowCount - 1] := TObject(0);
        Objects[1, RowCount - 1] := TObject(0);

        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearSummaryTable;
begin
    with TFormMain(Form).GridDatasheet do
    begin
        ColCount  := 4;
        //  This table needs no manual entry.
        RowCount  := 1;
        FixedCols := 1;
        FixedRows := 1;

        Cells[0, 0] := PositionName;
        Cells[1, 0] := AmplitudeName;
        Cells[2, 0] := TotalAmplitudeName;
        Cells[3, 0] := DifferenceName;
        ResetColWidths;
    end;
    TFormMain(Form).TabSheetSummary.TabVisible := False;
end;

procedure TFitViewer.FillSummaryTable(ExperimentalProfile: TTitlePointsSet;
    CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
    DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
var
    Table: TSummaryTable;
    Outcome: TSummaryOutcome;
    TableCol, TableRow: longint;
begin
    //  WHAT THE TABLE SAYS is decided in summary_table, where it can be tested;
    //  what is left here is putting it into a grid. The two used to be one
    //  method, so the column count, the row count, which curve belongs to which
    //  interval and where each value lands were all unreachable by any test.
    Table := TSummaryTable.Create;
    try
        Outcome := Table.Build(ExperimentalProfile, CurvesList,
            ComputedProfile, DeltaProfile, RFactorBounds);

        if Outcome = soModelIncomplete then
            //  Asked before there is anything to show. Silent, and the grid is
            //  left alone: this happens during an ordinary refresh.
            Exit;

        if not FUpdateGrids then
            Exit;

        if Outcome = soNoIntervals then
        begin
            //  Until the user has closed a first interval - and a module's own
            //  markup mode never creates one - there is nothing to tabulate, so
            //  the table is emptied rather than left showing the last fit.
            LogClientState('summary table: no data intervals, table cleared');
            ClearSummaryTable;
            Exit;
        end;

        TFormMain(Form).TabSheetSummary.TabVisible := True;
        with TFormMain(Form).GridDatasheet do
        begin
            ColCount := Table.ColCount;
            RowCount := Table.RowCount;
            FixedCols := 1;
            FixedRows := 1;
            //  NOT named Col and Row. A grid has properties by those names - the
            //  cursor position - and inside `with` they shadow a local, so
            //  `for Col := ...` walks the grid's cursor instead of a counter.
            //  Here it refused to compile, which is luck rather than protection;
            //  see the `with` entry in findings.md for the time it did compile.
            for TableCol := 0 to Table.ColCount - 1 do
                for TableRow := 0 to Table.RowCount - 1 do
                    Cells[TableCol, TableRow] :=
                        Table.CellAt(TableCol, TableRow);
            ResetColWidths;
        end;
        TFormMain(Form).FModifiedDatasheet := True;
    finally
        Table.Free;
    end;
end;

procedure TFitViewer.FillCurveTable(CurveList: TMSCRCurveList);
begin
    CheckAssigned(CurveList, 'the curve list to tabulate');

    TFormMain(Form).FCurveList := CurveList;
    TFormMain(Form).FCurveGrid.Assign(TFormMain(Form).GridParameters, CurveList);
    TFormMain(Form).FModifiedParameters := True;
    TFormMain(Form).TabSheetCurveAttributes.TabVisible := True;
end;
{$ENDIF}

{ GetMaxCurveNum and GetPointsNumInBounds MOVED to summary_table, as
  MaxCurvesInAnyInterval and PointsInBounds. Both are arithmetic over two point
  sets and neither named a widget; they were here only because the one method
  that used them wrote into a grid. }

{$IFDEF SERVER}
procedure TFitViewer.Paint;
begin
    TFormMain(Form).Chart.Paint;
end;
{$ENDIF}

end.
