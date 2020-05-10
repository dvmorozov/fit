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
    Classes, Contnrs, curve_points_set, Graphics, named_points_set,
    neutron_points_set, points_set, self_copied_component,
    SysUtils, title_points_set,
{$IFNDEF SERVER}
    fit_client, int_fit_viewer,
{$ENDIF}
    Forms, mscr_specimen_list, TAGraph;

{$IFNDEF SERVER}
// Switch on updating legend and grids.
{$DEFINE USE_LEGEND}
{$DEFINE USE_GRIDS}
{$ENDIF}

const
    { Display mode constants. }
    XCM_2T    = 0;
    XCM_T     = 1;
    XCM_SINTL = 2;

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
    protected
{$IFNDEF SERVER}
        FFitClient: TFitClient;
{$ENDIF}
        FXCoordMode: longint;
        FMaxX, FMinX, FMaxY, FMinY: double;
        FViewMarkers: boolean;
        FForm: TForm;
        { Enables updating grids. By default is true. }
        FUpdateGrids: boolean;
        { Enables updating legend. By default is true. }
        FUpdateLegends: boolean;
        { Enables animation mode in which UI is updated on every
          computation cycle not only on finishing. By default is false. }
        FAnimationMode: boolean;

        procedure SetXCoordMode(AMode: longint);

    protected
        { List of data sets for each item of which chart serie is related.
          The list is passive, it contains pointers to external data. }
        FPointsSetList: TComponentList;
        { Returns maximum number of curves in one of given R-factor intervals. }
        function GetMaxCurveNum(CurvesList: TSelfCopiedCompList;
            RFactorBounds: TTitlePointsSet): longint;
        { Returns total number of profile points belonging to any of intervals. }
        function GetPointsNumInBounds(Profile: TTitlePointsSet;
            RFactorBounds: TTitlePointsSet): longint;
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
        procedure ClearDatasheetTable;
{$ENDIF}
        function ValToStr(Value: double): string;
        { Clears serie set and fills it again. }
        procedure PlotPointsSet(PointsSet: TNeutronPointsSet);

    public
{$IFNDEF SERVER}
        procedure SetFitClient(AFitClient: TFitClient);
    protected
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
            CurvePointsSetList: TSelfCopiedCompList;
            CurveList: TMSCRCurveList);
        { Method of IFitViewer interface. }
        procedure PlotRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotComputedProfile(Sender: TObject;
            ComputedProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotDeltaProfile(Sender: TObject;
            DeltaProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotSelectedPoints(Sender: TObject;
            SelectedPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideCurvePositions(Sender: TObject;
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
        { Method of IFitViewer interface. }
        procedure SetUpdateGrids(Update: boolean);
        { Method of IFitViewer interface. }
        procedure SetUpdateLegends(Update: boolean);
{$IFDEF USE_GRIDS}
        { Method of IFitViewer interface. }
        procedure FillDatasheetTable(ExperimentalProfile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
{$ENDIF}
{$IFNDEF SERVER}
        { Method of IFitViewer interface. }
        procedure ShowTime;
        { Method of IFitViewer interface. }
        procedure ShowRFactor;
        { Method of IFitViewer interface. }
        procedure ShowHint(Hint: string);
        { Method of IFitViewer interface. }
        procedure SetAnimationMode(On: boolean);
        { Method of IFitViewer interface. }
        function GetAnimationMode: boolean;
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

        function GetMaxX: double;
        function GetMinX: double;
        function GetMaxY: double;
        function GetMinY: double;
        procedure GetMinMax(var AMinX, AMaxX, AMinY, AMaxY: double);

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property XCoordMode: longint read FXCoordMode write SetXCoordMode;
        property Form: TForm read FForm write FForm;
    end;

implementation

uses form_main;

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1e100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1e100;

{========================== TFitViewer ==================================}
{$IFNDEF SERVER}
procedure TFitViewer.SetFitClient(AFitClient: TFitClient);
begin
    FFitClient := AFitClient;
    FFitClient.FFitViewer := Self;
end;

{$ENDIF}
const
    ColorPalette: array[1..16] of TColor =
        (clRed, clGreen, clYellow, clBlue, clBlack, clGray, clFuchsia, clTeal,
        clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua, clBlack);

procedure TFitViewer.Plot;
var
    PointsSet: TNeutronPointsSet;
    j:  longint;
begin
    Assert(Assigned(FPointsSetList));

    for j := 0 to FPointsSetList.Count - 1 do
    begin
        PointsSet := TNeutronPointsSet(FPointsSetList.Items[j]);
        PlotPointsSet(PointsSet);
    end;
end;

procedure TFitViewer.PlotPointsSet(PointsSet: TNeutronPointsSet);
var
    Serie: TTASerie;
    i:  longint;
begin
    Assert(Assigned(PointsSet));
    Assert(Assigned(FPointsSetList));

    Serie := TTASerie(TFormMain(Form).Chart.GetSerie(FPointsSetList.IndexOf(PointsSet)));
    Serie.Clear;
    with PointsSet do
        for i := 0 to PointsCount - 1 do
            case XCoordMode of
                //!!! zdes' pochemu-to voznikala oshibka kompilyatsii, hotya
                //GetColor vozvraschaet tot zhe tip, chto potreblyaet AddXY !!!
                XCM_2T: Serie.AddXY(Point2T[i], PointIntensity[i], Serie.SeriesColor);
                XCM_T: Serie.AddXY(PointT[i], PointIntensity[i], Serie.SeriesColor);
                XCM_SINTL:
                    Serie.AddXY(PointSinTL[i], PointIntensity[i], Serie.SeriesColor);
            end;{case XCoordMode of...}
end;

procedure TFitViewer.PlotSelectedProfileInterval(Sender: TObject; SelectedArea: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    Assert(Assigned(SelectedArea));
    Assert(Assigned(FPointsSetList));

    if FPointsSetList.IndexOf(SelectedArea) = -1 then
    begin
        //  dobavlenie nabora tochek v spisok naborov tochek
        FPointsSetList.Add(SelectedArea);
        //  dobavlenie serii
        Serie := TTASerie.Create(nil);
        Serie.PointStyle := psRectangle;
        Serie.ShowPoints := FViewMarkers;
        Serie.Title := SelectedArea.FTitle;
        Serie.SeriesColor := clRed;
        Serie.PointBrushStyle := bsClear;

        TFormMain(Form).Chart.AddSerie(Serie);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject('Selected area', Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
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
    CurvePointsSetList: TSelfCopiedCompList; CurveList: TMSCRCurveList);

    procedure AddPointsSetToChart(PointsSet: TNamedPointsSet; Index: longint);
    var
        Serie: TTASerie;
    begin
        Assert(Assigned(Form));

        if FPointsSetList.IndexOf(PointsSet) = -1 then
        begin
            Serie := TTASerie.Create(nil);
            try
                Serie.PointStyle := psRectangle;
                Serie.ShowPoints := FViewMarkers;
                TFormMain(Form).Chart.AddSerie(Serie);
            except
                Serie.Free;
                raise;
            end;
            FPointsSetList.Add(PointsSet);

            Serie.Title := PointsSet.GetCurveTypeName + ' ' + IntToStr(Index);
{$IFDEF USE_LEGEND}
            if FUpdateLegends then
            begin
                TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
                TFormMain(Form).CheckListBoxLegend.Checked[
                    TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
            end;
{$ENDIF}
            if Index <= 16 then
                Serie.SeriesColor := ColorPalette[Index]
            else
                Serie.SeriesColor := ColorPalette[Index mod 16];
        end;
    end;

var
    PointsSet: TNamedPointsSet;
    j:  longint;
begin
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillCurveTable(CurveList);
{$ENDIF}
    Assert(Assigned(CurvePointsSetList));

    for j := 0 to CurvePointsSetList.Count - 1 do
    begin
        PointsSet := TNamedPointsSet(CurvePointsSetList.Items[j]);
        AddPointsSetToChart(PointsSet, j + 1);
        PlotPointsSet(PointsSet);
    end; {for j := 0 to GL.Count - 1 do...}
end;

{$hints on}

procedure TFitViewer.Clear(Sender: TObject);
begin
    Assert(Assigned(Form));

    while TFormMain(Form).Chart.SeriesCount <> 0 do
        TFormMain(Form).Chart.DeleteSerie(TFormMain(Form).Chart.GetSerie(0));
{$IFDEF USE_LEGEND}
    if FUpdateLegends then
        TFormMain(Form).CheckListBoxLegend.Items.Clear;
{$ENDIF}
    Assert(Assigned(FPointsSetList));

    FPointsSetList.Clear;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
    begin
        ClearDataTable;
        ClearBackgroundTable;
        ClearPositionsTable;
        ClearBoundsTable;
        ClearCurveTable;
        ClearDatasheetTable;
    end;
{$ENDIF}
end;

procedure TFitViewer.Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
var
    Index: longint;
begin
    Assert(Assigned(PointsSet));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    Index := FPointsSetList.IndexOf(PointsSet);
    // el-t v CheckListBox svyazan s el-tom v FPointsSetList tol'ko po indeksu
    if Index <> -1 then
    begin
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
            TFormMain(Form).CheckListBoxLegend.Items.Delete(Index);
{$ENDIF}
        if Index < TFormMain(Form).Chart.SeriesCount then
            TFormMain(Form).Chart.DeleteSerie(TFormMain(Form).Chart.GetSerie(Index));
        FPointsSetList.Extract(PointsSet);
    end;
end;

procedure TFitViewer.Refresh(Sender: TObject);
var
    i:  longint;
    PointsSet: TNeutronPointsSet;
begin
    Assert(Assigned(FPointsSetList));

    for i := 0 to FPointsSetList.Count - 1 do
    begin
        PointsSet := TNeutronPointsSet(FPointsSetList.Items[i]);
        RefreshPointsSet(Sender, PointsSet);
    end;
end;

procedure TFitViewer.RefreshPointsSet(Sender: TObject; PointsSet: TNeutronPointsSet);
var
    Index, j: longint;
    Serie: TTASerie;
begin
    Assert(Assigned(PointsSet));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    Index := FPointsSetList.IndexOf(PointsSet);
    Assert(Index <> -1);

    Serie := TTASerie(TFormMain(Form).Chart.GetSerie(Index));
    Assert(Serie.Count = PointsSet.PointsCount);

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
    Assert(Assigned(RFactorBounds));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    if FPointsSetList.IndexOf(RFactorBounds) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            Serie.PointStyle := psVertLineTB;
            Serie.ImageSize := 3;
            Serie.SeriesColor := clBlue;
            Serie.ShowLines := False;
            Serie.ShowPoints := True;
            Serie.InitShowLines := Serie.ShowLines;
            Serie.InitShowPoints := Serie.ShowPoints;
            Serie.Title := RFactorBounds.FTitle;

            TFormMain(Form).Chart.AddSerie(Serie);
        except
            Serie.Free;
            raise
        end;

        FPointsSetList.Add(RFactorBounds);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
    end;
    //  !!! pri ispol'zovanii psVertLineXX trebuetsya sortirovka !!!
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
    Assert(Assigned(CurvePositions));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    if FPointsSetList.IndexOf(CurvePositions) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            Serie.PointStyle := psDiagCross;
            Serie.ImageSize := 5;
            Serie.SeriesColor := clBlack;
            Serie.ShowLines := False;
            Serie.ShowPoints := True;
            Serie.InitShowLines := Serie.ShowLines;
            Serie.InitShowPoints := Serie.ShowPoints;
            Serie.Title := CurvePositions.FTitle;

            TFormMain(Form).Chart.AddSerie(Serie);
        except
            Serie.Free;
            raise;
        end;

        FPointsSetList.Add(CurvePositions);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
    end;
    //  !!! pri ispol'zovanii psVertLineXX trebuetsya sortirovka !!!
    //  !!! dlya vyvoda tablitsy trebuetsya sortirovka !!!
    CurvePositions.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillPositionsTable(CurvePositions);
{$ENDIF}
    PlotPointsSet(CurvePositions);
end;

procedure TFitViewer.PlotSelectedPoints(Sender: TObject;
    SelectedPoints: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    Assert(Assigned(SelectedPoints));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    if FPointsSetList.IndexOf(SelectedPoints) = -1 then
    begin
        Serie := TTASerie.Create(nil);

        try
            Serie.PointStyle := psVertLineBT;
            Serie.ImageSize := 3;
            Serie.SeriesColor := clGreen;
            Serie.ShowLines := False;
            Serie.ShowPoints := True;
            Serie.InitShowLines := Serie.ShowLines;
            Serie.InitShowPoints := Serie.ShowPoints;
            Serie.Title := SelectedPoints.FTitle;

            TFormMain(Form).Chart.AddSerie(Serie);
        except
            Serie.Free;
            raise;
        end;
        FPointsSetList.Add(SelectedPoints);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
    end;
    //  !!! pri ispol'zovanii psVertLineXX trebuetsya sortirovka !!!
    SelectedPoints.Sort;
    PlotPointsSet(SelectedPoints);
end;

procedure TFitViewer.PlotComputedProfile(Sender: TObject; ComputedProfile: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    Assert(Assigned(ComputedProfile));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    Serie := TTASerie.Create(nil);
    try
        Serie.PointStyle := psRectangle;
        Serie.ShowPoints := FViewMarkers;
        Serie.SeriesColor := clBlack;
        Serie.Title := ComputedProfile.FTitle;

        TFormMain(Form).Chart.AddSerie(Serie);
    except
        Serie.Free;
        raise;
    end;
    FPointsSetList.Add(ComputedProfile);
{$IFDEF USE_LEGEND}
    if FUpdateLegends then
    begin
        TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
        TFormMain(Form).CheckListBoxLegend.Checked[
            TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
    end;
{$ENDIF}
    Plot; //??? sdelat' optimal'no - bez polnogo perestroeniya
end;

procedure TFitViewer.PlotDeltaProfile(Sender: TObject; DeltaProfile: TTitlePointsSet);
var
    Serie: TTASerie;
begin
    Assert(Assigned(DeltaProfile));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    Serie := TTASerie.Create(nil);
    try
        Serie.PointStyle := psRectangle;
        Serie.ShowPoints := FViewMarkers;
        Serie.SeriesColor := clGreen;
        Serie.Title := DeltaProfile.FTitle;

        TFormMain(Form).Chart.AddSerie(Serie);
    except
        Serie.Free;
        raise;
    end;
    FPointsSetList.Add(DeltaProfile);
{$IFDEF USE_LEGEND}
    TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
    TFormMain(Form).CheckListBoxLegend.Checked[
        TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
{$ENDIF}
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
    Assert(Assigned(BackgroundPoints));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    if FPointsSetList.IndexOf(BackgroundPoints) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            Serie.PointStyle := psCircle;
            Serie.ImageSize := 3;
            Serie.SeriesColor := clGray;
            Serie.ShowLines := True;
            Serie.ShowPoints := True;
            Serie.InitShowLines := Serie.ShowLines;
            Serie.InitShowPoints := Serie.ShowPoints;
            Serie.Title := BackgroundPoints.FTitle;

            TFormMain(Form).Chart.AddSerie(Serie);
        except
            Serie.Free;
            raise;
        end;
        FPointsSetList.Add(BackgroundPoints);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
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
    Assert(Assigned(ExpProfile));
    Assert(Assigned(FPointsSetList));
    Assert(Assigned(Form));

    if FPointsSetList.IndexOf(ExpProfile) = -1 then
    begin
        Serie := TTASerie.Create(nil);
        try
            Serie.PointStyle := psRectangle;
            Serie.ShowPoints := FViewMarkers;
            Serie.SeriesColor := clRed;
            Serie.PointBrushStyle := bsClear;
            Serie.Title := ExpProfile.FTitle;

            TFormMain(Form).Chart.AddSerie(Serie);
        except
            Serie.Free;
            raise;
        end;

        FPointsSetList.Add(ExpProfile);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(Serie.Title, Serie);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(Serie)] := True;
        end;
{$ENDIF}
    end;
    ExpProfile.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillDataTable(ExpProfile);
{$ENDIF}
    PlotPointsSet(ExpProfile);
end;

procedure TFitViewer.SetUpdateGrids(Update: boolean);
begin
    FUpdateGrids := Update;
end;

procedure TFitViewer.SetUpdateLegends(Update: boolean);
begin
    FUpdateLegends := Update;
end;

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

procedure TFitViewer.SetAnimationMode(On: boolean);
begin
    FAnimationMode := On;
    if On then
    begin
        FUpdateGrids   := False;
        FUpdateLegends := False;
    end
    else
    begin
        FUpdateGrids   := True;
        FUpdateLegends := True;
    end;
end;

function TFitViewer.GetAnimationMode: boolean;
begin
    Result := FAnimationMode;
end;

{$ENDIF}

constructor TFitViewer.Create(AOwner: TComponent);
begin
    inherited;
    { List shouldn't destroy components,
      they are destroyed by owners. }
    FPointsSetList := TComponentList.Create(False);
    FXCoordMode    := 0;
    FUpdateGrids   := True;
    FUpdateLegends := True;
end;

destructor TFitViewer.Destroy;
begin
    FPointsSetList.Free;
    inherited;
end;

function TFitViewer.GetActiveCurveIndex: longint;
var
    i:  longint;
    Serie: TTASerie;
begin
    Result := -1;

    Assert(Assigned(Form));
    Assert(TFormMain(Form).Chart.SeriesCount <> 0);

    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        Serie := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        if Serie.ShowPoints or Serie.ShowLines then
        begin
            Result := i;
            Break;
        end;
    end;{for i := 0 to SeriesCount - 1 do...}
    Assert(Result <> -1);
end;

function TFitViewer.GetActivePointsSet: TNeutronPointsSet;
var
    ActiveCurveIndex: longint;
begin
    Assert(Assigned(FPointsSetList));

    ActiveCurveIndex := GetActiveCurveIndex;

    Assert(ActiveCurveIndex >= 0);
    Assert(ActiveCurveIndex < FPointsSetList.Count);
    Result := TNeutronPointsSet(FPointsSetList.Items[ActiveCurveIndex]);
end;

function TFitViewer.GetPointsSet(ActiveCurveIndex: longint): TNeutronPointsSet;
begin
    Assert(Assigned(FPointsSetList));

    Assert(ActiveCurveIndex >= 0);
    Assert(ActiveCurveIndex < FPointsSetList.Count);
    Result := TNeutronPointsSet(FPointsSetList.Items[ActiveCurveIndex]);
end;

procedure TFitViewer.SetXCoordMode(AMode: longint);
begin
    FXCoordMode := AMode;
    Plot;   //  TODO: dolzhen byt' obrabotchik sobytiya, a rezhim
            //  dolzhen ustanavlivat'sya v TIntegralIntmaker'e
end;

function TFitViewer.GetMaxX: double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(FMinX, FMaxX, FMinY, FMaxY);
    Result := FMaxX;
end;

function TFitViewer.GetMinX: double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(FMinX, FMaxX, FMinY, FMaxY);
    Result := FMinX;
end;

function TFitViewer.GetMaxY: double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(FMinX, FMaxX, FMinY, FMaxY);
    Result := FMaxY;
end;

function TFitViewer.GetMinY: double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(FMinX, FMaxX, FMinY, FMaxY);
    Result := FMinY;
end;

procedure TFitViewer.GetMinMax(var AMinX, AMaxX, AMinY, AMaxY: double);
var
    i, j: longint;
    PointsSet: TNeutronPointsSet;
begin
    AMinX := MAX_VALUE;
    AMaxX := MIN_VALUE;
    AMinY := MAX_VALUE;
    AMaxY := MIN_VALUE;
    Assert(Assigned(FPointsSetList));

    for i := 0 to FPointsSetList.Count - 1 do
        if FPointsSetList.Items[i] is TPointsSet then
        begin
            PointsSet := TNeutronPointsSet(FPointsSetList.Items[i]);
            for j := 0 to PointsSet.PointsCount - 1 do
            begin
                if PointsSet.PointXCoord[j] > FMaxX then
                    case XCoordMode of
                        XCM_T: AMaxX     := PointsSet.PointT[j];
                        XCM_2T: AMaxX    := PointsSet.Point2T[j];
                        XCM_SinTL: AMaxX := PointsSet.PointSinTL[j];
                    end;

                if PointsSet.PointXCoord[j] < FMinX then
                    case XCoordMode of
                        XCM_T: AMinX     := PointsSet.PointT[j];
                        XCM_2T: AMinX    := PointsSet.Point2T[j];
                        XCM_SinTL: AMinX := PointsSet.PointSinTL[j];
                    end;

                if PointsSet.PointYCoord[j] > AMaxY then
                    AMaxY := PointsSet.PointYCoord[j];
                if PointsSet.PointYCoord[j] < AMinY then
                    AMinY := PointsSet.PointYCoord[j];
            end;
        end; {if FPointsSetList.Items[i] is TPointsSet then...}
end;

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
    Assert(Assigned(Form));
    //  vkl./vykl. markerov imeet smysl tol'ko dlya teh grafikov,
    //  u kot. vklyucheno otobrazhenie liniy
    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        Serie := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        if (Serie.ShowLines) or (Serie.InitShowLines) then
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
        //  poka ruchnoy vvod ne podderzhivaetsya
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := StartName;
        Cells[1, 0] := StopName;

        ResetColWidths;
    end;
end;

{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.FillBoundsTable(RFactorBounds: TTitlePointsSet);
var
    i, RowIndex: longint;
begin
    Assert(Assigned(RFactorBounds));
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearBoundsTable;

    with TFormMain(Form).GridIntervals do
    begin
        ColCount  := 2;
        //  ruchnoy vvod v etu tabl. poka ne podderzhivaetsya,
        //  poetomu stroka ne doavlyaetsya
        RowCount  := RFactorBounds.PointsCount div 2 +
            RFactorBounds.PointsCount mod 2 +
              //  dop. stroka dobavl.
              //  pri nechetnom chisle tochek
            1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := StartName;
        Cells[1, 0] := StopName;

        i := 0;
        RowIndex := FixedRows;
        //  chislo tochek m.b. nechetnym, kogda posledniy interval ne zakryt
        while i < RFactorBounds.PointsCount do
        begin
            Cells[0, RowIndex] := ValToStr(RFactorBounds.PointXCoord[i]);
            Inc(i);
            if i < RFactorBounds.PointsCount then
                Cells[1, RowIndex] := ValToStr(RFactorBounds.PointXCoord[i]);
            Inc(i);
            Inc(RowIndex);
        end;

        ResetColWidths;
    end;
end;

{$ENDIF}

function TFitViewer.ValToStr(Value: double): string;
begin
    //  ogranichenie min. znacheniya pri isp. ffGeneral;
    //  chisla po modulyu men'she takogo - 0.00001 -
    //  otobrazhayutsya v eksponentsial'noy forme, poetomu
    //  budem schitat' takoe chislo minimal'no dopustimym
    //if Abs(Value) < 0.00001 then Value := 0;
    //  chisla takogo tipa - 10000000.999999 - izobrazhayutsya
    //  kak est', t.e. so vsemi razryadami;

    //  ffGeneral ne podhodit - ploho, kogda u chisel v
    //  tablitse raznoe chislo znakov posle zapyatoy;
    //  pri ispol'zovanii ffFixed malen'kie chisla
    //  okruglyayutsya do nulya avtomaticheski
    Result := FloatToStrF(Value, ffFixed, 8, 4);
end;

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearCurveTable;
begin
    with TFormMain(Form).GridParameters do
    begin
        //  poka ruchnoy vvod ne podderzhivaetsya,
        //  poetomu lishnyaya stroka ne dobavlyaetsya
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := '         ';
        Cells[1, 0] := '         ';

        ResetColWidths;
    end;
{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetParameters.TabVisible := False;
{$ENDIF}
end;

{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearPositionsTable;
begin
    with TFormMain(Form).GridSpecPositions do
    begin
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := NumberName;
        Cells[1, 0] := ArgumentName;

        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearBackgroundTable;
begin
    with TFormMain(Form).GridBackground do
    begin
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        ColCount  := 2;
        RowCount  := 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        ResetColWidths;
    end;
end;

procedure TFitViewer.FillPositionsTable(CurvePositions: TTitlePointsSet);
var
    j: longint;
begin
    Assert(Assigned(CurvePositions));
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearPositionsTable;

    with TFormMain(Form).GridSpecPositions do
    begin
        ColCount  := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        //RowCount := BackgroundPoints.PointsCount + 2;
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        RowCount  := CurvePositions.PointsCount + 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to CurvePositions.PointsCount - 1 do
        begin
            Cells[0, j + 1]   := ValToStr(CurvePositions.PointXCoord[j]);
            Cells[1, j + 1]   := ValToStr(CurvePositions.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
        end;

        ResetColWidths;
    end;
end;

procedure TFitViewer.FillBackgroundTable(BackgroundPoints: TTitlePointsSet);
var
    j: longint;
begin
    Assert(Assigned(BackgroundPoints));
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearBackgroundTable;

    with TFormMain(Form).GridBackground do
    begin
        ColCount  := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        //RowCount := BackgroundPoints.PointsCount + 2;
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        RowCount  := BackgroundPoints.PointsCount + 1;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to BackgroundPoints.PointsCount - 1 do
        begin
            Cells[0, j + 1]   := ValToStr(BackgroundPoints.PointXCoord[j]);
            Cells[1, j + 1]   := ValToStr(BackgroundPoints.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
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

        Cells[0, 0]   := ArgumentName;
        Cells[1, 0]   := ValueName;
        //  ochistka dopolnitel'noy stroki
        Cells[0, 1]   := '';
        Cells[1, 1]   := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        ResetColWidths;
    end;
end;

procedure TFitViewer.FillDataTable(Profile: TTitlePointsSet);
var
    j: longint;
begin
    //  vozvraschaet polnyy profil' ili vybrannyy v dannyy moment uchastok
    Assert(Assigned(Profile));
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearDataTable;

    with TFormMain(Form).GridData do
    begin
        ColCount  := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        RowCount  := Profile.PointsCount + 2;
        FixedCols := 0;
        FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to Profile.PointsCount - 1 do
        begin
            Cells[0, j + 1]   := ValToStr(Profile.PointXCoord[j]);
            Cells[1, j + 1]   := ValToStr(Profile.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
        end;
        //  ochistka dopolnitel'noy stroki
        Cells[0, RowCount - 1]   := '';
        Cells[1, RowCount - 1]   := '';
        //  priznaki NEzapolneniya poslednih yacheek
        Objects[0, RowCount - 1] := TObject(0);
        Objects[1, RowCount - 1] := TObject(0);

        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearDatasheetTable;
begin
    with TFormMain(Form).GridDatasheet do
    begin
        ColCount  := 4;
        //  ruchnoy vvod v etu tabl. ne nuzhen
        RowCount  := 1;
        FixedCols := 1;
        FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;
        Cells[2, 0] := SummarizedName;
        Cells[3, 0] := DeltaName;
        ResetColWidths;
    end;
{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetDatasheet.TabVisible := False;
{$ENDIF}
end;

procedure TFitViewer.FillDatasheetTable(ExperimentalProfile: TTitlePointsSet;
    CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
    DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
var
    i, j, k, StartIndex, EndIndex, RowIndex, ColIndex: longint;
    P:      TCurvePointsSet;
    StartX: double;
begin
    Assert(Assigned(ExperimentalProfile));
    Assert(Assigned(CurvesList));
    Assert(Assigned(ComputedProfile));
    Assert(Assigned(DeltaProfile));
    Assert(Assigned(RFactorBounds));
    Assert(RFactorBounds.PointsCount mod 2 = 0);
    Assert(RFactorBounds.PointsCount <> 0);

{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetDatasheet.TabVisible := True;
{$ENDIF}
    with TFormMain(Form).GridDatasheet do
    begin
        //  nastroyka parametrov setki
        //  chislo kolonok = 1 (fiks.) + 3
        //  (eksp. profil', rasschit. profil', raznost') +
        //  maksimal'noe chislo krivyh v nekotorom intervale
        ColCount    := 4 + GetMaxCurveNum(CurvesList, RFactorBounds);
        //  na kazhdyy interval dobavlyaetsya stroka zagolovka
        RowCount    := 1 + GetPointsNumInBounds(ExperimentalProfile, RFactorBounds) +
            RFactorBounds.PointsCount div 2;
        FixedCols   := 1;
        FixedRows   := 1;
        //  zapolnenie yacheek
        //  zagolovki stolbtsov (!!! d.b. ne men'she 4-h - sm. nizhe !!!)
        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;
        Cells[2, 0] := SummarizedName;
        Cells[3, 0] := DeltaName;
        for i := 4 to ColCount - 1 do
            Cells[i, 0] := 'Curve ' + IntToStr(i - 3);

        i := 0;
        RowIndex := FixedRows;
        while i < RFactorBounds.PointsCount do
        begin
            //  !!! RowIndex d. ukazyvat' na nachalo dannyh intervala !!!
            StartX := RFactorBounds.PointXCoord[i];
            //  formiruetsya zagolovok
            for j := 1 to ColCount - 1 do
                Cells[j, RowIndex] := '';
            Cells[1, RowIndex]     := 'Interval';
            Cells[2, RowIndex]     := 'number';
            Cells[3, RowIndex]     := IntToStr(i div 2 + 1);
            Inc(RowIndex);

            StartIndex := ExperimentalProfile.IndexOfValueX(RFactorBounds.PointXCoord[i]);
            EndIndex   := ExperimentalProfile.IndexOfValueX(RFactorBounds.PointXCoord[i + 1]);
            for j := StartIndex to EndIndex do
            begin
                Cells[0, RowIndex + j - StartIndex] :=
                    ValToStr(ExperimentalProfile.PointXCoord[j]);
                Cells[1, RowIndex + j - StartIndex] :=
                    ValToStr(ExperimentalProfile.PointYCoord[j]);
                Cells[2, RowIndex + j - StartIndex] :=
                    ValToStr(ComputedProfile.PointYCoord[j]);
                Cells[3, RowIndex + j - StartIndex] :=
                    ValToStr(DeltaProfile.PointYCoord[j]);
            end;
            //  po vsem krivym, otnosyaschimsya k dannomu intervalu
            ColIndex := 4;
            for j := 0 to CurvesList.Count - 1 do
            begin
                P := TCurvePointsSet(CurvesList.Items[j]);
                if StartX = P.PointXCoord[0] then
                begin
                    for k := 0 to P.PointsCount - 1 do
                        Cells[ColIndex, RowIndex + k] :=
                            ValToStr(P.PointYCoord[k]);
                    Inc(ColIndex);
                end;
            end;
            i := i + 2;
            RowIndex := RowIndex + EndIndex - StartIndex + 1;
        end;
        ResetColWidths;
    end;
    TFormMain(Form).FModifiedDatasheet := True;
end;

procedure TFitViewer.FillCurveTable(CurveList: TMSCRCurveList);
begin
    Assert(Assigned(CurveList));

    TFormMain(Form).FCurveList := CurveList;
    CurveList.GridAssign(TFormMain(Form).GridParameters);
    TFormMain(Form).FModifiedParameters := True;
{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetParameters.TabVisible := True;
{$ENDIF}
end;

{$ENDIF}

function TFitViewer.GetMaxCurveNum(CurvesList: TSelfCopiedCompList;
    RFactorBounds: TTitlePointsSet): longint;
var
    i, j, CurCount: longint;
    P:      TCurvePointsSet;
    StartX: double;
begin
    Result := 0;

    Assert(Assigned(CurvesList));
    Assert(Assigned(RFactorBounds));
    Assert(RFactorBounds.PointsCount mod 2 = 0);

    //  idem po vsem intervalam, podschityvaya krivye,
    //  kot. k nim otnosyatsya
    j := 0;
    while j < RFactorBounds.PointsCount do
    begin
        CurCount := 0;
        StartX   := RFactorBounds.PointXCoord[j];

        for i := 0 to CurvesList.Count - 1 do
        begin
            P := TCurvePointsSet(CurvesList.Items[i]);
            if StartX = P.PointXCoord[0] then
                Inc(CurCount);
        end;
        if CurCount > Result then
            Result := CurCount;
        j := j + 2;
    end;
end;

function TFitViewer.GetPointsNumInBounds(Profile: TTitlePointsSet;
    RFactorBounds: TTitlePointsSet): longint;
var
    j, StartIndex, EndIndex: longint;
begin
    Result := 0;

    Assert(Assigned(Profile));
    Assert(Assigned(RFactorBounds));
    Assert(RFactorBounds.PointsCount mod 2 = 0);

    j := 0;
    while j < RFactorBounds.PointsCount do
    begin
        StartIndex := Profile.IndexOfValueX(RFactorBounds.PointXCoord[j]);
        EndIndex := Profile.IndexOfValueX(RFactorBounds.PointXCoord[j + 1]);
        Result := Result + EndIndex - StartIndex + 1;
        j := j + 2;
    end;
end;

{$IFDEF SERVER}
procedure TFitViewer.Paint;
begin
    TFormMain(Form).Chart.Paint;
end;

{$ENDIF}

end.
