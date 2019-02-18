{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of component which can draw client data in UI.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit FitViewer;

{$MODE Delphi}

interface

uses DataLoader, Classes, SysUtils, Graphics, SelfCopied, CheckLst,
{$IFNDEF SERVER}
    FitClient, IntFitViewer,
{$ENDIF}
    tagraph, Forms, SelfCheckedComponentList, MSCRDataClasses;

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
    TFitViewer = class(TComponent, IFitViewer)
    protected
{$IFNDEF SERVER}
        FitClient: TFitClient;
{$ENDIF}
        FXCoordMode: LongInt;
        MaxX, MinX, MaxY, MinY: Double;
        ViewMarkers: Boolean;
        FForm: TForm;
        { Enables updating grids. By default is true. }
        FUpdateGrids: Boolean;
        { Enables updating legend. By default is true. }
        FUpdateLegends: Boolean;
        { Enables animation mode in which UI is updated on every
          computation cycle not only on finishing. By default is false. }
        FAnimationMode: Boolean;

        procedure SetXCoordMode(AMode: LongInt);

    protected
	{ List of data sets for each item of which chart serie is related.
	  The list is passive, it contains pointers to external data. }
        PointsSetList: TSelfCheckedComponentList;
	{ Returns maximum number of curves in one of given R-factor intervals. }
        function GetMaxCurveNum(
            CurvesList: TSelfCopiedCompList;
            RFactorIntervals: TTitlePointsSet
            ): LongInt;
	{ Returns total number of profile points belonging to any of intervals. }
        function GetPointsNumInIntervals(
            Profile: TTitlePointsSet;
            RFactorIntervals: TTitlePointsSet
            ): LongInt;
{$IFDEF USE_GRIDS}
        procedure FillIntervalsTable(RFactorIntervals: TTitlePointsSet);
        procedure FillDataTable(Profile: TTitlePointsSet);
        procedure FillBackgroundTable(BackgroundPoints: TTitlePointsSet);
        procedure FillPositionsTable(CurvePositions: TTitlePointsSet);
        procedure FillSpecimenTable(SpecimenList: TMSCRSpecimenList);

        procedure ClearIntervalsTable;
        procedure ClearDataTable;
        procedure ClearBackgroundTable;
        procedure ClearPositionsTable;
        procedure ClearSpecimenTable;
        procedure ClearDatasheetTable;
{$ENDIF}
        function ValToStr(Value: Double): string;
        { Clears serie set and fills it again. }
        procedure PlotPointsSet(SA: TNeutronPointsSet);

    public
{$IFNDEF SERVER}
        procedure SetFitClient(AFitClient: TFitClient);
    protected
{$ELSE}
        procedure Paint;
{$ENDIF}
    public
        { Method of IFitViewer interface. }
        procedure PlotBackground(
            Sender: TObject; BackgroundPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotDataPoints(
            Sender: TObject; DataPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotSelectedArea(
            Sender: TObject; SelectedArea: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotSpecimens(
            Sender: TObject;
            CurvePointsSetList: TSelfCopiedCompList;
            SpecimenList: TMSCRSpecimenList);
        { Method of IFitViewer interface. }
        procedure PlotRFactorIntervals(
            Sender: TObject; RFactorIntervals: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotCurvePositions(
            Sender: TObject; CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotGaussProfile(
            Sender: TObject; GaussProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotDeltaProfile(
            Sender: TObject; DeltaProfile: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure PlotSelectedPoints(
            Sender: TObject; SelectedPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideRFactorIntervals(
            Sender: TObject; RFactorIntervals: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideCurvePositions(
            Sender: TObject; CurvePositions: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideDataPoints(
            Sender: TObject; DataPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure HideBackground(
            Sender: TObject; BackgroundPoints: TTitlePointsSet);
        { Method of IFitViewer interface. }
        procedure Refresh(Sender: TObject);
	{ Does not clear series but only refreshes intencities. }
        procedure RefreshPointsSet(
            Sender: TObject; PointsSet: TNeutronPointsSet);
        { Method of IFitViewer interface. }
        procedure Clear(Sender: TObject);
        { Method of IFitViewer interface. }
        procedure Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
        { Method of IFitViewer interface. }
        procedure SetUpdateGrids(Update: Boolean);
        { Method of IFitViewer interface. }
        procedure SetUpdateLegends(Update: Boolean);
{$IFDEF USE_GRIDS}
        { Method of IFitViewer interface. }
        procedure FillDatasheetTable(
            Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList;
            GaussProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet;
            RFactorIntervals: TTitlePointsSet
            );
{$ENDIF}
        { Method of IFitViewer interface. }
        procedure ShowTime;
        { Method of IFitViewer interface. }
        procedure ShowRFactor;
        { Method of IFitViewer interface. }
        procedure ShowHint(Hint: string);
        { Method of IFitViewer interface. }
        procedure SetAnimationMode(On: Boolean);

        procedure SetViewMarkers(AViewMarkers: Boolean);
        procedure ViewAllMarkers;
	{ Clears all series and fills them again saving parameter values. }
        procedure Plot;
	{ Returns number of the first visible curve from curve list.
          This function actually gives the number of active curve
          when only single curve is visible in the chart. This should
          be checked separately. }
        function GetActiveCurve: LongInt;
        function GetActivePointsSet: TNeutronPointsSet;
        function GetPointsSet(ActiveNumber: LongInt): TNeutronPointsSet;

	{ Return boundary values among all curves. }
		
        function GetMaxX: Double;
        function GetMinX: Double;
        function GetMaxY: Double;
        function GetMinY: Double;
        procedure GetMinMax(var AMinX, AMaxX, AMinY, AMaxY: Double);

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        
        property XCoordMode: LongInt read FXCoordMode write SetXCoordMode;
        property Form: TForm read FForm write FForm;
    end;

implementation

uses Main,
{$IFNDEF SERVER}
    Unit1
{$ELSE}
    FormServer
{$ENDIF}
    ;

{========================== TFitViewer ==================================}
{$IFNDEF SERVER}
procedure TFitViewer.SetFitClient(AFitClient: TFitClient);
begin
    FitClient := AFitClient;
    FitClient.FitViewer := Self;
end;
{$ENDIF}
const
    ColorPalette: array[1..16] of TColor =
        (clRed, clGreen, clYellow, clBlue, clBlack, clGray, clFuchsia, clTeal,
         clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua, clBlack);
         
procedure TFitViewer.Plot;
var SA: TNeutronPointsSet;
    j: LongInt;
begin
    if not Assigned(PointsSetList) then Exit;
    
    for j := 0 to PointsSetList.Count - 1 do
    begin
        SA := TNeutronPointsSet(PointsSetList.Items[j]);
        PlotPointsSet(SA);
    end;
    //ViewAllMarkers;     //??? nado
end;

procedure TFitViewer.PlotPointsSet(SA: TNeutronPointsSet);
var LS: TTASerie;
    i: LongInt;
begin
    if not Assigned(SA) then Exit;
    
    LS := TTASerie(TFormMain(Form).Chart.GetSerie(PointsSetList.IndexOf(SA)));
    //LS.HorizAxis := aBottomAxis;
    LS.Clear;
    with SA do
        for i := 0 to PointsCount - 1 do
            case XCoordMode of
                //!!! zdes' pochemu-to voznikala oshibka kompilyatsii, hotya
                //GetColor vozvraschaet tot zhe tip, chto potreblyaet AddXY !!!
                XCM_2T: LS.AddXY(Point2T[i], PointIntensity[i], LS.SeriesColor);
                XCM_T: LS.AddXY(PointT[i], PointIntensity[i], LS.SeriesColor);
                XCM_SINTL:
                    LS.AddXY(PointSinTL[i], PointIntensity[i], LS.SeriesColor);
            end;{case XCoordMode of...}
end;

procedure TFitViewer.PlotSelectedArea(
    Sender: TObject; SelectedArea: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(SelectedArea));
    if not Assigned(SelectedArea) then Exit;

    if PointsSetList.IndexOf(SelectedArea) = -1 then
    begin
        //  dobavlenie nabora tochek v spisok naborov tochek
        PointsSetList.Add(SelectedArea);
        //  dobavlenie serii
        LS := TTASerie.Create(nil);
        LS.PointStyle := psRectangle;
        LS.ShowPoints := ViewMarkers;
        LS.Title := SelectedArea.Title;
        LS.SeriesColor := clRed;
        LS.PointBrushStyle := bsClear;

        TFormMain(Form).Chart.AddSerie(LS);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject('Selected area', LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
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

procedure TFitViewer.PlotSpecimens(
    Sender: TObject; CurvePointsSetList: TSelfCopiedCompList;
    SpecimenList: TMSCRSpecimenList);
var LS: TTASerie;
    SA: TCurvePointsSet;
    j: LongInt;
begin
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillSpecimenTable(SpecimenList);
{$ENDIF}
    //Assert(Assigned(CurvePointsSetList));
    if not Assigned(CurvePointsSetList) then Exit;
    
    for j := 0 to CurvePointsSetList.Count - 1 do
    begin
        SA := TCurvePointsSet(CurvePointsSetList.Items[j]);
        if PointsSetList.IndexOf(SA) = -1 then
        begin
            LS := TTASerie.Create(nil);
            LS.PointStyle := psRectangle;
            LS.ShowPoints := ViewMarkers;
            TFormMain(Form).Chart.AddSerie(LS);
            PointsSetList.Add(SA);

            LS.Title := SA.GetName + ' ' + IntToStr(j+1);
{$IFDEF USE_LEGEND}
            if FUpdateLegends then
            begin
                TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
                TFormMain(Form).CheckListBoxLegend.Checked[
                    TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
            end;
{$ENDIF}
            if j + 1 <= 16 then LS.SeriesColor := ColorPalette[j + 1]
            else LS.SeriesColor := ColorPalette[(j + 1) mod 16];
        end;
        PlotPointsSet(SA);
    end;{for j := 0 to GL.Count - 1 do...}
end;

procedure TFitViewer.Clear(Sender: TObject);
begin
    while TFormMain(Form).Chart.SeriesCount <> 0 do
        TFormMain(Form).Chart.DeleteSerie(TFormMain(Form).Chart.GetSerie(0));
{$IFDEF USE_LEGEND}
    if FUpdateLegends then
        TFormMain(Form).CheckListBoxLegend.Items.Clear;
{$ENDIF}
    if Assigned(PointsSetList) then PointsSetList.Clear;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
    begin
        ClearDataTable;
        ClearBackgroundTable;
        ClearPositionsTable;
        ClearIntervalsTable;
        ClearSpecimenTable;
        ClearDatasheetTable;
    end;
{$ENDIF}
end;

procedure TFitViewer.Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
var Index: LongInt;
begin
    if not Assigned(PointsSet) then Exit;
    if not Assigned(PointsSetList) then Exit;
    
    Index := PointsSetList.IndexOf(PointsSet);
    // el-t v CheckListBox svyazan s el-tom v PointsSetList tol'ko po indeksu
    if Index <> -1 then
    begin
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
            TFormMain(Form).CheckListBoxLegend.Items.Delete(Index);
{$ENDIF}
        if Index < TFormMain(Form).Chart.SeriesCount then
            TFormMain(Form).Chart.DeleteSerie(TFormMain(Form).Chart.GetSerie(Index));
        PointsSetList.Remove(PointsSet);
    end;
end;

procedure TFitViewer.Refresh(Sender: TObject);
var i: LongInt;
    NS: TNeutronPointsSet;
begin
    //Assert(Assigned(PointsSetList));
    if not Assigned(PointsSetList) then Exit;

    for i := 0 to PointsSetList.Count - 1 do
    begin
        NS := TNeutronPointsSet(PointsSetList.Items[i]);
        RefreshPointsSet(Sender, NS);
    end;
    //ViewAllMarkers; ??? nado
end;

procedure TFitViewer.RefreshPointsSet(
    Sender: TObject; PointsSet: TNeutronPointsSet);
var Index, j: LongInt;
    LS: TTASerie;
begin
    //Assert(Assigned(PointsSet));
    if not Assigned(PointsSet) then Exit;
    if not Assigned(PointsSetList) then Exit;

    Index := PointsSetList.IndexOf(PointsSet);
    Assert(Index <> -1);
 
    LS := TTASerie(TFormMain(Form).Chart.GetSerie(Index));
    Assert(LS.Count = PointsSet.PointsCount);
    with PointsSet do
        for j := 0 to PointsCount - 1 do
            LS.SetYValue(j, PointIntensity[j]);
end;

procedure TFitViewer.HideRFactorIntervals(
    Sender: TObject; RFactorIntervals: TTitlePointsSet);
begin
    Hide(Sender, RFactorIntervals);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearIntervalsTable;
{$ENDIF}
end;

procedure TFitViewer.PlotRFactorIntervals(
    Sender: TObject; RFactorIntervals: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(RFactorIntervals));
    if not Assigned(RFactorIntervals) then Exit;
    if not Assigned(PointsSetList) then Exit;

    if PointsSetList.IndexOf(RFactorIntervals) = -1 then
    begin
        LS := TTASerie.Create(nil);
        LS.PointStyle := psVertLineTB;
        LS.ImageSize := 3;
        LS.SeriesColor := clBlue;
        LS.ShowLines := False;
        LS.ShowPoints := True;
        LS.InitShowLines := LS.ShowLines;
        LS.InitShowPoints := LS.ShowPoints;
        LS.Title := RFactorIntervals.Title;
        
        TFormMain(Form).Chart.AddSerie(LS);
        PointsSetList.Add(RFactorIntervals);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
        end;
{$ENDIF}
    end;
    //  !!! pri ispol'zovanii psVertLineXX trebuetsya sortirovka !!!
    RFactorIntervals.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillIntervalsTable(RFactorIntervals);
{$ENDIF}
    PlotPointsSet(RFactorIntervals);
end;

procedure TFitViewer.HideCurvePositions(
    Sender: TObject; CurvePositions: TTitlePointsSet);
begin
    Hide(Sender, CurvePositions);
{$IFDEF USE_GRIDS}
    ClearPositionsTable;
{$ENDIF}
end;

procedure TFitViewer.PlotCurvePositions(
    Sender: TObject; CurvePositions: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(CurvePositions));
    if not Assigned(CurvePositions) then Exit;
    if not Assigned(PointsSetList) then Exit;

    if PointsSetList.IndexOf(CurvePositions) = -1 then
    begin
        LS := TTASerie.Create(nil);
        LS.PointStyle := psDiagCross;
        LS.ImageSize := 5;
        LS.SeriesColor := clBlack;
        LS.ShowLines := False;
        LS.ShowPoints := True;
        LS.InitShowLines := LS.ShowLines;
        LS.InitShowPoints := LS.ShowPoints;
        LS.Title := CurvePositions.Title;
        
        TFormMain(Form).Chart.AddSerie(LS);
        PointsSetList.Add(CurvePositions);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
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

procedure TFitViewer.PlotSelectedPoints(
    Sender: TObject; SelectedPoints: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(SelectedPoints));
    if not Assigned(SelectedPoints) then Exit;
    if not Assigned(PointsSetList) then Exit;

    if PointsSetList.IndexOf(SelectedPoints) = -1 then
    begin
        LS := TTASerie.Create(nil);
        LS.PointStyle := psVertLineBT;
        LS.ImageSize := 3;
        LS.SeriesColor := clGreen;
        LS.ShowLines := False;
        LS.ShowPoints := True;
        LS.InitShowLines := LS.ShowLines;
        LS.InitShowPoints := LS.ShowPoints;
        LS.Title := SelectedPoints.Title;
        
        TFormMain(Form).Chart.AddSerie(LS);
        PointsSetList.Add(SelectedPoints);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
        end;
{$ENDIF}
    end;
    //  !!! pri ispol'zovanii psVertLineXX trebuetsya sortirovka !!!
    SelectedPoints.Sort;
    PlotPointsSet(SelectedPoints);
end;

procedure TFitViewer.PlotGaussProfile(
    Sender: TObject; GaussProfile: TTitlePointsSet
    );
var LS: TTASerie;
begin
    //Assert(Assigned(GaussProfile));
    if not Assigned(GaussProfile) then Exit;
    if not Assigned(PointsSetList) then Exit;

    LS := TTASerie.Create(nil);
    LS.PointStyle := psRectangle;
    LS.ShowPoints := ViewMarkers;
    LS.SeriesColor := clBlack;
    LS.Title := GaussProfile.Title;
    
    TFormMain(Form).Chart.AddSerie(LS);
    PointsSetList.Add(GaussProfile);
{$IFDEF USE_LEGEND}
    if FUpdateLegends then
    begin
        TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
        TFormMain(Form).CheckListBoxLegend.Checked[
            TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
    end;
{$ENDIF}
    Plot; //??? sdelat' optimal'no - bez polnogo perestroeniya
end;

procedure TFitViewer.PlotDeltaProfile(
    Sender: TObject; DeltaProfile: TTitlePointsSet
    );
var LS: TTASerie;
begin
    //Assert(Assigned(DeltaProfile));
    if not Assigned(DeltaProfile) then Exit;
    if not Assigned(PointsSetList) then Exit;

    LS := TTASerie.Create(nil);
    LS.PointStyle := psRectangle;
    LS.ShowPoints := ViewMarkers;
    LS.SeriesColor := clGreen;
    LS.Title := DeltaProfile.Title;
    
    TFormMain(Form).Chart.AddSerie(LS);
    PointsSetList.Add(DeltaProfile);
{$IFDEF USE_LEGEND}
    TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
    TFormMain(Form).CheckListBoxLegend.Checked[
        TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
{$ENDIF}
    Plot; //??? sdelat' optimal'no - bez polnogo perestroeniya
end;

procedure TFitViewer.HideDataPoints(
    Sender: TObject; DataPoints: TTitlePointsSet);
begin
    Hide(Sender, DataPoints);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearDataTable;
{$ENDIF}
end;

procedure TFitViewer.HideBackground(
    Sender: TObject; BackgroundPoints: TTitlePointsSet);
begin
    Hide(Sender, BackgroundPoints);
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        ClearBackgroundTable;
{$ENDIF}
end;

procedure TFitViewer.PlotBackground(
    Sender: TObject; BackgroundPoints: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(BackgroundPoints));
    if not Assigned(BackgroundPoints) then Exit;
    if not Assigned(PointsSetList) then Exit;

    if PointsSetList.IndexOf(BackgroundPoints) = -1 then
    begin
        LS := TTASerie.Create(nil);
        LS.PointStyle := psCircle;
        LS.ImageSize := 3;
        LS.SeriesColor := clGray;
        LS.ShowLines := True;//False;
        LS.ShowPoints := True;
        LS.InitShowLines := LS.ShowLines;
        LS.InitShowPoints := LS.ShowPoints;
        LS.Title := BackgroundPoints.Title;
        
        TFormMain(Form).Chart.AddSerie(LS);
        PointsSetList.Add(BackgroundPoints);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
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

procedure TFitViewer.PlotDataPoints(
    Sender: TObject; DataPoints: TTitlePointsSet);
var LS: TTASerie;
begin
    //Assert(Assigned(DataPoints));
    if not Assigned(DataPoints) then Exit;
    if not Assigned(PointsSetList) then Exit;

    if PointsSetList.IndexOf(DataPoints) = -1 then
    begin
        LS := TTASerie.Create(nil);
        LS.PointStyle := psRectangle;
        LS.ShowPoints := ViewMarkers;
        LS.SeriesColor := clRed;
        LS.PointBrushStyle := bsClear;
        LS.Title := DataPoints.Title;
        
        TFormMain(Form).Chart.AddSerie(LS);
        PointsSetList.Add(DataPoints);
{$IFDEF USE_LEGEND}
        if FUpdateLegends then
        begin
            TFormMain(Form).CheckListBoxLegend.Items.AddObject(LS.Title, LS);
            TFormMain(Form).CheckListBoxLegend.Checked[
                TFormMain(Form).CheckListBoxLegend.Items.IndexOfObject(LS)] := True;
        end;
{$ENDIF}
    end;
    DataPoints.Sort;
{$IFDEF USE_GRIDS}
    if FUpdateGrids then
        FillDataTable(DataPoints);
{$ENDIF}
    PlotPointsSet(DataPoints);
end;

procedure TFitViewer.SetUpdateGrids(Update: Boolean);
begin
    FUpdateGrids := Update;
end;

procedure TFitViewer.SetUpdateLegends(Update: Boolean);
begin
    FUpdateLegends := Update;
end;

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

procedure TFitViewer.SetAnimationMode(On: Boolean);
begin
    FAnimationMode := On;
end;

constructor TFitViewer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    PointsSetList := TSelfCheckedComponentList.Create(nil);
    PointsSetList.SetState(cfPassive);
    FXCoordMode := 0;
    FUpdateGrids := true;
    FUpdateLegends := true;
end;

destructor TFitViewer.Destroy;
begin
    PointsSetList.Free;
    inherited Destroy;
end;

function TFitViewer.GetActiveCurve: LongInt;
var i: LongInt;
    TS: TTASerie;
begin
    Result := -1;
    Assert(TFormMain(Form).Chart.SeriesCount <> 0);

    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        TS := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        if TS.ShowPoints or TS.ShowLines then
        begin
            Result := i;
            Break;
        end
    end;{for i := 0 to SeriesCount - 1 do...}
    Assert(Result <> -1);
end;

function TFitViewer.GetActivePointsSet: TNeutronPointsSet;
var ActiveNumber: LongInt;
begin
    if not Assigned(PointsSetList) then begin Result := nil; Exit; end;
    ActiveNumber := GetActiveCurve;
    Result := TNeutronPointsSet(PointsSetList.Items[ActiveNumber]);
end;

function TFitViewer.GetPointsSet(ActiveNumber: LongInt): TNeutronPointsSet;
begin
    if not Assigned(PointsSetList) then begin Result := nil; Exit; end;
    Result := TNeutronPointsSet(PointsSetList.Items[ActiveNumber]);
end;

procedure TFitViewer.SetXCoordMode(AMode: LongInt);
var MinX,MaxX,MinY,MaxY: Double;
begin
    FXCoordMode := AMode;
    Plot;   //  dolzhen byt' obrabotchik sobytiya, a rezhim
            //  dolzhen ustanavlivat'sya v TIntegralIntmaker'e
    GetMinMax(MinX,MaxX,MinY,MaxY);
end;

function TFitViewer.GetMaxX: Double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(MinX,MaxX,MinY,MaxY);
    Result := MaxX;
end;

function TFitViewer.GetMinX: Double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(MinX,MaxX,MinY,MaxY);
    Result := MinX;
end;

function TFitViewer.GetMaxY: Double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(MinX,MaxX,MinY,MaxY);
    Result := MaxY;
end;

function TFitViewer.GetMinY: Double; //  sredi vseh prisoedinennyh krivyh
begin
    GetMinMax(MinX,MaxX,MinY,MaxY);
    Result := MinY;
end;

procedure TFitViewer.GetMinMax(var AMinX, AMaxX, AMinY, AMaxY: Double);
                                    //  sredi vseh prisoedinennyh krivyh
var i, j: LongInt;
    PS: TNeutronPointsSet;
begin
    AMinX := MAX_VALUE; AMaxX := MIN_VALUE;
    AMinY := MAX_VALUE; AMaxY := MIN_VALUE;
    if not Assigned(PointsSetList) then Exit;
    
    for i := 0 to PointsSetList.Count - 1 do
    begin
        if PointsSetList.Items[i] is TPointsSet then
        begin
            PS := TNeutronPointsSet(PointsSetList.Items[i]);
            for j := 0 to PS.PointsCount - 1 do
            begin
                if PS.PointXCoord[j] > MaxX then
                    case XCoordMode of
                        XCM_T: AMaxX := PS.PointT[j];
                        XCM_2T: AMaxX := PS.Point2T[j];
                        XCM_SinTL: AMaxX := PS.PointSinTL[j];
                    end;
                    
                if PS.PointXCoord[j] < MinX then
                    case XCoordMode of
                        XCM_T: AMinX := PS.PointT[j];
                        XCM_2T: AMinX := PS.Point2T[j];
                        XCM_SinTL: AMinX := PS.PointSinTL[j];
                    end;
                    
                if PS.PointYCoord[j] > AMaxY then AMaxY := PS.PointYCoord[j];
                if PS.PointYCoord[j] < AMinY then AMinY := PS.PointYCoord[j];
            end;
        end;{if PointsSetList.Items[i] is TPointsSet then...}
    end;
end;

procedure TFitViewer.SetViewMarkers(AViewMarkers: Boolean);
begin
    ViewMarkers := AViewMarkers;
    ViewAllMarkers;
end;

procedure TFitViewer.ViewAllMarkers;
var i: LongInt;
    TS: TTASerie;
begin
    //  vkl./vykl. markerov imeet smysl tol'ko dlya teh grafikov,
    //  u kot. vklyucheno otobrazhenie liniy
    for i := 0 to TFormMain(Form).Chart.SeriesCount - 1 do
    begin
        TS := TTASerie(TFormMain(Form).Chart.GetSerie(i));
        if (TS.ShowLines) or (TS.InitShowLines) then
        begin
            TS.ShowPoints := ViewMarkers;
            TS.InitShowPoints := ViewMarkers;
        end;
    end;
end;

{$IFDEF USE_GRIDS}
procedure TFitViewer.ClearIntervalsTable;
begin
    with TFormMain(Form).GridIntervals do
    begin
        ColCount := 2;
        //  poka ruchnoy vvod ne podderzhivaetsya
        RowCount := 1;//2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := StartName;
        Cells[1, 0] := StopName;

        //  ochistka dopolnitel'noy stroki
        (*
        Cells[0, 1] := '';
        Cells[1, 1] := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        *)
        ResetColWidths;
    end;
end;
{$ENDIF}

{$IFDEF USE_GRIDS}
procedure TFitViewer.FillIntervalsTable(RFactorIntervals: TTitlePointsSet);
var i, RowIndex: LongInt;
begin
    //Assert(Assigned(RFactorIntervals));
    if not Assigned(RFactorIntervals) then Exit;
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearIntervalsTable;

    with TFormMain(Form).GridIntervals do
    begin
        ColCount := 2;
        //  ruchnoy vvod v etu tabl. poka ne podderzhivaetsya,
        //  poetomu stroka ne doavlyaetsya
        RowCount := RFactorIntervals.PointsCount div 2 +
            RFactorIntervals.PointsCount mod 2 +    //  dop. stroka dobavl.
                                                    //  pri nechetnom chisle tochek
            1;//2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := StartName;
        Cells[1, 0] := StopName;

        i := 0; RowIndex := FixedRows;
        //  chislo tochek m.b. nechetnym, kogda posledniy interval ne zakryt
        while i < RFactorIntervals.PointsCount do
        begin
            Cells[0, RowIndex] := ValToStr(RFactorIntervals.PointXCoord[i]);
            Inc(i);
            if i < RFactorIntervals.PointsCount then
                Cells[1, RowIndex] := ValToStr(RFactorIntervals.PointXCoord[i]);
            Inc(i);
            Inc(RowIndex);
        end;
        //  ochistka dopolnitel'noy stroki
        (*
        if RowCount > FixedRows then
        begin
            Cells[0, RowCount - 1] := '';
            Cells[1, RowCount - 1] := '';
            //  priznaki NEzapolneniya yacheek
            Objects[0, RowCount - 1] := TObject(0);
            Objects[1, RowCount - 1] := TObject(0);
        end;
        *)
        ResetColWidths;
    end;
end;
{$ENDIF}

function TFitViewer.ValToStr(Value: Double): string;
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
procedure TFitViewer.ClearSpecimenTable;
begin
    with TFormMain(Form).GridParameters do
    begin
        //  poka ruchnoy vvod ne podderzhivaetsya,
        //  poetomu lishnyaya stroka ne dobavlyaetsya
        ColCount := 2; RowCount := 1;//2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := '         ';
        Cells[1, 0] := '         ';

        //  ochistka dopolnitel'noy stroki
        (*
        Cells[0, 1] := '';
        Cells[1, 1] := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        *)
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
        ColCount := 2; RowCount := 1;//2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := NumberName;
        Cells[1, 0] := ArgumentName;

        //  ochistka dopolnitel'noy stroki
        (*
        Cells[0, 1] := '';
        Cells[1, 1] := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        *)
        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearBackgroundTable;
begin
    with TFormMain(Form).GridBackground do
    begin
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        ColCount := 2; RowCount := 1;//2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        //  ochistka dopolnitel'noy stroki
        (*
        Cells[0, 1] := '';
        Cells[1, 1] := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        *)
        ResetColWidths;
    end;
end;

procedure TFitViewer.FillPositionsTable(CurvePositions: TTitlePointsSet);
var j: LongInt;
begin
    //Assert(Assigned(CurvePositions));
    if not Assigned(CurvePositions) then Exit;
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearPositionsTable;

    with TFormMain(Form).GridSpecPositions do
    begin
        ColCount := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        //RowCount := BackgroundPoints.PointsCount + 2;
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        RowCount := CurvePositions.PointsCount + 1;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to CurvePositions.PointsCount - 1 do
        begin
            Cells[0, j + 1] := ValToStr(CurvePositions.PointXCoord[j]);
            Cells[1, j + 1] := ValToStr(CurvePositions.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
        end;
        //  ochistka dopolnitel'noy stroki
        (*
        if RowCount > FixedRows then
        begin
            Cells[0, RowCount - 1] := '';
            Cells[1, RowCount - 1] := '';
            //  priznaki NEzapolneniya yacheek
            Objects[0, RowCount - 1] := TObject(0);
            Objects[1, RowCount - 1] := TObject(0);
        end;
        *)
        ResetColWidths;
    end;
end;

procedure TFitViewer.FillBackgroundTable(BackgroundPoints: TTitlePointsSet);
var j: LongInt;
begin
    //Assert(Assigned(BackgroundPoints));
    if not Assigned(BackgroundPoints) then Exit;
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearBackgroundTable;

    with TFormMain(Form).GridBackground do
    begin
        ColCount := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        //RowCount := BackgroundPoints.PointsCount + 2;
        //  poka ruchnoy vvod ne podderzhivaetsya, poetomu
        //  lishnyaya stroka ne dobavlyaetsya
        RowCount := BackgroundPoints.PointsCount + 1;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to BackgroundPoints.PointsCount - 1 do
        begin
            Cells[0, j + 1] := ValToStr(BackgroundPoints.PointXCoord[j]);
            Cells[1, j + 1] := ValToStr(BackgroundPoints.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
        end;
        //  ochistka dopolnitel'noy stroki
        (*
        if RowCount > FixedRows then
        begin
            Cells[0, RowCount - 1] := '';
            Cells[1, RowCount - 1] := '';
            //  priznaki NEzapolneniya yacheek
            Objects[0, RowCount - 1] := TObject(0);
            Objects[1, RowCount - 1] := TObject(0);
        end;
        *)
        ResetColWidths;
    end;
end;

procedure TFitViewer.ClearDataTable;
begin
    with TFormMain(Form).GridData do
    begin
        ColCount := 2; RowCount := 2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;
        //  ochistka dopolnitel'noy stroki
        Cells[0, 1] := '';
        Cells[1, 1] := '';
        //  priznaki NEzapolneniya yacheek
        Objects[0, 1] := TObject(0);
        Objects[1, 1] := TObject(0);
        ResetColWidths;
    end;
end;

procedure TFitViewer.FillDataTable(Profile: TTitlePointsSet);
var j: LongInt;
begin
    //  vozvraschaet polnyy profil' ili vybrannyy v dannyy moment uchastok
    //Assert(Assigned(Profile));
    if not Assigned(Profile) then Exit;
    //  !!! nel'zya isp., potomu chto sbivaet fokus vvoda !!!
    //ClearDataTable;

    with TFormMain(Form).GridData do
    begin
        ColCount := 2;
        //  dobavlyaetsya vsegda odna lishnyaya stroka,
        //  chtoby mozhno bylo vvodit' novye znacheniya vruchnuyu
        RowCount := Profile.PointsCount + 2;
        FixedCols := 0; FixedRows := 1;

        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;

        for j := 0 to Profile.PointsCount - 1 do
        begin
            Cells[0, j + 1] := ValToStr(Profile.PointXCoord[j]);
            Cells[1, j + 1] := ValToStr(Profile.PointYCoord[j]);
            //  priznaki zapolneniya yacheek
            Objects[0, j + 1] := TObject(1);
            Objects[1, j + 1] := TObject(1);
        end;
        //  ochistka dopolnitel'noy stroki
        Cells[0, RowCount - 1] := '';
        Cells[1, RowCount - 1] := '';
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
        ColCount := 4;
        //  ruchnoy vvod v etu tabl. ne nuzhen
        RowCount := 1;
        FixedCols := 1; FixedRows := 1;

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

procedure TFitViewer.FillDatasheetTable(
    Profile: TTitlePointsSet;
    CurvesList: TSelfCopiedCompList;
    GaussProfile: TTitlePointsSet;
    DeltaProfile: TTitlePointsSet;
    RFactorIntervals: TTitlePointsSet
    );
var i, j, k, StartIndex, EndIndex, RowIndex, ColIndex: LongInt;
    P: TCurvePointsSet;
    StartX: Double;
begin
    //Assert(Assigned(Profile));
    //Assert(Assigned(CurvesList));
    //Assert(Assigned(GaussProfile));
    //Assert(Assigned(DeltaProfile));
    //Assert(Assigned(RFactorIntervals));
    //Assert(RFactorIntervals.PointsCount mod 2 = 0);
    //Assert(RFactorIntervals.PointsCount <> 0);
    if not Assigned(Profile) then Exit;
    if not Assigned(CurvesList) then Exit;
    if not Assigned(GaussProfile) then Exit;
    if not Assigned(DeltaProfile) then Exit;
    if not Assigned(RFactorIntervals) then Exit;
    if not (RFactorIntervals.PointsCount mod 2 = 0) then Exit;
    if RFactorIntervals.PointsCount = 0 then Exit;
{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetDatasheet.TabVisible := True;
{$ENDIF}
    with TFormMain(Form).GridDatasheet do
    begin
        //  nastroyka parametrov setki
        //  chislo kolonok = 1 (fiks.) + 3
        //  (eksp. profil', rasschit. profil', raznost') +
        //  maksimal'noe chislo krivyh v nekotorom intervale
        ColCount := 4 + GetMaxCurveNum(CurvesList, RFactorIntervals);
        //  na kazhdyy interval dobavlyaetsya stroka zagolovka
        RowCount := 1 + GetPointsNumInIntervals(Profile, RFactorIntervals) +
            RFactorIntervals.PointsCount div 2;
        FixedCols := 1; FixedRows := 1;
        //  zapolnenie yacheek
        //  zagolovki stolbtsov (!!! d.b. ne men'she 4-h - sm. nizhe !!!)
        Cells[0, 0] := ArgumentName;
        Cells[1, 0] := ValueName;
        Cells[2, 0] := SummarizedName;
        Cells[3, 0] := DeltaName;
        for i := 4 to ColCount - 1 do Cells[i, 0] := 'Curve ' + IntToStr(i - 3);

        i := 0; RowIndex := FixedRows;
        while i < RFactorIntervals.PointsCount do
        begin
            //  !!! RowIndex d. ukazyvat' na nachalo dannyh intervala !!!
            StartX := RFactorIntervals.PointXCoord[i];
            //  formiruetsya zagolovok
            for j := 1 to ColCount - 1 do Cells[j, RowIndex] := '';
            Cells[1, RowIndex] := 'Interval';
            Cells[2, RowIndex] := 'number';
            Cells[3, RowIndex] := IntToStr(i div 2 + 1);
            Inc(RowIndex);

            StartIndex := Profile.IndexOfValueX(RFactorIntervals.PointXCoord[i]);
            EndIndex := Profile.IndexOfValueX(RFactorIntervals.PointXCoord[i + 1]);
            for j := StartIndex to EndIndex do
            begin
                Cells[0, RowIndex + j - StartIndex] :=
                    ValToStr(Profile.PointXCoord[j]);
                Cells[1, RowIndex + j - StartIndex] :=
                    ValToStr(Profile.PointYCoord[j]);
                Cells[2, RowIndex + j - StartIndex] :=
                    ValToStr(GaussProfile.PointYCoord[j]);
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
    TFormMain(Form).ModifiedDatasheet := True;
end;

procedure TFitViewer.FillSpecimenTable(SpecimenList: TMSCRSpecimenList);
begin
    //Assert(Assigned(SpecimenList));
    if not Assigned(SpecimenList) then Exit;
    TFormMain(Form).SpecimenList := SpecimenList;
    SpecimenList.GridAssign(TFormMain(Form).GridParameters);
    TFormMain(Form).ModifiedParameters := True;
{$IFDEF WINDOWS}
    TFormMain(Form).TabSheetParameters.TabVisible := True;
{$ENDIF}
end;
{$ENDIF}

function TFitViewer.GetMaxCurveNum(
    CurvesList: TSelfCopiedCompList;
    RFactorIntervals: TTitlePointsSet
    ): LongInt;
var i, j, CurCount: LongInt;
    P: TCurvePointsSet;
    StartX: Double;
begin
    Result := 0;

    //Assert(Assigned(CurvesList));
    //Assert(Assigned(RFactorIntervals));
    //Assert(RFactorIntervals.PointsCount mod 2 = 0);
    if not Assigned(CurvesList) then Exit;
    if not Assigned(RFactorIntervals) then Exit;
    if not (RFactorIntervals.PointsCount mod 2 = 0) then Exit;

    //  idem po vsem intervalam, podschityvaya krivye,
    //  kot. k nim otnosyatsya
    j := 0;
    while j < RFactorIntervals.PointsCount do
    begin
        CurCount := 0;
        StartX := RFactorIntervals.PointXCoord[j];

        for i := 0 to CurvesList.Count - 1 do
        begin
            P := TCurvePointsSet(CurvesList.Items[i]);
            if StartX = P.PointXCoord[0] then Inc(CurCount);
        end;
        if CurCount > Result then Result := CurCount;
        j := j + 2;
    end;
end;

function TFitViewer.GetPointsNumInIntervals(
    Profile: TTitlePointsSet;
    RFactorIntervals: TTitlePointsSet
    ): LongInt;
var j, StartIndex, EndIndex: LongInt;
begin
    Result := 0;
    
    //Assert(Assigned(Profile));
    //Assert(Assigned(RFactorIntervals));
    //Assert(RFactorIntervals.PointsCount mod 2 = 0);
    if not Assigned(Profile) then Exit;
    if not Assigned(RFactorIntervals) then Exit;
    if not (RFactorIntervals.PointsCount mod 2 = 0) then Exit;

    j := 0;
    while j < RFactorIntervals.PointsCount do
    begin
        StartIndex := Profile.IndexOfValueX(RFactorIntervals.PointXCoord[j]);
        EndIndex := Profile.IndexOfValueX(RFactorIntervals.PointXCoord[j + 1]);
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



