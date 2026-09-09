// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IFitViewer that records what it was asked to draw instead of drawing it.)

WHAT IT UNLOCKS. int_fit_viewer declares this interface so the client's logic can
be driven without a chart, and nothing had ever implemented it but the real
TFitViewer - which needs TAGraph, a form and a widget set. So fit_client.pas, 793
lines deciding what to plot and when to refresh, could only be reached through the
one path that cannot run headlessly.

WHAT IT ASSERTS ON. Not pixels - which series were asked for, how many points each
carried, and how many times a refresh happened. Those are the client's decisions;
the drawing is the view's, and the view is excluded from the coverage target for
exactly that reason (see docs/contributing/testing.md).

Every series is remembered by NAME and by point count, because the failure this
guards against is a series plotted from the wrong set - which looks like a plot,
not like an error.

See mock_support for the -SIcorba lifetime rule.
}
unit mock_fit_viewer;

{$MODE Delphi}

interface

uses
    Classes, SysUtils,
    int_fit_viewer, mock_support, module_view_types, mscr_specimen_list,
    named_points_set, neutron_points_set, self_copied_component,
    title_points_set;

type
    TMockFitViewer = class(TMockBase, IFitViewer)
    private
        FRefreshes: longint;
        FClears: longint;
        FHints: TStringList;
        FModulePanels: TStringList;
        { Point counts by series name, so a test can say which series got what. }
        FCounts: TStringList;
        { The two lists the last PlotCurves was given. }
        FLastCurves: TSelfCopiedCompList;
        FLastCurveAttributes: TMSCRCurveList;
        procedure Remember(const ASeries: string; APoints: TTitlePointsSet);
    public
        constructor Create; override;
        destructor Destroy; override;

        //  IFitViewer - the plotted series
        procedure PlotCurves(Sender: TObject;
            CurvePointsSetList: TSelfCopiedCompList;
            CurveList: TMSCRCurveList);
        procedure PlotSelectedPoints(Sender: TObject;
            SelectedPoints: TTitlePointsSet);
        procedure PlotRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        procedure HideRFactorBounds(Sender: TObject;
            RFactorBounds: TTitlePointsSet);
        procedure PlotCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure HideCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure PlotResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure TabulateCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure HideResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure PlotExpProfile(Sender: TObject; DataPoints: TTitlePointsSet);
        procedure HideExpProfile(Sender: TObject; DataPoints: TTitlePointsSet);
        procedure PlotSelectedProfileInterval(Sender: TObject;
            SelectedArea: TTitlePointsSet);
        procedure PlotBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        procedure HideBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        procedure PlotComputedProfile(Sender: TObject;
            ComputedProfile: TTitlePointsSet);
        procedure PlotDeltaProfile(Sender: TObject;
            DeltaProfile: TTitlePointsSet);

        //  IFitViewer - a module's own series and panel
        procedure PlotModuleSeries(Sender: TObject; const AName: string;
            APoints: TTitlePointsSet; ALabels: TStrings;
            const AStyle: TModuleSeriesStyle;
            const AOwnerCurveId: string = '');
        procedure HideModuleSeries(Sender: TObject; APoints: TTitlePointsSet);
        procedure HideSeriesOwnedBy(const ACurveId: string);
        procedure ShowModulePanel(const APanelId: string; const ARows: TOutline);

        //  IFitViewer - the rest
        procedure Refresh(Sender: TObject);
        procedure RefreshPointsSet(Sender: TObject;
            PointsSet: TNeutronPointsSet);
        procedure Clear(Sender: TObject);
        procedure Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
        //  DECLARED UNCONDITIONALLY, unlike in the interface itself. int_fit_viewer
        //  defines USE_GRIDS and USE_LEGEND inside its own source under
        //  {$IFNDEF SERVER}, so those symbols are NOT visible here - guarding these
        //  three the same way left the interface unimplemented and the compiler
        //  said so. Implementing them always is correct either way: if a build ever
        //  did compile the interface without them, these become three unused public
        //  methods rather than an error.
        procedure FillSummaryTable(Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
        procedure SetUpdateGrids(Update: boolean);
        procedure SetUpdateLegends(Update: boolean);
        procedure ShowTime;
        procedure ShowRFactor;
        procedure ShowHint(Hint: string);

        { How many points the named series was last plotted with, or -1 when it
          was never plotted at all. The two are different answers: a series
          plotted empty is a series the client decided to draw. }
        function PointsPlottedIn(const ASeries: string): longint;
        { True when the named series was plotted at least once. }
        function Plotted(const ASeries: string): boolean;
        { Every hint shown, in order. }
        property Hints: TStringList read FHints;
        { Panel ids shown, in order, with their row counts. }
        property ModulePanels: TStringList read FModulePanels;
        { What the last PlotCurves was handed, so a test can look INSIDE the
          model rather than only count it: which points each curve got, which
          handle it carries, whether the parameters line up with it.

          BORROWED, NOT OWNED, and that is the whole of the contract: the client
          frees both lists at the start of its next refresh, so a test must read
          them before it refreshes again. Nil until PlotCurves has been called. }
        property LastCurves: TSelfCopiedCompList read FLastCurves;
        property LastCurveAttributes: TMSCRCurveList read FLastCurveAttributes;
        property Refreshes: longint read FRefreshes;
        property Clears: longint read FClears;
    end;

implementation

constructor TMockFitViewer.Create;
begin
    inherited Create;
    FHints := TStringList.Create;
    FModulePanels := TStringList.Create;
    FCounts := TStringList.Create;
end;

destructor TMockFitViewer.Destroy;
begin
    FCounts.Free;
    FModulePanels.Free;
    FHints.Free;
    inherited;
end;

{ A nil set is recorded as -1 rather than skipped: "asked to plot nothing" and
  "never asked" are different client decisions, and only one of them is a bug. }
procedure TMockFitViewer.Remember(const ASeries: string;
    APoints: TTitlePointsSet);
begin
    if Assigned(APoints) then
        FCounts.Values[ASeries] := IntToStr(APoints.PointsCount)
    else
        FCounts.Values[ASeries] := '-1';
    FLog.Note(ASeries);
end;

function TMockFitViewer.PointsPlottedIn(const ASeries: string): longint;
begin
    if FCounts.IndexOfName(ASeries) < 0 then
        Result := -1
    else
        Result := StrToIntDef(FCounts.Values[ASeries], -1);
end;

function TMockFitViewer.Plotted(const ASeries: string): boolean;
begin
    Result := FLog.Saw(ASeries);
end;

procedure TMockFitViewer.PlotCurves(Sender: TObject;
    CurvePointsSetList: TSelfCopiedCompList; CurveList: TMSCRCurveList);
begin
    //  Counted by CURVES, not by points: this is the one series that is a list of
    //  series, and how many curves the model holds is the interesting number.
    if Assigned(CurvePointsSetList) then
        FCounts.Values['PlotCurves'] := IntToStr(CurvePointsSetList.Count)
    else
        FCounts.Values['PlotCurves'] := '-1';
    //  Kept as pointers, never copied: see the note on the properties. Copying
    //  would answer questions about a copy, and the handle a curve carries is
    //  exactly the thing a copy is entitled to change.
    FLastCurves := CurvePointsSetList;
    FLastCurveAttributes := CurveList;
    FLog.Note('PlotCurves');
end;

procedure TMockFitViewer.PlotSelectedPoints(Sender: TObject;
    SelectedPoints: TTitlePointsSet);
begin
    Remember('PlotSelectedPoints', SelectedPoints);
end;

procedure TMockFitViewer.PlotRFactorBounds(Sender: TObject;
    RFactorBounds: TTitlePointsSet);
begin
    Remember('PlotRFactorBounds', RFactorBounds);
end;

procedure TMockFitViewer.HideRFactorBounds(Sender: TObject;
    RFactorBounds: TTitlePointsSet);
begin
    Remember('HideRFactorBounds', RFactorBounds);
end;

procedure TMockFitViewer.PlotCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Remember('PlotCurvePositions', CurvePositions);
end;

procedure TMockFitViewer.HideCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Remember('HideCurvePositions', CurvePositions);
end;

procedure TMockFitViewer.PlotResultedCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Remember('PlotResultedCurvePositions', CurvePositions);
end;

procedure TMockFitViewer.TabulateCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Remember('TabulateCurvePositions', CurvePositions);
end;

procedure TMockFitViewer.HideResultedCurvePositions(Sender: TObject;
    CurvePositions: TTitlePointsSet);
begin
    Remember('HideResultedCurvePositions', CurvePositions);
end;

procedure TMockFitViewer.PlotExpProfile(Sender: TObject;
    DataPoints: TTitlePointsSet);
begin
    Remember('PlotExpProfile', DataPoints);
end;

procedure TMockFitViewer.HideExpProfile(Sender: TObject;
    DataPoints: TTitlePointsSet);
begin
    Remember('HideExpProfile', DataPoints);
end;

procedure TMockFitViewer.PlotSelectedProfileInterval(Sender: TObject;
    SelectedArea: TTitlePointsSet);
begin
    Remember('PlotSelectedProfileInterval', SelectedArea);
end;

procedure TMockFitViewer.PlotBackground(Sender: TObject;
    BackgroundPoints: TTitlePointsSet);
begin
    Remember('PlotBackground', BackgroundPoints);
end;

procedure TMockFitViewer.HideBackground(Sender: TObject;
    BackgroundPoints: TTitlePointsSet);
begin
    Remember('HideBackground', BackgroundPoints);
end;

procedure TMockFitViewer.PlotComputedProfile(Sender: TObject;
    ComputedProfile: TTitlePointsSet);
begin
    Remember('PlotComputedProfile', ComputedProfile);
end;

procedure TMockFitViewer.PlotDeltaProfile(Sender: TObject;
    DeltaProfile: TTitlePointsSet);
begin
    Remember('PlotDeltaProfile', DeltaProfile);
end;

procedure TMockFitViewer.PlotModuleSeries(Sender: TObject; const AName: string;
    APoints: TTitlePointsSet; ALabels: TStrings;
    const AStyle: TModuleSeriesStyle;
    const AOwnerCurveId: string = '');
begin
    //  Recorded under the MODULE'S OWN NAME, since that is what the contract says
    //  identifies the series - two module series must not collide.
    Remember('module:' + AName, APoints);
    //  AND UNDER ITS OWNER, where it has one, so a test can ask what a curve
    //  was drawn with without holding the series objects.
    if AOwnerCurveId <> '' then
        Remember('owner:' + AOwnerCurveId, APoints);
end;

procedure TMockFitViewer.HideSeriesOwnedBy(const ACurveId: string);
begin
    //  Under the HANDLE, not under a series - the point of the call is that the
    //  caller names a curve and nothing else.
    Remember('HideSeriesOwnedBy:' + ACurveId, nil);
end;

procedure TMockFitViewer.HideModuleSeries(Sender: TObject;
    APoints: TTitlePointsSet);
begin
    Remember('HideModuleSeries', APoints);
end;

procedure TMockFitViewer.ShowModulePanel(const APanelId: string;
    const ARows: TOutline);
begin
    FModulePanels.Add(APanelId + '=' + IntToStr(Length(ARows)));
    FLog.Note('ShowModulePanel', APanelId);
end;

procedure TMockFitViewer.Refresh(Sender: TObject);
begin
    Inc(FRefreshes);
    FLog.Note('Refresh');
end;

procedure TMockFitViewer.RefreshPointsSet(Sender: TObject;
    PointsSet: TNeutronPointsSet);
begin
    FLog.Note('RefreshPointsSet');
end;

procedure TMockFitViewer.Clear(Sender: TObject);
begin
    Inc(FClears);
    FLog.Note('Clear');
end;

procedure TMockFitViewer.Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
begin
    FLog.Note('Hide');
end;

procedure TMockFitViewer.FillSummaryTable(Profile: TTitlePointsSet;
    CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
    DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
begin
    FLog.Note('FillSummaryTable');
end;

procedure TMockFitViewer.SetUpdateGrids(Update: boolean);
begin
    FLog.Note('SetUpdateGrids', BoolToStr(Update, True));
end;

procedure TMockFitViewer.SetUpdateLegends(Update: boolean);
begin
    FLog.Note('SetUpdateLegends', BoolToStr(Update, True));
end;

procedure TMockFitViewer.ShowTime;
begin
    FLog.Note('ShowTime');
end;

procedure TMockFitViewer.ShowRFactor;
begin
    FLog.Note('ShowRFactor');
end;

procedure TMockFitViewer.ShowHint(Hint: string);
begin
    FHints.Add(Hint);
    FLog.Note('ShowHint', Hint);
end;

end.
