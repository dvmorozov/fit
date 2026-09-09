// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains interface defining methods to display data in chart and grids.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_fit_viewer;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, mscr_specimen_list, neutron_points_set, self_copied_component,
    title_points_set, module_view_types;

{$IFNDEF SERVER}
// Switch on updating legend and grids.
{$DEFINE USE_LEGEND}
{$DEFINE USE_GRIDS}
{$ENDIF}

type
    { Defines interface allowing to display data in chart and grids.
      Drawing methods for grids and chart legend are optional, can be
      included by conditional compilation. }
    IFitViewer = interface
        { Draws diagram data. }

        { Handler drawing specimen curves. Provides different ways of displaying data.
          Component which will actually display the data must store all pointers
          to visual components inside its own memory to be able hide them. }
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
        { Where the built model's curves sit, as opposed to where they were
          picked. A separate series because it is a separate statement. }
        procedure PlotResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        { Puts ACurvePositions into the positions TABLE.

          SEPARATE FROM PLOTTING, because which set the table shows is not
          which set the chart draws: the chart draws both, and the table shows
          the picks when there are any and the model's own positions when there
          are none. That decision is points_tables.PositionsForTable, and the
          client makes it - it is the only side that holds both sets. }
        procedure TabulateCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure HideResultedCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure PlotExpProfile(Sender: TObject;
            DataPoints: TTitlePointsSet);
        procedure HideExpProfile(Sender: TObject;
            DataPoints: TTitlePointsSet);
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

        { Draws a series a module owns - markers, optionally joined, optionally
          captioned - under a name the module chooses. APoints carries one point
          per marker and ALabels the caption for each, in the same order; ALabels
          may be nil.

          The points arrive in FINAL positions: a module whose markers belong
          on the cumulative model rather than on the component that owns them
          offsets them in its presenter, so the view draws what it is given and
          decides nothing.

          AStyle is described in the framework's own terms (module_view_types),
          never the charting component's, so the component can be replaced
          without touching this contract or any module.

          AOwnerCurveId NAMES THE CURVE THIS SERIES WAS DRAWN FOR, as the handle
          the model addresses that instance by - so deleting that curve takes
          this series with it. Empty for a series that belongs to the model as a
          whole rather than to one curve, which is also what a contributor that
          has no handle to give should pass: an empty owner keeps today's
          behaviour, where the series leaves the chart only when everything is
          replotted. }
        procedure PlotModuleSeries(Sender: TObject; const AName: string;
            APoints: TTitlePointsSet; ALabels: TStrings;
            const AStyle: TModuleSeriesStyle;
            const AOwnerCurveId: string = '');
        procedure HideModuleSeries(Sender: TObject; APoints: TTitlePointsSet);
        { Takes EVERY series drawn for one curve off the chart, framework's and
          contributor's alike, with its legend row.

          WHY THE RELATION IS RECORDED RATHER THAN RECONSTRUCTED: a curve is not
          one line, and a series carries a Title and nothing else - a title is
          neither unique nor an identity, and a contributor draws one series per
          curve under one name. See series_register. }
        procedure HideSeriesOwnedBy(const ACurveId: string);
        { Shows a module's rows in its own panel, or clears it when the array is
          empty. APanelId names which panel; the rows arrive already flattened,
          indented and captioned, so the view owns no vocabulary and no
          tree-building - the part that can be wrong stays where tests reach it.

          Why a panel at all: a chart can only imply a hierarchy. It draws a
          nested structure as more markers rather than as structure, so which
          item refines which is not readable from it. }
        procedure ShowModulePanel(const APanelId: string; const ARows: TOutline);
        { Refreshes all curves. }
        procedure Refresh(Sender: TObject);
        { Refreshes curve in the case of adding new or changing point. }
        procedure RefreshPointsSet(Sender: TObject;
            PointsSet: TNeutronPointsSet);
        { Is called before cleaning all diagram data. }
        procedure Clear(Sender: TObject);
        { Hides given point set and removes corresponding item from CheckBox. }
        procedure Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
{$IFDEF USE_GRIDS}
        { Handler to fill data table. }
        procedure FillSummaryTable(Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList; ComputedProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet; RFactorBounds: TTitlePointsSet);
        procedure SetUpdateGrids(Update: boolean);
{$ENDIF}
{$IFDEF USE_LEGEND}
        procedure SetUpdateLegends(Update: boolean);
{$ENDIF}
        { Displays computation time. }
        procedure ShowTime;
        { Displays latest not necessarily best R-factor value. }
        procedure ShowRFactor;
        { Displays hint about progress. }
        procedure ShowHint(Hint: string);
    end;

implementation

end.
