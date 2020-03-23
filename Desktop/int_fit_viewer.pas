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
    mscr_specimen_list, self_copied_component, title_points_set, neutron_points_set;

type
    { Defines interface allowing to display data in chart and grids.
      Drawing methods for grids and chart legend are optional, can be
      included by conditional compilation. }
    IFitViewer = interface
        { Draws diagram data. }

        { Handler drawing specimen curves. Provides different ways of displaying data.
          Component which will actually display the data must store all pointers
          to visual components inside its own memory to be able hide them. }
        procedure PlotSpecimens(Sender: TObject;
            CurvePointsSetList: TSelfCopiedCompList;
            SpecimenList: TMSCRSpecimenList);
        procedure PlotSelectedPoints(Sender: TObject;
            SelectedPoints: TTitlePointsSet);
        procedure PlotRFactorIntervals(Sender: TObject;
            RFactorIntervals: TTitlePointsSet);
        procedure HideRFactorIntervals(Sender: TObject;
            RFactorIntervals: TTitlePointsSet);
        procedure PlotCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure HideCurvePositions(Sender: TObject;
            CurvePositions: TTitlePointsSet);
        procedure PlotDataPoints(Sender: TObject;
            DataPoints: TTitlePointsSet);
        procedure HideDataPoints(Sender: TObject;
            DataPoints: TTitlePointsSet);
        procedure PlotSelectedArea(Sender: TObject;
            SelectedArea: TTitlePointsSet);
        procedure PlotBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        procedure HideBackground(Sender: TObject;
            BackgroundPoints: TTitlePointsSet);
        procedure PlotGaussProfile(Sender: TObject;
            GaussProfile: TTitlePointsSet);
        procedure PlotDeltaProfile(Sender: TObject;
            DeltaProfile: TTitlePointsSet);
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
        procedure FillDatasheetTable(Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList; GaussProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet; RFactorIntervals: TTitlePointsSet);
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
        { Turns on/off animation mode in which UI is updated
          on every computation cycle not only on finishing. }
        procedure SetAnimationMode(On: boolean);
        { Returns status of current animation mode. }
        function GetAnimationMode: boolean;
    end;

implementation

end.
