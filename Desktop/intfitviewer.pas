unit IntFitViewer;

{$mode delphi}

interface

uses
  Classes, SysUtils, SelfCheckedComponentList, MSCRDataClasses, SelfCopied,
  DataLoader;

type
    IFitViewer = interface
        { Draws diagram data. }

        { Handler drawing specimen curves. Provides different ways of displaying data.
          Component which will actually display the data must store all pointers
          to visual components inside its own memory to be able hide them. }
        procedure PlotSpecimens(Sender: TObject;
                CurvePointsSetList: TSelfCopiedCompList;
                SpecimenList: TMSCRSpecimenList);
        procedure PlotSelectedPoints(
                Sender: TObject; SelectedPoints: TTitlePointsSet);
        procedure PlotRFactorIntervals(
                Sender: TObject; RFactorIntervals: TTitlePointsSet);
        procedure HideRFactorIntervals(
                Sender: TObject; RFactorIntervals: TTitlePointsSet);
        procedure PlotCurvePositions(
                Sender: TObject; CurvePositions: TTitlePointsSet);
        procedure HideCurvePositions(
                Sender: TObject; CurvePositions: TTitlePointsSet);
        procedure PlotDataPoints(
                Sender: TObject; DataPoints: TTitlePointsSet);
        procedure HideDataPoints(
                Sender: TObject; DataPoints: TTitlePointsSet);
        procedure PlotSelectedArea(
                Sender: TObject; SelectedArea: TTitlePointsSet);
        procedure PlotBackground(
                Sender: TObject; BackgroundPoints: TTitlePointsSet);
        procedure HideBackground(
                Sender: TObject; BackgroundPoints: TTitlePointsSet);
        procedure PlotGaussProfile(
                Sender: TObject; GaussProfile: TTitlePointsSet);
        procedure PlotDeltaProfile(
                Sender: TObject; DeltaProfile: TTitlePointsSet);
        { Refreshes all curves. }
        procedure Refresh(Sender: TObject);
        { Refreshes curve in the case of adding new or changing point. }
        procedure RefreshPointsSet(
                Sender: TObject; PointsSet: TNeutronPointsSet);
        { Is called before cleaning all diagram data. }
        procedure Clear(Sender: TObject);
        procedure Hide(Sender: TObject; PointsSet: TNeutronPointsSet);
        {$IFDEF USE_GRIDS}
        { Handler to fill data table. }
        procedure TFitViewer.FillDatasheetTable(
            Profile: TTitlePointsSet;
            CurvesList: TSelfCopiedCompList;
            GaussProfile: TTitlePointsSet;
            DeltaProfile: TTitlePointsSet;
            RFactorIntervals: TTitlePointsSet
            );
        {$ENDIF}
    end;

implementation

end.

