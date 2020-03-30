{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains base interface of communication from client to server.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_fit_service;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, mscr_specimen_list, named_points_set, points_set,
    self_copied_component, SysUtils, title_points_set
{$IFDEF _WINDOWS}
    , persistent_curve_parameters
{$ENDIF}
    ;

type
    { Server states. Sequence of states is designated by numbers. }
    TFitServerState = (
        { Waiting of loading profile data. }
        ProfileWaiting,
        { Background isn't removed yet after last profile loading.
          State must not change on loading background points. }
        BackNotRemoved,
        { Computation is performed. }
        AsyncOperation,
        { States below should be used only to inform user - optimization
          should be allowed in any case when background removed (ready to
          fit parameters in automatic mode). }
        ReadyForAutoFit,
        { Ready to fit with given user constraints. }
        ReadyForFit,
        { Computation has been finished, allows further restarting. }
        Finished
        );

    { Defines base interface of communication from client to server. }
    IFitService = interface
        function GetMaxRFactor: double;
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetBackFactor: double;
        procedure SetBackFactor(ABackFactor: double);
        function GetCurveThresh: double;
        procedure SetCurveThresh(ACurveThresh: double);
        function GetCurveType: TCurveTypeId;
{$IFNDEF FIT}
        { https://github.com/dvmorozov/fit/issues/160 }
        procedure SetCurveType(ACurveType: TCurveTypeId);
{$ENDIF}
        function GetState: TFitServerState;
        function GetWaveLength: double;
        procedure SetWaveLength(AWaveLength: double);
        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);
        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);

        { All GetXXXX methods create and return A NEW OBJECT,
          responsibility to free it is put on calling code. }

        { Returns hint or error message received from the server. }
        function SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
        function GetProfilePointsSet: TTitlePointsSet;
        function GetSelectedArea: TTitlePointsSet;

        { Returns hint or error message received from the server. }
        function SetBackgroundPointsSet(ABackgroundPoints: TTitlePointsSet): string;
        function GetBackgroundPoints: TTitlePointsSet;

        { Returns hint or error message received from the server. }
        function SetCurvePositions(ACurvePositions: TPointsSet): string;
        function GetCurvePositions: TTitlePointsSet;

        { Returns hint or error message received from the server. }
        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet;
{$IFDEF _WINDOWS}
        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means initialization. }
            CP: Curve_parameters);
{$ENDIF}
        procedure AddPointToData(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        procedure ReplacePointInData(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);

        function GetCurveList: TMSCRCurveList;
        function GetCurveCount: longint;
        function GetCurveParameterCount(SpecIndex: longint): longint;
        procedure GetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        procedure SetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            Value: double);
        function GetCurvesList: TSelfCopiedCompList;

        function GetCalcProfilePointsSet: TTitlePointsSet;
        function GetDeltaProfilePointsSet: TTitlePointsSet;

        { Asynchronous (long) methods. }

        { Returns hint or error message received from the server. }
        function SmoothProfile: string;
        procedure SubtractBackground(Auto: boolean);
        { Returns hint or error message received from the server. }
        function DoAllAutomatically: string;
        { Returns hint or error message received from the server. }
        function MinimizeDifference: string;
{$IFNDEF EXCLUDE_SOMETHING}
        { Returns hint or error message received from the server. }
        function MinimizeDifferenceAgain: string;
{$ENDIF}
        { Returns hint or error message received from the server. }
        function MinimizeNumberOfCurves: string;
        { Returns hint or error message received from the server. }
        function ComputeCurveBounds: string;
        { Returns hint or error message received from the server. }
        function ComputeBackgroundPoints: string;
        { Returns hint or error message received from the server. }
        function ComputeCurvePositions: string;
        { Returns hint or error message received from the server. }
        function SelectAllPointsAsCurvePositions: string;
{$IFDEF FITCGI}
        function GetGraph(const Width: longint;
            const Height: longint): TMemoryStream;
        function GetProfileChunk(const ChunkNum: longint): TTitlePointsSet;
        function GetProfileChunkCount: longint;
{$ENDIF}
        { Control methods. }

        procedure StopAsyncOper;
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;

        { Synchronous methods. }
        { Returns hint or error message received from the server. }
        function SelectArea(StartPointIndex, StopPointIndex: longint): string;
        { Returns hint or error message received from the server. }
        function ReturnToTotalProfile: string;
        procedure CreateCurveList;
    end;

    IFitProblem = interface(IFitService)
        procedure CreateProblem;
        function GetProblemId: longint;
        procedure SetProblemId(AProblemId: longint);
        procedure DiscardProblem(AProblemId: longint);
    end;

implementation

end.
