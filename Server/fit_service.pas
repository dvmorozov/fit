{
  This software is distributed under GPL
  in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

  @abstract(Contains definition of class implementing server logic. Doesn't contain any user interface interaction.
  Exception handling and converting them into messages understandable for caller should be done in boundary objects.
  Throwing EAssertionFailed should be considered as inadmissibility of state for calling the method. At that state
  of the server should be kept. Throwing any other exception should be considered as fatal error. At that state of
  the server should be brought into initial which was just after start. Sometimes when method can execute the action
  accroding to its semantics EAssertionFailed can be catched inside it.)

  @author(Dmitry Morozov dvmorozov@hotmail.com,
  LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
  Facebook: https://www.facebook.com/dmitry.v.morozov)
}

unit fit_service;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

{
  TODO: Replace EAssertionFailed by an exception of another type
  because exceptions of this type can be thrown from libraries.
  In this case keeping the state of application can't be guaranteed.
}

uses
    calculated_curve_parameter, Classes, Contnrs, curve_points_set,
    curve_types_singleton, gauss_points_set, int_curve_type_selector,
    log, mscr_specimen_list, named_points_set,
    persistent_curve_parameter_container, persistent_curve_parameters,
    points_set, special_curve_parameter, SysUtils, title_points_set
{$IFDEF FIT}
    {Proxy to client to call it back.}
    , fit_server_proxy
{$ENDIF}
    , fit_task, int_client_callback, int_fit_service, MyExceptions
    , self_copied_component, SimpMath
{$IFDEF _WINDOWS}
{$IFDEF WINDOWS_SPECIFIC}
    , user_points_set
{$ENDIF}
    , user_curve_parameter, Windows
{$ENDIF}
    ;

type
    TRecreateServer = procedure of object;

    { The component which implements all server logic. It divides the task
      of profile fitting on a few subtasks of fitting on intervals.
      The intervals should be defined before manually or automatically.
      All interface methods should allow calling in arbitrary state, this
      should not be considered as inadmissible situation, but corresponding
      error code should be returned.
      In fitting knowledge of wavelength is not required. This is why all
      components TNeutronPointsSet were changed on TPointsSet. This also
      allows to abstract from tasks of neutron diffraction and to come to
      tasks from arbitrary field in which data have form of profile.
      It is impossible simply to replace TNeutronPointsSet on TPointsSet
      because at the server side methods of TFitViewer are called which
      require TTitlePointsSet. Moreover graphics is still implemented at
      server side. This requires usage of TNeutronPointsSet which supports
      argument recalculation.
      Interface methods results shows is requested operation allowed or not.
      Instead of EAssertionFailed the EUserException is used because it is
      impossible to guarantee that exception will not be thrown from library.
      This is acceptable only for classes which process user commands. Rest of
      the classes should use EAssertionFailed in the case of inadmissible state.
      Boundary class shoud interpret such exception as fatal error but
      EUserException as state errors.
      This implementation performs all operations in the thread of caller.
      It should store all the data necessary for operations including selected
      intervals because client can be unable to store data.
    }
    TFitService = class(TInterfacedObject, IClientCallback, IFitService)
    protected
        FCurveTypeSelector: ICurveTypeSelector;
{$IFDEF FIT}
        FFitProxy:   TFitServerProxy;
{$ENDIF}
        FWaveLength: double; // TODO: remove this.
        procedure SetWaveLength(AWaveLength: double);

    protected
        FState:      TFitServerState;
        { The state that preceded the transition to AsyncOperation. }
        FSavedState: TFitServerState;
        { State changing. The state should not change within additional thread, because the change destroys it. }
        procedure SetState(AState: TFitServerState); virtual;

    protected
        FBackFactor:    double;
        { Data of full experimental profile. }
        FExpProfile:    TTitlePointsSet;
        { The curve obtained by the sum of all curves of adjustable intervals
          to calculate the total R-factor along the entire profile. }
        FCalcProfile:   TTitlePointsSet;
        { The curve obtained by calculating difference
          between experimental and calculated profiles. }
        FDeltaProfile:  TTitlePointsSet;
        { Part of the whole profile with which user works at the given moment. }
        FSelectedArea:  TTitlePointsSet;
        { List of background points used in transition between manual and
          automatic modes of selection. }
        FBackgroundPoints: TTitlePointsSet;
        { Pairs of points defining intervals of R-factor calculation.
          Should always be displayed allowing user to see where R-factor
          is calculated. }
        FRFactorBounds: TTitlePointsSet;

        { Contains all curves collected from tasks (for example,
          for separate intervals). Items are added synchronously to the list. }
        FCurvesList:     TSelfCopiedCompList;
        { Positions of curves. Only X-coordinates are used. }
        FCurvePositions: TTitlePointsSet;
        { Containers of parameters of curves.
          TODO: change type and remove SetWaveLength. }
        FCurveList:      TMSCRCurveList;

        { Dependent on this flag either data of the selected interval are used
          or data of the whole profile. }
        FSelectedAreaMode: boolean;
        { List of subtasks for fitting parts of profile on intervals.
          By default is active. }
        FTaskList:   TComponentList;
        { Parameters of user defined curve. The object is created by server.
          It is necessary to provide parameter editing on the client-side. }
        FParams:     Curve_parameters;
        { The expression for user defined curve. }
        FCurveExpr:  string;
        { Allows to retrieve the value from the client-side. }
        FMaxRFactor: double;

        FCurveThresh: double;
        FBackgroundVariationEnabled: boolean;
        FCurveScalingEnabled: boolean;

        { Is set up to True after finishing first cycle of calculation. }
        FFitDone:   boolean;
        { Current total value of R-factor for all subtasks. }
        FCurrentMinimum: double;
        { The starting time of continuous operation. }
        FStartTime: TDateTime;
        { Adds new point to the given point set. Second call with the same coordinates
          removes point from the list. At this the list object is replaced by new one. }
        procedure AddPoint(var Points: TTitlePointsSet; XValue, YValue: double);

    protected
        { Indicates that specimen positions were assigned automatically.
          That is at each point different from background. }
        FCurvePositionsAssignedAutomatically: boolean;
        FDoneDisabled: boolean;

        { These methods are executed in the separate thread. }

        procedure DoneProc; virtual;

        { Methods used by optimization algorithm to update
          information in achieving of new minimum.
          Calls IClientCallback's ShowCurMin method. }
        procedure ShowCurMinInternal; virtual;

        { IClientCallback }

        { Updates profile data after background subtraction. }
        procedure ShowProfile; virtual;
        { Regenerates resulting list of curves, recalculates
          resulting profile and updates current minimum value. }
        procedure ShowCurMin(Min: double); virtual;
        { TODO: implement. }
        procedure Done; virtual;
        { TODO: implement. }
        procedure ComputeCurveBoundsDone; virtual;
        { TODO: implement. }
        procedure ComputeBackgroundPointsDone; virtual;
        { TODO: implement. }
        procedure ComputeCurvePositionsDone; virtual;

        { The algorithm methods. They are executed asynchronously. }

        { Calculates boundaries of R-factor intervals based on data obtained
          from ComputeCurvePositionsActual. }
        procedure ComputeCurveBoundsAlg;
        procedure ComputeCurveBoundsDoneProcActual;
        { Calculates reference points for linear cut up the background.
          The points aren't sorted by X. }
        function ComputeBackgroundPointsActual(Data: TPointsSet): TPointsSet;
        { Calculates background points. }
        procedure ComputeBackgroundPointsAlg;
        procedure ComputeBackgroundPointsDoneProcActual;
        { Calculates peak positions which will be taken as specimen positions. }
        procedure ComputeCurvePositionsAlg;
        { Selects all points as specimen positions. }
        procedure SelectAllPointsAsCurvePositionsAlg;
        procedure ComputeCurvePositionsDoneProcActual;

        { Wrappers for corresponding methods of TFitTask. }

        procedure MinimizeNumberOfCurvesAlg; virtual;
        procedure MinimizeDifferenceAlg; virtual;
        procedure MinimizeDifferenceAgainAlg; virtual;
        procedure DoAllAutomaticallyAlg;

        { Low-level methods used by algorithms. }

        procedure SmoothProfileActual(ANeutronPointsSet: TPointsSet);
        { Linearly subtracts background at the given interval of points. }
        procedure SubtractBackgroundLinearly(Data: TPointsSet;
            StartIndex: longint; EndIndex: longint);

        { Integrates specimen curve and adds resulting value to the list of results. }
        procedure AddCurveToList(Points: TCurvePointsSet;
        { Indexes of start and end points defining boundaries of the peak. }
            StartPointIndex, StopPointIndex: longint);
        { Searches for peak points and return them. }
        function ComputeCurvePositionsActual(SearchMinimums: boolean): TTitlePointsSet;
        { Fills the list of peak positions for automatic fit. }
        procedure ComputeCurvePositionsForAutoAlg;
        function IntegrateWithBoundaries(Points: TPointsSet;
            StartPointIndex, StopPointIndex: longint): double;
        function Integrate(Points: TPointsSet): double;
        { Calculates the R-factor for FCalcProfile and SelectProfileInterval by sum for all tasks. }
        function GetTotalRFactor: double;
        function GetTotalAbsRFactor: double;
        function GetTotalSqrRFactor: double;
        { Copies data from given list to the list of selected interval. }
        procedure SelectProfileIntervalActual(Points: TPointsSet;
            StartPointIndex, StopPointIndex: longint);
        function CreateTaskObject: TFitTask; virtual;
        { Creates subtasks for selected intervals. If the intervals were not selected generates them automatically. }
        procedure CreateTasks;
        procedure InitTasks;

        { Auxiliary methods. }

        procedure CreateResultedProfile;
        { Calculates profile containing differences between calculated and experimental data.
          In the calculation all curves are included. Will not work properly if curves are overlapped. }
        procedure CreateDeltaProfile;
        procedure CreateResultedCurvesList;
        { Collects resulting set of curve positions. Points should not be collected from subtasks because
          in this case part of points can be missed. This can confise the user. }
        // procedure CreateResultedCurvePositions;
        { Iterates through list of curves and creates common list of parameters
          of all curves complementing them with calculated parameters. }
        procedure CreateCurveListAlg;
        { Prepares intermediate results for user. }
        procedure GoToReadyForFit;

        { Checks expression and fills list of parameters. }
{$IFDEF _WINDOWS}
        procedure CreateParameters(ACurveExpr: string);
{$ENDIF}
        function GetAllInitialized: boolean;
        { Does not really create any thread. Simply calls methods synchronously. }
        procedure RecreateMainCalcThread(ACurrentTask: TThreadMethod;
            ADoneProc: TThreadMethod); virtual;

    public
        constructor Create;
        destructor Destroy; override;

        { Interface methods changing state shoud notify about it. }

        { Set experimental profile data. }
        function SetProfilePointsSet(APointsSet: TTitlePointsSet): string;

        { Methods return copies of objects, they should be free by caller. }

        function GetCalcProfilePointsSet: TTitlePointsSet;
        function GetDeltaProfilePointsSet: TTitlePointsSet;
        { Get experimental profile data. }
        function GetProfilePointsSet: TTitlePointsSet;
        { Get data for the selected interval. }
        function GetSelectedProfileInterval: TTitlePointsSet;

        function SetBackgroundPointsSet(ABackgroundPoints:
            TTitlePointsSet): string;
        function GetBackgroundPoints: TTitlePointsSet;

        function SetCurvePositions(ACurvePositions: TPointsSet): string;
        function GetCurvePositions: TTitlePointsSet;

        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet;
{$IFDEF _WINDOWS}
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Equality to Nil means initialization. }
            CP: Curve_parameters);
        function GetSpecialCurveParameters: Curve_parameters;
{$ENDIF}
        { The server should support primitives for adding and updating points
          to support thin clients which can not store all set of data. }
        { All methods call AddPoint. }

        procedure AddPointToProfile(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        { All methods call ReplacePoint. }

        procedure ReplacePointInProfile(PrevXValue, PrevYValue, NewXValue,
            NewYValue: double);
        procedure ReplacePointInBackground(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);

        { Returns list of parameters of all curves. }
        function GetCurveList: TMSCRCurveList;
        { Returns list of components containing sets of points. }
        function GetCurvesList: TSelfCopiedCompList;

        { These methods check validity of server state and
          throw EUserException in the case when state is invalid. }

        function GetCurveCount: longint;
        function GetCurvePoints(SpecIndex: longint): TNamedPointsSet;
        function GetCurveParameterCount(SpecIndex: longint): longint;
        procedure GetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        procedure SetCurveParameter(SpecIndex: longint; ParamIndex: longint;
            Value: double);

        { Asynchronous long-term operations. }
        { Smoothes experimental data. Returns describing message.
          TODO: so far is executed synchronously. Refactor to asynchronous processing. }
        function SmoothProfile: string;
        { Linearly subtracts background in the SelectProfileInterval and recreates SelectProfileInterval.
          TODO: unify with SubtractBackground. }
        procedure SubtractBackground;
        { Subtracts the background by linear approximation. When Auto is True then
          background points selected before (no matter by which way) are dropped out.
          TODO: when it is called as interface method should return text message. }
        procedure SubtractBackground(Auto: boolean);
        { Completely automatic procedure of finding model curves. }
        function DoAllAutomatically: string; virtual;
        { Performs model fitting (initial or subsequent). Corresponds to MinimizeDifference. }
        function MinimizeDifference: string; virtual;
        { Performs model fitting without initialization of bounds. }
        function MinimizeDifferenceAgain: string; virtual;
        { Search for model describing experimental data with given accuracy
          by minimum number of specimens. Sequentially reducing the number
          of specimens. }
        function MinimizeNumberOfCurves: string; virtual;
        { Searches for intervals of application of curves. }
        function ComputeCurveBounds: string; virtual;
        { Searches for background points. }
        function ComputeBackgroundPoints: string; virtual;
        { Searches for curve positions. }
        function ComputeCurvePositions: string; virtual;
        function SelectAllPointsAsCurvePositions: string; virtual;

        { Control operations. }

        { Stops long-term operation asynchronously. Calls termination procedure. }
        procedure StopAsyncOper; virtual; abstract;
        { Stops long-term operation synchronously without calling termination procedure. }
        procedure AbortAsyncOper; virtual; abstract;
        { Returns True in asynchronous operation mode. }
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;

        { Synchronous operations. }

        { Transfers part of profile data to the list of selected interval. }
        function SelectProfileInterval(StartPointIndex, StopPointIndex: longint): string;
        function SelectEntireProfile: string;
        { Defines starting and finishing point for each curve (specimen),
          integrates it and puts parameters into resulting list. }
        procedure CreateCurveList;

        { The fields setting and getting of which are not related with sensitive
          for the actor or long-term activity are better implemented by properties. }

        { Maximum allowed value of R-factor. }
        procedure SetMaxRFactor(AMaxRFactor: double);
        function GetMaxRFactor: double;
        property MaxRFactor: double read GetMaxRFactor write SetMaxRFactor;
        { Denominator of ratio of background to maximal intensity. }
        procedure SetBackFactor(ABackFactor: double);
        function GetBackFactor: double;
        property BackFactor: double read GetBackFactor write SetBackFactor;
        { The threshold for determination of curve (specimen) boundaries. It is supposed
          that background was cut out. The curve boundaries are defined by exceeding
          the threshold by curve function. The same threshold removes instances with
          too small amplitude. }
        procedure SetCurveThresh(ACurveThresh: double);
        function GetCurveThresh: double;
        property CurveThresh: double read GetCurveThresh write SetCurveThresh;

{$IFNDEF FIT}
        { https://github.com/dvmorozov/fit/issues/160 }
        procedure SetCurveType(ACurveTypeId: TCurveTypeId);
{$ENDIF}
        function GetCurveType: TCurveTypeId;
        property CurveTypeId: TCurveTypeId read GetCurveType
{$IFNDEF FIT} write SetCurveType
{$ENDIF};

        function GetState: TFitServerState;
        property State: TFitServerState read GetState;

        function GetWaveLength: double;
        property WaveLength: double read GetWaveLength write SetWaveLength;
        property SelectedAreaMode: boolean read FSelectedAreaMode;
{$IFDEF FIT}
        { This can be equal to Nil. }
        property FitProxy: TFitServerProxy read FFitProxy write FFitProxy;
{$ENDIF}
        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);

        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);
    end;

const
    InadmissibleServerState: string = 'Inadmissible state for this operation.';
    InadmissibleData: string = 'Inadmissible data for this operation.';
    NowCalculation: string =
        // 'Now calculation is performing.';
        'Now Calculation is executing.';
    RFactorStillNotCalculated: string = 'Not calculated';
    CalcAborted: string = 'Calculation aborted.';
    IsProfileWaiting: string = 'Now the program waits data.';
    // dannaya stroka dolzhna poyavlyat'sya kogda vse neobhodimye dlya rascheta
    // dannye byli zadany pol'zovatelem
    IsReadyForFit: string =
        'Now the program is ready for fitting with selected conditions.';
    // dannaya stroka dolzhna poyavlyat'sya kogda nekotorye dannye esche ne
    // zadany pol'zovatelem i v sluchae zapuska podgonki budut vybrany programmoy
    IsReadyForAutoFit: string =
        'Now the program is ready for fitting with automatically selected conditions.';
    BackRemoving: string = 'Now background points should be defined.';
    DataMustBeSet: string = 'Define data before.';
    BackPointsNum: string =
        'The number of background points should not be less then 2.';
    RangeAlready: string = 'Range of data already selected.';
    EntireAlready: string = 'Entire dataset already selected.';
    NotAllData: string =
        'Not all necessary data has been defined for the calculation.';
    // Tekst d.b. primerno takoy: Ne vse neobhodimye dlya rascheta dannye opredeleny.
    // Vy dolzhny zadat' samostoyatel'no, ili pozvolit' programme sgenerirovat'
    // sleduyuschie dannye: intervaly primeneniya patternov, tochki privyazki i
    // nachal'nye znacheniya parametrov patternov.
    StillNotDone: string = 'The calculation still not accomplished.';
    // Raschet ne byl zapuschen
    CalcNotStarted: string = 'The calculation not started.';
    CurveListNotReady: string = 'Curve list not ready.';
    CurveParameterListNotReady: string = 'Curve parameter list not ready.';
    InadmissibleCurveIndex: string = 'Inadmissible specimen index.';
    InadmissibleParameterIndex: string = 'Inadmissible parameter index.';
    CRLF: string = #13#10;

implementation

uses
    app;

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1E100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1E100;

{$IFDEF _WINDOWS}
function ParseAndCalcExpression(Expr: LPCSTR; ParamList: LPCSTR;
    Result: PDouble): longint; cdecl;
    external 'MathExpr' Name 'ParseAndCalcExpression';
function GetSymbols: LPCSTR; cdecl; external 'MathExpr' Name 'GetSymbols';
procedure FreeSymbols(Symbols: LPCSTR); cdecl;
    external 'MathExpr' Name 'FreeSymbols';
{$ENDIF}

{ ================================= TFitService ================================= }
function TFitService.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    try
        Assert(Assigned(APointsSet));

        { TODO: Copying points is required. }
        FExpProfile.Free;
        FExpProfile := TTitlePointsSet(APointsSet.GetCopy);
        if FExpProfile.PointsCount = 0 then
            raise EUserException.Create(InadmissibleData);

        SetState(BackNotRemoved);
        if Result = '' then
            Result := BackRemoving
        else
            Result := Result + ' ' + BackRemoving;
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

function TFitService.SetBackgroundPointsSet(ABackgroundPoints:
    TTitlePointsSet): string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    try
        Assert(Assigned(ABackgroundPoints)); // kriticheskaya oshibka
        Assert(Assigned(FExpProfile));

        FBackgroundPoints.Free;
        FBackgroundPoints := ABackgroundPoints;

        if FExpProfile.PointsCount > 2 then
        begin
            SetState(BackNotRemoved);
            if Result = '' then
                Result := BackRemoving
            else
                Result := Result + ' ' + BackRemoving;
        end
        else
        begin
            SetState(ProfileWaiting);
            if Result = '' then
                Result := IsProfileWaiting
            else
                Result := Result + ' ' + IsProfileWaiting;
        end;
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

function TFitService.SetCurvePositions(ACurvePositions: TPointsSet): string;
var
    i:   longint;
    Msg: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    try
        Assert(Assigned(ACurvePositions));
        Assert(Assigned(FCurveList));

        FCurvePositions.Clear;
        FCurveList.Clear;

        for i := 0 to ACurvePositions.PointsCount - 1 do
            AddPoint(FCurvePositions, ACurvePositions.PointXCoord[i],
                ACurvePositions.PointYCoord[i]);
        ACurvePositions.Free;

        if FExpProfile.PointsCount > 2 then
            GoToReadyForFit
        else
            SetState(ProfileWaiting);

        Msg := '';
        if State = ProfileWaiting then
            Msg := IsProfileWaiting
        else if State = ReadyForFit then
            Msg := IsReadyForFit
        else if State = ReadyForAutoFit then
            Msg := IsReadyForAutoFit;

        if Result = '' then
            Result := Msg
        else
            Result := Result + ' ' + Msg;
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

function TFitService.SetRFactorBounds(ARFactorBounds: TPointsSet): string;
var
    i:   longint;
    Msg: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    try
        Assert(Assigned(ARFactorBounds));
        Assert(Assigned(FCurveList));

        FRFactorBounds.Clear;
        FCurveList.Clear;

        for i := 0 to ARFactorBounds.PointsCount - 1 do
            AddPoint(FRFactorBounds, ARFactorBounds.PointXCoord[i],
                ARFactorBounds.PointYCoord[i]);
        ARFactorBounds.Free;

        if FExpProfile.PointsCount > 2 then
            GoToReadyForFit
        else
            SetState(ProfileWaiting);

        Msg := '';
        if State = ProfileWaiting then
            Msg := IsProfileWaiting
        else if State = ReadyForFit then
            Msg := IsReadyForFit
        else if State = ReadyForAutoFit then
            Msg := IsReadyForAutoFit;

        if Result = '' then
            Result := Msg
        else
            Result := Result + ' ' + Msg;
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

destructor TFitService.Destroy;
begin
    SetState(ProfileWaiting);

    FBackgroundPoints.Free;
    FRFactorBounds.Free;
    FCurvePositions.Free;
    FExpProfile.Free;
    FParams.Free;
    inherited;
end;

constructor TFitService.Create;
begin
    inherited;

    FParams := Curve_parameters.Create(nil);

    FMaxRFactor  := 0.0001; // 0.01%
    FBackFactor  := 30;
    FCurveThresh := 0;
    // Sets default curve type.
    FCurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;

    FBackgroundVariationEnabled := False;
    FCurveScalingEnabled := True;

    // chtoby mozhno bylo dobavlyat' tochki tablichno bez vhoda
    // v spets. rezhim
    FExpProfile     := TTitlePointsSet.Create(nil);
    FBackgroundPoints := TTitlePointsSet.Create(nil);
    FRFactorBounds  := TTitlePointsSet.Create(nil);
    // elementy v eti spiski d. dobavlyat'sya sinhronno
    FCurvePositions := TTitlePointsSet.Create(nil);
    FCurveList      := TMSCRCurveList.Create;
    FCurveList.FWaveLength := WaveLength;
    FCurvesList     := TSelfCopiedCompList.Create;

    SetState(ProfileWaiting);
end;

procedure TFitService.SubtractBackground(Auto: boolean);
var
    Data, Background: TPointsSet;
    i, StartIndex, EndIndex: longint;
    SavedI: double;
begin
    if State = AsyncOperation then
        // kak vnutrenniy metod v sluchae nedop. sost. vybras. iskl.
        Assert(FSavedState = BackNotRemoved)
    else
    // kak interfeysnyy vybrasyvaet spets. isklyuchenie
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState +
            CRLF + DataMustBeSet)// kriticheskaya oshibka
    // povtornoe udalenie fona zapreschat' ni k chemu
    // if (State <> BackNotRemoved) then
    // raise EUserException.Create(InadmissibleServerState);
    ;

    Assert(Assigned(FBackgroundPoints));
    (* dlya podderzhki vyzova cherez web-interfeys - sm.nizhe
      if not Auto then
      begin
      //  kriticheskaya oshibka
      //  opyat'-taki kak interfeysnyy...
      if not FBackgroundPoints.PointsCount >= 2 then
      raise EUserException.Create(InadmissibleData + CRLF +
      BackPointsNum);
      end;
    *)

    try
        if FSelectedAreaMode then
            Data := FSelectedArea
        else
            Data := FExpProfile;
        Assert(Assigned(Data));
        Data.Sort;

        if not Auto then
        begin
            // dlya podderzhki vyzova cherez web-interfeys
            if FBackgroundPoints.PointsCount < 2 then
                Background := ComputeBackgroundPointsActual(Data)
            else
                Background := FBackgroundPoints;
        end
        else
            Background := ComputeBackgroundPointsActual(Data);
        try
            // zaschischaet resursy Background ot poteri
            Assert(Assigned(Background));
            Background.Sort;

            StartIndex := Data.IndexOfValueX(Background.PointXCoord[0]);
            Assert(StartIndex <> -1);

            for i := 1 to Background.PointsCount - 1 do
            begin
                EndIndex := Data.IndexOfValueX(Background.PointXCoord[i]);
                Assert(EndIndex <> -1);
                // poskol'ku SubstractLinearly vychitaet fon dlya
                // vseh tochek vklyuchaya granichnye, to trebuetsya
                // sohranenie intensivnosti posledney tochki dlya
                // togo, chtoby na sleduyuschem otrezke vychitanie
                // bylo sdelano korrektno; pri posleduyuschem
                // primenenii SubtractBackgroundLinearly v etoy tochke
                // nichego krome nulya poluchit'sya ne mozhet
                SavedI := Data.PointYCoord[EndIndex];
                SubtractBackgroundLinearly(Data, StartIndex, EndIndex);
                StartIndex := EndIndex;
                Data.PointYCoord[EndIndex] := SavedI;
            end;
            // otmenyaet vosstanovlenie intensivnosti
            // dlya posledney tochki poslednego otrezka
            Data.PointYCoord[EndIndex] := 0;
        except
            if Background <> FBackgroundPoints then
                Background.Free;
            FBackgroundPoints.Clear;
            raise;
        end;
        if Background <> FBackgroundPoints then
            Background.Free;
        // !!! nuzhno ochischat', chtoby tochki fona ne povisali v vozduhe !!!
        FBackgroundPoints.Clear;
        // nel'zya menyat' sostoyanie vnutri dochernego potoka -
        // eto privodit k ego unichtozheniyu
        if State <> AsyncOperation then
            SetState(ReadyForAutoFit);
    except
        // nel'zya menyat' sostoyanie vnutri dochernego potoka -
        // eto privodit k ego unichtozheniyu
        if State <> AsyncOperation then
            SetState(ProfileWaiting);
        raise;
    end;
end;

   // !!! algoritm osnovan na tom, chto fon dlya neytronogramm imeet vognutyy profil',
   // poetomu na kazhdom shage ischetsya lokal'nyy minimum v storony ot global'nogo;
   // dlya drugih tipov fona dannyy algoritm rabotat' ne budet !!!;
   // !!! dolzhna vozvraschat' korrektnyy ukazatel' ili vybrasyvat' isklyuchenie !!!
function TFitService.ComputeBackgroundPointsActual(Data: TPointsSet): TPointsSet;
var
    Min, CurMin: double;
    LeftMin, RightMin: double;
    MinIndex: longint;
    LeftIndex, RightIndex: longint;
    i:    longint;
    Flag: boolean;
begin
    Assert(Assigned(Data));
    Assert(Data is TPointsSet);
    // sozdanie spiska tochek fona
    Result := TPointsSet.Create(nil);
    try
        // vybiraem tochku s min. int. dlya opredeleniya
        Min      := Data.PointYCoord[0];
        MinIndex := 0;
        for i := 1 to Data.PointsCount - 1 do
            if Data.PointYCoord[i] < Min then
            begin
                Min      := Data.PointYCoord[i];
                MinIndex := i;
            end;

        LeftIndex  := MinIndex;
        RightIndex := MinIndex;
        LeftMin    := Min;
        RightMin   := Min;

        Result.AddNewPoint(Data.PointXCoord[MinIndex], Min);

        Flag := True;
        while Flag do
        begin
            // nahodim sleduyuschuyu po amplitude tochku
            Flag     := False;
            // ischem sleva
            CurMin   := Data.PointYCoord[0];
            MinIndex := 0;
            for i := 1 to LeftIndex - 1 do
                if (Data.PointYCoord[i] < CurMin) and
                    (Data.PointYCoord[i] >= LeftMin) then
                begin
                    CurMin   := Data.PointYCoord[i];
                    MinIndex := i;
                end;

            if MinIndex < LeftIndex then
            begin
                LeftIndex := MinIndex;
                LeftMin   := CurMin;
                Flag      := True;
                Result.AddNewPoint(Data.PointXCoord[MinIndex], CurMin);
            end;

            // ischem sprava
            if RightIndex + 1 <= Data.PointsCount - 1 then
            begin
                CurMin   := Data.PointYCoord[RightIndex + 1];
                MinIndex := RightIndex + 1;
                for i := RightIndex + 2 to Data.PointsCount - 1 do
                    if (Data.PointYCoord[i] < CurMin) and
                        (Data.PointYCoord[i] >= RightMin) then
                    begin
                        CurMin   := Data.PointYCoord[i];
                        MinIndex := i;
                    end;
            end;

            if MinIndex > RightIndex then
            begin
                RightIndex := MinIndex;
                RightMin := CurMin;
                Flag := True;
                Result.AddNewPoint(Data.PointXCoord[MinIndex], CurMin);
            end;
        end;
    except
        Result.Free;
        raise;
    end;
end;

function TFitService.ComputeCurvePositionsActual(SearchMinimums: boolean): TTitlePointsSet;
var
    Data: TPointsSet;
    ExtremumValue: double;
    ExtremumX0: double;
    ExtremumIndex: longint;
    ExtremumFound: boolean;
    LeftIndex, RightIndex: longint;
    // LeftIndex2, RightIndex2: LongInt;
    LeftX0, RightX0, Temp: double;
    i:    longint;
    GlobalExtremum: double;

    function GetBoundaryValue: double;
    begin
        if SearchMinimums then
            Result := MAX_VALUE
        else
            Result := MIN_VALUE;
    end;

    function IsFirstBetter(Value1, Value2: double): boolean;
    begin
        if SearchMinimums then
            Result := Value1 < Value2
        else
            Result := Value1 > Value2;
    end;

begin
    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;

    SearchMinimums := True;

    Assert(Assigned(Data));
    Data.Sort;

    Result := TTitlePointsSet.Create(nil);
    try
        // tsiklicheski vybiraetsya minimal'naya tochka sredi teh,
        // prinadlezhnost' kotoryh k pikam esche ne opredelena
        repeat
            ExtremumValue := GetBoundaryValue;
            ExtremumFound := False;

            for i := 0 to Data.PointsCount - 1 do
                if IsFirstBetter(Data.PointYCoord[i], ExtremumValue) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    ExtremumValue := Data.PointYCoord[i];
                    ExtremumX0    := Data.PointXCoord[i];
                    ExtremumIndex := i;
                    ExtremumFound := True;
                end;
            // predotvraschaet zatsiklivanie v sluchae, kogda vse tochki
            // profilya vybrany (takoe mozhet byt', naprimer, pri slishkom
            // maloy velichine nizhney granitsy poiska pikov)
            // !!! vyhod d.b. do dobavleniya tochki !!!
            if not ExtremumFound then
                Break;
            // pervyy naydennyy maksimum yavlyaetsya global'nym
            if Result.PointsCount = 0 then
                GlobalExtremum := ExtremumValue;
            Result.AddNewPoint(ExtremumX0, Data.PointYCoord[ExtremumIndex]);

            // opredelyaem granitsy pika dlya vychisleniya faktora rashodimosti
            Temp      := ExtremumValue;
            LeftIndex := ExtremumIndex;
            LeftX0    := ExtremumX0;
            // !!! trebuetsya zaschita ot dubley inache budet sboy sortirovki !!!
            for i := ExtremumIndex - 1 downto 0 do
                if not IsFirstBetter(Data.PointYCoord[i], Temp) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    Temp      := Data.PointYCoord[i];
                    LeftIndex := i;
                    LeftX0    := Data.PointXCoord[i];
                    Result.AddNewPoint(LeftX0, Data.PointYCoord[LeftIndex]);
                end
                else
                    Break// !!! dlya sravneniya d. ispol'zovat'sya <,
            // tak kak inache piki mogut smykat'sya !!!
            ;
            // iskusstvennoe ushirenie pika vlevo
            (*
              if LeftIndex < 10 then LeftIndex2 := 0
              else LeftIndex2 := LeftIndex - 10;
              for i := LeftIndex - 1 downto LeftIndex2 do
              begin
              if (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
              begin
              LeftIndex := i;
              LeftX0 := Data.PointXCoord[i];
              Result.AddNewPoint(LeftX0, Data.PointYCoord[LeftIndex]);
              end
              end;
            *)
            Temp    := ExtremumValue;
            RightIndex := ExtremumIndex;
            RightX0 := ExtremumX0;
            for i := ExtremumIndex + 1 to Data.PointsCount - 1 do
                if not IsFirstBetter(Data.PointYCoord[i], Temp) and
                    (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
                begin
                    Temp    := Data.PointYCoord[i];
                    RightIndex := i;
                    RightX0 := Data.PointXCoord[i];
                    Result.AddNewPoint(RightX0, Data.PointYCoord[RightIndex]);
                end
                else
                    Break// !!! dlya sravneniya d. ispol'zovat'sya <,
            // tak kak inache piki mogut smykat'sya !!!
            ;
            // iskusstvennoe ushirenie pika vpravo
            (*
              if RightIndex + 10 > Data.PointsCount - 1 then
              RightIndex2 := Data.PointsCount - 1
              else RightIndex2 := RightIndex + 10;
              for i := RightIndex + 1 to RightIndex2 do
              begin
              if (Result.IndexOfValueX(Data.PointXCoord[i]) = -1) then
              begin
              RightIndex := i;
              RightX0 := Data.PointXCoord[i];
              Result.AddNewPoint(RightX0, Data.PointYCoord[RightIndex]);
              end
              else Break;
              end;
            *)

            // ??? nuzhno otsenivat' velichinu shuma v fone i ustanavlivat' dolyu ot
            // maksimuma kot. byla by bol'she shuma, krome togo nuzhna vozmozhnost'
            // vvoda dannoy velichiny vruchnuyu;
            // zdes' nuzhna ne dolya maksimuma, a nekotoroe znachenie (poskol'ku
            // mozhet prisutstvovat' fon), kotoroe k tomu zhe d.b. razlichnym po
            // profilyu (poprostu fon d.b. udalen!)
        until ExtremumValue < GlobalExtremum / BackFactor;
    except
        Result.Free;
        raise;
    end;
end;

procedure TFitService.ComputeCurvePositionsForAutoAlg;
begin
    FCurvePositions.Free;
    FCurvePositions      := nil;
    // vse tochki pikov vybirayutsya v kachestve tochek privyazki krivyh
    // TODO: use special value of TExtremumMode and generalize algorithm.
    FCurvePositions      := ComputeCurvePositionsActual(False);
    FCurvePositionsAssignedAutomatically := True;
end;

procedure TFitService.ComputeCurvePositionsAlg;
var
    ExtremumMode: TExtremumMode;

    procedure SearchExtremums(Minimums: boolean);
    var
        Peaks: TPointsSet;

        procedure SelectExtremums;
        var
            i:    longint;
            Data: TPointsSet;
            PrevValue, CurValue: double;
            PeakFound: boolean;
            X:    double;
            LastPoint: boolean;

            function DerivativeChanged: boolean;
            begin
                if Minimums then
                    Result := CurValue > PrevValue
                else
                    Result := CurValue < PrevValue;
            end;

        begin
            Assert(Assigned(Peaks));
            { Peaks collecton contains all data points having values different
              from some estimated average by defined value. }
            Assert(Peaks.PointsCount >= 3);

            Peaks.Sort; // !!!

            if FSelectedAreaMode then
                Data := FSelectedArea
            else
                Data := FExpProfile;
            Assert(Assigned(Data));
            Data.Sort;
            // iz Peaks, poluchennyh posle vyzova ComputeCurvePositionsActual,
            // isklyuchayutsya vse tochki, krome lokal'nyh maksimumov vnutri
            // kazhdogo pika
            PeakFound := False;
            PrevValue := Peaks.PointYCoord[0];
            for i := 1 to Peaks.PointsCount - 1 do
            begin
                CurValue := Peaks.PointYCoord[i];
                if not PeakFound then
                begin
                    { Inflection point is searched. Last point is included
                      if data are going in the given direction. }
                    LastPoint := i = Peaks.PointsCount - 1;
                    if DerivativeChanged or (not DerivativeChanged and LastPoint)
                    then
                    begin
                        if LastPoint then
                        begin
                            X := Peaks.PointXCoord[i];
                            // zaschita ot dubley
                            if FCurvePositions.IndexOfValueX(X) = -1 then
                                FCurvePositions.AddNewPoint(X, CurValue);
                        end
                        else
                        begin
                            X := Peaks.PointXCoord[i - 1];
                            // zaschita ot dubley
                            if FCurvePositions.IndexOfValueX(X) = -1 then
                                FCurvePositions.AddNewPoint(X, PrevValue);
                        end;
                        PeakFound := True;
                    end;
                end
                else
                if not DerivativeChanged then
                    PeakFound := False// ischem peregib vniz
                ;
                PrevValue     := CurValue;
            end;
        end;

    begin
        Peaks := ComputeCurvePositionsActual(Minimums);
        try
            SelectExtremums;
        except
            Peaks.Free;
            raise;
        end;
        Peaks.Free;
    end;

begin
    Assert(Assigned(FCurvePositions));
    { Points selected at previous steps are removed. }
    FCurvePositions.Clear;

    ExtremumMode := FCurveTypeSelector.GetSelectedExtremumMode;

    case ExtremumMode of
        OnlyMaximums:
            SearchExtremums(False);
        OnlyMinimums:
            SearchExtremums(True);
        MaximumsAndMinimums:
        begin
            SearchExtremums(False);
            SearchExtremums(True);
        end;
    end;
end;

procedure TFitService.ComputeCurvePositionsDoneProcActual;
begin
    try
        // iz AsyncOperation perehodit v prezhnee sostoyanie
        SetState(FSavedState);
        FState := FSavedState; // trebuetsya pri perehode iz AsyncOperation
        // !!! d.b. zdes', a ne v ComputeCurvePositions, t.k.
        // etot metod vyzyvaetsya iz naslednika !!!
        GoToReadyForFit;
{$IFDEF FIT}
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeCurvePositionsDone;
{$ENDIF}
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
end;

procedure TFitService.SelectAllPointsAsCurvePositionsAlg;
var
    i:    longint;
    Data: TPointsSet;
begin
    Assert(Assigned(FCurvePositions));
    FCurvePositions.Clear;

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    Assert(Assigned(Data));
    Data.Sort;
    for i := 0 to Data.PointsCount - 1 do
        FCurvePositions.AddNewPoint(Data.PointXCoord[i], Data.PointYCoord[i]);
end;

procedure TFitService.ComputeCurveBoundsAlg;
var
    i:     longint;
    Data:  TPointsSet;
    First: boolean;
    Peaks: TPointsSet;
    X:     double;
begin
    // !!! spisok ne ochischaetsya, chtoby naydennye tochki dobavlyalis'
    // k tochkam vybrannym pol'zovatelem !!!
    Assert(Assigned(FRFactorBounds));
    Peaks := ComputeCurvePositionsActual(False);
    try
        Assert(Assigned(Peaks));
        // dazhe v samom uzkom pike d.b.
        // ne men'she 3-h tochek
        // !!! nuzhno obrabatyvat' vse sluchai !!!
        // Assert(Peaks.PointsCount >= 3);
        if Peaks.PointsCount = 0 then
            Exit;
        Peaks.Sort; // !!!

        if FSelectedAreaMode then
            Data := FSelectedArea
        else
            Data := FExpProfile;
        Assert(Assigned(Data));
        Data.Sort;
        // iz Peaks, poluchennyh posle vyzova ComputeCurvePositionsActual,
        // isklyuchayutsya vse tochki, krome tochek ogranichivayuschih pik
        First := False;
        for i := 0 to Data.PointsCount - 1 do
            if Peaks.IndexOfValueX(Data.PointXCoord[i]) <> -1 then
            begin
                // naydena tochka pika
                if not First then
                begin
                    // pervaya tochka pika - levaya granitsa
                    First := True;
                    // zaschita ot dubley tochek
                    X     := Data.PointXCoord[i];
                    if FRFactorBounds.IndexOfValueX(X) = -1 then
                        FRFactorBounds.AddNewPoint(X, Data.PointYCoord[i]);
                end;
                // ostal'nye tochki propuskaem...
            end
            else
            if First then
            begin
                // predyduschaya tochka - pravaya granitsa
                X := Data.PointXCoord[i - 1];
                if FRFactorBounds.IndexOfValueX(X) = -1 then
                    FRFactorBounds.AddNewPoint(X,
                        Data.PointYCoord[i - 1]);
                First := False;
            end// ne tochka pika
        ;
        if First then
        begin
            // perebrali vse tochki, no pravoy granitsy ne naschli -
            // v kachestve pravoy granitsy berem poslednyuyu tochku
            X := Data.PointXCoord[i];
            // zaschita ot dubley
            if FRFactorBounds.IndexOfValueX(X) = -1 then
                FRFactorBounds.AddNewPoint(X, Data.PointYCoord[i]);
            First := False;
        end;
    finally
        Peaks.Free;
    end;
end;

procedure TFitService.ComputeCurveBoundsDoneProcActual;
begin
    try
        // iz AsyncOperation perehodit v prezhnee sostoyanie
        SetState(FSavedState);
        FState := FSavedState; // trebuetsya pri perehode iz AsyncOperation
        // !!! d.b. zdes', a ne v ComputeCurveBounds, t.k.
        // etot metod vyzyvaetsya iz naslednika !!!
        GoToReadyForFit;
{$IFDEF FIT}
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeCurveBoundsDone;
{$ENDIF}
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
end;

procedure TFitService.ComputeBackgroundPointsAlg;
var
    Data, Background: TPointsSet;
    i: longint;
begin
    Assert(Assigned(FBackgroundPoints));

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    Assert(Assigned(Data));
    Data.Sort;

    Background := ComputeBackgroundPointsActual(Data);
    try
        Assert(Assigned(Background));
        // !!! spisok nuzhno ochischat', chtoby iskluchit'
        // dublikaty tochek; bez ochistki (chtoby sohranit'
        // vybor pol'zovatelya) nuzhno pri dobavlenii tochek
        // proveryat' na nalichie !!!
        FBackgroundPoints.Clear;
        for i := 0 to Background.PointsCount - 1 do
            FBackgroundPoints.AddNewPoint(Background.PointXCoord[i],
                Background.PointYCoord[i]);
    finally
        Background.Free;
    end;
end;

function TFitService.ComputeCurvePositions: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    RecreateMainCalcThread(ComputeCurvePositionsAlg,
        ComputeCurvePositionsDoneProcActual);
end;

function TFitService.SelectAllPointsAsCurvePositions: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    RecreateMainCalcThread(SelectAllPointsAsCurvePositionsAlg,
        ComputeCurvePositionsDoneProcActual);
end;

function TFitService.ComputeCurveBounds: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    RecreateMainCalcThread(ComputeCurveBoundsAlg, ComputeCurveBoundsDoneProcActual);
end;

function TFitService.ComputeBackgroundPoints: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    RecreateMainCalcThread(ComputeBackgroundPointsAlg,
        ComputeBackgroundPointsDoneProcActual);
end;

procedure TFitService.ComputeBackgroundPointsDoneProcActual;
begin
    try
        // iz AsyncOperation perehodit v prezhnee sostoyanie
        SetState(FSavedState);
        FState := FSavedState; // trebuetsya pri perehode iz AsyncOperation
{$IFDEF FIT}
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeBackgroundPointsDone;
{$ENDIF}
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
end;

// sglazhivanie bez smescheniya
procedure TFitService.SmoothProfileActual(ANeutronPointsSet: TPointsSet);
var
    i:      longint;
    // MaxBefore, MaxAfter: Double;
    SumBefore, SumAfter: double;
    Window: array [0 .. 1] of double;
    Intensity: double;

    function SumByWindow(NewValue: double): double;
    begin
        Result    := (Window[0] + Window[1] + NewValue) / 3;
        Window[0] := Window[1];
        Window[1] := NewValue;
    end;

begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(ANeutronPointsSet));
    with ANeutronPointsSet do
    begin
        // esli normirovat' na maksimum, to pri sohranenii maksimal'noy
        // amplitudy otnositel'naya velichina bolee nizkih pikov vozrastaet;
        // predpochtitel'nee normirovat' na summu, no pri etom maksimal'naya
        // intensivnost' umen'shaetsya, kak i dolzhno byt' (eto ponyatnee)
        // vychislyaetsya polnaya summa dlya normirovki
        SumBefore := 0;
        for i := 0 to PointsCount - 1 do
            SumBefore := SumBefore + PointYCoord[i];
        (*
          MaxBefore := PointYCoord[0];
          for i := 1 to PointsCount - 1 do
          if PointYCoord[i] > MaxBefore then
          MaxBefore := PointYCoord[i];
        *)
        // bez takoy initsializatsii nachalo zavalivaetsya
        for i := 0 to 1 do
            Window[i] := PointYCoord[0];

        for i := 1 to PointsCount do
        begin
            if i = PointsCount then
                Intensity := PointYCoord[PointsCount - 1]
            else
                Intensity := PointYCoord[i];
            PointYCoord[i - 1] := SumByWindow(Intensity);
        end;
        (*
          //  korrektnoe opredelenie maksimuma
          MaxAfter := PointYCoord[0];
          for i := 1 to PointsCount - 1 do
          if PointYCoord[i] > MaxAfter then
          MaxAfter := PointYCoord[i];
        *)
        SumAfter := 0;
        for i := 0 to PointsCount - 1 do
            SumAfter := SumAfter + PointYCoord[i];
        // normirovka
        for i := 0 to PointsCount - 1 do
            PointYCoord[i] := PointYCoord[i] * SumBefore / SumAfter;
        // MaxBefore / MaxAfter;
    end;
end;

function TFitService.SmoothProfile: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    try
        SmoothProfileActual(FExpProfile);
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

function TFitService.GetBackgroundPoints: TTitlePointsSet;
begin
    if Assigned(FBackgroundPoints) then
        Result := TTitlePointsSet(FBackgroundPoints.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetProfilePointsSet: TTitlePointsSet;
begin
    if Assigned(FExpProfile) then
        Result := TTitlePointsSet(FExpProfile.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetSelectedProfileInterval: TTitlePointsSet;
begin
    if Assigned(FSelectedArea) then
        Result := TTitlePointsSet(FSelectedArea.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCalcProfilePointsSet: TTitlePointsSet;
begin
    if Assigned(FCalcProfile) then
        Result := TTitlePointsSet(FCalcProfile.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetDeltaProfilePointsSet: TTitlePointsSet;
begin
    if Assigned(FDeltaProfile) then
        Result := TTitlePointsSet(FDeltaProfile.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetRFactorBounds: TTitlePointsSet;
begin
    if Assigned(FRFactorBounds) then
        Result := TTitlePointsSet(FRFactorBounds.GetCopy)
    else
        Result := nil;
end;

{$IFDEF _WINDOWS}

function TFitService.GetSpecialCurveParameters: Curve_parameters;
begin
    Result := FParams;
end;

{$ENDIF}

function TFitService.GetCurvePositions: TTitlePointsSet;
begin
    if Assigned(FCurvePositions) then
        Result := TTitlePointsSet(FCurvePositions.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCurvesList: TSelfCopiedCompList;
begin
    if Assigned(FCurvesList) then
        Result := TSelfCopiedCompList(FCurvesList.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCurveCount: longint;
begin
    if not Assigned(FCurvesList) then
        raise EUserException.Create(CurveListNotReady);
    Result := FCurvesList.Count;
end;

function TFitService.GetCurvePoints(SpecIndex: longint): TNamedPointsSet;
var
    Count: longint;
begin
    Count := GetCurveCount;
    if (SpecIndex < 0) or (SpecIndex >= Count) then
        raise EUserException.Create(InadmissibleCurveIndex);

    Result := TNamedPointsSet(TNamedPointsSet(
        FCurvesList.Items[SpecIndex]).GetCopy);
end;

function TFitService.GetCurveParameterCount(SpecIndex: longint): longint;
var
    SpecParamList:   TMSCRCurveList;
    CurveParameters: Curve_parameters;
begin
    SpecParamList := GetCurveList;
    if not Assigned(SpecParamList) then
        raise EUserException.Create(CurveParameterListNotReady);
    if (SpecIndex < 0) or (SpecIndex >= SpecParamList.Count) then
        raise EUserException.Create(InadmissibleCurveIndex);

    CurveParameters := Curve_parameters(SpecParamList.Items[SpecIndex]);
    Result := CurveParameters.Params.Count;
end;

procedure TFitService.GetCurveParameter(SpecIndex: longint; ParamIndex: longint;
    var Name: string; var Value: double; var Type_: longint);
var
    SpecParamList: TMSCRCurveList;
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
begin
    SpecParamList := GetCurveList;
    if not Assigned(SpecParamList) then
        raise EUserException.Create(CurveParameterListNotReady);
    if (SpecIndex < 0) or (SpecIndex >= SpecParamList.Count) then
        raise EUserException.Create(InadmissibleCurveIndex);

    CurveParameters := Curve_parameters(SpecParamList.Items[SpecIndex]);
    if (ParamIndex < 0) or (ParamIndex >= CurveParameters.Params.Count) then
        raise EUserException.Create(InadmissibleParameterIndex);

    Parameter := CurveParameters[ParamIndex];
    Name      := Parameter.Name;
    Value     := Parameter.Value;
    Type_     := longint(Parameter.Type_);
end;

procedure TFitService.SetCurveParameter(SpecIndex: longint; ParamIndex: longint;
    Value: double);
var
    SpecParamList: TMSCRCurveList;
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
begin
    SpecParamList := GetCurveList;
    if not Assigned(SpecParamList) then
        raise EUserException.Create(CurveParameterListNotReady);
    if (SpecIndex < 0) or (SpecIndex >= SpecParamList.Count) then
        raise EUserException.Create(InadmissibleCurveIndex);

    CurveParameters := Curve_parameters(SpecParamList.Items[SpecIndex]);
    if (ParamIndex < 0) or (ParamIndex >= CurveParameters.Params.Count) then
        raise EUserException.Create(InadmissibleParameterIndex);

    Parameter := CurveParameters[ParamIndex];
    Parameter.Value := Value;
    // TODO: dlya isklucheniya izbytochnogo perescheta pri
    // izmenenii srazu neskol'kih parametrov mozhno sdelat'
    // v interfeyse otdel'nuyu funktsiyu perescheta; odnako
    // eto uslozhnit interfeys i mozhet privesti k oschibkam,
    // kogda interfeys budet ispol'zovat'sya storonnimi prilozheniyami
    GoToReadyForFit;
end;

procedure TFitService.SubtractBackgroundLinearly(Data: TPointsSet;
    StartIndex: longint; EndIndex: longint);
var
    i:     longint;
    Delta: double;
    I0:    double;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(EndIndex > StartIndex);
    Assert(Assigned(Data));

    with Data do
    begin
        Delta := (PointYCoord[EndIndex] - PointYCoord[StartIndex]) /
            (PointXCoord[EndIndex] - PointXCoord[StartIndex]);
        I0    := PointYCoord[StartIndex];
        for i := StartIndex to EndIndex do
            PointYCoord[i] := PointYCoord[i] - I0 -
                (PointXCoord[i] - PointXCoord[StartIndex]) * Delta;
    end;
end;

procedure TFitService.SubtractBackground;
var
    SA: TPointsSet;
begin
    if FSelectedAreaMode then
        SA := FSelectedArea
    else
        SA := FExpProfile;
    Assert(Assigned(SA));

    SubtractBackgroundLinearly(SA, 0, SA.PointsCount - 1);
end;

procedure TFitService.SelectProfileIntervalActual(Points: TPointsSet;
    StartPointIndex, StopPointIndex: longint);
var
    i: longint;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(Points));
    Assert(Points.PointsCount <> 0);
    Assert((StartPointIndex >= 0) and (StopPointIndex < Points.PointsCount));
    Assert(Points <> FSelectedArea);

    FSelectedArea.Free;
    FSelectedArea := nil;
    FSelectedArea := TTitlePointsSet.Create(nil);
    for i := StartPointIndex to StopPointIndex do
        FSelectedArea.AddNewPoint(Points.PointXCoord[i], Points.PointYCoord[i]);
end;

function TFitService.SelectProfileInterval(StartPointIndex, StopPointIndex: longint): string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    // if State = AsyncOperation then
    // raise EUserException.Create(InadmissibleServerState + CRLF +
    // NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);
    if FSelectedAreaMode then
    begin
        Result := RangeAlready;
        Exit;
    end;

    Assert(Assigned(FExpProfile));
    SelectProfileIntervalActual(FExpProfile, StartPointIndex, StopPointIndex);
    FSelectedAreaMode := True;
end;

function TFitService.SelectEntireProfile: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    // if State = AsyncOperation then
    // raise EUserException.Create(InadmissibleServerState + CRLF +
    // NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);
    if not FSelectedAreaMode then
    begin
        Result := EntireAlready;
        Exit;
    end;

    Assert(Assigned(FExpProfile));
    FSelectedAreaMode := False;
    FSelectedArea.Free;
    FSelectedArea := nil;
end;

function TFitService.IntegrateWithBoundaries(Points: TPointsSet;
    StartPointIndex, StopPointIndex: longint): double;
var
    i: longint;
    TempDouble: double;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(Points));
    Assert((StartPointIndex >= 0) and (StopPointIndex < Points.PointsCount));

    TempDouble := 0;
    with Points do
        for i := StartPointIndex to StopPointIndex do
            TempDouble := TempDouble + PointYCoord[i];
    Result := TempDouble;
end;

procedure TFitService.AddCurveToList(Points: TCurvePointsSet;
    StartPointIndex, StopPointIndex: longint);
var
    CurveParameters: Curve_parameters;
    Integral: double;

    procedure AddNewParameter(Name: string; Value: double);
    var
        Parameter: TCalculatedCurveParameter;
        Container: TPersistentCurveParameterContainer;
    begin
        try
            Parameter      := TCalculatedCurveParameter.Create;
            Parameter.Name := Name;
            Parameter.Value := Value;

            Container := TPersistentCurveParameterContainer(
                CurveParameters.Params.Add);
            try
                Container.Parameter := Parameter;
            except
                CurveParameters.Params.Delete(Container.ID);
                Container.Free;
            end;

        except
            Parameter.Free;
            raise;
        end;
    end;

begin
    Assert(Assigned(Points));
    Assert(Assigned(FCurveList));
    Integral := IntegrateWithBoundaries(Points, StartPointIndex, StopPointIndex);

    CurveParameters := Curve_parameters(Points.Parameters.GetCopy);
    try
        CurveParameters.FSavedInitHash := Points.FInitHash;
        // dobavlyayutya vychislyaemye parametry
        AddNewParameter(StartPosName, Points.PointXCoord[StartPointIndex]);
        AddNewParameter(FinishPosName, Points.PointXCoord[StopPointIndex]);
        AddNewParameter('Integral', Integral);

        FCurveList.Add(CurveParameters);

    except
        CurveParameters.Free;
        raise;
    end;
end;

function TFitService.Integrate(Points: TPointsSet): double;
var
    i: longint;
    TempDouble: double;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(Points));

    TempDouble := 0;
    with Points do
        for i := 0 to PointsCount - 1 do
            TempDouble := TempDouble + PointYCoord[i];
    Result := TempDouble;
end;

function TFitService.GetTotalRFactor: double;
var
    i:  longint;
    FT: TFitTask;
begin
    Result := 0;
    Assert(Assigned(FTaskList));
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT     := TFitTask(FTaskList.Items[i]);
        Result := Result + FT.GetCurMin;
    end;
end;

function TFitService.GetTotalAbsRFactor: double;
var
    i:  longint;
    FT: TFitTask;
begin
    Result := 0;
    Assert(Assigned(FTaskList));
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT     := TFitTask(FTaskList.Items[i]);
        Result := Result + FT.GetCurAbsMin;
    end;
end;

function TFitService.GetTotalSqrRFactor: double;
var
    i:  longint;
    FT: TFitTask;
begin
    Result := 0;
    Assert(Assigned(FTaskList));
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT     := TFitTask(FTaskList.Items[i]);
        Result := Result + FT.GetCurSqrMin;
    end;
end;

procedure TFitService.DoneProc;

    function AllTasksDone: boolean;
    var
        i:  longint;
        FT: TFitTask;
    begin
        Result := True;
        for i := 0 to FTaskList.Count - 1 do
        begin
            FT := TFitTask(FTaskList.Items[i]);
            if not FT.GetAllDone then
            begin
                Result := False;
                Break;
            end;
        end;
    end;

begin
    try
        Assert(Assigned(FTaskList));

        ShowCurMinInternal;

        if AllTasksDone then
        begin
            // vyzyvatsya v osnovnom potoke servera,
            // t.e. v tom zhe potoke, chto i ServerStub,
            // poetomu mozhno ispuskat' te zhe isklyucheniya
            CreateResultedCurvesList;
            // tochki privyazki ne dolzhny sobirat'sya iz podzadach,
            // tak kak pri etom budut propadat' tochki ne voschedschie
            // v podzadachi, a eto mozhet ozadachivat' pol'zovatelya
            // CreateResultedCurvePositions;
            CreateResultedProfile;
            CreateDeltaProfile;
            CreateCurveListAlg;

            if FCurvePositionsAssignedAutomatically then
            begin
                // esli tochki privyazki byli naydeny avtomaticheski,
                // to ih pokazyvat' ne nuzhno, t.k. eto vse tochki
                // otlichnye ot fona
                FCurvePositions.Clear;
                FCurvePositionsAssignedAutomatically := False;
            end;

            FState   := FSavedState; // vossta. sost. predshestvovashee
            // vhodu v AsyncOperation
            FFitDone := True;
            SetState(Finished);
{$IFDEF FIT}
            if (not FDoneDisabled) and Assigned(FitProxy) then
                FitProxy.Done;
{$ENDIF}
        end;
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
end;

procedure TFitService.ShowCurMinInternal;
begin
    if GetAllInitialized then
    begin
        FCurrentMinimum := GetTotalRFactor;
        // vyzyvaetsya v osnovnom potoke servera,
        // t.e. v tom zhe potoke, chto i ServerStub,
        // poetomu mozhno ispuskat' te zhe isklyucheniya
        ShowCurMin(FCurrentMinimum);
    end;
end;

procedure TFitService.ShowProfile;
begin
{$IFDEF FIT}
    if Assigned(FitProxy) then
        FitProxy.ShowProfile;
{$ENDIF}
end;

{$HINTS off}

procedure TFitService.ShowCurMin(Min: double);
begin
{$IFDEF FIT}
    if Assigned(FitProxy) then
    begin
        { These calls are necessary for animation mode. }
        CreateResultedProfile;
        CreateResultedCurvesList;
        FitProxy.ShowCurMin(FCurrentMinimum);
    end;
{$ENDIF}
end;

{$HINTS on}

procedure TFitService.Done;
begin

end;

procedure TFitService.ComputeCurveBoundsDone;
begin

end;

procedure TFitService.ComputeBackgroundPointsDone;
begin

end;

procedure TFitService.ComputeCurvePositionsDone;
begin

end;

function TFitService.GetAllInitialized: boolean;
var
    i:  longint;
    FT: TFitTask;
begin
    // !!! ne dolzhen ispuskat' isklucheniya, tak kak
    // vyzyvaetsya iz interfeysnyh metodov !!!
    // Assert(Assigned(FTaskList));
    if Assigned(FTaskList) then
    begin
        Result := True;
        for i := 0 to FTaskList.Count - 1 do
        begin
            FT := TFitTask(FTaskList.Items[i]);
            if not FT.GetCurMinInitialized then
            begin
                Result := False;
                Break;
            end;
        end;
    end
    else
        Result := False;
end;

procedure TFitService.MinimizeNumberOfCurvesAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeNumberOfCurves;
    end;
end;

procedure TFitService.MinimizeDifferenceAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeDifference;
    end;
end;

procedure TFitService.MinimizeDifferenceAgainAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    Assert(Assigned(FTaskList));
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeDifferenceAgain;
    end;
end;

function TFitService.MinimizeNumberOfCurves: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    // if State <> ReadyForFit then
    // raise EUserException.Create(
    // InadmissibleServerState + CRLF + NotAllData);
    // vmesto oshibki - sozdanie neobhodimyh dannyh
    if FRFactorBounds.PointsCount < 2 then
    begin
        FRFactorBounds.Clear;
        ComputeCurveBoundsAlg;
    end;
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoAlg;
    SetState(ReadyForFit);

    RecreateMainCalcThread(MinimizeNumberOfCurvesAlg, DoneProc);
end;

function TFitService.MinimizeDifferenceAgain: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);
    if State <> ReadyForFit then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NotAllData);

    RecreateMainCalcThread(MinimizeDifferenceAgainAlg, DoneProc);
end;

function TFitService.MinimizeDifference: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    // if State <> ReadyForFit then
    // raise EUserException.Create(
    // InadmissibleServerState + CRLF + NotAllData);
    // vmesto oshibki - sozdanie neobhodimyh dannyh
    if FRFactorBounds.PointsCount < 2 then
    begin
        FRFactorBounds.Clear;
        ComputeCurveBoundsAlg;
    end;
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoAlg;
    SetState(ReadyForFit);

    RecreateMainCalcThread(MinimizeDifferenceAlg, DoneProc);
end;

function TFitService.GetCurveList: TMSCRCurveList;
begin
    // vozvraschaem chto est' bez proverki
    Result := TMSCRCurveList(FCurveList.GetCopy);
end;

procedure TFitService.CreateResultedProfile;
var
    i, j:      longint;
    FitTask:   TFitTask;
    PointsSet: TPointsSet;
    ScalingFactor: double;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FTaskList));
    Assert(Assigned(FExpProfile));
    FCalcProfile.Free;
    FCalcProfile := nil;
    FCalcProfile := TTitlePointsSet.Create(nil);
    // ustanavlivaetsya trebuemoe kol-vo tochek
    for i := 0 to FExpProfile.PointsCount - 1 do
        FCalcProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask   := TFitTask(FTaskList.Items[i]);
        PointsSet := FitTask.GetCalcProfile;

        Assert(Assigned(PointsSet));
        Assert((FitTask.EndIndex - FitTask.BegIndex + 1) =
            PointsSet.PointsCount);
        Assert((FitTask.BegIndex >= 0) and (FitTask.BegIndex <
            FCalcProfile.PointsCount));
        Assert((FitTask.EndIndex >= 0) and (FitTask.EndIndex <
            FCalcProfile.PointsCount));

        ScalingFactor := FitTask.GetScalingFactor;
        for j := FitTask.BegIndex to FitTask.EndIndex do
            FCalcProfile.PointYCoord[j] :=
                FCalcProfile.PointYCoord[j] + PointsSet.PointYCoord[j -
                FitTask.BegIndex] * ScalingFactor;
    end;
end;

   // tochki privyazki ne dolzhny sobirat'sya iz podzadach,
   // tak kak pri etom budut propadat' tochki ne voschedschie
   // v podzadachi, a eto mozhet ozadachivat' pol'zovatelya
(*
  procedure TFitService.CreateResultedCurvePositions;
  var i, j: LongInt;
  FT: TFitTask;
  PS: TPointsSet;
  begin
  //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
  Assert(Assigned(TaskList));
  Assert(Assigned(CurvePositions));

  CurvePositions.Clear;

  for i := 0 to TaskList.Count - 1 do
  begin
  FT := TFitTask(TaskList.Items[i]);
  PS := FT.GetCurvePositions;
  Assert(Assigned(PS));
  for j := 0 to PS.PointsCount - 1 do
  CurvePositions.AddNewPoint(PS.PointXCoord[j], PS.PointYCoord[j]);
  end;
  end;
*)

procedure TFitService.CreateDeltaProfile;
var
    i, j:      longint;
    FitTask:   TFitTask;
    PointsSet: TPointsSet;
    ScalingFactor: double;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FTaskList));
    Assert(Assigned(FExpProfile));

    FDeltaProfile.Free;
    FDeltaProfile := nil;
    FDeltaProfile := TTitlePointsSet.Create(nil);
    // ustanavlivaetsya trebuemoe kol-vo tochek
    for i := 0 to FExpProfile.PointsCount - 1 do
        // !!! zapolnyaetsya nulem, potomu chto tam, gde net
        // rasschitannogo profilya ne vychislyaetsya fakt. rash.,
        // poetomu ne imeet smysla schitat' raznost' !!!
        FDeltaProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask   := TFitTask(FTaskList.Items[i]);
        PointsSet := FitTask.GetCalcProfile;

        Assert(Assigned(PointsSet));
        Assert((FitTask.EndIndex - FitTask.BegIndex + 1) =
            PointsSet.PointsCount);
        Assert((FitTask.BegIndex >= 0) and (FitTask.BegIndex <
            FCalcProfile.PointsCount));
        Assert((FitTask.EndIndex >= 0) and (FitTask.EndIndex <
            FCalcProfile.PointsCount));

        ScalingFactor := FitTask.GetScalingFactor;
        for j := FitTask.BegIndex to FitTask.EndIndex do
            FDeltaProfile.PointYCoord[j] :=
                FExpProfile.PointYCoord[j] - PointsSet.PointYCoord[j -
                FitTask.BegIndex] * ScalingFactor;
    end;
end;

procedure TFitService.CreateResultedCurvesList;
var
    i, j, k:   longint;
    FitTask:   TFitTask;
    TaskCurvesList: TSelfCopiedCompList;
    ScalingFactor: double;
    CurveCopy: TPointsSet;
begin
    Assert(Assigned(FTaskList));

    FCurvesList.Free;
    FCurvesList := nil;
    FCurvesList := TSelfCopiedCompList.Create;

    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask := TFitTask(FTaskList.Items[i]);
        ScalingFactor := FitTask.GetScalingFactor;
        TaskCurvesList := FitTask.GetCurvesList;

        Assert(Assigned(TaskCurvesList));

        for j := 0 to TaskCurvesList.Count - 1 do
        begin
            CurveCopy := TPointsSet(TPointsSet(TaskCurvesList.Items[j])
                .GetCopy);
            for k := 0 to CurveCopy.PointsCount - 1 do
                CurveCopy.PointYCoord[k] :=
                    CurveCopy.PointYCoord[k] * ScalingFactor;
            FCurvesList.Add(CurveCopy);
        end;
    end;
end;

procedure TFitService.SetState(AState: TFitServerState);
begin
    // pobochnym effektom yavlyaetsya initsializatsiya dannyh;
    // prosche vsego i nadezhney dlya kazhdogo sostoyaniya provodit'
    // initsializatsiyu sootvetstvuyuschih dannyh nezavisimo ot
    // predyduschego sostoyaniya
    case AState of
        // ozhidanie dannyh profilya posle zagruzki;
        // !!! zdes' sostoyanie servera dolzhno polnost'yu
        // privodit'sya k tomu, kotoroe bylo posle zapuska !!!
        // !!! d.b. vozmozhnost' povtornogo vhozhdeniya v eto sostoyaniya !!!
        ProfileWaiting:
        begin
            Assert(Assigned(FExpProfile));
            Assert(Assigned(FBackgroundPoints));
            Assert(Assigned(FRFactorBounds));
            Assert(Assigned(FCurvePositions));
            Assert(Assigned(FCurveList));
            // chtoby mozhno bylo dobavlyat' tochki tablichno bez vhoda
            // v spets. rezhim
            FExpProfile.Clear;
            // !!! ne dolzhen udalyat'sya pri vhode v BackNotRemoved !!!
            FBackgroundPoints.Clear;
            FRFactorBounds.Clear;
            FCurvePositions.Clear;
            FCurveList.Clear;
            FCurvesList.Clear;

            FSelectedArea.Free;
            FSelectedArea := nil;
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        // fon esche ne otsechen (profil' i/ili SelectedArea zagruzheny)
        BackNotRemoved:
        begin
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        // vypolnyaetsya dlitel'naya operatsiya
        AsyncOperation: ;
        // fon uzhe otsechen (gotovnost' k podgonke
        // krivyh v avtomaticheskom rezhime)
        ReadyForAutoFit:
        begin
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        // gotovnost' k podgonke pri zadannyh ogranicheniyah
        ReadyForFit:
        begin
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
    end;
    if FState <> AsyncOperation then
    begin
        FSavedState := FState;
        FState      := AState;
    end
    else
        FSavedState := AState; // dlya posleduyuschego vosstanovleniya
end;

function TFitService.GetState: TFitServerState;
begin
    Result := FState;
end;

procedure TFitService.SetWaveLength(AWaveLength: double);
begin
    Assert(Assigned(FCurveList));
    FWaveLength := AWaveLength;
    FCurveList.FWaveLength := WaveLength;
end;

function TFitService.GetWaveLength: double;
begin
    Result := FWaveLength;
end;

function TFitService.GetBackgroundVariationEnabled: boolean;
begin
    Result := FBackgroundVariationEnabled;
end;

procedure TFitService.SetBackgroundVariationEnabled(AEnable: boolean);
begin
    FBackgroundVariationEnabled := AEnable;
end;

function TFitService.GetCurveScalingEnabled: boolean;
begin
    Result := FCurveScalingEnabled;
end;

procedure TFitService.SetCurveScalingEnabled(AEnabled: boolean);
begin
    FCurveScalingEnabled := AEnabled;
end;

procedure TFitService.DoAllAutomaticallyAlg;
begin
    // сохраняется пользовательский выбор кривой;
    // https://action.mindjet.com/task/14588987
    // udalyaetsya vse, chto bylo vybrano pol'zovatelem
    FRFactorBounds.Clear;
    FBackgroundPoints.Clear;

    if FSavedState = BackNotRemoved then
    begin
        // pri povtornyh zapuskah fon ne udalyaetsya
        SubtractBackground(True);
        ShowProfile;
    end;
    // TODO: mozhno optimizirovat' razbiv na nesk. funktsiy
    // i vyzyvaya ComputeCurvePositionsActual tol'ko odin raz

    // set of curve positions selected by user is saved if given
    // https://action.mindjet.com/task/14588987
    // https://github.com/dvmorozov/fit/issues/12
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoAlg;

    ComputeCurveBoundsAlg;
    MinimizeNumberOfCurvesAlg;
end;

function TFitService.DoAllAutomatically: string;
begin
    Result := '';
    if State = AsyncOperation then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;

    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    FStartTime := Now;
    try
        RecreateMainCalcThread(DoAllAutomaticallyAlg, DoneProc);
    except
        on E: Exception do
        begin
            SetState(ProfileWaiting);
            E.Message := E.Message + CRLF + IsProfileWaiting;
            raise;
        end;
    end;
end;

function TFitService.CreateTaskObject: TFitTask;
begin
    Result := TFitTask.Create(nil, FBackgroundVariationEnabled,
        FCurveScalingEnabled);
end;

procedure TFitService.CreateTasks;
var
    i, j:    longint;
    FitTask: TFitTask;
    Data, Temp: TPointsSet;
    BegIndex, EndIndex, PosIndex: longint;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FRFactorBounds));
    Assert(Assigned(FCurvePositions));
    // zadachu nuzhno vypolnyat' nastol'ko, naskol'ko vozmozhno;
    // dlya poslednego nezakrytogo intervala podzadacha ne sozdaetsya;
    // eto proshe, chem zhestko ogranichivat', delat' proverki i
    // vyvodit' soobscheniya
    // Assert(FRFactorBounds.PointsCount mod 2 = 0);

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    Assert(Assigned(Data));
    Data.Sort;

    FRFactorBounds.Sort; // !!! tochki, ogranichivayuschie intervaly m.b.
    // peredany izvne, poetomu sortirovka ne
    // pomeshaet !!!
    FCurvePositions.Sort; // !!! to zhe samoe !!!

    FTaskList.Free;
    FTaskList := nil;
    FTaskList := TComponentList.Create;

    // sozdanie i zapolnenie podzadach
    // odna podzadacha sozdaetsya dazhe kogda intervaly ne opredeleny;
    // !!! odna tochka v nabore granits - interval ne opredelen !!!
    if FRFactorBounds.PointsCount < 2 then
    (* v kachestve intervala beretsya ves' nabor dannyh -
          ne ochen' horosho, t.k. posle vyzova avtomaticheskoy
          generatsii intervalov rezul'tat ob'edinyaetsya s dannym,
          pri etom mogut voznikat' lishnie tochki
          FitTask := CreateTaskObject;
          try
          BegIndex := 0;
          EndIndex := Data.PointsCount - 1;
          //  delayutsya sootvet. dobavleniya v nabor granits intervala
          FRFactorBounds.AddNewPoint(
          Data.PointXCoord[BegIndex], Data.PointYCoord[BegIndex]);
          FRFactorBounds.AddNewPoint(
          Data.PointXCoord[EndIndex], Data.PointYCoord[EndIndex]);

          FitTask.BegIndex := BegIndex;
          FitTask.EndIndex := EndIndex;
          //  kopirovanie i ustanovka uchastka profilya
          Temp := TPointsSet(Data.GetCopy);
          try
          FitTask.SetProfilePointsSet(Temp);
          except
          Temp.Free; raise;
          end;
          //  kopirovanie i ustanovka tochek dlya raspolozheniya
          //  ekzemplyarov patterna
          Temp := TPointsSet(FCurvePositions.GetCopy);
          try
          FitTask.SetCurvePositions(Temp);
          except
          Temp.Free; raise;
          end;
          //  ustanovka dop. parametrov
          FitTask.MaxAcceptableRFactor := MaxRFactor;
          FitTask.CurveTypeId := CurveTypeId;
          if CurveTypeId = Special then
          FitTask.SetSpecialCurve(FCurveExpr, Curve_parameters(FParams.GetCopy));
          FitTask.ServerShowCurMin := ShowCurMin;
          FitTask.ServerDoneProc := DoneProc;

          FTaskList.Add(FitTask);
          except
          FitTask.Free; raise;
          end;
        *)// avtomaticheskiy poisk granits intervalov
         // avtomaticheskiy poisk meschaet udalyat' tochki,
         // poetomu poka otklyuchen
         // FRFactorBounds.Clear;
         // ComputeCurveBoundsAlg;
    ;
    // else
    // begin
    j := 0;
    while j <= FRFactorBounds.PointsCount - 1 do
    begin
        // chislo tochek mozhet byt' ne chetnym, kogda pol'zovatel' izmenyaet
        // granitsy intervalov posle sozdaniya ekzemplyarov patterna -
        // nuzhno korrektno obrabatyvat' takuyu situatsiyu;
        // dlya nezakrytogo intervala podzadacha ne sozdayetsya
        if j + 1 > FRFactorBounds.PointsCount - 1 then
            Break;
        FitTask := CreateTaskObject;
        try
            BegIndex := Data.IndexOfValueX(FRFactorBounds.PointXCoord[j]);
            EndIndex := Data.IndexOfValueX(FRFactorBounds.PointXCoord[j + 1]);
            Assert(BegIndex <> -1);
            Assert(EndIndex <> -1);

            FitTask.BegIndex := BegIndex;
            FitTask.EndIndex := EndIndex;
            // kopirovanie i ustanovka uchastka profilya
            Temp := TPointsSet.Create(nil);
            try
                for i := BegIndex to EndIndex do
                    Temp.AddNewPoint(Data.PointXCoord[i], Data.PointYCoord[i]);
                FitTask.SetProfilePointsSet(Temp);
            except
                Temp.Free;
                raise;
            end;
            // kopirovanie i ustanovka chasti tochek dlya raspolozheniya
            // krivyh popadayuschih v zadannyy interval
            Temp := TPointsSet.Create(nil);
            try
                for i := 0 to FCurvePositions.PointsCount - 1 do
                begin
                    PosIndex :=
                        Data.IndexOfValueX(FCurvePositions.PointXCoord[i]);
                    Assert(PosIndex <> -1);
                    if (PosIndex >= BegIndex) and (PosIndex <= EndIndex) then
                        Temp.AddNewPoint(FCurvePositions.PointXCoord[i],
                            FCurvePositions.PointYCoord[i]);
                end;
                FitTask.SetCurvePositions(Temp);
            except
                Temp.Free;
                raise;
            end;
            // ustanovka dop. parametrov
            FitTask.MaxAcceptableRFactor := MaxRFactor;
{$IFDEF WINDOWS_SPECIFIC}
            if IsEqualGUID(CurveTypeId, TUserPointsSet.GetCurveTypeId) then
                FitTask.SetSpecialCurve(FCurveExpr,
                    Curve_parameters(FParams.GetCopy));
{$ENDIF}
            FitTask.ServerShowCurMin := ShowCurMinInternal;
            FitTask.ServerDoneProc   := DoneProc;

            FTaskList.Add(FitTask);
        except
            FitTask.Free;
            raise;
        end;

        j := j + 2;
    end;
    // end;
end;

procedure TFitService.InitTasks;
var
    i:  longint;
    FT: TFitTask;
begin
    // metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    Assert(Assigned(FTaskList));
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        // rabotaet ne optimal'no, no podhodit dlya
        // povtornoy initsializatsii pri dobavlenii /
        // udalenii tochek privyazki ekzemplyarov patterna
        FT.RecreateCurves(FCurveList);
        FT.ComputeProfile;
    end;
end;

procedure TFitService.CreateCurveListAlg;
var
    NS:   TCurvePointsSet;
    StartPointIndex, StopPointIndex: longint;
    i, j: longint;
begin
    Assert(Assigned(FCurvesList));
    Assert(Assigned(FCurveList));
    FCurveList.Clear;

    for i := 0 to FCurvesList.Count - 1 do
    begin
        NS := TCurvePointsSet(FCurvesList.Items[i]);
        // opredelenie indeksov granichnyh tochek sravneniem s porogom
        StartPointIndex := -1;
        StopPointIndex := -1;
        for j := 0 to NS.PointsCount - 1 do
            if StartPointIndex = -1 then
            begin
                if Abs(NS.PointYCoord[j]) >= CurveThresh then
                    StartPointIndex := j;
            end
            else
            if Abs(NS.PointYCoord[j]) < CurveThresh then
            begin
                StopPointIndex := j - 1;
                Break;
            end;

        if StartPointIndex <> -1 then
        begin
            // krivye so slishkom maloy intensivnost'yu ne vklyuchayutsya v spisok
            if StopPointIndex = -1 then
                StopPointIndex := NS.PointsCount - 1;
            AddCurveToList(NS, StartPointIndex, StopPointIndex);
        end;
    end;
end;

procedure TFitService.CreateCurveList;
begin
    // interfeysnyy metod, poetomu proveryaet sostoyanie
    if not FFitDone then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            StillNotDone);
    try
        CreateCurveListAlg;
    except
        SetState(ProfileWaiting);
        raise;
    end;
end;

procedure TFitService.SetMaxRFactor(AMaxRFactor: double);
var
    i:  longint;
    FT: TFitTask;
begin
    FMaxRFactor := AMaxRFactor;
    if Assigned(FTaskList) then
        for i := 0 to FTaskList.Count - 1 do
        begin
            FT := TFitTask(FTaskList.Items[i]);
            FT.MaxAcceptableRFactor := AMaxRFactor;
        end;
end;

function TFitService.GetMaxRFactor: double;
begin
    Result := FMaxRFactor;
end;

procedure TFitService.SetBackFactor(ABackFactor: double);
begin
    FBackFactor := ABackFactor;
end;

function TFitService.GetBackFactor: double;
begin
    Result := FBackFactor;
end;

procedure TFitService.SetCurveThresh(ACurveThresh: double);
begin
    FCurveThresh := ACurveThresh;
end;

function TFitService.GetCurveThresh: double;
begin
    Result := FCurveThresh;
end;

{$IFNDEF FIT}

{ https://github.com/dvmorozov/fit/issues/160 }
procedure TFitServer.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
    FCurveTypeSelector.SelectCurveType(ACurveTypeId);
end;

{$ENDIF}

function TFitService.GetCurveType: TCurveTypeId;
begin
    Result := FCurveTypeSelector.GetSelectedCurveType;
end;

{$IFDEF _WINDOWS}

procedure TFitService.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters);
var
    i: longint;
    FitTask: TFitTask;
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    if not Assigned(CP) then // pervonach. initsializatsiya
        CreateParameters(ACurveExpr)
    else
    begin
        // zdes' eto uzhe ne yavl. dopustimoy oshibkoy pol'zovatelya -
        // eto fatal'n. oshibka programmy
        Assert(Length(ACurveExpr) <> 0);
        FParams.Free;
        FParams := CP;
    end;
    // esli ne proizoshlo isklyucheniya zapol. znachenie
    FCurveExpr := ACurveExpr;

    if Assigned(FTaskList) then
        for i := 0 to FTaskList.Count - 1 do
        begin
            FitTask := TFitTask(FTaskList.Items[i]);
            FitTask.SetSpecialCurve(FCurveExpr,
                Curve_parameters(FParams.GetCopy));
        end;
end;

{$ENDIF}

function TFitService.AsyncOper: boolean;
begin
    if State = AsyncOperation then
        Result := True
    else
        Result := False;
end;

function TFitService.GetRFactorStr: string;
var
    RFactor: double;
begin
    if GetAllInitialized then
    begin
        RFactor := GetTotalRFactor;
        Result  := FloatToStrF(RFactor, ffFixed, 10, 8);
    end
    else
        Result := RFactorStillNotCalculated;
end;

function TFitService.GetAbsRFactorStr: string;
var
    F: double;
begin
    if GetAllInitialized then
    begin
        F      := GetTotalAbsRFactor;
        Result := // FloatToStr(F);
            FloatToStrF(F, ffFixed, 10, 8);
    end
    else
        Result := RFactorStillNotCalculated;
end;

function TFitService.GetSqrRFactorStr: string;
var
    F: double;
begin
    if GetAllInitialized then
    begin
        F      := GetTotalSqrRFactor;
        Result := // FloatToStr(F);
            FloatToStrF(F, ffFixed, 10, 8);
    end
    else
        Result := RFactorStillNotCalculated;
end;

function TFitService.GetCalcTimeStr: string;
var
    Day, Hour, Min, Sec: longint;
    TimeStr:   string;
    TotalTime: TDateTime;
begin
    // https://www.evernote.com/shard/s132/nl/14501366/6dd2bdde-01b1-481b-adf2-665e1af55e51
    TotalTime := Now - FStartTime;

    Sec  := Trunc(TotalTime * 86400);
    Day  := Sec div 86400;
    Sec  := Sec mod 86400;
    Hour := Sec div 3600;
    Sec  := Sec mod 3600;
    Min  := Sec div 60;
    Sec  := Sec mod 60;

    // The date is counted since 12/30/1899.
    TimeStr := IntToStr(Day) + ' day(s) ';

    if Hour < 10 then
        TimeStr := TimeStr + '0';
    TimeStr     := TimeStr + IntToStr(Hour) + ':';
    if Min < 10 then
        TimeStr := TimeStr + '0';
    TimeStr     := TimeStr + IntToStr(Min) + ':';
    if Sec < 10 then
        TimeStr := TimeStr + '0';
    TimeStr     := TimeStr + IntToStr(Sec);
    Result      := TimeStr;
end;

{$IFDEF _WINDOWS}

procedure TFitService.CreateParameters(ACurveExpr: string);
var
    Result:     longint;
    ExprResult: double;
    { TODO: remake without using LPCSTR. }
    Symbols, Saved: LPCSTR;
    Parameter:  TSpecialCurveParameter;
    Container:  TPersistentCurveParameterContainer;
begin
    Assert(Assigned(FParams));
    Assert(Assigned(FParams.Params));

    if Length(ACurveExpr) = 0 then
        raise EUserException.Create('Inadmissible or invalid expression.');

    Result := ParseAndCalcExpression(PChar(ACurveExpr), '', @ExprResult);

    if (Result = 1)      // razobrano polnost'yu
        or (Result = -1) // est' parametry (znacheniya kot. esche ne opredeleny)
    then
    begin
        // pervonachal'noe zapolnenie parametrov
        FParams.Params.Clear;
        // List of parameter names separated by zeros.
        Symbols := GetSymbols;
        Saved   := Symbols;
        try
            while Assigned(Symbols) and (Length(Symbols) <> 0) do
            begin
                Parameter := TUserCurveParameter.Create;

                try
                    Parameter.Name := Symbols;
                    // raspoznaetsya imya argumenta
                    if UpperCase(Parameter.Name) = 'X' then
                        Parameter.Type_ := Argument
                    else
                    // raspoznaetsya tipichnyy fiksirovannyy parametr polozheniya
                    if UpperCase(Parameter.Name) = 'X0' then
                        Parameter.Type_ := InvariablePosition
                    else
                        Parameter.Type_ := Variable;

                    Symbols := Symbols + Length(Symbols) + 1;

                    Container :=
                        TPersistentCurveParameterContainer(FParams.Params.Add);

                    try
                        Container.Parameter := Parameter;
                    except
                        FParams.Params.Delete(Container.ID);
                        Container.Free;
                        raise;
                    end;

                except
                    Parameter.Free;
                    raise;
                end;
            end;
        finally
            FreeSymbols(Saved);
        end;

        if FParams.Count = 0 then
            // argument-to dolzhen byt'
            raise EUserException.Create('Lack of argument.');
        if FParams.Count = 1 then
            // edinstvennyy parametr m.b. tol'ko argumentom
        begin
            Parameter := FParams[0];
            Parameter.Type_ := Argument;
        end;
    end
    else
        raise EUserException.Create('Inadmissible or invalid expression.');
end;

{$ENDIF}

// !!! povtornyy vyzov dlya dannyh koordinat udalyaet tochku iz spiska !!!
procedure TFitService.AddPoint(var Points: TTitlePointsSet; XValue, YValue: double);
var
    i: longint;
begin
    Assert(Assigned(Points));

    // ischem zadannuyu tochku v vybrannom spiske tochek
    for i := 0 to Points.PointsCount - 1 do
        if Abs(XValue - Points.PointXCoord[i]) <= TINY then
        begin
            if Abs(YValue - Points.PointYCoord[i]) <= TINY then
                Points.DeletePoint(XValue)
            // zamena znacheniya
            else
                Points.PointYCoord[i] := YValue;
            Exit;
        end;
    // tochka ne naydena - dobavlyaem novuyu
    Points.AddNewPoint(XValue, YValue);
end;

procedure TFitService.AddPointToProfile(XValue, YValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);

    Assert(Assigned(FExpProfile));
    AddPoint(FExpProfile, XValue, YValue); // dobavlyaet i udalyaet tochki

    if FExpProfile.PointsCount = 0 then
        SetState(ProfileWaiting)
    else
        SetState(BackNotRemoved);
end;

procedure TFitService.AddPointToBackground(XValue, YValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FBackgroundPoints));
    AddPoint(FBackgroundPoints, XValue, YValue);
end;

procedure TFitService.GoToReadyForFit;
begin
    if State = ProfileWaiting then
        Exit;
    SetState(ReadyForAutoFit); // !!! udalyaet podzadachi !!!

    if // proverka nuzhna, t.k. imenno takomy
       // sochetaniyu sootvetstvuet gotovnost'
       // k podgonke s parametrami pol'zovatelya

    (FRFactorBounds.PointsCount <> 0) and (FCurvePositions.PointsCount <> 0) then
    begin
        // trebuetsya peresozdanie podzadach i ekzemplyarov patterna,
        // potomu chto menyayutsya granitsy
        CreateTasks; // !!! sozdayutsya vremenno !!!
        InitTasks;

        CreateResultedCurvesList;
        // tochki privyazki ne dolzhny sobirat'sya iz podzadach,
        // tak kak pri etom budut propadat' tochki ne voschedschie
        // v podzadachi, a eto mozhet ozadachivat' pol'zovatelya
        // CreateResultedCurvePositions;
        CreateResultedProfile;
        CreateDeltaProfile;
        CreateCurveListAlg;

        SetState(ReadyForFit); // !!! udalyaet podzadachi !!!
    end;
end;

procedure TFitService.AddPointToRFactorBounds(XValue, YValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FRFactorBounds));
    AddPoint(FRFactorBounds, XValue, YValue);
    GoToReadyForFit;
end;

procedure TFitService.AddPointToCurvePositions(XValue, YValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FCurvePositions));
    AddPoint(FCurvePositions, XValue, YValue);
    // mozhno sdelat' optimal'nee, no dlya etogo podzadachi
    // d.b. uzhe sozdany i spisok granits intervalov d.b. nepustym
    GoToReadyForFit;
end;

procedure TFitService.ReplacePointInProfile(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    // dopolnyat' dannye vsegda mozhno
    // if State = ProfileWaiting then
    // raise EUserException.Create(InadmissibleServerState + CRLF +
    // DataMustBeSet);

    if FSelectedAreaMode then
    begin
        Assert(Assigned(FSelectedArea));
        FSelectedArea.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    end
    else
    begin
        Assert(Assigned(FExpProfile));
        FExpProfile.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    end;
    // dannye zdes' mogut byt' tol'ko dobavleny ili izmeneny,
    // poetomu FExpProfile.PointsCount = 0 mozhno ne proveryat'
    SetState(BackNotRemoved);
end;

procedure TFitService.ReplacePointInBackground(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);

begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FBackgroundPoints));
    FBackgroundPoints.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    // dannye zdes' mogut byt' tol'ko dobavleny ili izmeneny,
    // poetomu FBackgroundPoints.PointsCount = 0 mozhno ne proveryat'
    SetState(BackNotRemoved);
end;

procedure TFitService.ReplacePointInRFactorBounds(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FRFactorBounds));
    FRFactorBounds.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitService.ReplacePointInCurvePositions(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    if State = AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NowCalculation);
    if State = ProfileWaiting then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            DataMustBeSet);

    Assert(Assigned(FCurvePositions));
    FCurvePositions.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitService.RecreateMainCalcThread(ACurrentTask: TThreadMethod;
    ADoneProc: TThreadMethod);
begin
    Assert(Assigned(ACurrentTask));
    Assert(Assigned(ADoneProc));
    ACurrentTask;
    ADoneProc;
end;

end.
