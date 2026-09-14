// SPDX-License-Identifier: GPL-3.0-or-later
{
  This software is distributed under GPL
  in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

  @abstract(Contains definition of class implementing server logic. Doesn't contain any user interface interaction.
  Exception handling and converting them into messages understandable for caller should be done in boundary objects.

  WHICH EXCEPTION MEANS WHAT. EUserException says the caller asked for something
  this program does not support in its current state; the server keeps its state
  and the boundary turns it into a message the user reads. EInternalCheckFailed
  (Common/checks.pas) says this program is wrong about itself; it is logged where
  it happens and is a defect. Anything else is a fatal error, after which the
  server should be brought back to the state it had just after start.

  This unit used to name EAssertionFailed for the middle case. It no longer
  raises it - the Stage 3E conversion replaced every Assert with a checks.pas
  call - and the distinction Assert could not draw is exactly the one
  EInternalCheckFailed exists to draw.)

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

uses
    calculated_curve_parameter, Classes, Contnrs, curve_identity_registry,
    curve_instance_id, curve_points_set, DateUtils, module_project_state,
    curve_types_singleton, gauss_points_set, int_curve_type_selector,
    log, mscr_specimen_list, named_points_set,
    persistent_curve_parameter_container, persistent_curve_parameters,
    points_set, special_curve_parameter, SysUtils, Variants, title_points_set
    {Proxy to client to call it back.}
    , fit_server_proxy
    , fit_task, int_client_callback, int_fit_service, MyExceptions
    , fit_statistics, fit_service_statistics, fit_advice
    //  Which states admit which operation, and what the user is told
    //  when one does not. Thirty inline copies until it was asked for.
    , service_state_rules
    //  Where the background is, given the data alone.
    , background_search
    //  What a user's own formula declares, and the five refusals they read
    //  while looking at their own text. A hundred and thirty lines of it used
    //  to be a method here.
    , user_formula_parameters
    //  The two weighting names, and what an unrecognised one means.
    , fit_weighting
    , self_copied_component, SimpMath

    , user_points_set, user_curve_parameter, native_math_expr, fit_loss
    , int_curve_factory, minimizer_registry, minimizer_registration
    , int_app_module, module_registry
    , fphttpclient, fpjson
    , checks;

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
      Interface methods raise EUserException for a request this service does not
      admit in its current state, because that is a message the user should read.
      A statement the code believes about ITSELF is checked with Common/checks.pas
      and fails as EInternalCheckFailed, which the boundary treats as a fault.
      The reason the two must stay distinct is that a catch-all written for the
      first would otherwise silently absorb the second.
      This implementation performs all operations in the thread of caller.
      It should store all the data necessary for operations including selected
      intervals because client can be unable to store data.
    }
    TFitService = class(TInterfacedObject, IClientCallback, IFitService)
    protected
        FCurveTypeSelector: ICurveTypeSelector;
        { THIS problem's curve type. The selector is a process-wide SINGLETON, so
          holding the selection only there meant a server with two problems open
          had the second one silently change the first: setting a curve type
          for one made every other problem fit that type too. The selector is still
          driven, because the desktop menu reads it to draw its checkmark, but
          what gets FITTED comes from here. }
        FCurveTypeId: TCurveTypeId;
        { Callback sink for progress/completion. In-process this is the client
          proxy; on the REST server it is the session that records pollable state. }
        FFitProxy:   IClientCallback;
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
        { One per registered module, made when the problem is. A build with no
          modules has an empty list and every module path becomes a no-op -
          which is exactly what the public build must do. }
        FModuleSessions: array of IModuleSession;

        { Contains all curves collected from tasks (for example,
          for separate intervals). Items are added synchronously to the list. }
        FCurves:        TSelfCopiedCompList;
        { Positions of curves. Only X-coordinates are used.

          MODEL INPUT, and only that: the user's picks. Every X is a sample of
          the profile and no two are equal, because this is what CreateTasks
          looks up in the data and what every instance is seeded from - and the
          seed is the key a rebuild restores that instance's fitted parameters
          by. A fit must never write here; see FResultedCurvePositions. }
        FCurvePositions: TTitlePointsSet;
        { Where the curves of the built model actually sit: one point per
          instance that has a position parameter, at the instance's own fitted
          x0.

          DERIVED AND READ-ONLY. It is what the chart reports as the model's
          positions, and nothing reads it back into the model - which is what
          lets it hold values FCurvePositions may not: an x0 off the sample
          grid, and two instances that have converged on the same one. }
        FResultedCurvePositions: TTitlePointsSet;
        { WHICH INSTANCE EACH PICK STANDS FOR - the maintained half of the
          model, and the one thing here that is neither an input nor rebuilt
          from one.

          Every model edit destroys and rebuilds the curve instances, so the
          values a fit found have to be given back to objects that did not exist
          a moment ago. This says which is which. Identity is issued to the PICK,
          which survives the rebuild, and inherited by whatever instance is built
          from it - see curve_identity_registry for why that is the only place it
          can live.

          It also records which instances a COMPLETED FIT produced values for.
          That is not derivable from FCurveAttributes, which is rebuilt on every
          model edit and therefore holds current values whether an optimiser ever
          ran or not, so it cannot tell a fitted curve from one that has only
          just been seeded. }
        FIdentity: TCurveIdentityRegistry;
        { Containers of parameters of curves.
          TODO: change type and remove SetWaveLength. }
        FCurveAttributes: TMSCRCurveList;

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
        { Selected minimizer algorithm (MIN_KIND_* constant), passed to each task. }
        FMinimizerKind: longint;
        { Objective being minimised (LOSS_KIND_* constant). }
        FLossKind: longint;
        { Residual weighting for the Python backend ('poisson'/'none'). }
        FWeighting: string;
        { Compute-server URL, passed to each task; empty = fit in-process. }
        FServerUrl: string;
        { Loopback URL of the Python sidecar, set by fit_server when the Python
          minimizer is selected. }
        FPythonSidecarUrl: string;

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
        { Fills the picked positions automatically, but only for a model that
          is actually built from them. }
        procedure ComputeCurvePositionsForAutoIfPlacedByPositions;
        { Brings the identity registry back into step with the picks.

          ONE IDEMPOTENT SYNC rather than a mirror of every verb that touches
          the pick set. Picks are changed in more ways than they appear to be:
          AddPoint is a TOGGLE, so the verb that adds one also deletes one; and
          the automatic algorithms replace the whole set. Mirroring each branch
          would leave the two out of step exactly where it was least expected.

          A MOVE MUST BE APPLIED BEFORE THIS RUNS - see the pick verbs. To this
          method a move is indistinguishable from a delete plus an add, which
          would issue a new handle and orphan the fit the move exists to keep. }
        { Takes into the model the curves an automatic run DELETED.

          MinimizeNumberOfCurves removes instances - that is what it is for -
          and it removes them from the task's OWN COPY of the picks. The service
          was never told, so its pick list still held every original pick and
          the very next rebuild seeded them all again: the reduction evaporated
          on the next click, and nothing said so.

          THE ONE THING A FIT MAY DO TO THE PICKS, and only this. It deletes,
          never moves and never adds, so the two invariants that make a pick set
          model input - unique x values, every one a real sample of the profile
          - are untouched. Moving one would break both, and did once. }
        procedure AdoptCurveRemovalsFromTasks;
        procedure SyncIdentityToPicks;
        { True when a COMPLETED FIT left values against ACurve, so a rebuild
          restores something the optimiser found rather than a seed. }
        function IsFittedInstance(ACurve: TCurvePointsSet): boolean;
        { Records which instances the fit that has just finished produced values
          for. }
        procedure RememberFittedInstances;
        { Refuses a move that would silently discard a fitted curve's
          parameters; does nothing when the move is harmless. }
        procedure RefuseMarkupMoveThatWouldLoseTheFit(
            AAnyCurveIsFitted: boolean);
        function IntegrateWithBoundaries(Points: TPointsSet;
            StartPointIndex, StopPointIndex: longint): double;
        { Calculates the R-factor for FCalcProfile and SelectProfileInterval by sum for all tasks. }
        function IsWavePatternTypeSelected: boolean;
        { What the selected type's own module calls its placement gesture. }
        function PlacementGestureName: string;
        procedure RefuseIntervalsNoModuleAccepts;
        { Pooled parts of every interval's contribution - see TotalLossParts. }
        function TotalLossParts: TLossParts;
        function GetTotalRFactor: double;
        function GetTotalAbsRFactor: double;
        function GetTotalSqrRFactor: double;
        { Copies data from given list to the list of selected interval. }
        procedure SelectProfileIntervalActual(Points: TPointsSet;
            StartPointIndex, StopPointIndex: longint);
        function CreateTaskObject: TFitTask; virtual;
        { Creates subtasks for selected intervals. If the intervals were not selected generates them automatically. }
        procedure CreateTasks;
        procedure InitTasks; overload;
        procedure InitTasks(AForFitting: boolean); overload;

        { Auxiliary methods. }

        procedure CreateResultedProfile;
        { Calculates profile containing differences between calculated and experimental data.
          In the calculation all curves are included. Will not work properly if curves are overlapped. }
        procedure CreateDeltaProfile;
        procedure CollectCurves;
        { Collects resulting set of curve positions. }
        procedure CreateResultedCurvePositions;
        { Iterates through list of curves and creates common list of parameters
          of all curves complementing them with calculated values. }
        procedure CollectCurveAttributes;
        { Prepares intermediate results for user. }
        procedure GoToReadyForFit;

        procedure CreateModuleSessions;
        { Asks the modules to drop the markup that placed one instance, and says
          whether one of them owned it. See DeleteCurve. }
        function AskModulesToRemoveInstance(const AId: TCurveInstanceId;
            out ARemoved: TInstanceHandles): boolean;
        function SinkNamed(const AKind: string): IModulePointSink;
        { True when any module has enough marked for a fit to be worth starting.
          Joined to the framework's own preconditions by OR: a module's markup is
          an alternative way to describe a model, not an addition to the usual
          one. }
        function AnyModuleContributesFitReadiness: boolean;

        { Checks expression and fills list of parameters. }

        procedure CreateParameters(ACurveExpr: string);

        function GetAllInitialized: boolean;
        { Does not really create any thread. Simply calls methods synchronously. }
        procedure RecreateMainCalcThread(ACurrentTask: TThreadMethod;
            ADoneProc: TThreadMethod); virtual;

    public
        constructor Create;
        destructor Destroy; override;

        { ------------------------- the module host -------------------------

          What a module may reach on the problem it belongs to. Deliberately
          small, and deliberately made of things the framework already does:
          a module extends the application, it does not get a private door into
          it.

          The dependency runs ONE WAY. A module uses these; nothing here knows
          what any module is. That is what lets a build contain no modules at
          all, and it is the inversion the old arrangement lacked - where the
          framework's own units had to name the pack. }

        { Rebuild the model after the module changed what it holds. The same
          call the built-in point sets make, so a module's markup and an
          ordinary curve position reach the engine by one path. }
        procedure ModuleStateChanged;
        { Refuses when the problem cannot accept picks right now - mid-fit, or
          before a profile is loaded - with the same messages the built-in point
          sets give, so a module needs no vocabulary of its own for it. }
        procedure CheckCanAcceptPicks;
        { Adds a pick to a module's own point set, by the set's name. }
        procedure AddPointToSet(const AKind: string; XValue, YValue: double);
        procedure ReplacePointInSet(const AKind: string;
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        { Reads or writes a module resource. The reply is the resource itself -
          a JSON document the module defines - not an ok-wrapped envelope, which
          is what these payloads already were on the wire. }
        function GetModuleProjectStates: TModuleStateArray;
        function ModuleGet(const AResource: string): string;
        function ModulePost(const AResource, APayload: string): string;

        { Where the sidecar is, or empty when it is not available. }
        function PythonSidecarUrl: string;
        { The point set the SELECTED curve type is placed from, or empty when it
          is placed from a single curve position (TNamedPointsSet.
          PlacedByPointSet). A module compares it with its own set's name to ask
          "is what the user is about to create mine?" - without the framework
          having to know which modules exist. }
        function SelectedCurvePlacedByPointSet: string;
        { The curves as they stand, and the computed profile they sum to. Read
          only: a module derives its own view (an overlay, a report) from what
          the engine has already built. }
        function CurrentCurves: TSelfCopiedCompList;

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

        function SetCurvePositions(ACurvePositions: TPointsSet;
            const AIds: TCurveInstanceIdList = nil): string;
        function GetCurvePositions: TTitlePointsSet;
        function GetCurvePositionIds: TCurveInstanceIdList;
        function GetResultedCurvePositions: TTitlePointsSet;

        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet;

        procedure ClearSpecialCurve;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Equality to Nil means initialization. }
            CP: Curve_parameters);
        function GetSpecialCurveParameters: Curve_parameters;

        { The server should support primitives for adding and updating points
          to support thin clients which can not store all set of data. }
        { All methods call AddPoint. }

        procedure AddPointToProfile(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);

        { All methods call ReplacePoint. }

        procedure ReplacePointInProfile(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(PrevXValue, PrevYValue,
            NewXValue, NewYValue: double);

        { Returns list of parameters of all curves. }
        function GetCurveAttributes: TMSCRCurveList;
        { Returns list of components containing sets of points. }
        function GetCurves: TSelfCopiedCompList;

        { These methods check validity of server state and
          throw EUserException in the case when state is invalid. }

        function GetCurveCount: longint;
        { True when ANY instance of the model carries values an optimiser
          produced, as opposed to seeds.

          PUBLIC rather than protected, which is where it was: it is what
          RefuseMarkupMoveThatWouldLoseTheFit decides on, and a restore has to
          be able to establish it - so a test of the restore has to be able to
          read it, through the same member production uses. }
        function AnyCurveIsFitted: boolean;
        function GetCurveInstanceId(ACurveIndex: longint): string;
        function IsCurveFitted(ACurveIndex: longint): boolean;
        function IndexOfCurveInstance(const AInstanceId: string): longint;
        procedure SetPointUnique(var Points: TTitlePointsSet;
            XValue, YValue: double);
        { Takes the handle AHandle for the pick at ASeed, so the instance
          rebuilt there is the same instance to everything downstream.

          An EMPTY handle means the caller has none for this pick and one is to
          be issued as usual - a project saved before a fit ran carries none for
          the picks placed since. Anything else that is not a handle is REFUSED:
          read as "no handle" it would silently become a brand new curve, and
          the values saved for the old one would be dropped with no sign of it. }
        procedure AdoptOfferedIdentity(const AHandle: string; ASeed: double);
        { Writes one instance's parameter values into the per-round report,
          matched by name. See SetCurveValues, its only caller. }
        procedure WriteCurveValues(ACurveIndex: longint;
            const AParams: TCurveParamValues);
        function DeleteCurve(ACurveIndex: longint): string;
        function GetCurveParameterCount(ACurveIndex: longint): longint;
        procedure GetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        function GetCurveParameterError(ACurveIndex: longint;
            ParamIndex: longint): double;
        function GetCurveParameterValue(ACurveIndex: longint;
            ParamIndex: longint): Variant;
        procedure SetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            Value: double);
        function SetCurveValues(const AEntries: TCurveValuesList): string;

        { Asynchronous long-term operations. }
        { Smoothes experimental data. Returns describing message.
          TODO: so far is executed synchronously. Refactor to asynchronous processing. }
        function SmoothProfile: string;
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
        procedure StopAsyncOper; virtual;
        { Stops long-term operation synchronously without calling termination procedure. }
        procedure AbortAsyncOper; virtual;
        { Makes room for the command about to run: aborts an operation in flight
          if there is one, and answers the note the caller passes back - empty
          when nothing was running.

          FOURTEEN COMMANDS BEGAN WITH THIS, six lines apiece. Aborting is not
          refusing - the command goes ahead either way - so what the caller owes
          the user is a note saying the previous calculation was cancelled to
          make room, and returning that note is the whole of this. Whether a
          state has something to abort is service_state_rules' answer; doing it
          and saying so is this. }
        function AbortedToMakeRoom: string;
        { What both of them mean for a service that has no background operation
          to stop. }
        procedure ClearStaleAsyncOperation;
        { Returns True in asynchronous operation mode. }
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;
        function GetStatistics: TFitStatistics;

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

        function GetCurveType: TCurveTypeId;
        procedure SetCurveType(ACurveTypeId: TCurveTypeId);
        property CurveTypeId: TCurveTypeId read GetCurveType write SetCurveType;

        function GetState: TFitServerState;
        property State: TFitServerState read GetState;

        function GetWaveLength: double;
        property WaveLength: double read GetWaveLength write SetWaveLength;
        property SelectedAreaMode: boolean read FSelectedAreaMode;
        { This can be equal to Nil. }
        property FitProxy: IClientCallback read FFitProxy write FFitProxy;
        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);
        function GetMinimizerKind: longint;
        procedure SetMinimizerKind(AKind: longint);
        function GetLossKind: longint;
        procedure SetLossKind(AKind: longint);
        function GetWeighting: string;
        procedure SetWeighting(const AValue: string);
        function GetServerUrl: string;
        procedure SetServerUrl(const AUrl: string);
        { Where the engine reaches the Python sidecar (set by fit_server). }
        procedure SetPythonSidecarUrl(const AUrl: string);

        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);
    end;

const
    InadmissibleData: string = 'The data is not valid for this operation.';
    RFactorStillNotCalculated: string = 'Not calculated';
    IsProfileWaiting: string = 'Waiting for data to be loaded.';
    //  Shown once the user has supplied everything the calculation needs.
    IsReadyForFit: string =
        'Now the program is ready for fitting with selected conditions.';
    //  Shown when some of it is still missing and the program would choose it
    //  itself if a fit were started.
    IsReadyForAutoFit: string =
        'Now the program is ready for fitting with automatically selected conditions.';
    BackRemoving: string = 'Now background points should be defined.';
    BackPointsNum: string =
        'The number of background points should not be less then 2.';
    RangeAlready: string = 'Range of data already selected.';
    EntireAlready: string = 'Entire dataset already selected.';
    NotAllData: string =
        'Not all necessary data has been defined for the calculation.';
    //  The text should say roughly: not everything the calculation needs has
    //  been defined. You must supply, or let the program generate, the intervals
    //  the patterns apply to, their anchor points and their initial parameter
    //  values.
    CRLF: string = #13#10;

implementation

//  NO `uses app;` HERE ANY MORE, and its absence is the fix rather than a
//  tidy-up. app.pas uses Forms and constructs, in its initialization, a
//  desktop client application object and an HTTP client pointed at the
//  default server URL. This unit referenced neither of the two identifiers
//  app.pas exports - the clause was dead - but linking it meant the headless
//  compute server built a desktop client and a connection to itself on every
//  start-up, and dragged the LCL onto the engine path while doing it.
//  See docs/contributing/findings.md.

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1E100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1E100;

//  ParseAndCalcExpression / GetSymbols / FreeSymbols are provided cross-platform
//  by the native_math_expr unit (formerly the external 'MathExpr' library).

{ ================================= TFitService ================================= }
function TFitService.SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
begin
    Result := AbortedToMakeRoom;

    try
        CheckThat(Assigned(APointsSet), 'APointsSet is missing when it is required');

        //  A NEW PROFILE IS A NEW PROBLEM.
        //
        //  Everything the service holds besides the profile is expressed in the
        //  profile's own x-values: background points, curve positions, data
        //  intervals and pattern bounds are all picks ON the data, and the
        //  curves, the calculated profile and the difference are computed FROM
        //  it. Carried over to a profile loaded from another file they are not
        //  stale, they are meaningless - CreateTasks looks every curve position
        //  up in the data and fails its internal check when the x is not there
        //  ('PosIndex <> -1'), which is what opening a second data file used to
        //  cost the user as soon as anything rebuilt the tasks.
        //
        //  ProfileWaiting is the transition defined as "the state the server had
        //  at start-up", which is precisely what a freshly loaded profile
        //  deserves, so go through it rather than repeat its resets here. It is
        //  re-entrant by design, and the profile is installed immediately after.
        SetState(ProfileWaiting);

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
    Result := AbortedToMakeRoom;

    try
        CheckThat(Assigned(ABackgroundPoints), 'ABackgroundPoints is missing when it is required'); // kriticheskaya oshibka
        CheckAssigned(FExpProfile, 'the experimental profile');

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

function TFitService.SetCurvePositions(ACurvePositions: TPointsSet;
    const AIds: TCurveInstanceIdList): string;
var
    i:   longint;
    Msg: string;
begin
    Result := AbortedToMakeRoom;

    try
        CheckThat(Assigned(ACurvePositions), 'ACurvePositions is missing when it is required');
        CheckAssigned(FCurveAttributes, 'the curve attributes');

        //  BEFORE ANYTHING IS CLEARED. A refusal must not be the reason the
        //  model was emptied, and a caller whose handles do not line up with
        //  its picks has told us nothing we can act on - see the note on
        //  IFitService.SetCurvePositions for why this is not padded.
        if (Length(AIds) > 0) and
           (Length(AIds) <> ACurvePositions.PointsCount) then
        begin
            ACurvePositions.Free;
            raise EUserException.Create(
                'The picks were sent with ' + IntToStr(Length(AIds)) +
                ' curve identifiers for ' +
                IntToStr(ACurvePositions.PointsCount) + ' points. ' +
                'There must be one for each, or none at all.');
        end;

        FCurvePositions.Clear;
        FCurveAttributes.Clear;

        for i := 0 to ACurvePositions.PointsCount - 1 do
        begin
            //  NOT AddPoint: a repeated coordinate in a bulk write is a
            //  malformed input, and the toggle would leave no point at that x
            //  at all. See SetPointUnique.
            SetPointUnique(FCurvePositions, ACurvePositions.PointXCoord[i],
                ACurvePositions.PointYCoord[i]);
            //  IN STEP WITH THE WRITE ABOVE, and that is the whole reason it is
            //  in this loop rather than a pass over AIds afterwards:
            //  SetPointUnique collapses a repeated abscissa to ONE point, so a
            //  later pass indexing AIds by point number would be off by one for
            //  every pick after the duplicate - and an id that slid by one is
            //  not an error anywhere, it is another curve's shape restored onto
            //  this one.
            if Length(AIds) > 0 then
                AdoptOfferedIdentity(AIds[i], ACurvePositions.PointXCoord[i]);
        end;
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

function TFitService.GetCurvePositionIds: TCurveInstanceIdList;
var
    i: longint;
    Id: TCurveInstanceId;
begin
    Result := nil;
    if not Assigned(FCurvePositions) then
        Exit;
    SetLength(Result, FCurvePositions.PointsCount);
    for i := 0 to FCurvePositions.PointsCount - 1 do
    begin
        //  From the identity registry rather than from the curve list, and the
        //  difference matters: the registry is keyed by the SEED, so it answers
        //  for a pick whose instance has not been built yet and for one whose
        //  curve has since moved. The curve list is keyed by neither.
        Id := FIdentity.IdForSeed(FCurvePositions.PointXCoord[i]);
        if IsCurveInstanceId(Id) then
            //  The wire form, so the file and the URL spell a handle the same
            //  way and nothing has to know which of the two it is holding.
            Result[i] := CurveInstanceIdToWire(Id)
        else
            //  EMPTY, NOT ABSENT: one entry per pick is the contract, and a
            //  short list would pair a handle with the wrong pick instead of
            //  failing.
            Result[i] := '';
    end;
end;

function TFitService.SetRFactorBounds(ARFactorBounds: TPointsSet): string;
var
    i:   longint;
    Msg: string;
begin
    Result := AbortedToMakeRoom;

    try
        CheckThat(Assigned(ARFactorBounds), 'ARFactorBounds is missing when it is required');
        CheckAssigned(FCurveAttributes, 'the curve attributes');

        FRFactorBounds.Clear;
        FCurveAttributes.Clear;

        for i := 0 to ARFactorBounds.PointsCount - 1 do
            //  See SetCurvePositions above: the same reasoning, and the bounds
            //  are picked in pairs, where losing one end is worse still.
            SetPointUnique(FRFactorBounds, ARFactorBounds.PointXCoord[i],
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
    FResultedCurvePositions.Free;
    FExpProfile.Free;
    FParams.Free;
    FIdentity.Free;
    inherited;
end;

constructor TFitService.Create;
begin
    inherited;

    FParams := Curve_parameters.Create(nil);
    //  FIRST, because SetState(ProfileWaiting) at the end of this constructor
    //  clears it along with everything else the problem holds.
    FIdentity := TCurveIdentityRegistry.Create;

    FMaxRFactor  := 0.0001; // 0.01%
    FBackFactor  := 30;
    FCurveThresh := 0;
    // Sets default curve type.
    FCurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;
    FCurveTypeId := FCurveTypeSelector.GetSelectedCurveType;

    FBackgroundVariationEnabled := False;
    FMinimizerKind := MIN_KIND_DHS;   //  original Downhill Simplex by default
    FLossKind := LOSS_KIND_RFACTOR;   //  the corrected, data-normalised form
    FWeighting := WEIGHTING_POISSON;
    FCurveScalingEnabled := True;

    //  So that points can be added through the table without entering the
    //  special mode.
    FExpProfile     := TTitlePointsSet.Create(nil);
    FBackgroundPoints := TTitlePointsSet.Create(nil);
    FRFactorBounds  := TTitlePointsSet.Create(nil);
    //  After the framework's own state, so a module's session can read it while
    //  being made. A build with no modules gets an empty list here and every
    //  module path becomes a no-op.
    CreateModuleSessions;
    //  Entries are added to these lists together.
    FCurvePositions := TTitlePointsSet.Create(nil);
    FResultedCurvePositions := TTitlePointsSet.Create(nil);
    FCurveAttributes := TMSCRCurveList.Create;
    FCurveAttributes.FWaveLength := WaveLength;
    FCurves := TSelfCopiedCompList.Create;

    SetState(ProfileWaiting);
end;

procedure TFitService.SubtractBackground(Auto: boolean);
var
    Data, Background: TPointsSet;
    i, StartIndex, EndIndex: longint;
    SavedI: double;
begin
    if State = AsyncOperation then
        //  As an internal method: an inadmissible state raises.
        CheckThat(FSavedState = BackNotRemoved, 'the background must still be in place before it can be removed again')
    else
    //  As an interface method: it raises the dedicated exception.
    if ProfileRefusal(State) <> rfNone then
        RefuseIf(ProfileRefusal(State))
    //  There is no reason to refuse subtracting the background twice.
    // if (State <> BackNotRemoved) then
    // raise EUserException.Create(InadmissibleServerState);
    ;

    CheckAssigned(FBackgroundPoints, 'the background points');
    (* To support being called through the web interface - see below.
      if not Auto then
      begin
      //  a fatal error
      //  again, as an interface method...
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
        CheckAssigned(Data, 'the data record');
        Data.Sort;

        if not Auto then
        begin
            //  To support being called through the web interface.
            if FBackgroundPoints.PointsCount < 2 then
                Background := ProposeBackgroundPoints(Data)
            else
                Background := FBackgroundPoints;
        end
        else
            Background := ProposeBackgroundPoints(Data);
        try
            //  Guards the Background resources against being lost.
            CheckAssigned(Background, 'the background points to subtract');
            Background.Sort;

            StartIndex := Data.IndexOfValueX(Background.PointXCoord[0]);
            CheckThat(StartIndex <> -1, 'the first background point must fall on a sample of the profile');

            for i := 1 to Background.PointsCount - 1 do
            begin
                EndIndex := Data.IndexOfValueX(Background.PointXCoord[i]);
                CheckThat(EndIndex <> -1, 'every background point must fall on a sample of the profile');
                //  SubtractBackgroundLinearly subtracts at every point
                //  INCLUDING the boundaries, so the last point's intensity is
                //  saved and put back - otherwise the next segment would start
                //  from a point that can only ever come out as zero.
                SavedI := Data.PointYCoord[EndIndex];
                SubtractBackgroundLinearly(Data, StartIndex, EndIndex);
                StartIndex := EndIndex;
                Data.PointYCoord[EndIndex] := SavedI;
            end;
            //  Undoes that restoration for the very last point.
            Data.PointYCoord[EndIndex] := 0;
        except
            if Background <> FBackgroundPoints then
                Background.Free;
            FBackgroundPoints.Clear;
            raise;
        end;
        if Background <> FBackgroundPoints then
            Background.Free;
        //  CLEARED, so the background points are not left hanging.
        FBackgroundPoints.Clear;
        //  The state must not change inside a child thread: that destroys the
        //  thread.
        if State <> AsyncOperation then
            SetState(ReadyForAutoFit);
    except
        //  The state must not change inside a child thread: that destroys the
        //  thread.
        if State <> AsyncOperation then
            SetState(ProfileWaiting);
        raise;
    end;
end;
{ THE ALGORITHM IS background_search's, where its own domain assumption can be
  tested. Eighty-two lines lived here, touching no field of this class and
  calling nothing else on it, and HALF OF THEM HAD NEVER RUN: the only test that
  reached them fed data whose minimum sits at the left edge, so the entire
  leftward walk - and with it the concave-background assumption the whole thing
  rests on - was dead. }

function TFitService.ComputeCurvePositionsActual(SearchMinimums: boolean):
TTitlePointsSet;
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

    CheckAssigned(Data, 'the data record');
    Data.Sort;

    Result := TTitlePointsSet.Create(nil);
    try
        //  Repeatedly takes the lowest point among those not yet assigned to a
        //  peak.
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
            //  Prevents an endless loop when every point of the profile has
            //  been taken, which can happen when the lower bound of the peak
            //  search is set too small. THE EXIT MUST COME BEFORE THE POINT IS
            //  ADDED.
            if not ExtremumFound then
                Break;
            // pervyy naydennyy maksimum yavlyaetsya global'nym
            if Result.PointsCount = 0 then
                GlobalExtremum := ExtremumValue;
            Result.AddNewPoint(ExtremumX0, Data.PointYCoord[ExtremumIndex]);

            //  The peak's bounds, for computing the R-factor.
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
                    Break//  THE COMPARISON MUST BE <, otherwise the peaks run
            //  into one another.
            ;
            //  Widening the peak to the left, artificially.
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
                    Break;
            //  THE COMPARISON MUST BE <, otherwise the peaks run into one
            //  another.
            //  Widening the peak to the right, artificially.
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

            //  ??? The noise in the background should be estimated and the
            //  threshold set as a fraction of the maximum that exceeds it, with a
            //  way to enter that value by hand. What is wanted here is not a
            //  fraction of the maximum but an absolute value - a background may
            //  be present - and it should vary along the profile. (Simply put:
            //  the background should be removed first.)
        until ExtremumValue < GlobalExtremum / BackFactor;
    except
        Result.Free;
        raise;
    end;
end;

procedure TFitService.ComputeCurvePositionsForAutoAlg;
begin
    FCurvePositions.Free;
    FCurvePositions := nil;
    //  Every peak point becomes an anchor point for a curve.
    // TODO: use special value of TExtremumMode and generalize algorithm.
    FCurvePositions := ComputeCurvePositionsActual(False);
end;

procedure TFitService.ComputeCurvePositionsForAutoIfPlacedByPositions;
begin
    //  A CURVE TYPE PLACED FROM A POINT SET IS NOT PLACED FROM POSITIONS. It
    //  builds its instances from its own markup - the bounds of its own
    //  segments - and TFitTask never reads the position list for it. Filling
    //  that list here therefore does not help the fit in any way, and it is not
    //  harmless: the automatic algorithm makes one position per DATA POINT, and
    //  those positions are shown on the chart and returned to the client as the
    //  model's "Curve positions". The user saw a marker at every sample of a
    //  profile described by two patterns (D26).
    if SelectedCurvePlacedByPointSet <> '' then
    begin
        WriteLog(Format('positions: not computed automatically - the selected ' +
            'curve type is placed from the "%s" point set',
            [SelectedCurvePlacedByPointSet]), Notification);
        Exit;
    end;

    ComputeCurvePositionsForAutoAlg;
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
            CheckAssigned(Peaks, 'the peaks found in the profile');
            { Peaks collecton contains all data points having values different
              from some estimated average by defined value. }
            CheckThat(Peaks.PointsCount >= 3, 'even the narrowest peak needs three points to be located');

            Peaks.Sort; // !!!

            if FSelectedAreaMode then
                Data := FSelectedArea
            else
                Data := FExpProfile;
            CheckAssigned(Data, 'the data record');
            Data.Sort;
            //  Of the Peaks that ComputeCurvePositionsActual returned, only the
            //  local maxima within each peak are kept.
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
                            //  Guards against duplicates.
                            if FCurvePositions.IndexOfValueX(X) = -1 then
                                FCurvePositions.AddNewPoint(X, CurValue);
                        end
                        else
                        begin
                            X := Peaks.PointXCoord[i - 1];
                            //  Guards against duplicates.
                            if FCurvePositions.IndexOfValueX(X) = -1 then
                                FCurvePositions.AddNewPoint(X, PrevValue);
                        end;
                        PeakFound := True;
                    end;
                end
                else
                if not DerivativeChanged then
                    PeakFound := False;
                //  Look for a downward inflection.
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
    CheckAssigned(FCurvePositions, 'the curve positions');
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
        //  Back from AsyncOperation to the previous state.
        SetState(FSavedState);
        FState := FSavedState; //  needed on the way out of AsyncOperation
        //  MUST BE HERE and not in ComputeCurvePositions, because a descendant
        //  calls this method.
        GoToReadyForFit;
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeCurvePositionsDone;
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
    CheckAssigned(FCurvePositions, 'the curve positions');

    FCurvePositions.Clear;

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    CheckAssigned(Data, 'the data record');
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
    //  THE LIST IS NOT CLEARED, so the points found here are added to the ones
    //  the user picked.
    CheckAssigned(FRFactorBounds, 'the R-factor bounds');

    Peaks := ComputeCurvePositionsActual(False);
    try
        CheckAssigned(Peaks, 'the peaks found in the profile');
        //  Even the narrowest peak should have at least 3 points.
        //  TODO: every case must be handled.
        // CheckThat(Peaks.PointsCount >= 3, 'even the narrowest peak needs three points to be located');
        if Peaks.PointsCount = 0 then
            Exit;
        Peaks.Sort; // !!!

        if FSelectedAreaMode then
            Data := FSelectedArea
        else
            Data := FExpProfile;
        CheckAssigned(Data, 'the data record');

        Data.Sort;
        //  Of the Peaks that ComputeCurvePositionsActual returned, only the
        //  points bounding each peak are kept.
        First := False;
        for i := 0 to Data.PointsCount - 1 do
            if Peaks.IndexOfValueX(Data.PointXCoord[i]) <> -1 then
            begin
                //  A peak point.
                if not First then
                begin
                    //  The first is the left bound.
                    First := True;
                    //  Guards against duplicate points.
                    X     := Data.PointXCoord[i];
                    if FRFactorBounds.IndexOfValueX(X) = -1 then
                        FRFactorBounds.AddNewPoint(X, Data.PointYCoord[i]);
                end;
                //  The rest are skipped.
            end
            else
            if First then
            begin
                //  The previous point is the right bound.
                X := Data.PointXCoord[i - 1];
                if FRFactorBounds.IndexOfValueX(X) = -1 then
                    FRFactorBounds.AddNewPoint(X,
                        Data.PointYCoord[i - 1]);
                First := False;
            end;
            //  Not a peak point.
        if First then
        begin
            //  Every point was walked and no right bound was found, so the last
            //  point becomes one.
            X := Data.PointXCoord[i];
            //  Guards against duplicates.
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
        //  Back from AsyncOperation to the previous state.
        SetState(FSavedState);
        FState := FSavedState; //  needed on the way out of AsyncOperation
        //  MUST BE HERE and not in ComputeCurveBounds, because a descendant
        //  calls this method.
        GoToReadyForFit;
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeCurveBoundsDone;
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
    CheckAssigned(FBackgroundPoints, 'the background points');

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;

    CheckAssigned(Data, 'the data record');
    Data.Sort;

    Background := ProposeBackgroundPoints(Data);
    try
        CheckAssigned(Background, 'the background points to subtract');
        //  THE LIST MUST BE CLEARED to keep duplicates out. Without clearing it
        //  - to preserve the user's picks - every addition would have to check
        //  whether the point is already there.
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
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    RecreateMainCalcThread(ComputeCurvePositionsAlg,
        ComputeCurvePositionsDoneProcActual);
end;

function TFitService.SelectAllPointsAsCurvePositions: string;
begin
    Result := '';

    //  THE AUTOMATIC STRATEGY IS FOR PEAKS, NOT FOR WAVES.
    //
    //  Seeding a curve at every data point is how the automatic run has always
    //  started for ordinary curve types, and it stays that way for them: a peak's
    //  amplitude is seeded from the data and unwanted peaks are pruned away
    //  again, so starting from everywhere converges on something.
    //
    //  A pattern placed from a point set is not a peak. It spans a stretch the
    //  its amplitude is free, and nothing prunes it - so one pattern per bar is
    //  100 patterns and ~1200 free parameters on a 100-bar series, which is not a
    //  count of anything and which the user experiences as a hang. Until there is
    //  an automatic strategy built for waves - it will be a different algorithm,
    //  not this one - a pattern is placed by hand, by its bounds.
    if IsWavePatternTypeSelected then
        raise EUserException.Create(
            'Selecting every point as a curve position is meant for peak-like ' +
            'curves; for the selected type it would create one pattern per ' +
            'point.' + CRLF +
            //  The remedy names the module's own gesture, asked of the module,
            //  so this refusal stays true for a pack the engine has never heard
            //  of.
            'Mark it by its two ends instead, with the ' +
            PlacementGestureName + ' command.');

    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    RecreateMainCalcThread(SelectAllPointsAsCurvePositionsAlg,
        ComputeCurvePositionsDoneProcActual);
end;

function TFitService.ComputeCurveBounds: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    RecreateMainCalcThread(ComputeCurveBoundsAlg, ComputeCurveBoundsDoneProcActual);
end;

function TFitService.ComputeBackgroundPoints: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    RecreateMainCalcThread(ComputeBackgroundPointsAlg,
        ComputeBackgroundPointsDoneProcActual);
end;

procedure TFitService.ComputeBackgroundPointsDoneProcActual;
begin
    try
        //  Back from AsyncOperation to the previous state.
        SetState(FSavedState);
        FState := FSavedState;
        // trebuetsya pri perehode iz AsyncOperation
        if (not FDoneDisabled) and Assigned(FitProxy) then
            FitProxy.ComputeBackgroundPointsDone;
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
end;

//  Smoothing without a shift.
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
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(Assigned(ANeutronPointsSet), 'ANeutronPointsSet is missing when it is required');
    with ANeutronPointsSet do
    begin
        //  Normalising by the maximum keeps the largest amplitude and inflates
        //  the relative size of the lower peaks. Normalising by the SUM is
        //  preferable: the maximum intensity then decreases, as it should, which
        //  is easier to understand. The full sum is computed for that.
        SumBefore := 0;
        for i := 0 to PointsCount - 1 do
            SumBefore := SumBefore + PointYCoord[i];
        (*
          MaxBefore := PointYCoord[0];
          for i := 1 to PointsCount - 1 do
          if PointYCoord[i] > MaxBefore then
          MaxBefore := PointYCoord[i];
        *)
        //  Without this initialisation the start of the profile sags.
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
          //  Determines the maximum correctly.
          MaxAfter := PointYCoord[0];
          for i := 1 to PointsCount - 1 do
          if PointYCoord[i] > MaxAfter then
          MaxAfter := PointYCoord[i];
        *)
        SumAfter := 0;
        for i := 0 to PointsCount - 1 do
            SumAfter := SumAfter + PointYCoord[i];
        //  Normalisation.
        for i := 0 to PointsCount - 1 do
            PointYCoord[i] := PointYCoord[i] * SumBefore / SumAfter;
        // MaxBefore / MaxAfter;
    end;
end;

function TFitService.SmoothProfile: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

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

function TFitService.GetSpecialCurveParameters: Curve_parameters;
begin
    //  Return a copy: callers take ownership of the result (e.g. assign it to a
    //  Curve_type), and the service keeps owning FParams. Returning FParams
    //  directly made them share one object -> double-free / access violation.
    if Assigned(FParams) then
        Result := Curve_parameters(FParams.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCurvePositions: TTitlePointsSet;
begin
    if Assigned(FCurvePositions) then
        Result := TTitlePointsSet(FCurvePositions.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetResultedCurvePositions: TTitlePointsSet;
begin
    if Assigned(FResultedCurvePositions) then
        Result := TTitlePointsSet(FResultedCurvePositions.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCurves: TSelfCopiedCompList;
begin
    if Assigned(FCurves) then
        Result := TSelfCopiedCompList(FCurves.GetCopy)
    else
        Result := nil;
end;

function TFitService.GetCurveCount: longint;
begin
    CheckAssigned(FCurves, 'the curve list');
    Result := FCurves.Count;
end;

function TFitService.GetCurveInstanceId(ACurveIndex: longint): string;
begin
    Result := '';
    if not Assigned(FCurves) then
        Exit;
    if (ACurveIndex < 0) or (ACurveIndex >= FCurves.Count) then
        Exit;
    //  The WIRE form - no braces, because this is what goes into a URL path.
    Result := CurveInstanceIdToWire(
        TCurvePointsSet(FCurves.Items[ACurveIndex]).FInstanceId);
end;

{ WHERE THAT INSTANCE IS NOW - which is the whole point of addressing a curve by
  its handle rather than by a number. The order of FCurves is derived: it follows
  the fit intervals and the picks inside them, so adding a pick to the left of an
  existing one renumbers everything after it. A caller holding an index across
  such an edit silently addresses a different curve; a caller holding a handle
  either finds its own or is told it is gone. }
function TFitService.IsCurveFitted(ACurveIndex: longint): boolean;
begin
    Result := False;
    if not Assigned(FCurves) then
        Exit;
    if (ACurveIndex < 0) or (ACurveIndex >= FCurves.Count) then
        Exit;
    Result := IsFittedInstance(TCurvePointsSet(FCurves.Items[ACurveIndex]));
end;

function TFitService.IndexOfCurveInstance(const AInstanceId: string): longint;
var
    i:  longint;
    Id: TCurveInstanceId;
begin
    Result := -1;
    if not Assigned(FCurves) then
        Exit;
    //  Not an identifier at all - a malformed path segment. Refused rather than
    //  resolved to curve 0, which is what parsing it as a number used to do.
    if not TryStrToCurveInstanceId(AInstanceId, Id) then
        Exit;

    for i := 0 to FCurves.Count - 1 do
        if SameCurveInstanceId(TCurvePointsSet(FCurves.Items[i]).FInstanceId,
            Id) then
        begin
            Result := i;
            Exit;
        end;
end;

function TFitService.GetCurveParameterCount(ACurveIndex: longint): longint;
var
    CurveParameters: Curve_parameters;
begin
    CheckIndex(ACurveIndex, FCurveAttributes.Count, 'the curves in the model');

    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);
    Result := CurveParameters.Params.Count;
end;

function TFitService.GetCurveParameterValue(ACurveIndex: longint;
    ParamIndex: longint): Variant;
var
    CurveParameters: Curve_parameters;
begin
    Result := Null;
    if (ACurveIndex < 0) or (ACurveIndex >= FCurveAttributes.Count) then
        Exit;
    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);
    if (ParamIndex < 0) or (ParamIndex >= CurveParameters.Params.Count) then
        Exit;
    Result := CurveParameters[ParamIndex].TypedValue;
end;

procedure TFitService.GetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
    var Name: string; var Value: double; var Type_: longint);
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(ACurveIndex, FCurveAttributes.Count, 'the curves in the model');

    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);
    CheckIndex(ParamIndex, CurveParameters.Params.Count,
        'the parameters this curve carries');

    Parameter := CurveParameters[ParamIndex];
    Name      := Parameter.Name;
    Value     := Parameter.Value;
    Type_     := longint(Parameter.Type_);
end;

function TFitService.GetCurveParameterError(ACurveIndex: longint;
    ParamIndex: longint): double;
var
    CurveParameters: Curve_parameters;
begin
    Result := -1;
    if (ACurveIndex < 0) or (ACurveIndex >= FCurveAttributes.Count) then
        Exit;
    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);
    if (ParamIndex >= 0) and (ParamIndex < CurveParameters.Params.Count) then
        Result := CurveParameters[ParamIndex].Error;
end;

{ Writes one instance's values into the per-round report, by parameter NAME.

  Refuses a name the curve does not carry rather than dropping it: a name this
  build does not know means the project and this build disagree about the model,
  and a silently skipped value restores a curve missing exactly the number
  nobody will go looking for. }
procedure TFitService.WriteCurveValues(ACurveIndex: longint;
    const AParams: TCurveParamValues);
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
    i, j: longint;
    Found: boolean;
begin
    CheckIndex(ACurveIndex, FCurveAttributes.Count, 'the curves in the model');
    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);

    for i := 0 to High(AParams) do
    begin
        Found := False;
        for j := 0 to CurveParameters.Params.Count - 1 do
        begin
            Parameter := CurveParameters[j];
            if Parameter.Name <> AParams[i].Name then
                Continue;
            Parameter.Value := AParams[i].Value;
            Parameter.Error := AParams[i].Error;
            Found := True;
            Break;
        end;
        if not Found then
            raise EUserException.Create('The curve has no parameter named "' +
                AParams[i].Name + '".');
    end;
end;

function TFitService.SetCurveValues(const AEntries: TCurveValuesList): string;
var
    i, Fitted: longint;
    Ids: array of TCurveInstanceId;
begin
    CheckAssigned(FCurveAttributes, 'the curve attributes');

    //  EVERY ENTRY IS CHECKED AND WRITTEN BEFORE ANYTHING IS REBUILT, so a
    //  refusal in the middle of a restore does not leave half a model behind.
    for i := 0 to High(AEntries) do
        WriteCurveValues(AEntries[i].CurveIndex, AEntries[i].Params);

    //  WHICH INSTANCES CARRY OPTIMISER RESULTS. MarkFitted is absolute - it
    //  says "these and no others" - so the list is built over every entry that
    //  claims a fit rather than set one at a time.
    SetLength(Ids, 0);
    Fitted := 0;
    for i := 0 to High(AEntries) do
        if AEntries[i].Fitted then
        begin
            SetLength(Ids, Fitted + 1);
            Ids[Fitted] := TCurvePointsSet(
                FCurves.Items[AEntries[i].CurveIndex]).FInstanceId;
            Inc(Fitted);
        end;
    FIdentity.MarkFitted(Ids);

    //  ONCE, at the end. The values are in the report; they reach the curves
    //  because this rebuild hands each instance what is stored under its
    //  handle - the same order SetCurveParameter depends on, for the same
    //  reason, and the reason its guard test looks at the calculated profile.
    GoToReadyForFit;

    Result := '';
    if State = ProfileWaiting then
        Result := IsProfileWaiting
    else if State = ReadyForFit then
        Result := IsReadyForFit
    else if State = ReadyForAutoFit then
        Result := IsReadyForAutoFit;
end;

procedure TFitService.SetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
    Value: double);
var
    CurveParameters: Curve_parameters;
    Parameter: TSpecialCurveParameter;
begin
    CheckIndex(ACurveIndex, FCurveAttributes.Count, 'the curves in the model');

    CurveParameters := Curve_parameters(FCurveAttributes.Items[ACurveIndex]);
    CheckIndex(ParamIndex, CurveParameters.Params.Count,
        'the parameters this curve carries');

    Parameter := CurveParameters[ParamIndex];
    Parameter.Value := Value;
    //  THE ORDER IS WHAT MAKES THIS WORK, and it is not obvious. The value is
    //  written into the curve ATTRIBUTES - the per-round report - and reaches
    //  the curves themselves only because GoToReadyForFit rebuilds every
    //  instance and TFitTask.RestoreCurveValues hands each one the values
    //  stored under its handle. The report is regenerated from the curves
    //  afterwards, so the edit survives only because the restore happens first.
    //
    //  Guarded by TRestApiTest.AParameterWrittenByHandleReachesTheModel, which
    //  checks the CALCULATED PROFILE and not just the value read back: the
    //  profile is built from the curves, so it is the only thing that can tell
    //  a write that reached the model from one that reached only the report.
    //
    //  TODO: to avoid recomputing when several parameters change at once, the
    //  interface could gain a separate recompute function. That complicates the
    //  interface, though, and invites mistakes once third-party applications use
    //  it.
    GoToReadyForFit;
end;

procedure TFitService.SubtractBackgroundLinearly(Data: TPointsSet;
    StartIndex: longint; EndIndex: longint);
var
    i:     longint;
    Delta: double;
    I0:    double;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(EndIndex > StartIndex, 'a background stretch must end after it begins');
    CheckAssigned(Data, 'the data record');

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

procedure TFitService.SelectProfileIntervalActual(Points: TPointsSet;
    StartPointIndex, StopPointIndex: longint);
var
    i: longint;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(Points, 'the point set');
    CheckThat(Points.PointsCount <> 0, 'an interval cannot be selected from a set with no points');
    CheckThat((StartPointIndex >= 0) and (StopPointIndex < Points.PointsCount), 'the selected interval must lie inside the set it is taken from');
    CheckThat(Points <> FSelectedArea, 'an interval must be selected from the profile, not from the interval already selected');

    FSelectedArea.Free;
    FSelectedArea := nil;
    FSelectedArea := TTitlePointsSet.Create(nil);
    for i := StartPointIndex to StopPointIndex do
        FSelectedArea.AddNewPoint(Points.PointXCoord[i], Points.PointYCoord[i]);
end;

function TFitService.SelectProfileInterval(StartPointIndex, StopPointIndex:
    longint): string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));
    if FSelectedAreaMode then
    begin
        Result := RangeAlready;
        Exit;
    end;

    CheckAssigned(FExpProfile, 'the experimental profile');
    SelectProfileIntervalActual(FExpProfile, StartPointIndex, StopPointIndex);
    FSelectedAreaMode := True;
end;

function TFitService.SelectEntireProfile: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));
    if not FSelectedAreaMode then
    begin
        Result := EntireAlready;
        Exit;
    end;

    CheckAssigned(FExpProfile, 'the experimental profile');
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
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(Points, 'the point set');
    CheckThat((StartPointIndex >= 0) and (StopPointIndex < Points.PointsCount), 'the selected interval must lie inside the set it is taken from');

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
    CheckAssigned(Points, 'the point set');
    CheckAssigned(FCurveAttributes, 'the curve attributes');
    Integral := IntegrateWithBoundaries(Points, StartPointIndex, StopPointIndex);

    CurveParameters := Curve_parameters(Points.Parameters.GetCopy);
    try
        CurveParameters.FInstanceId := Points.FInstanceId;
        //  The calculated parameters are added.
        AddNewParameter('Integral', Integral);

        FCurveAttributes.Add(CurveParameters);

    except
        CurveParameters.Free;
        raise;
    end;
end;

{ FIGURES FOR THE MODEL AS A WHOLE, over every selected interval.

  Each interval is its own sub-task with its own stretch of profile, so each
  measures itself; the total then POOLS their parts and divides once, exactly as
  a single interval does. Summing the per-interval ratios - which is what these
  did - is not an aggregate of anything: two intervals reading 0.01 came to 0.02,
  so marking a third, well-fitted interval made the reported fit look worse. }
function TFitService.TotalLossParts: TLossParts;
var
    i: longint;
begin
    CheckAssigned(FTaskList, 'the fit task list');

    Result := Default(TLossParts);
    for i := 0 to FTaskList.Count - 1 do
        AddLossParts(Result, TFitTask(FTaskList.Items[i]).GetLossParts);
end;

{ Is the selected curve type placed by marking an extent rather than by one
  curve position? The task answers the same question for itself; asked here
  because only this level sees the intervals. A capability, not a type test
  (TNamedPointsSet.PlacedByPointSet). }
function TFitService.IsWavePatternTypeSelected: boolean;
var
    Cls: TCurveClass;
begin
    Cls := FindCurveClassById(FCurveTypeId);
    Result := Assigned(Cls) and (Cls.PlacedByPointSet <> '');
end;

function TFitService.PlacementGestureName: string;
var
    Cls: TCurveClass;
    Sink: IModulePointSink;
begin
    Result := 'marking';
    Cls := FindCurveClassById(FCurveTypeId);
    if not Assigned(Cls) then
        Exit;
    Sink := SinkNamed(Cls.PlacedByPointSet);
    if Assigned(Sink) then
        Result := Sink.DisplayName;
end;

{ ---------------------------- the module host ---------------------------- }

procedure TFitService.ModuleStateChanged;
begin
    GoToReadyForFit;
end;

function TFitService.AbortedToMakeRoom: string;
begin
    Result := '';
    if MustAbortRunningOperation(State) then
    begin
        AbortAsyncOper;
        Result := CalcAborted;
    end;
end;

procedure TFitService.CheckCanAcceptPicks;
begin
    RefuseIf(PickRefusal(State));
end;

{ Makes each registered module's state for this problem. Called once, when the
  problem is created, so a module's state has exactly the problem's lifetime. }
procedure TFitService.CreateModuleSessions;
var
    Mods: TAppModuleArray;
    i: longint;
begin
    Mods := RegisteredModules;
    SetLength(FModuleSessions, Length(Mods));
    for i := 0 to High(Mods) do
        FModuleSessions[i] := Mods[i].CreateSession(Self);
end;

function TFitService.AnyModuleContributesFitReadiness: boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(FModuleSessions) do
        if FModuleSessions[i].ContributesFitReadiness then
            Exit(True);
end;

function TFitService.SinkNamed(const AKind: string): IModulePointSink;
var
    i: longint;
    Sink: IModulePointSink;
begin
    Result := nil;
    for i := 0 to High(FModuleSessions) do
    begin
        Sink := FModuleSessions[i].PointSink;
        if Assigned(Sink) and (Sink.SetName = AKind) then
            Exit(Sink);
    end;
end;

procedure TFitService.AddPointToSet(const AKind: string; XValue, YValue: double);
var
    Sink: IModulePointSink;
begin
    Sink := SinkNamed(AKind);
    if not Assigned(Sink) then
        //  A USER-visible error, not an internal check: in a build without the
        //  module this is simply a request for something that is not installed,
        //  which is an ordinary outcome rather than a broken program.
        raise EUserException.CreateFmt(
            'No installed component collects "%s" points.', [AKind]);
    Sink.AddPoint(XValue, YValue);
end;

procedure TFitService.ReplacePointInSet(const AKind: string;
    PrevXValue, PrevYValue, NewXValue, NewYValue: double);
var
    Sink: IModulePointSink;
begin
    Sink := SinkNamed(AKind);
    if not Assigned(Sink) then
        raise EUserException.CreateFmt(
            'No installed component collects "%s" points.', [AKind]);

    //  ONLY WHEN THIS SET IS WHAT PLACES THE CURVES. A module may collect points
    //  for its own reasons, and refusing to move those would be refusing
    //  something that costs nothing. When the selected type IS placed from this
    //  set, moving a point re-derives every instance the markup placed - all of
    //  them with new seeds - so the whole model's fit goes, not one curve's.
    //  Asked of the capability rather than of a list of module names.
    if SameText(SelectedCurvePlacedByPointSet, AKind) then
        RefuseMarkupMoveThatWouldLoseTheFit(AnyCurveIsFitted);

    Sink.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

function TFitService.GetModuleProjectStates: TModuleStateArray;
var
    Mods: TAppModuleArray;
    Info: TModuleResource;
    Resource, Json: string;
    Answered: boolean;
    i, j, n: longint;
begin
    Result := nil;
    n := 0;
    Mods := RegisteredModules;
    for i := 0 to High(Mods) do
    begin
        Resource := Mods[i].Name + '/' + ProjectStateResource;
        //  ASKED ONLY IF DECLARED, so a build whose modules keep nothing does
        //  not go looking for a resource that does not exist.
        if not FindModuleResource(Resource, Info) then
            Continue;
        //  ASKED THROUGH THE SESSIONS RATHER THAN THROUGH ModuleGet, and that
        //  is not a shortcut: ModuleGet RAISES when no session answers, and a
        //  module that declares this resource may still have nothing to keep in
        //  a particular problem - a markup nobody has placed yet. Saving must
        //  not fail because a module had nothing to say. Declining is silence,
        //  not an error, exactly as it is for every other resource.
        Json := '';
        Answered := False;
        for j := 0 to High(FModuleSessions) do
            if FModuleSessions[j].TryGet(Resource, Json) then
            begin
                Answered := True;
                Break;
            end;
        if not Answered then
            Continue;
        SetLength(Result, n + 1);
        Result[n].Module := Mods[i].Name;
        Result[n].Content := Json;
        Inc(n);
    end;
end;

function TFitService.ModuleGet(const AResource: string): string;
var
    i: longint;
begin
    for i := 0 to High(FModuleSessions) do
        if FModuleSessions[i].TryGet(AResource, Result) then
            Exit;
    raise EUserException.CreateFmt(
        'No installed component answers "%s". This build offers: %s.',
        [AResource, KnownModuleResources]);
end;

function TFitService.ModulePost(const AResource, APayload: string): string;
var
    i: longint;
begin
    for i := 0 to High(FModuleSessions) do
        if FModuleSessions[i].TryPost(AResource, APayload, Result) then
            Exit;
    raise EUserException.CreateFmt(
        'No installed component answers "%s". This build offers: %s.',
        [AResource, KnownModuleResources]);
end;

function TFitService.PythonSidecarUrl: string;
begin
    Result := FPythonSidecarUrl;
end;

function TFitService.SelectedCurvePlacedByPointSet: string;
var
    Cls: TCurveClass;
begin
    Result := '';
    Cls := FindCurveClassById(FCurveTypeId);
    if Assigned(Cls) then
        Result := Cls.PlacedByPointSet;
end;

function TFitService.CurrentCurves: TSelfCopiedCompList;
begin
    Result := FCurves;
end;

function TFitService.GetTotalRFactor: double;
begin
    //  Must match TFitTask.GetRFactor, which reports the squared form.
    Result := GetTotalSqrRFactor;
end;

function TFitService.GetTotalAbsRFactor: double;
begin
    Result := LossFromParts(LOSS_KIND_RELATIVE, TotalLossParts);
end;

function TFitService.GetTotalSqrRFactor: double;
begin
    Result := LossFromParts(LOSS_KIND_RFACTOR, TotalLossParts);
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
        CheckAssigned(FTaskList, 'the fit task list');

        ShowCurMinInternal;

        if AllTasksDone then
        begin
            //  Runs on the server's main thread - the same one as ServerStub -
            //  so it may raise the same exceptions.
            CollectCurves;
            CollectCurveAttributes;
            CreateResultedCurvePositions;
            CreateResultedProfile;
            CreateDeltaProfile;

            //  AFTER CollectCurves, so the instances are the ones the fit
            //  actually left behind.
            RememberFittedInstances;
            //  ...and so are the picks. An automatic run deletes curves, and
            //  until this the picks it deleted them for stayed behind and
            //  seeded them all again on the next edit.
            AdoptCurveRemovalsFromTasks;

            FState   := FSavedState; // vossta. sost. predshestvovashee
            //  entering AsyncOperation
            FFitDone := True;
            SetState(Finished);
            if (not FDoneDisabled) and Assigned(FitProxy) then
                FitProxy.Done;
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
        //  Runs on the server's main thread - the same one as ServerStub - so
        //  it may raise the same exceptions.
        ShowCurMin(FCurrentMinimum);
    end;
end;

procedure TFitService.ShowProfile;
begin
    if Assigned(FitProxy) then
        FitProxy.ShowProfile;
end;

{$HINTS off}
{ TODO: remove Min from parameters. }
procedure TFitService.ShowCurMin(Min: double);
begin
    if Assigned(FitProxy) then
    begin
        { These calls are necessary for animation mode. }
        CreateResultedProfile;
        CollectCurves;
        FitProxy.ShowCurMin(FCurrentMinimum);
    end;
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
    //  MUST NOT RAISE: it is called from the interface methods.
    // CheckAssigned(FTaskList, 'the fit task list');
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

{ AN INTERVAL WITH NO WAVE IN IT IS REFUSED, not fitted.

  There is nothing to fit over it: the user asked for that stretch to be scored
  and drew no count across it. Left to proceed, the sub-task finds no bounds,
  falls through to the automatic path - which treats every data point as a curve
  position - and builds one pattern per bar. On a 100-bar stretch that is ~1200
  parameters, and the user experiences it as a hang.

  Checked when a FIT STARTS, not when the intervals are set. CreateTasks also
  runs on ordinary state transitions, so refusing there would make it impossible
  to mark the interval before drawing the pattern - a normal order of work.

  No intervals at all means the whole profile, which necessarily contains every
  pattern, so there is nothing to check. }
{ Asks every module whether each fit interval can be fitted with what it holds.

  The framework owns the intervals and does the iterating; a module owns the
  judgement and the wording. What it prevents is an interval fitted with no
  model at all, where the engine's auto-mode generates one curve per data point
  and the application appears to hang. }
procedure TFitService.RefuseIntervalsNoModuleAccepts;
var
    j, m: longint;
    Lo, Hi: double;
begin
    if Length(FModuleSessions) = 0 then
        Exit;
    CheckAssigned(FRFactorBounds, 'the R-factor bounds');
    if FRFactorBounds.PointsCount < 2 then
        Exit;

    FRFactorBounds.Sort;
    j := 0;
    while j + 1 <= FRFactorBounds.PointsCount - 1 do
    begin
        Lo := FRFactorBounds.PointXCoord[j];
        Hi := FRFactorBounds.PointXCoord[j + 1];
        for m := 0 to High(FModuleSessions) do
            FModuleSessions[m].CheckIntervalAllowed(Lo, Hi);
        Inc(j, 2);
    end;
end;

procedure TFitService.MinimizeNumberOfCurvesAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    //  Internal: raises no exception for an inadmissible state.
    RefuseIntervalsNoModuleAccepts;
    CreateTasks;
    InitTasks(True);
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
    RefuseIntervalsNoModuleAccepts;
    CreateTasks;
    InitTasks(True);
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
    CheckAssigned(FTaskList, 'the fit task list');

    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeDifferenceAgain;
    end;
end;

function TFitService.MinimizeNumberOfCurves: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    // if State <> ReadyForFit then
    // raise EUserException.Create(
    // InadmissibleServerState + CRLF + NotAllData);
    //  Instead of an error, the data that is needed is created.
    if FRFactorBounds.PointsCount < 2 then
    begin
        FRFactorBounds.Clear;
        ComputeCurveBoundsAlg;
    end;
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoIfPlacedByPositions;
    SetState(ReadyForFit);

    RecreateMainCalcThread(MinimizeNumberOfCurvesAlg, DoneProc);
end;

function TFitService.MinimizeDifferenceAgain: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));
    if State <> ReadyForFit then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            NotAllData);

    RecreateMainCalcThread(MinimizeDifferenceAgainAlg, DoneProc);
end;

function TFitService.MinimizeDifference: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

    FStartTime := Now;
    // if State <> ReadyForFit then
    // raise EUserException.Create(
    // InadmissibleServerState + CRLF + NotAllData);
    //  Instead of an error, the data that is needed is created.
    if FRFactorBounds.PointsCount < 2 then
    begin
        FRFactorBounds.Clear;
        ComputeCurveBoundsAlg;
    end;
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoIfPlacedByPositions;
    SetState(ReadyForFit);

    RecreateMainCalcThread(MinimizeDifferenceAlg, DoneProc);
end;

function TFitService.GetCurveAttributes: TMSCRCurveList;
begin
    Result := TMSCRCurveList(FCurveAttributes.GetCopy);
end;

procedure TFitService.CreateResultedProfile;
var
    i, j:      longint;
    FitTask:   TFitTask;
    PointsSet: TPointsSet;
    ScalingFactor: double;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FTaskList, 'the fit task list');
    CheckAssigned(FExpProfile, 'the experimental profile');

    FCalcProfile.Free;
    FCalcProfile := TTitlePointsSet.Create(nil);
    //  As many points as are needed.
    for i := 0 to FExpProfile.PointsCount - 1 do
        FCalcProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask   := TFitTask(FTaskList.Items[i]);
        PointsSet := FitTask.GetCalcProfile;

        CheckAssigned(PointsSet, 'the point set');
        CheckThat((FitTask.EndIndex - FitTask.BegIndex + 1) = PointsSet.PointsCount, 'a task must compute exactly as many points as the interval it was given');
        CheckIndex(FitTask.BegIndex, FCalcProfile.PointsCount,
            'the computed profile a task interval begins in');
        CheckIndex(FitTask.EndIndex, FCalcProfile.PointsCount,
            'the computed profile a task interval ends in');

        ScalingFactor := FitTask.GetScalingFactor;
        for j := FitTask.BegIndex to FitTask.EndIndex do
            FCalcProfile.PointYCoord[j] :=
                FCalcProfile.PointYCoord[j] + PointsSet.PointYCoord[j -
                FitTask.BegIndex] * ScalingFactor;
    end;
end;

{ WHERE THE BUILT MODEL'S CURVES ACTUALLY SIT - one point per instance, at the
  instance's own x0.

  https://github.com/dvmorozov/fit/issues/200

  Derived from FCurves, which is every collected instance whatever placed it, so
  this is one statement for every curve type rather than a special case for the
  types placed from their own point set. A peak seeded from a pick reports the x0
  it was fitted to; a pattern placed from its own markup reports the
  same thing, and neither needs the other's mechanism.

  IT DOES NOT WRITE FCurvePositions, and the distinction is the whole point. The
  picks are input - unique X, each a sample of the profile, and the seed an
  instance's fitted parameters are restored by. What is built is an OUTPUT that
  satisfies none of those: x0 is continuous and off the grid, and two instances
  may converge on one value. Writing the second into the first asserted in
  TPointsSet.Sort on the next redraw, failed the grid lookup in CreateTasks, and
  destroyed the parameter carry-over. Two meanings, two sets.

  It also no longer concatenates the sub-tasks' own position lists. Those are the
  slices of the picks each task was given, so summing them restated the input;
  FCurves is what the model turned out to be. }
procedure TFitService.CreateResultedCurvePositions;
var
    i, Index:  longint;
    Curve:     TCurvePointsSet;
    Data:      TPointsSet;
begin
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(FResultedCurvePositions, 'the resulted curve positions');

    FResultedCurvePositions.Clear;

    //  The profile the x0 values are to be read against - the selected area when
    //  one is in force, exactly as CreateTasks chooses it.
    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    if not Assigned(Data) or (Data.PointsCount = 0) then
        Exit;

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := TCurvePointsSet(FCurves.Items[i]);
        //  An instance with no position parameter is not anchored at an x, so
        //  there is no position to report for it.
        if not Curve.Hasx0 then
            Continue;

        //  NEAREST, not exact: x0 is a fitted parameter and moves off the grid,
        //  whereas a picked position was by construction a data point. The x
        //  reported is the curve's own; only the y - which exists to put the
        //  marker on the profile - is taken from the sample next to it.
        Index := Data.IndexOfNearestToX(Curve.x0);
        if Index = -1 then
            Continue;

        FResultedCurvePositions.AddNewPoint(Curve.x0, Data.PointYCoord[Index]);
    end;
end;

procedure TFitService.CreateDeltaProfile;
var
    i, j:      longint;
    FitTask:   TFitTask;
    PointsSet: TPointsSet;
    ScalingFactor: double;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FTaskList, 'the fit task list');
    CheckAssigned(FExpProfile, 'the experimental profile');

    FDeltaProfile.Free;
    FDeltaProfile := nil;
    FDeltaProfile := TTitlePointsSet.Create(nil);
    //  As many points as are needed.
    for i := 0 to FExpProfile.PointsCount - 1 do
        //  FILLED WITH ZERO: where there is no calculated profile the R-factor
        //  is not computed either, so a difference would mean nothing.
        FDeltaProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask   := TFitTask(FTaskList.Items[i]);
        PointsSet := FitTask.GetCalcProfile;

        CheckAssigned(PointsSet, 'the point set');
        CheckThat((FitTask.EndIndex - FitTask.BegIndex + 1) = PointsSet.PointsCount, 'a task must compute exactly as many points as the interval it was given');
        CheckIndex(FitTask.BegIndex, FCalcProfile.PointsCount,
            'the computed profile a task interval begins in');
        CheckIndex(FitTask.EndIndex, FCalcProfile.PointsCount,
            'the computed profile a task interval ends in');

        ScalingFactor := FitTask.GetScalingFactor;
        for j := FitTask.BegIndex to FitTask.EndIndex do
            FDeltaProfile.PointYCoord[j] :=
                FExpProfile.PointYCoord[j] - PointsSet.PointYCoord[j -
                FitTask.BegIndex] * ScalingFactor;
    end;
end;

procedure TFitService.CollectCurves;
var
    i, j, k:   longint;
    FitTask:   TFitTask;
    TaskCurves: TSelfCopiedCompList;
    ScalingFactor: double;
    CurveCopy: TNamedPointsSet;
begin
    CheckAssigned(FTaskList, 'the fit task list');

    FCurves.Free;
    FCurves := TSelfCopiedCompList.Create;

    { Collect all curves into single list. }
    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask := TFitTask(FTaskList.Items[i]);
        ScalingFactor := FitTask.GetScalingFactor;
        TaskCurves := FitTask.GetCurves;

        CheckAssigned(TaskCurves, 'the curves a finished task built');

        for j := 0 to TaskCurves.Count - 1 do
        begin
            CurveCopy := TNamedPointsSet(TNamedPointsSet(TaskCurves.Items[j])
                .GetCopy);
            //  TODO: move scaling into separate method.
            for k := 0 to CurveCopy.PointsCount - 1 do
                CurveCopy.PointYCoord[k] :=
                    CurveCopy.PointYCoord[k] * ScalingFactor;

            CurveCopy.FTitle := CurveCopy.GetCurveTypeName + ' [' +
                IntToStr(i + 1) + ',' + IntToStr(j + 1) + ']';
            FCurves.Add(CurveCopy);
        end;
    end;
end;

procedure TFitService.SetState(AState: TFitServerState);
var
    ModuleIndex: longint;
begin
    { Every state change reinitializes data, so a wrong transition silently
      discards the problem - log them all. }
    WriteLog(Format('state %s -> %s', [FitServerStateName(FState),
        FitServerStateName(AState)]), Debug);
    //  Initialising the data is a side effect. Doing that initialisation for
    //  each state independently of the previous one is both simpler and more
    //  reliable.
    case AState of
        //  Waiting for profile data after a load. THE SERVER'S STATE MUST BE
        //  RETURNED COMPLETELY to what it was at start-up, and it must be
        //  possible to enter this state again.
        ProfileWaiting:
        begin
            CheckAssigned(FExpProfile, 'the experimental profile');
            CheckAssigned(FBackgroundPoints, 'the background points');
            CheckAssigned(FRFactorBounds, 'the R-factor bounds');
            CheckAssigned(FCurvePositions, 'the curve positions');
            CheckAssigned(FResultedCurvePositions,
                'the resulted curve positions');
            CheckAssigned(FCurveAttributes, 'the curve attributes');
            //  So that points can be added through the table without entering
            //  the special mode.
            FExpProfile.Clear;
            // !!! ne dolzhen udalyat'sya pri vhode v BackNotRemoved !!!
            FBackgroundPoints.Clear;
            FRFactorBounds.Clear;
            FCurvePositions.Clear;
            FResultedCurvePositions.Clear;
            FCurveAttributes.Clear;
            //  A new profile has had no fit. FFitDone is deliberately left
            //  alone: it is read elsewhere and resetting it here would be a
            //  behaviour change beyond this one.
            //  The model goes with the picks it was keyed on. Left behind, a
            //  handle issued for the old profile would be inherited by an
            //  instance of the new one that happens to sit at the same x.
            FIdentity.Clear;
            FCurves.Clear;
            //  A module's picks are made on the profile exactly like the points
            //  above, so they belong to the profile that is going away. Left
            //  behind they are not merely stale: a markup would be placed over
            //  stretches of the new data that have nothing to do with what the
            //  user marked, and nothing would say so.
            for ModuleIndex := 0 to High(FModuleSessions) do
                FModuleSessions[ModuleIndex].Reset;

            FSelectedArea.Free;
            FSelectedArea := nil;
            //  ...and with the selected area gone, the flag that says "work on
            //  the selected area" has to go too - otherwise every later use of
            //  the data would resolve to the nil interval instead of the profile.
            FSelectedAreaMode := False;
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        //  The background has not been subtracted yet (the profile and/or the
        //  selected area are loaded).
        BackNotRemoved:
        begin
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        //  A long operation is running.
        AsyncOperation: ;
        //  The background has been subtracted: ready to fit curves
        //  automatically.
        ReadyForAutoFit:
        begin
            FCalcProfile.Free;
            FCalcProfile := nil;
            FDeltaProfile.Free;
            FDeltaProfile := nil;
            FTaskList.Free;
            FTaskList := nil;
        end;
        //  Ready to fit under the given restrictions.
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
        FSavedState := AState; //  to be restored afterwards
end;

function TFitService.GetState: TFitServerState;
begin
    Result := FState;
end;

procedure TFitService.SetWaveLength(AWaveLength: double);
begin
    CheckAssigned(FCurveAttributes, 'the curve attributes');

    FWaveLength := AWaveLength;
    FCurveAttributes.FWaveLength := WaveLength;
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

procedure TFitService.SetCurveType(ACurveTypeId: TCurveTypeId);
begin
    //  REFUSE AN UNKNOWN TYPE. TCurveTypesSingleton.SelectCurveType leaves the
    //  previous selection in place when the id is not registered - silently, with
    //  no error and no log - so a client asking for a type this build does not
    //  have got a DIFFERENT curve type and no indication of it.
    //
    //  That is how a whole vertical once stayed dead in fit_server: the
    //  pattern units were not linked, SetCurveType did nothing, no pattern was
    //  ever built, and the fit degenerated into auto-mode with one curve per data
    //  point - which presented to the user as the server hanging. Every layer
    //  degraded quietly instead of one of them saying no.
    if not Assigned(FindCurveClassById(ACurveTypeId)) then
        raise EUserException.Create(
            'This server does not support the curve type ' +
            GUIDToString(ACurveTypeId) + '. It is not registered in this ' +
            'build, so no curve of that type could be created.');

    //  Stored HERE and nowhere else. Driving the process-wide selector as well
    //  looked harmless - the desktop menu reads it - but it made one problem's
    //  choice leak into every other problem in the same server, and into every
    //  problem created afterwards, since a new service seeds from it. The menu
    //  checkmark is the CLIENT's business (TFitClient.SelectCurveType keeps the
    //  client-side registry in step); a server must not mutate global state on
    //  behalf of one session.
    //  A DIFFERENT SHAPE HAS NO FITTED VALUES. The parameters of one curve type
    //  mean nothing to another, so whatever a fit found before this is not a
    //  result for the model that exists after it - and a move must not be refused
    //  on the strength of it.
    if not IsEqualGUID(FCurveTypeId, ACurveTypeId) then
        FIdentity.MarkFitted([]);

    FCurveTypeId := ACurveTypeId;
end;

function TFitService.GetLossKind: longint;
begin
    Result := FLossKind;
end;

procedure TFitService.SetLossKind(AKind: longint);
begin
    //  Validated here rather than at the point of use: an unknown kind reaching
    //  the engine raises mid-fit, which is a much worse place to find out.
    if not IsKnownLoss(AKind) then
        //  Lists what this build actually offers rather than a numeric range:
        //  the objectives need not be contiguous once one can be contributed
        //  from outside fit_loss, and a range would name values that do not
        //  exist while excluding one that does.
        raise EUserException.Create(Format(
            'Unknown loss function %d. This build offers: %s.',
            [AKind, KnownLossNames]));
    FLossKind := AKind;
end;

function TFitService.GetMinimizerKind: longint;
begin
    Result := FMinimizerKind;
end;

procedure TFitService.SetMinimizerKind(AKind: longint);
begin
    FMinimizerKind := AKind;
end;

function TFitService.GetWeighting: string;
begin
    Result := FWeighting;
end;

procedure TFitService.SetWeighting(const AValue: string);
begin
    FWeighting := AValue;
end;

function TFitService.GetServerUrl: string;
begin
    Result := FServerUrl;
end;

procedure TFitService.SetServerUrl(const AUrl: string);
begin
    FServerUrl := AUrl;
end;

procedure TFitService.SetPythonSidecarUrl(const AUrl: string);
begin
    FPythonSidecarUrl := AUrl;
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
    //  The user's curve selection is preserved.
    //  https://action.mindjet.com/task/14588987
    //  Everything else the user chose is discarded.
    FRFactorBounds.Clear;
    FBackgroundPoints.Clear;

    if FSavedState = BackNotRemoved then
    begin
        //  On repeated runs the background is not subtracted again.
        SubtractBackground(True);
        ShowProfile;
    end;
    // TODO: mozhno optimizirovat' razbiv na nesk. funktsiy
    //  and calling ComputeCurvePositionsActual only once

    // Set of curve positions selected by user is saved if provided.
    // https://action.mindjet.com/task/14588987
    // https://github.com/dvmorozov/fit/issues/12
    if FCurvePositions.PointsCount = 0 then
        ComputeCurvePositionsForAutoIfPlacedByPositions;

    ComputeCurveBoundsAlg;
    MinimizeNumberOfCurvesAlg;
end;

function TFitService.DoAllAutomatically: string;
begin
    Result := AbortedToMakeRoom;

    RefuseIf(ProfileRefusal(State));

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
var
    ScalingEnabled: boolean;
begin
    //  Curve scaling is the native engine's own trick (fit shape, then scale the
    //  amplitude to match the integral). An out-of-process backend fits the whole
    //  model including amplitude, so scaling must be off - otherwise the profile
    //  rebuild would scale an already-fitted amplitude. Applied at creation so it
    //  holds for every task instance, including those recreated to rebuild the
    //  profile after the fit.
    ScalingEnabled := FCurveScalingEnabled and
        MinimizerSupportsCurveScaling(FMinimizerKind);
    Result := TFitTask.Create(nil, FBackgroundVariationEnabled, ScalingEnabled);
    //  THIS problem's curve type, so a task never reads the process-wide
    //  selection and two problems in one server stay independent.
    Result.CurveTypeId := FCurveTypeId;
    Result.MinimizerKind := FMinimizerKind;
    Result.LossKind := FLossKind;
    Result.Weighting := FWeighting;
    Result.ServerUrl := FServerUrl;
    Result.PythonUrl := FPythonSidecarUrl;
end;

procedure TFitService.CreateTasks;
var
    i, j:    longint;
    FitTask: TFitTask;
    Data, Temp: TPointsSet;
    BegIndex, EndIndex, PosIndex: longint;
    ModuleIndex: longint;
begin
    //  REFUSE A USER-DEFINED TYPE WITH NO FORMULA. This is the one place every
    //  operation that builds curves passes through, so it is the one place the
    //  check has to be. Without it the service went on fitting whatever formula
    //  it happened to hold - including one belonging to a user curve the client
    //  had already deleted, which then reappeared in the result as yet another
    //  "User Defined" curve with no entry in the menu to explain it.
    //
    //  Reachable in two ways: the deleted curve was the one being fitted (the
    //  client then calls ClearSpecialCurve), or the type was restored from the
    //  settings on start-up, where the formula is not stored at all.
    if IsEqualGUID(FCurveTypeId, TUserPointsSet.GetCurveTypeId) and
       (Trim(FCurveExpr) = '') then
        raise EUserException.Create('The selected curve type is a user-defined ' +
            'curve, but no formula is set for it - the curve it was created ' +
            'from no longer exists.' + CRLF +
            'Select a curve type that does exist (Model \ Curve Type) and try ' +
            'again.');

    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FRFactorBounds, 'the R-factor bounds');
    CheckAssigned(FCurvePositions, 'the curve positions');
    //  Do as much of the task as possible: no sub-task is created for a final
    //  unclosed interval. That is simpler than refusing outright, checking and
    //  reporting.
    // CheckThat(FRFactorBounds.PointsCount mod 2 = 0, 'fit_service: FRFactorBounds.PointsCount mod 2 = 0');

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    CheckAssigned(Data, 'the data record');
    Data.Sort;

    FRFactorBounds.Sort;
    FCurvePositions.Sort;

    //  THE ONE PLACE EVERY PATH THAT BUILDS CURVES PASSES THROUGH, which is why
    //  the model is brought back into step with the picks here rather than in
    //  each of the verbs that change them.
    SyncIdentityToPicks;

    FTaskList.Free;
    FTaskList := nil;
    FTaskList := TComponentList.Create;

    //  THE DEFAULT DATA INTERVAL IS THE WHOLE PROFILE.
    //
    //  Sub-tasks are built from the data intervals, so with none defined there
    //  would be nothing to fit - a fit that silently produces no curves. An
    //  undefined interval means "no restriction", not "no data", so the entire
    //  profile becomes one interval here.
    //
    //  Materialised into FRFactorBounds rather than handled as a special case
    //  further down: everything else (the sub-task loop, the summary table, the
    //  chart overlay, the statistics) then sees one ordinary interval, and the
    //  user can see on the chart what is actually being fitted. One point in the
    //  set is an interval the user has not finished, and counts as undefined.
    if FRFactorBounds.PointsCount < 2 then
    begin
        FRFactorBounds.Clear;
        FRFactorBounds.AddNewPoint(Data.PointXCoord[0], Data.PointYCoord[0]);
        FRFactorBounds.AddNewPoint(
            Data.PointXCoord[Data.PointsCount - 1],
            Data.PointYCoord[Data.PointsCount - 1]);
        WriteLog('No data interval defined; the whole profile is taken as one.',
            Notification);
    end;

    //  DATA INTERVALS ARE DISJOINT BY DESIGN - this is an invariant to
    //  preserve, not an accident of the encoding.
    //
    //  Each interval becomes its own TFitTask: a sub-problem over its own
    //  stretch of the profile, with its own curves, minimized INDEPENDENTLY of
    //  the others. That independence is the point. It is what makes the sub-
    //  tasks parallelizable, and the intended unit of parallelism when the
    //  distributed/GPU compute stage exploits it.
    //
    //  Two consequences follow, and both are deliberate:
    //    * the bounds may be sorted and walked in consecutive pairs, because for
    //      disjoint intervals the sorted order IS the pairing - overlapping
    //      bounds simply collapse into different, still-disjoint intervals,
    //      which is the intended behaviour rather than corruption;
    //    * an odd trailing point is an interval the user has not finished, and
    //      is skipped.
    //
    //  Do NOT reuse this encoding for items that may overlap. A module whose
    //  items nest or share endpoints cannot use it for exactly that reason.
    j := 0;
    while j <= FRFactorBounds.PointsCount - 1 do
    begin
        //  The count can be odd when the user changes the interval bounds after
        //  the pattern instances were created, which has to be handled properly.
        //  No sub-task is created for an unclosed interval.
        if j + 1 > FRFactorBounds.PointsCount - 1 then
            Break;
        FitTask := CreateTaskObject;
        try
            BegIndex := Data.IndexOfValueX(FRFactorBounds.PointXCoord[j]);
            EndIndex := Data.IndexOfValueX(FRFactorBounds.PointXCoord[j + 1]);
            CheckThat(BegIndex <> -1, 'the start of a marked interval must fall on a sample of the profile');
            CheckThat(EndIndex <> -1, 'every background point must fall on a sample of the profile');

            FitTask.BegIndex := BegIndex;
            FitTask.EndIndex := EndIndex;
            //  Copy the profile interval in.
            Temp := TPointsSet.Create(nil);
            try
                for i := BegIndex to EndIndex do
                    Temp.AddNewPoint(Data.PointXCoord[i], Data.PointYCoord[i]);
                FitTask.SetProfilePointsSet(Temp);
            except
                Temp.Free;
                raise;
            end;
            //  Copy in the subset of positions that fall inside the interval.
            Temp := TPointsSet.Create(nil);
            try
                for i := 0 to FCurvePositions.PointsCount - 1 do
                begin
                    PosIndex :=
                        Data.IndexOfValueX(FCurvePositions.PointXCoord[i]);
                    CheckThat(PosIndex <> -1, 'every curve position must fall on a sample of the profile');

                    if (PosIndex >= BegIndex) and (PosIndex <= EndIndex) then
                        Temp.AddNewPoint(FCurvePositions.PointXCoord[i],
                            FCurvePositions.PointYCoord[i]);
                end;
                FitTask.SetCurvePositions(Temp);
            except
                Temp.Free;
                raise;
            end;
            //  The patterns belonging to this interval, filtered like the curve
            //  positions are. A pattern is never split across sub-tasks: both of
            //  its ends must fall inside, because half a pattern has no meaning.
            for ModuleIndex := 0 to High(FModuleSessions) do
                FitTask.AddModuleState(
                    FModuleSessions[ModuleIndex].SliceForInterval(
                        Data.PointXCoord[BegIndex], Data.PointXCoord[EndIndex]));
            //  The remaining parameters.
            FitTask.MaxAcceptableRFactor := MaxRFactor;

            //  The formula is what the user-defined type IS - the type alone
            //  says nothing about the shape to fit. See the guard at the top of
            //  this method for why it cannot be missing here.
            if IsEqualGUID(CurveTypeId, TUserPointsSet.GetCurveTypeId) then
                FitTask.SetSpecialCurve(FCurveExpr,
                    Curve_parameters(FParams.GetCopy));

            //  BORROWED, not given: the model outlives every task, which is
            //  the whole reason identity is kept on the service.
            FitTask.Identity      := FIdentity;
            //  Which interval this is - all a positionless instance has to be
            //  keyed by, having no pick to be keyed by.
            FitTask.IntervalIndex := FTaskList.Count;
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
begin
    InitTasks(False);
end;

{ AForFitting says whether an unfittable markup is an error or just an empty
  picture - see TFitTask.FRefuseUnfittable. }
procedure TFitService.InitTasks(AForFitting: boolean);
var
    i:  longint;
    FT: TFitTask;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FTaskList, 'the fit task list');
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.RefuseUnfittable := AForFitting;
        //  Not optimal, but it does for re-initialising when anchor points of
        //  pattern instances are added or removed.
        FT.RecreateCurves(FCurveAttributes);
        //  The model that has just been built decides what may be done to it -
        //  and it decides it HERE, not when a fit starts, because what is built
        //  here is also what the user is shown. Curve scaling is the case that
        //  matters: it multiplies the whole model onto the profile, so a model
        //  that sets its own amplitude was drawn at one scale before the fit and
        //  another after it. A pattern placed between two picked points then
        //  appeared nowhere near them, which reads as a pattern that was not
        //  drawn at all.
        FT.EnforceLossCompatibility;
        FT.ComputeProfile;
    end;
end;

procedure TFitService.CollectCurveAttributes;
var
    NS:   TCurvePointsSet;
    StartPointIndex, StopPointIndex: longint;
    i, j: longint;
begin
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(FCurveAttributes, 'the curve attributes');

    FCurveAttributes.Clear;

    for i := 0 to FCurves.Count - 1 do
    begin
        NS := TCurvePointsSet(FCurves.Items[i]);
        //  WHERE THIS CURVE EXISTS, as a fact the server states rather than one
        //  the chart has to infer. A curve is carried as a point per profile
        //  sample whatever its shape, so the array alone cannot say where the
        //  curve is and where it is merely absent.
        //
        //  STRICTLY above the threshold. With the default threshold of 0 the
        //  old `>=` test was satisfied by the value 0 itself, so a compactly
        //  supported curve - one that is EXACTLY zero outside its own extent -
        //  reported the whole profile as its own. Drawn, it then stepped from
        //  its level to zero at the edge of its support, which on data that
        //  sits far from zero is a vertical line the height of the chart. The
        //  additive model was right throughout; only the extent was wrong.
        //
        //  Nothing changes for an analytic curve: a Gaussian's tails are small
        //  but never exactly 0, so `> 0` still admits them and a user-chosen
        //  threshold still cuts them where it did.
        //  FIRST AND LAST above the threshold, not the first RUN of them. The
        //  scan used to stop at the first point that fell back below it, which
        //  makes the extent depend on what the curve does in the MIDDLE: a
        //  nested bounded component contributes its shape minus its own
        //  chord, so it is zero wherever it crosses that chord - and the run
        //  would end at the first such crossing, cutting the curve off in the
        //  middle of itself. For a single-humped peak the two definitions
        //  coincide, which is why the difference never showed before.
        StartPointIndex := -1;
        StopPointIndex := -1;
        for j := 0 to NS.PointsCount - 1 do
            if Abs(NS.PointYCoord[j]) > CurveThresh then
            begin
                if StartPointIndex = -1 then
                    StartPointIndex := j;
                StopPointIndex := j;
            end;

        //  A curve that is nowhere above a threshold of ZERO is flat zero, and
        //  it stays in the list with its whole range - which is what it got
        //  before. MEMBERSHIP MUST NOT DEPEND ON SHAPE: the wire indexes the
        //  curve points by FCurves and their parameters by FCurveAttributes,
        //  so a curve missing from one of them makes every later index name a
        //  different curve in the two lists.
        if (StartPointIndex = -1) and (CurveThresh = 0) and
           (NS.PointsCount > 0) then
        begin
            StartPointIndex := 0;
            StopPointIndex := NS.PointsCount - 1;
        end;

        if StartPointIndex <> -1 then
            //  Curves with too little intensity are left out of the list.
            AddCurveToList(NS, StartPointIndex, StopPointIndex);
    end;
end;

procedure TFitService.CreateCurveList;
begin
    //  An interface method, so it checks the state.
    RefuseIf(ResultRefusal(FFitDone));
    try
        CollectCurveAttributes;
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

function TFitService.GetCurveType: TCurveTypeId;
begin
    //  This problem's own type, NOT the global selection.
    Result := FCurveTypeId;
end;

{ See IFitService.ClearSpecialCurve. }
procedure TFitService.ClearSpecialCurve;
begin
    RefuseIf(BusyRefusal(State));

    FCurveExpr := '';
    //  Kept assigned but empty: everything here treats FParams as a live object
    //  (CreateParameters fills it in place), only its content is now unknown.
    FParams.Free;
    FParams := Curve_parameters.Create(nil);
    FParams.Params.Clear;
end;

procedure TFitService.SetSpecialCurveParameters(ACurveExpr: string;
    CP: Curve_parameters);
var
    i: longint;
    FitTask: TFitTask;
begin
    RefuseIf(PickRefusal(State));

    if not Assigned(CP) then // pervonach. initsializatsiya
        CreateParameters(ACurveExpr)
    else
    begin
        //  At this point it is no longer an admissible user error - it is a
        //  fatal program error.
        CheckThat(Length(ACurveExpr) <> 0, 'a special curve needs an expression to evaluate');
        FParams.Free;
        FParams := CP;
    end;
    //  Fill the value in if no exception was raised.
    FCurveExpr := ACurveExpr;

    if Assigned(FTaskList) then
        for i := 0 to FTaskList.Count - 1 do
        begin
            FitTask := TFitTask(FTaskList.Items[i]);
            FitTask.SetSpecialCurve(FCurveExpr,
                Curve_parameters(FParams.GetCopy));
        end;
end;

{ Neither of these can find an operation to stop: TFitService runs a task to
  completion inside RecreateMainCalcThread, so by the time any other request is
  served the operation is over. They are implemented all the same, because THIS
  is the class the REST worker instantiates (fit_server_session.pas) - only the
  in-process desktop uses the threaded subclasses that do the real thing here.
  Left abstract, they did not read as "must override" to that worker; they read
  as "Abstract method called", which is what the user was shown when a file was
  opened while the problem was marked busy. }
procedure TFitService.StopAsyncOper;
begin
    ClearStaleAsyncOperation;
end;

procedure TFitService.AbortAsyncOper;
begin
    ClearStaleAsyncOperation;
end;

procedure TFitService.ClearStaleAsyncOperation;
begin
    //  The same refusal the threaded services give: asked to stop when nothing
    //  was started.
    RefuseIf(AbortRefusal(State));

    //  Marked busy with nothing running. RecreateMainCalcThread now restores the
    //  state itself when a task raises, so this should no longer be reachable -
    //  but recovering beats refusing every request from here on because of a
    //  flag no operation will ever clear.
    WriteLog('the problem was marked busy with nothing running; state restored to ' +
        FitServerStateName(FSavedState), Warning);
    FState := FSavedState;
end;

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

function TFitService.GetStatistics: TFitStatistics;
begin
    Result := ServiceStatistics(Self);
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

{ THE RULES AND THE FIVE REFUSALS BELONG TO user_formula_parameters, which is where
  they can be tested: what a user's formula declares is a question about an
  expression and five strings, and it had none of this service's business in it.
  A hundred and thirty lines lived here and not one had ever been executed by a
  test - the naming convention, the starting-value probe and every message the
  user reads while looking at their own text. }
procedure TFitService.CreateParameters(ACurveExpr: string);
begin
    DiscoverFormulaParameters(ACurveExpr, FParams);
end;

//  CALLING THIS AGAIN WITH THE SAME COORDINATES DELETES THE POINT. The two
//  picks of a range gesture are one add and one delete, which is why a module
//  that collects PAIRS of picks must not route them through here.
//  THE BULK PATH'S ENTRY POINT, and the difference from AddPoint below is the
//  whole reason it exists.
//
//  Both keep a pick set free of duplicate abscissae, which is the invariant
//  findings.md names AddPoint as the one place for: CreateTasks looks every pick
//  up in the data, every instance is seeded from one, and the pick carries the
//  handle that instance's fitted values are handed back by. Two entries at one x
//  would mean two instances where the model has one.
//
//  What differs is what a REPEAT means. Interactively it is the user clicking
//  the same sample twice, and taking the point away is the gesture - AddPoint's
//  own header says so, and the client mirrors it. In a bulk write it is a
//  malformed input, and annihilating it leaves NO point at that x, which is not
//  a meaning any caller asked for. Reached by SetCurvePositions and
//  SetRFactorBounds, which say "these are the picks".
procedure TFitService.AdoptOfferedIdentity(const AHandle: string;
    ASeed: double);
var
    Offered, Existing: TCurveInstanceId;
begin
    //  "I have none for this one" - ordinary, and not an error.
    if Trim(AHandle) = '' then
        Exit;

    if not TryStrToCurveInstanceId(AHandle, Offered) then
        raise EUserException.Create('"' + AHandle +
            '" is not a curve identifier.');

    //  ONE HANDLE PER PICK, mirroring SetPointUnique's one point per abscissa.
    //  A repeated abscissa in one write collapses to a single point and the
    //  later value wins, so the later handle must win too - otherwise the picks
    //  and the identities disagree about how many curves there are.
    //
    //  The same-handle case is left to Adopt rather than removed and re-added,
    //  because removing it would discard the entry's Fitted flag - and the
    //  commonest write of all is the one that offers each pick the handle it
    //  already has.
    Existing := FIdentity.IdForSeed(ASeed);
    if IsCurveInstanceId(Existing) and
       not SameCurveInstanceId(Existing, Offered) then
        FIdentity.RemoveSeed(ASeed);

    FIdentity.Adopt(Offered, ASeed);
end;

procedure TFitService.SetPointUnique(var Points: TTitlePointsSet;
    XValue, YValue: double);
var
    i: longint;
begin
    CheckAssigned(Points, 'the point set');

    for i := 0 to Points.PointsCount - 1 do
        if Abs(XValue - Points.PointXCoord[i]) <= TINY then
        begin
            //  The later value wins, which is the only reading of "these are
            //  the picks" that keeps every abscissa the caller named.
            Points.PointYCoord[i] := YValue;
            Exit;
        end;
    Points.AddNewPoint(XValue, YValue);
end;

procedure TFitService.AddPoint(var Points: TTitlePointsSet; XValue, YValue: double);
var
    i: longint;
begin
    CheckAssigned(Points, 'the point set');

    //  Look for the given point in the selected set.
    for i := 0 to Points.PointsCount - 1 do
        if Abs(XValue - Points.PointXCoord[i]) <= TINY then
        begin
            if Abs(YValue - Points.PointYCoord[i]) <= TINY then
                Points.DeletePoint(XValue)
            //  Replace the value.
            else
                Points.PointYCoord[i] := YValue;
            Exit;
        end;
    //  Not found, so it is a new one.
    Points.AddNewPoint(XValue, YValue);
end;

procedure TFitService.AddPointToProfile(XValue, YValue: double);
begin
    RefuseIf(BusyRefusal(State));

    CheckAssigned(FExpProfile, 'the experimental profile');
    AddPoint(FExpProfile, XValue, YValue); // dobavlyaet i udalyaet tochki

    if FExpProfile.PointsCount = 0 then
        SetState(ProfileWaiting)
    else
        SetState(BackNotRemoved);
end;

procedure TFitService.AddPointToBackground(XValue, YValue: double);
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FBackgroundPoints, 'the background points');
    AddPoint(FBackgroundPoints, XValue, YValue);
end;

{ Is there anything saved for this instance that a rebuild would put back?

  The question is asked of the ACTUAL state rather than of a "has a fit run" flag.
  FFitDone exists and looks like the flag for this, but it is set once and never
  reset - not even by SetState(ProfileWaiting) - so keying on it would refuse
  moves for the rest of the session, including on a freshly loaded profile. }
procedure TFitService.AdoptCurveRemovalsFromTasks;
var
    i, j, k:  longint;
    FT:       TFitTask;
    Data:     TPointsSet;
    Survivors: TPointsSet;
    X:        double;
    PosIndex: longint;
    Kept:     boolean;
    Doomed:   array of double;
begin
    if not Assigned(FTaskList) then
        Exit;
    CheckAssigned(FCurvePositions, 'the curve positions');

    if FSelectedAreaMode then
        Data := FSelectedArea
    else
        Data := FExpProfile;
    if not Assigned(Data) then
        Exit;

    //  Collected first, deleted afterwards: DeletePoint replaces the whole
    //  point set object, so removing while walking it would walk a freed one.
    SetLength(Doomed, 0);

    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        Survivors := FT.GetCurvePositions;
        if not Assigned(Survivors) then
            Continue;

        for j := 0 to FCurvePositions.PointsCount - 1 do
        begin
            X := FCurvePositions.PointXCoord[j];
            //  Only picks this task was responsible for. A pick outside every
            //  fit interval belongs to no task, and no task can have an opinion
            //  about it.
            PosIndex := Data.IndexOfValueX(X);
            if (PosIndex < FT.BegIndex) or (PosIndex > FT.EndIndex) then
                Continue;

            Kept := False;
            for k := 0 to Survivors.PointsCount - 1 do
                if Abs(Survivors.PointXCoord[k] - X) <= TINY then
                begin
                    Kept := True;
                    Break;
                end;

            if not Kept then
            begin
                SetLength(Doomed, Length(Doomed) + 1);
                Doomed[High(Doomed)] := X;
            end;
        end;
    end;

    if Length(Doomed) = 0 then
        Exit;

    for i := 0 to High(Doomed) do
    begin
        WriteLog(Format('positions: the automatic run removed the curve ' +
            'seeded at %g, so its pick goes with it', [Doomed[i]]),
            Notification);
        FCurvePositions.DeletePoint(Doomed[i]);
        FIdentity.RemoveSeed(Doomed[i]);
    end;
end;

procedure TFitService.SyncIdentityToPicks;
var
    Seeds: array of double;
    i: longint;
begin
    CheckAssigned(FCurvePositions, 'the curve positions');

    //  WHAT SURVIVES IS THE REGISTRY'S RULE, not this method's - see
    //  TCurveIdentityRegistry.KeepOnlySeeds. The loop was here, where nothing
    //  could ask it anything without an engine, a module and a rebuild; it kept
    //  positionless instances and dropped everything else, which quietly
    //  included every instance an analysis pack had placed - those have a seed
    //  and no pick, and a pack's pick set is empty.
    SetLength(Seeds, FCurvePositions.PointsCount);
    for i := 0 to FCurvePositions.PointsCount - 1 do
        Seeds[i] := FCurvePositions.PointXCoord[i];
    FIdentity.KeepOnlySeeds(Seeds);

    //  The handles themselves are issued on demand, where the instance is
    //  built (TFitTask.IdentifyCurve), because only there is it known whether
    //  the curve type has a position at all.
end;

function TFitService.IsFittedInstance(ACurve: TCurvePointsSet): boolean;
begin
    Result := Assigned(ACurve) and FIdentity.IsFitted(ACurve.FInstanceId);
end;

{ Called where a fit COMPLETES, and nowhere a model is merely rebuilt. The
  handles are the same ones RestoreCurveValues matches on, so recording them
  here is exactly recording "there are optimiser results filed under these
  instances". }
procedure TFitService.RememberFittedInstances;
var
    i:   longint;
    Ids: array of TCurveInstanceId;
begin
    SetLength(Ids, 0);
    if Assigned(FCurves) then
    begin
        SetLength(Ids, FCurves.Count);
        for i := 0 to FCurves.Count - 1 do
            Ids[i] := TCurvePointsSet(FCurves.Items[i]).FInstanceId;
    end;
    FIdentity.MarkFitted(Ids);
end;

function TFitService.AnyCurveIsFitted: boolean;
var
    i: longint;
begin
    Result := False;
    if not Assigned(FCurves) then
        Exit;

    for i := 0 to FCurves.Count - 1 do
        if IsFittedInstance(TCurvePointsSet(FCurves.Items[i])) then
        begin
            Result := True;
            Exit;
        end;
end;

{ The decision itself lives in fit_advice, with the wording - so the UI can ask
  the same question and get the same answer instead of a copy that drifts.

  ONLY FOR A MODULE'S OWN MARKUP now. Moving a picked curve position is allowed:
  the pick carries an identity that the move takes with it, so its curve keeps
  the shape the optimiser found and is re-seeded where the user put it. A
  markup point places every instance at once and has no such correspondence -
  see fit_advice.AdviseMoveMarkupPoint. }
procedure TFitService.RefuseMarkupMoveThatWouldLoseTheFit(
    AAnyCurveIsFitted: boolean);
var
    Reason: string;
begin
    if AdviseMoveMarkupPoint(AAnyCurveIsFitted, Reason) then
        Exit;

    WriteLog('positions: a markup move was refused - it would have rebuilt ' +
        'every curve and discarded the fit', Notification);
    raise EUserException.Create(Reason);
end;

procedure TFitService.GoToReadyForFit;
begin
    if State = ProfileWaiting then
        Exit;
    SetState(ReadyForAutoFit); // !!! udalyaet podzadachi !!!

    if // proverka nuzhna, t.k. imenno takomy
       //  This combination means the model is ready to be fitted with the
       //  user's own parameters.

    //  A data interval is NOT required: none means "the whole profile", which
    //  CreateTasks materialises. Requiring one here made a placed pattern
    //  invisible - the user had defined what to fit, but not where, and the
    //  where has a perfectly good default.
    ((FRFactorBounds.PointsCount <> 0) or AnyModuleContributesFitReadiness) and
    //  ...or wave bounds, which define whole patterns rather than seed points.
    //  Without this a module's own point set fell straight through: its sink
    //  called this, the guard failed for want of curve POSITIONS, no tasks were
    //  built and no curves collected - so a pattern the user had just placed was
    //  invisible until they pressed Fit.
    ((FCurvePositions.PointsCount <> 0) or AnyModuleContributesFitReadiness) then
    begin
        //  The state change comes FIRST, because entering ReadyForFit RESETS the
        //  computed data - it frees FCalcProfile, FDeltaProfile and the task
        //  list. It used to come last, so every profile computed just above was
        //  destroyed by the line that concluded building it, leaving the service
        //  in ReadyForFit with FCalcProfile permanently nil.
        //
        //  That silently disabled two things. Candidate ranking reads the placed
        //  model's statistics, which need the calculated profile, so EVERY
        //  candidate came back "not scored" and the complexity penalty - the
        //  whole basis of the ranking - did nothing. And the wave overlay takes
        //  pivot y-values from the cumulative profile, which was therefore nil.
        //
        //  Nothing failed; both produced empty or flat results, which is exactly
        //  the quiet degradation Stage 3E exists to remove.
        SetState(ReadyForFit); // !!! udalyaet podzadachi !!!

        //  The sub-tasks and the pattern instances have to be rebuilt, because
        //  the bounds change.
        CreateTasks; // !!! sozdayutsya vremenno !!!
        InitTasks;

        CollectCurves;
        CreateResultedCurvePositions;
        CreateResultedProfile;
        CreateDeltaProfile;
        CollectCurveAttributes;
    end;
end;

procedure TFitService.AddPointToRFactorBounds(XValue, YValue: double);
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FRFactorBounds, 'the R-factor bounds');
    AddPoint(FRFactorBounds, XValue, YValue);
    GoToReadyForFit;
end;

procedure TFitService.AddPointToCurvePositions(XValue, YValue: double);
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FCurvePositions, 'the curve positions');
    AddPoint(FCurvePositions, XValue, YValue);
    //  This could be done better, but that needs the sub-tasks to exist already
    //  and the list of interval bounds to be non-empty.
    GoToReadyForFit;
end;

procedure TFitService.ReplacePointInProfile(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    RefuseIf(BusyRefusal(State));

    if FSelectedAreaMode then
    begin
        CheckAssigned(FSelectedArea, 'the selected area');
        FSelectedArea.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    end
    else
    begin
        CheckAssigned(FExpProfile, 'the experimental profile');
        FExpProfile.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    end;
    //  Data can only be added or changed here, so FExpProfile.PointsCount = 0
    //  need not be checked.
    SetState(BackNotRemoved);
end;

procedure TFitService.ReplacePointInBackground(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);

begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FBackgroundPoints, 'the background points');
    FBackgroundPoints.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    //  Data can only be added or changed here, so FBackgroundPoints.PointsCount
    //  = 0 need not be checked.
    SetState(BackNotRemoved);
end;

procedure TFitService.ReplacePointInRFactorBounds(PrevXValue, PrevYValue,
    NewXValue, NewYValue: double);
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FRFactorBounds, 'the R-factor bounds');
    FRFactorBounds.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
end;

procedure TFitService.ReplacePointInCurvePositions(PrevXValue,
    PrevYValue, NewXValue, NewYValue: double);
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FCurvePositions, 'the curve positions');

    //  THE IDENTITY MOVES WITH THE PICK, and it must move BEFORE the pick set
    //  is touched. Afterwards a move is indistinguishable from deleting one
    //  point and adding another, which is exactly what used to make this
    //  operation lose a fit - and what it was refused for.
    //
    //  The curve keeps everything the optimiser found about its shape and is
    //  re-seeded at the new position; TFitTask.RestoreCurveValues is where that
    //  distinction is applied.
    FIdentity.TakeSeedFrom(PrevXValue, NewXValue);
    FCurvePositions.ReplacePoint(PrevXValue, PrevYValue, NewXValue, NewYValue);
    //  REBUILD, so the moved curve is where the user just put it. Without this
    //  the pick moved on the chart and the model did not - until some later
    //  edit happened to rebuild it.
    GoToReadyForFit;
end;

{ Removes one curve, named by its position in the model.

  THE MIRROR OF AdoptCurveRemovalsFromTasks. That method exists because the
  optimiser removes curves and their picks have to go with them; this one is the
  same act asked for by the user instead of decided by the fitter, so it is
  written as its reflection rather than as a second mechanism.

  WHY THE PICK GOES TOO. Every model edit reaches GoToReadyForFit, which rebuilds
  every instance from the picks: RecreateCurves drops an instance whose position
  is no longer among them and creates one for every pick that has none. Dropping
  only the identity would leave the pick standing, and the next rebuild would put
  a fresh instance there with a NEW handle - the curve back, unfitted, and the
  deletion undone.

  A CURVE WITH NO POSITION has no pick to remove: its handle was issued against a
  fit interval rather than a sample, so the identity is dropped and the rebuild
  is left to decide what the model holds. That is one instance per interval for
  such a type, which is what it was before the delete as well - so the honest
  answer is to say so rather than to pretend one instance of it can be singled
  out. }
{ Asks every module to drop what placed AId, and says whether one did.

  The FIRST to claim it wins and the rest are not asked: an instance is placed
  once, by one module, and asking on would let a second module remove markup for
  a handle it does not own. }
function TFitService.AskModulesToRemoveInstance(const AId: TCurveInstanceId;
    out ARemoved: TInstanceHandles): boolean;
var
    i: longint;
begin
    Result := False;
    ARemoved := nil;
    for i := 0 to High(FModuleSessions) do
        if FModuleSessions[i].TryRemoveInstance(CurveInstanceIdToWire(AId),
            ARemoved) then
        begin
            WriteLog(Format('curves: the "%s" module dropped the markup that '
                + 'placed %s', [FModuleSessions[i].Kind,
                CurveInstanceIdToWire(AId)]), Notification);
            Exit(True);
        end;
end;

function TFitService.DeleteCurve(ACurveIndex: longint): string;
var
    Id:    TCurveInstanceId;
    i:     longint;
    Pos:   longint;
    Entry: TCurveIdentity;
    Seed:  double;
    { Every instance a module took with the one asked for - a pattern nested in
      it has no leg left to hang from. Removed after the curve itself, so the
      index this was asked about stays valid until then. }
    WentWithIt: TInstanceHandles;
    Other: longint;
    OtherId: TCurveInstanceId;
begin
    RefuseIf(PickRefusal(State));

    CheckAssigned(FCurves, 'the curves this model holds');
    CheckAssigned(FCurvePositions, 'the curve positions');
    CheckIndex(ACurveIndex, FCurves.Count, 'the curve to delete');

    Id := TCurvePointsSet(FCurves.Items[ACurveIndex]).FInstanceId;

    //  THE REGISTRY'S PUBLIC SURFACE, enumerated. Its own IndexOfId is private,
    //  and the entry is wanted here for one thing only - whether this instance
    //  was placed by a pick, and which one.
    Pos := -1;
    for i := 0 to FIdentity.Count - 1 do
        if SameCurveInstanceId(FIdentity.Item(i).Id, Id) then
        begin
            Pos := i;
            Break;
        end;

    if Pos < 0 then
        //  An instance the registry never issued a handle to cannot be told
        //  apart from any other, so there is nothing safe to remove. It is a
        //  fault rather than a refusal: every instance is identified as it is
        //  built (TFitTask.IdentifyCurve).
        raise EUserException.Create(
            'This curve carries no handle, so it cannot be removed on its own.');

    Entry := FIdentity.Item(Pos);

    //  WHAT HAS TO GO WITH IT is a rule about identity and is answered by the
    //  registry, where it can be asked without an engine, a module and a
    //  rebuild. What is left here is the doing.
    case RemovalOf(Entry) of
        crMarkupThatPlacedIt:
        begin
            //  PLACED FROM A MODULE'S MARKUP, so the markup is what has to lose
            //  it: the instance is rebuilt from that markup on every model edit,
            //  and removing the curve and its handle alone would delete it for
            //  exactly as long as it takes GoToReadyForFit below to put it back.
            //  Asked of the modules rather than decided here - only the one that
            //  placed it knows which mark produced it, and what else has to go
            //  with it.
            if not AskModulesToRemoveInstance(Id, WentWithIt) then
                //  A REFUSAL IN WORDS, not a deletion that undoes itself. No
                //  module claimed the instance, so the markup that placed it is
                //  not reachable from here, and going ahead would report success
                //  and show the curve again a moment later.
                raise EUserException.Create('This curve was placed by an '
                    + 'analysis pack from its own markup, and no pack '
                    + 'recognises it, so removing it here would last only '
                    + 'until the next rebuild.');
            FIdentity.RemoveId(Id);
        end;
        crIdentityOnly:
        begin
            WriteLog('curves: removing the identity of a curve with no ' +
                'position; what the model holds is decided by the rebuild',
                Notification);
            FIdentity.RemoveId(Id);
        end;
        crPickAndIdentity:
        begin
            Seed := Entry.Seed;
            WriteLog(Format('curves: removing the curve seeded at %g, and the ' +
                'pick it was seeded from', [Seed]), Notification);
            //  BOTH, together, in the order AdoptCurveRemovalsFromTasks uses.
            FCurvePositions.DeletePoint(Seed);
            FIdentity.RemoveId(Id);
        end;
    end;

    //  AND OUT OF WHAT THE MODEL REPORTS, which GoToReadyForFit does NOT do.
    //
    //  This claimed the state change rebuilt the curve list. It does not:
    //  CollectCurves rebuilds it, and only a finished fit calls that. So the
    //  deleted curve went on being reported by GetCurves - the panel kept
    //  showing it, the chart kept drawing it, and a second attempt on the same
    //  row found an instance whose identity had already gone and was told "this
    //  curve carries no handle". The pick was removed and nothing looked like
    //  it.
    //
    //  Removed by index from both lists, because they are paired by position -
    //  that pairing is how the attributes row's handle is found in the first
    //  place, a few lines above.
    if ACurveIndex < FCurveAttributes.Count then
        FCurveAttributes.Delete(ACurveIndex);
    FCurves.Delete(ACurveIndex);

    //  AND EVERYTHING THAT WENT WITH IT, for a curve a module placed: removing
    //  the markup can remove more instances than the one named - a pattern
    //  nested in the deleted one has no leg left to hang from - and the rebuild
    //  below refreshes the reported list only while something still describes
    //  the model. With the last markup gone nothing does, so a child whose
    //  parent had just been deleted went on being drawn, hanging from nothing.
    //
    //  BY HANDLE, one at a time, because each removal renumbers the lists.
    for Other := 0 to High(WentWithIt) do
    begin
        Pos := IndexOfCurveInstance(WentWithIt[Other]);
        if Pos < 0 then
            //  Already gone: the one the caller named is in this list too, and
            //  a module may report an instance the model never built.
            Continue;
        if Pos < FCurveAttributes.Count then
            FCurveAttributes.Delete(Pos);
        FCurves.Delete(Pos);
    end;
    for Other := 0 to High(WentWithIt) do
        if TryStrToCurveInstanceId(WentWithIt[Other], OtherId) then
            FIdentity.RemoveId(OtherId);

    //  AND THE FITTED POSITION MARKERS, which are DERIVED from the curve list
    //  and were left behind: after deleting every curve one marker stayed on
    //  the chart, in a series whose legend row said "Fitted positions" while
    //  the model held nothing to have a fitted position. Rebuilt rather than
    //  poked at, by the function that owns the derivation - the same one both
    //  other callers use after collecting curves.
    CreateResultedCurvePositions;

    //  THEN the state, so a fit that follows rebuilds from the picks that are
    //  left rather than from the ones that were there when this began.
    GoToReadyForFit;

    Result := '';
    if State = ProfileWaiting then
        Result := IsProfileWaiting
    else if State = ReadyForFit then
        Result := IsReadyForFit
    else if State = ReadyForAutoFit then
        Result := IsReadyForAutoFit;
end;

procedure TFitService.RecreateMainCalcThread(ACurrentTask: TThreadMethod;
    ADoneProc: TThreadMethod);
var
    Started: TDateTime;

    function PointsCountOf(APoints: TPointsSet): longint;
    begin
        if Assigned(APoints) then
            Result := APoints.PointsCount
        else
            Result := -1;   //  not allocated
    end;

begin
    CheckThat(Assigned(ACurrentTask), 'ACurrentTask is missing when it is required');
    CheckThat(Assigned(ADoneProc), 'ADoneProc is missing when it is required');

    if State = AsyncOperation then
        AbortAsyncOper;
    FDoneDisabled := False;
    { The done procedures restore FSavedState, which SetState only records on
      the transition into AsyncOperation. Without entering that state the
      restore would fall back to the state preceding the current one
      (ProfileWaiting after SetProfile), whose handler clears the problem. }
    SetState(AsyncOperation);

    Started := Now;
    try
        ACurrentTask;
    except
        //  A TASK THAT RAISES NEVER REACHES ADoneProc - and ADoneProc is what
        //  takes the problem back out of AsyncOperation. Without this the problem
        //  stayed marked busy for the rest of its life: the client's status bar
        //  read "Minimising R-factor" forever, and the next request that tries to
        //  interrupt that phantom operation - opening another data file, say -
        //  went to AbortAsyncOper and died there.
        //
        //  Restored the way DoneProc restores it: the state that preceded the
        //  operation, assigned directly rather than through SetState, because
        //  nothing was computed and there is nothing to reinitialise.
        FState := FSavedState;
        WriteLog(Format('operation failed after %d ms; state restored to %s',
            [MilliSecondsBetween(Now, Started), FitServerStateName(FState)]),
            Warning);
        raise;
    end;
    ADoneProc;

    { The operation's whole effect on the problem, so an action that reports
      success while producing nothing is visible in the log. }
    WriteLog(Format('operation done in %d ms; state %s; profile %d, ' +
        'background %d, positions %d, bounds %d, curves %d points',
        [MilliSecondsBetween(Now, Started), FitServerStateName(FState),
        PointsCountOf(FExpProfile), PointsCountOf(FBackgroundPoints),
        PointsCountOf(FCurvePositions), PointsCountOf(FRFactorBounds),
        FCurves.Count]), Notification);
end;

end.
