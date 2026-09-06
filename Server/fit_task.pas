// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class representing single optimization task.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_task;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    asym_pseudo_voigt_points_set, background_parameters, Classes,
    curve_identity_registry,
    curve_instance_id, curve_points_set, curve_types_singleton, Math,
    downhill_simplex_minimizer, gauss_points_set, int_curve_type_selector,
    int_curve_factory,
    int_minimizer, log, lorentz_points_set,
    mscr_specimen_list, named_points_set,
    doniach_sunjic_points_set, emg_points_set, moffat_points_set, pearson7_points_set,
    skewed_gaussian_points_set, step_points_set, voigt_points_set,
    persistent_curve_parameter_container, persistent_curve_parameters,
    points_set, pseudo_voigt_points_set, self_copied_component,
    special_curve_parameter, SysUtils,
    //  The two weighting names, and what an unrecognised one means.
    fit_weighting,
    two_branches_pseudo_voigt_points_set, typinfo

    , user_points_set, int_fit_service, fit_loss, loss_compatibility, fit_advice
    //  The concrete pattern types are NOT named here any more. They used to be,
    //  purely so they were linked into fit_server - an engine unit carrying a
    //  linkage dependency that nothing connected to what it protected. That job
    //  belongs to curve_type_registration, which names every curve type in one
    //  place AND verifies at start-up that they are present (Stage 3D).
    , checks, MyExceptions, int_app_module
    , curve_builder_registry;

type
    { Fits profile interval by model curves.
      Provides variable parameters and evaluation function for optimization
      algorithm.
      It is inherited from TComponent to allow inserting into TComponentList. }
    TFitTask = class(TComponent)
    protected
        FBegIndex:    longint;
        FEndIndex:    longint;
        { Enables curve scaling. Generally it should be true, otherwise
          optimization could stuck in local minimum. However it could be
          set to false for some special curve types. }
        FCurveScalingEnabled: boolean;
        { Maximal acceptable value of R-factor for minimizing number of curves. }
        FMaxAcceptableRFactor:  double;
        FCurveTypeSelector: ICurveTypeSelector;
        { THIS task's curve type. Seeded from the global selection so an
          in-process caller behaves as before, but owned here: the selector is a
          process-wide SINGLETON, so a server handling two problems had one
          problem's curve type silently become the other's. }
        FCurveTypeId: TCurveTypeId;
        { WHO ISSUES THE HANDLES this task's instances are identified by.
          BORROWED, never owned: the model it records outlives every task, which
          is the whole reason identity is kept there and not here.

          nil is admissible and ordinary - a task built outside a service (the
          marshalling path rebuilds one from a wire problem, and tests build
          bare ones) fits instances that are never rebuilt, so they need no
          identity. IdentifyCurve says what that means. }
        FIdentity: TCurveIdentityRegistry;
        { The fallback used when no service supplied one - see IdentityRegistry.
          OWNED, unlike FIdentity, and the only one this class frees. }
        FOwnedIdentity: TCurveIdentityRegistry;
        { Which fit interval this task is, among the intervals the service
          split the profile into. Only an instance that NO pick places needs it
          - the user-defined formula curve, of which there is one per interval,
          and which is therefore keyed by the interval instead. }
        FIntervalIndex: longint;
        { Expression defining user curve type. }
        FCurveExpr:   string;
        { Parameters of user defined curve. Parameters are given from the caller. 
          The object is used to construct curve instances. }
        FUserDefinedParameters: Curve_parameters;
        { Part of experimental profile corresponding to model interval. }
        FExpProfile:  TPointsSet;
        { List of background points. }
        FBackground:  TPointsSet;
        FSavedBackground: TPointsSet;
        FBackgroundWasSaved: boolean;
        { The calculated profile. Every value is calculated as a sum of values 
          of corresponding points of every curve (specimen) and background. }
        FCalcProfile: TPointsSet;
        { Contains positions of curves. Only X-coordinates are used. }
        FCurvePositions: TPointsSet;
        { Set of curves used to model experimental data inside given interval. }
        FCurves:      TSelfCopiedCompList;
        FModuleStates: array of IModuleTaskState;
        { REFUSING IS FOR FITTING, not for looking. RecreateCurves runs on
          ordinary state changes too - the client asks for a placed pattern to be
          drawn before anything is fitted - and a markup that is not yet fittable
          is normal then: the user may mark an interval before drawing the
          pattern inside it. Refusing there makes a legitimate order of work
          impossible, so a preview builds nothing and only a real fit refuses. }
        FRefuseUnfittable: boolean;
        { List of parameters of curves which are common for all the instances. }
        FCommonVariableParameters: Curve_parameters;
        { Background parameters. }
        FA, FB, FC, Fx0: double;

        //  ========================= optimizer data ===========================
        FMinimizer: TMinimizer;

        { Index of pattern instance (specimen) parameters of which are variated at the moment. }
        FCurveNum: longint;
        { Index of parameter of pattern instance which is variated at the moment. }
        FParamNum: longint;
        FEndOfCycle: boolean;
        { Flag signalling to terminate all internal loops. }
        FTerminated: boolean;
        { Index of common parameter which is variated at the moment. }
        FCommonVaryingIndex: longint;
        { Index of background point amplitude of which is variated at the moment. }
        FBackgroundVaryingIndex: longint;
        { Flag indicating that common parameters are variated at the moment. }
        FCommonVaryingFlag: boolean;
        { Flag indicating that amplitudes of background points are variated at the moment. }
        FBackgroundVaryingFlag: boolean;
        { Enables background variation. }
        FEnableBackgroundVariation: boolean;
        { Selected minimizer algorithm (MIN_KIND_* constant). Drives Optimization. }
        FMinimizerKind: longint;
        { Objective being minimised (LOSS_KIND_* constant). Defaults to the legacy
          R-factor, so a task that never sets it behaves exactly as before (D2). }
        FLossKind: longint;
        { Residual weighting for the Python backend ('poisson'/'none'); ignored by
          the native engine. }
        FWeighting: string;
        { When non-empty, the fit is performed by the standalone compute server at
          this URL instead of the in-process engine. }
        FServerUrl: string;
        { URL of the Python (lmfit) sidecar, used when MinimizerKind selects it. }
        FPythonUrl: string;

        FShowCurMin: TShowCurMin;
        FDoneProc:   TThreadMethod;
        function GetProfileIntegral: double;
        function GetCalcProfileIntegral: double;

        { Methods which are used by the optimizer. }

        { Calculates R-factor. }
        function GetFunc: double;
        { Computes evaluation function. }
        procedure ComputeFunc;
        { Returns initial variation step for current variable parameter. }
        function GetVariationStep: double;
        { Does nothing. Should be implemented because is used by pointer.
          See OnSetStep. }
        procedure SetVariationStep(NewStepValue: double);
        { Moves iteration to next variable parameter. }
        procedure SetNextParam;
        { Sets iteration to the first variable parameter. }
        procedure SetFirstParam;
        { Returns variable parameter value. }
        function GetParam: double;
        { Sets variable parameter value. }
        procedure SetParam(NewParamValue: double);
        { Returns True at the end of iteration cycle. }
        function EndOfCycle: boolean;
        { Gathers the calculated and observed values actually being compared -
          the points inside the union of the curve ranges, or all of them when no
          range is set. Used by every loss except the legacy R-factor, which keeps
          its own inlined copy so it cannot drift (D2). }
        { The points every figure is measured over - see CollectFittedPoints. }
        procedure CollectFittedPoints(out ACalc, AObs: TLossDoubleArray);
        { Calculates R-factor used for optimization. }
        function GetOptimizingRFactor: double;
        { Calculates R-factor used for comparison with maximal acceptable value. }
        function GetRFactor: double;

    protected
        { Current minimum value of R-factor by which maximal acceptable value is set up. 
          Last achived minimum value is stored to avoid redundant computations and locks
          in multithreaded environment. }
        FCurMin:    double;
        FCurSqrMin: double;
        FCurAbsMin: double;
        FCurMinInitialized: boolean;
        { Flag indicating that asynchronous operation executed as subtask was Terminated.
          For this class is always True for now because the class does not support asynchronous operations. }
        FAllDone:   boolean;

        { These methods notify service about computation progress. }
        { Notifies service about achievement of new minimum value.
          This method recomputes data if necessary to be in
          consistent state with minimum R-factor value. }
        procedure ShowCurMin; virtual;
        { Notifies service about finishing computation. }
        procedure Done; virtual;

        { Algorithms are methods executed asynchronously. }

        { Searches for set of curves fitting experimental profile with given accuracy
          sequentially decreasing number of curves. }
        procedure MinimizeNumberOfCurvesAlg;
        { Executes cycle of fitting of parameters of curves. }
        procedure Optimization;

        { Low-level methods of algorithms. }

        procedure BackupCurveParameters;
        procedure RestoreCurveParameters;
        { Sums all pattern instances and FBackground into single calculated profile. }
        procedure ComputeCurveSum;
        procedure AddCurveToProfile(PS: TCurvePointsSet);
        { Removes from list of curve positions those points
          for which calculated curves have zero amplitude. }
        function DeleteCurvesWithSmallAmplitude: boolean;
        { Deletes from list of curve positions the point
          in which amplitude of curve is minimal. }
        function DeleteCurveWithMinimalAmplitude(var Deleted: TCurvePointsSet): boolean;
        { Removes from list of curve positions the point
          in which experimental profile has maximal derivative.  }
        function DeleteCurveWithMaxExpDerivative(var Deleted: TCurvePointsSet): boolean;

        { Auxiliary methods. }
        { Deletes poins with given X from the list passed via parameter. }
        procedure DeletePoint(var Points: TPointsSet; XValue: double);
        procedure AddPointToCurvePositions(XValue: double);
        { Creates the downhill simplex optimization algorithm. }
        procedure CreateDHSMinimizer;
        { Who issues this task's handles: the service's registry when there is
          one, and a private one of its own when there is not.

          A TASK WITHOUT A SERVICE IS NOT A TASK WITHOUT IDENTITY. The
          marshalling path rebuilds a task from a wire problem, and tests build
          bare ones; both rebuild curves and both restore values onto them, so
          both need handles that are stable across those rebuilds. Skipping
          identity when nobody supplied a registry made RestoreCurveValues a
          no-op in exactly those cases - quietly, which is the failure this
          whole mechanism exists to remove. }
        function IdentityRegistry: TCurveIdentityRegistry;
        { Refuses a handle another curve of THIS build pass already carries. }
        procedure RefuseDuplicateIdentity(ACurve: TCurvePointsSet);
        { Gives an instance the handle that says WHICH curve it is, so a later
          rebuild can hand it back the values a fit found. See IdentifyCurve. }
        procedure IdentifyCurve(Curve: TCurvePointsSet);
        { Creates an instance of ACurveTypeId - the SELECTED type when the
          caller has no reason to say otherwise, and a named one when it has.
          A model whose instances were placed at different times may hold more
          than one type, and rebuilding them all as whatever is selected NOW
          silently retypes the model. }
        function CreatePatternInstance(const ACurveTypeId: TCurveTypeId;
            x0: double): TCurvePointsSet; overload;
        function CreatePatternInstance(x0: double): TCurvePointsSet; overload;
        procedure InitializeVariationSteps;

    public
        { How many profile points every reported figure is measured over: the
          size of the union of the curve ranges, or the whole profile when no
          curve declares one. The N behind the R-factors, and the only way to
          see from outside which points a fit is actually being scored on. }
        function FittedPointCount: longint;
        { The reported R-factors, measured over exactly the points the
          objective was measured over - see CollectFittedPoints. Public because
          they describe the task's CURRENT state, which is what a caller wanting
          to know how good the model is has to be able to ask; the CurMin family
          only carries whatever the last optimisation happened to report. }
        function GetSqrRFactor: double;
        function GetAbsRFactor: double;
        { This interval's contribution to the figures reported for the model as
          a whole. The service pools these and divides once, because a ratio
          cannot be summed - see fit_loss. }
        function GetLossParts: TLossParts;

        { True while a fit is being set up - see FRefuseUnfittable. }
        property RefuseUnfittable: boolean
            read FRefuseUnfittable write FRefuseUnfittable;
        { The curve type THIS task builds. Set by the service from the problem
          it belongs to, so two problems in one server cannot share a selection
          through the process-wide curve-type singleton. }
        property CurveTypeId: TCurveTypeId read FCurveTypeId write FCurveTypeId;
        { The model's identity registry. Borrowed - see FIdentity. }
        property Identity: TCurveIdentityRegistry
            read FIdentity write FIdentity;
        { Which fit interval this task is - see FIntervalIndex. }
        property IntervalIndex: longint
            read FIntervalIndex write FIntervalIndex;
        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: boolean;
            ACurveScalingEnabled: boolean); overload;
        destructor Destroy; override;

        { Sets up experimental profile data. }
        procedure SetProfilePointsSet(APointsSet: TPointsSet);
        procedure SetCurvePositions(ACurvePositions: TPointsSet);
        { Takes ownership, like SetCurvePositions. }
        { State a module contributed for this interval. The task holds it and
          the module reads it back; the task itself knows only that it is
          there. Refcounted, so the lifetime rule that used to be a comment -
          "the task frees what it was handed" - is now the language's. }
        { ------------------- the curve-building seam -------------------

          What a curve type placed from a point set (TNamedPointsSet.
          PlacedByPointSet) needs in order to build its own instances. The
          engine cannot build them - only the type knows what its picks mean -
          so the type does the building and the task supplies the machinery it
          would otherwise duplicate.

          The dependency runs one way: a curve type uses these; nothing here
          names any curve type. }

        { The data being fitted. A pattern spans it and needs its y range. }
        function ProfilePoints: TPointsSet;
        { The positions the user placed, if any. }
        function PlacedPositions: TPointsSet;
        { True when this task must refuse an unfittable model rather than
          quietly building an empty one - a fit rather than a preview. }
        function MustRefuseUnfittable: boolean;
        { Gives a curve the stretch of this task's profile it covers. The one
          place a curve gets points. }
        procedure CreatePointsFor(ACurve: TCurvePointsSet);
        { What is built so far, so a builder can RECONCILE - keep what still
          exists, create what is new, drop what went - instead of clearing and
          rebuilding. Rebuilding is what throws away everything a fit has
          learned about the curves that did not change. }
        function BuiltCurves: TSelfCopiedCompList;
        { Drops one built curve and frees it. }
        procedure RemoveBuiltCurve(ACurve: TCurvePointsSet);
        { Re-wires a curve that was KEPT across a rebuild into this call's
          parameter list. AddBuiltCurve does this for a new curve, along with
          taking ownership and stamping the values it is recognised by; a kept
          curve needs only the wiring, because it is already owned and already
          recognised. Leaving it out gives a curve that is fitted and drawn but
          absent from the attributes the client reads. }
        procedure RewireBuiltCurve(ACurve: TCurvePointsSet;
            AStoredValues: TMSCRCurveList);
        { Makes one instance of the selected type, seeds it from the model, and
          takes ownership. }
        function NewInstanceAt(AX0: double): TCurvePointsSet;
        { The same, of a type the caller names rather than the selected one. }
        function NewInstanceOfType(const ACurveTypeId: TCurveTypeId;
            AX0: double): TCurvePointsSet;
        procedure AddBuiltCurve(ACurve: TCurvePointsSet;
            AStoredValues: TMSCRCurveList);

        procedure AddModuleState(AState: IModuleTaskState);
        function ModuleState(const AKind: string): IModuleTaskState;
        { Returns final list of curve positions. }
        function GetCurvePositions: TPointsSet;
        { Returns final set of model curves (pattern instances). }
        function GetCurves: TSelfCopiedCompList;
        { Returns final calculated profile. }
        function GetCalcProfile: TPointsSet;
        { Returns current minimal achived value of R-factor (FCurMin). }
        function GetCurMin: double; virtual;
        function GetCurAbsMin: double; virtual;
        function GetCurSqrMin: double; virtual;
        function GetCurMinInitialized: boolean; virtual;
        function GetAllDone: boolean; virtual;

        procedure SetSpecialCurve(ACurveExpr: string;
            AParams: Curve_parameters);
        { Recreates pattern instances (curves). It should be public
          for initial calculation of R-factor for overall profile. }
        procedure RecreateCurves(AStoredValues: TMSCRCurveList);
        { Searches pattern specimen by hash and sets its parameters 
          from the given list. }
        procedure RestoreCurveValues(AStoredValues: TMSCRCurveList;
            Curve: TCurvePointsSet);
        { Recalculates all pattern instances and FBackground.
          Calculates resulting profile. }
        procedure ComputeProfile;

        { Runs the native in-process Downhill Simplex optimization to completion.
          Public so the native backend (native_fit_backend) can drive it through
          the IFitBackend seam. }
        procedure RunNativeOptimization;

        { Fits curves starting from given parameter set (initially or repeatedly). }
        procedure MinimizeDifference; virtual;
        procedure MinimizeDifferenceAgain; virtual;
        { Searches set of curves fitting exprerimental data with given
          accuracy sequentially decreasing number of such curves. }
        { True when any curve has no closed-form expression, so the
          formula-based (Python / remote) backends cannot evaluate the model. }
        function HasNonAnalyticCurve: boolean;
        { True when any placed curve declares a freely growing amplitude, which is
          what decides whether a self-normalising objective is legitimate (D18). }
        function HasUnboundedAmplitudeCurve: boolean;
        { Applies to this task what the model it now holds actually allows: a
          compatible objective, and curve scaling off for a model that sets its
          own amplitude. The rules are fit_advice's, which the client reads to
          explain them, so there is one statement of each and not two.

          Called whenever the curves are REBUILT, not only before fitting. The
          scaling factor rescales the whole model onto the profile, and the
          rebuilt model is what the user is shown - so a rule applied at fit
          time alone means the picture before the fit is drawn to a different
          scale than the picture after it. A pattern placed between two picked
          points was then drawn a third above them, and read as not drawn at
          all. }
        procedure EnforceLossCompatibility;
        { True when an out-of-process, formula-evaluating engine is selected. }
        function UsesFormulaBackend: boolean;
        procedure MinimizeNumberOfCurves; virtual;
        { Sets up termination flags and returns. }
        procedure StopAsyncOper; virtual;
        { Returns the factor scaling calculated points up to scale of experimental data. }
        function GetScalingFactor: double;
        { The value of the objective currently being minimised, for the profile as
          last computed. Exposed so tests can compare objectives directly instead
          of inferring them from fit outcomes. }

        property MaxAcceptableRFactor: double
            read FMaxAcceptableRFactor write FMaxAcceptableRFactor;
        { Read-only task state, so a backend can marshal the problem to a remote
          compute server (fit_task_marshalling.BuildProblemFromTask). }
        property ExpProfile: TPointsSet read FExpProfile;
        property BackgroundVariationEnabled: boolean read FEnableBackgroundVariation;
        property CurveScalingEnabled: boolean read FCurveScalingEnabled;
        { Selected minimizer algorithm (MIN_KIND_* constant). Defaults to the
          original Downhill Simplex; drives which algorithm Optimization runs. }
        property MinimizerKind: longint read FMinimizerKind write FMinimizerKind;
        { Objective being minimised (LOSS_KIND_* constant). Defaults to the legacy
          R-factor. The statistics reported after a fit do NOT follow this - they
          are always computed from the same fixed residual, so a chi-square from
          one fit stays comparable with a chi-square from another. }
        property LossKind: longint read FLossKind write FLossKind;
        { Residual weighting for the Python backend ('poisson'/'none'). }
        property Weighting: string read FWeighting write FWeighting;
        { URL of the standalone compute server. Empty = fit in-process (default). }
        property ServerUrl: string read FServerUrl write FServerUrl;
        { URL of the Python (lmfit) sidecar (used when MinimizerKind selects it). }
        property PythonUrl: string read FPythonUrl write FPythonUrl;
        { Callback to update information at achieving new minimum. }
        property ServerShowCurMin: TThreadMethod read FShowCurMin write FShowCurMin;
        property ServerDoneProc: TThreadMethod read FDoneProc write FDoneProc;
        { Attributes store indexes of begin and end of the task interval 
          for optimal rebuilding overall resulting profile. }
        property BegIndex: longint read FBegIndex write FBegIndex;
        property EndIndex: longint read FEndIndex write FEndIndex;
    end;

    {  A wrapper for an OpenCL implementation was declared here as an empty
       class - class(TComponent) public end - with no members, no implementation
       and no reference anywhere. A placeholder with no content is a comment
       pretending to be code: it costs a name in the interface, appears in every
       search for the task types, and says nothing the sentence you are reading
       does not. The intention is kept; the empty declaration is not. }

implementation

uses
    SimpMath, int_fit_backend,
    //  In the IMPLEMENTATION, not the interface: int_fit_backend's contract is
    //  stated in terms of TFitTask, so a registry that names IFitBackend can
    //  only be reached from here without a circular reference.
    minimizer_registry, minimizer_registration;

{================================== TFitTask ==================================}

function TFitTask.GetFunc: double;
begin
    Result := GetOptimizingRFactor;
end;

procedure TFitTask.ComputeFunc;
begin
    ComputeProfile;
end;

function TFitTask.GetCalcProfileIntegral: double;
var
    i: longint;
begin
    CheckAssigned(FCalcProfile, 'the calculated profile');

    Result := 0;
    for i := 0 to FCalcProfile.PointsCount - 1 do
        Result := Result + FCalcProfile.PointYCoord[i];
end;

function TFitTask.GetProfileIntegral: double;
var
    i: longint;
begin
    CheckAssigned(FExpProfile, 'the experimental profile');

    Result := 0;
    for i := 0 to FExpProfile.PointsCount - 1 do
        Result := Result + FExpProfile.PointYCoord[i];
end;

{ THE FITTED POINT SET, and the only definition of it.

  It is simply every point this task holds - because a task IS one fitting
  interval. TFitService.CreateTasks builds one sub-task per interval and hands it
  only that stretch of the profile (BegIndex..EndIndex), so restricting again
  here would be restricting twice. When no interval has been selected the service
  materialises the whole profile as one, which is what "no restriction" means.

  THE INTERVALS ARE WHY THIS IS SELECTABLE AT ALL. A pattern or a peak set that
  covers part of a long series should be scored on that part; the answer is for
  the user to mark the interval, not for a curve to declare a range behind their
  back. An earlier attempt did the latter - each curve publishing the stretch it
  answered for, and the R-factor taken over the union - which quietly overrode
  the interval the user had chosen and made the reported figure depend on where
  the model happened to sit rather than on what was asked for.

  What this exists for is the second half of that: numerator and denominator over
  the SAME points, and one definition of "which points" shared by the objective
  and by every figure reported next to it. }
procedure TFitTask.CollectFittedPoints(out ACalc, AObs: TLossDoubleArray);
var
    i, N: longint;
begin
    CheckAssigned(FCalcProfile, 'the calculated profile');
    CheckAssigned(FExpProfile, 'the experimental profile');

    N := FCalcProfile.PointsCount;
    if FExpProfile.PointsCount < N then
        N := FExpProfile.PointsCount;
    SetLength(ACalc, N);
    SetLength(AObs, N);
    for i := 0 to N - 1 do
    begin
        ACalc[i] := FCalcProfile.PointYCoord[i];
        AObs[i] := FExpProfile.PointYCoord[i];
    end;
end;

function TFitTask.GetOptimizingRFactor: double;
var
    Calc, Obs: TLossDoubleArray;
begin
    CollectFittedPoints(Calc, Obs);
    Result := EvaluateLoss(FLossKind, Calc, Obs, GetScalingFactor);
end;

function TFitTask.GetLossParts: TLossParts;
var
    Calc, Obs: TLossDoubleArray;
begin
    CollectFittedPoints(Calc, Obs);
    Result := LossPartsOf(Calc, Obs, GetScalingFactor);
end;

function TFitTask.FittedPointCount: longint;
var
    Calc, Obs: TLossDoubleArray;
begin
    CollectFittedPoints(Calc, Obs);
    Result := Length(Calc);
end;

function TFitTask.GetAbsRFactor: double;
var
    Calc, Obs: TLossDoubleArray;
begin
    CollectFittedPoints(Calc, Obs);
    Result := EvaluateLoss(LOSS_KIND_RELATIVE, Calc, Obs, GetScalingFactor);
end;

function TFitTask.GetRFactor: double;
begin
    Result := GetSqrRFactor;
    //  CHANGING WHICH R-FACTOR THIS RETURNS also changes the formula shown in
    //  the interface and the value stored in FCurMin (see ShowCurMin).
    //Result := GetAbsRFactor;
end;

function TFitTask.GetSqrRFactor: double;
var
    Calc, Obs: TLossDoubleArray;
begin
    CollectFittedPoints(Calc, Obs);
    Result := EvaluateLoss(LOSS_KIND_RFACTOR, Calc, Obs, GetScalingFactor);
end;

function TFitTask.GetVariationStep: double;
var
    Curve: TCurvePointsSet;
begin
    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
        Result := 0.1//  TODO: move into separate "FBackground" class.

    else
    if FCommonVaryingFlag then
    begin
        CheckIndex(FCommonVaryingIndex, FCommonVariableParameters.Params.Count,
                   'the shared parameter being varied');
        Result := FCommonVariableParameters[FCommonVaryingIndex].VariationStep;
    end
    else
    begin
        CheckIndex(FCurveNum, FCurves.Count, 'the curve being varied');
        Curve  := TCurvePointsSet(FCurves.Items[FCurveNum]);
        CheckIndex(FParamNum, Curve.VariableCount,
                   'the curve parameter being varied');
        Result := Curve.VariationSteps[FParamNum];
    end;
end;

{$hints off}
procedure TFitTask.SetVariationStep(NewStepValue: double);
begin

end;

{$hints on}

procedure TFitTask.SetNextParam;
var
    Curve: TCurvePointsSet;
    Count: longint;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(FCommonVariableParameters,
        'the parameters every curve in this task varies together');

    FEndOfCycle := True;
    if FCurves.Count <> 0 then
    begin
        //  Over the curve's parameters.
        CheckIndex(FCurveNum, FCurves.Count, 'the curve being varied');
        Curve := TCurvePointsSet(FCurves.Items[FCurveNum]);
        if FParamNum < Curve.VariableCount - 1 then
        begin
            Inc(FParamNum);
            FEndOfCycle := False;
            Exit;
        end;
    end;

    if FCurveNum < FCurves.Count - 1 then
    begin
        //  Over the curves.
        Inc(FCurveNum);
        FEndOfCycle      := False;
        FParamNum := 0;
        Exit;
    end;

    Count := FCommonVariableParameters.Params.Count;
    if FCommonVaryingFlag then
        if FCommonVaryingIndex < Count then
        begin
            Inc(FCommonVaryingIndex);
            //  On to the next one allowed to vary, stopping at the end. Written
            //  as a Break rather than a compound condition so the index can be
            //  checked before it is used to read anything.
            while FCommonVaryingIndex <> Count do
            begin
                CheckIndex(FCommonVaryingIndex, Count,
                           'the shared parameter being varied');
                if not FCommonVariableParameters[FCommonVaryingIndex].VariationDisabled then
                    Break;
                Inc(FCommonVaryingIndex);
            end;
        end;
    //  The next shared parameter whose variation is not disabled.
    if FCommonVaryingIndex < Count then
    begin
        FEndOfCycle := False;
        FCommonVaryingFlag := True;
        Exit;
    end;

    if FEnableBackgroundVariation then
    begin
        if FBackgroundVaryingFlag then
            Inc(FBackgroundVaryingIndex);
        //  Increments parameter index for next iteration.
        if FBackgroundVaryingIndex < //FBackground.PointsCount
            4 then
        begin
            //  There are still next variable FBackground parameters.
            FBackgroundVaryingFlag := True;
            FEndOfCycle := False;
            Exit;
        end;
    end;
end;

procedure TFitTask.SetFirstParam;
begin
    //  Internal: raises no exception for an inadmissible state.
    FCurveNum := 0;
    FParamNum := 0;
    FEndOfCycle      := False;
    //  The first shared parameter allowed to vary, or Count when none is. Asked
    //  for by name, and answered in one place with its own tests, because the
    //  loop this replaced read its own loop variable after the loop - see
    //  Curve_parameters.IndexOfFirstVarying for what that cost.
    FCommonVaryingIndex := FCommonVariableParameters.IndexOfFirstVarying;
    FCommonVaryingFlag      := False;
    FBackgroundVaryingIndex := 0;
    FBackgroundVaryingFlag  := False;
    FBackgroundWasSaved     := False;
end;

function TFitTask.GetParam: double;
var
    GP: TCurvePointsSet;
    Parameter: TSpecialCurveParameter;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(FCommonVariableParameters,
        'the parameters every curve in this task varies together');

    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
    begin
        CheckIndex(FBackgroundVaryingIndex, BACKGROUND_PARAMETER_COUNT,
                   'the background parameter being varied');
        //Result := FBackground.PointYCoord[FBackgroundVaryingIndex];
        //  WHICH COEFFICIENT THE INDEX MEANS is background_parameters', because
        //  it was written out here and again in SetParam - and a reordering
        //  applied to one copy has the optimiser reading the curvature and
        //  writing the offset, which no error reports.
        Result := BackgroundParameter(FBackgroundVaryingIndex, FA, FB, FC, Fx0);
    end
    else
    if FCommonVaryingFlag then
    begin
        CheckIndex(FCommonVaryingIndex, FCommonVariableParameters.Params.Count,
                   'the shared parameter being varied');
        Parameter := FCommonVariableParameters[FCommonVaryingIndex];
        Result    := Parameter.Value;
    end
    else
    begin
        CheckThat(FCurves.Count <> 0, 'a task must have built its curves before their parameters are read');

        CheckIndex(FCurveNum, FCurves.Count, 'the curve being varied');
        GP     := TCurvePointsSet(FCurves.Items[FCurveNum]);
        CheckIndex(FParamNum, GP.VariableCount,
                   'the curve parameter being varied');
        Result := GP.VariableValues[FParamNum];
    end;
end;

procedure TFitTask.SetParam(NewParamValue: double);
var
    GP: TCurvePointsSet;
    i:  longint;
    Parameter: TSpecialCurveParameter;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');
    CheckAssigned(FCommonVariableParameters,
        'the parameters every curve in this task varies together');

    if FEnableBackgroundVariation and FBackgroundVaryingFlag then
    begin
        CheckIndex(FBackgroundVaryingIndex, BACKGROUND_PARAMETER_COUNT,
                   'the background parameter being varied');
        //FBackground.PointYCoord[FBackgroundVaryingIndex] := NewParamValue;
        CheckThat(FSavedBackground.PointsCount = FBackground.PointsCount, 'the saved background must have a point for every point of the live one');
        if not FBackgroundWasSaved then
        begin
            for i := 0 to FSavedBackground.PointsCount - 1 do
                FSavedBackground.PointYCoord[i] := FBackground.PointYCoord[i];
            FBackgroundWasSaved := True;
        end;
        //  Including which of the four are stored as magnitudes: the curvature
        //  and the offset are, so a negative proposal comes back reflected
        //  rather than refused.
        SetBackgroundParameter(FBackgroundVaryingIndex, NewParamValue,
            FA, FB, FC, Fx0);
    end
    else
    if FCommonVaryingFlag then
    begin
        CheckIndex(FCommonVaryingIndex, FCommonVariableParameters.Params.Count,
                   'the shared parameter being varied');

        Parameter := FCommonVariableParameters[FCommonVaryingIndex];
        Parameter.Value := NewParamValue;

        //  A shared parameter is set on every instance.
        for i := 0 to FCurves.Count - 1 do
        begin
            GP    := TCurvePointsSet(FCurves.Items[i]);
            GP.ValuesByName[
                FCommonVariableParameters[FCommonVaryingIndex].Name
                ] := NewParamValue;
        end;
    end
    else
    begin
        CheckThat(FCurves.Count <> 0, 'a task must have built its curves before their parameters are read');
        //  This could accumulate error in the summed profile - its intensity
        //  drifting away from the sum of the curves' - but varying Sigma forces
        //  a full recomputation, so nothing accumulates.
        CheckIndex(FCurveNum, FCurves.Count, 'the curve being varied');
        GP := TCurvePointsSet(FCurves.Items[FCurveNum]);
        CheckIndex(FParamNum, GP.VariableCount,
                   'the curve parameter being varied');
        //  ??? faster in some cases
        GP.VariableValues[FParamNum] := NewParamValue;
        //GP.ReCalc;
        //AddCurveToProfile(GP);
    end;
end;

function TFitTask.EndOfCycle: boolean;
begin
    Result := FEndOfCycle;
end;

constructor TFitTask.Create(AOwner: TComponent; AEnableBackgroundVariation: boolean;
    ACurveScalingEnabled: boolean);
begin
    inherited Create(AOwner);
    //  Refusing is the DEFAULT: a task asked to build an unfittable model says
    //  so. Only the service's preview path, which draws a markup still being
    //  assembled, opts out - see FRefuseUnfittable.
    FRefuseUnfittable := True;
    FCommonVariableParameters := Curve_parameters.Create(nil);
    //  Curve_parameters creates one parameter in its constructor, which has to
    //  go.
    //  TODO: remove this.
    FCommonVariableParameters.Params.Clear;
    { Sets initial value of R-factor. }
    FMaxAcceptableRFactor := 0.01;
    FAllDone    := False;
    //  Sets default curve type
    FCurveTypeSelector := TCurveTypesSingleton.CreateCurveTypeSelector;
    //  Seeded from the global selection, then owned by this task. The service
    //  overwrites it with the CURVE TYPE OF ITS OWN PROBLEM - see the property.
    FCurveTypeId := FCurveTypeSelector.GetSelectedCurveType;

    FEnableBackgroundVariation := AEnableBackgroundVariation;
    FMinimizerKind := MIN_KIND_DHS;   //  original Downhill Simplex by default
    FLossKind := LOSS_KIND_RFACTOR;   //  the corrected, data-normalised form
    FWeighting := WEIGHTING_POISSON;
    FCurveScalingEnabled := ACurveScalingEnabled;
end;

destructor TFitTask.Destroy;
begin
    FExpProfile.Free;
    FCurves.Free;
    FCalcProfile.Free;
    FBackground.Free;
    FSavedBackground.Free;
    FCurvePositions.Free;
    FMinimizer.Free;
    FUserDefinedParameters.Free;
    FCommonVariableParameters.Free;
    //  Only the one this task MADE. FIdentity is the service's model and
    //  outlives every task built against it.
    FOwnedIdentity.Free;
    inherited;
end;

procedure TFitTask.ComputeProfile;
var
    i:     longint;
    Curve: TCurvePointsSet;
    RestoreBackground: boolean;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');
    CheckThat(FBackground.PointsCount = FSavedBackground.PointsCount, 'the live background must have a point for every point of the saved one');
    CheckThat(FExpProfile.PointsCount = FSavedBackground.PointsCount, 'the saved background must cover every point of the experimental profile');

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := FCurves.Items[i] as TCurvePointsSet;
        Curve.ReCalc;
    end;
    //  The background points.
    RestoreBackground := False;
    for i := 0 to FBackground.PointsCount - 1 do
    begin
        FBackground.PointYCoord[i] :=
            CalcPolinom2(FA, FB, FC, Fx0, FBackground.PointXCoord[i]);
        if (FBackground.PointYCoord[i] > FExpProfile.PointYCoord[i]) or
            (FBackground.PointYCoord[i] < 0) then
        begin
            RestoreBackground := True;
            Break;
        end;
    end;
    if RestoreBackground then
        for i := 0 to FBackground.PointsCount - 1 do
            FBackground.PointYCoord[i] := FSavedBackground.PointYCoord[i]
    else
        FBackgroundWasSaved := False;

    ComputeCurveSum;
end;

procedure TFitTask.BackupCurveParameters;
var
    i:  longint;
    PS: TCurvePointsSet;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');

    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TCurvePointsSet(FCurves.Items[i]);
        PS.BackupParameters;
    end;
end;

procedure TFitTask.RestoreCurveParameters;
var
    i:  longint;
    PS: TCurvePointsSet;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurves, 'the curve list');

    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TCurvePointsSet(FCurves.Items[i]);
        PS.RestoreParameters;
    end;
end;

procedure TFitTask.ComputeCurveSum;
var
    i:  longint;
    PS: TCurvePointsSet;
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCalcProfile, 'the calculated profile');
    CheckAssigned(FBackground, 'the background under the model');
    CheckAssigned(FCurves, 'the curve list');
    CheckThat(FBackground.PointsCount = FCalcProfile.PointsCount, 'the background must cover every point of the computed profile');

    //  Zero the profile.
    for i := 0 to FCalcProfile.PointsCount - 1 do
        FCalcProfile.PointYCoord[i] := 0;

    //  Compute the new one.
    for i := 0 to FCurves.Count - 1 do
    begin
        PS := TCurvePointsSet(FCurves.Items[i]);
        AddCurveToProfile(PS);
    end;
    //  Add the background.
    for i := 0 to FBackground.PointsCount - 1 do
        FCalcProfile.PointYCoord[i] :=
            FCalcProfile.PointYCoord[i] + FBackground.PointYCoord[i];
end;

{ A curve knows where in the profile it sits, so it does the adding. The index
  arithmetic that used to be written out here - and would have had to be written
  out at every other such loop - now exists once, on the curve. }
procedure TFitTask.AddCurveToProfile(PS: TCurvePointsSet);
begin
    PS.AddTo(FCalcProfile);
end;

procedure TFitTask.InitializeVariationSteps;
var
    i, j:  longint;
    Curve: TCurvePointsSet;
begin
    for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        FCommonVariableParameters[i].InitVariationStep;

    for i := 0 to FCurves.Count - 1 do
    begin
        Curve := TCurvePointsSet(FCurves.Items[i]);
        for j := 0 to Curve.VariableCount - 1 do
            Curve.InitVariationStep(j);
    end;
end;

procedure TFitTask.CreateDHSMinimizer;
begin
    FMinimizer.Free;
    FMinimizer := nil;
    FMinimizer := TDownhillSimplexMinimizer.Create(nil);
    FMinimizer.OnGetFunc := GetFunc;
    FMinimizer.OnComputeFunc := ComputeFunc;
    FMinimizer.OnGetVariationStep := GetVariationStep;
    FMinimizer.OnSetVariationStep := SetVariationStep;
    FMinimizer.OnSetNextParam := SetNextParam;
    FMinimizer.OnSetFirstParam := SetFirstParam;
    FMinimizer.OnGetParam := GetParam;
    FMinimizer.OnSetParam := SetParam;
    FMinimizer.OnEndOfCycle := EndOfCycle;
    FMinimizer.OnShowCurMin := ShowCurMin;

    InitializeVariationSteps;

    FEndOfCycle      := False;
    FParamNum := 0;
    FCurveNum := 0;
    FCommonVaryingFlag := False;
    FCommonVaryingIndex := 0;
    FBackgroundVaryingFlag := False;
    FBackgroundVaryingIndex := 0;
    FBackgroundWasSaved := False;
end;

   //  Drops the picked positions whose curves have no intensity left.
function TFitTask.DeleteCurvesWithSmallAmplitude: boolean;
var
    i, j: longint;
    GP:   TCurvePointsSet;
    MaxA: double;
    //  The largest curve amplitude. Below the constant that follows, a
    //  relative amplitude counts as zero.
const
    ZeroConst: double = 0.001;    //  0.1%
begin
    Result := False;
    //  Internal: raises no exception for an inadmissible state.
    MaxA   := 0;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        CheckThat(GP.A >= 0, 'a curve amplitude is never negative');
        if GP.A > MaxA then
            MaxA := GP.A;
    end;
    //  MaxA = 0 after an optimisation cycle means the model does not match the
    //  data at all, so every curve can go.
    i := 0;
    while i < FCurves.Count do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        CheckThat(GP.A >= 0, 'a curve amplitude is never negative');
        if (MaxA = 0) or (GP.A / MaxA < ZeroConst) then
        begin
            //  The corresponding picked position goes with it.
            for j := 0 to FCurvePositions.PointsCount - 1 do
                if Abs(FCurvePositions.PointXCoord[j] - GP.FInitx0) <= TINY then
                begin
                    DeletePoint(FCurvePositions, GP.FInitx0);
                    Result := True;
                    Break;
                end;  //if FCurvePositions.PointXCoord[j] = GP.FInitx0 then
            FCurves.Remove(GP);  //  frees GP
        end
        else
            Inc(i);
    end;
end;

function TFitTask.DeleteCurveWithMaxExpDerivative(var Deleted: TCurvePointsSet): boolean;
var
    Der, MaxDer: double;
    First: boolean;
    MaxGP, GP: TCurvePointsSet;
    i, Index: longint;
    SA: TPointsSet;
begin
    Result  := False;
    Deleted := nil;
    MaxGP   := nil;
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(FCurves.Count <> 0, 'a task must have built its curves before their parameters are read');
    if FCurves.Count <= 1 then
        Exit;
    CheckAssigned(FCurvePositions, 'the curve positions');
    SA := FExpProfile;
    CheckAssigned(SA, 'the experimental profile the curves are compared with');
    if FExpProfile.PointsCount <= 1 then
        Exit;

    First := True;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.Hasx0 then
            Exit;

        if First then
        begin
            //  First time round: take the derivative.
            Index := FExpProfile.IndexOfValueX(GP.FInitx0);
            CheckThat(Index <> -1, 'a curve position must fall on a sample of the experimental profile');
            if Index = FExpProfile.PointsCount - 1 then
                Exit;

            MaxDer := Abs(FExpProfile.PointYCoord[Index + 1] -
                FExpProfile.PointYCoord[Index]);
            MaxGP  := GP;
            First  := False;
        end
        else
        begin
            //  THIS CANNOT TAKE THE DERIVATIVE at the profile's last point, so
            //  a curve sitting there can never be deleted by it.
            Index := FExpProfile.IndexOfValueX(GP.FInitx0);
            CheckThat(Index <> -1, 'a curve position must fall on a sample of the experimental profile');
            if Index = FExpProfile.PointsCount - 1 then
                Break;

            Der := Abs(FExpProfile.PointYCoord[Index + 1] -
                FExpProfile.PointYCoord[Index]);

            if Der > MaxDer then
            begin
                MaxDer := Der;
                MaxGP  := GP;
            end;
        end;
    end;
    CheckAssigned(MaxGP, 'the curve with the steepest slope at its position');

    DeletePoint(FCurvePositions, MaxGP.FInitx0);
    Deleted := TCurvePointsSet(FCurves.Extract(MaxGP));
    Result  := True;
end;

   //  Drops the picked position whose curve has the smallest amplitude.
function TFitTask.DeleteCurveWithMinimalAmplitude(var Deleted: TCurvePointsSet): boolean;
var
    Min:   double;
    First: boolean;
    MinGP, GP: TCurvePointsSet;
    i:     longint;
begin
    Result := False;
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(FCurves.Count <> 0, 'a task must have built its curves before their parameters are read');
    if FCurves.Count <= 1 then
        Exit;

    First := True;
    for i := 0 to FCurves.Count - 1 do
    begin
        GP := TCurvePointsSet(FCurves.Items[i]);
        if not GP.HasA then
            Exit;

        CheckThat(GP.A >= 0, 'a curve amplitude is never negative');
        if First then
        begin
            Min   := GP.A;
            MinGP := GP;
            First := False;
        end
        else
        if GP.A < Min then
        begin
            Min   := GP.A;
            MinGP := GP;
        end;
    end;
    CheckAssigned(MinGP, 'the curve with the smallest amplitude');
    DeletePoint(FCurvePositions, MinGP.FInitx0);
    Deleted := TCurvePointsSet(FCurves.Extract(MinGP));
    Result  := True;
end;

{ WHETHER THIS PARAMETER IS ONE THE PICK SEEDS.

  Asked of the parameter's ROLE, not of its name. The roles exist precisely so
  that a curve type can call its position anything it likes and still be placed
  and initialised correctly, and a module's curve may name neither of these
  'x0' nor 'A'. A name test here would silently fail to re-seed exactly the
  curve types the framework knows least about. }
function IsSeededFromThePick(AParameter: TSpecialCurveParameter): boolean;
begin
    Result := AParameter.Type_ in
        [InvariablePosition, VariablePosition, Amplitude];
end;

{ GIVES AN INSTANCE THE HANDLE THAT SAYS WHICH CURVE IT IS.

  Every model edit reaches TFitService.GoToReadyForFit, which frees the task list
  and rebuilds every instance from the picks. The instance that comes back is a
  different object, so the values the last fit found have to be re-attached to
  it - and the handle is what says which values are its.

  The handle is ISSUED, to the pick, and merely looked up here. That is the
  whole point: the pick survives the rebuild, so the identity does, and because
  the identity is attached to the pick rather than computed from it, MOVING a
  pick keeps it. RestoreCurveValues is the other half.

  Three sources, in order:
    * a handle the curve already carries - a module that identifies its own
      curves sets one before handing the curve over, and its nested patterns
      depend on keeping it;
    * the registry entry for this instance's own pick;
    * the registry entry for this task's positionless instance, for a curve type
      with no position parameter at all (the user-defined formula curve), which
      no pick places and of which there is exactly one per fit interval.

  WHAT THIS REPLACED, because the shape of the old bug is worth keeping: the
  handle used to be a hash of the instance's INITIAL parameter values. It was a
  sum of per-parameter hashes, so it was order-independent and two instances
  whose values were permutations of each other collided; and it was computed
  from the seed, so moving a pick changed it and orphaned everything stored
  under it - which is why moving a fitted pick used to be refused outright. }
function TFitTask.IdentityRegistry: TCurveIdentityRegistry;
begin
    if Assigned(FIdentity) then
    begin
        Result := FIdentity;
        Exit;
    end;
    if not Assigned(FOwnedIdentity) then
        FOwnedIdentity := TCurveIdentityRegistry.Create;
    Result := FOwnedIdentity;
end;

procedure TFitTask.RefuseDuplicateIdentity(ACurve: TCurvePointsSet);
var
    i: longint;
begin
    if not Assigned(FCurves) then
        Exit;
    for i := 0 to FCurves.Count - 1 do
        if SameCurveInstanceId(
            TCurvePointsSet(FCurves.Items[i]).FInstanceId, ACurve.FInstanceId) then
            raise Exception.CreateFmt(
                'Two curves in this model claim the identity %s - one seeded ' +
                'at %g, the other at %g.',
                [CurveInstanceIdToStr(ACurve.FInstanceId),
                 TCurvePointsSet(FCurves.Items[i]).FInitx0, ACurve.FInitx0]);
end;

procedure TFitTask.IdentifyCurve(Curve: TCurvePointsSet);
begin
    CheckAssigned(Curve, 'the curve being given an identity');

    //  Already identified - by a module, or by an earlier pass over a curve
    //  that survived the rebuild. Re-issuing here would hand the same curve a
    //  second identity and orphan its values, which is exactly the failure
    //  being removed.
    if IsCurveInstanceId(Curve.FInstanceId) then
        Exit;

    if Curve.Hasx0 then
        Curve.FInstanceId := IdentityRegistry.IssueForSeed(Curve.FInitx0)
    else
        Curve.FInstanceId := IdentityRegistry.IssueForSlot(FIntervalIndex);
end;

{ Puts back the values a previous fit found for this instance, matched on the
  handle IdentifyCurve gave it.

  A MISS IS NOT ALWAYS AN ERROR, and that is why this does not simply raise. An
  instance created for a pick the user has just added has no saved values by
  definition, and that is the common case.

  But the two kinds of miss are no longer indistinguishable, which they were
  when the key was a hash. An instance whose handle the model does not know at
  all is new. An instance whose handle IS known to carry optimiser results, and
  which found nothing here, is an ORPHAN - values that exist and were not given
  back - and that is reported rather than left as a curve silently sitting at
  its starting guess.

  THE POSITION IS TAKEN FROM THE PICK, not from the stored values, when the pick
  has moved since those values were found. That is what makes moving a fitted
  pick work: the curve keeps the shape the optimiser found and goes where the
  user just put it. Without it the marker would move and the curve would not. }
procedure TFitTask.RestoreCurveValues(AStoredValues: TMSCRCurveList;
    Curve: TCurvePointsSet);
var
    i, j, k: longint;
    CurveParameters: Curve_parameters;
    Parameter, Parameter2: TSpecialCurveParameter;
    Restored, Reseed: boolean;
begin
    //  nil is admissible: the method's semantics allow it, and other methods of
    //  this class call it that way.
    if not Assigned(AStoredValues) then
        Exit;
    CheckAssigned(Curve, 'the curve being given an identity');

    //  An instance with no handle belongs to no model - see IdentifyCurve.
    if not IsCurveInstanceId(Curve.FInstanceId) then
        Exit;

    Restored := False;
    Reseed := IdentityRegistry.NeedsReseed(Curve.FInstanceId);

    for i := 0 to AStoredValues.Count - 1 do
    begin
        CurveParameters := Curve_parameters(AStoredValues.Items[i]);

        if SameCurveInstanceId(CurveParameters.FInstanceId,
            Curve.FInstanceId) then
        begin
            //  The stored parameter sets may contain calculated parameters,
            //  which must not be copied.
            for j := 0 to Curve.Parameters.Params.Count - 1 do
            begin
                Parameter := Curve.Parameters[j];

                //  WHERE THE CURVE SITS COMES FROM THE PICK when the pick has
                //  moved. Everything else - the widths, the shape parameters,
                //  a module's own values - is what the fit found, and is kept.
                //  The amplitude goes with the position: it is seeded from the
                //  data at the pick, so a value found at the old x is no more
                //  applicable than the old x itself.
                if Reseed and IsSeededFromThePick(Parameter) then
                    Continue;

                for k := 0 to CurveParameters.Params.Count - 1 do
                begin
                    Parameter2 := CurveParameters[k];
                    if Parameter.Name = Parameter2.Name then
                    begin
                        //  BY TYPE. Value is the numeric accessor: it reads 0
                        //  from a parameter holding text and writes that 0 back,
                        //  so restoring a curve's parameters wiped its identity -
                        //  a GUID kept in a parameter slot. The client then
                        //  received waveId as a number, could not tell one
                        //  pattern from another, and a child's parent link
                        //  resolved to nothing, so nested patterns disappeared.
                        //
                        //  A numeric value still goes through Value, which is
                        //  virtual and is where every parameter type clamps to
                        //  its own bounds; TypedValue assigns straight to the
                        //  field and would skip that.
                        if Parameter2.IsNumeric then
                            Parameter.Value := Parameter2.Value
                        else
                            Parameter.TypedValue := Parameter2.TypedValue;
                        Break;
                    end;
                end;
            end;
            Restored := True;
            Break;
        end;
    end;

    if Restored and Reseed then
        //  Honoured once. Left set, every later rebuild would keep discarding a
        //  position the user has not touched since.
        IdentityRegistry.ClearReseed(Curve.FInstanceId);

    if not Restored then
    begin
        if IdentityRegistry.IsFitted(Curve.FInstanceId) then
            //  THE CASE THAT USED TO BE INVISIBLE. The model says an optimiser
            //  produced values for this instance, and they were not handed
            //  back. Something has gone wrong that the user would otherwise see
            //  only as a curve that quietly reverted to its starting guess.
            WriteLog(Format('curves: the fitted values for the instance %s ' +
                '(seeded at %g) were not found - it has been left at its ' +
                'initial values',
                [CurveInstanceIdToStr(Curve.FInstanceId), Curve.FInitx0]),
                log.Warning)
        else
        if AStoredValues.Count <> 0 then
            WriteLog(Format('curves: no saved parameters for the instance ' +
                'seeded at %g - it starts from its initial values',
                [Curve.FInitx0]), log.Debug);
    end;
end;

function TFitTask.CreatePatternInstance(x0: double): TCurvePointsSet;
begin
    Result := CreatePatternInstance(FCurveTypeId, x0);
end;

function TFitTask.CreatePatternInstance(const ACurveTypeId: TCurveTypeId;
    x0: double): TCurvePointsSet;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
    SelectedCurveTypeId: TCurveTypeId;
    CurveClass: TCurveClass;
begin
    //  Explicitly nil first. The chain below has no else branch, so before this
    //  an unregistered - or merely newly registered - curve type left Result
    //  uninitialised and the code fell straight into dereferencing it.
    Result := nil;

    SelectedCurveTypeId := ACurveTypeId;
    if IsEqualGUID(SelectedCurveTypeId, TLorentzPointsSet.GetCurveTypeId) then
        Result := TLorentzPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TGaussPointsSet.GetCurveTypeId) then
        Result := TGaussPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := TPseudoVoigtPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TAsymPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := TAsymPseudoVoigtPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TPearson7PointsSet.GetCurveTypeId) then
        Result := TPearson7PointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TMoffatPointsSet.GetCurveTypeId) then
        Result := TMoffatPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TDoniachSunjicPointsSet.GetCurveTypeId) then
        Result := TDoniachSunjicPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TEmgPointsSet.GetCurveTypeId) then
        Result := TEmgPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TVoigtPointsSet.GetCurveTypeId) then
        Result := TVoigtPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TSkewedGaussianPointsSet.GetCurveTypeId) then
        Result := TSkewedGaussianPointsSet.Create(nil, x0)
    else
    if IsEqualGUID(SelectedCurveTypeId, TStepPointsSet.GetCurveTypeId) then
        Result := TStepPointsSet.Create(nil, x0)
    else

    if IsEqualGUID(SelectedCurveTypeId, TUserPointsSet.GetCurveTypeId) then
    begin
        Result := TUserPointsSet.Create(nil);
        TUserPointsSet(Result).Expression := FCurveExpr;
        TUserPointsSet(Result).SetParameters(
            Curve_parameters(FUserDefinedParameters.GetCopy));
    end
    else

    if IsEqualGUID(SelectedCurveTypeId,
        T2BranchesPseudoVoigtPointsSet.GetCurveTypeId) then
        Result := T2BranchesPseudoVoigtPointsSet.Create(nil, x0);

    //  Generic fallback for any type not named above.
    //
    //  Curve types self-register with curve_types_singleton, but this factory
    //  was a hardcoded list - so a newly registered type appeared in the menu
    //  and then crashed the engine when fitted. The registry is the authority
    //  on what exists, so ask it. The chain above is left exactly as it was, so
    //  every built-in curve is constructed by the same code as before and the
    //  Stage 0 baseline is untouched (D2); only previously-broken cases change.
    //
    //  The generic path uses the one-argument constructor every TNamedPointsSet
    //  has and then assigns x0, rather than the (AOwner, x0) overload the
    //  built-ins happen to declare, which is not part of the base contract.
    if not Assigned(Result) then
    begin
        CurveClass := FindCurveClassById(SelectedCurveTypeId);
        if Assigned(CurveClass) then
        begin
            Result := CurveClass.Create(nil);
            if Result.Hasx0 then
                Result.x0 := x0;
        end;
    end;

    if not Assigned(Result) then
        raise Exception.CreateFmt(
            'No curve class is registered for type %s, so no pattern instance ' +
            'could be created.', [GUIDToString(SelectedCurveTypeId)]);

    if FCommonVariableParameters.Count = 0 then
    begin
        for i := 0 to Result.Parameters.Count - 1 do
            if (Result.Parameters[i].Type_ = Shared) and
                (not Result.Parameters[i].VariationDisabled) then
            begin
                Parameter := Result.Parameters[i].CreateCopy;
                Parameter.InitValue;
                Parameter.InitVariationStep;

                try
                    Container :=
                        TPersistentCurveParameterContainer(
                            FCommonVariableParameters.Params.Add);

                    try
                        Container.Parameter := Parameter;
                    except
                        FCommonVariableParameters.Params.Delete(Container.ID);
                        Container.Free;
                        raise;
                    end;

                except
                    Parameter.Free;
                    raise;
                end;
            end;
        //  Initializing list of common parameters. It is performed only
        //  once when the first curve instance is created (it is assumed
        //  that all the instances have the same type).
        //  TODO: remove the assumption mentioned above.
        for i := 0 to FCommonVariableParameters.Params.Count - 1 do
        begin
            Container := TPersistentCurveParameterContainer(
                FCommonVariableParameters.Params.Items[i]);

            Container.Parameter.InitValue;
            Container.Parameter.InitVariationStep;
        end;
    end;
end;

procedure TFitTask.RecreateCurves(AStoredValues: TMSCRCurveList);
var
    i, j, k:        longint;
    Curve:          TCurvePointsSet;
    CurveFound:     boolean;
    CurvePosition:  double;

    CurveAmplitude: double;
    SelectedCurveTypeId: TCurveTypeId;
    IntervalSigma:  double;
    SelectedClass:  TCurveClass;
    Builder:        TCurveBuilder;

begin
    //  Internal: raises no exception for an inadmissible state.
    CheckAssigned(FCurvePositions, 'the curve positions');
    CheckAssigned(FExpProfile, 'the experimental profile');
    CheckThat(FExpProfile.PointsCount >= 2, 'a step between samples cannot be measured on fewer than two points');

    //  A sensible starting width for a user curve's SIGMA parameter: derived from
    //  the fitting interval rather than a fixed default, so it scales with the
    //  data (the optimizer then refines it). Applied to user curves only, so the
    //  built-in curves' behaviour - and the Stage 0 baseline - is unchanged.
    IntervalSigma := Abs(FExpProfile.PointXCoord[FExpProfile.PointsCount - 1] -
        FExpProfile.PointXCoord[0]) / 6;
    if IntervalSigma <= 0 then
        IntervalSigma := 0.25;

    //  Saves previously created curve instances.
    if not Assigned(FCurves) then
        FCurves := TSelfCopiedCompList.Create;

    //  sozdaem zanovo summarnyy profil'
    if Assigned(FCalcProfile) then
        FCalcProfile.Clear
    else
        FCalcProfile := TPointsSet.Create(nil);
    //  kol-vo tochek profilya ustanavlivaetsya ravnym kol-vu tochek uchastka
    for i := 0 to FExpProfile.PointsCount - 1 do
        FCalcProfile.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    //  The background points, built fresh.
    if Assigned(FBackground) then
        FBackground.Clear
    else
        FBackground := TPointsSet.Create(nil);
    for i := 0 to FExpProfile.PointsCount - 1 do
        FBackground.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    if Assigned(FSavedBackground) then
        FSavedBackground.Clear
    else
        FSavedBackground := TPointsSet.Create(nil);
    for i := 0 to FExpProfile.PointsCount - 1 do
        FSavedBackground.AddNewPoint(FExpProfile.PointXCoord[i], 0);

    //  A curve type placed from a POINT SET builds its own instances: only the
    //  type knows what its picks mean, and neither the pruning nor the creation
    //  below can describe them, both being keyed on one x per curve.
    //
    //  Asked of the CLASS, before anything is built. Deriving it from an attempt
    //  instead - "did the type handle this?" - answers no in the ordinary case
    //  where the type is selected but nothing is marked yet, and control then
    //  reaches the automatic path below, which treats every data point as a
    //  position and builds one curve per point. That is what the user saw as a
    //  hang.
    SelectedClass := FindCurveClassById(FCurveTypeId);
    if Assigned(SelectedClass) and (SelectedClass.PlacedByPointSet <> '') then
    begin
        WriteLog(Format('curves: %s is placed from the "%s" point set',
            [SelectedClass.ClassName, SelectedClass.PlacedByPointSet]),
            log.Notification);
        if FindCurveBuilder(SelectedClass.PlacedByPointSet, Builder) then
            //  True even when the builder refused or built nothing
            //  deliberately: it has dealt with the build either way, and
            //  falling through would be the one-curve-per-point path.
            if Builder(Self, AStoredValues) then
                //  THE PICKED POSITIONS ARE NOT TOUCHED HERE, and no version of
                //  this may write to them again. They are model INPUT: unique X,
                //  each one a sample of the profile, and the seed every instance
                //  is rebuilt from - which is also the key its fitted parameters
                //  are restored by (see IdentifyCurve). Writing the built curves'
                //  fitted x0 back into them broke all three at once: two
                //  instances converging on one x0 made X non-unique and the next
                //  redraw asserted in TPointsSet.Sort; a fitted x0 is off the
                //  sample grid, so the next CreateTasks could not find it; and
                //  changing the seed changed the restore key, silently discarding
                //  the very fit it was trying to report.
                //
                //  What the model actually contains is reported instead by
                //  TFitService.CreateResultedCurvePositions, derived from the
                //  built curves and read-only. Nothing reads it back.
                Exit;
    end;

    //  Drops the instances whose positions are no longer among the picks.
    k := 0;
    while k < FCurves.Count do
    begin
        CurveFound := False;
        Curve      := TCurvePointsSet(FCurves.Items[k]);
        //  A pattern with no position parameter has no instances to drop.
        if not Curve.Hasx0 then
            Break;

        for i := 0 to FCurvePositions.PointsCount - 1 do
            if Curve.FInitx0 = FCurvePositions.PointXCoord[i] then
            begin
                CurveFound := True;
                Break;
            end;

        if not CurveFound then
            FCurves.Remove(Curve)
        //  FCurves frees the components it holds, by default.

        else
            Inc(k);
    end;
    //  The negation of this condition means the curve list is not empty AND the
    //  pattern has no position parameter, in which case there is nothing to do.
    if (FCurves.Count = 0) or (TCurvePointsSet(FCurves.Items[0]).Hasx0) then
        if FCurvePositions.PointsCount = 0 then
        begin
            //  One instance per interval; if the pattern has a position
            //  parameter, it is set to the middle of the interval.
            (*  The first version of this added one instance of the default curve
                type, which turned out to be unusable: the single curve that
                algorithm created could not be deleted from the interval.
            Curve := CreatePatternInstance;

            try
                //  Each instance spans the interval the user selected.
                for j := 0 to SA.PointsCount - 1 do
                    Curve.AddNewPoint(SA.PointXCoord[j], 0);

                //  Amplitude and anchor come from the interval's middle point.
                CurvePosition := SA.PointXCoord[SA.PointsCount div 2];
                CurveAmplitude := SA.PointYCoord[SA.PointsCount div 2];
                if Curve.HasA then Curve.A := CurveAmplitude;
                //  The instance's position.
                if Curve.Hasx0 then
                begin
                    Curve.x0 := CurvePosition;
                    Curve.FInitx0 := CurvePosition;
                end;
                if Curve.HasSigma then Curve.Sigma := Sigma;
                //  Not filled in: it is not needed.
                //Curve.Lambda := WaveLength;
                IdentifyCurve(Curve);
                RestoreCurveValues(AStoredValues, Curve);
                //  The new instance joins the list.
                FCurves.Add(Curve);
                //  A pattern with an anchor contributes its anchor point, so it
                //  appears in the shared list afterwards.
                if TCurvePointsSet(FCurves.Items[0]).Hasx0 then
                    FCurvePositions.AddNewPoint(CurvePosition, CurveAmplitude);
            except
                Curve.Free;
                raise;
            end;
            *)

            SelectedCurveTypeId := FCurveTypeId;

            //  Now only a user curve is created here - the one type that need
            //  not have a position parameter.
            if IsEqualGUID(SelectedCurveTypeId, TUserPointsSet.GetCurveTypeId) then
            begin
                Curve := CreatePatternInstance(0);

                try
                    TUserPointsSet(Curve).Expression := FCurveExpr;
                    TUserPointsSet(Curve).SetParameters(
                        Curve_parameters(FUserDefinedParameters.GetCopy));

                    //  The stretch this curve covers - the whole interval for
                    //  a user curve, which has no compact support.
                    CreatePointsFor(Curve);

                    //  Amplitude and (if the curve has a position parameter)
                    //  position default to the middle of the interval - so a
                    //  positioned user curve is created here too, instead of being
                    //  silently discarded (which left nothing to fit).
                    CurvePosition :=
                        FExpProfile.PointXCoord[FExpProfile.PointsCount div 2];
                    CurveAmplitude :=
                        FExpProfile.PointYCoord[FExpProfile.PointsCount div 2];
                    if Curve.HasA then
                        Curve.A := CurveAmplitude;
                    if Curve.Hasx0 then
                    begin
                        Curve.x0 := CurvePosition;
                        Curve.FInitx0 := CurvePosition;
                    end;
                    if Curve.HasSigma then
                        Curve.Sigma := IntervalSigma;
                    IdentifyCurve(Curve);
                    RestoreCurveValues(AStoredValues, Curve);
                    FCurves.Add(Curve);

                    //  A positioned instance gets a marker for display, like the
                    //  built-in path.
                    if Curve.Hasx0 then
                        FCurvePositions.AddNewPoint(CurvePosition, CurveAmplitude);
                except
                    Curve.Free;
                    raise;
                end;
            end;

        end //  if FCurvePositions.PointsCount = 0
        else
            for i := 0 to FCurvePositions.PointsCount - 1 do
            begin
                CurveFound := False;
                CurvePosition := FCurvePositions.PointXCoord[i];

                for k := 0 to FCurves.Count - 1 do
                begin
                    Curve := TCurvePointsSet(FCurves.Items[k]);
                    if not Curve.Hasx0 then
                        Break;

                    if Abs(Curve.FInitx0 - CurvePosition) <= TINY then
                    begin
                        CurveFound := True;
                        Break;
                    end;
                end;

                if not CurveFound then
                    //  Either an anchor point with no instance of its own (on a
                    //  pattern that does have a position parameter), or the
                    //  instance list is empty.
                begin
                    //  A new instance.
                    Curve := CreatePatternInstance(CurvePosition);

                    try
                        //  The stretch this curve covers.
                        CreatePointsFor(Curve);

                        if Curve.HasA then
                            Curve.A := FCurvePositions.PointYCoord[i];
                        //  The instance's position.
                        if Curve.Hasx0 then
                        begin
                            Curve.x0      := FCurvePositions.PointXCoord[i];
                            Curve.FInitx0 := FCurvePositions.PointXCoord[i];
                        end;
                        //  User curves only: start SIGMA from the interval width
                        //  (built-in curves keep their own initialisation).
                        if (Curve is TUserPointsSet) and Curve.HasSigma then
                            Curve.Sigma := IntervalSigma;
                        //  Not filled in: it is not needed.
                        //Curve.Lambda := WaveLength;
                        IdentifyCurve(Curve);
                        RestoreCurveValues(AStoredValues, Curve);
                        //  The new instance joins the list.
                        FCurves.Add(Curve);
                        //  With no position parameter there is only ever one
                        //  instance.
                        if not Curve.Hasx0 then
                            Break;
                    except
                        Curve.Free;
                        raise;
                    end;
                end;
            end;
        //  Check the picked positions and add the instances they need.
end;

type
    TDeleteCurveStrategy = function(var Deleted: TCurvePointsSet): boolean of object;

procedure TFitTask.MinimizeNumberOfCurvesAlg;

    procedure DeleteCurves(Strategy: TDeleteCurveStrategy);
    var
        ZerosDeleted, PointDeleted: boolean;
        Deleted: TCurvePointsSet;
    begin
        Deleted      := nil;
        PointDeleted := False; PointDeleted := False;
        //  Drops the picked positions whose curves have no amplitude left, and
        //  those where the experimental profile's derivative is largest.
        while (GetRFactor < FMaxAcceptableRFactor) and (not FTerminated) do
        begin
            //  The previous optimisation cycle reduced the R-factor, so the
            //  parameters are saved here.
            BackupCurveParameters;
            ZerosDeleted := DeleteCurvesWithSmallAmplitude;
            Deleted.Free;
            Deleted      := nil;
            PointDeleted := Strategy(Deleted);

            if ZerosDeleted or PointDeleted then
            begin
                ComputeProfile;

                if GetRFactor > FMaxAcceptableRFactor then
                begin
                    Optimization;
                    if GetRFactor > FMaxAcceptableRFactor then
                    begin
                        //  Optimisation could not bring the R-factor into the
                        //  required range, so the last good state is restored.
                        //  ONLY the curve deleted at the point of largest
                        //  derivative comes back - curves deleted for having no
                        //  amplitude do not.
                        RestoreCurveParameters;
                        if PointDeleted then
                        begin
                            CheckAssigned(Deleted, 'the curve just removed from the model');
                            if Deleted.Hasx0 then
                                AddPointToCurvePositions(Deleted.FInitx0);
                            FCurves.Add(Deleted);
                            Deleted      := nil;
                            PointDeleted := False;
                        end;
                        ComputeProfile;
                        { Updates final optimal R-factor. }
                        ShowCurMin;
                        Break;
                    end;
                end
                else
                begin
                    { Updates final optimal R-factor. }
                    ShowCurMin;
                end;
            end
            else
                Break;
            if FCurves.Count <= 1 then
                Break;
        end;

        Deleted.Free;
    end;

begin
    { The first cycle of optimization. }
    Optimization;

    DeleteCurves(DeleteCurveWithMaxExpDerivative);

    DeleteCurves(DeleteCurveWithMinimalAmplitude);

    { Final cycle of optimization. }
    Optimization;
end;

{$hints off}
procedure TFitTask.RunNativeOptimization;
var
    ErrorCode: longint;
begin
    //  Only Downhill Simplex (MIN_KIND_DHS) is implemented today.
    CreateDHSMinimizer;
    FMinimizer.Minimize(ErrorCode);

    FMinimizer.Free;
    FMinimizer := nil;
end;

function TFitTask.HasNonAnalyticCurve: boolean;
var
    j: longint;
begin
    //  A curve with no closed-form expression cannot be marshalled to a remote
    //  or Python backend: the wire contract carries a formula string, which
    //  those engines evaluate instead of re-implementing the curve type. The
    //  A pattern built from picked pivots is the first such curve, and
    //  TNamedPointsSet.GetCurveExpression already returns empty for it, so no
    //  new flag is needed to detect this.
    Result := False;
    if not Assigned(FCurves) then
        Exit;
    for j := 0 to FCurves.Count - 1 do
        if TNamedPointsSet(FCurves.Items[j]).GetCurveExpression = '' then
        begin
            Result := True;
            Exit;
        end;
end;

function TFitTask.HasUnboundedAmplitudeCurve: boolean;
var
    j: longint;
begin
    Result := False;
    if not Assigned(FCurves) then
        Exit;
    for j := 0 to FCurves.Count - 1 do
        if TNamedPointsSet(FCurves.Items[j]).AmplitudeIsUnbounded then
        begin
            Result := True;
            Exit;
        end;
end;

procedure TFitTask.EnforceLossCompatibility;
var
    Advice: TFitAdvice;
    Changed: boolean;
begin
    //  The DECISION lives in fit_advice, which the client also calls to explain
    //  what a fit will do. Deciding here and explaining there would be two
    //  copies of the same rules, and the explanation would eventually describe
    //  something the engine no longer does - which is worse than not explaining
    //  at all, because it would be believed.
    Advice := AdviseFit(FLossKind, UsesFormulaBackend, not HasNonAnalyticCurve,
        HasUnboundedAmplitudeCurve, FCurveScalingEnabled);

    //  What CHANGES here, so the log below reports a correction rather than
    //  repeating a standing one. This runs on every rebuild - every pick the
    //  user makes - and a reason restated on each of them is a reason nobody
    //  reads.
    Changed := (Advice.LossKind <> FLossKind) or
        (Advice.CurveScalingDisabled and FCurveScalingEnabled);

    if Advice.LossKind <> FLossKind then
        FLossKind := Advice.LossKind;
    if Advice.CurveScalingDisabled then
        FCurveScalingEnabled := False;

    //  Logged whenever anything was overridden, so a support question about a
    //  surprising result is answerable from the log alone.
    if Changed and (Advice.Detail <> '') then
        WriteLog(Advice.Detail, Warning);
end;

function TFitTask.UsesFormulaBackend: boolean;
begin
    //  Asked of the engine rather than compared against one engine's id: an
    //  engine declares whether it fits by evaluating a curve's expression, and
    //  every engine that does shares the limitations that make a fallback
    //  necessary. A remote compute server does too, whichever engine it runs.
    Result := MinimizerNeedsFormula(FMinimizerKind) or (FServerUrl <> '');
end;

procedure TFitTask.Optimization;
var
    Context: TBackendContext;
    MinimizerInfo: TMinimizerInfo;
    Backend: IFitBackend;
    Started: TDateTime;
    j, SelfScaling: longint;
begin
    //  FIT LIFECYCLE LOGGING. A fit that hangs is otherwise a black box: the
    //  client waits, the server says nothing, and there is no way to tell a slow
    //  fit from a stuck one, or which backend it got stuck in. These four lines
    //  are what make that answerable from the log alone.
    Started := Now;
    SelfScaling := 0;
    if Assigned(FCurves) then
        for j := 0 to FCurves.Count - 1 do
            if TNamedPointsSet(FCurves.Items[j]).AmplitudeIsUnbounded then
                Inc(SelfScaling);
    WriteLog(Format('fit: START curves=%d (self-scaling=%d) points=%d ' +
        'minimizer=%d loss=%d scaling=%s',
        [FCurves.Count, SelfScaling,
         FCalcProfile.PointsCount, FMinimizerKind, FLossKind,
         BoolToStr(FCurveScalingEnabled, 'on', 'off')]), log.Notification);

    EnforceLossCompatibility;

    //  Route the fit through the compute-backend seam (D3/D7). The minimizer kind
    //  selects the backend; every backend is an IFitBackend, so the fit path is
    //  the same whichever runs it:
    //    - Python/lmfit sidecar (MIN_KIND_PYTHON_LM), when its URL is set;
    //    - the standalone compute server, when ServerUrl is set (may be remote);
    //    - otherwise the native in-process Downhill Simplex engine.
    //
    //  The client also greys the Python engine out for such a model, but the
    //  check is repeated here deliberately: the server must not depend on a
    //  client having done it. Falling back is better than failing, since the
    //  native engine fits these curves perfectly well - only the FORMULA-based
    //  backends cannot.
    //  Ensures the engine set exists wherever a fit runs, rather than trusting
    //  every host - client, compute server, a test building a task directly - to
    //  have called it at start-up. Idempotent, and the alternative is worse: a
    //  missing registration would silently fit natively while reporting the
    //  engine the user chose (D26).
    RegisterAllMinimizers;

    Context.PythonUrl := FPythonUrl;
    Context.ServerUrl := FServerUrl;

    Backend := nil;
    if AdviseFit(FLossKind, UsesFormulaBackend, not HasNonAnalyticCurve,
                 HasUnboundedAmplitudeCurve,
                 FCurveScalingEnabled).FallsBackToNativeEngine then
        //  Reasons already logged by EnforceLossCompatibility, called above.
        //  In-process specifically, NOT the default engine wherever it is
        //  configured: a configured compute server is itself a formula-based
        //  backend, so sending the model there is the very thing this guard
        //  prevents.
        Backend := NativeInProcessBackend
    else if FindMinimizer(FMinimizerKind, MinimizerInfo) then
        //  nil means the selected engine cannot run here - no sidecar URL, say -
        //  and the fallback below is then the ordinary native fit rather than a
        //  failure. An application with no Python must still fit (D4).
        Backend := MinimizerInfo.CreateBackend(Context);

    if not Assigned(Backend) then
        Backend := DefaultFitBackend(Context);
    WriteLog('fit: handing the model to the selected backend', log.Notification);
    try
        Backend.Fit(Self);
    finally
        //  Logged in a finally so an exception or a termination still records
        //  how far the fit got - the case where the duration matters most.
        WriteLog(Format('fit: DONE in %.2f s, R-factor %.6g',
            [(Now - Started) * 24 * 60 * 60, GetRFactor]), log.Notification);
    end;
end;

{$hints on}

procedure TFitTask.MinimizeDifference;
begin
    Optimization;
    Done;
end;

procedure TFitTask.MinimizeDifferenceAgain;
begin
    RecreateCurves(nil);
    Optimization;
    Done;
end;

procedure TFitTask.MinimizeNumberOfCurves;
begin
    MinimizeNumberOfCurvesAlg;
    Done;
end;

procedure TFitTask.AddPointToCurvePositions(XValue: double);
var
    Index: longint;
begin
    CheckAssigned(FExpProfile, 'the experimental profile');
    CheckAssigned(FCurvePositions, 'the curve positions');

    Index := FExpProfile.IndexOfValueX(XValue);
    CheckThat(Index <> -1, 'a curve position must fall on a sample of the experimental profile');

    FCurvePositions.AddNewPoint(XValue, FExpProfile.PointYCoord[Index]);
end;

//  Removes the point with the given X from the picked positions.
procedure TFitTask.DeletePoint(var Points: TPointsSet; XValue: double);
var
    j:    longint;
    Temp: TPointsSet;
begin
    //  Internal: raises no exception for an inadmissible state.
    //  Copied so that every parameter comes across.
    Temp := TPointsSet(Points.GetCopy);
    try
        Temp.Clear;
        for j := 0 to Points.PointsCount - 1 do
            if Abs(XValue - Points.PointXCoord[j]) > TINY then
                Temp.AddNewPoint(Points.PointXCoord[j], Points.PointYCoord[j]);

        Points.Free;
        Points := Temp;
    except
        Temp.Free;
        raise;
    end;
end;

procedure TFitTask.SetProfilePointsSet(APointsSet: TPointsSet);
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(Assigned(APointsSet), 'APointsSet is missing when it is required');

    FExpProfile.Free;
    FExpProfile := APointsSet;
end;

function TFitTask.ProfilePoints: TPointsSet;
begin
    Result := FExpProfile;
end;

function TFitTask.PlacedPositions: TPointsSet;
begin
    Result := FCurvePositions;
end;

function TFitTask.MustRefuseUnfittable: boolean;
begin
    Result := FRefuseUnfittable;
end;

{ Gives ACurve the stretch of this task's profile that it covers.

  The ONE place a curve gets points. A curve that is compactly supported takes
  only the samples inside its support; every other curve takes the whole
  interval, which is what all of them took before and is still the truth for a
  peak whose tails are small but never exactly zero.

  Refuses rather than clamps when a support falls off the profile entirely: that
  is a marked-up pattern nowhere near the data, which is a question about the
  model and not something to paper over with an empty curve. }
procedure TFitTask.CreatePointsFor(ACurve: TCurvePointsSet);
var
    Lo, Hi, i: longint;
begin
    CheckAssigned(ACurve, 'the curve to give points to');
    CheckAssigned(FExpProfile, 'the experimental profile');
    CheckThat(FExpProfile.PointsCount > 0, 'a curve cannot be given points from an empty profile');

    Lo := 0;
    Hi := FExpProfile.PointsCount - 1;

    if (ACurve.SupportMin > NegInfinity) or (ACurve.SupportMax < Infinity) then
    begin
        //  First and last sample INSIDE the support. Walked rather than searched
        //  because it happens once per curve per rebuild, and a linear walk needs
        //  no assumption about the profile being sorted.
        Lo := -1;
        Hi := -1;
        for i := 0 to FExpProfile.PointsCount - 1 do
            if ACurve.CoversSample(FExpProfile.PointXCoord[i]) then
            begin
                if Lo = -1 then
                    Lo := i;
                Hi := i;
            end;
        if Lo = -1 then
            raise EUserException.CreateFmt(
                'A curve was placed at %s..%s, which is outside the data ' +
                '(%s..%s), so it covers no point of it.',
                [FloatToStr(ACurve.SupportMin), FloatToStr(ACurve.SupportMax),
                 FloatToStr(FExpProfile.PointXCoord[0]),
                 FloatToStr(FExpProfile.PointXCoord[FExpProfile.PointsCount - 1])]);
    end;

    ACurve.SetWindow(FExpProfile, Lo, Hi);
end;

function TFitTask.BuiltCurves: TSelfCopiedCompList;
begin
    Result := FCurves;
end;

procedure TFitTask.RemoveBuiltCurve(ACurve: TCurvePointsSet);
begin
    //  FCurves owns what it holds, so Remove frees it - which is what the caller
    //  wants: a pattern the user deleted has no owner left.
    FCurves.Remove(ACurve);
end;

procedure TFitTask.RewireBuiltCurve(ACurve: TCurvePointsSet;
    AStoredValues: TMSCRCurveList);
begin
    RestoreCurveValues(AStoredValues, ACurve);
end;

function TFitTask.NewInstanceAt(AX0: double): TCurvePointsSet;
begin
    Result := CreatePatternInstance(AX0);
end;

function TFitTask.NewInstanceOfType(const ACurveTypeId: TCurveTypeId;
    AX0: double): TCurvePointsSet;
begin
    Result := CreatePatternInstance(ACurveTypeId, AX0);
end;

{ The last three steps every built curve needs, in one call: the start it will
  be recognised by on a later rebuild, the handle that says which instance it
  is, and the parameter wiring. Kept together because leaving one out produces a
  curve that is fitted but never recognised again. }
procedure TFitTask.AddBuiltCurve(ACurve: TCurvePointsSet;
    AStoredValues: TMSCRCurveList);
begin
    ACurve.FInitx0 := ACurve.x0;
    //  A MODULE MAY BRING ITS OWN. When it does, the handle is adopted rather
    //  than replaced - its nested patterns address each other by it - and the
    //  registry refuses a second instance under the same one instead of
    //  silently restoring one curve's values onto another.
    if IsCurveInstanceId(ACurve.FInstanceId) then
    begin
        //  TWO LIVE INSTANCES UNDER ONE HANDLE, caught here because this is
        //  where it can be seen: FCurves holds what THIS build pass has
        //  produced, and the registry spans rebuilds so it cannot tell one pass
        //  from the next. Left to run, the two would restore each other's
        //  fitted values and the user would see a curve wearing a shape it
        //  never had.
        RefuseDuplicateIdentity(ACurve);
        //  PLACED BY A MODULE, which is what the last argument records: this
        //  instance has a seed but no pick, so the pick set must not prune it.
        IdentityRegistry.Adopt(ACurve.FInstanceId, ACurve.FInitx0, True);
    end
    else
        IdentifyCurve(ACurve);
    RestoreCurveValues(AStoredValues, ACurve);
    FCurves.Add(ACurve);
end;

procedure TFitTask.AddModuleState(AState: IModuleTaskState);
begin
    //  Nil means the module has nothing in this interval, which is ordinary -
    //  most intervals belong to no module at all.
    if not Assigned(AState) then
        Exit;
    SetLength(FModuleStates, Length(FModuleStates) + 1);
    FModuleStates[High(FModuleStates)] := AState;
end;

function TFitTask.ModuleState(const AKind: string): IModuleTaskState;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(FModuleStates) do
        if FModuleStates[i].Kind = AKind then
            Exit(FModuleStates[i]);
end;

{ Creates one pattern per CLOSED item, placed between its two ends.

  Because each item carries both of its ends, nothing here depends on ordering:
  patterns may overlap freely, which is what nesting requires, and two patterns
  may share a boundary. An item still open is a gesture in progress and is simply
  not built. }

procedure TFitTask.SetCurvePositions(ACurvePositions: TPointsSet);
begin
    //  Internal: raises no exception for an inadmissible state.
    CheckThat(Assigned(ACurvePositions), 'ACurvePositions is missing when it is required');

    FCurvePositions.Free;
    FCurvePositions := ACurvePositions;
end;

function TFitTask.GetCurvePositions: TPointsSet;
begin
    Result := FCurvePositions;
end;

function TFitTask.GetCalcProfile: TPointsSet;
begin
    Result := FCalcProfile;
end;

function TFitTask.GetCurves: TSelfCopiedCompList;
begin
    Result := FCurves;
end;

function TFitTask.GetCurMin: double;
begin
    Result := FCurMin;
end;

function TFitTask.GetCurAbsMin: double;
begin
    Result := FCurAbsMin;
end;

function TFitTask.GetCurSqrMin: double;
begin
    Result := FCurSqrMin;
end;

function TFitTask.GetCurMinInitialized: boolean;
begin
    Result := FCurMinInitialized;
end;

function TFitTask.GetAllDone: boolean;
begin
    Result := FAllDone;
end;

procedure TFitTask.ShowCurMin;
begin
    //  RECOMPUTED, not reused, because: the R-factor shown may differ from the
    //  one being optimised, and the parameters may have been changed by a
    //  special algorithm - deleting "surplus" curves, for instance.
    FCurSqrMin := GetSqrRFactor;
    FCurAbsMin := GetAbsRFactor;
    FCurMin    := FCurSqrMin;    //  so it is not computed twice
    //  MUST AGREE WITH GetRFactor.
    FCurMinInitialized := True;
    ServerShowCurMin;
end;

procedure TFitTask.Done;
begin
    FAllDone := True;
    ServerDoneProc;
end;

procedure TFitTask.SetSpecialCurve(ACurveExpr: string; AParams: Curve_parameters);
begin
    //  RAISED, NOT WARNED ABOUT. The handler that used to sit here caught
    //  EAssertionFailed and logged a Warning, so the comment above it called
    //  this non-fatal - but CheckThat raises EInternalCheckFailed, which that
    //  handler never matched, so the `else raise` has been the real behaviour
    //  since the conversion. It is also the correct one: storing an empty
    //  expression would leave a special curve that cannot be evaluated, and a
    //  logged warning about it is exactly the silent degradation this codebase
    //  refuses.
    CheckThat(Length(ACurveExpr) <> 0,
        'a special curve needs an expression to evaluate');
    CheckThat(Assigned(AParams),
        'a special curve needs the parameters its expression refers to');

    FCurveExpr := ACurveExpr;
    FUserDefinedParameters.Free;
    FUserDefinedParameters := AParams;
end;

procedure TFitTask.StopAsyncOper;
begin
    FTerminated := True;
    if Assigned(FMinimizer) then
        FMinimizer.Terminated := True;
end;

function TFitTask.GetScalingFactor: double;
var
    CalcProfileIntegral, ProfileIntegral: double;
begin
    if FCurveScalingEnabled then
    begin
        CalcProfileIntegral := GetCalcProfileIntegral;
        ProfileIntegral     := GetProfileIntegral;

        if (CalcProfileIntegral = 0) or (ProfileIntegral = 0) then
        begin
            Result := 1;
            Exit;
        end;
        Result := ProfileIntegral / CalcProfileIntegral;
    end
    else
        Result := 1;
end;

end.
