// SPDX-License-Identifier: GPL-3.0-or-later
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
    self_copied_component, SysUtils, Variants, title_points_set

    , persistent_curve_parameters
    , fit_statistics
    ;

const
    { Minimizer algorithm selection (the seam for pluggable optimizers). Only the
      native Downhill Simplex exists today; new algorithms - e.g. the Stage 2
      Python Levenberg-Marquardt - append their own MIN_KIND_* value here. }
    MIN_KIND_DHS = 0;   //  Downhill Simplex (default; the original algorithm)
    MIN_KIND_PYTHON_LM = 1;   //  Python/lmfit Levenberg-Marquardt (sidecar backend)

type
    { The handles a bulk pick write may carry, one per point and in the same
      order - the text form of TCurveInstanceId, which is what every other
      member of this contract already trades handles in.

      TEXT, not TCurveInstanceId: this interface deals in handles as strings
      throughout (GetCurveInstanceId, IndexOfCurveInstance), so that a client
      never has to name the identity unit to talk to the engine. }
    TCurveInstanceIdList = array of string;

    { One module's own document, as the module wrote it.

      OPAQUE. The framework stores the text under the module's name and hands it
      back unchanged; reading it would mean the framework knowing what a module
      keeps, which is the one thing the module contract exists to avoid. }
    TModuleStateEntry = record
        Module: string;
        Content: string;
    end;
    TModuleStateArray = array of TModuleStateEntry;

    { One parameter's value as a restore hands it back: addressed BY NAME,
      because a model may hold curves of DIFFERENT types and two types can
      differ in parameter count and, at equal counts, in parameter name. An
      ordinal would put one curve's value under another's heading, which is a
      defect this codebase has already had once, in the parameters grid. }
    TCurveParamValue = record
        Name:  string;
        Value: double;
        { The optimiser's standard error; < 0 when it never estimated one. }
        Error: double;
    end;
    TCurveParamValues = array of TCurveParamValue;

    { Everything a restore has to say about ONE instance.

      ORDINAL, like every other member of this contract: a handle is resolved to
      an index at the wire's own boundary, so an index never outlives the
      request that made it.

      NO TEXT-VALUED PARAMETER, and that is deliberate rather than missing. The
      only text-valued parameters are a module's own (an identity, a label), and
      those are DERIVED by the module from state a project restores through the
      module's own channel - so writing one back here would be a second way to
      set the same thing, and one the framework would have to name a module to
      use. }
    TCurveValuesEntry = record
        CurveIndex: longint;
        Params: TCurveParamValues;
        { Whether an OPTIMISER produced these, as opposed to their being seeds a
          project happened to save. It cannot be derived - every instance has
          values from the moment it is placed - and it decides whether a rebuild
          re-seeds, whether a markup move is refused, and whether an instance
          the model has lost is reported as an orphan. }
        Fitted: boolean;
    end;
    TCurveValuesList = array of TCurveValuesEntry;

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
        { Selecting the curve type must go over the wire for a thin client (the
          in-process singleton is not reachable from another machine). }
        procedure SetCurveType(ACurveTypeId: TCurveTypeId);
        function GetState: TFitServerState;
        function GetWaveLength: double;
        procedure SetWaveLength(AWaveLength: double);
        function GetBackgroundVariationEnabled: boolean;
        procedure SetBackgroundVariationEnabled(AEnable: boolean);
        { Selected minimizer algorithm (MIN_KIND_* constant). }
        function GetMinimizerKind: longint;
        procedure SetMinimizerKind(AKind: longint);
        { The objective being minimised (LOSS_KIND_* in fit_loss). Which values
          are legitimate depends on the model - see loss_compatibility - and the
          engine substitutes a compatible one rather than fitting something it
          cannot trust. The statistics reported after a fit do NOT follow this:
          they are always computed from the same fixed residual, so figures from
          two fits stay comparable. }
        function GetLossKind: longint;
        procedure SetLossKind(AKind: longint);
        { Residual weighting for the Python backend: 'poisson' or 'none'. The
          native engine always fits unweighted and ignores this. }
        function GetWeighting: string;
        procedure SetWeighting(const AValue: string);
        { URL of the standalone compute server; empty = fit in-process. }
        function GetServerUrl: string;
        procedure SetServerUrl(const AUrl: string);
        function GetCurveScalingEnabled: boolean;
        procedure SetCurveScalingEnabled(AEnabled: boolean);

        { All GetXXXX methods create and return A NEW OBJECT,
          responsibility to free it is put on calling code. }

        { Returns hint or error message received from the server. }
        function SetProfilePointsSet(APointsSet: TTitlePointsSet): string;
        function GetProfilePointsSet: TTitlePointsSet;
        function GetSelectedProfileInterval: TTitlePointsSet;

        { Returns hint or error message received from the server. }
        function SetBackgroundPointsSet(ABackgroundPoints: TTitlePointsSet): string;
        function GetBackgroundPoints: TTitlePointsSet;

        { Returns hint or error message received from the server.

          AIDS IS THE HANDLE EACH PICK'S CURVE IS KNOWN BY, in the same order,
          and it is OPTIONAL: an empty list means "I have none", which is what
          every caller said before project files existed and is still what an
          ordinary interactive edit says.

          WHY IT IS HERE AND NOT ITS OWN VERB. Identity is issued to the model
          INPUT and inherited by the instance rebuilt from it, so the pick and
          its handle are one fact and must arrive together. A second call
          carrying handles would be a join key back to this one - which is the
          tell of a bypass - and would have to survive the rebuild this write
          triggers, in between.

          Offering a handle for a pick ADOPTS it, so the instance rebuilt at
          that abscissa is the same instance to everything downstream and the
          values a previous fit found for it can be handed back. An empty entry
          means "issue one", so a caller may know some handles and not others.

          A list of the wrong length is REFUSED rather than padded: nothing can
          know which pick the missing entry belonged to, and a wrong guess
          attaches one curve's values to another undetectably. }
        function SetCurvePositions(ACurvePositions: TPointsSet;
            const AIds: TCurveInstanceIdList = nil): string;
        { The user's PICKS: model input, unique X, each one a sample of the
          profile. A fit never changes them. }
        function GetCurvePositions: TTitlePointsSet;
        { The handle of the curve each pick stands for, in pick order.

          SYMMETRIC WITH THE WRITE, and that is the point: a caller reads the
          picks and these together and can hand exactly that back later.
          Without it, saving a model would mean re-deriving which curve each
          pick belongs to - the correspondence the handle exists to carry, and
          the one a content-addressed key could not keep.

          One entry per pick, ALWAYS, even where a pick has no instance yet
          (nothing can be built until there is a fit interval). That entry is
          EMPTY rather than absent, so Ids[i] always names pick i and a caller
          pairing them positionally cannot slide. }
        function GetCurvePositionIds: TCurveInstanceIdList;
        { Where the built model's curves actually sit: one point per instance
          with a position parameter, at its own fitted x0. Derived and
          read-only, so - unlike the picks - it may hold an x off the sample
          grid and two instances that converged on one value. }
        function GetResultedCurvePositions: TTitlePointsSet;

        { Returns hint or error message received from the server. }
        function SetRFactorBounds(ARFactorBounds: TPointsSet): string;
        function GetRFactorBounds: TTitlePointsSet;

        { ------------------------- module resources -------------------------

          One pair of verbs for everything a module contributes, carrying JSON
          the module defines. What used to be here instead was five verbs
          belonging to one analysis pack, which also forced this unit - the
          contract between client and engine - to use that pack's wire records.
          A framework contract that cannot compile without a particular module
          is not a framework contract.

          The reply is the resource itself, not an ok-wrapped envelope: these
          payloads already crossed the wire that way. }
        { Every module's project-state document, in one answer.

          COLLECTED SERVER-SIDE, over the registry the problem's sessions were
          made from - so the framework names no module, and a client that has a
          different set of modules linked cannot ask for the wrong ones.

          Only modules that DECLARE the resource are asked. ModuleGet raises
          when nothing answers, and a module with nothing to keep is not an
          error; the declaration is what tells the two apart, exactly as it does
          for the sidecar and long-running flags. }
        function GetModuleProjectStates: TModuleStateArray;
        function ModuleGet(const AResource: string): string;
        function ModulePost(const AResource, APayload: string): string;

        function GetSpecialCurveParameters: Curve_parameters;
        procedure SetSpecialCurveParameters(ACurveExpr: string;
        { Nil means initialization. }
            CP: Curve_parameters);
        { Forgets the user-defined formula. The client calls it when the user
          curve that was being fitted is deleted: without it the server would go
          on building curves from a formula that no longer exists anywhere in the
          UI, which is how a deleted curve type kept appearing in the fit. After
          this the user-defined type cannot be fitted until a formula is set
          again - see TFitService.CreateTasks. }
        procedure ClearSpecialCurve;

        procedure AddPointToProfile(XValue, YValue: double);
        procedure AddPointToBackground(XValue, YValue: double);
        procedure AddPointToRFactorBounds(XValue, YValue: double);
        procedure AddPointToCurvePositions(XValue, YValue: double);
        { A pick into a module's own point set, named by the module. Its own
          verb rather than one of the four above, because the shared add-a-point
          helper treats a repeated x as an edit-then-delete - right for a flat
          set, fatal for one whose items may share endpoints. }
        procedure AddPointToSet(const AKind: string; XValue, YValue: double);

        procedure ReplacePointInProfile(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInBackground(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInRFactorBounds(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInCurvePositions(
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);
        procedure ReplacePointInSet(const AKind: string;
            PrevXValue, PrevYValue, NewXValue, NewYValue: double);

        function GetCurveAttributes: TMSCRCurveList;
        function GetCurveCount: longint;
        { WHICH CURVE the one at ACurveIndex is - the handle issued to the
          instance, as text. Empty when the index names nothing.

          The ordinal methods beside this one stay ordinal on purpose: the wire
          resolves a handle to an index at its own boundary, so a caller cannot
          hold an index across an edit that reorders the model, and nothing in
          this contract has to change shape for it. }
        function GetCurveInstanceId(ACurveIndex: longint): string;
        { Whether an OPTIMISER produced the values the curve at ACurveIndex
          carries, as opposed to their being the seeds it was placed with.

          IT CANNOT BE DERIVED from the values: every instance has some from the
          moment it is placed. Nothing could read it before, so a client could
          save a model and not save whether it had been fitted - and restoring
          seeds as though they were results refuses edits to protect a fit that
          never happened, while restoring results as though they were seeds
          throws the fit away on the next rebuild. }
        function IsCurveFitted(ACurveIndex: longint): boolean;
        { The index the handle names right now, or -1 when the model no longer
          holds that instance. }
        function IndexOfCurveInstance(const AInstanceId: string): longint;
        { Removes the curve at ACurveIndex from the model.

          ORDINAL, like every other member here, and resolved from a handle at
          the wire's own boundary - see the note above.

          IT REMOVES THE PICK TOO, and it has to. The model is rebuilt from its
          inputs on every edit: RecreateCurves drops an instance whose position
          is no longer among the picks and creates one for every pick that has
          none, so dropping only the identity would leave the pick standing and
          the next rebuild would make a fresh instance there with a NEW handle.
          The curve would come back, unfitted, and the deletion would have
          undone itself.

          That pairing is not new: AdoptCurveRemovalsFromTasks already does
          exactly it in the other direction, when the optimiser is the one that
          drops a curve. This is the same operation asked for by the user.

          Answers the state message the other model edits answer with. }
        function DeleteCurve(ACurveIndex: longint): string;
        { The ordinal methods below take a POSITION in the model, not a handle.
          That is deliberate and safe: the position is resolved from a handle at
          the wire's own boundary, so it never outlives the request that made
          it. The parameter used to be called SpecIndex - "specimen index",
          vocabulary from the diffraction application this framework grew out
          of, and long since not what it names. }
        function GetCurveParameterCount(ACurveIndex: longint): longint;
        procedure GetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            var Name: string; var Value: double; var Type_: longint);
        { Estimated standard error of a fitted parameter; < 0 when unavailable. }
        function GetCurveParameterError(ACurveIndex: longint;
            ParamIndex: longint): double;
        { The parameter's value WITH its type - the same single member
          GetCurveParameter returns as a double. Use it for values that are not
          quantities (identity, labels). }
        function GetCurveParameterValue(ACurveIndex: longint;
            ParamIndex: longint): Variant;
        procedure SetCurveParameter(ACurveIndex: longint; ParamIndex: longint;
            Value: double);
        { Writes the values a previous fit found onto SEVERAL instances at once,
          and rebuilds the model ONCE.

          WHY NOT THE PER-PARAMETER SETTER ABOVE, REPEATED. Every call to it
          runs a whole rebuild, so restoring a ten-curve model would be fifty
          requests and fifty rebuilds. More than the cost: that setter takes a
          number and nothing else, so it cannot say whether an OPTIMISER
          produced the value - and without that a restored fit is
          indistinguishable from a restored seed.

          Returns the state message the other model edits answer with. }
        function SetCurveValues(const AEntries: TCurveValuesList): string;
        function GetCurves: TSelfCopiedCompList;

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
        { Returns hint or error message received from the server. }
        function MinimizeDifferenceAgain: string;
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
        { Control methods. }

        procedure StopAsyncOper;
        function AsyncOper: boolean;
        function GetCalcTimeStr: string;
        function GetRFactorStr: string;
        function GetAbsRFactorStr: string;
        function GetSqrRFactorStr: string;
        { Goodness-of-fit statistics for the current fit (Valid=False if none). }
        function GetStatistics: TFitStatistics;

        { Synchronous methods. }
        { Returns hint or error message received from the server. }
        function SelectProfileInterval(StartPointIndex, StopPointIndex: longint): string;
        { Returns hint or error message received from the server. }
        function SelectEntireProfile: string;
        procedure CreateCurveList;
    end;

{ The state's name, for diagnostics. }
function FitServerStateName(AState: TFitServerState): string;

implementation

function FitServerStateName(AState: TFitServerState): string;
begin
    case AState of
        ProfileWaiting: Result := 'ProfileWaiting';
        BackNotRemoved: Result := 'BackNotRemoved';
        AsyncOperation: Result := 'AsyncOperation';
        ReadyForAutoFit: Result := 'ReadyForAutoFit';
        ReadyForFit: Result := 'ReadyForFit';
        Finished: Result := 'Finished';
        else
            Result := 'Unknown(' + IntToStr(Ord(AState)) + ')';
    end;
end;

end.
