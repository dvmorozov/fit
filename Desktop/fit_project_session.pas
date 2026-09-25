// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A live problem into a project document, and a document back onto a
live problem.)

THE TWO HALVES OF THE FEATURE, and everything either of them touches goes through
IFitService - the same verbs the user's own gestures go through. Nothing here
reaches around the service to the engine, which is what makes a restored problem
indistinguishable from one built by hand and what stops this becoming a second
way to set up a fit.

WHAT A CAPTURE READS AND WHAT IS HANDED TO IT. The picks, the profile, the
background, the intervals, the settings and the fitted values are the engine's,
and are read from it. The argument axis, the picking mode, the tab in front,
which interval the user selected and where the data came from are the CLIENT'S -
the engine has never been told any of them - so they arrive as
TProjectClientContext rather than being invented here.

THE USER-DEFINED FORMULA IS IN THAT CONTEXT FOR A REASON WORTH KNOWING. The
server does not report the expression it is fitting: GET /special-params answers
with the parameters and not the formula, which findings.md records as a defect in
its own right. The client is the side that has it, so it hands it over. When that
gap is closed this can read it instead, and the document does not change.

THE ORDER OF AN APPLY IS NOT DECIDED HERE. fit_project_restore emits it as data,
because six of the engine's setters clear something another has just written and
every way of getting that wrong is silent. This unit is the loop that walks the
plan and the mapping from a step to the verbs that carry it out.
}
unit fit_project_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Variants,
    int_fit_service, points_set, title_points_set, fit_statistics,
    fit_points_json, log, curve_types_singleton, named_points_set,
    persistent_curve_parameters, persistent_curve_parameter_container,
    module_project_state, MyExceptions,
    fit_project_document, fit_project_restore,
    //  For TProjectParts: the fingerprint is reported part by part, and a
    //  part is what the file format already calls one.
    fit_project_archive;

type
    { What only the client knows, handed to a capture rather than read from the
      service - see the unit comment. }
    TProjectClientContext = record
        HasUi: boolean;
        Ui: TProjectUi;
        { The user-defined curve's formula, which the server will not report. }
        HasUserCurve: boolean;
        UserCurveExpression: string;
        { Which stretch of the profile the user selected, as INDICES: the
          profile may have been smoothed, so a coordinate need not name a
          sample any more. }
        SelectedIntervalInForce: boolean;
        SelectedIntervalFrom: longint;
        SelectedIntervalTo: longint;
        Provenance: TProjectProvenance;
    end;

{ A context with nothing in it, every field explicit. }
function EmptyProjectClientContext: TProjectClientContext;

{ Everything AService holds, plus what only AContext knows, as one document.

  APREVIOUS IS THE DOCUMENT THIS ONE REPLACES, and passing it is what keeps the
  format's one real promise. A part - or a member inside a part - written by a
  newer build is carried through untouched only if the writer starts from what
  was READ; a capture that started from an empty document threw all of it away
  on the first save, which is precisely the case the container exists to
  survive. Pass EmptyProjectDocument when there is nothing being replaced.

  NOTHING DERIVED IS TAKEN. The calculated profile, the delta and the fitted
  positions are all rebuilt from what this does capture; storing them beside
  their own inputs would make a second source of truth that can go stale. }
{ Whether a parameter's value is a QUANTITY, and so belongs in the file.

  WHAT THIS IS FOR. A project carries parameter values as doubles, because that
  is what an optimiser produces and what a restore hands back. A model may also
  hold parameters that are not quantities at all - a pattern's label, the handle
  of the wave it belongs to - and those arrive here as 0. Storing them was
  harmless; WRITING THEM BACK was not, and that is what a restore does: it wrote
  0 over the label a rebuilt instance had just been given, so the model that came
  back had lost the identity of every one of its parts.

  They are not lost by being left out. A parameter that is not a quantity is
  derived from what the module keeps - the markup, restored through the module's
  own project-state channel - and is regenerated when the model is rebuilt from
  it. What the project must not do is overwrite the answer with a zero.

  Exported so the rule can be tested for what it is: a decision about a value,
  with no service anywhere near it. }
function ValueIsAQuantity(const AValue: Variant): boolean;

function CaptureProject(AService: IFitService;
    const AContext: TProjectClientContext;
    const APrevious: TProjectDocument): TProjectDocument;

{ What the project would hold if it were saved now, reduced to one string that
  changes when - and only when - that content does: the window's own view (the
  tab, the selection, the picking mode) and the time stamps are left out; the
  axes, which are the project's, are not. What the
  workflow compares to tell unsaved work from none. '' when the engine cannot
  be asked, which the caller must read as "unknown", not as "empty". }
function ProjectFingerprint(AService: IFitService;
    const AContext: TProjectClientContext): string;

{ The same content, part by part: each part's name beside the MD5 of what that
  part holds. ProjectFingerprint is this list reduced to one string, so the two
  can never disagree about what was compared.

  WHY IT IS PUBLIC. "Project has been modified" with nothing modified is a
  report that cannot be acted on unless the program can say WHICH part of the
  document moved - the answer is a handful of bytes inside a section that the
  log truncates, and guessing at it is what this exists to stop. Empty when the
  engine could not be asked, exactly as ProjectFingerprint returns ''. }
function ProjectFingerprintParts(AService: IFitService;
    const AContext: TProjectClientContext): TProjectParts;

{ The names of the parts that do not hold the same thing in both lists, comma
  separated; '' when they agree. A part only one of them names is reported too,
  as "<name> (gone)" or "<name> (new)".

  BY NAME, NOT BY POSITION: a module's part appears and disappears with the
  module, and comparing two lists of different lengths pairwise would name every
  part after it. }
function DifferingPartNames(const ABefore, ANow: TProjectParts): string;

{ Those parts reduced to the one string ProjectFingerprint answers with; '' for
  an empty list, which is what a capture that failed returns. Exported so that a
  caller wanting both the parts and the fingerprint - the workflow does, one to
  compare and one to report with - asks the engine once rather than twice. }
function FingerprintOfParts(const AParts: TProjectParts): string;

{ Applies ADoc to AService, in the order fit_project_restore plans.

  AService must be a problem nothing else has been done to: setting a profile
  resets one, but a module session or a selected interval left over from a
  previous document would survive into this one.

  False, with AFault naming the step that failed, rather than an exception: this
  is driven by a menu command, and what the user needs to be told is which part
  of their project did not come back. }
function ApplyProject(AService: IFitService; const ADoc: TProjectDocument;
    out AFault: string): boolean;

implementation

uses
    md5, fit_project_json, special_curve_parameter;

{ ---- capture --------------------------------------------------------------- }

function EmptyProjectClientContext: TProjectClientContext;
begin
    Result := Default(TProjectClientContext);
end;

{ A point set as the document holds one. Takes ownership: every getter on
  IFitService that answers with a set hands over a copy. }
function TakePoints(APoints: TTitlePointsSet): TPointsData;
var
    i: longint;
begin
    Result := Default(TPointsData);
    if not Assigned(APoints) then
        Exit;
    try
        Result.Title := APoints.FTitle;
        SetLength(Result.X, APoints.PointsCount);
        SetLength(Result.Y, APoints.PointsCount);
        for i := 0 to APoints.PointsCount - 1 do
        begin
            Result.X[i] := APoints.PointXCoord[i];
            Result.Y[i] := APoints.PointYCoord[i];
        end;
    finally
        APoints.Free;
    end;
end;

function ValueIsAQuantity(const AValue: Variant): boolean;
begin
    //  Null is what a service answers for a parameter it could not report at
    //  all, and VarIsNumeric says nothing useful about it.
    Result := not VarIsNull(AValue) and VarIsNumeric(AValue);
end;

function CaptureCurves(AService: IFitService): TProjectCurves;
var
    i, j, n: longint;
    Nm: string;
    V: double;
    T: longint;
begin
    SetLength(Result, AService.GetCurveCount);
    for i := 0 to AService.GetCurveCount - 1 do
    begin
        Result[i].Id := AService.GetCurveInstanceId(i);
        Result[i].Fitted := AService.IsCurveFitted(i);
        SetLength(Result[i].Params, AService.GetCurveParameterCount(i));
        n := 0;
        for j := 0 to AService.GetCurveParameterCount(i) - 1 do
        begin
            //  QUANTITIES ONLY - see ValueIsAQuantity. A parameter holding a
            //  label or a handle is asked for by its own type and left out.
            if not ValueIsAQuantity(AService.GetCurveParameterValue(i, j)) then
                Continue;
            AService.GetCurveParameter(i, j, Nm, V, T);
            Result[i].Params[n].Name := Nm;
            Result[i].Params[n].Value := V;
            Result[i].Params[n].Error := AService.GetCurveParameterError(i, j);
            Inc(n);
        end;
        SetLength(Result[i].Params, n);
    end;
end;

function CaptureUserCurveParams(AService: IFitService): TProjectParams;
var
    CP: Curve_parameters;
    i: longint;
begin
    Result := nil;
    CP := AService.GetSpecialCurveParameters;
    if not Assigned(CP) then
        Exit;
    try
        SetLength(Result, CP.Count);
        for i := 0 to CP.Count - 1 do
        begin
            Result[i].Name := CP[i].Name;
            Result[i].Value := CP[i].Value;
            Result[i].Error := -1;
            Result[i].HasKind := True;
            Result[i].Kind := longint(CP[i].Type_);
            Result[i].Held := CP[i].VariationDisabled;
        end;
    finally
        CP.Free;
    end;
end;

function CaptureProject(AService: IFitService;
    const AContext: TProjectClientContext;
    const APrevious: TProjectDocument): TProjectDocument;
var
    Ids: TCurveInstanceIdList;
    States: TModuleStateArray;
    RFactor: double;
    i: longint;
begin
    Result := EmptyProjectDocument;

    //  EVERY PART AS IT WAS READ, before anything below overwrites the ones this
    //  build understands. Without it, opening a project written by a newer
    //  version and saving it deletes whatever that version added - the one
    //  outcome the whole part-based design exists to prevent, and one no user
    //  would have any way of noticing.
    Result.AsRead := APrevious.AsRead;

    Result.Profile := TakePoints(AService.GetProfilePointsSet);
    Result.Background := TakePoints(AService.GetBackgroundPoints);
    Result.Bounds := TakePoints(AService.GetRFactorBounds);
    Result.Positions := TakePoints(AService.GetCurvePositions);

    //  THE HANDLES, beside the picks they belong to and in the same order. Read
    //  as one answer rather than derived from the curve list, because a pick
    //  whose instance has not been built yet still has to hold its place.
    Ids := AService.GetCurvePositionIds;
    if Length(Ids) = Length(Result.Positions.X) then
    begin
        SetLength(Result.Positions.Ids, Length(Ids));
        for i := 0 to High(Ids) do
            Result.Positions.Ids[i] := Ids[i];
    end;

    Result.Settings.CurveTypeId := GUIDToString(AService.GetCurveType);
    Result.Settings.WaveLength := AService.GetWaveLength;
    Result.Settings.MaxRFactor := AService.GetMaxRFactor;
    Result.Settings.BackFactor := AService.GetBackFactor;
    Result.Settings.CurveThresh := AService.GetCurveThresh;
    Result.Settings.MinimizerKind := AService.GetMinimizerKind;
    Result.Settings.LossKind := AService.GetLossKind;
    Result.Settings.Weighting := AService.GetWeighting;
    Result.Settings.BackgroundVariationEnabled :=
        AService.GetBackgroundVariationEnabled;
    Result.Settings.CurveScalingEnabled := AService.GetCurveScalingEnabled;
    Result.Settings.Stated := AllProjectSettings;

    Result.Curves := CaptureCurves(AService);
    Result.Statistics := AService.GetStatistics;
    //  THE R-FACTOR OF THE FIT BEING SAVED. Reported as text because that is how
    //  the engine reports it, and text that is not a number means no fit has
    //  run - which stays as the -1 an empty document carries, so that "no fit"
    //  and "a perfect fit" are not the same value.
    if not TryStrToFloat(Trim(AService.GetRFactorStr), RFactor) then
        RFactor := -1;
    Result.RFactor := RFactor;

    //  WHEN, in UTC. Created is carried from the document being replaced: it
    //  means when this project came into being, not when it was last written.
    StampProject(Result, APrevious.CreatedUtc, Now);

    //  The client's own half.
    Result.HasUi := AContext.HasUi;
    Result.Ui := AContext.Ui;
    Result.SelectedIntervalInForce := AContext.SelectedIntervalInForce;
    Result.SelectedIntervalFrom := AContext.SelectedIntervalFrom;
    Result.SelectedIntervalTo := AContext.SelectedIntervalTo;
    Result.Provenance := AContext.Provenance;

    //  ASKED OF THE SERVICE, not handed in. The modules that matter are the
    //  SERVER's - the sessions the problem was built with - and a client may
    //  not have the same set linked. It is also the only way the framework can
    //  collect them without naming one.
    States := AService.GetModuleProjectStates;
    SetLength(Result.ModuleDocuments, Length(States));
    for i := 0 to High(States) do
    begin
        Result.ModuleDocuments[i].Module := States[i].Module;
        Result.ModuleDocuments[i].Content := States[i].Content;
    end;

    Result.HasUserCurve := AContext.HasUserCurve;
    if Result.HasUserCurve then
    begin
        Result.UserCurveExpression := AContext.UserCurveExpression;
        Result.UserCurveParams := CaptureUserCurveParams(AService);
    end;
end;

function ProjectFingerprintParts(AService: IFitService;
    const AContext: TProjectClientContext): TProjectParts;
var
    Doc: TProjectDocument;
    Parts: TProjectParts;
    i: longint;
begin
    Result := nil;
    try
        Doc := CaptureProject(AService, AContext, EmptyProjectDocument);
        Doc.CreatedUtc := '';
        Doc.ModifiedUtc := '';
        //  THE AXES ARE THE PROJECT'S, and are kept: an axis is saved with the
        //  project and restored from it, so choosing one is a change to the
        //  document. What the window happens to show - the tab in front, the
        //  selected curve, the picking mode - is not, and is left out.
        Doc.Ui.SelectionMode := 0;
        Doc.Ui.ActiveTab := 0;
        Doc.Ui.SelectedCurveId := '';
        //  THROUGH THE SAME ENCODING A SAVE WRITES, so nothing that reaches the
        //  file can change without changing this.
        Parts := ProjectToParts(Doc);
        SetLength(Result, Length(Parts));
        for i := 0 to High(Parts) do
        begin
            Result[i].Name := Parts[i].Name;
            //  THE HASH, NOT THE PART. What this is for is comparing and saying
            //  which one moved; keeping the whole document alive between one
            //  close and the next would be a second copy of the project in
            //  memory for a diagnostic.
            Result[i].Content := MD5Print(MD5String(Parts[i].Content));
        end;
    except
        //  An engine that cannot be reached cannot say what changed; the
        //  window's own flags are all that is left to go on.
        Result := nil;
    end;
end;

function FingerprintOfParts(const AParts: TProjectParts): string;
var
    Text: string;
    i: longint;
begin
    //  A DOCUMENT ALWAYS HAS PARTS - ProjectToParts writes four of them for an
    //  empty project - so none means the capture failed, which is the ''
    //  the caller must read as "unknown" rather than as "empty".
    if Length(AParts) = 0 then
        Exit('');
    Text := '';
    for i := 0 to High(AParts) do
        Text := Text + AParts[i].Name + #0 + AParts[i].Content + #0;
    Result := MD5Print(MD5String(Text));
end;

function ProjectFingerprint(AService: IFitService;
    const AContext: TProjectClientContext): string;
begin
    Result := FingerprintOfParts(ProjectFingerprintParts(AService, AContext));
end;

{ AList with AName appended, comma separated. }
function AndAlso(const AList, AName: string): string;
begin
    Result := AList;
    if Result <> '' then
        Result := Result + ', ';
    Result := Result + AName;
end;

function DifferingPartNames(const ABefore, ANow: TProjectParts): string;
var
    i, At: longint;
begin
    Result := '';
    for i := 0 to High(ANow) do
    begin
        At := IndexOfPart(ABefore, ANow[i].Name);
        if At < 0 then
            Result := AndAlso(Result, ANow[i].Name + ' (new)')
        else if ABefore[At].Content <> ANow[i].Content then
            Result := AndAlso(Result, ANow[i].Name);
    end;
    //  AND THE ONES THAT ARE NO LONGER THERE. A part that disappeared changes
    //  the document as surely as one that changed, and the loop above cannot
    //  see it.
    for i := 0 to High(ABefore) do
        if IndexOfPart(ANow, ABefore[i].Name) < 0 then
            Result := AndAlso(Result, ABefore[i].Name + ' (gone)');
end;

{ ---- apply ----------------------------------------------------------------- }

{ A document point set as the engine takes one. }
function GivePoints(const AP: TPointsData): TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.FTitle := AP.Title;
    for i := 0 to High(AP.X) do
        Result.AddNewPoint(AP.X[i], AP.Y[i]);
end;

procedure ApplySettings(AService: IFitService; const ADoc: TProjectDocument);
var
    TypeId: TGuid;
begin
    //  The curve type first among the settings, and before any handle is
    //  adopted: changing it clears every fitted flag.
    //
    //  AND ONLY WHEN THIS BUILD HAS IT. A project may name a type that came
    //  with an analysis pack this build does not carry, and the engine refuses
    //  an unregistered one - correctly, since it could not build a model from
    //  it. Asked here rather than caught, because refusing the whole file for
    //  that would make an otherwise perfectly readable project unopenable: the
    //  profile, the picks and every other setting are still exactly what they
    //  were, and losing all of them to one absent curve type is a far worse
    //  answer than opening on the type the engine already has.
    if (ADoc.Settings.CurveTypeId <> '') and
        TryStringToGUID(ADoc.Settings.CurveTypeId, TypeId) and
        (FindCurveClassById(TCurveTypeId(TypeId)) <> nil) then
        AService.SetCurveType(TypeId);
    //  ONLY WHAT THE DOCUMENT STATES: an absent member keeps the engine's value
    //  (TProjectSettingSet says why).
    with ADoc.Settings do
    begin
        if psWaveLength in Stated then
            AService.SetWaveLength(WaveLength);
        if psMaxRFactor in Stated then
            AService.SetMaxRFactor(MaxRFactor);
        if psBackFactor in Stated then
            AService.SetBackFactor(BackFactor);
        if psCurveThresh in Stated then
            AService.SetCurveThresh(CurveThresh);
        if psMinimizerKind in Stated then
            AService.SetMinimizerKind(MinimizerKind);
        if psLossKind in Stated then
            AService.SetLossKind(LossKind);
        if (psWeighting in Stated) and (Weighting <> '') then
            AService.SetWeighting(Weighting);
        if psBackgroundVariation in Stated then
            AService.SetBackgroundVariationEnabled(BackgroundVariationEnabled);
        if psCurveScaling in Stated then
            AService.SetCurveScalingEnabled(CurveScalingEnabled);
    end;
end;

procedure ApplyUserCurve(AService: IFitService; const ADoc: TProjectDocument);
var
    CP: Curve_parameters;
    Container: TPersistentCurveParameterContainer;
    i: longint;
begin
    CP := Curve_parameters.Create(nil);
    //  Curve_parameters starts with one placeholder parameter, which is not one
    //  of ours - the same clear the REST layer does before filling it.
    CP.Params.Clear;
    for i := 0 to High(ADoc.UserCurveParams) do
    begin
        Container := TPersistentCurveParameterContainer(CP.Params.Add);
        Container.Parameter.Name := ADoc.UserCurveParams[i].Name;
        Container.Parameter.Value := ADoc.UserCurveParams[i].Value;
        if ADoc.UserCurveParams[i].HasKind then
            Container.Parameter.Type_ :=
                TParameterType(ADoc.UserCurveParams[i].Kind);
        Container.Parameter.VariationDisabled := ADoc.UserCurveParams[i].Held;
    end;
    //  The engine takes ownership of the parameter set.
    AService.SetSpecialCurveParameters(ADoc.UserCurveExpression, CP);
end;

procedure ApplyPositions(AService: IFitService; const ADoc: TProjectDocument);
var
    Ids: TCurveInstanceIdList;
    i: longint;
begin
    SetLength(Ids, Length(ADoc.Positions.Ids));
    for i := 0 to High(ADoc.Positions.Ids) do
        Ids[i] := ADoc.Positions.Ids[i];
    //  THE PIVOT. The picks and their handles go together, so the instances
    //  rebuilt from them are the same instances the values are stored under.
    AService.SetCurvePositions(GivePoints(ADoc.Positions), Ids);
end;

procedure ApplyCurveValues(AService: IFitService; const ADoc: TProjectDocument);
var
    Entries: TCurveValuesList;
    i, j, n, Index_: longint;
begin
    SetLength(Entries, 0);
    n := 0;
    for i := 0 to High(ADoc.Curves) do
    begin
        //  BY HANDLE, resolved here because IFitService's members are ordinal
        //  and an index never outlives the request that made it.
        Index_ := AService.IndexOfCurveInstance(ADoc.Curves[i].Id);
        //  A handle the restored model does not hold is SKIPPED rather than
        //  fatal, and that is a judgement rather than laziness: it means the
        //  project describes a curve this build's model did not rebuild - a
        //  curve type that is gone, or a module that is not in this build - and
        //  refusing the whole file would make one absent extension cost the
        //  user everything else in their project.
        if Index_ < 0 then
            Continue;
        SetLength(Entries, n + 1);
        Entries[n].CurveIndex := Index_;
        Entries[n].Fitted := ADoc.Curves[i].Fitted;
        SetLength(Entries[n].Params, Length(ADoc.Curves[i].Params));
        for j := 0 to High(ADoc.Curves[i].Params) do
        begin
            Entries[n].Params[j].Name := ADoc.Curves[i].Params[j].Name;
            Entries[n].Params[j].Value := ADoc.Curves[i].Params[j].Value;
            Entries[n].Params[j].Error := ADoc.Curves[i].Params[j].Error;
        end;
        Inc(n);
    end;
    if n > 0 then
        AService.SetCurveValues(Entries);
end;

procedure ApplyModules(AService: IFitService; const ADoc: TProjectDocument);
var
    i: longint;
begin
    for i := 0 to High(ADoc.ModuleDocuments) do
        //  Through the module's own resource channel. The framework hands the
        //  document back to the module that wrote it and never reads it, which
        //  is what lets a module extend the project file without the framework
        //  naming a module.
        try
            AService.ModulePost(ADoc.ModuleDocuments[i].Module + '/' +
                ProjectStateResource, ADoc.ModuleDocuments[i].Content);
        except
            //  NOBODY ANSWERS, which means this build does not carry that pack.
            //  Skipped rather than fatal, and this is the whole point of a
            //  part-based format: the section stays in the file - the document
            //  carries it through untouched - so a build that does have the
            //  pack still finds it, and everything else in the project opens
            //  here meanwhile.
            //
            //  Refusing would be the opposite of what the container was built
            //  for: it would make one absent extension cost the user every
            //  other thing in their project.
            on E: EUserException do
                WriteLog('This build has no component that keeps "' +
                    ADoc.ModuleDocuments[i].Module +
                    '" state, so that part of the project was left as it is.',
                    Warning);
        end;
end;

procedure ApplyStep(AService: IFitService; const ADoc: TProjectDocument;
    AStep: TRestoreStep);
begin
    case AStep of
        rsProfile:
            AService.SetProfilePointsSet(GivePoints(ADoc.Profile));
        rsSettings:
            ApplySettings(AService, ADoc);
        rsUserCurve:
            ApplyUserCurve(AService, ADoc);
        rsBackground:
            AService.SetBackgroundPointsSet(GivePoints(ADoc.Background));
        rsSelectInterval:
            AService.SelectProfileInterval(ADoc.SelectedIntervalFrom,
                ADoc.SelectedIntervalTo);
        rsBounds:
            AService.SetRFactorBounds(GivePoints(ADoc.Bounds));
        rsPositions:
            ApplyPositions(AService, ADoc);
        rsModules:
            ApplyModules(AService, ADoc);
        rsCurveValues:
            ApplyCurveValues(AService, ADoc);
    end;
end;

function ApplyProject(AService: IFitService; const ADoc: TProjectDocument;
    out AFault: string): boolean;
var
    Plan: TRestorePlan;
    i: longint;
begin
    AFault := '';
    Result := True;
    Plan := PlanRestore(ADoc);
    for i := 0 to High(Plan) do
        try
            ApplyStep(AService, ADoc, Plan[i]);
        except
            //  STOPS AT THE FIRST FAILURE AND SAYS WHICH STEP. Carrying on
            //  would build the rest of the model on top of a step that did not
            //  happen, and the result would be a project that opened and was
            //  quietly wrong - which is worse than one that did not open.
            on E: Exception do
            begin
                AFault := 'This project could not be restored: ' +
                    RestoreStepName(Plan[i]) + ' - ' + E.Message;
                Exit(False);
            end;
        end;
end;

end.
