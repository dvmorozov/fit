// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base curve class allowing setting up type name.)

Copyright (C) Dmitry Morozov
}
unit named_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    SysUtils, Types, coordinate_axis, configurable_points_set, curve_points_set,
    explanation, self_copied_component, persistent_curve_parameters,
    fit_reduction, noise_rule;

type
    TNamedPointsSetClass = class of TNamedPointsSet;
    TExtremumMode   = (
        OnlyMaximums,
        OnlyMinimums,
        MaximumsAndMinimums
        );
    TCurveTypeId    = TGuid;
    { A curve type that names no noise rule (TNamedPointsSet.NoiseRule): raised
      when one is registered or an instance of one is made. }
    ECurveTypeWithoutNoiseRule = class(Exception);
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    private
        { The attribute should not be used in descendants. }
        FName: string;

    public
        { Sets name of curve type. The method is used in deserializing
          objects received from server. }
        procedure SetCurveTypeName(Name: string); virtual;
        { Returns unique name of curve type. }
        class function GetCurveTypeName: string; virtual; abstract;
        { Returns unique type identifier. }
        class function GetCurveTypeId: TCurveTypeId; virtual; abstract;
        { The curve's analytic formula as a text expression in x and its parameter
          names, in numpy syntax (e.g.
          'A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))'). This is what lets
          the Python backend stay model-agnostic: it evaluates whatever formula it
          is sent instead of re-implementing each curve type. Empty when the curve
          has no closed-form expression the backend can evaluate. }
        function GetCurveExpression: string; virtual;
        { True when instances of this type have a closed-form expression that
          the formula-based backends (Python sidecar, remote compute server) can
          evaluate - i.e. exactly when GetCurveExpression returns non-empty.

          It exists at CLASS level because the UI has to decide whether to offer
          those backends before any instance is placed. The two must agree, and
          a test walks the registry asserting they do, so a future type cannot
          override one and forget the other. }
        class function IsAnalytic: boolean; virtual;
        { Optional grouping for the curve-type menu. Empty (the default) means
          ungrouped, so every existing curve type keeps appearing exactly where
          it does now; a non-empty group asks the UI for a submenu of that name.
          Introduced for a wave-pattern pack, whose six pattern types would
          otherwise crowd the flat list. }
        class function GetCurveTypeGroup: string; virtual;
        { True when this type's amplitude can move freely over orders of
          magnitude during a fit, rather than being pinned near the data by how
          the curve is seeded.

          A capability, not a list of type names: whether a given objective
          may be used with a given curve type is DERIVED from this, so adding a
          seventh pattern type needs no edit to any compatibility table.

          What it guards: an objective normalised by the model's own integral
          (fit_loss.LossIsSelfNormalising) can be reduced by inflating the model
          instead of by fitting it, because the numerator is scale-invariant
          while the denominator is not. A peak never exploits that - its
          amplitude is seeded from the data and stays there - which is why the
          defect went unnoticed for 25 years. A curve free to grow exploits it
          immediately, so that pairing is refused. }
        class function AmplitudeIsUnbounded: boolean; virtual;
        { The named point set instances of this type are PLACED from, or empty
          when they are placed from a single curve position.

          A capability, not a type test. The engine has one decision to
          make before it can build a model - where the curves come from - and
          this is the answer to it: empty means the existing path, one x per
          curve, which is every peak type and therefore the default. A non-empty
          name says the type is placed by marking an extent, and names the point
          set the picks are collected into ('wave-bounds' for a wave
          pattern).

          WHY A NAME AND NOT A FLAG: the same answer tells the client which set
          a pick belongs to and the server which stored set to slice per fit
          interval, so a module can bring its own point set without another
          edit to the engine.

          WHY ASKED OF THE CLASS: the decision is a property of the type, known
          before anything is built. Deriving it from an attempt to build - "did
          the module handle this?" - is what makes an ordinary situation (the
          type is selected, nothing is marked yet) fall through to the
          position-based path, which with nothing marked generates one curve per
          data point. That presented as a hang once already. }
        class function PlacedByPointSet: string; virtual;
        { True for a BACKGROUND SHAPE: a curve put under the peaks of a fit
          interval rather than at a pick, one per interval, and never taken out
          by curve reduction.

          A capability, not a type list. Everything that treats the
          background differently - the peak-type menu leaving it out, the
          background menu offering it, the engine building one per interval
          beside the picks, curve reduction passing it by, the refusals in
          fit_advice - asks this and nothing else, so a module's own background
          shape takes part without an edit anywhere.

          False by default: every type this framework had before is a peak. }
        class function IsBackground: boolean; virtual;
        { Whether only the module that owns this type places its curves - over
          what its own computation decides, never at a pick of the user's. Such
          a type is not offered to be chosen (curve_type_menu) and is refused
          as the chosen type (fit_advice.AdviseChosenCurveType). False by
          default: every type is offered unless it says otherwise. }
        class function IsPlacedOnlyByItsModule: boolean; virtual;
        { Whether a model made of this type takes a separate BACKGROUND curve.
          False for a type whose curves describe the whole of the data, their
          level included: a background beside them would be counted twice.
          The engine refuses one, and the background menu greys out, through
          fit_advice.AdviseBackgroundForModel. True by default - a peak sits on
          a baseline. A capability, like IsBackground, so a module's types
          answer it without a list anywhere. }
        class function AcceptsBackground: boolean; virtual;
        { Whether this is the background Fit > Automatically puts under a
          model that has none. At most one registered type says so
          (testcase_background_shapes walks them), and the registry finds it
          (curve_types_singleton.DefaultBackgroundCurveTypeId) - so the engine
          names no background class, and a build with no background shape adds
          none. False by default. }
        class function IsDefaultBackground: boolean; virtual;
        { Whether the type IS a formula someone writes - the user's own - so
          that the shape lives in an expression and its parameters, not in
          the class. The engine hands every instance of such a type the
          formula it holds (ApplyFormula), refuses the type while it holds
          none, and starts a width from its fit interval rather than from the
          class. A capability, so the engine names no formula class - which is
          what let the user's formula type leave the framework for the
          scientific module (2026-10-17) - and a module may bring a formula
          type of its own. False by default. }
        class function IsDefinedByFormula: boolean; virtual;
        { Takes the formula and the parameters it names, for a type that
          IsDefinedByFormula; AParams becomes the instance's. A type that is
          not one is never asked, and frees what it is given. }
        procedure ApplyFormula(const AExpression: string;
            AParams: Curve_parameters); virtual;
        { Whether a reduction may remove this instance - TFitTask's reduction of
          the instances a module placed, after an automatic run's fit. True by
          default. An instance others hang from answers False, so a reduction
          goes from the innermost out; so does one whose removal would leave a
          stretch of the data with no instance at all. }
        function RemovableInReduction: boolean; virtual;
        { Whether the instance, as fitted, describes nothing the noise could not
          - ANoiseSigma being the data's typical noise. A reduction removes such
          a removable instance whatever BIC says: BIC weighs the noise as
          measured, and a shape squeezed to a jump or shrunk below the noise
          can still seem to pay. False by default. }
        function DescribesOnlyNoise(ANoiseSigma: double): boolean; virtual;
        { THE RULE THE NOISE OF THIS TYPE'S DATA IS MEASURED BY - the field's,
          since the noise is: a count's variance grows with the count, a
          price's with the square of the price (noise_rule). Everything that
          weighs a difference by the noise asks it: the fit's figures, the
          reduction's BIC, the verdict and the shaded stretches, the
          Difference structure strip and a module's scalogram.

          BY CONSTRUCTION, NOT BY CONVENTION. The base names none, and a type
          that names none is refused when it is registered
          (TCurveTypesSingleton.RegisterCurveType) and when an instance of it
          is made (AfterConstruction). Only the bare TNamedPointsSet is made
          without one: it is no curve type - it has no name and no id - but
          the shell the client draws the server's points in (wire_point_sets),
          judging nothing. }
        class function NoiseRule: TNoiseRuleClass; virtual;
        { True when the shape is defined only where x > 0 - a power law of x.
          fit_advice refuses such a shape as the background of data that
          reaches zero, in words, rather than letting the fit meet a NaN. }
        class function ArgumentMustBePositive: boolean; virtual;
        { True when no value of this curve may be below zero anywhere in its
          window - a background, which is a count of what lies under the peaks.

          A capability, not a type list: the engine lifts such a curve
          after every calculation (KeepValuesInRange) until its lowest point in
          the window is zero, and a formula backend is told to do the same
          (TFitCurveData.NonNegative), so a module's background takes part by
          answering this alone.

          WHY A LIFT, and not a clip at zero or a penalty in the objective. A
          clip leaves the parameters of a curve that is wholly below zero with
          nothing to change - every one of them gives the same zero - so the
          fit can never bring it back. A penalty only discourages: the fit
          trades it against the misfit, and a background still goes below zero
          when that pays. The lift is a guarantee and keeps the curve's own
          shape: the parameters that shape it still move the result, and the
          level, held at the lift, can still rise from it.

          False by default: a peak's sign is already its amplitude's, and
          amplitudes are bounded at zero. }
        class function ValuesAreNonNegative: boolean; virtual;
        { The parameter that is this curve's LEVEL - adding to it raises every
          value by the same amount - or '' when it has none. The lift is added
          to it (AbsorbLift), and a formula backend is told it so the level it
          returns is the one drawn. None by default. }
        function LevelParameterName: string; virtual;
        { Whether THIS instance is kept above zero: its type's values are not
          negative, and the data in its window are not negative either. The
          engine lifts by this, and a formula backend is told it. }
        function KeepsAboveZero: boolean;
        { Sets this curve's starting values from the data's own baseline: AX and
          AY are points of the profile the curve will be fitted to, taken where
          there is no peak (background_search.ProposeBackgroundPoints), in any
          order.

          What a pick is to a peak, this is to a background: the seed. Called by
          the engine only for a type whose IsBackground is True, BEFORE the
          values a previous fit found are restored, so a fitted background
          keeps its fit and a new one starts from the data. Does nothing by
          default. }
        procedure SeedFromBaseline(const AX, AY: array of double); virtual;
    protected
        { Lifts the values until the lowest is zero, for a type whose values
          may not be negative. }
        procedure KeepValuesInRange; override;
        { Called with the amount the values were just lifted by, so a type
          with a level among its parameters adds it there and the parameters
          the user reads describe the curve that is drawn. True when it did -
          the values are then recalculated from the parameters. By default it
          adds the lift to LevelParameterName, and answers False for a type
          that names none: that type keeps its parameters, and draws the lift. }
        function AbsorbLift(const ADelta: double): boolean; virtual;
        { Sets to zero every value a rounding error below it; True when the
          lowest value was no more than that. }
        function ZeroRoundingBelowZero: boolean;
    public
        { What each of this curve's points is DRAWN ON, given the whole model
          AModel it belongs to, or nil - the default - for zero.

          A CAPABILITY, NOT A BRANCH. A model is a sum of curves, and a
          component whose contribution is a deviation - a nested pattern, its
          own wiggle about its parent's leg, exactly zero at both ends - is
          correct as computed and useless as drawn: it sits at the bottom of
          the chart, far from what it belongs to. This says where it belongs
          without changing what it contributes: the fit, the residual and every
          statistic read PointYCoord alone, and only the chart adds this
          (TTitlePointsSet.DrawnY).

          WHY THE WHOLE MODEL: what a component rests on is other components -
          its parent, and the parent's parent - so the answer needs the list,
          not the curve alone. WHY ONE ARRAY, not a value per point: a type
          that has to index the model before it can answer does that once.

          ASKED ON THE SERVER, wherever a curve's points are sent - the points
          route and every animated frame (TFitService.CurveDrawnBaseline) - and
          carried as TPointsData.Baseline. An array of another length than
          PointsCount is not sent.

          REJECTED: drawing the line from a module's overlay as a series of
          its own. The framework's near-zero line would still be drawn beside
          it, and the overlay is not redrawn while a fit runs, so the line
          would freeze for the length of every fit. And shifting the VALUES
          instead would count what it rests on twice in the model's sum. }
        function DrawnBaselineIn(AModel: TSelfCopiedCompList): TDoubleDynArray;
            virtual;
        { The axis mode (axis_mode_registry) this curve type's ADimension is
          meant to be shown in, by id, or '' for none.

          A PREFERENCE, NOT AN AXIS. The type names a registered mode and the
          automatic rule (axis_choice) weighs it against the data's and the
          other curves'; the type builds nothing, so it needs no wavelength or
          any other setting, and a module's type can name its module's modes.

          None by default: a type that is about no field in particular (a user
          formula) says nothing, and the data or the general name answers.
          Saying nothing is also right for a coordinate a type cannot know - a
          wave pattern's value is a price, but whether its argument is a bar or
          a date is the data's to say. }
        class function PreferredAxisMode(ADimension: TAxisDimension): string; virtual;
        { A WEAKER preference: the axis mode ADimension is shown in when the
          data says nothing about it. Where PreferredAxisMode outranks the
          data, this yields to it - for a coordinate the type can assume but
          not know. A wave pattern's argument is a bar number unless the series
          was read by date, and then the data says Date. '' for none. }
        class function FallbackAxisMode(ADimension: TAxisDimension): string; virtual;
        { Returns algorithm of searching of extremum points. }
        class function GetExtremumMode: TExtremumMode; virtual; abstract;
        class function GetConfigurablePointsSet: TConfigurablePointsSetClass; virtual;
        { What this curve type is, in words a user can learn from: what its
          formula means, what each parameter controls, where the shape is used,
          what it does not cover, and where to read about it.

          THE CLASS STATES THE CONTENT; the curve-type explanation provider
          stamps the topic and, when this leaves it empty, the title - so an
          override never repeats the type's identity and cannot get it wrong.

          INCOMPLETE BY DEFAULT, deliberately. The base answers with no summary
          and no body, so a type that does not override this fails the
          registry-walking completeness test by name, instead of shipping a
          curve type the application cannot explain. }
        class function Explanation: TExplanation; virtual;
        { Refuses an instance of a curve type that names no noise rule. }
        procedure AfterConstruction; override;
    end;

{ Raises ECurveTypeWithoutNoiseRule when AClass - a curve type, not the bare
  TNamedPointsSet - names no noise rule. AWhen says what was refused. }
procedure CheckNoiseRule(AClass: TNamedPointsSetClass; const AWhen: string);

{ THE NOISE A MODEL OF CURVES OF ATYPES WEIGHS ITS RESIDUALS BY, at each point
  of AY - the whole profile, so the noise is the series' and not the fitting
  window's, as a module's own decisions measure it - and ARule, the rule that
  measured it; nil and nil with no type, or when no rule answers one variance
  per point (a variance for some other series would weigh samples by another
  sample's noise).

  ONE RULE FOR THE MODEL, and a rule that measures beats one that weighs as
  counts: saying the data are not counts is a claim about all the data the
  model is fitted to, while counts are what a peak or a background type says of
  any data it is given - a background under a price is still a price (see
  fit_advice.StatisticsWeightingFor). Among rules of one kind the first type's
  decides. Here, beside the capability it reads, so the client - which draws
  where the difference is not noise - and the server ask the same function. }
function ModelResidualNoise(const ATypes: array of TNamedPointsSetClass;
    const AY: array of double; out ARule: TNoiseRuleClass): TNoiseVariances;
    overload;
{ The same, without saying which rule measured it. }
function ModelResidualNoise(const ATypes: array of TNamedPointsSetClass;
    const AY: array of double): TNoiseVariances; overload;

implementation

uses
    non_configurable_points_set;

{============================ TNamedPointsSet =================================}

function ModelResidualNoise(const ATypes: array of TNamedPointsSetClass;
    const AY: array of double; out ARule: TNoiseRuleClass): TNoiseVariances;
var
    Pass, i: longint;
    Rule: TNoiseRuleClass;
begin
    Result := nil;
    ARule := nil;
    //  First the rules that measure, then those that weigh as counts.
    for Pass := 0 to 1 do
        for i := 0 to High(ATypes) do
        begin
            Rule := ATypes[i].NoiseRule;
            if not Assigned(Rule) or (Rule.WeighsFiguresAsCounts <> (Pass = 1)) then
                Continue;
            Result := Rule.Variances(AY);
            if Length(Result) = Length(AY) then
            begin
                ARule := Rule;
                Exit;
            end;
            Result := nil;
        end;
end;

function ModelResidualNoise(const ATypes: array of TNamedPointsSetClass;
    const AY: array of double): TNoiseVariances;
var
    Rule: TNoiseRuleClass;
begin
    Result := ModelResidualNoise(ATypes, AY, Rule);
end;

procedure CheckNoiseRule(AClass: TNamedPointsSetClass; const AWhen: string);
begin
    if (AClass <> TNamedPointsSet) and not Assigned(AClass.NoiseRule) then
        raise ECurveTypeWithoutNoiseRule.CreateFmt('%s: %s names no noise ' +
            'rule. Every curve type names the rule its field''s noise is ' +
            'measured by (TNamedPointsSet.NoiseRule; see noise_rule).',
            [AWhen, AClass.ClassName]);
end;

procedure TNamedPointsSet.AfterConstruction;
begin
    inherited AfterConstruction;
    CheckNoiseRule(TNamedPointsSetClass(ClassType), 'Not made');
end;

procedure TNamedPointsSet.SetCurveTypeName(Name: string);
begin
    FName := Name;
end;

function TNamedPointsSet.GetCurveExpression: string;
begin
    //  No closed-form expression by default; analytic curves override this.
    Result := '';
end;

class function TNamedPointsSet.IsAnalytic: boolean;
begin
    //  Analytic by default: every curve type this framework shipped with had a
    //  formula, so this keeps their behaviour unchanged.
    Result := True;
end;

class function TNamedPointsSet.GetCurveTypeGroup: string;
begin
    //  Ungrouped by default, so existing curve types are unaffected.
    Result := '';
end;

class function TNamedPointsSet.AmplitudeIsUnbounded: boolean;
begin
    //  Bounded by default: every peak type is seeded from the data it sits on.
    Result := False;
end;

class function TNamedPointsSet.PlacedByPointSet: string;
begin
    //  Placed from a single curve position by default, which is what every peak
    //  type does - so adding this capability changes nothing for them.
    Result := '';
end;

class function TNamedPointsSet.IsBackground: boolean;
begin
    Result := False;
end;

class function TNamedPointsSet.IsPlacedOnlyByItsModule: boolean;
begin
    Result := False;
end;

class function TNamedPointsSet.AcceptsBackground: boolean;
begin
    Result := True;
end;

class function TNamedPointsSet.IsDefaultBackground: boolean;
begin
    Result := False;
end;

class function TNamedPointsSet.IsDefinedByFormula: boolean;
begin
    Result := False;
end;

procedure TNamedPointsSet.ApplyFormula(const AExpression: string;
    AParams: Curve_parameters);
begin
    AParams.Free;
end;

function TNamedPointsSet.RemovableInReduction: boolean;
begin
    Result := True;
end;

function TNamedPointsSet.DescribesOnlyNoise(ANoiseSigma: double): boolean;
begin
    Result := False;
end;

class function TNamedPointsSet.NoiseRule: TNoiseRuleClass;
begin
    //  None: the framework knows no field's noise. Every curve type names its
    //  own, and CheckNoiseRule refuses one that does not.
    Result := nil;
end;

function TNamedPointsSet.DrawnBaselineIn(
    AModel: TSelfCopiedCompList): TDoubleDynArray;
begin
    //  Every type this framework has rests on nothing.
    Result := nil;
end;

class function TNamedPointsSet.ValuesAreNonNegative: boolean;
begin
    Result := False;
end;

function TNamedPointsSet.ZeroRoundingBelowZero: boolean;
const
    { A lowest point this close to zero, relative to the curve's own size, is
      the rounding of a curve that already touches zero. }
    ROUNDING = 1e-12;
var
    i: longint;
    Lowest, Largest: double;
begin
    Lowest := 0;
    Largest := 0;
    for i := 0 to PointsCount - 1 do
    begin
        if PointYCoord[i] < Lowest then
            Lowest := PointYCoord[i];
        if Abs(PointYCoord[i]) > Largest then
            Largest := Abs(PointYCoord[i]);
    end;
    Result := -Lowest <= ROUNDING * Largest;
    if Result then
        for i := 0 to PointsCount - 1 do
            if PointYCoord[i] < 0 then
                PointYCoord[i] := 0;
end;

function TNamedPointsSet.KeepsAboveZero: boolean;
begin
    Result := ValuesAreNonNegative and FWindowDataNonNegative;
end;

procedure TNamedPointsSet.KeepValuesInRange;
var
    i: longint;
    Lowest: double;

    function LowestValue: double;
    var
        j: longint;
    begin
        Result := PointYCoord[0];
        for j := 1 to PointsCount - 1 do
            if PointYCoord[j] < Result then
                Result := PointYCoord[j];
    end;

begin
    if (not KeepsAboveZero) or (PointsCount = 0) then
        Exit;
    //  A FIXED POINT, not a creep. A curve lifted once is rebuilt on every
    //  edit and every open from the level that carries the lift, and the
    //  polynomial evaluated afresh lands its lowest point a rounding error
    //  either side of zero. Lifting by that moved the level in the last place
    //  each time, and a project saved and reopened was not the project saved.
    //  Such a point is set to zero where it is, and the parameters are left
    //  alone; the guarantee - nothing below zero - is kept exactly either way.
    if ZeroRoundingBelowZero then
        Exit;
    Lowest := LowestValue;
    //  NaN compares false, so a curve the formula could not evaluate is left
    //  as it is rather than lifted by a NaN.
    if not (Lowest < 0) then
        Exit;
    if AbsorbLift(-Lowest) then
    begin
        //  THE POINTS ARE WHAT THE PARAMETERS GIVE, bit for bit, and not "raw
        //  plus the lift": a project is saved as parameters and reopened by
        //  evaluating them, and the two differ by a rounding error that the
        //  model-wide scaling factor then carries into every curve's integral.
        DoCalc;
        if ZeroRoundingBelowZero then
            Exit;
        //  THE GUARANTEE DOES NOT REST ON THE TYPE: a level that carried less
        //  than it was handed leaves the rest to be drawn, as a type with no
        //  level draws all of it.
        Lowest := LowestValue;
    end;
    for i := 0 to PointsCount - 1 do
        PointYCoord[i] := PointYCoord[i] - Lowest;
end;

function TNamedPointsSet.AbsorbLift(const ADelta: double): boolean;
begin
    Result := LevelParameterName <> '';
    if Result then
        ValuesByName[LevelParameterName] := ValuesByName[LevelParameterName] + ADelta;
end;

function TNamedPointsSet.LevelParameterName: string;
begin
    Result := '';
end;

class function TNamedPointsSet.ArgumentMustBePositive: boolean;
begin
    Result := False;
end;

{$hints off}
procedure TNamedPointsSet.SeedFromBaseline(const AX, AY: array of double);
begin
    //  A peak is seeded from its pick, not from a baseline.
end;

class function TNamedPointsSet.PreferredAxisMode(ADimension: TAxisDimension): string;
begin
    Result := '';
end;

class function TNamedPointsSet.FallbackAxisMode(ADimension: TAxisDimension): string;
begin
    Result := '';
end;
{$hints on}

class function TNamedPointsSet.Explanation: TExplanation;
begin
    //  Nothing to say by default - see the declaration for why that is the
    //  right way to fail.
    Result := Default(TExplanation);
end;

class function TNamedPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TNonConfigurablePointsSet;
end;

end.
