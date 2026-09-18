// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which parameter of a user-defined curve holds which role.)

A USER-DEFINED CURVE IS A FORMULA AND A SET OF NAMES, and the engine needs to
know which of those names means what: which is the abscissa, which places the
curve, which is its height, which is its width. Those are ROLES, and the rule is
that each is held by at most one parameter - the engine seeds the amplitude from
the data peak and the width from the fitting interval, and it cannot do either
for two parameters at once.

WHY IT WAS WORTH TAKING OUT. The rule lived in four combo-box change handlers,
each of which walked the parameter list clearing the role it was about to give
away and then gave it. Four copies of one invariant, in an LCL dialog, and
nothing anywhere asserted that a curve type ends up with one amplitude rather
than two or none. A curve type with two amplitudes is one the fit seeds twice
from the same peak; with none, it is one whose height is never estimated and
whose fit starts from a default that has nothing to do with the data.

FREEING A ROLE RETURNS THE PARAMETER TO Variable, which is what the four handlers
did and is the only sensible answer: a parameter that was the amplitude and is no
longer is still a parameter the fit varies.
}
unit parameter_roles;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, special_curve_parameter, persistent_curve_parameters;

type
    { The roles a parameter of a user-defined curve can be given. A subset of
      TParameterType: the others describe how a parameter behaves rather than
      what it stands for. }
    TParameterRole = (
        prArgument,   //  the abscissa the formula is written in
        prPosition,   //  where the curve sits
        prAmplitude,  //  its height, seeded from the data peak
        prWidth       //  its width, seeded from the fitting interval
        );

type
    { ONE ROW OF A LIST THE PROPERTIES DIALOG SHOWS - a combo item or a
      check-list entry - as a fact about the parameters rather than as a widget.

      `Parameter` is nil for the "(none)" row, which is what a role combo offers
      when the role may be left unheld. `Marked` is "this is the one": selected,
      for a combo; ticked, for the check-list. }
    TParameterChoice = record
        Name: string;
        Parameter: TSpecialCurveParameter;
        Marked: boolean;
    end;

    TParameterChoices = array of TParameterChoice;

const
    { The row that stands for no parameter. }
    NoneChoiceCaption = '(none)';

{ The parameter type a role is expressed as. }
function RoleType(ARole: TParameterRole): TParameterType;

{ True when this parameter is free to take ARole.

  The argument and the position are not candidates for the amplitude or the
  width: a name cannot be two things at once, and offering it would let the user
  make it so. A parameter already holding the role IS a candidate - otherwise
  the combo could not show what is currently selected. }
function CanHoldRole(AParameter: TSpecialCurveParameter;
    ARole: TParameterRole): boolean;

{ The parameter holding ARole, or nil when none does. }
function ParameterWithRole(AParams: Curve_parameters;
    ARole: TParameterRole): TSpecialCurveParameter;

{ How many parameters hold ARole. Should never exceed one; it is a function so
  that a test can say so. }
function CountWithRole(AParams: Curve_parameters;
    ARole: TParameterRole): longint;

{ Gives ARole to AParameter, taking it from whoever held it. Passing nil clears
  the role and gives it to nobody, which is what the combo's "(none)" means. }
procedure AssignRole(AParams: Curve_parameters; ARole: TParameterRole;
    AParameter: TSpecialCurveParameter);

{ What ticking or unticking a parameter's "fixed" box makes it.

  POSITION IS ITS OWN PAIR. Every other parameter goes between Variable and
  Shared - shared meaning held to one value across the curves of an interval -
  but a position has two types of its own, and folding it into the general case
  would turn a fixed position into a shared one and let the fit move it. }
function TypeAfterFixing(AType: TParameterType;
    AFixed: boolean): TParameterType;

{ WHICH PARAMETERS EACH LIST IN THE PROPERTIES DIALOG OFFERS. Four filters, each
  of them a rule about what a parameter may stand for, and each of them written
  out inside an LCL Fill* method where nothing could reach it.

  The abscissa combo offers every parameter: the formula decides what it is
  written in. }
function ArgumentChoices(AParams: Curve_parameters): TParameterChoices;

{ The position combo offers the parameters that describe the curve rather than
  its shape - and NOT, note, everything CanHoldRole(prPosition) admits: that
  says only "not the abscissa", so it would also offer a parameter already
  holding the amplitude or the width. The narrower list is what the dialog has
  always shown; see findings.md. }
function PositionChoices(AParams: Curve_parameters): TParameterChoices;

{ An amplitude or width combo: "(none)" and then every free parameter, marking
  the one that holds the role now. }
function RoleChoices(AParams: Curve_parameters;
    ARole: TParameterRole): TParameterChoices;

{ The "fixed" check-list: the same parameters the position combo offers, marked
  where the parameter is held fixed. The abscissa is absent - it is not a
  quantity the fit could vary in the first place. }
function FixedChoices(AParams: Curve_parameters): TParameterChoices;

{ The index of the marked row, or -1 when none is. A combo whose ItemIndex is
  set from this shows what is actually selected; -1 shows nothing, which is the
  honest answer when nothing holds the role. }
function MarkedIndex(const AChoices: TParameterChoices): longint;

implementation

function RoleType(ARole: TParameterRole): TParameterType;
begin
    case ARole of
        prArgument: Result := Argument;
        prPosition: Result := InvariablePosition;
        prAmplitude: Result := Amplitude;
        else
            Result := special_curve_parameter.Width;
    end;
end;

function CanHoldRole(AParameter: TSpecialCurveParameter;
    ARole: TParameterRole): boolean;
begin
    Result := False;
    if not Assigned(AParameter) then
        Exit;
    case ARole of
        prAmplitude, prWidth:
            Result := (AParameter.Type_ = Variable) or
                (AParameter.Type_ = Amplitude) or
                (AParameter.Type_ = special_curve_parameter.Width);
        prPosition:
            //  A position may be taken from any parameter that is not the
            //  abscissa: the formula's variable cannot also be where the curve
            //  sits.
            Result := AParameter.Type_ <> Argument;
        prArgument:
            //  Any of them; the formula decides what it is written in, and
            //  changing the abscissa re-frees whatever held it.
            Result := True;
    end;
end;

{ Returns every parameter holding ARole to an ordinary varied one. }
procedure ClearRole(AParams: Curve_parameters; ARole: TParameterRole);
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if ARole = prPosition then
        begin
            //  BOTH position types, because they are one role wearing two hats:
            //  fixed and varied. Clearing only the fixed one would leave a
            //  second parameter still placing the curve.
            if (P.Type_ = InvariablePosition) or
                (P.Type_ = VariablePosition) then
                P.Type_ := Variable;
        end
        else if P.Type_ = RoleType(ARole) then
            P.Type_ := Variable;
    end;
end;

function ParameterWithRole(AParams: Curve_parameters;
    ARole: TParameterRole): TSpecialCurveParameter;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    Result := nil;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if ARole = prPosition then
        begin
            if (P.Type_ = InvariablePosition) or
                (P.Type_ = VariablePosition) then
                Exit(P);
        end
        else if P.Type_ = RoleType(ARole) then
            Exit(P);
    end;
end;

function CountWithRole(AParams: Curve_parameters;
    ARole: TParameterRole): longint;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    Result := 0;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if ARole = prPosition then
        begin
            if (P.Type_ = InvariablePosition) or
                (P.Type_ = VariablePosition) then
                Inc(Result);
        end
        else if P.Type_ = RoleType(ARole) then
            Inc(Result);
    end;
end;

procedure AssignRole(AParams: Curve_parameters; ARole: TParameterRole;
    AParameter: TSpecialCurveParameter);
begin
    //  CLEARED FIRST, ALWAYS. Assigning without clearing is how a curve type
    //  ends up with two amplitudes, which the engine seeds twice from one peak.
    ClearRole(AParams, ARole);
    if Assigned(AParameter) then
        AParameter.Type_ := RoleType(ARole);
end;

{ Appends one row. }
procedure AddChoice(var AChoices: TParameterChoices; const AName: string;
    AParameter: TSpecialCurveParameter; AMarked: boolean);
var
    Last: longint;
begin
    Last := Length(AChoices);
    SetLength(AChoices, Last + 1);
    AChoices[Last].Name := AName;
    AChoices[Last].Parameter := AParameter;
    AChoices[Last].Marked := AMarked;
end;

{ True when this type describes where a curve sits or how strongly it is held,
  rather than what shape it has - the filter the position combo and the fixed
  check-list share. }
function IsPlaceableType(AType: TParameterType): boolean;
begin
    Result := (AType = Shared) or (AType = Variable) or
        (AType = InvariablePosition) or (AType = VariablePosition);
end;

function ArgumentChoices(AParams: Curve_parameters): TParameterChoices;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    Result := nil;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        AddChoice(Result, P.Name, P, P.Type_ = Argument);
    end;
end;

function PositionChoices(AParams: Curve_parameters): TParameterChoices;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    Result := nil;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if IsPlaceableType(P.Type_) then
            AddChoice(Result, P.Name, P,
                (P.Type_ = InvariablePosition) or (P.Type_ = VariablePosition));
    end;
end;

function RoleChoices(AParams: Curve_parameters;
    ARole: TParameterRole): TParameterChoices;
var
    i: longint;
    P: TSpecialCurveParameter;
    Held: boolean;
begin
    Result := nil;
    //  "(none)" FIRST AND ALWAYS PRESENT, even when a parameter holds the role:
    //  it is how the user takes the role away again, and a list without it
    //  would make the first assignment permanent.
    AddChoice(Result, NoneChoiceCaption, nil, False);
    Held := False;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if CanHoldRole(P, ARole) then
        begin
            AddChoice(Result, P.Name, P, P.Type_ = RoleType(ARole));
            if P.Type_ = RoleType(ARole) then
                Held := True;
        end;
    end;
    //  Nobody holds it, so "(none)" is what is selected.
    if not Held then
        Result[0].Marked := True;
end;

function FixedChoices(AParams: Curve_parameters): TParameterChoices;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    Result := nil;
    if not Assigned(AParams) then
        Exit;
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if not Assigned(P) then
            Continue;
        if IsPlaceableType(P.Type_) then
            //  TICKED means held fixed. The two fixed types are Shared - one
            //  value across an interval - and a position the fit may not move.
            AddChoice(Result, P.Name, P,
                (P.Type_ = Shared) or (P.Type_ = InvariablePosition));
    end;
end;

function MarkedIndex(const AChoices: TParameterChoices): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(AChoices) do
        if AChoices[i].Marked then
            Exit(i);
end;

function TypeAfterFixing(AType: TParameterType;
    AFixed: boolean): TParameterType;
begin
    if AFixed then
    begin
        if AType = VariablePosition then
            Result := InvariablePosition
        else
            Result := Shared;
    end
    else
    begin
        if AType = InvariablePosition then
            Result := VariablePosition
        else
            Result := Variable;
    end;
end;

end.
