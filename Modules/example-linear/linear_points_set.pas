// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A straight ramp over a bounded support - the example module's curve.)

WHAT IT IS. y = A + alpha*(x - x0) between x0-sigma and x0, and zero outside.
Four parameters - amplitude, position, width and a slope of its own invention -
so the example stays about the CONTRACT rather than about the mathematics.

WHY THIS SHAPE. It is deliberately not a peak: it is placed like one (from a
single curve position, the default), yet it is asymmetric and has a hard support
boundary, so it exercises the parts of the contract a Gaussian would not - a
closed-form expression whose text must agree with DoCalc, and an extremum mode
that is neither maxima-only nor minima-only.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit linear_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, points_set, named_points_set, curve_points_set,
    special_curve_parameter;

type
    { A straight ramp over a bounded support. }
    TLinearPointsSet = class(TNamedPointsSet)
    protected
        { Slope of the ramp. Held because DoCalc reads it on every point; the
          other three are reached through the inherited A/x0/Sigma. }
        FAlpha: TSpecialCurveParameter;

        procedure DoCalc; override;
        function LinePoint(AX: double): double;

    public
        constructor Create(AOwner: TComponent); override;

        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        function GetCurveExpression: string; override;
        class function GetCurveTypeGroup: string; override;
    end;

implementation

uses
    amplitude_curve_parameter, sigma_curve_parameter, user_curve_parameter,
    curve_types_singleton, int_curve_factory;

constructor TLinearPointsSet.Create(AOwner: TComponent);
var
    Parameter: TSpecialCurveParameter;
begin
    inherited;
    //  The three the framework already understands - it seeds them from the data
    //  and shows them in the parameters grid under these names.
    Parameter := TAmplitudeCurveParameter.Create;
    AddParameter(Parameter);

    //  The position, declared by ROLE rather than by class. The engine builds
    //  any registered type through the one-argument constructor and assigns x0
    //  afterwards, so a type must not require its position up front - and
    //  TPositionCurveParameter derives its variation boundaries from the data at
    //  CONSTRUCTION time, which on a curve that has no points yet pins it to
    //  whatever it was seeded with. The built-in peak types avoid that by taking
    //  x0 in their constructor; a module that wants the generic path declares the
    //  role instead, which is what VariablePosition means here.
    Parameter := TUserCurveParameter.Create;
    Parameter.Name := 'x0';
    Parameter.Type_ := VariablePosition;
    AddParameter(Parameter);

    Parameter := TSigmaCurveParameter.Create;
    AddParameter(Parameter);

    //  And one of this type's own invention. The framework knows nothing about
    //  'alpha' beyond its name: it appears in the grid, it is varied by the fit,
    //  and it is sent to a formula-based backend because the expression below
    //  names it. TUserCurveParameter is the general-purpose class for a
    //  parameter with no special meaning - nothing about it is specific to
    //  user-entered formulas.
    FAlpha := TUserCurveParameter.Create;
    FAlpha.Name := 'alpha';
    FAlpha.Value := 20.0;
    FAlpha.Type_ := Variable;
    AddParameter(FAlpha);

    InitListOfVariableParameters;
end;

class function TLinearPointsSet.GetCurveTypeName: string;
begin
    Result := 'Linear ramp';
end;

class function TLinearPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    //  Its own identity, and never a framework type's: the factory is keyed by
    //  this, so a collision would silently substitute one type for another.
    Result := StringToGUID('{94828867-50f4-49b0-9a5b-6d9db5ae3074}');
end;

class function TLinearPointsSet.GetExtremumMode: TExtremumMode;
begin
    //  Automatic placement may seed this type on either kind of feature: a ramp
    //  rising to a shoulder and one falling from it are the same curve.
    Result := MaximumsAndMinimums;
end;

class function TLinearPointsSet.GetCurveTypeGroup: string;
begin
    //  A group of its own, so the example is visibly a module's contribution in
    //  the Curve Type menu rather than something the framework ships.
    Result := 'Example';
end;

function TLinearPointsSet.GetCurveExpression: string;
begin
    //  What lets the formula-based backends (the Python sidecar, a remote
    //  compute server) fit this type without knowing it exists: they evaluate
    //  this text instead of re-implementing DoCalc.
    //
    //  IT MUST AGREE WITH DoCalc, including the support boundary - a formula
    //  that merely resembles the code fits a different curve on one backend
    //  than on another, which is far harder to notice than an outright error.
    //  A test asserts the two agree point for point.
    Result := 'where((x <= x0) & (x >= x0 - sigma), A + alpha*(x - x0), 0)';
end;

function TLinearPointsSet.LinePoint(AX: double): double;
begin
    //  Zero outside the support, so that several ramps sum into a profile
    //  without each one contributing everywhere.
    if (AX > x0) or (AX < x0 - Sigma) then
        Result := 0
    else
        Result := A + FAlpha.Value * (AX - x0);
end;

procedure TLinearPointsSet.DoCalc;
var
    j: longint;
begin
    //  Everything, and cleared first: points outside the support must be
    //  zero rather than whatever the previous parameters left there.
    for j := 0 to PointsCount - 1 do
        FPoints[j][2] := LinePoint(FPoints[j][1]);
end;

var
    CTS: ICurveFactory;

initialization
    //  Self-registration, exactly as the framework's own types do it - and the
    //  reason a module's front door must NAME this unit: a unit nothing
    //  references is not linked, so this section never runs and the type is
    //  absent with nothing to say so.
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TLinearPointsSet);
end.
