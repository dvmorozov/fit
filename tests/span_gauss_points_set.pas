// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A Gaussian with a compact support, for the tests that need one.)

The framework ships no compactly supported curve type, and a module's must not
be named here, so the tests that ask what a model leaves uncovered register this
one. It is a Gaussian that is EXACTLY zero outside [SpanFrom, SpanTo] - the
property a module's patterns have - and it inherits the Gaussian's explanation,
so the registry's completeness walk stays green.

THE SUPPORT IS FIXED, not measured from x0, so that each test states exactly
which samples it covers. It once had to be: a type built through
TFitTask.CreatePatternInstance's generic path had its position clamped to the
window's first sample, and a support that followed x0 moved under the test
(ATypeBuiltThroughTheGenericPathStaysWhereItWasPicked holds the fix).
}
unit span_gauss_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, gauss_points_set, named_points_set;

type
    TSpanGaussPointsSet = class(TGaussPointsSet)
    public
        { The one-argument constructor the engine builds a registered type
          with, building the Gaussian's parameters. }
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        function SupportMin: double; override;
        function SupportMax: double; override;
    public
        { Where every instance of the test type exists; each test sets its own. }
        class var SpanFrom, SpanTo: double;
    end;

implementation

uses
    curve_types_singleton;

const
    SpanGaussId: TGuid = '{3F0B5C61-8E2A-4B7D-A1C9-6D4E2F8B9A13}';

constructor TSpanGaussPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner, 0);
end;

class function TSpanGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian of compact support (test)';
end;

class function TSpanGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := SpanGaussId;
end;

function TSpanGaussPointsSet.SupportMin: double;
begin
    Result := SpanFrom;
end;

function TSpanGaussPointsSet.SupportMax: double;
begin
    Result := SpanTo;
end;

initialization
    TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
        TSpanGaussPointsSet);
end.
