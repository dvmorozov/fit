// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The noise of the model the client holds, as its curve types measure
it.)

WHY THE CLIENT ASKS THE TYPES ITSELF. The window draws where the difference is
not noise (difference_structure) from the samples it already holds, and the
noise it measures that against must be the server's: the variance the noise
rule of the model's curve types measures (TNamedPointsSet.NoiseRule, through
named_points_set.ModelResidualNoise - the function the server's figures, its
reduction's BIC and its verdict ask). Each curve's type is what
GET /curves states (curveType), and the class is the registered one, so no
array per sample is added to any reply: the noise is a property of the types
and of the data, and the client holds both. A curve whose type is not stated -
a server that predates the field - or not registered here takes no part.

WITH NO CURVE STATED - the first fit of picks, whose curves the server builds
as the run starts, animated before the window has read any - the type the
window has selected is the one they are built of, and its rule is asked. Only
with neither is there no noise: nil, and the window judges nothing.

Kept out of the client class so a test calls it.
}
unit model_noise;

{$mode objfpc}{$H+}

interface

uses
    mscr_specimen_list, fit_reduction, named_points_set;

{ The variance of the noise at each point of AY - the whole profile - by the
  noise rule of the curve types AAttrs states, or, when it states none this
  build knows, of ASelectedType - the type the window has selected; nil when
  neither is known. AAttrs may be nil. }
function ModelNoiseOfAttributes(AAttrs: TMSCRCurveList;
    const AY: array of double;
    const ASelectedType: TCurveTypeId): TNoiseVariances;

implementation

uses
    SysUtils, persistent_curve_parameters, int_curve_factory,
    curve_types_singleton;

function ModelNoiseOfAttributes(AAttrs: TMSCRCurveList;
    const AY: array of double;
    const ASelectedType: TCurveTypeId): TNoiseVariances;
var
    Types: array of TNamedPointsSetClass;
    Cls: TCurveClass;
    i: longint;
begin
    Types := nil;
    if Assigned(AAttrs) then
        for i := 0 to AAttrs.Count - 1 do
        begin
            if not (AAttrs.Items[i] is Curve_parameters) or IsEqualGUID(
                Curve_parameters(AAttrs.Items[i]).FCurveTypeId, GUID_NULL) then
                Continue;
            Cls := FindCurveClassById(Curve_parameters(AAttrs.Items[i]).FCurveTypeId);
            if not Assigned(Cls) then
                Continue;
            SetLength(Types, Length(Types) + 1);
            Types[High(Types)] := Cls;
        end;
    if Length(Types) = 0 then
    begin
        Cls := nil;
        if not IsEqualGUID(ASelectedType, GUID_NULL) then
            Cls := FindCurveClassById(ASelectedType);
        if Assigned(Cls) then
        begin
            SetLength(Types, 1);
            Types[0] := Cls;
        end;
    end;
    Result := ModelResidualNoise(Types, AY);
end;

end.
