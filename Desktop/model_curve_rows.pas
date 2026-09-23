// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The framework's rows of the Model panel, read off the curves the
client holds.)

OUT OF THE WINDOW, so the rows can be built by a test the way the window builds
them. They lived in TFormMain.RefreshModelStructure, which no test can reach;
and the one test that has to build them is a module's end-to-end one - a pack
whose tree is composed with these rows in the window, and which can only prove
that composition by building both halves the way the application does. A copy of
this loop in that test would be exactly the second implementation that drifts.

WHAT IS READ, AND FROM WHERE. The curves and their attributes are two lists the
client keeps, PAIRED BY INDEX - the pairing the wire itself uses. The handle and
the position come from the attributes; the title, and through it the curve's
type, from the curve.
}
unit model_curve_rows;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, self_copied_component, mscr_specimen_list, model_outline;

{ One row per curve of ACurves, in their order. Either list may be nil. }
function ModelCurveRowsOf(ACurves: TSelfCopiedCompList;
    AAttrs: TMSCRCurveList): TModelCurveRows;

implementation

uses
    curve_points_set, persistent_curve_parameters, special_curve_parameter,
    parameter_roles, curve_type_explanations, curve_instance_id;

function ModelCurveRowsOf(ACurves: TSelfCopiedCompList;
    AAttrs: TMSCRCurveList): TModelCurveRows;
var
    i, N: longint;
    Curve: TCurvePointsSet;
    Params: Curve_parameters;
    Position: TSpecialCurveParameter;
begin
    Result := nil;
    N := 0;
    if not Assigned(ACurves) then
        Exit;
    for i := 0 to ACurves.Count - 1 do
    begin
        if not (ACurves.Items[i] is TCurvePointsSet) then
            Continue;
        Curve := TCurvePointsSet(ACurves.Items[i]);
        SetLength(Result, N + 1);
        Result[N].Title := Curve.FTitle;
        //  THE CURVE'S OWN TYPE, not the selected one: a model can hold
        //  several. Read back from the title the engine gives every curve,
        //  because the type itself is not on the wire (model_outline).
        Result[N].Topic := CurveTypeTopicForName(
            CurveTypeNameOfTitle(Curve.FTitle));
        //  PAIRED BY INDEX with the attributes, which is the pairing the
        //  wire itself uses. The handle lives on the attributes row.
        Result[N].InstanceId := '';
        Result[N].HasPosition := False;
        Result[N].Position := 0;
        if Assigned(AAttrs) and (i < AAttrs.Count) and
            (AAttrs.Items[i] is Curve_parameters) then
        begin
            Params := Curve_parameters(AAttrs.Items[i]);
            Result[N].InstanceId := CurveInstanceIdToWire(Params.FInstanceId);
            //  THE POSITION COMES FROM THE ATTRIBUTES TOO, and asking the
            //  plotted curve was the defect: this asked Curve.Hasx0, and a
            //  curve that arrived over the wire as a point set carries no
            //  parameters at all - so Hasx0 was false for every one of them
            //  and ten curves of one type all read "Asym. Pseudo-Voigt"
            //  with nothing to tell them apart. The position is the ONLY
            //  thing that does, which is what model_outline says it is for.
            //
            //  Through ParameterWithRole, which is where "which parameter
            //  places this curve" already lives - both position types, one
            //  role wearing two hats.
            Position := ParameterWithRole(Params, prPosition);
            if Assigned(Position) then
            begin
                Result[N].HasPosition := True;
                Result[N].Position := Position.Value;
            end;
        end;
        Inc(N);
    end;
end;

end.
