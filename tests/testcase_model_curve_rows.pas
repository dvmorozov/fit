// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The framework's Model panel rows, read off the curves the client
holds.)

WHY THESE TESTS EXIST. The rows were built inside the window, where nothing could
test them, and they are half of what the panel composes: a module's tree is
matched against them by the curve each row stands for. So the handle has to come
from the attributes row paired with the curve by index, and the position from the
parameter holding the position role - the two readings that have each been wrong
before.

EXTRACTED, NOT NEW. This loop was moved unchanged out of
TFormMain.RefreshModelStructure, so these tests pass on their first run. They
were not first run against the old code, which no test could reach.
}
unit testcase_model_curve_rows;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    model_curve_rows, model_outline, self_copied_component, mscr_specimen_list,
    gauss_points_set, persistent_curve_parameters,
    persistent_curve_parameter_container, amplitude_curve_parameter,
    special_curve_parameter, curve_instance_id, curve_type_explanations;

type
    TModelCurveRowsTest = class(TTestCase)
    private
        FCurves: TSelfCopiedCompList;
        FAttrs: TMSCRCurveList;
        { One curve titled ATitle, and its attributes row carrying AId and,
          when AHasPosition, a parameter holding the position role. }
        procedure AddCurve(const ATitle: string; const AId: TCurveInstanceId;
            AHasPosition: boolean; APosition: double);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure OneRowPerCurveInTheModelsOrder;
        procedure ARowCarriesTheHandleOfTheAttributesRowPairedWithIt;
        procedure ARowCarriesThePositionOfThePositionParameter;
        procedure ACurveWithNoPositionParameterHasNoPosition;
        procedure ACurveWithNoAttributesRowHasNoHandle;
        procedure ARowExplainsItsOwnCurvesType;
        procedure NoCurvesNoRows;
    end;

implementation

procedure TModelCurveRowsTest.SetUp;
begin
    FCurves := TSelfCopiedCompList.Create(True);
    FAttrs := TMSCRCurveList.Create;
end;

procedure TModelCurveRowsTest.TearDown;
begin
    FreeAndNil(FAttrs);
    FreeAndNil(FCurves);
end;

procedure TModelCurveRowsTest.AddCurve(const ATitle: string;
    const AId: TCurveInstanceId; AHasPosition: boolean; APosition: double);
var
    C: TGaussPointsSet;
    Params: Curve_parameters;
    P: TSpecialCurveParameter;
begin
    C := TGaussPointsSet.Create(nil, APosition);
    C.FTitle := ATitle;
    FCurves.Add(C);

    Params := Curve_parameters(FAttrs.CreateNewObject);
    Params.FInstanceId := AId;
    //  An amplitude first, so the position is found by its role and not by
    //  being the only parameter there is.
    P := TAmplitudeCurveParameter.Create;
    P.Name := 'A';
    P.Type_ := Variable;
    P.Value := 7;
    TPersistentCurveParameterContainer(Params.Params.Add).Parameter := P;
    if AHasPosition then
    begin
        P := TAmplitudeCurveParameter.Create;
        P.Name := 'x0';
        P.Type_ := InvariablePosition;
        P.Value := APosition;
        TPersistentCurveParameterContainer(Params.Params.Add).Parameter := P;
    end;
    FAttrs.Add(Params);
end;

procedure TModelCurveRowsTest.OneRowPerCurveInTheModelsOrder;
var
    Rows: TModelCurveRows;
begin
    AddCurve('Gaussian [1]', NewCurveInstanceId, True, 10);
    AddCurve('Gaussian [2]', NewCurveInstanceId, True, 20);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertEquals(2, Length(Rows));
    AssertEquals('Gaussian [1]', Rows[0].Title);
    AssertEquals('Gaussian [2]', Rows[1].Title);
end;

procedure TModelCurveRowsTest.ARowCarriesTheHandleOfTheAttributesRowPairedWithIt;
var
    A, B: TCurveInstanceId;
    Rows: TModelCurveRows;
begin
    //  Two, so a handle read off the wrong row cannot pass.
    A := NewCurveInstanceId;
    B := NewCurveInstanceId;
    AddCurve('Gaussian [1]', A, True, 10);
    AddCurve('Gaussian [2]', B, True, 20);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertEquals(CurveInstanceIdToWire(A), Rows[0].InstanceId);
    AssertEquals(CurveInstanceIdToWire(B), Rows[1].InstanceId);
end;

procedure TModelCurveRowsTest.ARowCarriesThePositionOfThePositionParameter;
var
    Rows: TModelCurveRows;
begin
    AddCurve('Gaussian [1]', NewCurveInstanceId, True, 23.5);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertTrue(Rows[0].HasPosition);
    AssertEquals(23.5, Rows[0].Position, 1e-12);
end;

procedure TModelCurveRowsTest.ACurveWithNoPositionParameterHasNoPosition;
var
    Rows: TModelCurveRows;
begin
    AddCurve('Gaussian [1]', NewCurveInstanceId, False, 23.5);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertFalse(Rows[0].HasPosition);
end;

procedure TModelCurveRowsTest.ACurveWithNoAttributesRowHasNoHandle;
var
    Rows: TModelCurveRows;
    C: TGaussPointsSet;
begin
    //  A curve the attributes have not caught up with yet: listed, and naming
    //  no curve, so no command on one curve can act on it by mistake.
    C := TGaussPointsSet.Create(nil, 5);
    C.FTitle := 'Gaussian [1]';
    FCurves.Add(C);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertEquals(1, Length(Rows));
    AssertEquals('', Rows[0].InstanceId);
    AssertFalse(Rows[0].HasPosition);
end;

procedure TModelCurveRowsTest.ARowExplainsItsOwnCurvesType;
var
    Rows: TModelCurveRows;
begin
    AddCurve('Gaussian [1]', NewCurveInstanceId, True, 10);
    Rows := ModelCurveRowsOf(FCurves, FAttrs);
    AssertEquals(CurveTypeTopicForName('Gaussian'), Rows[0].Topic);
end;

procedure TModelCurveRowsTest.NoCurvesNoRows;
begin
    AssertEquals('an empty model', 0, Length(ModelCurveRowsOf(FCurves, FAttrs)));
    AssertEquals('no list at all', 0, Length(ModelCurveRowsOf(nil, nil)));
end;

initialization
    //  Plain objects in, records out: nothing crosses a process boundary.
    RegisterTest('unit', TModelCurveRowsTest);
end.
