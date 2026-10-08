// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What each objective declares, and the two refusals that protect a fit.)

The arithmetic itself is covered against real data by testcase_loss_real_data.
What is asserted here is the part that became a declaration rather than a branch:
which objectives exist, what they claim about themselves, and - the two that
matter - that an unknown objective is refused instead of quietly reading as a
perfect fit, and that one which cannot be pooled says so instead of pooling
wrongly.
}
unit testcase_loss_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_loss;

type
    TLossRegistryTest = class(TTestCase)
    published
        procedure EveryBuiltInObjectiveIsRegistered;
        procedure AnUnknownKindIsNotAnObjective;
        procedure EvaluatingAnUnknownKindRaisesRatherThanReadingAsAPerfectFit;
        procedure AnObjectiveThatCannotBePooledSaysSo;
        procedure OnlyTheLegacyFormIsSelfNormalising;
        procedure OnlyTheSquaredFormsAreLeastSquares;
        procedure TwoObjectivesCannotShareAKind;
        procedure EveryRegisteredObjectiveCanBeNamedAndEvaluated;
    end;

implementation

function EvaluateNothing(const ATerms: TLossTerms): double;
begin
    Result := 0;
end;

procedure TLossRegistryTest.EveryBuiltInObjectiveIsRegistered;
var
    Info: TLossInfo;
begin
    AssertTrue('R-factor', FindLoss(LOSS_KIND_RFACTOR, Info));
    AssertTrue('R-factor (legacy)', FindLoss(LOSS_KIND_RFACTOR_LEGACY, Info));
    AssertTrue('Sum of squares', FindLoss(LOSS_KIND_SUMSQ, Info));
    AssertTrue('Relative deviation', FindLoss(LOSS_KIND_RELATIVE, Info));
    //  The default must come first: registration order is menu order.
    AssertEquals('the default objective is offered first',
        LOSS_KIND_RFACTOR, RegisteredLosses[0].Kind);
end;

procedure TLossRegistryTest.AnUnknownKindIsNotAnObjective;
begin
    //  Was a range check, which accepts any gap between two real objectives and
    //  rejects anything contributed from outside this unit.
    AssertFalse('an id nothing registered is not a loss', IsKnownLoss(9999));
    AssertFalse('nor is a negative one', IsKnownLoss(-1));
    AssertEquals('and it is named as unknown rather than blank',
        'Unknown', LossName(9999));
end;

procedure TLossRegistryTest.EvaluatingAnUnknownKindRaisesRatherThanReadingAsAPerfectFit;
var
    Calc, Obs: TLossDoubleArray;
    Raised: boolean;
begin
    Calc := TLossDoubleArray.Create(1, 2, 3);
    Obs := TLossDoubleArray.Create(1, 2, 4);
    Raised := False;
    try
        EvaluateLoss(9999, Calc, Obs, 1);
    except
        on E: Exception do
            Raised := True;
    end;
    //  Zero is what a silent default would give, and zero error is a perfect
    //  fit - the most dangerous possible answer for a fitting tool (D26).
    AssertTrue('an unknown objective must be refused, not scored 0', Raised);
end;

procedure TLossRegistryTest.AnObjectiveThatCannotBePooledSaysSo;
var
    Parts: TLossParts;
    Raised: boolean;
begin
    Parts := Default(TLossParts);
    Parts.SumSq := 1;
    Parts.SumObs := 10;
    Raised := False;
    try
        LossFromParts(LOSS_KIND_RFACTOR_LEGACY, Parts);
    except
        on E: Exception do
            Raised := True;
    end;
    //  The legacy form divides by the MODEL's integral, which is not a property
    //  of the data, so parts from two intervals cannot be added and divided
    //  once. Declared as "no pooler" rather than left to a case statement's
    //  else branch.
    AssertTrue('the model-normalised form cannot be pooled', Raised);

    //  The others can, and still do.
    AssertEquals('sum of squares pools by addition',
        1.0, LossFromParts(LOSS_KIND_SUMSQ, Parts), 1e-12);
end;

procedure TLossRegistryTest.OnlyTheLegacyFormIsSelfNormalising;
begin
    //  This is the fact the loss/curve compatibility rule is derived from, so it
    //  decides which objectives an unbounded-amplitude model may use at all.
    AssertTrue('the legacy R-factor divides by the model',
        LossIsSelfNormalising(LOSS_KIND_RFACTOR_LEGACY));
    AssertFalse('the corrected one divides by the data',
        LossIsSelfNormalising(LOSS_KIND_RFACTOR));
    AssertFalse('sum of squares divides by nothing',
        LossIsSelfNormalising(LOSS_KIND_SUMSQ));
    AssertFalse('relative deviation divides by the data',
        LossIsSelfNormalising(LOSS_KIND_RELATIVE));
end;

procedure TLossRegistryTest.OnlyTheSquaredFormsAreLeastSquares;
begin
    //  Decides which objectives can be handed to a solver that wants a residual
    //  VECTOR rather than a scalar.
    AssertTrue('R-factor differs from sum of squares by a constant',
        LossIsLeastSquares(LOSS_KIND_RFACTOR));
    AssertTrue('sum of squares plainly is',
        LossIsLeastSquares(LOSS_KIND_SUMSQ));
    AssertFalse('the legacy form minimises a model-dependent ratio',
        LossIsLeastSquares(LOSS_KIND_RFACTOR_LEGACY));
    AssertFalse('absolute deviations are an L1 problem',
        LossIsLeastSquares(LOSS_KIND_RELATIVE));
end;

procedure TLossRegistryTest.TwoObjectivesCannotShareAKind;
var
    Info: TLossInfo;
    Raised: boolean;
begin
    Info := Default(TLossInfo);
    Info.Kind := LOSS_KIND_RFACTOR;
    Info.Name := 'Impostor';
    Info.Evaluate := @EvaluateNothing;
    Raised := False;
    try
        RegisterLoss(Info);
    except
        on E: ELossRegistration do
            Raised := True;
    end;
    //  A settings file names an objective by number; two answering to one number
    //  means the fit is not the one the user selected.
    AssertTrue('a second claim on a kind must be refused', Raised);
end;

procedure TLossRegistryTest.EveryRegisteredObjectiveCanBeNamedAndEvaluated;
var
    Losses: TLossInfoArray;
    Calc, Obs: TLossDoubleArray;
    i: longint;
begin
    Calc := TLossDoubleArray.Create(1, 2, 3);
    Obs := TLossDoubleArray.Create(1, 2, 4);
    Losses := RegisteredLosses;
    AssertTrue('the build must offer objectives', Length(Losses) > 0);
    for i := 0 to High(Losses) do
    begin
        //  Walks the whole registry, so an objective added later cannot arrive
        //  unnamed, undescribed, or unevaluable - it would then appear in the
        //  menu as a blank row that fails when chosen.
        AssertTrue('every objective has a name', Losses[i].Name <> '');
        AssertTrue(Losses[i].Name + ' has a description',
            Losses[i].Description <> '');
        AssertTrue(Losses[i].Name + ' can be evaluated',
            EvaluateLoss(Losses[i].Kind, Calc, Obs, 1) >= 0);
    end;
end;

initialization
    RegisterTest('unit', TLossRegistryTest);
end.
