// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Recording what a run reached and making a recorded model live again,
against a real engine.)

WHAT IS DRIVEN. A real TFitService, in process - the reason
testcase_project_session gives applies here word for word: a mock of IFitService
would prove that the getters and the setters were called, and the thing worth
knowing is that the model that comes back IS the model that was recorded. Nothing
crosses a process boundary, touches a file or runs the optimiser, so it is a unit
test.
}
unit testcase_model_history_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, title_points_set, gauss_points_set,
    fit_points_json, fit_project_document, fit_project_session,
    model_history, model_history_json, model_history_session;

type
    TModelHistorySessionTest = class(TTestCase)
    private
        FService: TFitService;
        FHistory: TModelHistory;
        { A peak, one interval, one pick under a known handle. }
        procedure GivenAModel(AHeight: double = 100);
        { The fit's result, as a fit would leave it: values, flagged fitted,
          measured. }
        procedure FittedTo(ASigma: double);
        function SigmaNow: double;
        function Record_(const AId: string): boolean;
        function MakeCurrent(const AId: string; out AFault: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARunIsRecordedAsTheModelItReached;
        procedure AndWithTheRFactorItMeasured;
        procedure ARunThatChangedNothingRecordsNothing;

        procedure MakingAnEntryCurrentPutsItsModelBack;
        procedure AndTheModelReportsItsRFactorAgain;
        procedure AndTheEntryIsCurrent;
        procedure AModelIsPutBackOnTheProfileItWasFittedTo;
        procedure EditsNoEntryHoldsAreKeptBeforeTheyAreReplaced;
        procedure ALiveModelAnEntryHoldsIsNotRecordedAgain;
        procedure AnEntryThatCannotBeRestoredIsRefusedInWords;
        procedure AnUnknownEntryIsRefusedInWords;

        procedure EveryIdIsNewAndNamesAPart;
    end;

implementation

const
    Handle = '{0A0A0A0A-1111-2222-3333-444444444444}';

{ Every entry AHistory holds, as Load takes them - so a test can load them back
  without a profile, which is what a damaged file looks like. Here rather than on
  TModelHistory: nothing in the program needs it (find-dead-code). }
function EntriesOf(AHistory: TModelHistory): THistoryEntries;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, AHistory.Count);
    for i := 0 to AHistory.Count - 1 do
        Result[i] := AHistory[i];
end;

procedure TModelHistorySessionTest.SetUp;
begin
    FService := TFitService.Create;
    FHistory := TModelHistory.Create;
end;

procedure TModelHistorySessionTest.TearDown;
begin
    FreeAndNil(FHistory);
    FreeAndNil(FService);
end;

procedure TModelHistorySessionTest.GivenAModel(AHeight: double);
var
    P, B, Picks: TTitlePointsSet;
    Ids: TCurveInstanceIdList;
    Svc: IFitService;
    i: longint;
begin
    Svc := FService;
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + AHeight * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    FService.SetRFactorBounds(B);
    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(10, 20);
    SetLength(Ids, 1);
    Ids[0] := Handle;
    FService.SetCurvePositions(Picks, Ids);
end;

procedure TModelHistorySessionTest.FittedTo(ASigma: double);
var
    Values: TCurveValuesList;
begin
    SetLength(Values, 1);
    Values[0].CurveIndex := 0;
    Values[0].Fitted := True;
    SetLength(Values[0].Params, 1);
    Values[0].Params[0].Name := 'sigma';
    Values[0].Params[0].Value := ASigma;
    FService.SetCurveValues(Values);
    FService.EvaluateModel;
end;

function TModelHistorySessionTest.SigmaNow: double;
var
    j: longint;
    Nm: string;
    V: double;
    T: longint;
begin
    Result := -1;
    for j := 0 to FService.GetCurveParameterCount(0) - 1 do
    begin
        FService.GetCurveParameter(0, j, Nm, V, T);
        if Nm = 'sigma' then
            Exit(V);
    end;
end;

function TModelHistorySessionTest.Record_(const AId: string): boolean;
begin
    Result := RecordRun(FService, EmptyProjectClientContext, FHistory,
        hrkMinimizeDifference, False, EncodeDate(2026, 10, 2), AId);
end;

function TModelHistorySessionTest.MakeCurrent(const AId: string;
    out AFault: string): boolean;
begin
    Result := MakeHistoryEntryCurrent(FService, EmptyProjectClientContext,
        FHistory, AId, EncodeDate(2026, 10, 2), 'kept', AFault);
end;

procedure TModelHistorySessionTest.ARunIsRecordedAsTheModelItReached;
var
    Doc: TProjectDocument;
    Sigma: double;
    j: longint;
begin
    GivenAModel;
    FittedTo(2.4);
    AssertTrue('recorded', Record_('a'));
    AssertEquals('one entry', 1, FHistory.Count);
    AssertTrue('restorable', FHistory.ModelToRestore(0, Doc));
    //  BY NAME: a parameter's position in the list is not what it is.
    Sigma := -1;
    for j := 0 to High(Doc.Curves[0].Params) do
        if Doc.Curves[0].Params[j].Name = 'sigma' then
            Sigma := Doc.Curves[0].Params[j].Value;
    AssertEquals('the value the run reached', 2.4, Sigma, 1e-12);
    //  The handle, however the engine spells it (it drops the braces).
    AssertTrue('under its handle: ' + Doc.Curves[0].Id,
        Pos('0a0a0a0a-1111', LowerCase(Doc.Curves[0].Id)) > 0);
    AssertEquals('on the profile it was fitted to', 21,
        Length(Doc.Profile.X));
end;

procedure TModelHistorySessionTest.AndWithTheRFactorItMeasured;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    AssertTrue('a number, for the History tab to show: ' +
        FloatToStr(FHistory[0].Model.RFactor), FHistory[0].Model.RFactor >= 0);
end;

procedure TModelHistorySessionTest.ARunThatChangedNothingRecordsNothing;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    AssertFalse('the same model again', Record_('b'));
    AssertEquals('one entry', 1, FHistory.Count);
end;

procedure TModelHistorySessionTest.MakingAnEntryCurrentPutsItsModelBack;
var
    Fault: string;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    FittedTo(1.1);
    Record_('b');
    AssertTrue('made current: ' + Fault, MakeCurrent('a', Fault));
    AssertEquals('the width a''s run found', 2.4, SigmaNow, 1e-12);
    AssertTrue('and fitted, as it was', FService.IsCurveFitted(0));
end;

procedure TModelHistorySessionTest.AndTheModelReportsItsRFactorAgain;
var
    Fault: string;
    RFactor: double;
begin
    //  The chart, the tables, the Summary and the status bar show the model
    //  made current as they show a fitted one - R-factor included.
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    FittedTo(1.1);
    Record_('b');
    MakeCurrent('a', Fault);
    AssertTrue('a number: ' + FService.GetRFactorStr,
        TryStrToFloat(Trim(FService.GetRFactorStr), RFactor));
    AssertEquals('the one a recorded', FHistory[0].Model.RFactor, RFactor,
        1e-9);
end;

procedure TModelHistorySessionTest.AndTheEntryIsCurrent;
var
    Fault: string;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    FittedTo(1.1);
    Record_('b');
    MakeCurrent('a', Fault);
    AssertEquals('a', FHistory.CurrentId);
    //  And the next run descends from it: the branch the History tab draws.
    FittedTo(1.7);
    Record_('c');
    AssertEquals('c started from a', 'a',
        FHistory[FHistory.IndexOf('c')].ParentId);
end;

procedure TModelHistorySessionTest.AModelIsPutBackOnTheProfileItWasFittedTo;
var
    Fault: string;
    P: TTitlePointsSet;
begin
    //  THE PROFILE TRAVELS WITH THE MODEL. Smoothing - or a reloaded file -
    //  replaces it, and a model put back onto data it was never fitted to
    //  would report an R-factor for a question nobody asked.
    GivenAModel(100);
    FittedTo(2.4);
    Record_('a');
    GivenAModel(50);
    FittedTo(2.4);
    Record_('b');
    AssertTrue('made current: ' + Fault, MakeCurrent('a', Fault));
    P := FService.GetProfilePointsSet;
    try
        AssertEquals('the profile a was fitted to', 110, P.PointYCoord[10],
            1e-9);
    finally
        P.Free;
    end;
end;

procedure TModelHistorySessionTest.EditsNoEntryHoldsAreKeptBeforeTheyAreReplaced;
var
    Fault: string;
    Kept: longint;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    //  An edit by hand, after the run: no entry holds it.
    FittedTo(0.9);
    AssertTrue('made current: ' + Fault, MakeCurrent('a', Fault));
    Kept := FHistory.IndexOf('kept');
    AssertTrue('the edit was kept', Kept >= 0);
    AssertEquals('as an edit', Ord(hrkEdited), Ord(FHistory[Kept].RunKind));
    AssertEquals('descending from where it was made', 'a',
        FHistory[Kept].ParentId);
    AssertEquals('and a is current', 'a', FHistory.CurrentId);
end;

procedure TModelHistorySessionTest.ALiveModelAnEntryHoldsIsNotRecordedAgain;
var
    Fault: string;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    FittedTo(1.1);
    Record_('b');
    MakeCurrent('a', Fault);
    AssertEquals('b held the live model; nothing kept', -1,
        FHistory.IndexOf('kept'));
    //  AND MAKING b CURRENT AGAIN keeps nothing either: a is the live model,
    //  and a is recorded - measured again after its restore, which must not
    //  make it look like a different model.
    MakeCurrent('b', Fault);
    AssertEquals('still nothing kept', -1, FHistory.IndexOf('kept'));
    AssertEquals('two entries', 2, FHistory.Count);
end;

procedure TModelHistorySessionTest.AnEntryThatCannotBeRestoredIsRefusedInWords;
var
    Entries: THistoryEntries;
    Fault: string;
begin
    GivenAModel;
    FittedTo(2.4);
    Record_('a');
    Entries := EntriesOf(FHistory);
    FHistory.Load(Entries, nil, '');
    FittedTo(1.1);
    AssertFalse('refused', MakeCurrent('a', Fault));
    AssertTrue('in words: ' + Fault, Pos('profile', Fault) > 0);
    AssertEquals('and the live model is untouched', 1.1, SigmaNow, 1e-12);
end;

procedure TModelHistorySessionTest.AnUnknownEntryIsRefusedInWords;
var
    Fault: string;
begin
    GivenAModel;
    AssertFalse('refused', MakeCurrent('nobody', Fault));
    AssertTrue('in words', Fault <> '');
end;

procedure TModelHistorySessionTest.EveryIdIsNewAndNamesAPart;
var
    A, B: string;
begin
    A := NewHistoryId;
    B := NewHistoryId;
    AssertTrue('not empty', A <> '');
    AssertFalse('new each time', A = B);
    AssertEquals('no braces', 0, Pos('{', A) + Pos('}', A));
    AssertEquals('lower case', LowerCase(A), A);
end;

initialization
    //  A real engine in process: no socket, no file, no optimiser run.
    RegisterTest('unit', TModelHistorySessionTest);
end.
