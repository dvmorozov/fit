// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Reading the model and the fit back off the wire: the curves, the
server's state, and the goodness-of-fit numbers.)

EVERYTHING THE USER SEES AFTER A FIT COMES THROUGH HERE. The parameters table,
the R-factor in the status bar, the statistics panel, whether the menus are
greyed out - each is a field pulled out of a JSON document the compute server
sent. None of it is computed on this side, so every one of these methods is a
place where a number can be read from the wrong key, or from the right key of the
wrong object, and become a plausible value on screen.

THAT IS THE WHOLE HAZARD, AND IT IS A QUIET ONE. A chi-square read from the
'aic' field is a number; a parameter value read from the curve next door is a
number; a state read from the wrong key is an integer that maps to some other
state and greys out the wrong half of the menu. Nothing raises, nothing logs, and
the user has no way to tell - these are exactly the values they came to the
program to find out.

AND THE GUARDS MATTER AS MUCH AS THE READS. The parameters table asks for curve
N and parameter M while the model is being rebuilt underneath it, so an index
past the end is an ordinary event rather than a caller in the wrong. Every one of
these answers a defined empty value instead of indexing into whatever the array
happens to hold.
}
unit testcase_service_reading;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Variants, fpcunit, testregistry,
    http_fit_service, int_fit_service, fit_statistics, mock_http_transport;

type
    TServiceReadingTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        { Two curves: the first with a numeric and a text parameter, the second
          with one parameter. Deliberately UNEQUAL, so an index that reached the
          wrong curve lands somewhere that is visibly different. }
        procedure GivenTwoCurves;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  How many curves there are.
        procedure TheCurveCountIsTheLengthOfTheList;
        procedure AModelWithNoCurvesCountsZero;

        //  Which curve is which.
        procedure ACurveIsIdentifiedByItsOwnHandle;
        procedure AnIndexPastTheEndHasNoHandle;
        procedure ANegativeIndexHasNoHandleEither;
        procedure ACurveIsFoundByItsHandle;
        procedure AHandleIsMatchedWithoutRegardToCase;
        procedure AnUnknownHandleIsNotFound;
        procedure AnEmptyHandleIsNotEvenAsked;

        //  The parameters of one curve.
        procedure EachCurveReportsItsOwnParameterCount;
        procedure AParameterIsReadFromTheCurveItBelongsTo;
        procedure ANumericParameterComesBackAsANumber;
        procedure ATextParameterComesBackAsText;
        procedure AParameterPastTheEndIsNull;
        procedure AParameterOfACurvePastTheEndIsNull;

        //  The uncertainty on a fitted value.
        procedure AnEstimatedErrorIsRead;
        procedure AnUnestimatedErrorIsNegative;
        procedure AnErrorOutOfRangeIsNegativeToo;

        //  What the server is doing.
        procedure TheStateIsReadFromItsOwnRoute;
        procedure ABusyServerSaysSo;
        procedure AnIdleServerSaysSo;
        procedure AServerThatSaysNothingIsNotBusy;

        //  How good the fit is.
        procedure TheRFactorsAreThreeDifferentFields;
        procedure TheElapsedTimeIsItsOwnField;
        procedure EveryStatisticIsReadFromItsOwnField;
        procedure StatisticsAreInvalidUntilAFitProducesThem;
        procedure AReplyWithNoStatisticsIsNotAFailure;
    end;

implementation

const
    BaseUrl = 'http://localhost:8080';

procedure TServiceReadingTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BaseUrl);
end;

procedure TServiceReadingTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

procedure TServiceReadingTest.GivenTwoCurves;
begin
    FSvc.Reply('curves',
        '{"curves":[' +
        '{"id":"A1","params":[' +
            '{"name":"A","value":10.5,"error":0.25},' +
            '{"name":"label","kind":"text","value":"peak one"}]},' +
        '{"id":"B2","params":[' +
            '{"name":"A","value":20.5}]}' +
        ']}');
end;

{ ---- how many curves there are --------------------------------------------- }

procedure TServiceReadingTest.TheCurveCountIsTheLengthOfTheList;
begin
    //  The number of rows the parameters table draws. One too few and the last
    //  curve of every model is invisible - present in the fit, absent from the
    //  screen, and the user cannot select what they cannot see.
    GivenTwoCurves;
    AssertEquals(2, FSvc.GetCurveCount);
end;

procedure TServiceReadingTest.AModelWithNoCurvesCountsZero;
begin
    //  The state between loading a profile and placing the first curve, which
    //  is where every session starts.
    FSvc.Reply('curves', '{"curves":[]}');
    AssertEquals(0, FSvc.GetCurveCount);
end;

{ ---- which curve is which -------------------------------------------------- }

procedure TServiceReadingTest.ACurveIsIdentifiedByItsOwnHandle;
begin
    //  THE HANDLE, NOT THE POSITION. A curve keeps its handle when the list is
    //  rebuilt around it, which is how the values a fit found are given back to
    //  the curve they belong to rather than to whatever is now in that slot.
    GivenTwoCurves;
    AssertEquals('A1', FSvc.GetCurveInstanceId(0));
    AssertEquals('B2', FSvc.GetCurveInstanceId(1));
end;

procedure TServiceReadingTest.AnIndexPastTheEndHasNoHandle;
begin
    //  ASKED WHILE THE MODEL IS BEING REBUILT, which is an ordinary event, not
    //  a caller in the wrong: the table redraws from indices it took before the
    //  fit removed a curve. An empty handle matches no curve, which is right;
    //  reading past the array would return whatever was next in memory as an
    //  identity.
    GivenTwoCurves;
    AssertEquals('', FSvc.GetCurveInstanceId(2));
end;

procedure TServiceReadingTest.ANegativeIndexHasNoHandleEither;
begin
    //  -1 is what "nothing is selected" looks like everywhere in this program,
    //  and it reaches here whenever a redraw outruns a selection change.
    GivenTwoCurves;
    AssertEquals('', FSvc.GetCurveInstanceId(-1));
end;

procedure TServiceReadingTest.ACurveIsFoundByItsHandle;
begin
    GivenTwoCurves;
    AssertEquals(1, FSvc.IndexOfCurveInstance('B2'));
end;

procedure TServiceReadingTest.AHandleIsMatchedWithoutRegardToCase;
begin
    //  A handle is a GUID, and a GUID makes the round trip through settings
    //  files and JSON in whichever case the writer chose. Matching case
    //  sensitively would lose a curve's fitted values on reload, which looks
    //  like the fit not having run.
    GivenTwoCurves;
    AssertEquals(0, FSvc.IndexOfCurveInstance('a1'));
end;

procedure TServiceReadingTest.AnUnknownHandleIsNotFound;
begin
    //  The curve was removed between the handle being taken and being looked
    //  up. -1 says so; any index would name a curve that is not the one asked
    //  for.
    GivenTwoCurves;
    AssertEquals(-1, FSvc.IndexOfCurveInstance('Z9'));
end;

procedure TServiceReadingTest.AnEmptyHandleIsNotEvenAsked;
begin
    //  NO REQUEST AT ALL. An empty handle is what a curve that has never been
    //  through a fit carries, and it is looked up on every refresh - once per
    //  curve, twice a second. Asking the server each time would be the polling
    //  traffic of the whole application for an answer that is known in advance.
    GivenTwoCurves;
    FSvc.Log.Clear;
    AssertEquals(-1, FSvc.IndexOfCurveInstance(''));
    AssertFalse('nothing was asked: ' + FSvc.Log.AsText,
        Pos('/curves', FSvc.Log.AsText) > 0);
end;

{ ---- the parameters of one curve ------------------------------------------- }

procedure TServiceReadingTest.EachCurveReportsItsOwnParameterCount;
begin
    //  UNEQUAL ON PURPOSE. With two curves of the same width, an index that
    //  reached the wrong curve would still land on a parameter and still return
    //  a number.
    GivenTwoCurves;
    AssertEquals('the first has two', 2, FSvc.GetCurveParameterCount(0));
    AssertEquals('the second has one', 1, FSvc.GetCurveParameterCount(1));
end;

procedure TServiceReadingTest.AParameterIsReadFromTheCurveItBelongsTo;
begin
    //  Both curves have a parameter called A, with different values - which is
    //  the arrangement in which reading from the wrong curve is visible rather
    //  than plausible.
    GivenTwoCurves;
    AssertEquals(10.5, double(FSvc.GetCurveParameterValue(0, 0)), 1E-9);
    AssertEquals(20.5, double(FSvc.GetCurveParameterValue(1, 0)), 1E-9);
end;

procedure TServiceReadingTest.ANumericParameterComesBackAsANumber;
begin
    //  The optimiser varies doubles; a value that arrived as text would be
    //  written back into the model as text and refuse to be fitted.
    GivenTwoCurves;
    AssertTrue('numeric', VarIsNumeric(FSvc.GetCurveParameterValue(0, 0)));
end;

procedure TServiceReadingTest.ATextParameterComesBackAsText;
begin
    //  A LABEL IS NOT A QUANTITY. Read as a number it becomes 0, and the cell
    //  that named the curve shows a zero instead - which reads as a parameter
    //  the fit drove to nothing.
    GivenTwoCurves;
    AssertEquals('peak one', string(FSvc.GetCurveParameterValue(0, 1)));
end;

procedure TServiceReadingTest.AParameterPastTheEndIsNull;
begin
    //  Null rather than 0: the table can tell "there is no such parameter" from
    //  "its value is zero", and only one of those means leave the cell blank.
    GivenTwoCurves;
    AssertTrue('null', VarIsNull(FSvc.GetCurveParameterValue(0, 5)));
    AssertTrue('and below zero too',
        VarIsNull(FSvc.GetCurveParameterValue(0, -1)));
end;

procedure TServiceReadingTest.AParameterOfACurvePastTheEndIsNull;
begin
    //  BOTH INDICES ARE CHECKED. Guarding only the parameter index would index
    //  the curve array out of range before the parameter index was ever
    //  consulted.
    GivenTwoCurves;
    AssertTrue('null', VarIsNull(FSvc.GetCurveParameterValue(9, 0)));
end;

{ ---- the uncertainty on a fitted value ------------------------------------- }

procedure TServiceReadingTest.AnEstimatedErrorIsRead;
begin
    //  The number beside the value in the table, after the ' ± '. It is what
    //  tells the user whether a fitted parameter means anything at all.
    GivenTwoCurves;
    AssertEquals(0.25, FSvc.GetCurveParameterError(0, 0), 1E-9);
end;

procedure TServiceReadingTest.AnUnestimatedErrorIsNegative;
begin
    //  NEGATIVE MEANS "NOT ESTIMATED", and it has to be distinguishable from
    //  zero: the native engine estimates no errors at all, and an uncertainty
    //  shown as ± 0 claims a fitted value is exact.
    GivenTwoCurves;
    AssertTrue('no estimate', FSvc.GetCurveParameterError(1, 0) < 0);
end;

procedure TServiceReadingTest.AnErrorOutOfRangeIsNegativeToo;
begin
    GivenTwoCurves;
    AssertTrue('no such parameter', FSvc.GetCurveParameterError(0, 9) < 0);
    AssertTrue('nor such a curve', FSvc.GetCurveParameterError(9, 0) < 0);
end;

{ ---- what the server is doing ---------------------------------------------- }

procedure TServiceReadingTest.TheStateIsReadFromItsOwnRoute;
begin
    //  THE STATE DECIDES WHICH MENUS ARE AVAILABLE. Read wrongly it greys out
    //  commands that would work, or offers commands that cannot - and the user
    //  is told nothing either way.
    FSvc.Reply('state', '{"state":2}');
    AssertTrue('running a computation', FSvc.GetState = AsyncOperation);
    FSvc.Reply('state', '{"state":0}');
    AssertTrue('waiting for a profile', FSvc.GetState = ProfileWaiting);
end;

procedure TServiceReadingTest.ABusyServerSaysSo;
begin
    //  Polled twice a second while a fit runs; it is what turns Stop on and
    //  everything else off.
    FSvc.Reply('async', '{"busy":true}');
    AssertTrue(FSvc.AsyncOper);
end;

procedure TServiceReadingTest.AnIdleServerSaysSo;
begin
    FSvc.Reply('async', '{"busy":false}');
    AssertFalse(FSvc.AsyncOper);
end;

procedure TServiceReadingTest.AServerThatSaysNothingIsNotBusy;
begin
    //  NOT BUSY IS THE SAFE DEFAULT: it leaves the commands available. Assuming
    //  busy would leave a window with everything greyed out and no way back,
    //  because the poll that would clear it is the one that answered nothing.
    FSvc.Reply('async', '{}');
    AssertFalse(FSvc.AsyncOper);
end;

{ ---- how good the fit is --------------------------------------------------- }

procedure TServiceReadingTest.TheRFactorsAreThreeDifferentFields;
begin
    //  THREE MEASURES OF THE SAME DISAGREEMENT, differing by how they weight
    //  it. They are shown side by side, so one read from another's field is a
    //  pair of numbers that agree when they should not - which reads as a fit
    //  that is better than it is.
    FSvc.Reply('stats',
        '{"rFactor":"1.5","absRFactor":"2.5","sqrRFactor":"3.5",' +
        '"calcTime":"00:07"}');
    AssertEquals('1.5', FSvc.GetRFactorStr);
    AssertEquals('2.5', FSvc.GetAbsRFactorStr);
    AssertEquals('3.5', FSvc.GetSqrRFactorStr);
end;

procedure TServiceReadingTest.TheElapsedTimeIsItsOwnField;
begin
    FSvc.Reply('stats', '{"calcTime":"00:07"}');
    AssertEquals('00:07', FSvc.GetCalcTimeStr);
end;

procedure TServiceReadingTest.EveryStatisticIsReadFromItsOwnField;
var
    S: TFitStatistics;
begin
    //  NINE FIELDS OF ONE NESTED OBJECT, every one a number, all of the same
    //  order of magnitude in a typical fit. There is no arrangement of them
    //  that looks wrong on screen, so a field read from its neighbour's key is
    //  invisible - and these are the numbers a result is judged by.
    //
    //  Distinct values here for exactly that reason.
    FSvc.Reply('stats',
        '{"statistics":{"valid":true,"dataPoints":101,"params":7,' +
        '"degreesOfFreedom":94,"chiSquare":11.5,"reducedChiSquare":12.5,' +
        '"rSquared":13.5,"aic":14.5,"bic":15.5}}');
    S := FSvc.GetStatistics;
    AssertTrue('valid', S.Valid);
    AssertEquals('data points', 101, S.DataPoints);
    AssertEquals('varying parameters', 7, S.Params);
    AssertEquals('degrees of freedom', 94, S.DegreesOfFreedom);
    AssertEquals('chi square', 11.5, S.ChiSquare, 1E-9);
    AssertEquals('reduced chi square', 12.5, S.ReducedChiSquare, 1E-9);
    AssertEquals('r squared', 13.5, S.RSquared, 1E-9);
    AssertEquals('aic', 14.5, S.AIC, 1E-9);
    AssertEquals('bic', 15.5, S.BIC, 1E-9);
end;

procedure TServiceReadingTest.StatisticsAreInvalidUntilAFitProducesThem;
begin
    //  THE FLAG IS THE WHOLE POINT of the record: without a fit there are no
    //  statistics, and a panel showing zeros claims a perfect fit of nothing.
    FSvc.Reply('stats', '{"statistics":{"valid":false}}');
    AssertFalse(FSvc.GetStatistics.Valid);
end;

procedure TServiceReadingTest.AReplyWithNoStatisticsIsNotAFailure;
begin
    //  An older server, or one that has not fitted anything yet, sends the
    //  R-factors and no statistics object. An empty record says so; raising
    //  here would take down the status poll over a panel nobody was looking at.
    FSvc.Reply('stats', '{"rFactor":"1.5"}');
    AssertFalse('not valid', FSvc.GetStatistics.Valid);
    AssertEquals('and empty', 0, FSvc.GetStatistics.DataPoints);
end;

initialization
    //  A unit test: the service over a mock transport. No socket and no server.
    RegisterTest('unit', TServiceReadingTest);
end.
