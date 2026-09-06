// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The nine fitting settings, read and written through the client, each
reaching its own field.)

NINE SETTINGS, EIGHTEEN TWO-LINE METHODS. Every one of them is `Assert the
service is there; forward`. There is nothing in any of them, which is exactly
why nothing tested them and exactly why a crossed pair is so easy to write:
`SetMaxRFactor` calling `SetBackFactor` compiles, runs, and is one word wrong in
a wall of near-identical methods.

WHAT A CROSSED PAIR DOES TO A FIT. These are not cosmetic preferences - they are
the terms of the optimisation. The maximum R-factor is when the fit declares
itself converged; the loss kind is the function being minimised; the weighting is
how much each data point counts. A dialog that writes the convergence threshold
into the background factor leaves the user with a fit that stops in the wrong
place AND a background subtraction they did not ask for, and the settings dialog
still shows both values exactly as they typed them - because the reader is
crossed the same way as the writer, and the round trip agrees with itself.

THAT SELF-CONSISTENCY IS THE HAZARD. A crossed pair is invisible from the
outside: write 0.05, read back 0.05. So these tests assert against the WIRE -
which field of the settings document was written, and which field a value came
out of - rather than against a round trip through the pair being tested.

Nine distinct values, none of them repeated, for the same reason.
}
unit testcase_client_settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport;

type
    TClientSettingsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TFitClient;
        { Answers /settings with one document carrying every setting, each at a
          value shared with no other - so a reader that took its neighbour's
          field is visible rather than plausible. }
        procedure GivenEverySetting;
        { True when the last write named AField. }
        function Wrote(const AField: string): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Reading: each from its own field.
        procedure TheConvergenceThresholdIsReadFromItsOwnField;
        procedure TheBackgroundFactorIsReadFromItsOwn;
        procedure TheCurveThresholdIsReadFromItsOwn;
        procedure WhetherTheBackgroundVariesIsReadFromItsOwn;
        procedure TheMinimizerKindIsReadFromItsOwn;
        procedure TheLossKindIsReadFromItsOwn;
        procedure TheWeightingIsReadFromItsOwn;
        procedure WhetherCurvesAreScaledIsReadFromItsOwn;

        //  Writing: each to its own field.
        procedure TheConvergenceThresholdIsWrittenToItsOwnField;
        procedure TheBackgroundFactorIsWrittenToItsOwn;
        procedure TheCurveThresholdIsWrittenToItsOwn;
        procedure WhetherTheBackgroundVariesIsWrittenToItsOwn;
        procedure TheMinimizerKindIsWrittenToItsOwn;
        procedure TheLossKindIsWrittenToItsOwn;
        procedure TheWeightingIsWrittenToItsOwn;
        procedure WhetherCurvesAreScaledIsWrittenToItsOwn;

        //  A setting that is not part of the fit.
        procedure TheServerAddressIsNotSentToTheServer;

        //  Defaults, when the server says nothing.
        procedure AnAbsentThresholdReadsAsZeroRatherThanFailing;
        procedure TheWeightingHasANamedDefault;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TClientSettingsTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TClientSettingsTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TClientSettingsTest.GivenEverySetting;
begin
    FSvc.Reply('settings',
        '{"maxRFactor":0.011,"backFactor":0.022,"curveThresh":0.033,' +
        '"backgroundVariation":true,"minimizerKind":4,"lossKind":5,' +
        '"weighting":"chi","curveScaling":true}');
end;

function TClientSettingsTest.Wrote(const AField: string): boolean;
begin
    Result := Pos('"' + AField + '"', FSvc.LastBody) > 0;
end;

{ ---- reading: each from its own field -------------------------------------- }

procedure TClientSettingsTest.TheConvergenceThresholdIsReadFromItsOwnField;
begin
    //  WHEN THE FIT DECLARES ITSELF DONE. Read from the wrong field it stops
    //  somewhere else entirely - too early is a worse model reported as
    //  converged, too late is a fit that runs its whole budget every time.
    GivenEverySetting;
    AssertEquals(0.011, FClient.MaxRFactor, 1E-9);
end;

procedure TClientSettingsTest.TheBackgroundFactorIsReadFromItsOwn;
begin
    GivenEverySetting;
    AssertEquals(0.022, FClient.BackFactor, 1E-9);
end;

procedure TClientSettingsTest.TheCurveThresholdIsReadFromItsOwn;
begin
    GivenEverySetting;
    AssertEquals(0.033, FClient.CurveThresh, 1E-9);
end;

procedure TClientSettingsTest.WhetherTheBackgroundVariesIsReadFromItsOwn;
begin
    //  Whether the fit may move the background while it fits the peaks. Read
    //  from another boolean's field, the user's choice is silently reversed.
    GivenEverySetting;
    AssertTrue(FClient.BackgroundVariationEnabled);
end;

procedure TClientSettingsTest.TheMinimizerKindIsReadFromItsOwn;
begin
    //  WHICH ALGORITHM RUNS. Crossed with the loss kind - the other small
    //  integer beside it - the fit runs a different optimiser than the one the
    //  menu shows as chosen.
    GivenEverySetting;
    AssertEquals(4, FClient.MinimizerKind);
end;

procedure TClientSettingsTest.TheLossKindIsReadFromItsOwn;
begin
    //  The function being minimised: the definition of what "a better fit"
    //  means. Distinct from the minimizer kind above by one value, deliberately.
    GivenEverySetting;
    AssertEquals(5, FClient.LossKind);
end;

procedure TClientSettingsTest.TheWeightingIsReadFromItsOwn;
begin
    //  How much each data point counts. It changes the answer, not just how
    //  fast it is reached.
    GivenEverySetting;
    AssertEquals('chi', FClient.Weighting);
end;

procedure TClientSettingsTest.WhetherCurvesAreScaledIsReadFromItsOwn;
begin
    GivenEverySetting;
    AssertTrue(FClient.CurveScalingEnabled);
end;

{ ---- writing: each to its own field ---------------------------------------- }

procedure TClientSettingsTest.TheConvergenceThresholdIsWrittenToItsOwnField;
begin
    //  ASSERTED AGAINST THE WIRE, not against a round trip. A crossed pair
    //  agrees with itself: write 0.05, read back 0.05, and the settings dialog
    //  shows exactly what the user typed while the fit uses it as something
    //  else.
    FClient.MaxRFactor := 0.05;
    AssertTrue('the field it belongs in: ' + FSvc.LastBody,
        Wrote('maxRFactor'));
    AssertFalse('and not its neighbour', Wrote('backFactor'));
end;

procedure TClientSettingsTest.TheBackgroundFactorIsWrittenToItsOwn;
begin
    FClient.BackFactor := 0.06;
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('backFactor'));
    AssertFalse('not the threshold', Wrote('maxRFactor'));
end;

procedure TClientSettingsTest.TheCurveThresholdIsWrittenToItsOwn;
begin
    FClient.CurveThresh := 0.07;
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('curveThresh'));
end;

procedure TClientSettingsTest.WhetherTheBackgroundVariesIsWrittenToItsOwn;
begin
    FClient.BackgroundVariationEnabled := True;
    AssertTrue('its own field: ' + FSvc.LastBody,
        Wrote('backgroundVariation'));
    AssertFalse('not the other boolean', Wrote('curveScaling'));
end;

procedure TClientSettingsTest.TheMinimizerKindIsWrittenToItsOwn;
begin
    FClient.MinimizerKind := 2;
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('minimizerKind'));
    AssertFalse('not the loss kind', Wrote('lossKind'));
end;

procedure TClientSettingsTest.TheLossKindIsWrittenToItsOwn;
begin
    FClient.LossKind := 3;
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('lossKind'));
    AssertFalse('not the minimizer kind', Wrote('minimizerKind'));
end;

procedure TClientSettingsTest.TheWeightingIsWrittenToItsOwn;
begin
    FClient.Weighting := 'none';
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('weighting'));
    AssertTrue('carrying the value: ' + FSvc.LastBody,
        Pos('none', FSvc.LastBody) > 0);
end;

procedure TClientSettingsTest.WhetherCurvesAreScaledIsWrittenToItsOwn;
begin
    FClient.CurveScalingEnabled := True;
    AssertTrue('its own field: ' + FSvc.LastBody, Wrote('curveScaling'));
    AssertFalse('not the other boolean', Wrote('backgroundVariation'));
end;

{ ---- a setting that is not part of the fit --------------------------------- }

procedure TClientSettingsTest.TheServerAddressIsNotSentToTheServer;
begin
    //  WHERE THE SERVER IS cannot be stored ON the server: it is what the
    //  client needs in order to reach it at all. Sent as a setting it would be
    //  written to whichever server was already reachable and lost on the next
    //  start, and pointing the client somewhere new would silently fail.
    FSvc.Log.Clear;
    FClient.ServerUrl := 'http://elsewhere:9090';
    AssertFalse('nothing was sent: ' + FSvc.Log.AsText,
        Pos('/settings', FSvc.Log.AsText) > 0);
    AssertEquals('and it took effect locally',
        'http://elsewhere:9090', FClient.ServerUrl);
end;

{ ---- defaults, when the server says nothing -------------------------------- }

procedure TClientSettingsTest.AnAbsentThresholdReadsAsZeroRatherThanFailing;
begin
    //  AN OLDER SERVER, or one that has not been configured yet, sends a
    //  document without the field. A default rather than an exception, because
    //  this is read while the settings dialog is being built - raising there
    //  would leave the user unable to open the dialog that would fix it.
    FSvc.Reply('settings', '{}');
    AssertEquals(0.0, FClient.MaxRFactor, 1E-9);
end;

procedure TClientSettingsTest.TheWeightingHasANamedDefault;
begin
    //  NOT AN EMPTY STRING. The weighting names a function, and an empty name
    //  matches none - so the default has to be a real one. Poisson is right for
    //  counting data, which is what this program fits.
    FSvc.Reply('settings', '{}');
    AssertEquals('poisson', FClient.Weighting);
end;

initialization
    //  A unit test: the client over a mock transport. No server.
    RegisterTest('unit', TClientSettingsTest);
end.
