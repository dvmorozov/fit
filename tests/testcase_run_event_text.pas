// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the Run Log says of each decision the framework makes.)
}
unit testcase_run_event_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_progress_json, run_event_text,
    residual_randomness;

type
    TRunEventTextTest = class(TTestCase)
    published
        procedure ARemovalSaysWhatWentAndWhatBicSaid;
        procedure ACurveWithNoPositionIsNamedByItsType;
        procedure AVetoSaysWhatBicWantedAndWhyItWasRefused;
        procedure ANoiseRemovalSaysItDescribedNothing;
        procedure TheVerdictIsTheStatusBarsPhrase;
        procedure EveryEventNamesATopicAndTheFramework;
    end;

implementation

procedure TRunEventTextTest.ARemovalSaysWhatWentAndWhatBicSaid;
var
    E: TRunEvent;
begin
    E := CurveRemovedEvent('Gaussian', True, 12.5, 401.2, 389.0);
    AssertEquals('Gaussian at 12.5 removed', E.Title);
    AssertTrue(E.Detail, Pos('BIC 401.2 with it, 389.0 without', E.Detail) > 0);
end;

procedure TRunEventTextTest.ACurveWithNoPositionIsNamedByItsType;
begin
    AssertEquals('Ramp removed',
        CurveRemovedEvent('Ramp', False, 0, 10, 5).Title);
end;

procedure TRunEventTextTest.AVetoSaysWhatBicWantedAndWhyItWasRefused;
var
    E: TRunEvent;
begin
    E := CurveKeptAsPatternEvent('Gaussian', True, 12.5, 401.2, 389.0, 0.41,
        0.003);
    AssertEquals('Gaussian at 12.5 kept', E.Title);
    AssertTrue(E.Detail, Pos('p = 0.41', E.Detail) > 0);
    AssertTrue(E.Detail, Pos('p = 0.003', E.Detail) > 0);
    AssertEquals('fitting/is-the-difference-noise', E.Topic);
end;

procedure TRunEventTextTest.ANoiseRemovalSaysItDescribedNothing;
var
    E: TRunEvent;
begin
    E := CurveRemovedAsNoiseEvent('Flat', False, 0);
    AssertEquals('Flat removed: only noise', E.Title);
end;

procedure TRunEventTextTest.TheVerdictIsTheStatusBarsPhrase;
var
    R: TResidualRandomness;
begin
    R := Default(TResidualRandomness);
    R.Tested := True;
    R.P := 0.31;
    R.Samples := 400;
    AssertEquals(ResidualVerdictText(R), RunVerdictEvent(R).Title);
end;

procedure TRunEventTextTest.EveryEventNamesATopicAndTheFramework;
var
    E: array of TRunEvent;
    i: longint;
    R: TResidualRandomness;
begin
    R := Default(TResidualRandomness);
    R.Tested := True;
    E := [CurveRemovedEvent('a', False, 0, 1, 0),
        CurveKeptAsPatternEvent('a', False, 0, 1, 0, 0.5, 0.01),
        CurveRemovedAsNoiseEvent('a', False, 0), RunVerdictEvent(R)];
    for i := 0 to High(E) do
    begin
        AssertTrue('a topic', E[i].Topic <> '');
        AssertEquals('the framework''s', RunEventFramework, E[i].Source);
        AssertTrue('a section', E[i].Section <> '');
        AssertTrue('a reason', E[i].Detail <> '');
    end;
end;

initialization
    RegisterTest('unit', TRunEventTextTest);
end.
