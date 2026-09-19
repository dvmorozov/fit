// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Clearing the whole model: asked first, and only then done.)

ENTERED WHERE THE WINDOW ENTERS. Model > Clear Model calls
ClearModelWithConsent with the window as the host and the application's own
client, so these tests hand it a recording host and a client over the mocked
transport - the same two seams, and no step of the gesture skipped. Whether the
model is actually EMPTY afterwards is asked of a real engine in
testcase_model_clearing_engine.

THE REFUSAL IS THE CASE THAT MATTERS. A confirmation that is shown and then
ignored is worse than none: it teaches the user that "No" is a formality.
}
unit testcase_model_clearing;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_ui_host, fit_client, model_clearing,
    mock_ui_host, mock_http_transport, mock_fit_viewer;

type
    TModelClearingTest = class(TTestCase)
    private
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TFitClient;
        procedure GivenCurves(ACount: longint);
        function Deletions: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheUserIsAskedBeforeAnythingIsRemoved;
        procedure SayingNoRemovesNothing;
        procedure SayingYesRemovesEveryCurve;
        procedure AnEmptyModelIsNotAskedAbout;
        procedure TheQuestionSaysHowManyCurvesWillGo;
        procedure AndThatItCannotBeUndone;
        procedure OneCurveIsNotSpokenOfInThePlural;
        procedure ALeftoverRefusalIsGivenInTheServersWords;
        procedure SeveralLeftoversAreCounted;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TModelClearingTest.SetUp;
begin
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    //  Clearing ends in a refresh, and a refresh reads the profile.
    FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
end;

procedure TModelClearingTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
    FHost := nil;
    FreeAndNil(FHostObj);
end;

procedure TModelClearingTest.GivenCurves(ACount: longint);
var
    i: longint;
    Json: string;
begin
    Json := '';
    for i := 1 to ACount do
    begin
        if Json <> '' then
            Json := Json + ',';
        Json := Json + Format(
            '{"id":"{%d0000000-0000-0000-0000-000000000000}",' +
            '"params":[{"name":"x0","value":%d,"type":2,"error":-1}]}', [i, i]);
    end;
    FSvc.Reply('curves', '{"ok":true,"curves":[' + Json + ']}');
end;

function TModelClearingTest.Deletions: longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FSvc.Log.Calls.Count - 1 do
        if Pos('/points/positions/{', FSvc.Log.Calls[i]) > 0 then
            Inc(Result);
end;

procedure TModelClearingTest.TheUserIsAskedBeforeAnythingIsRemoved;
begin
    GivenCurves(2);
    ClearModelWithConsent(FHost, FClient);
    AssertEquals('one confirmation', 1, FHostObj.Log.CountOf('Confirm'));
end;

procedure TModelClearingTest.SayingNoRemovesNothing;
begin
    GivenCurves(2);
    FHostObj.ConfirmAnswer := False;
    FSvc.Log.Clear;
    AssertEquals('declined', -1, ClearModelWithConsent(FHost, FClient));
    AssertEquals('and nothing went: ' + FSvc.Log.AsText, 0, Deletions);
end;

procedure TModelClearingTest.SayingYesRemovesEveryCurve;
begin
    GivenCurves(3);
    FHostObj.ConfirmAnswer := True;
    FSvc.Log.Clear;
    //  How many actually went is the engine's to say, and a mocked transport
    //  answers the same model after every deletion - so only the asking is
    //  asserted here; the count is asserted in testcase_model_clearing_engine.
    ClearModelWithConsent(FHost, FClient);
    AssertEquals('each one asked for', 3, Deletions);
end;

procedure TModelClearingTest.AnEmptyModelIsNotAskedAbout;
begin
    //  The command is greyed for an empty model; reached anyway - a shortcut
    //  racing a refresh - a question about removing nothing is noise.
    GivenCurves(0);
    AssertEquals('nothing removed', 0, ClearModelWithConsent(FHost, FClient));
    AssertEquals('and nobody asked', 0, FHostObj.Log.CountOf('Confirm'));
end;

procedure TModelClearingTest.TheQuestionSaysHowManyCurvesWillGo;
begin
    GivenCurves(3);
    FHostObj.ConfirmAnswer := False;
    ClearModelWithConsent(FHost, FClient);
    AssertTrue('names the count: ' + FHostObj.LastConfirmText,
        Pos('3 curves', FHostObj.LastConfirmText) > 0);
end;

procedure TModelClearingTest.AndThatItCannotBeUndone;
begin
    AssertTrue(ClearModelQuestion(2),
        Pos('cannot be undone', ClearModelQuestion(2)) > 0);
end;

procedure TModelClearingTest.OneCurveIsNotSpokenOfInThePlural;
begin
    AssertTrue(ClearModelQuestion(1),
        Pos('1 curve ', ClearModelQuestion(1)) > 0);
    AssertEquals(ClearModelQuestion(1), 0,
        Pos('1 curves', ClearModelQuestion(1)));
end;

procedure TModelClearingTest.ALeftoverRefusalIsGivenInTheServersWords;
begin
    AssertTrue(ClearModelLeftoverMessage(1, 'A pack keeps it.'),
        Pos('A pack keeps it.', ClearModelLeftoverMessage(1, 'A pack keeps it.')) > 0);
end;

procedure TModelClearingTest.SeveralLeftoversAreCounted;
begin
    AssertTrue(ClearModelLeftoverMessage(2, ''),
        Pos('2 curves stay', ClearModelLeftoverMessage(2, '')) > 0);
end;

initialization
    RegisterTest('unit', TModelClearingTest);
end.
