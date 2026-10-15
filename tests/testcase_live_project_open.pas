// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Opening a project against a real compute server: whole, or not at all.)

FOUND IN USE. Open Project put a document into the one live problem step by
step, the profile first; a later step was refused, the earlier ones stayed, and
the window - still showing the project open before - saved the mixture into
that project's file on its next save (findings.md, "A failed open rewrote the
project open before it"). The restore now builds the document on a new problem
and switches to it only when every step succeeded (IFitService.BeginReplacement).

Through the HTTP client and a real fit_server, which is the path the window
takes: the in-process engine cannot hold a second problem, so only this route
can say whether a failed open leaves the problem alone.
}
unit testcase_live_project_open;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, worker_process_harness,
    int_fit_service, title_points_set, gauss_points_set,
    polynomial_background_points_set, fit_project_document,
    fit_project_session;

type
    TLiveProjectOpenTest = class(TWorkerProcessTest)
    private
        procedure GiveAProfile(ACount: longint);
        function ProfileCount: longint;
        { The problem as it stands, as a document - then given ACount points. }
        function ADocumentOf(ACount: longint): TProjectDocument;
    published
        procedure AnOpenThatFailsLeavesTheProblemAsItWas;
        procedure AnOpenThatSucceedsIsTheWholeDocument;
    end;

implementation

procedure TLiveProjectOpenTest.GiveAProfile(ACount: longint);
var
    P: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    try
        for i := 0 to ACount - 1 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - ACount / 2) / 3)));
        FSvc.SetProfilePointsSet(P);
    finally
        P.Free;
    end;
end;

function TLiveProjectOpenTest.ProfileCount: longint;
var
    P: TTitlePointsSet;
begin
    P := FSvc.GetProfilePointsSet;
    try
        Result := P.PointsCount;
    finally
        P.Free;
    end;
end;

function TLiveProjectOpenTest.ADocumentOf(ACount: longint): TProjectDocument;
var
    i: longint;
begin
    Result := CaptureProject(FSvc, EmptyProjectClientContext,
        EmptyProjectDocument);
    SetLength(Result.Profile.X, ACount);
    SetLength(Result.Profile.Y, ACount);
    for i := 0 to ACount - 1 do
    begin
        Result.Profile.X[i] := i;
        Result.Profile.Y[i] := 5 + i;
    end;
end;

procedure TLiveProjectOpenTest.AnOpenThatFailsLeavesTheProblemAsItWas;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    GiveAProfile(21);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetMaxRFactor(0.042);

    //  ANOTHER PROJECT, which its settings step refuses: it states both the
    //  background variation and a background curve, a pair the engine refuses.
    Doc := ADocumentOf(61);
    Doc.Settings.BackgroundVariationEnabled := True;
    Doc.Settings.BackgroundCurveTypeId :=
        GUIDToString(DefaultBackgroundCurveTypeId);
    Doc.Settings.Stated := Doc.Settings.Stated +
        [psBackgroundVariation, psBackgroundCurveType];
    Doc.Settings.MaxRFactor := 0.5;

    AssertFalse('the open failed', ApplyProject(FSvc, Doc, Fault));
    AssertEquals('the profile it had: ' + Fault, 21, ProfileCount);
    AssertEquals('the ceiling it had', 0.042, FSvc.GetMaxRFactor, 1e-12);
    AssertTrue('the curve type it had',
        IsEqualGUID(TGaussPointsSet.GetCurveTypeId, FSvc.GetCurveType));
end;

procedure TLiveProjectOpenTest.AnOpenThatSucceedsIsTheWholeDocument;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    GiveAProfile(21);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetMaxRFactor(0.042);
    Doc := ADocumentOf(61);
    Doc.Settings.MaxRFactor := 0.5;
    Doc.Settings.Stated := Doc.Settings.Stated + [psMaxRFactor];

    AssertTrue('opened: ' + Fault, ApplyProject(FSvc, Doc, Fault));
    AssertEquals('its profile', 61, ProfileCount);
    AssertEquals('its ceiling', 0.5, FSvc.GetMaxRFactor, 1e-12);
end;

initialization
    //  An INTEGRATION test: a compute server process.
    RegisterTest('integration', TLiveProjectOpenTest);
end.
