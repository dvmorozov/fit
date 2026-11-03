// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The numbers this module keeps for a problem, through a project's
round trip.)

The wavelength the angles read and the fraction the background search stops at
were framework settings of a project; they are this module's since 2026-10-17
and 2026-10-18, kept by its session and written in its own part of the file.
The framework's project tests (testcase_project_session) capture and apply a
problem with no module at all; these are what the module adds to that round
trip: its numbers travel in its part, and a file written before they were its
own still hands them over when opened.

A real TFitService in process, as there: a capture applied to a fresh engine
must produce the same problem, which a mock could not show.
}
unit testcase_scientific_project_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, int_fit_service, fit_service,
    title_points_set, fit_project_document, fit_project_session,
    fit_project_restore, problem_value_store, gauss_points_set,
    scientific_module, scientific_app_module;

type
    TScientificProjectSessionTest = class(TTestCase)
    private
        FSource: TFitService;
        FTarget: TFitService;
        { A peaked profile, a Gaussian selected and a wavelength given. }
        procedure GivenAProblem(AService: TFitService);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheWavelengthTravelsInTheModulesOwnPart;
        procedure AnOldFilesWavelengthIsTheProblemsWhenOpened;
        procedure AnOldFilesBackgroundFractionIsTheProblemsWhenOpened;
    end;

implementation

procedure TScientificProjectSessionTest.SetUp;
begin
    //  Before the problems are made: each takes a session of every module
    //  registered by then.
    RegisterScientificModule;
    FSource := TFitService.Create;
    FTarget := TFitService.Create;
end;

procedure TScientificProjectSessionTest.TearDown;
begin
    FreeAndNil(FSource);
    FreeAndNil(FTarget);
end;

procedure TScientificProjectSessionTest.GivenAProblem(AService: TFitService);
var
    P: TTitlePointsSet;
    Svc: IFitService;
    i: longint;
begin
    Svc := AService;
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    AService.SetProfilePointsSet(P);
    Svc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    ProblemValueStore(FormerWaveLengthValue).Put(Svc, 1.54056);
end;

procedure TScientificProjectSessionTest.TheWavelengthTravelsInTheModulesOwnPart;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, EmptyProjectClientContext,
        EmptyProjectDocument);
    AssertFalse('not among the framework''s settings',
        psWaveLength in Doc.Settings.Stated);
    AssertTrue('applied: ' + Fault, ApplyProject(FTarget, Doc, Fault));
    AssertEquals('the wavelength, through the module''s own part', 1.54056,
        ProblemValueStore(FormerWaveLengthValue).Get(FTarget), 1e-9);
end;

{ A FILE WRITTEN BEFORE 2026-10-17 states the wavelength among the
  framework's settings. The same number is the problem's after opening it, now
  kept by this module - read from the old place. }
procedure TScientificProjectSessionTest.AnOldFilesWavelengthIsTheProblemsWhenOpened;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, EmptyProjectClientContext,
        EmptyProjectDocument);
    //  As an old file reads: no module part, the number among the settings.
    Doc.ModuleDocuments := nil;
    Doc.Settings.WaveLength := 1.5406;
    Include(Doc.Settings.Stated, psWaveLength);
    AssertTrue(Fault, ApplyProject(FTarget, Doc, Fault));
    AssertEquals(1.5406, ProblemValueStore(FormerWaveLengthValue).Get(FTarget), 1e-9);
end;

{ The background fraction, likewise: a file written before 2026-10-18 states
  it among the settings, and it becomes this module's for the problem - which
  its peak search reads (BackFactorFor). }
procedure TScientificProjectSessionTest.AnOldFilesBackgroundFractionIsTheProblemsWhenOpened;
var
    Doc: TProjectDocument;
    Fault: string;
begin
    GivenAProblem(FSource);
    Doc := CaptureProject(FSource, EmptyProjectClientContext,
        EmptyProjectDocument);
    Doc.ModuleDocuments := nil;
    Doc.Settings.BackFactor := 12;
    Include(Doc.Settings.Stated, psBackFactor);
    AssertTrue(Fault, ApplyProject(FTarget, Doc, Fault));
    AssertEquals('the module''s now', 12,
        ProblemValueStore(BackgroundFractionValue).Get(FTarget), 1e-9);
    AssertEquals('what its peak search reads', 12, BackFactorFor(FTarget), 1e-9);
end;

initialization
    RegisterTest('unit', TScientificProjectSessionTest);
end.
