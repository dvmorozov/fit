// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide the desktop application carries, registered in one call.)

WHY ONE FRONT DOOR. The guide is read in three places: Help > Explain Everything
in the window, the Explain pane when a link leads there, and the published site,
which is generated from a registry dump. If the window registered its chapters
one by one and the dumper did the same, the two lists would drift and the site
would describe a different application from the one a user installs. Both call
this instead.

READING ORDER. Explain Everything lists namespaces in the order they were
registered, so the chapters are registered here in the order a new user needs
them, and the window calls this before any module or curve type registers - the
guide comes first in the index, then what the build's curve types and modules
explain about themselves.

WHAT BELONGS HERE. What the desktop client offers the user whatever modules the
build contains: its menus, its panes, its files, the fit commands. A module
explains its own commands through its own provider, and a curve type through
curve_type_explanations; neither is repeated here.
}
unit client_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation_registry;

{ Registers every chapter of the guide, then the fit-progress explanations.
  Idempotent, like every registration front door. }
procedure RegisterClientExplanations;

{ The same providers, in the same order, for a caller that walks them without
  registering - the completeness tests. }
function ClientExplanationProviders: TExplanationProviders;

implementation

uses
    static_explanations, fit_progress_explanations,
    guide_getting_started, guide_projects, guide_data, guide_model,
    guide_fitting, guide_window;

var
    Chapters: array of TStaticExplanationProvider;

procedure EnsureChapters;

    procedure Add(const ANamespace: string; ABuild: TExplanationsBuilder);
    begin
        SetLength(Chapters, Length(Chapters) + 1);
        Chapters[High(Chapters)] :=
            TStaticExplanationProvider.Create(ANamespace, ABuild);
    end;

begin
    if Length(Chapters) > 0 then
        Exit;
    Add(GettingStartedNamespace, @GettingStartedExplanations);
    Add(ProjectsNamespace, @ProjectsExplanations);
    Add(DataNamespace, @DataExplanations);
    Add(ModelNamespace, @ModelExplanations);
    Add(FittingNamespace, @FittingExplanations);
    Add(WindowNamespace, @WindowExplanations);
end;

function ClientExplanationProviders: TExplanationProviders;
var
    i: longint;
begin
    EnsureChapters;
    Result := nil;
    SetLength(Result, Length(Chapters) + 1);
    for i := 0 to High(Chapters) do
        Result[i] := Chapters[i];
    Result[High(Result)] := FitProgressExplanationProvider;
end;

procedure RegisterClientExplanations;
var
    Providers: TExplanationProviders;
    i: longint;
begin
    Providers := ClientExplanationProviders;
    for i := 0 to High(Providers) do
        RegisterExplanationProvider(Providers[i]);
end;

procedure FreeChapters;
var
    i: longint;
begin
    for i := 0 to High(Chapters) do
        Chapters[i].Free;
    Chapters := nil;
end;

finalization
    FreeChapters;
end.
