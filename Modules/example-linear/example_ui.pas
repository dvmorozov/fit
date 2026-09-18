// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The smallest UI a module can contribute, and how it explains itself.)

A MODULE NAMES NO WIDGET. It declares its menu as data - one entry here - and the
window builds it. The entry carries a Topic, so the Explain pane can say what it
does before it is chosen, and choosing it asks the host to put an explanation in
front of the user: the two halves of the self-explaining contract a module owes.

NO ROW MENU, deliberately. The framework asks only the module whose rows fill the
Model panel what it offers over a row, and this module places ordinary curves,
whose rows are the framework's. A module that fills the panel with rows of its
own - an analysis pack's markup - answers RowMenuItems; see int_ui_host.
}
unit example_ui;

{$mode objfpc}{$H+}

interface

uses
    int_ui_host;

const
    ExplainRampId = 'example.explain';

function ExampleUiModule: IUiModule;
procedure RegisterExampleUi;

implementation

uses
    module_view_types, curve_type_explanations, linear_points_set;

type
    TExampleUi = class(TObject, IUiModule)
    public
        function Name: string;
        function MenuItems: TUiMenuDeclArray;
        procedure Command(const AId, AData: string; AHost: IUiHost);
        function PanelId: string;
        function PanelEmptyText: string;
        function PanelDetachedSuffix: string;
        procedure PanelSelectionChanged(const ARowId, ARowText: string;
            AHost: IUiHost);
        function RowMenuItems(const ARowId: string): TUiMenuDeclArray;
        function ReportCaption: string;
    end;

var
    Instance: TExampleUi = nil;

function TExampleUi.Name: string;
begin
    Result := 'example';
end;

function TExampleUi.MenuItems: TUiMenuDeclArray;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0] := Default(TUiMenuDecl);
    Result[0].Id := ExplainRampId;
    Result[0].Caption := 'Explain the Linear Ramp';
    Result[0].Hint := 'Shows what the linear ramp is, what its parameters do and ' +
        'what it does not cover.';
    Result[0].Kind := mkCommand;
    Result[0].Topic := CurveTypeTopic(TLinearPointsSet);
end;

procedure TExampleUi.Command(const AId, AData: string; AHost: IUiHost);
begin
    if AId = ExplainRampId then
        AHost.ShowExplanation(CurveTypeTopic(TLinearPointsSet));
end;

function TExampleUi.PanelId: string;
begin
    Result := '';
end;

function TExampleUi.PanelEmptyText: string;
begin
    Result := 'The example module shows no panel of its own.';
end;

function TExampleUi.PanelDetachedSuffix: string;
begin
    Result := '';
end;

procedure TExampleUi.PanelSelectionChanged(const ARowId, ARowText: string;
    AHost: IUiHost);
begin
end;

function TExampleUi.RowMenuItems(const ARowId: string): TUiMenuDeclArray;
begin
    Result := nil;
end;

{ NO REPORT. A ramp places a curve and judges nothing, so this module declines
  the report tab - and a build carrying only it has none. }
function TExampleUi.ReportCaption: string;
begin
    Result := '';
end;

function ExampleUiModule: IUiModule;
begin
    if not Assigned(Instance) then
        Instance := TExampleUi.Create;
    Result := Instance;
end;

procedure RegisterExampleUi;
begin
    RegisterUiModule(ExampleUiModule);
end;

finalization
    Instance.Free;
end.
