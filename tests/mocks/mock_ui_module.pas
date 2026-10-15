// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IUiModule that declares a name and remembers what it was asked.)

WHY IT EXISTS. int_ui_host declares the contract between a feature module and the
application window, and the framework deliberately contains no module - the one
that exists lives in a separate private repository. So the registry that keeps a
build's UI modules unambiguous had nothing to register, and measured zero covered
lines.

WHAT IT IS FOR. Only the registry's rules: a name, and enough of the rest of the
interface to be a legal module. Anything a real module decides - what its panel
says, what its commands do - is that module's own business and is not modelled
here.

See mock_support for the -SIcorba lifetime rule: this is a plain TObject, the
fixture holds the object and the interface separately, and it nils the interface
before freeing the object.
}
unit mock_ui_module;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, int_ui_host, module_view_types, mock_support;

type
    TMockUiModule = class(TMockBase, IUiModule)
    private
        FName: string;
        FItems: TUiMenuDeclArray;
        FRowItems: TUiMenuDeclArray;
        FRowMenuAskedFor: TStringList;
        FReportCaption: string;
        FPanelId: string;
        FPanelIdSet: boolean;
        FRowMenuFailure: string;
    public
        { What PanelId answers. Left alone it is the module's name and
          '.panel'; a test about a module WITHOUT a panel - or with one
          declared as blanks - says so here. }
        procedure SetPanelId(const AId: string);
        { Makes RowMenuItems raise with AMessage, as a module that cannot say
          what it offers does. }
        procedure SetRowMenuFailure(const AMessage: string);
        { What ReportCaption answers; '' - no report - until a test says so. }
        procedure SetReportCaption(const ACaption: string);
        constructor Create(const AName: string);
        { What this module declares. Left alone, it is the one entry below -
          enough to be a legal module, which is all the registry's rules need.
          A test about what the SURFACES do with a declaration says so here
          rather than inventing a second mock. }
        procedure SetMenuItems(const AItems: TUiMenuDeclArray);
        destructor Destroy; override;
        { What this module offers over a row - the same entries whatever the
          row, which is all a test of the SURFACE needs. }
        procedure SetRowMenuItems(const AItems: TUiMenuDeclArray);
        { Every row this module's row menu was asked for, in order. }
        property RowMenuAskedFor: TStringList read FRowMenuAskedFor;

        //  IUiModule
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

implementation

procedure TMockUiModule.SetReportCaption(const ACaption: string);
begin
    FReportCaption := ACaption;
end;

function TMockUiModule.ReportCaption: string;
begin
    Result := FReportCaption;
end;

constructor TMockUiModule.Create(const AName: string);
begin
    inherited Create;
    FName := AName;
    FRowMenuAskedFor := TStringList.Create;
end;

destructor TMockUiModule.Destroy;
begin
    FRowMenuAskedFor.Free;
    inherited;
end;

procedure TMockUiModule.SetRowMenuItems(const AItems: TUiMenuDeclArray);
begin
    FRowItems := AItems;
end;

function TMockUiModule.RowMenuItems(const ARowId: string): TUiMenuDeclArray;
begin
    FRowMenuAskedFor.Add(ARowId);
    FLog.Note('RowMenuItems', ARowId);
    if FRowMenuFailure <> '' then
        raise Exception.Create(FRowMenuFailure);
    Result := FRowItems;
end;

function TMockUiModule.Name: string;
begin
    Result := FName;
end;

procedure TMockUiModule.SetMenuItems(const AItems: TUiMenuDeclArray);
begin
    FItems := AItems;
end;

function TMockUiModule.MenuItems: TUiMenuDeclArray;
var
    Item: TUiMenuDecl;
begin
    if Length(FItems) > 0 then
        Exit(FItems);

    //  ONE entry, named after the module, so two mocks in one test declare
    //  distinguishable menus.
    Item := Default(TUiMenuDecl);
    Item.Id := FName + '.act';
    Item.Caption := FName;
    Item.Kind := mkCommand;
    SetLength(Result, 1);
    Result[0] := Item;
end;

procedure TMockUiModule.Command(const AId, AData: string; AHost: IUiHost);
begin
    FLog.Note('Command', AId + '/' + AData);
end;

procedure TMockUiModule.SetPanelId(const AId: string);
begin
    FPanelId := AId;
    FPanelIdSet := True;
end;

procedure TMockUiModule.SetRowMenuFailure(const AMessage: string);
begin
    FRowMenuFailure := AMessage;
end;

function TMockUiModule.PanelId: string;
begin
    if FPanelIdSet then
        Result := FPanelId
    else
        Result := FName + '.panel';
end;

function TMockUiModule.PanelEmptyText: string;
begin
    //  NAMED, so two mocks answer differently. A test asking "did the window
    //  take the RIGHT module's wording" cannot tell with two identical answers,
    //  and taking the wrong one is the failure worth catching.
    Result := 'nothing to show in ' + FName;
end;

function TMockUiModule.PanelDetachedSuffix: string;
begin
    Result := ' (detached from ' + FName + ')';
end;

procedure TMockUiModule.PanelSelectionChanged(const ARowId, ARowText: string;
    AHost: IUiHost);
begin
    FLog.Note('PanelSelectionChanged', ARowId);
end;

end.
