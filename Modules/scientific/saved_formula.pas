// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A formula the user saved: its name, its expression and its
parameters, one file each in the settings folder.)

MOVED FROM THE CORE'S SETTINGS UNIT (app_settings) on 2026-10-17, with the
user's formula type: only that type's dialogs, its saved list and its menu
use it. THE CLASS NAME IS KEPT, Curve_type, because it is written into every
file a user has saved (*.cpr, as a component) and read back by name.
}
unit saved_formula;

{$mode delphi}

interface

uses
    Classes, SysUtils, persistent_curve_parameters;

type
    { Contains and serializes attributes of mathematical expression. }
    Curve_type = class(TComponent)
    private
        FName: string;
        FExpression: string;
        FParameters: Curve_parameters;

        procedure SetParameters(AParameters: Curve_parameters);

        function GetParams: TCollection;
        procedure SetParams(AParams: TCollection);

    public
        { File name to serialize / deserialize data. }
        FFileName: string;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure DefineProperties(Filer: TFiler); override;
        property Parameters: Curve_parameters read FParameters write SetParameters;

    published
        { Published properties are used in XML-serializing. }

        property Name: string read FName write FName;
        property Expression: string read FExpression write FExpression;
        //  By this way component is not written into XML-stream, we need to use DefineProperties.
        //property Params: Curve_parameters read FParams write FParams;
        { Expression parameters. }
        property Params: TCollection read GetParams write SetParams;
    end;

{ The class a saved formula's file names, for reading it back; nil for any
  other name. }
function SavedFormulaComponentClass(const AClassName: string): TComponentClass;

implementation

constructor Curve_type.Create(AOwner: TComponent);
begin
    inherited;
    Parameters := Curve_parameters.Create(nil);
end;

destructor Curve_type.Destroy;
begin
    Parameters.Free;
    inherited;
end;

procedure Curve_type.SetParameters(AParameters: Curve_parameters);
begin
    FParameters.Free;
    FParameters := AParameters;
end;

{ Empty on purpose. TComponent.DefineProperties defines the pseudo-properties
  Left and Top, which a file of this component neither carries nor needs.

  The parameters ARE persisted, through the published Params property rather
  than through a filer callback; testcase_curve_params_persistence drives that
  path. }
procedure Curve_type.DefineProperties(Filer: TFiler);
begin
end;

function Curve_type.GetParams: TCollection;
begin
    Result := Parameters.Params;
end;

procedure Curve_type.SetParams(AParams: TCollection);
begin
    Parameters.Params.Free;
    Parameters.Params := AParams;
end;

function SavedFormulaComponentClass(const AClassName: string): TComponentClass;
begin
    if CompareText(AClassName, 'Curve_type') = 0 then
        Result := Curve_type
    else
        Result := nil;
end;

end.
