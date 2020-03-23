{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TConfigurableUserPointsSet.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit configurable_user_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, configurable_points_set;

type
    { Special implementation used by TUserPointsSet. }
    TConfigurableUserPointsSet = class(TConfigurablePointsSet)
    public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
        class function HasConfigurableParameters: boolean; override;
{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
        class function ShowConfigurationDialog: boolean; override;
{$ENDIF}
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
        class function HasDefaults: boolean; override;
        { Sets up default values for user configurable parameters. }
        class procedure SetDefaults; override;
    end;

implementation

uses
    Controls, Dialogs,
{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
{$IFDEF _WINDOWS}
    user_points_set_prop_dialog, expression_parser_adapter, curve_type_storage_adapter,
    curve_type_parameters_factory, create_user_points_set_dlg_adapter, app_settings,
    int_curve_type_parameters_factory, int_create_user_points_set_dlg,
    int_curve_type_storage, int_expression_parser,
{$ENDIF}
{$ENDIF}
    app;

class function TConfigurableUserPointsSet.HasConfigurableParameters: boolean;
begin
    Result := True;
end;

{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
class function TConfigurableUserPointsSet.ShowConfigurationDialog: boolean;
{$IFDEF _WINDOWS}
var
    ct: Curve_type;
    ep: IExpressionParser;
    da: ICreateUserPointsSetDlg;
    cf: ICurveTypeParametersFactory;
    ca: ICurveTypeStorage;

label
    dlg1, dlg2;
{$ENDIF}
begin
{$IFDEF _WINDOWS}
    ep := TExpressionParserAdapter.Create;
    da := TCreateUserPointsSetDlgAdapter.Create;
    cf := TCurveTypeParametersFactory.Create;
    ca := TCurveTypeStorageAdapter.Create;

    dlg1:
        Result := False;
    ct := nil;
    case da.ShowModal of
        mrOk:
        begin
            ct := cf.CreateUserCurveType(da.GetName, da.GetExpression,
                ep.ParseExpression(ct.Expression));

            ca.AddCurveType(ct);
            goto dlg2;
        end;
        else
            Exit;
    end;

    dlg2:
        UserPointsSetPropDlg.FCurveType := ct;
    case UserPointsSetPropDlg.ShowModal of
        mrOk:
            ca.UpdateCurveType(ct);//  Rewrites selected settings.


        mrRetry:
        begin
            //  Deletes curve on retry.
            ca.DeleteCurveType(ct);
            goto dlg1;
        end;
        else
            Exit;
    end;
{$ENDIF}
end;

{$ENDIF}

class function TConfigurableUserPointsSet.HasDefaults: boolean;
begin
    Result := False;
end;

class procedure TConfigurableUserPointsSet.SetDefaults;
begin
    //  Do nothing.
end;

end.
