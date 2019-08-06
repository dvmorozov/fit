unit ConfigurableUserPointsSet;

{$mode delphi}

interface

uses
  Classes, SysUtils, ConfigurablePointsSet;

type
  { Special implementation used by TUserPointsSet. }
  TConfigurableUserPointsSet = class(TConfigurablePointsSet)
  public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
      class function HasConfigurableParameters: Boolean; override;
{$IFNDEF SERVER}
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
      class function ShowConfigurationDialog: Boolean; override;
{$ENDIF}
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
      class function HasDefaults: Boolean; override;
      { Sets up default values for user configurable parameters. }
      class procedure SetDefaults; override;
  end;

implementation

uses
{$IFNDEF SERVER}
  UserPointsSetPropDialog, ExpressionParserAdapter, CurveTypeStorageAdapter,
  main_form, CurveTypeParametersFactory, CreateUserPointsSetDlgAdapter,
{$ELSE}
  FormServer,
{$ENDIF}
  Settings, Controls, Main, Dialogs;

class function TConfigurableUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := True;
end;

{$IFNDEF SERVER}
class function TConfigurableUserPointsSet.ShowConfigurationDialog: Boolean;
var ct: Curve_type;
    ep: TExpressionParserAdapter;
    da: TCreateUserPointsSetDlgAdapter;
    cf: TCurveTypeParametersFactory;
    ca: TCurveTypeStorageAdapter;

label dlg1, dlg2;
begin
    ep := TExpressionParserAdapter.Create;
    da := TCreateUserPointsSetDlgAdapter.Create;
    cf := TCurveTypeParametersFactory.Create;
    ca := TCurveTypeStorageAdapter.Create;
{$IFNDEF EXCLUDE_SOMETHING}
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
        else Exit;
    end;

dlg2:
    UserPointsSetPropDlg.ct := ct;
    case UserPointsSetPropDlg.ShowModal of
        mrOk:
            begin
                //  Rewrites selected settings.
                ca.UpdateCurveType(ct);
            end;

        mrRetry:
            begin
                //  Deletes curve on retry.
                ca.DeleteCurveType(ct);
                goto dlg1;
            end;
    else Exit;
    end;
{$ENDIF}
end;
{$ENDIF}

class function TConfigurableUserPointsSet.HasDefaults: Boolean;
begin
    Result := False;
end;

class procedure TConfigurableUserPointsSet.SetDefaults;
begin
    //  Do nothing.
end;

end.

