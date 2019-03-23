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
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
      class function ShowConfigurationDialog: Boolean; override;
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
      class function HasDefaults: Boolean; override;
      { Sets up default values for user configurable parameters. }
      class procedure SetDefaults; override;
  end;

implementation

uses
  UserPointsSetPropDialog, ExpressionParserAdapter,
  CreateUserPointsSetDlgAdapter, Settings, Controls, Main, Dialogs, Unit1;

class function TConfigurableUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := True;
end;

class function TConfigurableUserPointsSet.ShowConfigurationDialog: Boolean;
var ct: Curve_type;
    epa: TExpressionParserAdapter;
    da: TCreateUserPointsSetDlgAdapter;

label dlg1, dlg2;
begin
    epa := TExpressionParserAdapter.Create;
    da := TCreateUserPointsSetDlgAdapter.Create;
{$IFNDEF EXCLUDE_SOMETHING}
dlg1:
    Result := False;
    ct := nil;
    case da.ShowModal of
        mrOk:
            begin
                ct := Curve_type.Create(nil);
                ct.Name := da.GetName;
                ct.Expression := da.GetExpression;
                ct.Parameters := epa.ParseExpression(ct.Expression);

                //  Saving curve parameters.
                FormMain.Settings.Curve_types.Add(ct);
                FormMain.WriteCurve(ct);

                //FormMain.DeleteDummyCurve;
                //  Adds new menu item.
                FormMain.AddCurveMenuItem(ct);

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
                DeleteFile(PChar(ct.FileName));
                FormMain.WriteCurve(ct);
            end;

        mrRetry:
            begin
                //  Deletes curve on retry.
                FormMain.DeleteCurve(ct);
                goto dlg1;
            end;
    else Exit;
    end;
{$ENDIF}
end;

class function TConfigurableUserPointsSet.HasDefaults: Boolean;
begin
    Result := False;
end;

class procedure TConfigurableUserPointsSet.SetDefaults;
begin
    //  Do nothing.
end;

end.

