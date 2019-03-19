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
  CreateUserPointsSetDialog, UserPointsSetPropDialog,
  Settings, Controls, Main, MyExceptions, Dialogs, Unit1;

class function TConfigurableUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := True;
end;

class function TConfigurableUserPointsSet.ShowConfigurationDialog: Boolean;
var ct: Curve_type;
label dlg1, dlg2;
begin
{$IFNDEF EXCLUDE_SOMETHING}
    //  State machine.
dlg1:
    Result := False;
    ct := nil;
    CreateUserPointsSetDlg.ActiveControl := CreateUserPointsSetDlg.EditExpression;
    case CreateUserPointsSetDlg.ShowModal of
        mrOk:
            begin
                try
                    //  Initial parsing.
                    FitClientApp_.FitClient.SetSpecialCurveParameters(
                        CreateUserPointsSetDlg.EditExpression.Text, nil);
                    Result := True;
                except
                    on E: EUserException do
                        begin
                            MessageDlg(E.Message, mtError, [mbOk], 0);
                        end;
                    else raise;
                end;

                if Result then
                begin
                    ct := Curve_type.Create(nil);
                    ct.Name := CreateUserPointsSetDlg.EditCurveName.Text;
                    ct.Expression := CreateUserPointsSetDlg.EditExpression.Text;
                    ct.Parameters :=
                        FitClientApp_.FitClient.GetSpecialCurveParameters;

                    //  Saving curve parameters.
                    FormMain.Settings.Curve_types.Add(ct);
                    FormMain.WriteCurve(ct);

                    //FormMain.DeleteDummyCurve;
                    //  Adds new menu item.
                    FormMain.AddCurveMenuItem(ct);

                    goto dlg2;
                end
                else goto dlg1;
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

