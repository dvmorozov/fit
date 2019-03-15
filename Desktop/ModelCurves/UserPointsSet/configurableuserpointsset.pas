unit ConfigurableUserPointsSet;

{$mode delphi}

interface

uses
  Classes, SysUtils, ConfigurablePointsSet;

type
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

uses CreateUserPointsSetDialog, UserPointsSetPropDialog, Settings, Controls,
  Main, MyExceptions, Dialogs, Unit1;

class function TConfigurableUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := True;
end;

class function TConfigurableUserPointsSet.ShowConfigurationDialog: Boolean;
var ct: Curve_type;
    Success: Boolean;
label dlg1, dlg2;
begin
    Result := True; //???
{$IFNDEF EXCLUDE_SOMETHING}
    //  mashina sostoyaniy
dlg1:
    ct := nil;
    CreateSpecialCurveDlg.ActiveControl := CreateSpecialCurveDlg.EditExpression;
    case CreateSpecialCurveDlg.ShowModal of
        mrOk:
            begin
                Success := False;

                try
                    //  pervonach. razbor
                    FitClientApp_.FitClient.SetSpecialCurveParameters(
                        CreateSpecialCurveDlg.EditExpression.Text, nil);
                    Success := True;
                except
                    on E: EUserException do
                        begin
                            MessageDlg(E.Message, mtError, [mbOk], 0);
                        end;
                    else raise;
                end;

                if Success then
                begin
                    ct := Curve_type.Create(nil);
                    ct.Name := CreateSpecialCurveDlg.EditCurveName.Text;
                    ct.Expression := CreateSpecialCurveDlg.EditExpression.Text;
                    FormMain.Settings.Curve_types.Add(ct);
                    ct.Parameters :=
                        FitClientApp_.FitClient.GetSpecialCurveParameters;
                    FormMain.WriteCurve(ct);

                    //DeleteDummyCurve;
                    //  dobavlyaem vo vse menyu novyy element
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
                //  perezapisyvaetsya dlya sohraneniya
                //  sdelannyh ustanovok parametrov
                DeleteFile(PChar(ct.FileName));
                FormMain.WriteCurve(ct);
            end;
        mrRetry:
            begin
                //  pri vozvrate udalyaem
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

