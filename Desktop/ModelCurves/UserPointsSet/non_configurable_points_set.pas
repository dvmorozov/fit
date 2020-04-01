{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TNonConfigurablePointsSet.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit non_configurable_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    configurable_points_set, SysUtils;

type
  { Should be used by all curve types which
    don't have user configurable parameters. }
    TNonConfigurablePointsSet = class(TConfigurablePointsSet)
    public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
        class function HasConfigurableParameters: boolean; override;
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
        class function ShowConfigurationDialog: boolean; override;
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
        class function HasDefaults: boolean; override;
      { Sets up default values for user configurable parameters. }
        class procedure SetDefaults; override;
    end;

implementation

class function TNonConfigurablePointsSet.HasConfigurableParameters: boolean;
begin
    Result := False;
end;

class function TNonConfigurablePointsSet.ShowConfigurationDialog: boolean;
begin
    Result := False;
end;

class function TNonConfigurablePointsSet.HasDefaults: boolean;
begin
    Result := False;
end;

class procedure TNonConfigurablePointsSet.SetDefaults;
begin

end;

end.
