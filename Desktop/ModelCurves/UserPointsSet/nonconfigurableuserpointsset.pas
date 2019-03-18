unit NonConfigurableUserPointsSet;

{$mode delphi}

interface

uses
  Classes, SysUtils, ConfigurablePointsSet;

type
  { Should be used by all curve types which
    don't have user configurable parameters. }
  TNonConfigurableUserPointsSet = class
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

class function TNonConfigurableUserPointsSet.HasConfigurableParameters: Boolean;
begin
    Result := False;
end;

class function TNonConfigurableUserPointsSet.ShowConfigurationDialog: Boolean;
begin
    Result := False;
end;

class function TNonConfigurableUserPointsSet.HasDefaults: Boolean;
begin
    Result := False;
end;

class procedure TNonConfigurableUserPointsSet.SetDefaults;
begin

end;

end.

