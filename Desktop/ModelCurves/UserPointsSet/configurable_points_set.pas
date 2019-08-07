unit configurable_points_set;

{$mode delphi}

interface


type
  TConfigurablePointsSetClass = class of TConfigurablePointsSet;

  { Defines methods for configuring user defined parameters of curve type. }
  TConfigurablePointsSet = class
  public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
      class function HasConfigurableParameters: Boolean; virtual; abstract;
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
      class function ShowConfigurationDialog: Boolean; virtual; abstract;
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
      class function HasDefaults: Boolean; virtual; abstract;
      { Sets up default values for user configurable parameters. }
      class procedure SetDefaults; virtual; abstract;
  end;

implementation

end.

