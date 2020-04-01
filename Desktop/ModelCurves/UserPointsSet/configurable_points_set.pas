{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TConfigurablePointsSet.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit configurable_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

type
    TConfigurablePointsSetClass = class of TConfigurablePointsSet;

    { Defines methods for configuring user defined parameters of curve type. }
    TConfigurablePointsSet = class
    public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
        class function HasConfigurableParameters: boolean; virtual; abstract;
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
        class function ShowConfigurationDialog: boolean; virtual; abstract;
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
        class function HasDefaults: boolean; virtual; abstract;
      { Sets up default values for user configurable parameters. }
        class procedure SetDefaults; virtual; abstract;
    end;

implementation

end.
