{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base curve class allowing setting up type name.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit named_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses curve_points_set, configurable_points_set;

type
    TNamedPointsSetClass = class of TNamedPointsSet;
    TExtremumMode   = (
        OnlyMaximums,
        OnlyMinimums,
        MaximumsAndMinimums
        );
    TCurveTypeId    = TGuid;
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    private
        { The attribute should not be used in descendants. }
        FName: string;

    public
        { Sets name of curve type. The method is used in deserializing
          objects received from server. }
        procedure SetCurveTypeName(Name: string); virtual;
        { Returns unique name of curve type. }
        class function GetCurveTypeName: string; virtual; abstract;
        { Returns unique type identifier. }
        class function GetCurveTypeId: TCurveTypeId; virtual; abstract;
        { Returns algorithm of searching of extremum points. }
        class function GetExtremumMode: TExtremumMode; virtual; abstract;
        class function GetConfigurablePointsSet: TConfigurablePointsSetClass; virtual;
    end;

implementation

uses non_configurable_points_set;

{============================ TNamedPointsSet =================================}

procedure TNamedPointsSet.SetCurveTypeName(Name: string);
begin
    FName := Name;
end;

class function TNamedPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TNonConfigurablePointsSet;
end;

end.
