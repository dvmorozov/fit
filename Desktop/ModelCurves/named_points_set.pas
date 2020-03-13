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
    TExtremumMode = (
        OnlyMaximums,
        OnlyMinimums,
        MaximumsAndMinimums
    );
    TCurveTypeId = TGuid;
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    private
        { The attribute should not be used in descendants. }
        FName: string;
        FCurveTypeId: TCurveTypeId;

    public
        { Sets name of curve type. The method is used in deserializing
          objects received from server. }
        procedure SetCurveTypeName(Name: string); virtual;
        { Returns unique identifier of curve type. }
        function GetCurveTypeId: TCurveTypeId; virtual;
        { Returns name of curve type. It's better to use function
          instead of property because property assumes storing data
          in object, but storing any data is not necessary in this case. }
        function GetCurveTypeName: string; virtual;
        { Abstract method returning unique type identifier. }
        class function GetCurveTypeId_: TCurveTypeId; virtual; abstract;
        { Returns algorithm of searching of extremum points. }
        class function GetExtremumMode: TExtremumMode; virtual; abstract;

        class function GetConfigurablePointsSet:
            TConfigurablePointsSetClass; virtual;
    end;

implementation

uses non_configurable_points_set;

{============================ TNamedPointsSet =================================}

function TNamedPointsSet.GetCurveTypeName: string;
begin
    Result := FName;
end;

procedure TNamedPointsSet.SetCurveTypeName(Name: string);
begin
    FName := Name;
end;

function TNamedPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := FCurveTypeId;
end;

class function TNamedPointsSet.GetConfigurablePointsSet: TConfigurablePointsSetClass;
begin
    Result := TNonConfigurablePointsSet;
end;

end.

