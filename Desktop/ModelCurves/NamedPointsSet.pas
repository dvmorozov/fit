{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base curve class allowing setting up type name.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit NamedPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils, CurvePointsSet;

type
    TCurveTypeId = TGuid;
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    private
        { The attribute should not be used in descendants. }
        FName: string;
        
    public
        { Returns name of the curve type. It's better to use function
          instead of property because property assumes storing data
          in object, but storing any data is not necessary in this case. }
        function GetTypeName: string; virtual;
        { The method is used in deserializing objects received from server. }
        procedure SetName(AName: string); virtual;
    end;

implementation

{============================ TNamedPointsSet =================================}

function TNamedPointsSet.GetTypeName: string;
begin
    Result := FName;
end;

procedure TNamedPointsSet.SetName(AName: string);
begin
    FName := AName;
end;

end.

