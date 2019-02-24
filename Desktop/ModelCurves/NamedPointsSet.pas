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
    { Base curve class allowing setting up type name. Type name distinguishes
      this curve from all other curve types, as opposite to the 'Title' attributes
      which is used to distinguish separate curve instances. }
    TNamedPointsSet = class(TCurvePointsSet)
    protected
        FName: string;
        
    public
        function GetName: string; override;
        procedure SetName(AName: string);
    end;

implementation

{============================ TNamedPointsSet =================================}

function TNamedPointsSet.GetName: string;
begin
    Result := FName;
end;

procedure TNamedPointsSet.SetName(AName: string);
begin
    FName := AName;
end;

end.

