{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class representing point set having title.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit title_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, points_set, neutron_points_set;

type
    { Point set with title. TODO: must implement functionality of argument
      recalculation. }
    TTitlePointsSet = class(TNeutronPointsSet)
    public
        { Title which is displayed in chart legend. }
        Title: string;
        
        procedure CopyParameters(const Dest: TObject); override;
        constructor CreateFromPoints(
            AOwner: TComponent; const Points: TPointsSet);
    end;

implementation

{============================ TTitlePointsSet =================================}

procedure TTitlePointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TTitlePointsSet(Dest).Title := Title;
end;

constructor TTitlePointsSet.CreateFromPoints(
    AOwner: TComponent; const Points: TPointsSet);
begin
    Assert(Assigned(Points));
    inherited Create(AOwner);
    CopyPointsFrom(Points);
end;

end.


