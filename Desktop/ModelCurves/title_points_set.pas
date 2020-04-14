{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class representing point set having title.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit title_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, neutron_points_set, SysUtils;

type
    { Point set with title. TODO: must implement functionality of argument
      recalculation. }
    TTitlePointsSet = class(TNeutronPointsSet)
    public
        { FTitle which is displayed in chart legend. }
        FTitle: string;

        procedure CopyParameters(Dest: TObject); override;
    end;

implementation

{============================ TTitlePointsSet =================================}

procedure TTitlePointsSet.CopyParameters(Dest: TObject);
begin
    inherited;
    TTitlePointsSet(Dest).FTitle := FTitle;
end;

end.
