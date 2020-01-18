{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains interface defining callback methods to notify client adout
computation progress.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_client_callback;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

type
    { Defines callback functions called from server to client. }
    IClientCallback = interface
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
    end;

implementation

end.

