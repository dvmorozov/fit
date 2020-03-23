{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains interface defining callback methods to notify client adout
computation progress.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
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
        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
    end;

implementation

end.
