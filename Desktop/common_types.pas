{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of enumerable constants.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit common_types;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

type
    { Server states. Sequence of states is designated by numbers. }
    TFitServerState = (
        { waiting of loading profile data }
        ProfileWaiting,
        { background isn't removed yet after last profile loading; state must not change on loading background points (2) }
        BackNotRemoved,
        { long operation is performed }
        AsyncOperation,
        { states below should be used only to inform user - fitting should be allowed in any case }     
        { background removed (ready to fit parameters in automatic mode) }
        ReadyForAutoFit,
        { ready to fit with given user constraints }
        ReadyForFit,
        Finished
        );

implementation

end.

