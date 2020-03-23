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
        { Waiting of loading profile data. }
        ProfileWaiting,
        { Background isn't removed yet after last profile loading.
          State must not change on loading background points. }
        BackNotRemoved,
        { Computation is performed. }
        AsyncOperation,
        { States below should be used only to inform user - optimization
          should be allowed in any case when background removed (ready to
          fit parameters in automatic mode). }
        ReadyForAutoFit,
        { Ready to fit with given user constraints. }
        ReadyForFit,
        { Computation has been finished, allows further restarting. }
        Finished
        );

implementation

end.
