{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of enumerable constants.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit common_types;

{$mode objfpc}{$H+}

interface


type
    { Server states. Sequence of states is designated by numbers. }
    TFitServerState = (
        { waiting of loading profile data (1) }
        ProfileWaiting,
        { background isn't removed yet after last profile loading; state must not change on loading background points (2) }
        BackNotRemoved,
        { long operation is performed (4) }
        AsyncOperation,
        { states below should be used only to inform user - fitting should be allowed in any case }     
        { background removed (ready to fit parameters in automatic mode) (3) }
        ReadyForAutoFit,
        { ready to fit with given user constraints (3) }
        ReadyForFit
        );

implementation

end.

