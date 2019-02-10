unit IntClientCallback;

{$mode delphi}

interface

uses
  Classes, SysUtils;

{ Defines callback functions called from server to client. }
type
    IClientCallback = interface
        procedure ShowCurMin(Min: Double);
        procedure ShowProfile();
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;
    end;

implementation

end.

