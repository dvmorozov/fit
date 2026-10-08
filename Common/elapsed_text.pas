// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a duration reads, the one way it is written everywhere.)

"d day(s) hh:mm:ss" - the status bar's elapsed time while a fit runs (the window,
fit_progress) and once it is over (the server, TFitService.GetCalcTimeStr), so the
panel does not change its shape the moment a fit ends.

ONE FUNCTION, NOT TWO COPIES. The server's copy padded each field itself from the
wall clock, so which of its branches ran depended on how many seconds a test
happened to take, and the coverage of fit_service moved between runs of the same
code. Here the padding is the format's, and the value is a parameter a test can
fix.
}
unit elapsed_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math;

{ ASeconds as "d day(s) hh:mm:ss"; a part second is dropped, and a negative
  duration - two clocks a moment apart - reads as none. }
function ElapsedText(ASeconds: double): string;

implementation

function ElapsedText(ASeconds: double): string;
var
    Sec, Day, Hour, Min: int64;
begin
    Sec := Trunc(Max(ASeconds, 0));
    Day := Sec div 86400;
    Sec := Sec mod 86400;
    Hour := Sec div 3600;
    Sec := Sec mod 3600;
    Min := Sec div 60;
    Sec := Sec mod 60;
    Result := Format('%d day(s) %.2d:%.2d:%.2d', [Day, Hour, Min, Sec]);
end;

end.
