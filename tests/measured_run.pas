// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Whether the suite is running under valgrind, as the coverage tasks
run it.)

A TIMING ASSERTION MEASURES VALGRIND THERE, not the program: callgrind runs
the suite tens of times slower. A test asserting a duration asks
UnderValgrind first and leaves the time unasserted when it is - everything
else it checks still runs, and the ordinary suite still asserts the time.

Known by LD_PRELOAD, which valgrind sets to its own vgpreload libraries on
Linux, the one platform the coverage runs on: no task has to set anything.
}
unit measured_run;

{$mode objfpc}{$H+}

interface

{ Whether ALdPreload - LD_PRELOAD's value - names valgrind's libraries. }
function UnderValgrindFrom(const ALdPreload: string): boolean;

{ Whether this process runs under valgrind. }
function UnderValgrind: boolean;

implementation

uses
    SysUtils;

function UnderValgrindFrom(const ALdPreload: string): boolean;
begin
    Result := Pos('vgpreload', ALdPreload) > 0;
end;

function UnderValgrind: boolean;
begin
    Result := UnderValgrindFrom(GetEnvironmentVariable('LD_PRELOAD'));
end;

end.
