// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a fit interval ends, for the module state handed to it.)

TWO INTERVALS MAY MEET AT ONE SAMPLE. The bounds are a flat list of sample x
values, sorted and walked in pairs (TFitService.CreateTasks), so the same x
cannot bound two intervals. A module whose items run end to end proposes one
interval per item; where two items share an end, the earlier interval ends one
sample short and the later one owns the shared sample - the rule such items
already follow between themselves inside one model. Slicing the module's state
for the earlier interval then needs its EXCLUSIVE right limit, so the item
ending on that shared sample still belongs to it
(docs/internal/fit-performance.md, stage 7).

A pure function of indexes, so the rule is tested without a service.
}
unit fit_interval_limits;

{$mode objfpc}{$H+}

interface

{ The x an item belonging to the interval ending at sample AEndIndex may end at:
  the next interval's first sample when that interval begins right after this
  one (ANextBegIndex = AEndIndex + 1), the interval's own last sample otherwise
  - a gap, an overlap, or no next interval (ANextBegIndex < 0). }
function IntervalHiLimitX(const ADataX: array of double;
    AEndIndex, ANextBegIndex: longint): double;

implementation

function IntervalHiLimitX(const ADataX: array of double;
    AEndIndex, ANextBegIndex: longint): double;
begin
    if (ANextBegIndex = AEndIndex + 1) and (ANextBegIndex <= High(ADataX)) then
        Result := ADataX[ANextBegIndex]
    else
        Result := ADataX[AEndIndex];
end;

end.
