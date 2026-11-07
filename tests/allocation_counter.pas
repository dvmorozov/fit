// SPDX-License-Identifier: GPL-3.0-or-later
{ How many heap allocations a stretch of code makes.

  For the tests that hold a hot path to "allocates nothing" (fit-performance.md):
  a string built where none is needed shows up here as a count, where a timing
  would only show it as noise. Counts through a memory manager that forwards
  every call to the one in force, so nothing about the allocation changes.

  One stretch at a time, on the calling thread's watch - a thread allocating
  meanwhile is counted too, which in the single-threaded test runner nothing
  does. }
unit allocation_counter;

{$mode objfpc}{$H+}

interface

{ Starts counting. Must be paired with StopCountingAllocations, in a finally. }
procedure StartCountingAllocations;
{ Stops counting and answers how many allocations were made since the start. }
function StopCountingAllocations: longint;

implementation

var
    GPlainManager: TMemoryManager;
    GAllocations: longint;

function CountingGetMem(Size: PtrUInt): Pointer;
begin
    Inc(GAllocations);
    Result := GPlainManager.GetMem(Size);
end;

function CountingAllocMem(Size: PtrUInt): Pointer;
begin
    Inc(GAllocations);
    Result := GPlainManager.AllocMem(Size);
end;

function CountingReAllocMem(var P: Pointer; Size: PtrUInt): Pointer;
begin
    Inc(GAllocations);
    Result := GPlainManager.ReAllocMem(P, Size);
end;

procedure StartCountingAllocations;
var
    Counting: TMemoryManager;
begin
    GetMemoryManager(GPlainManager);
    Counting := GPlainManager;
    Counting.GetMem := @CountingGetMem;
    Counting.AllocMem := @CountingAllocMem;
    Counting.ReAllocMem := @CountingReAllocMem;
    GAllocations := 0;
    SetMemoryManager(Counting);
end;

function StopCountingAllocations: longint;
begin
    SetMemoryManager(GPlainManager);
    Result := GAllocations;
end;

end.
