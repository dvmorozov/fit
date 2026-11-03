// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Whether this run is being measured, which is what a timing
assertion has to know.)

The coverage tasks run the suite under valgrind's callgrind, which runs it
tens of times slower: a decomposition that takes two minutes took an hour and
a half there, and three timing assertions failed the measured run every time
for a defect that was not there. measured_run tells a test when its clock is
valgrind's, so it can leave the time unasserted and still run everything else.
}
unit testcase_measured_run;

{$mode objfpc}{$H+}

interface

uses
    fpcunit, testregistry, measured_run;

type
    TMeasuredRunTest = class(TTestCase)
    published
        procedure ValgrindIsKnownByTheLibraryItPreloads;
        procedure AnOrdinaryRunIsNotMeasured;
    end;

implementation

procedure TMeasuredRunTest.ValgrindIsKnownByTheLibraryItPreloads;
begin
    AssertTrue(UnderValgrindFrom(
        '/usr/libexec/valgrind/vgpreload_core-amd64-linux.so:' +
        '/usr/libexec/valgrind/vgpreload_memcheck-amd64-linux.so'));
end;

procedure TMeasuredRunTest.AnOrdinaryRunIsNotMeasured;
begin
    AssertFalse(UnderValgrindFrom(''));
    AssertFalse(UnderValgrindFrom('/usr/lib/libjemalloc.so'));
end;

initialization
    RegisterTest('unit', TMeasuredRunTest);
end.
