// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every test states whether it is a unit test or an integration test.)

WHY THIS EXISTS. Coverage is measured over the UNIT suite alone. An integration
test drives the same lines over and over to check behaviour, so it inflates the
number without reaching anything new - and it is slow, because it starts servers
and runs the optimiser to convergence. Measured once: 7 classes accounted for 98%
of a 132-second run, and the remaining 460 tests together took 2.6 seconds. Under
callgrind, which simulates every instruction, that difference is the difference
between minutes and hours.

THE RULE. A test is an INTEGRATION test if it depends on anything outside its own
process:

  * a process boundary - it starts a compute server, speaks HTTP, needs the
    Python sidecar;
  * the FILESYSTEM - it reads a fixture, writes a settings file, opens a data
    file. A file is an external dependency exactly as a socket is: it can be
    missing, stale, or left behind by whatever ran before;
  * or it drives the optimiser to convergence, which is neither cheap nor a test
    of one unit.

Everything else is a UNIT test.

An earlier version of this rule kept fixture-reading tests on the unit side, on
the grounds that reading Data/2.dat is fast. Speed is not the criterion - the
dependency is.

This test fails when a test class is registered outside those two suites, which is
what a plain RegisterTest(TFoo) does. That matters because the failure is
otherwise invisible in the direction that hurts: an unclassified test silently
vanishes from --suite=unit, and coverage then reports a number for a suite that
quietly stopped running it.
}
unit testcase_suite_split;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, testutils;

type
    TSuiteSplitTest = class(TTestCase)
    published
        procedure EveryTestIsEitherAUnitOrAnIntegrationTest;
        procedure TheUnitSuiteExists;
    end;

implementation

const
    UNIT_SUITE        = 'unit';
    INTEGRATION_SUITE = 'integration';

procedure TSuiteSplitTest.EveryTestIsEitherAUnitOrAnIntegrationTest;
var
    i: longint;
    Name, Stray: string;
begin
    Stray := '';
    for i := 0 to GetTestRegistry.ChildTestCount - 1 do
    begin
        Name := GetTestRegistry.Test[i].TestName;
        if (Name <> UNIT_SUITE) and (Name <> INTEGRATION_SUITE) then
            Stray := Stray + ' ' + Name;
    end;
    //  Named, not counted: the point of failing is to say which one to classify.
    AssertEquals('these tests are registered outside both suites -' +
        ' use RegisterTest(''unit'', ...) or RegisterTest(''integration'', ...):' +
        Stray, '', Stray);
end;

procedure TSuiteSplitTest.TheUnitSuiteExists;
begin
    //  A misspelled suite name empties --suite=unit rather than failing, and an
    //  empty run reports coverage of nothing. The stray check above catches the
    //  misspelling itself; this catches the case where NOTHING was classified.
    //
    //  Only the unit suite is required to exist, and the reason is narrower than
    //  it used to say here. The old claim was that the light suite links no
    //  integration tests at all; it links four - testcase_dat_loader,
    //  testcase_expr_fidelity, testcase_log and testcase_ohlc_loader - because
    //  reading a fixture from disk makes a test an integration test while needing
    //  no LCL to do it. What the light binary has none of is an integration test
    //  that needs a SERVER, since those pull the engine and so the LCL.
    //
    //  So the asymmetry stands, but for a different reason: an integration suite
    //  may legitimately be absent from a binary that links no such test, whereas
    //  a missing unit suite always means nothing was classified.
    AssertTrue('there is a unit suite to measure coverage over',
        Assigned(GetTestRegistry.FindTest(UNIT_SUITE)));
end;

initialization
    RegisterTest(UNIT_SUITE, TSuiteSplitTest);
end.
