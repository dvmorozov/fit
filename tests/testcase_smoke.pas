// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_smoke;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry;
type
  TSmokeTest = class(TTestCase)
  published
    procedure HarnessWorks;
  end;
implementation
procedure TSmokeTest.HarnessWorks;
begin
  AssertEquals('test harness arithmetic', 4, 2 + 2);
end;
initialization
  RegisterTest('unit', TSmokeTest);
end.
