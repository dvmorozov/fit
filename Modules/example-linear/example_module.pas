// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Everything the example module contributes, in one call.)

A MODULE'S FRONT DOOR. A host calls RegisterExampleModule and gets the whole
contribution; it calls nothing else about this module, and no framework file
names it.

The uses clause below is what LINKS the module. A unit nothing references is not
compiled into the binary, so its initialization section never runs and its curve
type is simply absent - with nothing to say so. That is not hypothetical: a whole
feature was once missing from the compute server for exactly this reason, with
every test passing. ExpectCurveTypes is the answer: it asserts, in whichever
binary is running, that the types really did register.
}
unit example_module;

{$mode objfpc}{$H+}

interface

{ Registers everything this module contributes. Idempotent, so a host that calls
  it twice - or a test that calls it after the application already has - is fine. }
procedure RegisterExampleModule;

implementation

uses
    curve_type_registration,
    //  Naming it is what links it; it self-registers from its own
    //  initialization section, and the call below verifies that happened here.
    linear_points_set;

procedure RegisterExampleModule;
begin
    ExpectCurveTypes('Example', [TLinearPointsSet]);
end;

end.
