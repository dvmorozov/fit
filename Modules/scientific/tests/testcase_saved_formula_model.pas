// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The component a saved formula's file holds, and which class reads it.)

A user's formula is saved as a Curve_type component (saved_formula), the
scientific module's since 2026-10-17: the framework's settings no longer read
it, the module does. These were the settings model's tests of that component
and moved here with it, so the framework's suite names no unit of this module.
}
unit testcase_saved_formula_model;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, app_settings, saved_formula,
    persistent_curve_parameters;

type
    TSavedFormulaModelTest = class(TTestCase)
    published
        //  The curve-type component a saved formula's file carries.
        procedure ACurveTypeStartsWithAnEmptyParameterSet;
        procedure ACurveTypesParametersAreItsOwnCollection;
        procedure ReplacingTheParameterSetReplacesTheCollection;
        //  Which class a component named in such a file is read as.
        procedure ASavedFormulaIsTheModulesToRead;
    end;

implementation

procedure TSavedFormulaModelTest.ACurveTypeStartsWithAnEmptyParameterSet;
var
    C: Curve_type;
begin
    C := Curve_type.Create(nil);
    try
        AssertTrue('the parameter set exists', Assigned(C.Parameters));
        AssertTrue('and its collection does too', Assigned(C.Params));
        //  ONE, not none. Curve_parameters seeds a placeholder named '?' because
        //  the filer writes an empty collection incorrectly - a workaround its
        //  own comment records. Asserted here so that the placeholder is a known
        //  fact rather than something a reader mistakes for a real parameter.
        AssertEquals('the placeholder the filer needs', 1, C.Params.Count);
        AssertEquals('and no expression', '', C.Expression);
    finally
        C.Free;
    end;
end;

procedure TSavedFormulaModelTest.ACurveTypesParametersAreItsOwnCollection;
var
    C: Curve_type;
begin
    //  Params is a view onto Parameters.Params, not a copy. A copy would make the
    //  saved file describe a parameter set the application then ignores.
    C := Curve_type.Create(nil);
    try
        C.Parameters.Params.Add;
        AssertEquals('the addition is visible through Params',
            2, C.Params.Count);
        AssertTrue('because it is the same collection',
            C.Params = C.Parameters.Params);
    finally
        C.Free;
    end;
end;

procedure TSavedFormulaModelTest.ReplacingTheParameterSetReplacesTheCollection;
var
    C: Curve_type;
    Fresh: Curve_parameters;
begin
    //  The setter frees what it replaces, which is what makes assigning a loaded
    //  parameter set over a default one safe rather than a leak.
    C := Curve_type.Create(nil);
    try
        C.Parameters.Params.Add;
        Fresh := Curve_parameters.Create(nil);
        C.Parameters := Fresh;
        AssertTrue('the new set is in place', C.Parameters = Fresh);
        //  Back to one - the fresh set's own placeholder - rather than the two
        //  the replaced set had. Reading a different count here would mean the
        //  old collection is still the one being read.
        AssertEquals('and the old collection went with the old set',
            1, C.Params.Count);
    finally
        C.Free;
    end;
end;

{ A saved formula's file names Curve_type: the settings do not read it, the
  module does. }
procedure TSavedFormulaModelTest.ASavedFormulaIsTheModulesToRead;
begin
    AssertTrue('not the settings''', SettingsComponentClass('Curve_type') = nil);
    AssertTrue('the module''s, the name as any case writes it',
        SavedFormulaComponentClass('curve_TYPE') = Curve_type);
end;

initialization
    //  A unit test: nothing leaves memory.
    RegisterTest('unit', TSavedFormulaModelTest);
end.
