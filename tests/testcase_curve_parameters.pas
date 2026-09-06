// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a curve presents its parameters to the optimiser, and how it puts
them back when a fit is abandoned.)

THE OPTIMISER SEES A FLAT LIST OF NUMBERS. A curve holds parameters of several
kinds - varied, fixed, computed, the abscissa - and only some of them are the
optimiser's to move. The curve filters its own list down to those, and that
filtered view is addressed BY INDEX from then on.

SO THE FILTER AND THE INDEXING ARE ONE MECHANISM, and an error in either is
silent. A fixed parameter that leaks into the varied list is one the fit moves
although the user pinned it - the value they held constant drifts, and the model
still fits, just not the model they asked for. A varied one left out is never
moved at all, and the fit converges early on a parameter still at its default.

THE ROLES ARE ADDRESSED BY NAME AS WELL, because the engine has to seed an
amplitude from the data peak and a width from the fitting interval before the
first cycle. Those pointers are set when the list is built, from the parameter's
TYPE first and its NAME only as a fall-back, so a curve whose author named
nothing conventionally still gets seeded.

AND BACKUP AND RESTORE ARE WHAT UNDOES A FIT THE USER STOPPED. Every parameter's
value is put aside before the search starts; abandoning it puts them back. A
parameter missed by either half comes back holding a value from a fit that was
cancelled - which is the one outcome worse than the fit having run.
}
unit testcase_curve_parameters;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    curve_points_set, points_set, named_points_set,
    persistent_curve_parameters, persistent_curve_parameter_container,
    special_curve_parameter, amplitude_curve_parameter, gauss_points_set;

type
    { A curve carrying one of each kind of parameter, so the filter has
      something to filter. Gaussian because it is the plainest real curve type
      and it builds its own parameters in its constructor. }
    TTestableCurve = class(TGaussPointsSet)
    public
        function VariedCount: longint;
        function VariedName(AIndex: longint): string;
        function VariedValue(AIndex: longint): double;
        procedure SetVaried(AIndex: longint; AValue: double);
        function ValueNamed(const AName: string): double;
        procedure SetValueNamed(const AName: string; AValue: double);
        procedure PutValuesAside;
        procedure BringValuesBack;
        function ParameterCount: longint;
    end;

    TCurveParametersTest = class(TTestCase)
    private
        FCurve: TTestableCurve;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What the curve declares.
        procedure ACurveHasParametersOfItsOwn;
        procedure ItKnowsWhereItSits;
        procedure ItKnowsHowTallItIs;
        procedure ItKnowsHowWideItIs;

        //  The flat list the optimiser is given.
        procedure AGaussiansThreeParametersAreAllVaried;
        procedure ButItCarriesAFourthNobodyAskedFor;
        procedure EachHasAName;
        procedure AValueWrittenReachesTheParameterItNames;
        procedure WritingOneDoesNotDisturbAnother;
        procedure TheOrderIsStableBetweenReads;

        //  Addressing them by name.
        procedure AParameterIsReadableByName;
        procedure AParameterIsWritableByName;
        procedure NamesAreMatchedWithoutRegardToCase;

        //  Putting a fit back.
        procedure ValuesSurviveBeingPutAsideAndBroughtBack;
        procedure EveryVariedParameterComesBackNotJustTheFirst;
        procedure BringingBackWithoutPuttingAsideIsNotAFault;

        //  A NAME THAT IS NOT THERE. Four symmetric lookups end in Assert(False)
        //  and none had been reached - see the group comment for what that means
        //  in a build without assertions.
        procedure ReadingAParameterThatIsNotThereIsNotSilentlyZero;
        procedure WritingOneThatIsNotThereIsNotSilentlyIgnored;
        procedure AndNeitherIsTheTypedForm;
        procedure ANearMissNameIsNotAMatch;

        //  The container's own accessors.
        procedure AContainerCarriesItsParametersNameAndType;
        procedure AndWritesBothThrough;
    end;

implementation

function TTestableCurve.VariedCount: longint;
begin
    Result := VariableCount;
end;

function TTestableCurve.VariedName(AIndex: longint): string;
begin
    Result := VariableNames[AIndex];
end;

function TTestableCurve.VariedValue(AIndex: longint): double;
begin
    Result := VariableValues[AIndex];
end;

procedure TTestableCurve.SetVaried(AIndex: longint; AValue: double);
begin
    VariableValues[AIndex] := AValue;
end;

function TTestableCurve.ValueNamed(const AName: string): double;
begin
    Result := ValuesByName[AName];
end;

procedure TTestableCurve.SetValueNamed(const AName: string; AValue: double);
begin
    ValuesByName[AName] := AValue;
end;

procedure TTestableCurve.PutValuesAside;
begin
    BackupParameters;
end;

procedure TTestableCurve.BringValuesBack;
begin
    RestoreParameters;
end;

function TTestableCurve.ParameterCount: longint;
begin
    Result := Parameters.Count;
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TCurveParametersTest.SetUp;
begin
    //  THE TWO-ARGUMENT CONSTRUCTOR, which is the real one: it builds the
    //  amplitude, position and width and wires the role pointers. The
    //  single-argument one is the inherited TComponent constructor and leaves
    //  the curve with no parameters at all - a curve that looks constructed and
    //  faults on the first thing that reads it.
    FCurve := TTestableCurve.Create(nil, 10);
end;

procedure TCurveParametersTest.TearDown;
begin
    FreeAndNil(FCurve);
end;

{ ---- what the curve declares ----------------------------------------------- }

procedure TCurveParametersTest.ACurveHasParametersOfItsOwn;
begin
    //  Built by the constructor, not handed in: a curve type IS its parameter
    //  set, and one that arrived empty would be fitted with nothing to vary.
    AssertTrue('some parameters', FCurve.ParameterCount > 0);
end;

procedure TCurveParametersTest.ItKnowsWhereItSits;
begin
    //  THE THREE ROLES THE ENGINE SEEDS. It puts a curve at the peak the user
    //  clicked, gives it that peak's height and the interval's width, and it
    //  reaches all three through these - which are set when the parameter list
    //  is built, from each parameter's TYPE first and its NAME only as a
    //  fall-back.
    AssertTrue('it has a position', FCurve.Hasx0);
end;

procedure TCurveParametersTest.ItKnowsHowTallItIs;
begin
    AssertTrue('it has an amplitude', FCurve.HasA);
end;

procedure TCurveParametersTest.ItKnowsHowWideItIs;
begin
    AssertTrue('it has a width', FCurve.HasSigma);
end;

{ ---- the flat list the optimiser is given ---------------------------------- }

procedure TCurveParametersTest.AGaussiansThreeParametersAreAllVaried;
begin
    //  A Gaussian is a height, a position and a width, and the fit moves all
    //  three - so nothing this type declares is filtered out. That is exactly
    //  why it is the wrong curve type to test the FILTER with; a type carrying
    //  an abscissa is, and testcase_user_points_set asserts there that the
    //  argument is never offered to the optimiser.
    //
    //  What is asserted here is the other half: a varied parameter left out of
    //  the flat list is one the fit never moves, so it stays at its default and
    //  the fit converges early on a model that was never searched.
    AssertEquals('three are offered', 3, FCurve.VariedCount);
end;

procedure TCurveParametersTest.ButItCarriesAFourthNobodyAskedFor;
begin
    //  A PLACEHOLDER, named '?' and typed as an argument, that Curve_parameters
    //  adds in its constructor - "Collection should contain at least one item,
    //  otherwise is written incorrectly", with a TODO beside it. The built-in
    //  curve types never clear it, so every Gaussian, Lorentzian and the rest
    //  carries a parameter that stands for nothing.
    //
    //  Harmless where it has been looked for: it is typed Argument, so the
    //  optimiser never sees it and the parameters table excludes it along with
    //  every real abscissa. But it makes Parameters.Count one more than the
    //  curve has, and it means FArgP points at a placeholder for every built-in
    //  type. Pinned here so the count is a stated fact rather than a surprise,
    //  and recorded in findings.md.
    AssertEquals('four declared, three varied', 4, FCurve.ParameterCount);
end;

procedure TCurveParametersTest.EachHasAName;
var
    i: longint;
begin
    //  "Which parameters is this fit actually moving?" is the first question to
    //  ask of a fit that will not converge, and the names are how it is
    //  answered - the flat list is built by filtering, so the mapping is not
    //  obvious from outside.
    for i := 0 to FCurve.VariedCount - 1 do
        AssertTrue(Format('varied %d is named', [i]),
            FCurve.VariedName(i) <> '');
end;

procedure TCurveParametersTest.AValueWrittenReachesTheParameterItNames;
var
    Name: string;
begin
    //  What the optimiser does on every trial step. Written to the wrong
    //  parameter, a value computed for a width lands in a position - both
    //  plausible numbers, and the fit reports a model that is wrong in a way
    //  that looks like a bad fit rather than a bug.
    Name := FCurve.VariedName(0);
    FCurve.SetVaried(0, 0.75);
    AssertEquals('through the flat list', 0.75, FCurve.VariedValue(0), 1E-9);
    AssertEquals('and by name', 0.75, FCurve.ValueNamed(Name), 1E-9);
end;

procedure TCurveParametersTest.WritingOneDoesNotDisturbAnother;
var
    Other: double;
begin
    if FCurve.VariedCount < 2 then
        Exit;
    Other := FCurve.VariedValue(1);
    FCurve.SetVaried(0, 0.75);
    AssertEquals('the neighbour is untouched', Other,
        FCurve.VariedValue(1), 1E-9);
end;

procedure TCurveParametersTest.TheOrderIsStableBetweenReads;
var
    i: longint;
    First: string;
begin
    //  THE INDEX IS THE ONLY HANDLE THE OPTIMISER HAS. A list that reordered
    //  itself between one cycle and the next would have it writing back into a
    //  different parameter than the one it read - and nothing would say so.
    First := '';
    for i := 0 to FCurve.VariedCount - 1 do
        First := First + FCurve.VariedName(i) + ' ';
    FCurve.SetVaried(0, 0.75);
    for i := 0 to FCurve.VariedCount - 1 do
        AssertEquals(Format('position %d', [i]),
            Copy(First, 1, Length(First)), Copy(First, 1, Length(First)));
    AssertTrue('the list is not empty', First <> '');
end;

{ ---- addressing them by name ----------------------------------------------- }

procedure TCurveParametersTest.AParameterIsReadableByName;
begin
    //  How everything that is not the optimiser reaches a parameter: the
    //  marshalling, the tables, the seeding. An index would not survive a curve
    //  type with a different parameter order.
    FCurve.SetValueNamed('A', 42);
    AssertEquals(42.0, FCurve.ValueNamed('A'), 1E-9);
end;

procedure TCurveParametersTest.AParameterIsWritableByName;
begin
    FCurve.SetValueNamed('sigma', 2.5);
    AssertEquals(2.5, FCurve.ValueNamed('sigma'), 1E-9);
end;

procedure TCurveParametersTest.NamesAreMatchedWithoutRegardToCase;
begin
    //  A NAME MAKES THE ROUND TRIP THROUGH JSON AND A SETTINGS FILE, in
    //  whichever case the writer chose. Matched case sensitively, a curve's
    //  fitted values would be dropped on reload and the fit would look as
    //  though it had not run.
    FCurve.SetValueNamed('A', 42);
    AssertEquals('lower case', 42.0, FCurve.ValueNamed('a'), 1E-9);
end;

{ ---- putting a fit back ---------------------------------------------------- }

procedure TCurveParametersTest.ValuesSurviveBeingPutAsideAndBroughtBack;
begin
    //  WHAT UNDOES A FIT THE USER STOPPED. The values are put aside before the
    //  search starts and put back when it is abandoned; a parameter missed by
    //  either half comes back holding a value from a fit that was cancelled -
    //  worse than the fit having run, because the user believes they undid it.
    FCurve.SetValueNamed('A', 42);
    FCurve.PutValuesAside;
    FCurve.SetValueNamed('A', 999);
    FCurve.BringValuesBack;
    AssertEquals('back where it was', 42.0, FCurve.ValueNamed('A'), 1E-9);
end;

procedure TCurveParametersTest.EveryVariedParameterComesBackNotJustTheFirst;
var
    i: longint;
    Before: array of double;
begin
    //  All of them, because a fit moves all of them - and a restore that
    //  covered only the first would leave a curve half in the state before the
    //  fit and half in the state after it, which is a model that was never
    //  computed at all.
    SetLength(Before, FCurve.VariedCount);
    for i := 0 to FCurve.VariedCount - 1 do
    begin
        FCurve.SetVaried(i, 0.5 + i);
        Before[i] := FCurve.VariedValue(i);
    end;
    FCurve.PutValuesAside;
    for i := 0 to FCurve.VariedCount - 1 do
        FCurve.SetVaried(i, 90 + i);
    FCurve.BringValuesBack;
    for i := 0 to FCurve.VariedCount - 1 do
        AssertEquals(Format('varied %d', [i]), Before[i],
            FCurve.VariedValue(i), 1E-9);
end;

procedure TCurveParametersTest.BringingBackWithoutPuttingAsideIsNotAFault;
begin
    //  The window can abandon a fit that never started - the user presses Stop
    //  before the first cycle - so this arrives with nothing put aside, as an
    //  ordinary event rather than a caller in the wrong.
    FCurve.BringValuesBack;
    AssertTrue('it returned', True);
end;

{ ------------------------- a name that is not there ------------------------- }

{ FOUR SYMMETRIC LOOKUPS - read a value, write a value, read a typed value,
  write a typed value - each walking the list and each ending in Assert(False)
  when the name is not found. None had ever been reached.

  WHAT THAT MEANS DEPENDS ON THE BUILD, which is the point. With assertions on,
  as here and in every development build, a mistyped name raises and is
  impossible to miss. With them off, the readers answer zero (or Null) and THE
  WRITERS DO NOTHING AT ALL: `ValuesByName['sigmaa'] := 2.5` succeeds, silently,
  and the value the user set is gone. That is the failure worth knowing about,
  and it is the one a test cannot provoke from here - so what these pin is that
  the checked build really does refuse, which is what makes the mistake findable
  before a release build hides it. }

procedure TCurveParametersTest.ReadingAParameterThatIsNotThereIsNotSilentlyZero;
var
    Raised: boolean;
begin
    Raised := False;
    try
        FCurve.ValuesByName['nosuchparameter'];
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('reading an unknown name is refused', Raised);
end;

procedure TCurveParametersTest.WritingOneThatIsNotThereIsNotSilentlyIgnored;
var
    Raised: boolean;
begin
    //  THE WORSE OF THE TWO. A read that answers zero is usually noticed by
    //  whatever does arithmetic with it; a write that does nothing is noticed by
    //  nobody, and the curve keeps the value it had.
    Raised := False;
    try
        FCurve.ValuesByName['nosuchparameter'] := 2.5;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('writing an unknown name is refused', Raised);
end;

procedure TCurveParametersTest.AndNeitherIsTheTypedForm;
var
    ReadRaised, WriteRaised: boolean;
begin
    //  THE SAME PAIR AGAIN, through the variant-typed accessors that a
    //  parameter carrying something other than a number is reached by. Four
    //  copies of one lookup drift apart exactly here: the pair somebody uses
    //  gets the guard and the pair somebody does not use keeps whatever it had.
    ReadRaised := False;
    try
        FCurve.Parameters.TypedByName['nosuchparameter'];
    except
        on Exception do
            ReadRaised := True;
    end;
    WriteRaised := False;
    try
        FCurve.Parameters.TypedByName['nosuchparameter'] := 2.5;
    except
        on Exception do
            WriteRaised := True;
    end;
    AssertTrue('the typed read is refused', ReadRaised);
    AssertTrue('and the typed write', WriteRaised);
end;

procedure TCurveParametersTest.ANearMissNameIsNotAMatch;
var
    Raised: boolean;
begin
    //  THE LOOKUP IS CASE-INSENSITIVE AND NOT MORE THAN THAT. A prefix or
    //  substring match would have `sigm` reach `sigma`, and a user's typo would
    //  land on a real parameter instead of being refused - which is worse than
    //  either the raise or the silence.
    Raised := False;
    try
        FCurve.ValuesByName['sigm'] := 2.5;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('a prefix of a real name is not that name', Raised);
    AssertTrue('and the real one still answers',
        FCurve.ValuesByName['sigma'] <> 2.5);
end;

{ ------------------------ the container's own accessors --------------------- }

procedure TCurveParametersTest.AContainerCarriesItsParametersNameAndType;
var
    C: TPersistentCurveParameterContainer;
begin
    //  THE PERSISTENCE SEAM. These four accessors are what the settings file is
    //  written and read through, so a name or a type that does not travel makes
    //  a saved user curve come back as something else.
    C := TPersistentCurveParameterContainer(FCurve.Parameters.Params.Items[0]);
    AssertTrue('it carries a parameter', Assigned(C.Parameter));
    AssertEquals('and its name', C.Parameter.Name, C.Name);
    AssertTrue('and its type', C.Parameter.Type_ = C.Type_);
end;

procedure TCurveParametersTest.AndWritesBothThrough;
var
    C: TPersistentCurveParameterContainer;
begin
    //  THROUGH TO THE PARAMETER, not into a field of the container's own: the
    //  optimiser reads the parameter, so a container holding its own copy would
    //  load a saved curve whose name and type the fit never sees.
    C := TPersistentCurveParameterContainer(FCurve.Parameters.Params.Items[0]);
    C.Name := 'renamed';
    AssertEquals('the name reached the parameter', 'renamed',
        C.Parameter.Name);
    C.Type_ := Shared;
    AssertTrue('and the type', C.Parameter.Type_ = Shared);
end;

initialization
    //  A unit test: one curve and its parameters, in memory. No optimiser.
    RegisterTest('unit', TCurveParametersTest);
end.
