// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The computed profile, pinned to values recorded before the curve
storage changed.)

WHY THIS EXISTS. Curves used to be carried as one point per profile sample
whatever their shape, and the engine summed them into the calculated profile BY
INDEX: curve point i and profile point i were the same x by construction. That
convention is being removed - a curve now holds only the samples it covers - and
the summation has to locate points some other way.

An off-by-one there is the one mistake that would be silent. The fit still
converges, to a slightly wrong answer, and every existing test still passes
because they assert convergence and parameter values rather than the profile
itself.

So the profile is pinned to numbers, and the numbers were RECORDED FROM THE
ENGINE as it stood before the change - not hand-computed, which would only pin a
second implementation of the same arithmetic. Regenerate them only when the model
they describe deliberately changes, never to make a failing test pass: a diff
here means the sum moved, which is exactly what this exists to catch.

COMPARED NUMERICALLY, NOT AS TEXT. The recorded lines were made on Linux, and
demanding all seventeen digits back failed on Windows and macOS - not because
the sum had moved, but because the tails of a Gaussian reach denormal values
where the last digits belong to the platform's exp() rather than to this engine.
The comparison below is relative, with a floor taken from the profile's own
peak, which still fails loudly for the shift this is guarding against.

The cases deliberately include OVERLAPPING curves and curves that meet end to
end, because a windowed curve can only be misplaced relative to another one.

TO REGENERATE, deliberately: re-add the recording branch that wrote the file
(it lived here and was removed once the values existed, so that no environment
variable can quietly rewrite the oracle during an ordinary run), or compute the
lines by the same route and replace them wholesale. Never edit a single value.
}
unit testcase_profile_fidelity;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    points_set, curve_points_set, named_points_set, gauss_points_set,
    lorentz_points_set, curve_types_singleton, fit_task, int_curve_type_selector;

type
    TProfileFidelityTest = class(TTestCase)
    private
        function CasesFile: string;
        { One case: a profile of ACount samples over [ALo, AHi], with Gaussians at
          the given positions, summed. Returns the calculated profile. }
        function ComputeCase(const ALo, AHi: double; ACount: longint;
            const APositions: array of double): TPointsSet;
        function CaseLine(const AName: string; APoints: TPointsSet): string;
        procedure Check(const AName: string; const ALo, AHi: double;
            ACount: longint; const APositions: array of double);
    published
        procedure AProfileOfOneCurveIsUnchanged;
        procedure AProfileOfTwoOverlappingCurvesIsUnchanged;
        procedure AProfileOfCurvesMeetingEndToEndIsUnchanged;
        procedure AProfileOnAnUnevenGridIsUnchanged;
    end;

implementation

const
    //  Full precision: the point is to catch a shift of one sample, and a
    //  rounded value would hide a small one.
    VALUE_FORMAT = '%.17g';

    //  What counts as "the same value". Platform differences in exp() are of
    //  the order of the last bit, around 1E-16 relative; a curve summed one
    //  sample out moves the profile by whole percent. Nine digits sits far
    //  above the first and far below the second.
    REL_TOL = 1E-9;
    //  And a floor, because a relative test on a denormal in the tail is a test
    //  of nothing: 1E-319 against 4E-320 is a factor of eight and still zero as
    //  far as this profile is concerned. Taken from the profile's own peak.
    FLOOR_FRACTION = 1E-12;

function TProfileFidelityTest.CasesFile: string;
begin
    Result := ExtractFilePath(ParamStr(0)) + 'profile_fidelity_cases.txt';
end;

function TProfileFidelityTest.ComputeCase(const ALo, AHi: double;
    ACount: longint; const APositions: array of double): TPointsSet;
var
    Profile, Positions: TPointsSet;
    Task: TFitTask;
    Sel: ICurveTypeSelector;
    i: longint;
    x, Step: double;
begin
    Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
    Sel.SelectCurveType(TGaussPointsSet.GetCurveTypeId);

    Profile := TPointsSet.Create(nil);
    Step := (AHi - ALo) / (ACount - 1);
    for i := 0 to ACount - 1 do
    begin
        x := ALo + i * Step;
        //  A shape with structure, so a shifted curve cannot coincidentally
        //  reproduce the same sum.
        Profile.AddNewPoint(x, 10 + 5 * Sin(x) + 0.5 * x);
    end;

    Positions := TPointsSet.Create(nil);
    for i := 0 to High(APositions) do
        Positions.AddNewPoint(APositions[i],
            10 + 5 * Sin(APositions[i]) + 0.5 * APositions[i]);

    Task := TFitTask.Create(nil, False, False);
    try
        Task.SetProfilePointsSet(Profile);
        Task.SetCurvePositions(Positions);
        Task.RecreateCurves(nil);
        Task.BegIndex := 0;
        Task.EndIndex := Task.GetCalcProfile.PointsCount - 1;
        Task.ComputeProfile;

        Result := TPointsSet.Create(nil);
        for i := 0 to Task.GetCalcProfile.PointsCount - 1 do
            Result.AddNewPoint(Task.GetCalcProfile.PointXCoord[i],
                Task.GetCalcProfile.PointYCoord[i]);
    finally
        Task.Free;
    end;
end;

function TProfileFidelityTest.CaseLine(const AName: string;
    APoints: TPointsSet): string;
var
    i: longint;
begin
    Result := AName;
    for i := 0 to APoints.PointsCount - 1 do
        Result := Result + ' ' + Format(VALUE_FORMAT, [APoints.PointYCoord[i]]);
end;

procedure TProfileFidelityTest.Check(const AName: string;
    const ALo, AHi: double; ACount: longint; const APositions: array of double);
var
    Got: TPointsSet;
    Cases, Want: TStringList;
    i: longint;
    Expected: string;
    Fmt: TFormatSettings;
    E, A, Scale, Tol: double;
begin
    Got := ComputeCase(ALo, AHi, ACount, APositions);
    try
        Cases := TStringList.Create;
        Want := TStringList.Create;
        try
            AssertTrue('the recorded profiles are there: ' + CasesFile,
                FileExists(CasesFile));
            Cases.LoadFromFile(CasesFile);
            Expected := '';
            for i := 0 to Cases.Count - 1 do
                if Copy(Cases[i], 1, Length(AName) + 1) = AName + ' ' then
                    Expected := Cases[i];
            AssertTrue('the case "' + AName + '" is recorded', Expected <> '');

            //  The file is written with a '.' whatever the machine's locale is,
            //  so it is read back the same way rather than through whatever
            //  DecimalSeparator the runner happens to have.
            Fmt := DefaultFormatSettings;
            Fmt.DecimalSeparator := '.';
            Want.Delimiter := ' ';
            Want.StrictDelimiter := True;
            Want.DelimitedText := Expected;
            //  Want[0] is the case name; the samples follow it.
            AssertEquals('the number of samples for "' + AName + '"',
                Want.Count - 1, Got.PointsCount);

            //  The tolerance is relative to the PROFILE, not to each value. The
            //  tails run down to denormals - 1E-319 and smaller - where the last
            //  digits are a property of the platform's exp(), not of this
            //  engine, and demanding them identical failed on Windows and macOS
            //  against numbers recorded on Linux.
            Scale := 0;
            for i := 1 to Want.Count - 1 do
                Scale := Max(Scale, Abs(StrToFloat(Want[i], Fmt)));

            for i := 1 to Want.Count - 1 do
            begin
                E := StrToFloat(Want[i], Fmt);
                A := Got.PointYCoord[i - 1];
                Tol := Max(REL_TOL * Abs(E), FLOOR_FRACTION * Scale);
                AssertTrue(Format(
                    'the computed profile for "%s" moved at sample %d: ' +
                    'expected %.17g, got %.17g', [AName, i - 1, E, A]),
                    Abs(A - E) <= Tol);
            end;
        finally
            Want.Free;
            Cases.Free;
        end;
    finally
        Got.Free;
    end;
end;

procedure TProfileFidelityTest.AProfileOfOneCurveIsUnchanged;
begin
    Check('one-curve', 0, 20, 101, [10.0]);
end;

procedure TProfileFidelityTest.AProfileOfTwoOverlappingCurvesIsUnchanged;
begin
    //  Overlapping: each curve covers samples the other also covers, so a shift
    //  in either changes the sum where they meet.
    Check('two-overlapping', 0, 20, 101, [8.0, 11.0]);
end;

procedure TProfileFidelityTest.AProfileOfCurvesMeetingEndToEndIsUnchanged;
begin
    Check('end-to-end', 0, 20, 101, [4.0, 16.0]);
end;

procedure TProfileFidelityTest.AProfileOnAnUnevenGridIsUnchanged;
begin
    //  A sample count that does not divide the range evenly, so the x values are
    //  not round numbers and an index/coordinate confusion cannot hide.
    Check('uneven-grid', 1, 13, 97, [5.0, 9.0]);
end;

initialization
    RegisterTest('integration', TProfileFidelityTest);
end.
