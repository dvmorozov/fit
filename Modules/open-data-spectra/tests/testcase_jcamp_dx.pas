// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What this module makes of a JCAMP-DX spectrum.)

TWO KINDS OF TEST, and both are needed. The decoder is exercised with string
literals in every form the standard allows - including the compressed ones that
no file in this repository happens to use, which is exactly why they would
otherwise be written once and never run. The loader is then run over a REAL
spectrum recorded from NIST, so that the numbers a user gets are the numbers the
service published.

THE FAILURES WORTH PINNING here are the quiet ones: a scale factor ignored gives
a spectrum of the right shape and the wrong size, and a repeated check value
counted twice shifts every later point by one channel. Both draw a plausible
picture.
}
unit testcase_jcamp_dx;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_loader, title_points_set, jcamp_asdf, jcamp_dx_loader;

type
    TJcampDxTest = class(TTestCase)
    private
        function Parse(const ALines: array of string): TTitlePointsSet;
    published
        //  The decoder.
        procedure PlainNumbersAreRead;
        procedure APackedLineNeedsNoSpaces;
        procedure TheFirstDigitCanCarryTheSign;
        procedure DifferencesAreAddedToWhatCameBefore;
        procedure ADuplicateCountIncludesTheValueItRepeats;
        procedure ADuplicateInsideDifferencesRepeatsTheDifference;
        procedure ACommentEndsTheData;
        //  The header rules.
        procedure ALabelIsComparedWithoutSpacesOrCase;
        procedure TheStepIsDerivedWhenItIsNotStated;
        procedure AFileStatingNoStepAtAllIsRefused;
        procedure AServicesNotFoundAnswerIsNotASpectrum;
        //  The loader, over lines and over a real file.
        procedure TheScaleFactorsAreApplied;
        procedure APeakTableIsReadAsPairs;
        procedure AFileWithNoDataSectionIsRefused;
    end;

    { READING A RECORDED FILE FROM DISK, which is a filesystem test whatever it
      asserts - so it says so rather than being counted among the unit tests
      (tests/README.md, testcase_suite_split). }
    TJcampDxFileTest = class(TTestCase)
    private
        function FixturePath(const AName: string): string;
    published
        procedure ARealNistSpectrumIsReadAsPublished;
    end;

implementation

function TJcampDxTest.Parse(const ALines: array of string): TTitlePointsSet;
var
    Loader: TJcampDxLoader;
    Lines: TStringList;
    i: longint;
begin
    Loader := TJcampDxLoader.Create(nil);
    Lines := TStringList.Create;
    try
        for i := Low(ALines) to High(ALines) do
            Lines.Add(ALines[i]);
        //  THE SAME PATH A FILE TAKES - LoadFromLines only skips the reading -
        //  so a test here cannot pass over a parser the application does not use.
        Loader.LoadFromLines(Lines);
        Result := Loader.GetPointsSetCopy;
    finally
        Lines.Free;
        Loader.Free;
    end;
end;

function TJcampDxFileTest.FixturePath(const AName: string): string;
begin
    //  Relative to the running binary, as every fixture path in this project
    //  is: the suite runs from the framework's tests directory.
    Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
        '../Modules/open-data-spectra/tests/fixtures/' + AName);
end;

procedure TJcampDxTest.PlainNumbersAreRead;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    Values := DecodeJcampLine('450.0 221 151 188', InDifference);
    AssertEquals('four numbers', 4, Length(Values));
    AssertEquals(450, Values[0], 1E-9);
    AssertEquals(188, Values[3], 1E-9);
    AssertFalse('nothing was a difference', InDifference);
end;

procedure TJcampDxTest.APackedLineNeedsNoSpaces;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    //  PAC: the sign IS the separator, so '221-151' is two numbers and the
    //  second is negative.
    Values := DecodeJcampLine('450.0+221-151', InDifference);
    AssertEquals('three numbers', 3, Length(Values));
    AssertEquals(221, Values[1], 1E-9);
    AssertEquals(-151, Values[2], 1E-9);
end;

procedure TJcampDxTest.TheFirstDigitCanCarryTheSign;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    //  SQZ: '@'=0, 'A'..'I'=1..9, 'a'..'i'=-1..-9, and the digits after the
    //  first are ordinary.
    Values := DecodeJcampLine('450.0C21a51@', InDifference);
    AssertEquals('four numbers', 4, Length(Values));
    AssertEquals('C21 is 321', 321, Values[1], 1E-9);
    AssertEquals('a51 is -151', -151, Values[2], 1E-9);
    AssertEquals('@ alone is zero', 0, Values[3], 1E-9);
end;

procedure TJcampDxTest.DifferencesAreAddedToWhatCameBefore;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    //  DIF: '%'=0, 'J'..'R'=+1..+9, 'j'..'r'=-1..-9, each against the value
    //  before it - so an error in one value corrupts every value after it, and
    //  this is the form most worth pinning.
    Values := DecodeJcampLine('450.0 100 J5 j5', InDifference);
    AssertEquals('four numbers', 4, Length(Values));
    AssertEquals('100 + 15', 115, Values[2], 1E-9);
    AssertEquals('115 - 15', 100, Values[3], 1E-9);
    AssertTrue('the line ended inside a run of differences', InDifference);
end;

procedure TJcampDxTest.ADuplicateCountIncludesTheValueItRepeats;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    //  DUP: 'S'..'Z' are 1..9 and the count INCLUDES the value being repeated,
    //  so 'V' after one value means four in all. Off by one here shifts every
    //  later point and still draws a believable curve.
    Values := DecodeJcampLine('450.0 100 V', InDifference);
    AssertEquals('x, then four values', 5, Length(Values));
    AssertEquals(100, Values[4], 1E-9);
end;

procedure TJcampDxTest.ADuplicateInsideDifferencesRepeatsTheDifference;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    //  Inside a run of differences the repeat is of the DIFFERENCE, which is
    //  what makes a straight stretch compress to two characters.
    Values := DecodeJcampLine('450.0 100 J0 T', InDifference);
    AssertEquals('x, the value, and two steps of ten', 4, Length(Values));
    AssertEquals(110, Values[2], 1E-9);
    AssertEquals(120, Values[3], 1E-9);
end;

procedure TJcampDxTest.ACommentEndsTheData;
var
    Values: TJcampValues;
    InDifference: boolean;
begin
    Values := DecodeJcampLine('450.0 100 $$ checked by hand', InDifference);
    AssertEquals('the comment is not data', 2, Length(Values));
end;

procedure TJcampDxTest.ALabelIsComparedWithoutSpacesOrCase;
var
    Label_, Value: string;
begin
    //  The standard says spaces, hyphens and case are not significant in a
    //  label, so '##DATA TYPE' and '##DataType' are one thing.
    AssertTrue(JcampLabel('##DATA TYPE=INFRARED SPECTRUM', Label_, Value));
    AssertEquals('DATATYPE', Label_);
    AssertEquals('INFRARED SPECTRUM', Value);
    AssertFalse('a data line is not a label',
        JcampLabel('450.0 221 151', Label_, Value));
end;

procedure TJcampDxTest.TheStepIsDerivedWhenItIsNotStated;
var
    Header: TJcampHeader;
    Step: double;
    Reason: string;
begin
    Header := Default(TJcampHeader);
    Header.FirstX := 450;
    Header.LastX := 3966;
    Header.NPoints := 880;
    AssertTrue(JcampStep(Header, Step, Reason));
    //  Over the GAPS between points, which is one fewer than the points.
    AssertEquals((3966 - 450) / 879, Step, 1E-9);
end;

procedure TJcampDxTest.AFileStatingNoStepAtAllIsRefused;
var
    Header: TJcampHeader;
    Step: double;
    Reason: string;
begin
    Header := Default(TJcampHeader);
    AssertFalse(JcampStep(Header, Step, Reason));
    //  Reading it with a guessed axis would give a spectrum of the right shape
    //  on the wrong scale - plausible, and wrong.
    AssertTrue(Reason, Pos('DELTAX', Reason) > 0);
end;

procedure TJcampDxTest.AServicesNotFoundAnswerIsNotASpectrum;
var
    Header: TJcampHeader;
begin
    Header := Default(TJcampHeader);
    Header.Title := 'Spectrum not found.';
    Header.HasData := False;
    //  What NIST answers for a spectrum it does not hold: a well-formed file
    //  with nothing in it, which would otherwise read as an empty profile.
    AssertTrue(JcampIsMissingSpectrum(Header));
end;

procedure TJcampDxTest.TheScaleFactorsAreApplied;
var
    Points: TTitlePointsSet;
begin
    Points := Parse(['##TITLE=Test', '##XFACTOR=2.0', '##YFACTOR=0.5',
        '##DELTAX=4.0', '##FIRSTX=100.0', '##XYDATA=(X++(Y..Y))',
        '50.0 10 20', '##END=']);
    try
        AssertEquals('two points', 2, Points.PointsCount);
        //  x is the line's own value times XFACTOR; ignoring it halves the axis.
        AssertEquals('the first x is scaled', 100, Points.PointXCoord[0], 1E-9);
        AssertEquals('and stepped by DELTAX', 104, Points.PointXCoord[1], 1E-9);
        AssertEquals('y is scaled too', 5, Points.PointYCoord[0], 1E-9);
    finally
        Points.Free;
    end;
end;

procedure TJcampDxTest.APeakTableIsReadAsPairs;
var
    Points: TTitlePointsSet;
begin
    //  A mass spectrum states every pair, so nothing is derived from a step.
    Points := Parse(['##TITLE=Test', '##PEAK TABLE=(XY..XY)',
        '39,100 50,20', '78,999', '##END=']);
    try
        AssertEquals('three peaks', 3, Points.PointsCount);
        AssertEquals(39, Points.PointXCoord[0], 1E-9);
        AssertEquals(999, Points.PointYCoord[2], 1E-9);
    finally
        Points.Free;
    end;
end;

procedure TJcampDxTest.AFileWithNoDataSectionIsRefused;
var
    Raised: string;
begin
    Raised := '';
    try
        Parse(['##TITLE=Test', '##END=']).Free;
    except
        on E: EInvalidFileFormat do
            Raised := E.Message;
    end;
    AssertTrue('refused in words: "' + Raised + '"',
        Pos('no data section', Raised) > 0);
end;

procedure TJcampDxFileTest.ARealNistSpectrumIsReadAsPublished;
var
    Loader: TJcampDxLoader;
    Points: TTitlePointsSet;
    Path: string;
begin
    Path := FixturePath('benzene-ir.jdx');
    AssertTrue('the recorded spectrum is there: ' + Path, FileExists(Path));
    Loader := TJcampDxLoader.Create(nil);
    try
        Loader.LoadDataSet(Path);
        Points := Loader.GetPointsSetCopy;
        try
            //  ##NPOINTS=880, and reading fewer or more is how an off-by-one
            //  in the compressed forms shows up.
            AssertEquals('every point NIST published', 880, Points.PointsCount);
            AssertEquals('##FIRSTX', 450, Points.PointXCoord[0], 1E-6);
            AssertEquals('##LASTX', 3966,
                Points.PointXCoord[Points.PointsCount - 1], 1E-6);
            //  ##FIRSTY=0.033074, which is 221 * ##YFACTOR: the scale factor
            //  this file would otherwise be read 6600 times too large.
            AssertEquals('##FIRSTY', 0.033074, Points.PointYCoord[0], 1E-6);
        finally
            Points.Free;
        end;
    finally
        Loader.Free;
    end;
end;

initialization
    //  The decoder and the header rules need nothing outside this process.
    RegisterTest('unit', TJcampDxTest);
    //  Reading the recorded spectrum touches the filesystem.
    RegisterTest('integration', TJcampDxFileTest);
end.
