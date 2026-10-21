// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The diffractometers' and refinement programs' own formats, read as
a pattern of 2 Theta against intensity.)

Two halves. Each format's rules over text written here (unit), and a real
PANalytical file from a public dataset read whole by the loader the import uses
(integration, because it reads a file; fixtures/README.md says where it came
from).
}
unit testcase_diffraction_formats;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    diffraction_formats, data_loader, title_points_set;

type
    TDiffractionFormatsTest = class(TTestCase)
    private
        function Lines(const AText: array of string): TStringList;
    published
        procedure AnXrdmlScanIsSpreadFromItsStartToItsEnd;
        procedure AnXrdmlListOfPositionsIsTakenAsItIs;
        procedure AnXrdmlScanWithoutAnglesIsRefused;
        procedure AnXrdmlWithCountsRatherThanIntensitiesIsRead;
        procedure NotXmlIsRefusedSayingSo;
        procedure AUxdOfCountsIsSteppedFromItsStart;
        procedure AUxdOfPairsIsReadAsPairs;
        procedure AUxdOfCountsWithNoStepIsRefused;
        procedure OnlyTheFirstUxdRangeIsRead;
        procedure AGsasStdBankIsInCentidegrees;
        procedure AGsasEsdBankSkipsItsEsds;
        procedure AGsasFxyeBankIsReadPointByPoint;
        procedure AGsasBankNotInAnglesIsRefused;
        procedure AGsasFileWithNoBankIsRefused;
        procedure ACifNumberDropsItsUncertainty;
        procedure CifTokensKeepQuotedStringsAndTextFieldsWhole;
        procedure APowderCifLoopIsAngleAgainstIntensity;
        procedure APowderCifOfCountsTakesItsAnglesFromItsRange;
        procedure ACifOfAStructureIsRefusedSayingWhy;
        procedure AnAngleReadTwiceKeepsItsFirstPoint;
        //  Each format through its loader, the way an import reaches it.
        procedure EachLoaderMakesItsPatternTheProfile;
        procedure AnXrdmlListNotMatchingItsIntensitiesIsRefused;
        procedure AnXrdmlScanWithNoStartIsRefused;
        procedure AnXrdmlWithNoScanIsRefused;
        procedure AnXrdmlScanWithNoIntensitiesIsRefused;
        procedure AGsasBankLineTooShortIsRefused;
        procedure AGsasConstBankWithNoStepIsRefused;
        procedure AGsasTypeNotReadIsRefusedByName;
        procedure AnXrdmlNumberThatIsNotOneIsRefused;
    end;

    TDiffractionFileTest = class(TTestCase)
    published
        procedure ARealXrdmlScanIsReadWhole;
    end;

implementation

function TDiffractionFormatsTest.Lines(const AText: array of string): TStringList;
var
    i: longint;
begin
    Result := TStringList.Create;
    for i := 0 to High(AText) do
        Result.Add(AText[i]);
end;

const
    XrdmlHead = '<?xml version="1.0"?><xrdMeasurements xmlns="http://www.xrdml.' +
        'com/XRDMeasurement/1.6"><xrdMeasurement><scan scanAxis="Gonio">' +
        '<dataPoints>';
    XrdmlTail = '</dataPoints></scan></xrdMeasurement></xrdMeasurements>';

procedure TDiffractionFormatsTest.AnXrdmlScanIsSpreadFromItsStartToItsEnd;
var
    P: TPattern;
begin
    P := XrdmlPattern(XrdmlHead +
        '<positions axis="2Theta" unit="deg"><startPosition>10</startPosition>' +
        '<endPosition>11</endPosition></positions>' +
        '<positions axis="Omega" unit="deg"><startPosition>5</startPosition>' +
        '<endPosition>5.5</endPosition></positions>' +
        '<intensities unit="counts">100 200 300</intensities>' + XrdmlTail);
    AssertEquals(3, Length(P.X));
    AssertEquals('2Theta, not Omega', 10, P.X[0], 1e-12);
    AssertEquals(10.5, P.X[1], 1e-12);
    AssertEquals(11, P.X[2], 1e-12);
    AssertEquals(300, P.Y[2], 1e-12);
end;

procedure TDiffractionFormatsTest.AnXrdmlListOfPositionsIsTakenAsItIs;
var
    P: TPattern;
begin
    P := XrdmlPattern(XrdmlHead +
        '<positions axis="2Theta" unit="deg"><listPositions>10 10.2 10.7' +
        '</listPositions></positions><intensities>1 2 3</intensities>' + XrdmlTail);
    AssertEquals(10.7, P.X[2], 1e-12);
end;

procedure TDiffractionFormatsTest.AnXrdmlScanWithoutAnglesIsRefused;
var
    Message_: string;
begin
    Message_ := '';
    try
        XrdmlPattern(XrdmlHead + '<positions axis="Omega"><startPosition>1' +
            '</startPosition><endPosition>2</endPosition></positions>' +
            '<intensities>1 2</intensities>' + XrdmlTail);
    except
        on E: EPatternFormat do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('no 2Theta', Message_) > 0);
end;

procedure TDiffractionFormatsTest.AnXrdmlWithCountsRatherThanIntensitiesIsRead;
var
    P: TPattern;
begin
    //  Older files (format 1.0) call them counts.
    P := XrdmlPattern(XrdmlHead + '<positions axis="2Theta"><startPosition>1' +
        '</startPosition><endPosition>2</endPosition></positions>' +
        '<counts>5 6</counts>' + XrdmlTail);
    AssertEquals(6, P.Y[1], 1e-12);
end;

procedure TDiffractionFormatsTest.NotXmlIsRefusedSayingSo;
var
    Message_: string;
begin
    Message_ := '';
    try
        XrdmlPattern('this is not XML');
    except
        on E: EPatternFormat do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('not well-formed', Message_) > 0);
end;

procedure TDiffractionFormatsTest.AUxdOfCountsIsSteppedFromItsStart;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['; a comment', '_FILEVERSION=1', '_START=20.000', '_STEPSIZE=0.02',
        '_COUNTS', '  100  110', '  120']);
    try
        P := UxdPattern(L);
        AssertEquals(3, Length(P.X));
        AssertEquals(20.04, P.X[2], 1e-9);
        AssertEquals(120, P.Y[2], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AUxdOfPairsIsReadAsPairs;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['_2THETACOUNTS', '20.00 100', '20.02 110']);
    try
        P := UxdPattern(L);
        AssertEquals(2, Length(P.X));
        AssertEquals(20.02, P.X[1], 1e-12);
        AssertEquals(110, P.Y[1], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AUxdOfCountsWithNoStepIsRefused;
var
    L: TStringList;
    Message_: string;
begin
    L := Lines(['_START=20', '100 110']);
    try
        Message_ := '';
        try
            UxdPattern(L);
        except
            on E: EPatternFormat do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('_STEPSIZE', Message_) > 0);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.OnlyTheFirstUxdRangeIsRead;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['_START=20', '_STEPSIZE=1', '1 2', '_START=50', '_STEPSIZE=1', '7 8']);
    try
        P := UxdPattern(L);
        AssertEquals(2, Length(P.X));
        AssertEquals(21, P.X[1], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasStdBankIsInCentidegrees;
var
    L: TStringList;
    P: TPattern;
begin
    //  Ten fields of eight: a two-digit counter count, then the intensity.
    L := Lines(['Quartz pattern', 'BANK 1 3 1 CONST 1000.0 2.0 0 0 STD',
        '    100.    200.    300.']);
    try
        P := GsasPattern(L);
        AssertEquals(3, Length(P.X));
        AssertEquals('centidegrees to degrees', 10.0, P.X[0], 1e-12);
        AssertEquals(10.04, P.X[2], 1e-12);
        AssertEquals(300, P.Y[2], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasEsdBankSkipsItsEsds;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['Quartz', 'BANK 1 2 1 CONST 1000.0 2.0 0 0 ESD',
        '   100.0    10.0   200.0    14.1']);
    try
        P := GsasPattern(L);
        AssertEquals(2, Length(P.X));
        AssertEquals(200, P.Y[1], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasFxyeBankIsReadPointByPoint;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['Quartz', 'BANK 1 2 2 CONST 1000 2 0 0 FXYE',
        '1000.0 100.0 10.0', '1002.0 110.0 10.5']);
    try
        P := GsasPattern(L);
        AssertEquals(2, Length(P.X));
        AssertEquals(10.02, P.X[1], 1e-12);
        AssertEquals(110, P.Y[1], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasBankNotInAnglesIsRefused;
var
    L: TStringList;
    Message_: string;
begin
    L := Lines(['TOF', 'BANK 1 3 1 TIME_MAP 1 0 0 0 STD', '   100.   200.']);
    try
        Message_ := '';
        try
            GsasPattern(L);
        except
            on E: EPatternFormat do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('TIME_MAP', Message_) > 0);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasFileWithNoBankIsRefused;
var
    L: TStringList;
    Raised: boolean;
begin
    L := Lines(['just a title', '1 2 3']);
    try
        Raised := False;
        try
            GsasPattern(L);
        except
            on E: EPatternFormat do
                Raised := True;
        end;
        AssertTrue(Raised);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.ACifNumberDropsItsUncertainty;
var
    V: double;
begin
    AssertTrue(TryCifNumber('1234(56)', V));
    AssertEquals(1234, V, 1e-12);
    AssertTrue(TryCifNumber('10.5', V));
    AssertFalse('unknown', TryCifNumber('?', V));
    AssertFalse('not applicable', TryCifNumber('.', V));
end;

procedure TDiffractionFormatsTest.CifTokensKeepQuotedStringsAndTextFieldsWhole;
var
    L, T: TStringList;
begin
    L := Lines(['data_x', '_title ''two words''  # a comment', ';', 'a text', 'field',
        ';', '_next 1']);
    T := CifTokens(L);
    try
        AssertEquals('data_x', T[0]);
        AssertEquals('two words', T[2]);
        AssertTrue('the field is one token: ' + T[3], Pos('field', T[3]) > 0);
        AssertEquals('_next', T[4]);
        AssertEquals(6, T.Count);
    finally
        T.Free;
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.APowderCifLoopIsAngleAgainstIntensity;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['data_pattern', 'loop_', '_pd_meas_2theta_scan',
        '_pd_meas_counts_total', '_pd_calc_intensity_total',
        '10.00 100(10) 90', '10.02 110(11) 95']);
    try
        P := PowderCifPattern(L);
        AssertEquals(2, Length(P.X));
        AssertEquals(10.02, P.X[1], 1e-12);
        AssertEquals('the measured counts, not the calculated', 110, P.Y[1], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.APowderCifOfCountsTakesItsAnglesFromItsRange;
var
    L: TStringList;
    P: TPattern;
begin
    L := Lines(['data_pattern', '_pd_meas_2theta_range_min 5.0',
        '_pd_meas_2theta_range_inc 0.5', 'loop_', '_pd_meas_counts_total',
        '10', '20', '30']);
    try
        P := PowderCifPattern(L);
        AssertEquals(3, Length(P.X));
        AssertEquals(6.0, P.X[2], 1e-12);
        AssertEquals(30, P.Y[2], 1e-12);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.ACifOfAStructureIsRefusedSayingWhy;
var
    L: TStringList;
    Message_: string;
begin
    L := Lines(['data_quartz', '_cell_length_a 4.913', 'loop_', '_atom_site_label',
        '_atom_site_fract_x', 'Si1 0.4697', 'O1 0.4135']);
    try
        Message_ := '';
        try
            PowderCifPattern(L);
        except
            on E: EPatternFormat do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('crystal structure', Message_) > 0);
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AnAngleReadTwiceKeepsItsFirstPoint;
var
    Loader: TUxdLoader;
    L: TStringList;
    P: TTitlePointsSet;
begin
    Loader := TUxdLoader.Create(nil);
    L := Lines(['_2THETACOUNTS', '20 1', '20 9', '21 2']);
    try
        Loader.LoadFromLines(L);
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals(2, P.PointsCount);
            AssertEquals(1, P.PointYCoord[0], 1e-12);
        finally
            P.Free;
        end;
    finally
        L.Free;
        Loader.Free;
    end;
end;

function LoadedThrough(ALoader: TDataLoader; const ALines: array of string): longint;
var
    L: TStringList;
    i: longint;
    P: TTitlePointsSet;
begin
    L := TStringList.Create;
    try
        for i := 0 to High(ALines) do
            L.Add(ALines[i]);
        ALoader.LoadFromLines(L);
        P := ALoader.GetPointsSetCopy;
        try
            Result := P.PointsCount;
        finally
            P.Free;
        end;
    finally
        L.Free;
        ALoader.Free;
    end;
end;

function RefusalOf(const AText: string): string;
begin
    Result := '';
    try
        XrdmlPattern(AText);
    except
        on E: EPatternFormat do
            Result := E.Message;
    end;
end;

procedure TDiffractionFormatsTest.EachLoaderMakesItsPatternTheProfile;
begin
    AssertEquals('xrdml', 2, LoadedThrough(TXrdmlLoader.Create(nil),
        [XrdmlHead + '<positions axis="2Theta"><startPosition>1</startPosition>' +
        '<endPosition>2</endPosition></positions><intensities>5 6</intensities>' +
        XrdmlTail]));
    AssertEquals('gsas', 2, LoadedThrough(TGsasLoader.Create(nil),
        ['t', 'BANK 1 2 2 CONST 100 1 0 0 FXYE', '100 5 1', '101 6 1']));
    AssertEquals('cif', 2, LoadedThrough(TPowderCifLoader.Create(nil),
        ['data_p', 'loop_', '_pd_meas_2theta_scan', '_pd_meas_counts_total',
        '1 5', '2 6']));
end;

procedure TDiffractionFormatsTest.AnXrdmlListNotMatchingItsIntensitiesIsRefused;
begin
    AssertTrue(Pos('lists 2 positions and 3 intensities', RefusalOf(XrdmlHead +
        '<positions axis="2Theta"><listPositions>1 2</listPositions></positions>' +
        '<intensities>5 6 7</intensities>' + XrdmlTail)) > 0);
end;

procedure TDiffractionFormatsTest.AnXrdmlScanWithNoStartIsRefused;
begin
    AssertTrue(Pos('no start and end', RefusalOf(XrdmlHead +
        '<positions axis="2Theta"><startPosition>x</startPosition>' +
        '<endPosition>2</endPosition></positions><intensities>5</intensities>' +
        XrdmlTail)) > 0);
end;

procedure TDiffractionFormatsTest.AnXrdmlWithNoScanIsRefused;
begin
    AssertTrue(Pos('no scan data', RefusalOf('<?xml version="1.0"?>' +
        '<xrdMeasurements/>')) > 0);
end;

procedure TDiffractionFormatsTest.AnXrdmlScanWithNoIntensitiesIsRefused;
begin
    AssertTrue(Pos('holds no intensities', RefusalOf(XrdmlHead +
        '<positions axis="2Theta"><startPosition>1</startPosition>' +
        '<endPosition>2</endPosition></positions>' + XrdmlTail)) > 0);
end;

procedure TDiffractionFormatsTest.AnXrdmlNumberThatIsNotOneIsRefused;
begin
    AssertTrue(Pos('is not a number', RefusalOf(XrdmlHead +
        '<positions axis="2Theta"><startPosition>1</startPosition>' +
        '<endPosition>2</endPosition></positions><intensities>5 x</intensities>' +
        XrdmlTail)) > 0);
end;

function GsasRefusal(const ALines: array of string): string;
var
    L: TStringList;
    i: longint;
begin
    Result := '';
    L := TStringList.Create;
    try
        for i := 0 to High(ALines) do
            L.Add(ALines[i]);
        try
            GsasPattern(L);
        except
            on E: EPatternFormat do
                Result := E.Message;
        end;
    finally
        L.Free;
    end;
end;

procedure TDiffractionFormatsTest.AGsasBankLineTooShortIsRefused;
begin
    AssertTrue(Pos('too short', GsasRefusal(['t', 'BANK 1 2'])) > 0);
end;

procedure TDiffractionFormatsTest.AGsasConstBankWithNoStepIsRefused;
begin
    AssertTrue(Pos('no start and step', GsasRefusal(['t',
        'BANK 1 2 1 CONST STD'])) > 0);
end;

procedure TDiffractionFormatsTest.AGsasTypeNotReadIsRefusedByName;
begin
    AssertTrue(Pos('ALT', GsasRefusal(['t',
        'BANK 1 2 1 CONST 100 1 0 0 ALT'])) > 0);
end;

{ ---------------------------------- files ------------------------------------ }

procedure TDiffractionFileTest.ARealXrdmlScanIsReadWhole;
var
    Loader: TXrdmlLoader;
    P: TTitlePointsSet;
begin
    Loader := TXrdmlLoader.Create(nil);
    try
        //  Relative to the running binary, as every fixture path is.
        Loader.LoadDataSet(ExpandFileName(ExtractFilePath(ParamStr(0)) +
            '../Modules/scientific/tests/fixtures/sand-718-44.xrdml'));
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals('every intensity of the scan', 5027, P.PointsCount);
            AssertEquals('from its start', 4.00656514, P.PointXCoord[0], 1e-8);
            AssertEquals('to its end', 69.99936587,
                P.PointXCoord[P.PointsCount - 1], 1e-8);
            AssertEquals('as the instrument counted', 1248, P.PointYCoord[0], 1e-12);
            AssertEquals(127, P.PointYCoord[P.PointsCount - 1], 1e-12);
        finally
            P.Free;
        end;
    finally
        Loader.Free;
    end;
end;

initialization
    RegisterTest('unit', TDiffractionFormatsTest);
    RegisterTest('integration', TDiffractionFileTest);
end.
