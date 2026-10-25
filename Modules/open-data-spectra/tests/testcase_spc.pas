// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Galactic SPC spectra: the header's rules over bytes written here, and
a real FTIR file from a public dataset read whole.)
}
unit testcase_spc;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, spc_loader, title_points_set;

type
    TSpcTest = class(TTestCase)
    private
        function Spc(AFlags, AVersion, AExponent: byte; APoints: longint;
            AFirst, ALast: double): TMemoryStream;
    published
        procedure AnEvenlySpacedSpectrumRunsFromFirstToLast;
        procedure IntegersAreScaledByTheExponent;
        procedure FloatsAreTakenAsTheyAre;
        procedure ShortIntegersAreScaledToo;
        procedure AnXArrayIsTakenAsItIs;
        procedure TheOldFormatIsRefusedByName;
        procedure TheBigEndianFormatIsRefusedByName;
        procedure AFileThatIsNoSpcIsRefused;
        procedure SpectraWithTheirOwnXAreRefused;
        procedure AShortFileIsRefused;
        procedure ItIsNotReadFromLines;
        procedure TheLoaderMakesTheSpectrumTheProfile;
        procedure AnEndCutShortIsRefused;
    end;

    TSpcFileTest = class(TTestCase)
    published
        procedure ARealFtirSpectrumIsReadWhole;
    end;

implementation

function TSpcTest.Spc(AFlags, AVersion, AExponent: byte; APoints: longint;
    AFirst, ALast: double): TMemoryStream;
var
    Header: array[0..511] of byte;
begin
    FillChar(Header, SizeOf(Header), 0);
    Header[0] := AFlags;
    Header[1] := AVersion;
    Header[3] := AExponent;
    PLongint(@Header[4])^ := NtoLE(APoints);
    PDouble(@Header[8])^ := AFirst;
    PDouble(@Header[16])^ := ALast;
    PLongint(@Header[24])^ := 1;
    Result := TMemoryStream.Create;
    Result.WriteBuffer(Header, SizeOf(Header));
end;

procedure WriteSubheader(AData: TStream);
var
    Sub: array[0..31] of byte;
begin
    FillChar(Sub, SizeOf(Sub), 0);
    AData.WriteBuffer(Sub, SizeOf(Sub));
end;

procedure WriteLong(AData: TStream; AValue: longint);
begin
    AValue := NtoLE(AValue);
    AData.WriteBuffer(AValue, 4);
end;

function MessageOf(AData: TStream): string;
begin
    Result := '';
    try
        SpcSpectrum(AData);
    except
        on E: ESpcFormat do
            Result := E.Message;
    end;
end;

procedure TSpcTest.AnEvenlySpacedSpectrumRunsFromFirstToLast;
var
    Data: TMemoryStream;
    S: TSpcSpectrum;
begin
    Data := Spc(0, $4B, 0, 3, 4000, 3000);
    try
        WriteSubheader(Data);
        WriteLong(Data, 1);
        WriteLong(Data, 2);
        WriteLong(Data, 3);
        S := SpcSpectrum(Data);
        AssertEquals(3, Length(S.X));
        AssertEquals(4000, S.X[0], 1e-9);
        AssertEquals(3500, S.X[1], 1e-9);
        AssertEquals(3000, S.X[2], 1e-9);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.IntegersAreScaledByTheExponent;
var
    Data: TMemoryStream;
    S: TSpcSpectrum;
begin
    //  y = integer * 2^(exponent - 32): 2^30 with exponent 3 is 2^1.
    Data := Spc(0, $4B, 3, 1, 1, 1);
    try
        WriteSubheader(Data);
        WriteLong(Data, 1 shl 30);
        S := SpcSpectrum(Data);
        AssertEquals(2, S.Y[0], 1e-12);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.FloatsAreTakenAsTheyAre;
var
    Data: TMemoryStream;
    S: TSpcSpectrum;
    F: single;
begin
    Data := Spc(0, $4B, $80, 1, 1, 1);
    try
        WriteSubheader(Data);
        F := 0.25;
        Data.WriteBuffer(F, 4);
        S := SpcSpectrum(Data);
        AssertEquals(0.25, S.Y[0], 1e-12);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.ShortIntegersAreScaledToo;
var
    Data: TMemoryStream;
    S: TSpcSpectrum;
    V: smallint;
begin
    //  y = integer * 2^(exponent - 16).
    Data := Spc(SpcFlagShortY, $4B, 16, 1, 1, 1);
    try
        WriteSubheader(Data);
        V := NtoLE(smallint(7));
        Data.WriteBuffer(V, 2);
        S := SpcSpectrum(Data);
        AssertEquals(7, S.Y[0], 1e-12);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.AnXArrayIsTakenAsItIs;
var
    Data: TMemoryStream;
    S: TSpcSpectrum;
    X: single;
begin
    Data := Spc(SpcFlagXValues, $4B, 32, 2, 0, 0);
    try
        X := 10;
        Data.WriteBuffer(X, 4);
        X := 25;
        Data.WriteBuffer(X, 4);
        WriteSubheader(Data);
        WriteLong(Data, 5);
        WriteLong(Data, 6);
        S := SpcSpectrum(Data);
        AssertEquals(25, S.X[1], 1e-6);
        AssertEquals('exponent 32 is the integer itself', 6, S.Y[1], 1e-12);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.TheOldFormatIsRefusedByName;
var
    Data: TMemoryStream;
begin
    Data := Spc(0, $4D, 0, 1, 1, 1);
    try
        AssertTrue(Pos('old format', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.TheBigEndianFormatIsRefusedByName;
var
    Data: TMemoryStream;
begin
    Data := Spc(0, $4C, 0, 1, 1, 1);
    try
        AssertTrue(Pos('big-endian', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.AFileThatIsNoSpcIsRefused;
var
    Data: TMemoryStream;
begin
    Data := Spc(0, $12, 0, 1, 1, 1);
    try
        AssertTrue(MessageOf(Data), Pos('not an SPC', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.SpectraWithTheirOwnXAreRefused;
var
    Data: TMemoryStream;
begin
    Data := Spc(SpcFlagXYXYs or SpcFlagMulti, $4B, 0, 1, 1, 1);
    try
        AssertTrue(Pos('TXYXYS', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.AShortFileIsRefused;
var
    Data: TMemoryStream;
begin
    Data := TMemoryStream.Create;
    try
        Data.WriteBuffer(PChar('short')^, 5);
        AssertTrue(Pos('too short', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcTest.ItIsNotReadFromLines;
var
    Loader: TSpcLoader;
    Lines: TStringList;
    Raised: boolean;
begin
    Loader := TSpcLoader.Create(nil);
    Lines := TStringList.Create;
    try
        Lines.Add('1 2');
        Raised := False;
        try
            Loader.LoadFromLines(Lines);
        except
            on E: ESpcFormat do
                Raised := True;
        end;
        AssertTrue(Raised);
    finally
        Lines.Free;
        Loader.Free;
    end;
end;

procedure TSpcTest.TheLoaderMakesTheSpectrumTheProfile;
var
    Data: TMemoryStream;
    Loader: TSpcLoader;
    P: TTitlePointsSet;
begin
    Data := Spc(0, $4B, 32, 2, 100, 200);
    Loader := TSpcLoader.Create(nil);
    try
        WriteSubheader(Data);
        WriteLong(Data, 7);
        WriteLong(Data, 9);
        Loader.ReadSpectrum(Data);
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals(2, P.PointsCount);
            AssertEquals(200, P.PointXCoord[1], 1e-9);
            AssertEquals(9, P.PointYCoord[1], 1e-12);
        finally
            P.Free;
        end;
    finally
        Loader.Free;
        Data.Free;
    end;
end;

procedure TSpcTest.AnEndCutShortIsRefused;
var
    Data: TMemoryStream;
begin
    //  Two points promised, one written; and an x array cut short.
    Data := Spc(0, $4B, 0, 2, 1, 2);
    try
        WriteSubheader(Data);
        WriteLong(Data, 1);
        AssertTrue(Pos('ends inside its spectrum', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
    Data := Spc(SpcFlagXValues, $4B, 0, 2, 1, 2);
    try
        AssertTrue(Pos('ends inside its x values', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
    Data := Spc(0, $4B, 0, 0, 1, 2);
    try
        AssertTrue('no points cannot be right',
            Pos('cannot be right', MessageOf(Data)) > 0);
    finally
        Data.Free;
    end;
end;

procedure TSpcFileTest.ARealFtirSpectrumIsReadWhole;
var
    Loader: TSpcLoader;
    P: TTitlePointsSet;
begin
    Loader := TSpcLoader.Create(nil);
    try
        //  Relative to the running binary, as every fixture path is.
        Loader.LoadDataSet(ExpandFileName(ExtractFilePath(ParamStr(0)) +
            '../Modules/open-data-spectra/tests/fixtures/tape-backing-ftir.spc'));
        P := Loader.GetPointsSetCopy;
        try
            AssertEquals('every point of the spectrum', 6950, P.PointsCount);
            AssertEquals('from its first wavenumber', 4000.1884765625,
                P.PointXCoord[0], 1e-9);
            AssertEquals('to its last', 649.9039916992188,
                P.PointXCoord[P.PointsCount - 1], 1e-9);
            AssertEquals('scaled as the header says', 0.007262601982802153,
                P.PointYCoord[0], 1e-15);
            AssertEquals(0.11706136167049408, P.PointYCoord[P.PointsCount - 1],
                1e-15);
        finally
            P.Free;
        end;
    finally
        Loader.Free;
    end;
end;

initialization
    RegisterTest('unit', TSpcTest);
    RegisterTest('integration', TSpcFileTest);
end.
