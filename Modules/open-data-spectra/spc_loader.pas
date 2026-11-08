// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Galactic SPC: the binary file infrared, Raman and UV-visible
spectrometers write, read as an ordinary profile.)

WHY THIS MODULE. SPC is a spectroscopy format, as JCAMP-DX is - Thermo's
GRAMS/OMNIC software writes it, and spectra are deposited in it (Zenodo holds
some thirty datasets of .spc files, counted 2026-10-20). A field's formats
belong to the field's module.

WHAT IS READ, by the published layout (Galactic Industries, "SPC file format",
the new-format header of 512 bytes, least significant byte first):

  * the first spectrum: its x values evenly from ffirst to flast, or the X
    array the file carries (TXVALS); its y values as 32-bit integers scaled by
    2^(exponent-32), 16-bit ones scaled by 2^(exponent-16) (TSPREC), or IEEE
    floats when the exponent is 0x80;
  * nothing else: the axis units, the memo and the log block are not kept.

WHAT IS REFUSED BY NAME, rather than read wrongly: the old format (0x4D) and
the big-endian one (0x4C), whose headers are laid out otherwise, and a file
whose spectra each carry their own x (TXYXYS), whose layout is a directory of
offsets. Each says what it is.

THE DECISION IS A FUNCTION OVER BYTES (SpcSpectrum), so every rule is reachable
from a test; the loader only reads the file.
}
unit spc_loader;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_loader;

const
    SpcExtensions = '.SPC';
    SpcFormatName = 'Galactic SPC spectrum';

    { Flags of ftflgs that change what follows the header. }
    SpcFlagShortY = $01;     //  TSPREC: 16-bit y
    SpcFlagMulti = $04;      //  TMULTI: several spectra
    SpcFlagXYXYs = $40;      //  TXYXYS: each spectrum its own x
    SpcFlagXValues = $80;    //  TXVALS: an x array after the header

type
    TSpcValues = array of double;

    TSpcSpectrum = record
        X: TSpcValues;
        Y: TSpcValues;
    end;

    ESpcFormat = class(EInvalidFileFormat);

    TSpcLoader = class(TDataLoader)
    protected
        { Binary: the file's bytes, handed to ReadSpectrum. }
        procedure LoadDataSetActually; override;
        procedure ParseLines(ALines: TStrings); override;
    public
        { The spectrum in AData becomes the profile - what reading the file
          does once it has the bytes, and what a test drives with bytes it
          wrote. }
        procedure ReadSpectrum(AData: TStream);
    end;

{ The first spectrum of the SPC file in AData. Raises ESpcFormat, in words, for
  one this reader does not read. }
function SpcSpectrum(AData: TStream): TSpcSpectrum;

implementation

uses
    Math;

const
    HeaderSize = 512;
    SubheaderSize = 32;
    NewFormat = $4B;
    BigEndianFormat = $4C;
    OldFormat = $4D;
    FloatExponent = $80;

function SpcSpectrum(AData: TStream): TSpcSpectrum;
var
    Header: array[0..HeaderSize - 1] of byte;
    Sub: array[0..SubheaderSize - 1] of byte;
    Flags, Version, Exponent: byte;
    Points, i: longint;
    First, Last: double;
    XFloat: single;
    YLong: longint;
    YShort: smallint;
    YFloat: single;
begin
    Result := Default(TSpcSpectrum);
    AData.Position := 0;
    if AData.Read(Header, HeaderSize) <> HeaderSize then
        raise ESpcFormat.Create('The file is too short to be an SPC spectrum.');
    Flags := Header[0];
    Version := Header[1];
    if Version = OldFormat then
        raise ESpcFormat.Create('The file is an SPC of the old format (4Dh), ' +
            'which this reader does not read; save it again from the ' +
            'spectrometer''s software, or export it as JCAMP-DX.');
    if Version = BigEndianFormat then
        raise ESpcFormat.Create('The file is a big-endian SPC (4Ch), which ' +
            'this reader does not read; export it as JCAMP-DX.');
    if Version <> NewFormat then
        raise ESpcFormat.Create(Format('The file is not an SPC spectrum: its ' +
            'version byte is %.2xh, where an SPC has 4Bh.', [Version]));
    if (Flags and SpcFlagXYXYs) <> 0 then
        raise ESpcFormat.Create('The SPC file holds spectra that each have ' +
            'their own x values (TXYXYS), which this reader does not read.');
    Exponent := Header[3];
    Points := LEtoN(PLongint(@Header[4])^);
    First := PDouble(@Header[8])^;
    Last := PDouble(@Header[16])^;
    if (Points <= 0) or (Points > 50 * 1000 * 1000) then
        raise ESpcFormat.Create(Format('The SPC header says the spectrum has ' +
            '%d points, which cannot be right.', [Points]));

    SetLength(Result.X, Points);
    SetLength(Result.Y, Points);
    if (Flags and SpcFlagXValues) <> 0 then
    begin
        //  THE X ARRAY, when the spectrum is not evenly spaced.
        for i := 0 to Points - 1 do
        begin
            if AData.Read(XFloat, 4) <> 4 then
                raise ESpcFormat.Create('The SPC file ends inside its x values.');
            Result.X[i] := XFloat;
        end;
    end
    else
        for i := 0 to Points - 1 do
            if Points > 1 then
                Result.X[i] := First + (Last - First) * i / (Points - 1)
            else
                Result.X[i] := First;

    //  THE FIRST SPECTRUM'S SUBHEADER: in a file of several, its exponent is
    //  the one its values are scaled by.
    if AData.Read(Sub, SubheaderSize) <> SubheaderSize then
        raise ESpcFormat.Create('The SPC file ends before its first spectrum.');
    if ((Flags and SpcFlagMulti) <> 0) and (Sub[1] <> 0) then
        Exponent := Sub[1];

    for i := 0 to Points - 1 do
    begin
        if Exponent = FloatExponent then
        begin
            if AData.Read(YFloat, 4) <> 4 then
                raise ESpcFormat.Create('The SPC file ends inside its spectrum.');
            Result.Y[i] := YFloat;
        end
        else if (Flags and SpcFlagShortY) <> 0 then
        begin
            if AData.Read(YShort, 2) <> 2 then
                raise ESpcFormat.Create('The SPC file ends inside its spectrum.');
            Result.Y[i] := LEtoN(YShort) * Power(2, shortint(Exponent) - 16);
        end
        else
        begin
            if AData.Read(YLong, 4) <> 4 then
                raise ESpcFormat.Create('The SPC file ends inside its spectrum.');
            Result.Y[i] := LEtoN(YLong) * Power(2, shortint(Exponent) - 32);
        end;
    end;
end;

procedure TSpcLoader.LoadDataSetActually;
var
    Data: TFileStream;
begin
    Data := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
        ReadSpectrum(Data);
    finally
        Data.Free;
    end;
end;

procedure TSpcLoader.ReadSpectrum(AData: TStream);
var
    Spectrum: TSpcSpectrum;
    i: longint;
begin
    CreatePointsSet;
    Spectrum := SpcSpectrum(AData);
    FPointsSet.Clear;
    for i := 0 to High(Spectrum.X) do
        if FPointsSet.IndexOfValueX(Spectrum.X[i]) = -1 then
            FPointsSet.AddNewPoint(Spectrum.X[i], Spectrum.Y[i]);
end;

procedure TSpcLoader.ParseLines(ALines: TStrings);
begin
    //  A BINARY FORMAT HAS NO LINES: what a test or a caller holding text
    //  would hand over is not an SPC file, and saying so beats parsing it.
    raise ESpcFormat.Create('An SPC spectrum is a binary file and cannot be ' +
        'read from lines of text.');
end;

end.
