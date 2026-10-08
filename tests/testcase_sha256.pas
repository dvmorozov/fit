// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(SHA-256, against the published test vectors.)

An update is installed only when the file downloaded has the digest the feed
names, so a digest that is wrong for some lengths would refuse a good installer
or pass a damaged one. The vectors are FIPS 180-2's and the NIST examples: the
empty message, one block, two blocks, and a million bytes - which crosses every
padding boundary a streamed file can.
}
unit testcase_sha256;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, sha256_digest;

type
    TSha256Test = class(TTestCase)
    published
        procedure TheEmptyMessage;
        procedure OneBlock;
        procedure TwoBlocks;
        procedure AMillionBytes;
        procedure EveryLengthAroundABlockAgreesWithItsStream;
    end;

    { Integration: it writes a file. }
    TSha256FileTest = class(TTestCase)
    published
        procedure AFileIsHashedAsItsBytes;
    end;

implementation

procedure TSha256Test.TheEmptyMessage;
begin
    AssertEquals('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        Sha256Hex(''));
end;

procedure TSha256Test.OneBlock;
begin
    AssertEquals('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
        Sha256Hex('abc'));
end;

procedure TSha256Test.TwoBlocks;
begin
    AssertEquals('248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1',
        Sha256Hex('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'));
end;

procedure TSha256Test.AMillionBytes;
begin
    AssertEquals('cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0',
        Sha256Hex(StringOfChar('a', 1000000)));
end;

{ A file is read in chunks that do not fall on block boundaries; each length
  around one and two blocks must hash the same streamed as whole. }
procedure TSha256Test.EveryLengthAroundABlockAgreesWithItsStream;
var
    n: integer;
    Text: string;
    S: TStringStream;
begin
    for n := 50 to 140 do
    begin
        Text := StringOfChar(Chr(Ord('a') + n mod 26), n);
        S := TStringStream.Create(Text);
        try
            AssertEquals('length ' + IntToStr(n), Sha256Hex(Text),
                Sha256HexOfStream(S, 7));
        finally
            S.Free;
        end;
    end;
end;

procedure TSha256FileTest.AFileIsHashedAsItsBytes;
var
    Path: string;
    F: TFileStream;
begin
    Path := GetTempFileName(GetTempDir(False), 'fit-sha');
    F := TFileStream.Create(Path, fmCreate);
    try
        F.WriteBuffer(PChar('abc')^, 3);
    finally
        F.Free;
    end;
    try
        AssertEquals(Sha256Hex('abc'), Sha256HexOfFile(Path));
    finally
        DeleteFile(Path);
    end;
end;

initialization
    RegisterTest('unit', TSha256Test);
    RegisterTest('integration', TSha256FileTest);
end.
