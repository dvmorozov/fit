// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(SHA-256, for checking that a downloaded file is the one published.)

WHY IT IS WRITTEN HERE. Free Pascal 3.2.2 ships MD5 and SHA-1 only (fpsha256
arrived later), an update must be checked against the SHA-256 its feed names,
and asking a system tool would be a different tool on each platform - which
non-negotiable 8 exists to prevent. The algorithm is FIPS 180-4's, written out
plainly; tests/testcase_sha256.pas holds it to the published vectors.

A digest is a check that the file arrived whole and unaltered from where the
feed points - not a signature: whoever can change the feed can change the
digest with it, which is why the feed itself is only ever read over https.

Copyright (C) Dmitry Morozov
}
unit sha256_digest;

{$mode objfpc}{$H+}
{$Q-}{$R-}

interface

uses
    Classes, SysUtils;

{ The digest of AData's bytes, lower-case hex. }
function Sha256Hex(const AData: string): string;
{ The digest of AStream from its current position to its end, read ACHUNK
  bytes at a time. }
function Sha256HexOfStream(AStream: TStream; AChunk: longint = 65536): string;
{ The digest of the file at APath. }
function Sha256HexOfFile(const APath: string): string;

implementation

type
    TSha256State = record
        H: array[0..7] of longword;
        Block: array[0..63] of byte;
        BlockLen: integer;
        TotalLen: qword;
    end;

const
    K: array[0..63] of longword = (
        $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
        $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
        $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
        $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
        $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
        $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
        $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
        $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

function Ror(X: longword; N: integer): longword; inline;
begin
    Result := (X shr N) or (X shl (32 - N));
end;

procedure Init(out S: TSha256State);
begin
    S := Default(TSha256State);
    S.H[0] := $6a09e667; S.H[1] := $bb67ae85; S.H[2] := $3c6ef372; S.H[3] := $a54ff53a;
    S.H[4] := $510e527f; S.H[5] := $9b05688c; S.H[6] := $1f83d9ab; S.H[7] := $5be0cd19;
end;

procedure Compress(var S: TSha256State);
var
    W: array[0..63] of longword;
    a, b, c, d, e, f, g, h, T1, T2: longword;
    i: integer;
begin
    for i := 0 to 15 do
        W[i] := (longword(S.Block[4 * i]) shl 24) or (longword(S.Block[4 * i + 1]) shl 16) or
            (longword(S.Block[4 * i + 2]) shl 8) or longword(S.Block[4 * i + 3]);
    for i := 16 to 63 do
        W[i] := (Ror(W[i - 2], 17) xor Ror(W[i - 2], 19) xor (W[i - 2] shr 10)) + W[i - 7] +
            (Ror(W[i - 15], 7) xor Ror(W[i - 15], 18) xor (W[i - 15] shr 3)) + W[i - 16];
    a := S.H[0]; b := S.H[1]; c := S.H[2]; d := S.H[3];
    e := S.H[4]; f := S.H[5]; g := S.H[6]; h := S.H[7];
    for i := 0 to 63 do
    begin
        T1 := h + (Ror(e, 6) xor Ror(e, 11) xor Ror(e, 25)) + ((e and f) xor ((not e) and g)) +
            K[i] + W[i];
        T2 := (Ror(a, 2) xor Ror(a, 13) xor Ror(a, 22)) + ((a and b) xor (a and c) xor (b and c));
        h := g; g := f; f := e; e := d + T1;
        d := c; c := b; b := a; a := T1 + T2;
    end;
    Inc(S.H[0], a); Inc(S.H[1], b); Inc(S.H[2], c); Inc(S.H[3], d);
    Inc(S.H[4], e); Inc(S.H[5], f); Inc(S.H[6], g); Inc(S.H[7], h);
end;

procedure Update(var S: TSha256State; const ABuf; ALen: longint);
var
    P: PByte;
    i: longint;
begin
    P := @ABuf;
    for i := 0 to ALen - 1 do
    begin
        S.Block[S.BlockLen] := P[i];
        Inc(S.BlockLen);
        if S.BlockLen = 64 then
        begin
            Compress(S);
            S.BlockLen := 0;
        end;
    end;
    Inc(S.TotalLen, ALen);
end;

function Finish(var S: TSha256State): string;
var
    Bits: qword;
    Pad: byte;
    Zero: byte;
    LenBytes: array[0..7] of byte;
    i: integer;
begin
    Bits := S.TotalLen * 8;
    Pad := $80;
    Zero := 0;
    Update(S, Pad, 1);
    while S.BlockLen <> 56 do
        Update(S, Zero, 1);
    for i := 0 to 7 do
        LenBytes[i] := byte(Bits shr (56 - 8 * i));
    Update(S, LenBytes, 8);
    Result := '';
    for i := 0 to 7 do
        Result := Result + LowerCase(IntToHex(S.H[i], 8));
end;

function Sha256Hex(const AData: string): string;
var
    S: TSha256State;
begin
    Init(S);
    if Length(AData) > 0 then
        Update(S, AData[1], Length(AData));
    Result := Finish(S);
end;

function Sha256HexOfStream(AStream: TStream; AChunk: longint): string;
var
    S: TSha256State;
    Buf: array of byte;
    N: longint;
begin
    Init(S);
    SetLength(Buf, AChunk);
    repeat
        N := AStream.Read(Buf[0], AChunk);
        if N > 0 then
            Update(S, Buf[0], N);
    until N <= 0;
    Result := Finish(S);
end;

function Sha256HexOfFile(const APath: string): string;
var
    F: TFileStream;
begin
    F := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
    try
        Result := Sha256HexOfStream(F);
    finally
        F.Free;
    end;
end;

end.
