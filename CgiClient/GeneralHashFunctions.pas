{
@abstract(General purpose hash function algorithms library.)

@author(Arash Partow - 2002)
}
unit GeneralHashfunctions;

interface

   (**************************************************************************)
   (*                                                                        *)
   (*          General Purpose Hash Function Algorithms Library              *)
   (*                                                                        *)
   (* Author: Arash Partow - 2002                                            *)
   (* URL: http://www.partow.net                                             *)
   (* URL: http://www.partow.net/programming/hashfunctions/index.html        *)
   (*                                                                        *)
   (* Copyright notice:                                                      *)
   (* Free use of the General Purpose Hash Function Algorithms Library is    *)
   (* permitted under the guidelines and in accordance with the most current *)
   (* version of the Common Public License.                                  *)
   (* http://www.opensource.org/licenses/cpl.php                             *)
   (*                                                                        *)
   (**************************************************************************)

{ Because the module is from another author, messages are suppressed. }
{$warnings off}
{$hints off}
{$notes off}

type
    THashfunction = function(const key: string): cardinal;


function RSHash(const Str: string): cardinal;
function JSHash(const Str: string): cardinal;
function PJWHash(const Str: string): cardinal;
function ELFHash(const Str: string): cardinal;
function BKDRHash(const Str: string): cardinal;
function SDBMHash(const Str: string): cardinal;
function DJBHash(const Str: string): cardinal;
function DEKHash(const Str: string): cardinal;
//function HRHash   (const Str : String) : Cardinal;
function FNVHash(const Str: string): cardinal;
function APHash(const Str: string): cardinal;

implementation

function RSHash(const Str: string): cardinal;
const
    b = 378551;
var
    a: cardinal;
    i: integer;
begin
    a      := 63689;
    Result := 0;
    for i := 1 to Length(Str) do
    begin
        Result := Result * a + Ord(Str[i]);
        a      := a * b;
    end;
end;

(* End Of RS Hash function *)


function JSHash(const Str: string): cardinal;
var
    i: integer;
begin
    Result := 1315423911;
    for i := 1 to Length(Str) do
        Result := Result xor ((Result shl 5) + Ord(Str[i]) + (Result shr 2));
end;

(* End Of JS Hash function *)


function PJWHash(const Str: string): cardinal;
const
    BitsInCardinal = Sizeof(cardinal) * 8;
const
    ThreeQuarters = (BitsInCardinal * 3) div 4;
const
    OneEighth = BitsInCardinal div 8;
const
    HighBits: cardinal = (not cardinal(0)) shl (BitsInCardinal - OneEighth);
var
    i:    cardinal;
    Test: cardinal;
begin
    Result := 0;
    for i := 1 to Length(Str) do
    begin
        Result := (Result shl OneEighth) + Ord(Str[i]);
        Test   := Result and HighBits;
        if (Test <> 0) then
            Result := (Result xor (Test shr ThreeQuarters)) and (not HighBits);
    end;
end;

(* End Of P. J. Weinberger Hash function *)


function ELFHash(const Str: string): cardinal;
var
    i: cardinal;
    x: cardinal;
begin
    Result := 0;
    for i := 1 to Length(Str) do
    begin
        Result := (Result shl 4) + Ord(Str[i]);
        x      := Result and $F0000000;
        if (x <> 0) then
            Result := Result xor (x shr 24);
        Result     := Result and (not x);
    end;
end;

(* End Of ELF Hash function *)


function BKDRHash(const Str: string): cardinal;
const
    Seed = 131; (* 31 131 1313 13131 131313 etc... *)
var
    i: cardinal;
begin
    Result := 0;
    for i := 1 to Length(Str) do
        Result := (Result * Seed) + Ord(Str[i]);
end;

(* End Of BKDR Hash function *)


function SDBMHash(const Str: string): cardinal;
var
    i: cardinal;
begin
    Result := 0;
    for i := 1 to Length(Str) do
        Result := Ord(str[i]) + (Result shl 6) + (Result shl 16) - Result;
end;

(* End Of SDBM Hash function *)


function DJBHash(const Str: string): cardinal;
var
    i: cardinal;
begin
    Result := 5381;
    for i := 1 to Length(Str) do
        Result := ((Result shl 5) + Result) + Ord(Str[i]);
end;

(* End Of DJB Hash function *)


function DEKHash(const Str: string): cardinal;
var
    i: cardinal;
begin
    Result := Length(Str);
    for i := 1 to Length(Str) do
        Result := ((Result shr 5) xor (Result shl 27)) xor Ord(Str[i]);
end;

(* End Of DEK Hash function *)

(*
function BPHash(const Str : String) : Cardinal;
var
  i : Cardinal;
begin
  Result := 0;
  for i := 1 to Length(Str) do
  begin
    Result := hash shl 7 xor Ord(Str[i]);
  end;
end;
*)
(* End Of BP Hash function *)


function FNVHash(const Str: string): cardinal;
const
    FNVPrime = $811C9DC5;
var
    i: cardinal;
begin
    Result := 0;
    for i := 1 to Length(Str) do
    begin
        Result := Result * FNVPrime;
        Result := Result xor Ord(Str[i]);
    end;
end;

(* End Of FNV Hash function *)


function APHash(const Str: string): cardinal;
var
    i: cardinal;
begin
    Result := $AAAAAAAA;
    for i := 1 to Length(Str) do
        if ((i - 1) and 1) = 0 then
            Result := Result xor ((Result shl 7) xor Ord(Str[i]) * (Result shr 3))
        else
            Result := Result xor
                (not ((Result shl 11) + Ord(Str[i]) xor (Result shr 5)));
end;

(* End Of AP Hash function *)


end.
