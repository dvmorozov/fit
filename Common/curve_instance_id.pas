// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An opaque handle to ONE curve instance.)

WHAT THIS IS. A value issued once, when a curve instance comes into existence,
and carried unchanged for as long as that instance means the same thing to the
user. It answers "WHICH curve" and nothing else. Two Gaussians have two
different ids; a Gaussian and a Lorentzian have unrelated ids. Nothing may be
inferred from the value: it is not ordered, not derived from the curve's
contents, and not parsed for meaning.

WHY IT IS ITS OWN TYPE, and not simply TGuid. The codebase already uses a GUID
for the OPPOSITE purpose. TCurveTypeId (named_points_set.pas) is a per-CLASS
constant - every TGaussPointsSet ever built returns the same GUID, ff4e399c-...
- and it selects what is fitted. Put a
per-instance handle of the same underlying type on the same object without
distinguishing the two and they will be confused, in code and in review. A
distinct type name makes the mistake visible at the point it is made.

WHAT IT REPLACED, because the reason matters. Instances used to be identified by
a hash of their INITIAL parameter values (TFitTask.CalcInitHash), which was a
surrogate for an identity they did not have: the server rebuilds every instance
from the picks on every model edit, so the only key derivable at both ends was
one computed from the seed. That key summed per-parameter hashes, so it was
order-independent and two instances whose values were permutations of each other
collided; and it was coupled to the seed, so MOVING a pick changed the key and
orphaned the values stored under it - which is why moving a fitted pick used to
be refused. An issued handle has neither weakness.

@author(Dmitry Morozov dvmorozov@hotmail.com)
}
unit curve_instance_id;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { An opaque per-instance handle. See the unit comment for why this is a
      distinct type from TCurveTypeId, which identifies a curve CLASS. }
    TCurveInstanceId = type TGuid;

{ The value meaning "no instance". Distinguishable from every issued id. }
function NoCurveInstanceId: TCurveInstanceId;
{ A fresh handle. Every call returns a value distinct from every other. }
function NewCurveInstanceId: TCurveInstanceId;
{ Whether two handles denote the same instance. }
function SameCurveInstanceId(const A, B: TCurveInstanceId): boolean;
{ Whether a handle was ever issued. }
function IsCurveInstanceId(const A: TCurveInstanceId): boolean;
{ The log and storage form: the registry format, braces included. }
function CurveInstanceIdToStr(const A: TCurveInstanceId): string;
{ THE WIRE FORM: the same identifier with no braces.

  A handle addresses a curve in a URL path (/curves/{handle}/points), and braces
  are not valid unencoded characters in a path segment. Rather than encode them
  at every call site and decode them at every route, the form that crosses the
  wire simply has none. TryStrToCurveInstanceId reads both, so the two are
  interchangeable on the way back in. }
function CurveInstanceIdToWire(const A: TCurveInstanceId): string;
{ Reads back what CurveInstanceIdToStr wrote. False - leaving AId unspecified -
  for anything else, so a caller cannot mistake a malformed id for a real one.
  Accepts the braceless form too, because a URL path segment is the main source
  and braces do not belong in one. }
function TryStrToCurveInstanceId(const AText: string;
    out AId: TCurveInstanceId): boolean;

implementation

function NoCurveInstanceId: TCurveInstanceId;
begin
    FillChar(Result, SizeOf(Result), 0);
end;

function NewCurveInstanceId: TCurveInstanceId;
var
    G: TGuid;
begin
    //  CreateGUID is the platform's own generator. It can fail, and a handle
    //  that was silently not issued would collide with every other one that was
    //  not issued - which is exactly the class of quiet degradation this
    //  identity exists to remove. So it raises.
    if CreateGUID(G) <> 0 then
        raise Exception.Create(
            'A curve instance identifier could not be issued.');
    Result := TCurveInstanceId(G);
end;

function SameCurveInstanceId(const A, B: TCurveInstanceId): boolean;
begin
    Result := IsEqualGUID(TGuid(A), TGuid(B));
end;

function IsCurveInstanceId(const A: TCurveInstanceId): boolean;
begin
    Result := not SameCurveInstanceId(A, NoCurveInstanceId);
end;

function CurveInstanceIdToStr(const A: TCurveInstanceId): string;
begin
    Result := GUIDToString(TGuid(A));
end;

function CurveInstanceIdToWire(const A: TCurveInstanceId): string;
begin
    Result := CurveInstanceIdToStr(A);
    //  GUIDToString always brackets, so this is a fixed trim rather than a
    //  search - but it is asserted, because a silently unbracketed form would
    //  put braces in a URL and be found only as a failing request.
    if (Length(Result) = 38) and (Result[1] = '{') and (Result[38] = '}') then
        Result := Copy(Result, 2, 36);
end;

function TryStrToCurveInstanceId(const AText: string;
    out AId: TCurveInstanceId): boolean;
var
    S: string;
begin
    AId := NoCurveInstanceId;
    Result := False;
    S := Trim(AText);
    if S = '' then
        Exit;
    //  A URL path segment carries no braces; the stored and logged form does.
    //  Both are the same identifier, so both are read.
    if (S[1] <> '{') and (Length(S) = 36) then
        S := '{' + S + '}';
    try
        AId := TCurveInstanceId(StringToGUID(S));
    except
        //  StringToGUID raises on anything malformed. That is an answer, not a
        //  fault: the caller asked whether this text IS an id.
        on E: EConvertError do
        begin
            AId := NoCurveInstanceId;
            Exit;
        end;
    end;
    Result := True;
end;

end.
