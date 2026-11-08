// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The samples of a fit interval that no curve covers, as stretches of x.)

WHY THIS EXISTS. A compactly supported curve is exactly zero outside its support
(TCurvePointsSet.SupportMin/SupportMax). A sample inside a fit interval that no
curve covers is therefore modelled as 0, and it still counts in the R-factor -
it must, or the figure would depend on how the model happens to be placed. One
such sample at the edge of a price series was 68 times the residual of every
other sample together, and the only trace of it was a line in a server log.

So the framework names those samples: which stretches, how many, and what share
of the squared difference they make. It does NOT drop them from the objective -
the fit must never quietly change what it measures (AGENTS, "nothing degrades
silently").

A STRETCH IS IN X, not in sample indices, because the indices are of whichever
profile a task was cut from and mean nothing on the wire or in a project file.
A stretch is a maximal run of consecutive uncovered samples of one interval, so
a sample of that interval lies in it exactly when its x lies between the ends.

ONE JSON SHAPE for the REST reply, the client's read and the project file - the
three places statistics are written - so the three cannot drift apart.
}
unit sample_coverage;

{$mode objfpc}{$H+}

interface

uses
    fpjson, Math;

type
    { One run of consecutive samples no curve covers: the x of its first and
      last sample, and how many samples it holds. }
    TSampleRange = record
        FromX, ToX: double;
        Count: longint;
    end;
    TSampleRanges = array of TSampleRange;

{ The runs of samples for which ACovered is False, in order. AX and ACovered
  are parallel: the x of each sample, and whether any curve covers it. }
function UncoveredRanges(const AX: array of double;
    const ACovered: array of boolean): TSampleRanges;
{ How many samples the stretches hold together. }
function UncoveredSampleCount(const ARanges: TSampleRanges): longint;
{ Whether a sample at AX is one of the stretches'. }
function InRanges(const ARanges: TSampleRanges; AX: double): boolean;
{ Appends AFrom to ATo, as one fit interval's stretches follow another's. }
procedure AppendRanges(var ATo: TSampleRanges; const AFrom: TSampleRanges);

{ Writes the stretches into a statistics object - and NOTHING when there are
  none, so the reply for every model that covers its intervals stays exactly
  what it was. AShare is their part of the squared difference, 0..1. }
procedure AddCoverageJson(AObject: TJSONObject; const ARanges: TSampleRanges;
    AShare: double);
{ Reads what AddCoverageJson wrote; absent fields are no stretch and no share,
  which is how a reply or a project file from before this reads. }
procedure ReadCoverageJson(AObject: TJSONObject; out ARanges: TSampleRanges;
    out AShare: double);

implementation

function UncoveredRanges(const AX: array of double;
    const ACovered: array of boolean): TSampleRanges;
var
    i, n: longint;
    Open: boolean;
begin
    Result := nil;
    n := 0;
    Open := False;
    for i := 0 to Min(High(AX), High(ACovered)) do
    begin
        if ACovered[i] then
        begin
            Open := False;
            Continue;
        end;
        if not Open then
        begin
            SetLength(Result, n + 1);
            Result[n].FromX := AX[i];
            Result[n].Count := 0;
            Inc(n);
            Open := True;
        end;
        Result[n - 1].ToX := AX[i];
        Inc(Result[n - 1].Count);
    end;
end;

function UncoveredSampleCount(const ARanges: TSampleRanges): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(ARanges) do
        Inc(Result, ARanges[i].Count);
end;

function InRanges(const ARanges: TSampleRanges; AX: double): boolean;
var
    i: longint;
begin
    for i := 0 to High(ARanges) do
        if (AX >= ARanges[i].FromX) and (AX <= ARanges[i].ToX) then
            Exit(True);
    Result := False;
end;

procedure AppendRanges(var ATo: TSampleRanges; const AFrom: TSampleRanges);
var
    i, n: longint;
begin
    n := Length(ATo);
    SetLength(ATo, n + Length(AFrom));
    for i := 0 to High(AFrom) do
        ATo[n + i] := AFrom[i];
end;

procedure AddCoverageJson(AObject: TJSONObject; const ARanges: TSampleRanges;
    AShare: double);
var
    A: TJSONArray;
    R: TJSONObject;
    i: longint;
begin
    if Length(ARanges) = 0 then
        Exit;
    AObject.Add('uncoveredSamples', UncoveredSampleCount(ARanges));
    A := TJSONArray.Create;
    for i := 0 to High(ARanges) do
    begin
        R := TJSONObject.Create;
        R.Add('from', ARanges[i].FromX);
        R.Add('to', ARanges[i].ToX);
        R.Add('count', ARanges[i].Count);
        A.Add(R);
    end;
    AObject.Add('uncoveredRanges', A);
    AObject.Add('uncoveredResidualShare', AShare);
end;

procedure ReadCoverageJson(AObject: TJSONObject; out ARanges: TSampleRanges;
    out AShare: double);
var
    D: TJSONData;
    A: TJSONArray;
    i, n: longint;
begin
    ARanges := nil;
    AShare := AObject.Get('uncoveredResidualShare', 0.0);
    D := AObject.Find('uncoveredRanges');
    if not (D is TJSONArray) then
        Exit;
    A := TJSONArray(D);
    n := 0;
    SetLength(ARanges, A.Count);
    for i := 0 to A.Count - 1 do
        if A.Items[i] is TJSONObject then
        begin
            ARanges[n].FromX := TJSONObject(A.Items[i]).Get('from', 0.0);
            ARanges[n].ToX := TJSONObject(A.Items[i]).Get('to', 0.0);
            ARanges[n].Count := TJSONObject(A.Items[i]).Get('count', 0);
            Inc(n);
        end;
    SetLength(ARanges, n);
end;

end.
