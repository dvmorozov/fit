// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Whether the wizard can go on, and what to say when it cannot.)

THE SHAPE IS fit_advice's, AND SO IS THE REASON. A decision expressed over plain
booleans can be tested exhaustively in milliseconds, and the same function
decides what happens and what the user is told - so a disabled button and its
explanation cannot drift apart. A refusal that says only "cannot continue" is
the failure mode this exists to prevent.

EVERY REFUSAL NAMES WHAT IT SAW. "No reader handles .xlsx files" is actionable;
"this file cannot be imported" is not, and the difference is the whole of
non-negotiable 10 at the point a user actually meets it.
}
unit data_source_advice;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { A verdict and its reason. Reason is empty exactly when Allowed is true:
      an allowed action has nothing to explain, and a refusal always does. }
    TDataSourceVerdict = record
        Allowed: boolean;
        Reason: string;
    end;

{ Whether the Search button can run.

  IT ASKS ONLY WHAT THE WINDOW CAN ANSWER. An earlier version also took "is this
  computer online", and nothing could tell it: there is no network detection
  here, so the branch was reachable from tests and from nowhere else. What a
  user actually meets when the network is down is the web client's own refusal,
  which names the address and says that fitting never needs a connection. }
function AdviseSearch(const AMissingField: string): TDataSourceVerdict;

{ Whether a found item can be taken forward: a container is expanded rather than
  imported, and a file is only usable when this build has a reader for it. }
function AdviseChoose(AIsLeaf, AHasLoader: boolean;
    const AExtension: string): TDataSourceVerdict;

{ Whether the previewed download can become a project. }
function AdviseCreate(AHasFile, AParsed: boolean; APointCount: longint;
    const AParseError: string): TDataSourceVerdict;

implementation

function Allow: TDataSourceVerdict;
begin
    Result.Allowed := True;
    Result.Reason := '';
end;

function Refuse(const AReason: string): TDataSourceVerdict;
begin
    Result.Allowed := False;
    Result.Reason := AReason;
end;

function AdviseSearch(const AMissingField: string): TDataSourceVerdict;
begin
    //  The unanswered question is the one the user is looking at, and it is
    //  named rather than described: "Enter Series id to search" tells them
    //  which box, where "fill in the fields" does not.
    if Trim(AMissingField) <> '' then
        Exit(Refuse('Enter ' + AMissingField + ' to search.'));
    Result := Allow;
end;

function AdviseChoose(AIsLeaf, AHasLoader: boolean;
    const AExtension: string): TDataSourceVerdict;
begin
    if not AIsLeaf then
        //  Not a refusal the user has to do anything about: it says what the
        //  row is, which is why the next gesture is to open it.
        Exit(Refuse('This is a record rather than a file. Open it to see ' +
            'what it holds.'));
    if not AHasLoader then
        Exit(Refuse('No reader in this build handles ' +
            LowerCase(AExtension) + ' files, so this one cannot be opened ' +
            'here. Choose another file from this record.'));
    Result := Allow;
end;

function AdviseCreate(AHasFile, AParsed: boolean; APointCount: longint;
    const AParseError: string): TDataSourceVerdict;
begin
    if not AHasFile then
        Exit(Refuse('Nothing has been downloaded yet.'));
    if not AParsed then
    begin
        if Trim(AParseError) <> '' then
            //  The reader's own words: it knows what it expected and what it
            //  found, and this function would only make that vaguer.
            Exit(Refuse(AParseError));
        Exit(Refuse('The downloaded file could not be read.'));
    end;
    if APointCount = 0 then
        //  A file that parses to nothing is the quiet failure that matters
        //  most: it becomes an empty chart that looks like a working import.
        Exit(Refuse('The downloaded file holds no data points. It may be a ' +
            'web page saying the series is unavailable rather than the data ' +
            'itself.'));
    Result := Allow;
end;

end.
