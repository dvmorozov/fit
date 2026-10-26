// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What is written to the log, and what the user is told, when a data
source fails.)

WHY IT IS WRITTEN DOWN AT ALL. A source depends on somebody else's computer, so
its failures are ordinary - and one met once, in front of a user, and gone by
the time anybody looks is the hardest kind to put right. Nothing under
Desktop/DataSources wrote a single log line before 2026-10-20, so a report of
"the search failed" carried nothing but the sentence on the screen.

THREE KINDS, ONE RULE EACH:

  an ANSWER (EDataSourceError) - "nothing found", "that DOI is elsewhere" - is
      logged as a notification and reaches the user as it was worded: the
      source has said what happened and what to try, and pointing at a log
      would make an ordinary answer read as a fault;
  a STOP (EWebCancelled) is not a failure at all, and is said in a line;
  a FAULT - the network, a reply that could not be read (EDataSourceFault), a
      defect - is logged as a warning, and the user's sentence ends by saying
      which file holds the details.

Everything here is a decision over plain values, so it is tested exhaustively
without a source, a wizard or a log file; data_source_wizard is the one place
that applies it, at every call it makes into a source.
}
unit data_source_failure;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { What the wizard was asking a source to do. }
    TSourceOperation = (
        soSearch,   //  answering the question the user typed
        soList,     //  listing what a source that asks nothing offers
        soOpen,     //  listing what a record holds
        soDownload, //  fetching the file chosen
        soRead      //  reading the fetched file with its loader
        );

{ What was being done, as part of a sentence: 'searching Published data',
  'opening "Quartz" in Published data'. }
function SourceOperationText(AOperation: TSourceOperation;
    const ASourceTitle, AItemTitle: string): string;

{ One log line for a failure: what was being done, by which source (its id),
  what was asked, the item's own reference, the kind of exception and its
  message. The kind tells a reader the network from a defect; the message alone
  is worded for the user and does not. }
function SourceFailureLogLine(AOperation: TSourceOperation;
    const ASourceId, ASourceTitle, AItemTitle, AQuery, ARef, AErrorKind,
    AMessage: string): string;

{ One log line for an operation that worked, with how many items came back. }
function SourceSuccessLogLine(AOperation: TSourceOperation;
    const ASourceId, ASourceTitle, AItemTitle, AQuery: string;
    ACount: longint): string;

{ AMessage, ending by saying where the details are; unchanged when there is no
  log file to point at. }
function WithLogPointer(const AMessage, ALogPath: string): string;

{ What the user is told when something nobody worded went wrong - a defect, a
  reply shaped in a way nothing expected: what was being done, the raw words,
  and where the details are. }
function UnexpectedFailureMessage(AOperation: TSourceOperation;
    const ASourceTitle, AItemTitle, AMessage, ALogPath: string): string;

{ One log line for one transfer: the method and address, the status that came
  back (or that none did), its size and how long it took, and what failed when
  something did. }
function TransferLogLine(const AMethod, AUrl: string; AStatus: longint;
    ABytes: int64; AMillis: int64; const AFailure: string): string;

implementation

function SourceOperationText(AOperation: TSourceOperation;
    const ASourceTitle, AItemTitle: string): string;
begin
    case AOperation of
        soSearch:
            Result := 'searching ' + ASourceTitle;
        soList:
            Result := 'listing what ' + ASourceTitle + ' offers';
        soOpen:
            Result := 'opening "' + AItemTitle + '" in ' + ASourceTitle;
        soDownload:
            Result := 'downloading "' + AItemTitle + '" from ' + ASourceTitle;
        else
            Result := 'reading "' + AItemTitle + '"';
    end;
end;

{ The facts a line carries after its sentence, those that are known. }
function Particulars(const ASourceId, AQuery, ARef: string): string;
begin
    Result := 'source ' + ASourceId;
    if AQuery <> '' then
        Result := Result + '; asked "' + AQuery + '"';
    if ARef <> '' then
        Result := Result + '; item ' + ARef;
end;

function SourceFailureLogLine(AOperation: TSourceOperation;
    const ASourceId, ASourceTitle, AItemTitle, AQuery, ARef, AErrorKind,
    AMessage: string): string;
begin
    Result := 'Data source failed while ' +
        SourceOperationText(AOperation, ASourceTitle, AItemTitle) + ' [' +
        Particulars(ASourceId, AQuery, ARef) + ']: ' + AErrorKind + ': ' +
        AMessage;
end;

function SourceSuccessLogLine(AOperation: TSourceOperation;
    const ASourceId, ASourceTitle, AItemTitle, AQuery: string;
    ACount: longint): string;
begin
    Result := 'Data source: ' +
        SourceOperationText(AOperation, ASourceTitle, AItemTitle) + ' [' +
        Particulars(ASourceId, AQuery, '') + '] gave ' + IntToStr(ACount) +
        ' item(s)';
end;

{ AText ending in exactly one full stop. }
function Sentence(const AText: string): string;
begin
    Result := TrimRight(AText);
    if (Result <> '') and not (Result[Length(Result)] in ['.', '!', '?']) then
        Result := Result + '.';
end;

function WithLogPointer(const AMessage, ALogPath: string): string;
begin
    if ALogPath = '' then
        Exit(AMessage);
    Result := Sentence(AMessage) + ' The details are in Fit''s log, ' +
        ALogPath + '.';
end;

function UnexpectedFailureMessage(AOperation: TSourceOperation;
    const ASourceTitle, AItemTitle, AMessage, ALogPath: string): string;
begin
    Result := WithLogPointer('Something went wrong while ' +
        SourceOperationText(AOperation, ASourceTitle, AItemTitle) + ': ' +
        AMessage, ALogPath);
end;

function TransferLogLine(const AMethod, AUrl: string; AStatus: longint;
    ABytes: int64; AMillis: int64; const AFailure: string): string;
begin
    Result := 'Web: ' + AMethod + ' ' + AUrl + ' -> ';
    if AStatus > 0 then
        Result := Result + 'HTTP ' + IntToStr(AStatus)
    else
        Result := Result + 'no answer';
    Result := Result + ', ' + IntToStr(ABytes) + ' bytes, ' +
        IntToStr(AMillis) + ' ms';
    if AFailure <> '' then
        Result := Result + '; ' + AFailure;
end;

end.
