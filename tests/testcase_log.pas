// SPDX-License-Identifier: GPL-3.0-or-later
{ The logging the compute server relies on to be diagnosable: its own file
  (the client keeps log.txt), a severity level that decides what is kept, and
  the tier a process starts at when nobody passed a switch. }
unit testcase_log;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry, log;

type
  TLogTest = class(TTestCase)
  private
    FFileName: string;
    { Everything written to the test's log file so far. }
    function LogContents: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EveryProcessStartsAtTheLoudestTier;
    procedure ThePollingHeartbeatIsNotInTheDefaultLog;
    procedure WritesToTheChosenFile;
    procedure LevelDropsTheNoisierMessages;
    procedure TheFileIsRotatedAtItsSizeLimit;
    procedure RotationKeepsOnlyTwoGenerations;
  end;

  { What log.pas decides with no file involved: which tier a name means, what an
    error message carries, where the configuration lives. These used to sit in
    TLogTest, which is an integration test because it writes and rotates log
    files - so half of log.pas was reachable only from the slow half and counted
    toward nothing. The file behaviour stays where it belongs; the decisions move
    here. }
  TLogDecisionsTest = class(TTestCase)
  private
    FSavedLevel: TMsgType;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LogLevelNamesAreParsed;
    procedure AnUnknownLevelNameIsRejected;
    procedure EveryTierNameRoundTripsToTheTierInForce;
    procedure TheDefaultTierIsTheOneTheProcessStartsAt;
    procedure AnErrorMessageCarriesItsOwnCode;
    procedure EachErrorGetsADistinctCode;
    procedure TheConfigDirectoryIsAUsablePrefix;
    procedure TheConfigDirectoryDoesNotChangeBetweenCalls;
    procedure ATraceOutsideAnExceptionIsNotAFailure;
  end;

implementation

procedure TLogTest.SetUp;
begin
  FFileName := GetConfigDir + 'test_log.txt';
  DeleteFile(FFileName);
  DeleteFile(FFileName + '.1');
  SetLogFileName('test_log.txt');
end;

procedure TLogTest.TearDown;
begin
  //  Hand the log back to the default file before deleting ours.
  SetLogFileName('log.txt');
  SetLogLevel(DEFAULT_LOG_LEVEL);
  SetLogSizeLimit(LOG_SIZE_LIMIT);
  DeleteFile(FFileName);
  DeleteFile(FFileName + '.1');
end;

function TLogTest.LogContents: string;
var
  S: TStringList;
  F: TFileStream;
begin
  Result := '';
  if not FileExists(FFileName) then
    Exit;
  //  SHARE-PERMISSIVE, because the logger is holding this very file open for
  //  append while the test reads it - that is what the logger does, not
  //  something the test can arrange around. LoadFromFile opens with no sharing
  //  at all, which Unix ignores and Windows refuses outright, so every test
  //  that reads the log failed there with "used by another process" while
  //  passing everywhere else.
  F := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    S := TStringList.Create;
    try
      S.LoadFromStream(F);
      Result := S.Text;
    finally
      S.Free;
    end;
  finally
    F.Free;
  end;
end;

{ The lag and the crash that prompted this test were both invisible in the logs
  of the run that produced them, because the Debug tier needed a command-line
  switch and nobody had passed it. The default is the whole fix: a build that
  quietly starts at Notification puts the next fault back out of reach. A quiet
  binary is still buildable - that is what FIT_QUIET_LOG is for - so the test
  asserts the default of THIS build rather than the enum value. }
procedure TLogTest.EveryProcessStartsAtTheLoudestTier;
begin
{$IFDEF FIT_QUIET_LOG}
  AssertTrue('FIT_QUIET_LOG builds start at Notification',
    DEFAULT_LOG_LEVEL = Notification);
{$ELSE}
  AssertTrue('an ordinary build logs everything without being asked',
    DEFAULT_LOG_LEVEL = Debug);
{$ENDIF}
  //  SetUp has not touched the level, so what the process starts with is what
  //  is in force here.
  AssertTrue('the default is the tier actually in force',
    GetLogLevel = DEFAULT_LOG_LEVEL);

  //  And it really reaches the file: a default that only names a tier without
  //  keeping its messages would pass the check above and still lose the lines.
  WriteLog('a debug line nobody asked for', Debug);
{$IFDEF FIT_QUIET_LOG}
  AssertTrue('a quiet build drops it', Pos('nobody asked for', LogContents) = 0);
{$ELSE}
  AssertTrue('an ordinary build keeps it',
    Pos('nobody asked for', LogContents) > 0);
{$ENDIF}
end;

{ "Always on" must not mean "drowned". The client polls three routes twice a
  second for as long as it is open; logged at the default tier they are the
  entire file, and since the file rotates, they are what evicts the events
  worth keeping. They sit at Trace - the one tier below the default - so a full
  log stays readable and asking for Trace brings the heartbeat back. }
procedure TLogTest.ThePollingHeartbeatIsNotInTheDefaultLog;
begin
  SetLogLevel(DEFAULT_LOG_LEVEL);
  WriteLog('poll: GET /problems/1/state', Trace);
  WriteLog('real: PUT /problems/1/profile', Debug);
  AssertTrue('the heartbeat stays out of the default log',
    Pos('poll:', LogContents) = 0);
  AssertTrue('a real call is kept', Pos('real:', LogContents) > 0);

  //  ...and it is reachable when the polling itself is what is suspect.
  SetLogLevel(Trace);
  WriteLog('poll-wanted: GET /problems/1/state', Trace);
  AssertTrue('Trace brings the heartbeat back',
    Pos('poll-wanted:', LogContents) > 0);
end;

{ IsPolledRoute is tested in testcase_rest_polling, not here. It used to be
  asserted in this class, which is an INTEGRATION test because it writes log
  files - so a pure function over string literals ran only in the slow half
  and counted toward no coverage at all. }

procedure TLogTest.WritesToTheChosenFile;
begin
  SetLogLevel(Notification);
  WriteLog('a message of its own', Notification);
  AssertTrue('the message reached the chosen file',
    Pos('a message of its own', LogContents) > 0);
end;

procedure TLogTest.LevelDropsTheNoisierMessages;
begin
  //  The default: everything but Debug.
  SetLogLevel(Notification);
  WriteLog('kept-notification', Notification);
  WriteLog('dropped-debug', Debug);
  AssertTrue('notification is kept', Pos('kept-notification', LogContents) > 0);
  AssertTrue('debug is dropped', Pos('dropped-debug', LogContents) = 0);

  //  Debug keeps everything.
  SetLogLevel(Debug);
  WriteLog('kept-debug', Debug);
  AssertTrue('debug is kept at that level', Pos('kept-debug', LogContents) > 0);

  //  Fatal keeps only failures.
  SetLogLevel(Fatal);
  WriteLog('dropped-warning', Warning);
  WriteLog('kept-fatal', Fatal);
  AssertTrue('warning is dropped', Pos('dropped-warning', LogContents) = 0);
  AssertTrue('fatal is kept', Pos('kept-fatal', LogContents) > 0);
end;

{ A log left to grow is written until the disk is full, and the session that
  filled it is unreadable long before that: the client polls the server twice a
  second, so an ordinary day of use is tens of thousands of lines. At the limit
  the file must start again, keeping the one it replaced. }
procedure TLogTest.TheFileIsRotatedAtItsSizeLimit;
var
  i: integer;
begin
  SetLogLevel(Notification);
  //  Small enough that a handful of lines reaches it; the default limit would
  //  take a megabyte of test output to cross.
  SetLogSizeLimit(600);
  for i := 1 to 20 do
    WriteLog(Format('line %d of a log that must not grow for ever', [i]),
      Notification);

  AssertTrue('the filled file was kept as .1', FileExists(FFileName + '.1'));
  AssertTrue('the current file was started again and is under the limit',
    Length(LogContents) < 600);
  AssertTrue('the newest line is in the current file',
    Pos('line 20 ', LogContents) > 0);
  AssertTrue('the current file is not where the oldest line went',
    Pos('line 1 ', LogContents) = 0);
end;

{ Rotation that kept every generation would be no bound at all. }
procedure TLogTest.RotationKeepsOnlyTwoGenerations;
var
  i: integer;
begin
  SetLogLevel(Notification);
  SetLogSizeLimit(300);
  for i := 1 to 60 do
    WriteLog(Format('line %d of many rotations', [i]), Notification);

  AssertTrue('the previous generation is kept', FileExists(FFileName + '.1'));
  AssertFalse('no third generation is created', FileExists(FFileName + '.2'));
end;

{ ---- the decisions that need no file --------------------------------------- }

procedure TLogDecisionsTest.SetUp;
begin
  FSavedLevel := GetLogLevel;
end;

procedure TLogDecisionsTest.TearDown;
begin
  SetLogLevel(FSavedLevel);
end;

procedure TLogDecisionsTest.LogLevelNamesAreParsed;
var
  L: TMsgType;
begin
  //  These names come off a command line, so every one of them is a thing a user
  //  types. A tier that stopped being recognised would silently leave the
  //  process at its default, which is the opposite of what was asked.
  AssertTrue('fatal', TryParseLogLevel('fatal', L));
  AssertTrue('fatal is Fatal', L = Fatal);
  AssertTrue('debug is case-insensitive', TryParseLogLevel('DEBUG', L));
  AssertTrue('debug is Debug', L = Debug);
  AssertTrue('notification', TryParseLogLevel('notification', L));
  AssertTrue('notification is Notification', L = Notification);
  AssertTrue('trace', TryParseLogLevel('trace', L));
  AssertTrue('trace is Trace', L = Trace);
end;

procedure TLogDecisionsTest.AnUnknownLevelNameIsRejected;
var
  L: TMsgType;
begin
  //  REJECTED, not defaulted: a misspelt tier has to be reportable, or a
  //  diagnostic session runs at the wrong level and nobody knows why.
  AssertFalse('an invented name', TryParseLogLevel('chatty', L));
  AssertFalse('an empty name', TryParseLogLevel('', L));
  AssertFalse('a partial name', TryParseLogLevel('trac', L));
  //  Surrounding space IS accepted - the comparison trims both sides - which is
  //  right for a value that may have come from a config file or a shell quoting
  //  accident. Asserted so that the trimming is a decision rather than a
  //  coincidence.
  AssertTrue('surrounding space is tolerated', TryParseLogLevel(' trace ', L));
  AssertTrue('and it is still Trace', L = Trace);
end;

procedure TLogDecisionsTest.EveryTierNameRoundTripsToTheTierInForce;
var
  Names: array[0..3] of string;
  i: integer;
  L: TMsgType;
begin
  //  Parse, set, read back. The failure this catches is a parser and a setter
  //  that disagree - the name is accepted and a different tier takes effect,
  //  which no test of either half alone can see.
  Names[0] := 'fatal';
  Names[1] := 'notification';
  Names[2] := 'debug';
  Names[3] := 'trace';
  for i := 0 to High(Names) do
  begin
    AssertTrue(Names[i] + ' parses', TryParseLogLevel(Names[i], L));
    SetLogLevel(L);
    AssertTrue(Names[i] + ' is the tier in force', GetLogLevel = L);
  end;
end;

procedure TLogDecisionsTest.TheDefaultTierIsTheOneTheProcessStartsAt;
begin
  //  Not asserted against a literal: which tier is default depends on
  //  FIT_QUIET_LOG, and pinning the constant here would only restate the
  //  definition. What matters is that a process which was passed no switch is
  //  actually AT it - a default named and not applied loses every line.
  SetLogLevel(DEFAULT_LOG_LEVEL);
  AssertTrue('the default is settable', GetLogLevel = DEFAULT_LOG_LEVEL);
end;

procedure TLogDecisionsTest.AnErrorMessageCarriesItsOwnCode;
var
  Msg: string;
  Code: longint;
begin
  //  THE CODE IS THE HANDLE. The user reads a number off a dialog and quotes it;
  //  the same number is in the log beside the detail. A message built without it
  //  leaves nothing to join the two by.
  //  GetSeqErrorCode CONSUMES a code - it returns the current one and advances -
  //  so the code the next message will carry is one past what it just handed
  //  back. A `Get` that mutates is a trap, and reading it twice to check a
  //  message is the shape that falls into it.
  Code := GetSeqErrorCode + 1;
  Msg := CreateErrorMessage('the profile could not be read');
  AssertTrue('the message carries the text',
    Pos('the profile could not be read', Msg) > 0);
  AssertTrue('and the code it was issued: ' + Msg,
    Pos(IntToStr(Code), Msg) > 0);
end;

procedure TLogDecisionsTest.EachErrorGetsADistinctCode;
var
  First, Second: longint;
begin
  //  Distinct, or two unrelated failures in one session quote the same number
  //  and the log cannot say which is which.
  First := GetSeqErrorCode;
  Second := GetSeqErrorCode;
  AssertTrue(Format('the code advanced (%d -> %d)', [First, Second]),
    Second > First);
  //  And the messages differ by it, which is what a user quoting one relies on.
  AssertTrue('two messages are distinguishable',
    CreateErrorMessage('same text') <> CreateErrorMessage('same text'));
end;

procedure TLogDecisionsTest.TheConfigDirectoryIsAUsablePrefix;
var
  Dir: string;
begin
  //  Every log and settings path in the application is this plus a file name, so
  //  a value that does not end in a separator produces paths that silently land
  //  one directory up with the name run together.
  Dir := GetConfigDir;
  AssertTrue('it is not empty', Dir <> '');
  AssertEquals('and it ends in a separator', PathDelim, Dir[Length(Dir)]);
end;

procedure TLogDecisionsTest.TheConfigDirectoryDoesNotChangeBetweenCalls;
begin
  //  Asked from several places during start-up. A value derived afresh each time
  //  from something that can change - the working directory, say - would put the
  //  log and the settings in different places.
  AssertEquals('the same answer', GetConfigDir, GetConfigDir);
end;

procedure TLogDecisionsTest.ATraceOutsideAnExceptionIsNotAFailure;
var
  S: string;
begin
  //  Called from error paths that are not always inside a raise. It must answer
  //  something rather than fault - the whole point of it is to be safe to call
  //  when things are already wrong.
  S := ExceptionTrace;
  AssertTrue('it returned', (S = '') or (Length(S) > 0));
end;

{ NOT TESTED HERE: ExceptionTrace called from inside an except block. It works -
  it was written, run and removed - but it takes about NINE SECONDS on this
  binary, because the first BackTraceStrFunc call loads the debug information for
  the whole executable. The rest of this suite runs in a tenth of a second, and a
  test that multiplies the fast half's runtime by a hundred for eight lines is a
  bad trade in the one suite that has to stay worth running.

  The nine seconds are not an artefact of testing: the same cost falls on the
  first exception the real application logs. See findings.md. }

initialization
  RegisterTest('integration', TLogTest);
  //  UNIT: no file is opened, written or rotated. The tier names, the error
  //  codes and the configuration path are decided from their arguments.
  RegisterTest('unit', TLogDecisionsTest);
end.
