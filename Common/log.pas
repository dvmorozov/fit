unit log;

interface

uses
    Classes, SysUtils;

const
    { A log that grows without a bound is written until the disk is full, and by
      then the interesting lines are unreachable anyway. At the limit the file is
      renamed to <name>.1 (replacing the previous .1) and a new one is started, so
      the two newest generations are always kept and nothing else. }
    LOG_SIZE_LIMIT = 32 * 1024 * 1024;

type
    { Ordered by severity: WriteLog keeps everything at or below the level in
      force. Trace is deliberately BELOW Debug and is the only tier off by
      default.

      Trace is for INNER LOOPS - output whose volume is set by an iteration
      count rather than by anything the user did. Today that is the routes the
      client polls twice a second, and the minimizer's per-iteration progress:
      one three-second fit writes some 340 lines, and an afternoon's session
      writes a hundred thousand "state = 5". Since the log rotates, that volume
      does not merely add noise - it is what evicts the events worth keeping.

      The tier is about volume, not value: these lines are diagnostics and are
      kept, in full, one request away (--log-level trace, /LOG_LEVEL=trace).
      Anything bounded by user actions belongs at Debug, where it is on by
      default. }
    TMsgType = (Fatal, Warning, Notification, Debug, Trace);

const
    { The tier every process starts at. Debug in a release - everything except
      the per-iteration detail, see TMsgType - because a fault that cannot be
      reproduced on demand has to be readable from the log the run already
      wrote, and a switch nobody passed is a switch that was off during the one
      run that mattered.

      Trace in a DETAILED build, made with FIT_DETAILED_LOG: the Debug build
      modes of every program's project and the build menu's development builds.
      A release is never one - packaging refuses a binary carrying
      DetailedLogNote's marker. Deliberately not Notification for a release,
      which FIT_QUIET_LOG used to offer and nothing ever defined: that was
      considered and declined with the user (fit-performance.md, stage 5). }
{$IFDEF FIT_DETAILED_LOG}
    DEFAULT_LOG_LEVEL = Trace;
{$ELSE}
    DEFAULT_LOG_LEVEL = Debug;
{$ENDIF}

procedure WriteLog(Msg: string; MsgType: TMsgType);
function GetSeqErrorCode: longint;
function CreateErrorMessage(Msg: string): string;
{ The folder holding the settings, ending in a separator and created on first
  use; '' when the environment names no home. Where it is on each platform, and
  the FIT_PROFILE_DIR that the build replaces it with, are app_data_root's. }
function GetConfigDir: string;
{ The folder holding the logs, the same way. Never the settings folder. }
function GetLogDir: string;

{ The call stack of the exception being handled. Only meaningful from inside an
  except block; outside one it describes no particular exception. }
function ExceptionTrace: string;

{ Writes to another file in the log directory instead of the default
  log.txt. Processes that run side by side (the desktop client and the compute
  server) must not append to the same file. Call before the first WriteLog. }
procedure SetLogFileName(const AFileName: string);
{ Messages above this severity are dropped. Debug, the default, logs
  everything; Notification drops only Debug. }
procedure SetLogLevel(AMsgType: TMsgType);
{ The tier in force, DEFAULT_LOG_LEVEL until SetLogLevel says otherwise. }
function GetLogLevel: TMsgType;
{ What a detailed build (FIT_DETAILED_LOG) says about itself; '' in any other.
  Each program logs it as it starts, so a reader learns why the log is long;
  and it is the marker packaging looks for in a binary - '(FIT_DETAILED_LOG)',
  in this text and nowhere else in any source - so a detailed build cannot be
  shipped by accident. }
function DetailedLogNote: string;
{ Whether a line of this tier would be written - WriteLog's own test. Ask it
  before BUILDING a line on a hot path: Pascal evaluates WriteLog's argument
  before WriteLog can check the tier, so a Format there runs, and is thrown
  away, on every call while the tier is off. }
function LogEnabled(AMsgType: TMsgType): boolean;
{ Mirrors every logged message to stderr, so a server run in a console shows
  its activity live. }
procedure SetLogEcho(AEcho: boolean);
{ The size at which the log file is rotated, in bytes; see LOG_SIZE_LIMIT for
  what rotation does. A process that logs far more or far less than the default
  assumes can set its own budget. }
procedure SetLogSizeLimit(ALimit: int64);
{ Parses a level name (fatal|warning|notification|debug|trace, in any case);
  returns False when the name is not one of them. }
function TryParseLogLevel(const AName: string; out AMsgType: TMsgType): boolean;

implementation

uses
    app_data_root;

const
    StrErrorID: string = ' Error identifier: ';

var
    SequentialErrorCode: longint = 1000;

function GetSeqErrorCode: longint;
begin
    Result := SequentialErrorCode;
    Inc(SequentialErrorCode);
end;

function CreateErrorMessage(Msg: string): string;
var
    EC: longint;
begin
    EC     := GetSeqErrorCode;
    Result := Msg + StrErrorID + IntToStr(EC);
end;

var
    LegacyAdopted: boolean = False;

{ What an earlier version left in HOME/Fit goes where it belongs now, once per
  process and before either folder is first handed out - so the settings a user
  had, and the curve types they defined, are read from the new place on the
  first start that looks there. }
procedure AdoptLegacyUserDir(const AEnv: TUserDirsEnvironment);
begin
    if LegacyAdopted then
        Exit;
    LegacyAdopted := True;
    try
        MoveLegacyUserFiles(LegacyUserDirIn(AEnv), AppConfigDirIn(AEnv),
            AppLogDirIn(AEnv));
    except
        //  Whatever could not be moved stays where it was; the program runs.
    end;
end;

{ ADir with a separator after it, created when it is not there; '' when there
  is no ADir or it cannot be made. }
function UsableDir(const ADir: string): string;
begin
    Result := '';
    if ADir = '' then
        Exit;
    if not DirectoryExists(ADir) and not ForceDirectories(ADir) then
        Exit;
    Result := IncludeTrailingPathDelimiter(ADir);
end;

function GetConfigDir: string;
var
    Env: TUserDirsEnvironment;
begin
    Env := UserDirsEnvironment;
    AdoptLegacyUserDir(Env);
    Result := UsableDir(AppConfigDirIn(Env));
end;

function GetLogDir: string;
var
    Env: TUserDirsEnvironment;
begin
    Env := UserDirsEnvironment;
    AdoptLegacyUserDir(Env);
    Result := UsableDir(AppLogDirIn(Env));
end;

var
    { The limit in force; LOG_SIZE_LIMIT until SetLogSizeLimit says otherwise. }
    LogSizeLimit: int64 = LOG_SIZE_LIMIT;

    LogCS: TRTLCriticalSection;
    LogMsgCount: longint = 1;
    Log:   TextFile;
    LogOpen: boolean = False;
    LogLevel: TMsgType = DEFAULT_LOG_LEVEL;
    LogEcho: boolean = False;
    { The file messages go to, in the log directory: log.txt until
      SetLogFileName names another. }
    LogFileName: string = 'log.txt';
    { Whether opening LogFileName is still to be tried. OPENED ON THE FIRST
      MESSAGE, not when the unit starts: every process used to create an empty
      log.txt the moment it loaded, before it could say which file it meant -
      and before the program had repaired an environment inherited from a snap,
      which decides where the log directory is. }
    LogPending: boolean = True;
    { Bytes in the open file, counted rather than queried: the file is open for
      append, so its size cannot be read cheaply on every line. }
    LogBytes: int64 = 0;

const
    LevelNames: array[TMsgType] of string =
        ('Fatal       ', 'Warning     ', 'Notification', 'Debug       ',
         'Trace       ');

{ The size of an existing file, 0 when it cannot be read. }
function ExistingFileSize(const AFullName: string): int64;
var
    Stream: TFileStream;
begin
    Result := 0;
    if not FileExists(AFullName) then
        Exit;
    try
        Stream := TFileStream.Create(AFullName, fmOpenRead or fmShareDenyNone);
        try
            Result := Stream.Size;
        finally
            Stream.Free;
        end;
    except
        Result := 0;
    end;
end;

{ Opens LogFileName; the caller holds the lock. Without a log directory there
  is no log: a bare name would land in whatever directory the process was
  started from, which is how a log.txt turned up beside a checkout. }
procedure OpenLog;
var
    Dir, FullName: string;
begin
    LogPending := False;
    Dir := GetLogDir;
    if Dir = '' then
        Exit;
    FullName := Dir + LogFileName;
    LogBytes := ExistingFileSize(FullName);
    AssignFile(Log, FullName);
    if FileExists(FullName) then
        Append(Log)
    else
        Rewrite(Log);
    LogOpen := True;
end;

procedure CloseLog;
begin
    if LogOpen then
    begin
        CloseFile(Log);
        LogOpen := False;
    end;
end;

{ Starts a new file, keeping the one just filled as <name>.1; the caller holds
  the lock. A failure here must not stop the process from running, so the log is
  simply left closed. }
procedure RotateLog;
var
    FullName, PrevName: string;
begin
    FullName := GetLogDir + LogFileName;
    PrevName := FullName + '.1';
    try
        CloseLog;
        //  QUALIFIED: on Windows the Windows unit is in scope and its
        //  DeleteFile takes a PChar, so the unqualified call does not compile
        //  there. SysUtils is the one meant on every platform.
        SysUtils.DeleteFile(PrevName);
        RenameFile(FullName, PrevName);
        OpenLog;
    except
        //  Running without a log beats failing to run.
    end;
end;

procedure InitializeLog;
begin
    InitCriticalSection(LogCS);
end;

procedure FinalizeLog;
begin
    CloseLog;
    DoneCriticalsection(LogCS);
end;

procedure SetLogFileName(const AFileName: string);
begin
    EnterCriticalSection(LogCS);
    try
        CloseLog;
        LogFileName := AFileName;
        LogPending := True;
    finally
        LeaveCriticalSection(LogCS);
    end;
end;

procedure SetLogLevel(AMsgType: TMsgType);
begin
    LogLevel := AMsgType;
end;

function GetLogLevel: TMsgType;
begin
    Result := LogLevel;
end;

procedure SetLogEcho(AEcho: boolean);
begin
    LogEcho := AEcho;
end;

procedure SetLogSizeLimit(ALimit: int64);
begin
    LogSizeLimit := ALimit;
end;

function ExceptionTrace: string;
var
    i: integer;
    Frames: PPointer;
begin
    Result := BackTraceStrFunc(ExceptAddr);
    Frames := ExceptFrames;
    for i := 0 to ExceptFrameCount - 1 do
        Result := Result + LineEnding + BackTraceStrFunc(Frames[i]);
end;

function TryParseLogLevel(const AName: string; out AMsgType: TMsgType): boolean;
var
    L: TMsgType;
begin
    Result := False;
    for L := Low(TMsgType) to High(TMsgType) do
        if CompareText(Trim(AName), Trim(LevelNames[L])) = 0 then
        begin
            AMsgType := L;
            Exit(True);
        end;
end;

{$hints off}
function DetailedLogNote: string;
begin
{$IFDEF FIT_DETAILED_LOG}
    Result := 'This is a detailed build (FIT_DETAILED_LOG): it starts at the ' +
        'Trace tier and writes every trace stream. Packages are built without it.';
{$ELSE}
    Result := '';
{$ENDIF}
end;

function LogEnabled(AMsgType: TMsgType): boolean;
begin
    //  Ordered by severity, so a lower level drops the noisier messages.
    Result := AMsgType <= LogLevel;
end;

procedure WriteLog(Msg: string; MsgType: TMsgType);
var
    Line: string;
begin
    if not LogEnabled(MsgType) then
        Exit;

    EnterCriticalSection(LogCS);
    try
        Line := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + Chr(9) +
            LevelNames[MsgType] + ':' + Chr(9) +
            //  The HTTP server handles connections on several threads.
            '[' + IntToStr(PtrUInt(GetCurrentThreadId)) + ']' + Chr(9) + Msg;

        if LogPending then
            try
                OpenLog;
            except
                //  A process that cannot write its log must still run.
            end;
        if LogOpen then
        begin
            Writeln(Log, Line);
            Flush(Log);
            Inc(LogBytes, Length(Line) + Length(LineEnding));
            if LogBytes >= LogSizeLimit then
                RotateLog;
        end;
        if LogEcho then
        begin
            Writeln(ErrOutput, Line);
            Flush(ErrOutput);
        end;

        Inc(LogMsgCount);
    except
        //  Exceptions are ignored.
    end;
    LeaveCriticalSection(LogCS);
end;

{$hints on}

initialization
    InitializeLog;

finalization
    FinalizeLog;
end.
