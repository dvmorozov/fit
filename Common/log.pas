unit log;

interface

uses
    Classes, SysUtils
{$IFDEF WINDOWS}
    , Windows
    , Shfolder
{$ENDIF}
    ;

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
    { The tier every process starts at. Debug - everything except the polling
      heartbeat, see TMsgType - because a fault that cannot be reproduced on
      demand has to be readable from the log the run already wrote, and a switch
      nobody passed is a switch that was off during the one run that mattered.
      Define FIT_QUIET_LOG to build a quiet binary without touching a call site. }
{$IFDEF FIT_QUIET_LOG}
    DEFAULT_LOG_LEVEL = Notification;
{$ELSE}
    DEFAULT_LOG_LEVEL = Debug;
{$ENDIF}

procedure WriteLog(Msg: string; MsgType: TMsgType);
function GetSeqErrorCode: longint;
function CreateErrorMessage(Msg: string): string;
function GetConfigDir: string;

{ The call stack of the exception being handled. Only meaningful from inside an
  except block; outside one it describes no particular exception. }
function ExceptionTrace: string;

{ Writes to another file in the config directory instead of the default
  log.txt. Processes that run side by side (the desktop client and the compute
  server) must not append to the same file. Call before the first WriteLog. }
procedure SetLogFileName(const AFileName: string);
{ Messages above this severity are dropped. Debug, the default, logs
  everything; Notification drops only Debug. }
procedure SetLogLevel(AMsgType: TMsgType);
{ The tier in force, DEFAULT_LOG_LEVEL until SetLogLevel says otherwise. }
function GetLogLevel: TMsgType;
{ Mirrors every logged message to stderr, so a server run in a console shows
  its activity live. }
procedure SetLogEcho(AEcho: boolean);
{ The size at which the log file is rotated, in bytes; see LOG_SIZE_LIMIT for
  what rotation does. A process that logs far more or far less than the default
  assumes can set its own budget. }
procedure SetLogSizeLimit(ALimit: int64);
{ Parses a level name (fatal|warning|notification|debug); returns False when the
  name is not one of them. }
function TryParseLogLevel(const AName: string; out AMsgType: TMsgType): boolean;

implementation

const
    StrErrorID: string = ' Error identifier: ';
    //  Application name for the config/log directory. Decoupled from the LCL
    //  (was Application.Title) so non-GUI code (and tests) need not link Forms.
    ConfigAppName = 'Fit';

{$IFDEF WINDOWS}
const
    Slash: string = '\';
{$ELSE}
const
    Slash: string = '/';
{$ENDIF}

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

function GetUserDir: string;
{$IFDEF WINDOWS}
var
    Path: array[0..MAX_PATH] of char;
{$ENDIF}
begin
{$IFDEF WINDOWS}
    Path[0] := #0;
    //  pochemu-to s flagom CSIDL_FLAG_CREATE ne rabotaet !
    SHGetFolderPath(0, {CSIDL_PERSONAL} CSIDL_APPDATA, 0, 0, @Path);
    Result := StrPas(Path);
{$ELSE}
    //  The user's home directory (config lives in $HOME/Fit/).
    Result := GetEnvironmentVariable('HOME');
{$ENDIF}
end;

function GetConfigDir: string;
var
    DirName: string;
begin
    //  The application's directory for the CURRENT user, and the file name
    //  within it.
    DirName := GetUserDir;
    if DirName <> '' then
    begin
        DirName := DirName + Slash + ConfigAppName + Slash;
        if not FileExists(DirName) then
            if not ForceDirectories(DirName) then
                DirName := ''; //  ne udalos' sozdat' katalog
    end;
    Result := DirName;
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
    { The name passed to OpenLog, needed to rotate the file it opened. }
    LogFileName: string = '';
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

{ Opens the log file; the caller holds the lock. }
procedure OpenLog(const AFileName: string);
var
    FullName: string;
begin
    FullName := GetConfigDir + AFileName;
    LogFileName := AFileName;
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
    FullName := GetConfigDir + LogFileName;
    PrevName := FullName + '.1';
    try
        CloseLog;
        //  QUALIFIED: on Windows the Windows unit is in scope and its
        //  DeleteFile takes a PChar, so the unqualified call does not compile
        //  there. SysUtils is the one meant on every platform.
        SysUtils.DeleteFile(PrevName);
        RenameFile(FullName, PrevName);
        OpenLog(LogFileName);
    except
        //  Running without a log beats failing to run.
    end;
end;

procedure InitializeLog;
begin
    InitCriticalSection(LogCS);
    try
        OpenLog('log.txt');
    except
        //  A process that cannot write its log must still run.
    end;
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
        OpenLog(AFileName);
    except
        //  Keep running without a log rather than failing to start.
    end;
    LeaveCriticalSection(LogCS);
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
procedure WriteLog(Msg: string; MsgType: TMsgType);
var
    Line: string;
begin
    //  Ordered by severity, so a lower level drops the noisier messages.
    if MsgType > LogLevel then
        Exit;

    EnterCriticalSection(LogCS);
    try
        Line := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + Chr(9) +
            LevelNames[MsgType] + ':' + Chr(9) +
            //  The HTTP server handles connections on several threads.
            '[' + IntToStr(PtrUInt(GetCurrentThreadId)) + ']' + Chr(9) + Msg;

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
