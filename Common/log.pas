unit log;

interface

uses
  Classes, SysUtils, Forms
{$IFDEF WINDOWS}
    , Windows
{$IFNDEF FITCGI}
    , Shfolder
{$ENDIF}
{$ENDIF}
    ;

type TMsgType = (Fatal, Warning, Notification, User);

procedure WriteLog(Msg: string; MsgType: TMsgType);
function GetSeqErrorCode: LongInt;
function CreateErrorMessage(Msg: string): string;
function GetConfigDir: string;

implementation

const
    StrErrorID: string = ' Error identifier: ';

{$IFDEF WINDOWS}
const Slash: string = '\';
{$ELSE}
const Slash: string = '/';
{$ENDIF}

var SequentialErrorCode: LongInt = 1000;

function GetSeqErrorCode: LongInt;
begin
    Result := SequentialErrorCode;
    Inc(SequentialErrorCode);
end;

function CreateErrorMessage(Msg: string): string;
var EC: LongInt;
begin
    EC := GetSeqErrorCode;
    Result := Msg + StrErrorID + IntToStr(EC);
end;

function GetUserDir: string;
{$IFDEF WINDOWS}
    {$IFNDEF FITCGI}
        var Path: array[0..MAX_PATH] of Char;
    {$ENDIF}
{$ENDIF}
begin
{$IFDEF WINDOWS}
{$IFNDEF FITCGI}
    Path[0] := #0;
    //  pochemu-to s flagom CSIDL_FLAG_CREATE ne rabotaet !
    (* WINDOWS *)
    SHGetFolderPath(0, (* CSIDL_PERSONAL *)CSIDL_APPDATA, 0, 0, @Path);
    Result := StrPas(Path);
{$ELSE}
    Result := '..\data\tmp\';
{$ENDIF}
{$ELSE}
    Result := //GetEnvironmentVariable('HOME');
                '/home/www/tmp/';
{$ENDIF}
end;

function GetConfigDir: string;
var
    DirName: string;
begin
    //  poluchenie puti k papke prilozheniya dlya
    //  tekuschego pol'zovatelya i imeni fayla
    DirName := GetUserDir;
    if DirName <> '' then
    begin
        DirName := DirName + Slash + Application.Title + Slash;
        if not FileExists(DirName) then
            if not ForceDirectories(DirName) then
                DirName := ''; //  ne udalos' sozdat' katalog
    end;
    Result := DirName;
end;

var LogCS: TRTLCriticalSection;
    LogMsgCount: LongInt = 1;
    Log: TextFile;

procedure InitializeLog;
var
    LogFileName: string;
begin
    InitCriticalSection(LogCS);

    LogFileName := GetConfigDir + 'log.txt';

    AssignFile(Log, LogFileName);
    if FileExists(LogFileName) then Append(Log)
    else Rewrite(Log);
end;

procedure FinalizeLog;
begin
    CloseFile(Log);
    DoneCriticalsection(LogCS);
end;

{$hints off}
procedure WriteLog(Msg: string; MsgType: TMsgType);
begin
    EnterCriticalSection(LogCS);
    try
{$IFNDEF WRITE_PARAMS_LOG}
        Writeln(Log, DateTimeToStr(Now));
{$ENDIF}
        if MsgType = Fatal then
            Writeln(Log,
{$IFNDEF WRITE_PARAMS_LOG}
                'Fatal:' + Chr(9) +
{$ENDIF}
                Msg)
        else
        if MsgType = Warning then
            Writeln(Log,
{$IFNDEF WRITE_PARAMS_LOG}
                'Warning:' + Chr(9) +
{$ENDIF}
                Msg)
        else
        if MsgType = Notification then
            Writeln(Log,
{$IFNDEF WRITE_PARAMS_LOG}
                'Notification:' + Chr(9) +
{$ENDIF}
                Msg)
        else
            Writeln(Log,
{$IFNDEF WRITE_PARAMS_LOG}
                'User mistake:' + Chr(9) +
{$ENDIF}
                Msg);

        Inc(LogMsgCount);

        Flush(Log);
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

