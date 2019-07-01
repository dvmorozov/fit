{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains entry point of the application.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit Main;

//{$MODE objfpc}{$H+}
{$MODE Delphi}
//{$DEFINE LOCAL_ACCESS}      //  vklyuchaet kompilyatsiyu servisa s
                            //  dostupom na lokal'nom komp'yutere
//{$DEFINE USE_DENWER}
//{$DEFINE WRITE_PARAMS_LOG}  //  vklyuchaet zapis' posledovatel'nosti
                            //  izmeneniya var'iruemyh parametrov

interface

{$IFDEF FITPRO}
    {$DEFINE USE_PROXY_DLL}
{$ELSE}
    {$IFDEF FITCGI}
        {$DEFINE USE_PROXY_DLL}
    {$ENDIF}
{$ENDIF}

uses SysUtils, Forms
{$IFDEF WINDOWS}
    , windows, shfolder
{$ENDIF}
{$IFDEF FITCLIENT}
    , FitClientApp
{$ENDIF}
{$IFDEF FITSERVER}
    , FitServerApp, PointsSet, NamedPointsSet
    {$IFNDEF FIT}
        {$IFNDEF FITP2P}
            , base_service_intf
        {$ENDIF}
    {$ENDIF}
{$ELSE}
    {$IFDEF USE_PROXY_DLL}
        , int_fit_service
    {$ELSE}
        {$IFDEF FITCGI}
            , DataLoader, PointsSet, NamedPointsSet
        {$ENDIF}
    {$ENDIF}
{$ENDIF}
    ;
//  peremennye vyneseny v otdel'nyi modul', dlya togo chtoby
//  k nim mozhno bylo by poluchit' dostup iz drugih modulei
{$IFDEF FITCLIENT}
var FitClientApp_: TFitClientApp;
{$ELSE}
{$ENDIF}

{$IFDEF FITCGI}
var Key: string = '';        //  key obtained during registration
    UserName: string = '';
    Proxy: IFitService;
{$ENDIF}    //  FITCGI

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        //  dlya prostogo statsionarnogo prilozheniya
        var FitServerApp_: TFitServerApp;
    {$ENDIF}
{$ENDIF}

//  funkcii dlya opredeleniya mestopolozheniya
//  ??? f-i d. rabotat' v mnogopotochnom okruzhenii
function GetUserDir: string;
function GetConfigDir: string;

{$IFDEF WINDOWS}
const Slash: string = '\';
{$ELSE}
const Slash: string = '/';
{$ENDIF}
const
    InternalError: string = 'Internal service error. Error code: ';
    StrErrorID: string = ' Error identifier: ';

type TMsgType = (Fatal, Surprising, Notification_, User);

procedure WriteLog(Msg: string; MsgType: TMsgType);
function GetSeqErrorCode(): LongInt;
//  dopolnyaet soobschenie kodom oschibki
function CreateErrorMessage(Msg: string): string;

const
    //  Global setting should be in this file because the
    //  same setting should be used in building client and server
    //  applications.
{$IFDEF LOCAL_ACCESS}
    //  IP dlya svyazi CGI-klienta s serverom prilozheniya
    InternalIP: string = '127.0.0.1';
    InternalPort: string = '1234';
    //  IP dlya svyazi brauzera s CGI-klientom;
    //  eti dannye vstavlyayutsya v ishodyaschie stranitsy
    //  'fit' ispol'zuetsya pri rabote s denwer'om
{$IFDEF USE_DENWER}
    ExternalIP: string = 'fit';
{$ELSE}
    ExternalIP: string = '127.0.0.1';
{$ENDIF}
{$ELSE}
    //  IP dlya svyazi CGI-klienta s serverom prilozheniya
    //InternalIP: string = '192.168.0.190';
    //  CGI-klient i server rabotayut na odnom kompyutere
    InternalIP: string = '127.0.0.1';
    InternalPort: string = '1234';
    //  IP dlya svyazi brauzera s CGI-klientom;
    //  eti dannye vstavlyayutsya v ishodyaschie stranitsy
    ExternalIP: string = 'ec2-54-158-234-101.compute-1.amazonaws.com';
{$ENDIF}

implementation
    
var SequentialErrorCode: LongInt = 1000;
function GetSeqErrorCode(): LongInt;
begin
    try
        Result := SequentialErrorCode;
        Inc(SequentialErrorCode);
    except
        //  vyhod isklyucheniy nedopustim
    end;
end;

function CreateErrorMessage(Msg: string): string;
var EC: LongInt;
begin
    try
        EC := GetSeqErrorCode;
        Result := Msg + StrErrorID + IntToStr(EC);
    except
        //  vyhod isklyucheniy nedopustim
    end;
end;

function GetUserDir: string;
{$IFDEF WINDOWS}
var Path: array[0..MAX_PATH] of Char;
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
var FileName: string;
begin
    //  poluchenie puti k papke prilozheniya dlya
    //  tekuschego pol'zovatelya i imeni fayla
    FileName := GetUserDir;
    if FileName <> '' then
    begin
        FileName := FileName + Slash + Application.Title + Slash;
        if not FileExists(FileName) then
            if not ForceDirectories(FileName) then
                FileName := ''; //  ne udalos' sozdat' katalog
    end;
    Result := FileName;
end;

var LogCS: TRTLCriticalSection;
    LogMsgCount: LongInt = 1;

procedure WriteLog(Msg: string; MsgType: TMsgType);
var Log: TextFile;
    FileName: string;
begin
    EnterCriticalSection(LogCS);
    try
        FileName := GetConfigDir + 'log.txt';

        AssignFile(Log, FileName);
        if FileExists(FileName) then Append(Log)
        else Rewrite(Log);
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
        if MsgType = Surprising then
            Writeln(Log,
{$IFNDEF WRITE_PARAMS_LOG}
                'Surprising:' + Chr(9) +
{$ENDIF}
                Msg)
        else
        if MsgType = Notification_ then
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
        CloseFile(Log);
    except
        //  ne dolzhen vybrasyvat' isklyucheniya - sam
        //  ispol'zuetsya dlya zapisi soobscheniy isklyucheniy
    end;
    LeaveCriticalSection(LogCS);
end;

{$IFDEF USE_PROXY_DLL}
type
    TFitServiceFactory = function: IFitService; cdecl;

var
    CreateFitServiceInstance: TFitServiceFactory;
    ProxyLibHandle: THandle;
{$ENDIF}

//  klyuch FIT vkluchaet i klienta, i server v odin modul' s
//  pryamoi svyaz'yu mezhdu nimi
initialization
{$IFDEF USE_PROXY_DLL}
    ProxyLibHandle := LoadLibrary('ClientProxy.dll');
    CreateFitServiceInstance :=
        GetProcAddress(ProxyLibHandle, 'CreateFitServiceInstance');

    if Assigned(CreateFitServiceInstance) then
    begin
{$IFDEF FITPRO}
        FitClientApp_.FitClient.FitProxy := CreateFitServiceInstance();
{$ELSE}
    {$IFDEF FITCGI}
        Proxy := CreateFitServiceInstance();
    {$ENDIF}
{$ENDIF}
    end;
{$ENDIF}

{$IFDEF FITCLIENT}
    FitClientApp_ := TFitClientApp.Create;
{$ENDIF}

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        FitServerApp_ := TFitServerApp.Create;
    {$ENDIF}
{$ENDIF}

{$IFDEF FITSERVER}
{$IFDEF FIT}
    FitServerApp_.FitProxy.FitStub := FitClientApp_.FitStub;
{$ENDIF}
{$ENDIF}
    InitCriticalSection(LogCS);

finalization
{$IFDEF FITCLIENT}
    FitClientApp_.Free;
{$ENDIF}

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        FitServerApp_.Free;
    {$ENDIF}
{$ENDIF}
    DoneCriticalsection(LogCS);
{$IFDEF FITPRO}
    FreeLibrary(ProxyLibHandle);
{$ENDIF}
end.

