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
    //{$IFDEF FITPRO}
    //  trebuetsya dlya cgifit
        , DataLoader, fit_server_proxy, synapse_tcp_protocol,
        synapse_http_protocol, soap_formatter, binary_formatter,
        base_service_intf
{$IFDEF FITCGI}
    , FitClientProxy, PointsSet, NamedPointsSet
{$ENDIF}
    //{$ENDIF}
{$ENDIF}
    ;
//  peremennye vyneseny v otdel'nyi modul', dlya togo chtoby
//  k nim mozhno bylo by poluchit' dostup iz drugih modulei
{$IFDEF FITCLIENT}
    var FitClientApp_: TFitClientApp;
    {$IFNDEF FIT}
        ProblemID: LongInt;     //  poka podderzhivaetsya tol'ko odin ProblemID
                                //  na klient
    {$ENDIF}
{$ELSE}
{$IFDEF FITCGI}
    var Proxy: TFitClientProxy;
        ProblemID: LongInt = 0;
        Key: string = '';       //  klyuch, poluchennyi vo vremya registratsii
        UserName: string = '';
{$ENDIF}    //  FITCGI
{$ENDIF}    //  FITCLIENT

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

{$IFDEF FITPRO} {$DEFINE USE_ARRAY_CONVERSION} {$ENDIF}
{$IFDEF FITCGI} {$DEFINE USE_ARRAY_CONVERSION} {$ENDIF}
{$IFDEF FITSERVER}{$IFNDEF FIT} {$DEFINE USE_ARRAY_CONVERSION} {$ENDIF}{$ENDIF}

{$IFDEF USE_ARRAY_CONVERSION}
function CreateRemotableArray(
    APointsSet: TPointsSet): TArrayOfFloatDoubleRemotable;
function CreateNamedPointsSet(
    ARemotable: TArrayOfFloatDoubleRemotable): TNamedPointsSet;
{$ENDIF}

const
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

{$IFDEF USE_ARRAY_CONVERSION}
function CreateRemotableArray(
    APointsSet: TPointsSet): TArrayOfFloatDoubleRemotable;
var i, Count: LongInt;
begin
    Result := nil;
    if not Assigned(APointsSet) then Exit;
    
    Result := TArrayOfFloatDoubleRemotable.Create;
    //  tochki zagruzhayutsya poparno
    Count := APointsSet.PointsCount * 2;
    Result.SetLength(Count);
    i := 0;
    while i < Count do
    begin
        Result.Item[i] := APointsSet.PointXCoord[i div 2];
        Inc(i);
        Result.Item[i] := APointsSet.PointYCoord[i div 2];
        Inc(i);
    end;
end;

function CreateNamedPointsSet(
    ARemotable: TArrayOfFloatDoubleRemotable): TNamedPointsSet;
var i: LongInt;
    X, Y: Double;
begin
    //  trebuetsya dopustit' ravenstvo nil
    Result := nil;
    if not Assigned(ARemotable) then Exit;
    
    Assert(ARemotable.Length mod 2 = 0);
    Result := TNamedPointsSet.Create(nil);

    i := 0;
    while i < ARemotable.Length do
    begin
        X := ARemotable.Item[i];
        Inc(i);
        Y := ARemotable.Item[i];
        Inc(i);
        Result.AddNewPoint(X, Y);
    end;
end;
{$ENDIF}

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

//  klyuch FIT vkluchaet i klienta, i server v odin modul' s
//  pryamoi svyaz'yu mezhdu nimi
initialization
{$IFDEF FITCLIENT}
    FitClientApp_ := TFitClientApp.Create;
{$ENDIF}
{$IFDEF FITSERVER}
{$IFDEF FIT}
    FitServerApp_ := TFitServerApp.Create;
{$ENDIF}
{$ENDIF}

{$IFDEF FITPRO} {$DEFINE USE_PROXY} {$ENDIF}
{$IFDEF FITCGI} {$DEFINE USE_PROXY} {$ENDIF}

{$IFDEF USE_PROXY}
    SYNAPSE_RegisterTCP_Transport();
    SYNAPSE_RegisterHTTP_Transport();
{$ENDIF}

{$IFDEF FITPRO}
    FitClientApp_.FitProxy.FitStub := //wst_CreateInstance_IFitServer;
        TFitServer_Proxy.Create(
            'IFitServer',
            'binary:',
            'TCP:Address=' + InternalIP +
            ';Port=' + InternalPort + ';target=IFitServer'
            );
    ProblemID := FitClientApp_.FitProxy.FitStub.CreateProblem;
{$ELSE}
{$IFDEF FIT}
    // trebuetsya obratnaya svyaz'
    FitClientApp_.FitProxy.FitStub := FitServerApp_.FitStub;
{$ELSE}
{$IFDEF FITCGI}
    Proxy := TFitClientProxy.Create;
    Proxy.FitStub := //wst_CreateInstance_IFitServer;
        TFitServer_Proxy.Create(
            'IFitServer',
            'binary:',
            'TCP:Address=' + InternalIP +
            ';Port=' + InternalPort + ';target=IFitServer'
            );
{$ENDIF}    //  FITCGI
{$ENDIF}    //  FIT
{$ENDIF}    //  FITPRO

{$IFDEF FITSERVER}
{$IFDEF FIT}
    FitServerApp_.FitProxy.FitStub := FitClientApp_.FitStub;
{$ENDIF}
{$ENDIF}
    InitCriticalSection(LogCS);

finalization
{$IFDEF FITPRO}
    FitClientApp_.FitProxy.FitStub.DiscardProblem(ProblemID);
    FitClientApp_.FitProxy.FitStub._Release;
{$ELSE}
{$IFDEF FITCGI}
    Proxy.FitStub._Release;
    Proxy.Free;
{$ENDIF}    //  FITCGI
{$ENDIF}    //  FITPRO

{$IFDEF FITCLIENT}
    FitClientApp_.Free;
{$ENDIF}

{$IFDEF FITSERVER}
{$IFDEF FIT}
    FitServerApp_.Free;
{$ENDIF}
{$ENDIF}
    DoneCriticalsection(LogCS);
end.

