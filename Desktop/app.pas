{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains entry point of the application.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit app;

//{$DEFINE LOCAL_ACCESS}      //  vklyuchaet kompilyatsiyu servisa s
                            //  dostupom na lokal'nom komp'yutere
//{$DEFINE USE_DENWER}

interface

uses SysUtils, Forms
{$IFDEF FITCLIENT}
    , fit_client_app
{$ENDIF}
{$IFDEF FITSERVER}
    {$IFDEF FIT}
        , fit_server_app
    {$ELSE}
        {$IFNDEF FITP2P}
            , base_service_intf
        {$ENDIF}
    {$ENDIF}
{$ELSE}
    {$IFDEF FITPRO}
        , int_fit_service
    {$ENDIF}
    {$IFDEF FITCGI}
        , int_fit_service
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
    Proxy: IFitProblem;
{$ENDIF}    //  FITCGI

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        //  dlya prostogo statsionarnogo prilozheniya
        var FitServerApp_: TFitServerApp;
    {$ENDIF}
{$ENDIF}

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

{$IFDEF FITPRO}
type
    TFitServiceFactory = function: IFitService; cdecl;
    TFitServiceDestuctor = procedure; cdecl;

var
    CreateFitServiceInstance: TFitServiceFactory;
    DestroyFitServiceInstance: TFitServiceDestuctor;
    ProxyLibHandle: THandle;
{$ENDIF}

{$IFDEF FITCGI}
type
    TFitProblemFactory = function: IFitProblem; cdecl;
    TFitProblemDestructor = procedure; cdecl;

var
    CreateFitProblemInstance: TFitProblemFactory;
    DestroyFitProblemInstance: TFitProblemDestructor;
    ProxyLibHandle: THandle;
{$ENDIF}

//  klyuch FIT vkluchaet i klienta, i server v odin modul' s
//  pryamoi svyaz'yu mezhdu nimi
initialization
{$IFDEF FITPRO}
    ProxyLibHandle := LoadLibrary('ClientProxy.dll');
    CreateFitServiceInstance :=
        GetProcAddress(ProxyLibHandle, 'CreateFitServiceInstance');
    DestroyFitServiceInstance :=
        GetProcAddress(ProxyLibHandle, 'DestroyFitServiceInstance');

    Assert(Assigned(CreateFitServiceInstance));
    Assert(Assigned(DestroyFitServiceInstance));
    FitClientApp_.FitClient.FitProxy := CreateFitServiceInstance();
{$ENDIF}

{$IFDEF FITCGI}
    ProxyLibHandle := LoadLibrary('ClientProxy.dll');
    CreateFitProblemInstance :=
        GetProcAddress(ProxyLibHandle, 'CreateFitProblemInstance');
    DestroyFitProblemInstance :=
        GetProcAddress(ProxyLibHandle, 'DestroyFitProblemInstance');

    Assert(Assigned(CreateFitProblemInstance));
    Assert(Assigned(DestroyFitProblemInstance));
    Proxy := CreateFitProblemInstance();
{$ENDIF}

{$IFDEF FITCLIENT}
    { Client is included into application. }
    FitClientApp_ := TFitClientApp.Create;
{$ENDIF}

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        FitServerApp_ := TFitServerApp.Create(nil);
    {$ENDIF}
{$ENDIF}

{$IFDEF FITSERVER}
    { Server is included into application. }
{$IFDEF FIT}
    { Link is established to make calls from client to server. }
    FitClientApp_.FitClient.FitProxy := FitServerApp_.FitStub;
    { Link is established to make calls from server to client. }
    FitServerApp_.FitProxy.FitStub := FitClientApp_.FitStub;
{$ENDIF}
{$ENDIF}

finalization
{$IFDEF FITCLIENT}
    FitClientApp_.Free;
{$ENDIF}

{$IFDEF FITSERVER}
    {$IFDEF FIT}
        FitServerApp_.Free;
    {$ENDIF}
{$ENDIF}

{$IFDEF FITPRO}
    DestroyFitServiceInstance;
    FreeLibrary(ProxyLibHandle);
{$ENDIF}
{$IFDEF FITCGI}
    DestroyFitProblemInstance;
    FreeLibrary(ProxyLibHandle);
{$ENDIF}
end.

