program http_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  indy_http_server, metadata_service, logger_extension, server_listener,
  server_service_soap, server_binary_formatter, server_service_xmlrpc, config_objects,
  user_service_intf, user_service_intf_binder, user_service_intf_imp;


var
  AppObject : TwstListener;
begin
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();

  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();

  //wst_CreateDefaultFile(wst_GetConfigFileName(),nil);
  
  AppObject := TwstIndyHttpListener.Create();
  try
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    AppObject.Start();
    ReadLn();
  finally
    FreeAndNil(AppObject);
  end;
end.
