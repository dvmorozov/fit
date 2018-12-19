program http_server;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ActiveX,
  delphi_init_com in '..\..\..\delphi_init_com.pas',
  server_service_soap,
  server_binary_formatter,
  server_service_xmlrpc,
  indy_http_server,
  metadata_service,
  logger_extension,
  wst_delphi_rtti_utils in '..\..\..\wst_delphi_rtti_utils.pas',
  server_listener in '..\..\..\server_listener.pas',
  config_objects in '..\..\..\config_objects.pas',
  user_service_intf in '..\..\user_service_intf.pas',
  user_service_intf_binder in '..\..\user_service_intf_binder.pas',
  user_service_intf_imp in '..\..\user_service_intf_imp.pas',
  server_service_intf in '..\..\..\server_service_intf.pas';

var
  AppObject : TwstListener; AppObject2 : TwstListener;
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
    ReadLn;
  finally
    FreeAndNil(AppObject);
  end;
end.

