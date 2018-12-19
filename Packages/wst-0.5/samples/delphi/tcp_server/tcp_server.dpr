program tcp_server;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils, ActiveX,
  base_service_intf,
  server_service_soap,
  base_binary_formatter,
  server_binary_formatter,
  metadata_service,
  metadata_service_imp,
  metadata_service_binder,
  //synapse_tcp_server,
  indy_tcp_server,
  user_service_intf,
  user_service_intf_binder,
  user_service_intf_imp,
  imp_helper,
  server_service_xmlrpc,
  server_listener;

{$INCLUDE wst.inc}

var
  listener : TwstListener;
begin
  CoInitialize(nil);
  try
    Server_service_RegisterBinaryFormat();
    Server_service_RegisterSoapFormat();
    Server_service_RegisterXmlRpcFormat();

    RegisterWSTMetadataServiceImplementationFactory();
    //Server_service_RegisterWSTMetadataServiceService();
    RegisterWSTMetadataServiceImplementationFactory();
  
    RegisterUserServiceImplementationFactory();
    Server_service_RegisterUserServiceService();
  
    WriteLn(Format('WST sample TCP Server listning on "%d"',[sSERVER_PORT]));
    WriteLn('Hit <enter> to stop.');
    //listener := TwstSynapseTcpListener.Create();
    listener := TwstIndyTcpListener.Create();
    listener.Start();
    ReadLn;
  finally
    CoUninitialize();
  end;
end.

