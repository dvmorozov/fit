{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
library lib_server;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,

  base_service_intf,
  server_service_intf,
  server_service_soap, server_binary_formatter, server_service_xmlrpc,
  metadata_repository, metadata_wsdl,
  metadata_service, metadata_service_binder, metadata_service_imp,
  library_base_intf, library_server_intf,

  user_service_intf_binder, user_service_intf_imp;



exports
  wstHandleRequest name WST_LIB_HANDLER;

begin
  RegisterStdTypes();
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();

  RegisterUserServiceImplementationFactory();
  Server_service_RegisterUserServiceService();

  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService();

end.
