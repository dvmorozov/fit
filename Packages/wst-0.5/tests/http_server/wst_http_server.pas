{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program wst_http_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  app_object, metadata_service, logger_extension;
  

var
  AppObject : TwstWebApplication;
begin
  AppObject := TwstWebApplication.Create();
  try
    WriteLn('"Web Service Toolkit" WebServer listening at:');
    WriteLn('');
    WriteLn('http://127.0.0.1:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    ReadLn();
  finally
    FreeAndNil(AppObject);
  end;
end.
