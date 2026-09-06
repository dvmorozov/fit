// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains entry point of the application.)

The desktop application is a THIN CLIENT: it contains no fitting engine. All
compute runs in fit_server, an independent process reached over HTTP+JSON and
started separately (possibly on another machine). The server URL is configured
in the UI and persisted; until then the default is used.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit app;

interface

uses
    Forms, SysUtils,
    fit_client_app,
    http_fit_service
    ;

var
    FitClientApp_: TFitClientApp;
    { The compute server, seen through IFitService. The form re-points it at the
      persisted URL once settings are read. }
    FitService_: THttpFitService;

implementation

initialization
    FitClientApp_ := TFitClientApp.Create;
    FitService_ := THttpFitService.Create(DEFAULT_SERVER_URL);
    { Every server call the client makes now goes over the network. }
    FitClientApp_.FitClient.FitService := FitService_;

finalization
    FitClientApp_.Free;
    FitService_.Free;
end.
