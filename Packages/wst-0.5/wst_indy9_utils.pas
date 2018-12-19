unit wst_indy9_utils;

interface
uses SysUtils, IdTCPServer;

type

  TwstIndy9Thread = class(TIdPeerThread)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

implementation
uses ActiveX;
{ TwstIndy9Thread }

procedure TwstIndy9Thread.AfterExecute;
begin
  CoUninitialize();
  inherited;
end;

procedure TwstIndy9Thread.BeforeExecute;
begin
  inherited;
  CoInitialize(nil);
end;

end.
