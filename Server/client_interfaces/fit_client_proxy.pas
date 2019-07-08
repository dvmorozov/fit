{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of proxy class calling server by means of XML-RPC.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_client_proxy;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, fit_problem;
  
type
    TFitClientProxy = class(TFitProblem)
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    end;

implementation

constructor TFitClientProxy.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    CreateProblem;
end;

destructor TFitClientProxy.Destroy;
begin
    DiscardProblem(FProblemId);
    inherited Destroy;
end;

end.

