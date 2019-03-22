{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of adapter class for IExpressionParser.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit ExpressionParserAdapter;

{$MODE Delphi}

interface

uses Classes, SysUtils, Settings, IntExpressionParser, CurvePointsSet,
  CBRCComponent;

type
    { Class-adapter implementing basic operation for parsing curve expression.
      Implemented as singleton. }
    TExpressionParserAdapter = class(TCBRCComponent, IExpressionParser)
    private
        class var FExpressionParserAdapter: TExpressionParserAdapter;
        constructor Init;

    public
        class function Create: TExpressionParserAdapter;

        function ParseExpression(Expression: string): Curve_parameters;
    end;

implementation

uses Main, MyExceptions, Dialogs;

constructor TExpressionParserAdapter.Init;
begin
    inherited Create(nil);
end;

class function TExpressionParserAdapter.Create: TExpressionParserAdapter;
begin
    if FExpressionParserAdapter = nil then
      FExpressionParserAdapter := TExpressionParserAdapter.Init;
    Result := FExpressionParserAdapter;
end;

function TExpressionParserAdapter.ParseExpression(
    Expression: string): Curve_parameters;
begin
    try
        FitClientApp_.FitClient.SetSpecialCurveParameters(Expression, nil);
        Result := FitClientApp_.FitClient.GetSpecialCurveParameters;
    except
        on E: EUserException do
            begin
                MessageDlg(E.Message, mtError, [mbOk], 0);
            end;
        else raise;
    end;
end;

end.


