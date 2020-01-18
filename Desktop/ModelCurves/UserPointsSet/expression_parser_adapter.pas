{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of adapter class for IExpressionParser.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit expression_parser_adapter;

interface

uses SysUtils, int_expression_parser,
{$IFNDEF FPC OR IFDEF WINDOWS}
    curve_points_set,
{$ENDIF}
    CBRCComponent;

type
    { Class-adapter implementing basic operation for parsing curve expression.
      Implemented as singleton. }
{$warnings off}
    TExpressionParserAdapter = class(TCBRCComponent, IExpressionParser)
    private
        class var FExpressionParserAdapter: TExpressionParserAdapter;
        constructor Init;

    public
        class function Create: TExpressionParserAdapter;
{$IFNDEF FPC OR IFDEF WINDOWS}
        function ParseExpression(Expression: string): Curve_parameters;
{$ENDIF}
    end;
{$warnings on}

implementation

uses app,
{$IFNDEF FPC OR IFDEF WINDOWS}
    MyExceptions,
{$ENDIF}
    Dialogs;

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

{$IFNDEF FPC OR IFDEF WINDOWS}
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
{$ENDIF}

end.


