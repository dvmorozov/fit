// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of adapter class for IExpressionParser.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit expression_parser_adapter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    int_expression_parser, SysUtils,

    persistent_curve_parameters;



type
    { Class-adapter implementing basic operation for parsing curve expression.
      Implemented as singleton. }
{$warnings off}
    TExpressionParserAdapter = class(TInterfacedObject, IExpressionParser)
    private
        constructor Init;

    public
        class function Create: IExpressionParser;

        function ParseExpression(Expression: string): Curve_parameters;

    end;

{$warnings on}

implementation

uses app,

    MyExceptions,

    Dialogs;

var
    ExpressionParserAdapter: TExpressionParserAdapter;

constructor TExpressionParserAdapter.Init;
begin
    inherited;
end;

class function TExpressionParserAdapter.Create: IExpressionParser;
begin
    Result := IExpressionParser(ExpressionParserAdapter);
end;


function TExpressionParserAdapter.ParseExpression(Expression: string): Curve_parameters;
begin
    //  Deterministic nil on failure so callers can detect an invalid formula
    //  instead of dereferencing an uninitialised result.
    Result := nil;
    try
        FitClientApp_.FitClient.SetSpecialCurveParameters(Expression, nil);
        Result := FitClientApp_.FitClient.GetSpecialCurveParameters;
    except
        on E: EUserException do
        begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
        else
            raise;
    end;
end;



initialization
    ExpressionParserAdapter := TExpressionParserAdapter.Init;

finalization
    ExpressionParserAdapter.Free;

end.
