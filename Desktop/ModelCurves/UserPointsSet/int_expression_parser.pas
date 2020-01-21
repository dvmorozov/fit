{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for parsing curve expression.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_expression_parser;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

{$IFDEF _WINDOWS}
uses curve_points_set;
{$ENDIF}

type
    { Interface defining basic operation for parsing curve expression. }
    IExpressionParser = interface
{$IFDEF _WINDOWS}
        function ParseExpression(Expression: string): Curve_parameters;
{$ENDIF}
    end;

implementation

end.


