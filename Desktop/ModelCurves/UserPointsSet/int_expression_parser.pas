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
uses
    persistent_curve_parameters;

type
    { Interface defining basic operation for parsing curve expression. }
    IExpressionParser = interface
        ['{b6b72f35-cf5d-43b0-9180-6370e420f026}']
        function ParseExpression(Expression: string): Curve_parameters;

    end;
{$ENDIF}

implementation

end.
