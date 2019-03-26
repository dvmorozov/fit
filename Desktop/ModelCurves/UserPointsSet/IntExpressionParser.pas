{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for parsing curve expression.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit IntExpressionParser;

{$MODE Delphi}

interface

uses CurvePointsSet;

type
    { Interface defining basic operation for parsing curve expression. }
    IExpressionParser = interface
        function ParseExpression(Expression: string): Curve_parameters;
    end;

implementation

end.


