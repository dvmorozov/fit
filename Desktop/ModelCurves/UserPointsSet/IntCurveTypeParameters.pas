{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for storing custom curve type parameters.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit IntCurveTypeParameters;

{$MODE Delphi}

interface

uses Classes, SysUtils, Settings;

type
    { Interface defining basic operation for storing custom curve type parameters. }
    ICurveTypeParameters = interface
        procedure SetExpression(Expression: string);
        procedure SetName(Name: string);
        procedure SetParameters(Parameters: Curve_parameters);
    end;

implementation

end.


