// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating custom curve type object.)

Copyright (C) Dmitry Morozov
}
unit int_curve_type_parameters_factory;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    app_settings, persistent_curve_parameters;

type
    { Interface defining operation for creating custom curve type object. }
    ICurveTypeParametersFactory = interface
        function CreateUserCurveType(Name: string; Expression: string;
            Parameters: Curve_parameters): Curve_type;
    end;

implementation

end.
