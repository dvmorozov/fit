// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for iterating through curve types.)

Copyright (C) Dmitry Morozov
}
unit int_curve_type_iterator;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    named_points_set, int_curve_factory;

type
    { Interface defining basic operation for iterating through curve types. }
    ICurveTypeIterator = interface
        procedure FirstCurveType;
        procedure NextCurveType;
        function EndCurveType: boolean;
        function GetCurveTypeName: string;
        function GetCurveTypeId: TCurveTypeId;
        function GetCurveTypeTag(CurveTypeId: TCurveTypeId): integer;
        { The current type's class, so callers can drive it generically (e.g. its
          configurable points set) without a hardcoded class list. }
        function GetCurrentCurveClass: TCurveClass;
    end;

implementation

end.
