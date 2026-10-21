// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for data loading.)

Copyright (C) Dmitry Morozov
}
unit int_data_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    title_points_set, coordinate_axis, sample_columns;

type
    { Interface defining basic operation for data loading. }
    IDataLoader = interface
        procedure LoadDataSet(AFileName: string);
        procedure Reload;
        function GetPointsSetCopy: TTitlePointsSet;
        { What the loaded data's ADimension is, as an axis mode id
          (axis_mode_registry), or '' when nothing says. What the automatic
          axis rule asks the data. }
        function CoordinateMode(ADimension: TAxisDimension): string;
        { The date of each point, when the argument counts bars of a dated
          series; empty otherwise. What names a bar on a date axis. }
        function ArgumentDates: TAxisDates;
        { Values read beside the one plotted, per sample and by name
          (sample_columns); empty when the file holds only the one. }
        function SampleColumns: TSampleColumns;
    end;

implementation

end.
