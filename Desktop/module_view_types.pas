// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a module may ask the view to draw, in the framework's own terms.)

WHY THIS UNIT EXISTS. The view contract (int_fit_viewer) is implemented by the
chart-backed viewer, by the test mock, and - once modules are separate - by
nothing a module can see. So the types crossing that contract have to belong to
the framework rather than to any one feature or to any one charting component.

TWO INDEPENDENT REASONS, and both have already cost this codebase something:

  1. A FEATURE'S types in a shared contract. IFitViewer used to declare one
     module's outline rows, so every implementer - including the test mock -
     depended on that module's unit to compile. The rows never contained
     anything specific to it; only their names did.

  2. THE CHART COMPONENT'S types in a shared contract. Packages/TAGraph is a
     local fork of a 2005 component and is scheduled to be replaced. If a module
     declared its markers with that component's TSeriePointStyle, replacing the
     chart would break every module and the presenter with it. Here the style is
     described in plain terms and TFitViewer translates at the one place it
     creates a series - so the swap stays inside the view.
}
unit module_view_types;

{$mode objfpc}{$H+}

interface

type
    { A colour, in the same 24-bit BGR encoding the LCL's TColor uses, so the
      view can pass it straight through.

      NOT TColor itself, and that is not a preference: this unit is reached from
      the light test suite, which compiles without the widget set on purpose so
      the contracts can be tested in seconds rather than minutes. Naming the LCL
      graphics unit here would drag the whole widget set into that suite. }
    TModuleColor = longint;

const
    { The few colours a module needs by name. Spelled out rather than imported,
      for the reason above; the values are the LCL's. }
    mcNavy  = $800000;
    mcRed   = $0000FF;
    mcGreen = $008000;
    mcBlue  = $FF0000;
    mcBlack = $000000;

type
    { One row of a hierarchy shown beside the chart.

      A depth-first flattening, each row carrying its own indent. Flat rather
      than a real tree because that is what both a tree control and a plain list
      can consume, and because flattening makes the ORDER explicit - and order is
      a claim the structure would otherwise leave implicit and untested. }
    TOutlineRow = record
        { How far to indent; 0 for a root. Derived from the parent chain, not
          from any depth the item reports about itself - an item that was
          re-parented would otherwise indent inconsistently with the tree it is
          actually shown in. }
        Indent:     longint;
        { What to show. Built by whoever owns the vocabulary, so the view has
          none of its own. }
        Caption:    string;
        { Identity, so selecting a row can address the thing it stands for
          without the view holding the object. }
        Id:         string;
        { The handle the MODEL addresses this row's curve by, or empty when the
          row stands for no curve.

          SEPARATE FROM Id, and that is the point. A contributor identifies its
          rows by its own markup - a wave guid, say - and the framework's
          commands on one curve take a curve handle; where the row is the
          framework's own the two are the same string, and where it is a pack's
          they are not. The window used to answer "which curve does the selected
          row name?" by asking whose rows these were, and gave nothing for every
          row a pack put there: Delete curve was greyed over every pattern in a
          wave count, however it was clicked. }
        CurveId:    string;
        { True when this row is only a root because its parent could not be
          found. Shown differently rather than silently: a detached row means the
          data is damaged, and that is worth seeing (D26). }
        IsDetached: boolean;
    end;

    TOutline = array of TOutlineRow;

    { How a module's markers are drawn. Framework vocabulary: the view maps
      these onto whatever the charting component of the day understands. Kept to
      shapes any plotting component can be expected to offer - a richer set would
      be a promise the next component may not keep. "No marker" is not a shape:
      it is ShowPoints below. }
    TModuleMarkerShape = (msCircle, msCross, msDiagCross, msSquare, msStar);

    TModuleSeriesStyle = record
        Shape:      TModuleMarkerShape;
        { Marker size in pixels. }
        Size:       integer;
        Color:      TModuleColor;
        { Join the points with a line. For a series whose ORDER is the meaning -
          a sequence of pivots, a path - the polyline is what makes it readable. }
        ShowLines:  boolean;
        ShowPoints: boolean;
        { Sort by x before drawing. False for any series whose order carries
          meaning: sorting such a series silently reorders what it says. }
        Sorted:     boolean;
    end;

{ A style with the defaults an ordinary marker series wants, so a caller sets
  only what it actually cares about and a field added later does not silently
  arrive as zero. }
function DefaultModuleSeriesStyle: TModuleSeriesStyle;

implementation

function DefaultModuleSeriesStyle: TModuleSeriesStyle;
begin
    Result.Shape := msCircle;
    Result.Size := 6;
    Result.Color := mcNavy;
    Result.ShowLines := True;
    Result.ShowPoints := True;
    //  Unsorted by default: a module series is a sequence far more often than it
    //  is a scatter, and sorting one that was a sequence is a silent corruption
    //  rather than a visible error.
    Result.Sorted := False;
end;

end.
