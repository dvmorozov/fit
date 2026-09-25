// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which series on the chart were drawn for which curve.)

WHY THE RELATION HAS TO BE RECORDED. A curve is not one line. An ordinary peak
is a single series; a curve placed from its own markup is that plus the markers
its contributor drew for it; and the next contributor will have its own
combination. Deleting the curve has to take all of them, and nothing on the
chart says which belong together - a series carries a Title and nothing else, and
a title is neither unique nor an identity.

WHAT STOOD IN FOR IT. The chart was rebuilt wholesale on every model change:
every model curve removed and re-added, and each contributor replotting its own
markers from its own redraw hook. That works, and it is why deleting a curve
already appeared to work - but it makes the removal a SIDE EFFECT of everyone
redrawing, so a contributor that draws per-curve series and registers no redraw
hook leaves its series behind with nothing to say so. Recording the owner makes
the removal an act rather than a consequence.

THE OWNER IS THE CURVE'S INSTANCE HANDLE, not an index. The model's order is
derived - it follows the fit intervals and the picks inside them - so an index
held across an edit names a different curve. The handle is issued once and
survives a rebuild.

NOTHING HERE KNOWS WHAT A SERIES IS. Both the series and the point set are held
as TObject, so this unit names no charting component and no widget set, and can
be tested with two plain objects. That is deliberate for the same reason
module_view_types gives: the chart component is a local fork of a 2005 library
and is expected to be replaced.
}
unit series_register;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes;

type
    { One series on the chart, and what it was drawn for. }
    TPlottedSeries = record
        { The chart's series. Opaque here. }
        Serie: TObject;
        { The point set it draws. Opaque here, and the key every existing caller
          already had in hand. }
        Points: TObject;
        { The curve that owns it, as the handle the model addresses that
          instance by.

          EMPTY FOR A SERIES THAT BELONGS TO THE MODEL AS A WHOLE - the profile,
          the computed profile, the difference, the background, the interval
          bounds, both position series, the picked points. Those are not
          anybody's curve and must survive a curve being deleted. }
        OwnerCurveId: string;
        { Whether this series was given a row in the legend.

          NOT ALWAYS: rows are added only while the legend is being updated, and
          the redraws during a running fit switch that off. So the legend is
          NOT index-parallel to the chart, which is the assumption that had it
          drawing one series against another's name. }
        HasLegendRow: boolean;
    end;

    TSeriesIndices = array of longint;

    { What is on the chart, in the order it was added.

      APPEND-ONLY WITH REMOVAL BY INDEX, mirroring the chart itself, so a caller
      that walks both cannot drift. }
    TSeriesRegister = class
    private
        FItems: array of TPlottedSeries;
    public
        procedure Add(ASerie, APoints: TObject; const AOwnerCurveId: string;
            AHasLegendRow: boolean);

        function Count: longint;
        function Item(AIndex: longint): TPlottedSeries;

        { -1 when nothing here draws that point set. }
        function IndexOfPoints(APoints: TObject): longint;
        { -1 when that series is not here. }
        function IndexOfSerie(ASerie: TObject): longint;

        { Every series that curve owns, HIGHEST INDEX FIRST.

          The order is the point: a caller removing them one at a time by index
          would otherwise have its later indices shifted by its own earlier
          removals - which silently removes the wrong series, or reads past the
          end. }
        function OwnedBy(const ACurveId: string): TSeriesIndices;

        { True when any series here belongs to that curve. }
        function AnyOwnedBy(const ACurveId: string): boolean;

        procedure Remove(AIndex: longint);
        procedure Clear;
    end;

implementation

procedure TSeriesRegister.Add(ASerie, APoints: TObject;
    const AOwnerCurveId: string; AHasLegendRow: boolean);
var
    N: longint;
begin
    N := Length(FItems);
    SetLength(FItems, N + 1);
    FItems[N].Serie := ASerie;
    FItems[N].Points := APoints;
    FItems[N].OwnerCurveId := AOwnerCurveId;
    FItems[N].HasLegendRow := AHasLegendRow;
end;

function TSeriesRegister.Count: longint;
begin
    Result := Length(FItems);
end;

function TSeriesRegister.Item(AIndex: longint): TPlottedSeries;
begin
    Result := Default(TPlottedSeries);
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    Result := FItems[AIndex];
end;

function TSeriesRegister.IndexOfPoints(APoints: TObject): longint;
var
    i: longint;
begin
    Result := -1;
    //  NIL MATCHES NOTHING, deliberately: an entry may legitimately hold no
    //  point set, and a caller asking about nil is asking about nothing.
    if not Assigned(APoints) then
        Exit;
    for i := 0 to High(FItems) do
        if FItems[i].Points = APoints then
            Exit(i);
end;

function TSeriesRegister.IndexOfSerie(ASerie: TObject): longint;
var
    i: longint;
begin
    Result := -1;
    if not Assigned(ASerie) then
        Exit;
    for i := 0 to High(FItems) do
        if FItems[i].Serie = ASerie then
            Exit(i);
end;

function TSeriesRegister.OwnedBy(const ACurveId: string): TSeriesIndices;
var
    i, N: longint;
begin
    Result := nil;
    N := 0;
    //  AN EMPTY HANDLE OWNS NOTHING. Every series that belongs to the model as
    //  a whole carries one, so answering them here would take the profile and
    //  the difference off the chart with the first curve deleted.
    if ACurveId = '' then
        Exit;
    //  Downwards, so a caller removing by index is not shifted by its own
    //  earlier removals.
    for i := High(FItems) downto 0 do
        if FItems[i].OwnerCurveId = ACurveId then
        begin
            SetLength(Result, N + 1);
            Result[N] := i;
            Inc(N);
        end;
end;

function TSeriesRegister.AnyOwnedBy(const ACurveId: string): boolean;
begin
    Result := Length(OwnedBy(ACurveId)) > 0;
end;

procedure TSeriesRegister.Remove(AIndex: longint);
var
    i: longint;
begin
    if (AIndex < 0) or (AIndex > High(FItems)) then
        Exit;
    for i := AIndex to High(FItems) - 1 do
        FItems[i] := FItems[i + 1];
    SetLength(FItems, Length(FItems) - 1);
end;

procedure TSeriesRegister.Clear;
begin
    SetLength(FItems, 0);
end;

end.
