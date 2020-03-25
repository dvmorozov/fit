{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for point set of experimental neutronogram.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit neutron_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, points_set, SysUtils;

type
    EWavelengthIsNotSpecified = class(Exception);

    { Implements point set of experimental neutronogram. It's assumed that
      point coordinates are expressed in 2 * Theta. }
    TNeutronPointsSet = class(TPointsSet)
    protected
        FLambda: double;

        function GetPointIntensity(index: longint): double;
        procedure SetPointIntensity(index: longint; Value: double);
        function GetPointT(index: longint): double;
        function GetPoint2T(index: longint): double;
        function GetPointSinTL(index: longint): double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        procedure CopyPointsFrom(const Points: TPointsSet);

        constructor Create(AOwner: TComponent); override;
        constructor CreateFromPoints(AOwner: TComponent;
            const Points: TPointsSet);

        property PointIntensity[index: longint]: double
            read GetPointIntensity write SetPointIntensity;
        property Lambda: double read FLambda write FLambda;
        property PointT[index: longint]: double read GetPointT;
        property Point2T[index: longint]: double read GetPoint2T;
        property PointSinTL[index: longint]: double read GetPointSinTL;
    end;

implementation

{=========================== TNeutronPointsSet ================================}

procedure TNeutronPointsSet.CopyParameters(const Dest: TObject);
begin
    inherited;
    TNeutronPointsSet(Dest).Lambda := Lambda;
end;

constructor TNeutronPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
end;

procedure TNeutronPointsSet.CopyPointsFrom(const Points: TPointsSet);
var
    i: longint;
begin
    Assert(Assigned(Points));
    for i := 0 to Points.PointsCount - 1 do
        AddNewPoint(Points.PointXCoord[i], Points.PointYCoord[i]);
end;

constructor TNeutronPointsSet.CreateFromPoints(AOwner: TComponent;
    const Points: TPointsSet);
begin
    Assert(Assigned(Points));
    inherited Create(AOwner);
    CopyPointsFrom(Points);
end;

function TNeutronPointsSet.GetPointT(index: longint): double;
begin
    Result := FPoints[index][1] / 2;
end;

function TNeutronPointsSet.GetPoint2T(index: longint): double;
begin
    Result := FPoints[index][1];
end;

function TNeutronPointsSet.GetPointSinTL(index: longint): double;
begin
    if Lambda <> 0 then
        Result := Sin((FPoints[index][1] * pi) / (2 * 180)) / Lambda
    else
        raise EWavelengthIsNotSpecified.Create('Wavelength undefined...');
end;

function TNeutronPointsSet.GetPointIntensity(index: longint): double;
begin
    Result := PointYCoord[index];
end;

procedure TNeutronPointsSet.SetPointIntensity(index: longint; Value: double);
begin
    FPoints[index][2] := Value;
end;

end.
