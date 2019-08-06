{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class for point set of experimental neutronogram.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit neutron_points_set;

{$MODE Delphi}

interface

uses Classes, SysUtils, points_set;

type
    EWavelengthIsNotSpecified = class(Exception);

    { Implements point set of experimental neutronogram. It's assumed that
      point coordinates are expressed in 2 * Theta. }
    TNeutronPointsSet = class(TPointsSet)
    protected
        FLambda: Double;
        
        function GetPointIntensity(index: LongInt): Double;
        procedure SetPointIntensity(index: LongInt; Value: Double);
        function GetPointT(index: LongInt): Double;
        function GetPoint2T(index: LongInt): Double;
        function GetPointSinTL(index: LongInt): Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        procedure CopyPointsFrom(const Points: TPointsSet);
        
        constructor Create(AOwner: TComponent); override;
        constructor CreateFromPoints(
            AOwner: TComponent; const Points: TPointsSet);
        
        property PointIntensity[index: LongInt]: Double
            read GetPointIntensity write SetPointIntensity;
        property Lambda: Double read FLambda write FLambda;
        property PointT[index: LongInt]: Double read GetPointT;
        property Point2T[index: LongInt]: Double read GetPoint2T;
        property PointSinTL[index: LongInt]: Double read GetPointSinTL;
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
var i: LongInt;
begin
    Assert(Assigned(Points));
    for i := 0 to Points.PointsCount - 1 do
        AddNewPoint(Points.PointXCoord[i], Points.PointYCoord[i]);
end;

constructor TNeutronPointsSet.CreateFromPoints(
    AOwner: TComponent; const Points: TPointsSet);
begin
    Assert(Assigned(Points));
    inherited Create(AOwner);
    CopyPointsFrom(Points);
end;

function TNeutronPointsSet.GetPointT(index: LongInt): Double;
begin
    Result := FPoints[index][1] / 2;
end;

function TNeutronPointsSet.GetPoint2T(index: LongInt): Double;
begin
    Result := FPoints[index][1];
end;

function TNeutronPointsSet.GetPointSinTL(index: LongInt): Double;
begin
    if Lambda <> 0 then
        Result := Sin((FPoints[index][1] * pi) / (2 * 180)) / Lambda
    else raise EWavelengthIsNotSpecified.Create('Wavelength undefined...')
end;

function TNeutronPointsSet.GetPointIntensity(index: LongInt): Double;
begin
    Result := PointYCoord[index];
end;

procedure TNeutronPointsSet.SetPointIntensity(index: LongInt; Value: Double);
begin
    FPoints[index][2] := Value;
end;

end.


