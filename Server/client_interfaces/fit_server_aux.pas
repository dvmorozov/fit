{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains server auxiliary functions.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_server_aux;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, PointsSet, NamedPointsSet, TitlePointsSet, fit_server,
  MyExceptions,
  { Modules of wst-0.5 }
  base_service_intf;
  
function CreateRemotableArray(
    APointsSet: TPointsSet): TArrayOfFloatDoubleRemotable;
function CreateNamedPointsSet(
    ARemotable: TArrayOfFloatDoubleRemotable): TNamedPointsSet;
function ProcessPointsResult(R: TPointsResult): TTitlePointsSet;

const OutOfServerResources: string = 'Out of server resources.';

implementation

{$INCLUDE wst.inc}

function CreateRemotableArray(
    APointsSet: TPointsSet): TArrayOfFloatDoubleRemotable;
var i, Count: LongInt;
begin
    Result := nil;
    if not Assigned(APointsSet) then Exit;

    Result := TArrayOfFloatDoubleRemotable.Create;
    //  tochki zagruzhayutsya poparno
    Count := APointsSet.PointsCount * 2;
    Result.SetLength(Count);
    i := 0;
    while i < Count do
    begin
        Result.Item[i] := APointsSet.PointXCoord[i div 2];
        Inc(i);
        Result.Item[i] := APointsSet.PointYCoord[i div 2];
        Inc(i);
    end;
end;

function CreateNamedPointsSet(
    ARemotable: TArrayOfFloatDoubleRemotable): TNamedPointsSet;
var i: LongInt;
    X, Y: Double;
begin
    //  trebuetsya dopustit' ravenstvo nil
    Result := nil;
    if not Assigned(ARemotable) then Exit;

    Assert(ARemotable.Length mod 2 = 0);
    Result := TNamedPointsSet.Create(nil);

    i := 0;
    while i < ARemotable.Length do
    begin
        X := ARemotable.Item[i];
        Inc(i);
        Y := ARemotable.Item[i];
        Inc(i);
        Result.AddNewPoint(X, Y);
    end;
end;

function ProcessPointsResult(R: TPointsResult): TTitlePointsSet;
var Points: TPointsSet;
    Res: LongInt;
    ErrMsg: string;
begin
    Result := nil; Res := 0; ErrMsg := '';
    if not Assigned(R) then raise Exception.Create(OutOfServerResources);

    try
        //  sozdaetsya promezhutochnyi ob'ekt dlya sovmestimosti
        Points := CreateNamedPointsSet(R._Result);
        try
            Res := R.ErrCode;
            ErrMsg := R.ErrMsg;
            if Assigned(Points) then
                Result := TTitlePointsSet.CreateFromPoints(nil, Points)
            else Result := nil;
        finally
            Points.Free;
        end;
    finally
        R.Free;
    end;

    case Res of
        -1: begin Result.Free; raise EUserException.Create(ErrMsg); end;
        -2: begin Result.Free; raise Exception.Create(ErrMsg); end;
    end;
end;

end.

