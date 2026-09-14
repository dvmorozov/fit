// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_dat_loader;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  dat_file_loader, title_points_set;
type
  TDatLoaderTest = class(TTestCase)
  private
    function DataDir: string;
  published
    procedure LoadsKnownProfile;
  end;
implementation

function TDatLoaderTest.DataDir: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
    DirectorySeparator + 'Data' + DirectorySeparator);
end;

procedure TDatLoaderTest.LoadsKnownProfile;
var
  Loader: TDATFileLoader;
  PS: TTitlePointsSet;
begin
  Loader := TDATFileLoader.Create(nil);
  try
    Loader.LoadDataSet(DataDir + '1.dat');
    PS := Loader.GetPointsSetCopy;
    try
      AssertEquals('point count', 1692, PS.PointsCount);
      AssertEquals('first X', 3.0, PS.PointXCoord[0], 1e-6);
      AssertEquals('first Y', 3377.0, PS.PointYCoord[0], 1e-6);
      AssertEquals('last X', 172.1, PS.PointXCoord[PS.PointsCount - 1], 1e-6);
      AssertEquals('last Y', 1434.0, PS.PointYCoord[PS.PointsCount - 1], 1e-6);
    finally
      PS.Free;
    end;
  finally
    Loader.Free;
  end;
end;

initialization
  RegisterTest('integration', TDatLoaderTest);
end.
