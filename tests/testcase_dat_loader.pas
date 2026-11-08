// SPDX-License-Identifier: GPL-3.0-or-later
{ Reading a DAT file from disk by its path - the step the parser tests
  (testcase_dat_parser) reach through lines instead. The file is written here,
  not taken from a sample folder: the framework tree holds no data of any field
  (the diffraction samples are the scientific module's, which has its own test
  that they load). }
unit testcase_dat_loader;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  dat_file_loader, title_points_set;
type
  TDatLoaderTest = class(TTestCase)
  private
    FFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadsAFileFromItsPath;
  end;
implementation

procedure TDatLoaderTest.SetUp;
var
  Lines: TStringList;
begin
  FFile := GetTempFileName(GetTempDir, 'datl') + '.dat';
  Lines := TStringList.Create;
  try
    Lines.Add('3.0 3377');
    Lines.Add('3.1 3350');
    Lines.Add('172.0 1500');
    Lines.Add('172.1 1434');
    Lines.SaveToFile(FFile);
  finally
    Lines.Free;
  end;
end;

procedure TDatLoaderTest.TearDown;
begin
  if FileExists(FFile) then
    DeleteFile(FFile);
end;

procedure TDatLoaderTest.LoadsAFileFromItsPath;
var
  Loader: TDATFileLoader;
  PS: TTitlePointsSet;
begin
  Loader := TDATFileLoader.Create(nil);
  try
    Loader.LoadDataSet(FFile);
    PS := Loader.GetPointsSetCopy;
    try
      AssertEquals('point count', 4, PS.PointsCount);
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
