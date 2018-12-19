unit udm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type

  { TDM }

  TDM = class(TDataModule)
    IM: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DM: TDM;

implementation

initialization
  {$I udm.lrs}

end.

