//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Algorithm;

{$MODE Delphi}

interface

uses Classes;

type
    TAlgorithm = class(TComponent)
    public
        procedure AlgorithmRealization; virtual; abstract;
    end;


implementation

initialization
    RegisterClass(TAlgorithm);
end.


