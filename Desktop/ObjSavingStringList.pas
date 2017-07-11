//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit ObjSavingStringList;

{$MODE Delphi}

interface

uses
    SelfCheckedComponentList, SelfCopied, SysUtils, Classes;

type
    TObjSavingStringList = class(TSelfCopiedCompList)
        //  spisok komponetov, kotoryy kazhdomu komponentu
        //  v spiske opredelyaet strokovyy identifikator
    protected
            
    public
        function GetObjectIdentifier(const ObjNum: LongInt): string; virtual;
    end;

implementation

function TObjSavingStringList.GetObjectIdentifier(const ObjNum: LongInt): string;
begin
    Result := 'Object ' + IntToStr(ObjNum);
end;

initialization
    RegisterClass(TObjSavingStringList);
end.


