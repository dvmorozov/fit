{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class mapping strings with objects.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit obj_saving_string_list;

{$MODE Delphi}

interface

uses
    self_copied_component, SysUtils, Classes;

type
    { List of components which for each object relates string identifier. }
    TObjSavingStringList = class(TSelfCopiedCompList)
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


