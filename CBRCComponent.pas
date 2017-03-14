//      ������� ����� ������ �������������� ���������, ����������� ��
//      ���� ������� ���������; ��������� �������� �������������� ���������,
//      ����������� ������ � ������ ��������� ��� ����������� ���������������
{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit CBRCComponent;

{$MODE Delphi}

interface

uses Classes;

type
    TCBRCComponent = class(TComponent)
        {  the component Controlled By References Counter (CBRC) }
        //  ���������, ����������� �������� ����������� ������ ������ �� ����
        //  ����� ����������; ��� ������ ����������� ��������� ������������
        //  ������ � ��� ������, ���� ����� ������ �� ��� ���������� ����� ����,
        //  ���� �� ����� ������ ����� ����� ����, �� ��������� �� ������������
        //  �� ��� ��� ���� �� ����� ������ ���������� (??? ���� �� �������
        //  ��������� �����, ������� �� �������� �������, ����������� ������
        //  ������ ������ � ��������� �� ������ �� IUnknown ������� ???)
        //  !!! ������ �� ������ ������ ��������� �� ���� ���������
        //  �������� �� �������� ��������, � ��������� ������ �����
        //  ������ ������ �� ������ ������������ !!!
    protected
        FRefCount: LongInt;         //  ����� ������ �� ������
        IntControlled: Boolean;     //  ������� ����, ��� �����������
                                    //  ������� ����������� ������ ������
                                    //  ��������������� � ������ Free

        function _AddRef: Integer; virtual; stdcall; 
        function _Release: Integer; virtual; stdcall;
    public
        procedure Free;
            //  ��������� ����� Free �� �����������, � ������ �������������
            //  �� ������ ����������� ��������� �� ������ ���������� ���� -
            //  ������ TCBRCComponent, ���� ����� ������ ����� ��������������
            //  ���� ����� ������� Free

        property RefCount: LongInt read FRefCount;
    end;

implementation

procedure TCBRCComponent.Free;
begin
    //  Free - ����� ������, ������� ����� ���������� ���� �����
    //  ������ �� ��� ����������� � ������, ������� ����� ��������
    //  ��������� ��������
    if Assigned(Self) then
    begin
        if RefCount = 0 then Destroy
        else IntControlled := True;
    end;
end;

function TCBRCComponent._AddRef: Integer;
begin
    Inc(FRefCount);
    Result := RefCount;
end;

function TCBRCComponent._Release: Integer;
begin
    Dec(FRefCount);
    Result := RefCount;
    if IntControlled and (Result = 0) then Free;
end;

initialization
    RegisterClass(TCBRCComponent);
end.

