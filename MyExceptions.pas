{------------------------------------------------------------------------------
    This software is distributed under GPL (see gpl.txt for details)
    in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) 1999-2008 D.Morozov (dvmorozov@mail.ru)
------------------------------------------------------------------------------}
unit MyExceptions;

{$MODE Delphi}

interface

uses
    Classes, SysUtils;

type
    //  ����������� ������, ��������� � ��������������
    //  ���������� ��������, ���������� �������������
    EUserException = class(Exception);

implementation

initialization
end.

