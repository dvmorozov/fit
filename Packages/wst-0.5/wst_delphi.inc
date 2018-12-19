{$IFNDEF HAS_QWORD}
  type
    QWord = type Int64;
    DWORD = LongWord;
    PtrInt = Integer;
    PByteArray = ^ByteArray;
    ByteArray = array[0..$effffff] of Byte;
    PtrUInt = Cardinal;
    SizeInt = Longint;
{$ENDIF}
