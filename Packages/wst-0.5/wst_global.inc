{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$DEFINE HAS_QWORD}
  //{$DEFINE USE_INLINE}
{$ELSE}
  {$UNDEF HAS_QWORD}
  {$UNDEF USE_INLINE}
{$ENDIF}
