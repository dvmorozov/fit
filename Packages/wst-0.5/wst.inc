{$IFNDEF FPC}
  const FPC_RELEASE = 0;
  const FPC_VERSION = 0;
{$ENDIF}

{$IFDEF FPC}
  {$IF( (FPC_VERSION = 2) and (FPC_RELEASE > 0) ) }
    {$define FPC_211}
  {$IFEND}
{$ENDIF}
