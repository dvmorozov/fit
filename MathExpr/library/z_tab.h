
/*********************************************************************
  Andre V.Kosteltsev (C) ZUBR Version - 3.0.0b. For Windows NT Console.
  Copyright (C) Andre V.Kosteltsev, 1995 - 1999. All rights reserved.

  This file prodused by ZUBR for used token definitions and
  union declaration.
 *********************************************************************/

#define NUMBER 257
#define VAR 258
#define VAR_BLTIN 259
#define BLTIN 260
#define UNDEF 261
#define UNARYMINUS 262

typedef union
{
   double  val;
   SYMBOL *sym;
} ZUBR_STYPE;

extern ZUBR_STYPE zubr_lval;

/*************************** End of File *****************************/
