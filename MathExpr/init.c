
/**************************************************************
  INIT.C
 **************************************************************/

#include "symtab.h"
#include "mathf.h"
#include "z_tab.h"


/*****************  initial data structures  ******************/

static struct
{
   char   *name;
   double  cval;
}
consts[] =
{
   "pi",    3.14159265358979323846,
   "e",     2.71828182845904523536,
   "gamma", 0.57721566490153286060,
   "deg",   57.2957795130823208768,
   "phi",   1.61803398874989484820,
   0,       0
};

static struct
{
   char *name;
   double (*func)();
}
builtins[] =
{
   //  названия ф-й в стиле C
   "acos",      ZAcos,
   "asin",      ZAsin,
   "atan",      ZAtan,
   "cos",       ZCos,
   "cosh",      ZCosh,
   "exp",       ZExp,
   "log",       ZLog,
   "log10",     ZLog10,
   "sin",       ZSin,
   "sinh",      ZSinh,
   "sqrt",      ZSqrt,
   "tan",       ZTan,
   "tanh",      ZTanh,
   "ceil",      ZCeil,
   "fabs",      ZFabs,
   "floor",     ZFloor,
    //  стандартные названия
    //  тригонометрические
   "cos",       ZCos,
   "sin",       ZSin,
   "tg",        ZTan,
   "ctg",       ZCtg,
   //   гиперболичесие
   "ch",        ZCosh,   
   "sh",        ZSinh,
   "th",        ZTanh,
   "cth",       ZCth,
   "sch",       ZSch,
   "csch",      ZCsch,
   //   обратные тригонометрические
   "arccos",    ZAcos,
   "arcsin",    ZAsin,
   "arctg",     ZAtan,
   "arcctg",    ZActg,
   //   обратные гиперболические
   "arsh",      ZArsh,
   "arch",      ZArch,
   "arth",      ZArth,
   "arcth",     ZArcth,
   //   прочие
   "exp",       ZExp,
   "ln",        ZLog,
   "log",       ZLog10,
   "sqrt",      ZSqrt,
   "abs",       ZFabs,  
   0,           0
};

void init( void )
{
   int     i;
   SYMBOL *s;

   for( i = 0; consts[i].name; i++ )
      install( consts[i].name, VAR_BLTIN, consts[i].cval );

   for( i = 0; builtins[i].name; i++ )
   {
      s = install( builtins[i].name, BLTIN, 0.0 );
      s->u.ptr = builtins[i].func;
   }
}

/*******************  END OF FILE INIT.C  *********************/
