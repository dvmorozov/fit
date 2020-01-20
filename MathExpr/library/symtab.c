
/**************************************************************
  SYMBOL.C
 **************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "symtab.h"
#include "z_tab.h"
#include "environment.h"

SYMBOL *symlist = 0;    /* указатель на таблицу символов */

SYMBOL *lookup( char *s )
{
   SYMBOL *sp;

   for( sp = symlist; sp != (SYMBOL *)0; sp = sp->next )
      if( my_strcmp( sp->name, s ) == 0 ) return( sp );

   return( 0 );         /* запись не найдена */
}


SYMBOL *install( char *s, int t, double d )
{
   SYMBOL *sp;

   sp = (SYMBOL *)my_malloc( sizeof( SYMBOL ) );
   sp->name  = my_malloc( strlen( s ) + 1 );

   my_strcpy( sp->name, s );

   sp->type  = t;
   sp->u.val = d;
   sp->next  = symlist; /* alloc in begin of list */
   symlist   = sp;

   return( sp );
}

/*******************  END OF FILE SYMTAB.C  *******************/
