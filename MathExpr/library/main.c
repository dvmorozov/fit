
#include <stdio.h>  /* printf() used by parser   if set ZUBR_DEBUG */
#include <stdlib.h> /* getenv() used by parser for test ZUBR_DEBUG */

#define not_defined_zubr_sccsid   1

#define ZUBR_KOSTELTSEV_ZUBR 1
#ifndef not_defined_zubr_sccsid
static char zubr_sccsid[] = "@(#)zubrpar 3.0.0b Andre V.Kosteltsev 19/03/99";
#endif

int zubr_parse();


#include "init.h"
#include "symtab.h"
#include "mathf.h"
#include <ctype.h>

#include "debug.h"
#include "environment.h"

double expr_result;
char *expr;
int char_index;
int char_count; /*  полное кол-во символов в выражении. включа€ заверш. 0*/
int stop;       /*  признак завершени€ разбора выражени€*/
int success;

typedef union
{
   double  val;
   SYMBOL *sym;
} ZUBR_STYPE;

#define NUMBER 257
#define VAR 258
#define VAR_BLTIN 259
#define BLTIN 260
#define UNDEF 261
#define UNARYMINUS 262

#define ZUBR_ERRCODE 256

int zubr_lhs[] =
{
    -1,     0,     0,     0,     0,     0,     2,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,
};

int zubr_len[] =
{
     2,     0,     2,     3,     3,     3,     3,     1,     1,     1,
     1,     4,     3,     3,     3,     3,     3,     3,     2,
};

int zubr_defred[] =
{
     1,     0,     0,     7,     0,     9,     0,     0,     2,     0,
     0,     0,     5,     0,     0,     0,    10,     0,     0,     0,
     0,     0,     0,     4,     3,     0,     0,    17,     0,     0,
     0,     0,     0,    11,
};

int zubr_dgoto[] =
{
     1,    10,    16,
};

int zubr_sindex[] =
{
     0,   -10,    -6,     0,   -48,     0,   -16,   -39,     0,   -39,
     1,    10,     0,   -39,   -39,   -68,     0,   -33,   -39,   -39,
   -39,   -39,   -39,     0,     0,   -20,   -26,     0,   -40,   -40,
   -68,   -68,   -68,     0,
};

int zubr_rindex[] =
{
     0,     0,     0,     0,    -5,     0,     0,     0,     0,     0,
     0,   -14,     0,     0,     0,     8,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    -7,     0,     0,    42,    61,
    22,    49,    56,     0,
};

int zubr_gindex[] =
{
     0,    98,    38,
};

#define ZUBR_TABLESIZE 250
int zubr_table[] =
{
     8,     9,    20,     6,    12,     8,     7,    21,    27,    20,
    18,    23,    19,    13,    21,    33,    20,    18,    18,    19,
    24,    21,    20,    18,    14,    19,    22,    21,    10,    10,
     9,    10,    14,    10,     6,     7,     8,     8,     8,    11,
     8,     0,     8,    20,    18,     0,    19,     0,    21,    18,
    18,    18,    12,    18,    22,    18,     0,     0,     0,    15,
     0,    22,     0,    14,    14,    14,    16,    14,    22,    14,
     0,    13,     0,     0,    22,     0,     0,     0,     0,     0,
    10,     0,     0,    12,     0,    12,     0,    12,     0,     8,
    15,    15,    15,     0,    15,    22,    15,    16,    16,    16,
     0,    16,    13,    16,    13,    15,    13,    17,     0,     0,
     0,    25,    26,     0,     0,     0,    28,    29,    30,    31,
    32,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     3,     4,
     5,     6,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     2,     3,     4,     5,
     6,
};

int zubr_check[] =
{
    10,    40,    42,    10,    10,    10,    45,    47,    41,    42,
    43,    10,    45,    61,    47,    41,    42,    43,    10,    45,
    10,    47,    42,    43,    40,    45,    94,    47,    42,    43,
    40,    45,    10,    47,    41,    45,    41,    42,    43,     1,
    45,    -1,    47,    42,    43,    -1,    45,    -1,    47,    41,
    42,    43,    10,    45,    94,    47,    -1,    -1,    -1,    10,
    -1,    94,    -1,    41,    42,    43,    10,    45,    94,    47,
    -1,    10,    -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,
    94,    -1,    -1,    41,    -1,    43,    -1,    45,    -1,    94,
    41,    42,    43,    -1,    45,    94,    47,    41,    42,    43,
    -1,    45,    41,    47,    43,     7,    45,     9,    -1,    -1,
    -1,    13,    14,    -1,    -1,    -1,    18,    19,    20,    21,
    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,   258,
   259,   260,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   256,   257,   258,   259,
   260,
};

#define ZUBR_FINAL 1
#ifndef ZUBR_DEBUG
#define ZUBR_DEBUG 0
#endif
#define ZUBR_MAXTOKEN 262
#if ZUBR_DEBUG
char *zubr_name[] =
{
"end-of-file",0,0,0,0,0,0,0,0,0,"'\\n'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,"'('","')'","'*'","'+'",0,"'-'",0,"'/'",0,0,0,0,0,0,0,0,0,0,0,
0,0,"'='",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'^'",
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,"NUMBER","VAR","VAR_BLTIN","BLTIN","UNDEF","UNARYMINUS",
};

char *zubr_rule[] =
{
"$accept : list",
"list :",
"list : list '\\n'",
"list : list assign '\\n'",
"list : list expr '\\n'",
"list : list error '\\n'",
"assign : VAR '=' expr",
"expr : NUMBER",
"expr : VAR",
"expr : VAR_BLTIN",
"expr : assign",
"expr : BLTIN '(' expr ')'",
"expr : expr '+' expr",
"expr : expr '-' expr",
"expr : expr '*' expr",
"expr : expr '/' expr",
"expr : expr '^' expr",
"expr : '(' expr ')'",
"expr : '-' expr",
};
#endif

#define     zubr_clearin    (zubr_char=(-1))
#define     zubr_errok      (zubr_errflag=0)
#ifdef  ZUBR_STACKSIZE
#ifndef ZUBR_MAXDEPTH
#define     ZUBR_MAXDEPTH   ZUBR_STACKSIZE
#endif
#else
#ifdef  ZUBR_MAXDEPTH
#define     ZUBR_STACKSIZE  ZUBR_MAXDEPTH
#else
#define     ZUBR_STACKSIZE  500
#define     ZUBR_MAXDEPTH   500
#endif
#endif

#if ZUBR_DEBUG
int            zubr_debug;
#endif
int            zubr_nerrs;
int            zubr_errflag;
int            zubr_char;
int           *zubr_ssp;

ZUBR_STYPE    *zubr_vsp;
ZUBR_STYPE     zubr_val;
ZUBR_STYPE     zubr_lval;

int            zubr_ss[ZUBR_STACKSIZE];

ZUBR_STYPE     zubr_vs[ZUBR_STACKSIZE];

#define        zubr_stacksize  ZUBR_STACKSIZE


/********************  END OF GRAMMAR  ***********************/

//  функци€ формировани€ строки ошибки
void warning( char *s, char *t )
{
}

void zubr_error( char *s )
{
   warning( s, (char *)0 );
}
//  лексический анализатор
#define VAR_NAME_LEN    100         //  максимальна€ длина идентификатора переменной

int zubr_lex( void )
{
   int c;

   if(stop)return 0;
   MY_ASSERT(char_index < char_count);

   //   пропускаем пробелы и табы   
   while( (c=expr[char_index++]) == ' ' || c == '\t' );
   //   символ '\0' служит признаком остановки работы парсера,
   //   поэтому при встрече замен€етс€ на '\n' - признак конца
   //   выражени€
   if( c == '\0' ) {stop = 1; return( '\n' );}
   if( c == '.' || isdigit( c ) )
   {
      int n = expr[char_index++];

      //  точка распознаетс€ как число 0.0
      //  функции scanf() нельз€ отдавать число "."
      if( c == '.' && !isdigit( n ) )
      {
         expr[--char_index] = n;
         zubr_lval.val = 0.0;
         return( NUMBER );
      }
      expr[--char_index] = n;

      //  распознано начало числа
      expr[--char_index] = c;
      //  сканируем число и устанавливаем индекс на следующий символ
      n = sscanf_s( &expr[char_index], "%lf", &zubr_lval.val );
      while( expr[char_index] == '.'   ||
             isdigit(expr[char_index]) ||
             expr[char_index] == 'E'   ||
             expr[char_index] == 'e'   ||
             (expr[char_index] == '+' && (expr[char_index - 1] == 'E' || expr[char_index - 1] == 'e')) ||
             (expr[char_index] == '-' && (expr[char_index - 1] == 'E' || expr[char_index - 1] == 'e')) )
            char_index++; 
      return( NUMBER );
   }
   if( isalpha( c ) )
   {
      SYMBOL *sp;
      char sbuf[VAR_NAME_LEN + 1];
      int var_name_index = 0;                       //  число записанных в буфер символов   

      do
      {//   загрузка в пром. буфер имени переменной
       //   им€ не м.б. длиннее VAR_NAME_LEN символов;
       //   !!! символы не вход€щие во врем. буфер просто отбрасываютс€ !!!
         
         if( var_name_index < VAR_NAME_LEN ) 
            sbuf[var_name_index++] = c;

         c = expr[char_index++];
      } while( ( char_index < char_count ) && isalnum( c ) );

      expr[--char_index] = c;                       //  возврат последнего символа, не
                                                    //  относ€щегос€ к идентификатору
      sbuf[var_name_index] = '\0';                  //  запись завершающего нул€ идентификатора
      my_strlwr( sbuf );                            //  парсер не чувствителен к регистру символов

      if( (sp = lookup( sbuf ) ) == (SYMBOL *)0 )   //  запись в таблице символов не найдена - 
         sp = install( sbuf, UNDEF, 0.0 );          //  устанавливаем как UNDEF
      zubr_lval.sym = sp;
      //    лексический анализатор возвращает VAR,
      //    когда значение переменной не найдено в таблице
      return( sp->type == UNDEF ? VAR : sp->type );
   }
   return( c );
}

int parse_expression(char* expr_, double *result)
{
    MY_ASSERT(expr_);
    MY_ASSERT(result);

    expr = expr_;
    char_index = 0;
    char_count = my_strlen(expr) + 1;
    stop = 0;
    success = PARSING_SUCCESS;

    zubr_parse();
    *result = expr_result;
    return success;
}


/*******************  END OF FILE MAIN.Y  *********************/

#define ZUBR_ABORT     goto zubr_abort
#define ZUBR_ACCEPT    goto zubr_accept
#define ZUBR_NEWERROR  goto zubr_newerror
#define ZUBR_ERROR     goto zubr_errlab


int zubr_parse()
{
   register int zubr_m, zubr_n, zubr_state;
#if ZUBR_DEBUG
   register char *zubr_s;

   if( zubr_s = getenv("ZUBR_DEBUG") )
   {
      zubr_n = *zubr_s;
      if( zubr_n >= '0' && zubr_n <= '9' ) zubr_debug = zubr_n - '0';
   }
#endif

   zubr_nerrs   = 0;
   zubr_errflag = 0;
   zubr_char    = (-1);

   zubr_ssp     = zubr_ss;
   zubr_vsp     = zubr_vs;
  *zubr_ssp     = zubr_state = 0;

zubr_loop:
   if( zubr_n = zubr_defred[zubr_state] ) goto zubr_reduce;
   if( zubr_char < 0 )
   {
      if( (zubr_char = zubr_lex()) < 0 ) zubr_char = 0;
#if ZUBR_DEBUG
      if( zubr_debug )
      {
         zubr_s = 0;
         if( zubr_char <= ZUBR_MAXTOKEN ) zubr_s = zubr_name[zubr_char];
         if( !zubr_s ) zubr_s = "illegal-symbol";
         printf( "zubr_debug: state %d, reading %d (%s)\n",
                 zubr_state, zubr_char, zubr_s );
      }
#endif
   } /*  End if( zubr_char < 0 )  */

   if( (zubr_n = zubr_sindex[zubr_state]) && (zubr_n += zubr_char) >= 0 &&
       zubr_n <= ZUBR_TABLESIZE && zubr_check[zubr_n] == zubr_char )
   {
#if ZUBR_DEBUG
      if( zubr_debug )
         printf( "zubr_debug: state %d, shifting to state %d\n",
                 zubr_state, zubr_table[zubr_n] );
#endif
      if( zubr_ssp >= zubr_ss + zubr_stacksize - 1 )
      {
         goto zubr_overflow;
      }
      *++zubr_ssp = zubr_state = zubr_table[zubr_n];
      *++zubr_vsp = zubr_lval;
      zubr_char = (-1);
      if( zubr_errflag > 0 ) --zubr_errflag;
      goto zubr_loop;
   }

   if( (zubr_n = zubr_rindex[zubr_state]) && (zubr_n += zubr_char) >= 0 &&
       zubr_n <= ZUBR_TABLESIZE && zubr_check[zubr_n] == zubr_char )
   {
      zubr_n = zubr_table[zubr_n];
      goto zubr_reduce;
   }
   if (zubr_errflag) goto zubr_inrecovery;
#ifdef not_defined_zubr_sccsid
   goto zubr_newerror;
#endif
zubr_newerror:
   zubr_error( "syntax error" );
#ifdef not_defined_zubr_sccsid
   goto zubr_errlab;
#endif
zubr_errlab:
   ++zubr_nerrs;
zubr_inrecovery:
   if( zubr_errflag < 3 )
   {
      zubr_errflag = 3;
      for( ;; )
      {
         if( (zubr_n = zubr_sindex[*zubr_ssp]) && (zubr_n += ZUBR_ERRCODE) >= 0 &&
             zubr_n <= ZUBR_TABLESIZE && zubr_check[zubr_n] == ZUBR_ERRCODE )
         {
#if ZUBR_DEBUG
            if( zubr_debug )
               printf( "zubr_debug: state %d, error recovery shifting\
 to state %d\n", *zubr_ssp, zubr_table[zubr_n] );
#endif
            if( zubr_ssp >= zubr_ss + zubr_stacksize - 1 )
            {
               goto zubr_overflow;
            }
            *++zubr_ssp = zubr_state = zubr_table[zubr_n];
            *++zubr_vsp = zubr_lval;
            goto zubr_loop;
         }
         else
         {
#if ZUBR_DEBUG
            if( zubr_debug )
               printf( "zubr_debug: error recovery discarding state %d\n",
                       *zubr_ssp );
#endif
            if( zubr_ssp <= zubr_ss ) goto zubr_abort;
            --zubr_ssp;
            --zubr_vsp;
         }
      } /*  End of for( ;; )  */
   } /*  End if( zubr_errflag < 3 )  */
   else
   {
      if( zubr_char == 0 ) goto zubr_abort;
#if ZUBR_DEBUG
      if( zubr_debug )
      {
         zubr_s = 0;
         if( zubr_char <= ZUBR_MAXTOKEN ) zubr_s = zubr_name[zubr_char];
         if( !zubr_s ) zubr_s = "illegal-symbol";
printf( "zubr_debug: state %d, error recovery discards token %d (%s)\n",
        zubr_state, zubr_char, zubr_s );
      }
#endif
      zubr_char = (-1);
      goto zubr_loop;
   }
zubr_reduce:
#if ZUBR_DEBUG
   if( zubr_debug )
      printf( "zubr_debug: state %d, reducing by rule %d (%s)\n",
              zubr_state, zubr_n, zubr_rule[zubr_n] );
#endif
   zubr_m   = zubr_len[zubr_n];
   zubr_val = zubr_vsp[1-zubr_m];

   switch( zubr_n )
   {
      case 2:
         {
            /*  пустое выражение*/
         }
         break;
      case 3:
         {
            /*  разобрано выражение, добавл€ющее */
            /*  значение параметра в таблицу*/
         }
         break;
      case 4:
         {
            /*  разобрано выражение, дающее вычислимый результат;*/
            /*  вывод результата*/
            expr_result = zubr_vsp[-1].val;
         }
         break;
      case 5:
         {
            /*  обнаружена ошибка (парсер не может свернуть выражение)*/
            success = PARSING_FAILURE;
            zubr_errok;
         }
         break;
      case 6:
         { 
                zubr_val.val = zubr_vsp[-2].sym->u.val = zubr_vsp[0].val; zubr_vsp[-2].sym->type = VAR; 
                if(success == PARSING_SUCCESS)success = ASSIGN_SUCCESS;
            }
         break;
      case 8:
         {
            /*  $1 указывает на голову очереди, $$ указывает на текущий результат*/
            if (zubr_vsp[0].sym->type == UNDEF)
            {
                success = PARAMS_UNDEFINED; /*  значени€ некот. переменных не определены*/
            }
            zubr_val.val = zubr_vsp[0].sym->u.val;         /*  результату присваиваетс€ значение переменной справа*/
         }
         break;
      case 9:
         {
            zubr_val.val = zubr_vsp[0].sym->u.val;         /*  результату присваиваетс€ значение переменной справа*/
         }
         break;
      case 11:
         { zubr_val.val = (*(zubr_vsp[-3].sym->u.ptr))(zubr_vsp[-1].val); }
         break;
      case 12:
         { zubr_val.val = zubr_vsp[-2].val + zubr_vsp[0].val; }
         break;
      case 13:
         { zubr_val.val = zubr_vsp[-2].val - zubr_vsp[0].val; }
         break;
      case 14:
         { zubr_val.val = zubr_vsp[-2].val * zubr_vsp[0].val; }
         break;
      case 15:
         { if( zubr_vsp[0].val != 0.0 ){ zubr_val.val = zubr_vsp[-2].val / zubr_vsp[0].val;} }
         break;
      case 16:
         { zubr_val.val = ZPow (zubr_vsp[-2].val, zubr_vsp[0].val); }
         break;
      case 17:
         { zubr_val.val =  zubr_vsp[-1].val; }
         break;
      case 18:
         { zubr_val.val = -zubr_vsp[0].val; }
         break;

   } /*  End of switch( zubr_n )  */

   zubr_ssp   -=  zubr_m;
   zubr_state  = *zubr_ssp;
   zubr_vsp   -=  zubr_m;
   zubr_m      =  zubr_lhs[zubr_n];

   if( zubr_state == 0 && zubr_m == 0 )
   {
#if ZUBR_DEBUG
      if( zubr_debug )
         printf( "zubr_debug: after reduction, shifting from state 0 to\
 state %d\n", ZUBR_FINAL );
#endif
      zubr_state  = ZUBR_FINAL;
      *++zubr_ssp = ZUBR_FINAL;
      *++zubr_vsp = zubr_val;
      if( zubr_char < 0 )
      {
         if( (zubr_char = zubr_lex()) < 0 ) zubr_char = 0;
#if ZUBR_DEBUG
         if( zubr_debug )
         {
            zubr_s = 0;
            if( zubr_char <= ZUBR_MAXTOKEN) zubr_s = zubr_name[zubr_char];
            if( !zubr_s ) zubr_s = "illegal-symbol";
            printf( "zubr_debug: state %d, reading %d (%s)\n",
                    ZUBR_FINAL, zubr_char, zubr_s);
         }
#endif
      }
      if( zubr_char == 0 ) goto zubr_accept;
      goto zubr_loop;
   } /*  End if( zubr_state == 0 && zubr_m == 0 )  */

   if( (zubr_n = zubr_gindex[zubr_m]) && (zubr_n += zubr_state) >= 0 &&
       zubr_n <= ZUBR_TABLESIZE && zubr_check[zubr_n] == zubr_state )
      zubr_state = zubr_table[zubr_n];
   else
      zubr_state = zubr_dgoto[zubr_m];
#if ZUBR_DEBUG
   if( zubr_debug )
      printf( "zubr_debug: after reduction, shifting from state %d \
to state %d\n", *zubr_ssp, zubr_state );
#endif
   if( zubr_ssp >= zubr_ss + zubr_stacksize - 1 )
   {
      goto zubr_overflow;
   }
   *++zubr_ssp = zubr_state;
   *++zubr_vsp = zubr_val;
   goto zubr_loop;
zubr_overflow:
   zubr_error( "zubr stack overflow" );
zubr_abort:
   return( 1 );
zubr_accept:
   return( 0 );
}
