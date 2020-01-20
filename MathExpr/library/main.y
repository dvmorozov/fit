
/**************************************************************
  MAIN.Y
 **************************************************************/

 /* ??? сделать обработку исключений */

%{

#include "init.h"
#include "symtab.h"
#include "mathf.h"
#include <ctype.h>

#include "debug.h"
#include "environment.h"

double expr_result;
char *expr;
int char_index;
int char_count; //  полное кол-во символов в выражении. включая заверш. 0
int stop;       //  признак завершения разбора выражения
int success;

%}

%union
{
   double  val;
   SYMBOL *sym;
}
%token <val>   NUMBER
%token <sym>   VAR VAR_BLTIN BLTIN UNDEF    /* VAR_BLTIN введен для того, чтобы отличать константы (переменные)
                                               заданные пользователем от предопределенных констант */
%type  <val>   expr assign
%right '='
%left '+' '-'   /* left associative, same precedence */
%left '*' '/'   /* left associative, higher precedence */
%left UNARYMINUS
%right '^'      /* power */
%%

//  символом, завершающим выражение является \n;
//  корневое выражение никогда не сворачивается, 
//  поскольку представляет собой бескон. список
list:    /* nothing */
       | list '\n'
         {
            //  пустое выражение
         }
       | list assign '\n'       
         {
            //  разобрано выражение, добавляющее 
            //  значение параметра в таблицу
         }
       | list expr '\n'
         {
            //  разобрано выражение, дающее вычислимый результат;
            //  вывод результата
            expr_result = $2;
         }
       | list error '\n'
         {
            //  обнаружена ошибка (парсер не может свернуть выражение)
            success = PARSING_FAILURE;
            zubr_errok;
         }
       ;

assign:  VAR '=' expr 
            { 
                $$ = $1->u.val = $3; $1->type = VAR; 
                if(success == PARSING_SUCCESS)success = ASSIGN_SUCCESS;
            }

expr:    NUMBER
       | VAR    //  символ VAR соответствует типу UNDEF
         {
            //  $1 указывает на голову очереди, $$ указывает на текущий результат
            if ($1->type == UNDEF)
            {
                success = PARAMS_UNDEFINED; //  значения некот. переменных не определены
            }
            $$ = $1->u.val;         //  результату присваивается значение переменной справа
         }
       | VAR_BLTIN
         {
            $$ = $1->u.val;         //  результату присваивается значение переменной справа
         }
       | assign                     //  вхождение assign в expr означает, что могут существовать цепочки присвоений
       | BLTIN '(' expr ')'        { $$ = (*($1->u.ptr))($3); }
       | expr '+' expr             { $$ = $1 + $3; }
       | expr '-' expr             { $$ = $1 - $3; }
       | expr '*' expr             { $$ = $1 * $3; }

       | expr '/' expr             { if( $3 != 0.0 ){ $$ = $1 / $3;} }  //  иначе не работает, когда перем. в правой части не определена

       | expr '^' expr             { $$ = ZPow ($1, $3); }
       | '(' expr ')'              { $$ =  $2; }
       | '-' expr %prec UNARYMINUS { $$ = -$2; }
       ;
%%

/********************  END OF GRAMMAR  ***********************/

//  функция формирования строки ошибки
void warning( char *s, char *t )
{
}

void zubr_error( char *s )
{
   warning( s, (char *)0 );
}
//  лексический анализатор
#define VAR_NAME_LEN    100         //  максимальная длина идентификатора переменной

int zubr_lex( void )
{
   int c;

   if(stop)return 0;
   MY_ASSERT(char_index < char_count);

   //   пропускаем пробелы и табы   
   while( (c=expr[char_index++]) == ' ' || c == '\t' );
   //   символ '\0' служит признаком остановки работы парсера,
   //   поэтому при встрече заменяется на '\n' - признак конца
   //   выражения
   if( c == '\0' ) {stop = 1; return( '\n' );}
   if( c == '.' || isdigit( c ) )
   {
      int n = expr[char_index++];

      //  точка распознается как число 0.0
      //  функции scanf() нельзя отдавать число "."
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
      n = sscanf( &expr[char_index], "%lf", &zubr_lval.val );
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
       //   имя не м.б. длиннее VAR_NAME_LEN символов;
       //   !!! символы не входящие во врем. буфер просто отбрасываются !!!
         
         if( var_name_index < VAR_NAME_LEN ) 
            sbuf[var_name_index++] = c;

         c = expr[char_index++];
      } while( ( char_index < char_count ) && isalnum( c ) );

      expr[--char_index] = c;                       //  возврат последнего символа, не
                                                    //  относящегося к идентификатору
      sbuf[var_name_index] = '\0';                  //  запись завершающего нуля идентификатора
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
