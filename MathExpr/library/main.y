
/**************************************************************
  MAIN.Y
 **************************************************************/

 /* ??? ������� ��������� ���������� */

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
int char_count; //  ������ ���-�� �������� � ���������. ������� ������. 0
int stop;       //  ������� ���������� ������� ���������
int success;

%}

%union
{
   double  val;
   SYMBOL *sym;
}
%token <val>   NUMBER
%token <sym>   VAR VAR_BLTIN BLTIN UNDEF    /* VAR_BLTIN ������ ��� ����, ����� �������� ��������� (����������)
                                               �������� ������������� �� ���������������� �������� */
%type  <val>   expr assign
%right '='
%left '+' '-'   /* left associative, same precedence */
%left '*' '/'   /* left associative, higher precedence */
%left UNARYMINUS
%right '^'      /* power */
%%

//  ��������, ����������� ��������� �������� \n;
//  �������� ��������� ������� �� �������������, 
//  ��������� ������������ ����� ������. ������
list:    /* nothing */
       | list '\n'
         {
            //  ������ ���������
         }
       | list assign '\n'       
         {
            //  ��������� ���������, ����������� 
            //  �������� ��������� � �������
         }
       | list expr '\n'
         {
            //  ��������� ���������, ������ ���������� ���������;
            //  ����� ����������
            expr_result = $2;
         }
       | list error '\n'
         {
            //  ���������� ������ (������ �� ����� �������� ���������)
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
       | VAR    //  ������ VAR ������������� ���� UNDEF
         {
            //  $1 ��������� �� ������ �������, $$ ��������� �� ������� ���������
            if ($1->type == UNDEF)
            {
                success = PARAMS_UNDEFINED; //  �������� �����. ���������� �� ����������
            }
            $$ = $1->u.val;         //  ���������� ������������� �������� ���������� ������
         }
       | VAR_BLTIN
         {
            $$ = $1->u.val;         //  ���������� ������������� �������� ���������� ������
         }
       | assign                     //  ��������� assign � expr ��������, ��� ����� ������������ ������� ����������
       | BLTIN '(' expr ')'        { $$ = (*($1->u.ptr))($3); }
       | expr '+' expr             { $$ = $1 + $3; }
       | expr '-' expr             { $$ = $1 - $3; }
       | expr '*' expr             { $$ = $1 * $3; }

       | expr '/' expr             { if( $3 != 0.0 ){ $$ = $1 / $3;} }  //  ����� �� ��������, ����� �����. � ������ ����� �� ����������

       | expr '^' expr             { $$ = ZPow ($1, $3); }
       | '(' expr ')'              { $$ =  $2; }
       | '-' expr %prec UNARYMINUS { $$ = -$2; }
       ;
%%

/********************  END OF GRAMMAR  ***********************/

//  ������� ������������ ������ ������
void warning( char *s, char *t )
{
}

void zubr_error( char *s )
{
   warning( s, (char *)0 );
}
//  ����������� ����������
#define VAR_NAME_LEN    100         //  ������������ ����� �������������� ����������

int zubr_lex( void )
{
   int c;

   if(stop)return 0;
   MY_ASSERT(char_index < char_count);

   //   ���������� ������� � ����   
   while( (c=expr[char_index++]) == ' ' || c == '\t' );
   //   ������ '\0' ������ ��������� ��������� ������ �������,
   //   ������� ��� ������� ���������� �� '\n' - ������� �����
   //   ���������
   if( c == '\0' ) {stop = 1; return( '\n' );}
   if( c == '.' || isdigit( c ) )
   {
      int n = expr[char_index++];

      //  ����� ������������ ��� ����� 0.0
      //  ������� scanf() ������ �������� ����� "."
      if( c == '.' && !isdigit( n ) )
      {
         expr[--char_index] = n;
         zubr_lval.val = 0.0;
         return( NUMBER );
      }
      expr[--char_index] = n;

      //  ���������� ������ �����
      expr[--char_index] = c;
      //  ��������� ����� � ������������� ������ �� ��������� ������
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
      int var_name_index = 0;                       //  ����� ���������� � ����� ��������   

      do
      {//   �������� � ����. ����� ����� ����������
       //   ��� �� �.�. ������� VAR_NAME_LEN ��������;
       //   !!! ������� �� �������� �� ����. ����� ������ ������������� !!!
         
         if( var_name_index < VAR_NAME_LEN ) 
            sbuf[var_name_index++] = c;

         c = expr[char_index++];
      } while( ( char_index < char_count ) && isalnum( c ) );

      expr[--char_index] = c;                       //  ������� ���������� �������, ��
                                                    //  ������������ � ��������������
      sbuf[var_name_index] = '\0';                  //  ������ ������������ ���� ��������������
      my_strlwr( sbuf );                            //  ������ �� ������������ � �������� ��������

      if( (sp = lookup( sbuf ) ) == (SYMBOL *)0 )   //  ������ � ������� �������� �� ������� - 
         sp = install( sbuf, UNDEF, 0.0 );          //  ������������� ��� UNDEF
      zubr_lval.sym = sp;
      //    ����������� ���������� ���������� VAR,
      //    ����� �������� ���������� �� ������� � �������
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
