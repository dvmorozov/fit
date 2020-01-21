//--------------------------------------------------------------------------------------------------
//  MathExpr.cpp : Defines the entry point for the DLL application.
//
//  - преобразование типов указателей требуется, потому что это файл C++
//  - парсер сделан в виде dll, а не приложения потому что так можно
//  сохранять состояние м-у вызовами (например разобранные выражения)
//  - критическая секция защищает данные парсера при работе в многопоточной
//  среде (можно сделать эффективнее, создавая отдельный парсер для каждого
//  потока, т.е. собрать все данные парсера в ону структуру и хранить ее в
//  стеке потока, но при этом возникает проблема обмена данными м-у потоками)
//  - простой (медленный способ вычисления):
//      - установка значений переменной и параметров набором выражений
//      - установка выражения в общем виде; совместно с предыдущими выражениями
//      получается частная форма выражения
//      - вычисление
//  - быстрый способ вычисления:
//      - установка, разбор и сохр. в пром. форме выражения в общем виде
//      - установка и разбор выражений для параметров и переменной (порядок
//      м.б. произвольным) или загрузка значений в виде двоичных чисел (порядок
//      д.б. фиксирован)
//      - вычисление
//--------------------------------------------------------------------------------------------------

#include "stdafx.h"
#include "MathExpr.h"
#include "..\symtab.h"
#include "..\debug.h"
#include "..\environment.h"
#include "..\z_tab.h"
//--------------------------------------------------------------------------------------------------

extern "C"
{
extern void init(void);
//  возвращает PARSING_SUCCESS в случае успешного вычисления, 
//	ASSIGN_SUCCESS в случае успешного присвоения значения параметру,
//	PARSING_FAILURE в случае ошибки, 
//  PARAMS_UNDEFINED если значения параметров не заданы
extern int parse_expression(char* expr, double* result);
extern SYMBOL *symlist;
}

CRITICAL_SECTION cs;
//--------------------------------------------------------------------------------------------------

BOOL APIENTRY DllMain( 
    HANDLE hModule, 
    DWORD  ul_reason_for_call, 
    LPVOID lpReserved
    )
{
    switch (ul_reason_for_call)
    {
        case DLL_PROCESS_ATTACH:
            InitializeCriticalSection(&cs);
            init();     //  первонач. иниц. при загрузке в процесс
            break;
        case DLL_THREAD_ATTACH:
        case DLL_THREAD_DETACH:
            break;
        case DLL_PROCESS_DETACH:
            DeleteCriticalSection(&cs);
            break;
    }
    return TRUE;
}
//--------------------------------------------------------------------------------------------------

// This is an example of an exported variable
//MATHEXPR_API int nMathExpr=0;

//  Разобранное выражение д. сохраняться в промежуточной 
//  форме, чтобы не разбирать текст каждый раз
//  Выражения добавляются в список без возможности 
//  удаления и сохраняются до конца работы программы
typedef struct
{
    char    *expr;      //  сохраненное исходное выражение
    int     expr_id;    //  идентификатор данного выражения, отданный вызывающей программе
    void    *next;
} parsed_expression;

parsed_expression *all_expressions = 0; //  связанная цепь разобранных выражений
int last_id = 0;                        //  последний идентификатор присвоенный разобранному выражению
                                        //  0 - признак того, что список выражений пуст
//--------------------------------------------------------------------------------------------------

static void *alloc_expression(void)
{
    parsed_expression *result;
    
    result = (parsed_expression*)my_malloc(sizeof(parsed_expression));
    if(result)
    {
        zero_memory(result, sizeof(parsed_expression));
    }
    return result;
}
//--------------------------------------------------------------------------------------------------

static void free_expression(parsed_expression *b)
{
    MY_ASSERT(b);

    my_free(b);
}
//--------------------------------------------------------------------------------------------------

static void free_expression_chain(parsed_expression *h)
{
    parsed_expression *cur;

    //  допускается
    //MY_ASSERT(h);
    while(h)
    {
        cur = (parsed_expression*)h->next;
        free_expression(h);
        h = cur;
    }
}
//--------------------------------------------------------------------------------------------------
//  возвращает 0 в случае нехватки ресурсов
void add_expression(char* expression, int expr_id, int *result)
{
    parsed_expression *h, *cur_expr;

    MY_ASSERT(result);
    *result = 0;

    //  создание записи выражения и инициализация
    h = (parsed_expression*)alloc_expression();
    MY_ASSERT(h);
    if(!h)return;

    h->expr = (char*)my_malloc(my_strlen(expression) + 1);
    if(!h->expr){my_free(h); return;}

    my_strcpy(h->expr, expression);
    h->expr_id = expr_id;
    
    if(!all_expressions)all_expressions = h;
    else
    {// поиск конца цепочки
        cur_expr = all_expressions;
        while(cur_expr->next)cur_expr = (parsed_expression*)cur_expr->next;
        cur_expr->next = h;
    }
    *result = 1;
}
//--------------------------------------------------------------------------------------------------
//  поиск выражения по идент.
parsed_expression *find_expression(int expr_id)
{
    parsed_expression *cur_expr;

    if(!all_expressions)return 0;
    else
    {
        cur_expr = all_expressions;
        do
        {
            if(cur_expr->expr_id == expr_id)return cur_expr;
            cur_expr = (parsed_expression*)cur_expr->next;
        }
        while(cur_expr);
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
//  выполняет разбор параметров, разбор и вычисление выражения
MATHEXPR_API int ParseAndCalcExpression(char* Expr, char* ParamList, double* Result)
{
    int parsing_result;
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! исключения не должны выходить из интерфейсных методов !!!
	try
	{
		*Result = 0.0;
		//  разбор выражений для параметров;
		//  параметры передаются в виде строки, завершающейся двойным нулем;
		//  отдельные выражения для параметров разделяются нулями
		while(my_strlen(ParamList))
		{
			//  Result здесь ничего не значит
			parsing_result = parse_expression(ParamList, Result);
			if(!parsing_result)break;
			ParamList = ParamList + my_strlen(ParamList) + 1;
		}
    
		//  разбор и вычисление основного выражения
		if(parsing_result)
			parsing_result = parse_expression(Expr, Result);
	}
	catch(...)
	{
		parsing_result = PARSING_FAILURE;
	}
    
    LeaveCriticalSection(&cs);
    return parsing_result;
}
//--------------------------------------------------------------------------------------------------
//  Выполняет предварительный разбор выражения и сохраняет его
//  в промежуточной форме - первый этап быстрого способа расчета
//  Возвращает 1, если выражение успешно разобрано
//  В ExprId возвращается идентификатор разобранного выражения
MATHEXPR_API int ParseExpression(char* Expr, int* ExprId)
{
    int result;

    MY_ASSERT(Expr);
    MY_ASSERT(ExprId);
    //  поскольку ф-и интерфейсные, то должна быть
    //  обработка недопустимых значений параметров
    if(!Expr || !ExprId)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! исключения не должны выходить из интерфейсных методов !!!
	try
	{
		*ExprId = last_id + 1;
		add_expression(Expr, *ExprId, &result);
		if(result)  //  фиксируем новый идентификатор
		{
			last_id = last_id + 1;
			result = PARSING_SUCCESS;
		}
	}
	catch(...)
	{
		result = PARSING_FAILURE;
	}
    
    LeaveCriticalSection(&cs);
    return result;
}
//--------------------------------------------------------------------------------------------------
//  Выполняет разбор выражений для параметров и переменной и 
//  вычисляет исходное предварительно разобранное выражение -
//  второй этап быстрого способа расчета
MATHEXPR_API int CalcExpression(int ExprId, char* ParamList, double* Result)
{
    int parsing_result;
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! исключения не должны выходить из интерфейсных методов !!!
	try
	{
		*Result = 0.0;
		//  поиск выражения по идент.
		parsed_expression *expr = find_expression(ExprId);
		MY_ASSERT(expr);
		if(!expr)return PARSING_FAILURE;

		parsing_result = parse_expression(expr->expr, Result);
	}
	catch(...)
	{
		parsing_result = PARSING_FAILURE;
	}
    
    LeaveCriticalSection(&cs);
    return parsing_result;
}
//--------------------------------------------------------------------------------------------------
//  Вычисляет исходное предварительно разобранное выражение -
//  второй этап быстрого способа расчета
MATHEXPR_API int CalcExpression2(int ExprId, double* ParamList, int ParamCount, double* Result)
{
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! исключения не должны выходить из интерфейсных методов !!!
	try
	{
		//???
	}
	catch(...)
	{

	}

    LeaveCriticalSection(&cs);

    return PARSING_FAILURE;
}
//--------------------------------------------------------------------------------------------------
//  Возвращает строку имен символов, завершающуюся двойным нулем,
//  в которой отдельные имена разделены нулями
//??? вместе с названиями нужно возвращать значения (через внешний ук-ль)
MATHEXPR_API char* GetSymbols()
{
    SYMBOL *sp;
    char *temp, *temp2;
    int first = 1;

    temp = 0;

	try
	{
		for(sp = symlist; sp != (SYMBOL *)0; sp = sp->next)
		{
			if((sp->type != BLTIN) && (sp->type != VAR_BLTIN))
			{
				if(first)
				{
					int len = my_strlen(sp->name) + 2;
					temp = (char*)my_malloc(len);
					if(!temp)return 0;
					my_strcpy(temp, sp->name);
					//  !!! строка должна сразу иметь 2 заверш. нуля
					//  для правильной работы my_strcatz !!!
					temp[len - 1] = 0;
					first = 0;
				}
				else
				{
					temp2 = my_strcatz(temp, sp->name);
					my_free(temp);
					if(!temp2)return 0;
					temp = temp2;
				}
			}
		}
	}
	catch(...)
	{
		//	my_free не вызывается, чтобы не допустить
		//	повторного возникновения исключения
		temp = 0;
	}

    return temp;
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API void FreeSymbols(char *Symbols)
{
    if(Symbols)my_free(Symbols);
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API int __stdcall ParseAndCalcExpressionStd(char* Expr, char* ParamList, double* Result)
{
    return ParseAndCalcExpression(Expr, ParamList, Result);
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API int __stdcall ParseExpressionStd(char* Expr, int* ExprId)
{
    return ParseExpression(Expr, ExprId);
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API int __stdcall CalcExpressionStd(int ExprId, char* ParamList, double* Result)
{
    return CalcExpression(ExprId, ParamList, Result);
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API int __stdcall CalcExpression2Std(int ExprId, double* ParamList, int ParamCount, double* Result)
{
    return CalcExpression2(ExprId, ParamList, ParamCount, Result);
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API char* __stdcall GetSymbolsStd()
{
    return GetSymbols();
}
//--------------------------------------------------------------------------------------------------

MATHEXPR_API void __stdcall FreeSymbolsStd(char *Symbols)
{
    FreeSymbols(Symbols);
}
//--------------------------------------------------------------------------------------------------