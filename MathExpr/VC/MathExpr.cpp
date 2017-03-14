//--------------------------------------------------------------------------------------------------
//  MathExpr.cpp : Defines the entry point for the DLL application.
//
//  - �������������� ����� ���������� ���������, ������ ��� ��� ���� C++
//  - ������ ������ � ���� dll, � �� ���������� ������ ��� ��� �����
//  ��������� ��������� �-� �������� (�������� ����������� ���������)
//  - ����������� ������ �������� ������ ������� ��� ������ � �������������
//  ����� (����� ������� �����������, �������� ��������� ������ ��� �������
//  ������, �.�. ������� ��� ������ ������� � ��� ��������� � ������� �� �
//  ����� ������, �� ��� ���� ��������� �������� ������ ������� �-� ��������)
//  - ������� (��������� ������ ����������):
//      - ��������� �������� ���������� � ���������� ������� ���������
//      - ��������� ��������� � ����� ����; ��������� � ����������� �����������
//      ���������� ������� ����� ���������
//      - ����������
//  - ������� ������ ����������:
//      - ���������, ������ � ����. � ����. ����� ��������� � ����� ����
//      - ��������� � ������ ��������� ��� ���������� � ���������� (�������
//      �.�. ������������) ��� �������� �������� � ���� �������� ����� (�������
//      �.�. ����������)
//      - ����������
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
//  ���������� PARSING_SUCCESS � ������ ��������� ����������, 
//	ASSIGN_SUCCESS � ������ ��������� ���������� �������� ���������,
//	PARSING_FAILURE � ������ ������, 
//  PARAMS_UNDEFINED ���� �������� ���������� �� ������
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
            init();     //  ��������. ����. ��� �������� � �������
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

//  ����������� ��������� �. ����������� � ������������� 
//  �����, ����� �� ��������� ����� ������ ���
//  ��������� ����������� � ������ ��� ����������� 
//  �������� � ����������� �� ����� ������ ���������
typedef struct
{
    char    *expr;      //  ����������� �������� ���������
    int     expr_id;    //  ������������� ������� ���������, �������� ���������� ���������
    void    *next;
} parsed_expression;

parsed_expression *all_expressions = 0; //  ��������� ���� ����������� ���������
int last_id = 0;                        //  ��������� ������������� ����������� ������������ ���������
                                        //  0 - ������� ����, ��� ������ ��������� ����
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

    //  �����������
    //MY_ASSERT(h);
    while(h)
    {
        cur = (parsed_expression*)h->next;
        free_expression(h);
        h = cur;
    }
}
//--------------------------------------------------------------------------------------------------
//  ���������� 0 � ������ �������� ��������
void add_expression(char* expression, int expr_id, int *result)
{
    parsed_expression *h, *cur_expr;

    MY_ASSERT(result);
    *result = 0;

    //  �������� ������ ��������� � �������������
    h = (parsed_expression*)alloc_expression();
    MY_ASSERT(h);
    if(!h)return;

    h->expr = (char*)my_malloc(my_strlen(expression) + 1);
    if(!h->expr){my_free(h); return;}

    my_strcpy(h->expr, expression);
    h->expr_id = expr_id;
    
    if(!all_expressions)all_expressions = h;
    else
    {// ����� ����� �������
        cur_expr = all_expressions;
        while(cur_expr->next)cur_expr = (parsed_expression*)cur_expr->next;
        cur_expr->next = h;
    }
    *result = 1;
}
//--------------------------------------------------------------------------------------------------
//  ����� ��������� �� �����.
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
//  ��������� ������ ����������, ������ � ���������� ���������
MATHEXPR_API int ParseAndCalcExpression(char* Expr, char* ParamList, double* Result)
{
    int parsing_result;
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! ���������� �� ������ �������� �� ������������ ������� !!!
	try
	{
		*Result = 0.0;
		//  ������ ��������� ��� ����������;
		//  ��������� ���������� � ���� ������, ������������� ������� �����;
		//  ��������� ��������� ��� ���������� ����������� ������
		while(my_strlen(ParamList))
		{
			//  Result ����� ������ �� ������
			parsing_result = parse_expression(ParamList, Result);
			if(!parsing_result)break;
			ParamList = ParamList + my_strlen(ParamList) + 1;
		}
    
		//  ������ � ���������� ��������� ���������
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
//  ��������� ��������������� ������ ��������� � ��������� ���
//  � ������������� ����� - ������ ���� �������� ������� �������
//  ���������� 1, ���� ��������� ������� ���������
//  � ExprId ������������ ������������� ������������ ���������
MATHEXPR_API int ParseExpression(char* Expr, int* ExprId)
{
    int result;

    MY_ASSERT(Expr);
    MY_ASSERT(ExprId);
    //  ��������� �-� ������������, �� ������ ����
    //  ��������� ������������ �������� ����������
    if(!Expr || !ExprId)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! ���������� �� ������ �������� �� ������������ ������� !!!
	try
	{
		*ExprId = last_id + 1;
		add_expression(Expr, *ExprId, &result);
		if(result)  //  ��������� ����� �������������
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
//  ��������� ������ ��������� ��� ���������� � ���������� � 
//  ��������� �������� �������������� ����������� ��������� -
//  ������ ���� �������� ������� �������
MATHEXPR_API int CalcExpression(int ExprId, char* ParamList, double* Result)
{
    int parsing_result;
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! ���������� �� ������ �������� �� ������������ ������� !!!
	try
	{
		*Result = 0.0;
		//  ����� ��������� �� �����.
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
//  ��������� �������� �������������� ����������� ��������� -
//  ������ ���� �������� ������� �������
MATHEXPR_API int CalcExpression2(int ExprId, double* ParamList, int ParamCount, double* Result)
{
    MY_ASSERT(ParamList);
    MY_ASSERT(Result);
    if(!ParamList || !Result)return PARSING_FAILURE;

    EnterCriticalSection(&cs);
	//	!!! ���������� �� ������ �������� �� ������������ ������� !!!
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
//  ���������� ������ ���� ��������, ������������� ������� �����,
//  � ������� ��������� ����� ��������� ������
//??? ������ � ���������� ����� ���������� �������� (����� ������� ��-��)
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
					//  !!! ������ ������ ����� ����� 2 ������. ����
					//  ��� ���������� ������ my_strcatz !!!
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
		//	my_free �� ����������, ����� �� ���������
		//	���������� ������������� ����������
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