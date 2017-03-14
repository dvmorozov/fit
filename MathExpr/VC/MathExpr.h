
// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the MATHEXPR_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// MATHEXPR_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef MATHEXPR_EXPORTS
#define MATHEXPR_API __declspec(dllexport) 
#else
#define MATHEXPR_API __declspec(dllimport) 
#endif

extern "C"
{
extern MATHEXPR_API int nMathExpr;

MATHEXPR_API int ParseExpression(char* Expr, int* ExprId);
MATHEXPR_API int CalcExpression(int ExprId, char* ParamList, double* Result);
MATHEXPR_API int CalcExpression2(int ExprId, double* ParamList, int ParamCount, double* Result);
MATHEXPR_API int ParseAndCalcExpression(char* Expr, char* ParamList, double* Result);
MATHEXPR_API char* GetSymbols();
MATHEXPR_API void FreeSymbols(char *Symbols);

MATHEXPR_API int __stdcall ParseExpressionStd(char* Expr, int* ExprId);
MATHEXPR_API int __stdcall CalcExpressionStd(int ExprId, char* ParamList, double* Result);
MATHEXPR_API int __stdcall CalcExpression2Std(int ExprId, double* ParamList, int ParamCount, double* Result);
MATHEXPR_API int __stdcall ParseAndCalcExpressionStd(char* Expr, char* ParamList, double* Result);
MATHEXPR_API int __stdcall ParseAndCalcExpressionStd(char* Expr, char* ParamList, double* Result);
MATHEXPR_API char* __stdcall GetSymbolsStd();
MATHEXPR_API void __stdcall FreeSymbolsStd(char *Symbols);
}
