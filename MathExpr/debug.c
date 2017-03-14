//-------------------------------------------------------------------------------------------------

#include "debug.h"
//-------------------------------------------------------------------------------------------------

#include <windows.h>
#include <assert.h>
//-------------------------------------------------------------------------------------------------

#ifdef _DEBUG

void my_assert(char* exp, char* file, unsigned int line)
{
    _assert(exp, file, line);
}
//-------------------------------------------------------------------------------------------------

void my_trace0(const char* file, const unsigned int line)
{
    char str[13];
    wsprintf(str, " %i\n", line);
    OutputDebugString(file);
    OutputDebugString(str);
}
//-------------------------------------------------------------------------------------------------

void my_trace_int(const int val, const char* file, const unsigned int line)
{
    char str[13];
    wsprintf(str, "%i\n", val);
    OutputDebugString(str);
}
//-------------------------------------------------------------------------------------------------

void my_trace_str_label(
    const char* str, const char* file, const unsigned int line)
{
    OutputDebugString(str);
}
#endif  //  _DEBUG
//-------------------------------------------------------------------------------------------------



