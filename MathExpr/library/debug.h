//--------------------------------------------------------------------------------------------------

#ifndef debug_h
#define debug_h
//--------------------------------------------------------------------------------------------------

#include <assert.h>

//  !!! чтобы можно было вызывать ф-и модуля из C-файлов !!!
#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _DEBUG
#define MY_ASSERT(exp) assert(exp)
#else
#define MY_ASSERT(exp)
#endif

#ifdef _UNLIMITED_RESOURCES
#define MY_UNLRES_ASSERT(exp) MY_ASSERT(exp)
#else
#define MY_UNLRES_ASSERT(exp)
#endif

#ifdef _DEBUG
#define MY_VERIFY(exp) MY_ASSERT(exp)
#else
#define MY_VERIFY(exp) exp
#endif

void my_trace0(const char*, const unsigned int);
void my_trace_str_label(const char*, const char*, const unsigned int);
void my_trace_int(const int, const char*, const unsigned int);

#ifdef _DEBUG
#define MY_TRACE0 (my_trace0(__FILE__, __LINE__))
#define MY_TRACE_STR(str) (my_trace_str_label(str, __FILE__, __LINE__))
#define MY_TRACE_INT(val) (my_trace_int(val, __FILE__, __LINE__))
#else
#define MY_TRACE0
#define MY_TRACE_STR(str)
#define MY_TRACE_INT(val)
#endif

#ifdef __cplusplus
}
#endif

//--------------------------------------------------------------------------------------------------
#endif  //  debug_h
//--------------------------------------------------------------------------------------------------

