//--------------------------------------------------------------------------------------------------
//  данный модуль позволяет исключить использование C-библиотеки
//--------------------------------------------------------------------------------------------------

#include "debug.h"
//--------------------------------------------------------------------------------------------------

#include <Windows.h>
//--------------------------------------------------------------------------------------------------

void* my_malloc(int size)
{
    MY_ASSERT(size);
    return HeapAlloc(GetProcessHeap(), HEAP_NO_SERIALIZE, size);
}
//--------------------------------------------------------------------------------------------------

void my_free(void* ptr)
{
    MY_ASSERT(ptr);
    MY_VERIFY(HeapFree(GetProcessHeap(), HEAP_NO_SERIALIZE, ptr));
}
//--------------------------------------------------------------------------------------------------

void zero_memory(void *ptr, int size)
{
    int i;
    MY_ASSERT(ptr);
    MY_ASSERT(size);
    //ZeroMemory(ptr, size);
    //memset(ptr, 0, size);
    for(i = 0; i < size; i++)
    {
        ((unsigned char*)ptr)[i] = 0;
    }
}
//--------------------------------------------------------------------------------------------------

void my_memcpy(void *dest, void *src, int size)
{
    int i;
    MY_ASSERT(dest);
    MY_ASSERT(src);
    MY_ASSERT(size);
    //CopyMemory(dest, src, size);
    //memcpy(dest, src, size);
    for(i = 0; i < size; i++)
    {
        ((unsigned char*)dest)[i] = ((unsigned char*)src)[i];
    }
}
//--------------------------------------------------------------------------------------------------

int my_strlen(char *str)
{
    MY_ASSERT(str);
    return lstrlen(str);
}
//--------------------------------------------------------------------------------------------------

char* my_strcpy(char *dest, char *src)
{
    MY_ASSERT(dest);
    MY_ASSERT(src);
    return lstrcpy(dest, src);
}
//--------------------------------------------------------------------------------------------------
//  !!! работает не так как стандартная функция strcat !!!
char* my_strcat(char *str1, char *str2)
{
    char *result = my_malloc(my_strlen(str1) + my_strlen(str2) + 1);
    if(result)
    {
        my_strcpy(result, str1);
        my_strcpy(result + my_strlen(str1), str2);
    }
    return result;
}
//--------------------------------------------------------------------------------------------------
//  складывает строки вместе с завершающими нулями
char* my_strcatz(char *str1, char *str2)
{
    int len = 0;
    char *result;
    char *saved = str1;

    while(my_strlen(str1))
    {
        len += my_strlen(str1) + 1;
        str1 += my_strlen(str1) + 1;
    }
    str1 = saved;

    len += my_strlen(str2) + 2;
    result = my_malloc(len); result[0] = 0;
    saved = result;

    if(result)
    {
        while(my_strlen(str1))
        {
            my_strcpy(result, str1);
            result += my_strlen(str1) + 1;
            str1 += my_strlen(str1) + 1;
        }
        my_strcpy(result, str2);
        result = saved;
        result[len - 1] = 0;
    }
    return result;
}
//--------------------------------------------------------------------------------------------------

int my_strcmp(const char *string1, const char *string2)
{
    return lstrcmp(string1, string2);
}
//--------------------------------------------------------------------------------------------------

char* my_strlwr(char *str)
{
    _strlwr_s(str, strlen(str));
    return str;
}
//--------------------------------------------------------------------------------------------------
