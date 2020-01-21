//--------------------------------------------------------------------------------------------------
#ifndef environment_h
#define environment_h
//--------------------------------------------------------------------------------------------------

#ifdef __cplusplus
extern "C"
{
#endif  //  __cplusplus

extern void* my_malloc(int size);
extern void my_free(void* ptr);
extern void zero_memory(void *ptr, int size);
extern void my_memcpy(void *dest, void *src, int size);
extern int my_strlen(char *str);
extern char* my_strcpy(char *dest, char* src);
extern int my_strcmp(const char *string1, const char *string2);
extern char* my_strcatz(char *str1, char *str2);
extern char* my_strlwr(char *str);

#define PARSING_SUCCESS		1
#define ASSIGN_SUCCESS		2
#define PARSING_FAILURE		0
#define PARAMS_UNDEFINED	(-1)

#ifdef __cplusplus
}
#endif  //  __cplusplus
//--------------------------------------------------------------------------------------------------
#endif  //  environment_h
//--------------------------------------------------------------------------------------------------
