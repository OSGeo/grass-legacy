#ifndef __UTILITY_H
#define __UTILITY_H

#ifndef __STDTYPES_H
#include <stdtypes.H>
#endif

/* @FPUB */
char* format (const char* fmt, ...);
const char* cmd2name(const char* argv0, const char* argv1 = "");
int rtoi(const char * roman);
const char* itor(int i);
bool fcopy(const char* orig, const char* dest);
bool fexist(const char* file);
const char * encode(const char * data);
const char * decode(const char * data);

int  stricmp(const char*, const char*);

const char* esc(const char*);

#define ODD(x)   (x & 1)
#define EVEN(x) !(x & 1) 

#ifdef __UTILITY_CPP
char __tmp_string[1024];
#else
extern char __tmp_string[1024];
#endif


/* @END */
#endif // __UTILITY_H

