#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "stdtypes.H"		 
#include "checks.H"

#define buildmsg() char msg[256];va_list argptr;va_start(argptr,fmt);vsprintf(msg,fmt,argptr);va_end(argptr)


int fatal_box(const char* fmt, ...)
{
  buildmsg();

  fprintf(stderr, "%s\n", msg);
  getchar();
  exit(1);

  return 0;
}


int error_box(const char* fmt, ...)
{
  buildmsg();

  fprintf(stderr, "%s\n", msg);
  getchar();

  return 0;
}


int warning_box(const char* fmt, ...)
{
  buildmsg();
}


int message_box(const char* fmt, ...)
{
  buildmsg();

  return 0;
}

int sorry_box(const char* fmt, ...)
{
  buildmsg();

  return 0;
}

int yesno_box(const char* fmt, ...)
{
  buildmsg();
  return 0;
}


int yesnofatal_box(const char* fmt, ...)
{
  buildmsg();

  char s[256]; sprintf(s, "%s\nContinuare ugualmente?", msg);
  const int ret = yesno_box("%s", s);
  if (!ret) fatal_box("");
  return FALSE;
}



int yesnocancel_box(const char* fmt, ...)
{
  buildmsg();

  return 0;
}
