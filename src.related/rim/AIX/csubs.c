/* C subroutines for AIX Rim */

#include <stdio.h>

/* issue prompt */

cprompt(str,l)
char *str;
int  *l;
{
   str[*l] = '\0';
   fputs(str,stdout);
}

/* return date */

#include <time.h>

czdate(y,m,d)
int *y, *m, *d;
{
   struct tm *t;
   int it;

   t = localtime(&it);
   *y = t->tm_year + 1900;
   *m = t->tm_mon + 1;
   *d = t->tm_mday;
}

/* return time */

cztime(h,m,s)
int *h, *m, *s;
{
   struct tm *t;
   int it;

   t = localtime(&it);
   *h = t->tm_hour;
   *m = t->tm_min;
   *s = t->tm_sec;
}


/* catch interrupts */

#include <sys/signal.h>

extern int rimstp;    /* rim common holding hxflag */

czsigl_catch()
{
   czsigl();
   rimstp++;
}

czsigl()
{
   signal(SIGINT,czsigl_catch);
}
