/**********************************************************************
   error.c      - report any error (fatal, warning, etc.)
 *********************************************************************/

#include "xc.xclip.h"
#include <varargs.h>
#include <setjmp.h>

extern XclipGlobalData *zzGlobal;
extern jmp_buf backHere;

zzerror(s) 
char *s; 
{
    extern int zzlineno;
    extern char zztext[];
    extern XclipGlobalData *zzGlobal;

    fprintf(stderr,"\n\"%s\", line %d:\n<%s>",zzGlobal->scriptFile,zzlineno,s);

    fprintf(stderr," at or near \"%s\"\n",zztext);

    if (zzGlobal->standAlone)
        exit(0);
    else
	longjmp(backHere,1);
}

XCWarning(s,t,Global)
    char *s, *t;
    XclipGlobalData *Global;
{
    fprintf(stderr,"%s: WARNING <%s>\n\t%s\n",Global->progName,s,t);
}

XCError(s,t,Global)
    char *s, *t;
    XclipGlobalData *Global;
{
    fprintf(stderr,"%s: ERROR <%s>\n\t%s\n",Global->progName,s,t);
}

XCFatalError(s,t)
    char *s, *t;
{
    fprintf(stderr,"xclip: FATAL ERROR <%s>\n\t%s\n",s,t);
    if (zzGlobal->standAlone)
        exit(1);
}

void
XCInterrupt()
{
    XCFatalError("Caught signal causing death","Later Dude!!");
}

XCUsage(s)
char *s;
{
    char *blanks, *ptr;
    int i;

    ptr = blanks = XtMalloc(strlen(s)+1);
    for ( i = 0; i < strlen(s); i++ )
        *ptr++ = ' ';
    *ptr = '\0';

    fprintf(stderr,"usage: %s [-v][-|+c][-help][-patch][-version]\n",s);
    fprintf(stderr,"       %s [-Idir][-Dname][-Dname=def]\n",blanks);
    fprintf(stderr,"       %s [-flag name:\"on\"|\"off\"]*[-parm name:value]*\n"
	,blanks);
    fprintf(stderr,"       %s [scriptname]\n",blanks);
    fprintf(stderr,"where:\n");
    fprintf(stderr,"        -v - verbose mode\n");
    fprintf(stderr,"      +/-c - do/do not capture output (overrides script)\n");
    fprintf(stderr,"     -help - print this message\n");
    fprintf(stderr,"    -patch - print version (patchlevel) information\n");
    fprintf(stderr,"  -version -                ditto\n");
    fprintf(stderr,"     -Idir - add dir to cpp(1) include search list\n");
    fprintf(stderr,"    -Dname - define name as 1 (true) to cpp(1)\n");
    fprintf(stderr,"-Dname=def - define name as def to cpp(1)\n");
    fprintf(stderr,"     -flag - default flag \"name\" to \"on\" or \"off\"\n");
    fprintf(stderr,"     -parm - default parameter \"name\" to \"value\"\n");
    fprintf(stderr,"\nNOTE: -flag and -parm can occur multiple times\n");
    if (zzGlobal->standAlone)
        exit(1);
}

DoFprintf(va_alist)
va_dcl
{
    va_list argp;
    int level, i;
    char *fmt;

    va_start(argp);
    level = va_arg(argp, int);
    for ( i = 0; i < level; i++)
	fprintf(stderr,"\t");
    fmt = va_arg(argp,char *);
    vfprintf(stderr,fmt,argp);
    va_end(argp);

}
