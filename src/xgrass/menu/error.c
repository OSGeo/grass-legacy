/**********************************************************************
   error.c      - report any error (fatal, warning, etc.)
 *********************************************************************/

#include "xgrass.h"

yyerror(s) 
char *s; 
{
    extern int yylineno;
    extern char yytext[];

    fprintf(stderr,"\n\"%s\", line %d:\n<%s>",_XG_Global.menuData.specFile,
	yylineno,s);

    fprintf(stderr," at or near \"%s\"\n",yytext);

    exit(0);
}

Warning(s,t)
    char *s, *t;
{
    fprintf(stderr,"%s: WARNING <%s>\n\t%s\n",_XG_Global.progName,s,t);
}

Error(s,t)
    char *s, *t;
{
    fprintf(stderr,"%s: ERROR <%s>\n\t%s\n",_XG_Global.progName,s,t);
}

FatalError(s,t)
    char *s, *t;
{
    fprintf(stderr,"%s: FATAL ERROR <%s>\n\t%s\n",_XG_Global.progName,s,t);
    XgrassExit(1);
}

ReallyFatalError(s,t)
    char *s, *t;
{
    fprintf(stderr,"%s: FATAL ERROR <%s>\n\t%s\n",_XG_Global.progName,s,t);
    exit(1);
}

void
Interrupt()
{
    fprintf(stderr,"%s: FATAL ERROR <%s>\n\t*NOTE*\n",_XG_Global.progName,
         "Caught interrupt signal");
    XgExitOnIntr();
}

Usage(s)
char *s;
{
    char *blanks, *ptr;
    int i;

    ptr = blanks = XtMalloc(strlen(s)+1);
    for ( i = 0; i < strlen(s); i++ )
        *ptr++ = ' ';
    *ptr = '\0';

    fprintf(stderr,"usage: %s [-v][-help][-patch][-version]\n",s);
    fprintf(stderr,"       %s [-Idir][-Dname][-Dname=def]\n",blanks);
    fprintf(stderr,"       %s [-f specification-file] -dbase XX -location YY -mapset ZZ\n",blanks);
    fprintf(stderr,"       %s [-f specification-file] session-file\n",blanks);
    fprintf(stderr,"where:\n");
    fprintf(stderr,"        -v - verbose mode\n");
    fprintf(stderr,"     -help - print this message\n");
    fprintf(stderr,"    -patch - print version (patchlevel) information\n");
    fprintf(stderr,"  -version -                ditto\n");
    fprintf(stderr,"    -dbase - use the next argument as the database name\n");
    fprintf(stderr," -location - use the next argument as the location name\n");
    fprintf(stderr,"   -mapset - use the next argument as the mapset name\n");
    fprintf(stderr,"     -Idir - add dir to cpp(1) include search list\n");
    fprintf(stderr,"    -Dname - define name as 1 (true) to cpp(1)\n");
    fprintf(stderr,"-Dname=def - define name as def to cpp(1)\n");
    fprintf(stderr,"        -f - use the next argument as the specfication file\n");
    exit(1);
}
