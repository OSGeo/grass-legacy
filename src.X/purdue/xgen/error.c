#include "xgen.h"

void
XgenFatalError(proc,expl)
    char * proc;
    char * expl;
{
    fprintf(stderr,"\n%s: FATAL ERROR in %s\n\tEXPLANATION: %s\n\n",
		xgenGD.progName,proc,expl);
    XgenExit(-1);
}

void
XgenFatalWarning(proc,expl)
    char * proc;
    char * expl;
{
    fprintf(stderr,"\n%s: FATAL WARNING in %s\n\tEXPLANATION: %s\n\n",
		xgenGD.progName,proc,expl);
}

void
XgenWarning(proc,expl)
    char * proc;
    char * expl;
{
    fprintf(stderr,"\n%s: WARNING in %s\n\texplanation: %s\n\n",
		xgenGD.progName,proc,expl);
}

yyerror(s,flag) 
char *s; 
Bool flag;
/* the flag if True says do not display the yytext token */
{
    extern int yylineno;
    extern char yytext[];

    fprintf(stderr,"\n\"%s\", line %d:\n<%s>",
		(xgenGD.scriptFile != NULL )?xgenGD.scriptFile:"stdin",yylineno,s);

    if ( flag )
        fprintf(stderr," at or near \"%s\"",yytext);
    fprintf(stderr,"\n");

    XgenExit(0);
}

yywarning(s,flag) 
char *s; 
Bool flag;
/* the flag if True says do not display the yytext token */
{
    extern int yylineno;
    extern char yytext[];

    fprintf(stderr,"\n\"%s\", line %d: warning \n<%s>",
        (xgenGD.scriptFile != NULL )?xgenGD.scriptFile:"stdin",yylineno,s);
    if ( !flag )
        fprintf(stderr," at or near \"%s\"",yytext);
    fprintf(stderr,"\n");
}

usage(s)
char *s;
{

    fprintf(stderr,"usage: %s [-v] [-c] [-po] [-nocpp] scriptname\n",s);
	XgenExit(1);
}
