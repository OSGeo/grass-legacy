/**********************************************************************
   error.c      - report any error (fatal, warning, etc.)
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"

void
XgenFatalError(proc, expl)
    char                           *proc;
    char                           *expl;
{
    fprintf(stderr, "\n%s: FATAL ERROR in %s\n\tEXPLANATION: %s\n",
            xgenGD.progName, proc, expl);
    XgenExit(-1);
}

void
XgenFatalWarning(proc, expl)
    char                           *proc;
    char                           *expl;
{
    fprintf(stderr, "\n%s: FATAL WARNING in %s\n\tEXPLANATION: %s\n",
            xgenGD.progName, proc, expl);
}

void
XgenWarning(proc, expl)
    char                           *proc;
    char                           *expl;
{
    if (verbose)
        fprintf(stderr, "\n%s: WARNING in %s\n\texplanation: %s\n",
                xgenGD.progName, proc, expl);
}

void
ExpandError(resource, ptr, type)
    Resource                       *resource;
    caddr_t                         ptr;
    int                             type;
{
    switch (type) {
    case ENVIRONMENT:
        {
            Environ                        *eptr = (Environ *) ptr;

	    if ( verbose)
            fprintf(stderr, "\tin environment %s, %s resource\n", eptr->name,
                    resource->name);
        }
        break;
    case SHELL:
        {
            Shell                          *sptr = (Shell *) ptr;

	    if ( verbose)
            fprintf(stderr, "\tin shell %s, %s resource\n", sptr->name,
                    resource->name);
        }
        break;
    case OBJECT:
        {
            InterfaceObject                *optr = (InterfaceObject *) ptr;

	    if ( verbose)
            fprintf(stderr, "\tin object %s, %s resource\n", optr->name,
                    resource->name);
        }
        break;
    default:
        break;
    }
    return;
}

void
yyerror(s, flag)
    char                           *s;
    Bool                            flag;
/* the flag if True says do not display the yytext token */
{
    extern int                      yylineno;
    extern char                     yytext[];

    fprintf(stderr, "\n\"%s\", line %d:\n<%s>",
    (xgenGD.scriptFile != NULL) ? xgenGD.scriptFile : "stdin", yylineno, s);

    if (flag)
        fprintf(stderr, " at or near \"%s\"", yytext);
    fprintf(stderr, "\n");

    XgenExit(0);
}

void
yywarning(s, flag)
    char                           *s;
    Bool                            flag;
/* the flag if True says do not display the yytext token */
{
    extern int                      yylineno;
    extern char                     yytext[];

    fprintf(stderr, "\n\"%s\", line %d: warning \n<%s>",
    (xgenGD.scriptFile != NULL) ? xgenGD.scriptFile : "stdin", yylineno, s);
    if (!flag)
        fprintf(stderr, " at or near \"%s\"", yytext);
    fprintf(stderr, "\n");
}

void
usage(s)
    char                           *s;
{
    char                           *blanks, *ptr;
    int                             i;

    ptr = blanks = XtMalloc(strlen(s) + 1);
    for (i = 0; i < (int) strlen(s); i++)
        *ptr++ = ' ';
    *ptr = '\0';

    fprintf(stderr, "usage: %s [-v][-c][-p][-n][-help][-patch][-version]\n", s);
    fprintf(stderr, "       %s [-I_\bd_\bi_\br][-Dname][-Dname=def]\n", blanks);
    fprintf(stderr, "       %s [some X Toolkit options] scriptname\n", blanks);
    fprintf(stderr, "where:\n");
    fprintf(stderr, "        -v - verbose mode\n");
    fprintf(stderr, "        -c - display control box\n");
    fprintf(stderr, "        -p - parse only\n");
    fprintf(stderr, "        -n - do not pass thru C preprocessor\n");
    fprintf(stderr, "     -help - print this message\n");
    fprintf(stderr, "    -patch - print version (patchlevel) information\n");
    fprintf(stderr, "  -version -                ditto\n");
    fprintf(stderr, "     -Idir - add dir to cpp(1) include search list\n");
    fprintf(stderr, "    -Dname - define name as 1 (true) to cpp(1)\n");
    fprintf(stderr, "-Dname=def - define name as def to cpp(1)\n");
    XgenExit(1);
}
