#include <stdio.h>
#include "local_proto.h"
#define MAXLINES 18
#define TITLE (char *) 0

static char *Title  = TITLE;

static int nlines = 0;

int instructions (int n)
{
    if (n == 0) nlines = MAXLINES;
    if (nlines >= MAXLINES)
    {
	if(Title) fprintf (stdout,"%s\n", Title);
	fprintf (stdout," Buttons:\n") ;
	fprintf (stdout,"  %s where am i\n", lefts) ;
	fprintf (stdout,"  %s mark point\n", middles) ;
	fprintf (stdout,"  %s done\n\n", rights) ;
	nlines = 0 ;
    }
    nlines += n;

    return 0;
}
