#include "gis.h"
/*****************************************************************
 *
 * G_yes (question, dflt)
 *
 * print the question and get a yes/no response from the user
 * if dflt is 1 a RETURN is taken to be yes
 * if dflt is 0 a RETURN is taken to be no
 * if dflt is -1 a RETURN is not a valid response
 *
 * returns 0 no, 1 yes
 ***************************************************************/
#include <stdio.h>

int G_yes (char *question,int dflt)
{
    char answer[100];

    fflush (stdout);
    while (1)
    {
	fprintf (stderr,"%s", question);
	while (1)
	{
	    fprintf (stderr,"(y/n) ");
	    if (dflt >= 0) fprintf (stderr,dflt==0?"[n] ":"[y] ");
	    fflush (stderr);
	    if (!G_gets(answer)) break;
	    G_strip (answer);
	    switch (*answer)
	    {
	    case 'y': case 'Y':	return (1);
	    case 'n': case 'N':	return (0);
	    case 0: if (dflt >= 0) return (dflt);
	    }
	}
    }
}
