/******************************************************

NAME:		I_ask()

FUNCTION:	print a prompt, input user response

USAGE:		I_ask (prompt, answer, nullok)

ACTION:		ctrl-d will cause exit
		leading and trailing white space is removed
		tabs are replaced with blanks.
		non-printing chars are removed.

		if (!nullok)
			null entry causes ask to try again

RETURNS:	0 if NULL answer, 1 otherwise

PARMS:		prompt: prompt to print
		answer: buffer to read answer into
			if NULL just wait for c/r
		nullok: allow RETURN alone
******************************************************/
#include "imagery.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int I_ask ( char *prompt, char *answer, int nullok)
{
    char *a;
    char *b;
    char buf[200];

    fflush (stdout);
    fflush (stderr);
    if (!answer)
    {
	answer = buf;
	nullok = 1;
    }

    do
    {
	fprintf (stderr, "%s", prompt);
	if(!fgets(answer,128,stdin))
	{
	    fprintf(stderr, "\n");
	    exit(1);
	}

/* collapse white space to single blank */
/* remove non-printing chars */

	for (a = b = answer; *a = *b++; )
	{
	    if (*a == ' ' || *a == '\t')
	    {
		*a = ' ';
		while (*b == ' ' || *b == '\t')
		    b++;
	    }

	    if (*a >= ' ' && *a < 0177)
		a++;
	}

/* remove leading blank */
	if (*(a=answer) == ' ')
	    for (b = a+1; *a++ = *b++; )
		    ;

/* remove trailing blank */
	for (b = 0, a = answer; *a; a++)
	    if (*a != ' ')
		b = a;
	if (b++) *b = 0;

	if (strcmp (answer,"exit") == 0)
	    exit(1);

    } while (!nullok && *answer == 0);

    return (*answer != 0);
}
