/*
 * G_chop - chop leading and trailing white spaces: 
 *          space, \f, \n, \r, \t, \v
 *        - returns pointer to string
 *    
 * char *G_chop (char *s)
 *
 * modified copy of G_squeeze();    RB March 2000
 *                          <Radim.Blazek@dhv.cz>
 *
 */

#include <ctype.h>

char *G_chop (line)
    char *line;
{
    register char *f = line, *t = line;

    while (isspace (*f))  	/* go to first non white-space char */
        f++;

    if (! *f)			/* no more chars in string */
    {
        *t = '\0';
	return (line);
    }

    for (t = line; *t; t++)  	/* go to end */
        ;
    while ( isspace (*--t) )	
	;
    *++t = '\0';  		/* remove trailing white-spaces */

    t = line;
    while (*f)			/* copy */   
            *t++ = *f++;
    *t = '\0';

    return (line);
}
