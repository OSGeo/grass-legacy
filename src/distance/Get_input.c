/* %W% %G% */
#include "gis.h"
#include "disfrm.h"

/* get input from stdin for to/from CATS and DIST ranges.
 * returns number of DIST values
 */
Get_input ()
{
    char buf[1024], *p;
    char tmpbuf[512];
    int istty;
    int num;
    int Next_dist;
    int Dist, From;
    char **tp, **token;

    Next_dist = 1;
    istty = isatty (0);
    while (input (buf, ""))
    {
	G_squeeze (buf);
	if (*buf == 0) continue;
	token = G_tokenize (buf, " ");
	if (*buf == 'c')		/* cats */
	{
	    Dist = 0;
	    From = 1;
	}
	else if (*buf == 'd')
	{
	    Dist = 1;
	    From = 0;
	}
	else
	{
	    fprintf (stderr, "Bad input format.\n");
	    if (!istty)
		exit (-1);
	    fprintf (stderr, "To set the categories\n");
	    fprintf (stderr, " cats n ...\n");
	    fprintf (stderr, "To set the distances\n");
	    fprintf (stderr, " dist n ...\n");
	    continue;
	}
	for (tp = token+1 ; *tp ; tp++)
	{
	    char *index ();

	    if ((num = atoi (*tp)) == 0)
	    {
		if (index (*tp, '0') == NULL)
		{
		    sprintf (tmpbuf, "Bad data format: '%s'\n", buf);
		    G_fatal_error (tmpbuf);
		}
	    }
	    if (num <  0)	/* crudely handle negatives */
		num *= -1;
	    if (From)
	    {
		if (num >= 0 && num <= table_len)
		    table[num] = 1;
	    }
	    else	/* dist */
	    {
		if (Next_dist < MAXDIST)
		{
		    dist[Next_dist] = num;
		    Next_dist++;
		}
	    }
	}
	G_free_tokens (token);
    }
    return (Next_dist);
}

subcmp (substr, str)
    char *substr, *str;
{
    register char *p;

    for (p = substr ; *p ; p++)
    {
	if (*p != *str)
	    return (*p - *str);
	str++;
    }
    return (0);
}
