#include "gis.h"
long_list (statf)
    struct Cell_stats *statf;
{
    CELL cat;
    long count;	/* not used, but required by cell stats call */

    while (G_next_cell_stat (&cat, &count, statf))
	printf ("%ld\n", (long) cat);
}

compact_list (statf)
    struct Cell_stats *statf;
{
    CELL cat1,cat2,temp;
    int len;
    long count;	/* not used, but required by cell stats call */

    len = 0;
    G_next_cell_stat (&cat1, &count, statf);
    cat2 = cat1;
    while (G_next_cell_stat (&temp, &count, statf))
    {
	if (temp != cat2 + 1)
	{
	    show (cat1, cat2, &len) ;
	    cat1 = temp;
	}
	cat2 = temp;
    }
    show (cat1, cat2, &len);
    printf ("\n");
}

static
show (low, high, len)
    int *len;
    CELL low, high;
{
    char text[100];
    char xlen;

    if (low+1 == high)
    {
	show (low, low, len);
	show (high, high, len);
	return  ;
    }

    if (low == high)
        sprintf (text, "%ld ", (long) low);
    else
        sprintf (text, "%ld%s%ld ", (long) low, low<0 ? " thru " : "-", (long) high);

    xlen = strlen (text);
    if (xlen + *len > 78)
    {
	printf ("\n");
	*len = 0;
    }
    printf ("%s", text);
    *len += xlen;
}

compact_range_list (negmin, negmax, zero, posmin, posmax)
    CELL negmin, negmax, zero, posmin, posmax;
{
    if (negmin)
    {
	printf ("%ld", (long)negmin);
	if (negmin!=negmax)
	    printf (" thru %ld", (long) negmax);
	printf ("\n");
    }
    if (zero)
	printf ("0\n");
    if (posmin)
    {
	printf ("%ld", (long)posmin);
	if (posmin!=posmax)
	    printf (" thru %ld", (long)posmax);
	printf ("\n");
    }
}

range_list (negmin, negmax, zero, posmin, posmax)
    CELL negmin, negmax, zero, posmin, posmax;
{
    if (negmin)
    {
	printf ("%ld\n",(long)negmin);
	if (negmin != negmax)
	    printf ("%ld\n",(long)negmax);
    }
    if (zero)
	printf ("0\n");
    if (posmin)
    {
	printf ("%ld\n",(long)posmin);
	if (posmin != posmax)
	    printf ("%ld\n",(long)posmax);
    }
}
