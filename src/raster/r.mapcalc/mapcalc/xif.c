#include "gis.h"
/********************************************************************
 if(a)             1 if a is non zero, 0 otherwise
 if(a,b)           b if a is non zero, 0 otherwise
 if(a,b,c)         b if a is non zero, c otherwise
 if(a,b,c,d)       b if a is positive, c if a is zero, d if a is negative
********************************************************************/

i_if (argc, argv, cell, ncols)
    CELL *argv[];
    register CELL *cell;
    register int ncols;
{
    register CELL *a, *b, *c, *d;

    a=argv[0];
    if (argc > 1) b=argv[1];
    if (argc > 2) c=argv[2];
    if (argc > 3) d=argv[3];

    if (argc == 1)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? 1 : 0) ;
	    a++;
	}
    }
    else if (argc == 2)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? *b : 0) ;
	    a++;
	    b++;
	}
    }
    else if (argc == 3)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? *b : *c) ;
	    a++;
	    b++;
	    c++;
	}
    }
    else if (argc == 4)
    {
	while (ncols-- > 0)
	{
	    if (*a > 0)
		*cell++ = *b;
	    else if (*a == 0)
		*cell++ = *c;
	    else
		*cell++ = *d;
	    a++;
	    b++;
	    c++;
	    d++;
	}
    }
}

x_if (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
{
    register double *a, *b, *c, *d;

    a=argv[0];
    if (argc > 1) b=argv[1];
    if (argc > 2) c=argv[2];
    if (argc > 3) d=argv[3];

    if (argc == 1)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? 1 : 0) ;
	    a++;
	}
    }
    else if (argc == 2)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? *b : 0) ;
	    a++;
	    b++;
	}
    }
    else if (argc == 3)
    {
	while (ncols-- > 0)
	{
	    *cell++ = (*a ? *b : *c) ;
	    a++;
	    b++;
	    c++;
	}
    }
    else if (argc == 4)
    {
	while (ncols-- > 0)
	{
	    if (*a > 0)
		*cell++ = *b;
	    else if (*a == 0)
		*cell++ = *c;
	    else
		*cell++ = *d;
	    a++;
	    b++;
	    c++;
	    d++;
	}
    }
}

n_if (n,name) char *name;
{
    if (n >= 1 && n <= 4) return 1;
    fprintf (stderr, "%s - ", name);
    if (n < 1)
	fprintf (stderr, "no ");
    else
	fprintf (stderr, "too many ");
    fprintf (stderr, "arguments specified. usage:\n");
    fprintf (stderr, "  %s(x)        1 if x not zero, 0 otherwise\n",name);
    fprintf (stderr, "  %s(x,a)      a if x not zero, 0 otherwise\n",name);
    fprintf (stderr, "  %s(x,a,b)    a if x not zero, b otherwise\n",name);
    fprintf (stderr, "  %s(x,a,b,c)  a if x > 0, b if x is zero, c if x < 0\n",name);

    return 0;
}
