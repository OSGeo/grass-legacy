#include "glob.h"
/********************************************************************
 if(a)             1 if a is non zero, 0 otherwise
 if(a,b)           b if a is non zero, 0 otherwise
 if(a,b,c)         b if a is non zero, c otherwise
 if(a,b,c,d)       b if a is positive, c if a is zero, d if a is negative
********************************************************************/


x_if (argc, argv, xcell, ncols)
    double *argv[];
    register double *xcell;
    register int ncols;
{
    register double *a, *b, *c, *d;

    a=argv[0];
    if (argc > 1) b=argv[1];
    if (argc > 2) c=argv[2];
    if (argc > 3) d=argv[3];

    if (argc == 1)
    {
	for ( ; ncols-- > 0; a++, xcell++)
	{
	    if(ISNULL_D(a))
		SETNULL_D(xcell);
	    else
		*xcell = (*a ? 1 : 0) ;
	}
    }
    else if (argc == 2)
    {
	for ( ; ncols-- > 0; a++, b++, xcell++)
	{
	    if(ISNULL_D(a) || ISNULL_D(b))
		SETNULL_D(xcell);
	    else
		*xcell = (*a ? *b : 0) ;
	}
    }
    else if (argc == 3)
    {
	for ( ; ncols-- > 0; a++, b++, c++, xcell++)
	{
	    if(ISNULL_D(a))
		SETNULL_D(xcell);
	    else if (*a)
	    {
		if(ISNULL_D(b))
		    SETNULL_D(xcell);
		else
		    *xcell = *b;
	    }
	    else
	    {
		if(ISNULL_D(c))
		    SETNULL_D(xcell);
		else
		    *xcell = *c;
	    }
	}
    }
    else if (argc == 4)
    {
	for ( ; ncols-- > 0; a++, b++, c++, d++, xcell++)
	{
	    if(ISNULL_D(a))
		SETNULL_D(xcell);
	    else if (*a > 0)
	    {
		if (ISNULL_D(b))
		    SETNULL_D(xcell);
		else
		    *xcell = *b;
	    }
	    else if (*a == 0)
	    {
		if (ISNULL_D(c))
		    SETNULL_D(xcell);
		else
		    *xcell = *c;
	    }
	    else
	    {
		if (ISNULL_D(d))
		    SETNULL_D(xcell);
		else
		    *xcell = *d;
	    }
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
