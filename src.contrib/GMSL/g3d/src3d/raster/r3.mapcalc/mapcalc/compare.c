#include "glob.h"

eq_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a == *b);
	}
	a++;
	b++;
    }
}

ne_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a != *b);
	}
	a++;
	b++;
    }
}

gt_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a > *b);
	}
	a++;
	b++;
    }
}

ge_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a >= *b);
	}
	a++;
	b++;
    }
}

lt_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a < *b);
	}
	a++;
	b++;
    }
}

le_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a = (*a <= *b);
	}
	a++;
	b++;
    }
}
