/* @(#)math.c	2.2  10/1/87 */
#include "glob.h"

add (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
	*a++ += *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

add_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
	*a++ += *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

subtract (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
	*a++ -= *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

subtract_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
	*a++ -= *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

multiply (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
	*a++ *= *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

multiply_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
	*a++ *= *b++;	/* SOME COMPILERS HAVE TROUBLE WITH THIS!!! */
}

divide (a, b, n)
    CELL *a, *b;
{
    CELL v;

    while (n-- > 0)
    {
	if (v = *b++)
	    *a++ /= v;
	else
	    *a++ = 0;
    }
}

divide_x (a, b, n)
    double *a, *b;
{
    double v,x;

    while (n-- > 0)
    {
	if (v = *b++)
	{
	    floating_point_exception = 0;
	    x = *a / v;
	    if(floating_point_exception)
		x = 0.0;
	    *a++ = x;
	}
	else
	    *a++ = 0;
    }
}

modulus (a, b, n)
    CELL *a, *b;
{
    CELL v;

    while (n-- > 0)
    {
	if (v = *b++)
	    *a++ %= v;
	else
	    *a++ = 0;
    }
}

modulus_x (a, b, n)
    double *a, *b;
{
    double v;
    int k;

    while (n-- > 0)
    {
	if (v = *b++)
	{
	    floating_point_exception = 0;
	    k = *a / v;
	    if (floating_point_exception)
		*a++ = 0;
	    else
		*a++ -= k*v ;
	}
	else
	    *a++ = 0;
    }
}
