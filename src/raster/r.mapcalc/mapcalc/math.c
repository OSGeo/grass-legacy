#include "glob.h"

/* Bugs in C compilers (particularly MASSCOMP) 
 * caused expressions of the form
 *    *a++ *= *b++
 * to compile incorrectly. The problem was mixing *= (or +=, etc)
 * with two ++ operators. Therefore, the code was modified to
 * do the ++ in separate expressions, at the cost of speed.
*/
add (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a += *b;
	a++;
	b++;
    }
}

add_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a += *b;
	a++;
	b++;
    }
}

subtract (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a -= *b;
	a++;
	b++;
    }
}

subtract_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a -= *b;
	a++;
	b++;
    }
}

multiply (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a *= *b;
	a++;
	b++;
    }
}

multiply_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a *= *b;
	a++;
	b++;
    }
}

divide (a, b, n)
    CELL *a, *b;
{
    CELL v;

    while (n-- > 0)
    {
	if (v = *b++)
	    *a /= v;
	else
	    *a = 0;
	a++;
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
	    *a %= v;
	else
	    *a = 0;
	a++;
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
		*a = 0;
	    else
		*a -= k*v ;
	    a++;
	}
	else
	    *a++ = 0;
    }
}
