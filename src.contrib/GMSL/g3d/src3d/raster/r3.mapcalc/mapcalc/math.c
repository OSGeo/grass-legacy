#include "glob.h"

add_x (a, b, n)
    double *a, *b;
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a += *b;
	}
    }
}

subtract_x (a, b, n)
    double *a, *b;
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a -= *b;
	}
    }
}

multiply_x (a, b, n)
    double *a, *b;
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    *a *= *b;
	}
    }
}

divide_x (a, b, n)
    double *a, *b;
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b) || *b == 0.0)
	{
	    SETNULL_D(a);
	}
	else
	{
	    floating_point_exception = 0;
	    *a = *a / *b;
	    if(floating_point_exception)
		SETNULL_D(a);
	}
    }
}

modulus_x (a, b, n)
    double *a, *b;
{
    int k;
    double v;

    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b) || *b == 0.0)
	{
	    SETNULL_D(a);
	}
	else
	{
	    floating_point_exception = 0;
	    v = *b;
	    k = *a / v;
	    if (floating_point_exception)
		SETNULL_D(a);
	    else
		*a -= k*v ;
	}
    }
}

power_x (a, b, n)
    double *a, *b;
{
    double pow();
    double ceil();

    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL_D(a) || ISNULL_D(b))
	{
	    SETNULL_D(a);
	}
	else
	{
	    floating_point_exception = 0;
	/* if a is negative, pow() requires b to be integer */
	    if (*a < 0.0 && *b != ceil(*b))
	    {
		SETNULL_D(a);
	    }
	    else
	    {
		*a = pow(*a,*b);
		if (floating_point_exception)
		    SETNULL_D(a);
	    }
	}
    }
}
