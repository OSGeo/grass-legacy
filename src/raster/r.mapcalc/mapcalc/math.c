#include "glob.h"

int 
add (CELL *a, CELL *b, int n)
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a += *b;
	}
    }

    return 0;
}

int 
add_x (double *a, double *b, int n)
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

    return 0;
}

int 
subtract (CELL *a, CELL *b, int n)
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a -= *b;
	}
    }

    return 0;
}

int 
subtract_x (double *a, double *b, int n)
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

    return 0;
}

int 
multiply (CELL *a, CELL *b, int n)
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a *= *b;
	}
    }

    return 0;
}

int 
multiply_x (double *a, double *b, int n)
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

    return 0;
}

int 
divide (CELL *a, CELL *b, int n)
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL(a) || ISNULL(b) || *b == 0)
	{
	    SETNULL(a);
	}
	else
	{
	    *a /= *b;
	    integer_division_occurred = 1;
	}
    }

    return 0;
}

int 
divide_x (double *a, double *b, int n)
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

    return 0;
}

int 
modulus (CELL *a, CELL *b, int n)
{
    for ( ; n-- > 0; a++, b++)
    {
	if(ISNULL(a) || ISNULL(b) || *b == 0)
	{
	    SETNULL(a);
	}
	else
	{
	    *a %= *b;
	}
    }

    return 0;
}

int 
modulus_x (double *a, double *b, int n)
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

    return 0;
}

int 
power_x (double *a, double *b, int n)
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

    return 0;
}
