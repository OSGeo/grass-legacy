#include "glob.h"
#include "func_proto.h"

int eq_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a == *b);
	}
	a++;
	b++;
    }

    return 0;
}

int eq_x (double *a, double *b, int n)
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
    return 0;
}

int 
ne_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a != *b);
	}
	a++;
	b++;
    }
    return 0;
}

int 
ne_x (double *a, double *b, int n)
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
    return 0;
}

int 
gt_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a > *b);
	}
	a++;
	b++;
    }
    return 0;
}

int 
gt_x (double *a, double *b, int n)
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
    return 0;
}

int 
ge_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a >= *b);
	}
	a++;
	b++;
    }
    return 0;
}

int 
ge_x (double *a, double *b, int n)
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
    return 0;
}

int 
lt_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a < *b);
	}
	a++;
	b++;
    }
    return 0;
}

int 
lt_x (double *a, double *b, int n)
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
    return 0;
}

int 
le_i (CELL *a, CELL *b, int n)
{
    while (n-- > 0)
    {
	if(ISNULL(a) || ISNULL(b))
	{
	    SETNULL(a);
	}
	else
	{
	    *a = (*a <= *b);
	}
	a++;
	b++;
    }
    return 0;
}

int 
le_x (double *a, double *b, int n)
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
    return 0;
}
