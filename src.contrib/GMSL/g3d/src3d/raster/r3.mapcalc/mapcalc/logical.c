#include "glob.h"

#define FALSE 0
#define TRUE  1
#define ISFALSE(x) (x==0)
#define ISTRUE(x)  (x!=0)


/* The processing of NULL vs False(zero)/True(Non-zero)
 * NULL && NULL = NULL
 * NULL && F    = F
 * NULL && T    = NULL
 *
 * NULL || NULL = NULL
 * NULL || T    = T
 * NULL || F    = NULL
 */

and_x (a, b, n)
    double *a, *b;
{
    int na, nb;
    while (n-- > 0)
    {
	na = ISNULL_D(a);
	nb = ISNULL_D(b);
	if(na)
	{
	    if (nb || ISTRUE(*b))
		SETNULL_D(a);
	    else
		*a = FALSE;
	}
	else if (nb)
	{
	    if (na || ISTRUE(*a))
		SETNULL_D(a);
	    else
		*a = FALSE;
	}
	else
	{
	    *a = (*a && *b);
	}
	a++;
	b++;
    }
}

or_x (a, b, n)
    double *a, *b;
{
    int na, nb;
    while (n-- > 0)
    {
	na = ISNULL_D(a);
	nb = ISNULL_D(b);
	if(na)
	{
	    if (nb || ISFALSE(*b))
		SETNULL_D(a);
	    else
		*a = TRUE;
	}
	else if (nb)
	{
	    if (na || ISFALSE(*a))
		SETNULL_D(a);
	    else
		*a = TRUE;
	}
	else
	{
	    *a = (*a || *b);
	}
	a++;
	b++;
    }
}
