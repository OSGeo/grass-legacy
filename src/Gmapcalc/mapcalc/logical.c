#include "glob.h"

and_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a && *b);
	a++;
	b++;
    }
}

and_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a && *b);
	a++;
	b++;
    }
}

or_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a || *b);
	a++;
	b++;
    }
}

or_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a || *b);
	a++;
	b++;
    }
}
