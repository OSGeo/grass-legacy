#include "glob.h"

eq_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a == *b);
	a++;
	b++;
    }
}

eq_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a == *b);
	a++;
	b++;
    }
}

ne_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a != *b);
	a++;
	b++;
    }
}

ne_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a != *b);
	a++;
	b++;
    }
}

gt_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a > *b);
	a++;
	b++;
    }
}

gt_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a > *b);
	a++;
	b++;
    }
}

ge_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a >= *b);
	a++;
	b++;
    }
}

ge_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a >= *b);
	a++;
	b++;
    }
}

lt_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a < *b);
	a++;
	b++;
    }
}

lt_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a < *b);
	a++;
	b++;
    }
}

le_i (a, b, n)
    CELL *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a <= *b);
	a++;
	b++;
    }
}

le_x (a, b, n)
    double *a, *b;
{
    while (n-- > 0)
    {
	*a = (*a <= *b);
	a++;
	b++;
    }
}
