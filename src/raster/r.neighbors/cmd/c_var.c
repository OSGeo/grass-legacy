#include "gis.h"
double
d_var (values, n)
    register CELL *values;
{
    register double ave;
    register double sum;
    double d;
    register int i;

    if (n == 0) return 0;
    ave = 0;
    for (i = 0; i < n; i++)
	ave += values[i];

    ave /= (double) n;
    sum = 0;
    for (i = 0; i < n; i++)
    {
	d = values[i] - ave;
	sum += d*d;
    }

    return sum/n;
}

CELL
c_var(values, n)
    register CELL *values;
{
    return (CELL) (d_var(values, n) + .5);
}

