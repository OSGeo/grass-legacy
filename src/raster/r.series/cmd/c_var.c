#include "gis.h"

double d_var(DCELL *values, int n)
{
	double sum, ave, sumsq;
	int i;

	sum = 0;

	for (i = 0; i < n; i++)
		sum += values[i];

	ave = sum / n;

	sumsq = 0;

	for (i = 0; i < n; i++)
	{
		double d = values[i] - ave;
		sumsq += d * d;
	}

	return sumsq / n;
}

DCELL c_var(DCELL *values, int n)
{
	return (DCELL) d_var(values, n);
}

