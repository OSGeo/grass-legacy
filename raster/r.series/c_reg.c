#include "gis.h"

static void regression(DCELL *result, DCELL *values, int n, int offset)
{
	DCELL xsum, ysum;
	DCELL xbar, ybar;
	DCELL numer, denom;
	int count;
	int i;

	xsum = ysum = 0.0;
	count = 0;

	for (i = 0; i < n; i++)
	{
		if (G_is_d_null_value(&values[i]))
			continue;

		xsum += i;
		ysum += values[i];
		count++;
	}

	if (count == 0)
	{
		G_set_d_null_value(result, 1);
		return;
	}

	xbar = xsum / count;
	ybar = ysum / count;

	numer = 0.0;
	for (i = 0; i < n; i++)
		if (!G_is_d_null_value(&values[i]))
			numer += i * values[i];
	numer -= count * xbar * ybar;

	denom = 0.0;
	for (i = 0; i < n; i++)
		if (!G_is_d_null_value(&values[i]))
			denom += (DCELL) i * i;
	denom -= count * xbar * xbar;

	*result = offset
		? (ybar - xbar * numer / denom)
		: (numer / denom);
}

void c_reg_m(DCELL *result, DCELL *values, int n)
{
	regression(result, values, n, 0);
}

void c_reg_c(DCELL *result, DCELL *values, int n)
{
	regression(result, values, n, 1);
}

