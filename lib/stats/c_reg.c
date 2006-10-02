#include <grass/gis.h>

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

	if (count < 2)
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

static void regression_w(DCELL *result, DCELL (*values)[2], int n, int offset)
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
		if (G_is_d_null_value(&values[i][0]))
			continue;

		xsum += i * values[i][1];
		ysum += values[i][0] * values[i][1];
		count += values[i][1];
	}

	if (count < 2)
	{
		G_set_d_null_value(result, 1);
		return;
	}

	xbar = xsum / count;
	ybar = ysum / count;

	numer = 0.0;
	for (i = 0; i < n; i++)
		if (!G_is_d_null_value(&values[i][0]))
			numer += i * values[i][0] * values[i][1];
	numer -= count * xbar * ybar;

	denom = 0.0;
	for (i = 0; i < n; i++)
		if (!G_is_d_null_value(&values[i][0]))
			denom += (DCELL) i * i * values[i][1];
	denom -= count * xbar * xbar;

	*result = offset
		? (ybar - xbar * numer / denom)
		: (numer / denom);
}

void w_reg_m(DCELL *result, DCELL (*values)[2], int n)
{
	regression_w(result, values, n, 0);
}

void w_reg_c(DCELL *result, DCELL (*values)[2], int n)
{
	regression_w(result, values, n, 1);
}

