/* sum: sum of x
 * sum2: sum of x squared
 * n:    number of points
 */

double
I_variance (sum, sum2, n)
    double sum, sum2;
{
    if (n < 2)
	return ( (double) 0.0);
    else
	return ((sum2 - sum * sum / n) / (n - 1));
}

double
I_stddev (sum, sum2, n)
    double sum, sum2;
{
    double sqrt(), I_variance();

    if (n < 2)
	return ( (double) -99.0);
    else
	return sqrt (I_variance (sum, sum2, n));
}
