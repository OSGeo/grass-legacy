/* compute r (correlation)
 * for x agasinst y
 */
int 
correlation (double *x, double *y, int n, double *r)
{
    double sqrt();
    double mx, my, sx, sy, cov;
    int i;

    *r = 0.0;
    if (n <= 0)
	return 0;

    mx = my = 0.0;
    for (i = 0; i < n; i++)
    {
	mx += x[i];
	my += y[i];
    }
    mx /= n;
    my /= n;

    sx = sy = cov = 0.0;
    for (i = 0; i < n; i++)
    {
	sx += (x[i] - mx) * (x[i] - mx);
	sy += (y[i] - my) * (y[i] - my);
	cov += (x[i] - mx) * (y[i] - my);
    }
    *r = cov / sqrt(sx*sy);
    return 1;
}
