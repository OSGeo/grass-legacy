/* compute t score for a given correlation r */

tscore(r,n,t)
    double r, *t;
{
    double sqrt();

    *t = 0.0;
    if (n <= 2)
	return 0;

    *t = r / sqrt((1.0-r*r)/(n-2));
    return 1;
}
