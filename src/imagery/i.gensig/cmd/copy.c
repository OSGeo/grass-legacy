#include "imagery.h"
copy_covariances (a, b, n)
    double **a, **b;
{
    int b1, b2;
    for (b1 = 0; b1 < n; b1++)
	for (b2 = 0; b2 < n; b2++)
	    a[b1][b2] = b[b1][b2];
}
