#include<stdio.h>
#include<math.h>
#include"cdh.h"

double * coeff_variation(x, n)
double *x;
int n;
{
    static double y[2];
    double s2, s3, s4, sum2=0.0, sum4=0.0;
    int i;

    for (i = 0; i < n; ++i) 
	sum2 += log(x[i]);
    for (i = 0; i < n; ++i) 
	sum4 += (log(x[i]) - sum2 / n) * (log(x[i]) - sum2 / n);
    s2 = sum4 / (n - 1);
    s3 = exp(s2) - 1;
    s4 = sqrt(s3);
    y[0] = sqrt(exp(s4/(n-1))-1);
#ifdef NOISY
 fprintf (stdout,"  TEST23 CV(L)  =%10.4f\n",s4);
#endif /* NOISY */
    return y;
}
