#include <grass/imagery.h>
/************************************************************
 * add_cov (n1, n2, m1a, m1b, m2a, m2b, cov1, cov2)
 *
 * for merging two signatures it is necessary to compute the
 * new covariance matrix for the merged class from the means
 * and covariance matrices of the tow classes to be merged.
 * this routine computes the new covariances.
 *
 * n1,n2     number of points in each signatures
 * m1a,m1b   means for band a,b in sig 1
 * m2a,m2b   means for band a,b in sig 2
 * cov1      covariances for the band a,b in sig 1
 * cov2      covariances for the band a,b in sig 2
 ***********************************************************/
double I_add_covariances (int n1,int n2,
    double m1a,double m1b,double m2a,double m2b,
    double cov1,double cov2)
{
    int n;
    double sum1, sum2;
    double cov;
    double ma,mb;

    n = n1 + n2;
    ma = (m1a * n1 + m2a * n2) / n;
    mb = (m1b * n1 + m2b * n2) / n;

    sum1 = cov1 * (n1-1) + n1 * m1a * m1b;
    sum2 = cov2 * (n2-1) + n2 * m2a * m2b;
    cov = (sum1+sum2 - n*ma*mb) / (n-1);

    return cov;
}
