#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "local_proto.h"


/* this is actually the Weisberg-Bingham stat. I need to
   OCR the constants and implment this correctly */

double *shapiro_francia(double *x, int n)
{
    static double y[2];
    double suma = 0.0, sumb = 0.0, sumc = 0.0, sumd = 0.0, z, *xcopy;
    int i;

    if ((xcopy = (double *)malloc(n * sizeof(double))) == NULL) {
	fprintf(stderr, "Memory error in shapiro_francia\n");
	exit(EXIT_FAILURE);
    }

    for (i = 0; i < n; ++i)
	xcopy[i] = x[i];

    qsort(xcopy, n, sizeof(double), dcmp);

    for (i = 0; i < n; ++i) {
	z = xinormal((i + 1 - 0.375) / (n + 0.25));
	suma += z * xcopy[i];
	sumb += z * z;
	sumc += xcopy[i];
	sumd += xcopy[i] * xcopy[i];
    }

    y[0] = suma * suma / sumb / (sumd - sumc * sumc / n);

#ifdef NOISY
    fprintf(stdout, "  TEST14 SF(N)  =%10.4f\n", y[0]);
#endif /* NOISY */

    free(xcopy);

    return y;
}				/* test14_ */
