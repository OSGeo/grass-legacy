#include "imagery.h"
/* safe to call only during checkpoint(2) and after
 * I_cluster_exec() completes
 * otherwise call I_cluster_sum2() before calling this routine
 */
print_class_means(fd,C)
    FILE *fd;
    struct Cluster *C;
{
    int band;
    int c;
    double I_stddev();
    int n;

    fprintf (fd, "\nclass means/stddev for each band\n\n");

    for (c = 0; c < C->nclasses; c++)
    {
	fprintf (fd, "\n");
	fprintf (fd, "class %d (%d)\n", c+1, n = C->count[c]);
	fprintf (fd, "  means ");
	if (n > 0)
	    for (band = 0; band < C->nbands; band++)
		fprintf (fd, " %6.2lf", C->sum[band][c]/n);
	fprintf (fd, "\n");
	fprintf (fd, "  stddev");
	if (n > 1)
	    for (band = 0; band < C->nbands; band++)
		fprintf (fd, " %6.2lf",
		    I_stddev(C->sum[band][c], C->sum2[band][c], n));
	fprintf (fd, "\n");
    }
    fprintf (fd, "\n");
}
