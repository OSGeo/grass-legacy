#include "imagery.h"
print_band_means(fd,C)
    FILE *fd;
    struct Cluster *C;
{
    int band;
    double I_stddev();

    fprintf (fd, "\n");
    fprintf (fd, "means and standard deviations for %d band%s\n\n",
	C->nbands, C->nbands == 1 ? "" : "s");
    fprintf (fd, " means  ");
    for (band = 0; band < C->nbands; band++)
	fprintf (fd," %6.2lf",C->band_sum[band]/C->npoints);
    fprintf (fd,"\n");
    fprintf (fd, " stddev ");
    for (band = 0; band < C->nbands; band++)
	fprintf (fd, " %6.2lf",
	    I_stddev(C->band_sum[band], C->band_sum2[band], C->npoints));
    fprintf (fd, "\n\n");
}
