#include "imagery.h"

#define FMT1 "%g/%d=%.1lf"
#define FMT2 "%g/%d=?"

print_centroids (fd, C)
    FILE *fd;
    struct Cluster *C;
{
    int band, cat;
    char buf[40];

    fprintf (fd, "class centroids (sum/count=mean)\n");
    for (band = 0; band < C->nbands; band++)
    {
	fprintf (fd, "band %d", band+1);
	for (cat = 0; cat < C->nclasses; cat++)
	{
	    if (C->count[cat])
		sprintf (buf, FMT1, C->sum[band][cat], C->count[cat],
		(double) C->sum[band][cat] / (double) C->count[cat]);
	    else
		sprintf (buf, FMT2, C->sum[band][cat], C->count[cat]);
	    fprintf (fd, "%s %-18s",cat%4?"":"\n",buf);
	}
	fprintf (fd, "\n");
    }
}
