#include <math.h>
#include <grass/imagery.h>

int I_cluster_reassign(struct Cluster *C, int *interrupted)
{
    double min,d,z;
    int q;
    int c,np;
    int old;
    int p, band, class;
    int changes;
    int first;

    changes = 0;
    for (c = 0; c < C->nclasses; c++)
    {
	C->countdiff[c] = 0;
	for (band = 0; band < C->nbands; band++)
	    C->sumdiff[band][c] = 0;
    }

    min = HUGE_VAL;
    class = 0;
    for (p = 0; p < C->npoints; p++)
    {
	if (*interrupted) return 0;
	if (C->class[p] < 0)	/* point to be ignored */
	    continue;

/* find minimum distance to center of all classes */
	first = 1;
	for (c = 0; c < C->nclasses; c++)
	{
	    d = 0;
	    np = C->count[c];
	    if (np == 0) continue;
	    for (band = 0; band < C->nbands; band++)
	    {
		z =  C->points[band][p] * np - C->sum[band][c] ;
		d += z*z;
	    }
	    d /= (np*np);

	    if (first || (d < min))
	    {
		class = c;
		min = d;
		first = 0;
	    }
	}

	if (C->class[p] != class)
	{
	    old = C->class[p];
	    C->class[p] = class;
	    changes++;

	    C->countdiff[class]++;
	    C->countdiff[old]--;

	    for (band = 0; band < C->nbands; band++)
	    {
		q = (int) C->points[band][p];
		C->sumdiff[band][class] += q;
		C->sumdiff[band][old] -= q;
	    }
	}
    }

    if (changes)
    {
	for (c=0; c < C->nclasses; c++)
	{	
	    C->count[c] += C->countdiff[c];
	    for (band = 0; band < C->nbands; band++)
		C->sum[band][c] += C->sumdiff[band][c];
	}
    }

    return changes ;
}
