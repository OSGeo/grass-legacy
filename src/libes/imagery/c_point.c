/****************************************************************
 * I_cluster_point (C,x)
 *     struct Cluster *C;
 *     CELL *x;
 *
 * adds the point x to the list of data points to be "clustered"
 *
 * returns
 *   0  ok
 *  -1  out of memory, point not added
 *   1  all values are zero, point not added
 *
 * the dimension of x must agree with the number of bands specified
 * in the initializing call to I_cluster_begin()
 *
 * note: if all values in x are zero, the point is rejected
 ***************************************************************/

#include "imagery.h"

I_cluster_point (C,x)
    struct Cluster *C;
    CELL *x;
{
    int band;

/* reject points that are all zero */
    for (band = 0; band < C->nbands; band++)
	if (x[band]) break;
    if (band >= C->nbands)
	return 1;

/* extend the arrays for each band, if necessary */
    if(!extend(C,1))
	return -1;

/* add the point to the points arrays */
    for (band = 0; band < C->nbands; band++)
    {
	register double z;

	z = C->points[band][C->npoints] = x[band];
	C->band_sum[band]  += z;
	C->band_sum2[band] += z*z;
    }
    C->npoints++;
    return 0;
}

I_cluster_begin_point_set (C, n)
    struct Cluster *C;
{
    return extend(C,n) ? 0 : -1;
}

I_cluster_point_part (C, x, band, n)
    struct Cluster *C;
    register CELL x;
{
    C->points[band][C->npoints+n] = x;
    C->band_sum[band]  += (double) x;
    C->band_sum2[band] += (double) x*x;
}

I_cluster_end_point_set (C, n)
    struct Cluster *C;
{
    int band;
    int cur,next;

    cur = C->npoints;
    n += C->npoints;
    for (next = cur; next < n; next++)
    {
	if (!all_zero (C, next))
	{
	    if (cur != next)
		for (band = 0; band < C->nbands; band++)
		    C->points[band][cur] = C->points[band][next] ;
	    cur++;
	}
    }
    return C->npoints = cur;
}

static
all_zero (C,i)
    struct Cluster *C;
{
    int band;

    for (band = 0; band < C->nbands; band++)
	if (C->points[band][i])
	    return 0;
    return 1;
}
static
extend(C,n)
    struct Cluster *C;
{
    int band;
    while ((C->npoints+n) > C->np)
    {
	C->np += 128;
	for (band = 0; band < C->nbands; band++)
	{
	    C->points[band] = (CELL *) I_realloc (C->points[band], C->np * sizeof (CELL)) ;
	    if (C->points[band] == NULL)
		return 0;
	}
    }
    return 1;
}
