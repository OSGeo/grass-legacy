#include <stdio.h>
#include "gis.h"
#include "Vect.h"

double fabs ();

get_line_center (x, y, Points)
    double *x, *y;
    struct line_pnts *Points;
{
    register int i;
    register int n_points;
    register double *ux, *uy;
    double dist;		/* running total of line length */
    double half_dist;		/* half total line length */
    double len;			/* tmp length of current line seg */
    double frac;		/* overshoot / line length */

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
	return -1;
    if (n_points == 1)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
	
    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
	dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
    if (dist == 0.0)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }

    half_dist = dist / 2.0;

    dist = 0.0;
    for (i = 1 ; i < n_points ; i++)
    {
	len = (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
	dist += len;
	if (dist >= half_dist)  /* we're there */
	{
	    frac = 1 - (dist - half_dist) / len;
	    *x = frac * (ux[i]-ux[i-1]) + ux[i-1];
	    *y = frac * (uy[i]-uy[i-1]) + uy[i-1];
	    return (0);
	}
    }

    fprintf (stderr, "Get_line_center failed.\n");
    *x = Points->x[0];
    *y = Points->y[0];
    return (-1);
}
