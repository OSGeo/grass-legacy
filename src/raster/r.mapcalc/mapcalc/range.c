#include "glob.h"
#include "mapcalc.h"

#define FPFMT "%.6f"

void print_range (char *result)
{
    long lmin, lmax;
    double dmin, dmax;
    char *mapset;
    char tmin[50], tmax[50];
    int ok;

    mapset = G_mapset();
    if (map_is_fp (result, mapset))
    {
	if(ok = get_dcell_range (result, mapset, &dmin, &dmax))
	{
	    sprintf (tmin, FPFMT, dmin);
	    G_trim_decimal (tmin);
	    sprintf (tmax, FPFMT, dmax);
	    G_trim_decimal (tmax);
	}
    }
    else
    {
	if(ok = get_cell_range (result, mapset, &lmin, &lmax))
	{
	    sprintf (tmin, "%ld", lmin);
	    sprintf (tmax, "%ld", lmax);
	}
    }
    if (ok)
	fprintf (stdout,"range: %s %s\n", tmin, tmax);
}

int get_cell_range (char *name, char *mapset, long *lmin, long *lmax)
{
    struct Range range;
    CELL min, max;

    if (G_read_range (name, mapset, &range) < 0)
	return 0;
    G_get_range_min_max (&range, &min, &max);
    if (ISNULL(&min) || ISNULL(&max))
	return 0;
    *lmin = min;
    *lmax = max;
    return 1;
}

int get_dcell_range (char *name, char *mapset, double *dmin, double *dmax)
{
    struct FPRange range;
    DCELL min, max;

    if (G_read_fp_range (name, mapset, &range) < 0)
	return 0;
    G_get_fp_range_min_max (&range, &min, &max);
    if (ISNULL_D(&min) || ISNULL_D(&max))
	return 0;
    *dmin = min;
    *dmax = max;
    return 1;
}
