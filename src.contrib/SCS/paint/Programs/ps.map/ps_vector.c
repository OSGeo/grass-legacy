/* Functions: PS_vector_plot, PS_plot_vect
**
** Author: Paul W. Carlson	March 1992
*/

#include "Vect.h"
#include "ps_info.h"

PS_vector_plot(P_map)
struct Map_info *P_map;
{
    struct line_pnts *Points;
    int i, k, np;
    double *xarray, *yarray;

    /* allocate memory for coordinates */
    Points = Vect_new_line_struct();

    /* process only vectors in current window */
    Vect_set_constraint_region(P_map, PS.w.north, PS.w.south, 
		PS.w.east, PS.w.west);

    /* read and plot vectors */
    k = 0;
    while (1)
    {
	int ret;

	if (0 > (ret = Vect_read_next_line(P_map, Points)))
	{
	    if (ret == -1) G_warning("Read error in vector file\n");
	    break;
	}
	np = Points->n_points;
	xarray = Points->x;
	yarray = Points->y;
	for (i = 0; i < np - 1; i++)
	{
	    PS_plot_vect(xarray[0], yarray[0], xarray[1], yarray[1], i);
	    if (k == 2)
	    {
		fprintf(PS.fp, "\n");
		k = 0;
	    }
	    else
	    {
		fprintf(PS.fp, " ");
		k++;
	    }
	    xarray++;
	    yarray++;
	}
	fprintf(PS.fp, "D ");
    }
    fprintf(PS.fp, "\n");
    return 0;
}



PS_plot_vect(east1, north1, east2, north2, i)
double east1, north1, east2, north2;
int i;
{
    double x1, x2, y1, y2;

    y1 = YCONV(north1);
    y2 = YCONV(north2);

    if (PS.w.proj == PROJECTION_LL)
    {
	if (east1 > east2)
	    while ((east1 - east2) > 180) east2 += 360;
	else if (east2 > east1)
	    while ((east2 - east1) > 180) east1 += 360;
	while (east1 > PS.w.east)
	{
	    east1 -= 360.0;
	    east2 -= 360.0;
	}
	while (east1 < PS.w.west)
	{
	    east1 += 360.0;
	    east2 += 360.0;
	}
	x1 = XCONV(east1);
	x2 = XCONV(east2);

	draw_vect(x1, y1, x2, y2, i);

	if (east2 > PS.w.east || east2 < PS.w.west)
	{
	    while (east2 > PS.w.east)
	    {
		east1 -= 360.0;
		east2 -= 360.0;
	    }
	    while (east2 < PS.w.west)
	    {
		east1 += 360.0;
		east2 += 360.0;
	    }
	    x1 = XCONV(east1);
	    x2 = XCONV(east2);
	    draw_vect(x1, y1, x2, y2, i);
	}
    }
    else
    {
	x1 = XCONV(east1);
	x2 = XCONV(east2);
	draw_vect(x1, y1, x2, y2, i);
    }
}

