/* Functions: PS_vector_plot
**
** Author: Paul W. Carlson	March 1992
** modified to use G_plot_line() by Olga Waupotitsch on dec,93
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

        start_line(xarray[0], yarray[0]);

	for (i = 0; i < np - 1; i++)
	{
            sec_draw = 0;
            G_plot_line(xarray[0], yarray[0], xarray[1], yarray[1]);
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
	fprintf(PS.fp, "D\n");
    }
    fprintf(PS.fp, "\n");
    return 0;
}

