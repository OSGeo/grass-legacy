/* plot2() - Level Two vector reading 
* 
* Michael Shapiro,
* U.S. Army Construction Engineering
* Research Laboratory
*
*/

#include "gis.h"
#include "Vect.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();

extern int quiet;

int 
plot2 (char *name, char *mapset, struct line_pnts *Points)
{
    double *x, *y;
    double N,S,E,W;
    char buf[128];
    int i, np;
    int nlines;
    struct Cell_head window;
    /*char *dig__P_init(), *err;*/
    struct Map_info P_map;

	if (!quiet) {
		fprintf (stdout,"Initializing [%s] ... ", name);
		fflush (stdout);
	}

    Vect_set_open_level(2);
    if (0 > Vect_open_old (&P_map, name, mapset))
    {
	/*
	fprintf (stderr, "\nWARNING: vector file [%s] - Could not open Level 2\n", name);
	*/
	return -1;
    }

    Vect__get_window (&P_map, &N, &S, &E, &W);
	if (!quiet) {
		fprintf (stdout,"Plotting ... "); fflush (stdout);
		Vect_print_header(&P_map);
	}

    G_get_set_window (&window);
    if (!quiet) {
		fprintf (stdout,"\n");
		G_format_northing (N, buf, window.proj);
		fprintf (stdout," North: %s\n", buf);

		G_format_northing (S, buf, window.proj);
		fprintf (stdout," South: %s\n", buf);

		G_format_easting (E, buf, window.proj);
		fprintf (stdout," East:  %s\n", buf);

		G_format_easting (W, buf, window.proj);
		fprintf (stdout," West:  %s\n", buf);
		fprintf (stdout,"\n");
	}

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    nlines = V2_num_lines (&P_map);

    /* let library do window checking for us */

    Vect_set_constraint_region (&P_map, window.north, window.south, window.east, window.west);

    while (1)
    {
	int ret;

        if (0 > (ret = Vect_read_next_line (&P_map, Points)))
	{
	    if (ret == -1)
		G_warning ("Read error\n");
	    break;
	}
	np = Points->n_points;
	x  = Points->x;
	y =  Points->y;
	for (i=1; i < np; i++)
	{
	    G_plot_line (x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }

	if (!quiet)
		fprintf (stdout,"Done\n");

    return 0;
}
