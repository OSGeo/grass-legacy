/* plot2() - Level Two vector reading */

#include "gis.h"
#include "digit.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();


plot2 (name, mapset)
    char *name, *mapset;
{
    struct Map_info P_map;
    double *x, *y;
    int i, np;
    int line, nlines;
    double N,S,E,W;
    struct Cell_head window;
    struct line_pnts *p;
    char *dig__P_init(), *err;

    printf ("Initializing [%s in %s] ... ", name, mapset);
    fflush (stdout);

    if (NULL != (err = dig__P_init (name, mapset, &P_map)))
    {
	fprintf (stderr, "\nWARNING: vector file [%s in %s] - %s\n",
		name, mapset, err);
	return -1;
    }
    printf ("Plotting ... "); fflush (stdout);

    G_get_set_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    nlines = dig_P_num_lines (&P_map);
    for (line = 1; line <= nlines; line++)
    {
	if (dig_P_get_line_bbox (&P_map, line, &N, &S, &E, &W) < 0)
	{
	    fprintf (stderr, "\nWARNING: vector file [%s in %s] - read error\n",
		name, mapset);
	    return -1;
	}
	if (!G_window_overlap (&window, N, S, E, W))
	    continue;
        if (dig_P_read_line (&P_map, line, &p) < 0)
	{
	    fprintf (stderr, "\nWARNING: vector file [%s in %s] - read error\n",
		name, mapset);
	    return -1;
	}
	np = p->n_points;
	x  = p->x;
	y =  p->y;
	for(i=1; i < np; i++)
	{
	    G_plot_line (x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }
    printf ("Done\n");
    return 0;
}
