/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_u_to_d_col();
extern double D_u_to_d_row();

extern int D_move_abs();
extern int D_cont_abs();
extern int fillcolor;
extern int linecolor;
extern struct Cell_head window;


plot1 (name, mapset, Points)
    char *name, *mapset;
    struct line_pnts *Points;
{
    int i, natt;
    struct Map_info Map;
    double *x, *y;
    double N, S, E, W;
    int line;
    int nlines;
    int x_screen[4096], y_screen[4096];

    /*fd = open_vect (name, mapset);*/
    i = Vect_open_old (&Map, name, mapset);

    if (2 > i)
	G_fatal_error ("Failed opening vector file");

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    printf ("Plotting ... "); fflush (stdout);
    nlines = V2_num_areas(&Map);
    for (line = 1;line <= nlines; line++)
    {
	switch (Vect_get_area_points(&Map, line, Points))
	{
	case -1:
	    Vect_close (&Map);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	    break;
	case -2:
	    printf ("Done\n");
	    Vect_close (&Map);
	    return  0;
	    break;
	}
	V2_get_area_bbox(&Map, line, &N, &S, &E, &W);
	if ( S > window.north || N < window.south || 
	     W > window.east || E < window.west)
		continue;

	for(i=0; i < Points->n_points; i++)
	{
		    x_screen[i] = (int) (D_u_to_d_col( (*(Points->x+i))));
		    y_screen[i] = (int) (D_u_to_d_row( (*(Points->y+i))));
	}
	R_standard_color(fillcolor);
	R_polygon_abs(x_screen,y_screen,Points->n_points);
	if (linecolor > 0)
	{
		R_standard_color(linecolor);
		R_polyline_abs(x_screen,y_screen,Points->n_points);
	}
    }
}
