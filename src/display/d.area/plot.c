/* plot1() - Level One vector reading */
/* 12-30-1999 Bill Hughes
     Changed to dynamic allocation of x_screen, y_screen to remove the
     4096 vector line limit. */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"

extern int fillcolor;
extern int linecolor;
extern struct Cell_head window;

int plot1 (char *name, char *mapset, struct line_pnts *Points)
{
    int i, natt;
    struct Map_info Map;
    double *x, *y;
    double N, S, E, W;
    int line;
    int nlines;
    int *x_screen, *y_screen;

    /*fd = open_vect (name, mapset);*/
    i = Vect_open_old (&Map, name, mapset);

    if (2 > i)
	G_fatal_error ("Failed opening vector file");

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    fprintf (stdout,"Plotting ... \n"); fflush (stdout);
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
	    fprintf (stdout,"Done\n");
	    Vect_close (&Map);
	    return  0;
	    break;
	}
	V2_get_area_bbox(&Map, line, &N, &S, &E, &W);
	if ( S > window.north || N < window.south || 
	     W > window.east || E < window.west)
		continue;

	if(NULL == (x_screen = (int *)G_malloc(sizeof(int) * Points->n_points)))
 		G_fatal_error("Cannot allocate %d integers",Points->n_points);
	if(NULL == (y_screen = (int *)G_malloc(sizeof(int) * Points->n_points)))
 		G_fatal_error("Cannot allocate %d integers",Points->n_points);
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
	G_free(x_screen);
	G_free(y_screen);
    }
    return 0;
}
