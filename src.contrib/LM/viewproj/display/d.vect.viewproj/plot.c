/*
****************************************************************************
*
* MODULE:       d.vect.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Tuan Tran, LMMS
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a vector map in a map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/* vector reading */

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "Vect.h"
#include "raster.h"	/* For R_* - Bev Wallace */
#include "display.h"	/* For D_* - Bev Wallace */

#include "coord_systems.viewproj.h"


static void point_loop (struct line_pnts *Points, struct Cell_head region)
{
	latlon_coord_type llp1,llp2;
	screen_coord_type sp1,sp2;
	double *x, *y;
	int i;

	x = Points->x;
	y = Points->y;

	for (i = 1; i < Points->n_points; i++)
	{
	    llp1.lat = y[0];
            llp2.lat = y[1];

	    /* Bev Wallace added G_adjust_east_longitude for wrap-around */
	    llp1.lon = G_adjust_east_longitude (x[0], region.west);
	    llp2.lon = G_adjust_east_longitude (x[1], region.west);

	    /* BOTH points must be in the region - Bev Wallace */
	    if (
		(region.east >= llp1.lon && region.west <= llp1.lon && 
		region.north >= llp1.lat && region.south <= llp1.lat) &&
		(region.east >= llp2.lon && region.west <= llp2.lon && 
		region.north >= llp2.lat && region.south <= llp2.lat)) {  
			sp1 = latlon_to_screen (llp1);
			sp2 = latlon_to_screen (llp2);
			R_move_abs (sp1.x, sp1.y);
			R_cont_abs (sp2.x, sp2.y);
	    }
	    /*G_plot_line(x[0], y[0], x[1], y[1]);*/
	    x++;
	    y++;
	}
}


/* plot1_viewproj() - Level One vector reading */
/* same as plot1(), but instead of G_plotline I use the viewproj routines */

int plot1_viewproj (char *name, char *mapset, struct line_pnts *Points)
{
    struct Map_info Map;

    /* Get the region - Tuan Tran */
    struct Cell_head region;
    G_get_window(&region);

    Vect_set_open_level (1);
    if (1 > Vect_open_old (&Map, name, mapset))
	G_fatal_error ("Failed opening vector file");

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);
/*
    fprintf (stdout, "\n...  n=%.3f s=%.3f e=%.3f w=%.3f\n", 
	region.north, region.south, region.east, region.west);
*/
    fprintf (stdout, "Plot ... "); fflush (stdout);
    fflush (stdout);

    while (1)
    {
        switch (Vect_read_next_line (&Map, Points))
	{
	case -1:
	    Vect_close (&Map);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	case -2: /* EOF */
	    fprintf (stdout, "Done\n");
	    fflush (stdout);
	    Vect_close (&Map);
	    return  0;
	}
	point_loop (Points, region);
    }
}


/* plot2_viewproj() - Level Two vector reading */
/* same as plot2(), but instead of G_plotline I use the viewproj routines */

int plot2_viewproj (char *name, char *mapset, struct line_pnts *Points)
{
    struct Cell_head window;
    /*char *dig__P_init(), *err;*/
    struct Map_info P_map;

    fprintf (stdout, "Initializing [%s] ... ", name);
    fflush (stdout);

    /*
    if (NULL != (err = dig__P_init (name, mapset, &P_map)))
    */
    if (2 > Vect_open_old (&P_map, name, mapset))
    {
	/*
	fprintf (stderr, "\nWARNING: vector file [%s] - Could not open Level 2\n", name);
	*/
	return -1;
    }

    fprintf (stdout, "Plot 2 ... "); 
    fflush (stdout);

/*    G_get_set_window (&window); */
    G_get_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);



#ifdef OLD
{
    int line, nlines;
    double N,S,E,W;
    
    nlines = V2_num_lines (&P_map);
    for (line = 1; line <= nlines; line++)
    {
	if (V2_get_line_bbox (&P_map, line, &N, &S, &E, &W) < 0)
	{
	    fprintf (stderr, "\nWARNING: vector file [%s] - read error\n", name);
	    return -1;
	}
	if (!G_window_overlap (&window, N, S, E, W))
	    continue;
        if (V2_read_line (&P_map, Points, line) < 0)
	{
	    fprintf (stderr, "\nWARNING: vector file [%s] - read error\n", name);
	    return -1;
	}
	point_loop (Points, window);
    }
}
#endif


    /* let library do window checking for us */

    Vect_set_constraint_region (&P_map, window.north, window.south, window.east, window.west);


    fprintf (stdout, "\n...Vect_set_constraint_region n=%.3f s=%.3f e=%.3f w=%.3f\n", 
	window.north, window.south, window.east, window.west);
    fflush (stdout);

    while (1)
    {
	int ret;

        if (0 > (ret = Vect_read_next_line (&P_map, Points)))
	{
	    if (ret == -1) {
		fprintf (stderr, "\nWARNING: vector file [%s] - Read error\n", 
			name);
		G_warning ("Read error\n");
	    }
	    break;
	}
	point_loop (Points, window);
    }

    fprintf (stdout, "Done\n");
    fflush (stdout);

   return 0;
}
