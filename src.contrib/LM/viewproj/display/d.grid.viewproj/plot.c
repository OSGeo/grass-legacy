/*
****************************************************************************
*
* MODULE:       d.grid.viewproj
* AUTHOR(S):    Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a grid in a map projection.
*		Based upon d.grid
* COPYRIGHT:    (C) 2003 by Lockheed Martin Space Systems, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <math.h>

#include "gis.h"
#include "display.h"
#include "raster.h"

#include "coord_systems.viewproj.h"

/* 
 * Similar to d.grid's plot().
 * Instead of G_plot_line, use the viewproj routines.
 * Draw grid lines in many segments because they may be curved
 */

/* #define DEBUG */

/* Bev Wallace 6/18/03
 * Increased NUM_GRID_POINTS from 100 to 800 for the following case:
	g.region n=90 s=-90 e=451 w=91; d.erase
	d.auto.viewproj projname=LambertAzimuthalEqualArea
	d.mon.viewproj; d.grid.viewproj size=90
*/

#ifdef DEBUG
#define NUM_GRID_POINTS	3
#else
#define NUM_GRID_POINTS	800 /* For curved grid lines */
#endif


static int viewproj_lat_line (struct Cell_head window, double lat)
{
	int i;
	double delta;
	latlon_coord_type llp1;	/* For viewproj */
	screen_coord_type sp0, sp1;	/* For viewproj */

	/* Move to the first point */
	llp1.lat = lat;
	llp1.lon = window.west;
	sp1 = latlon_to_screen (llp1);
	R_move_abs (sp1.x, sp1.y);

#ifdef DEBUG
	fprintf (stdout, "viewproj_lat_line lat=%f lon=%f\n", 
		llp1.lat, llp1.lon);
	fprintf (stdout, " north=%f south=%f east=%f west=%f\n", 
		window.north, window.south, window.east, window.west);
	fflush (stdout);
#endif

	/* Draw the grid points along the line */
	sp0.x = -1; sp0.y = -1;	/* Initialize */
	delta = (window.east - window.west) / (float) NUM_GRID_POINTS;
	for (i = 0; i < NUM_GRID_POINTS; i++) {
		llp1.lon += delta;
		sp1 = latlon_to_screen (llp1);
#ifdef DEBUG
		fprintf (stdout, " i=%d lat=%f lon=%f\n", 
			i, llp1.lat, llp1.lon);
		fflush (stdout);
#endif
		/* Draw if it differs from the previous screen point */
		if (sp0.x != sp1.x || sp0.y != sp1.y) {
			R_cont_abs (sp1.x, sp1.y);
			sp0.x = sp1.x; sp0.y = sp1.y;
		}
	} 
	return 0;
}


static int viewproj_lon_line (struct Cell_head window, double lon)
{
	int i;
	double delta;
	latlon_coord_type llp1;	/* For viewproj */
	screen_coord_type sp0, sp1;	/* For viewproj */

	/* Move to the first point */
	llp1.lat = window.south;
	llp1.lon = G_adjust_east_longitude (lon, window.west);
	sp1 = latlon_to_screen (llp1);
	R_move_abs (sp1.x, sp1.y);

#ifdef DEBUG
	fprintf (stdout, "viewproj_lon_line lat=%f lon=%f\n", 
		llp1.lat, llp1.lon);
	fprintf (stdout, " north=%f south=%f east=%f west=%f\n", 
		window.north, window.south, window.east, window.west);
	fflush (stdout);
#endif

	/* Draw the grid points along the line */
	sp0.x = -1; sp0.y = -1;	/* Initialize */
	delta = (window.north - window.south) / (float) NUM_GRID_POINTS;
	for (i = 0; i < NUM_GRID_POINTS; i++) {
		llp1.lat += delta;
		sp1 = latlon_to_screen (llp1);
#ifdef DEBUG
		fprintf (stdout, " i=%d lat=%f lon=%f\n", 
			i, llp1.lat, llp1.lon);
		fflush (stdout);
#endif
		/* Draw if it differs from the previous screen point */
		if (sp0.x != sp1.x || sp0.y != sp1.y) {
			R_cont_abs (sp1.x, sp1.y);
			sp0.x = sp1.x; sp0.y = sp1.y;
		}
	} 
	return 0;
}


int plot_grid_viewproj (double grid_size, double east, double north)
{
	double x,y;
	struct Cell_head window ;

	G_get_window (&window);

	G_setup_plot (
	    D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	    D_move_abs, D_cont_abs);

	/* Draw vertical grids - starts at the west */
	if (window.west > east)
		x = ceil((window.west - east)/grid_size) * grid_size + east ;
	else
		x = east - floor((east - window.west)/grid_size) * grid_size ;

	while (x <= window.east)
	{
        	viewproj_lon_line (window, x);
		x += grid_size;
	}

	/* Draw horizontal grids - starts at the south */
	if (window.south > north)
		y = ceil((window.south - north)/grid_size) * grid_size + north ;
	else 
		y = north - floor((north-window.south)/grid_size) * grid_size ;

	while (y <= window.north)
	{
        	viewproj_lat_line (window, y);
		y += grid_size;
	}

	return 0;
}
